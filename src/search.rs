use chess::{Board, ChessMove, MoveGen, EMPTY};
use std::time::Instant;

use crate::evaluation::{evaluate_board_incremental, EvalState};
use crate::helpers::{move_score, HistoryHeuristic, MAX_DEPTH, mvv_lva_score};
use crate::tt::{TranspositionTable, TTFlag};

const MATE_SCORE: i32 = 100000;
const INFINITY: i32 = 1000000;

pub struct SearchState {
    pub tt: TranspositionTable,
    pub history: HistoryHeuristic,
    pub killer_moves: [[Option<ChessMove>; 2]; MAX_DEPTH],
    pub counter_moves: [[Option<ChessMove>; 64]; 64],
    pub nodes_searched: u64,
    pub null_cutoffs: u64,
    pub lmr_attempts: u64,
    pub pvs_research: u64,
    pub repetition_avoids: u64,
    pub futility_prunes: u64,
    pub stop_search: bool,
    pub position_history: Vec<u64>,
    pub search_history: [u64; MAX_DEPTH * 2],
}

impl SearchState {
    pub fn new() -> Self {
        Self {
            tt: TranspositionTable::new(256),
            history: HistoryHeuristic::new(),
            killer_moves: [[None; 2]; MAX_DEPTH],
            counter_moves: [[None; 64]; 64],
            nodes_searched: 0,
            null_cutoffs: 0,
            lmr_attempts: 0,
            pvs_research: 0,
            repetition_avoids: 0,
            futility_prunes: 0,
            stop_search: false,
            position_history: Vec::new(),
            search_history: [0; MAX_DEPTH * 2],
        }
    }

    pub fn clear_for_search(&mut self) {
        self.nodes_searched = 0;
        self.null_cutoffs = 0;
        self.lmr_attempts = 0;
        self.pvs_research = 0;
        self.repetition_avoids = 0;
        self.futility_prunes = 0;
        self.stop_search = false;
        self.killer_moves = [[None; 2]; MAX_DEPTH];
        self.search_history = [0; MAX_DEPTH * 2];
    }

    pub fn is_repetition(&self, hash: u64, ply: usize) -> bool {
        let count_in_game = self.position_history.iter().filter(|&&h| h == hash).count();
        
        let mut count_in_search = 0;
        let mut check_ply = ply;
        
        while check_ply >= 2 {
            check_ply -= 2;
            if check_ply < self.search_history.len() && self.search_history[check_ply] == hash {
                count_in_search += 1;
            }
        }
        
        count_in_game + count_in_search >= 3
    }
    
    pub fn is_repetition_move(&self, board: &Board, mv: ChessMove, ply: usize) -> bool {
        let new_board = board.make_move_new(mv);
        let new_hash = new_board.get_hash();
        self.is_repetition(new_hash, ply + 1)
    }

    pub fn get_move_order_score(&self, board: &Board, mv: ChessMove, ply: usize, prev_move: Option<ChessMove>) -> i32 {
        let is_capture = board.piece_on(mv.get_dest()).is_some() ||
                        (board.piece_on(mv.get_source()) == Some(chess::Piece::Pawn) &&
                         mv.get_source().get_file() != mv.get_dest().get_file() &&
                         board.piece_on(mv.get_dest()).is_none());
        
        if is_capture {
            return 10000000 + move_score(board, mv);
        }
        
        let mut score = 0;
        
        // No legality checks - if killer doesn't match a legal move, it won't be used
        if ply < MAX_DEPTH {
            if let Some(killer) = self.killer_moves[ply][0] {
                if mv == killer {
                    score = 900000;
                }
            }
            if score == 0 {
                if let Some(killer) = self.killer_moves[ply][1] {
                    if mv == killer {
                        score = 800000;
                    }
                }
            }
        }
        
        if score == 0 {
            if let Some(prev) = prev_move {
                let from = prev.get_source().to_index();
                let to = prev.get_dest().to_index();
                if let Some(counter) = self.counter_moves[from][to] {
                    if mv == counter {
                        score = 700000;
                    }
                }
            }
        }
        
        if score == 0 {
            score = self.history.get_score(mv);
        }
        
        score
    }
}

fn quiescence(board: &Board, eval_state: &mut EvalState, mut alpha: i32, beta: i32, ply_from_root: i32, state: &mut SearchState) -> i32 {
    state.nodes_searched += 1;
    
    let in_check = board.checkers() != &EMPTY;
    
    // Stand pat only valid when not in check
    if !in_check {
        let stand_pat = evaluate_board_incremental(board, eval_state, ply_from_root);
        
        if stand_pat >= beta {
            return beta;
        }
        
        if alpha < stand_pat {
            alpha = stand_pat;
        }
    }
    
    // Emergency depth limit only - much higher than before
    if ply_from_root >= 64 {
        return evaluate_board_incremental(board, eval_state, ply_from_root);
    }
    
    // When in check, we must search ALL legal moves (not just captures)
    if in_check {
        let move_gen = MoveGen::new_legal(board);
        let moves: Vec<ChessMove> = move_gen.collect();
        
        // Checkmate detection
        if moves.is_empty() {
            return -MATE_SCORE + ply_from_root;
        }
        
        for mv in moves {
            let new_board = board.make_move_new(mv);
            let mut new_eval = *eval_state;
            new_eval.apply_move(board, mv, board.side_to_move());
            
            let score = -quiescence(&new_board, &mut new_eval, -beta, -alpha, ply_from_root + 1, state);
            
            if score >= beta {
                return beta;
            }
            if score > alpha {
                alpha = score;
            }
        }
        
        return alpha;
    }
    
    // Not in check - search captures and checks
    let stand_pat = evaluate_board_incremental(board, eval_state, ply_from_root);
    
    // More conservative delta pruning - only in non-tactical positions
    const BIG_DELTA: i32 = 1500;  // Increased from 900
    if stand_pat + BIG_DELTA < alpha && ply_from_root > 2 {
        return alpha;
    }
    
    let move_gen = MoveGen::new_legal(board);
    let mut moves: Vec<(ChessMove, i32, bool)> = Vec::with_capacity(32);
    
    for mv in move_gen {
        let dest_piece = board.piece_on(mv.get_dest());
        let source_piece = board.piece_on(mv.get_source());
        
        let is_capture = dest_piece.is_some() ||
            (source_piece == Some(chess::Piece::Pawn) &&
             mv.get_source().get_file() != mv.get_dest().get_file() &&
             dest_piece.is_none());
        
        // Check if move gives check (only in first 2 plies to avoid explosion)
        let gives_check = if ply_from_root < 2 {
            let new_board = board.make_move_new(mv);
            new_board.checkers() != &EMPTY
        } else {
            false
        };
        
        if is_capture || gives_check || mv.get_promotion().is_some() {
            let score = if is_capture {
                mvv_lva_score(board, mv)
            } else if gives_check {
                100000  // Checks are interesting
            } else if mv.get_promotion().is_some() {
                200000  // Promotions very interesting
            } else {
                0
            };
            
            moves.push((mv, score, is_capture));
        }
    }
    
    // Sort moves by score
    moves.sort_unstable_by_key(|(_, score, _)| -score);
    
    for (mv, _, is_capture) in moves {
        // Much less aggressive futility pruning
        if is_capture {
            if let Some(captured) = board.piece_on(mv.get_dest()) {
                let capture_value = crate::evaluation::piece_value(captured);
                
                // Only prune clearly bad captures deep in the tree
                if ply_from_root > 4 && stand_pat + capture_value + 400 < alpha {
                    continue;
                }
            }
        }
        
        let new_board = board.make_move_new(mv);
        let mut new_eval = *eval_state;
        new_eval.apply_move(board, mv, board.side_to_move());
        
        let score = -quiescence(&new_board, &mut new_eval, -beta, -alpha, ply_from_root + 1, state);
        
        if score >= beta {
            return beta;
        }
        if score > alpha {
            alpha = score;
        }
    }

    alpha
}

fn negamax(
    board: &Board, 
    eval_state: &mut EvalState, 
    depth: i32, 
    mut alpha: i32, 
    beta: i32, 
    ply_from_root: usize, 
    state: &mut SearchState,
    prev_move: Option<ChessMove>,
) -> (i32, Vec<ChessMove>) {
    state.nodes_searched += 1;

    if state.stop_search {
        return (0, vec![]);
    }

    let current_hash = board.get_hash();
    state.search_history[ply_from_root] = current_hash;

    if ply_from_root > 0 && state.is_repetition(current_hash, ply_from_root) {
        return (0, vec![]);
    }

    let alpha_orig = alpha;
    let is_pv_node = beta - alpha > 1;

    // FIXED: Don't validate TT move - trust it or let it fail naturally
    let (tt_hit, tt_score, tt_move) = state.tt.probe(board, depth, alpha, beta, ply_from_root as i32);
    
    if tt_hit && ply_from_root > 0 && !is_pv_node {
        return (tt_score, tt_move.map_or(vec![], |m| vec![m]));
    }

    if depth == 0 {
        let score = quiescence(board, eval_state, alpha, beta, ply_from_root as i32, state);
        return (score, vec![]);
    }

    let in_check = board.checkers() != &EMPTY;
    
    let mated_value = -MATE_SCORE + ply_from_root as i32;
    if mated_value > alpha {
        alpha = mated_value;
        if beta <= mated_value {
            return (mated_value, vec![]);
        }
    }
    
    let static_eval = if in_check {
        -INFINITY
    } else {
        evaluate_board_incremental(board, eval_state, ply_from_root as i32)
    };
    
    if !is_pv_node && !in_check && depth <= 6 && static_eval - 80 * depth >= beta {
        return (static_eval, vec![]);
    }
    
    if ply_from_root > 0 && depth >= 3 && !in_check && static_eval >= beta {
        if let Some(null_board) = board.null_move() {
            let r = if depth >= 6 { 3 } else { 2 };
            let mut null_eval = *eval_state;
            let (null_score, _) = negamax(&null_board, &mut null_eval, depth - 1 - r, -beta, -beta + 1, ply_from_root + 1, state, None);
            
            if -null_score >= beta {
                state.null_cutoffs += 1;
                if null_score.abs() < MATE_SCORE - 100 {
                    return (beta, vec![]);
                }
            }
        }
    }
    
    // FIXED: Don't validate IID move either
    let tt_move = if tt_move.is_none() && depth >= 4 && is_pv_node {
        let mut iid_eval = *eval_state;
        let (_, pv) = negamax(board, &mut iid_eval, depth - 2, alpha, beta, ply_from_root, state, prev_move);
        pv.first().copied()
    } else {
        tt_move
    };

    struct ScoredMove {
        mv: ChessMove,
        score: i32,
        is_capture: bool,
        gives_check: bool,
    }

    let mut scored_moves: Vec<ScoredMove> = MoveGen::new_legal(board)
        .map(|mv| {
            let dest_piece = board.piece_on(mv.get_dest());
            let source_piece = board.piece_on(mv.get_source());
            
            let is_capture = dest_piece.is_some() || 
                            (source_piece == Some(chess::Piece::Pawn) &&
                             mv.get_source().get_file() != mv.get_dest().get_file());
            
            let new_board = board.make_move_new(mv);
            let gives_check = new_board.checkers() != &EMPTY;
            
            let mut score = state.get_move_order_score(board, mv, ply_from_root, prev_move);
            if Some(mv) == tt_move {
                score = 100000000;
            }
            
            ScoredMove { mv, score, is_capture, gives_check }
        })
        .collect();

    scored_moves.sort_unstable_by_key(|sm| -sm.score);

    let mut best_score = -INFINITY;
    let mut best_move = None;
    let mut best_pv = vec![];
    let mut move_count = 0;
    let mut quiets_tried = Vec::new();

    for scored in scored_moves {
        let mv = scored.mv;
        let is_capture = scored.is_capture;
        let gives_check = scored.gives_check;
        
        if !is_pv_node && !in_check && depth <= 8 && move_count >= 3 && !is_capture && !gives_check {
            if move_count >= 3 + depth as usize * 2 {
                continue;
            }
        }

        let new_board = board.make_move_new(mv);
        let mut new_eval = *eval_state;
        new_eval.apply_move(board, mv, board.side_to_move());

        let (score, sub_pv) = if move_count == 0 {
            let (s, pv) = negamax(&new_board, &mut new_eval, depth - 1, -beta, -alpha, ply_from_root + 1, state, Some(mv));
            (-s, pv)
        } else {
            let mut reduction: i32 = 0;
            if !in_check && !gives_check && !is_capture && mv.get_promotion().is_none() && move_count >= 3 && depth >= 3 {
                reduction = 1;
                
                if !is_pv_node {
                    reduction += 1;
                }
                
                if move_count >= 6 && depth >= 5 {
                    reduction += 1;
                }
                
                if ply_from_root < MAX_DEPTH {
                    if Some(mv) == state.killer_moves[ply_from_root][0] || 
                       Some(mv) == state.killer_moves[ply_from_root][1] {
                        reduction = (reduction - 1).max(0);
                    }
                }
                
                if let Some(prev) = prev_move {
                    let from = prev.get_source().to_index();
                    let to = prev.get_dest().to_index();
                    if Some(mv) == state.counter_moves[from][to] {
                        reduction = (reduction - 1).max(0);
                    }
                }
                
                reduction = reduction.min(depth - 1);
                
                if reduction > 0 {
                    state.lmr_attempts += 1;
                }
            }

            let (s, mut pv) = negamax(&new_board, &mut new_eval, depth - 1 - reduction, -alpha - 1, -alpha, ply_from_root + 1, state, Some(mv));
            let mut score = -s;

            if score > alpha {
                if reduction > 0 {
                    let mut research_eval = *eval_state;
                    research_eval.apply_move(board, mv, board.side_to_move());
                    let (s2, pv2) = negamax(&new_board, &mut research_eval, depth - 1, -alpha - 1, -alpha, ply_from_root + 1, state, Some(mv));
                    score = -s2;
                    pv = pv2;
                }

                if alpha < score && score < beta {
                    state.pvs_research += 1;
                    let mut pvs_eval = *eval_state;
                    pvs_eval.apply_move(board, mv, board.side_to_move());
                    let (s3, pv3) = negamax(&new_board, &mut pvs_eval, depth - 1, -beta, -alpha, ply_from_root + 1, state, Some(mv));
                    score = -s3;
                    pv = pv3;
                }
            }

            (score, pv)
        };

        move_count += 1;
        
        if !is_capture {
            quiets_tried.push(mv);
        }

        if score > best_score {
            best_score = score;
            best_move = Some(mv);
            
            best_pv = vec![mv];
            best_pv.extend(sub_pv);

            if score > alpha {
                alpha = score;

                if score >= beta {
                    if !is_capture && ply_from_root < MAX_DEPTH {
                        state.history.update(mv, depth, ply_from_root);
                        
                        if state.killer_moves[ply_from_root][0] != Some(mv) {
                            state.killer_moves[ply_from_root][1] = state.killer_moves[ply_from_root][0];
                            state.killer_moves[ply_from_root][0] = Some(mv);
                        }
                        
                        if let Some(prev) = prev_move {
                            let from = prev.get_source().to_index();
                            let to = prev.get_dest().to_index();
                            state.counter_moves[from][to] = Some(mv);
                        }
                        
                        for &quiet in &quiets_tried {
                            if quiet != mv {
                                state.history.penalize(quiet, depth);
                            }
                        }
                    }

                    state.tt.store(board, depth, beta, TTFlag::LowerBound, best_move, ply_from_root as i32);
                    return (beta, best_pv);
                }
            }
        }
    }

    if move_count == 0 {
        if board.checkers() != &EMPTY {
            return (-MATE_SCORE + ply_from_root as i32, vec![]);
        }
        return (0, vec![]);
    }

    let flag = if best_score <= alpha_orig {
        TTFlag::UpperBound
    } else {
        TTFlag::Exact
    };

    state.tt.store(board, depth, best_score, flag, best_move, ply_from_root as i32);
    (best_score, best_pv)
}

pub fn pick_move_timed(board: &Board, max_depth: i32, time_limit: Option<f64>, state: &mut SearchState) -> (Option<ChessMove>, Vec<(ChessMove, i32)>) {
    state.clear_for_search();

    let start_time = Instant::now();
    let time_for_move = time_limit.unwrap_or(999999.0);

    let root_eval = EvalState::from_board(board);

    let mut moves: Vec<ChessMove> = MoveGen::new_legal(board).collect();
    
    let non_draw_moves: Vec<ChessMove> = moves.iter()
        .filter(|&&mv| {
            let new_board = board.make_move_new(mv);
            new_board.status() != chess::BoardStatus::Stalemate
        })
        .copied()
        .collect();

    if !non_draw_moves.is_empty() {
        moves = non_draw_moves;
    }

    let static_eval = root_eval.get_score(board.side_to_move());
    if static_eval > 100 {
        let non_repetition_moves: Vec<ChessMove> = moves.iter()
            .filter(|&&mv| !state.is_repetition_move(board, mv, 0))
            .copied()
            .collect();
        
        if !non_repetition_moves.is_empty() {
            state.repetition_avoids = (moves.len() - non_repetition_moves.len()) as u64;
            moves = non_repetition_moves;
        }
    }

    moves.sort_by_cached_key(|&mv| -move_score(board, mv));

    let mut best_move = None;
    let mut best_score = -INFINITY;
    let mut pv_move = None;
    let mut all_root_moves = vec![];

    for current_depth in 1..=max_depth {
        if start_time.elapsed().as_secs_f64() > time_for_move * 0.85 {
            break;
        }

        let (mut search_alpha, mut search_beta) = if current_depth >= 4 && best_move.is_some() {
            (best_score - 50, best_score + 50)
        } else {
            (-INFINITY, INFINITY)
        };

        let mut window = 50;

        loop {
            let mut current_moves = moves.clone();
            
            // FIXED: Just check if PV exists in move list, no expensive validation
            if let Some(pv) = pv_move {
                if let Some(pos) = current_moves.iter().position(|&m| m == pv) {
                    current_moves.remove(pos);
                    current_moves.insert(0, pv);
                }
            }

            let mut current_best_move = None;
            let mut current_best_score = -INFINITY;
            let mut current_pv = vec![];
            let mut root_scores = vec![];

            for &mv in &current_moves {
                if start_time.elapsed().as_secs_f64() > time_for_move * 0.95 {
                    state.stop_search = true;
                    break;
                }

                let new_board = board.make_move_new(mv);
                
                let mut new_eval = root_eval;
                new_eval.apply_move(board, mv, board.side_to_move());
                
                let (score, pv) = negamax(&new_board, &mut new_eval, current_depth - 1, -search_beta, -search_alpha, 1, state, Some(mv));
                let score = -score;

                if score > current_best_score {
                    current_best_score = score;
                    current_best_move = Some(mv);
                    current_pv = vec![mv];
                    current_pv.extend(pv);
                }

                root_scores.push((mv, score));
            }

            if !state.stop_search && current_depth >= 4 && 
            (current_best_score <= search_alpha || current_best_score >= search_beta) {
                window *= 2;
                search_alpha = current_best_score - window;
                search_beta = current_best_score + window;
                continue;
            }

            if state.stop_search {
                break;
            }

            best_score = current_best_score;
            best_move = current_best_move;
            pv_move = best_move;
            all_root_moves = root_scores;

            let elapsed = start_time.elapsed().as_millis() as u64;
            let nps = if elapsed > 0 {
                state.nodes_searched * 1000 / elapsed
            } else {
                0
            };

            let (_, _, hit_rate) = state.tt.get_stats();

            let score_str = if best_score.abs() > 90000 {
                let plies_to_mate = MATE_SCORE - best_score.abs();
                let mate_in = (plies_to_mate + 1) / 2;
                if best_score < 0 {
                    format!("score mate -{}", mate_in)
                } else {
                    format!("score mate {}", mate_in)
                }
            } else {
                format!("score cp {}", best_score)
            };

            let pv_str: Vec<String> = current_pv.iter().map(|m| format!("{}", m)).collect();
            println!("info depth {} nodes {} time {} nps {} {} hashfull {} pv {}", 
                current_depth, state.nodes_searched, elapsed, nps, score_str, hit_rate as i32, pv_str.join(" "));

            break;
        }

        if state.stop_search {
            break;
        }
    }

    let filled = state.tt.table.iter().filter(|e| e.is_some()).count();
    let fill_pct = (filled * 100) / state.tt.size;
    let (hits, misses, _) = state.tt.get_stats();
    println!("# TT: {}/{} entries ({}%), {} hits, {} misses", filled, state.tt.size, fill_pct, hits, misses);
    println!("# Stats - Null: {}, LMR: {}, PVS: {}, Futility: {}, Reps avoided: {}", 
        state.null_cutoffs, state.lmr_attempts, state.pvs_research, state.futility_prunes, state.repetition_avoids);

    (best_move, all_root_moves)
}