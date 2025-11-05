use chess::{Board, ChessMove, MoveGen, EMPTY};
use std::time::Instant;
use rand::prelude::*;

use crate::evaluation::evaluate_board;
use crate::helpers::{order_moves, move_score, mvv_lva_score, capture_exchange_value, HistoryHeuristic, MAX_DEPTH};
use crate::tt::{TranspositionTable, TTFlag};

const MATE_SCORE: i32 = 100000;
const INFINITY: i32 = 1000000;

pub struct SearchState {
    pub tt: TranspositionTable,
    pub history: HistoryHeuristic,
    pub killer_moves: [[Option<ChessMove>; 2]; MAX_DEPTH],
    pub nodes_searched: u64,
    pub null_cutoffs: u64,
    pub lmr_attempts: u64,
    pub pvs_research: u64,
    pub stop_search: bool,
}

impl SearchState {
    pub fn new() -> Self {
        Self {
            tt: TranspositionTable::new(256),
            history: HistoryHeuristic::new(),
            killer_moves: [[None; 2]; MAX_DEPTH],
            nodes_searched: 0,
            null_cutoffs: 0,
            lmr_attempts: 0,
            pvs_research: 0,
            stop_search: false,
        }
    }

    pub fn clear_for_search(&mut self) {
        self.nodes_searched = 0;
        self.null_cutoffs = 0;
        self.lmr_attempts = 0;
        self.pvs_research = 0;
        self.stop_search = false;
        self.killer_moves = [[None; 2]; MAX_DEPTH];
    }
}

fn quiescence(board: &Board, mut alpha: i32, beta: i32, ply_from_root: i32, state: &mut SearchState) -> i32 {
    let stand_pat = evaluate_board(board, ply_from_root);
    
    if stand_pat >= beta {
        return beta;
    }
    
    if ply_from_root >= 20 {
        return stand_pat;
    }
    
    const BIG_DELTA: i32 = 900;
    if stand_pat + BIG_DELTA < alpha {
        return alpha;
    }
    
    if alpha < stand_pat {
        alpha = stand_pat;
    }

    // Generate captures - check both regular captures and en passant
    let mut captures: Vec<ChessMove> = MoveGen::new_legal(board)
        .filter(|mv| {
            board.piece_on(mv.get_dest()).is_some() ||
            (board.piece_on(mv.get_source()) == Some(chess::Piece::Pawn) &&
             mv.get_source().get_file() != mv.get_dest().get_file() &&
             board.piece_on(mv.get_dest()).is_none())
        })
        .filter(|&mv| capture_exchange_value(board, mv) >= -100)
        .collect();

    captures.sort_by_key(|&mv| -mvv_lva_score(board, mv));

    for mv in captures {
        let new_board = board.make_move_new(mv);
        let score = -quiescence(&new_board, -beta, -alpha, ply_from_root + 1, state);
        
        if score >= beta {
            return beta;
        }
        if score > alpha {
            alpha = score;
        }
    }

    alpha
}

fn negamax(board: &Board, depth: i32, mut alpha: i32, beta: i32, ply_from_root: usize, state: &mut SearchState) -> (i32, Vec<ChessMove>) {
    state.nodes_searched += 1;

    if state.stop_search {
        return (0, vec![]);
    }

    let alpha_orig = alpha;

    // TT probe
    let (tt_hit, tt_score, tt_move) = state.tt.probe(board, depth, alpha, beta);
    if tt_hit && ply_from_root > 0 {
        return (tt_score, tt_move.map_or(vec![], |m| vec![m]));
    }

    if depth == 0 {
        let score = quiescence(board, alpha, beta, ply_from_root as i32, state);
        return (score, vec![]);
    }

    // Null move pruning
    // Note: Python version also checks board.halfmove_clock < 90, but the chess crate
    // doesn't expose halfmove clock on Board. This is a minor optimization that rarely
    // affects play, so we omit it.
    let in_check = board.checkers() != &EMPTY;
    if ply_from_root > 0 && depth >= 3 && !in_check {
        if let Some(null_board) = board.null_move() {
            let r = if depth >= 6 { 3 } else { 2 };
            let (null_score, _) = negamax(&null_board, depth - 1 - r, -beta, -beta + 1, ply_from_root + 1, state);
            
            if -null_score >= beta {
                state.null_cutoffs += 1;
                return (beta, vec![]);
            }
        }
    }

    let mut moves: Vec<ChessMove> = MoveGen::new_legal(board).collect();
    
    if moves.is_empty() {
        if board.checkers() != &EMPTY {
            return (-MATE_SCORE + ply_from_root as i32, vec![]);
        }
        return (0, vec![]);
    }

    // Move ordering
    if let Some(tt_mv) = tt_move {
        if let Some(pos) = moves.iter().position(|&m| m == tt_mv) {
            moves.remove(pos);
            moves.insert(0, tt_mv);
        }
    } else {
        order_moves(board, &mut moves, ply_from_root, &state.history, &state.killer_moves);
    }

    let mut best_score = -INFINITY;
    let mut best_move = None;
    let mut pv = vec![];

    for (i, &mv) in moves.iter().enumerate() {
        // Check if capture (including en passant)
        let is_capture = board.piece_on(mv.get_dest()).is_some() ||
                        (board.piece_on(mv.get_source()) == Some(chess::Piece::Pawn) &&
                         mv.get_source().get_file() != mv.get_dest().get_file() &&
                         board.piece_on(mv.get_dest()).is_none());
        
        let new_board = board.make_move_new(mv);
        let gives_check = new_board.checkers() != &EMPTY;

        let score = if i == 0 {
            let (s, sub_pv) = negamax(&new_board, depth - 1, -beta, -alpha, ply_from_root + 1, state);
            pv = sub_pv;
            -s
        } else {
            // LMR
            let mut reduction = 0;
            if !in_check && !gives_check && !is_capture && mv.get_promotion().is_none() && i >= 3 && depth >= 3 {
                reduction = 1;
                if i >= 6 && depth >= 5 {
                    reduction = 2;
                }
                if reduction > 0 {
                    state.lmr_attempts += 1;
                }
            }

            let (mut s, mut sub_pv) = negamax(&new_board, depth - 1 - reduction, -alpha - 1, -alpha, ply_from_root + 1, state);
            let mut score = -s;

            if score > alpha {
                if reduction > 0 {
                    let (s2, sub_pv2) = negamax(&new_board, depth - 1, -alpha - 1, -alpha, ply_from_root + 1, state);
                    score = -s2;
                    sub_pv = sub_pv2;
                }

                if alpha < score && score < beta {
                    state.pvs_research += 1;
                    let (s3, sub_pv3) = negamax(&new_board, depth - 1, -beta, -alpha, ply_from_root + 1, state);
                    score = -s3;
                    sub_pv = sub_pv3;
                }
            }

            pv = sub_pv;
            score
        };

        if score > best_score {
            best_score = score;
            best_move = Some(mv);
            let mut new_pv = vec![mv];
            new_pv.extend(pv.clone());
            pv = new_pv;

            if score > alpha {
                alpha = score;

                if score >= beta {
                    state.history.update(mv, depth, ply_from_root);
                    if !is_capture && ply_from_root < MAX_DEPTH {
                        state.killer_moves[ply_from_root][1] = state.killer_moves[ply_from_root][0];
                        state.killer_moves[ply_from_root][0] = Some(mv);
                    }

                    state.tt.store(board, depth, beta, TTFlag::LowerBound, best_move);
                    return (beta, pv);
                }
            }
        }
    }

    let flag = if best_score <= alpha_orig {
        TTFlag::UpperBound
    } else {
        TTFlag::Exact
    };

    state.tt.store(board, depth, best_score, flag, best_move);
    (best_score, pv)
}

pub fn pick_move_timed(board: &Board, max_depth: i32, time_limit: Option<f64>, stochastic: bool, state: &mut SearchState) -> (Option<ChessMove>, Vec<(ChessMove, i32)>) {
    state.clear_for_search();

    let start_time = Instant::now();
    let time_for_move = time_limit.unwrap_or(999999.0);

    let mut moves: Vec<ChessMove> = MoveGen::new_legal(board).collect();
    
    // Filter draw moves (only stalemate check - more comprehensive draw detection would require game history)
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

    // Initial move ordering by score
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
                let (score, pv) = negamax(&new_board, current_depth - 1, -search_beta, -search_alpha, 1, state);
                let score = -score;

                if score > current_best_score {
                    current_best_score = score;
                    current_best_move = Some(mv);
                    current_pv = vec![mv];
                    current_pv.extend(pv);
                }

                root_scores.push((mv, score));
            }

            if !state.stop_search && current_depth >= 4 && (current_best_score <= search_alpha || current_best_score >= search_beta) {
                // Widen aspiration window (double it)
                window *= 2;
                search_alpha = current_best_score - window;
                search_beta = current_best_score + window;
                root_scores.clear();
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
                let mate_in = (100000 - best_score.abs()) / 2;
                if best_score < 0 {
                    format!("score mate {}", -mate_in)
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
    println!("# Null cutoffs: {}, LMR: {}, PVS research: {}", state.null_cutoffs, state.lmr_attempts, state.pvs_research);

    if stochastic && !all_root_moves.is_empty() && all_root_moves.len() == moves.len() {
        let threshold = best_score - 10;
        let good_moves: Vec<ChessMove> = all_root_moves.iter()
            .filter(|(_, score)| *score >= threshold)
            .map(|(mv, _)| *mv)
            .collect();
        
        if good_moves.len() > 1 {
            let mut rng = rand::rng();
            best_move = good_moves.choose(&mut rng).copied();
            println!("# Stochastic: chose from {} good moves", good_moves.len());
        }
    }

    (best_move, all_root_moves)
}

// Backwards compatibility wrapper
pub fn pick_move(board: &Board, depth: i32, state: &mut SearchState) -> Option<ChessMove> {
    let (best_move, _) = pick_move_timed(board, depth, None, false, state);
    best_move
}