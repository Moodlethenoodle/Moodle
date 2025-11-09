use chess::{Board, ChessMove};
use crate::evaluation::piece_value;

pub const MAX_DEPTH: usize = 32;

pub struct HistoryHeuristic {
    table: [[i32; 64]; 64],
}

impl HistoryHeuristic {
    pub fn new() -> Self {
        Self {
            table: [[0; 64]; 64],
        }
    }

    pub fn update(&mut self, mv: ChessMove, depth: i32, _ply: usize) {
        let bonus = depth * depth;
        let from = mv.get_source().to_index();
        let to = mv.get_dest().to_index();
        self.table[from][to] = (self.table[from][to] * 7 + bonus * 8) / 8;
    }
    
    // NEW: Penalize moves that didn't cause cutoff
    pub fn penalize(&mut self, mv: ChessMove, depth: i32) {
        let penalty = depth * depth;
        let from = mv.get_source().to_index();
        let to = mv.get_dest().to_index();
        self.table[from][to] = (self.table[from][to] * 7 - penalty * 8) / 8;
    }

    pub fn get_score(&self, mv: ChessMove) -> i32 {
        let from = mv.get_source().to_index();
        let to = mv.get_dest().to_index();
        self.table[from][to]
    }

    pub fn clear(&mut self) {
        self.table = [[0; 64]; 64];
    }
}

pub fn mvv_lva_score(board: &Board, mv: ChessMove) -> i32 {
    let mut score = 0;
    
    // Check if this is a capture (including en passant)
    let is_capture = board.piece_on(mv.get_dest()).is_some() || 
                     (board.piece_on(mv.get_source()) == Some(chess::Piece::Pawn) && 
                      mv.get_source().get_file() != mv.get_dest().get_file() &&
                      board.piece_on(mv.get_dest()).is_none());
    
    if is_capture {
        if let Some(victim) = board.piece_on(mv.get_dest()) {
            if let Some(attacker) = board.piece_on(mv.get_source()) {
                score = piece_value(victim) * 10 - piece_value(attacker);
            }
        } else {
            // En passant capture - victim is a pawn
            if let Some(attacker) = board.piece_on(mv.get_source()) {
                score = 100 * 10 - piece_value(attacker); // Pawn value is 100
            }
        }
    }
    
    if let Some(promotion) = mv.get_promotion() {
        score += piece_value(promotion) * 100;
    }
    
    score
}

pub fn move_score(board: &Board, mv: ChessMove) -> i32 {
    mvv_lva_score(board, mv)
}

pub fn capture_exchange_value(board: &Board, mv: ChessMove) -> i32 {
    // Check if this is a capture (including en passant)
    let is_capture = board.piece_on(mv.get_dest()).is_some() || 
                     (board.piece_on(mv.get_source()) == Some(chess::Piece::Pawn) && 
                      mv.get_source().get_file() != mv.get_dest().get_file() &&
                      board.piece_on(mv.get_dest()).is_none());
    
    if !is_capture {
        return 0;
    }
    
    if let Some(victim) = board.piece_on(mv.get_dest()) {
        if let Some(attacker) = board.piece_on(mv.get_source()) {
            return piece_value(victim) - piece_value(attacker);
        }
    } else {
        // En passant capture - victim is a pawn
        if let Some(attacker) = board.piece_on(mv.get_source()) {
            return 100 - piece_value(attacker); // Pawn value is 100
        }
    }
    
    0
}

pub fn order_moves(
    board: &Board, 
    moves: &mut Vec<ChessMove>, 
    ply: usize, 
    history: &HistoryHeuristic,
    killer_moves: &[[Option<ChessMove>; 2]; MAX_DEPTH]
) {
    let mut scored_moves: Vec<(i32, usize, ChessMove)> = moves.iter().enumerate().map(|(i, &mv)| {
        // Check if this is a capture (including en passant)
        let is_capture = board.piece_on(mv.get_dest()).is_some() || 
                         (board.piece_on(mv.get_source()) == Some(chess::Piece::Pawn) && 
                          mv.get_source().get_file() != mv.get_dest().get_file() &&
                          board.piece_on(mv.get_dest()).is_none());
        
        let score = if is_capture {
            mvv_lva_score(board, mv) + 10000
        } else {
            let mut s = history.get_score(mv);
            if ply < MAX_DEPTH {
                if Some(mv) == killer_moves[ply][0] {
                    s += 9000;
                } else if Some(mv) == killer_moves[ply][1] {
                    s += 8000;
                }
            }
            s
        };
        (score, i, mv)
    }).collect();

    scored_moves.sort_by(|a, b| b.0.cmp(&a.0));
    *moves = scored_moves.into_iter().map(|(_, _, mv)| mv).collect();
}