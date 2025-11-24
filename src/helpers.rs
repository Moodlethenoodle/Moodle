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