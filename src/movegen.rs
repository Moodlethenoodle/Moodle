use chess::{Board, ChessMove, MoveGen, Piece};
use crate::evaluation::piece_value;
use crate::helpers::{mvv_lva_score};

pub struct QuiescenceMoveGen {
    moves: Vec<ChessMove>,
    scores: Vec<i32>,
    current_index: usize,
}

impl QuiescenceMoveGen {
    pub fn new(board: &Board) -> Self {
        let mut moves = Vec::with_capacity(16);
        let mut scores = Vec::with_capacity(16);

        for mv in MoveGen::new_legal(board) {
            let dest_piece = board.piece_on(mv.get_dest());
            let is_capture = dest_piece.is_some() ||
                (board.piece_on(mv.get_source()) == Some(Piece::Pawn) &&
                 mv.get_source().get_file() != mv.get_dest().get_file() &&
                 dest_piece.is_none());

            if is_capture {
                let see_value = Self::simple_capture_value(board, mv, dest_piece);
                if see_value >= -100 {
                    let score = mvv_lva_score(board, mv);
                    moves.push(mv);
                    scores.push(score);
                }
            }
        }

        Self {
            moves,
            scores,
            current_index: 0,
        }
    }

    #[inline]
    pub fn next(&mut self) -> Option<ChessMove> {
        if self.current_index < self.moves.len() {
            let mut best_idx = self.current_index;
            let mut best_score = self.scores[self.current_index];

            for i in (self.current_index + 1)..self.moves.len() {
                if self.scores[i] > best_score {
                    best_score = self.scores[i];
                    best_idx = i;
                }
            }

            self.moves.swap(self.current_index, best_idx);
            self.scores.swap(self.current_index, best_idx);
            
            let mv = self.moves[self.current_index];
            self.current_index += 1;
            Some(mv)
        } else {
            None
        }
    }

    #[inline]
    fn simple_capture_value(board: &Board, mv: ChessMove, dest_piece: Option<Piece>) -> i32 {
        if let Some(victim) = dest_piece {
            if let Some(attacker) = board.piece_on(mv.get_source()) {
                return piece_value(victim) - piece_value(attacker);
            }
        } else if board.piece_on(mv.get_source()) == Some(Piece::Pawn) &&
                  mv.get_source().get_file() != mv.get_dest().get_file() {
            // En passant
            if let Some(attacker) = board.piece_on(mv.get_source()) {
                return 100 - piece_value(attacker);
            }
        }
        0
    }
}