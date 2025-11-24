use chess::{Board, ChessMove, MoveGen, Piece};
use crate::evaluation::piece_value;
use crate::helpers::{mvv_lva_score, HistoryHeuristic, MAX_DEPTH};

#[derive(PartialEq, Clone, Copy)]
enum MoveGenStage {
    TTMove,
    GenerateCaptures,
    GoodCaptures,
    Killers,
    GenerateQuiets,
    Quiets,
    BadCaptures,
    Done,
}

pub struct IncrementalMoveGen {
    stage: MoveGenStage,
    tt_move: Option<ChessMove>,
    moves: Vec<ChessMove>,
    scores: Vec<i32>,
    current_index: usize,
    bad_captures: Vec<(ChessMove, i32)>,
    killer_moves: [Option<ChessMove>; 2],
}

impl IncrementalMoveGen {
    pub fn new(
        board: &Board,
        tt_move: Option<ChessMove>,
        ply: usize,
        _history: &HistoryHeuristic,
        killer_moves: &[[Option<ChessMove>; 2]; MAX_DEPTH],
    ) -> Self {
        let killers = if ply < MAX_DEPTH {
            killer_moves[ply]
        } else {
            [None, None]
        };

        // Validate TT move is legal - OPTIMIZATION: Don't validate here, validate when we try to use it
        let validated_tt_move = tt_move; // We'll check legality when we actually return it

        Self {
            stage: if validated_tt_move.is_some() {
                MoveGenStage::TTMove
            } else {
                MoveGenStage::GenerateCaptures
            },
            tt_move: validated_tt_move,
            moves: Vec::with_capacity(32), // OPTIMIZATION: Pre-allocate common size
            scores: Vec::with_capacity(32),
            current_index: 0,
            bad_captures: Vec::with_capacity(8), // Usually fewer bad captures
            killer_moves: killers,
        }
    }

    pub fn next(&mut self, board: &Board, history: &HistoryHeuristic) -> Option<ChessMove> {
        loop {
            match self.stage {
                MoveGenStage::TTMove => {
                    self.stage = MoveGenStage::GenerateCaptures;
                    if let Some(mv) = self.tt_move {
                        // OPTIMIZATION: Only validate TT move when we actually try to use it
                        if MoveGen::new_legal(board).any(|m| m == mv) {
                            return Some(mv);
                        }
                        // If invalid, fall through to next stage
                    }
                }

                MoveGenStage::GenerateCaptures => {
                    self.moves.clear();
                    self.scores.clear();
                    self.bad_captures.clear();
                    self.current_index = 0;

                    // OPTIMIZATION: Generate only captures first, then filter
                    for mv in MoveGen::new_legal(board) {
                        // OPTIMIZATION: Inline the capture check
                        let dest_piece = board.piece_on(mv.get_dest());
                        let is_capture = dest_piece.is_some() || 
                            (board.piece_on(mv.get_source()) == Some(Piece::Pawn) &&
                             mv.get_source().get_file() != mv.get_dest().get_file() &&
                             dest_piece.is_none());

                        if is_capture {
                            if Some(mv) == self.tt_move {
                                continue; // Skip TT move
                            }

                            let score = mvv_lva_score(board, mv);
                            
                            // OPTIMIZATION: Improved SEE
                            if Self::see_improved(board, mv, dest_piece, 0) {
                                self.moves.push(mv);
                                self.scores.push(score);
                            } else {
                                self.bad_captures.push((mv, score));
                            }
                        }
                    }

                    self.stage = MoveGenStage::GoodCaptures;
                }

                MoveGenStage::GoodCaptures => {
                    if self.current_index < self.moves.len() {
                        // OPTIMIZATION: Selection sort is faster for small lists than full sort
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
                        return Some(mv);
                    } else {
                        self.stage = MoveGenStage::Killers;
                        self.current_index = 0;
                    }
                }

                MoveGenStage::Killers => {
                    while self.current_index < 2 {
                        let killer_idx = self.current_index;
                        self.current_index += 1;

                        if let Some(mv) = self.killer_moves[killer_idx] {
                            if Some(mv) == self.tt_move {
                                continue;
                            }

                            // OPTIMIZATION: Check if it's a capture first (cheaper)
                            let dest_piece = board.piece_on(mv.get_dest());
                            let is_capture = dest_piece.is_some() || 
                                (board.piece_on(mv.get_source()) == Some(Piece::Pawn) &&
                                 mv.get_source().get_file() != mv.get_dest().get_file() &&
                                 dest_piece.is_none());

                            if is_capture {
                                continue;
                            }

                            // OPTIMIZATION: Only check legality for non-captures
                            if MoveGen::new_legal(board).any(|m| m == mv) {
                                return Some(mv);
                            }
                        }
                    }

                    self.stage = MoveGenStage::GenerateQuiets;
                }

                MoveGenStage::GenerateQuiets => {
                    self.moves.clear();
                    self.scores.clear();
                    self.current_index = 0;

                    for mv in MoveGen::new_legal(board) {
                        // OPTIMIZATION: Same inline capture check
                        let dest_piece = board.piece_on(mv.get_dest());
                        let is_capture = dest_piece.is_some() ||
                            (board.piece_on(mv.get_source()) == Some(Piece::Pawn) &&
                             mv.get_source().get_file() != mv.get_dest().get_file() &&
                             dest_piece.is_none());

                        if !is_capture {
                            if Some(mv) == self.tt_move ||
                               Some(mv) == self.killer_moves[0] ||
                               Some(mv) == self.killer_moves[1] {
                                continue;
                            }

                            let score = history.get_score(mv);
                            self.moves.push(mv);
                            self.scores.push(score);
                        }
                    }

                    self.stage = MoveGenStage::Quiets;
                }

                MoveGenStage::Quiets => {
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
                        return Some(mv);
                    } else {
                        self.stage = MoveGenStage::BadCaptures;
                        self.current_index = 0;
                    }
                }

                MoveGenStage::BadCaptures => {
                    if self.current_index < self.bad_captures.len() {
                        let mut best_idx = self.current_index;
                        let mut best_score = self.bad_captures[self.current_index].1;

                        for i in (self.current_index + 1)..self.bad_captures.len() {
                            if self.bad_captures[i].1 > best_score {
                                best_score = self.bad_captures[i].1;
                                best_idx = i;
                            }
                        }

                        self.bad_captures.swap(self.current_index, best_idx);
                        
                        let mv = self.bad_captures[self.current_index].0;
                        self.current_index += 1;
                        return Some(mv);
                    } else {
                        self.stage = MoveGenStage::Done;
                    }
                }

                MoveGenStage::Done => {
                    return None;
                }
            }
        }
    }

    // OPTIMIZED SEE - takes dest_piece as parameter to avoid double lookup
    #[inline]
    fn see_improved(board: &Board, mv: ChessMove, dest_piece: Option<Piece>, threshold: i32) -> bool {
        let from = mv.get_source();
        let to = mv.get_dest();
        
        // Initial capture value
        let capture_value = if let Some(captured) = dest_piece {
            piece_value(captured)
        } else if board.piece_on(from) == Some(Piece::Pawn) &&
                  from.get_file() != to.get_file() {
            100 // En passant
        } else {
            return false; // Not a capture
        };

        // Value of piece making the capture
        let attacker_value = if let Some(piece) = board.piece_on(from) {
            piece_value(piece)
        } else {
            return false;
        };

        // Simplified SEE: Check if we win material even after losing our piece
        let gain = capture_value - attacker_value;
        
        // If we win material or break even, it's good
        gain >= threshold
    }
}

// OPTIMIZED Quiescence move generator
pub struct QuiescenceMoveGen {
    moves: Vec<ChessMove>,
    scores: Vec<i32>,
    current_index: usize,
}

impl QuiescenceMoveGen {
    pub fn new(board: &Board) -> Self {
        let mut moves = Vec::with_capacity(16); // OPTIMIZATION: Pre-allocate
        let mut scores = Vec::with_capacity(16);

        for mv in MoveGen::new_legal(board) {
            // OPTIMIZATION: Inline capture check and use cached dest_piece
            let dest_piece = board.piece_on(mv.get_dest());
            let is_capture = dest_piece.is_some() ||
                (board.piece_on(mv.get_source()) == Some(Piece::Pawn) &&
                 mv.get_source().get_file() != mv.get_dest().get_file() &&
                 dest_piece.is_none());

            if is_capture {
                // OPTIMIZATION: Only generate captures that aren't obviously terrible
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
            // Selection sort for partial ordering
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

    // OPTIMIZATION: Take dest_piece as parameter to avoid double lookup
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