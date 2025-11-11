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

        // Validate TT move is legal
        let validated_tt_move = if let Some(mv) = tt_move {
            if MoveGen::new_legal(board).any(|m| m == mv) {
                Some(mv)
            } else {
                None
            }
        } else {
            None
        };

        Self {
            stage: if validated_tt_move.is_some() {
                MoveGenStage::TTMove
            } else {
                MoveGenStage::GenerateCaptures
            },
            tt_move: validated_tt_move,
            moves: Vec::new(),
            scores: Vec::new(),
            current_index: 0,
            bad_captures: Vec::new(),
            killer_moves: killers,
        }
    }

    pub fn next(&mut self, board: &Board, history: &HistoryHeuristic) -> Option<ChessMove> {
        loop {
            match self.stage {
                MoveGenStage::TTMove => {
                    self.stage = MoveGenStage::GenerateCaptures;
                    if let Some(mv) = self.tt_move {
                        return Some(mv);
                    }
                }

                MoveGenStage::GenerateCaptures => {
                    // Generate all captures and winning/equal captures
                    self.moves.clear();
                    self.scores.clear();
                    self.bad_captures.clear();
                    self.current_index = 0;

                    for mv in MoveGen::new_legal(board) {
                        let is_capture = board.piece_on(mv.get_dest()).is_some() ||
                            (board.piece_on(mv.get_source()) == Some(Piece::Pawn) &&
                             mv.get_source().get_file() != mv.get_dest().get_file() &&
                             board.piece_on(mv.get_dest()).is_none());

                        if is_capture {
                            // Skip TT move if already returned
                            if Some(mv) == self.tt_move {
                                continue;
                            }

                            let score = mvv_lva_score(board, mv);
                            
                            // SEE threshold: only try "good" captures (roughly even or winning)
                            if Self::see(board, mv, 0) {
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
                        // Pick highest scoring capture
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
                            // Check killer is legal and not already tried
                            if Some(mv) == self.tt_move {
                                continue;
                            }

                            let is_capture = board.piece_on(mv.get_dest()).is_some() ||
                                (board.piece_on(mv.get_source()) == Some(Piece::Pawn) &&
                                 mv.get_source().get_file() != mv.get_dest().get_file() &&
                                 board.piece_on(mv.get_dest()).is_none());

                            if is_capture {
                                continue;
                            }

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
                        let is_capture = board.piece_on(mv.get_dest()).is_some() ||
                            (board.piece_on(mv.get_source()) == Some(Piece::Pawn) &&
                             mv.get_source().get_file() != mv.get_dest().get_file() &&
                             board.piece_on(mv.get_dest()).is_none());

                        if !is_capture {
                            // Skip if already tried
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
                        // Pick highest scoring quiet
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
                        // Pick highest scoring bad capture
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

    // Simple Static Exchange Evaluation
    // Returns true if the capture wins material or breaks even
    fn see(board: &Board, mv: ChessMove, threshold: i32) -> bool {
        let from = mv.get_source();
        let to = mv.get_dest();

        let mut swap_list = Vec::new();
        
        // Initial capture value
        if let Some(captured) = board.piece_on(to) {
            swap_list.push(piece_value(captured));
        } else if board.piece_on(from) == Some(Piece::Pawn) &&
                  from.get_file() != to.get_file() {
            // En passant
            swap_list.push(100);
        } else {
            return false; // Not a capture
        }

        // Value of piece making the capture
        let attacker_value = if let Some(piece) = board.piece_on(from) {
            piece_value(piece)
        } else {
            return false;
        };

        swap_list.push(attacker_value);

        // Simplified SEE: just check first exchange
        let gain = swap_list[0] - swap_list[1];
        gain >= threshold
    }

}

// Quiescence move generator (captures only)
pub struct QuiescenceMoveGen {
    moves: Vec<ChessMove>,
    scores: Vec<i32>,
    current_index: usize,
}

impl QuiescenceMoveGen {
    pub fn new(board: &Board) -> Self {
        let mut moves = Vec::new();
        let mut scores = Vec::new();

        for mv in MoveGen::new_legal(board) {
            let is_capture = board.piece_on(mv.get_dest()).is_some() ||
                (board.piece_on(mv.get_source()) == Some(Piece::Pawn) &&
                 mv.get_source().get_file() != mv.get_dest().get_file() &&
                 board.piece_on(mv.get_dest()).is_none());

            if is_capture {
                // Simple SEE filter: skip obviously bad captures
                let see_value = Self::simple_capture_value(board, mv);
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

    pub fn next(&mut self) -> Option<ChessMove> {
        if self.current_index < self.moves.len() {
            // Pick highest scoring capture
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

    fn simple_capture_value(board: &Board, mv: ChessMove) -> i32 {
        if let Some(victim) = board.piece_on(mv.get_dest()) {
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