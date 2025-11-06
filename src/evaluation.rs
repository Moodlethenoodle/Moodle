use chess::{Board, Color, Piece, Square, ChessMove};

pub const PAWN_VALUE: i32 = 100;
pub const KNIGHT_VALUE: i32 = 300;
pub const BISHOP_VALUE: i32 = 300;
pub const ROOK_VALUE: i32 = 500;
pub const QUEEN_VALUE: i32 = 900;

pub const PAWN_PST: [i32; 64] = [
     0,  0,  0,  0,  0,  0,  0,  0,
     5, 10, 10,-20,-20, 10, 10,  5,
     5, -5,-10,  0,  0,-10, -5,  5,
     0,  0,  0, 20, 20,  0,  0,  0,
     5,  5, 10, 25, 25, 10,  5,  5,
    10, 10, 20, 30, 30, 20, 10, 10,
    50, 50, 50, 50, 50, 50, 50, 50,
     0,  0,  0,  0,  0,  0,  0,  0
];

pub const KNIGHT_PST: [i32; 64] = [
    -50,-40,-30,-30,-30,-30,-40,-50,
    -40,-20,  0,  0,  0,  0,-20,-40,
    -30,  0, 10, 15, 15, 10,  0,-30,
    -30,  5, 15, 20, 20, 15,  5,-30,
    -30,  0, 15, 20, 20, 15,  0,-30,
    -30,  5, 10, 15, 15, 10,  5,-30,
    -40,-20,  0,  5,  5,  0,-20,-40,
    -50,-40,-30,-30,-30,-30,-40,-50
];

pub const BISHOP_PST: [i32; 64] = [
    -20,-10,-10,-10,-10,-10,-10,-20,
    -10,  5,  0,  0,  0,  0,  5,-10,
    -10, 10, 10, 10, 10, 10, 10,-10,
    -10,  0, 10, 10, 10, 10,  0,-10,
    -10,  5,  5, 10, 10,  5,  5,-10,
    -10,  0,  5, 10, 10,  5,  0,-10,
    -10,  0,  0,  0,  0,  0,  0,-10,
    -20,-10,-10,-10,-10,-10,-10,-20
];

pub const ROOK_PST: [i32; 64] = [
     0,  0,  0,  0,  0,  0,  0,  0,
     5, 10, 10, 10, 10, 10, 10,  5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
     0,  0,  0,  5,  5,  0,  0,  0
];

pub const QUEEN_PST: [i32; 64] = [
    -20,-10,-10, -5, -5,-10,-10,-20,
    -10,  0,  0,  0,  0,  0,  0,-10,
    -10,  0,  5,  5,  5,  5,  0,-10,
     -5,  0,  5,  5,  5,  5,  0, -5,
      0,  0,  5,  5,  5,  5,  0, -5,
    -10,  5,  5,  5,  5,  5,  0,-10,
    -10,  0,  5,  0,  0,  0,  0,-10,
    -20,-10,-10, -5, -5,-10,-10,-20
];

pub const KING_MG_PST: [i32; 64] = [
    20, 30, 10,  0,  0, 10, 30, 20,
    20, 20,  0,  0,  0,  0, 20, 20,
   -10,-20,-20,-20,-20,-20,-20,-10,
   -20,-30,-30,-40,-40,-30,-30,-20,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30
];

pub const KING_EG_PST: [i32; 64] = [
   -50,-30,-30,-30,-30,-30,-30,-50,
   -30,-30,  0,  0,  0,  0,-30,-30,
   -30,-10, 20, 30, 30, 20,-10,-30,
   -30,-10, 30, 40, 40, 30,-10,-30,
   -30,-10, 30, 40, 40, 30,-10,-30,
   -30,-10, 20, 30, 30, 20,-10,-30,
   -30,-20,-10,  0,  0,-10,-20,-30,
   -50,-40,-30,-20,-20,-30,-40,-50
];

pub fn mirror_square(sq: Square) -> Square {
    let rank = sq.get_rank().to_index() ^ 7;
    let file = sq.get_file().to_index();
    Square::make_square(chess::Rank::from_index(rank), chess::File::from_index(file))
}

pub fn piece_value(piece: Piece) -> i32 {
    match piece {
        Piece::Pawn => PAWN_VALUE,
        Piece::Knight => KNIGHT_VALUE,
        Piece::Bishop => BISHOP_VALUE,
        Piece::Rook => ROOK_VALUE,
        Piece::Queen => QUEEN_VALUE,
        Piece::King => 0,
    }
}

// Incremental evaluation structure
#[derive(Clone, Copy, Debug)]
pub struct EvalState {
    pub mg_score: i32,  // Middlegame score
    pub eg_score: i32,  // Endgame score
    pub total_material: i32,  // Total material on board (both sides)
}

impl EvalState {
    pub fn new() -> Self {
        Self {
            mg_score: 0,
            eg_score: 0,
            total_material: 0,
        }
    }

    // Initialize from scratch for a board
    pub fn from_board(board: &Board) -> Self {
        let mut state = Self::new();
        
        // Calculate total material first
        for sq in *board.combined() {
            if let Some(piece) = board.piece_on(sq) {
                if piece != Piece::King {
                    state.total_material += piece_value(piece);
                }
            }
        }

        // Now evaluate all pieces
        for sq in *board.combined() {
            if let Some(piece) = board.piece_on(sq) {
                let color = board.color_on(sq).unwrap();
                state.add_piece(piece, color, sq);
            }
        }

        state
    }

    // Add a piece to the evaluation
    fn add_piece(&mut self, piece: Piece, color: Color, sq: Square) {
        let sq_idx = sq.to_index();
        let mirrored_idx = mirror_square(sq).to_index();

        let (base_value, mg_pst, eg_pst) = match piece {
            Piece::Pawn => (PAWN_VALUE, &PAWN_PST, &PAWN_PST),
            Piece::Knight => (KNIGHT_VALUE, &KNIGHT_PST, &KNIGHT_PST),
            Piece::Bishop => (BISHOP_VALUE, &BISHOP_PST, &BISHOP_PST),
            Piece::Rook => (ROOK_VALUE, &ROOK_PST, &ROOK_PST),
            Piece::Queen => (QUEEN_VALUE, &QUEEN_PST, &QUEEN_PST),
            Piece::King => (0, &KING_MG_PST, &KING_EG_PST),
        };

        if color == Color::White {
            self.mg_score += base_value + mg_pst[sq_idx];
            self.eg_score += base_value + eg_pst[sq_idx];
        } else {
            self.mg_score -= base_value + mg_pst[mirrored_idx];
            self.eg_score -= base_value + eg_pst[mirrored_idx];
        }
    }

    // Remove a piece from the evaluation
    fn remove_piece(&mut self, piece: Piece, color: Color, sq: Square) {
        let sq_idx = sq.to_index();
        let mirrored_idx = mirror_square(sq).to_index();

        let (base_value, mg_pst, eg_pst) = match piece {
            Piece::Pawn => (PAWN_VALUE, &PAWN_PST, &PAWN_PST),
            Piece::Knight => (KNIGHT_VALUE, &KNIGHT_PST, &KNIGHT_PST),
            Piece::Bishop => (BISHOP_VALUE, &BISHOP_PST, &BISHOP_PST),
            Piece::Rook => (ROOK_VALUE, &ROOK_PST, &ROOK_PST),
            Piece::Queen => (QUEEN_VALUE, &QUEEN_PST, &QUEEN_PST),
            Piece::King => (0, &KING_MG_PST, &KING_EG_PST),
        };

        if color == Color::White {
            self.mg_score -= base_value + mg_pst[sq_idx];
            self.eg_score -= base_value + eg_pst[sq_idx];
        } else {
            self.mg_score += base_value + mg_pst[mirrored_idx];
            self.eg_score += base_value + eg_pst[mirrored_idx];
        }

        // Update material count
        if piece != Piece::King {
            self.total_material -= piece_value(piece);
        }
    }

    // Apply a move incrementally
    pub fn apply_move(&mut self, board: &Board, mv: ChessMove, moving_color: Color) {
        let from = mv.get_source();
        let to = mv.get_dest();
        let piece = board.piece_on(from).unwrap();

        // Handle capture (including en passant)
        if let Some(captured) = board.piece_on(to) {
            self.remove_piece(captured, !moving_color, to);
        } else if piece == Piece::Pawn && from.get_file() != to.get_file() {
            // En passant
            let capture_sq = Square::make_square(from.get_rank(), to.get_file());
            self.remove_piece(Piece::Pawn, !moving_color, capture_sq);
        }

        // Handle castling - move the rook
        if piece == Piece::King && from.get_file().to_index().abs_diff(to.get_file().to_index()) == 2 {
            let (rook_from, rook_to) = if to.get_file().to_index() > from.get_file().to_index() {
                // Kingside
                (Square::make_square(from.get_rank(), chess::File::H),
                 Square::make_square(from.get_rank(), chess::File::F))
            } else {
                // Queenside
                (Square::make_square(from.get_rank(), chess::File::A),
                 Square::make_square(from.get_rank(), chess::File::D))
            };
            self.remove_piece(Piece::Rook, moving_color, rook_from);
            self.add_piece(Piece::Rook, moving_color, rook_to);
        }

        // Move the piece
        self.remove_piece(piece, moving_color, from);
        
        // Handle promotion
        if let Some(promotion) = mv.get_promotion() {
            self.add_piece(promotion, moving_color, to);
        } else {
            self.add_piece(piece, moving_color, to);
        }
    }

    // Get final score with phase interpolation
    pub fn get_score(&self, side_to_move: Color) -> i32 {
        // Endgame when total material < 2000
        let is_endgame = self.total_material < 2000;
        
        let score = if is_endgame {
            self.eg_score
        } else {
            // Could do phase interpolation here for smoother transition
            // For now, just use mg_score
            self.mg_score
        };

        if side_to_move == Color::White {
            score
        } else {
            -score
        }
    }
}

// Incremental evaluation
pub fn evaluate_board_incremental(board: &Board, eval_state: &EvalState, ply_from_root: i32) -> i32 {
    let status = board.status();
    
    if status == chess::BoardStatus::Checkmate {
        return -99999 + ply_from_root;
    }
    if status == chess::BoardStatus::Stalemate {
        return 0;
    }

    eval_state.get_score(board.side_to_move())
}