use chess::{Board, Color, Piece, Square, ChessMove, BitBoard};

pub const PAWN_VALUE: i32 = 100;
pub const KNIGHT_VALUE: i32 = 300;
pub const BISHOP_VALUE: i32 = 300;
pub const ROOK_VALUE: i32 = 500;
pub const QUEEN_VALUE: i32 = 900;

// Pawn structure penalties/bonuses
const DOUBLED_PAWN_PENALTY: i32 = 10;
const ISOLATED_PAWN_PENALTY: i32 = 15;
const BACKWARD_PAWN_PENALTY: i32 = 8;
const PASSED_PAWN_BONUS: [i32; 8] = [0, 5, 10, 20, 35, 60, 100, 0]; // by rank
const CONNECTED_PAWN_BONUS: i32 = 5;

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

// Pawn structure cache - stores precomputed pawn evaluations
#[derive(Clone, Copy, Debug)]
pub struct PawnStructure {
    pub score: i32,  // Total pawn structure score (white perspective)
}

impl PawnStructure {
    pub fn new() -> Self {
        Self { score: 0 }
    }

    // Evaluate pawn structure for both sides
    pub fn from_board(board: &Board) -> Self {
        let mut structure = Self::new();
        
        let white_pawns = board.pieces(Piece::Pawn) & board.color_combined(Color::White);
        let black_pawns = board.pieces(Piece::Pawn) & board.color_combined(Color::Black);
        
        structure.score += Self::evaluate_pawns(white_pawns, black_pawns, Color::White);
        structure.score -= Self::evaluate_pawns(black_pawns, white_pawns, Color::Black);
        
        structure
    }

    fn evaluate_pawns(our_pawns: BitBoard, enemy_pawns: BitBoard, color: Color) -> i32 {
        let mut score = 0;
        
        // Build file masks for each file
        let mut file_masks = [0u8; 8];
        for sq in our_pawns {
            file_masks[sq.get_file().to_index()] += 1;
        }
        
        // Evaluate each pawn
        for sq in our_pawns {
            let file = sq.get_file().to_index();
            let rank = sq.get_rank().to_index();
            let actual_rank = if color == Color::White { rank } else { 7 - rank };
            
            // Doubled pawns
            if file_masks[file] > 1 {
                score -= DOUBLED_PAWN_PENALTY;
            }
            
            // Isolated pawns (no friendly pawns on adjacent files)
            let has_neighbor = (file > 0 && file_masks[file - 1] > 0) || 
                              (file < 7 && file_masks[file + 1] > 0);
            if !has_neighbor {
                score -= ISOLATED_PAWN_PENALTY;
            } else {
                // Connected pawns (friendly pawn on adjacent file, same or one rank behind)
                let connected = Self::is_connected(sq, our_pawns, color);
                if connected {
                    score += CONNECTED_PAWN_BONUS;
                }
            }
            
            // Passed pawns (no enemy pawns blocking or attacking)
            if Self::is_passed(sq, enemy_pawns, color) {
                score += PASSED_PAWN_BONUS[actual_rank];
            }
            
            // Backward pawns (behind all friendly pawns on adjacent files and can't advance safely)
            if has_neighbor && Self::is_backward(sq, our_pawns, enemy_pawns, color) {
                score -= BACKWARD_PAWN_PENALTY;
            }
        }
        
        score
    }

    fn is_connected(sq: Square, our_pawns: BitBoard, color: Color) -> bool {
        let file = sq.get_file().to_index();
        let rank = sq.get_rank().to_index();
        
        for check_sq in our_pawns {
            let check_file = check_sq.get_file().to_index();
            let check_rank = check_sq.get_rank().to_index();
            
            // Check adjacent files
            if check_file.abs_diff(file) == 1 {
                // Same rank or one rank behind
                if color == Color::White {
                    if check_rank == rank || check_rank == rank - 1 {
                        return true;
                    }
                } else {
                    if check_rank == rank || check_rank == rank + 1 {
                        return true;
                    }
                }
            }
        }
        false
    }

    fn is_passed(sq: Square, enemy_pawns: BitBoard, color: Color) -> bool {
        let file = sq.get_file().to_index();
        let rank = sq.get_rank().to_index();
        
        for enemy_sq in enemy_pawns {
            let enemy_file = enemy_sq.get_file().to_index();
            let enemy_rank = enemy_sq.get_rank().to_index();
            
            // Check if enemy pawn is on same or adjacent file
            if enemy_file.abs_diff(file) <= 1 {
                // Check if enemy pawn is ahead
                if color == Color::White && enemy_rank > rank {
                    return false;
                } else if color == Color::Black && enemy_rank < rank {
                    return false;
                }
            }
        }
        true
    }

    fn is_backward(sq: Square, our_pawns: BitBoard, enemy_pawns: BitBoard, color: Color) -> bool {
        let file = sq.get_file().to_index();
        let rank = sq.get_rank().to_index();
        
        // Check if this pawn is behind all friendly pawns on adjacent files
        let mut most_backward_neighbor_rank = if color == Color::White { 0 } else { 7 };
        let mut has_neighbor = false;
        
        for check_sq in our_pawns {
            let check_file = check_sq.get_file().to_index();
            let check_rank = check_sq.get_rank().to_index();
            
            if check_file.abs_diff(file) == 1 {
                has_neighbor = true;
                if color == Color::White {
                    most_backward_neighbor_rank = most_backward_neighbor_rank.max(check_rank);
                } else {
                    most_backward_neighbor_rank = most_backward_neighbor_rank.min(check_rank);
                }
            }
        }
        
        if !has_neighbor {
            return false;
        }
        
        // If behind neighbors, check if advance square is attacked
        let is_behind = if color == Color::White {
            rank < most_backward_neighbor_rank
        } else {
            rank > most_backward_neighbor_rank
        };
        
        if !is_behind {
            return false;
        }
        
        // Check if advance square is attacked by enemy pawn
        let advance_rank = if color == Color::White { rank + 1 } else { rank - 1 };
        if advance_rank > 7 {
            return false;
        }
        
        for enemy_sq in enemy_pawns {
            let enemy_file = enemy_sq.get_file().to_index();
            let enemy_rank = enemy_sq.get_rank().to_index();
            
            if enemy_file.abs_diff(file) == 1 {
                if color == Color::White && enemy_rank == advance_rank + 1 {
                    return true;
                } else if color == Color::Black && enemy_rank == advance_rank - 1 {
                    return true;
                }
            }
        }
        
        false
    }
}

// Incremental evaluation structure
#[derive(Clone, Copy, Debug)]
pub struct EvalState {
    pub mg_score: i32,  // Middlegame score
    pub eg_score: i32,  // Endgame score
    pub total_material: i32,  // Total material on board (both sides)
    pub pawn_structure: PawnStructure,  // Cached pawn structure evaluation
}

impl EvalState {
    pub fn new() -> Self {
        Self {
            mg_score: 0,
            eg_score: 0,
            total_material: 0,
            pawn_structure: PawnStructure::new(),
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

        // Evaluate pawn structure
        state.pawn_structure = PawnStructure::from_board(board);

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
        
        let mut pawn_structure_changed = false;

        // Handle capture (including en passant)
        if let Some(captured) = board.piece_on(to) {
            self.remove_piece(captured, !moving_color, to);
            if captured == Piece::Pawn {
                pawn_structure_changed = true;
            }
        } else if piece == Piece::Pawn && from.get_file() != to.get_file() {
            // En passant
            let capture_sq = Square::make_square(from.get_rank(), to.get_file());
            self.remove_piece(Piece::Pawn, !moving_color, capture_sq);
            pawn_structure_changed = true;
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
        
        if piece == Piece::Pawn {
            pawn_structure_changed = true;
        }
        
        // Handle promotion
        if let Some(promotion) = mv.get_promotion() {
            self.add_piece(promotion, moving_color, to);
        } else {
            self.add_piece(piece, moving_color, to);
        }
        
        // Recalculate pawn structure if it changed
        if pawn_structure_changed {
            let new_board = board.make_move_new(mv);
            self.pawn_structure = PawnStructure::from_board(&new_board);
        }
    }

    // Get final score with phase interpolation
    pub fn get_score(&self, side_to_move: Color) -> i32 {
        // Endgame when total material < 2000
        let is_endgame = self.total_material < 2000;
        
        let mut score = if is_endgame {
            self.eg_score
        } else {
            self.mg_score
        };
        
        // Add pawn structure evaluation
        score += self.pawn_structure.score;

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