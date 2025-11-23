use chess::{Board, Color, Piece, Square, ChessMove, File, EMPTY};

// Material values
pub const PAWN_VALUE: i32 = 100;
pub const KNIGHT_VALUE: i32 = 320;
pub const BISHOP_VALUE: i32 = 330;
pub const ROOK_VALUE: i32 = 500;
pub const QUEEN_VALUE: i32 = 900;

// Evaluation weights
const BISHOP_PAIR_BONUS: i32 = 30;
const DOUBLED_PAWN_PENALTY: i32 = 10;
const ISOLATED_PAWN_PENALTY: i32 = 20;
const PASSED_PAWN_BONUS: [i32; 8] = [0, 10, 20, 35, 60, 90, 150, 0];
const ROOK_OPEN_FILE: i32 = 20;
const ROOK_SEMI_OPEN: i32 = 10;
const ROOK_SEVENTH: i32 = 20;
const MOBILITY_KNIGHT: i32 = 4;
const MOBILITY_BISHOP: i32 = 3;
const MOBILITY_ROOK: i32 = 2;
const MOBILITY_QUEEN: i32 = 1;
const PAWN_SHIELD: i32 = 10;

// Piece-square tables (from White's perspective)
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
    -40,-20,  0,  5,  5,  0,-20,-40,
    -30,  5, 10, 15, 15, 10,  5,-30,
    -30,  5, 15, 20, 20, 15,  5,-30,
    -30,  5, 15, 20, 20, 15,  5,-30,
    -30,  5, 10, 15, 15, 10,  5,-30,
    -40,-20,  0,  0,  0,  0,-20,-40,
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
     0,  0,  0,  5,  5,  0,  0,  0,
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

#[inline]
pub fn mirror_square(sq: Square) -> Square {
    unsafe { Square::new((sq.to_index() ^ 56) as u8) }
}

#[inline]
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

#[derive(Clone, Copy, Debug)]
pub struct EvalState {
    pub mg_score: i32,
    pub eg_score: i32,
    pub material: i32,
    pub phase: i32, // Material phase (higher = middlegame)
    pub white_bishops: u8,
    pub black_bishops: u8,
}

impl EvalState {
    pub fn new() -> Self {
        Self {
            mg_score: 0,
            eg_score: 0,
            material: 0,
            phase: 0,
            white_bishops: 0,
            black_bishops: 0,
        }
    }

    pub fn from_board(board: &Board) -> Self {
        let mut state = Self::new();
        
        // Phase values: Knight=1, Bishop=1, Rook=2, Queen=4
        let phase_values = [0, 1, 1, 2, 4, 0];
        
        for sq in *board.combined() {
            let piece = board.piece_on(sq).unwrap();
            let color = board.color_on(sq).unwrap();
            let is_white = color == Color::White;
            
            // Material and phase
            if piece != Piece::King {
                state.material += piece_value(piece);
                state.phase += phase_values[piece.to_index()];
            }
            
            // Bishop counting
            if piece == Piece::Bishop {
                if is_white { state.white_bishops += 1; } else { state.black_bishops += 1; }
            }
            
            // PST evaluation
            let sq_idx = if is_white { sq.to_index() } else { mirror_square(sq).to_index() };
            let (mg_val, eg_val) = match piece {
                Piece::Pawn => (PAWN_VALUE + PAWN_PST[sq_idx], PAWN_VALUE + PAWN_PST[sq_idx]),
                Piece::Knight => (KNIGHT_VALUE + KNIGHT_PST[sq_idx], KNIGHT_VALUE + KNIGHT_PST[sq_idx]),
                Piece::Bishop => (BISHOP_VALUE + BISHOP_PST[sq_idx], BISHOP_VALUE + BISHOP_PST[sq_idx]),
                Piece::Rook => (ROOK_VALUE + ROOK_PST[sq_idx], ROOK_VALUE + ROOK_PST[sq_idx]),
                Piece::Queen => (QUEEN_VALUE + QUEEN_PST[sq_idx], QUEEN_VALUE + QUEEN_PST[sq_idx]),
                Piece::King => (KING_MG_PST[sq_idx], KING_EG_PST[sq_idx]),
            };
            
            if is_white {
                state.mg_score += mg_val;
                state.eg_score += eg_val;
            } else {
                state.mg_score -= mg_val;
                state.eg_score -= eg_val;
            }
        }
        
        state
    }

    // Add the missing get_score method
    pub fn get_score(&self, side_to_move: Color) -> i32 {
        let phase = self.phase.min(24);
        let mg_weight = phase;
        let eg_weight = 24 - phase;
        let score = (self.mg_score * mg_weight + self.eg_score * eg_weight) / 24;
        
        if side_to_move == Color::White {
            score
        } else {
            -score
        }
    }

    pub fn apply_move(&mut self, board: &Board, mv: ChessMove, color: Color) {
        let from = mv.get_source();
        let to = mv.get_dest();
        let piece = board.piece_on(from).unwrap();
        
        // Handle captures
        if let Some(captured) = board.piece_on(to) {
            self.remove_piece(captured, !color, to);
        } else if piece == Piece::Pawn && from.get_file() != to.get_file() {
            // En passant
            let ep_sq = Square::make_square(from.get_rank(), to.get_file());
            self.remove_piece(Piece::Pawn, !color, ep_sq);
        }
        
        // Handle castling
        if piece == Piece::King && from.get_file().to_index().abs_diff(to.get_file().to_index()) == 2 {
            let (rook_from, rook_to) = if to > from {
                (Square::make_square(from.get_rank(), File::H), Square::make_square(from.get_rank(), File::F))
            } else {
                (Square::make_square(from.get_rank(), File::A), Square::make_square(from.get_rank(), File::D))
            };
            self.remove_piece(Piece::Rook, color, rook_from);
            self.add_piece(Piece::Rook, color, rook_to);
        }
        
        // Move piece
        self.remove_piece(piece, color, from);
        if let Some(promotion) = mv.get_promotion() {
            self.add_piece(promotion, color, to);
        } else {
            self.add_piece(piece, color, to);
        }
    }

    #[inline]
    fn add_piece(&mut self, piece: Piece, color: Color, sq: Square) {
        let is_white = color == Color::White;
        let sq_idx = if is_white { sq.to_index() } else { mirror_square(sq).to_index() };
        
        if piece != Piece::King {
            self.material += piece_value(piece);
            self.phase += [0, 1, 1, 2, 4, 0][piece.to_index()];
        }
        
        if piece == Piece::Bishop {
            if is_white { self.white_bishops += 1; } else { self.black_bishops += 1; }
        }
        
        let (mg_val, eg_val) = match piece {
            Piece::Pawn => (PAWN_VALUE + PAWN_PST[sq_idx], PAWN_VALUE + PAWN_PST[sq_idx]),
            Piece::Knight => (KNIGHT_VALUE + KNIGHT_PST[sq_idx], KNIGHT_VALUE + KNIGHT_PST[sq_idx]),
            Piece::Bishop => (BISHOP_VALUE + BISHOP_PST[sq_idx], BISHOP_VALUE + BISHOP_PST[sq_idx]),
            Piece::Rook => (ROOK_VALUE + ROOK_PST[sq_idx], ROOK_VALUE + ROOK_PST[sq_idx]),
            Piece::Queen => (QUEEN_VALUE + QUEEN_PST[sq_idx], QUEEN_VALUE + QUEEN_PST[sq_idx]),
            Piece::King => (KING_MG_PST[sq_idx], KING_EG_PST[sq_idx]),
        };
        
        if is_white {
            self.mg_score += mg_val;
            self.eg_score += eg_val;
        } else {
            self.mg_score -= mg_val;
            self.eg_score -= eg_val;
        }
    }

    #[inline]
    fn remove_piece(&mut self, piece: Piece, color: Color, sq: Square) {
        let is_white = color == Color::White;
        let sq_idx = if is_white { sq.to_index() } else { mirror_square(sq).to_index() };
        
        if piece != Piece::King {
            self.material -= piece_value(piece);
            self.phase -= [0, 1, 1, 2, 4, 0][piece.to_index()];
        }
        
        if piece == Piece::Bishop {
            if is_white { self.white_bishops -= 1; } else { self.black_bishops -= 1; }
        }
        
        let (mg_val, eg_val) = match piece {
            Piece::Pawn => (PAWN_VALUE + PAWN_PST[sq_idx], PAWN_VALUE + PAWN_PST[sq_idx]),
            Piece::Knight => (KNIGHT_VALUE + KNIGHT_PST[sq_idx], KNIGHT_VALUE + KNIGHT_PST[sq_idx]),
            Piece::Bishop => (BISHOP_VALUE + BISHOP_PST[sq_idx], BISHOP_VALUE + BISHOP_PST[sq_idx]),
            Piece::Rook => (ROOK_VALUE + ROOK_PST[sq_idx], ROOK_VALUE + ROOK_PST[sq_idx]),
            Piece::Queen => (QUEEN_VALUE + QUEEN_PST[sq_idx], QUEEN_VALUE + QUEEN_PST[sq_idx]),
            Piece::King => (KING_MG_PST[sq_idx], KING_EG_PST[sq_idx]),
        };
        
        if is_white {
            self.mg_score -= mg_val;
            self.eg_score -= eg_val;
        } else {
            self.mg_score += mg_val;
            self.eg_score += eg_val;
        }
    }
}

// Fast pawn evaluation
fn eval_pawns(board: &Board) -> i32 {
    let white_pawns = board.pieces(Piece::Pawn) & board.color_combined(Color::White);
    let black_pawns = board.pieces(Piece::Pawn) & board.color_combined(Color::Black);
    
    let mut score = 0;
    
    // File masks for doubled/isolated detection
    let mut white_files = [0u8; 8];
    let mut black_files = [0u8; 8];
    
    for sq in white_pawns {
        white_files[sq.get_file().to_index()] += 1;
    }
    for sq in black_pawns {
        black_files[sq.get_file().to_index()] += 1;
    }
    
    // Evaluate white pawns
    for sq in white_pawns {
        let file = sq.get_file().to_index();
        let rank = sq.get_rank().to_index();
        
        // Doubled pawns
        if white_files[file] > 1 {
            score -= DOUBLED_PAWN_PENALTY;
        }
        
        // Isolated pawns
        let has_neighbor = (file > 0 && white_files[file - 1] > 0) || (file < 7 && white_files[file + 1] > 0);
        if !has_neighbor {
            score -= ISOLATED_PAWN_PENALTY;
        }
        
        // Passed pawns
        let mut is_passed = true;
        for enemy_sq in black_pawns {
            let e_file = enemy_sq.get_file().to_index();
            let e_rank = enemy_sq.get_rank().to_index();
            if e_file.abs_diff(file) <= 1 && e_rank > rank {
                is_passed = false;
                break;
            }
        }
        if is_passed {
            score += PASSED_PAWN_BONUS[rank];
        }
    }
    
    // Evaluate black pawns
    for sq in black_pawns {
        let file = sq.get_file().to_index();
        let rank = sq.get_rank().to_index();
        
        if black_files[file] > 1 {
            score += DOUBLED_PAWN_PENALTY;
        }
        
        let has_neighbor = (file > 0 && black_files[file - 1] > 0) || (file < 7 && black_files[file + 1] > 0);
        if !has_neighbor {
            score += ISOLATED_PAWN_PENALTY;
        }
        
        let mut is_passed = true;
        for enemy_sq in white_pawns {
            let e_file = enemy_sq.get_file().to_index();
            let e_rank = enemy_sq.get_rank().to_index();
            if e_file.abs_diff(file) <= 1 && e_rank < rank {
                is_passed = false;
                break;
            }
        }
        if is_passed {
            score -= PASSED_PAWN_BONUS[7 - rank];
        }
    }
    
    score
}

// Fast rook evaluation
fn eval_rooks(board: &Board) -> i32 {
    let mut score = 0;
    
    let white_rooks = board.pieces(Piece::Rook) & board.color_combined(Color::White);
    let black_rooks = board.pieces(Piece::Rook) & board.color_combined(Color::Black);
    let white_pawns = board.pieces(Piece::Pawn) & board.color_combined(Color::White);
    let black_pawns = board.pieces(Piece::Pawn) & board.color_combined(Color::Black);
    
    for sq in white_rooks {
        let file = sq.get_file();
        let rank = sq.get_rank().to_index();
        
        // Check file occupation
        let mut has_white = false;
        let mut has_black = false;
        for pawn_sq in *board.pieces(Piece::Pawn) {
            if pawn_sq.get_file() == file {
                if (white_pawns.0 & (1u64 << pawn_sq.to_index())) != 0 { has_white = true; }
                if (black_pawns.0 & (1u64 << pawn_sq.to_index())) != 0 { has_black = true; }
            }
        }
        
        if !has_white && !has_black { score += ROOK_OPEN_FILE; }
        else if !has_white { score += ROOK_SEMI_OPEN; }
        
        if rank == 6 { score += ROOK_SEVENTH; }
    }
    
    for sq in black_rooks {
        let file = sq.get_file();
        let rank = sq.get_rank().to_index();
        
        let mut has_white = false;
        let mut has_black = false;
        for pawn_sq in *board.pieces(Piece::Pawn) {
            if pawn_sq.get_file() == file {
                if (white_pawns.0 & (1u64 << pawn_sq.to_index())) != 0 { has_white = true; }
                if (black_pawns.0 & (1u64 << pawn_sq.to_index())) != 0 { has_black = true; }
            }
        }
        
        if !has_white && !has_black { score -= ROOK_OPEN_FILE; }
        else if !has_black { score -= ROOK_SEMI_OPEN; }
        
        if rank == 1 { score -= ROOK_SEVENTH; }
    }
    
    score
}

// Fast mobility evaluation
fn eval_mobility(board: &Board) -> i32 {
    let mut score = 0;
    
    for sq in *board.color_combined(Color::White) {
        if let Some(piece) = board.piece_on(sq) {
            let mob = match piece {
                Piece::Knight => (chess::get_knight_moves(sq) & !board.color_combined(Color::White)).popcnt() as i32 * MOBILITY_KNIGHT,
                Piece::Bishop => (chess::get_bishop_moves(sq, *board.combined()) & !board.color_combined(Color::White)).popcnt() as i32 * MOBILITY_BISHOP,
                Piece::Rook => (chess::get_rook_moves(sq, *board.combined()) & !board.color_combined(Color::White)).popcnt() as i32 * MOBILITY_ROOK,
                Piece::Queen => ((chess::get_bishop_moves(sq, *board.combined()) | chess::get_rook_moves(sq, *board.combined())) & !board.color_combined(Color::White)).popcnt() as i32 * MOBILITY_QUEEN,
                _ => 0,
            };
            score += mob;
        }
    }
    
    for sq in *board.color_combined(Color::Black) {
        if let Some(piece) = board.piece_on(sq) {
            let mob = match piece {
                Piece::Knight => (chess::get_knight_moves(sq) & !board.color_combined(Color::Black)).popcnt() as i32 * MOBILITY_KNIGHT,
                Piece::Bishop => (chess::get_bishop_moves(sq, *board.combined()) & !board.color_combined(Color::Black)).popcnt() as i32 * MOBILITY_BISHOP,
                Piece::Rook => (chess::get_rook_moves(sq, *board.combined()) & !board.color_combined(Color::Black)).popcnt() as i32 * MOBILITY_ROOK,
                Piece::Queen => ((chess::get_bishop_moves(sq, *board.combined()) | chess::get_rook_moves(sq, *board.combined())) & !board.color_combined(Color::Black)).popcnt() as i32 * MOBILITY_QUEEN,
                _ => 0,
            };
            score -= mob;
        }
    }
    
    score
}

// Fast king safety (middlegame only)
fn eval_king_safety(board: &Board) -> i32 {
    let mut score = 0;
    
    let white_king = (board.pieces(Piece::King) & board.color_combined(Color::White)).to_square();
    let black_king = (board.pieces(Piece::King) & board.color_combined(Color::Black)).to_square();
    let white_pawns = board.pieces(Piece::Pawn) & board.color_combined(Color::White);
    let black_pawns = board.pieces(Piece::Pawn) & board.color_combined(Color::Black);
    
    // White king shield
    let wk_file = white_king.get_file().to_index();
    let wk_rank = white_king.get_rank().to_index();
    for pawn_sq in white_pawns {
        let p_file = pawn_sq.get_file().to_index();
        let p_rank = pawn_sq.get_rank().to_index();
        if p_file.abs_diff(wk_file) <= 1 && p_rank > wk_rank && p_rank <= wk_rank + 2 {
            score += PAWN_SHIELD;
        }
    }
    
    // Black king shield
    let bk_file = black_king.get_file().to_index();
    let bk_rank = black_king.get_rank().to_index();
    for pawn_sq in black_pawns {
        let p_file = pawn_sq.get_file().to_index();
        let p_rank = pawn_sq.get_rank().to_index();
        if p_file.abs_diff(bk_file) <= 1 && p_rank < bk_rank && p_rank >= bk_rank.saturating_sub(2) {
            score -= PAWN_SHIELD;
        }
    }
    
    score
}

pub fn evaluate_board_incremental(board: &Board, eval_state: &EvalState, ply_from_root: i32) -> i32 {
    let status = board.status();
    
    if status == chess::BoardStatus::Checkmate {
        return -99999 + ply_from_root;
    }
    if status == chess::BoardStatus::Stalemate {
        return 0;
    }
    
    // Tapered eval between middlegame and endgame
    let phase = eval_state.phase.min(24); // Cap at 24 (starting phase)
    let mg_weight = phase;
    let eg_weight = 24 - phase;
    let mut score = (eval_state.mg_score * mg_weight + eval_state.eg_score * eg_weight) / 24;
    
    // Bishop pair bonus
    if eval_state.white_bishops >= 2 { score += BISHOP_PAIR_BONUS; }
    if eval_state.black_bishops >= 2 { score -= BISHOP_PAIR_BONUS; }
    
    // Positional evaluation (computed fresh each time)
    score += eval_pawns(board);
    score += eval_rooks(board);
    score += eval_mobility(board);
    
    // King safety only in middlegame
    if phase > 12 {
        score += eval_king_safety(board);
    }
    
    // Return from side to move perspective
    if board.side_to_move() == Color::White { score } else { -score }
}