use chess::{Board, Color, Piece, Square, ChessMove, File, Rank, BitBoard};

// Material values

pub const PAWN_VALUE: i32 = 100;
pub const KNIGHT_VALUE: i32 = 320;
pub const BISHOP_VALUE: i32 = 330;
pub const ROOK_VALUE: i32 = 500;
pub const QUEEN_VALUE: i32 = 900;

const BISHOP_PAIR_BONUS: i32 = 50;
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
const CONNECTED_ROOKS: i32 = 15;
const KING_ATTACK_WEIGHT: i32 = 5;
const WEAK_KING_SQUARE: i32 = 15;

// OPTIMIZED: Bitboard masks for fast pawn evaluation
const FILE_MASKS: [u64; 8] = [
    0x0101010101010101, // A file
    0x0202020202020202, // B file
    0x0404040404040404, // C file
    0x0808080808080808, // D file
    0x1010101010101010, // E file
    0x2020202020202020, // F file
    0x4040404040404040, // G file
    0x8080808080808080, // H file
];

const ADJACENT_FILES_MASKS: [u64; 8] = [
    0x0202020202020202, // Files adjacent to A (just B)
    0x0505050505050505, // Files adjacent to B (A and C)
    0x0A0A0A0A0A0A0A0A, // Files adjacent to C (B and D)
    0x1414141414141414, // Files adjacent to D (C and E)
    0x2828282828282828, // Files adjacent to E (D and F)
    0x5050505050505050, // Files adjacent to F (E and G)
    0xA0A0A0A0A0A0A0A0, // Files adjacent to G (F and H)
    0x4040404040404040, // Files adjacent to H (just G)
];

const RANK_MASKS: [u64; 8] = [
    0x00000000000000FF, // Rank 1
    0x000000000000FF00, // Rank 2
    0x0000000000FF0000, // Rank 3
    0x00000000FF000000, // Rank 4
    0x000000FF00000000, // Rank 5
    0x0000FF0000000000, // Rank 6
    0x00FF000000000000, // Rank 7
    0xFF00000000000000, // Rank 8
];

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
    pub phase: i32,
    pub white_bishops: u8,
    pub black_bishops: u8,
    pub pawn_score: i32,
    pub rook_score: i32,
    pub king_safety_score: i32,
    pub mobility_score: i32,
}

impl EvalState {
    pub fn new() -> Self {
        Self {
            mg_score: 0, eg_score: 0, material: 0, phase: 0,
            white_bishops: 0, black_bishops: 0,
            pawn_score: 0, rook_score: 0, king_safety_score: 0, mobility_score: 0,
        }
    }

    pub fn from_board(board: &Board) -> Self {
        let mut state = Self::new();
        let phase_values = [0, 1, 1, 2, 4, 0];
        
        for sq in *board.combined() {
            let piece = board.piece_on(sq).unwrap();
            let color = board.color_on(sq).unwrap();
            let is_white = color == Color::White;
            
            if piece != Piece::King {
                state.material += piece_value(piece);
                state.phase += phase_values[piece.to_index()];
            }
            
            if piece == Piece::Bishop {
                if is_white { state.white_bishops += 1; } else { state.black_bishops += 1; }
            }
            
            let sq_idx = if is_white { sq.to_index() } else { mirror_square(sq).to_index() };
            let (mg_val, eg_val) = Self::get_pst_values(piece, sq_idx);
            
            if is_white {
                state.mg_score += mg_val;
                state.eg_score += eg_val;
            } else {
                state.mg_score -= mg_val;
                state.eg_score -= eg_val;
            }
        }
        
        state.pawn_score = eval_pawns(board);
        state.rook_score = eval_rooks(board) + eval_connected_rooks(board);
        state.king_safety_score = eval_king_safety(board);
        state.mobility_score = eval_mobility(board);
        
        state
    }

    #[inline]
    fn get_pst_values(piece: Piece, sq_idx: usize) -> (i32, i32) {
        match piece {
            Piece::Pawn => (PAWN_VALUE + PAWN_PST[sq_idx], PAWN_VALUE + PAWN_PST[sq_idx]),
            Piece::Knight => (KNIGHT_VALUE + KNIGHT_PST[sq_idx], KNIGHT_VALUE + KNIGHT_PST[sq_idx]),
            Piece::Bishop => (BISHOP_VALUE + BISHOP_PST[sq_idx], BISHOP_VALUE + BISHOP_PST[sq_idx]),
            Piece::Rook => (ROOK_VALUE + ROOK_PST[sq_idx], ROOK_VALUE + ROOK_PST[sq_idx]),
            Piece::Queen => (QUEEN_VALUE + QUEEN_PST[sq_idx], QUEEN_VALUE + QUEEN_PST[sq_idx]),
            Piece::King => (KING_MG_PST[sq_idx], KING_EG_PST[sq_idx]),
        }
    }

    pub fn get_score(&self, side_to_move: Color) -> i32 {
        let phase = self.phase.max(0).min(24);  // FIXED: Clamp to prevent negative
        let score = (self.mg_score * phase + self.eg_score * (24 - phase)) / 24;
        if side_to_move == Color::White { score } else { -score }
    }

    // FIXED: Must be called BEFORE board.make_move()
    // Pass the board in its current (pre-move) state along with the move to make
    pub fn apply_move(&mut self, board: &Board, mv: ChessMove, color: Color) {
        let from = mv.get_source();
        let to = mv.get_dest();
        let piece = board.piece_on(from).unwrap();
        
        let mut pawn_changed = piece == Piece::Pawn;
        let mut rook_changed = piece == Piece::Rook;
        let king_changed = piece == Piece::King;
        
        // Handle normal captures (check current board for captured piece)
        if let Some(captured) = board.piece_on(to) {
            self.remove_piece(captured, !color, to);
            pawn_changed |= captured == Piece::Pawn;
            rook_changed |= captured == Piece::Rook;
        } 
        // Handle en passant (pawn moved diagonally to empty square)
        else if piece == Piece::Pawn && from.get_file() != to.get_file() {
            // FIXED: En passant captured pawn is behind the destination
            let ep_rank = if color == Color::White { Rank::Fifth } else { Rank::Fourth };
            let ep_sq = Square::make_square(ep_rank, to.get_file());
            self.remove_piece(Piece::Pawn, !color, ep_sq);
            pawn_changed = true;
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
            rook_changed = true;
        }
        
        // Move piece
        self.remove_piece(piece, color, from);
        if let Some(promotion) = mv.get_promotion() {
            self.add_piece(promotion, color, to);
            pawn_changed = true;
            rook_changed |= promotion == Piece::Rook;
        } else {
            self.add_piece(piece, color, to);
        }
        
        // Mark dirty scores for recalculation
        if pawn_changed { self.pawn_score = i32::MIN; }
        if rook_changed { self.rook_score = i32::MIN; }
        if king_changed || pawn_changed { self.king_safety_score = i32::MIN; }
        self.mobility_score = i32::MIN;
    }

    #[inline]
    fn add_piece(&mut self, piece: Piece, color: Color, sq: Square) {
        let is_white = color == Color::White;
        let sq_idx = if is_white { sq.to_index() } else { mirror_square(sq).to_index() };
        
        if piece != Piece::King {
            self.material += piece_value(piece);
            self.phase += [0, 1, 1, 2, 4, 0][piece.to_index()];
        }
        
        // FIXED: Update bishop counters
        if piece == Piece::Bishop {
            if is_white { self.white_bishops += 1; } else { self.black_bishops += 1; }
        }
        
        let (mg_val, eg_val) = Self::get_pst_values(piece, sq_idx);
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
        
        // FIXED: Update bishop counters
        if piece == Piece::Bishop {
            if is_white { 
                self.white_bishops = self.white_bishops.saturating_sub(1); 
            } else { 
                self.black_bishops = self.black_bishops.saturating_sub(1); 
            }
        }
        
        let (mg_val, eg_val) = Self::get_pst_values(piece, sq_idx);
        if is_white {
            self.mg_score -= mg_val;
            self.eg_score -= eg_val;
        } else {
            self.mg_score += mg_val;
            self.eg_score += eg_val;
        }
    }
}

fn eval_pawns(board: &Board) -> i32 {
    let white_pawns = board.pieces(Piece::Pawn) & board.color_combined(Color::White);
    let black_pawns = board.pieces(Piece::Pawn) & board.color_combined(Color::Black);
    
    let mut score = 0;
    let mut white_files = [0u8; 8];
    let mut black_files = [0u8; 8];
    
    for sq in white_pawns { white_files[sq.get_file().to_index()] += 1; }
    for sq in black_pawns { black_files[sq.get_file().to_index()] += 1; }
    
    score += eval_pawns_for_side(white_pawns, black_pawns, &white_files, true);
    score -= eval_pawns_for_side(black_pawns, white_pawns, &black_files, false);
    
    score
}

fn eval_pawns_for_side(pawns: BitBoard, enemy_pawns: BitBoard, files: &[u8; 8], is_white: bool) -> i32 {
    let mut score = 0;
    
    for sq in pawns {
        let file = sq.get_file().to_index();
        let rank = sq.get_rank().to_index();
        
        // Doubled pawns
        if files[file] > 1 { 
            score -= DOUBLED_PAWN_PENALTY; 
        }
        
        // Isolated pawns using bitwise check
        if (pawns.0 & ADJACENT_FILES_MASKS[file]) == 0 {
            score -= ISOLATED_PAWN_PENALTY;
        }
        
        // Passed pawns using correct bitboard masks
        let passed_mask = if is_white {
            // White: check files in front (file and adjacent files)
            let front_files = FILE_MASKS[file] | ADJACENT_FILES_MASKS[file];
            // Only ranks ahead of this pawn
            let ahead_ranks = 0xFFFFFFFFFFFFFFFFu64 << ((rank + 1) * 8);
            front_files & ahead_ranks
        } else {
            // Black: check files in front (below for black)
            let front_files = FILE_MASKS[file] | ADJACENT_FILES_MASKS[file];
            // Only ranks ahead (below) of this pawn
            let ahead_ranks = if rank == 0 { 
                0 
            } else { 
                (1u64 << (rank * 8)) - 1
            };
            front_files & ahead_ranks
        };
        
        if (enemy_pawns.0 & passed_mask) == 0 {
            score += PASSED_PAWN_BONUS[if is_white { rank } else { 7 - rank }];
        }
    }
    
    score
}

fn eval_rooks(board: &Board) -> i32 {
    let mut score = 0;
    let white_pawns = board.pieces(Piece::Pawn) & board.color_combined(Color::White);
    let black_pawns = board.pieces(Piece::Pawn) & board.color_combined(Color::Black);
    
    score += eval_rooks_for_side(board, Color::White, white_pawns, black_pawns, 6);
    score -= eval_rooks_for_side(board, Color::Black, white_pawns, black_pawns, 1);
    
    score
}

fn eval_rooks_for_side(board: &Board, color: Color, white_pawns: BitBoard, black_pawns: BitBoard, seventh_rank: usize) -> i32 {
    let mut score = 0;
    let rooks = board.pieces(Piece::Rook) & board.color_combined(color);
    
    for sq in rooks {
        let file = sq.get_file().to_index();
        let rank = sq.get_rank().to_index();
        
        // Check file pawns with bitwise operations
        let file_mask = FILE_MASKS[file];
        let has_white = (white_pawns.0 & file_mask) != 0;
        let has_black = (black_pawns.0 & file_mask) != 0;
        
        if !has_white && !has_black { 
            score += ROOK_OPEN_FILE; 
        } else if (color == Color::White && !has_white) || (color == Color::Black && !has_black) {
            score += ROOK_SEMI_OPEN;
        }
        
        if rank == seventh_rank { 
            score += ROOK_SEVENTH; 
        }
    }
    
    score
}

fn eval_mobility(board: &Board) -> i32 {
    eval_mobility_for_side(board, Color::White) - eval_mobility_for_side(board, Color::Black)
}

fn eval_mobility_for_side(board: &Board, color: Color) -> i32 {
    let mut score = 0;
    let combined = *board.combined();
    let own_pieces = *board.color_combined(color);
    
    let knights = board.pieces(Piece::Knight) & own_pieces;
    let bishops = board.pieces(Piece::Bishop) & own_pieces;
    let rooks = board.pieces(Piece::Rook) & own_pieces;
    let queens = board.pieces(Piece::Queen) & own_pieces;
    
    for sq in knights {
        score += (chess::get_knight_moves(sq) & !own_pieces).popcnt() as i32 * MOBILITY_KNIGHT;
    }
    for sq in bishops {
        score += (chess::get_bishop_moves(sq, combined) & !own_pieces).popcnt() as i32 * MOBILITY_BISHOP;
    }
    for sq in rooks {
        score += (chess::get_rook_moves(sq, combined) & !own_pieces).popcnt() as i32 * MOBILITY_ROOK;
    }
    for sq in queens {
        score += ((chess::get_bishop_moves(sq, combined) | chess::get_rook_moves(sq, combined)) & !own_pieces).popcnt() as i32 * MOBILITY_QUEEN;
    }
    
    score
}

fn eval_king_safety(board: &Board) -> i32 {
    eval_king_safety_for_side(board, Color::White, board.pieces(Piece::Pawn) & board.color_combined(Color::White))
    - eval_king_safety_for_side(board, Color::Black, board.pieces(Piece::Pawn) & board.color_combined(Color::Black))
}

fn eval_king_safety_for_side(board: &Board, color: Color, pawns: BitBoard) -> i32 {
    let mut score = 0;
    let king = (board.pieces(Piece::King) & board.color_combined(color)).to_square();
    let k_file = king.get_file().to_index();
    let k_rank = king.get_rank().to_index();
    
    let file_mask = FILE_MASKS[k_file] | 
                    if k_file > 0 { FILE_MASKS[k_file - 1] } else { 0 } |
                    if k_file < 7 { FILE_MASKS[k_file + 1] } else { 0 };
    
    // Pawn shield evaluation
    let shield_mask = if color == Color::White {
        if k_rank + 2 < 8 {
            file_mask & (RANK_MASKS[k_rank + 1] | RANK_MASKS[k_rank + 2])
        } else if k_rank + 1 < 8 {
            file_mask & RANK_MASKS[k_rank + 1]
        } else {
            0
        }
    } else {
        if k_rank >= 2 {
            file_mask & (RANK_MASKS[k_rank - 1] | RANK_MASKS[k_rank - 2])
        } else if k_rank >= 1 {
            file_mask & RANK_MASKS[k_rank - 1]
        } else {
            0
        }
    };
    
    let shield_pawns = pawns.0 & shield_mask;
    let shield_count = shield_pawns.count_ones() as i32;
    score += shield_count * PAWN_SHIELD;
    
    // Bonus for pawns on starting rank in front of king
    let (starting_rank, shield_rank_offset) = if color == Color::White { (1, 1) } else { (6, -1) };
    
    if k_rank == starting_rank {
        let close_shield_rank = (k_rank as i32 + shield_rank_offset) as usize;
        if close_shield_rank < 8 {
            let close_shield_mask = file_mask & RANK_MASKS[close_shield_rank];
            if (pawns.0 & close_shield_mask) != 0 {
                score += 5 * (pawns.0 & close_shield_mask).count_ones() as i32;
            }
        }
    }
    
    // Penalty for weak king with few pawn shields
    let advanced_rank = if color == Color::White { 2 } else { 5 };
    let is_king_advanced = if color == Color::White { 
        k_rank > advanced_rank 
    } else { 
        k_rank < advanced_rank 
    };
    
    if shield_count < 2 && !is_king_advanced {
        score -= WEAK_KING_SQUARE * (2 - shield_count);
    }
    
    // Penalty for king too far forward
    if is_king_advanced {
        let advancement = if color == Color::White { 
            (k_rank - advanced_rank) as i32 
        } else { 
            (advanced_rank - k_rank) as i32 
        };
        score -= 10 * advancement;
    }
    
    // Penalty for king in center
    if k_file > 1 && k_file < 6 { 
        score -= 15; 
    }
    
    // King zone attacks
    let king_zone_bb = chess::get_king_moves(king);
    let mut attack_count = 0;
    
    let enemy_color = !color;
    let enemy_pieces = *board.color_combined(enemy_color);
    let combined = *board.combined();
    
    for sq in board.pieces(Piece::Knight) & enemy_pieces {
        if (chess::get_knight_moves(sq) & king_zone_bb).0 != 0 {
            attack_count += 1;
        }
    }
    for sq in board.pieces(Piece::Bishop) & enemy_pieces {
        if (chess::get_bishop_moves(sq, combined) & king_zone_bb).0 != 0 {
            attack_count += 1;
        }
    }
    for sq in board.pieces(Piece::Rook) & enemy_pieces {
        if (chess::get_rook_moves(sq, combined) & king_zone_bb).0 != 0 {
            attack_count += 2;
        }
    }
    for sq in board.pieces(Piece::Queen) & enemy_pieces {
        if ((chess::get_bishop_moves(sq, combined) | chess::get_rook_moves(sq, combined)) & king_zone_bb).0 != 0 {
            attack_count += 3;
        }
    }
    
    score -= attack_count * KING_ATTACK_WEIGHT;
    score
}

fn eval_connected_rooks(board: &Board) -> i32 {
    eval_connected_rooks_for_side(board, Color::White) - eval_connected_rooks_for_side(board, Color::Black)
}

fn eval_connected_rooks_for_side(board: &Board, color: Color) -> i32 {
    let rooks = board.pieces(Piece::Rook) & board.color_combined(color);
    if rooks.popcnt() < 2 { return 0; }
    
    let combined = *board.combined();
    let mut score = 0;
    let mut processed = 0u64;
    
    for sq in rooks {
        let sq_bit = 1u64 << sq.to_index();
        if (processed & sq_bit) != 0 { 
            continue; 
        }
        
        let attacks = chess::get_rook_moves(sq, combined);
        let connected = attacks.0 & rooks.0 & !sq_bit;
        
        if connected != 0 {
            score += CONNECTED_ROOKS;
            processed |= sq_bit | connected;
        }
    }
    
    score
}

pub fn evaluate_board_incremental(board: &Board, eval_state: &mut EvalState, ply_from_root: i32) -> i32 {
    let status = board.status();
    
    if status == chess::BoardStatus::Checkmate { return -99999 + ply_from_root; }
    if status == chess::BoardStatus::Stalemate { return 0; }
    
    let phase = eval_state.phase.max(0).min(24);  // FIXED: Clamp to prevent negative
    let mut score = (eval_state.mg_score * phase + eval_state.eg_score * (24 - phase)) / 24;
    
    if eval_state.white_bishops >= 2 { score += BISHOP_PAIR_BONUS; }
    if eval_state.black_bishops >= 2 { score -= BISHOP_PAIR_BONUS; }
    
    // Always recalculate dirty scores
    if eval_state.pawn_score == i32::MIN { 
        eval_state.pawn_score = eval_pawns(board); 
    }
    score += eval_state.pawn_score;
    
    if eval_state.rook_score == i32::MIN { 
        eval_state.rook_score = eval_rooks(board) + eval_connected_rooks(board);
    }
    score += eval_state.rook_score;
    
    // King safety only matters in middlegame
    if phase > 12 {
        if eval_state.king_safety_score == i32::MIN { 
            eval_state.king_safety_score = eval_king_safety(board);
        }
        score += eval_state.king_safety_score;
    }
    
    // Always recalculate mobility when dirty
    if eval_state.mobility_score == i32::MIN { 
        eval_state.mobility_score = eval_mobility(board);
    }
    score += eval_state.mobility_score;
    
    if board.side_to_move() == Color::White { score } else { -score }
}