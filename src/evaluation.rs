use chess::{Board, Color, Piece, Square, ChessMove, BitBoard, File, Rank};

pub const PAWN_VALUE: i32 = 100;
pub const KNIGHT_VALUE: i32 = 320;
pub const BISHOP_VALUE: i32 = 330;
pub const ROOK_VALUE: i32 = 500;
pub const QUEEN_VALUE: i32 = 900;

// Pawn structure penalties/bonuses
const DOUBLED_PAWN_PENALTY: i32 = 10;
const ISOLATED_PAWN_PENALTY: i32 = 20;
const BACKWARD_PAWN_PENALTY: i32 = 8;
const PASSED_PAWN_BONUS: [i32; 8] = [0, 5, 10, 20, 35, 60, 100, 0];
const CONNECTED_PAWN_BONUS: i32 = 7;
const PROTECTED_PASSED_PAWN_BONUS: [i32; 8] = [0, 10, 15, 30, 50, 80, 120, 0];

// Piece bonuses
const BISHOP_PAIR_BONUS: i32 = 30;
const ROOK_OPEN_FILE_BONUS: i32 = 20;
const ROOK_SEMI_OPEN_FILE_BONUS: i32 = 10;
const ROOK_ON_SEVENTH_BONUS: i32 = 20;
const QUEEN_EARLY_DEVELOPMENT_PENALTY: i32 = 10;

// King safety
const PAWN_SHIELD_BONUS: i32 = 10;
const KING_ATTACK_WEIGHT: [i32; 5] = [0, 0, 50, 75, 88]; // indexed by # of attackers

// Mobility (per legal move)
const KNIGHT_MOBILITY: i32 = 4;
const BISHOP_MOBILITY: i32 = 3;
const ROOK_MOBILITY: i32 = 2;
const QUEEN_MOBILITY: i32 = 1;

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

pub fn mirror_square(sq: Square) -> Square {
    let rank = sq.get_rank().to_index() ^ 7;
    let file = sq.get_file().to_index();
    Square::make_square(Rank::from_index(rank), File::from_index(file))
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

#[derive(Clone, Copy, Debug)]
pub struct PawnStructure {
    pub score: i32,
}

impl PawnStructure {
    pub fn new() -> Self {
        Self { score: 0 }
    }

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
        let mut file_masks = [0u8; 8];
        
        for sq in our_pawns {
            file_masks[sq.get_file().to_index()] += 1;
        }
        
        for sq in our_pawns {
            let file = sq.get_file().to_index();
            let rank = sq.get_rank().to_index();
            let actual_rank = if color == Color::White { rank } else { 7 - rank };
            
            if file_masks[file] > 1 {
                score -= DOUBLED_PAWN_PENALTY;
            }
            
            let has_neighbor = (file > 0 && file_masks[file - 1] > 0) || 
                              (file < 7 && file_masks[file + 1] > 0);
            
            if !has_neighbor {
                score -= ISOLATED_PAWN_PENALTY;
            } else {
                if Self::is_connected(sq, our_pawns, color) {
                    score += CONNECTED_PAWN_BONUS;
                }
            }
            
            let is_passed = Self::is_passed(sq, enemy_pawns, color);
            if is_passed {
                score += PASSED_PAWN_BONUS[actual_rank];
                
                // Bonus for protected passed pawns
                if Self::is_protected(sq, our_pawns, color) {
                    score += PROTECTED_PASSED_PAWN_BONUS[actual_rank] / 2;
                }
            }
            
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
            
            if check_file.abs_diff(file) == 1 {
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

    fn is_protected(sq: Square, our_pawns: BitBoard, color: Color) -> bool {
        let file = sq.get_file().to_index();
        let rank = sq.get_rank().to_index();
        
        for check_sq in our_pawns {
            let check_file = check_sq.get_file().to_index();
            let check_rank = check_sq.get_rank().to_index();
            
            if check_file.abs_diff(file) == 1 {
                if color == Color::White && check_rank == rank - 1 {
                    return true;
                } else if color == Color::Black && check_rank == rank + 1 {
                    return true;
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
            
            if enemy_file.abs_diff(file) <= 1 {
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
        
        let is_behind = if color == Color::White {
            rank < most_backward_neighbor_rank
        } else {
            rank > most_backward_neighbor_rank
        };
        
        if !is_behind {
            return false;
        }
        
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

#[derive(Clone, Copy, Debug)]
pub struct EvalState {
    pub mg_score: i32,
    pub eg_score: i32,
    pub total_material: i32,
    pub pawn_structure: PawnStructure,
    pub white_bishop_count: u8,
    pub black_bishop_count: u8,
}

impl EvalState {
    pub fn new() -> Self {
        Self {
            mg_score: 0,
            eg_score: 0,
            total_material: 0,
            pawn_structure: PawnStructure::new(),
            white_bishop_count: 0,
            black_bishop_count: 0,
        }
    }

    pub fn from_board(board: &Board) -> Self {
        let mut state = Self::new();
        
        for sq in *board.combined() {
            if let Some(piece) = board.piece_on(sq) {
                if piece != Piece::King {
                    state.total_material += piece_value(piece);
                }
                if piece == Piece::Bishop {
                    let color = board.color_on(sq).unwrap();
                    if color == Color::White {
                        state.white_bishop_count += 1;
                    } else {
                        state.black_bishop_count += 1;
                    }
                }
            }
        }

        state.pawn_structure = PawnStructure::from_board(board);

        for sq in *board.combined() {
            if let Some(piece) = board.piece_on(sq) {
                let color = board.color_on(sq).unwrap();
                state.add_piece(piece, color, sq);
            }
        }

        state
    }

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

        if piece != Piece::King {
            self.total_material -= piece_value(piece);
        }
        
        if piece == Piece::Bishop {
            if color == Color::White {
                self.white_bishop_count -= 1;
            } else {
                self.black_bishop_count -= 1;
            }
        }
    }

    pub fn apply_move(&mut self, board: &Board, mv: ChessMove, moving_color: Color) {
        let from = mv.get_source();
        let to = mv.get_dest();
        let piece = board.piece_on(from).unwrap();
        
        let mut pawn_structure_changed = false;

        if let Some(captured) = board.piece_on(to) {
            self.remove_piece(captured, !moving_color, to);
            if captured == Piece::Pawn {
                pawn_structure_changed = true;
            }
        } else if piece == Piece::Pawn && from.get_file() != to.get_file() {
            let capture_sq = Square::make_square(from.get_rank(), to.get_file());
            self.remove_piece(Piece::Pawn, !moving_color, capture_sq);
            pawn_structure_changed = true;
        }

        if piece == Piece::King && from.get_file().to_index().abs_diff(to.get_file().to_index()) == 2 {
            let (rook_from, rook_to) = if to.get_file().to_index() > from.get_file().to_index() {
                (Square::make_square(from.get_rank(), File::H),
                 Square::make_square(from.get_rank(), File::F))
            } else {
                (Square::make_square(from.get_rank(), File::A),
                 Square::make_square(from.get_rank(), File::D))
            };
            self.remove_piece(Piece::Rook, moving_color, rook_from);
            self.add_piece(Piece::Rook, moving_color, rook_to);
        }

        self.remove_piece(piece, moving_color, from);
        
        if piece == Piece::Pawn {
            pawn_structure_changed = true;
        }
        
        if let Some(promotion) = mv.get_promotion() {
            self.add_piece(promotion, moving_color, to);
        } else {
            self.add_piece(piece, moving_color, to);
        }
        
        if pawn_structure_changed {
            let new_board = board.make_move_new(mv);
            self.pawn_structure = PawnStructure::from_board(&new_board);
        }
    }

    pub fn get_score(&self, side_to_move: Color) -> i32 {
        let is_endgame = self.total_material < 2000;
        
        let mut score = if is_endgame {
            self.eg_score
        } else {
            self.mg_score
        };
        
        score += self.pawn_structure.score;
        
        // Bishop pair bonus
        if self.white_bishop_count >= 2 {
            score += BISHOP_PAIR_BONUS;
        }
        if self.black_bishop_count >= 2 {
            score -= BISHOP_PAIR_BONUS;
        }

        if side_to_move == Color::White {
            score
        } else {
            -score
        }
    }
}

// Helper to evaluate rook positions
fn evaluate_rook_position(board: &Board, sq: Square, color: Color) -> i32 {
    let mut score = 0;
    let file = sq.get_file();
    let rank = sq.get_rank();
    
    // Check for open/semi-open files
    let our_pawns = board.pieces(Piece::Pawn) & board.color_combined(color);
    let enemy_pawns = board.pieces(Piece::Pawn) & board.color_combined(!color);
    
    let mut has_our_pawn = false;
    let mut has_enemy_pawn = false;
    
    for check_sq in *board.pieces(Piece::Pawn) {
        if check_sq.get_file() == file {
            if our_pawns.0 & (1u64 << check_sq.to_index()) != 0 {
                has_our_pawn = true;
            }
            if enemy_pawns.0 & (1u64 << check_sq.to_index()) != 0 {
                has_enemy_pawn = true;
            }
        }
    }
    
    if !has_our_pawn && !has_enemy_pawn {
        score += ROOK_OPEN_FILE_BONUS;
    } else if !has_our_pawn {
        score += ROOK_SEMI_OPEN_FILE_BONUS;
    }
    
    // Rook on 7th rank bonus
    let seventh_rank = if color == Color::White { 6 } else { 1 };
    if rank.to_index() == seventh_rank {
        score += ROOK_ON_SEVENTH_BONUS;
    }
    
    score
}

// Simplified mobility evaluation
fn evaluate_mobility(board: &Board, color: Color) -> i32 {
    let mut score = 0;
    
    // Count pseudo-legal moves for each piece type
    for sq in *board.color_combined(color) {
        if let Some(piece) = board.piece_on(sq) {
            let mobility = match piece {
                Piece::Knight => {
                    let attacks = chess::get_knight_moves(sq);
                    let moves = attacks & !board.color_combined(color);
                    moves.popcnt() as i32 * KNIGHT_MOBILITY
                }
                Piece::Bishop => {
                    let attacks = chess::get_bishop_moves(sq, *board.combined());
                    let moves = attacks & !board.color_combined(color);
                    moves.popcnt() as i32 * BISHOP_MOBILITY
                }
                Piece::Rook => {
                    let attacks = chess::get_rook_moves(sq, *board.combined());
                    let moves = attacks & !board.color_combined(color);
                    moves.popcnt() as i32 * ROOK_MOBILITY
                }
                Piece::Queen => {
                    let attacks = chess::get_bishop_moves(sq, *board.combined()) | 
                                 chess::get_rook_moves(sq, *board.combined());
                    let moves = attacks & !board.color_combined(color);
                    moves.popcnt() as i32 * QUEEN_MOBILITY
                }
                _ => 0,
            };
            score += mobility;
        }
    }
    
    score
}

// King safety evaluation
fn evaluate_king_safety(board: &Board, color: Color, is_endgame: bool) -> i32 {
    if is_endgame {
        return 0;
    }
    
    let mut score = 0;
    
    // Find king
    let king_sq = (board.pieces(Piece::King) & board.color_combined(color)).to_square();
    let king_file = king_sq.get_file().to_index();
    let king_rank = king_sq.get_rank().to_index();
    
    // Pawn shield
    let our_pawns = board.pieces(Piece::Pawn) & board.color_combined(color);
    let shield_rank = if color == Color::White { king_rank + 1 } else { king_rank - 1 };
    
    if shield_rank <= 7 {
        for file_offset in -1..=1 {
            let check_file = (king_file as i32 + file_offset) as usize;
            if check_file < 8 {
                for pawn_sq in our_pawns {
                    if pawn_sq.get_file().to_index() == check_file {
                        let pawn_rank = pawn_sq.get_rank().to_index();
                        if color == Color::White && pawn_rank >= shield_rank && pawn_rank <= shield_rank + 1 {
                            score += PAWN_SHIELD_BONUS;
                        } else if color == Color::Black && pawn_rank <= shield_rank && pawn_rank >= shield_rank - 1 {
                            score += PAWN_SHIELD_BONUS;
                        }
                    }
                }
            }
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

    let mut score = eval_state.get_score(board.side_to_move());
    
    let is_endgame = eval_state.total_material < 2000;
    
    // Add positional bonuses (computed dynamically)
    let white_mobility = evaluate_mobility(board, Color::White);
    let black_mobility = evaluate_mobility(board, Color::Black);
    let mobility_diff = white_mobility - black_mobility;
    let white_king_safety = evaluate_king_safety(board, Color::White, is_endgame);
    let black_king_safety = evaluate_king_safety(board, Color::Black, is_endgame);
    let king_safety_diff = white_king_safety - black_king_safety;
    
    // Evaluate rook positions
    let mut rook_bonus = 0;
    let white_rooks = board.pieces(Piece::Rook) & board.color_combined(Color::White);
    for sq in white_rooks {
        rook_bonus += evaluate_rook_position(board, sq, Color::White);
    }
    let black_rooks = board.pieces(Piece::Rook) & board.color_combined(Color::Black);
    for sq in black_rooks {
        rook_bonus -= evaluate_rook_position(board, sq, Color::Black);
    }
    
    if board.side_to_move() == Color::White {
        score += mobility_diff + king_safety_diff + rook_bonus;
    } else {
        score -= mobility_diff + king_safety_diff + rook_bonus;
    }
    
    score
}