#evaluation.py

import chess

PIECE_VALUES = {
    chess.PAWN: 100,
    chess.KNIGHT: 300,
    chess.BISHOP: 300,
    chess.ROOK: 500,
    chess.QUEEN: 900,
    chess.KING: 0
}

PAWN_PST = [
     0,  0,  0,  0,  0,  0,  0,  0,
     5, 10, 10,-20,-20, 10, 10,  5,
     5, -5,-10,  0,  0,-10, -5,  5,
     0,  0,  0, 20, 20,  0,  0,  0,
     5,  5, 10, 25, 25, 10,  5,  5,
    10, 10, 20, 30, 30, 20, 10, 10,
    50, 50, 50, 50, 50, 50, 50, 50,
     0,  0,  0,  0,  0,  0,  0,  0
]

KNIGHT_PST = [
    -50,-40,-30,-30,-30,-30,-40,-50,
    -40,-20,  0,  0,  0,  0,-20,-40,
    -30,  0, 10, 15, 15, 10,  0,-30,
    -30,  5, 15, 20, 20, 15,  5,-30,
    -30,  0, 15, 20, 20, 15,  0,-30,
    -30,  5, 10, 15, 15, 10,  5,-30,
    -40,-20,  0,  5,  5,  0,-20,-40,
    -50,-40,-30,-30,-30,-30,-40,-50
]

BISHOP_PST = [
    -20,-10,-10,-10,-10,-10,-10,-20,
    -10,  5,  0,  0,  0,  0,  5,-10,
    -10, 10, 10, 10, 10, 10, 10,-10,
    -10,  0, 10, 10, 10, 10,  0,-10,
    -10,  5,  5, 10, 10,  5,  5,-10,
    -10,  0,  5, 10, 10,  5,  0,-10,
    -10,  0,  0,  0,  0,  0,  0,-10,
    -20,-10,-10,-10,-10,-10,-10,-20
]

ROOK_PST = [
     0,  0,  0,  0,  0,  0,  0,  0,
     5, 10, 10, 10, 10, 10, 10,  5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
     0,  0,  0,  5,  5,  0,  0,  0
]

QUEEN_PST = [
    -20,-10,-10, -5, -5,-10,-10,-20,
    -10,  0,  0,  0,  0,  0,  0,-10,
    -10,  0,  5,  5,  5,  5,  0,-10,
     -5,  0,  5,  5,  5,  5,  0, -5,
      0,  0,  5,  5,  5,  5,  0, -5,
    -10,  5,  5,  5,  5,  5,  0,-10,
    -10,  0,  5,  0,  0,  0,  0,-10,
    -20,-10,-10, -5, -5,-10,-10,-20
]

KING_MG_PST = [
    20, 30, 10,  0,  0, 10, 30, 20,
    20, 20,  0,  0,  0,  0, 20, 20,
   -10,-20,-20,-20,-20,-20,-20,-10,
   -20,-30,-30,-40,-40,-30,-30,-20,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30
]

KING_EG_PST = [
   -50,-30,-30,-30,-30,-30,-30,-50,
   -30,-30,  0,  0,  0,  0,-30,-30,
   -30,-10, 20, 30, 30, 20,-10,-30,
   -30,-10, 30, 40, 40, 30,-10,-30,
   -30,-10, 30, 40, 40, 30,-10,-30,
   -30,-10, 20, 30, 30, 20,-10,-30,
   -30,-20,-10,  0,  0,-10,-20,-30,
   -50,-40,-30,-20,-20,-30,-40,-50
]

SQUARE_MIRROR = [chess.square_mirror(sq) for sq in chess.SQUARES]

def get_lsb_square(bb):
    """Get the square index of the least significant bit"""
    return (bb & -bb).bit_length() - 1

def evaluate_board(board: chess.Board, ply_from_root) -> float:
    """Optimized evaluation using direct bitboard access"""
    if board.is_checkmate():
        return -99999 + ply_from_root
    if board.is_stalemate():
        return 0

    material = 0
    
    white_pieces = board.occupied_co[chess.WHITE]
    black_pieces = board.occupied_co[chess.BLACK]
    
    # Pawns
    wp = board.pawns & white_pieces
    bp = board.pawns & black_pieces
    while wp:
        sq_idx = get_lsb_square(wp)
        material += 100 + PAWN_PST[sq_idx]
        wp &= wp - 1
    while bp:
        sq_idx = get_lsb_square(bp)
        material -= 100 + PAWN_PST[SQUARE_MIRROR[sq_idx]]
        bp &= bp - 1
    
    # Knights
    wn = board.knights & white_pieces
    bn = board.knights & black_pieces
    while wn:
        sq_idx = get_lsb_square(wn)
        material += 300 + KNIGHT_PST[sq_idx]
        wn &= wn - 1
    while bn:
        sq_idx = get_lsb_square(bn)
        material -= 300 + KNIGHT_PST[SQUARE_MIRROR[sq_idx]]
        bn &= bn - 1
    
    # Bishops
    wb = board.bishops & white_pieces
    bb = board.bishops & black_pieces
    while wb:
        sq_idx = get_lsb_square(wb)
        material += 300 + BISHOP_PST[sq_idx]
        wb &= wb - 1
    while bb:
        sq_idx = get_lsb_square(bb)
        material -= 300 + BISHOP_PST[SQUARE_MIRROR[sq_idx]]
        bb &= bb - 1
    
    # Rooks
    wr = board.rooks & white_pieces
    br = board.rooks & black_pieces
    while wr:
        sq_idx = get_lsb_square(wr)
        material += 500 + ROOK_PST[sq_idx]
        wr &= wr - 1
    while br:
        sq_idx = get_lsb_square(br)
        material -= 500 + ROOK_PST[SQUARE_MIRROR[sq_idx]]
        br &= br - 1
    
    # Queens
    wq = board.queens & white_pieces
    bq = board.queens & black_pieces
    while wq:
        sq_idx = get_lsb_square(wq)
        material += 900 + QUEEN_PST[sq_idx]
        wq &= wq - 1
    while bq:
        sq_idx = get_lsb_square(bq)
        material -= 900 + QUEEN_PST[SQUARE_MIRROR[sq_idx]]
        bq &= bq - 1
    
    # King - with endgame detection
    is_endgame = abs(material) < 2000
    king_pst = KING_EG_PST if is_endgame else KING_MG_PST
    
    wk = board.kings & white_pieces
    bk = board.kings & black_pieces
    if wk:
        sq_idx = get_lsb_square(wk)
        material += king_pst[sq_idx]
    if bk:
        sq_idx = get_lsb_square(bk)
        material -= king_pst[SQUARE_MIRROR[sq_idx]]

    return material if board.turn == chess.WHITE else -material