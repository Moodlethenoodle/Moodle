#helpers.py

import chess
from evaluation import PIECE_VALUES

MAX_DEPTH = 32
killer_moves = [[None, None] for _ in range(MAX_DEPTH)]

class HistoryHeuristic:
    def __init__(self):
        self.table = [[0 for _ in range(64)] for _ in range(64)]
        
    def update(self, move, depth, ply):
        """Update history for a move that caused a cutoff"""
        bonus = depth * depth
        from_sq = move.from_square
        to_sq = move.to_square
        
        # Gravity-based aging
        self.table[from_sq][to_sq] = (self.table[from_sq][to_sq] * 7 + bonus * 8) // 8
    
    def get_score(self, move):
        """Get history score"""
        return self.table[move.from_square][move.to_square]
    
    def clear(self):
        """Clear history table"""
        self.table = [[0 for _ in range(64)] for _ in range(64)]
    
history_heuristic = HistoryHeuristic()

def mvv_lva_score(board: chess.Board, move: chess.Move) -> int:
    """MVV-LVA scoring for captures"""
    score = 0
    if board.is_capture(move):
        victim = board.piece_at(move.to_square)
        attacker = board.piece_at(move.from_square)
        if victim and attacker:
            score = PIECE_VALUES[victim.piece_type] * 10 - PIECE_VALUES[attacker.piece_type]
    if move.promotion:
        score += PIECE_VALUES[move.promotion] * 100
    return score

def move_score(board: chess.Board, move: chess.Move) -> int:
    """Move scoring - use MVV-LVA for speed"""
    return mvv_lva_score(board, move)

def order_moves(board, moves, ply):
    """Optimized move ordering with pre-computed scores"""
    move_scores = []
    
    for i, move in enumerate(moves):
        if board.is_capture(move):
            score = mvv_lva_score(board, move) + 10000
        else:
            score = history_heuristic.get_score(move)
            if ply < MAX_DEPTH:
                if move == killer_moves[ply][0]:
                    score += 9000
                elif move == killer_moves[ply][1]:
                    score += 8000
        
        move_scores.append((score, i, move))
    
    move_scores.sort(reverse=True)
    return [move for score, idx, move in move_scores]

def capture_exchange_value(board: chess.Board, move: chess.Move) -> int:
    """Simple capture exchange value (victim - attacker)"""
    if not board.is_capture(move):
        return 0
    
    victim = board.piece_at(move.to_square)
    attacker = board.piece_at(move.from_square)
    
    if not victim or not attacker:
        return 0
    
    victim_value = PIECE_VALUES[victim.piece_type]
    attacker_value = PIECE_VALUES[attacker.piece_type]
    
    return victim_value - attacker_value