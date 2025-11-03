#tt.py

import chess

# Transposition Table Entry Types
EXACT = 0
LOWER_BOUND = 1  # Alpha bound (beta cutoff)
UPPER_BOUND = 2  # Beta bound (all moves failed low)

class TTEntry:
    __slots__ = ['zobrist', 'depth', 'score', 'flag', 'best_move']
    
    def __init__(self, zobrist, depth, score, flag, best_move):
        self.zobrist = zobrist
        self.depth = depth
        self.score = score
        self.flag = flag
        self.best_move = best_move

class TranspositionTable:
    def __init__(self, size_mb=256):
        """
        Initialize transposition table
        size_mb: Size in megabytes (default 256MB)
        """
        self._init_table(size_mb)
    
    def _init_table(self, size_mb):
        """Initialize or resize the table"""
        bytes_per_entry = 40
        self.size = (size_mb * 1024 * 1024) // bytes_per_entry
        self.table = [None] * self.size
        self.hits = 0
        self.misses = 0
    
    def resize(self, size_mb):
        """Resize the transposition table"""
        self._init_table(size_mb)
    
    def _get_hash(self, board):
        """Get zobrist hash - compatible with different python-chess versions"""
        if hasattr(board, 'zobrist_hash'):
            return board.zobrist_hash()
        elif hasattr(board, '_transposition_key'):
            return hash(board._transposition_key())
        else:
            return hash(board.fen())
    
    def index(self, zobrist_hash):
        """Get index for a zobrist hash"""
        return zobrist_hash % self.size
    
    def store(self, board, depth, score, flag, best_move=None):
        """Store with depth-preferred replacement"""
        zobrist = self._get_hash(board)
        idx = self.index(zobrist)
        
        existing = self.table[idx]
        
        # Replace if: empty, same position, or we searched deeper
        if existing is None or existing.zobrist == zobrist or existing.depth <= depth:
            self.table[idx] = TTEntry(zobrist, depth, score, flag, best_move)
    
    def probe(self, board, depth, alpha, beta):
        zobrist = self._get_hash(board)
        idx = self.index(zobrist)
        entry = self.table[idx]
        
        if entry is None or entry.zobrist != zobrist:
            self.misses += 1
            return False, 0, None
        
        # Found the position - always return the move for ordering
        best_move = entry.best_move
        
        # Can we use the score?
        if entry.depth >= depth:
            self.hits += 1
            
            if entry.flag == EXACT:
                return True, entry.score, best_move
            elif entry.flag == LOWER_BOUND and entry.score >= beta:
                return True, entry.score, best_move
            elif entry.flag == UPPER_BOUND and entry.score <= alpha:
                return True, entry.score, best_move
        
        # Can't use score, but return move for ordering
        self.misses += 1
        return False, 0, best_move

    def get_pv(self, board, max_depth=10):
        """Extract principal variation from transposition table"""
        pv = []
        seen_positions = set()
        
        for _ in range(max_depth):
            zobrist = self._get_hash(board)
            if zobrist in seen_positions:
                break
            seen_positions.add(zobrist)
            
            idx = self.index(zobrist)
            entry = self.table[idx]
            
            if entry is None or entry.zobrist != zobrist or entry.best_move is None:
                break
            
            if entry.best_move not in board.legal_moves:
                break
            
            pv.append(entry.best_move)
            board.push(entry.best_move)
        
        # Undo moves
        for _ in range(len(pv)):
            board.pop()
        
        return pv
    
    def clear(self):
        """Clear the transposition table"""
        self.table = [None] * self.size
        self.hits = 0
        self.misses = 0
    
    def get_stats(self):
        """Get hit/miss statistics"""
        total = self.hits + self.misses
        hit_rate = (self.hits / total * 100) if total > 0 else 0
        return {
            'hits': self.hits,
            'misses': self.misses,
            'hit_rate': hit_rate
        }

# Global transposition table
tt = TranspositionTable(size_mb=256)