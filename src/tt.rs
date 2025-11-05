use chess::{Board, ChessMove};

#[derive(Clone, Copy, PartialEq)]
pub enum TTFlag {
    Exact,
    LowerBound,
    UpperBound,
}

#[derive(Clone)]
pub struct TTEntry {
    pub zobrist: u64,
    pub depth: i32,
    pub score: i32,
    pub flag: TTFlag,
    pub best_move: Option<ChessMove>,
}

pub struct TranspositionTable {
    pub table: Vec<Option<TTEntry>>,
    pub size: usize,
    pub hits: u64,
    pub misses: u64,
}

impl TranspositionTable {
    pub fn new(size_mb: usize) -> Self {
        let bytes_per_entry = 40;
        let size = (size_mb * 1024 * 1024) / bytes_per_entry;
        Self {
            table: vec![None; size],
            size,
            hits: 0,
            misses: 0,
        }
    }

    fn index(&self, zobrist: u64) -> usize {
        (zobrist as usize) % self.size
    }

    pub fn store(&mut self, board: &Board, depth: i32, score: i32, flag: TTFlag, best_move: Option<ChessMove>) {
        let zobrist = board.get_hash();
        let idx = self.index(zobrist);
        
        let should_replace = match &self.table[idx] {
            None => true,
            Some(entry) => entry.zobrist == zobrist || entry.depth <= depth,
        };

        if should_replace {
            self.table[idx] = Some(TTEntry {
                zobrist,
                depth,
                score,
                flag,
                best_move,
            });
        }
    }

    pub fn probe(&mut self, board: &Board, depth: i32, alpha: i32, beta: i32) -> (bool, i32, Option<ChessMove>) {
        let zobrist = board.get_hash();
        let idx = self.index(zobrist);

        match &self.table[idx] {
            None => {
                self.misses += 1;
                (false, 0, None)
            }
            Some(entry) => {
                if entry.zobrist != zobrist {
                    self.misses += 1;
                    return (false, 0, None);
                }

                let best_move = entry.best_move;

                if entry.depth >= depth {
                    self.hits += 1;
                    
                    let use_score = match entry.flag {
                        TTFlag::Exact => true,
                        TTFlag::LowerBound => entry.score >= beta,
                        TTFlag::UpperBound => entry.score <= alpha,
                    };

                    if use_score {
                        return (true, entry.score, best_move);
                    }
                }

                self.misses += 1;
                (false, 0, best_move)
            }
        }
    }

    pub fn clear(&mut self) {
        self.table = vec![None; self.size];
        self.hits = 0;
        self.misses = 0;
    }

    pub fn get_stats(&self) -> (u64, u64, f64) {
        let total = self.hits + self.misses;
        let hit_rate = if total > 0 {
            (self.hits as f64 / total as f64) * 100.0
        } else {
            0.0
        };
        (self.hits, self.misses, hit_rate)
    }
}