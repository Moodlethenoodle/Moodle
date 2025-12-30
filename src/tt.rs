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
    pub age: u8,
}

pub struct TranspositionTable {
    pub table: Vec<Option<TTEntry>>,
    pub size: usize,
    pub mask: usize,
    pub hits: u64,
    pub misses: u64,
    pub generation: u8,
    pub filled_count: usize,
}

impl TranspositionTable {
    pub fn new(size_mb: usize) -> Self {
        let bytes_per_entry = std::mem::size_of::<Option<TTEntry>>();
        let entries = (size_mb * 1024 * 1024) / bytes_per_entry;
        
        let size = if entries.is_power_of_two() {
            entries
        } else {
            entries.next_power_of_two() / 2
        };
        
        Self {
            table: vec![None; size],
            size,
            mask: size - 1,
            hits: 0,
            misses: 0,
            generation: 0,
            filled_count: 0,
        }
    }

    #[inline]
    fn index(&self, zobrist: u64) -> usize {
        (zobrist as usize) & self.mask
    }

    pub fn new_search(&mut self) {
        self.generation = self.generation.wrapping_add(1);
    }

    #[inline]
    fn is_over_capacity(&self) -> bool {
        (self.filled_count * 100) / self.size > 95
    }

    pub fn store(&mut self, board: &Board, depth: i32, score: i32, flag: TTFlag, best_move: Option<ChessMove>, ply: i32) {
        let zobrist = board.get_hash();
        let idx = self.index(zobrist);
        
        let adjusted_score = if score > 90000 {
            score + ply
        } else if score < -90000 {
            score - ply
        } else {
            score
        };
        
        let is_over_capacity = self.is_over_capacity();
        
        let should_replace = match &self.table[idx] {
            None => true,
            Some(entry) => {
                if entry.zobrist == zobrist {
                    true
                } else {
                    let age_diff = self.generation.wrapping_sub(entry.age);
                    
                    if is_over_capacity {
                        age_diff >= 5 || depth > entry.depth
                    } else {
                        age_diff >= 10 || depth >= entry.depth
                    }
                }
            }
        };

        if should_replace {
            let was_empty = self.table[idx].is_none();
            
            self.table[idx] = Some(TTEntry {
                zobrist,
                depth,
                score: adjusted_score,
                flag,
                best_move,
                age: self.generation,
            });
            
            if was_empty {
                self.filled_count += 1;
            }
        }
    }

    pub fn probe(&mut self, board: &Board, depth: i32, alpha: i32, beta: i32, ply: i32) -> (bool, i32, Option<ChessMove>) {
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
                    
                    let adjusted_score = if entry.score > 90000 {
                        entry.score - ply
                    } else if entry.score < -90000 {
                        entry.score + ply
                    } else {
                        entry.score
                    };
                    
                    let use_score = match entry.flag {
                        TTFlag::Exact => true,
                        TTFlag::LowerBound => adjusted_score >= beta,
                        TTFlag::UpperBound => adjusted_score <= alpha,
                    };

                    if use_score {
                        return (true, adjusted_score, best_move);
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
        self.filled_count = 0;
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