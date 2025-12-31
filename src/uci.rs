use chess::{Board, ChessMove, Color, MoveGen};
use std::io::{self, BufRead, Write};
use std::str::FromStr;

use crate::search::{pick_move_timed, SearchState};

pub fn run() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    
    let mut board = Board::default();
    let mut state = SearchState::new();

    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let parts: Vec<&str> = line.split_whitespace().collect();

        if parts.is_empty() {
            continue;
        }

        match parts[0] {
            "uci" => {
                println!("id name Iteration1");
                println!("id author Moodlethenoodle");
                println!("option name Hash type spin default 256 min 1 max 2048");
                println!("uciok");
                stdout.flush().unwrap();
            }
            "isready" => {
                println!("readyok");
                stdout.flush().unwrap();
            }
            "setoption" => {
                if line.contains("name Hash") {
                    if let Some(value_pos) = parts.iter().position(|&p| p == "value") {
                        if let Some(size_str) = parts.get(value_pos + 1) {
                            if let Ok(size) = size_str.parse::<usize>() {
                                state.tt = crate::tt::TranspositionTable::new(size);
                                println!("# Hash table resized to {}MB", size);
                            }
                        }
                    }
                }
                stdout.flush().unwrap();
            }
            "ucinewgame" => {
                state.tt.clear();
                state.history.clear();
                state.position_history.clear();  // Clear position history for new game
                println!("# New game - cleared TT, history, and position history");
                stdout.flush().unwrap();
            }
            "position" => {
                state.position_history.clear();
                state.halfmove_clock = 0;  // NEW: Reset halfmove clock

                if parts[1] == "startpos" {
                    board = Board::default();
                    
                    if let Some(moves_pos) = parts.iter().position(|&p| p == "moves") {
                        for move_str in &parts[moves_pos + 1..] {
                            state.position_history.push(board.get_hash());
                            
                            if let Ok(mv) = ChessMove::from_san(&board, move_str) {
                                // NEW: Update halfmove clock
                                if state.is_irreversible(&board, mv) {
                                    state.halfmove_clock = 0;
                                } else {
                                    state.halfmove_clock += 1;
                                }
                                board = board.make_move_new(mv);
                            } else if let Ok(mv) = ChessMove::from_str(move_str) {
                                // NEW: Update halfmove clock
                                if state.is_irreversible(&board, mv) {
                                    state.halfmove_clock = 0;
                                } else {
                                    state.halfmove_clock += 1;
                                }
                                board = board.make_move_new(mv);
                            }
                        }
                    }
                } else if parts[1] == "fen" {
                    let moves_pos = parts.iter().position(|&p| p == "moves");
                    let fen_end = moves_pos.unwrap_or(parts.len());
                    let fen_str = parts[2..fen_end].join(" ");
                    
                    if let Ok(new_board) = Board::from_str(&fen_str) {
                        board = new_board;
                        
                        // NEW: Parse halfmove clock from FEN if present
                        let fen_parts: Vec<&str> = fen_str.split_whitespace().collect();
                        if fen_parts.len() >= 5 {
                            if let Ok(halfmove) = fen_parts[4].parse::<u32>() {
                                state.halfmove_clock = halfmove;
                            }
                        }
                        
                        if let Some(moves_pos) = moves_pos {
                            for move_str in &parts[moves_pos + 1..] {
                                state.position_history.push(board.get_hash());
                                
                                if let Ok(mv) = ChessMove::from_san(&board, move_str) {
                                    // NEW: Update halfmove clock
                                    if state.is_irreversible(&board, mv) {
                                        state.halfmove_clock = 0;
                                    } else {
                                        state.halfmove_clock += 1;
                                    }
                                    board = board.make_move_new(mv);
                                } else if let Ok(mv) = ChessMove::from_str(move_str) {
                                    // NEW: Update halfmove clock
                                    if state.is_irreversible(&board, mv) {
                                        state.halfmove_clock = 0;
                                    } else {
                                        state.halfmove_clock += 1;
                                    }
                                    board = board.make_move_new(mv);
                                }
                            }
                        }
                    }
                }
            }
            "go" => {
                let mut movetime = None;
                let mut wtime = None;
                let mut btime = None;
                let mut winc = None;
                let mut binc = None;
                let mut movestogo = None;
                let mut max_depth = None;

                let mut i = 1;
                while i < parts.len() {
                    match parts[i] {
                        "movetime" => {
                            if let Some(val) = parts.get(i + 1) {
                                movetime = val.parse::<u64>().ok();
                            }
                            i += 2;
                        }
                        "wtime" => {
                            if let Some(val) = parts.get(i + 1) {
                                wtime = val.parse::<u64>().ok();
                            }
                            i += 2;
                        }
                        "btime" => {
                            if let Some(val) = parts.get(i + 1) {
                                btime = val.parse::<u64>().ok();
                            }
                            i += 2;
                        }
                        "winc" => {
                            if let Some(val) = parts.get(i + 1) {
                                winc = val.parse::<u64>().ok();
                            }
                            i += 2;
                        }
                        "binc" => {
                            if let Some(val) = parts.get(i + 1) {
                                binc = val.parse::<u64>().ok();
                            }
                            i += 2;
                        }
                        "movestogo" => {
                            if let Some(val) = parts.get(i + 1) {
                                movestogo = val.parse::<u32>().ok();
                            }
                            i += 2;
                        }
                        "depth" => {
                            if let Some(val) = parts.get(i + 1) {
                                max_depth = val.parse::<i32>().ok();
                            }
                            i += 2;
                        }
                        "infinite" => {
                            max_depth = Some(99);
                            i += 1;
                        }
                        _ => i += 1,
                    }
                }

                let time_limit = if let Some(mt) = movetime {
                    Some((mt as f64 / 1000.0 - 0.05).max(0.01))
                } else if board.side_to_move() == Color::White {
                    if let Some(wt) = wtime {
                        let increment = winc.unwrap_or(0) as f64;
                        let moves_left = movestogo.unwrap_or(30) as f64;
                        let safe_time = (wt as i64 - 1500).max(100) as f64;
                        Some(((safe_time / 1000.0 / moves_left * 0.85) + (increment / 1000.0 * 0.9)).max(0.01))
                    } else {
                        None
                    }
                } else {
                    if let Some(bt) = btime {
                        let increment = binc.unwrap_or(0) as f64;
                        let moves_left = movestogo.unwrap_or(30) as f64;
                        let safe_time = (bt as i64 - 1500).max(100) as f64;
                        Some(((safe_time / 1000.0 / moves_left * 0.85) + (increment / 1000.0 * 0.9)).max(0.01))
                    } else {
                        None
                    }
                };

                let depth = max_depth.unwrap_or(64);

                let (best_move, _) = pick_move_timed(&board, depth, time_limit, &mut state);

                if let Some(mv) = best_move {
                    println!("bestmove {}", mv);
                } else {
                    let legal_moves: Vec<ChessMove> = MoveGen::new_legal(&board).collect();
                    if !legal_moves.is_empty() {
                        println!("bestmove {}", legal_moves[0]);
                    }
                }
                stdout.flush().unwrap();
            }
            "quit" => break,
            _ => {}
        }
    }
}