#uci.py 

import chess
from search import pick_move_timed
from tt import tt
from helpers import history_heuristic
import sys

# Force unbuffered output for PyInstaller compatibility
sys.stdout.reconfigure(line_buffering=True) if hasattr(sys.stdout, 'reconfigure') else None
sys.stderr.reconfigure(line_buffering=True) if hasattr(sys.stderr, 'reconfigure') else None

board = chess.Board()
stochastic_mode = False

def uci_print(message):
    """Print with guaranteed flush for UCI communication"""
    print(message, flush=True)

def main():
    global stochastic_mode
    
    while True:
        try:
            line = input()
        except EOFError:
            break

        if line == "uci":
            uci_print("id name Iteration1")
            uci_print("id author Moodlethenoodle")
            uci_print("option name Stochastic type check default false")
            uci_print("option name Hash type spin default 256 min 1 max 2048")
            uci_print("uciok")

        elif line == "isready":
            uci_print("readyok")

        elif line.startswith("setoption"):
            if "name Stochastic" in line:
                if "value true" in line:
                    stochastic_mode = True
                    uci_print("# Stochastic mode enabled")
                else:
                    stochastic_mode = False
                    uci_print("# Stochastic mode disabled")
            elif "name Hash" in line:
                try:
                    parts = line.split()
                    value_idx = parts.index("value") + 1
                    hash_size = int(parts[value_idx])
                    tt.resize(hash_size)
                    uci_print(f"# Hash table resized to {hash_size}MB")
                except (ValueError, IndexError):
                    pass

        elif line == "ucinewgame":
            # Clear transposition table and history on new game
            tt.clear()
            history_heuristic.clear()
            uci_print("# New game - cleared TT and history")

        elif line.startswith("position"):
            if "startpos" in line:
                board.reset()
                if "moves" in line:
                    moves = line.split("moves")[1].strip().split()
                    for move in moves:
                        board.push_uci(move)

            elif "fen" in line:
                fen = line[8:].strip()
                if "moves" in fen:
                    fen, moves_str = fen.split("moves")
                    board.set_fen(fen.strip())
                    for move in moves_str.strip().split():
                        board.push_uci(move)
                else:
                    board.set_fen(fen)

        elif line.startswith("go"):
            parts = line.split()
            movetime = None
            wtime = None
            btime = None
            winc = None
            binc = None
            movestogo = None
            max_depth = None
            
            i = 1
            while i < len(parts):
                if parts[i] == "movetime":
                    movetime = int(parts[i + 1])
                    i += 2
                elif parts[i] == "wtime":
                    wtime = int(parts[i + 1])
                    i += 2
                elif parts[i] == "btime":
                    btime = int(parts[i + 1])
                    i += 2
                elif parts[i] == "winc":
                    winc = int(parts[i + 1])
                    i += 2
                elif parts[i] == "binc":
                    binc = int(parts[i + 1])
                    i += 2
                elif parts[i] == "movestogo":
                    movestogo = int(parts[i + 1])
                    i += 2
                elif parts[i] == "depth":
                    max_depth = int(parts[i + 1])
                    i += 2
                elif parts[i] == "infinite":
                    max_depth = 99
                    i += 1
                else:
                    i += 1
            
            # Calculate time allocation with safety buffer
            time_limit = None
            SAFETY_BUFFER_MS = 1500  # 1.5 second safety buffer
            
            if movetime:
                time_limit = max(movetime / 1000.0 - 0.05, 0.01)  # Just 50ms for movetime
            elif board.turn == chess.WHITE and wtime:
                increment = winc if winc else 0
                moves_left = movestogo if movestogo else 30
                
                # Reserve 1.5 seconds as safety buffer
                safe_time = max(wtime - SAFETY_BUFFER_MS, 100)
                
                # Use 85% of safe time per move + 90% of increment
                time_limit = max((safe_time / 1000.0 / moves_left * 0.85) + (increment / 1000.0 * 0.9), 0.01)
                
            elif board.turn == chess.BLACK and btime:
                increment = binc if binc else 0
                moves_left = movestogo if movestogo else 30
                
                # Reserve 1.5 seconds as safety buffer
                safe_time = max(btime - SAFETY_BUFFER_MS, 100)
                
                # Use 85% of safe time per move + 90% of increment
                time_limit = max((safe_time / 1000.0 / moves_left * 0.85) + (increment / 1000.0 * 0.9), 0.01)
            
            if max_depth is None:
                max_depth = 64  # Search deeper by default
            
            best_move, all_moves = pick_move_timed(board, max_depth, time_limit, stochastic_mode)
            
            if best_move:
                uci_print(f"bestmove {best_move.uci()}")
            else:
                # Fallback to any legal move
                legal_moves = list(board.legal_moves)
                if legal_moves:
                    uci_print(f"bestmove {legal_moves[0].uci()}")

        elif line == "quit":
            break

if __name__ == '__main__':
    main()