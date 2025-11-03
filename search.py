#search.py

import chess
import time
import sys
import random
from evaluation import evaluate_board
from helpers import order_moves, move_score, history_heuristic, killer_moves, mvv_lva_score, capture_exchange_value, MAX_DEPTH
from tt import tt, EXACT, LOWER_BOUND, UPPER_BOUND

depth = 5
nodes_searched = 0
null_cutoffs = 0
lmr_attempts = 0
pvs_research = 0
stop_search = False

def quiescence(board: chess.Board, alpha, beta, ply_from_root=0, max_qs_depth=20):
    stand_pat = evaluate_board(board, ply_from_root)
    
    if stand_pat >= beta:
        return beta
    
    if ply_from_root >= max_qs_depth:
        return stand_pat
    
    BIG_DELTA = 900
    if stand_pat + BIG_DELTA < alpha:
        return alpha
    
    if alpha < stand_pat:
        alpha = stand_pat

    # Generate captures and filter bad ones
    captures = []
    for move in board.generate_legal_captures():
        if capture_exchange_value(board, move) >= -100:
            captures.append(move)
    
    captures.sort(key=lambda m: mvv_lva_score(board, m), reverse=True)
    
    for move in captures:
        board.push(move)
        score = -quiescence(board, -beta, -alpha, ply_from_root + 1, max_qs_depth)
        board.pop()
        
        if score >= beta:
            return beta
        if score > alpha:
            alpha = score
            
    return alpha

def negamax(board: chess.Board, depth, alpha, beta, ply_from_root):
    global nodes_searched, null_cutoffs, lmr_attempts, pvs_research
    nodes_searched += 1

    if stop_search:
        return 0, []
    
    alpha_orig = alpha

    # Probe transposition table
    tt_hit, tt_score, tt_move = tt.probe(board, depth, alpha, beta)
    if tt_hit and ply_from_root > 0:
        return tt_score, [tt_move] if tt_move else []
    
    pv = []
    
    if depth == 0:
        score = quiescence(board, alpha, beta, ply_from_root)
        return score, pv

    # NULL MOVE PRUNING with adaptive R
    if (ply_from_root > 0 and
        depth >= 3 and
        not board.is_check() and
        board.halfmove_clock < 90):
        
        board.push(chess.Move.null())
        R = 3 if depth >= 6 else 2
        null_score, _ = negamax(board, depth - 1 - R, -beta, -beta + 1, ply_from_root + 1)
        null_score = -null_score
        board.pop()
        
        if null_score >= beta:
            null_cutoffs += 1
            return beta, pv

    moves = list(board.legal_moves)
    if not moves:
        if board.is_checkmate():
            return -100000 + ply_from_root, pv
        return 0, pv
    
    # Use TT move for ordering
    if tt_move and tt_move in moves:
        moves.remove(tt_move)
        moves.insert(0, tt_move)
    else:
        moves = order_moves(board, moves, ply_from_root)
    
    best_score = -1000000
    best_move = None
    in_check = board.is_check()
    
    for i, move in enumerate(moves):
        is_capture = board.is_capture(move)
        
        board.push(move)
        gives_check = board.is_check()
        
        # PRINCIPAL VARIATION SEARCH
        if i == 0:
            score, sub_pv = negamax(board, depth - 1, -beta, -alpha, ply_from_root + 1)
            score = -score
        else:
            # Late Move Reductions
            reduction = 0
            if (not in_check and 
                not gives_check and
                not is_capture and
                not move.promotion and
                i >= 3 and
                depth >= 3):
                reduction = 1
                if i >= 6 and depth >= 5:
                    reduction = 2
                if reduction > 0:
                    lmr_attempts += 1
            
            # Null window search (possibly reduced)
            score, sub_pv = negamax(board, depth - 1 - reduction, -alpha - 1, -alpha, ply_from_root + 1)
            score = -score
            
            # Re-search if it beat alpha
            if score > alpha:
                if reduction > 0:
                    # Re-search at full depth with null window
                    score, sub_pv = negamax(board, depth - 1, -alpha - 1, -alpha, ply_from_root + 1)
                    score = -score
                
                # Full window search if score is between alpha and beta
                if alpha < score < beta:
                    pvs_research += 1
                    score, sub_pv = negamax(board, depth - 1, -beta, -alpha, ply_from_root + 1)
                    score = -score
        
        board.pop()

        if score > best_score:
            best_score = score
            best_move = move
            pv = [move] + sub_pv
            
            if score > alpha:
                alpha = score
                
                if score >= beta:
                    # Beta cutoff
                    history_heuristic.update(move, depth, ply_from_root)
                    if not board.is_capture(move) and ply_from_root < MAX_DEPTH:
                        killer_moves[ply_from_root][1] = killer_moves[ply_from_root][0]
                        killer_moves[ply_from_root][0] = move
                    
                    tt.store(board, depth, beta, LOWER_BOUND, best_move)
                    return beta, pv
    
    # Store in transposition table
    if best_score <= alpha_orig:
        tt.store(board, depth, best_score, UPPER_BOUND, best_move)
    else:
        tt.store(board, depth, best_score, EXACT, best_move)
    
    return best_score, pv

def pick_move_timed(board: chess.Board, max_depth, time_limit=None, stochastic=False):
    """
    Pick move with time management and optional stochastic mode
    Returns: (best_move, list of (move, score) tuples)
    """
    global nodes_searched, null_cutoffs, lmr_attempts, pvs_research, stop_search
    nodes_searched = 0
    null_cutoffs = 0
    lmr_attempts = 0
    pvs_research = 0
    stop_search = False
    
    # Clear killer moves for new search
    for i in range(MAX_DEPTH):
        killer_moves[i][0] = None
        killer_moves[i][1] = None
    
    best_move = None
    best_score = -1000000
    principal_variation = []
    pv_move = None
    all_root_moves = []
    
    start_time = time.time()
    time_for_move = time_limit if time_limit else 999999.0
    
    moves = list(board.legal_moves)
    
    # Filter out obvious draw moves only if we have better options
    non_draw_moves = []
    for move in moves:
        board.push(move)
        is_draw = board.can_claim_draw()
        board.pop()
        if not is_draw:
            non_draw_moves.append(move)
    
    if non_draw_moves:
        moves = non_draw_moves
    
    moves.sort(key=lambda move: move_score(board, move), reverse=True)
    
    for current_depth in range(1, max_depth + 1):
        # Check time before starting new depth - use more of time budget
        if time.time() - start_time > time_for_move * 0.85:
            break
        
        # Aspiration window search
        if current_depth >= 4 and best_move is not None:
            window = 50
            search_alpha = best_score - window
            search_beta = best_score + window
        else:
            search_alpha = -1000000
            search_beta = 1000000
        
        current_best_move = None
        current_best_score = -1000000
        current_pv = []
        root_move_scores = []
        
        # Use PV move from previous iteration
        if pv_move and pv_move in moves:
            moves.remove(pv_move)
            moves.insert(0, pv_move)
        
        aspiration_fail = False
        while True:
            for i, move in enumerate(moves):
                # Check time limit during search
                if time.time() - start_time > time_for_move * 0.95:
                    stop_search = True
                    break
                
                board.push(move)
                score, pv = negamax(board, current_depth - 1, -search_beta, -search_alpha, 1)
                score = -score
                board.pop()
                
                if score > current_best_score:
                    current_best_score = score
                    current_best_move = move
                    current_pv = [move] + pv
                
                root_move_scores.append((move, score))
            
            # Check if we failed aspiration window
            if not stop_search and current_depth >= 4 and (current_best_score <= search_alpha or current_best_score >= search_beta):
                # Widen window and re-search
                window *= 2
                search_alpha = current_best_score - window
                search_beta = current_best_score + window
                root_move_scores = []
                aspiration_fail = True
                continue
            else:
                break
        
        if stop_search:
            break
        
        best_score = current_best_score
        best_move = current_best_move
        principal_variation = current_pv
        pv_move = best_move
        all_root_moves = root_move_scores
        
        elapsed = int((time.time() - start_time) * 1000)
        nps = nodes_searched * 1000 // max(elapsed, 1)
        
        tt_stats = tt.get_stats()
        
        # Format score with mate detection
        if abs(best_score) > 90000:
            mate_in = (100000 - abs(best_score)) // 2
            if best_score < 0:
                mate_in = -mate_in
            score_str = f"score mate {mate_in}"
        else:
            score_str = f"score cp {int(best_score)}"
        
        pv_str = ' '.join(m.uci() for m in principal_variation)
        print(f"info depth {current_depth} nodes {nodes_searched} time {elapsed} nps {nps} {score_str} hashfull {int(tt_stats['hit_rate'])} pv {pv_str}", flush=True)
    
    # TT diagnostics
    filled = sum(1 for entry in tt.table if entry is not None)
    fill_pct = (filled * 100) // tt.size if tt.size > 0 else 0
    print(f"# TT: {filled}/{tt.size} entries ({fill_pct}%), {tt_stats['hits']} hits, {tt_stats['misses']} misses", flush=True)
    print(f"# Null cutoffs: {null_cutoffs}, LMR: {lmr_attempts}, PVS research: {pvs_research}", flush=True)
    
    # Stochastic mode - only if we have complete move scores
    if stochastic and all_root_moves and len(all_root_moves) == len(moves):
        threshold = best_score - 10
        good_moves = [move for move, score in all_root_moves if score >= threshold]
        if len(good_moves) > 1:
            best_move = random.choice(good_moves)
            print(f"# Stochastic: chose from {len(good_moves)} good moves", flush=True)
    
    return best_move, all_root_moves

# Backwards compatibility wrapper
def pick_move(board, depth):
    move, _ = pick_move_timed(board, depth, time_limit=None, stochastic=False)
    return move