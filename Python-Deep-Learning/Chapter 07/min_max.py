from tic_tac_toe import available_moves, apply_move, has_winner
import sys


def _score_line(line):
    minus_count = line.count(-1)
    plus_count = line.count(1)
    if minus_count + plus_count < 3:
        if minus_count == 2:
            return -1
        elif plus_count == 2:
            return 1
    return 0


def evaluate(board_state):
    """Get a rough score for how good we think this board position is for the plus_player. Does this based on number of
    2 in row lines we have.

    Args:
        board_state (3x3 tuple of int): The board state we are evaluating

    Returns:
        int: evaluated score for the position for the plus player, posative is good for the plus player, negative good
            for the minus player
    """
    score = 0
    for x in range(3):
        score += _score_line(board_state[x])
    for y in range(3):
        score += _score_line([i[y] for i in board_state])

    # diagonals
    score += _score_line([board_state[i][i] for i in range(3)])
    score += _score_line([board_state[2 - i][i] for i in range(3)])

    return score


def min_max(board_state, side, max_depth, evaluation_func=evaluate):
    """Runs the min_max_algorithm on a given board_sate for a given side, to a given depth in order to find the best
    move

    Args:
        board_state (3x3 tuple of int): The board state we are evaluating
        side (int): either +1 or -1
        max_depth (int): how deep we want our tree to go before we use the evaluate method to determine how good the
        position is.
        evaluation_func (board_state -> int): Function used to evaluate the position for the plus player

    Returns:
        (best_score(int), best_score_move((int, int)): the move found to be best and what it's min-max score was
    """
    best_score = None
    best_score_move = None

    moves = list(available_moves(board_state))
    if not moves:
        # this is a draw
        return 0, None

    for move in moves:
        new_board_state = apply_move(board_state, move, side)
        winner = has_winner(new_board_state)
        if winner != 0:
            return winner * 10000, move
        else:
            if max_depth <= 1:
                score = evaluation_func(new_board_state)
            else:
                score, _ = min_max(new_board_state, -side, max_depth - 1)
            if side > 0:
                if best_score is None or score > best_score:
                    best_score = score
                    best_score_move = move
            else:
                if best_score is None or score < best_score:
                    best_score = score
                    best_score_move = move
    return best_score, best_score_move


def min_max_alpha_beta(board_state, side, max_depth, evaluation_func=evaluate, alpha=-sys.float_info.max,
                       beta=sys.float_info.max):
    """Runs the min_max_algorithm on a given board_sate for a given side, to a given depth in order to find the best
    move

    Args:
        board_state (3x3 tuple of int): The board state we are evaluating
        side (int): either +1 or -1
        max_depth (int): how deep we want our tree to go before we use the evaluate method to determine how good the
        position is.
        evaluation_func (board_state -> int): Function used to evaluate the position for the plus player
        alpha (float): Used when this is called recursively, normally ignore
        beta (float): Used when this is called recursively, normally ignore

    Returns:
        (best_score(int), best_score_move((int, int)): the move found to be best and what it's min-max score was
    """
    best_score_move = None
    moves = list(available_moves(board_state))
    if not moves:
        return 0, None

    for move in moves:
        new_board_state = apply_move(board_state, move, side)
        winner = has_winner(new_board_state)
        if winner != 0:
            return winner * 10000, move
        else:
            if max_depth <= 1:
                score = evaluation_func(new_board_state)
            else:
                score, _ = min_max_alpha_beta(new_board_state, -side, max_depth - 1, alpha, beta)

        if side > 0:
            if score > alpha:
                alpha = score
                best_score_move = move
        else:
            if score < beta:
                beta = score
                best_score_move = move
        if alpha >= beta:
            break

    return alpha if side > 0 else beta, best_score_move


def min_max_player(board_state, side):
    return min_max(board_state, side, 5)[1]
