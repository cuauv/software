import itertools

def calc_key(new_data_perm, old_data, heuristic):
    return sum([heuristic(new, old) for (new, old) in zip(new_data_perm, old_data)])

def find_best_match(old_data, new_data, heuristic):
    """
    Finds the best way to align arbitrary new data with old data. Tests all possible
    permutations of the new data so that heuristic score is maximized.
    """
    permutations = itertools.permutations(new_data)
    diffs = [(permutation, calc_key(permutation, old_data, heuristic)) for permutation in permutations]
    sorted_diffs = sorted(diffs, key=lambda tup: tup[1])
    return sorted_diffs[0][0]
