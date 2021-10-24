try:
    import cupy as xp
except ImportError:
    import numpy as xp

def decide(x, L_sym):
    assert L_sym >= 1, 'There must be at least one sample per symbol'

    x = x.reshape(x.shape[: -1] + (-1, L_sym)).sum(axis=2)
    x = x.argmax(axis=0)

    return x

def decode(x, symbol_size):
    assert symbol_size >= 1, 'Each symbol must encode at least one bit'

    assert len(x) % (8 // symbol_size) == 0, (
        'Symbols must yield an integer number of bytes')

    symbols_per_byte = 8 // symbol_size

    x = x.reshape(-1, symbols_per_byte)
    x *= 2 ** xp.flip(symbol_size * xp.arange(symbols_per_byte), axis=0)
    x = x.sum(axis=1)

    return x.astype('B').tobytes()