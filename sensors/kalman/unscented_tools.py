from cupy import zeros, dot, outer, array
from cupy.linalg import cholesky


def calculate_sigmas(x, dim_x, P, kappa):

    sp = zeros((2*dim_x+1, dim_x))
    sp[0] = array(x)
    root = cholesky((dim_x+kappa)*P).T
    sp[1:dim_x+1] = array(x) + root
    sp[dim_x+1:2*dim_x+2] = array(x) - root
    return sp


def unscented_transform(sp, weights, noise):

    x_bar = dot(weights, sp)
    num_sp, Nx = sp.shape
    P = zeros((Nx, Nx))

    for k in range(num_sp):
        P += weights[k] * outer(sp[k] - x_bar, sp[k] - x_bar)

    if noise is not None:
        P += array(noise)

    return x_bar, P
