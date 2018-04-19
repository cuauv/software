import numpy as np

# Ported from original BCF implementation in MATLAB
def shape_context(cont, n_ref=5, n_dist=5, n_theta=12, b_tangent=1):
    n_pt = cont.shape[0]
    X = np.array([cont[:, 0]]).transpose()
    Y = np.array([cont[:, 1]]).transpose()
    # Set reference point
    si = np.round(np.linspace(1, n_pt, n_ref)).astype('int32') - 1
    V = cont[si, :]
    vx = np.array([V[:, 0]])
    vy = np.array([V[:, 1]])
    # Orientations and geodesic distances between all landmark points
    Xs = np.tile(X, (1, n_ref))
    vxs = np.tile(vx, (n_pt, 1))
    dXs = Xs - vxs
    Ys = np.tile(Y, (1, n_ref))
    vys = np.tile(vy, (n_pt, 1))
    dYs = Ys - vys
    dis_mat = np.sqrt(dXs**2 + dYs**2)
    ang_mat = np.arctan2(dYs, dXs)

    # Normalize shape context with the tangent orientation
    if b_tangent:
        Xs = np.append(np.append([X[-1]], X, axis=0), [X[0]], axis=0)
        Ys = np.append(np.append([Y[-1]], Y, axis=0), [Y[0]], axis=0)
        gX = np.gradient(Xs, axis=0)
        gY = np.gradient(Ys, axis=0)

        thetas = np.arctan2(gY, gX)
        thetas = thetas[1:-1]
        thetas = np.tile(thetas, (1, n_ref))

        ang_mat = ang_mat - thetas
        idx = np.where(ang_mat > np.pi)
        ang_mat[idx] = ang_mat[idx] - 2 * np.pi
        idx = np.where(ang_mat < -np.pi)
        ang_mat[idx] = ang_mat[idx] + 2 * np.pi

    # Compute shape context
    sc = shape_context_core(dis_mat, ang_mat, n_dist, n_theta)
    return sc, V, dis_mat, ang_mat

def shape_context_core(dists, angles, n_dist=10, n_theta=16):
    n_pts = dists.shape[1]
    # Using log distances
    logbase = 1.5
    mean_dis = np.mean(dists.flatten())
    b = 1
    a = (logbase ** (0.75 * n_dist) - b) / mean_dis

    dists = np.floor(np.log(a * dists + b) / np.log(logbase))
    dists = np.maximum(dists, np.resize(1, dists.shape))
    dists = np.minimum(dists, np.resize(n_dist, dists.shape))

    # Preprocessing angles
    delta_ang = 2 * np.pi / n_theta
    angles = np.ceil((angles + np.pi) / delta_ang)
    angles = np.maximum(angles, np.resize(1, angles.shape))
    angles = np.minimum(angles, np.resize(n_theta, angles.shape))

    # Shape context
    sc_hist = np.zeros((n_theta * n_dist, n_pts))
    sctmp = np.zeros((n_dist, n_theta))

    for v in range(n_pts):
        for dis in range(n_dist):
            for ang in range(n_theta):
                sctmp[dis, ang] = np.count_nonzero(np.logical_and(dists[:, v] == dis + 1, angles[:, v] == ang + 1))
        sc_hist[:, v] = sctmp.flatten(order='F')
    sc_hist /= dists.shape[0]
    return sc_hist

if __name__ == "__main__":
    cont = np.array([
   [ 6.0000,    5.8000],
   [15.2614,    6.8000],
   [26.5275,    6.8000],
   [35.3747,    8.8000],
   [43.8640,    7.9360],
   [54.6595,   10.8000],
   [63.3583,   13.1583],
   [71.9397,   11.8000],
   [82.5597,   15.3597],
   [91.1588,   17.9588]
   ])
    print(shape_context(cont))
