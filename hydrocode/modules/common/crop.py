def find_bounds(L_total, L_crop, center):
    assert L_crop >= 0, 'Crop length must be at least 0'
    assert L_total >= L_crop, (
        'Total length must be at least as large as crop length')

    end = max(min(center + L_crop // 2, L_total), L_crop)
    start = end - L_crop

    return (start, end)