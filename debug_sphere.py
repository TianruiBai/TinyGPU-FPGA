#!/usr/bin/env python3
"""Sample the sphere region densely from PPM."""
import sys

with open(sys.argv[1]) as f:
    magic = f.readline().strip()
    dims = f.readline().split()
    maxval = f.readline().strip()
    w, h = int(dims[0]), int(dims[1])
    vals = []
    for line in f:
        vals.extend(int(v) for v in line.split())

print(f"Dense sample of sphere region (rows 30-70, cols 40-90):")
print(f"  Sphere center approx at row 64, col 64")
print()

# Map colors
def cname(r, g, b):
    if r == 100 and g == 150 and b == 230: return 'SKY'
    if r == 0 and g == 0 and b == 0: return 'BLK'
    if r == 255 and g == 0 and b == 0: return 'RED'
    if r == 79 and g == 127 and b == 218: return 'MIR'
    if r == 204 and g == 0 and b == 0: return 'DRD'
    if r == 100 and g == 0 and b == 0: return 'drd'
    if r == 60 and g == 180 and b == 60: return 'GRN'
    if r == 15 and g == 50 and b == 15: return 'dGR'
    if r == 255 and g == 255 and b == 255: return 'WHT'
    return f'{r:02x}{g:02x}{b:02x}'

# Print the sphere region
for row in range(30, 72, 2):
    line = f"r{row:3d}: "
    for col in range(36, 96, 2):
        idx = (row * w + col) * 3
        r, g, b = vals[idx], vals[idx+1], vals[idx+2]
        c = cname(r, g, b)
        line += f"{c:>3s} "
    print(line)

print()
print("Sphere 2 region (rows 26-46, cols 80-112):")
for row in range(26, 46, 2):
    line = f"r{row:3d}: "
    for col in range(80, 112, 2):
        idx = (row * w + col) * 3
        r, g, b = vals[idx], vals[idx+1], vals[idx+2]
        c = cname(r, g, b)
        line += f"{c:>3s} "
    print(line)
