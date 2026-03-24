#!/usr/bin/env python3
"""Quick PPM analysis script for ray-tracing testbench output."""
import sys

def analyze(path):
    with open(path) as f:
        magic = f.readline().strip()
        dims = f.readline().split()
        maxval = f.readline().strip()
        w, h = int(dims[0]), int(dims[1])
        vals = []
        for line in f:
            vals.extend(line.split())

    print(f"File: {path}")
    print(f"Format: {magic}, {w}x{h}, max={maxval}, total values={len(vals)}")
    print()

    # Sample pixels at a grid
    print("Sample pixels (row, col) -> R G B:")
    for row in [0, 16, 32, 48, 64, 80, 96, 112, 127]:
        for col in [0, 32, 64, 96, 127]:
            idx = (row * w + col) * 3
            r, g, b = int(vals[idx]), int(vals[idx+1]), int(vals[idx+2])
            print(f"  [{row:3d},{col:3d}] R={r:3d} G={g:3d} B={b:3d}")
        print()

    # Color histogram - count unique colors
    colors = {}
    for i in range(0, len(vals), 3):
        c = (int(vals[i]), int(vals[i+1]), int(vals[i+2]))
        colors[c] = colors.get(c, 0) + 1

    print(f"Unique colors: {len(colors)}")
    print("Top 15 most common colors:")
    for c, cnt in sorted(colors.items(), key=lambda x: -x[1])[:15]:
        pct = 100.0 * cnt / (w * h)
        print(f"  R={c[0]:3d} G={c[1]:3d} B={c[2]:3d}  count={cnt:5d} ({pct:5.1f}%)")

    # Check specific expected colors
    print()
    print("Expected color presence:")
    red = (255, 0, 0)
    black = (0, 0, 0)
    sky = (100, 150, 230)
    white = (255, 255, 255)
    green_like = any(g > 100 and r < 100 and b < 100 for (r, g, b) in colors)
    print(f"  Red (255,0,0):     {'YES' if red in colors else 'NO'} ({colors.get(red, 0)} pixels)")
    print(f"  Black (0,0,0):     {'YES' if black in colors else 'NO'} ({colors.get(black, 0)} pixels)")
    print(f"  Sky (100,150,230): {'YES' if sky in colors else 'NO'} ({colors.get(sky, 0)} pixels)")
    print(f"  White (255,255,255): {'YES' if white in colors else 'NO'} ({colors.get(white, 0)} pixels)")
    print(f"  Green-ish (g>100,r<100,b<100): {'YES' if green_like else 'NO'}")

for path in sys.argv[1:]:
    analyze(path)
    print("=" * 60)
