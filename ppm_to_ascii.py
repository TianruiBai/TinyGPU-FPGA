#!/usr/bin/env python3
"""Generate ASCII art visualization of PPM frame."""
import sys

def ppm_to_ascii(path, out_path):
    with open(path) as f:
        magic = f.readline().strip()
        dims = f.readline().split()
        maxval = f.readline().strip()
        w, h = int(dims[0]), int(dims[1])
        vals = []
        for line in f:
            vals.extend(line.split())

    # Map RGB to characters
    def pixel_char(r, g, b):
        if r == 100 and g == 150 and b == 230: return '~'  # sky
        if r == 0 and g == 0 and b == 0: return '#'        # black checker
        if r == 255 and g == 0 and b == 0: return 'R'      # red checker
        if r == 79 and g == 127 and b == 218: return 'O'   # mirror (sky reflect)
        if r == 204 and g == 0 and b == 0: return 'r'      # dark shadow
        if r == 100 and g == 0 and b == 0: return 'd'      # darker shadow
        if r == 60 and g == 180 and b == 60: return 'G'    # green sphere
        if r == 15 and g == 50 and b == 15: return 'g'     # dark green
        if r == 255 and g == 255 and b == 255: return '*'   # specular
        return '?'

    with open(out_path, 'w') as out:
        out.write(f"Legend: ~=sky #=black R=red O=mirror r=shadow d=dark_shadow G=green g=dark_green\n\n")
        # Downsample 2:1 for readability
        for row in range(0, h, 2):
            line = ""
            for col in range(0, w, 2):
                idx = (row * w + col) * 3
                r, g, b = int(vals[idx]), int(vals[idx+1]), int(vals[idx+2])
                line += pixel_char(r, g, b)
            out.write(line + "\n")
    print(f"Written {out_path}")

for i, path in enumerate(sys.argv[1:]):
    ppm_to_ascii(path, f"rt_frame_{i}_ascii.txt")
