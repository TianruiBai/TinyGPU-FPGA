#!/usr/bin/env python3
"""Convert PPM to upscaled PNG for viewing."""
from PIL import Image

for i in range(3):
    fname = f'rt_frame_{i}.ppm'
    with open(fname) as f:
        magic = f.readline().strip()
        dims = f.readline().split()
        maxval = f.readline().strip()
        w, h = int(dims[0]), int(dims[1])
        vals = []
        for line in f:
            vals.extend(int(v) for v in line.split())

    img = Image.new('RGB', (w, h))
    for y in range(h):
        for x in range(w):
            idx = (y * w + x) * 3
            img.putpixel((x, y), (vals[idx], vals[idx+1], vals[idx+2]))

    out = f'rt_frame_{i}.png'
    img = img.resize((512, 512), Image.NEAREST)
    img.save(out)
    print(f'Saved {out}')
