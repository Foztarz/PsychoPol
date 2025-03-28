import numpy as np
import sys

file_path = sys.argv[1]

data = np.load(file_path, allow_pickle=True)

for i, row in enumerate(data, start=0):
    print(f"image {i}: {row}")

