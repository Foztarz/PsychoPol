import math
import sys

def calculate_distance(x, y, center_x=640, center_y=360):
    return math.sqrt((x - center_x) ** 2 + (y - center_y) ** 2)

def process_tsv(input_file, output_file):
    with open(input_file, 'r') as infile, open(output_file, 'w') as outfile:
        for line in infile:
            parts = line.strip().split(',') 
            if len(parts) == 2:
                try:
                    x, y = map(float, parts)
                    distance = calculate_distance(x, y)
                    outfile.write(f"{distance}\n")
                except ValueError:
                    print(f"Skipping invalid line: {line.strip()}")

if __name__ == "__main__":
    input_filename = sys.argv[1]  
    output_filename = sys.argv[2]
    process_tsv(input_filename, output_filename)
    print("Processing complete. Distances saved to", output_filename)
