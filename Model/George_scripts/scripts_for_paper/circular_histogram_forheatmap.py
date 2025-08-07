import sys
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statistics
import os

def polar_to_cartesian(angle_deg, length):
    angle_rad = np.radians(angle_deg)
    x = length * np.cos(angle_rad)
    y = length * np.sin(angle_rad)
    return x, y

def min_angle_difference(a_deg, b_deg):
    return abs((a_deg - b_deg + 180) % 360 - 180)

def main():
    if len(sys.argv) < 6:
        print("Usage: python plot_eye_vectors.py <data.tsv> <sun_azimuth_deg> <sun_elevation> <output.svg> <output.tsv>")
        sys.exit(1)

    # Parse command-line arguments
    input_file = sys.argv[1]
    sun_azimuth_deg = float(sys.argv[2])
    sun_elevation = float(sys.argv[3])
    output_svg = sys.argv[4]
    output_tsv = sys.argv[5]

    # Load data
    df = pd.read_csv(input_file, sep='\t')

    combined_angles = []
    combined_lengths = []
    absolute_errors = []
    output_rows = []
    errors = []

    for i, row in df.iterrows():
        try:
            # Convert both eye vectors from polar to cartesian
            x1, y1 = polar_to_cartesian(row['First Eye Angle'], row['First Eye Vector Length'])
            x2, y2 = polar_to_cartesian(row['Second Eye Angle'], row['Second Eye Vector Length'])

            # Combine vectors
            x_combined = x1 + x2
            y_combined = y1 + y2

            total_vector_angle = np.arctan2(y_combined, x_combined)  # Radians
            length_combined = np.hypot(x_combined, y_combined)

            # Compensate for sun movement in polar histogram
            angle_with_offset = -total_vector_angle - np.radians(i * 5)
            combined_angles.append(angle_with_offset)
            combined_lengths.append(length_combined)

            # Calculate true sun azimuth for this row
            rotation_angle = i * 5
            true_sun_deg = -sun_azimuth_deg - rotation_angle  # rotated sun
            estimated_angle_deg = np.degrees(total_vector_angle)

            # Calculate absolute angular error
            error_deg = min_angle_difference(true_sun_deg, estimated_angle_deg)
            absolute_errors.append(error_deg)

            # Store output row
            output_rows.append({
                'sun_elevation': sun_elevation,
                'sun_azimuth': true_sun_deg,
                'error': error_deg
            })

        except Exception as e:
            errors.append(f"Row {i}: {e}")

    # Compute and print mean absolute angular error
    mean_abs_error = round(statistics.mean(absolute_errors), 2)
    print(f"\nMean Absolute Angular Error: {mean_abs_error:.2f}°")

    # Convert angles for histogram binning
    combined_angles_deg = np.degrees(combined_angles) % 360
    bin_edges = np.arange(0, 360 + 5, 5)
    bin_centers = np.radians(bin_edges[:-1] + 2.5)
    counts, _ = np.histogram(combined_angles_deg, bins=bin_edges)
    sqrt_counts = np.sqrt(counts)

    # Create polar histogram
    fig = plt.figure(figsize=(8, 10))
    ax = fig.add_subplot(111, polar=True)

    ax.set_theta_zero_location('N')
    ax.set_theta_direction(1)
    ax.grid(False)
    ax.set_xticks([])
    ax.set_yticks([])

    ax.bar(
        bin_centers,
        sqrt_counts,
        width=np.radians(5),
        bottom=0,
        color='cornflowerblue',
        align='center'
    )

    # Optional: highlight original sun azimuth
    angle_rad = np.radians(sun_azimuth_deg)
    max_radius = max(sqrt_counts) if len(sqrt_counts) > 0 else 1
    ax.plot(angle_rad, max_radius, marker='o', color='orange', markersize=20)

    ax.set_frame_on(True)
    plt.tight_layout()
    plt.savefig(output_svg, format="svg")
    plt.show()

    # Save results to TSV (append or create)
    output_df = pd.DataFrame(output_rows)
    file_exists = os.path.exists(output_tsv)
    output_df.to_csv(output_tsv, sep='\t', mode='a', index=False, header=not file_exists)
    print(f"Results saved to: {output_tsv}")

    # Report row-level errors, if any
    if errors:
        print("\nErrors encountered during processing:")
        for err in errors:
            print(err)

if __name__ == "__main__":
    main()
