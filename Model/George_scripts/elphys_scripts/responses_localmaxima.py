import neo
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import sys
import os

# Function definitions (as provided)

angles = np.linspace(0, 360, 21)[:-1]  # 20 angles between 0 and 360

def make_views(ax, angles, elevation=None, width=8, height=6, prefix='tmprot_', **kwargs):
    files = []
    ax.figure.set_size_inches(width, height)
    for i, angle in enumerate(angles):
        ax.view_init(elev=elevation, azim=angle)
        fname = f'{prefix}{i:03d}.jpeg'
        ax.figure.savefig(fname)
        files.append(fname)
    return files

def make_movie(files, output, fps=10, bitrate=1800, **kwargs):
    output_name, output_ext = os.path.splitext(output)
    command = {
        '.mp4': f'mencoder "mf://{" ".join(files)}" -mf fps={fps} -o {output_name}.mp4 -ovc lavc -lavcopts vcodec=libx264:vbitrate={bitrate}',
        '.ogv': f'ffmpeg -i {output_name}.mp4 -r {fps} {output}'
    }
    os.system(command[output_ext])

def make_gif(files, output, delay=100, repeat=True, **kwargs):
    loop = -1 if repeat else 0
    os.system(f'convert -delay {delay} -loop {loop} {" ".join(files)} {output}')

def make_strip(files, output, **kwargs):
    os.system(f'montage -tile 1x -geometry +0+0 {" ".join(files)} {output}')

def rotanimate(ax, angles, output, **kwargs):
    output_ext = os.path.splitext(output)[1]
    files = make_views(ax, angles, **kwargs)
    D = {'.mp4': make_movie, '.ogv': make_movie, '.gif': make_gif, '.jpeg': make_strip, '.png': make_strip}
    D[output_ext](files, output, **kwargs)
    for f in files:
        os.remove(f)
        
# Ensure the filename is provided as an argument
if len(sys.argv) < 2:
    print("Usage: python script.py <filename>")
    sys.exit(1)

# Get the filename from the command line arguments
filename = sys.argv[1]

# Create an instance of WinWcpIO with the given filename
reader = neo.io.WinWcpIO(filename)

# Initialize a list to store filtered response arrays
filtered_responses_list = []

# Read the data from the file into a Block object
block = reader.read_block()

# Iterate over segments in the block
for segment in block.segments:
    # Ensure segment has at least two analog signals
    if len(segment.analogsignals) >= 2:
        stimulus_signal = None
        response_signal = None
        # Find pA and mV signals in the segment
        for signal in segment.analogsignals:
            if "pA" in str(signal.units):
                stimulus_signal = signal
            elif "mV" in str(signal.units):
                response_signal = signal

        # Proceed if both stimulus and response signals are found
        if stimulus_signal is not None and response_signal is not None:
            # Convert signals to NumPy arrays
            response_data = np.array(response_signal).flatten()
            min_value = np.min(response_data)
            response_data = response_data - min_value

            # Determine the size of each part
            part_size = len(response_data) // 21

            # Find the maximum value in each part and store in filtered_responses
            filtered_responses = []
            for i in range(21):
                part_data = response_data[i * part_size : (i + 1) * part_size]
                max_value = np.max(part_data)
                filtered_responses.append(max_value)

            # Append filtered responses to the list
            filtered_responses_list.append(filtered_responses)

# Convert the list of arrays to a 2D NumPy array
all_responses_array = np.array(filtered_responses_list)

# Normalize the responses
max_value = np.max(all_responses_array)
all_responses_array = all_responses_array / max_value

# Create a meshgrid for plotting
time_grid, trial_grid = np.meshgrid(np.arange(all_responses_array.shape[1]), np.arange(all_responses_array.shape[0]))

# Create a 3D plot
fig = plt.figure(figsize=(10, 6))
ax = fig.add_subplot(111, projection='3d')

# Plot the surface
surf = ax.plot_surface(time_grid, trial_grid, all_responses_array, cmap='coolwarm')

# Add labels and title
ax.set_xlabel('Time (samples)')
ax.set_ylabel('Trial')
ax.set_zlabel('Response Amplitude')
plt.title(f'Volcano Plot of Filtered Responses from {os.path.basename(filename)}')

# Add color bar
fig.colorbar(surf, shrink=0.5, aspect=5)

#plt.show()

# Create an animated gif (20ms between frames)
rotanimate(ax, angles, f'{os.path.basename(filename)}.gif', delay=20)
