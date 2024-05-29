import neo
import matplotlib.pyplot as plt
import numpy as np
from mpl_toolkits.mplot3d import Axes3D
from scipy.interpolate import griddata
import os, sys

# Function definitions (as provided)

angles = np.linspace(0, 360, 21)[:-1]  # 20 angles between 0 and 360

def make_views(ax, angles, elevation=None, width=4, height=3, prefix='tmprot_', **kwargs):
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
        '.mp4': f'mencoder "mf://{" ".join(files)}" -mf fps={fps} -o {output_name}.mp4 -ovc lavc -lavcopts vcodec=msmpeg4v2:vbitrate={bitrate}',
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

# Check if a filename is provided as an argument
if len(sys.argv) != 2:
    print("Usage: python script.py <filename>")
    sys.exit(1)

# Create an instance of WinWcpIO with the given filename
filename = sys.argv[1]
reader = neo.io.WinWcpIO(filename)

# Read the data from the file into a Block object
block = reader.read_block()

# Initialize a list to store "Analog Signal 1" arrays
analog_signal_1_list = []

# Iterate over segments in the block and extract "Analog Signal 1"
for seg_idx, segment in enumerate(block.segments):
    for sig_idx, signal in enumerate(segment.analogsignals):
        if sig_idx == 1:  # Assuming "Analog Signal 1" corresponds to the second signal
            signal_data = np.array(signal).flatten()
            analog_signal_1_list.append(signal_data)

# Convert the list to a 2D NumPy array
analog_signal_1_array = np.vstack(analog_signal_1_list)

# Normalize the 2D array by its minimum and maximum values
min_value = np.min(analog_signal_1_array)
analog_signal_1_array = analog_signal_1_array - min_value
max_value = np.max(analog_signal_1_array)
normalized_array = analog_signal_1_array / max_value

# Interpolate data to a higher resolution grid
x = np.arange(normalized_array.shape[1])
y = np.arange(normalized_array.shape[0])
x, y = np.meshgrid(x, y)
z = normalized_array

x_high_res = np.linspace(x.min(), x.max(), 200)
y_high_res = np.linspace(y.min(), y.max(), 200)
x_high_res, y_high_res = np.meshgrid(x_high_res, y_high_res)

z_high_res = griddata((x.flatten(), y.flatten()), z.flatten(), (x_high_res, y_high_res), method='cubic')

# Generate a 3D heatmap (volcano plot)
fig = plt.figure(figsize=(10, 6))
ax = fig.add_subplot(111, projection='3d')

# Plot the surface
ax.plot_surface(x_high_res, y_high_res, z_high_res, cmap='coolwarm', edgecolor='none')

# Set plot labels
ax.set_xlabel('Time (samples)')
ax.set_ylabel('Segment Index')
ax.set_zlabel('Normalized Amplitude (mV)')
ax.set_title('3D Heatmap (Volcano Plot) of Analog Signal 1')

plt.show()

plt.axis('off')  # remove axes for visual appeal

# Create an animated gif (20ms between frames)
rotanimate(ax, angles, 'movie.gif', delay=20)

# Create a movie with 10 frames per second and 'quality' 2000
rotanimate(ax, angles, 'movie.mp4', fps=10, bitrate=2000)

# Create an ogv movie
rotanimate(ax, angles, 'movie.ogv', fps=10)
