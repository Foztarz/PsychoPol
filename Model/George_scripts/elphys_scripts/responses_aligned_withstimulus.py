import neo
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import sys

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
            stimulus_data = np.array(stimulus_signal).flatten()
            response_data = np.array(response_signal).flatten()
            min_value = np.min(response_data)
            response_data = response_data - min_value
            sampling_rate = stimulus_signal.sampling_rate

            # Filter response data where stimulus is greater than 4500
            filtered_responses = response_data[stimulus_data > 4500]

            # Append filtered responses to the list
            filtered_responses_list.append(filtered_responses)

# Convert the list of arrays to a 2D NumPy array
all_responses_array = np.vstack(filtered_responses_list)


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
plt.title('Volcano Plot of Filtered Responses')

# Add color bar
fig.colorbar(surf, shrink=0.5, aspect=5)

plt.show()
