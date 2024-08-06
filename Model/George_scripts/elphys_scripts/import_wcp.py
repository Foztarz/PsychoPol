import neo
import matplotlib.pyplot as plt
import numpy as np
import sys

# Create an instance of WinWcpIO with the given filename
filename = sys.argv[1]
reader = neo.io.WinWcpIO(filename)

# Read the data from the file into a Block object
block = reader.read_block()

# Display information about the Block
#print(block)

# Iterate over segments in the block and print details
for segment in block.segments:
    print(f"Segment: {segment}")

    # Iterate over analog signals in the segment and print details
    for signal in segment.analogsignals:
        print(f"Analog Signal: {signal}")
        print(f"Signal Shape: {signal.shape}")
        print(f"Signal Duration: {signal.duration}")
        print(f"Signal Sampling Rate: {signal.sampling_rate}")

        # Convert the signal to a NumPy array
        signal_data = np.array(signal)

        # Extracting the signal data and plotting
        if signal.shape[1] == 1:
            signals = signal_data.flatten()  # Flatten the signal array to 1D
            time = np.arange(len(signals)) / signal.sampling_rate  # Time axis
            plt.figure(figsize=(8, 6))
            plt.plot(time, signals, '-', markersize=4)
            plt.xlabel('Time')
            plt.ylabel('Amplitude')
            plt.title('Signal vs Time')
            plt.grid(True)
            plt.show()
        elif signal.shape[1] == 2:
            signals = signal_data[:, 0].flatten()  # Flatten the first channel of the signal array to 1D
            wavelengths = signal_data[:, 1].flatten()  # Flatten the second channel of the signal array to 1D
            plt.figure(figsize=(8, 6))
            plt.plot(signals, wavelengths, '-', markersize=4)
            plt.xlabel('Time')
            plt.ylabel('Amplitude (mV)')
            plt.title('Signal vs Time')
            plt.grid(True)
            plt.show()
