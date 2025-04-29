import neo
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import sys
import os
        
# ensure the filename is provided as an argument
if len(sys.argv) < 2:
    print("Usage: python script.py <filename>")
    sys.exit(1)

# filename from the command line argument
filename = sys.argv[1]

# instance of WinWcpIO with the given filename
reader = neo.io.WinWcpIO(filename)

# list to store filtered response arrays
filtered_responses_list = []

# read the data from the file into a Block object
block = reader.read_block()

# iterate over segments in the block
min_overall_response = 0 # initiate a minimum response variable
for segment in block.segments:
    # ensure segment has at least two analog signals
    if len(segment.analogsignals) >= 2:
        stimulus_signal = None
        response_signal = None
        # find pA and mV signals in the segment
        for signal in segment.analogsignals:
            if "pA" in str(signal.units):
                stimulus_signal = signal
            elif "mV" in str(signal.units):
                response_signal = signal

        # proceed if both stimulus and response signals are found
        if stimulus_signal is not None and response_signal is not None:
            # convert signals to NumPy arrays
            response_data = np.array(response_signal).flatten()
            
            min_local_response = min(response_data)
            
            if min_overall_response > min_local_response:
                min_overall_response = min_local_response
            
            # size of each part
            part_size = len(response_data) // 21
            
            # find the maximum value in each part and store in filtered_responses
            filtered_responses = []
            for i in range(21):
                part_data = response_data[i * part_size : (i + 1) * part_size]
                max_value = np.max(part_data)
                filtered_responses.append(max_value)

            # append filtered responses to the list
            filtered_responses_list.append(filtered_responses)

# convert the list of arrays to a 2D NumPy array
all_responses_array = np.array(filtered_responses_list)

all_responses_array = all_responses_array - min_overall_response # subtract noise

# prints the maximum values of each part in the 21*21 grid
print(all_responses_array)

