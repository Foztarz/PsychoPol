import neo
import matplotlib.pyplot as plt
import numpy as np
from scipy.optimize import curve_fit
import sys
from scipy.special import gamma

# instance of WinWcpIO with the given filename
filename = sys.argv[1]
reader = neo.io.WinWcpIO(filename)
start_time = float(sys.argv[2])  # start time for step function from command line
end_time = float(sys.argv[3])    # end time for step function from command line
# read data from the file into a Block object
block = reader.read_block()


# Gaussian function to fit
def gaussian(x, amp, mu, sigma, offset):
    return amp * np.exp(-((x - mu) ** 2) / (2 * sigma ** 2)) + offset

# plot a specific part of the signal and fit a Gaussian
def plot_signal_part_with_gaussian(signal_data, sampling_rate, part_number, num_parts=21):
    # n of samples per part
    total_samples = len(signal_data)
    samples_per_part = total_samples // num_parts

    # time axis for the whole signal
    total_time = total_samples / float(sampling_rate)
    time = np.linspace(0, total_time, total_samples)

    if part_number < 1 or part_number > num_parts:
        print(f"Invalid part number. Please choose a number between 1 and {num_parts}.")
        return

    # start and end indices for the selected part
    start_idx = (part_number - 1) * samples_per_part
    if part_number == num_parts:  # for the last part which may not perfectly divide
        end_idx = total_samples
    else:
        end_idx = start_idx + samples_per_part

    # get the specific part of the signal
    signal_part = signal_data[start_idx:end_idx]
    time_part = time[start_idx:end_idx]

    # Find the index corresponding to start_time in the time_part
    start_idx_fit = np.searchsorted(time_part, start_time)

    # Slice the signal and time from start_time to the end of the part
    signal_fit_part = signal_part[start_idx_fit:]
    time_fit_part = time_part[start_idx_fit:]

    # Check for the first and last value above 1000 in the fit region
    first_above_1000 = None
    last_above_1000 = None

    for i, value in enumerate(signal_fit_part):
        if value > 1000:
            first_above_1000 = (value, time_fit_part[i])
            print(f"First value above 1000: {value} at time {time_fit_part[i]:.3f} seconds")
            break

    for i, value in reversed(list(enumerate(signal_fit_part))):
        if value > 1000:
            last_above_1000 = (value, time_fit_part[i])
            print(f"Last value above 1000: {value} at time {time_fit_part[i]:.3f} seconds")
            break

    # fit the Gaussian to the signal data
    try:
        # initial guess for the parameters [amplitude, mean, std dev, offset]
        initial_guess = [np.max(signal_fit_part), time_fit_part[np.argmax(signal_fit_part)], np.std(time_fit_part), np.min(signal_fit_part)]
        popt, pcov, infodict, errmsg, ier = curve_fit(gaussian, time_fit_part, signal_fit_part, p0=initial_guess, full_output=True)

        fvec = infodict['fvec']

        # sum of squares of residuals
        sum_of_squares = np.sum(fvec ** 2)
        print("Sum of squares of residuals (fvec**2):", sum_of_squares)

        # fitted Gaussian parameters
        amp, mu, sigma, offset = popt

        # Gaussian fit
        gaussian_fit = gaussian(time_fit_part, *popt)

        # plot signal part
        plt.figure(figsize=(8, 6))
        plt.plot(time_fit_part, signal_fit_part, '-', label="Signal", markersize=4)
        plt.plot(time_fit_part, gaussian_fit, '--', label="Gaussian Fit", color='red')

        # vertical line at the mean (mu)
        plt.axvline(mu, color='green', linestyle='--', label=f'Mean (mu) = {mu:.3f} s')

        plt.xlabel('Time (s)')
        plt.ylabel('Amplitude')
        plt.title(f'Signal Part {part_number}/{num_parts} with Gaussian Fit and Mean')
        plt.legend()
        plt.grid(True)
        plt.show()

    except RuntimeError as e:
        print(f"Gaussian fit failed: {e}")


# Iterate over segments in the block and print details
for segment in block.segments:
    print(f"Segment: {segment}")

    # Iterate over analog signals in the segment and print details
    for signal in segment.analogsignals:
        print(f"Analog Signal: {signal}")
        print(f"Signal Shape: {signal.shape}")
        print(f"Signal Duration: {signal.duration}")
        print(f"Signal Sampling Rate: {signal.sampling_rate}")

        signal_data = np.array(signal).flatten()  # Flatten the signal to 1D

        part_number = 15  # Change this to any part number between 1 and 21

        plot_signal_part_with_gaussian(signal_data, signal.sampling_rate, part_number)
