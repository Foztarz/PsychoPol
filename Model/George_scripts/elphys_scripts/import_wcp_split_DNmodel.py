import neo
import matplotlib.pyplot as plt
import numpy as np
from scipy.optimize import curve_fit
import sys

numb=0
# instance of WinWcpIO with the given filename
filename = sys.argv[1]
start_time = float(sys.argv[2])  # start time for step function from command line
end_time = float(sys.argv[3])    # end time for step function from command line

reader = neo.io.WinWcpIO(filename)

# read data from the file into a Block object
block = reader.read_block()

# manual convolution
def manual_convolution(f, g):
    len_f = len(f)
    len_g = len(g)
    result = np.zeros(len_f)

    for i in range(len_f):
        sum_value = 0.0
        for j in range(len_g):
            if i - j >= 0:  # Ensure valid index
                sum_value += f[i - j] * g[j]
        result[i] = sum_value

    return result

def compute_R_DN(T_input, t, tau1, sigma, n, amp, offset):
    tau2 = 2*tau1
    #print(t)
    t = t - start_time # the time array has to start from 0 !!!

    h1_values = t * np.exp(-t / tau1)

    R_L = manual_convolution(T_input, h1_values)

    R_LN = np.abs(R_L) ** n
    h2_values = np.exp(-t / tau2)

    normalization_term = manual_convolution(np.abs(R_LN), h2_values)

    R_DN = amp*np.abs(R_LN)**n / (sigma ** n + normalization_term ** n) + offset

    return R_DN

def create_step_function(t, start_time, end_time):
    step_func = np.where((t >= start_time) & (t <= end_time), 1, 0)

    return step_func

def fit_R_DN(t, signal_data, start_time, end_time):
    def fitting_function(tau1, sigma, n, amp, offset):
        T_input = create_step_function(t, start_time, end_time)
        return compute_R_DN(T_input, t, tau1, sigma, n, amp, offset)

    tau1_initial = 0.04
    sigma_initial = 1
    n_initial = 1
    amp_initial = 250
    offset_initial = 2

    # Use curve_fit to fit all three parameters (tau1, sigma, n)
    popt, _ = curve_fit(lambda t, tau1, sigma, n, amp, offset: fitting_function(tau1, sigma, n, amp, offset),
                        t, signal_data, p0=[tau1_initial, sigma_initial, n_initial,amp_initial, offset_initial], maxfev = 5000)

    # Return the fitted parameters tau1, sigma, and n
    tau1_opt, sigma_opt, n_opt, amp_opt, offset_opt = popt
    return tau1_opt, sigma_opt, n_opt, amp_opt, offset_opt

def plot_signal_part_with_R_DN(signal_data, sampling_rate, part_number, num_parts=21):
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

    # fit the R_DN to the signal data (only from start_time to the end of the part)
    try:
        tau1_opt, sigma_opt, n_opt, amp_opt, offset_opt = fit_R_DN(time_fit_part, signal_fit_part, start_time, end_time)


        # create the input step function and compute the fitted data
        T_input = create_step_function(time_fit_part, start_time, end_time)
        fitted_data = compute_R_DN(T_input, time_fit_part, tau1_opt, sigma_opt, n_opt, amp_opt, offset_opt)
        max_index = np.argmax(fitted_data)
        max_value = fitted_data[max_index]
        max_time = time_fit_part[max_index]

        # plot signal part (restricted to the fitting time window)
        plt.figure(figsize=(8, 6))
        plt.plot(time_fit_part, signal_fit_part, '-', label="Signal", markersize=4)
        plt.plot(time_fit_part, fitted_data, '--', label=f"R_DN Fit (tau1 = {tau1_opt:.4f})", color='red')

        # vertical line at the start and end of the step function
        plt.axvline(start_time, color='green', linestyle='--', label=f'Start Time = {start_time:.3f} s')
        plt.axvline(end_time, color='purple', linestyle='--', label=f'End Time = {end_time:.3f} s')
        plt.axvline(max_time, color='black', linestyle='--', label=f'Max Value at {max_time:.3f} s')

        plt.xlabel('Time (s)')
        plt.ylabel('Amplitude')
        plt.title(f'Signal Part {part_number}/{num_parts} with R_DN Fit (from start_time)')
        plt.legend()
        plt.grid(True)
        plt.show()

        print(f"Fitted tau1: {tau1_opt:.4f}, sigma: {sigma_opt:.4f}, n: {n_opt:.4f}")

    except RuntimeError as e:
        print(f"R_DN fit failed: {e}")


# iterate over segments in the block and print details
for segment in block.segments:

    print(f"Segment: {segment}")

    for signal in segment.analogsignals:

        print(f"Analog Signal: {signal}")
        print(f"Signal Shape: {signal.shape}")
        print(f"Signal Duration: {signal.duration}")
        print(f"Signal Sampling Rate: {signal.sampling_rate}")

        signal_data = np.array(signal).flatten()  # flatten the signal to 1D

        # adjust the signal to be positive if needed
        shift_value = np.abs(np.min(signal_data))  # Find the minimum value in the signal
        signal_data = signal_data + shift_value  # Shift the entire signal to positive

        part_number = 12  # Change this to any part number between 1 and 21

        plot_signal_part_with_R_DN(signal_data, signal.sampling_rate, part_number)
