import numpy as np
import matplotlib.pyplot as plt
import sys
from scipy.optimize import curve_fit

# usage: python hill_transformation_spec_sensitivity.py <intensity_response_data> <spec_mV_data>
# Hill equation
def hill_equation(I, Emax, Khalf, n):
    return Emax * (I**n) / (I**n + Khalf**n)

# data (response vs intensity data)
data = np.loadtxt(sys.argv[1])

# Emax values for normalization across recordings
emax_all = ['17.32222381416418','13.927460474315554','9.939365887537477','36.15404932783421','21.57367055613062',
            '10.087752683541495','28.70585723137354','19.693811204215915','31.633307805550935','18.898050784143944',
            '35.36402614970819','6.671551081563187','24.675935376963505','26.454229805051735','28.885503587362805',
            '11.974288990981092','30.923986106223747','26.27691456482146','13.76510960981805','31.983957479580695',
            '26.738349026504242','18.152266401723796','12.340686516695529','28.74574500380221','20.68114628893091',
            '18.59478606478243','15.121589436892366','15.137692810743678','19.274326170474488','29.499327061245474',
            '11.47555336012822','21.664150310674263','52.3221022666786','44.01914291371869','50.51234334445386',
            '46.52481219118933','47.114694582975595','49.15777696096988','50.84435992565729','45.15456637346612',
            '32.63803337290997','24.628947473492374','41.85579325872576','25.888677448858132']

emax_all = [float(emax) for emax in emax_all]

# intensity and response values, 2 columns
intensity_log = data[:, 1]
response_values = data[:, 0]

# ensure response values until saturation
max_response = np.max(response_values)
threshold = 0.95 * max_response
max_response_index = np.argmax(response_values)

# adjust response values for the threshold
below_threshold_index = None
for i in range(max_response_index + 1, len(response_values)):
    if response_values[i] < threshold:
        below_threshold_index = i
        break

if below_threshold_index is not None:
    response_values[below_threshold_index:] = max_response

# intensities from log scale to linear scale
intensity_linear = 10**intensity_log

# initial guess for curve fitting (Emax, Khalf, Hill slope)
initial_guess = (30, 0.03, 2)

# curve fitting to optimize parameters for the Hill equation
params, covariance = curve_fit(hill_equation, intensity_linear, response_values, p0=initial_guess)
Emax_opt, Khalf_opt, n_opt = params

print(f"Emax_opt = {Emax_opt}, Khalf_opt = {Khalf_opt}, n_opt = {n_opt}")

# relative sensitivity values using the optimized parameters
sensitivity_values = hill_equation(intensity_linear, Emax_opt, Khalf_opt, n_opt)

# plot of original response values and the Hill equation fit
plt.figure(figsize=(8, 6))
plt.scatter(intensity_log, sensitivity_values, color='blue', label='Hill transformed responses')
plt.scatter(intensity_log, response_values, color='red', label='Response Values')
plt.xlabel('Intensity (log scale)')
plt.ylabel('Response')
plt.title('PR responses vs Intensity')
plt.grid(True)
plt.legend()
plt.tight_layout()
plt.show()

# normalization, see Supplementary Methods
def normalize_mV(max_value, min_value, Emax_opt):
    mV_ratio = max_value / Emax_opt
    print(f'ratio max_response / Emax_opt: {mV_ratio}')
    if mV_ratio > 1.05:
        if max_value < 40:
            filtered_emax_list = [emax for emax in emax_all if max_value < float(emax) < 41]
            if len(filtered_emax_list) == 0:
                normalized_max = max_value / max_value # =1
                normalized_min = min_value / max_value
            else:
                average_below_40 = sum(filtered_emax_list) / len(filtered_emax_list)
                normalized_max = max_value / average_below_40
                normalized_min = min_value / average_below_40
        else:
            filtered_emax_list = [emax for emax in emax_all if float(emax) > max_value > 40]
            if len(filtered_emax_list) == 0:
                normalized_max = max_value / max_value
                normalized_min = min_value / max_value
            else:
                average_above_40 = sum(filtered_emax_list) / len(filtered_emax_list)
                normalized_max = max_value / average_above_40
                normalized_min = min_value / average_above_40
    elif mV_ratio < 1:
        print('yes')
        normalized_max = max_value / Emax_opt
        normalized_min = min_value / Emax_opt
    else:
        normalized_max = max_value / float(Emax_opt)
        normalized_min = min_value / float(Emax_opt)
    return normalized_max, normalized_min

# spec sensitivity based on normalized values and Hill parameters
def compute_spec_sensitivity(normalized_value, Khalf_opt, n_opt):
    if normalized_value == 1:
        return 1
    else:
        return Khalf_opt * (normalized_value / (1 - normalized_value)) ** (1 / n_opt)

# mV values from the input text file, one per line
mV_values = np.loadtxt(sys.argv[2]) 

# spec sensitivity for each mV value
spec_sensitivities = []
for max_value in mV_values:
    normalized_value, _ = normalize_mV(max_value, max_value, Emax_opt)
    spec_sensitivity = compute_spec_sensitivity(normalized_value, Khalf_opt, n_opt)
    spec_sensitivities.append(spec_sensitivity)

# output the spectral sensitivities
for i, sensitivity in enumerate(spec_sensitivities):
    print(f"mV value: {mV_values[i]}, spec sensitivity: {sensitivity}")
