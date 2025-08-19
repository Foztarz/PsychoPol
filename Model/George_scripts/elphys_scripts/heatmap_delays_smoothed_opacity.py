import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
import sys
import scipy.ndimage
from matplotlib.patches import Ellipse

## usage: python heatmap_delays_smoothed_opacity.py <delays_matrix> <spatial_sensitivity_matrix>

# original data and sum-of-squares data OR spatial sensitivity data/matrix
file_path = sys.argv[1]  # path to the first Excel file
ss_path = sys.argv[2]  # path to the sum-of-squares file OR spatial sensitivity data/matrix if you want to scale by that

# load both matrices
df = pd.read_excel(file_path, header=None)
df_ss = pd.read_excel(ss_path, header=None)

# convert to numpy array and create mask
data_array = df.to_numpy()
mask = ~np.isnan(data_array)
data_array_filled = np.nan_to_num(data_array, nan=0.0)

# Gaussian filter to the original data
smoothed_data = scipy.ndimage.gaussian_filter(data_array_filled, sigma=1)
smoothed_mask = scipy.ndimage.gaussian_filter(mask.astype(float), sigma=1)
smoothed_mask[smoothed_mask == 0] = np.nan
filtered_result = smoothed_data / smoothed_mask

# Gaussian filter to the sum-of-squares data for opacity calculation
data_array_ss = df_ss.to_numpy()
mask_ss = ~np.isnan(data_array_ss)
data_array_ss_filled = np.nan_to_num(data_array_ss, nan=0.0)

smoothed_data_ss = scipy.ndimage.gaussian_filter(data_array_ss_filled, sigma=1)
smoothed_mask_ss = scipy.ndimage.gaussian_filter(mask_ss.astype(float), sigma=1)
smoothed_mask_ss[smoothed_mask_ss == 0] = np.nan
filtered_result_ss = smoothed_data_ss / smoothed_mask_ss

# normalize opacity: Max opacity corresponds to the max value in filtered_result_ss, min opacity corresponds to min value
max_opacity = np.nanmax(filtered_result_ss)
min_opacity = np.nanmin(filtered_result_ss)
opacity_map = (filtered_result_ss - min_opacity) / (max_opacity - min_opacity)
opacity_map[np.isnan(opacity_map)] = 0  # NaNs have zero opacity

# plot positions with sensitivity > 0.5, run hill_transformation_contour_likelihood_gaussians.py to get these
##positions = [             ## for 240527_011_bombus
##    [4, 14], [4, 15], [4, 20], [5, 13], [5, 14], [5, 15], [5, 16], [5, 17],
##    [5, 18], [5, 19], [5, 20], [6, 13], [6, 14], [6, 15], [6, 16], [6, 17],
##    [6, 18], [6, 19], [7, 12], [7, 13], [7, 14], [7, 15], [7, 16], [7, 17],
##    [7, 18], [8, 12], [8, 13], [8, 14], [8, 15], [8, 16], [8, 17], [9, 14],
##    [9, 15], [9, 16], [11, 9], [11, 10], [12, 8], [12, 9], [12, 10], [12, 11],
##    [12, 12], [13, 7], [13, 8], [13, 9], [13, 10], [13, 11], [13, 12], [13, 13],
##    [14, 7], [14, 8], [14, 9], [14, 10], [14, 11], [14, 12], [14, 13], [14, 14],
##    [14, 15], [14, 16], [15, 7], [15, 8], [15, 9], [15, 10], [15, 11], [15, 12],
##    [15, 13], [15, 14], [15, 15], [15, 16], [16, 8], [16, 9], [16, 10], [16, 11],
##    [16, 12], [16, 13], [16, 14], [16, 15], [17, 11], [17, 12], [17, 13], [17, 14]
##]

##positions = [         ## for 240529_017_bombus
##    [12, 10], [12, 11], [12, 12], [12, 13], [12, 14], [12, 15], [12, 16], [12, 17],[13, 6], [13, 7], [13, 8], [13, 9], [13, 10], [13, 11], [13, 12], [13, 13],
##    [13, 14], [13, 15], [13, 16], [13, 17], [13, 18], [13, 19], [13, 20],[14, 5], [14, 6], [14, 7], [14, 8], [14, 9], [14, 10], [14, 11], [14, 12],
##    [14, 13], [14, 14], [14, 15], [14, 16], [14, 17], [14, 18], [14, 19], [14, 20],[15, 5], [15, 6], [15, 7], [15, 8], [15, 9], [15, 10], [15, 11], [15, 12],[15, 13], [15, 14], [15, 15], [15, 16], [15, 17], [15, 18], [15, 19],
##    [16, 6], [16, 7], [16, 8], [16, 9], [16, 10], [16, 11], [16, 12], [16, 13],
##    [16, 14], [16, 15], [16, 16],
##    [17, 7], [17, 8], [17, 9], [17, 10], [17, 11], [17, 12], [17, 13], [17, 14],[17, 15], [17, 16],
##    [18, 8], [18, 9], [18, 10], [18, 11], [18, 12], [18, 13], [18, 14], [18, 15],[18, 16], [18, 17],
##    [19, 11], [19, 12], [19, 13], [19, 14], [19, 15], [19, 16], [19, 17], [19, 18],[19, 19], [19, 20],
##    [20, 11], [20, 12], [20, 13], [20, 14], [20, 15], [20, 16], [20, 17], [20, 18],
##    [20, 19], [20, 20]
##]

##positions = [       ## for 240530_021_bombus
##    [9, 10],[9, 11],[10, 9],[10, 10],[10, 11],
##    [10, 12],[11, 9],[11, 10],[11, 11],[11, 12],[12, 9],[12, 10],[12, 11],[16, 11],[16, 12],[16, 13],[16, 14],[16, 15],[16, 16],
##    [17, 11],[17, 12],[17, 13],[17, 14],
##    [17, 15],[17, 16],[18, 11],[18, 12],[18, 13],[18, 14],[18, 15],[19, 11],[19, 12],[19, 13]
##]

##positions = [         ## for 240524_012_apis
##    [5, 5], [6, 4], [6, 5], [6, 6], [6, 7], [7, 3], [7, 4], [7, 5], [7, 6], [7, 7], [8, 2], [8, 3], [8, 4], [8, 5], [8, 6], [8, 7], [8, 8], [9, 2], [9, 3], [9, 4],
##    [9, 5], [9, 6], [9, 7], [9, 8], [9, 9], [10, 2], [10, 3], [10, 4], [10, 5], [10, 6], [10, 7], [10, 8], [10, 9], [10, 10], [11, 3], [11, 4], [11, 5], [11, 6], [11, 7], 
##    [11, 8], [11, 9], [11, 10], [11, 11], [12, 3], [12, 4], [12, 5], [12, 6], [12, 7], [12, 8], [12, 9], [12, 10], [12, 11], [13, 4], [13, 5], [13, 6], [13, 7], [13, 8], 
##    [13, 9], [13, 10], [14, 9], [14, 10], [14, 11], [14, 12], [15, 7], [15, 8], [15, 9], [15, 10], [15, 11], [15, 12], [15, 13], [16, 5], [16, 6], [16, 7], [16, 8], [16, 9], 
##    [16, 10], [16, 11], [16, 12], [16, 13], [17, 5], [17, 6], [17, 7], [17, 8], [17, 9], [17, 10], [17, 11], [17, 12], [17, 13], [18, 4], [18, 5], [18, 6], [18, 7], [18, 8], 
##    [18, 9], [18, 10], [18, 11], [18, 12], [18, 13], [19, 5], [19, 6], [19, 7], [19, 8], [19, 9], [19, 10], [19, 11], [19, 12], [20, 6], [20, 7], [20, 8], [20, 9], [20, 10], 
##    [20, 11], [20, 12]
##]

# positions = [       ## for 240524_020_apis
#     [8, 13], [8, 14], [8, 15], [9, 6], [9, 7], [9, 8], [9, 9], [9, 10], [9, 11], [9, 12], [9, 13], [9, 14], [9, 15], [9, 16], [10, 4], [10, 5], [10, 6], [10, 7], [10, 8], [10, 9],
#     [10, 10], [10, 11], [10, 12], [10, 13], [10, 14], [10, 15], [10, 16], [10, 17], [11, 3], [11, 4], [11, 5], [11, 6], [11, 7], [11, 8], [11, 9], [11, 10], [11, 11], [11, 12], [11, 13], [11, 14],
#     [11, 15], [11, 16], [11, 17], [12, 3], [12, 4], [12, 5], [12, 6], [12, 7], [12, 8], [12, 9], [12, 10], [12, 11], [12, 12], [12, 13], [12, 14], [12, 15], [12, 16], [12, 17], [13, 5], [13, 6],
#     [13, 7], [13, 8], [13, 9], [13, 10], [13, 11], [13, 12], [13, 13], [13, 14], [13, 15], [13, 16], [14, 6], [14, 7], [14, 8], [14, 9], [14, 10], [14, 11], [14, 12], [14, 13], [14, 14], [15, 7],
#     [15, 8], [15, 9], [15, 10], [15, 11], [15, 12], [15, 13], [15, 14], [15, 15], [15, 16], [16, 8], [16, 9], [16, 10], [16, 11], [16, 12], [16, 13], [16, 14], [16, 15], [16, 16], [16, 17], [17, 8],
#     [17, 9], [17, 10], [17, 11], [17, 12], [17, 13], [17, 14], [17, 15], [17, 16], [17, 17], [18, 8], [18, 9], [18, 10], [18, 11], [18, 12], [18, 13], [18, 14], [18, 15], [18, 16], [18, 17], [19, 9],
#     [19, 10], [19, 11], [19, 12], [19, 13], [19, 14], [19, 15], [19, 16], [19, 17], [20, 11], [20, 12], [20, 13], [20, 14], [20, 15], [20, 16]
# ]

positions = [ ## for apis_250322_020
    [6, 9],[6, 10],[6, 11],[6, 12],[6, 13],[7, 7],[7, 8],[7, 9],[7, 10],[7, 11],[7, 12],[7, 13],[8, 7],[8, 8],[8, 9],[8, 10],[8, 11],[8, 12],[8, 13],[8, 14],[9, 7],[9, 8],
 [9, 9],[9, 10],[9, 11],[9, 12],[9, 13],[9, 14],[10, 7],[10, 8],[10, 9],[10, 10],[10, 11],[10, 12],[10, 13],[10, 14],[11, 7],[11, 8],[11, 9],[11, 10],[11, 11],[11, 12],[11, 13],
 [12, 9],[12, 10],[12, 11],[12, 12],[12, 13],[13, 10],[13, 11],[13, 12],[13, 13],[14, 9],[14, 10],[14, 11],[14, 12],[15, 9],[15, 10],[15, 11],[15, 12],[16, 9],
 [16, 10],[16, 11],[17, 10]]



# positions data
df_filtered_positions = np.full(filtered_result.shape, np.nan)
for i, j in positions:
    df_filtered_positions[i, j] = filtered_result[i, j]

# color scale boundaries
mean_value = np.nanmean([filtered_result[i, j] for i, j in positions])
std_value = np.nanstd([filtered_result[i, j] for i, j in positions])
vmin, vmax = mean_value - 2 * std_value, mean_value + 2 * std_value

# smoothed data with opacity
plt.figure(figsize=(10, 8))
ax = sns.heatmap(filtered_result, cmap="viridis", annot=False,
                 cbar_kws={'label': 'Response delay (sec)'}, xticklabels=False, yticklabels=False,
                 linewidths=0, linecolor='black', vmin=vmin, vmax=vmax, alpha=opacity_map)

# ellipses, manually check the RF properties, x0, y0, sigmas and theta values
major_axis = max(3.18, 3.12) * 2.355
minor_axis = min(3.18, 3.12) * 2.355
major_axis2 = max(4.63, 4.21) * 2.355
minor_axis2 = min(4.63, 4.21) * 2.355
#major_axis3 = max(2, 1.88) * 2.355
#minor_axis3 = min(2, 1.88) * 2.355
ellipse = Ellipse((10.31, 8.75), width=major_axis, height=minor_axis, angle=-np.degrees(0.74), edgecolor='r', fc='None', lw=4)
ellipse2 = Ellipse((10.19, 14.88), width=major_axis2, height=minor_axis2, angle=-np.degrees(1.543), edgecolor='r', fc='None', lw=4)
#ellipse3 = Ellipse((13.66, 16.67), width=major_axis3, height=minor_axis3, angle=-np.degrees(2.96), edgecolor='r', fc='None', lw=4)
plt.gca().add_patch(ellipse)
plt.gca().add_patch(ellipse2)
#plt.gca().add_patch(ellipse3)

plt.show()

# specified positions only
plt.figure(figsize=(10, 8))
ax = sns.heatmap(df_filtered_positions, cmap="viridis", annot=False, 
                 cbar_kws={'label': 'Response delay (sec)'}, xticklabels=False, yticklabels=False,
                 linewidths=0, linecolor='black', vmin=vmin, vmax=vmax, alpha=opacity_map)

# ellipses
major_axis = max(3.18, 3.12) * 2.355
minor_axis = min(3.18, 3.12) * 2.355
major_axis2 = max(4.63, 4.21) * 2.355
minor_axis2 = min(4.63, 4.21) * 2.355
#major_axis3 = max(2, 1.88) * 2.355
#minor_axis3 = min(2, 1.88) * 2.355
ellipse = Ellipse((10.31, 8.75), width=major_axis, height=minor_axis, angle=-np.degrees(0.74), edgecolor='r', fc='None', lw=4)
ellipse2 = Ellipse((10.19, 14.88), width=major_axis2, height=minor_axis2, angle=-np.degrees(1.543), edgecolor='r', fc='None', lw=4)
#ellipse3 = Ellipse((13.66, 16.67), width=major_axis3, height=minor_axis3, angle=-np.degrees(2.96), edgecolor='r', fc='None', lw=4)
plt.gca().add_patch(ellipse)
plt.gca().add_patch(ellipse2)
#plt.gca().add_patch(ellipse3)

        
for spine in ax.spines.values():
    spine.set_visible(True)
    spine.set_linewidth(2)
    spine.set_edgecolor('black')

plt.show()

