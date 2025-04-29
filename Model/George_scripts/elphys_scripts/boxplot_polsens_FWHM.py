import sys
import matplotlib.pyplot as plt
import numpy as np

# correct usage
if len(sys.argv) != 2:
    print("Usage: python script.py <file_path>")
    sys.exit(1)

# file path from the command-line argument
file_path = sys.argv[1]

# dictionaries to hold data for each region and species
data = {'main retina': {'Apis': [], 'Bombus': []},
        'marginal': {'Apis': [], 'Bombus': []},
        'DRA': {'Apis': [], 'Bombus': []}}

# organize the data by region and species
with open(file_path, 'r') as file:
    for line in file:
        # skip empty lines
        if not line.strip():
            continue

        try:
            value, category, species = line.strip().split('\t')
            value = float(value)

            if category in data and species in ['Apis', 'Bombus']:
                data[category][species].append(value)
            else:
                print(f"Warning: Unknown region '{category}' or species '{species}' in data file")
        except ValueError:
            print(f"Skipping malformed line: {line.strip()}")
            continue

fig, ax = plt.subplots(figsize=(10, 10))

categories = ['main retina', 'marginal', 'DRA']

colors = {'Apis': '#FF9999',
          'Bombus': '#ADD8E6'}

font_size = 18
tick_label_size = 16
y_tick_size = 18

# combined data for boxplots (side by side for Apis and Bombus)
positions = np.arange(1, len(categories) * 2, 2)  # Apis (1, 3, 5)
bombus_positions = positions + 1  # Bombus (2, 4, 6)

# boxplots for Apis and Bombus
line_width = 2.5
dotted_median_style = {'linestyle': '--', 'color': 'red', 'linewidth': line_width}
dotted_median_style_bombus = {'linestyle': '--', 'color': 'blue', 'linewidth': line_width}

for i, category in enumerate(categories):
    apis_data = data[category]['Apis']
    bombus_data = data[category]['Bombus']

    if apis_data:
        apis_box = ax.boxplot(apis_data, positions=[positions[i]], widths=0.4, patch_artist=True,
                              showmeans=False, meanline=False,
                              boxprops=dict(linewidth=line_width),
                              whiskerprops=dict(linewidth=line_width),
                              capprops=dict(linewidth=line_width),
                              medianprops=dotted_median_style,
                              flierprops=dict(marker='o', markersize=8, markerfacecolor='black'))
        for box in apis_box['boxes']:
            box.set_facecolor(colors['Apis'])
            box.set_alpha(0.5)
            
    if bombus_data:
        bombus_box = ax.boxplot(bombus_data, positions=[bombus_positions[i]], widths=0.4, patch_artist=True,
                                showmeans=False, meanline=False,
                                boxprops=dict(linewidth=line_width),
                                whiskerprops=dict(linewidth=line_width),
                                capprops=dict(linewidth=line_width),
                                medianprops=dotted_median_style_bombus,
                                flierprops=dict(marker='o', markersize=8, markerfacecolor='black'))
        for box in bombus_box['boxes']:
            box.set_facecolor(colors['Bombus'])
            box.set_alpha(0.5)

# scatter plots for each region with jitter
for i, category in enumerate(categories):
    apis_data = data[category]['Apis']
    bombus_data = data[category]['Bombus']

    if apis_data:
        jitter = np.random.uniform(-0.05, 0.05, size=len(apis_data))
        ax.scatter(np.full_like(apis_data, positions[i]) + jitter, apis_data, color='black', alpha=0.6)

    if bombus_data:
        jitter = np.random.uniform(-0.05, 0.05, size=len(bombus_data))
        ax.scatter(np.full_like(bombus_data, bombus_positions[i]) + jitter, bombus_data, color='black', alpha=0.6)

ax.set_title('Polarization Sensitivity: Apis vs Bombus', fontsize=font_size + 2)
ax.set_ylabel('Polarization Sensitivity', fontsize=font_size)

ax.set_xticks(positions + 0.5)
ax.set_xticklabels(categories, fontsize=font_size)

ax.tick_params(axis='both', which='major', labelsize=tick_label_size)
ax.yaxis.set_tick_params(labelsize=y_tick_size)

ax.grid(True, linewidth=line_width * 0.5)

from matplotlib.lines import Line2D

legend_handles = [Line2D([0], [0], color=colors['Apis'], lw=4, label='Apis'),
                  Line2D([0], [0], color=colors['Bombus'], lw=4, label='Bombus')]

ax.legend(handles=legend_handles, loc='upper right', fontsize=font_size)

plt.show()
