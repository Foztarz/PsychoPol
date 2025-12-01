#!/usr/bin/env python3
import os
import sys
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import UnivariateSpline
#from scipy import i0  # modified Bessel functions of the first kind
from scipy.special import i0, i1
TARGET_FILENAME = "vonMises.tsv.vonmises_mixture_summary.tsv"
#TARGET_FILENAME = "vonMises.svg.vonmises_mixture_summary.tsv"

# --------------------------------------------------------------------
# Helper functions
# --------------------------------------------------------------------
def angle_difference(mu1, mu2):
    """Compute absolute circular difference <= 180."""
    diff = abs(mu1 - mu2)
    return min(diff, 360 - diff)

def sd_from_kappa(kappa):
    """Compute circular standard deviation (degrees) from von Mises kappa."""
    if np.isnan(kappa) or kappa <= 0:
        return np.nan
    R = i1(kappa) / i0(kappa)
    R = np.clip(R, 1e-10, 1 - 1e-10)  # avoid log(0)
    sd_rad = np.sqrt(-2 * np.log(R))
    return np.degrees(sd_rad)


def p_closest_to_sun(df):
    """Return p-value of the mixture component whose mu is closer to the sun azimuth."""
    required = {"sun_azimuth_input", "mix_mu1_deg", "mix_mu2_deg", "mix_p"}
    if not required.issubset(df.columns):
        return pd.Series([np.nan] * len(df), index=df.index)

    diffs_1 = np.abs((df["mix_mu1_deg"] - df["sun_azimuth_input"] + 180) % 360 - 180)
    diffs_2 = np.abs((df["mix_mu2_deg"] - df["sun_azimuth_input"] + 180) % 360 - 180)
    closer_to_mu1 = diffs_1 <= diffs_2

    p_vals = np.where(closer_to_mu1, df["mix_p"], 1 - df["mix_p"])
    return pd.Series(p_vals, index=df.index, name="p_closest_to_sun")


# --------------------------------------------------------------------
# Plot functions
# --------------------------------------------------------------------
def plot_main_p(folder, x, main_p,
                smoothing=500,
                scatter_color="blue", spline_color="dodgerblue"):
    """Plot main_p bounded in [0,1] without logit transform."""
    x = np.array(x)
    main_p = np.array(main_p)

    # Ensure monotonic x
    for i in range(1, len(x)):
        if x[i] <= x[i - 1]:
            x[i] = x[i - 1] + 1e-6

    fig, ax = plt.subplots(figsize=(7, 5))
    ax.scatter(x, main_p, s=60, alpha=0.7, color=scatter_color,
               edgecolor="k", linewidth=0.3, label="main_p")

    if len(x) > 3:
        spline = UnivariateSpline(x, main_p, s=smoothing)
        x_new = np.linspace(x.min(), x.max(), 500)
        y_smooth = np.clip(spline(x_new), 0, 1)
        ax.plot(x_new, y_smooth, color=spline_color, lw=3, alpha=0.8, label="Spline fit")

    ax.set_title(f"{folder} (main_p)", fontsize=16, fontweight="bold")
    ax.set_xlabel("Sun Elevation", fontsize=14)
    ax.set_ylabel("main_p", fontsize=14)
    ax.set_ylim(-0.03, 1.03)
    ax.grid(True, linestyle="--", alpha=0.3)
    ax.tick_params(axis="both", which="major", labelsize=12)
    ax.legend(fontsize=12)

    plt.tight_layout()
    safe_name = folder.strip("/").replace("/", "_").replace(" ", "_")
    out_file = f"{safe_name}_main_p.svg"
    plt.savefig(out_file, format="svg")
    plt.close(fig)
    print(f"Saved {out_file}")


def plot_column(folder, x, y, column_name,
                smoothing=500, y_min=None, y_max=None,
                scatter_color="darkgreen", spline_color="darkblue"):
    """Simple publication-quality plot of a single column."""
    x = np.array(x)
    y = np.array(y)

    # Fix x monotonicity
    for i in range(1, len(x)):
        if x[i] <= x[i - 1]:
            x[i] = x[i - 1] + 1e-6

    if y_min is None:
        y_min = np.nanmin(y)
    if y_max is None:
        y_max = np.nanmax(y)

    fig, ax = plt.subplots(figsize=(7, 5))
    ax.scatter(x, y, s=60, alpha=0.7, color=scatter_color,
               edgecolor="k", linewidth=0.3, label="Data")

    if len(x) > 3:
        y_scaled = (y - y_min) / (y_max - y_min)
        y_scaled = np.clip(y_scaled, 1e-6, 1 - 1e-6)
        y_logit = np.log(y_scaled / (1 - y_scaled))
        spline = UnivariateSpline(x, y_logit, s=smoothing)
        x_new = np.linspace(x.min(), x.max(), 500)
        y_smooth_logit = spline(x_new)
        y_smooth = y_min + (y_max - y_min) * (np.exp(y_smooth_logit) / (1 + np.exp(y_smooth_logit)))
        ax.plot(x_new, y_smooth, color=spline_color, lw=3, alpha=0.8, label="Spline fit")

    ax.set_title(f"{folder} ({column_name})", fontsize=16, fontweight="bold")
    ax.set_xlabel("Sun Elevation", fontsize=14)
    ax.set_ylabel(column_name, fontsize=14)
    ax.tick_params(axis="both", which="major", labelsize=12)
    ax.grid(True, linestyle="--", alpha=0.3)
    ax.legend(fontsize=12)

    buffer = 0.03 * (y_max - y_min)
    ax.set_ylim(y_min - buffer, y_max + buffer)
    plt.tight_layout()

    safe_name = folder.strip("/").replace("/", "_").replace(" ", "_")
    out_file = f"{safe_name}_{column_name}.svg"
    plt.savefig(out_file, format="svg")
    plt.close(fig)
    print(f"Saved {out_file}")

def plot_sd_closest_vs_other(folder, x, sd_closest, sd_other, smoothing=500, tsv_path=None):
    """Plot standard deviations (degrees) of mixture components:
    closest to the sun, the other, and optionally an extra spline from TSV."""

    import pandas as pd
    import numpy as np
    import matplotlib.pyplot as plt
    from scipy.interpolate import UnivariateSpline

    x = np.array(x)
    sd_closest = np.array(sd_closest)
    sd_other = np.array(sd_other)

    # Ensure monotonic x
    for i in range(1, len(x)):
        if x[i] <= x[i - 1]:
            x[i] = x[i - 1] + 1e-6

    fig, ax = plt.subplots(figsize=(7, 5))

    # Scatter points
    ax.scatter(x, sd_closest, s=60, alpha=0.7, color="mediumvioletred",
               edgecolor="k", linewidth=0.3, label="SD (closest to sun)")
    ax.scatter(x, sd_other, s=60, alpha=0.7, color="gray",
               edgecolor="k", linewidth=0.3, label="SD (other)")

    # Load optional TSV
    if tsv_path:
        df_extra = pd.read_csv(tsv_path, sep="\t", header=None, names=["sun_elevation", "SD"])
        x_extra = df_extra["sun_elevation"].values
        sd_extra = df_extra["SD"].values


    # Combine all SDs to define scaling range
    all_sd = np.concatenate(
        [sd_closest, sd_other] + ([sd_extra] if sd_extra is not None else [])
    )
    y_min = 0
    y_max = np.nanmax(all_sd) * 1.1

    # Spline smoothing in logit space
    if len(x) > 3:
        for y, color, label in [
            (sd_closest, "darkred", "Spline (closest)"),
            (sd_other, "black", "Spline (other)"),
        ]:
            mask_valid = ~np.isnan(y)
            if mask_valid.sum() > 3:
                y_scaled = (y[mask_valid] - y_min) / (y_max - y_min)
                y_scaled = np.clip(y_scaled, 1e-6, 1 - 1e-6)
                y_logit = np.log(y_scaled / (1 - y_scaled))
                spline = UnivariateSpline(x[mask_valid], y_logit, s=smoothing)
                x_new = np.linspace(np.nanmin(x[mask_valid]), np.nanmax(x[mask_valid]), 500)
                y_smooth_logit = spline(x_new)
                y_smooth = y_min + (y_max - y_min) * (np.exp(y_smooth_logit) / (1 + np.exp(y_smooth_logit)))
                ax.plot(x_new, y_smooth, color=color, lw=3, alpha=0.8, label=label)

    # Add extra spline + scatter points if TSV provided
    if x_extra is not None and len(x_extra) > 0:
        mask_valid = ~np.isnan(sd_extra)
        if mask_valid.sum() > 0:
            # Keep only valid entries
            x_valid = x_extra[mask_valid]
            y_valid = sd_extra[mask_valid]

            # Scatter points
            ax.scatter(x_valid, y_valid, s=60, alpha=0.7, color="blue",
                       edgecolor="k", linewidth=0.3, label="SD (overall)")

            # If enough points, add spline
            if len(x_valid) > 3:
                # Sort x for spline
                sort_idx = np.argsort(x_valid)
                x_sorted = x_valid[sort_idx]
                y_sorted = y_valid[sort_idx]

                # Spline in logit space
                y_scaled = (y_sorted - y_min) / (y_max - y_min)
                y_scaled = np.clip(y_scaled, 1e-6, 1 - 1e-6)
                y_logit = np.log(y_scaled / (1 - y_scaled))

                spline = UnivariateSpline(x_sorted, y_logit, s=smoothing)
                x_new = np.linspace(np.nanmin(x_sorted), np.nanmax(x_sorted), 500)
                y_smooth_logit = spline(x_new)
                y_smooth = y_min + (y_max - y_min) * (np.exp(y_smooth_logit) / (1 + np.exp(y_smooth_logit)))

                ax.plot(x_new, y_smooth, color="blue", lw=3, alpha=0.8, linestyle="--", label="Spline (overall)")


    ax.set_title(f"{folder} (SD: closest vs other vs overall)", fontsize=16, fontweight="bold")
    ax.set_xlabel("Sun Elevation", fontsize=14)
    ax.set_ylim(0, 220)
    ax.set_ylabel("Standard Deviation (°)", fontsize=14)
    ax.tick_params(axis="both", which="major", labelsize=12)
    ax.grid(True, linestyle="--", alpha=0.3)
    ax.legend(fontsize=12)

    plt.tight_layout()
    safe_name = folder.strip("/").replace("/", "_").replace(" ", "_")
    out_file = f"{safe_name}_sd_closest_vs_other_plus_overall.svg"
    plt.savefig(out_file, format="svg")
    plt.close(fig)
    print(f"Saved {out_file}")


def plot_angle_and_p(folder, x, angle_diff, p_closest,
                     y_min_angle=0, y_max_angle=180,
                     y_min_p=0, y_max_p=1,
                     smoothing=500):
    """Dual-axis plot styled consistently with plot_column (same borders, sizes, etc)."""
    x = np.array(x)
    angle_diff = np.array(angle_diff)
    p_closest = np.array(p_closest)

    # Ensure monotonic x for spline
    for i in range(1, len(x)):
        if x[i] <= x[i - 1]:
            x[i] = x[i - 1] + 1e-6

    fig, ax1 = plt.subplots(figsize=(7, 5))

    # Create twin axis once
    ax2 = ax1.twinx()

    # Force both axes to occupy the same rectangle
    ax_box = [0.12, 0.12, 0.78, 0.78]
    ax1.set_position(ax_box)
    ax2.set_position(ax_box)

    # --- Left axis: angle difference ---
    ax1.scatter(
        x, angle_diff, s=60, alpha=0.7,
        color="darkorange", edgecolor="k", linewidth=0.3, label="Angle diff"
    )

    if len(x) > 3:
        # Logit spline fit (bounded data 0–180)
        y_scaled = (angle_diff - y_min_angle) / (y_max_angle - y_min_angle)
        y_scaled = np.clip(y_scaled, 1e-6, 1 - 1e-6)
        y_logit = np.log(y_scaled / (1 - y_scaled))
        spline_angle = UnivariateSpline(x, y_logit, s=smoothing)
        x_new = np.linspace(x.min(), x.max(), 500)
        y_smooth_logit = spline_angle(x_new)
        y_smooth = y_min_angle + (y_max_angle - y_min_angle) * (
            np.exp(y_smooth_logit) / (1 + np.exp(y_smooth_logit))
        )
        ax1.plot(x_new, y_smooth, color="firebrick", lw=3, alpha=0.8, label="Angle diff spline")

    buffer_angle = 0.03 * (y_max_angle - y_min_angle)
    ax1.set_ylim(y_min_angle - buffer_angle, y_max_angle + buffer_angle)
    ax1.set_yticks(np.arange(0, 181, 30))
    ax1.set_xlabel("Sun Elevation", fontsize=14)
    ax1.set_ylabel("Angle Difference (°)", fontsize=14, color="darkorange")
    ax1.tick_params(axis="y", labelcolor="darkorange")
    ax1.tick_params(axis="both", which="major", labelsize=12)
    ax1.grid(True, linestyle="--", alpha=0.3)
    ax1.axhline(90, color="gray", linestyle="--", alpha=0.6, lw=1)

    # --- Right axis: p_closest_to_sun (direct spline on raw data) ---
    mask_valid = ~np.isnan(p_closest)
    x_p = x[mask_valid]
    y_p = p_closest[mask_valid]

    if len(x_p) > 0:
        ax2.scatter(
            x_p, y_p, s=60, alpha=0.7,
            color="purple", edgecolor="k", linewidth=0.3, label="p_closest_to_sun"
        )

        if len(x_p) > 3:
            spline_p = UnivariateSpline(x_p, y_p, s=smoothing)
            x_new_p = np.linspace(x_p.min(), x_p.max(), 500)
            y_smooth_p = np.clip(spline_p(x_new_p), 0, 1)
            ax2.plot(x_new_p, y_smooth_p, color="magenta", lw=3, alpha=0.8, label="p spline")

    buffer_p = 0.03 * (y_max_p - y_min_p)
    ax2.set_ylim(y_min_p - buffer_p, y_max_p + buffer_p)
    ax2.set_yticks(np.linspace(0, 1, 5))
    ax2.set_ylabel("p_closest_to_sun", fontsize=14, color="purple")
    ax2.tick_params(axis="y", labelcolor="purple")
    ax2.tick_params(axis="both", which="major", labelsize=12)

    # --- Consistent title and layout ---
    fig.suptitle(f"{folder} (Angle diff & p_closest_to_sun)", fontsize=16, fontweight="bold")

    # Unified legend style and location
    ax1.legend(loc="upper left", fontsize=12)
    ax2.legend(loc="upper right", fontsize=12)

    # Don’t use tight_layout (it breaks the fixed box alignment)
    # plt.tight_layout()

    safe_name = folder.strip("/").replace("/", "_").replace(" ", "_")
    out_file = f"{safe_name}_angle_diff_with_p_closest.svg"
    plt.savefig(out_file, format="svg")
    plt.close(fig)
    print(f"Saved {out_file}")



# --------------------------------------------------------------------
# Main
# --------------------------------------------------------------------
if len(sys.argv) < 2:
    print("Usage: python script.py <folder1> <folder2> ... [--no-spline]")
    sys.exit(1)

folders = [arg for arg in sys.argv[1:] if not arg.startswith("--")]
use_spline = "--no-spline" not in sys.argv

for folder in folders:
    records = []

    for root, dirs, files in os.walk(folder):
        if TARGET_FILENAME in files:
            file_path = os.path.join(root, TARGET_FILENAME)
            try:
                df = pd.read_csv(file_path, sep="\t")
                for _, row in df.iterrows():
                    records.append({
                        "sun_elevation": row.get("sun_elevation", np.nan),
                        "sun_azimuth_input": row.get("sun_azimuth_input", np.nan),
                        "mix_mu1_deg": row.get("mix_mu1_deg", np.nan),
                        "mix_mu2_deg": row.get("mix_mu2_deg", np.nan),
                        "mix_p": row.get("mix_p", np.nan),
                        "mix_kappa1": row.get("mix_kappa1", np.nan),
                        "mix_kappa2": row.get("mix_kappa2", np.nan),
                        "lrt_stat": row.get("lrt_stat", np.nan),
                        "main_p": row.get("main_p", np.nan),
                        "single_mu_deg": row.get("single_mu_deg", np.nan)
                    })
            except Exception as e:
                print(f"Skipping {file_path}: {e}")

    if not records:
        print(f"No valid data in {folder}, skipping.")
        continue

    df = pd.DataFrame(records).dropna(subset=["sun_elevation"]).sort_values("sun_elevation").reset_index(drop=True)

    df["angle_diff"] = [
        angle_difference(m1, m2) if not np.isnan(m1) and not np.isnan(m2) else np.nan
        for m1, m2 in zip(df["mix_mu1_deg"], df["mix_mu2_deg"])
    ]

    df["az_diff_single"] = [
        angle_difference(sun, mu) if not np.isnan(sun) and not np.isnan(mu) else np.nan
        for sun, mu in zip(df["sun_azimuth_input"], df["single_mu_deg"])
    ]

    df["az_diff_mix"] = [
        min(angle_difference(sun, mu1), angle_difference(sun, mu2))
        if not any(np.isnan([sun, mu1, mu2])) else np.nan
        for sun, mu1, mu2 in zip(df["sun_azimuth_input"], df["mix_mu1_deg"], df["mix_mu2_deg"])
    ]

    df["p_closest_to_sun"] = p_closest_to_sun(df)

    
    # Compute circular SDs (degrees) for each mixture component
    df["sd1_deg"] = [
        sd_from_kappa(k) if not np.isnan(k) else np.nan
        for k in df["mix_kappa1"]
    ]

    df["sd2_deg"] = [
        sd_from_kappa(k) if not np.isnan(k) else np.nan
        for k in df["mix_kappa2"]
    ]
    
    # Compute which component's mean is closer to the sun azimuth
    diffs_1 = np.abs((df["mix_mu1_deg"] - df["sun_azimuth_input"] + 180) % 360 - 180)
    diffs_2 = np.abs((df["mix_mu2_deg"] - df["sun_azimuth_input"] + 180) % 360 - 180)
    closer_to_mu1 = diffs_1 <= diffs_2

    # Assign SDs to closest and other components
    df["sd_closest_to_sun"] = [
        sd1 if closer else sd2
        for sd1, sd2, closer in zip(df["sd1_deg"], df["sd2_deg"], closer_to_mu1)
    ]

    df["sd_other"] = [
        sd2 if closer else sd1
        for sd1, sd2, closer in zip(df["sd1_deg"], df["sd2_deg"], closer_to_mu1)
    ]

    x = df["sun_elevation"].values

    # Plot LRT statistic
    if "lrt_stat" in df and not df["lrt_stat"].isna().all():
        plot_column(folder, x, df["lrt_stat"].values, "lrt_stat",
                    scatter_color="darkgreen", spline_color="darkblue")

    # Plot angle diff and p_closest_to_sun together
    if "angle_diff" in df and not df["angle_diff"].isna().all():
        plot_angle_and_p(folder, x, df["angle_diff"].values, df["p_closest_to_sun"].values)

    # Plot single azimuth difference
    if "az_diff_single" in df and not df["az_diff_single"].isna().all():
        plot_column(folder, x, df["az_diff_single"].values, "az_diff_single",
                    y_min=0, y_max=180,
                    scatter_color="teal", spline_color="navy")

    # Plot mixture azimuth difference
    if "az_diff_mix" in df and not df["az_diff_mix"].isna().all():
        plot_column(folder, x, df["az_diff_mix"].values, "az_diff_mix",
                    y_min=0, y_max=180,
                    scatter_color="brown", spline_color="darkred")

    # Plot main_p
    if "main_p" in df and not df["main_p"].isna().all():
        plot_main_p(folder, x, df["main_p"].values)

    tsv_file = f"{folder}/overall_sd_data.tsv"

    if "sd_closest_to_sun" in df and not df["sd_closest_to_sun"].isna().all():
        plot_sd_closest_vs_other(
            folder,
            x,
            df["sd_closest_to_sun"].values,
            df["sd_other"].values,
            tsv_path=tsv_file if os.path.isfile(tsv_file) else None
        )



