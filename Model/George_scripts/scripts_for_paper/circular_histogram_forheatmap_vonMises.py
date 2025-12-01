#!/usr/bin/env python3
"""
plot_eye_vectors_vonmises_mixture_mainweight.py

Fits single and 2-component von Mises mixture, then chooses the "main"
component according to a selection rule (default: 'weight' meaning p>0.5).

Usage:
  python plot_eye_vectors_vonmises_mixture_mainweight.py <data.tsv> <sun_azimuth_deg> <sun_elevation> <output.svg> <output.tsv> [--main-rule RULE] [--alpha ALPHA] [--ref-deg REF]

Dependencies: numpy, pandas, scipy, matplotlib
"""

import sys, os, argparse
import math
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import statistics
from scipy import optimize, special
from scipy.special import logsumexp
from scipy.stats import chi2

# -------------------------
# Helpers
# -------------------------
def polar_to_cartesian(angle_deg, length):
    angle_rad = np.radians(angle_deg)
    x = length * np.cos(angle_rad)
    y = length * np.sin(angle_rad)
    return x, y

def min_angle_difference(a_deg, b_deg):
    return abs((a_deg - b_deg + 180) % 360 - 180)

def circular_mean(angles_rad):
    return np.angle(np.mean(np.exp(1j * angles_rad)))

def mean_resultant_length(angles_rad):
    C = np.mean(np.cos(angles_rad))
    S = np.mean(np.sin(angles_rad))
    R = np.hypot(C, S)
    return R

def kappam_from_R(R):
    if np.isnan(R) or R < 0:
        return 0.0
    if R < 0.53:
        return 2*R + R**3 + (5.0/6.0)*R**5
    elif R < 0.85:
        return -0.4 + 1.39*R + 0.43/(1.0 - R)
    else:
        return 1.0/(R**3 - 4*R**2 + 3*R)

def vonmises_logpdf(angles, mu, kappa):
    # safe logpdf; returns array
    i0 = special.i0(kappa)
    if not np.isfinite(i0) or i0 <= 0:
        return -np.inf * np.ones_like(angles)
    return kappa * np.cos(angles - mu) - np.log(2 * np.pi * i0)

def vonmises_sum_loglike(angles, mu, kappa):
    lp = vonmises_logpdf(angles, mu, kappa)
    if np.any(np.isneginf(lp)):
        return -1e12
    return np.sum(lp)

# -------------------------
# Single von Mises fit
# -------------------------
def negloglike_single(params, angles):
    mu = params[0]
    kappa = float(np.exp(params[1]))
    if not np.isfinite(mu) or not np.isfinite(kappa) or kappa < 0:
        return 1e12
    ll = vonmises_sum_loglike(angles, mu, kappa)
    return -ll

def fit_single_vonmises(angles, verbose=True):
    angles = np.asarray(angles)
    if angles.size == 0:
        return 0.0, 0.0, None
    angles = (angles + np.pi) % (2*np.pi) - np.pi
    mu0 = circular_mean(angles)
    R = mean_resultant_length(angles)
    kappa_mom = max(1e-8, kappam_from_R(R))
    k_starts = [max(1e-3, kappa_mom), 0.1, 1.0, 5.0, 50.0]
    best_res, best_nll = None, np.inf
    for k0 in k_starts:
        init = np.array([mu0, np.log(k0)])
        try:
            res = optimize.minimize(
                negloglike_single,
                x0=init,
                args=(angles,),
                method='L-BFGS-B',
                bounds=[(-np.pi, np.pi), (np.log(1e-12), np.log(1e4))],
                options={'maxiter': 20000}
            )
            if verbose:
                print(f"[single] init k0={k0:.4g} -> success={res.success}, nll={res.fun:.6f}, mu={res.x[0]:.4f}, kappa={np.exp(res.x[1]):.6f}")
            if res.fun < best_nll:
                best_nll = res.fun
                best_res = res
        except Exception as e:
            if verbose:
                print(f"[single] opt failed k0={k0}: {e}")
    if best_res is None:
        best_res = res
    mu_hat, kappa_hat = best_res.x[0], float(np.exp(best_res.x[1]))
    ll = -best_res.fun
    return mu_hat, kappa_hat, {'res': best_res, 'loglike': ll, 'R': R}

# -------------------------
# Mixture fit
# -------------------------
def negloglike_mixture(params, angles):
    mu1 = params[0]
    k1 = float(np.exp(params[1]))
    mu2 = params[2]
    k2 = float(np.exp(params[3]))
    logit_p = params[4]
    try:
        p = 1.0 / (1.0 + np.exp(-logit_p))
    except OverflowError:
        p = 0.0 if logit_p < 0 else 1.0
    if not np.isfinite(mu1) or not np.isfinite(mu2) or not np.isfinite(k1) or not np.isfinite(k2):
        return 1e12
    if k1 < 0 or k2 < 0:
        return 1e12
    lp1 = vonmises_logpdf(angles, mu1, k1)
    lp2 = vonmises_logpdf(angles, mu2, k2)
    if p <= 0:
        ll = np.sum(lp2)
    elif p >= 1:
        ll = np.sum(lp1)
    else:
        comp_log = np.vstack((np.log(p) + lp1, np.log(1 - p) + lp2))
        ll = np.sum(logsumexp(comp_log, axis=0))
    return -ll

def fit_mixture_vonmises(angles, verbose=True):
    angles = np.asarray(angles)
    if angles.size == 0:
        return 0.0, 0.0, 0.0, 0.0, 0.5, None
    angles = (angles + np.pi) % (2*np.pi) - np.pi
    # histogram seeds for mus
    bin_edges = np.linspace(-np.pi, np.pi, 73)
    counts, _ = np.histogram(angles, bins=bin_edges)
    bin_centers = (bin_edges[:-1] + bin_edges[1:]) / 2.0
    idx_sorted = np.argsort(counts)[::-1]
    if counts.sum() == 0:
        mu1_init = circular_mean(angles)
        mu2_init = (mu1_init + np.pi) % (2*np.pi) - np.pi
    else:
        mu1_init = bin_centers[idx_sorted[0]]
        mu2_init = bin_centers[idx_sorted[1]] if idx_sorted.size > 1 else (mu1_init + np.pi) % (2*np.pi) - np.pi
    R = mean_resultant_length(angles)
    kappa_mom = max(1e-8, kappam_from_R(R))
    kappa_starts = [max(1e-3, kappa_mom), 1.0, 5.0, 50.0]
    p_init = 0.5
    best_res, best_nll = None, np.inf
    for k1_0 in kappa_starts:
        for k2_0 in kappa_starts:
            init = np.array([mu1_init, np.log(k1_0), mu2_init, np.log(k2_0), np.log(p_init/(1.0-p_init))])
            try:
                res = optimize.minimize(
                    negloglike_mixture,
                    x0=init,
                    args=(angles,),
                    method='L-BFGS-B',
                    bounds=[(-np.pi, np.pi), (np.log(1e-12), np.log(1e4)),
                            (-np.pi, np.pi), (np.log(1e-12), np.log(1e4)),
                            (None, None)],
                    options={'maxiter': 30000}
                )
                if verbose:
                    p_est = 1.0 / (1.0 + np.exp(-res.x[4]))
                    print(f"[mix] init k1={k1_0}, k2={k2_0} -> success={res.success}, nll={res.fun:.6f}, mu1={res.x[0]:.4f}, k1={np.exp(res.x[1]):.6f}, mu2={res.x[2]:.4f}, k2={np.exp(res.x[3]):.6f}, p={p_est:.4f}")
                if res.fun < best_nll:
                    best_nll = res.fun
                    best_res = res
            except Exception as e:
                if verbose:
                    print(f"[mix] opt failed k1={k1_0}, k2={k2_0}: {e}")
    if best_res is None:
        init = np.array([mu1_init, np.log(max(kappa_mom,1e-3)), mu2_init, np.log(max(kappa_mom,1e-3)), 0.0])
        res = optimize.minimize(negloglike_mixture, x0=init, args=(angles,), method='Nelder-Mead', options={'maxiter':30000})
        best_res = res
        best_nll = res.fun
    mu1_hat = best_res.x[0]; k1_hat = float(np.exp(best_res.x[1]))
    mu2_hat = best_res.x[2]; k2_hat = float(np.exp(best_res.x[3]))
    p_hat = 1.0/(1.0 + np.exp(-best_res.x[4]))
    ll = -best_res.fun
    return mu1_hat, k1_hat, mu2_hat, k2_hat, p_hat, {'res': best_res, 'loglike': ll, 'R': R}

# -------------------------
# Reorder components using a rule (default: weight)
# -------------------------
def reorder_mixture_components(mu1, k1, mu2, k2, p, angles, rule='weight', alpha=1.0, ref_angle=None):
    angles = np.asarray(angles)
    angles = (angles + np.pi) % (2 * np.pi) - np.pi
    lp1 = vonmises_logpdf(angles, mu1, k1)
    lp2 = vonmises_logpdf(angles, mu2, k2)
    logp1 = np.log(p) if p > 0 else -np.inf
    logp2 = np.log(1.0 - p) if (1.0 - p) > 0 else -np.inf
    stacked = np.vstack((logp1 + lp1, logp2 + lp2))
    lse = logsumexp(stacked, axis=0)
    log_r1 = (logp1 + lp1) - lse
    log_r2 = (logp2 + lp2) - lse
    r1 = np.exp(log_r1); r2 = np.exp(log_r2)
    N1 = float(r1.sum()); N2 = float(r2.sum())
    n = float(angles.size)
    info = {'N1': N1, 'N2': N2, 'p': float(p), 'kappa1': float(k1), 'kappa2': float(k2)}
    if rule == 'weight':
        if p >= 0.5:
            main = 1; choice = 'weight: p >= 0.5'
        else:
            main = 2; choice = 'weight: p < 0.5'
    elif rule == 'responsibility':
        if N1 >= N2:
            main = 1; choice = 'responsibility: N1 >= N2'
        else:
            main = 2; choice = 'responsibility: N2 > N1'
    elif rule == 'kappa':
        if k1 >= k2:
            main = 1; choice = 'kappa: k1 >= k2 (sharpest)'
        else:
            main = 2; choice = 'kappa: k2 > k1 (sharpest)'
    elif rule == 'composite':
        score1 = (N1 / n) * (k1 ** alpha)
        score2 = (N2 / n) * (k2 ** alpha)
        if score1 >= score2:
            main = 1; choice = f'composite: score1 >= score2 (alpha={alpha})'
        else:
            main = 2; choice = f'composite: score2 > score1 (alpha={alpha})'
        info.update({'score1': score1, 'score2': score2, 'alpha': alpha})
    elif rule == 'closest_to_ref':
        if ref_angle is None:
            raise ValueError("ref_angle required for closest_to_ref rule")
        d1 = abs((np.degrees(mu1 - ref_angle) + 180) % 360 - 180)
        d2 = abs((np.degrees(mu2 - ref_angle) + 180) % 360 - 180)
        if d1 <= d2:
            main = 1; choice = 'closest_to_ref: component1 closer'
        else:
            main = 2; choice = 'closest_to_ref: component2 closer'
        info.update({'d1_deg': d1, 'd2_deg': d2, 'ref_deg': (np.degrees(ref_angle)+360)%360})
    else:
        # fallback -> weight
        if p >= 0.5:
            main = 1; choice = 'fallback weight'
        else:
            main = 2; choice = 'fallback weight'
    if main == 1:
        mu_main, k_main = mu1, k1
        mu_other, k_other = mu2, k2
        p_main = p
        N_main, N_other = N1, N2
    else:
        mu_main, k_main = mu2, k2
        mu_other, k_other = mu1, k1
        p_main = 1.0 - p
        N_main, N_other = N2, N1
    info.update({
        'main_choice': choice,
        'main_index': int(main),
        'N_main': float(N_main),
        'N_other': float(N_other),
        'p_main': float(p_main),
        'p_other': float(1.0 - p_main)
    })
    return mu_main, k_main, mu_other, k_other, p_main, info

# -------------------------
# AIC/BIC utilities
# -------------------------
def aic_bic(loglike, n_params, n_samples):
    aic = 2*n_params - 2*loglike
    bic = np.log(n_samples)*n_params - 2*loglike
    return aic, bic

# -------------------------
# Main entry
# -------------------------
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('input_tsv')
    parser.add_argument('sun_azimuth_deg', type=float)
    parser.add_argument('sun_elevation', type=float)
    parser.add_argument('output_svg')
    parser.add_argument('output_tsv')
    parser.add_argument('--main-rule', choices=['weight','responsibility','kappa','composite','closest_to_ref'], default='weight')
    parser.add_argument('--alpha', type=float, default=1.0, help='alpha for composite score (score = (N/n)*kappa^alpha)')
    parser.add_argument('--ref-deg', type=float, default=None, help='reference deg for closest_to_ref rule')
    args = parser.parse_args()

    input_file = args.input_tsv
    sun_azimuth_deg = args.sun_azimuth_deg
    sun_elevation = args.sun_elevation

    # Use input folder for outputs
    input_dir = os.path.dirname(os.path.abspath(input_file))
    output_svg = os.path.join(input_dir, os.path.basename(args.output_svg))
    output_tsv = os.path.join(input_dir, os.path.basename(args.output_tsv))

    main_rule = args.main_rule
    alpha = args.alpha
    ref_deg = args.ref_deg

    df = pd.read_csv(input_file, sep='\t')
    combined_angles = []
    combined_lengths = []
    absolute_errors = []
    output_rows = []
    errors = []

    for i, row in df.iterrows():
        try:
            x1, y1 = polar_to_cartesian(row['First Eye Angle'], row['First Eye Vector Length'])
            x2, y2 = polar_to_cartesian(row['Second Eye Angle'], row['Second Eye Vector Length'])
            x_combined = x1 + x2; y_combined = y1 + y2
            total_vector_angle = np.arctan2(y_combined, x_combined)
            length_combined = np.hypot(x_combined, y_combined)
            angle_with_offset = -total_vector_angle - np.radians(i * 5)
            combined_angles.append(angle_with_offset)
            combined_lengths.append(length_combined)
            rotation_angle = i * 5
            true_sun_deg = -sun_azimuth_deg - rotation_angle
            estimated_angle_deg = np.degrees(total_vector_angle)
            error_deg = min_angle_difference(true_sun_deg, estimated_angle_deg)
            absolute_errors.append(error_deg)
            output_rows.append({'sun_elevation': sun_elevation, 'sun_azimuth': true_sun_deg, 'error': error_deg})
        except Exception as e:
            errors.append(f"Row {i}: {e}")

    mean_abs_error = round(statistics.mean(absolute_errors), 2) if absolute_errors else float('nan')
    print(f"\nMean Absolute Angular Error: {mean_abs_error:.2f}°")

    angles = np.array(combined_angles)
    angles = (angles + np.pi) % (2*np.pi) - np.pi
    n = angles.size
    print(f"Number of angle samples: {n}")

    # Fit single
    print("\nFitting single von Mises...")
    mu_hat, kappa_hat, single_info = fit_single_vonmises(angles, verbose=True)
    ll_single = single_info['loglike']
    print(f"[single] mu (deg) = {(np.degrees(mu_hat)+360)%360:.4f}, kappa = {kappa_hat:.6f}, loglike = {ll_single:.6f}")

    # Fit mixture
    print("\nFitting 2-component von Mises mixture...")
    mu1_hat, k1_hat, mu2_hat, k2_hat, p_hat, mix_info = fit_mixture_vonmises(angles, verbose=True)
    ll_mix = mix_info['loglike']
    print(f"[mix] mu1 (deg) = {(np.degrees(mu1_hat)+360)%360:.4f}, k1 = {k1_hat:.6f}")
    print(f"[mix] mu2 (deg) = {(np.degrees(mu2_hat)+360)%360:.4f}, k2 = {k2_hat:.6f}, p = {p_hat:.6f}, loglike = {ll_mix:.6f}")

    # reorder according to chosen rule
    ref_angle = np.radians(ref_deg) if ref_deg is not None else None
    mu_main, k_main, mu_other, k_other, p_main, reorder_info = reorder_mixture_components(
        mu1_hat, k1_hat, mu2_hat, k2_hat, p_hat, angles, rule=main_rule, alpha=alpha, ref_angle=ref_angle
    )
    print(f"\nMain component chosen by: {reorder_info['main_choice']}")
    print(f"Main mu (deg): {(np.degrees(mu_main)+360)%360:.6f}, kappa: {k_main:.6f}, p_main: {p_main:.6f}")
    print(f"Other mu (deg): {(np.degrees(mu_other)+360)%360:.6f}, kappa: {k_other:.6f}, p_other: {1.0-p_main:.6f}")
    print(f"Expected counts N_main, N_other: {reorder_info['N_main']:.3f}, {reorder_info['N_other']:.3f}")

    # AIC/BIC and LRT
    n_params_single = 2
    n_params_mix = 5
    aic_single, bic_single = aic_bic(ll_single, n_params_single, n)
    aic_mix, bic_mix = aic_bic(ll_mix, n_params_mix, n)
    lrt_stat = 2.0 * (ll_mix - ll_single)
    df_diff = n_params_mix - n_params_single
    try:
        p_value = chi2.sf(lrt_stat, df=df_diff)
    except Exception:
        p_value = float('nan')
    print(f"\nAIC single = {aic_single:.3f}, AIC mix = {aic_mix:.3f}")
    print(f"BIC single = {bic_single:.3f}, BIC mix = {bic_mix:.3f}")
    print("\nLikelihood Ratio Test (approx):")
    print(f"  LRT stat = {lrt_stat:.6f}, df = {df_diff}, p (chi2 approx) = {p_value:.3e}")
    print("Note: chi2 p-value is approximate for mixtures; use parametric bootstrap for a reliable p-value.")

    # Plot
    bin_edges = np.arange(0, 360 + 5, 5)
    combined_angles_deg = (np.degrees(angles) + 360) % 360
    counts, _ = np.histogram(combined_angles_deg, bins=bin_edges)
    sqrt_counts = np.sqrt(counts)
    bin_centers_rad = np.radians(bin_edges[:-1] + 2.5)

    fig = plt.figure(figsize=(8,10))
    ax = fig.add_subplot(111, polar=True)
    ax.set_theta_zero_location('N'); ax.set_theta_direction(1)
    ax.grid(False); ax.set_xticks([]); ax.set_yticks([])
    ax.bar(bin_centers_rad, sqrt_counts, width=np.radians(5), bottom=0, color='cornflowerblue', align='center', label='data (sqrt counts)')

    if n > 0:
        theta_plot = np.linspace(0, 2*np.pi, 720)
        pdf_single = np.exp(kappa_hat * np.cos(theta_plot - mu_hat)) / (2 * np.pi * special.i0(kappa_hat))
        max_radius = max(sqrt_counts) if len(sqrt_counts) > 0 else 1.0
        pdf_single_scaled = pdf_single * (max_radius / np.max(pdf_single))
        ax.plot(theta_plot, pdf_single_scaled, linewidth=2.5, linestyle='-', label='single von Mises')

        pdf1 = np.exp(k1_hat * np.cos(theta_plot - mu1_hat)) / (2 * np.pi * special.i0(k1_hat))
        pdf2 = np.exp(k2_hat * np.cos(theta_plot - mu2_hat)) / (2 * np.pi * special.i0(k2_hat))
        pdf_mix = p_hat * pdf1 + (1-p_hat) * pdf2
        pdf_mix_scaled = pdf_mix * (max_radius / np.max(pdf_mix))
        ax.plot(theta_plot, pdf_mix_scaled, linewidth=2.5, linestyle='--', label='mixture von Mises')

        # mark main and other mixture components
        ax.plot(mu_main, max_radius*1.05, marker='o', markersize=10, color='red', label='mu_main')
        ax.plot(mu_other, max_radius*1.05, marker='x', markersize=8, color='black', label='mu_other')

        # --- Add the single von Mises mean as a blue dot ---
        ax.plot(mu_hat, max_radius*1.1, marker='o', markersize=10, color='blue', label='single mean')

    ax.legend(loc='upper right')
    ax.set_frame_on(True)
    # Convert sun azimuth to radians and adjust for polar plot
    sun_az_rad = np.radians((sun_azimuth_deg + 360) % 360)

    # Plot sun azimuth as an orange dot
    ax.plot(sun_az_rad, max_radius*1.15, marker='o', markersize=10, color='orange', label='Sun azimuth')
    # mark main and other
    ax.plot(mu_main, max_radius*1.05, marker='o', markersize=10, color='red', label='mu_main')
    ax.plot(mu_other, max_radius*1.05, marker='x', markersize=8, color='black', label='mu_other')

    # mark sun azimuth
    ax.plot(sun_az_rad, max_radius*1.15, marker='o', markersize=10, color='orange', label='Sun azimuth')

    plt.tight_layout()
    plt.savefig(output_svg, format='svg')
    plt.show()
    print(f"Saved figure to: {output_svg}")

    # Save per-row results
    output_df = pd.DataFrame(output_rows)
    file_exists = os.path.exists(output_tsv)
    output_df.to_csv(output_tsv, sep='\t', mode='a', index=False, header=not file_exists)
    print(f"Results appended to: {output_tsv}")

    # Save summary
    summary_path = output_tsv + '.vonmises_mixture_summary.tsv'
    summary_exists = os.path.exists(summary_path)
    summary_row = {
        'input_file': os.path.basename(input_file),
        'n_samples': n,
        'sun_elevation': sun_elevation,
        'sun_azimuth_input': sun_azimuth_deg,
        'single_mu_deg': (np.degrees(mu_hat) + 360) % 360,
        'single_kappa': kappa_hat,
        'single_loglike': ll_single,
        'single_aic': aic_single,
        'single_bic': bic_single,
        'mix_mu1_deg': (np.degrees(mu1_hat) + 360) % 360,
        'mix_kappa1': k1_hat,
        'mix_mu2_deg': (np.degrees(mu2_hat) + 360) % 360,
        'mix_kappa2': k2_hat,
        'mix_p': p_hat,
        'mix_loglike': ll_mix,
        'mix_aic': aic_mix,
        'mix_bic': bic_mix,
        'main_choice': reorder_info['main_choice'],
        'main_mu_deg': (np.degrees(mu_main) + 360) % 360,
        'main_kappa': k_main,
        'main_p': p_main,
        'main_N': reorder_info['N_main'],
        'other_mu_deg': (np.degrees(mu_other) + 360) % 360,
        'other_kappa': k_other,
        'other_p': reorder_info['p_other'],
        'other_N': reorder_info['N_other'],
        'lrt_stat': lrt_stat,
        'lrt_df_approx': df_diff,
        'lrt_p_chi2_approx': p_value,
        'mean_abs_error_deg': mean_abs_error
    }
    pd.DataFrame([summary_row]).to_csv(summary_path, sep='\t', mode='a', index=False, header=not summary_exists)
    print(f"Saved fit summary to: {summary_path}")

    if errors:
        print("\nErrors encountered during processing:")
        for e in errors:
            print(e)

if __name__ == '__main__':
    main()
