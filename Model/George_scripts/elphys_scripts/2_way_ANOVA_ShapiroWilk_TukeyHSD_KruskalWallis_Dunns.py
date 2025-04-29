import sys
import pandas as pd
import numpy as np
import statsmodels.api as sm
from statsmodels.formula.api import ols
from statsmodels.stats.multicomp import pairwise_tukeyhsd
from scipy.stats import shapiro, kruskal
import scikit_posthocs as sp
import matplotlib.pyplot as plt

def main(filename):
    ## main function that performs 2-way ANOVA, Shapiro-Wilk test on residuals
    ## and based on the normality test, either Tukey's HSD test or Kruskal-Wallis and subsequent Dunn's test
    ## input data file must be of the following form:
    df = pd.read_csv(filename, sep="\t", header=None, names=["value", "region", "species"])

    # log-transform the values
    df["value"] = np.log(df["value"])

    # 2-way ANOVA model
    model = ols("value ~ C(region) + C(species) + C(region):C(species)", data=df).fit()
    anova_table = sm.stats.anova_lm(model, typ=2)

    # ANOVA results
    print("=== Two-way ANOVA ===")
    print(anova_table)

    # residual normality check
    stat, p = shapiro(model.resid)
    print("\n=== Shapiro-Wilk Test for Normality of Residuals ===")
    print(f"Statistic={stat:.4f}, p-value={p:.4f}")

     # histogram of residuals
    plt.figure(figsize=(8, 5))
    plt.hist(model.resid, bins=20, edgecolor='black', alpha=0.7)
    plt.title("Histogram of Residuals")
    plt.xlabel("Residual")
    plt.ylabel("Frequency")
    plt.grid(True)
    plt.tight_layout()
    plt.show()
    
    if p > 0.05:
        print("Residuals appear to be normally distributed (p > 0.05)")
        print("\n=== Tukey's HSD Post Hoc Test ===")
        tukey = pairwise_tukeyhsd(endog=df["value"], groups=df["region"] + "_" + df["species"], alpha=0.05)
        print(tukey)
    else:
        print("Residuals do not appear to be normally distributed (p <= 0.05)")

        # Kruskal-Wallis by region
        print("\n=== Kruskal-Wallis Test by Region ===")
        grouped_region = [group["value"].values for name, group in df.groupby("region")]
        kw_region_stat, kw_region_p = kruskal(*grouped_region)
        print(f"Statistic={kw_region_stat:.4f}, p-value={kw_region_p:.4f}")

        if kw_region_p <= 0.05:
            print("\n--- Dunn's Post Hoc Test for Region (Bonferroni corrected) ---")
            dunn_region = sp.posthoc_dunn(df, val_col="value", group_col="region", p_adjust="bonferroni")
            print(dunn_region)

        # Kruskal-Wallis by species
        print("\n=== Kruskal-Wallis Test by Species ===")
        grouped_species = [group["value"].values for name, group in df.groupby("species")]
        kw_species_stat, kw_species_p = kruskal(*grouped_species)
        print(f"Statistic={kw_species_stat:.4f}, p-value={kw_species_p:.4f}")

        if kw_species_p <= 0.05:
            print("\n--- Dunn's Post Hoc Test for Species (Bonferroni corrected) ---")
            dunn_species = sp.posthoc_dunn(df, val_col="value", group_col="species", p_adjust="bonferroni")
            print(dunn_species)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python anova_script.py <datafile.tsv>")
        sys.exit(1)

    main(sys.argv[1])
