import sys
import pandas as pd
import statsmodels.formula.api as smf
from scipy.stats import chi2
from statsmodels.stats.multicomp import pairwise_tukeyhsd
from scipy.stats import shapiro

def fit_model(formula, df):
    ## fits ordinary least squares to x~y values
    return smf.ols(formula, data=df).fit()


def tukey_on_region_after_model(df):
    ## fits the selected model to the data and performs Shapiro-Wilk test on residuals
    ## and subsequent Tukey's HSD test on the regions
    print("\nChecking residual normality before Tukey's HSD:")

    # fit model
    model = smf.ols('FWHM ~ PS + region + species', data=df).fit()

    # residuals
    df['resid'] = model.resid

    # Shapiro-Wilk test for normality on residuals
    shapiro_stat, shapiro_p = shapiro(df['resid'])
    print(f"Shapiro-Wilk test: W={shapiro_stat:.4f}, p={shapiro_p:.4g}")

    if shapiro_p < 0.05:
        print("Warning: Residuals deviate significantly from normality (p < 0.05).")
    else:
        print("Residuals appear normally distributed (p ≥ 0.05). Proceeding with Tukey's HSD.")

    # Tukey’s HSD
    print("\nTukey's HSD: Region comparison")
    tukey = pairwise_tukeyhsd(endog=df['FWHM'], groups=df['region'], alpha=0.05)
    print(tukey.summary())
    
def print_aic_lrt_table(full_model, models):
    print("\nTable 1: AIC Scores and LRT vs Full Model")
    print(f"{'Model':35} | {'AIC':>10} | {'ΔDeviance':>12} | {'Δd.f.':>6} | {'P-value':>8}")
    print("-" * 80)
    
    full_llf = full_model.llf
    full_df = full_model.df_model
    
    for m in models:
        label = m["Label"]
        model = m["Model"]
        aic = m["AIC"]
        
        delta_deviance = -2 * (model.llf - full_llf) # LR
        df_diff = full_df - model.df_model
        p_val = chi2.sf(delta_deviance, abs(df_diff)) # chi^2 statistic
        
        print(f"{label:35} | {aic:10.2f} | {delta_deviance:12.4f} | {df_diff:6} | {p_val:8.4f}")


def print_lrt_table(base_label, base_model, nested_models):
    ## prints the LRT table
    print(f"\nTable 2: Likelihood Ratio Tests vs {base_label}")
    print(f"{'Reduced Model':35} | {'AIC':>10} | {'ΔDeviance':>12} | {'Δd.f.':>6} | {'P-value':>8}")
    print("-" * 90)

    base_llf = base_model.llf
    base_df = base_model.df_model

    for label, model in nested_models:
        aic = model.aic
        delta_deviance = -2 * (model.llf - base_llf) # LR
        df_diff = base_df - model.df_model
        p_val = chi2.sf(delta_deviance, abs(df_diff)) # chi^2 statistic

        print(f"{label:35} | {aic:10.2f} | {delta_deviance:12.4f} | {df_diff:6} | {p_val:8.4f}")


def main():
    if len(sys.argv) < 2:
        print("Usage: python model_fwhm_stepwise.py <data.tsv>")
        sys.exit(1)

    # data with no headers
    df = pd.read_csv(sys.argv[1], sep="\t", header=None)
    df.columns = ['FWHM', 'PS', 'region', 'species']
    df['region'] = pd.Categorical(df['region'], categories=['main_retina', 'marginal', 'DRA'], ordered=True) # order the regions because we expect FWHM to increase from main retina to DRA
    df['species'] = df['species'].astype('category')

    # base models for AIC comparison
    models_info = [
        ("Model 1: Full model",             'FWHM ~ PS * region * species'),
        ("Model 2: No 3-way interaction",   'FWHM ~ PS + region + species + PS:region + PS:species + region:species'),
        ("Model 3: No region:species",      'FWHM ~ PS + region + species + PS:region + PS:species'),
        ("Model 4: No PS:species",          'FWHM ~ PS + region + species + PS:region'),
        ("Model 5: No species",             'FWHM ~ PS + region + PS:region'),
        ("Model 6: Additive model",         'FWHM ~ PS + region'),
    ]

    models = []
    for label, formula in models_info: # we fit the different models
        model = fit_model(formula, df)
        models.append({
            "Label": label,
            "Model": model,
            "AIC": model.aic,
            "df_model": model.df_model
        })

    # Table 1: AIC
    # full model is Model 1
    full_model = next(m for m in models if m["Label"].startswith("Model 1"))
    print_aic_lrt_table(full_model["Model"], models) # AIC comparison against full model


    # get best AIC model
    best = min(models, key=lambda x: x["AIC"])
    best_label = best["Label"]
    best_model = best["Model"]
    print(f"\nBest model based on AIC: {best_label}\n")

    # define LRT comparisons by nesting from the best model
    lrt_models = []

    # nested reductions based on best model
    if best_label.startswith("Model 3"):
        lrt_models = [
            ("Model 3_1: No PS:region",        fit_model('FWHM ~ PS + region + species + PS:species', df)),
            ("Model 3_2: No species",          fit_model('FWHM ~ PS + region + PS:region', df)),
            ("Model 3_3: No region",           fit_model('FWHM ~ PS + species + PS:species', df)),
        ]
    elif best_label.startswith("Model 4"):
        lrt_models = [
            ("Model 4_1: No PS:region",        fit_model('FWHM ~ PS + region + species', df)),
            ("Model 4_2: No region",           fit_model('FWHM ~ PS + species', df)),
            ("Model 4_3: No species",          fit_model('FWHM ~ PS + region + PS:region', df)),
        ]
    elif best_label.startswith("Model 5"):
        lrt_models = [
            ("Model 5_1: No PS:region",        fit_model('FWHM ~ PS + region', df)),
            ("Model 5_2: No region",           fit_model('FWHM ~ PS', df)),
        ]

    # Table 2: LRTs
    if lrt_models:
        print_lrt_table(best_label, best_model, lrt_models)
    else:
        print("No nested reductions defined for this best model.")

    for label, model in lrt_models:
        print(f"\nSummary of {label}:")
        print(model.summary())

    ## perform the Tukey's HSD test
    tukey_on_region_after_model(df)
    
if __name__ == "__main__":
    main()
