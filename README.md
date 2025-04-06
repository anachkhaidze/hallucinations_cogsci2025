# Visual Imagery Vividness Predicts the Complexity of Induced Hallucinations

This repository contains all the data, code, and materials required to reproduce the analyses presented in the above article. The Ganzflicker study can be found [here](https://forms.gle/tdKRKhva3uqC68tS9).

This repository is a Quarto project endowed with an `renv` R environment to ensure the stability of the packages used in the analysis and to guarantee the reproducibility of the results.

### Contents

1. **Hallucination descriptions preprocessing and NLP analysis**
   - Analysis code: `hallucination_preprocess_assign_ls_norms.ipynb`
   - Datafile: `hallucinations.csv`
   - Lancaster Sensorimotor Norms [(Lynott et al., 2020)](https://link.springer.com/article/10.3758/s13428-019-01316-z): `lancaster.csv`

2. **Regression models**
   - Analysis code: `hallucinations_cogsci2025.R`
   - Datafile: `hallucinations_df_for_r.csv`

3. **Code to recreate plots**
   - Jupyter Notebook: `hallucination_plots.ipynb`
