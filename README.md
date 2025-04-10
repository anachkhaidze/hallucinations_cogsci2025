# Visual Imagery Vividness Predicts the Complexity of Induced Hallucinations

This repository contains all the data, code, and materials required to reproduce the analyses presented in the above article.

### Contents

1. **Analysis scrtipts**
   - Preprocess hallucination descriptions and conduct NLP: `analysis/hallucination_preprocess_assign_ls_norms.ipynb`
   - Run regression models: `analysis/hallucinations_cogsci2025.R`
   - Generate plots: `analysis/hallucination_plots.ipynb`
   - Create combined descriptions from weak and strong imagers for generating _DALL·E_ images: `analysis/create_subsets_for_dalle.ipynb`

2. **Data**
   - Raw hallucination data for preprocessing and NLP: `data/hallucinations.csv`
   - Preprocessed data for regression analysis: `data/hallucinations_df_for_r.csv`
   - Grouped descriptions from weak imagers: `data/grouped_descriptions_aphantasia.csv`
   - Grouped descriptions from strong imagers: `data/grouped_descriptions_imagery.csv`
   - _DALL·E_ images: `images`

3. **Materials**
   - Lancaster Sensorimotor Norms [(Lynott et al., 2020)](https://link.springer.com/article/10.3758/s13428-019-01316-z): `norms/lancaster.csv`
   - The Ganzflicker experience [(Königsmark et al., 2021)](https://www.sciencedirect.com/science/article/pii/S0010945221001957) can be found [here](https://forms.gle/tdKRKhva3uqC68tS9). Happy Hallucinating!
  
### References
   - Königsmark, V. T., Bergmann, J., & Reeder, R. R. (2021). The Ganzflicker experience: High probability of seeing vivid and complex pseudo-hallucinations with imagery but not aphantasia. Cortex, 141, 522–534.
   - Lynott, D., Connell, L., Brysbaert, M., Brand, J., & Carney, J. (2020). The Lancaster Sensorimotor Norms: Multidimensional measures of perceptual and action strength for 40,000 English words. Behavior Research Methods, 52(3), 1271–1291.
