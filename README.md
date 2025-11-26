Author: L. Geers 

Date: December 2024

This script locates and statistically tests the effect of one fixed effect in time-series data using a cross-validation procedure combined with linear mixed-effects models (LMMs). 
The method ensures that statistical tests are conducted on time points identified in an unbiased way, reducing the risk of overfitting and capitalizing on the robustness of mixed-effects modeling.
The approach is inspired by Mathôt & Vilotijević (2022).

⚙️ Methodology

1. Data subdivision

- The dataset is split into 4 subsets.
- Each subset is iteratively treated as the test set, while the remaining three form the training set.

2. Training phase

- A LMM with the fixed effect is run on each sample in the training set. The sample with the highest absolute z-value is selected as the candidate time point in the held-out testing set.
- This procedure is repeated for all subsets.

3. Testing phase
- A single LMM is conducted for the fixed effect on the samples identified through cross-validation.

📂 Data Requirements

The script assumes a trial-based dataset with the following columns:
- Subject identifier named "subj"
- Trial number named "trial"
- Fixed effect named as desired but specified in "Parameters" section
- Dependent variable named as desired but specified in "Parameters" section

🔧 Parameters

Before running the script, make sure to specify:
- Fixed effect name → the column representing the experimental condition.
- Dependent variable name → the column containing time-series values.
- Number of samples → total samples per trial.

📖 References

Mathôt, S., & Vilotijević, A. (2022). Cross-validation for time-series data in experimental psychology. Behavior Research Methods, 54, 1–15. https://doi.org/10.3758/s13428-021-01684-0
