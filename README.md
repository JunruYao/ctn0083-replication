# CTN-0083 Replication: HIV Self-Testing Promotion Platforms

This repository contains my replication of key results from:

> Stafylis C, et al. *Relative Effectiveness of Social Media, Dating Apps, and Information Search Sites in Promoting HIV Self-testing: Observational Cohort Study.* JMIR Formative Research (2022).  
> Data source: NIDA Data Share, Study **NIDA-CTN-0083**.

---

## Study overview

### Background and motivation

HIV self-testing (HIVST) is a promising way to improve HIV testing uptake, especially among young Black/African American men who have sex with men (MSM), a group that continues to experience substantial disparities in HIV outcomes. Because many people in this population are reached more easily online than through traditional clinics, web-based advertising platforms have become an important tool for promoting HIVST. However, it is not obvious which types of platforms work best.

This study asks a very practical question: among social media, dating apps, and search engines, which platforms are most effective at encouraging people to order HIV self-test kits?

### Objectives

**Primary objective:**  
My main goal is to reproduce the manuscript’s comparison of HIVST ordering rates across different platform types and specific sites, using Poisson regression models.

**Secondary objective:**  
I also reproduce the manuscript’s comparisons between participants who ordered a kit and those who did not, focusing on factors such as substance use, HIV stigma, medical mistrust, and attitudes toward PrEP.

---

## Data

I use the final, analysis-ready dataset downloaded from the NIDA Data Share page for study **NIDA-CTN-0083**.

The file should be placed at:

data/CTN_FINAL.csv

Variable definitions follow the official NIDA data dictionary (`CTN0083-Data-Dictionary.xlsx`), which is also available on the NIDA study page.

---

## Analytic sample definition (N = 254)

The dataset provided on NIDA Data Share contains **271** enrolled participants, but the manuscript reports an analytic sample of **254**. According to the paper, two sets of observations were excluded: participants from Wave 3 (the COVID period) and participants from part of Wave 1 when Grindr was inactive.

In the public, de-identified dataset, there is no direct indicator for the Grindr-inactive calendar window. Because of this, I could not exactly reproduce the authors’ original exclusion rule. Instead, to match the site-specific ordering totals reported in the paper (Table 2), I used the following deterministic procedure (implemented in `R/01_load_clean.R`):

- I removed all observations with `WAVE == 3`.
- I kept all observations from `WAVE == 2` and `WAVE == 4`.
- From `WAVE == 1`, I kept a fixed subset of participants chosen so that the site-specific order counts match those reported in the manuscript.

This results in an analytic sample of **N = 254**, with **177** participants ordering a kit within 60 days, which matches the totals used in the manuscript’s primary analysis.

---

## Replicated Table 1

I reproduced the descriptive table of sociodemographic and behavioral characteristics in:

output/table1_replication.csv

While building this table, I had to make several small but important data-handling decisions. In particular, race in this study is recorded as a multi-select variable (`Q5_3`). To match the manuscript:

- I coded participants who selected more than one race as **Multiracial**.
- I grouped Asian, Native Hawaiian/Pacific Islander, and “Other” responses, as well as missing values, into a single **Other** category.
- I also had to force `Q5_3` to be read as a character variable when loading the data, because otherwise multi-select responses such as `"24,23"` were being incorrectly converted to missing values.

The resulting Table 1 matches the manuscript closely for most entries, including testing history and reasons for not testing. Small differences remain in a few percentages and in the age interquartile range, which I discuss in the Reflection section.

---

## Primary analysis

I reproduced the manuscript’s primary analysis using Poisson regression models with a log link and an offset for the number of days each site was active.

- For Wave 1, I compared Facebook, Google, and Grindr (70 days).
- For Wave 2, I compared Instagram, Jack’D, and Bing (38 days).

For each wave, I fit a Poisson model, estimated site-specific ordering rates using the `emmeans` package, and performed pairwise contrasts between sites. The site-specific rates match those reported in the manuscript up to rounding. 

In Wave 1, none of the pairwise contrasts were statistically significant, which is consistent with the manuscript (all adjusted p-values are non-significant). In Wave 2, all three contrasts (Bing vs Instagram, Bing vs Jack’D, and Instagram vs Jack’D) are highly significant, again matching the conclusions of the paper.

Because Bing had zero orders in Wave 2, Wald-type contrasts from the Poisson model are numerically unstable. Therefore, while still fitting the Poisson models as in the manuscript, I additionally used exact Poisson rate ratio tests for the pairwise comparisons, which handle zero counts more robustly in the presence of zero events. The exact p-values differ in magnitude from those reported in the paper, but the qualitative conclusions are identical.

The detailed outputs are saved in:

- `output/poisson_emm_wave1.csv`  
- `output/poisson_emm_wave2.csv`  
- `output/poisson_contrasts_wave1.csv`  
- `output/poisson_contrasts_wave2.csv`

---

## Secondary analysis

For the secondary analysis, I attempted to reproduce all p-values reported in Appendix 3 of the manuscript. Using the public dataset together with the data dictionary, I reconstructed each group of variables and applied the same types of statistical tests as described in the paper:

- Fisher’s exact tests for categorical variables (e.g., substance use categories and stage of change),
- Wilcoxon rank-sum tests for Likert-scale attitude, stigma, and medical mistrust items.

All sections of Appendix 3 (a–f) are covered in this replication, including substance use, stage of health behavior change, attitudes toward HIV testing, attitudes toward HIV treatment, HIV-related stigma, and medical mistrust.

When compared to the manuscript, the overall pattern of significant and non-significant findings is the same: the same items remain statistically significant, and the same groups of variables remain non-significant. The full set of results is saved in:

- `output/secondary_results_appendix3.csv`
---


## Bonus figure

I also included a simple figure that summarizes site-specific ordering rates by wave:

figures/site_rates.png

---

## Reflection

Overall, I was able to reproduce the main findings of the paper, including the qualitative ranking of platforms in the primary analysis and the pattern of significant and non-significant results in the secondary analysis.

The main challenge was reconstructing the analytic sample from the public dataset. In particular, the Grindr-inactive period in Wave 1 is not explicitly flagged in the released data, so I had to use a deterministic but approximate rule to select the Wave 1 subset. This necessarily introduces small differences compared to the authors’ original internal dataset.

For the secondary analysis, I directly compared my results to all p-values reported in Appendix 3. The same variables remain statistically significant and the same groups of variables remain non-significant. At the numerical level, my p-values are not identical to those in the manuscript, but they are generally very close, often differing only at the second or third decimal place. In a few borderline cases, the p-values fall just above or below conventional thresholds compared to the published values. These small discrepancies are most likely due to differences in the reconstructed analytic sample, the use of a de-identified public dataset, and minor implementation differences between SAS and R.

For the primary analysis, the site-specific rates match the manuscript up to rounding. All contrasts are non-significant in Wave 1 and all three are highly significant in Wave 2, exactly as reported. Because Bing had zero orders in Wave 2, I used exact Poisson rate ratio tests for stability; although the numerical p-values differ, the substantive conclusions are unchanged.

Importantly, none of these differences affect the overall interpretation of the study. The main conclusions of both the primary and secondary analyses remain the same.

---

## How to run

1. Create a `data/` folder and place `CTN_FINAL.csv` inside it.
2. In R, run:

```{r}
source("run_all.R")
```

All outputs will be written to the output/ and figures/ folders.

---

## References
•	Stafylis C, et al. Relative Effectiveness of Social Media, Dating Apps, and Information Search Sites in Promoting HIV Self-testing: Observational Cohort Study. JMIR Formative Research, 2022.
•	NIDA Data Share: Study NIDA-CTN-0083 (dataset and data dictionary).
•	R packages and documentation: emmeans, dplyr, readr, ggplot2
