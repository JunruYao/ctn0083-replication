# run_all.R
# One-shot replication runner: produces Table 1, primary analysis outputs, and secondary analysis outputs.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})

source("R/01_load_clean.R")
source("R/02_table1.R")
source("R/03_primary_poisson.R")
source("R/04_secondary.R")

# Load + analytic sample
dat <- load_data("data/CTN_FINAL.csv")
analytic <- make_analytic_sample(dat)

# 1. Table 1
tab1 <- make_table1(analytic)
write_csv(tab1, "output/table1_replication.csv")

# 2. Primary analysis
tab2 <- make_table2(analytic)
write_csv(tab2, "output/table2_replication.csv")

w1 <- fit_poisson_wave(tab2, "1a", p_adjust_method = "BH")
w2 <- fit_poisson_wave(tab2, "2",  p_adjust_method = "BH")

write_csv(w1$emm, "output/poisson_emm_wave1.csv")
write_csv(w2$emm, "output/poisson_emm_wave2.csv")
write_csv(w1$contrasts, "output/poisson_contrasts_wave1.csv")
write_csv(w2$contrasts, "output/poisson_contrasts_wave2.csv")

# 3. Secondary analysis
sec_all <- make_secondary_results_appendix3(analytic, dictionary_path = "CTN0083-Data-Dictionary.xlsx")
write_csv(sec_all, "output/secondary_results_appendix3.csv")

# -----------------------
# Bonus figure: site-specific ordering rates
# -----------------------
fig_dat <- tab2 %>%
  mutate(wave = if_else(wave_label == "1a", "Wave 1", "Wave 2"))

p <- ggplot(fig_dat, aes(x = SITE, y = rate)) +
  geom_col() +
  facet_wrap(~wave, scales = "free_x") +
  labs(
    title = "HIV self-test kit ordering rates by advertising site and wave",
    x = "Site",
    y = "Orders per day"
  ) +
  theme_minimal()

ggsave("figures/site_rates.png", p, width = 9, height = 4, dpi = 200)

message("Done. Outputs written to output/ and figures/.")
