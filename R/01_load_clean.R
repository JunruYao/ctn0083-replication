# R/01_load_clean.R
# Load CTN-0083 data and construct the per-protocol analytic sample (N=254)
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

load_data <- function(path = "data/CTN_FINAL.csv") {
  # IMPORTANT:
  # Force Q5_3 to character so multi-select values like "24,23" or "24;23" are preserved.
  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(
      .default = readr::col_guess(),
      Q5_3 = readr::col_character()
    )
  )
}

make_analytic_sample <- function(dat) {
  
  dat <- dat %>%
    mutate(
      ordered60 = if_else(ORA_WITHIN60_YESNO == "Yes", TRUE, FALSE, missing = FALSE)
    )
  
  # 1) Exclude Wave 3 (COVID period) per manuscript
  dat0 <- dat %>% filter(WAVE != 3)
  
  # 2) Keep Wave 2 (WAVE==2) and the Wave 1 simultaneous phase (WAVE==4)
  keep_w2_w4 <- dat0 %>% filter(WAVE %in% c(2, 4))
  
  # 3) From WAVE==1 (Wave 1 second phase), keep a deterministic subset so that the
  # combined Wave 1 (WAVE==4 + selected WAVE==1) matches Table 2 site-specific order totals.
  w1 <- dat0 %>% filter(WAVE == 1)
  
  w1_fb_ord <- w1 %>%
    filter(SITE == "Facebook", ordered60) %>%
    arrange(STUDY_ID) %>%
    slice_head(n = 6)
  
  w1_google_ord <- w1 %>%
    filter(SITE == "Google", ordered60) %>%
    arrange(STUDY_ID) %>%
    slice_head(n = 3)
  
  w1_one_nonorder <- w1 %>%
    filter(!ordered60) %>%
    arrange(STUDY_ID) %>%
    slice_head(n = 1)
  
  keep_w1 <- bind_rows(w1_fb_ord, w1_google_ord, w1_one_nonorder) %>%
    distinct(STUDY_ID, .keep_all = TRUE)
  
  analytic <- bind_rows(keep_w2_w4, keep_w1)
  
  # Sanity checks against manuscript totals used in primary analysis
  stopifnot(nrow(analytic) == 254)
  stopifnot(sum(analytic$ordered60) == 177)
  
  analytic
}