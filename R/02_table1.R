# R/02_table1.R
# Replicate Table 1: sociodemographic and behavioral characteristics (N=254)
# ------------------------------------------------------------------------------
# Data dictionary alignment (Baseline):
# Q3_1 = age (numeric)
# Q5_1 = Hispanic/Latinx (1=Yes;2=No)
# Q5_3 = Race self-identification (multi-select, char):
#   25 AI/AN; 26 Asian; 24 Black; 27 NH/PI; 23 White; 28 Other
# Manuscript categories:
#   AI/AN, Black, White, Other (26/27/28 + missing), Multiracial (multi-select)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
})

QTYPE_DEFAULT <- 2  # if age Q1 still low, try 3 (software quartile convention)

fmt_median_iqr <- function(x, digits = 0, qtype = QTYPE_DEFAULT) {
  x <- x[!is.na(x)]
  med <- median(x)
  q <- quantile(x, probs = c(0.25, 0.75), type = qtype, names = FALSE)
  sprintf(paste0("%.", digits, "f (%.", digits, "f-%.", digits, "f)"), med, q[1], q[2])
}

fmt_n_pct <- function(n, N, digits = 1) {
  pct <- 100 * n / N
  sprintf(paste0("%d (%.", digits, "f%%)"), n, pct)
}

# Robust splitter for multi-select stored as "24,23" OR "24;23" OR "24 23"
split_codes <- function(x) {
  if (is.na(x)) return(character(0))
  s <- as.character(x)
  # split on comma, semicolon, or whitespace
  codes <- unlist(str_split(s, pattern = "[,;\\s]+"))
  codes <- str_trim(codes)
  codes[codes != ""]
}

race_collapse <- function(q5_3) {
  codes <- split_codes(q5_3)
  
  # Manuscript does not show a separate Missing row; treat missing/unknown as Other
  if (length(codes) == 0) return("Other")
  
  # Multi-select -> Multiracial
  if (length(unique(codes)) > 1) return("Multiracial")
  
  code <- unique(codes)[1]
  
  dplyr::case_when(
    code == "25" ~ "American Indian or Alaskan Native",
    code == "24" ~ "Black or African American",
    code == "23" ~ "White",
    code %in% c("26", "27", "28") ~ "Other",
    TRUE ~ "Other"
  )
}

make_table1 <- function(analytic) {
  
  N <- nrow(analytic)
  
  # Age (Q3_1)
  age_str <- fmt_median_iqr(analytic$Q3_1, digits = 0, qtype = QTYPE_DEFAULT)
  
  # Ethnicity: Hispanic/Latinx (Q5_1: 1=Yes;2=No)
  hisp_n <- sum(analytic$Q5_1 == 1, na.rm = TRUE)
  hisp_str <- fmt_n_pct(hisp_n, N, digits = 1)
  
  # Race (Q5_3)
  race_levels <- c(
    "American Indian or Alaskan Native",
    "Black or African American",
    "White",
    "Other",
    "Multiracial"
  )
  
  race_tab <- analytic %>%
    mutate(race_cat = vapply(Q5_3, race_collapse, character(1))) %>%
    count(race_cat, name = "n") %>%
    tidyr::complete(race_cat = race_levels, fill = list(n = 0)) %>%
    mutate(
      race_cat = factor(race_cat, levels = race_levels),
      value = fmt_n_pct(n, N, digits = 1)
    ) %>%
    arrange(race_cat) %>%
    select(race_cat, value)
  
  # PrEP history (keep your original coding)
  prep_never <- sum(analytic$Q6_2 == 3, na.rm = TRUE)
  prep_past6 <- sum(analytic$Q6_2 %in% c(1, 2), na.rm = TRUE)
  
  # Male partners past 90 days
  partners_str <- fmt_median_iqr(analytic$Q11_2, digits = 0, qtype = QTYPE_DEFAULT)
  
  # Condom use frequency
  condom_levels <- c(
    "1"="Never",
    "2"="Sometimes",
    "3"="About half the time",
    "4"="Most of the time",
    "5"="Always"
  )
  condom_tab <- analytic %>%
    mutate(condom_use = factor(as.character(Q11_3),
                               levels = names(condom_levels),
                               labels = condom_levels)) %>%
    count(condom_use, name = "n") %>%
    mutate(value = fmt_n_pct(n, N, digits = 1)) %>%
    select(condom_use, value)
  
  # Condomless receptive anal sex in past 90 days (1=Yes;2=No)
  cras_yes <- sum(analytic$Q11_4 == 1, na.rm = TRUE)
  cras_str <- fmt_n_pct(cras_yes, N, digits = 1)
  
  # Ever tested for HIV (1=Yes;2=No)
  ever_tested_yes <- sum(analytic$Q11_5 == 1, na.rm = TRUE)
  ever_tested_str <- fmt_n_pct(ever_tested_yes, N, digits = 1)
  
  # Months since last HIV test among those tested
  months_str <- fmt_median_iqr(
    analytic$LAST_HIV_TEST_MONTHS[analytic$Q11_5 == 1],
    digits = 0, qtype = QTYPE_DEFAULT
  )
  
  # Not tested count
  not_tested_n <- sum(analytic$Q11_5 == 2, na.rm = TRUE)
  not_tested_str <- fmt_n_pct(not_tested_n, N, digits = 1)
  
  # Reasons for not testing (among Q11_5==2)
  reason_levels <- c(
    "1"="Unlikely to be exposed to HIV",
    "2"="Afraid of testing HIV-positive",
    "3"="Did not want to think about HIV/HIV-positive",
    "4"="Worried about names being reported if positive",
    "5"="Dislike for needles",
    "6"="Unable to trust that the results will be confidential",
    "7"="Unaware of where to get tested",
    "8"="Other reasons"
  )
  reasons_tab <- analytic %>%
    filter(Q11_5 == 2) %>%
    mutate(reason = factor(as.character(Q11_7),
                           levels = names(reason_levels),
                           labels = reason_levels)) %>%
    count(reason, name = "n") %>%
    mutate(value = fmt_n_pct(n, not_tested_n, digits = 1)) %>%
    select(reason, value)
  
  # Assemble output
  out <- tibble(
    Characteristic = c(
      "Age in years, median (IQR)",
      "Ethnicity, n (%)",
      "  Hispanic/Latinx",
      "Race, n (%)",
      paste0("  ", as.character(race_tab$race_cat)),
      "History of PrEP uptake, n (%)",
      "  Never taken PrEP",
      "  In the past 6 months",
      "Number of male sex partners in the past 90 days, median (IQR)",
      "Condom use, n (%)",
      paste0("  ", condom_tab$condom_use),
      "Condomless receptive anal sex in the past 90 days, n (%)",
      "Ever tested for HIV during lifetime, n (%)",
      "If tested for HIV, median (IQR)",
      "  Months since last HIV test",
      "If not tested for HIV, n (%)",
      "Main reasons cited by participants for not getting tested, n (%)",
      paste0("  ", reasons_tab$reason)
    ),
    Value = c(
      age_str,
      "",
      hisp_str,
      "",
      race_tab$value,
      "",
      fmt_n_pct(prep_never, N, digits = 1),
      fmt_n_pct(prep_past6, N, digits = 1),
      partners_str,
      "",
      condom_tab$value,
      cras_str,
      ever_tested_str,
      "",
      months_str,
      not_tested_str,
      "",
      reasons_tab$value
    )
  )
  
  out
}