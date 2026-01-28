# R/04_secondary.R
# Secondary analysis: reproduce ALL Appendix 3 p-values (N=254)
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(stringr)
  library(purrr)
  library(tibble)
})

# ---- 1) Load data dictionary and helper to find variable IDs ----

load_dictionary <- function(path = "CTN0083-Data-Dictionary.xlsx") {
  dd_raw <- read_excel(path, col_names = TRUE)
  names(dd_raw) <- c("FIELD_ID", "FIELD_DESCRIPTION", "VALID_VALUES", "TIMEPOINT", "VARIABLE_TYPE")
  dd <- dd_raw %>%
    filter(!is.na(FIELD_ID)) %>%
    mutate(
      FIELD_ID = as.character(FIELD_ID),
      FIELD_DESCRIPTION = as.character(FIELD_DESCRIPTION)
    )
  dd
}

# Find a variable by matching text in FIELD_DESCRIPTION (case-insensitive).
# Returns a single FIELD_ID, throws error if not uniquely found (so you notice mismatch).
find_var <- function(dd, pattern, prefer_startswith = NULL) {
  hits <- dd %>%
    filter(str_detect(str_to_lower(FIELD_DESCRIPTION), str_to_lower(pattern)))
  
  if (!is.null(prefer_startswith) && nrow(hits) > 1) {
    hits2 <- hits %>% filter(str_starts(FIELD_ID, prefer_startswith))
    if (nrow(hits2) > 0) hits <- hits2
  }
  
  if (nrow(hits) == 0) {
    stop(paste0("Could not find variable for pattern: '", pattern, "'"))
  }
  if (nrow(hits) > 1) {
    stop(paste0(
      "Multiple variables matched pattern: '", pattern, "'. Matched FIELD_IDs: ",
      paste(hits$FIELD_ID, collapse = ", "),
      ". Please refine the pattern."
    ))
  }
  hits$FIELD_ID[1]
}

# ---- 2) Tests used in Appendix 3 ----

p_fisher <- function(x, g) {
  df <- tibble(x = x, g = g) %>% filter(!is.na(x), !is.na(g))
  if (nrow(df) == 0) return(NA_real_)
  fisher.test(table(df$x, df$g))$p.value
}

p_wilcox <- function(x, g) {
  df <- tibble(x = x, g = g) %>% filter(!is.na(x), !is.na(g))
  if (nrow(df) == 0) return(NA_real_)
  xnum <- suppressWarnings(as.numeric(df$x))
  if (all(is.na(xnum))) stop("Wilcoxon: x is not numeric/convertible.")
  wilcox.test(xnum ~ df$g)$p.value
}

# ---- 3) Appendix 3 section (a): substance use categories ----
# Appendix says: None / Problem use / High Risk; and sometimes problem+high risk combined.
# We build a 3-level variable from the two DSM-like items per substance:
#   score = (criterion1 == Yes) + (criterion2 == Yes)
#   None      : use == No
#   Problem   : use == Yes and score == 1
#   High risk : use == Yes and score == 2
# If you want to collapse Problem + High risk (as footnote 3 suggests), set collapse = TRUE.

make_substance_cat <- function(use, crit1, crit2, collapse = FALSE) {
  # expected coding: 1=Yes, 2=No (per dictionary)
  out <- rep(NA_character_, length(use))
  
  # none if not used
  out[use == 2] <- "None"
  
  # if used, compute score
  used_idx <- which(use == 1)
  score <- rep(NA_integer_, length(use))
  score[used_idx] <- (crit1[used_idx] == 1) + (crit2[used_idx] == 1)
  
  out[use == 1 & score == 0] <- "None"         # conservative: treat as none if used but no criteria
  out[use == 1 & score == 1] <- "Problem use"
  out[use == 1 & score == 2] <- "High risk"
  
  if (collapse) {
    out[out %in% c("Problem use", "High risk")] <- "Problem/High risk"
  }
  
  factor(out, levels = if (collapse) c("None", "Problem/High risk") else c("None", "Problem use", "High risk"))
}

# ---- 4) Main function: reproduce all Appendix 3 p-values ----

make_secondary_results_appendix3 <- function(
    analytic,
    dictionary_path = "CTN0083-Data-Dictionary.xlsx"
) {
  dd <- load_dictionary(dictionary_path)
  
  # ---- a) Substance use blocks (Appendix 3a) ----
  # These are the triads in your dataset:
  # Alcohol: use Q13_1, criteria Q13_3, Q13_4
  # Cannabis: use Q13_5, criteria Q13_6, Q13_7
  # Stimulants (cocaine/crack): use Q13_8, criteria Q13_9, Q13_10
  # Opioid (prescription opioid): use Q13_14, criteria Q13_15, Q13_16
  # Sedative: use Q13_17, criteria Q13_18, Q13_19
  # Prescribed stimulant: use Q13_20, criteria Q13_21, Q13_22
  
  substance_specs <- tribble(
    ~label,                   ~use,    ~c1,     ~c2,      ~collapse,
    "Alcohol",                "Q13_1",  "Q13_3", "Q13_4",  FALSE,
    "Cannabis",               "Q13_5",  "Q13_6", "Q13_7",  FALSE,
    "Stimulants",             "Q13_8",  "Q13_9", "Q13_10", TRUE,   # often collapsed in Appendix due low counts
    "Opioid",                 "Q13_14", "Q13_15","Q13_16", TRUE,
    "Sedative",               "Q13_17", "Q13_18","Q13_19", TRUE,
    "Prescribed stimulant",   "Q13_20", "Q13_21","Q13_22", TRUE
  )
  
  res_a <- substance_specs %>%
    mutate(
      var_in_data = map_lgl(use, ~ .x %in% names(analytic)) &
        map_lgl(c1,  ~ .x %in% names(analytic)) &
        map_lgl(c2,  ~ .x %in% names(analytic))
    ) %>%
    filter(var_in_data) %>%
    mutate(
      x = pmap(list(use, c1, c2, collapse), ~ make_substance_cat(analytic[[..1]], analytic[[..2]], analytic[[..3]], collapse = ..4)),
      p_value = map_dbl(x, ~ p_fisher(.x, analytic$ordered60)),
      test = "Fisher exact (Appendix 3a)",
      appendix_section = "a"
    ) %>%
    select(appendix_section, outcome = label, test, p_value)
  
  # ---- b) Stage of Health Behavior Change (Appendix 3b) ----
  # We locate this variable by matching the exact text in the dictionary.
  # If the dictionary text doesn’t contain these statements, you MUST provide the FIELD_ID manually.
  # (In many releases, it is a single item with 5 levels.)
  
  stage_var <- tryCatch({
    # use one of the category statements as a search key
    find_var(dd, "I do not see any need to regularly test for HIV")
  }, error = function(e) NA_character_)
  
  res_b <- if (!is.na(stage_var) && stage_var %in% names(analytic)) {
    tibble(
      appendix_section = "b",
      outcome = "Stage of Health Behavior Change (overall)",
      test = "Fisher exact (Appendix 3b)",
      p_value = p_fisher(factor(analytic[[stage_var]]), analytic$ordered60)
    )
  } else {
    tibble(
      appendix_section = "b",
      outcome = "Stage of Health Behavior Change (overall)",
      test = "Fisher exact (Appendix 3b)",
      p_value = NA_real_
    )
  }
  
  # ---- c) Attitudes toward HIV testing (Appendix 3c) ----
  # These are Likert items; Appendix marks Fisher/Wilcoxon—here we follow Appendix and use Fisher if categorical, otherwise Wilcoxon.
  # In practice these are ordinal → Wilcoxon is appropriate and matches the Appendix '‡' style.
  
  hiv_test_items <- c(
    "Getting tested for HIV helps people feel better",
    "Getting tested for HIV helps people from getting HIV",
    "People in my life would leave if I had HIV",
    "People who tested positive for HIV should hide it from others",
    "I would rather not know if I have HIV"
  )
  
  hiv_test_vars <- map_chr(hiv_test_items, ~ find_var(dd, .x, prefer_startswith = "Q"))
  
  res_c <- tibble(
    appendix_section = "c",
    outcome = hiv_test_items,
    var = hiv_test_vars
  ) %>%
    filter(var %in% names(analytic)) %>%
    mutate(
      test = "Wilcoxon rank-sum (Appendix 3c)",
      p_value = map_dbl(var, ~ p_wilcox(analytic[[.x]], analytic$ordered60))
    ) %>%
    select(appendix_section, outcome, test, p_value)
  
  # ---- d) Attitudes toward HIV treatment (Appendix 3d) ----
  # Appendix explicitly says Wilcoxon.
  hiv_treat_items <- c(
    "I am less threatened by the idea of being HIV positive than I used to be.",
    "I am less worried about HIV infection than I used to be",
    "I think HIV/AIDS is less of a problem than it used to be",
    "I think HIV/AIDS is a less serious threat than it used to be because of new HIV/AIDS treatments",
    "I am much less concerned about becoming HIV positive myself because of new HIV/AIDS treatments",
    "I think that condom use during sex is less necessary now that new HIV/AIDS treatments are available",
    "I think that someone who is HIV positive now needs to care less about condom us",
    "I think that the need for condom use is less than it used to be, because you can always start new treatments",
    "I think that someone who is HIV positive and uses new HIV/AIDS treatments can be cured",
    "I think that new HIV/AIDS treatments can eradicate the virus from your body"
  )
  
  hiv_treat_vars <- map_chr(hiv_treat_items, ~ find_var(dd, .x, prefer_startswith = "Q"))
  
  res_d <- tibble(
    appendix_section = "d",
    outcome = hiv_treat_items,
    var = hiv_treat_vars
  ) %>%
    filter(var %in% names(analytic)) %>%
    mutate(
      test = "Wilcoxon rank-sum (Appendix 3d)",
      p_value = map_dbl(var, ~ p_wilcox(analytic[[.x]], analytic$ordered60))
    ) %>%
    select(appendix_section, outcome, test, p_value)
  
  # ---- e) HIV-related stigma (Appendix 3e) ----
  stigma_items <- c(
    "I feel afraid of people living with HIV/AIDS",
    "I could not be friends with someone who has HIV/AIDS",
    "People who get HIV/AIDS through sex or drug use got what they deserve",
    "I feel anger toward people with HIV/AIDS"
  )
  
  stigma_vars <- map_chr(stigma_items, ~ find_var(dd, .x, prefer_startswith = "Q"))
  
  res_e <- tibble(
    appendix_section = "e",
    outcome = stigma_items,
    var = stigma_vars
  ) %>%
    filter(var %in% names(analytic)) %>%
    mutate(
      test = "Wilcoxon rank-sum (Appendix 3e)",
      p_value = map_dbl(var, ~ p_wilcox(analytic[[.x]], analytic$ordered60))
    ) %>%
    select(appendix_section, outcome, test, p_value)
  
  # ---- f) Medical mistrust (Appendix 3f) ----
  mistrust_items <- c(
    "You'd better be cautious when dealing with healthcare organizations.",
    "Patients have sometimes been deceived or misled by health care organizations.",
    "When health care organizations make mistakes they usually cover it up.",
    "Health care organizations have sometimes done harmful experiments on patients without their knowledge.",
    "Health care organizations don’t always keep your information totally private.",
    "Sometimes I wonder if health care organizations really know what they are doing.",
    "Mistakes are common in health care organizations."
  )
  
  mistrust_vars <- map_chr(mistrust_items, ~ find_var(dd, .x, prefer_startswith = "Q"))
  
  res_f <- tibble(
    appendix_section = "f",
    outcome = mistrust_items,
    var = mistrust_vars
  ) %>%
    filter(var %in% names(analytic)) %>%
    mutate(
      test = "Wilcoxon rank-sum (Appendix 3f)",
      p_value = map_dbl(var, ~ p_wilcox(analytic[[.x]], analytic$ordered60))
    ) %>%
    select(appendix_section, outcome, test, p_value)
  
  bind_rows(res_a, res_b, res_c, res_d, res_e, res_f) %>%
    arrange(appendix_section, outcome)
}