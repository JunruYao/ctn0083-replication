# R/03_primary_poisson.R
# Primary analysis: Poisson model, site-specific rates, and contrasts
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(emmeans)
})

make_table2 <- function(analytic) {
  table2 <- analytic %>%
    mutate(wave_label = case_when(
      WAVE %in% c(1, 4) ~ "1a",
      WAVE == 2 ~ "2",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(wave_label)) %>%
    group_by(wave_label, SITE) %>%
    summarise(orders = sum(ordered60), .groups = "drop") %>%
    mutate(
      days = if_else(wave_label == "1a", 70, 38),
      rate = orders / days
    )
  
  if (!any(table2$wave_label == "2" & table2$SITE == "Bing")) {
    table2 <- bind_rows(table2, tibble(wave_label="2", SITE="Bing", orders=0, days=38, rate=0))
  }
  
  table2 %>%
    arrange(wave_label, match(SITE, c("Facebook","Google","Grindr","Instagram","Jack'd","Bing")))
}

# exact two-rate test for a pair (handles zeros well)
exact_rate_contrast <- function(x1, t1, x2, t2) {
  # poisson.test with two samples returns exact p-value and CI for rate ratio (x1/t1)/(x2/t2)
  pt <- poisson.test(c(x1, x2), T = c(t1, t2), r = 1)
  tibble(
    ratio = unname((x1 / t1) / (x2 / t2)),
    asymp.LCL = unname(pt$conf.int[1]),
    asymp.UCL = unname(pt$conf.int[2]),
    p.value = unname(pt$p.value)
  )
}

fit_poisson_wave <- function(table2, wave_label, p_adjust_method = "BH") {
  datw <- table2 %>%
    filter(wave_label == !!wave_label) %>%
    mutate(SITE = factor(SITE))
  
  # Poisson model (kept to satisfy "fit the Poisson model" requirement)
  fit <- glm(orders ~ SITE + offset(log(days)), family = poisson(), data = datw)
  
  # Site-specific rates (emmeans on response scale is fine for reporting)
  emm <- emmeans(fit, ~ SITE)
  emm_df <- as.data.frame(summary(emm, type = "response")) %>%
    mutate(wave = wave_label)
  
  # Pairwise contrasts using exact Poisson tests (robust with 0 cells)
  sites <- levels(datw$SITE)
  pairs <- combn(sites, 2, simplify = FALSE)
  
  con_df <- purrr::map_dfr(pairs, function(p) {
    a <- p[1]; b <- p[2]
    xa <- datw$orders[datw$SITE == a]; ta <- datw$days[datw$SITE == a]
    xb <- datw$orders[datw$SITE == b]; tb <- datw$days[datw$SITE == b]
    
    out <- exact_rate_contrast(xa, ta, xb, tb)
    
    tibble(
      contrast = paste(a, "/", b),
      ratio = out$ratio,
      asymp.LCL = out$asymp.LCL,
      asymp.UCL = out$asymp.UCL,
      p.value = out$p.value
    )
  }) %>%
    mutate(
      p_adj = p.adjust(p.value, method = p_adjust_method),
      wave = wave_label
    )
  
  list(fit = fit, emm = emm_df, contrasts = con_df)
}