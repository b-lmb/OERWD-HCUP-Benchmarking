########################## Load Required Libraries ########################## 

library(data.table)  
library(dplyr)         
library(tidyr)        
library(survey)        
library(car)          
library(broom)         
library(purrr)         
library(stringr)    
library(magrittr)
library(Kendall)

########################## NIS Denominator Summary ########################## 

nis_denom_total <- svydesign(id = ~1, weights = ~weight, data = nis_denom_aggregated)

nis_denom_total_result <- svytotal(~encounters, nis_denom_total)
nis_denom_cdiv   <- svyby(~encounters, ~censusdivision, nis_denom_total, svytotal)
nis_denom_female <- svyby(~encounters, ~FEMALE, nis_denom_total, svytotal)
nis_denom_race   <- svyby(~encounters, ~RACE, nis_denom_total, svytotal)
nis_denom_age    <- svyby(~encounters, ~age_group, nis_denom_total, svytotal)

print(nis_denom_total_result)
print(nis_denom_cdiv)
print(nis_denom_female)
print(nis_denom_race)
print(nis_denom_age)

########################## OERWD Denominator Summary ########################## 

oerwd_denom_total   <- oerwd_denom[, .(encounters = sum(encounters))]
oerwd_denom_cdiv    <- oerwd_denom[, .(encounters = sum(encounters)), by = censusdivision]
oerwd_denom_female  <- oerwd_denom[, .(encounters = sum(encounters)), by = FEMALE]
oerwd_denom_race    <- oerwd_denom[, .(encounters = sum(encounters)), by = RACE]
oerwd_denom_age     <- oerwd_denom[, .(encounters = sum(encounters)), by = age_group]

print(oerwd_denom_total)
print(oerwd_denom_cdiv)
print(oerwd_denom_female)
print(oerwd_denom_race)
print(oerwd_denom_age)

########################## NIS Case Totals ########################## 

nis_total_asp <- svydesign(id = ~1, weights = ~weight, data = nis_asp_final)
nis_total_hist <- svydesign(id = ~1, weights = ~weight, data = nis_hist_final)

nis_total_asp_result <- svytotal(~cases, nis_total_asp)
nis_div_asp   <- svyby(~cases, ~censusdivision, nis_total_asp, svytotal)
nis_female_asp <- svyby(~cases, ~FEMALE, nis_total_asp, svytotal)
nis_race_asp   <- svyby(~cases, ~RACE, nis_total_asp, svytotal)
nis_age_asp    <- svyby(~cases, ~age_group, nis_total_asp, svytotal)

nis_total_hist_result <- svytotal(~cases, nis_total_hist)
nis_cdiv_hist   <- svyby(~cases, ~censusdivision, nis_total_hist, svytotal)
nis_female_hist <- svyby(~cases, ~FEMALE, nis_total_hist, svytotal)
nis_race_hist   <- svyby(~cases, ~RACE, nis_total_hist, svytotal)
nis_age_hist    <- svyby(~cases, ~age_group, nis_total_hist, svytotal)

print(nis_total_asp_result)
print(nis_div_asp)
print(nis_female_asp)
print(nis_race_asp)
print(nis_age_asp)

print(nis_total_hist_result)
print(nis_cdiv_hist)
print(nis_female_hist)
print(nis_race_hist)
print(nis_age_hist)

########################## OERWD Case Totals ########################## 

oerwd_total_asp     <- svydesign(id = ~1, weights = ~weight, data = asp_final %>% filter(id == 1))
oerwd_total_asp_w   <- svydesign(id = ~1, weights = ~weight, data = asp_final_w %>% filter(id == 1))
oerwd_total_hist    <- svydesign(id = ~1, weights = ~weight, data = hist_final %>% filter(id == 1))
oerwd_total_hist_w  <- svydesign(id = ~1, weights = ~weight, data = hist_final_w %>% filter(id == 1))

# Aspergillosis
oerwd_total_asp_result <- svytotal(~cases, oerwd_total_asp)
oerwd_cdiv_asp         <- svyby(~cases, ~censusdivision, oerwd_total_asp, svytotal)
oerwd_female_asp       <- svyby(~cases, ~FEMALE, oerwd_total_asp, svytotal)
oerwd_race_asp         <- svyby(~cases, ~RACE, oerwd_total_asp, svytotal)
oerwd_age_asp          <- svyby(~cases, ~age_group, oerwd_total_asp, svytotal)

# Histoplasmosis
oerwd_total_hist_result <- svytotal(~cases, oerwd_total_hist)
oerwd_cdiv_hist         <- svyby(~cases, ~censusdivision, oerwd_total_hist, svytotal)
oerwd_female_hist       <- svyby(~cases, ~FEMALE, oerwd_total_hist, svytotal)
oerwd_race_hist         <- svyby(~cases, ~RACE, oerwd_total_hist, svytotal)
oerwd_age_hist          <- svyby(~cases, ~age_group, oerwd_total_hist, svytotal)

print(oerwd_total_asp_result)
print(oerwd_cdiv_asp)
print(oerwd_female_asp)
print(oerwd_race_asp)
print(oerwd_age_asp)

print(oerwd_total_hist_result)
print(oerwd_cdiv_hist)
print(oerwd_female_hist)
print(oerwd_race_hist)
print(oerwd_age_hist)

########################## Case Survey Designs ##########################

unweighted_design_asp <- svydesign(id = ~1, weights = ~weight, data = asp_final)
weighted_design_asp   <- svydesign(id = ~1, weights = ~weight, data = asp_final_w)

unweighted_design_hist <- svydesign(id = ~1, weights = ~weight, data = hist_final)
weighted_design_hist   <- svydesign(id = ~1, weights = ~weight, data = hist_final_w)

########################## Survey-Weighted Models ##########################

# Aspergillosis
model_asp_total     <- svyglm(cbind(cases, encounters - cases) ~ id, design = unweighted_design_asp, family = quasibinomial())
model_asp_total_w   <- svyglm(cbind(cases, encounters - cases) ~ id, design = weighted_design_asp,   family = quasibinomial())
model_asp_cdiv_w    <- svyglm(cbind(cases, encounters - cases) ~ censusdivision * id, design = weighted_design_asp, family = quasibinomial())
model_asp_female_w  <- svyglm(cbind(cases, encounters - cases) ~ FEMALE * id, design = weighted_design_asp, family = quasibinomial())
model_asp_race_w    <- svyglm(cbind(cases, encounters - cases) ~ RACE * id, design = weighted_design_asp, family = quasibinomial())
model_asp_age_w     <- svyglm(cbind(cases, encounters - cases) ~ age_group * id, design = weighted_design_asp, family = quasibinomial())

# Histoplasmosis
model_hist_total_w  <- svyglm(cbind(cases, encounters - cases) ~ id, design = weighted_design_hist, family = quasibinomial())
model_hist_cdiv_w   <- svyglm(cbind(cases, encounters - cases) ~ censusdivision * id, design = weighted_design_hist, family = quasibinomial())
model_hist_female_w <- svyglm(cbind(cases, encounters - cases) ~ FEMALE * id, design = weighted_design_hist, family = quasibinomial())
model_hist_race_w   <- svyglm(cbind(cases, encounters - cases) ~ RACE * id, design = weighted_design_hist, family = quasibinomial())
model_hist_age_w    <- svyglm(cbind(cases, encounters - cases) ~ age_group * id, design = weighted_design_hist, family = quasibinomial())

########################## Yearly Prevalence Tables ##########################

# Subset designs
design_asp_nis     <- subset(weighted_design_asp, id == 0)
design_asp_oerwd   <- subset(weighted_design_asp, id == 1)
design_hist_nis    <- subset(weighted_design_hist, id == 0)
design_hist_oerwd  <- subset(weighted_design_hist, id == 1)

# Aspergillosis prevalence
asp_nis_prevalence <- svyby(~cases, ~YEAR, design_asp_nis, svytotal) %>%
  mutate(encounters = svyby(~encounters, ~YEAR, design_asp_nis, svytotal)$encounters,
         prevalence = cases / encounters * 10000,
         id = 0) %>%
  select(id, YEAR, cases, se.cases = se, encounters, prevalence)

asp_oerwd_prevalence <- svyby(~cases, ~YEAR, design_asp_oerwd, svytotal) %>%
  mutate(encounters = svyby(~encounters, ~YEAR, design_asp_oerwd, svytotal)$encounters,
         prevalence = cases / encounters * 10000,
         id = 1) %>%
  select(id, YEAR, cases, se.cases = se, encounters, prevalence)

asp_yearly_prev <- bind_rows(asp_nis_prevalence, asp_oerwd_prevalence) %>%
  arrange(YEAR, id)
row.names(asp_yearly_prev) <- NULL

# Histoplasmosis prevalence
hist_nis_prevalence <- svyby(~cases, ~YEAR, design_hist_nis, svytotal) %>%
  mutate(encounters = svyby(~encounters, ~YEAR, design_hist_nis, svytotal)$encounters,
         prevalence = cases / encounters * 10000,
         id = 0) %>%
  select(id, YEAR, cases, se.cases = se, encounters, prevalence)

hist_oerwd_prevalence <- svyby(~cases, ~YEAR, design_hist_oerwd, svytotal) %>%
  mutate(encounters = svyby(~encounters, ~YEAR, design_hist_oerwd, svytotal)$encounters,
         prevalence = cases / encounters * 10000,
         id = 1) %>%
  select(id, YEAR, cases, se.cases = se, encounters, prevalence)

hist_yearly_prev <- bind_rows(hist_nis_prevalence, hist_oerwd_prevalence) %>%
  arrange(YEAR, id)
row.names(hist_yearly_prev) <- NULL

########################## Mann-Kendall Trend Tests ##########################

# Run Mann-Kendall tests
trend_results <- tibble(
  Disease = rep(c("Aspergillosis", "Histoplasmosis"), each = 2),
  Dataset = rep(c("NIS", "OERWD"), 2),
  Tau = c(
    MannKendall(asp_yearly_prev %>% filter(id == 0) %$% prevalence)$tau,
    MannKendall(asp_yearly_prev %>% filter(id == 1) %$% prevalence)$tau,
    MannKendall(hist_yearly_prev %>% filter(id == 0) %$% prevalence)$tau,
    MannKendall(hist_yearly_prev %>% filter(id == 1) %$% prevalence)$tau
  ),
  Pvalue = c(
    MannKendall(asp_yearly_prev %>% filter(id == 0) %$% prevalence)$sl,
    MannKendall(asp_yearly_prev %>% filter(id == 1) %$% prevalence)$sl,
    MannKendall(hist_yearly_prev %>% filter(id == 0) %$% prevalence)$sl,
    MannKendall(hist_yearly_prev %>% filter(id == 1) %$% prevalence)$sl
  )
)

print(asp_yearly_prev)
print(hist_yearly_prev)
print(trend_results)

########################## Total Prevalence CIs ##########################

get_total_prev_ci <- function(model) {
  coefs <- coef(model)
  coef_names <- names(coefs)
  
  make_row <- function(id1_val) {
    row <- setNames(rep(0, length(coefs)), coef_names)
    row["(Intercept)"] <- 1
    row["id1"] <- id1_val
    row
  }
  
  X <- rbind(
    total_nis = make_row(0),
    total_oerwd = make_row(1)
  )
  
  log_odds <- X %*% coefs
  se <- sqrt(diag(X %*% vcov(model) %*% t(X)))
  probs <- plogis(log_odds)
  lower <- plogis(log_odds - 1.96 * se)
  upper <- plogis(log_odds + 1.96 * se)
  
  data.frame(
    group = rownames(X),
    prevalence = probs * 10000,
    lower_95CI = lower * 10000,
    upper_95CI = upper * 10000
  )
}

# Run for all total models
asp_total_unw  <- get_total_prev_ci(model_asp_total)
asp_total_w    <- get_total_prev_ci(model_asp_total_w)
hist_total_unw <- get_total_prev_ci(model_hist_total)
hist_total_w   <- get_total_prev_ci(model_hist_total_w)

########################## Subgroup Prevalence CIs ##########################

get_prev_ci <- function(model, levels_main, interaction_prefix = NULL, ref = NULL) {
  coefs <- coef(model)
  
  n_levels <- length(levels_main)
  has_interaction <- !is.null(interaction_prefix)
  
  mat_list <- list()
  
  for (i in seq_along(levels_main)) {
    lvl <- levels_main[i]
    is_ref <- (!is.null(ref) && lvl == ref) || (is.null(ref) && i == 1)
    
    row_base <- c("(Intercept)" = 1)
    if (!is_ref) {
      row_base[paste0(names(coefs)[grepl(lvl, names(coefs)) & grepl(":", names(coefs)) == FALSE])] <- 1
    }
    row_base["id1"] <- 0
    row_base[setdiff(names(coefs), names(row_base))] <- 0
    row_base <- row_base[names(coefs)]
    
    row_id <- row_base
    row_id["id1"] <- 1
    if (has_interaction && !is_ref) {
      int_term <- paste0(lvl, ":id1")
      alt_term <- paste0("id1:", lvl)
      if (int_term %in% names(coefs)) {
        row_id[int_term] <- 1
      } else if (alt_term %in% names(coefs)) {
        row_id[alt_term] <- 1
      }
    }
    
    mat_list[[paste0(lvl, "_nis")]] <- row_base
    mat_list[[paste0(lvl, "_oerwd")]] <- row_id
  }
  
  X <- do.call(rbind, mat_list)
  log_odds <- X %*% coefs
  se <- sqrt(diag(X %*% vcov(model) %*% t(X)))
  probs <- plogis(log_odds)
  lower <- plogis(log_odds - 1.96 * se)
  upper <- plogis(log_odds + 1.96 * se)
  
  data.frame(
    group = rownames(X),
    prevalence = probs * 10000,
    lower_95CI = lower * 10000,
    upper_95CI = upper * 10000
  )
}

# Define subgroup levels
sex_levels  <- levels(asp_final$FEMALE)
race_levels <- levels(asp_final$RACE)
age_levels  <- levels(asp_final$age_group)
cdiv_levels <- levels(asp_final$censusdivision)

# Apply to all models
asp_sex_unw  <- get_prev_ci(model_asp_female, sex_levels, "FEMALE")
asp_sex_w    <- get_prev_ci(model_asp_female_w, sex_levels, "FEMALE")
hist_sex_unw <- get_prev_ci(model_hist_female, sex_levels, "FEMALE")
hist_sex_w   <- get_prev_ci(model_hist_female_w, sex_levels, "FEMALE")

asp_race_unw  <- get_prev_ci(model_asp_race, race_levels, "RACE")
asp_race_w    <- get_prev_ci(model_asp_race_w, race_levels, "RACE")
hist_race_unw <- get_prev_ci(model_hist_race, race_levels, "RACE")
hist_race_w   <- get_prev_ci(model_hist_race_w, race_levels, "RACE")

asp_age_unw  <- get_prev_ci(model_asp_age, age_levels, "age_group")
asp_age_w    <- get_prev_ci(model_asp_age_w, age_levels, "age_group")
hist_age_unw <- get_prev_ci(model_hist_age, age_levels, "age_group")
hist_age_w   <- get_prev_ci(model_hist_age_w, age_levels, "age_group")

asp_cdiv_unw  <- get_prev_ci(model_asp_cdiv, cdiv_levels, "censusdivision")
asp_cdiv_w    <- get_prev_ci(model_asp_cdiv_w, cdiv_levels, "censusdivision")
hist_cdiv_unw <- get_prev_ci(model_hist_cdiv, cdiv_levels, "censusdivision")
hist_cdiv_w   <- get_prev_ci(model_hist_cdiv_w, cdiv_levels, "censusdivision")

########################## Format & Combine Prevalence CIs ##########################

bind_results <- function(df, disease, variable, weighted) {
  round_half_up <- function(x, digits = 0) {
    posneg <- sign(x)
    z <- abs(x) * 10^digits + 0.5
    z <- trunc(z)
    z <- z / 10^digits
    z * posneg
  }
  
  df %>%
    mutate(
      level = str_remove(group, "_(nis|oerwd)$"),
      id_status = ifelse(str_detect(group, "_oerwd$"), "id1 == 1", "id1 == 0"),
      stratum = variable,
      variable = case_when(
        variable == "total" ~ "all",
        TRUE ~ paste0(stratum, " == ", level)
      ),
      disease = disease,
      weighted = weighted
    ) %>%
    select(disease, weighted, stratum, variable, id_status, prevalence, lower_95CI, upper_95CI) %>%
    mutate(across(c(prevalence, lower_95CI, upper_95CI), ~ round_half_up(.x, 1)))
}


# Combine all results
all_prev_ci <- bind_rows(
  bind_results(asp_total_unw,  "aspergillosis", "total", "unweighted"),
  bind_results(asp_total_w,    "aspergillosis", "total", "weighted"),
  bind_results(hist_total_unw, "histoplasmosis", "total", "unweighted"),
  bind_results(hist_total_w,   "histoplasmosis", "total", "weighted"),
  
  bind_results(asp_sex_unw,  "aspergillosis", "FEMALE", "unweighted"),
  bind_results(asp_sex_w,    "aspergillosis", "FEMALE", "weighted"),
  bind_results(hist_sex_unw, "histoplasmosis", "FEMALE", "unweighted"),
  bind_results(hist_sex_w,   "histoplasmosis", "FEMALE", "weighted"),
  
  bind_results(asp_race_unw,  "aspergillosis", "RACE", "unweighted"),
  bind_results(asp_race_w,    "aspergillosis", "RACE", "weighted"),
  bind_results(hist_race_unw, "histoplasmosis", "RACE", "unweighted"),
  bind_results(hist_race_w,   "histoplasmosis", "RACE", "weighted"),
  
  bind_results(asp_age_unw,  "aspergillosis", "age_group", "unweighted"),
  bind_results(asp_age_w,    "aspergillosis", "age_group", "weighted"),
  bind_results(hist_age_unw, "histoplasmosis", "age_group", "unweighted"),
  bind_results(hist_age_w,   "histoplasmosis", "age_group", "weighted"),
  
  bind_results(asp_cdiv_unw,  "aspergillosis", "censusdivision", "unweighted"),
  bind_results(asp_cdiv_w,    "aspergillosis", "censusdivision", "weighted"),
  bind_results(hist_cdiv_unw, "histoplasmosis", "censusdivision", "unweighted"),
  bind_results(hist_cdiv_w,   "histoplasmosis", "censusdivision", "weighted")
) %>%
  mutate(index = row_number()) %>%
  select(index, everything())

print(all_prev_ci)

########################## Contrast Tests (Linear Hypotheses) ##########################

# General helper for contrast generation
generate_contrasts <- function(model, prefix, levels_vec, model_name_prefix) {
  map_dfr(levels_vec[-1], function(level) {
    hypo <- paste0("id1 + ", prefix, level, ":id1 = 0")
    linearHypothesis(model, hypo) %>%
      tidy() %>%
      mutate(model = model_name_prefix, term = hypo)
  })
}

# Generate contrasts for Aspergillosis
asp_cdiv_contrast    <- generate_contrasts(model_asp_cdiv, "censusdivision", cdiv_levels, "asp_censusdivision_contrast")
asp_cdiv_contrast_w  <- generate_contrasts(model_asp_cdiv_w, "censusdivision", cdiv_levels, "asp_censusdivision_contrast_w")
asp_female_contrast  <- linearHypothesis(model_asp_female, "id1 + FEMALE1:id1 = 0") %>% tidy() %>% mutate(model = "asp_female_contrast", term = "id1 + FEMALE1:id1")
asp_female_contrast_w<- linearHypothesis(model_asp_female_w, "id1 + FEMALE1:id1 = 0") %>% tidy() %>% mutate(model = "asp_female_contrast_w", term = "id1 + FEMALE1:id1")
asp_race_contrast    <- generate_contrasts(model_asp_race, "RACE", race_levels, "asp_race_contrast")
asp_race_contrast_w  <- generate_contrasts(model_asp_race_w, "RACE", race_levels, "asp_race_contrast_w")
asp_age_contrast     <- generate_contrasts(model_asp_age, "age_group", age_levels, "asp_age_group_contrast")
asp_age_contrast_w   <- generate_contrasts(model_asp_age_w, "age_group", age_levels, "asp_age_group_contrast_w")

# Generate contrasts for Histoplasmosis
hist_cdiv_contrast    <- generate_contrasts(model_hist_cdiv, "censusdivision", cdiv_levels, "hist_censusdivision_contrast")
hist_cdiv_contrast_w  <- generate_contrasts(model_hist_cdiv_w, "censusdivision", cdiv_levels, "hist_censusdivision_contrast_w")
hist_female_contrast  <- linearHypothesis(model_hist_female, "id1 + FEMALE1:id1 = 0") %>% tidy() %>% mutate(model = "hist_female_contrast", term = "id1 + FEMALE1:id1")
hist_female_contrast_w<- linearHypothesis(model_hist_female_w, "id1 + FEMALE1:id1 = 0") %>% tidy() %>% mutate(model = "hist_female_contrast_w", term = "id1 + FEMALE1:id1")
hist_race_contrast    <- generate_contrasts(model_hist_race, "RACE", race_levels, "hist_race_contrast")
hist_race_contrast_w  <- generate_contrasts(model_hist_race_w, "RACE", race_levels, "hist_race_contrast_w")
hist_age_contrast     <- generate_contrasts(model_hist_age, "age_group", age_levels, "hist_age_group_contrast")
hist_age_contrast_w   <- generate_contrasts(model_hist_age_w, "age_group", age_levels, "hist_age_group_contrast_w")

extract_id1 <- function(model, model_name) {
  tidy(model) %>%
    filter(term == "id1") %>%
    mutate(model = model_name) %>%
    select(model, term, estimate, p.value)
}

extract_contrasts <- function(model, var, levels_vec, model_name) {
  map_dfr(levels_vec[-1], function(level) {
    hyp <- paste0("id1 + ", var, level, ":id1 = 0")
    linearHypothesis(model, hyp) %>%
      tidy() %>%
      mutate(model = model_name, term = hyp) %>%
      select(model, term, estimate, p.value)
  })
}

get_disease_id1_contrasts <- function(prefix, data, models) {
  bind_rows(
    extract_id1(models$total,     paste0(prefix, "_total")),
    extract_id1(models$total_w,   paste0(prefix, "_total_w")),
    extract_id1(models$female,    paste0(prefix, "_female")),
    extract_id1(models$female_w,  paste0(prefix, "_female_w")),
    extract_id1(models$cdiv,      paste0(prefix, "_censusdivision")),
    extract_id1(models$cdiv_w,    paste0(prefix, "_censusdivision_w")),
    extract_id1(models$race,      paste0(prefix, "_race")),
    extract_id1(models$race_w,    paste0(prefix, "_race_w")),
    extract_id1(models$age,       paste0(prefix, "_age_group")),
    extract_id1(models$age_w,     paste0(prefix, "_age_group_w")),
    
    extract_contrasts(models$female,   "FEMALE",         levels(data$FEMALE),        paste0(prefix, "_female")),
    extract_contrasts(models$female_w, "FEMALE",         levels(data$FEMALE),        paste0(prefix, "_female_w")),
    extract_contrasts(models$cdiv,     "censusdivision", levels(data$censusdivision),paste0(prefix, "_censusdivision")),
    extract_contrasts(models$cdiv_w,   "censusdivision", levels(data$censusdivision),paste0(prefix, "_censusdivision_w")),
    extract_contrasts(models$race,     "RACE",           levels(data$RACE),          paste0(prefix, "_race")),
    extract_contrasts(models$race_w,   "RACE",           levels(data$RACE),          paste0(prefix, "_race_w")),
    extract_contrasts(models$age,      "age_group",      levels(data$age_group),     paste0(prefix, "_age_group")),
    extract_contrasts(models$age_w,    "age_group",      levels(data$age_group),     paste0(prefix, "_age_group_w"))
  )
}

asp_models <- list(
  total     = model_asp_total,
  total_w   = model_asp_total_w,
  female    = model_asp_female,
  female_w  = model_asp_female_w,
  cdiv      = model_asp_cdiv,
  cdiv_w    = model_asp_cdiv_w,
  race      = model_asp_race,
  race_w    = model_asp_race_w,
  age       = model_asp_age,
  age_w     = model_asp_age_w
)

hist_models <- list(
  total     = model_hist_total,
  total_w   = model_hist_total_w,
  female    = model_hist_female,
  female_w  = model_hist_female_w,
  cdiv      = model_hist_cdiv,
  cdiv_w    = model_hist_cdiv_w,
  race      = model_hist_race,
  race_w    = model_hist_race_w,
  age       = model_hist_age,
  age_w     = model_hist_age_w
)

model_results_with_contrasts <- bind_rows(
  get_disease_id1_contrasts("asp", asp_final, asp_models),
  get_disease_id1_contrasts("hist", hist_final, hist_models)
) %>%
  arrange(model)

model_results_with_contrasts %>% mutate(significant = model_results_with_contrasts$p.value < 0.05)
print(model_results_with_contrasts)

########################## Combine Case Tables ##########################

# Create overall data table - histoplasmosis
combined_total_hist <- cbind(as.data.table(nis_total_hist_result), as.data.table(oerwd_total_hist_result), as.data.table(oerwd_total_hist_w_result))
names(combined_total_hist) <- c("nis_cases", "nis_se", "oerwd_cases", "oerwd_se", "oerwd_w_cases", "oerwd_w_se")
print(combined_total_hist)

# Create overall data table - aspergillosis
combined_total_asp <- cbind(as.data.table(nis_total_asp_result), as.data.table(oerwd_total_asp_result), as.data.table(oerwd_total_asp_w_result))
names(combined_total_asp) <- c("nis_cases", "nis_se", "oerwd_cases", "oerwd_se", "oerwd_w_cases", "oerwd_w_se")
print(combined_total_asp)

# Create gender data table - histoplasmosis
combined_gender_hist <- cbind(as.data.table(nis_female_hist), as.data.table(oerwd_female_hist), as.data.table(oerwd_female_hist_w))[ ,-c(4,7) ]
names(combined_gender_hist) <- c("FEMALE", "nis_cases", "nis_se", "oerwd_cases", "oerwd_se", "oerwd_w_cases", "oerwd_w_se")
print(combined_gender_hist)

# Create gender data table - aspergillosis
combined_gender_asp <- cbind(as.data.table(nis_female_asp), as.data.table(oerwd_female_asp), as.data.table(oerwd_female_asp_w))[ ,-c(4,7) ]
names(combined_gender_asp) <- c("FEMALE", "nis_cases", "nis_se", "oerwd_cases", "oerwd_se", "oerwd_w_cases", "oerwd_w_se")
print(combined_gender_asp)

# Create age data table - histoplasmosis
combined_age_hist <- cbind(as.data.table(nis_age_hist), as.data.table(oerwd_age_hist), as.data.table(oerwd_age_hist_w))[ ,-c(4,7) ]
names(combined_age_hist) <- c("age_group", "nis_cases", "nis_se", "oerwd_cases", "oerwd_se", "oerwd_w_cases", "oerwd_w_se")
print(combined_age_hist)

# Create gender data table - aspergillosis
combined_age_asp <- cbind(as.data.table(nis_age_asp), as.data.table(oerwd_age_asp), as.data.table(oerwd_age_asp_w))[ ,-c(4,7) ]
names(combined_age_asp) <- c("age_group", "nis_cases", "nis_se", "oerwd_cases", "oerwd_se", "oerwd_w_cases", "oerwd_w_se")
print(combined_age_asp)

# Create race data table - histoplasmosis
combined_race_hist <- cbind(as.data.table(nis_race_hist), as.data.table(oerwd_race_hist), as.data.table(oerwd_race_hist_w))[ ,-c(4,7) ]
names(combined_race_hist) <- c("RACE", "nis_cases", "nis_se", "oerwd_cases", "oerwd_se", "oerwd_w_cases", "oerwd_w_se")
print(combined_race_hist)

# Create race data table - aspergillosis
combined_race_asp <- cbind(as.data.table(nis_race_asp), as.data.table(oerwd_race_asp), as.data.table(oerwd_race_asp_w))[ ,-c(4,7) ]
names(combined_race_asp) <- c("RACE", "nis_cases", "nis_se", "oerwd_cases", "oerwd_se", "oerwd_w_cases", "oerwd_w_se")
print(combined_race_asp)

# Create cdiv data table - histoplasmosis
combined_cdiv_hist <- cbind(as.data.table(nis_cdiv_hist), as.data.table(oerwd_cdiv_hist), as.data.table(oerwd_cdiv_hist_w))[ ,-c(4,7) ]
names(combined_cdiv_hist) <- c("censusdivision", "nis_cases", "nis_se", "oerwd_cases", "oerwd_se", "oerwd_w_cases", "oerwd_w_se")
print(combined_cdiv_hist)

# Create cdiv data table - aspergillosis
combined_cdiv_asp <- cbind(as.data.table(nis_div_asp), as.data.table(oerwd_cdiv_asp), as.data.table(oerwd_cdiv_asp_w))[ ,-c(4,7) ]
names(combined_cdiv_asp) <- c("censusdivision", "nis_cases", "nis_se", "oerwd_cases", "oerwd_se", "oerwd_w_cases", "oerwd_w_se")
print(combined_cdiv_asp)

########################## Encounter Relative Difference Calculation ##########################

table2_func <- function(df) {
  df %>%
    mutate(
      # Relative difference (unweighted and weighted)
      reldiff_uw = (nis_cases - oerwd_cases) / nis_cases,
      reldiff_w  = (nis_cases - oerwd_w_cases) / nis_cases,
      
      # Standard error propagation (delta method)
      reldiff_uw_se = sqrt(((oerwd_cases / nis_cases^2)^2) * nis_se^2 +
                             ((1 / nis_cases)^2) * oerwd_se^2),
      reldiff_w_se  = sqrt(((oerwd_w_cases / nis_cases^2)^2) * nis_se^2 +
                             ((1 / nis_cases)^2) * oerwd_w_se^2),
      
      # Confidence intervals
      reldiff_uw_lowerci = reldiff_uw - 1.96 * reldiff_uw_se,
      reldiff_uw_upperci = reldiff_uw + 1.96 * reldiff_uw_se,
      reldiff_w_lowerci  = reldiff_w  - 1.96 * reldiff_w_se,
      reldiff_w_upperci  = reldiff_w  + 1.96 * reldiff_w_se,
      
      # Z-tests and p-values
      zvalue_uw = reldiff_uw / reldiff_uw_se,
      zvalue_w  = reldiff_w  / reldiff_w_se,
      p_value_uw = 2 * (1 - pnorm(abs(zvalue_uw))),
      p_value_w  = 2 * (1 - pnorm(abs(zvalue_w)))
    ) %>%
    mutate(
      # Convert to percentage and round
      across(c(reldiff_uw, reldiff_uw_lowerci, reldiff_uw_upperci,
               reldiff_w,  reldiff_w_lowerci,  reldiff_w_upperci),
             ~ round(.x * 100, 1))
    )
}

# Overall
combined_total_asp_results  <- table2_func(combined_total_asp)
combined_total_hist_results <- table2_func(combined_total_hist)

print(combined_total_asp_results)
print(combined_total_hist_results)

# Census Division
combined_cdiv_asp_results  <- table2_func(combined_cdiv_asp)
combined_cdiv_hist_results <- table2_func(combined_cdiv_hist)

print(combined_cdiv_asp_results)
print(combined_cdiv_hist_results)

# Gender
combined_gender_asp_results  <- table2_func(combined_gender_asp)
combined_gender_hist_results <- table2_func(combined_gender_hist)

print(combined_gender_asp_results)
print(combined_gender_hist_results)

# Race and Ethnicity
combined_race_asp_results  <- table2_func(combined_race_asp)
combined_race_hist_results <- table2_func(combined_race_hist)

print(combined_race_asp_results)
print(combined_race_hist_results)

# Age Group
combined_age_asp_results  <- table2_func(combined_age_asp)
combined_age_hist_results <- table2_func(combined_age_hist)

print(combined_age_asp_results)
print(combined_age_hist_results)

