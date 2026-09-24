# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: September, 2026 --------------------------------------------------------
# Script title: 03_final_estimation.R ------------------------------------------
#
# Notes: H1-H4 from population electoral quantities and frozen observed text measures.
# Outcomes and binary regressors stay 0/1; multiply estimates by 100 ONCE when
# reporting percentage points. All inferential models cluster on municipality.
# ------------------------------------------------------------------------------

# Required packages ------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(fixest)
  library(broom)
})

# ------------------------------------------------------------------------------
# Parameters -------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Output directory 
OUT_DIR <- Sys.getenv("MAYORAL_OUTPUT_DIR", "output")

# Input directory: analysis dataframe
FRAME_PATH <- Sys.getenv("MAYORAL_FRAME_PATH", 
                         "C:/Users/cedric.antunes/Downloads/votes_municipality/analysis_frame.rds")

DRIVE_OUTPUT_FOLDER_ID <- Sys.getenv("MAYORAL_DRIVE_OUTPUT_FOLDER_ID",
                                     "1IcDW6_Q9vezxq4zR06hJEGdAXYhb8jVd")

# Main estimates: confident classifications only
USE_CONFIDENT_MAIN <- TRUE

# Smallest Effect Size of Interest
SESOI_PP <- suppressWarnings(as.numeric(Sys.getenv("MAYORAL_SESOI_PP", "NA")))

if (!is.na(SESOI_PP) && (!is.finite(SESOI_PP) || SESOI_PP <= 0)) stop("Invalid SESOI")

# Vote margins
MARGINS_PP <- c(5, 10, 20)

dir.create(OUT_DIR, 
           recursive = TRUE, 
           showWarnings = FALSE)

output_names <- character()

wr <- function(x, n) {
  filename <- paste0(n, ".csv")
  write_csv(x, file.path(OUT_DIR, filename))
  output_names <<- unique(c(output_names, filename))
}

# My personal Google Drive folder ----------------------------------------------
googledrive::drive_auth(email = "cedricantunes07@gmail.com")

drive_folder <- googledrive::drive_get(googledrive::as_id(DRIVE_OUTPUT_FOLDER_ID))

stopifnot(nrow(drive_folder) == 1L, googledrive::is_folder(drive_folder))

if (!nzchar(FRAME_PATH)) {
  drive_files <- googledrive::drive_ls(drive_folder)
  frame_hit <- drive_files[drive_files$name == "analysis_frame.rds", ]
  if (nrow(frame_hit) != 1L)
    stop("Expected exactly one analysis_frame.rds in the Drive output folder")
  FRAME_PATH <- tempfile(fileext = "_analysis_frame.rds")
  googledrive::drive_download(frame_hit, path = FRAME_PATH, overwrite = TRUE)
}

if (!file.exists(FRAME_PATH)) stop("Analysis frame not found: ", FRAME_PATH)

# Reading the data -------------------------------------------------------------
d <- readRDS(FRAME_PATH)

# Outcomes ---------------------------------------------------------------------
BASE <- c(ANY = "TRANSPARENCY_ANY_MENTIONED", 
          PAST = "PAST_CLAIM_BINARY",
          GEN = "GENERAL_PROMISE_BINARY", 
          SPEC = "SPECIFIC_PROMISE_BINARY",
          STYLE = "RHETORIC_BINARY")

TVAR <- if (USE_CONFIDENT_MAIN) setNames(paste0(BASE, "_CONFIDENT"), names(BASE)) else BASE

# Nice labels for plotting
LABELS <- c(ANY = "Any transparency mention", 
            PAST = "Retrospective claim",
            GEN = "General prospective promise", 
            SPEC = "Specific prospective promise",
            STYLE = "Transparency as governing style")

stopifnot(all(TVAR %in% names(d)), !anyDuplicated(d$CAND_ID),
          all(unlist(d[unname(TVAR)]) %in% c(0, 1, NA)),
          all(is.na(unlist(d[d$TEXT_OBSERVED == 0, unname(TVAR)]))),
          nrow(d) == 50040L, n_distinct(d$RACE_ID) == 16704L,
          sum(d$PLAN_OBSERVED) == 39124L, sum(d$TEXT_OBSERVED) == 36995L,
          sum(d$SAMPLE_H2) == 14791L,
          n_distinct(d$RACE_ID[d$SAMPLE_H2 == 1]) == 5238L,
          sum(d$SAMPLE_H4_CONFIDENT) == 3657L,
          n_distinct(d$RACE_ID[d$SAMPLE_H4_CONFIDENT == 1]) == 1401L,
          sum(d$SAMPLE_H3_TOPTWO) == 19544L,
          n_distinct(d$RACE_ID[d$SAMPLE_H3_TOPTWO == 1]) == 9772L)

# Required variables
needed <- c("ELECTION_SCOPE", 
            "INCUMBENCY_STATUS", 
            "PRIOR_WINNER_UNAMBIGUOUS",
            "PRIOR_SUPPLEMENTARY_ELECTION", 
            "N_UNKNOWN_INCUMBENTS",
            "SAMPLE_H4_CONFIDENT", 
            "SAMPLE_H4_ALL", 
            "W_OBS")

if (!all(needed %in% names(d))) stop("Use analysis_frame.rds from revised script 02")

stopifnot(all(d$ELECTION_SCOPE == "Ordinary"),
          all(d$N_UNKNOWN_INCUMBENTS[d$SAMPLE_H2 == 1] == 0L))

# Summaries use the actual estimation rows, including any fixed-effect removals.
model_sample <- function(m, dat) {
  used <- dat[fixest::obs(m), , drop = FALSE]
  tibble(n = nobs(m), n_races = n_distinct(used$RACE_ID), n_municipalities = n_distinct(used$MUNI))
}

# Point estimates, cluster-t intervals and p values for a coefficient contrast.
contrast_pp <- function(m, dat, weights) {
  b <- coef(m)
  if (length(setdiff(names(weights), names(b)))) stop("Required model term was dropped")
  a <- setNames(rep(0, length(b)), names(b)); a[names(weights)] <- weights
  est <- sum(a * b); se <- sqrt(drop(t(a) %*% vcov(m) %*% a))
  df <- fixest::degrees_freedom(m, type = "t")
  tibble(estimate = 100 * est, std.error = 100 * se,
         conf.low = 100 * (est - qt(.975, df) * se),
         conf.high = 100 * (est + qt(.975, df) * se),
         p.value = 2 * pt(-abs(est / se), df), df = df) |>
    bind_cols(model_sample(m, dat))
}

coefficient_pp <- function(m, dat, term) contrast_pp(m, dat, setNames(1, term))

# Compare corrections in both directions. A previous winner is an incumbency
# proxy; discrepancies are not all necessarily errors in the older measure.
wr(d |> filter(PLAN_OBSERVED == 1) |> count(INCUMBENT_CORPUS, INCUMBENT_TRUE),
   "t_incumbency_correction")

wr(d |> filter(PLAN_OBSERVED == 1) |> count(ELECTED_CORPUS, ELECTED), "t_elected_correction")

# H2 / H4: retain races with BOTH candidate types in each estimation sample.
# This explicitly enforces the H4 estimand after restricting to users.
fit_status <- function(dat, y, users = FALSE, weight = NULL, any_var = TVAR[["ANY"]]) {
  z <- dat |> filter(SAMPLE_H2 == 1, !is.na(CHALLENGER_TRUE), !is.na(.data[[y]]), is.finite(LOG_N_WORDS))
  if (users) z <- z |> filter(.data[[any_var]] == 1)
  if (!is.null(weight)) z <- z |> filter(is.finite(.data[[weight]]), .data[[weight]] > 0)
  z <- z |> group_by(RACE_ID) |> filter(n_distinct(CHALLENGER_TRUE) == 2) |> ungroup()
  if (!nrow(z)) stop("Empty incumbent/challenger estimation sample for ", y)
  f <- as.formula(paste(y, "~ CHALLENGER_TRUE + LOG_N_WORDS | RACE_ID"))
  m <- if (is.null(weight)) feols(f, data = z, vcov = ~ MUNI) else
    feols(f, data = z, weights = z[[weight]], vcov = ~ MUNI)
  list(model = m, data = z)
}

status_table <- function(dat, vars = TVAR, users = FALSE, weight = NULL,
                         any_var = TVAR[["ANY"]]) {
  imap_dfr(vars, function(v, k) {
    z <- fit_status(dat, v, users, weight, any_var)
    coefficient_pp(z$model, z$data, "CHALLENGER_TRUE") |>
      mutate(key = k, outcome = LABELS[[k]])
  }) |> mutate(p_holm = p.adjust(p.value, "holm"))
}

h2 <- status_table(d)

wr(h2, "t_h2_main")

# Independent within-race OLS point checks on the revised frame. Clustered
# uncertainty is produced in R and is not part of these point-estimate checks.
benchmark <- c(ANY = 6.6962737784, 
               PAST = -0.8680393808,
               GEN = 5.7454066310, 
               SPEC = 5.1759728971,
               STYLE = 3.1054033905)

check_h2 <- h2 |> mutate(independent_point_check = benchmark[key],
                         difference_pp = estimate - independent_point_check)

wr(check_h2, "t_h2_independent_point_check")

if (USE_CONFIDENT_MAIN && any(abs(check_h2$difference_pp) > .01))
  stop("H2 point estimates differ from the independently reconstructed revised frame")

h4 <- status_table(d, TVAR[names(TVAR) != "ANY"], users = TRUE)

wr(h4, "t_h4_users")

h4_all <- status_table(d, BASE[names(BASE) != "ANY"], users = TRUE,
                       any_var = BASE[["ANY"]])

wr(h4_all, "t_h4_users_all_hits")

stopifnot(all(h2$n == 14791L), all(h2$n_races == 5238L),
          all(h4$n == 3657L), all(h4$n_races == 1401L),
          all(h4_all$n == 6048L), all(h4_all$n_races == 2232L))

# Strategy-specific length slopes preserve the omnibus model in the Rmd.
for (users in c(FALSE, TRUE)) {
  z <- d |> filter(SAMPLE_H2 == 1, is.finite(LOG_N_WORDS))
  if (users) z <- z |> filter(.data[[TVAR[["ANY"]]]] == 1)
  strategy_vars <- unname(TVAR[names(TVAR) != "ANY"])
  z <- z |> filter(if_all(all_of(strategy_vars), ~ !is.na(.x))) |>
    group_by(RACE_ID) |> filter(n_distinct(CHALLENGER_TRUE) == 2) |> ungroup() |>
    select(RACE_ID, MUNI, CHALLENGER_TRUE, LOG_N_WORDS, all_of(strategy_vars)) |>
    pivot_longer(all_of(strategy_vars), names_to = "strategy", values_to = "y") |>
    mutate(strategy = factor(strategy), RACE_STRATEGY = paste(RACE_ID, strategy, sep = "|"))
  m <- feols(y ~ CHALLENGER_TRUE * strategy + LOG_N_WORDS * strategy | RACE_STRATEGY,
             data = z, vcov = ~ MUNI)
  test <- wald(m, keep = "CHALLENGER_TRUE:strategy", print = FALSE)
  capture.output(print(test), file = file.path(OUT_DIR,
                                               if (users) "t_h4_omnibus.txt" else "t_h2_omnibus.txt"))
}

output_names <- unique(c(output_names, "t_h2_omnibus.txt", 
                         "t_h4_omnibus.txt"))

# H2 robustness. Holm adjustment is separate within each five-outcome family.
robust <- bind_rows(
  h2 |> mutate(spec = "Main"),
  status_table(d, weight = "W_OBS") |> mutate(spec = "Candidate-observability IPW"),
  status_table(d |> filter(SAMPLE_FULLY_OBSERVED == 1)) |> mutate(spec = "Fully observed races"),
  status_table(d, vars = BASE) |> mutate(spec = "All hits"),
  status_table(d |> filter(PRIOR_WINNER_UNAMBIGUOUS == 1)) |>
    mutate(spec = "Unambiguous previous-cycle winner"),
  status_table(d |> filter(PRIOR_SUPPLEMENTARY_ELECTION == 0)) |>
    mutate(spec = "No prior supplementary-election record"),
  map_dfr(sort(unique(d$YEAR)), function(y)
    status_table(d |> filter(YEAR == y)) |> mutate(spec = paste("Year", y)))
)

wr(robust, "t_h2_robustness")

# H1: binary transparency regressor, binary election outcome. Scale coefficient
# and uncertainty by 100 only at reporting, giving the 0-to-1 contrast in pp.
h1 <- imap_dfr(TVAR, function(v, k) {
  z <- d |> filter(SAMPLE_H1 == 1, !is.na(.data[[v]]), is.finite(LOG_N_WORDS)) |>
    mutate(TALK = .data[[v]])
  map_dfr(c("Race FE", "Race + party FE"), function(spec) {
    f <- if (spec == "Race FE") ELECTED ~ TALK + LOG_N_WORDS | RACE_ID else
      ELECTED ~ TALK + LOG_N_WORDS | RACE_ID + PARTY_F
    m <- feols(f, data = z, vcov = ~ MUNI)
    coefficient_pp(m, z, "TALK") |>
      mutate(key = k, outcome = LABELS[[k]], spec = spec,
             ci90_low = estimate - qt(.95, df) * std.error,
             ci90_high = estimate + qt(.95, df) * std.error,
             sesoi_pp = SESOI_PP,
             tost_p = if (is.na(SESOI_PP)) NA_real_ else
               pmax(pt((estimate - SESOI_PP) / std.error, df),
                    pt(-(estimate + SESOI_PP) / std.error, df)),
             equivalent = if (is.na(SESOI_PP)) NA else ci90_low > -SESOI_PP & ci90_high < SESOI_PP)
  })
}) |> group_by(spec) |> mutate(p_holm = p.adjust(p.value, "holm")) |> ungroup()

wr(h1, "t_h1_electoral_association")

# H3: preserve the revised Rmd estimands, with population ranks and margins.
# Absolute behavior: municipality + year FE, separate leader/runner-up slopes.
# Relative behavior: race FE and runner-up x margin, evaluated at 5/10/20 pp.
h3base <- d |> filter(TEXT_OBSERVED == 1, VALID_RANKING_TRUE == 1,
                      is.finite(TRUE_MARGIN_PP), is.finite(LOG_N_WORDS)) |>
  mutate(RUNNER_MARGIN = TRUE_RUNNER_UP * TRUE_MARGIN_10PP)
paired <- function(z, v) z |> filter(!is.na(.data[[v]])) |>
  group_by(RACE_ID) |>
  filter(sum(TRUE_RUNNER_UP == 1) == 1, sum(TRUE_RUNNER_UP == 0) >= 1) |> ungroup()
absolute <- list(); predictions <- list(); relative <- list(); interactions <- list()
for (k in names(TVAR)) {
  v <- TVAR[[k]]
  z <- paired(h3base |> filter(TRUE_RANK <= 2), v)
  stopifnot(all(table(z$RACE_ID) == 2))
  m <- feols(as.formula(paste(v,
                              "~ TRUE_RUNNER_UP + TRUE_MARGIN_10PP + RUNNER_MARGIN + LOG_N_WORDS | MUNI + YEAR")),
             data = z, vcov = ~ MUNI)
  absolute[[k]] <- bind_rows(
    contrast_pp(m, z, c(TRUE_MARGIN_10PP = 1)) |> mutate(role = "Leader"),
    contrast_pp(m, z, c(TRUE_MARGIN_10PP = 1, RUNNER_MARGIN = 1)) |> mutate(role = "Runner-up")
  ) |> mutate(key = k, outcome = LABELS[[k]])
  # Standardized levels are point estimates only: slope vcov alone does not
  # capture the uncertainty in the absorbed fixed effects used in predictions.
  used <- z[fixest::obs(m), , drop = FALSE]
  predictions[[k]] <- crossing(role_code = c(0L, 1L), margin_pp = MARGINS_PP) |>
    pmap_dfr(function(role_code, margin_pp) {
      nd <- used |> mutate(TRUE_RUNNER_UP = role_code,
                           TRUE_MARGIN_10PP = margin_pp / 10,
                           RUNNER_MARGIN = role_code * margin_pp / 10)
      tibble(key = k, outcome = LABELS[[k]], role = if (role_code == 0) "Leader" else "Runner-up",
             margin_pp = margin_pp, predicted_probability_pp = 100 * mean(predict(m, newdata = nd)))
    })
  specs <- list(
    "Runner-up minus leader" = h3base |> filter(TRUE_RANK <= 2),
    "Runner-up minus lower-ranked candidates" = h3base |> filter(TRUE_RANK >= 2),
    "Runner-up challenger minus lower-ranked challengers" = h3base |>
      filter(SAMPLE_H2 == 1, CHALLENGER_TRUE == 1, TRUE_RANK >= 2))
  relative_k <- list()
  for (label in names(specs)) {
    zz <- paired(specs[[label]], v)
    if (!nrow(zz)) stop("Empty H3 comparison: ", label)
    mm <- feols(as.formula(paste(v, "~ TRUE_RUNNER_UP + RUNNER_MARGIN + LOG_N_WORDS | RACE_ID")),
                data = zz, vcov = ~ MUNI)
    interactions[[paste(k, label)]] <- coefficient_pp(mm, zz, "RUNNER_MARGIN") |>
      mutate(key = k, outcome = LABELS[[k]], comparison = label)
    relative_k[[label]] <- map_dfr(MARGINS_PP, function(margin)
      contrast_pp(mm, zz, c(TRUE_RUNNER_UP = 1, RUNNER_MARGIN = margin / 10)) |>
        mutate(key = k, outcome = LABELS[[k]], comparison = label, margin_pp = margin))
  }
  relative[[k]] <- bind_rows(relative_k)
}

absolute <- bind_rows(absolute) |> group_by(role) |>
  mutate(p_holm = p.adjust(p.value, "holm")) |> ungroup()

relative <- bind_rows(relative) |> group_by(comparison, margin_pp) |>
  mutate(p_holm = p.adjust(p.value, "holm")) |> ungroup()

interactions <- bind_rows(interactions) |> group_by(comparison) |>
  mutate(p_holm = p.adjust(p.value, "holm")) |> ungroup()

wr(absolute, "t_h3_absolute_margin_slopes")

wr(bind_rows(predictions), "t_h3_standardized_predictions")

wr(relative, "t_h3_relative_contrasts_5_10_20pp")

wr(interactions, "t_h3_relative_margin_interactions")

# Descriptive worst-case prevalence bounds, not bounds on a causal/FE effect.
manski <- function(z, v) {
  observed <- !is.na(z[[v]])
  tibble(population_n = nrow(z), measured_n = sum(observed),
         coverage = mean(observed), observed_prevalence_pp = 100 * mean(z[[v]], na.rm = TRUE),
         lower_pp = 100 * sum(z[[v]], na.rm = TRUE) / nrow(z),
         upper_pp = 100 * (sum(z[[v]], na.rm = TRUE) + sum(!observed)) / nrow(z))
}

bounds <- imap_dfr(TVAR, function(v, k) bind_rows(
  manski(d, v) |> mutate(sample = "Full population"),
  manski(d |> filter(FRAC_OBSERVED_RACE >= .9), v) |> mutate(sample = "Races at least 90% observed")
) |> mutate(key = k, outcome = LABELS[[k]]))

wr(bounds, "t_manski_prevalence_bounds")

# Lee-style trimming sensitivity for the UNADJUSTED difference in group means.
# Incumbency is observational. This is not a causal Lee bound or a bound on H2.
# Trim an exact fraction of probability mass, including fractional tied values.
# No quantile cutoff is used: that approach fails with binary outcomes.
tail_mean <- function(x, keep_fraction, upper = FALSE) {
  x <- sort(x[is.finite(x)], decreasing = upper)
  stopifnot(length(x) > 0, keep_fraction > 0, keep_fraction <= 1)
  mass <- length(x) * keep_fraction
  weights <- pmin(1, pmax(0, mass - (seq_along(x) - 1)))
  sum(weights * x) / mass
}

trim_sensitivity <- imap_dfr(TVAR, function(v, k) {
  z <- d |> filter(INCUMBENT_CONTESTED_TRUE == 1, !is.na(INCUMBENT_TRUE))
  inc <- z |> filter(INCUMBENT_TRUE == 1); chal <- z |> filter(CHALLENGER_TRUE == 1)
  pi <- mean(!is.na(inc[[v]])); pc <- mean(!is.na(chal[[v]]))
  keep <- min(pi, pc) / max(pi, pc)
  if (pi <= 0 || pc <= 0) stop("No observations for trimming sensitivity")
  if (pc >= pi) {
    lo <- tail_mean(chal[[v]], keep) - mean(inc[[v]], na.rm = TRUE)
    hi <- tail_mean(chal[[v]], keep, TRUE) - mean(inc[[v]], na.rm = TRUE)
  } else {
    lo <- mean(chal[[v]], na.rm = TRUE) - tail_mean(inc[[v]], keep, TRUE)
    hi <- mean(chal[[v]], na.rm = TRUE) - tail_mean(inc[[v]], keep)
  }
  tibble(key = k, outcome = LABELS[[k]], coverage_incumbents = pi, coverage_challengers = pc,
         trimmed_fraction = 1 - keep, lower_pp = 100 * lo, upper_pp = 100 * hi,
         estimand = "Unadjusted challenger-minus-incumbent difference; descriptive sensitivity")
})

wr(trim_sensitivity, "t_trimming_sensitivity_unadjusted")

# Descriptives and figure
wr(imap_dfr(TVAR, function(v, k) tibble(key = k, outcome = LABELS[[k]],
                                        population_n = nrow(d), usable_text_n = sum(d$TEXT_OBSERVED),
                                        prevalence_pp = 100 * mean(d[[v]][d$TEXT_OBSERVED == 1], na.rm = TRUE))), "t_prevalence")

wr(tibble(population_candidates = nrow(d), population_races = n_distinct(d$RACE_ID),
          municipalities = n_distinct(d$MUNI), corpus_candidates = sum(d$PLAN_OBSERVED),
          usable_text_candidates = sum(d$TEXT_OBSERVED),
          fully_observed_races = d |> distinct(RACE_ID, SAMPLE_FULLY_OBSERVED) |>
            pull(SAMPLE_FULLY_OBSERVED) |> sum()), "t_table1_population")

# ------------------------------------------------------------------------------
# Plotting ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
p <- h2 |> mutate(outcome = factor(outcome, levels = rev(unname(LABELS)))) |>
  ggplot(aes(estimate, outcome)) + geom_vline(xintercept = 0, color = "grey60") +
  geom_segment(aes(x = conf.low, xend = conf.high, yend = outcome), linewidth = .5) +
  geom_point(size = 2.4) + theme_bw() +
  labs(x = "Challenger minus incumbent (percentage points)", y = NULL,
       title = "Within-race differences in transparency communication",
       subtitle = "Length-adjusted estimates; municipality-clustered 95% confidence intervals")

ggsave(file.path(OUT_DIR, "fig2_challenger_effects.png"), p, width = 9, height = 4, dpi = 300)

capture.output(sessionInfo(), file = file.path(OUT_DIR, "session_03.txt"))

output_names <- unique(c(output_names, "fig2_challenger_effects.png", "session_03.txt"))

# ------------------------------------------------------------------------------
# Saving outputs ---------------------------------------------------------------
# ------------------------------------------------------------------------------
existing <- googledrive::drive_ls(drive_folder)
duplicates <- existing |> filter(name %in% output_names) |> count(name) |> filter(n > 1L)
if (nrow(duplicates))
  stop("Duplicate output filenames in Drive: ", paste(duplicates$name, collapse = ", "))
missing_local <- output_names[!file.exists(file.path(OUT_DIR, output_names))]
if (length(missing_local))
  stop("Expected local outputs are missing: ", paste(missing_local, collapse = ", "))
tryCatch({
  for (name in output_names)
    googledrive::drive_put(file.path(OUT_DIR, name), path = drive_folder, name = name)
}, error = function(e) {
  stop("Drive upload did not finish. Local outputs remain in ",
       normalizePath(OUT_DIR, winslash = "/"),
       ". Some Drive files may already be updated; rerun after resolving: ",
       conditionMessage(e), call. = FALSE)
})
