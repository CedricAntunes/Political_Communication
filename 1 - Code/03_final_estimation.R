# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: September, 2026 --------------------------------------------------------
# Script title: 03_final_estimation.R ------------------------------------------
#
# Notes: H1-H4 from population electoral quantities and frozen observed text measures.
# Outcomes and binary regressors stay 0/1; multiply estimates by 100 ONCE when
# reporting percentage points. All inferential models cluster on municipality.
#
# Revision (September 2026):
#   1. H1 adds an incumbency control and keeps races whose winner has usable text.
#      The previous specification is still reported, labelled as such.
#   2. Equivalence tests need MAYORAL_SESOI_PP; a message flags when it is unset.
#   3. The empty "Unambiguous previous-cycle winner" row is replaced by a
#      flexible length control (fixed effects for LENGTH_BINS log-length bins).
#   4. New: joint test that the challenger gap is equal across election years.
#   5. New: planned contrasts (general vs specific; retrospective vs prospective)
#      from the stacked strategy model, for the H2 and H4 samples.
#   6. New: H3 sensitivity dropping races whose top two include a candidate
#      without an approved registration (votes later annulled).
#   7. The stored H2 point check is renamed a regression test.
#   8. Drive upload is optional (MAYORAL_UPLOAD) and the account is configurable.
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
if (is.na(SESOI_PP))
  message("MAYORAL_SESOI_PP is not set: H1 equivalence-test columns will be NA.")

# Flexible length control: number of log-length bins used as fixed effects
LENGTH_BINS <- 20L

# Registration statuses whose votes count as valid (H3 sensitivity)
APPROVED_STATUS <- c("DEFERIDO", "DEFERIDO COM RECURSO")

# Drive upload (set MAYORAL_UPLOAD=FALSE to keep outputs local)
UPLOAD <- as.logical(Sys.getenv("MAYORAL_UPLOAD", "TRUE"))
DRIVE_EMAIL <- Sys.getenv("MAYORAL_DRIVE_EMAIL", "cedricantunes07@gmail.com")

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
if (UPLOAD || !nzchar(FRAME_PATH)) {
  googledrive::drive_auth(email = DRIVE_EMAIL)
  drive_folder <- googledrive::drive_get(googledrive::as_id(DRIVE_OUTPUT_FOLDER_ID))
  stopifnot(nrow(drive_folder) == 1L, googledrive::is_folder(drive_folder))
}

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
            "W_OBS",
            "des_situacao_candidatura")

if (!all(needed %in% names(d))) stop("Use analysis_frame.rds from revised script 02")

stopifnot(all(d$ELECTION_SCOPE == "Ordinary"),
          all(d$N_UNKNOWN_INCUMBENTS[d$SAMPLE_H2 == 1] == 0L))

# Log-length bins over the text-observed candidates (flexible length control).
d <- d |> mutate(LENGTH_BIN = if_else(is.finite(LOG_N_WORDS),
                                      ntile(if_else(is.finite(LOG_N_WORDS), LOG_N_WORDS, NA_real_),
                                            LENGTH_BINS), NA_integer_))

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
fit_status <- function(dat, y, users = FALSE, weight = NULL, any_var = TVAR[["ANY"]],
                       length_bins = FALSE) {
  z <- dat |> filter(SAMPLE_H2 == 1, !is.na(CHALLENGER_TRUE), !is.na(.data[[y]]), is.finite(LOG_N_WORDS))
  if (users) z <- z |> filter(.data[[any_var]] == 1)
  if (!is.null(weight)) z <- z |> filter(is.finite(.data[[weight]]), .data[[weight]] > 0)
  z <- z |> group_by(RACE_ID) |> filter(n_distinct(CHALLENGER_TRUE) == 2) |> ungroup()
  if (!nrow(z)) stop("Empty incumbent/challenger estimation sample for ", y)
  f <- as.formula(if (length_bins) paste(y, "~ CHALLENGER_TRUE | RACE_ID + LENGTH_BIN") else
    paste(y, "~ CHALLENGER_TRUE + LOG_N_WORDS | RACE_ID"))
  m <- if (is.null(weight)) feols(f, data = z, vcov = ~ MUNI) else
    feols(f, data = z, weights = z[[weight]], vcov = ~ MUNI)
  list(model = m, data = z)
}

status_table <- function(dat, vars = TVAR, users = FALSE, weight = NULL,
                         any_var = TVAR[["ANY"]], length_bins = FALSE) {
  imap_dfr(vars, function(v, k) {
    z <- fit_status(dat, v, users, weight, any_var, length_bins)
    coefficient_pp(z$model, z$data, "CHALLENGER_TRUE") |>
      mutate(key = k, outcome = LABELS[[k]])
  }) |> mutate(p_holm = p.adjust(p.value, "holm"))
}

h2 <- status_table(d)

wr(h2, "t_h2_main")

# Regression test: H2 point estimates must match the stored values from the
# verified run. It guards against unintended changes; it is not an independent
# replication.
benchmark <- c(ANY = 6.6962737784, 
               PAST = -0.8680393808,
               GEN = 5.7454066310, 
               SPEC = 5.1759728971,
               STYLE = 3.1054033905)

check_h2 <- h2 |> mutate(stored_estimate = benchmark[key],
                         difference_pp = estimate - stored_estimate)

wr(check_h2, "t_h2_regression_test")

if (USE_CONFIDENT_MAIN && any(abs(check_h2$difference_pp) > .01))
  stop("H2 point estimates differ from the stored values of the verified run")

h4 <- status_table(d, TVAR[names(TVAR) != "ANY"], users = TRUE)

wr(h4, "t_h4_users")

h4_all <- status_table(d, BASE[names(BASE) != "ANY"], users = TRUE,
                       any_var = BASE[["ANY"]])

wr(h4_all, "t_h4_users_all_hits")

stopifnot(all(h2$n == 14791L), all(h2$n_races == 5238L),
          all(h4$n == 3657L), all(h4$n_races == 1401L),
          all(h4_all$n == 6048L), all(h4_all$n_races == 2232L))

# Strategy-specific length slopes preserve the omnibus model in the Rmd.
# The same stacked model yields the planned contrasts the theory needs. Each
# strategy's challenger effect is CHALLENGER_TRUE plus its interaction (zero
# for the reference level), so contrasts are differences of those sums.
challenger_effect <- function(strategy_var, levels) {
  w <- c(CHALLENGER_TRUE = 1)
  if (strategy_var != levels[1]) w[paste0("CHALLENGER_TRUE:strategy", strategy_var)] <- 1
  w
}
combine_weights <- function(...) {
  parts <- list(...)
  nm <- unique(unlist(lapply(parts, names)))
  setNames(vapply(nm, function(n) sum(vapply(parts, function(p)
    if (n %in% names(p)) p[[n]] else 0, numeric(1))), numeric(1)), nm)
}
planned <- list()
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
  lv <- levels(z$strategy)
  eff <- function(key) challenger_effect(TVAR[[key]], lv)
  contrasts <- list(
    "General minus specific promise" =
      combine_weights(eff("GEN"), -eff("SPEC")),
    "Retrospective claim minus prospective promises (mean of general and specific)" =
      combine_weights(eff("PAST"), -0.5 * eff("GEN"), -0.5 * eff("SPEC")))
  n_strategies <- nlevels(z$strategy)
  planned[[as.character(users)]] <- imap_dfr(contrasts, function(w, label)
    contrast_pp(m, z, w[w != 0]) |>
      mutate(contrast = label,
             sample = if (users) "H4: transparency users" else "H2: incumbent-contested races",
             n = n / n_strategies))   # stacked rows back to candidates
}
planned <- bind_rows(planned) |> group_by(sample) |>
  mutate(p_holm = p.adjust(p.value, "holm")) |> ungroup()
stopifnot(all(planned$n[planned$sample == "H2: incumbent-contested races"] == 14791L),
          all(planned$n[planned$sample == "H4: transparency users"] == 3657L))
wr(planned, "t_planned_contrasts")

output_names <- unique(c(output_names, "t_h2_omnibus.txt", 
                         "t_h4_omnibus.txt"))

# H2 robustness. Holm adjustment is separate within each five-outcome family.
robust <- bind_rows(
  h2 |> mutate(spec = "Main"),
  # W_OBS is modelled with electoral rank as a predictor. Use it only for these
  # communication outcomes, never for H1 or H3, where rank or election is the outcome.
  status_table(d, weight = "W_OBS") |> mutate(spec = "Candidate-observability IPW"),
  status_table(d |> filter(SAMPLE_FULLY_OBSERVED == 1)) |> mutate(spec = "Fully observed races"),
  status_table(d, vars = BASE) |> mutate(spec = "All hits"),
  # Replaces the former "Unambiguous previous-cycle winner" row, which SAMPLE_H2
  # already imposes and which therefore reproduced the main row exactly.
  status_table(d, length_bins = TRUE) |>
    mutate(spec = paste0("Flexible length control (", LENGTH_BINS, " bins)")),
  status_table(d |> filter(PRIOR_SUPPLEMENTARY_ELECTION == 0)) |>
    mutate(spec = "No prior supplementary-election record"),
  map_dfr(sort(unique(d$YEAR)), function(y)
    status_table(d |> filter(YEAR == y)) |> mutate(spec = paste("Year", y)))
)

wr(robust, "t_h2_robustness")

# Is the challenger gap equal across election years? One model per outcome on
# the H2 sample, with year-specific challenger effects and length slopes (race
# FE absorb year levels). Joint cluster-robust Wald test of the two differences
# from 2012, plus each difference as a contrast.
year_het <- imap_dfr(TVAR, function(v, k) {
  z <- fit_status(d, v)$data |> mutate(Y2016 = as.integer(YEAR == 2016L),
                                       Y2020 = as.integer(YEAR == 2020L))
  m <- feols(as.formula(paste(v, "~ CHALLENGER_TRUE + CHALLENGER_TRUE:Y2016 +",
                              "CHALLENGER_TRUE:Y2020 + LOG_N_WORDS + LOG_N_WORDS:Y2016 +",
                              "LOG_N_WORDS:Y2020 | RACE_ID")), data = z, vcov = ~ MUNI)
  terms <- c("CHALLENGER_TRUE:Y2016", "CHALLENGER_TRUE:Y2020")
  if (length(setdiff(terms, names(coef(m))))) stop("Year interaction dropped for ", v)
  b <- coef(m)[terms]; V <- vcov(m)[terms, terms]
  df2 <- fixest::degrees_freedom(m, type = "t")
  f_stat <- drop(t(b) %*% solve(V, b)) / length(terms)
  bind_rows(
    contrast_pp(m, z, c("CHALLENGER_TRUE:Y2016" = 1)) |> mutate(term = "2016 minus 2012"),
    contrast_pp(m, z, c("CHALLENGER_TRUE:Y2020" = 1)) |> mutate(term = "2020 minus 2012")
  ) |> mutate(key = k, outcome = LABELS[[k]], joint_f = f_stat, joint_df1 = length(terms),
              joint_df2 = df2, joint_p = pf(f_stat, length(terms), df2, lower.tail = FALSE))
})
wr(year_het, "t_h2_year_heterogeneity")

# H1: binary transparency regressor, binary election outcome. Scale coefficient
# and uncertainty by 100 only at reporting, giving the 0-to-1 contrast in pp.
# Main specifications control for incumbency: challengers use transparency more
# and win less often, so an unadjusted estimate mixes the two. They also keep
# only races whose winner has usable text; in the other races every observed
# candidate lost, which adds no information and dilutes the estimate. The
# previous specification is reported for comparison and should not be the main
# electoral result.
H1_SPECS <- list(
  "Race FE + incumbency" =
    list(f = ELECTED ~ TALK + INCUMBENT_TRUE + LOG_N_WORDS | RACE_ID, adjusted = TRUE),
  "Race + party FE + incumbency" =
    list(f = ELECTED ~ TALK + INCUMBENT_TRUE + LOG_N_WORDS | RACE_ID + PARTY_F, adjusted = TRUE),
  "Race FE, no incumbency control (previous)" =
    list(f = ELECTED ~ TALK + LOG_N_WORDS | RACE_ID, adjusted = FALSE))

h1 <- imap_dfr(TVAR, function(v, k) {
  base <- d |> filter(SAMPLE_H1 == 1, !is.na(.data[[v]]), is.finite(LOG_N_WORDS)) |>
    mutate(TALK = .data[[v]])
  adjusted <- base |> filter(!is.na(INCUMBENT_TRUE)) |>
    group_by(RACE_ID) |> filter(any(ELECTED == 1L)) |> ungroup()
  imap_dfr(H1_SPECS, function(s, spec) {
    z <- if (s$adjusted) adjusted else base
    m <- feols(s$f, data = z, vcov = ~ MUNI)
    coefficient_pp(m, z, "TALK") |>
      mutate(key = k, outcome = LABELS[[k]], spec = spec,
             sample = if (s$adjusted) "Known incumbency; winner's plan observed" else "SAMPLE_H1",
             ci90_low = estimate - qt(.95, df) * std.error,
             ci90_high = estimate + qt(.95, df) * std.error,
             sesoi_pp = SESOI_PP,
             tost_p = if (is.na(SESOI_PP)) NA_real_ else
               pmax(pt((estimate - SESOI_PP) / std.error, df),
                    pt(-(estimate + SESOI_PP) / std.error, df)),
             equivalent = if (is.na(SESOI_PP)) NA else ci90_low > -SESOI_PP & ci90_high < SESOI_PP)
  })
}) |> group_by(spec) |> mutate(p_holm = p.adjust(p.value, "holm")) |> ungroup()

# Checkpoints from the verified revised run (September 2026).
stopifnot(all(h1$n[h1$sample != "SAMPLE_H1"] == 29856L),
          all(h1$n_races[h1$sample != "SAMPLE_H1"] == 10631L),
          all(h1$n[h1$sample == "SAMPLE_H1"] == 33142L))

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

# The H3 models, unchanged, wrapped so they can be rerun on a restricted base.
run_h3 <- function(h3base, with_predictions = TRUE) {
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
    if (with_predictions) {
      used <- z[fixest::obs(m), , drop = FALSE]
      predictions[[k]] <- crossing(role_code = c(0L, 1L), margin_pp = MARGINS_PP) |>
        pmap_dfr(function(role_code, margin_pp) {
          nd <- used |> mutate(TRUE_RUNNER_UP = role_code,
                               TRUE_MARGIN_10PP = margin_pp / 10,
                               RUNNER_MARGIN = role_code * margin_pp / 10)
          tibble(key = k, outcome = LABELS[[k]], role = if (role_code == 0) "Leader" else "Runner-up",
                 margin_pp = margin_pp, predicted_probability_pp = 100 * mean(predict(m, newdata = nd)))
        })
    }
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
  list(
    absolute = bind_rows(absolute) |> group_by(role) |>
      mutate(p_holm = p.adjust(p.value, "holm")) |> ungroup(),
    predictions = bind_rows(predictions),
    relative = bind_rows(relative) |> group_by(comparison, margin_pp) |>
      mutate(p_holm = p.adjust(p.value, "holm")) |> ungroup(),
    interactions = bind_rows(interactions) |> group_by(comparison) |>
      mutate(p_holm = p.adjust(p.value, "holm")) |> ungroup())
}

h3 <- run_h3(h3base)
wr(h3$absolute, "t_h3_absolute_margin_slopes")
wr(h3$predictions, "t_h3_standardized_predictions")
wr(h3$relative, "t_h3_relative_contrasts_5_10_20pp")
wr(h3$interactions, "t_h3_relative_margin_interactions")

# H3 sensitivity: ranks and margins count votes received, including votes for
# candidates whose registration was later denied, cancelled or withdrawn (legally
# annulled). Drop races where such a candidate is in the population top two.
annulled_top2 <- d |>
  filter(TRUE_RANK <= 2, !des_situacao_candidatura %in% APPROVED_STATUS) |>
  distinct(RACE_ID) |> pull(RACE_ID)
wr(d |> distinct(RACE_ID, YEAR) |>
     mutate(annulled_top2 = RACE_ID %in% annulled_top2) |>
     count(YEAR, annulled_top2, name = "races"), "t_h3_annulled_vote_races")
h3_reg <- run_h3(h3base |> filter(!RACE_ID %in% annulled_top2), with_predictions = FALSE)
wr(h3_reg$absolute, "t_h3_absolute_margin_slopes_registered_top2")
wr(h3_reg$relative, "t_h3_relative_contrasts_registered_top2")
wr(h3_reg$interactions, "t_h3_relative_margin_interactions_registered_top2")

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
if (!UPLOAD) {
  message("MAYORAL_UPLOAD is FALSE: outputs kept in ", normalizePath(OUT_DIR, winslash = "/"))
} else {
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
}
