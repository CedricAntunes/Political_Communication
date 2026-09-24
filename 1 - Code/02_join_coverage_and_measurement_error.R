# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: September, 2026 --------------------------------------------------------
# Script title: 02_join_coverage_and_measurement_error.R -----------------------
# 
# Notes: Join frozen text measures to the population electoral frame from script 01.
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

# Default: download the reviewed script 01 inputs from the Drive output folder.
SPINE_PATH <- Sys.getenv("MAYORAL_SPINE_PATH", "")
EXCLUSIONS_PATH <- Sys.getenv("MAYORAL_EXCLUSIONS_PATH", "")
DRIVE_OUTPUT_FOLDER_ID <- Sys.getenv("MAYORAL_DRIVE_OUTPUT_FOLDER_ID",
                                     "1IcDW6_Q9vezxq4zR06hJEGdAXYhb8jVd")

# Corpus local path
CORPUS_PATH <- Sys.getenv("MAYORAL_CORPUS_PATH", 
                          "C:/Users/cedric.antunes/Downloads/votes_municipality/corpus_full_v2.csv")

# Capping
WEIGHT_TRIM <- c(0.01, 0.99)

# Creating directory 
dir.create(OUT_DIR, 
           recursive = TRUE, 
           showWarnings = FALSE)

output_names <- character()
wr <- function(x, name) {
  filename <- paste0(name, ".csv")
  write_csv(x, file.path(OUT_DIR, filename))
  output_names <<- unique(c(output_names, filename))
}

# Personal Google Drive folder -------------------------------------------------
googledrive::drive_auth(email = "cedricantunes07@gmail.com")

# Drive path
drive_folder <- googledrive::drive_get(googledrive::as_id(DRIVE_OUTPUT_FOLDER_ID))
stopifnot(nrow(drive_folder) == 1L, googledrive::is_folder(drive_folder))

drive_files <- googledrive::drive_ls(drive_folder)

download_input <- function(filename) {
  hit <- drive_files[drive_files$name == filename, ]
  if (nrow(hit) != 1L) stop("Expected exactly one ", filename, " in Drive output folder")
  path <- tempfile(fileext = paste0("_", filename))
  googledrive::drive_download(hit, path = path, overwrite = TRUE)
  path
}
if (!nzchar(SPINE_PATH)) SPINE_PATH <- download_input("population_spine.rds")
if (!nzchar(EXCLUSIONS_PATH))
  EXCLUSIONS_PATH <- download_input("m0_excluded_nonordinary_candidates.csv")

# Outcomes ---------------------------------------------------------------------
OUTCOMES <- c("TRANSPARENCY_ANY_MENTIONED", 
              "PAST_CLAIM_BINARY",
              "GENERAL_PROMISE_BINARY", 
              "SPECIFIC_PROMISE_BINARY", 
              "RHETORIC_BINARY")

# Focusing only on outcomes classified with confidence
OUTCOMES <- c(OUTCOMES, paste0(OUTCOMES, "_CONFIDENT"))

# Outcome counts
COUNTS <- c("TRANSPARENCY_HIT_COUNT_ALL", 
            "TRANSPARENCY_HIT_COUNT_CONFIDENT",
            "TRANSPARENCY_UNCERTAIN_COUNT", 
            "PAST_CLAIM_COUNT", 
            "GENERAL_PROMISE_COUNT",
            "SPECIFIC_PROMISE_COUNT", 
            "RHETORIC_COUNT", 
            "TRANSPARENCY_TOTAL_PER_1000",
            "TRANSPARENCY_TOTAL_PER_1000_CONFIDENT")

spine <- readRDS(SPINE_PATH)

# Forcing sanity!
stopifnot(nrow(spine) == 50040L, !anyDuplicated(spine$CAND_ID),
          all(spine$YEAR %in% c(2012L, 2016L, 2020L)),
          all(spine$ELECTION_SCOPE == "Ordinary"),
          n_distinct(spine$RACE_ID) == 16704L,
          sum(is.na(spine$INCUMBENT_TRUE)) == 720L)

required_spine <- c("ELECTION_SCOPE", 
                    "INCUMBENCY_STATUS", 
                    "N_UNKNOWN_INCUMBENTS",
                    "PRIOR_SUPPLEMENTARY_ELECTION", 
                    "PRIOR_WINNER_UNAMBIGUOUS")

if (!all(required_spine %in% names(spine))) stop("Use outputs from revised script 01")

stopifnot(all(is.na(spine$N_INCUMBENTS_TRUE[spine$N_UNKNOWN_INCUMBENTS > 0])))

excluded_keys <- read_csv(EXCLUSIONS_PATH,
                          col_types = cols(.default = col_character()), progress = FALSE)

stopifnot(nrow(problems(excluded_keys)) == 0L,
          all(c("CAND_ID", "YEAR", "EXCLUSION_REASON") %in% names(excluded_keys)),
          all(excluded_keys$EXCLUSION_REASON == "Supplementary"))

excluded_keys <- excluded_keys |> filter(YEAR %in% c("2012", "2016", "2020")) |>
  distinct(CAND_ID, .keep_all = TRUE)

stopifnot(nrow(excluded_keys) == 18L, !any(excluded_keys$CAND_ID %in% spine$CAND_ID))

# Reading corpus ---------------------------------------------------------------
corpus_raw <- readBin(CORPUS_PATH, what = "raw",
                      n = file.info(CORPUS_PATH)$size)

nul_bytes_removed <- sum(corpus_raw == as.raw(0L))

corpus_read_path <- CORPUS_PATH

if (nul_bytes_removed > 0L) {
  corpus_read_path <- tempfile(fileext = ".csv")
  writeBin(corpus_raw[corpus_raw != as.raw(0L)], corpus_read_path)
}

# Deleting raw corpus 
rm(corpus_raw)

wr(tibble(source_file = basename(CORPUS_PATH),
          nul_bytes_removed = nul_bytes_removed),
   "m1b_corpus_byte_cleaning")

stopifnot(nul_bytes_removed == 1L)

corpus <- read_csv(corpus_read_path,
                   col_types = cols(.default = col_character()),
                   progress = FALSE)

stopifnot(nrow(problems(corpus)) == 0L,
          nrow(corpus) == 39448L, ncol(corpus) == 60L)

needed <- c("UF", 
            "ANO_ELEICAO", 
            "SEQUENCIAL_CANDIDATO", 
            "N_WORDS",
            "PLEDGE_AVAILABLE", 
            "VOTE_SHARE_CANDIDATO", 
            "QTDE_VOTOS",
            "INCUMBENT", 
            "ELECTED", OUTCOMES)

# Sanity!
if (length(setdiff(needed, names(corpus))))
  stop("Missing corpus fields: ", paste(setdiff(needed, names(corpus)), collapse = ", "))

as_number <- function(x) {
  y <- suppressWarnings(as.numeric(x))
  if (any(!is.na(x) & is.na(y))) stop("Non-numeric value in numeric corpus field")
  y
}

# Corupus preaparation: Observeved candidate unique identifier -----------------
corpus <- corpus |>
  mutate(CAND_ID = paste(trimws(UF), trimws(ANO_ELEICAO), trimws(SEQUENCIAL_CANDIDATO), sep = "|"),
         across(any_of(c(OUTCOMES, COUNTS, "N_WORDS", "PLEDGE_AVAILABLE", "TEXT_FOUND",
                         "INCUMBENT", "ELECTED", "IS_INCUMBENT", "CHALLENGER",
                         "VOTE_SHARE_CANDIDATO", "QTDE_VOTOS", "QTDE_VOTOS_SUM",
                         "populacao", "log_populacao", "DUMMY_LAI_LEGISLATION")), as_number))
stopifnot(!anyNA(corpus$ANO_ELEICAO), !anyNA(corpus$UF), !anyNA(corpus$SEQUENCIAL_CANDIDATO),
          all(corpus$PLEDGE_AVAILABLE %in% c(0, 1, NA)),
          all(unlist(corpus[OUTCOMES]) %in% c(0, 1, NA)))
# Runoff duplicates have identical text payloads, although round/status fields
# can differ.

text_fields <- intersect(c(OUTCOMES, COUNTS, "N_WORDS", "PLEDGE_AVAILABLE", "TEXT_FOUND"),
                         names(corpus))

conflicts <- corpus |>
  group_by(CAND_ID) |>
  summarise(across(all_of(text_fields), ~ n_distinct(.x, na.rm = FALSE)), .groups = "drop") |>
  filter(if_any(all_of(text_fields), ~ .x > 1))

wr(conflicts, "m1b_duplicate_text_conflicts")

if (nrow(conflicts)) stop("Conflicting text payloads within candidate ID; inspect audit")

n_raw <- nrow(corpus)

n_unavailable_raw <- sum(corpus$PLEDGE_AVAILABLE == 0, na.rm = TRUE)

corpus <- corpus |> distinct(CAND_ID, .keep_all = TRUE)
stopifnot(n_raw == 39448L, nrow(corpus) == 39136L)

n_unique_before_scope <- nrow(corpus)

# Only these documented nonordinary IDs may be excluded. Other unmatched keys
# are errors, not presumed missing text or silently dropped observations.
excluded_corpus <- corpus |> inner_join(
  excluded_keys |> select(CAND_ID, EXCLUSION_REASON), by = "CAND_ID")

wr(excluded_corpus |> select(CAND_ID, ANO_ELEICAO, EXCLUSION_REASON,
                             PLEDGE_AVAILABLE, N_WORDS), "m1b_excluded_nonordinary_corpus")

stopifnot(nrow(excluded_corpus) == 12L)

corpus <- corpus |> anti_join(excluded_keys |> select(CAND_ID), by = "CAND_ID")

stopifnot(nrow(corpus) == 39124L)

unmatched <- corpus |> filter(!CAND_ID %in% spine$CAND_ID) |> select(CAND_ID)

wr(unmatched, "m1b_unmatched_corpus_keys")

if (nrow(unmatched)) stop("Unexpected corpus keys do not match the ordinary roster")

wr(tibble(raw_rows = n_raw, unique_before_scope = n_unique_before_scope,
          duplicate_rows = n_raw - n_unique_before_scope,
          supplementary_exclusions = nrow(excluded_corpus),
          eligible_unique_candidates = nrow(corpus), eligible_join_rate = 1),
   "m1b_join_audit")

# Keeping old quantities under explicit diagnostic-only names.
carry <- intersect(c(OUTCOMES, COUNTS, "N_WORDS", "PLEDGE_AVAILABLE", "TEXT_FOUND",
                     "EXTRACT_STATUS", "CLASSIFICATION_SAMPLE", "populacao",
                     "log_populacao", "HAS_LAI_LEGISLATION", "DUMMY_LAI_LEGISLATION",
                     "YEAR_LEGISLATION", "INCUMBENT", "ELECTED", "VOTE_SHARE_CANDIDATO",
                     "QTDE_VOTOS", "QTDE_VOTOS_SUM"), names(corpus))

payload <- corpus |> select(CAND_ID, all_of(carry)) |>
  rename(INCUMBENT_CORPUS = INCUMBENT, ELECTED_CORPUS = ELECTED,
         LEGACY_SHARE_RAW = VOTE_SHARE_CANDIDATO, LEGACY_SECTION_VOTES = QTDE_VOTOS)

# Analysis dataset -------------------------------------------------------------
d <- spine |> left_join(payload, by = "CAND_ID") |>
  mutate(PLAN_OBSERVED = as.integer(CAND_ID %in% corpus$CAND_ID),
         TEXT_STATUS = case_when(
           PLAN_OBSERVED == 0 ~ "Absent from corpus",
           is.na(PLEDGE_AVAILABLE) | !is.finite(N_WORDS) ~ "Missing text metadata",
           PLEDGE_AVAILABLE == 0 ~ "Unavailable pledge",
           N_WORDS <= 0 ~ "Available flag but no positive word count",
           TRUE ~ "Usable text"),
         TEXT_OBSERVED = as.integer(TEXT_STATUS == "Usable text"),
         LOG_N_WORDS = log(if_else(TEXT_OBSERVED == 1, N_WORDS, NA_real_)),
         across(any_of(c(OUTCOMES, COUNTS)), ~ if_else(TEXT_OBSERVED == 1, .x, NA_real_)),
         RANK_BIN = factor(if_else(TRUE_RANK >= 4, "4th+", paste0(TRUE_RANK)),
                           levels = c("1", "2", "3", "4th+")),
         PARTY_F = factor(coalesce(sigla_partido, "MISSING")))

stopifnot(nrow(d) == nrow(spine), !anyDuplicated(d$CAND_ID),
          !anyNA(d[d$TEXT_OBSERVED == 1, OUTCOMES]))

# Reconciled checks: 1,792 is a raw-row count; 1,768 is after deduplication.
stopifnot(n_unavailable_raw == 1792L,
          sum(d$TEXT_STATUS == "Unavailable pledge") == 1768L,
          sum(d$TEXT_STATUS == "Missing text metadata") == 354L,
          sum(d$TEXT_STATUS == "Available flag but no positive word count") == 7L,
          sum(d$PLAN_OBSERVED) == 39124L,
          sum(d$TEXT_OBSERVED) == 36995L)

wr(d |> count(YEAR, TEXT_STATUS, name = "candidates"), "m1b_text_availability")

wr(d |> filter(PLAN_OBSERVED == 1, TEXT_OBSERVED == 0) |>
     select(CAND_ID, YEAR, RACE_ID, N_WORDS, PLEDGE_AVAILABLE, any_of("TEXT_FOUND"), TEXT_STATUS),
   "m1b_text_exclusions")

# Both denominators are reported; prevalence and estimation use usable text.
coverage <- function(dat, groups) dat |>
  group_by(across(all_of(groups))) |>
  summarise(candidates = n(), corpus_rows = sum(PLAN_OBSERVED),
            usable_texts = sum(TEXT_OBSERVED), coverage_corpus = mean(PLAN_OBSERVED),
            coverage_text = mean(TEXT_OBSERVED), .groups = "drop")

wr(coverage(d, "YEAR"), "m1b_coverage_by_year")
wr(coverage(d, "RANK_BIN"), "m1b_coverage_by_true_rank")
wr(coverage(d |> filter(INCUMBENT_CONTESTED_TRUE == 1), c("YEAR", "INCUMBENT_TRUE")),
   "m1b_coverage_by_status")
wr(coverage(d, "RACE_TYPE_TRUE"), "m1b_coverage_by_race_type")
wr(coverage(d, "INCUMBENCY_STATUS"), "m1b_coverage_by_incumbency_certainty")
wr(coverage(d, "PRIOR_SUPPLEMENTARY_ELECTION"), "m1b_coverage_by_prior_supplementary")
wr(coverage(d, "ELECTED"), "m1b_coverage_by_elected")
wr(coverage(d, "sigla_partido"), "m1b_coverage_by_party")
wr(coverage(d, "N_CANDIDATES_TRUE"), "m1b_coverage_by_race_size")
race_cov <- coverage(d, c("RACE_ID", "YEAR"))
wr(race_cov, "m1b_coverage_by_race")

# Diagnostic only: distinguishing the stored wrong-share problem from the effect
# of restricting otherwise correct municipal vote counts to corpus candidates.
legacy <- d |> filter(PLAN_OBSERVED == 1)

finite_share <- legacy$LEGACY_SHARE_RAW[is.finite(legacy$LEGACY_SHARE_RAW)]

if (!length(finite_share)) stop("No legacy shares to audit")

share_multiplier <- if (max(finite_share) <= 1 + 1e-8) 100 else 1
legacy <- legacy |>
  group_by(RACE_ID) |>
  mutate(STORED_SHARE_PP = coalesce(share_multiplier * LEGACY_SHARE_RAW,
                                    100 * LEGACY_SECTION_VOTES / sum(LEGACY_SECTION_VOTES, na.rm = TRUE)),
         SUBSET_SHARE_PP = 100 * FIRST_ROUND_VOTES / sum(FIRST_ROUND_VOTES)) |>
  ungroup()
audit_measure <- function(share_col, label) {
  z <- legacy |> group_by(RACE_ID) |>
    mutate(DIAG_RANK = rank(-.data[[share_col]], ties.method = "min", na.last = "keep"),
           DIAG_VALID = sum(DIAG_RANK == 1, na.rm = TRUE) == 1 &
             sum(DIAG_RANK == 2, na.rm = TRUE) == 1) |> ungroup()
  rank_table <- z |> filter(!is.na(DIAG_RANK)) |>
    count(diag_rank = pmin(DIAG_RANK, 4), true_rank = pmin(TRUE_RANK, 4)) |>
    group_by(diag_rank) |> mutate(row_share = n / sum(n)) |> ungroup()
  margins <- z |> filter(DIAG_VALID, VALID_RANKING_TRUE == 1) |>
    group_by(RACE_ID, YEAR) |>
    summarise(TRUE_MARGIN_PP = first(TRUE_MARGIN_PP),
              DIAG_MARGIN_PP = .data[[share_col]][DIAG_RANK == 1] -
                .data[[share_col]][DIAG_RANK == 2], .groups = "drop") |>
    mutate(ERROR_PP = DIAG_MARGIN_PP - TRUE_MARGIN_PP)
  summary <- tibble(diagnostic = label,
                    first_place_correct = mean(z$TRUE_RANK[z$DIAG_RANK == 1] == 1, na.rm = TRUE),
                    runner_up_correct = mean(z$TRUE_RANK[z$DIAG_RANK == 2] == 2, na.rm = TRUE),
                    margin_correlation = cor(margins$DIAG_MARGIN_PP, margins$TRUE_MARGIN_PP),
                    margin_mae_pp = mean(abs(margins$ERROR_PP)),
                    share_mae_pp = mean(abs(z[[share_col]] - z$TRUE_VOTE_SHARE_PP), na.rm = TRUE),
                    true_close_5pp_recalled = mean(margins$DIAG_MARGIN_PP[margins$TRUE_MARGIN_PP <= 5] <= 5))
  wr(rank_table, paste0("m1b_rank_", label))
  wr(margins, paste0("m1b_margin_", label))
  list(summary = summary, margins = margins)
}

stored <- audit_measure("STORED_SHARE_PP", "stored_share")

subset_only <- audit_measure("SUBSET_SHARE_PP", "corpus_subset_only")

wr(bind_rows(stored$summary, subset_only$summary), "m1b_measurement_error_summary")

# Recomputed on the ordinary-election corpus; not the superseded handoff frame.
stopifnot(abs(stored$summary$first_place_correct - 0.709369144284822) < 1e-8,
          abs(stored$summary$runner_up_correct - 0.663571428571429) < 1e-8,
          abs(stored$summary$margin_correlation - 0.624786970408339) < 1e-8)

# ------------------------------------------------------------------------------
# Plotting ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
p <- ggplot(stored$margins, aes(TRUE_MARGIN_PP, DIAG_MARGIN_PP)) +
  geom_abline(slope = 1, intercept = 0, linewidth = .4) +
  geom_point(alpha = .08, size = .5) + facet_wrap(~ YEAR) + coord_equal() +
  labs(x = "Population margin (pp)", y = "Legacy stored-share margin (pp)") + theme_bw()

ggsave(file.path(OUT_DIR, "fig_margin_error.png"), p, width = 9, height = 3.5, dpi = 300)
p <- coverage(d, "RANK_BIN") |>
  ggplot(aes(RANK_BIN, coverage_text)) + geom_col(width = .65) +
  scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(100*x, "%")) +
  labs(x = "Population first-round rank", y = "Candidates with usable text") + theme_bw()
ggsave(file.path(OUT_DIR, "fig_coverage_by_rank.png"), p, width = 6, height = 4, dpi = 300)

# Candidate observability weights are a sensitivity analysis. They do not by
# themselves address selection into races with both candidate types observed.
obs_data <- d |> filter(!is.na(INCUMBENT_TRUE), !is.na(RANK_BIN), VALID_VOTES_RACE > 0)

m_obs <- feols(TEXT_OBSERVED ~ RANK_BIN + log(VALID_VOTES_RACE) + factor(YEAR) +
                 INCUMBENT_TRUE + log(N_CANDIDATES_TRUE) | PARTY_F,
               data = obs_data, vcov = ~ MUNI, fixef.rm = "none")

wr(tidy(m_obs, conf.int = TRUE), "m1b_observability_model")

obs_data$PS_RAW <- as.numeric(predict(m_obs, newdata = obs_data))

stopifnot(nobs(m_obs) == nrow(obs_data), all(is.finite(obs_data$PS_RAW)))

obs_data <- obs_data |>
  mutate(PS_OBSERVED = pmin(pmax(PS_RAW, .01), .99),
         W_STAB_RAW = mean(TEXT_OBSERVED) / PS_OBSERVED)

trim <- quantile(obs_data$W_STAB_RAW[obs_data$TEXT_OBSERVED == 1], WEIGHT_TRIM)

obs_data$W_OBS <- pmin(pmax(obs_data$W_STAB_RAW, trim[1]), trim[2])

wr(obs_data |> filter(TEXT_OBSERVED == 1) |>
     summarise(n = n(), mean_weight = mean(W_OBS), min_weight = min(W_OBS),
               max_weight = max(W_OBS), ess = sum(W_OBS)^2 / sum(W_OBS^2),
               probability_clipped = mean(PS_RAW < .01 | PS_RAW > .99),
               weight_trimmed = mean(W_OBS != W_STAB_RAW)), "m1b_weight_diagnostics")

# Weights are used only for observed, usable text. Unknown candidate statuses
# remain outside this sensitivity model; its sample is reported separately.
obs_data$W_OBS[obs_data$TEXT_OBSERVED == 0] <- NA_real_

d <- d |> left_join(obs_data |> select(CAND_ID, PS_OBSERVED, W_OBS), by = "CAND_ID")

# Samples use population electoral quantities and usable text, never the
# number of winners present in the corpus.
d <- d |> group_by(RACE_ID) |>
  mutate(FRAC_OBSERVED_RACE = mean(TEXT_OBSERVED),
         TOP_TWO_OBSERVED = as.integer(first(VALID_RANKING_TRUE) == 1 &
                                         sum(TEXT_OBSERVED == 1 & TRUE_RANK <= 2) == 2),
         INCUMBENT_OBSERVED = as.integer(any(TEXT_OBSERVED == 1 & INCUMBENT_TRUE == 1,
                                             na.rm = TRUE)),
         N_CHALLENGERS_OBSERVED = sum(TEXT_OBSERVED == 1 & CHALLENGER_TRUE == 1,
                                      na.rm = TRUE)) |> ungroup() |>
  mutate(SAMPLE_H1 = as.integer(TEXT_OBSERVED == 1 & N_WINNERS_TRUE == 1),
         SAMPLE_H2 = as.integer(coalesce(INCUMBENT_CONTESTED_TRUE == 1 &
                                           TEXT_OBSERVED == 1 & !is.na(INCUMBENT_TRUE) & INCUMBENT_OBSERVED == 1 & N_CHALLENGERS_OBSERVED >= 1, FALSE)),
         SAMPLE_H3_TOPTWO = as.integer(TEXT_OBSERVED == 1 & VALID_RANKING_TRUE == 1 &
                                         TRUE_RANK <= 2 & TOP_TWO_OBSERVED == 1),
         SAMPLE_FULLY_OBSERVED = as.integer(FRAC_OBSERVED_RACE == 1))

# Documenting why the explicit both-sides/text sample differs from the old brief.
h2_flow <- d |> group_by(RACE_ID) |>
  summarise(contested = first(INCUMBENT_CONTESTED_TRUE) == 1,
            any_corpus = any(PLAN_OBSERVED == 1),
            incumbent_corpus = any(PLAN_OBSERVED == 1 & INCUMBENT_TRUE == 1, na.rm = TRUE),
            challenger_corpus = any(PLAN_OBSERVED == 1 & CHALLENGER_TRUE == 1, na.rm = TRUE),
            usable_pair = any(SAMPLE_H2 == 1), .groups = "drop") |>
  filter(contested)
wr(tibble(stage = c("Population incumbent-contested", "Any corpus candidate",
                    "Incumbent in corpus", "Both types in corpus", "Both types with usable text"),
          races = c(nrow(h2_flow), sum(h2_flow$any_corpus), sum(h2_flow$incumbent_corpus),
                    sum(h2_flow$incumbent_corpus & h2_flow$challenger_corpus),
                    sum(h2_flow$usable_pair))), "m1b_h2_sample_flow")

stopifnot(sum(d$SAMPLE_H2) == 14791L,
          n_distinct(d$RACE_ID[d$SAMPLE_H2 == 1]) == 5238L)

# H4 requires BOTH an incumbent user and a challenger user after restricting
# to transparency users. Record confident-main and all-hit sensitivity samples.
for (suffix in c("CONFIDENT", "ALL")) {
  any_var <- if (suffix == "CONFIDENT") "TRANSPARENCY_ANY_MENTIONED_CONFIDENT" else
    "TRANSPARENCY_ANY_MENTIONED"
  users <- d |> filter(SAMPLE_H2 == 1, .data[[any_var]] == 1) |>
    group_by(RACE_ID) |>
    filter(any(INCUMBENT_TRUE == 1), any(CHALLENGER_TRUE == 1)) |> ungroup()
  d[[paste0("SAMPLE_H4_", suffix)]] <- as.integer(d$CAND_ID %in% users$CAND_ID)
}

stopifnot(sum(d$SAMPLE_H4_CONFIDENT) == 3657L,
          n_distinct(d$RACE_ID[d$SAMPLE_H4_CONFIDENT == 1]) == 1401L,
          all(d$N_UNKNOWN_INCUMBENTS[d$SAMPLE_H2 == 1] == 0L),
          all(is.finite(d$W_OBS[d$SAMPLE_H2 == 1])))

flags <- c("PLAN_OBSERVED", 
           "TEXT_OBSERVED", 
           "SAMPLE_H1", 
           "SAMPLE_H2",
           "SAMPLE_H3_TOPTWO", 
           "SAMPLE_FULLY_OBSERVED",
           "SAMPLE_H4_CONFIDENT", 
           "SAMPLE_H4_ALL")

wr(map_dfr(flags, function(f) {
  z <- d |> filter(.data[[f]] == 1)
  tibble(sample = f, candidates = nrow(z), races = n_distinct(z$RACE_ID))
}), "m1b_analysis_sample_sizes")

stopifnot(!any(c("VOTE_SHARE_CANDIDATO", "VOTE_RANK", "MARGIN_PP", "INCUMBENT",
                 "IS_INCUMBENT", "CHALLENGER") %in% names(d)))

# Remove legacy votes/shares from the estimation file after saving diagnostics.
d <- d |> select(-LEGACY_SHARE_RAW, -LEGACY_SECTION_VOTES, -any_of("QTDE_VOTOS_SUM"))

# ------------------------------------------------------------------------------
# Saving the data --------------------------------------------------------------
# ------------------------------------------------------------------------------
# Analysis dataset
saveRDS(d, file.path(OUT_DIR, "analysis_frame.rds"))
capture.output(sessionInfo(), file = file.path(OUT_DIR, "session_02.txt"))
output_names <- unique(c(output_names, "fig_margin_error.png",
                         "fig_coverage_by_rank.png", "analysis_frame.rds", "session_02.txt"))

# Update only this run's named outputs. Existing script 01 files are untouched.
existing <- googledrive::drive_ls(drive_folder)
duplicates <- existing |> filter(name %in% output_names) |> count(name) |> filter(n > 1L)
if (nrow(duplicates)) stop("Duplicate output filenames in Drive: ",
                           paste(duplicates$name, collapse = ", "))
tryCatch({
  for (name in output_names)
    googledrive::drive_put(file.path(OUT_DIR, name), path = drive_folder, name = name)
}, error = function(e) {
  stop("Drive upload did not finish. Local outputs remain in ",
       normalizePath(OUT_DIR, winslash = "/"),
       ". Some Drive files may already be updated; rerun after resolving: ",
       conditionMessage(e), call. = FALSE)
})
