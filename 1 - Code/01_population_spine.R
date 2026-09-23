# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: September, 2026 --------------------------------------------------------
# Script title: 01_population_spine.R ------------------------------------------
#
# Notes: ordinary elections only; unknown incumbency preserved;
# validated outputs uploaded to Votes Municipality > output in Google Drive.
# Builds the POPULATION frame of first-round mayoral candidates (2012, 2016,
# 2020) from the TSE municipal returns, and computes every electoral quantity
# that must NOT be derived from the observed-pledge sample:
#
#   TRUE_VOTE_SHARE_PP, TRUE_RANK, TRUE_FIRST_PLACE, TRUE_RUNNER_UP,
#   TRUE_MARGIN_PP, ELECTED, N_CANDIDATES_TRUE, N_INCUMBENTS_TRUE,
#   RACE_TYPE_TRUE, ENC_TRUE
# ------------------------------------------------------------------------------

# Cleaning my environment 
rm(list = ls())

# Managing memory
gc()

# Required packages ------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(purrr)
})

# ------------------------------------------------------------------------------
# Parameters -------------------------------------------------------------------
# ------------------------------------------------------------------------------
SPINE_CFG <- list(
  # Input (local) directory
  ROSTER_DIR = Sys.getenv("MAYORAL_ROSTER_DIR", 
                          unset = "C:/Users/cedric.antunes/Downloads/votes_municipality"),
  
  # 2008 establishes previous-cycle winners; it is excluded from final outputs.
  YEARS = c(2008L, 2012L, 2016L, 2020L),
  ANALYSIS_YEARS = c(2012L, 2016L, 2020L),
  
  # Output directory (in my personal Google Drive)
  OUT_DIR = Sys.getenv("MAYORAL_OUTPUT_DIR", unset = "output"),
  DRIVE_OUTPUT_FOLDER_ID = Sys.getenv("MAYORAL_DRIVE_OUTPUT_FOLDER_ID",
                                      unset = "1IcDW6_Q9vezxq4zR06hJEGdAXYhb8jVd")
)

# Columns of interest
ROSTER_COLS <- c(
  "ano_eleicao", "descricao_eleicao", "num_turno", "uf", "cod_mun_tse", "cod_mun_ibge",
  "nome_municipio", "id_candidato", "sequencial_candidato", "cpf_candidato",
  "numero_candidato", "nome_candidato", "nome_urna_candidato",
  "numero_partido", "sigla_partido", "nome_coligacao",
  "des_situacao_candidatura", "desc_sit_tot_turno", "qtde_votos"
)

# Identifier lengths vary across years/states. Preserve candidate IDs exactly
# as character strings; never force them to a common width. Only CPF is padded
# to eleven digits after ruling out missing-value sentinels.
ROSTER_TYPES <- cols(
  .default             = col_character(),
  ano_eleicao          = col_integer(),
  num_turno            = col_integer(),
  qtde_votos           = col_double(),
  sequencial_candidato = col_character(),
  cpf_candidato        = col_character(),
  numero_candidato     = col_character(),
  cod_mun_tse          = col_character(),
  cod_mun_ibge         = col_character()
)

# ------------------------------------------------------------------------------
# Loading the data -------------------------------------------------------------
# ------------------------------------------------------------------------------
read_roster_year <- function(year) {
  path <- file.path(SPINE_CFG$ROSTER_DIR,
                    sprintf("votes_municipality_%d.csv", year))
  
  raw <- read_csv(path, col_types = ROSTER_TYPES, progress = TRUE)
  if (nrow(problems(raw))) stop("Parsing problems in ", path)
  missing_cols <- setdiff(c(ROSTER_COLS, "descricao_cargo"), names(raw))
  if (length(missing_cols))
    stop("Missing expected columns in ", basename(path), ": ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  
  stopifnot(!anyNA(raw$ano_eleicao), all(raw$ano_eleicao == year),
            !anyNA(raw$descricao_cargo), all(raw$descricao_cargo == "PREFEITO"),
            !anyNA(raw$num_turno), all(raw$num_turno %in% c(1L, 2L)),
            all(c(1L, 2L) %in% raw$num_turno))
  if (any(!is.finite(raw$qtde_votos)) || any(raw$qtde_votos < 0))
    stop("Invalid vote counts in ", path)
  
  raw |>
    select(all_of(ROSTER_COLS)) |>
    # Defensive: the extract should already be mayoral only, but do not assume.
    mutate(
      sequencial_candidato = str_trim(sequencial_candidato),
      # -4 is a missing-CPF sentinel in the supplied export, never an identity.
      cpf_candidato = if_else(str_detect(str_trim(cpf_candidato), "^[0-9]{1,11}$"),
                              str_pad(str_trim(cpf_candidato), 11, pad = "0"), NA_character_),
      cod_mun_tse          = str_trim(cod_mun_tse),
      uf                   = str_trim(uf)
    )
}

roster_raw <- map_dfr(SPINE_CFG$YEARS, read_roster_year)

# ------------------------------------------------------------------------------
# SeparatING candidate rows from blank / null ballot rows ----------------------
# VOTO BRANCO carries id_candidato "B####" and VOTO NULO "N####". Both have a
# missing sequencial_candidato. They are kept for ballot-total diagnostics;
# vote shares always use candidate votes, excluding blank and null ballots.
# ------------------------------------------------------------------------------
roster_raw <- roster_raw |>
  mutate(
    ROW_TYPE = case_when(
      !is.na(sequencial_candidato) & nzchar(sequencial_candidato) ~ "candidate",
      str_detect(coalesce(nome_candidato, ""), "BRANCO")          ~ "blank",
      str_detect(coalesce(nome_candidato, ""), "NULO")            ~ "null",
      TRUE                                                         ~ "other"
    )
  )

stopifnot(!any(roster_raw$ROW_TYPE == "other"))

# ------------------------------------------------------------------------------
# Election scope ---------------------------------------------------------------
# Election year alone does not identify an election event: the supplied 2008
# and 2020 files also contain supplementary elections. Match the exact ordinary
# election label before computing totals, winners, ranks, or prior winners.
# Unknown labels stop the script rather than being silently discarded.
# ------------------------------------------------------------------------------
roster_raw <- roster_raw |>
  mutate(
    ELECTION_SCOPE = case_when(
      str_squish(descricao_eleicao) == paste("ELEIÇÕES MUNICIPAIS", ano_eleicao) ~
        "Ordinary",
      str_detect(str_to_upper(coalesce(descricao_eleicao, "")), "SUPLEMENTAR") ~
        "Supplementary",
      TRUE ~ "Unrecognized"
    )
  )

# Tabuation: elections
election_scope_audit <- roster_raw |>
  count(ano_eleicao, descricao_eleicao, ELECTION_SCOPE, num_turno, ROW_TYPE,
        name = "N_INPUT_ROWS")

# Excluded candidates
excluded_candidates <- roster_raw |>
  filter(ELECTION_SCOPE != "Ordinary", ROW_TYPE == "candidate") |>
  transmute(CAND_ID = paste(uf, ano_eleicao, sequencial_candidato, sep = "|"),
            YEAR = ano_eleicao, UF = uf, MUNI = cod_mun_tse, nome_municipio,
            descricao_eleicao, num_turno, EXCLUSION_REASON = ELECTION_SCOPE)

# Supplementary elections
supplementary_history <- roster_raw |>
  filter(ELECTION_SCOPE == "Supplementary") |>
  distinct(PRIOR_YEAR = ano_eleicao, MUNI = cod_mun_tse) |>
  mutate(PRIOR_SUPPLEMENTARY_ELECTION = 1L)

# Roster of ordinary elections (ELEÇÕES ORDINÁRIAS)
roster_ordinary <- roster_raw |> filter(ELECTION_SCOPE == "Ordinary")

# Total votes
ballot_totals <- roster_ordinary |>
  group_by(ano_eleicao, num_turno, cod_mun_tse) |>
  summarise(
    TOTAL_VOTES_RACE = sum(qtde_votos, na.rm = TRUE),
    VALID_VOTES_RACE = sum(qtde_votos[ROW_TYPE == "candidate"], na.rm = TRUE),
    BLANK_VOTES_RACE = sum(qtde_votos[ROW_TYPE == "blank"], na.rm = TRUE),
    NULL_VOTES_RACE  = sum(qtde_votos[ROW_TYPE == "null"], na.rm = TRUE),
    .groups = "drop"
  )

# Sanity checks ----------------------------------------------------------------
candidates <- roster_ordinary |> filter(ROW_TYPE == "candidate")
stopifnot(all(grepl("^[0-9]{11}$", na.omit(candidates$cpf_candidato))),
          !any(candidates$cpf_candidato == "00000000000", na.rm = TRUE),
          !anyNA(candidates$cod_mun_tse), !anyNA(candidates$uf))

# ------------------------------------------------------------------------------
# Candidate key ----------------------------------------------------------------
# Matches the CAND_ID construction in final_analysis_ready.Rmd so the corpus
# joins without a crosswalk: paste(UF, YEAR, SEQUENCIAL, sep = "|").
# ------------------------------------------------------------------------------
candidates <- candidates |>
  mutate(
    YEAR    = ano_eleicao,
    UF      = uf,
    MUNI    = cod_mun_tse,
    RACE_ID = paste(MUNI, YEAR, sep = "|"),
    CAND_ID = paste(UF, YEAR, sequencial_candidato, sep = "|")
  )

# A candidate appears once per round. The key must be unique within round.
dup_key <- candidates |>
  count(CAND_ID, num_turno) |>
  filter(n > 1)

if (nrow(dup_key))
  stop("CAND_ID is not unique within round for ", nrow(dup_key), " keys. ",
       "Inspect before proceeding.", call. = FALSE)

# Every runoff record must carry back to an ordinary first-round record.
if (nrow(anti_join(candidates |> filter(num_turno == 2L),
                   candidates |> filter(num_turno == 1L) |> select(CAND_ID),
                   by = "CAND_ID")))
  stop("Runoff candidate without an ordinary first-round row.")

# ------------------------------------------------------------------------------
# Final electoral outcome, carried back to the first-round row -----------------
# A runoff winner is ELEITO on the round-2 row only. The design note specifies
# that final status is carried to the single first-round row per candidate.
# ------------------------------------------------------------------------------
final_outcome <- candidates |>
  group_by(CAND_ID) |>
  summarise(
    ELECTED         = as.integer(any(desc_sit_tot_turno == "ELEITO", na.rm = TRUE)),
    REACHED_RUNOFF  = as.integer(any(num_turno == 2L)),
    N_ROUNDS_RUN    = n_distinct(num_turno),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# First-round frame ------------------------------------------------------------
# ------------------------------------------------------------------------------
spine <- candidates |>
  filter(num_turno == 1L) |>
  left_join(final_outcome, by = "CAND_ID") |>
  left_join(
    ballot_totals |> filter(num_turno == 1L) |> select(-num_turno),
    by = c("ano_eleicao", "cod_mun_tse")
  ) |>
  mutate(
    FIRST_ROUND_VOTES = qtde_votos,
    DENOM_RACE = VALID_VOTES_RACE,
    TRUE_VOTE_SHARE_PP = if_else(DENOM_RACE > 0,
                                 100 * FIRST_ROUND_VOTES / DENOM_RACE,
                                 NA_real_)
  )

# ------------------------------------------------------------------------------
# True rank, top two, and margin -----------------------------------------------
# ------------------------------------------------------------------------------
spine <- spine |>
  group_by(RACE_ID) |>
  mutate(
    N_CANDIDATES_TRUE = n(),
    TRUE_RANK = rank(-FIRST_ROUND_VOTES, ties.method = "min", na.last = "keep"),
    N_AT_RANK1 = sum(TRUE_RANK == 1, na.rm = TRUE),
    N_AT_RANK2 = sum(TRUE_RANK == 2, na.rm = TRUE),
    TRUE_FIRST_PLACE = as.integer(TRUE_RANK == 1),
    TRUE_RUNNER_UP   = as.integer(TRUE_RANK == 2),
    VALID_RANKING_TRUE = as.integer(N_AT_RANK1 == 1 & N_AT_RANK2 == 1),
    N_WINNERS_TRUE = sum(ELECTED == 1, na.rm = TRUE),
    # Effective number of candidates, a cleaner fragmentation control than a
    # raw count when race size varies this much.
    ENC_TRUE = if (first(VALID_VOTES_RACE) > 0)
      1 / sum((TRUE_VOTE_SHARE_PP / 100)^2) else NA_real_
  ) |>
  ungroup()

# Vote margin
race_margin <- spine |>
  filter(VALID_RANKING_TRUE == 1) |>
  group_by(RACE_ID) |>
  summarise(
    LEADER_SHARE_PP    = max(TRUE_VOTE_SHARE_PP[TRUE_RANK == 1]),
    RUNNER_UP_SHARE_PP = max(TRUE_VOTE_SHARE_PP[TRUE_RANK == 2]),
    .groups = "drop"
  ) |>
  mutate(TRUE_MARGIN_PP = LEADER_SHARE_PP - RUNNER_UP_SHARE_PP)

# True vote margin
spine <- spine |>
  left_join(race_margin, by = "RACE_ID") |>
  mutate(
    TRUE_MARGIN_10PP = TRUE_MARGIN_PP / 10,
    CLOSE_5PP_TRUE   = as.integer(TRUE_MARGIN_PP <= 5),
    CLOSE_10PP_TRUE  = as.integer(TRUE_MARGIN_PP <= 10)
  )

# ------------------------------------------------------------------------------
# Incumbency by CPF linkage ----------------------------------------------------
# INCUMBENT_TRUE retains its existing name for downstream compatibility, but
# means "same CPF as the previous ORDINARY-election winner in this municipality".
# It does not establish who was actually serving at the next election date.
# Supplementary-election history is flagged for sensitivity analysis; the input
# has no election-event dates sufficient to reconstruct intervening officeholders.
# Prior races with zero/multiple winners, missing prior-winner CPF, missing
# current CPF, or no prior ordinary race produce NA, never challenger/open seat
# ------------------------------------------------------------------------------
prior_races <- spine |>
  group_by(YEAR, MUNI) |>
  summarise(PRIOR_N_WINNERS_TRUE = first(N_WINNERS_TRUE),
            PRIOR_WINNER_CPF_MISSING = any(ELECTED == 1 & is.na(cpf_candidato)),
            .groups = "drop") |>
  rename(PRIOR_YEAR = YEAR)

prior_winners <- spine |>
  filter(N_WINNERS_TRUE == 1L, ELECTED == 1, !is.na(cpf_candidato)) |>
  transmute(PRIOR_YEAR = YEAR, MUNI, PRIOR_WINNER_CPF = cpf_candidato)
stopifnot(!anyDuplicated(prior_winners[c("PRIOR_YEAR", "MUNI")]))

spine <- spine |>
  mutate(PRIOR_YEAR = YEAR - 4L) |>
  left_join(prior_races, by = c("PRIOR_YEAR", "MUNI")) |>
  left_join(prior_winners, by = c("PRIOR_YEAR", "MUNI")) |>
  left_join(supplementary_history, by = c("PRIOR_YEAR", "MUNI")) |>
  mutate(
    PRIOR_SUPPLEMENTARY_ELECTION = coalesce(PRIOR_SUPPLEMENTARY_ELECTION, 0L),
    PRIOR_CYCLE_AVAILABLE = as.integer(!is.na(PRIOR_N_WINNERS_TRUE)),
    PRIOR_WINNER_UNAMBIGUOUS = as.integer(coalesce(
      PRIOR_N_WINNERS_TRUE == 1L & !PRIOR_WINNER_CPF_MISSING, FALSE)),
    INCUMBENCY_STATUS = case_when(
      PRIOR_CYCLE_AVAILABLE == 0L ~ "Prior ordinary race unavailable",
      PRIOR_N_WINNERS_TRUE != 1L ~ "Prior ordinary winner count is not one",
      PRIOR_WINNER_CPF_MISSING ~ "Prior ordinary winner CPF missing",
      is.na(cpf_candidato) ~ "Current candidate CPF missing",
      TRUE ~ "Known"
    ),
    INCUMBENT_TRUE = if_else(INCUMBENCY_STATUS == "Known",
                             as.integer(cpf_candidato == PRIOR_WINNER_CPF),
                             NA_integer_),
    CHALLENGER_TRUE = 1L - INCUMBENT_TRUE
  ) |>
  select(-PRIOR_WINNER_CPF)

spine <- spine |>
  group_by(RACE_ID) |>
  mutate(
    N_INCUMBENTS_KNOWN = sum(INCUMBENT_TRUE == 1L, na.rm = TRUE),
    N_UNKNOWN_INCUMBENTS = sum(is.na(INCUMBENT_TRUE)),
    # Race composition is known only when every candidate can be classified.
    N_INCUMBENTS_TRUE = if_else(N_UNKNOWN_INCUMBENTS == 0L,
                                N_INCUMBENTS_KNOWN, NA_integer_),
    RACE_TYPE_TRUE = case_when(
      is.na(N_INCUMBENTS_TRUE) ~ "Unknown",
      N_INCUMBENTS_TRUE == 0L ~ "Open seat",
      N_INCUMBENTS_TRUE == 1L ~ "Incumbent contested",
      TRUE ~ "Multiple coded incumbents"
    ),
    INCUMBENT_CONTESTED_TRUE = as.integer(N_INCUMBENTS_TRUE == 1L)
  ) |>
  ungroup()

# Keep 2008 only through the linkage step, never in the analysis outputs.
spine <- spine |> filter(YEAR %in% SPINE_CFG$ANALYSIS_YEARS)

# Sanity checks ----------------------------------------------------------------
stopifnot(
  !any(duplicated(spine$CAND_ID)),
  all(spine$ELECTION_SCOPE == "Ordinary"),
  !any(spine$CAND_ID %in% excluded_candidates$CAND_ID),
  all(is.finite(spine$TRUE_VOTE_SHARE_PP)),
  all(is.na(spine$INCUMBENT_TRUE[spine$PRIOR_WINNER_UNAMBIGUOUS == 0L])),
  all(spine$N_UNKNOWN_INCUMBENTS[spine$RACE_TYPE_TRUE == "Open seat"] == 0L),
  all(is.na(spine$N_INCUMBENTS_TRUE[spine$N_UNKNOWN_INCUMBENTS > 0L])),
  all(spine$TRUE_VOTE_SHARE_PP >= 0 & spine$TRUE_VOTE_SHARE_PP <= 100,
      na.rm = TRUE),
  all(spine$TRUE_MARGIN_PP >= 0, na.rm = TRUE)
)

share_check <- spine |>
  group_by(RACE_ID) |>
  summarise(s = sum(TRUE_VOTE_SHARE_PP, na.rm = TRUE), .groups = "drop")
if (any(abs(share_check$s - 100) > 1e-8))
  stop("Vote shares do not sum to 100 within every race. Inspect before proceeding.")

# ------------------------------------------------------------------------------
# Race-level dataframe ---------------------------------------------------------
# ------------------------------------------------------------------------------
race_frame <- spine |>
  group_by(RACE_ID, YEAR, UF, MUNI, nome_municipio) |>
  summarise(
    N_CANDIDATES_TRUE        = first(N_CANDIDATES_TRUE),
    N_WINNERS_TRUE           = first(N_WINNERS_TRUE),
    N_INCUMBENTS_TRUE        = first(N_INCUMBENTS_TRUE),
    N_INCUMBENTS_KNOWN       = first(N_INCUMBENTS_KNOWN),
    N_UNKNOWN_INCUMBENTS     = first(N_UNKNOWN_INCUMBENTS),
    PRIOR_CYCLE_AVAILABLE    = first(PRIOR_CYCLE_AVAILABLE),
    PRIOR_N_WINNERS_TRUE     = first(PRIOR_N_WINNERS_TRUE),
    PRIOR_WINNER_UNAMBIGUOUS = first(PRIOR_WINNER_UNAMBIGUOUS),
    PRIOR_SUPPLEMENTARY_ELECTION = first(PRIOR_SUPPLEMENTARY_ELECTION),
    RACE_TYPE_TRUE           = first(RACE_TYPE_TRUE),
    INCUMBENT_CONTESTED_TRUE = first(INCUMBENT_CONTESTED_TRUE),
    VALID_RANKING_TRUE       = first(VALID_RANKING_TRUE),
    TRUE_MARGIN_PP           = first(TRUE_MARGIN_PP),
    ENC_TRUE                 = first(ENC_TRUE),
    VALID_VOTES_RACE         = first(VALID_VOTES_RACE),
    TOTAL_VOTES_RACE         = first(TOTAL_VOTES_RACE),
    HAD_RUNOFF               = as.integer(any(REACHED_RUNOFF == 1, na.rm = TRUE)),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Diagnostics ------------------------------------------------------------------
# ------------------------------------------------------------------------------
cat("\n== Population spine ==\n")
cat("First-round candidate-elections:", nrow(spine), "\n")
cat("Municipality-election races:     ", n_distinct(spine$RACE_ID), "\n")
cat("Municipalities:                  ", n_distinct(spine$MUNI), "\n\n")

print(spine |>
        count(YEAR, name = "candidates") |>
        left_join(race_frame |> count(YEAR, name = "races"), by = "YEAR"))

cat("\n== Winners per race (population) ==\n")
print(race_frame |> count(N_WINNERS_TRUE))

cat("\n== Ranking validity ==\n")
print(race_frame |> count(VALID_RANKING_TRUE))

cat("\n== True first-round margin (pp) ==\n")
print(race_frame |>
        filter(VALID_RANKING_TRUE == 1) |>
        group_by(YEAR) |>
        summarise(races = n(),
                  median = median(TRUE_MARGIN_PP),
                  mean   = mean(TRUE_MARGIN_PP),
                  within_5pp  = mean(TRUE_MARGIN_PP <= 5),
                  within_10pp = mean(TRUE_MARGIN_PP <= 10),
                  .groups = "drop"))

cat("\n== Race composition (unknown classifications preserved) ==\n")
print(race_frame |> count(YEAR, RACE_TYPE_TRUE))

cat("\n== Runoffs ==\n")
print(race_frame |> group_by(YEAR) |> summarise(runoff_races = sum(HAD_RUNOFF)))

# Data checkpoints -------------------------------------------------------------
stopifnot(nrow(spine) == 50040L,
          n_distinct(spine$RACE_ID) == 16704L,
          n_distinct(spine$MUNI) == 5568L,
          sum(race_frame$N_WINNERS_TRUE == 0) == 201L,
          sum(race_frame$N_WINNERS_TRUE == 1) == 16503L,
          !any(race_frame$N_WINNERS_TRUE > 1L))
checks <- race_frame |>
  group_by(YEAR) |>
  summarise(candidates = sum(N_CANDIDATES_TRUE), races = n(),
            incumbent_contested = sum(INCUMBENT_CONTESTED_TRUE == 1L, na.rm = TRUE),
            unknown_incumbency_races = sum(RACE_TYPE_TRUE == "Unknown"),
            unknown_incumbency_candidates = sum(N_UNKNOWN_INCUMBENTS),
            prior_supplementary_races = sum(PRIOR_SUPPLEMENTARY_ELECTION == 1L),
            median_valid_margin_pp = median(TRUE_MARGIN_PP, na.rm = TRUE),
            .groups = "drop") |>
  arrange(YEAR)
stopifnot(
  identical(as.integer(checks$YEAR), c(2012L, 2016L, 2020L)),
  identical(as.integer(checks$candidates), c(15122L, 16132L, 18786L)),
  all(checks$races == 5568L),
  identical(as.integer(checks$incumbent_contested), c(2369L, 2586L, 3186L)),
  identical(as.integer(checks$unknown_incumbency_races), c(103L, 81L, 52L)),
  identical(as.integer(checks$unknown_incumbency_candidates), c(281L, 255L, 184L)),
  identical(as.integer(checks$prior_supplementary_races), c(149L, 0L, 0L)),
  all(abs(checks$median_valid_margin_pp -
            c(10.0738347946, 11.4285714286, 12.3227917121)) < 1e-6)
)
# Margins require a unique leader and runner-up. Single-candidate races and
# tied top-two rankings have undefined margins, never an imposed zero.

# ------------------------------------------------------------------------------
# Saving the data --------------------------------------------------------------
# ------------------------------------------------------------------------------
# My personal Google Drive
googledrive::drive_auth(email = "cedricantunes07@gmail.com")
drive_folder <- googledrive::drive_get(
  googledrive::as_id(SPINE_CFG$DRIVE_OUTPUT_FOLDER_ID))

# Data documentation -----------------------------------------------------------
dir.create(SPINE_CFG$OUT_DIR, showWarnings = FALSE, recursive = TRUE)
write_csv(checks, file.path(SPINE_CFG$OUT_DIR, "m0_population_checkpoints.csv"))
write_csv(election_scope_audit,
          file.path(SPINE_CFG$OUT_DIR, "m0_election_scope.csv"))
write_csv(excluded_candidates,
          file.path(SPINE_CFG$OUT_DIR, "m0_excluded_nonordinary_candidates.csv"))
write_csv(spine |> filter(is.na(INCUMBENT_TRUE)) |>
            select(CAND_ID, YEAR, RACE_ID, ELECTED, INCUMBENCY_STATUS,
                   PRIOR_N_WINNERS_TRUE, PRIOR_WINNER_CPF_MISSING),
          file.path(SPINE_CFG$OUT_DIR, "m0_unknown_incumbency.csv"))
write_csv(race_frame |> filter(N_WINNERS_TRUE != 1L),
          file.path(SPINE_CFG$OUT_DIR, "m0_ambiguous_winner_races.csv"))
write_csv(spine |> distinct(RACE_ID, YEAR, PRIOR_N_WINNERS_TRUE,
                            PRIOR_WINNER_UNAMBIGUOUS) |>
            filter(PRIOR_WINNER_UNAMBIGUOUS == 0L),
          file.path(SPINE_CFG$OUT_DIR, "m0_prior_winner_ambiguities.csv"))
write_csv(race_frame |> filter(PRIOR_SUPPLEMENTARY_ELECTION == 1L),
          file.path(SPINE_CFG$OUT_DIR, "m0_prior_supplementary_races.csv"))
saveRDS(spine, file.path(SPINE_CFG$OUT_DIR, "population_spine.rds"))
write_csv(spine, file.path(SPINE_CFG$OUT_DIR, "population_spine.csv"))
write_csv(race_frame, file.path(SPINE_CFG$OUT_DIR, "population_race_frame.csv"))
capture.output(sessionInfo(), file = file.path(SPINE_CFG$OUT_DIR, "session_01.txt"))

# Upload only this script's explicit outputs, never unrelated local files.
output_names <- c(
  "m0_population_checkpoints.csv", "m0_election_scope.csv",
  "m0_excluded_nonordinary_candidates.csv", "m0_unknown_incumbency.csv",
  "m0_ambiguous_winner_races.csv", "m0_prior_winner_ambiguities.csv",
  "m0_prior_supplementary_races.csv", "population_spine.rds",
  "population_spine.csv", "population_race_frame.csv", "session_01.txt"
)
existing <- googledrive::drive_ls(drive_folder)
duplicate_names <- existing |> filter(name %in% output_names) |>
  count(name) |> filter(n > 1L)
if (nrow(duplicate_names))
  stop("Duplicate output names in Drive; resolve before uploading: ",
       paste(duplicate_names$name, collapse = ", "))

tryCatch({
  for (name in output_names) {
    googledrive::drive_put(
      media = file.path(SPINE_CFG$OUT_DIR, name),
      path = drive_folder, name = name
    )
  }
}, error = function(e) {
  stop("Drive upload did not finish. Local outputs are saved in ",
       normalizePath(SPINE_CFG$OUT_DIR, winslash = "/"),
       ". Some Drive files may already be updated; rerun after resolving: ",
       conditionMessage(e), call. = FALSE)
})
message("01 complete: outputs saved to Google Drive > Votes Municipality > output\n",
        "https://drive.google.com/drive/folders/", SPINE_CFG$DRIVE_OUTPUT_FOLDER_ID,
        "\nLocal copies: ", normalizePath(SPINE_CFG$OUT_DIR, winslash = "/"))
message("Review the new audits before script 02. Its old 50,058-row checkpoint ",
        "and treatment of 12 supplementary corpus candidates need revision.")
