# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: September, 2026 --------------------------------------------------------
# Script title: 06_coalition_size_descriptives.R -------------------------------
#
# Notes: Coalition size (number of parties behind a mayoral candidacy) and
# transparency communication. 
#
#   N_PARTIES = number of "/" separators plus one. COMPOSICAO_LEGENDA holds the
#   party name for single-party candidacies in every year (so they count as 1)
#   and "#NULO#" when the composition is unknown (kept NA). If only
#   COMPOSICAO_COLIGACAO is available, TIPO_LEGENDA is required, because that
#   field codes 2012 single-party candidacies as "#NULO".
# ------------------------------------------------------------------------------

# Required packages ------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(ggplot2)
  library(fixest)
})

setFixest_notes(FALSE)  # singleton races are dropped silently; they carry no within-race variation

# ------------------------------------------------------------------------------
# Parameters -------------------------------------------------------------------
# ------------------------------------------------------------------------------
LOCAL_DIR    <- "C:/Users/cedric.antunes/Downloads/votes_municipality"
OUT_DIR      <- Sys.getenv("MAYORAL_OUTPUT_DIR", "output")
FRAME_PATH   <- Sys.getenv("MAYORAL_FRAME_PATH", file.path(LOCAL_DIR, "analysis_frame.rds"))
PARQUET_PATH <- Sys.getenv("MAYORAL_COALITION_PATH",
                           file.path(LOCAL_DIR, "candidatos_gr_mun_final.parquet"))
ROSTER_DIR   <- Sys.getenv("MAYORAL_ROSTER_DIR", LOCAL_DIR)   # optional audit
PARQUET_DRIVE_ID <- "1A-n2PdEt3Avuj2VP60mjEBeGkZjVu1Ul"       # Votes Municipality > input
UPLOAD       <- as.logical(Sys.getenv("MAYORAL_UPLOAD", "FALSE"))
DRIVE_OUTPUT_FOLDER_ID <- Sys.getenv("MAYORAL_DRIVE_OUTPUT_FOLDER_ID",
                                     "1IcDW6_Q9vezxq4zR06hJEGdAXYhb8jVd")
YEARS        <- c(2012L, 2016L, 2020L)
MIN_COVERAGE <- 0.99   # share of usable-text candidates that must join
MAX_CONFLICTS <- 10    # candidates with conflicting sizes tolerated (set to NA)

TVAR <- c(ANY = "TRANSPARENCY_ANY_MENTIONED_CONFIDENT", PAST = "PAST_CLAIM_BINARY_CONFIDENT",
          GEN = "GENERAL_PROMISE_BINARY_CONFIDENT", SPEC = "SPECIFIC_PROMISE_BINARY_CONFIDENT",
          STYLE = "RHETORIC_BINARY_CONFIDENT")
LABELS <- c(ANY = "Any transparency mention", PAST = "Retrospective claim",
            GEN = "General prospective promise", SPEC = "Specific prospective promise",
            STYLE = "Transparency as governing style")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
written <- character()
wr <- function(x, name) {
  f <- file.path(OUT_DIR, paste0(name, ".csv")); write_csv(x, f); written <<- c(written, f)
}

# ------------------------------------------------------------------------------
# Coalition size from composition and list type --------------------------------
# ------------------------------------------------------------------------------
coalition_size <- function(comp, tipo = NA_character_) {
  comp <- str_squish(toupper(comp))
  isolated <- str_detect(coalesce(toupper(tipo), ""), "ISOLADO")
  sentinel <- is.na(comp) | comp == "" | str_detect(comp, "^#")
  case_when(isolated ~ 1L, sentinel ~ NA_integer_, TRUE ~ str_count(comp, "/") + 1L)
}

# Numeric IDs must never pass through scientific notation.
as_key <- function(x) {
  if (inherits(x, "integer64")) as.character(x)
  else if (is.numeric(x)) sprintf("%.0f", x)
  else str_trim(as.character(x))
}

# Reading the parquet (only the needed columns) --------------------------------
if (!file.exists(PARQUET_PATH)) {
  googledrive::drive_auth(email = "cedricantunes07@gmail.com")
  PARQUET_PATH <- tempfile(fileext = ".parquet")
  googledrive::drive_download(googledrive::as_id(PARQUET_DRIVE_ID), path = PARQUET_PATH)
}

ALIASES <- list(YEAR  = c("ANO_ELEICAO", "ano_eleicao"),
                TURNO = c("NUM_TURNO", "NR_TURNO", "num_turno"),
                UF    = c("SIGLA_UF", "UF", "SG_UF", "uf"),
                SEQ   = c("SEQUENCIAL_CANDIDATO", "SQ_CANDIDATO", "sequencial_candidato"),
                CARGO = c("DESCRICAO_CARGO", "DS_CARGO", "descricao_cargo"),
                TIPO  = c("TIPO_LEGENDA", "TP_AGREMIACAO", "tipo_legenda"),
                COMP  = c("COMPOSICAO_LEGENDA", "composicao_legenda",
                          "COMPOSICAO_COLIGACAO", "DS_COMPOSICAO_COLIGACAO",
                          "composicao_coligacao"))
use_arrow <- requireNamespace("arrow", quietly = TRUE)
schema <- if (use_arrow) names(arrow::open_dataset(PARQUET_PATH)) else
  nanoparquet::read_parquet_schema(PARQUET_PATH)$name
pick <- unlist(map(ALIASES, ~ intersect(.x, schema)[1]))
if (anyNA(pick[names(pick) != "TIPO"]))
  stop("Parquet lacks: ", paste(names(pick)[is.na(pick)], collapse = ", "),
       ". Columns found: ", paste(schema, collapse = ", "), call. = FALSE)
if (!str_detect(pick["COMP"], "(?i)legenda") && is.na(pick["TIPO"]))
  stop(pick["COMP"], " codes 2012 single-party candidacies as '#NULO'; TIPO_LEGENDA is required.",
       call. = FALSE)
message("Coalition composition read from ", pick["COMP"])
pick <- pick[!is.na(pick)]

raw <- if (use_arrow) arrow::read_parquet(PARQUET_PATH, col_select = all_of(unname(pick))) else
  nanoparquet::read_parquet(PARQUET_PATH, col_select = unname(pick))
raw <- as.data.frame(raw)
names(raw) <- names(pick)[match(names(raw), pick)]
if (!"TIPO" %in% names(raw)) raw$TIPO <- NA_character_

coal <- raw |>
  mutate(YEAR = as.integer(YEAR)) |>
  # First round only: 2012 runoff rows carry "#NULO#" compositions.
  filter(YEAR %in% YEARS, as.integer(TURNO) == 1L, str_squish(toupper(CARGO)) == "PREFEITO") |>
  mutate(CAND_ID = paste(str_trim(UF), YEAR, as_key(SEQ), sep = "|"),
         N_PARTIES = coalition_size(COMP, TIPO))
rm(raw)

# One size per candidate. The few candidates with conflicting first-round
# compositions are set to NA (kept in the frame, excluded from these models);
# more than MAX_CONFLICTS signals a structural problem and stops the script.
conflicts <- coal |> distinct(CAND_ID, N_PARTIES) |> count(CAND_ID) |> filter(n > 1)
wr(coal |> filter(CAND_ID %in% conflicts$CAND_ID) |> distinct(CAND_ID, COMP, N_PARTIES),
   "t_coalition_key_conflicts")
if (nrow(conflicts) > MAX_CONFLICTS)
  stop(nrow(conflicts), " candidates have conflicting coalition sizes", call. = FALSE)
if (nrow(conflicts))
  message(nrow(conflicts), " candidate(s) with conflicting sizes set to NA; see t_coalition_key_conflicts.csv")
coal <- coal |>
  mutate(N_PARTIES = if_else(CAND_ID %in% conflicts$CAND_ID, NA_integer_, N_PARTIES)) |>
  distinct(CAND_ID, .keep_all = TRUE) |> select(CAND_ID, N_PARTIES)

# ------------------------------------------------------------------------------
# Join -------------------------------------------------------------------------
# ------------------------------------------------------------------------------
d <- readRDS(FRAME_PATH) |>
  left_join(coal, by = "CAND_ID") |>
  mutate(SIZE_BIN = cut(N_PARTIES, c(0, 1, 3, 6, Inf),
                        labels = c("1 party", "2-3 parties", "4-6 parties", "7+ parties")))
stopifnot(!anyDuplicated(d$CAND_ID))

coverage <- d |> filter(TEXT_OBSERVED == 1) |>
  summarise(usable_text = n(), joined = mean(CAND_ID %in% coal$CAND_ID),
            size_known = mean(!is.na(N_PARTIES)))
wr(coverage, "t_coalition_coverage")
if (coverage$joined < MIN_COVERAGE)
  stop(sprintf("Only %.1f%% of usable-text candidates found in the parquet. Check the key format.",
               100 * coverage$joined))

# Optional audit against an independent field: the rosters' COMPOSICAO_COLIGACAO
# (with TIPO_LEGENDA), as used in script 01. Expect about 99.9% agreement.
if (file.exists(file.path(ROSTER_DIR, "votes_municipality_2012.csv"))) {
  roster <- map_dfr(YEARS, ~ read_csv(
    file.path(ROSTER_DIR, sprintf("votes_municipality_%d.csv", .x)),
    col_types = cols(.default = "c"), progress = FALSE,
    col_select = c(ano_eleicao, uf, sequencial_candidato, tipo_legenda, composicao_coligacao))) |>
    filter(!is.na(sequencial_candidato)) |>
    transmute(CAND_ID = paste(str_trim(uf), ano_eleicao, str_trim(sequencial_candidato), sep = "|"),
              N_ROSTER = coalition_size(composicao_coligacao, tipo_legenda)) |>
    distinct(CAND_ID, .keep_all = TRUE)
  audit <- d |> inner_join(roster, by = "CAND_ID") |>
    summarise(compared = n(), agree = mean(coalesce(N_PARTIES == N_ROSTER,
                                                    is.na(N_PARTIES) & is.na(N_ROSTER))))
  wr(audit, "t_coalition_roster_agreement")
  if (audit$agree < .99) warning(sprintf("Parquet and roster sizes agree for only %.1f%%",
                                         100 * audit$agree))
}

# ------------------------------------------------------------------------------
# Descriptives -----------------------------------------------------------------
# ------------------------------------------------------------------------------
wr(d |>
     mutate(STATUS = case_when(INCUMBENT_TRUE == 1 ~ "Incumbent",
                               INCUMBENT_TRUE == 0 ~ "Non-incumbent", TRUE ~ "Unknown")) |>
     group_by(YEAR, STATUS) |>
     summarise(candidates = n(), size_missing = mean(is.na(N_PARTIES)),
               mean_parties = mean(N_PARTIES, na.rm = TRUE),
               median_parties = median(N_PARTIES, na.rm = TRUE),
               single_party = mean(N_PARTIES == 1, na.rm = TRUE), .groups = "drop"),
   "t_coalition_descriptives")

# Distribution of coalition-size bins by election (descriptive) ----------------
# All candidacies with a known size. For the text-observed sample, add
# TEXT_OBSERVED == 1 to the filter.
size_dist <- d |>
  filter(!is.na(SIZE_BIN)) |>
  count(YEAR, SIZE_BIN, name = "candidates") |>
  group_by(YEAR) |>
  mutate(share = candidates / sum(candidates)) |>
  ungroup()
wr(size_dist, "t_coalition_size_distribution")
print(round(100 * prop.table(xtabs(candidates ~ YEAR + SIZE_BIN, size_dist), 1), 1))

fig_dist <- file.path(OUT_DIR, "fig_coalition_size_distribution.png")
ggsave(fig_dist, width = 7, height = 4.5, dpi = 300,
       ggplot(size_dist, aes(factor(YEAR), share, fill = SIZE_BIN)) +
         geom_col(position = position_stack(reverse = TRUE), width = .7) +
         geom_text(aes(label = scales::percent(share, accuracy = 1),
                       colour = if_else(as.integer(SIZE_BIN) >= 3, "white", "black")),
                   position = position_stack(vjust = .5, reverse = TRUE), size = 3.5) +
         scale_fill_brewer(palette = "Blues") + scale_colour_identity() +
         scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
         labs(x = NULL, y = "Share of mayoral candidacies", fill = "Coalition size",
              title = "Coalition size of mayoral candidacies by election",
              caption = "") +
         theme_bw(base_size = 11) +
         theme(panel.grid = element_blank(), plot.title.position = "plot",
               plot.caption.position = "plot",
               plot.caption = element_text(hjust = 0, colour = "grey30")))
written <- c(written, fig_dist)

# Share of each exact coalition size by election (descriptive) -----------------
# Sizes at or above TOP_CODE are pooled; the share denominator is the same as above.
TOP_CODE <- 12L
exact_dist <- d |>
  filter(!is.na(N_PARTIES)) |>
  mutate(SIZE = factor(pmin(N_PARTIES, TOP_CODE), levels = 1:TOP_CODE,
                       labels = c(1:(TOP_CODE - 1), paste0(TOP_CODE, "+")))) |>
  count(YEAR, SIZE, name = "candidates", .drop = FALSE) |>
  group_by(YEAR) |>
  mutate(share = candidates / sum(candidates)) |>
  ungroup()
wr(exact_dist, "t_coalition_size_exact_distribution")

fig_exact <- file.path(OUT_DIR, "fig_coalition_size_exact_distribution.png")
ggsave(fig_exact, width = 9, height = 4.5, dpi = 300,
       ggplot(exact_dist, aes(SIZE, share, fill = factor(YEAR))) +
         geom_col(position = position_dodge(width = .8), width = .75) +
         scale_fill_brewer(palette = "Dark2") +
         scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, .05))) +
         labs(x = "Number of parties behind the candidacy", y = "Share of mayoral candidacies",
              fill = NULL, title = "Coalition size of mayoral candidacies by election",
              caption = paste0("Descriptive. Shares within each election; sizes of ", TOP_CODE,
                               " or more pooled. 1 = party running alone.")) +
         theme_bw(base_size = 11) +
         theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
               legend.position = "top", plot.title.position = "plot",
               plot.caption.position = "plot",
               plot.caption = element_text(hjust = 0, colour = "grey30")))
written <- c(written, fig_exact)

# ------------------------------------------------------------------------------
# Estimation -------------------------------------------------------------------
# ------------------------------------------------------------------------------
# LPMs with race FE: candidates compared with rivals in the same race.
# Percentage points; CIs clustered by municipality; Holm across the 5 outcomes.
tidy_pp <- function(rhs, fe, dat, keep) imap_dfr(TVAR, function(v, k) {
  m  <- feols(as.formula(paste(v, "~", rhs, "|", fe)), data = dat, cluster = ~ MUNI)
  ct <- coeftable(m); ci <- confint(m)
  tibble(term = rownames(ct), estimate = 100 * ct[, 1], std.error = 100 * ct[, 2],
         conf.low = 100 * ci[, 1], conf.high = 100 * ci[, 2], p.value = ct[, 4],
         n = nobs(m), n_races = unname(m$fixef_sizes["RACE_ID"]),
         key = k, outcome = LABELS[[k]]) |>
    filter(str_detect(term, keep))
})

est <- d |> filter(TEXT_OBSERVED == 1, !is.na(N_PARTIES), !is.na(INCUMBENT_TRUE),
                   is.finite(LOG_N_WORDS))
ctrl <- "INCUMBENT_TRUE + LOG_N_WORDS"

size <- bind_rows(
  tidy_pp(paste("N_PARTIES +", ctrl), "RACE_ID", est, "^N_PARTIES$") |>
    mutate(spec = "Race FE"),
  tidy_pp(paste("N_PARTIES +", ctrl), "RACE_ID + PARTY_F", est, "^N_PARTIES$") |>
    mutate(spec = "Race + party FE"),
  tidy_pp(paste("i(SIZE_BIN, ref = '1 party') +", ctrl), "RACE_ID", est, "SIZE_BIN") |>
    mutate(spec = "Race FE")
) |>
  mutate(term = if_else(term == "N_PARTIES", "Per additional party",
                        paste(str_remove(term, "^SIZE_BIN::"), "vs 1 party"))) |>
  group_by(spec, term) |> mutate(p_holm = p.adjust(p.value, "holm")) |> ungroup()
wr(size, "t_coalition_size")

# Does coalition size account for the H2 challenger gap? Same sample, both fits.
h2 <- d |> filter(SAMPLE_H2 == 1, !is.na(N_PARTIES), is.finite(LOG_N_WORDS)) |>
  group_by(RACE_ID) |> filter(n_distinct(CHALLENGER_TRUE) == 2) |> ungroup()
h2_control <- bind_rows(
  tidy_pp("CHALLENGER_TRUE + LOG_N_WORDS", "RACE_ID", h2, "^CHALLENGER_TRUE$") |>
    mutate(spec = "Without coalition size"),
  tidy_pp("CHALLENGER_TRUE + N_PARTIES + LOG_N_WORDS", "RACE_ID", h2, "^CHALLENGER_TRUE$") |>
    mutate(spec = "With coalition size")
) |> group_by(spec) |> mutate(p_holm = p.adjust(p.value, "holm")) |> ungroup()
wr(h2_control, "t_h2_coalition_control")

# ------------------------------------------------------------------------------
# Plotting ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
pd <- position_dodge(width = .5)
p <- size |>
  mutate(outcome = factor(outcome, levels = rev(LABELS)),
         term = factor(term, levels = unique(term)),
         spec = factor(spec, levels = c("Race FE", "Race + party FE")),
         holm = if_else(p_holm < .05, "Holm p < .05", "Holm p >= .05")) |>
  ggplot(aes(estimate, outcome, colour = spec)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), position = pd) +
  geom_point(aes(shape = holm), position = pd, size = 2.2, fill = "white") +
  scale_shape_manual(values = c("Holm p < .05" = 16, "Holm p >= .05" = 21)) +
  scale_colour_brewer(palette = "Dark2") +
  facet_wrap(~ term, nrow = 1, scales = "free_x") +
  labs(x = "Percentage points", y = NULL,
       title = "Coalition size and transparency communication",
       subtitle = "Within-race LPMs controlling for incumbency and log word count; 95% CIs clustered by municipality",
       caption = "Associational. Bins are relative to single-party candidacies (race FE).") +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        legend.position = "bottom", legend.title = element_blank(),
        plot.title.position = "plot", plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, colour = "grey30"),
        strip.background = element_rect(fill = "grey95"))
fig <- file.path(OUT_DIR, "fig_coalition_size.png")
ggsave(fig, p, width = 12, height = 4.5, dpi = 300)
written <- c(written, fig)

# Console summary --------------------------------------------------------------
print(coverage)
print(size |> filter(term == "Per additional party") |>
        select(spec, outcome, estimate, conf.low, conf.high, p_holm, n, n_races))
print(h2_control |> select(spec, outcome, estimate, conf.low, conf.high, n, n_races))

# ------------------------------------------------------------------------------
# Saving the plots -------------------------------------------------------------
# ------------------------------------------------------------------------------
if (UPLOAD) {
  googledrive::drive_auth(email = "cedricantunes07@gmail.com")
  folder <- googledrive::drive_get(googledrive::as_id(DRIVE_OUTPUT_FOLDER_ID))
  for (f in written) googledrive::drive_put(f, path = folder, name = basename(f))
}
