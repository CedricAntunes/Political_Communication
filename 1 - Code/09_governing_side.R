# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: September, 2026 --------------------------------------------------------
# Script title: 09_governing_side.R --------------------------------------------
#
# Notes: Is transparency avoidance a matter of personal incumbency or of being on
# the governing side? Each candidate is placed relative to the mayor elected in
# the previous ordinary election in the same municipality (the "outgoing
# mayor"; 2008 winners code the 2012 races):
#   Incumbent           the outgoing mayor running again (INCUMBENT_TRUE);
#   Mayor's party       same party number as the outgoing mayor at election;
#   Coalition partner   party number in the outgoing mayor's electoral coalition;
#   Opposition          every other candidate (reference group).
# Parties are matched by NUMERO_PARTIDO, which survives renames (e.g. PMDB and
# MDB are both 15). Mergers that change a number are not linked. The coalition
# is the ELECTORAL coalition four years earlier, not the governing coalition.
# Within-race LPMs (race FE, log plan length, municipality-clustered CIs), for
# all races and for open-seat races only. Associational evidence.
# ------------------------------------------------------------------------------

# Required packages ------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(fixest)
  library(ggplot2)
})
setFixest_notes(FALSE)

# Parameters -------------------------------------------------------------------
LOCAL_DIR  <- "C:/Users/cedric.antunes/Downloads/votes_municipality"
OUT_DIR    <- Sys.getenv("MAYORAL_OUTPUT_DIR", "output")
FRAME_PATH <- Sys.getenv("MAYORAL_FRAME_PATH", file.path(LOCAL_DIR, "analysis_frame.rds"))
ROSTER_DIR <- Sys.getenv("MAYORAL_ROSTER_DIR", LOCAL_DIR)
TVAR   <- c(ANY = "TRANSPARENCY_ANY_MENTIONED_CONFIDENT", PAST = "PAST_CLAIM_BINARY_CONFIDENT",
            GEN = "GENERAL_PROMISE_BINARY_CONFIDENT", SPEC = "SPECIFIC_PROMISE_BINARY_CONFIDENT",
            STYLE = "RHETORIC_BINARY_CONFIDENT")
LABELS <- c(ANY = "Any transparency mention", PAST = "Retrospective claim",
            GEN = "General prospective promise", SPEC = "Specific prospective promise",
            STYLE = "Transparency as governing style")
SIDES  <- c("Opposition", "Incumbent", "Mayor's party", "Coalition partner")
PALETTE <- c("Incumbent" = "#0072B2", "Mayor's party" = "#D55E00",
             "Coalition partner" = "#009E73")                      # as in script 04

dir.create(file.path(OUT_DIR, "figures"), recursive = TRUE, showWarnings = FALSE)
wr <- function(x, name) write_csv(x, file.path(OUT_DIR, paste0(name, ".csv")))
norm <- function(x) gsub("[^A-Z0-9]", "", toupper(x))   # "PC do B" = "PC DO B" = "PCDOB"

# Outgoing mayors from the rosters (ordinary elections) ------------------------
ros <- map_dfr(c(2008L, 2012L, 2016L), function(y)
  read_csv(file.path(ROSTER_DIR, sprintf("votes_municipality_%d.csv", y)),
           col_types = cols(.default = "c"), progress = FALSE,
           col_select = c(ano_eleicao, descricao_eleicao, num_turno, uf, cod_mun_tse,
                          sequencial_candidato, cpf_candidato, numero_partido, sigla_partido,
                          composicao_legenda, desc_sit_tot_turno))) |>
  filter(!is.na(sequencial_candidato), !grepl("SUPLEMENTAR", toupper(descricao_eleicao))) |>
  mutate(YEAR = as.integer(ano_eleicao), MUNI = str_trim(cod_mun_tse),
         CAND = paste(str_trim(uf), YEAR, str_trim(sequencial_candidato), sep = "|"))

# Abbreviation -> party number, per election year (abbreviations are unique within a year)
party_map <- ros |> distinct(YEAR, KEY = norm(sigla_partido), PARTY = as.integer(numero_partido))
stopifnot(!anyDuplicated(party_map[c("YEAR", "KEY")]))

# Winner = ELEITO in any round (runoffs included); composition from round 1.
winners <- ros |> group_by(YEAR, MUNI, CAND) |>
  summarise(ELECTED = any(desc_sit_tot_turno == "ELEITO", na.rm = TRUE),
            W_CPF = first(cpf_candidato), W_PARTY = as.integer(first(numero_partido)),
            COMP = first(composicao_legenda[num_turno == "1"]), .groups = "drop") |>
  filter(ELECTED) |> group_by(YEAR, MUNI) |> filter(n() == 1L) |> ungroup()

coalitions <- winners |> filter(!is.na(COMP), !str_detect(COMP, "^#")) |>
  mutate(KEY = map(str_split(COMP, "/"), norm)) |> select(YEAR, MUNI, KEY) |> unnest(KEY) |>
  left_join(party_map, by = c("YEAR", "KEY"))
wr(coalitions |> summarise(tokens = n(), matched = mean(!is.na(PARTY))),
   "t_governing_side_token_matching")
coalitions <- coalitions |> filter(!is.na(PARTY)) |>
  group_by(YEAR, MUNI) |> summarise(W_COALITION = list(unique(PARTY)), .groups = "drop")

prior <- winners |> left_join(coalitions, by = c("YEAR", "MUNI")) |>
  transmute(YEAR = YEAR + 4L, MUNI, W_CPF, W_PARTY,
            W_COALITION = map2(W_COALITION, W_PARTY, ~ union(.y, if (is.null(.x)) integer() else .x)))

# Candidates relative to the outgoing mayor ------------------------------------
d <- readRDS(FRAME_PATH) |>
  filter(TEXT_OBSERVED == 1, is.finite(LOG_N_WORDS), !is.na(INCUMBENT_TRUE),
         RACE_TYPE_TRUE %in% c("Incumbent contested", "Open seat")) |>
  inner_join(prior, by = c("YEAR", "MUNI")) |>
  mutate(PARTY = as.integer(numero_partido),
         SIDE  = factor(case_when(
           INCUMBENT_TRUE == 1                           ~ "Incumbent",
           PARTY == W_PARTY                              ~ "Mayor's party",
           map2_lgl(PARTY, W_COALITION, ~ .x %in% .y)    ~ "Coalition partner",
           TRUE                                          ~ "Opposition"), levels = SIDES))

# Validation: every coded incumbent is the outgoing mayor identified here.
stopifnot(all(d$cpf_candidato[d$SIDE == "Incumbent"] == d$W_CPF[d$SIDE == "Incumbent"]))
wr(d |> count(YEAR, RACE_TYPE_TRUE, SIDE, name = "candidates"), "t_governing_side_counts")

# Estimation: each side minus opposition candidates in the same race -----------
estimate <- function(dat, sample_label) imap_dfr(TVAR, function(v, k) {
  z <- dat |> filter(!is.na(.data[[v]])) |>
    group_by(RACE_ID) |> filter(n_distinct(SIDE) > 1, any(SIDE == "Opposition")) |> ungroup()
  m <- feols(as.formula(paste(v, "~ i(SIDE, ref = 'Opposition') + LOG_N_WORDS | RACE_ID")),
             data = z, cluster = ~ MUNI)
  terms <- grep("^SIDE::", names(coef(m)), value = TRUE)
  ci <- confint(m)[terms, ]
  tibble(sample = sample_label, key = k, outcome = LABELS[[k]],
         side = sub("^SIDE::", "", terms), estimate = 100 * coef(m)[terms],
         conf.low = 100 * ci[, 1], conf.high = 100 * ci[, 2], p.value = pvalue(m)[terms],
         n = nobs(m), n_races = n_distinct(z$RACE_ID),
         n_side = as.vector(table(z$SIDE))[match(sub("^SIDE::", "", terms), SIDES)])
})

est <- bind_rows(estimate(d, "All races"),
                 estimate(d |> filter(RACE_TYPE_TRUE == "Open seat"), "Open-seat races")) |>
  group_by(sample, side) |> mutate(p_holm = p.adjust(p.value, "holm")) |> ungroup()
wr(est, "t_governing_side_estimates")
print(est |> select(sample, side, key, estimate, conf.low, conf.high, p_holm, n_side, n_races),
      n = Inf)

# Figure (same look as script 04) ----------------------------------------------
pd <- position_dodge(width = 0.6)
p <- est |>
  mutate(outcome = factor(outcome, levels = rev(LABELS)),
         side = factor(side, levels = names(PALETTE)),
         holm = factor(if_else(p_holm < .05, "Holm p < .05", "Holm p >= .05"),
                       levels = c("Holm p < .05", "Holm p >= .05"))) |>
  ggplot(aes(estimate, outcome, colour = side, group = side)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), linewidth = 0.5, position = pd) +
  geom_point(aes(shape = holm), size = 2.3, fill = "white", position = pd) +
  facet_wrap(~ sample, nrow = 1) +
  scale_colour_manual(values = PALETTE) +
  scale_shape_manual(values = c("Holm p < .05" = 16, "Holm p >= .05" = 21), drop = FALSE) +
  guides(colour = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  labs(x = "Difference from opposition candidates in the same race (percentage points)", y = NULL,
       title = "Transparency communication and governing-side status",
       subtitle = "Candidates placed relative to the outgoing mayor; race FE and log plan length; 95% CIs clustered by municipality",
       caption = paste0("Parties matched by party number across elections. Coalition = the outgoing mayor's ",
                        "electoral coalition four years earlier.\nHolm adjustment across the five outcomes, ",
                        "within each sample and group. Associational.")) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        plot.title.position = "plot", plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, colour = "grey30"),
        legend.position = "bottom", legend.title = element_blank(), legend.box = "vertical",
        strip.background = element_rect(fill = "grey95"))

for (ext in c("png", "pdf"))
  ggsave(file.path(OUT_DIR, "figures", paste0("fig_governing_side.", ext)), p,
         width = 12, height = 5, dpi = 300, device = if (ext == "pdf") cairo_pdf else "png")
