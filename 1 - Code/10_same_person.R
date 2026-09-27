# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: September, 2026 --------------------------------------------------------
# Script title: 10_same_person.R -----------------------------------------------
#
# Notes: Same person, different status. Candidates are linked across the 2012,
# 2016 and 2020 elections by CPF. Does the same person's transparency
# communication change once they govern?
#
#   Main sample: people with two or more usable plans whose FIRST observed run
#   was as a non-incumbent. Those who won and later ran as incumbents are
#   compared with those who ran again without having won (repeat candidates).
#
#   Model: y = b * INCUMBENT + log plan length + person FE + year FE.
#   b is the within-person change in the probability of each type of
#   transparency communication when the candidate runs as incumbent, net of the
#   common change across elections shown by repeat candidates.
#
#   Placebo: a lead for the run in which the future incumbent won (before
#   governing). If winners were already changing before taking office, the lead
#   picks it up; it should be close to zero.
#
#   Robustness: all linked candidates, including those first seen as incumbents.
#   Associational: winners and repeat losers may differ in how their
#   communication would have evolved anyway; the placebo checks one version.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(fixest)
  library(ggplot2)
})
setFixest_notes(FALSE)

# Parameters -------------------------------------------------------------------
OUT_DIR    <- Sys.getenv("MAYORAL_OUTPUT_DIR", "output")
FRAME_PATH <- Sys.getenv("MAYORAL_FRAME_PATH",
                         "C:/Users/cedric.antunes/Downloads/votes_municipality/analysis_frame.rds")
TVAR   <- c(ANY = "TRANSPARENCY_ANY_MENTIONED_CONFIDENT", PAST = "PAST_CLAIM_BINARY_CONFIDENT",
            GEN = "GENERAL_PROMISE_BINARY_CONFIDENT", SPEC = "SPECIFIC_PROMISE_BINARY_CONFIDENT",
            STYLE = "RHETORIC_BINARY_CONFIDENT")
LABELS <- c(ANY = "Any transparency mention", PAST = "Retrospective claim",
            GEN = "General prospective promise", SPEC = "Specific prospective promise",
            STYLE = "Transparency as governing style")
SERIES <- c("Running as incumbent (main)" = "#0072B2",
            "Running as incumbent (all linked candidates)" = "#D55E00",
            "Placebo: the run they won, before governing" = "#009E73")   # palette of script 04

dir.create(file.path(OUT_DIR, "figures"), recursive = TRUE, showWarnings = FALSE)
wr <- function(x, name) write_csv(x, file.path(OUT_DIR, paste0(name, ".csv")))

# Linked panel of candidates ----------------------------------------------------
d <- readRDS(FRAME_PATH) |>
  filter(TEXT_OBSERVED == 1, is.finite(LOG_N_WORDS), !is.na(INCUMBENT_TRUE),
         grepl("^[0-9]{11}$", cpf_candidato), cpf_candidato != "00000000000") |>
  rename(PERSON = cpf_candidato)
stopifnot(!anyDuplicated(d[c("PERSON", "YEAR")]))   # one mayoral run per person-year

panel <- d |> group_by(PERSON) |> filter(n() >= 2L) |> arrange(YEAR, .by_group = TRUE) |>
  mutate(FIRST_NONINC = first(INCUMBENT_TRUE) == 0L,
         # Lead: the non-incumbent run immediately before a run as incumbent
         LEAD = as.integer(INCUMBENT_TRUE == 0L & lead(INCUMBENT_TRUE, default = 0L) == 1L &
                             lead(YEAR, default = 0L) == YEAR + 4L)) |>
  ungroup()

wr(panel |> group_by(PERSON) |>
     summarise(path = paste(ifelse(INCUMBENT_TRUE == 1, "I", "N"), collapse = "-"),
               years = paste(YEAR, collapse = "-"), .groups = "drop") |>
     count(path, years, name = "people", sort = TRUE), "t_same_person_paths")

# Descriptive two-by-two: consecutive runs, non-incumbent in the first ---------
pairs <- panel |> group_by(PERSON) |> arrange(YEAR, .by_group = TRUE) |>
  mutate(NEXT_YEAR = lead(YEAR), NEXT_INC = lead(INCUMBENT_TRUE)) |> ungroup() |>
  filter(INCUMBENT_TRUE == 0L, NEXT_YEAR == YEAR + 4L) |>
  select(PERSON, YEAR, NEXT_INC) |>
  mutate(GROUP = if_else(NEXT_INC == 1L, "Won, then ran as incumbent", "Ran again as non-incumbent"))
did <- imap_dfr(TVAR, function(v, k) {
  both <- pairs |>
    left_join(panel |> select(PERSON, YEAR, before = all_of(v)), by = c("PERSON", "YEAR")) |>
    left_join(panel |> transmute(PERSON, YEAR = YEAR - 4L, after = .data[[v]]),
              by = c("PERSON", "YEAR"))
  both |> group_by(GROUP) |>
    summarise(people = n(), before_pct = 100 * mean(before), after_pct = 100 * mean(after),
              change_pp = after_pct - before_pct, .groups = "drop") |>
    mutate(key = k, outcome = LABELS[[k]],
           difference_in_changes_pp = change_pp[GROUP == "Won, then ran as incumbent"] -
             change_pp[GROUP == "Ran again as non-incumbent"])
})
wr(did, "t_same_person_two_by_two")

# Within-person models ----------------------------------------------------------
fit <- function(dat, rhs, series, term) imap_dfr(TVAR, function(v, k) {
  z <- dat |> filter(!is.na(.data[[v]]))
  m <- feols(as.formula(paste0("I(100 * ", v, ") ~ ", rhs, " + LOG_N_WORDS | PERSON + YEAR")),
             data = z, cluster = ~ MUNI)
  ci <- confint(m)[term, ]
  tibble(series = series, key = k, outcome = LABELS[[k]], estimate = coef(m)[[term]],
         conf.low = ci[[1]], conf.high = ci[[2]], p.value = pvalue(m)[[term]],
         n = nobs(m), people = n_distinct(z$PERSON),
         switchers = z |> group_by(PERSON) |> summarise(s = n_distinct(INCUMBENT_TRUE) > 1) |>
           pull(s) |> sum())
})

main <- panel |> filter(FIRST_NONINC)
est <- bind_rows(
  fit(main, "INCUMBENT_TRUE", names(SERIES)[1], "INCUMBENT_TRUE"),
  fit(panel, "INCUMBENT_TRUE", names(SERIES)[2], "INCUMBENT_TRUE"),
  fit(main, "INCUMBENT_TRUE + LEAD", names(SERIES)[3], "LEAD")
) |> group_by(series) |> mutate(p_holm = p.adjust(p.value, "holm")) |> ungroup()
wr(est, "t_same_person_estimates")
print(did |> filter(key == "ANY")); print(est, n = Inf, width = Inf)

# Figure (same look as script 04) ----------------------------------------------
pd <- position_dodge(width = 0.6)
p <- est |>
  mutate(outcome = factor(outcome, levels = rev(LABELS)),
         series = factor(series, levels = names(SERIES)),
         holm = factor(if_else(p_holm < .05, "Holm p < .05", "Holm p >= .05"),
                       levels = c("Holm p < .05", "Holm p >= .05"))) |>
  ggplot(aes(estimate, outcome, colour = series, group = series)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), linewidth = 0.5, position = pd) +
  geom_point(aes(shape = holm), size = 2.3, fill = "white", position = pd) +
  scale_colour_manual(values = SERIES) +
  scale_shape_manual(values = c("Holm p < .05" = 16, "Holm p >= .05" = 21), drop = FALSE) +
  guides(colour = guide_legend(order = 1, ncol = 1), shape = guide_legend(order = 2)) +
  labs(x = "Within-person change (percentage points)", y = NULL,
       title = "Same person, different status: transparency communication once in office",
       subtitle = sprintf("%s people linked by CPF across elections; person and year FE, log plan length; 95%% CIs clustered by municipality",
                          format(n_distinct(main$PERSON), big.mark = ",")),
       caption = paste0("Main: people first observed as non-incumbents; those who won and ran again as ",
                        "incumbents vs. repeat non-incumbent candidates.\nPlacebo: the run in which future ",
                        "incumbents won, before governing. Holm across the five outcomes within each series. ",
                        "Associational.")) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        plot.title.position = "plot", plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, colour = "grey30"),
        legend.position = "bottom", legend.title = element_blank(), legend.box = "vertical")

for (ext in c("png", "pdf"))
  ggsave(file.path(OUT_DIR, "figures", paste0("fig_same_person.", ext)), p,
         width = 9, height = 5, dpi = 300, device = if (ext == "pdf") cairo_pdf else "png")
