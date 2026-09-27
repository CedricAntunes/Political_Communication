# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: September, 2026 --------------------------------------------------------
# Script title: 08_margin_by_coalition.R ---------------------------------------
#
# Notes: Does the relationship between electoral competitiveness (first-round
# margin) and transparency communication depend on coalition size, for
# challengers and for incumbents? Incumbent-contested races, candidates with a
# usable plan and a known coalition size.
#
#   Figure: share of plans mentioning transparency by margin band, one panel per
#   coalition size, challengers and incumbents as lines (descriptive).
#   Tests, estimated separately by status, controlling for plan length and year:
#     (1) the margin slope (per 10 points) within each coalition-size group, and a
#         joint Wald test that the slopes are equal across groups;
#     (2) a continuous interaction: how the margin slope changes per additional
#         party.
#   Margins are observed after the election and compared across races, and 2020
#   has much smaller coalitions (see script 06): associational evidence only.
#
# Inputs: analysis_frame.rds (script 02) and coalition_size_by_candidate.csv
# (script 05).
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(fixest)
  library(ggplot2)
})
setFixest_notes(FALSE)

# Parameters -------------------------------------------------------------------
LOCAL_DIR  <- "C:/Users/cedric.antunes/Downloads/votes_municipality"
OUT_DIR    <- Sys.getenv("MAYORAL_OUTPUT_DIR", "output")
FRAME_PATH <- Sys.getenv("MAYORAL_FRAME_PATH", file.path(LOCAL_DIR, "analysis_frame.rds"))
SIZE_PATH  <- Sys.getenv("MAYORAL_COALITION_SIZES",
                         file.path(OUT_DIR, "coalition_size_by_candidate.csv"))
OUTCOME    <- "TRANSPARENCY_ANY_MENTIONED_CONFIDENT"
SIZE_BINS  <- c("1 party", "2-3 parties", "4-6 parties", "7+ parties")   # as in 05 and 06
BANDS      <- c("0-5", "5-10", "10-20", "20+")
PALETTE    <- c(Challengers = "#0072B2", Incumbents = "#D55E00")         # as in 04 and 07

if (!file.exists(SIZE_PATH)) stop("Run script 05 first: ", SIZE_PATH, " not found.", call. = FALSE)
dir.create(file.path(OUT_DIR, "figures"), recursive = TRUE, showWarnings = FALSE)

# Sample -----------------------------------------------------------------------
d <- readRDS(FRAME_PATH) |>
  left_join(read_csv(SIZE_PATH, show_col_types = FALSE), by = "CAND_ID") |>
  filter(TEXT_OBSERVED == 1, INCUMBENT_CONTESTED_TRUE == 1, !is.na(INCUMBENT_TRUE),
         VALID_RANKING_TRUE == 1, is.finite(TRUE_MARGIN_PP), is.finite(LOG_N_WORDS),
         !is.na(.data[[OUTCOME]]), !is.na(N_PARTIES)) |>
  mutate(Y      = 100 * .data[[OUTCOME]],
         SIZE   = factor(cut(N_PARTIES, c(0, 1, 3, 6, Inf), labels = SIZE_BINS), levels = SIZE_BINS),
         BAND   = factor(cut(TRUE_MARGIN_PP, c(0, 5, 10, 20, Inf), labels = BANDS,
                             include.lowest = TRUE), levels = BANDS),
         STATUS = factor(if_else(INCUMBENT_TRUE == 1, "Incumbents", "Challengers"),
                         levels = names(PALETTE)))

# Descriptive shares: status x coalition size x margin band --------------------
cells <- d |> group_by(STATUS, SIZE) |> group_modify(function(z, key) {
  m <- feols(Y ~ 0 + BAND, data = z, cluster = ~ MUNI)
  lv <- sub("^BAND", "", names(coef(m))); ci <- confint(m)
  tibble(band = factor(lv, levels = BANDS), share = unname(coef(m)),
         conf.low = ci[, 1], conf.high = ci[, 2],
         candidates = as.vector(table(z$BAND))[match(lv, BANDS)])
}) |> ungroup()
write_csv(cells, file.path(OUT_DIR, "t_margin_by_coalition_cells.csv"))

# Tests, by status ------------------------------------------------------------
tests <- d |> group_by(STATUS) |> group_modify(function(z, key) {
  # (1) Margin slope within each coalition-size group, plus equality test
  m1 <- feols(Y ~ i(SIZE, TRUE_MARGIN_10PP) + i(SIZE) + LOG_N_WORDS | YEAR,
              data = z, cluster = ~ MUNI)
  sl <- grep("TRUE_MARGIN_10PP", names(coef(m1)), value = TRUE)
  ci <- confint(m1)[sl, ]
  w  <- wald(m1, keep = "TRUE_MARGIN_10PP", print = FALSE)   # H0: all slopes are zero
  b  <- coef(m1)[sl]; V <- vcov(m1)[sl, sl]
  R  <- cbind(-1, diag(length(sl) - 1))                        # H0: slopes are equal
  f_eq <- drop(t(R %*% b) %*% solve(R %*% V %*% t(R), R %*% b)) / nrow(R)
  df2  <- fixest::degrees_freedom(m1, type = "t")
  # (2) Continuous interaction: change in the margin slope per additional party
  m2 <- feols(Y ~ TRUE_MARGIN_10PP * N_PARTIES + LOG_N_WORDS | YEAR,
              data = z, cluster = ~ MUNI)
  ix <- "TRUE_MARGIN_10PP:N_PARTIES"
  bind_rows(
    tibble(test = paste("Margin slope per 10 pp,", sub(".*::(.*):.*", "\\1", sl)),
           estimate = unname(b), conf.low = ci[, 1], conf.high = ci[, 2],
           p.value = 2 * pt(-abs(b / sqrt(diag(V))), df2)),
    tibble(test = "Joint test: slopes equal across coalition sizes",
           statistic = f_eq, df1 = nrow(R), df2 = df2,
           p.value = pf(f_eq, nrow(R), df2, lower.tail = FALSE)),
    tibble(test = "Joint test: all slopes zero", statistic = w$stat, df1 = w$df1,
           df2 = w$df2, p.value = w$p),
    tibble(test = "Change in margin slope per additional party",
           estimate = coef(m2)[[ix]], conf.low = confint(m2)[ix, 1],
           conf.high = confint(m2)[ix, 2], p.value = pvalue(m2)[[ix]])
  ) |> mutate(n = nobs(m1))
}) |> ungroup()
write_csv(tests, file.path(OUT_DIR, "t_margin_by_coalition_tests.csv"))
print(tests, n = Inf, width = Inf)

# Figure -----------------------------------------------------------------------
eq <- tests |> filter(grepl("slopes equal", test))
cap <- paste0("Joint test that the margin slope is equal across coalition sizes (plan length and year ",
              "controlled): challengers p = ", sprintf("%.2f", eq$p.value[eq$STATUS == "Challengers"]),
              "; incumbents p = ", sprintf("%.2f", eq$p.value[eq$STATUS == "Incumbents"]), ".\n",
              "Descriptive comparison across races; margins are observed after the election.")
pd <- position_dodge(width = 0.35)
p <- ggplot(cells, aes(band, share, colour = STATUS, group = STATUS)) +
  geom_line(aes(linetype = STATUS), linewidth = 0.5, position = pd) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), linewidth = 0.5, position = pd) +
  geom_point(size = 2.3, position = pd) +
  facet_wrap(~ SIZE, nrow = 1) +
  scale_colour_manual(values = PALETTE) +
  scale_linetype_manual(values = c(Challengers = "solid", Incumbents = "dashed")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 75),
                     breaks = seq(0, 70, 10), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "First-round margin between leader and runner-up (points)",
       y = "Plans mentioning transparency",
       title = "Transparency communication by competitiveness and coalition size",
       subtitle = sprintf("Incumbent-contested races (%s candidates); panels by number of parties behind the candidacy; 95%% CIs clustered by municipality",
                          format(nrow(d), big.mark = ",")),
       caption = cap) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.title.position = "plot", plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, colour = "grey30"),
        legend.position = "bottom", legend.title = element_blank(),
        strip.background = element_rect(fill = "grey95"))

for (ext in c("png", "pdf"))
  ggsave(file.path(OUT_DIR, "figures", paste0("fig_margin_by_coalition.", ext)), p,
         width = 12, height = 4.5, dpi = 300, device = if (ext == "pdf") cairo_pdf else "png")
