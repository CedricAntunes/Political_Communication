# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: September, 2026 --------------------------------------------------------
# Script title: 07_transparency_by_margin.R ------------------------------------
#
# Notes: Share of challengers and incumbents whose plan mentions transparency,
# by first-round margin band, in incumbent-contested races. Also a one-number
# summary: change per 10 points of margin, controlling for plan length and
# year (and, in a second version, municipality). 
# ------------------------------------------------------------------------------

# Required packages ------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(fixest)
  library(ggplot2)
})
setFixest_notes(FALSE)

# Parameters -------------------------------------------------------------------
OUT_DIR    <- Sys.getenv("MAYORAL_OUTPUT_DIR", "output")
FRAME_PATH <- Sys.getenv("MAYORAL_FRAME_PATH",
                         "C:/Users/cedric.antunes/Downloads/votes_municipality/analysis_frame.rds")
OUTCOME    <- "TRANSPARENCY_ANY_MENTIONED_CONFIDENT"
BREAKS     <- c(0, 5, 10, 15, 20, 30, Inf)
BANDS      <- c("0-5", "5-10", "10-15", "15-20", "20-30", "30+")
PALETTE    <- c(Challengers = "#0072B2", Incumbents = "#D55E00")   # as in script 04

dir.create(file.path(OUT_DIR, "figures"), recursive = TRUE, showWarnings = FALSE)

# Sample: incumbent-contested races with a unique top two ----------------------
d <- readRDS(FRAME_PATH) |>
  filter(TEXT_OBSERVED == 1, INCUMBENT_CONTESTED_TRUE == 1, !is.na(INCUMBENT_TRUE),
         VALID_RANKING_TRUE == 1, is.finite(TRUE_MARGIN_PP), is.finite(LOG_N_WORDS),
         !is.na(.data[[OUTCOME]])) |>
  mutate(Y      = 100 * .data[[OUTCOME]],
         BAND   = cut(TRUE_MARGIN_PP, BREAKS, labels = BANDS, include.lowest = TRUE),
         STATUS = factor(if_else(INCUMBENT_TRUE == 1, "Incumbents", "Challengers"),
                         levels = names(PALETTE)))

# Share by band, with municipality-clustered 95% CIs ---------------------------
bands <- d |> group_by(STATUS) |> group_modify(function(z, key) {
  m <- feols(Y ~ 0 + BAND, data = z, cluster = ~ MUNI)
  ci <- confint(m)
  tibble(band = factor(sub("^BAND", "", names(coef(m))), levels = BANDS),
         share = unname(coef(m)), conf.low = ci[, 1], conf.high = ci[, 2],
         candidates = as.vector(table(z$BAND))[match(sub("^BAND", "", names(coef(m))), BANDS)])
}) |> ungroup()
write_csv(bands, file.path(OUT_DIR, "t_transparency_by_margin_bands.csv"))

# Change per 10 points of margin (plan length controlled) ----------------------
slope <- function(z, fe) {
  m <- feols(as.formula(paste("Y ~ TRUE_MARGIN_10PP + LOG_N_WORDS |", fe)),
             data = z, cluster = ~ MUNI)
  ci <- confint(m)["TRUE_MARGIN_10PP", ]
  tibble(fixed_effects = fe, per_10pp = coef(m)[["TRUE_MARGIN_10PP"]],
         conf.low = ci[[1]], conf.high = ci[[2]], n = nobs(m))
}
slopes <- d |> group_by(STATUS) |>
  group_modify(~ bind_rows(slope(.x, "YEAR"), slope(.x, "YEAR + MUNI"))) |> ungroup()
write_csv(slopes, file.path(OUT_DIR, "t_transparency_margin_slopes.csv"))
print(bands); print(slopes)

# Figure -----------------------------------------------------------------------
s <- slopes |> filter(fixed_effects == "YEAR")
fmt <- function(g) with(s[s$STATUS == g, ], sprintf("%s %.2f [%.2f, %.2f]", tolower(g),
                                                    per_10pp, conf.low, conf.high))
pd <- position_dodge(width = 0.3)
p <- ggplot(bands, aes(band, share, colour = STATUS, group = STATUS)) +
  geom_line(aes(linetype = STATUS), linewidth = 0.5, position = pd) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), linewidth = 0.5, position = pd) +
  geom_point(size = 2.3, position = pd) +
  scale_colour_manual(values = PALETTE) +
  scale_linetype_manual(values = c(Challengers = "solid", Incumbents = "dashed")) +
  # Axis from zero: the claim is flatness, and a truncated axis exaggerates wiggles.
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 60),
                     breaks = seq(0, 60, 10), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "First-round margin between leader and runner-up (points)",
       y = "Plans mentioning transparency",
       title = "Transparency communication by electoral competitiveness",
       subtitle = sprintf("Incumbent-contested races (%s candidates, %s races); 95%% CIs clustered by municipality",
                          format(nrow(d), big.mark = ","), format(n_distinct(d$RACE_ID), big.mark = ",")),
       caption = paste0("Change per 10 points of margin, controlling for plan length and year: ",
                        fmt("Challengers"), "; ", fmt("Incumbents"), ".\n",
                        "Descriptive comparison across races; margins are observed after the election.")) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.title.position = "plot", plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, colour = "grey30"),
        legend.position = "bottom", legend.title = element_blank())

for (ext in c("png", "pdf"))
  ggsave(file.path(OUT_DIR, "figures", paste0("fig_transparency_by_margin.", ext)), p,
         width = 8, height = 4.5, dpi = 300, device = if (ext == "pdf") cairo_pdf else "png")
