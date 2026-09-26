# Author: Cedric Antunes (FGV-CEPESP) ------------------------------------------
# Date: September, 2026 --------------------------------------------------------
# Script title: 04_plot_tests.R ------------------------------------------------
#
# Notes: Plots every test written by 03_final_estimation.R. 
# Filled points: Holm-adjusted p < .05 within the family used in script 03.
# ------------------------------------------------------------------------------

# Required packages ------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})

# ------------------------------------------------------------------------------
# Parameters -------------------------------------------------------------------
# ------------------------------------------------------------------------------
OUT_DIR <- Sys.getenv("MAYORAL_OUTPUT_DIR", "output")
FIG_DIR <- file.path(OUT_DIR, "figures")
UPLOAD  <- as.logical(Sys.getenv("MAYORAL_UPLOAD", "FALSE"))
DRIVE_OUTPUT_FOLDER_ID <- Sys.getenv("MAYORAL_DRIVE_OUTPUT_FOLDER_ID",
                                     "1IcDW6_Q9vezxq4zR06hJEGdAXYhb8jVd")

# Caption for the H1 figure (describes the incumbency-adjusted models in 03).
H1_NOTE <- paste0("Within-race models controlling for incumbency and log word count; races whose ",
                  "winner's plan is observed.\nObservational association, not an effect.")

# The main H1 figure shows the incumbency-adjusted models only. Set
# MAYORAL_H1_SHOW_PREVIOUS=TRUE to add the previous, unadjusted specification
# (a supplement version of the figure).
H1_SHOW_PREVIOUS <- as.logical(Sys.getenv("MAYORAL_H1_SHOW_PREVIOUS", "FALSE"))
H1_SPEC_ORDER <- c("Race FE + incumbency", "Race + party FE + incumbency",
                   "Race FE, no incumbency control (previous)")

dir.create(FIG_DIR, 
           recursive = TRUE, 
           showWarnings = FALSE)

# Same labels and order as script 03 (first label plots on top)
LABELS <- c(ANY = "Any transparency mention", PAST = "Retrospective claim",
            GEN = "General prospective promise", SPEC = "Specific prospective promise",
            STYLE = "Transparency as governing style")

# ------------------------------------------------------------------------------
# Helpers ----------------------------------------------------------------------
# ------------------------------------------------------------------------------
rd <- function(name) {
  path <- file.path(OUT_DIR, paste0(name, ".csv"))
  if (!file.exists(path)) stop("Missing 03 output: ", path, call. = FALSE)
  x <- read_csv(path, show_col_types = FALSE, progress = FALSE)
  x$outcome <- factor(LABELS[x$key], levels = rev(LABELS))
  if ("comparison" %in% names(x))  # keep 03's order, not alphabetical
    x$comparison <- factor(x$comparison, levels = unique(x$comparison))
  if ("p_holm" %in% names(x))
    x$holm <- factor(if_else(x$p_holm < .05, "Holm p < .05", "Holm p >= .05"),
                     levels = c("Holm p < .05", "Holm p >= .05"))
  x
}

omnibus <- function(name) {
  x <- trimws(readLines(file.path(OUT_DIR, name)))
  v <- function(tag) as.numeric(sub("^\\[1\\]\\s*", "", x[match(paste0("$", tag), x) + 1]))
  p <- v("p")
  sprintf("Omnibus Wald test of equal challenger effects across strategies: F(%d, %d) = %.2f, %s.",
          v("df1"), v("df2"), v("stat"), if (p < .001) "p < 0.001" else sprintf("p = %.3f", p))
}

# ------------------------------------------------------------------------------
# Shared look: every figure uses this theme, palette and set of sizes ----------
# ------------------------------------------------------------------------------
# Rule: one series is drawn in INK; several series take PALETTE colours in the
# order of their factor levels; shape always encodes Holm significance
# (filled = p < .05, hollow = p >= .05), never a grouping.
INK        <- "grey15"
REF_LINE   <- "grey60"
PALETTE    <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9")  # Okabe-Ito
POINT_SIZE <- 2.3    # every point estimate
CI95_WIDTH <- 0.5    # 95% intervals and lines
CI90_WIDTH <- 1.4    # 90% intervals (H1)
BAND_WIDTH <- 3      # descriptive bands (trimming bounds)
DODGE      <- 0.6    # spacing between series at the same row

scale_colour_project <- function(...) scale_colour_manual(values = PALETTE, ...)

theme_set(theme_bw(base_size = 11) +
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major.y = element_blank(),
                  plot.title.position = "plot", plot.caption.position = "plot",
                  plot.caption = element_text(hjust = 0, colour = "grey30"),
                  legend.position = "bottom", legend.title = element_blank(),
                  legend.box = "vertical",
                  strip.background = element_rect(fill = "grey95")))

# One coefficient plot for every estimate file. `group` dodges series by colour.
coef_plot <- function(dat, title, subtitle = NULL, caption = NULL, group = NULL,
                      xlab = "Estimate (percentage points)", y = "outcome", ci90 = FALSE) {
  pd <- position_dodge(width = if (is.null(group)) 0 else DODGE)
  if (is.null(group)) dat$.series <- "all" else dat$.series <- dat[[group]]
  p <- ggplot(dat, aes(estimate, .data[[y]], colour = .series, group = .series)) +
    geom_vline(xintercept = 0, colour = REF_LINE) +
    geom_linerange(aes(xmin = conf.low, xmax = conf.high), position = pd, linewidth = CI95_WIDTH) +
    (if (ci90) geom_linerange(aes(xmin = ci90_low, xmax = ci90_high), position = pd,
                              linewidth = CI90_WIDTH)) +
    labs(x = xlab, y = NULL, title = title, subtitle = subtitle, caption = caption)
  p <- if ("holm" %in% names(dat))
    p + geom_point(aes(shape = holm), position = pd, size = POINT_SIZE, fill = "white") +
    scale_shape_manual(values = c("Holm p < .05" = 16, "Holm p >= .05" = 21), drop = FALSE)
  else p + geom_point(position = pd, size = POINT_SIZE)
  p <- p + guides(shape = guide_legend(order = 2))
  if (is.null(group)) p + scale_colour_manual(values = INK, guide = "none") else
    p + scale_colour_project() + guides(colour = guide_legend(order = 1))
}

figs <- list()
save_fig <- function(p, name, w = 8, h = 4.5) {
  for (ext in c("png", "pdf"))
    ggsave(file.path(FIG_DIR, paste0(name, ".", ext)), p, width = w, height = h,
           dpi = 300, device = if (ext == "pdf") cairo_pdf else "png")
  figs[[name]] <<- p
}

# ------------------------------------------------------------------------------
# Prevalence with worst-case (Manski) bounds -----------------------------------
# ------------------------------------------------------------------------------
bounds <- rd("t_manski_prevalence_bounds")
save_fig(
  ggplot(bounds, aes(observed_prevalence_pp, outcome, colour = sample)) +
    geom_linerange(aes(xmin = lower_pp, xmax = upper_pp), linewidth = CI95_WIDTH,
                   position = position_dodge(width = DODGE)) +
    geom_point(size = POINT_SIZE, position = position_dodge(width = DODGE)) +
    scale_colour_project() +
    labs(x = "Prevalence among candidates (%)", y = NULL,
         title = "Prevalence of transparency communication",
         subtitle = "Point: observed prevalence among usable texts. Bar: worst-case bounds for unobserved plans",
         caption = "Descriptive. Bounds assign every missing plan to 0 (lower) or 1 (upper)."),
  "fig_prevalence_bounds", h = 4)

# ------------------------------------------------------------------------------
# H2: within-race challenger minus incumbent -----------------------------------
# ------------------------------------------------------------------------------
h2 <- rd("t_h2_main")
save_fig(coef_plot(h2, "H2. Challengers invoke transparency more than the incumbent they face",
                   sprintf("Race FE LPM with log word count; 95%% CIs clustered by municipality. N = %s candidates, %s races",
                           format(h2$n[1], big.mark = ","), format(h2$n_races[1], big.mark = ",")),
                   omnibus("t_h2_omnibus.txt"),
                   xlab = "Challenger minus incumbent (percentage points)"),
         "fig_h2_main", h = 4)

# ------------------------------------------------------------------------------
# H2 robustness and election-year estimates ------------------------------------
# "Unambiguous previous-cycle winner" is dropped: SAMPLE_H2 already imposes it,
# so it reproduces the main row exactly.
# ------------------------------------------------------------------------------
rob <- rd("t_h2_robustness") |>
  filter(spec != "Unambiguous previous-cycle winner") |>
  mutate(type = case_when(spec == "Main" ~ "Main",
                          grepl("^Year", spec) ~ "Election year",
                          TRUE ~ "Sensitivity") |>
           factor(levels = c("Main", "Sensitivity", "Election year")),
         spec = factor(spec, levels = rev(unique(spec))))
save_fig(coef_plot(rob, "H2 robustness and election-specific estimates", group = "type", y = "spec",
                   subtitle = "Challenger minus incumbent; 95% CIs clustered by municipality",
                   caption = "Holm adjustment within each five-outcome family.",
                   xlab = "Challenger minus incumbent (percentage points)") +
           facet_wrap(vars(factor(outcome, levels = LABELS)), nrow = 1, labeller = label_wrap_gen(18)),
         "fig_h2_robustness", w = 12, h = 5)

# ------------------------------------------------------------------------------
# H4: strategy choice among transparency users ---------------------------------
# ------------------------------------------------------------------------------
h4 <- bind_rows(rd("t_h4_users") |> mutate(hits = "Confident hits"),
                rd("t_h4_users_all_hits") |> mutate(hits = "All hits")) |>
  mutate(hits = factor(hits, levels = c("Confident hits", "All hits")))   # main series first
save_fig(coef_plot(h4, "H4. Strategy choice among transparency users", group = "hits",
                   subtitle = "Races where the incumbent and at least one challenger mention transparency",
                   caption = paste("Confident hits.", omnibus("t_h4_omnibus.txt")),
                   xlab = "Challenger minus incumbent (percentage points)"),
         "fig_h4_users", h = 4)

# ------------------------------------------------------------------------------
# H1: electoral association (95% thin, 90% thick; SESOI band if set) -----------
# ------------------------------------------------------------------------------
h1 <- rd("t_h1_electoral_association")
if (!all(h1$spec %in% H1_SPEC_ORDER))
  stop("Unexpected H1 specifications; rerun the revised 03_final_estimation.R")
if (!H1_SHOW_PREVIOUS) h1 <- h1 |> filter(!grepl("(previous)", spec, fixed = TRUE))
h1 <- h1 |> mutate(spec = factor(spec, levels = intersect(H1_SPEC_ORDER, unique(spec))))
sesoi <- unique(na.omit(h1$sesoi_pp))
p <- coef_plot(h1, "H1. Transparency communication and election", group = "spec",
               subtitle = "Within-race difference in win probability; thin 95% CI, thick 90% CI",
               caption = H1_NOTE, xlab = "Difference in probability of election (percentage points)",
               ci90 = TRUE)
if (length(sesoi))
  p <- p + geom_vline(xintercept = c(-1, 1) * sesoi[1], linetype = "dashed", colour = REF_LINE)
save_fig(p, paste0("fig_h1_electoral", if (H1_SHOW_PREVIOUS) "_with_previous" else ""), h = 4.5)

# ------------------------------------------------------------------------------
# H3: absolute margin slopes for leader and runner-up --------------------------
# ------------------------------------------------------------------------------
save_fig(coef_plot(rd("t_h3_absolute_margin_slopes"), group = "role",
                   "H3. Transparency communication and race closeness (absolute)",
                   "Top-two candidates; municipality and year FE; 95% CIs clustered by municipality",
                   xlab = "Change per 10 pp of first-round margin (percentage points)"),
         "fig_h3_absolute", h = 4)

# ------------------------------------------------------------------------------
# H3: standardized predictions (point estimates only, as in 03) ----------------
# ------------------------------------------------------------------------------
save_fig(
  ggplot(rd("t_h3_standardized_predictions"),
         aes(margin_pp, predicted_probability_pp, colour = role)) +
    geom_line(linewidth = CI95_WIDTH) + geom_point(size = POINT_SIZE) + scale_colour_project() +
    facet_wrap(vars(factor(outcome, levels = LABELS)), nrow = 1, scales = "free_y",
               labeller = label_wrap_gen(18)) +
    scale_x_continuous(breaks = c(5, 10, 20)) +
    labs(x = "First-round margin (pp)", y = "Predicted probability (%)",
         title = "H3. Standardized predictions by role and margin",
         caption = "Point predictions only; slope uncertainty excludes the absorbed fixed effects."),
  "fig_h3_predictions", w = 12, h = 4)

# ------------------------------------------------------------------------------
# H3: relative contrasts at 5, 10 and 20 pp ------------------------------------
# ------------------------------------------------------------------------------
rel <- rd("t_h3_relative_contrasts_5_10_20pp") |>
  mutate(margin = factor(paste(margin_pp, "pp margin"),
                         levels = paste(sort(unique(margin_pp)), "pp margin")))
save_fig(coef_plot(rel, "H3. Runner-up contrasts at selected margins", group = "margin",
                   subtitle = "Race FE; 95% CIs clustered by municipality",
                   caption = "Holm adjustment within comparison and margin.",
                   xlab = "Runner-up minus comparison group (percentage points)") +
           facet_wrap(~ comparison, ncol = 1, labeller = label_wrap_gen(60)),
         "fig_h3_relative", h = 9)

# ------------------------------------------------------------------------------
# H3: runner-up x margin interaction -------------------------------------------
# ------------------------------------------------------------------------------
save_fig(coef_plot(rd("t_h3_relative_margin_interactions"), group = "comparison",
                   "H3. Does the runner-up gap change with the margin?",
                   "Coefficient on runner-up x margin (per 10 pp); race FE",
                   xlab = "Interaction (percentage points per 10 pp of margin)") +
           guides(colour = guide_legend(ncol = 1, order = 1)),
         "fig_h3_interactions", h = 5)

# ------------------------------------------------------------------------------
# Trimming sensitivity (unadjusted, descriptive) -------------------------------
# ------------------------------------------------------------------------------
trim <- rd("t_trimming_sensitivity_unadjusted")
save_fig(
  ggplot(trim, aes(y = outcome)) +
    geom_vline(xintercept = 0, colour = REF_LINE) +
    geom_linerange(aes(xmin = lower_pp, xmax = upper_pp), linewidth = BAND_WIDTH,
                   colour = PALETTE[1], alpha = .35) +
    geom_point(data = h2, aes(x = estimate), size = POINT_SIZE, colour = INK) +
    labs(x = "Challenger minus incumbent (percentage points)", y = NULL,
         title = "Trimming sensitivity for differential plan availability",
         subtitle = sprintf("Band: bounds on the unadjusted difference (%.1f%% of the better-covered group trimmed). Point: H2 within-race estimate",
                            100 * max(trim$trimmed_fraction)),
         caption = "Descriptive sensitivity; not a causal bound and not a bound on the fixed-effects estimate."),
  "fig_trimming", h = 4)

message(length(figs), " figures written to ", normalizePath(FIG_DIR, winslash = "/"))

# ------------------------------------------------------------------------------
# Saving plots -----------------------------------------------------------------
# ------------------------------------------------------------------------------
if (UPLOAD) {
  googledrive::drive_auth(email = "cedricantunes07@gmail.com")
  folder <- googledrive::drive_get(googledrive::as_id(DRIVE_OUTPUT_FOLDER_ID))
  for (f in list.files(FIG_DIR, full.names = TRUE))
    googledrive::drive_put(f, path = folder, name = basename(f))
}
