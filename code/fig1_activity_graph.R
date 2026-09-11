# code/fig1_activity_graph.R
# ---------------------------------------------------------------------------
# Figure 1 - daily action-space trajectory for a single individual.
#
# The figure is an illustration, not an estimate: the trajectory is a stylized
# person-day, not a record from the TU survey. It was originally drawn by hand
# and shipped as a PNG with no generating script, which is why the manifest
# carried it as UNOWNED and why its axis text ended up at roughly 3 pt on the
# printed page. Reviewer 2 (R2 comment 2.6) asked for larger fonts.
#
# The vertex coordinates and label positions below were measured off the
# original high-resolution render (activity_graph.png, 19937 x 9114) so the
# rebuild traces the same path rather than an approximation of it.
#
# The backdrop is the author's own isometric city render, supplied from
# Pub_ActionSpace_TG/"space time graph.png". It is stretched to the panel and
# faded 40% toward white, which is the strength recovered from the submitted
# figure by masking its artwork and regressing the remaining pixels on the
# source. The fade is baked into data/assets/fig1_city_backdrop.png so the
# figure does not depend on a file in a sibling project.
#
# Output is sized so the figure sits at \textwidth in the manuscript with axis
# text near 8 pt and activity labels near 10 pt on the printed page.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
  library(png)
  library(grid)
})

source("code/utils_io.R")

fig_dir <- get_manuscript_fig_dir()
out_dir <- file.path("results", "baseline")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

BACKDROP <- file.path("data", "assets", "fig1_city_backdrop.png")

# ---- the stylized day -------------------------------------------------------
# Home until 07:00, commute out to 30 km, a midday errand, a second work block,
# an exercise stop at 10 km on the way home, and an evening visit at 5 km.
traj <- data.frame(
  hour = c(0, 7, 7.50, 8, 12, 12.25, 12.50, 17, 17.50, 18.45, 19, 20.30, 20.45, 22.45, 23, 24),
  km   = c(0, 0,   15, 30, 30,    33,    30, 30,    10,    10,  0,     0,     5,     5,  0,  0)
)

labels <- data.frame(
  hour  = c(3.46, 9.91, 12.53, 14.91, 18.69, 21.63),
  km    = c(1.68, 30.71, 33.71, 30.71, 11.20, 5.68),
  label = c("Home", "Work", "Errand", "Work", "Spinning", "Friend's")
)

# Vertices carrying a red dot in the original: every one except the two ends of
# the flat "Home" stretch that the axis line already marks.
dots <- traj

LINE_BLUE <- "#0000FF"
DOT_RED   <- "#FF0000"
FILL_BLUE <- "#BADDE8"

# ---- backdrop ---------------------------------------------------------------
# Stretched over the whole panel, as in the submitted figure, which is why the
# isometric geometry is wider than the square source.
backdrop <- rasterGrob(readPNG(BACKDROP), width = unit(1, "npc"),
                       height = unit(1, "npc"), interpolate = TRUE)

# Fill opacity 0.85: the submitted figure reads about #B4D8E4 inside the
# envelope over a backdrop near #B9C6C9, which is FILL_BLUE at roughly that
# alpha.
p <- ggplot(traj, aes(hour, km)) +
  annotation_custom(backdrop, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_area(fill = FILL_BLUE, alpha = 0.85) +
  geom_line(colour = LINE_BLUE, linewidth = 1.5) +
  geom_point(data = dots, colour = DOT_RED, size = 2.6) +
  geom_text(data = labels, aes(label = label), colour = LINE_BLUE,
            fontface = "bold", size = 4.4, vjust = -0.35) +
  scale_x_continuous(breaks = 0:24, expand = expansion(mult = c(0.005, 0.005))) +
  scale_y_continuous(breaks = seq(0, 30, 10), limits = c(0, 35.5),
                     expand = expansion(mult = c(0, 0.01))) +
  labs(title = "Space-time out-of-home action graph",
       x = "Time of Day (Hours)", y = "Distance from Home (km)") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey92", linewidth = 0.3),
    plot.title       = element_text(size = rel(1.15), hjust = 0),
    axis.text        = element_text(colour = "grey20"),
    axis.title       = element_text(colour = "grey20")
  )

for (path in c(file.path(fig_dir, "activity_graph_v2_small.png"),
               file.path(out_dir, "activity_graph_v2_small.png"))) {
  ggsave(path, p, width = 9, height = 4.3, dpi = 400)
  cat("wrote", path, "\n")
}
