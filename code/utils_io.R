# code/utils_io.R

get_scenario_label <- function(scenario) {
  labels <- c(
    baseline     = "All individuals",
    sex1         = "Males",
    sex2         = "Females",
    city_10000   = "Cities ≥ 10,000",
    city_25000   = "Cities ≥ 25,000",
    city_50000   = "Cities ≥ 50,000",
    city_100000  = "Cities ≥ 100,000",
    bw3          = "Bandwidth H_D = 3 km",
    bw7          = "Bandwidth H_D = 7 km"
  )
  
  if (!scenario %in% names(labels)) {
    warning("Unknown scenario: ", scenario, " — using raw name.")
    return(scenario)
  }
  
  labels[[scenario]]
}


get_dirs <- function(scenario) {
  in_dir  <- file.path("data", "kernels", scenario)
  out_dir <- file.path("results", scenario)
  if (!dir.exists(in_dir)) stop("Scenario input folder not found: ", in_dir)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  list(in_dir = in_dir, out_dir = out_dir)
}


# Where scripts write the figure files the manuscript includes.
#
# Resolution order:
#   1. ACTIONSPACE_FIG_DIR, if set
#   2. ../Overleaf_source/figures, when this checkout sits beside the
#      manuscript source (the author's working copy)
#   3. results/figures_manuscript, otherwise
#
# Step 3 is what a fresh clone gets. Scripts previously hardcoded an absolute
# path into the author's OneDrive, so every manuscript figure failed to export
# on any other machine.
get_manuscript_fig_dir <- function() {
  fig_dir <- Sys.getenv("ACTIONSPACE_FIG_DIR", unset = "")

  if (!nzchar(fig_dir)) {
    overleaf <- file.path("..", "Overleaf_source", "figures")
    fig_dir  <- if (dir.exists(overleaf)) {
      overleaf
    } else {
      file.path("results", "figures_manuscript")
    }
  }

  if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
  fig_dir
}
