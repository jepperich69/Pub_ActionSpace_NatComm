# code/utils_io.R

get_scenario_label <- function(scenario) {
  labels <- c(
    baseline     = "All individuals",
    sex1         = "Males",
    sex2         = "Females",
    city_10000   = "Cities ≥ 10,000",
    city_25000   = "Cities ≥ 25,000",
    city_50000   = "Cities ≥ 50,000",
    city_100000  = "Cities ≥ 100,000"
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
