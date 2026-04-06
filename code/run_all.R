# code/run_all.R
scenarios <- c(
  "baseline",
  "sex1",
  "sex2",
  "city_10000",
  "city_25000",
  "city_50000",
  "city_100000"
)

for (s in scenarios) {
  cat("\n====================\nRunning:", s, "\n====================\n")
  
  status <- system2(
    "Rscript",
    c("code/run_one_scenario.R", s),
    stdout = "",
    stderr = ""
  )
  
  if (status != 0) {
    stop("Scenario failed: ", s)
  }
}

cat("\nALL SCENARIOS COMPLETED SUCCESSFULLY\n")
