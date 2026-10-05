## ----setup, include=FALSE-----------------------------------------------------
### run with: 
#DBSCAN_RUN_PYTHON_VIGNETTE=true Rscript -e 'rmarkdown::render("vignettes/python.Rmd")'

run_python <- identical(
  tolower(Sys.getenv("DBSCAN_RUN_PYTHON_VIGNETTE")),
  "true"
)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

if (run_python) {
  Sys.setenv(RETICULATE_PYTHON = "managed")
  reticulate::py_require(c("numpy", "pandas", "rpy2"))
}

