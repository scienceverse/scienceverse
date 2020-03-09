## set default options for scienceverse_options:
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.scienceverse <- list(
  )
  toset <- !(names(op.scienceverse) %in% names(op))
  if(any(toset)) options(op.scienceverse[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  paste(
    "\n************",
    "Welcome to scienceverse For support and examples visit:",
    "http://scienceverse.github.io/",
    "- Get and set global package options with: scienceverse_options()",
    "************",
    sep = "\n"
  ) %>% packageStartupMessage()
}
