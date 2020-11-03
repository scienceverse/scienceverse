#' Set/get global scienceverse options
#'
#' Global options are used to set default values.
#'
#' @param ... One of four: (1) nothing, then returns all options as a list; (2) a name of an option element, then returns its value; (3) a name-value pair which sets the corresponding option to the new value (and returns nothing), (4) a list with option-value pairs which sets all the corresponding arguments.
#'
#' @return a list of options, values of an option, or nothing
#' @export
#'
#' @examples
#'
#' scienceverse_options() # see all options
#'
#' scienceverse_options("quiet") # see value of scienceverse.quiet
#' \dontrun{
#' scienceverse_options(quiet = TRUE)
#' }
scienceverse_options <- function (...) {
  # code from afex::afex_options
  dots <- list(...)
  if (length(dots) == 0) {
    op <- options()
    scienceverse_op <- op[grepl("^scienceverse.", names(op))]
    names(scienceverse_op) <- sub("^scienceverse.", "", names(scienceverse_op))
    return(scienceverse_op)
  } else if (is.list(dots[[1]])) {
    newop <- dots[[1]]
    names(newop) <- paste0("scienceverse.", names(newop))
    options(newop)
  } else if (!is.null(names(dots))) {
    newop <- dots
    names(newop) <- paste0("scienceverse.", names(newop))
    options(newop)
  } else if (is.null(names(dots))) {
    if (length(dots) > 1)
      stop("scienceverse_options() can only return the value of a single option.",
           call. = FALSE)
    return(getOption(paste0("scienceverse.", unlist(dots))))
  } else {
    warning("Unsupported command to scienceverse_options(), nothing done.",
            call. = FALSE)
  }
}

#' @rdname scienceverse_options
#' @export
sv_opts <- scienceverse_options
