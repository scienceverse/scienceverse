#' Save data in files
#'
#' @param study A study list object with class scivrs_study
#' @param dir The directory in which to save to data
#' @param format The data format to save with (defaults to tsv)
#' @param ... Further arguments to pass to write.table or write.csv
#'
#' @return the study object
#' @export
#'
make_data <- function(study, dir = "data", format = c("tsv", "csv"), ...) {
  if (dir == "") dir <- "./"
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  format <- match.arg(format)

  for (d in study$data) {
    filename <- sprintf("%s/%s_data.%s", dir, d$id, format)

    # write data
    if (format == "tsv") {
      write.table(d$data, filename, sep = "\t", row.names = FALSE, ...)
    } else if (format == "csv") {
      write.csv(d$data, filename, row.names = FALSE, ...)
    }

    # write codebooks
    if ("codebook" %in% names(d)) {
      cbname <- sprintf("%s/%s_data.json", dir, d$id)
      d$codebook %>%
        jsonlite::toJSON(auto_unbox = TRUE) %>%
        jsonlite::prettify(4) %>%
        write(cbname)
    }
  }

  invisible(study)
}
