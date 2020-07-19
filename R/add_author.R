#' Add an Author
#'
#' @param study A study list object with class scivrs_study
#' @param orcid the author's unique ORCiD (see https://orcid.org/)
#' @param surname a character string with the author's last name(s)
#' @param given a character string with the author's given name(s)
#' @param roles a vector of roles from the CRediT taxonomy (see https://casrai.org/credit/); use credit_roles() to view the full list
#' @param ... further info to add to author object
#'
#' @return A study object with class scivrs_study
#' @export
#'
#' @examples
#' s <- study() %>% add_author(
#'   orcid = "0000-0002-7523-5539",
#'   surname = "DeBruine", given = "Lisa M.",
#'   roles = c("Conceptualization", "Methodology")
#' )
#' study_to_json(s)
#'
add_author <- function(study, orcid, surname, given = "",
                       roles = c(), ...) {

  idx <- get_idx(study, section = "authors")

  study$authors[[idx]] <- author(orcid, surname, given, roles, ...)

  invisible(study)
}

#' Author in CRediT Format
#'
#' @param orcid the author's unique ORCiD (see https://orcid.org/)
#' @param surname a character string with the author's last name(s)
#' @param given a character string with the author's given name(s)
#' @param roles a vector of roles from the CRediT taxonomy (see https://casrai.org/credit/); use credit_roles() to view the full list
#' @param ... further info to add to author object
#'
#' @return a list with class scivrs_author
#' @export
#'
#' @examples
#' author(
#'   orcid = "0000-0002-7523-5539",
#'   surname = "DeBruine", given = "Lisa M.",
#'   roles = c("Conceptualization", "Methodology")
#' )
author <- function(orcid, surname, given = "", roles = c(), ...) {
  role_names <- credit_roles("name")

  # check roles are in list
  if (is.numeric(roles)) {
    roles <- role_names[roles]
  }

  chk_roles <- names(roles)

  if (is.null(chk_roles)) { chk_roles <- roles }
  bad_roles <- chk_roles[!(chk_roles %in% role_names)]
  if (length(bad_roles)) {
    # check for abbreviations
    bad_roles <- bad_roles[!(bad_roles %in% credit_roles("abbr"))]
    if (length(bad_roles)) {
      stop("These roles do not exist in the CRediT taxonomy: ",
           paste(bad_roles, collapse = ", "), "\n  See http://credit.casrai.org/")
    }
    # convert to correct names
    chk_roles <- role_names[credit_roles("abbr") %in% chk_roles]
  }

  orcid <- check_orcid(orcid)
  a <- list(
    orcid = orcid,
    name = list(surname = surname, given = given),
    roles = chk_roles
  )
  a <- c(a, list(...))

  class(a) <- c("scivrs_author", "list")

  a
}



