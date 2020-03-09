#' Convert scivrs_author list to CRediT JATS format
#'
#' @param author list with class scivrs_author
#'
#' @return string in JATS format (see https://jats4r.org/credit-taxonomy)
#' @export
#'
#' @examples
#' ld <- author(
#'   orcid = "0000-0002-7523-5539",
#'   surname = "DeBruine", given = "Lisa M.",
#'   roles = list("Conceptualization", "Methodology")
#' )
#' author_jats(ld)
author_jats <- function(author) {
  contrib_author <- '<contrib contrib-type="author">
  <name>
    <surname>%s</surname>
    <given-names>%s</given-names>
  </name>
  %s
</contrib>'

  contrib_role <- '<role vocab="CRediT" vocab-identifier=" http://credit.casrai.org/">%s</role>'

  sprintf(contrib_role, author$roles) %>%
    paste(collapse = "\n  ") %>%
    sprintf(contrib_author, author$name[["surname"]], author$name[["given"]], .)
}
