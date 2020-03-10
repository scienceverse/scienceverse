#' Convert scivrs_author list to CRediT JATS format
#'
#' @param author list with class scivrs_author or list with class reg_study and at least one entry for authors
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
  if ("reg_study" %in% class(author)) {
    aa <- author$authors
  } else if ("scivrs_author" %in% class(author)) {
    aa <- list(author)
  }

  for (i in 1:length(aa)) {
    contrib_author <- '<contrib>%s
  <string-name>
    <surname>%s</surname>
    <given-names>%s</given-names>
  </string-name>
  %s
</contrib>'

    orcid <- check_orcid(aa[[i]]$orcid)
    if (orcid != FALSE) {
        orcid <- paste0('\n  <contrib-id authenticated="true" contrib-id-type="orcid">https://orcid.org/', orcid, '</contrib-id>')
    } else {
      orcid <- ''
    }

    contrib_role <- '<role content-type="https://dictionary.casrai.org/Contributor_Roles/%s" >%s</role>'
    underscore_roles <- gsub("\\W+", "_", aa[[i]]$roles)

    aa[i] <- sprintf(contrib_role, underscore_roles, aa[[i]]$roles) %>%
      paste(collapse = "\n  ") %>%
      sprintf(contrib_author,
              orcid,
              aa[[i]]$name[["surname"]],
              aa[[i]]$name[["given"]], .)
  }

  paste0("<contrib-group>\n", paste(aa, collapse = "\n"), "\n</contrib-group>")
}


