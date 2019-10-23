#' CRediT Roles
#'
#' @param display Whether to display the category names, explanations, or abbreviations
#'
#' @return list of roles
#' @export
#'
#' @examples
#' credit_roles()
credit_roles <- function(display = c("names", "explain", "abbr")) {
  roles <- list(
    "Conceptualization" = "Ideas; formulation or evolution of overarching research goals and aims.",
    "Data curation" = "Management activities to annotate (produce metadata), scrub data and maintain research data (including software code, where it is necessary for interpreting the data itself) for initial use and later re-use.",
    "Formal analysis" = "Application of statistical, mathematical, computational, or other formal techniques to analyse or synthesize study data.",
    "Funding acquisition" = "Acquisition of the financial support for the project leading to this publication.",
    "Investigation" = "Conducting a research and investigation process, specifically performing the experiments, or data/evidence collection.",
    "Methodology" = "Development or design of methodology; creation of models.",
    "Project administration" = "Management and coordination responsibility for the research activity planning and execution.",
    "Resources" = "Provision of study materials, reagents, materials, patients, laboratory samples, animals, instrumentation, computing resources, or other analysis tools.",
    "Software" = "Programming, software development; designing computer programs; implementation of the computer code and supporting algorithms; testing of existing code components.",
    "Supervision" = "Oversight and leadership responsibility for the research activity planning and execution, including mentorship external to the core team.",
    "Validation" = "Verification, whether as a part of the activity or separate, of the overall replication/reproducibility of results/experiments and other research outputs.",
    "Visualization" = "Preparation, creation and/or presentation of the published work, specifically visualization/data presentation.",
    "Writing – original draft" = "Preparation, creation and/or presentation of the published work, specifically writing the initial draft (including substantive translation).",
    "Writing – review & editing" = "Preparation, creation and/or presentation of the published work by those from the original research group, specifically critical review, commentary or revision – including pre- or post-publication stages."
  )

  abbr <- c(
    "con",
    "dat",
    "ana",
    "fun",
    "inv",
    "met",
    "adm",
    "res",
    "sof",
    "sup",
    "val",
    "vis",
    "dra",
    "edi"
  )

  if ("explain" == display[1]) {
    for (i in 1:length(roles)) {
      cname <- names(roles)[i]
      cdesc <- roles[[i]]
      paste0("\033[40m\033[37m[", i, "/", abbr[i], "]\033[39m\033[49m \033[1m", cname, "\033[22m: ", cdesc, "\n") %>%
        cat()
    }
  } else if ("abbr" == display[1]) {
    abbr
  } else {
    names(roles)
  }
}



#' Create Author
#'
#' @param name a named vector c(surname = "LastName", give = "First Names")
#' @param roles a vector of roles (e.g., c("Conceptualization", "Methodology")
#' @param ... further info to add to author object
#'
#' @return a list with class scivrs_author
#' @export
#'
#' @examples
#' author(name = list(surname = "DeBruine", given = "Lisa M."),
#'        roles = list("Conceptualization", "Methodology"))
author <- function(name = c(), roles = c(), ...) {
  # check roles are in list
  if (is.numeric(roles)) {
    roles <- credit_roles()[roles]
  }

  chk_roles <- names(roles)

  if (is.null(chk_roles)) { chk_roles <- roles }
  bad_roles <- chk_roles[!(chk_roles %in% credit_roles())]
  if (length(bad_roles)) {
    # check for abbreviations
    bad_roles <- bad_roles[!(bad_roles %in% credit_roles("abbr"))]
    if (length(bad_roles)) {
      stop("These roles do not exist in the CRediT taxonomy: ",
           paste(bad_roles, collapse = ", "), "\n  See http://credit.casrai.org/")
    }
    # convert to correct names
    chk_roles <- credit_roles()[credit_roles("abbr") %in% chk_roles]
  }

  a <- list(
    name = name,
    roles = chk_roles
  )

  class(a) <- c("scivrs_author", "list")

  a
}


#' Convert scivrs_author list to CRediT JATS format
#'
#' @param author list with class scivrs_author
#'
#' @return string in JATS format
#' @export
#'
#' @examples
#' ld <- author(
#'   name = list(surname = "DeBruine", given = "Lisa M."),
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
