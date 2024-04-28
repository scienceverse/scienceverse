#' Get study from XML file
#'
#' Currently only works for XML created by grobid
#'
#' @param filename the path to the XML file
#' @param xml_type the type of xml file to parse
#'
#' @return A study object with class scivrs_study
#' @export
study_from_xml <- function(filename, xml_type = c("auto", "grobid")) {
  xml_type <- match.arg(xml_type)

  # handle list of files or a directory
  if (length(filename) > 1) {
    s <- lapply(filename, study_from_xml, xml_type = xml_type)
    return(s)
  } else if (file.info(filename)$isdir) {
    xmls <- list.files(filename, "\\.xml", full.names = TRUE)
    if (length(xmls) == 0) {
      stop("There are no xml files in the directory ", filename)
    }
    s <- study_from_xml(xmls)
    return(s)
  }

  if (!file.exists(filename)) {
    stop("The file ", filename, " does not exist.")
  }

  xml <- tryCatch(xml2::read_xml(filename), error = function(e) {
    stop("The file ", filename, " could not be read as XML")
  })

  # deal with XML types (TODO: support more than grobid?)
  xml_type_guess <- dplyr::case_when(
    xml2::xml_name(xml) == "TEI" ~ "grobid",
    .default = "unknown"
  )

  if (xml_type == "auto") xml_type <- xml_type_guess

  if (xml_type == "grobid") {
    if (xml2::xml_name(xml) != "TEI") {
      stop("This XML file does not parse as a valid Grobid TEI.")
    }

    xlist <- xml2::as_list(xml)
    s <- study()

    s$info$title <- xlist$TEI$teiHeader$fileDesc$titleStmt$title[[1]]
    s$name <- s$info$title

    # abstract
    s$info$abstract <- xlist$TEI$teiHeader$profileDesc$abstract |>
      unlist() |>
      paste(collapse = " ") |>
      trimws()

    # get authors
    ana <- xlist$TEI$teiHeader$fileDesc$sourceDesc$biblStruct$analytic
    authors <- ana[names(ana) == "author"]

    for (a in authors) {
      family <- a$persName$surname[[1]]
      given <- a$persName$forename[[1]]
      orcid <- NULL
      #orcid <- get_orcid(family, given)

      s <- add_author(s, family, given, orcid)
    }

    # get body text

    # function for removing empty lines
    remove_empty <- function(list) {
      empty <- grepl("^\\s*$", list)
      list[!empty]
    }

    body <- xlist$TEI$text$body
    body_divs <- body[names(body) == "div"]
    headers <- sapply(body_divs, function(b) b$head[[1]]) |> unname()

    # add spacers ?
    bflat <- unlist(body_divs)
    refs <- grep("ref", names(bflat))
    bflat[refs] <- ""
    s$full_text <- paste(bflat, collapse = " ")
  } else {
    stop("This function cannot yet handle an XML of type ", xml_type)
  }

  return(s)
}
