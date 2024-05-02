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

  # handle list of files or a directory----
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

  # deal with XML types----
  # TODO: support more than grobid?
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

    # abstract ----
    s$info$abstract <- xlist$TEI$teiHeader$profileDesc$abstract |>
      unlist() |>
      paste(collapse = " ") |>
      trimws()

    # get authors ----
    ana <- xlist$TEI$teiHeader$fileDesc$sourceDesc$biblStruct$analytic
    authors <- ana[names(ana) == "author"]

    for (a in authors) {
      family <- a$persName$surname[[1]]
      given <- a$persName$forename[[1]]
      email <- a$email[[1]]
      orcid <- a$idno[[1]]
      if (is.null(orcid)) {
        orcid_lookup <- get_orcid(family, given)
        if (length(orcid_lookup) == 1) orcid <- orcid_lookup
      }

      s <- add_author(s, family, given, orcid, email = email)
    }

    # process text----
    body <- xlist$TEI$text$body
    ft <- full_text_table_from_grobid(body)
    s$full_text <- ft
  } else {
    stop("This function cannot yet handle an XML of type ", xml_type)
  }

  return(s)
}


#' Get full text table from grobid
#'
#' @param body body section of grobid xml as list (e.g., `xml2::as_list(xml)$TEI$text$body`)
#'
#' @return a data frame of the classified full text
#' @keywords internal
#'
full_text_table_from_grobid <- function(body) {
  empty <- grepl("^\\s*$", body)
  body <- body[!empty]

  # add indices to body
  add_name_index <- function(x) {
    if (length(x) == 0 || is.null(names(x))) return(x)

    names(x) <- paste0(names(x), "_", seq_along(x))
    lapply(x, \(x2) {
      if (is.list(x2)) {
        add_name_index(x2)
      } else {
        x2
      }
    })
  }

  b <- add_name_index(body)

  # put text into a data frame
  bflat <- unlist(b)
  ft <- data.frame(
    tag = names(bflat),
    text = unname(bflat)
  )

  # classify elements
  ft$type <- regexpr("[a-z]+_(\\.|_|\\d)*$", ft$tag) |>
    regmatches(ft$tag, m = _) |>
    gsub("[^a-z]", "", x = _)

  ft$section <- regexpr("^[^\\.]+_\\d+", ft$tag) |>
    regmatches(ft$tag, m = _)

  add_tag <- function(df, tag) {
    pattern <- paste0("(^|\\.)", tag, "_\\d+")
    rows <- grepl(pattern, df$tag)
    m <- regexpr(pattern, df$tag)
    df[rows, tag] <- regmatches(df$tag, m = m)
    vals <- gsub(".+_", "", df[, tag])

    return(vals)
  }

  ft$div <- add_tag(ft, "div")
  ft$p <- add_tag(ft, "p")
  ft$s <- add_tag(ft, "s")

  # make table of sections and classify
  headers <- ft[grepl("head", ft$tag), c("section", "text")]
  missing_sections <- data.frame(
    section = unique(ft$section) %>% setdiff(headers$section),
    text = ""
  )
  sections <- rbind(headers, missing_sections)

  intro <- grepl("intro", sections$text, ignore.case = TRUE)
  method <- grepl("method", sections$text, ignore.case = TRUE)
  results <- grepl("result", sections$text, ignore.case = TRUE)
  discussion <- grepl("discuss", sections$textaders, ignore.case = TRUE)
  sections$section_class[intro] <- "intro"
  sections$section_class[method] <- "method"
  sections$section_class[results] <- "results"
  sections$section_class[discussion] <- "discussion"

  # assume sections are the same class as previous if unclassified
  for (i in seq_along(sections$section_class)) {
    if (is.na(sections$section_class[i]))
      sections$section_class[i] <- sections$section_class[i-1]
  }

  # add last to override non-div sections
  sec_labels <- gsub("_\\d+$", "", sections$section)
  sections$section_class[sec_labels != "div"] <- sec_labels[sec_labels != "div"]

  # add sections to full text
  ft <- dplyr::left_join(ft, sections[, c("section", "section_class")], by = c("section"))

  return(ft)
}
