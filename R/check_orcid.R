#' Check validity of ORCiD
#'
#' @param orcid a 16-character ORCiD in bare or URL format
#'
#' @return a formatted 16-character ORCiD or FALSE
#' @export
#'
#' @examples
#' check_orcid("0000-0002-7523-5539")
#' check_orcid("0000-0002-0247-239X")
#' check_orcid("https://orcid.org/0000-0002-0247-239X")
#' check_orcid("0000-0002-0247-2394") # incorrect, return FALSE
check_orcid <- function(orcid) {
  baseDigits <- gsub("[^0-9X]", "", orcid)

  if (nchar(baseDigits) != 16) {
    if (scienceverse_options("verbose")) {
      warning("The ORCiD ", orcid, " is not valid.")
    }
    return(FALSE)
  }

  total <- 0
  for (i in 1:(nchar(baseDigits)-1)) {
    digit <- substr(baseDigits, i, i) %>% as.integer()
    total <- (total + digit) * 2
  }
  remainder <- total %% 11;
  result <- (12 - remainder) %% 11;
  result <- ifelse(result == 10, "X", result)

  if (result == substr(baseDigits, 16, 16)) {
    paste(substr(baseDigits, 1, 4),
          substr(baseDigits, 5, 8),
          substr(baseDigits, 9, 12),
          substr(baseDigits, 13, 16),
          sep = "-")
  } else {
    if (scienceverse_options("verbose")) {
      warning("The ORCiD ", orcid, " is not valid.")
    }
    return(FALSE)
  }
}



#' Get ORCiD from Name
#'
#' @param family The family (last) name to search for
#' @param given An optional given (first) name to search for. Initials will be converted from, e.g., L M to L\* M\*
#'
#' @return A vector of matching ORCiDs
#' @export
#'
#' @examples
#' get_orcid("DeBruine", "Lisa")
#'
get_orcid <- function(family, given = "*") {
  if (is.null(family) || trimws(family) == "") {
    stop("You must include a family name")
  }

  if (is.null(given) || trimws(given) == "") {
    given <- "*"
  }

  query <- "https://pub.orcid.org/v3.0/search/?q=family-name:%s+AND+given-names:%s"

  given2 <- given %>%
    trimws() %>%
    gsub("^(\\w)\\.?$", "\\1\\*", .) %>% # single initial
    gsub("^(.)\\.?\\s", "\\1\\* ", .) %>% # initial initial
    gsub("\\s(.)\\.?$", " \\1\\*", .) %>% # ending initial
    gsub("\\s(.)\\.?\\s", " \\1\\* ", .) %>% # internal initial
    utils::URLencode()

  family2 <- trimws(family) %>% utils::URLencode()
  url <- sprintf(query, family2, given2) %>% url("rb")
  on.exit(close(url))

  xml <- tryCatch(xml2::read_xml(url), error = function(e) {
    warning("You might not have an internet connection")
    return(list())
  })
  l <- xml2::as_list(xml)

  if (scienceverse_options("verbose")) {
    n <- length(l$search)
    if (n == 0) {
      message("No ORCID found for ", given, " ", family)
    } else if (n > 1) {
      message("Multiple (", n, ") ORCIDs found for ", given, " ", family)
    }
  }

  sapply(l$search, function(res) {
    res$`orcid-identifier`$path
  }) %>% unlist() %>% unname()
}
