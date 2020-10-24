## Functions ----

# https://github.com/daattali/advanced-shiny/tree/master/reactive-trigger
# instantiate a reactive trigger with myTrigger <- makeReactiveTrigger()
# call myTrigger$depend() in any reactive code that should re-run when the trigger is fired
# call myTrigger$trigger() to set off the trigger
makeReactiveTrigger <- function() {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}

get_credit_roles <- function() {
  x <- utils::capture.output(credit_roles())
  x %>%
    gsub("^\\[\\S+\\] ", "* **", .) %>%
    gsub(": ", "**: ", .) %>%
    paste(collapse = "\n") %>%
    markdown::renderMarkdown(text = .) %>%
    HTML()
}

make_aut_list <- function(authors) {
  if (length(authors) == 0) return(data.frame())
  tbl <- mapply(function(a, i) {
    olink <-  paste0(" <a href='https://orcid.org/",
                     a$orcid, "' target='_blank'>",
                     a$orcid, "</a>")

    order_input <- sprintf("<select class='aut_order'>", i)
    for (j in 1:length(authors)) {
      order_input <- sprintf(
        "%s\n<option value='%d'%s>%d</option>",
        order_input, j,
        ifelse(j == i, " selected", ""), j)
    }
    order_input <- sprintf("%s\n</select>", order_input)

    list(
      order = order_input,
      surname = a$surname,
      given = a$given,
      orcid = ifelse(!isFALSE(a$orcid), olink, ""),
      roles = paste(a$roles, collapse = ", ")
    )
  }, authors, seq_along(authors))

  t(tbl) %>% as.data.frame()
}

make_hyp_list <- function(h) {
  tbl <- sapply(h, function(x) {
    crit <- sapply(x$criteria, `[[`, "id") %>%
      paste0(collapse = ", ")

    list(
      id = x$id,
      criteria = crit,
      corroboration = if_nowt(x$corroboration$evaluation),
      falsification = if_nowt(x$falsification$evaluation)
    )
  })

  if (length(tbl) == 0) return(data.frame())

  t(tbl) %>% as.data.frame()
}

make_dat_list <- function(d) {
  if (length(d) == 0) return(data.frame())

  tbl <- sapply(d, function(x) {
    cols <- ""
    if (!is.null(x$codebook$variableMeasured)) {
      cols <- x$codebook$variableMeasured %>%
        sapply(`[[`, "name") %>%
        paste(collapse = ", ")
    } else if (is.data.frame(x$data)) {
      cols <- names(x$data) %>%
        paste(collapse = ", ")
    }
    list(
      id = x$id,
      columns = cols
    )
  })

  t(tbl) %>% as.data.frame()
}

make_ana_list <- function(s) {
  if (length(s$analyses) == 0) return(data.frame())

  tbl <- sapply(s$analyses, function(x) {
    code <- output_custom_code(s, x$id) %>%
      strsplit("\n") %>%`[[`(1)
    if (length(code) > 4) code <- c(code[1:3], "...")
    code <- paste(code, collapse = "\n") %>%
      paste0("<pre><code>", ., "</code></pre>")

    res <- nested_list(x$results) %>%
      markdown::renderMarkdown(text = .) %>%
      HTML()

    list(
      id = x$id,
      code = code
    )
  })

  t(tbl) %>% as.data.frame()
}

make_section_list <- function(study, section, extra = NULL) {
  sec <- study[[section]]
  if (length(sec) == 0) return("")

  mapply(function(x, i) {
    ex <- ifelse(is.function(extra), extra(x), "")
    sprintf("1. [<a class='section_edit' section='%s' section_idx='%d'>edit</a>] [<a class='section_delete' section='%s' section_idx='%d'>delete</a>] %s %s\n\n",
            section, i, section, i, x$id, ex)
  }, sec, 1:length(sec)) %>%
    paste0(collapse = "\n") %>%
    markdown::renderMarkdown(text = .) %>%
    HTML()
}

make_crit_list <- function(criteria) {
  crit <- do.call(dplyr::bind_rows, criteria)
  if (is.null(crit) | nrow(crit) == 0) return(data.frame())

  crit$conclusion <- NULL

  crit$delete <- sprintf("<button class='delete' idx='%d'><i class='fa fa-times'></i></button>",
                         1:nrow(crit))
  as.data.frame(crit)
}


design_summary <- function(des) {
  w <- names(des$within)
  b <- names(des$between)
  p <- des$params

  # fix for problem with 1-factor params table in faux
  if (names(p)[1] == ".") names(p)[1] <- c(w, b)[1]
  vars <- length(w) + length(b)
  if (vars > 1) {
    # add [w] or [b] to var names
    n <- names(p)[1:vars]
    n[n %in% w] <- paste(n[n %in% w], "[w]")
    n[n %in% b] <- paste(n[n %in% b], "[b]")
    names(p)[1:vars] <- n
  }
  if (length(w) > 0) {
    # add [r] to correlation columns
    a <- vars+1
    b <- ncol(p) - 3
    n <- names(p)[a:b]
    n <- paste(n, "[r]")
    names(p)[a:b] <- n
  }
  p
}

debug_msg <- function(txt) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  if (is_local) message(txt)
}

