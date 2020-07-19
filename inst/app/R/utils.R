## Functions ----

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

make_author_list <- function(authors) {
  i <- 0
  alist <- list()

  for (a in authors) {
    i <- i + 1
    edit <- actionLink(paste0("author_edit_", i), "edit",
                       class = "author_edit", data = i)
    del <- actionLink(paste0("author_del_", i), "delete",
                      class = "author_delete", data = i)

    order_input <- sprintf("<select class='author_order'>", i)
    for (j in 1:length(authors)) {
      order_input <- sprintf("%s\n<option value='%d'%s>%d</option>",
                             order_input, j,
                             ifelse(j == i, " selected", ""), j)
    }
    order_input <- sprintf("%s\n</select>", order_input)

    olink <-  paste0(" <a href='https://orcid.org/",
                     a$orcid, "' target='_blank'>",
                     a$orcid, "</a>")
    roles <- paste(":", paste(a$roles, collapse = ", "))
    txt <- sprintf("<li>%s [%s] [%s] %s, %s%s%s </li>",
                   order_input, edit, del, a$surname, a$given,
                   ifelse(!isFALSE(a$orcid), olink, ""),
                   ifelse(length(a$roles)>0, roles, ""))

    alist[[i]] <- HTML(txt)
  }

  do.call(tags$ul, alist)
}

