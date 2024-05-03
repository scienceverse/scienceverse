#' Get Power Calculations as List
#'
#' @param study A study list object with class scivrs_study
#' @param values Whether or not to retun criterion values (default FALSE)
#'
#' @return a list of power (and criteria) values
#' @export
#'
#' @examples
#' study() %>%
#'  add_hypothesis("H1") %>%
#'  add_analysis("A1", t.test(y~B1, data = D1)) %>%
#'  add_criterion("C1", "p.value", "<", 0.05) %>%
#'  add_analysis("A2", t.test(y~B1, data = D2)) %>%
#'  add_criterion("C2", "p.value", "<", 0.05) %>%
#'  add_eval("corroboration", "C1 & C2") %>%
#'  add_eval("falsification", "!C1 & !C2") %>%
#'  add_sim_data("D1", between = 2, n = 25, mu = c(0, 0.5)) %>%
#'  add_sim_data("D2", between = 2, n = 50, mu = c(0, 0.5)) %>%
#'  study_power(rep = 100) %>%
#'  get_power()
#'
get_power <- function(study, values = FALSE) {
  power <- list()

  power$power <- lapply(study$hypotheses, `[[`, "power")
  names(power$power) <- lapply(study$hypotheses, `[[`, "id")

  if (values) {
    power$results <- lapply(study$analyses, `[[`, "power")
    names(power$results) <- lapply(study$analyses, `[[`, "id")
  }

  power
}
