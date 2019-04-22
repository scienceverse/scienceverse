
<!-- README.md is generated from README.Rmd. Please edit that file -->
reg
===

The goal of reg is to generate and process machine-readable study descriptions.

Installation
------------

You can install the released version of pipeline from [GitHub](https://github.com/scienceverse/reg) with:

``` r
devtools::install_github("scienceverse/reg")
```

Example
-------

``` r
library(reg)
```

``` r
iris_study <- study("Iris Petals")

iris_study <- add_hypothesis(iris_study, NULL, "Petal length and width will be significantly correlated")

iris_study <- add_analysis(iris_study, 1, "cor.test", list(
  x = ".data$Petal.Length",
  y = ".data$Petal.Width",
  alternative = "two.sided",
  method = "pearson",
  conf.level = 0.95
))

iris_study <- add_criterion(iris_study, 1, 1, "p.value", "<", 0.05)

iris_study <- add_data(iris_study, 1, iris)

iris_study <- study_analyse(iris_study)

study_save(iris_study, "iris.json")

# this doesn't work yet
study_report(iris_study, "postreg")
```

Now that you've saved the study framework as a JSON file, you can also load it in using the `study()` function.

``` r
reloaded_iris_study <- study("iris.json")
```

### Create by Piping

You can also pipe together the steps to create a study object, save it, and generate a report.

``` r
iris_study <- study("Iris Petals") %>%
  add_hypothesis(1, "Petal length and width will be positively and significantly correlated") %>%
  add_analysis(1, "cor.test", list(
    x = ".data$Petal.Length",
    y = ".data$Petal.Width",
    alternative = "two.sided",
    method = "pearson",
    conf.level = 0.95
  )) %>%
  add_criterion(1, 1, "p.value", "<", 0.05) %>%
  add_criterion(1, 1, "estimate", ">", 0) %>%
  add_data(1, iris) %>%
  study_analyse() %>%
  study_save("iris.json") %>%
  study_report("postreg")
```

### Functional Setup

``` r
# set up empty study object
mystudy <- study("IAT Demo")

# add hypothesis 1
mystudy <- add_hypothesis(
  study = mystudy,
  id = "H1", # defaults to index
  description = "Mean RT will be significantly slower in the incongruent condition compared to the congruent condition.",
  evaluation = "&"
)

# add analysis 1
mystudy <- add_analysis(
  study = mystudy,
  id = "H1_ttest",
  func = "t.test",
  params = list(
    x = ".data$incongruent",
    y = ".data$congruent",
    alternative = "two.sided",
    paired = TRUE,
    var.equal = FALSE,
    conf.level = 0.95
  )
)

# add criterion 1 to hypothesis 1
mystudy <- add_criterion(
  study = mystudy,
  hypothesis_id = "H1", # defaults to last hypothesis
  analysis_id = "H1_ttest", # defaults to last analysis or index 1 if none yet
  result = "p.value",
  operator = "<",
  comparator = .05
)

# add criterion 2 to hypothesis 1
mystudy <- add_criterion(
  study = mystudy,
  hypothesis_id = "H1",
  analysis_id = "H1_ttest",
  result = "estimate",
  operator = ">",
  comparator = 0
)

# add hypothesis 2
mystudy <- add_hypothesis(
  study = mystudy,
  id = "H2",
  description = "Reaction times for congruent and incongruent trials will be signifiantly and positively correlated.",
  evaluation = "&"
)

# add analysis 2
mystudy <- add_analysis(
  study = mystudy,
  id = "H2_cor",
  func = "cor.test",
  params = list(
    x = ".data$congruent",
    y = ".data$incongruent",
    alternative = "two.sided",
    method = "pearson",
    conf.level = 0.95
  )
)

# add criterion 2 to hypothesis 2
mystudy <- add_criterion(
  study = mystudy,
  hypothesis_id = "H2",
  analysis_id = "H2_cor",
  result = "p.value",
  operator = "<",
  comparator = 0.05
)

# add criterion 2 to hypothesis 2
mystudy <- add_criterion(
  study = mystudy,
  hypothesis_id = "H2",
  analysis_id = "H2_cor",
  result = "estimate",
  operator = ">",
  comparator = 0
)

# generate pre-registration report
study_report(mystudy, type="prereg")

# add data
n_sub <- 50
dat <- faux::rnorm_multi(n_sub, 2, .5, c(500, 750), 250,
                         c("congruent", "incongruent"))
dat$sub_id = 1:n_sub
add_data(mystudy, 1, dat)

# run analyses
study_analyse(mystudy)

# generate post-registration report
study_report(mystudy, type="postreg")
```

### Object-oriented setup

``` r

mystudy <- study("IAT Demo")  %>%
  add_hypothesis(
    id = "H1", 
    description = "Mean RT will be significantly slower in the incongruent condition compared to the congruent condition.",
    evaluation = "&") %>%
  add_analysis(
    id = "H1_ttest",
    func = "t.test",
    params = list(
      x = ".data$incongruent",
      y = ".data$congruent",
      alternative = "two.sided",
      paired = TRUE,
      var.equal = FALSE,
      conf.level = 0.95
    )
  ) %>%
  add_criterion(
    hypothesis_id = "H1",
    analysis_id = "H1_ttest",
    result = "p.value",
    operator = "<",
    comparator = .05
  ) %>%
  add_criterion(
    hypothesis_id = "H1",
    analysis_id = "H1_ttest",
    result = "estimate",
    operator = ">",
    comparator = 0
  ) %>%
  add_hypothesis(
    id = "H2",
    description = "Reaction times for congruent and incongruent trials will be signifiantly and positively correlated.",
    evaluation = "&"
  ) %>%
  add_analysis(
    id = "H2_cor",
    func = "cor.test",
    params = list(
      x = ".data$congruent",
      y = ".data$incongruent",
      alternative = "two.sided",
      method = "pearson",
      conf.level = 0.95
    )
  ) %>%
  add_criterion(
    hypothesis_id = "H2",
    analysis_id = "H2_cor",
    result = "p.value",
    operator = "<",
    comparator = 0.05
  ) %>%
  add_criterion(
    hypothesis_id = "H2",
    analysis_id = "H2_cor",
    result = "estimate",
    operator = ">",
    comparator = 0
  ) %>%
  study_report(type="prereg") %>%
  add_data(1, dat) %>%
  study_analyse() %>%
  study_report(type="postreg")
```
