
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
# create a new study object
iris_study <- study("Iris Petals")

# add a hypothesis
iris_study <- add_hypothesis(iris_study, description = "Petal length and width will be significantly correlated")

# add an analysis
iris_study <- add_analysis(
  iris_study, func = "cor.test", 
  params = list(
    x = ".data$Petal.Length",
    y = ".data$Petal.Width",
    alternative = "two.sided",
    method = "pearson",
    conf.level = 0.95
  )
)

# add a criterion for the last added hypothesis
iris_study <- add_criterion(iris_study, 
                            result = "p.value", 
                            operator = "<", 
                            comparator = 0.05)

# add data
iris_study <- add_data(iris_study, data = iris)

# run the analyses on the data
iris_study <- study_analyse(iris_study)

# save the framework to a JSON file
study_save(iris_study, "iris.json")
```

``` r
# generate a post-registration report
study_report(iris_study, "postreg")
```

``` r
# output section of the report
iris_study %>%
  output_hypotheses() %>%
  output_analyses() %>%
  output_results()
```

Hypotheses
----------

### Hypothesis 1

Petal length and width will be significantly correlated

-   Criterion 1 is confirmed if analysis yields p.value &lt; 0.05

If all criteria are met, this hypothesis is supported.

Analyses
--------

### 

We will run `cor.test(x = .data$Petal.Length, y = .data$Petal.Width, alternative = two.sided, method = pearson, conf.level = 0.95)`

Results
-------

### Hypothesis 1

Petal length and width will be significantly correlated

-   Criterion 1 was p.value &lt; 0.05 in analysis 1.
    The result was p.value = 0

**Conclusion**: Congratulations! All criteria were met, this hypothesis was supported.

Now that you've saved the study framework as a JSON file, you can also load it in using the `study()` function.

``` r
reloaded_iris_study <- study("iris.json")
```

### Create by Piping

You can also pipe together the steps to create a study object, save it, and generate a report.

``` r
iris_study <- study("Iris Petals") %>%
  add_hypothesis("Petal length and width will be positively and significantly correlated") %>%
  add_analysis("cor.test", list(
    x = ".data$Petal.Length",
    y = ".data$Petal.Width",
    alternative = "two.sided",
    method = "pearson",
    conf.level = 0.95
  )) %>%
  add_criterion("p.value", "<", 0.05) %>%
  add_criterion("estimate", ">", 0) %>%
  add_data(iris) %>%
  study_analyse() %>%
  study_save("iris.json")
```
