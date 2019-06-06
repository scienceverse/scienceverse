---
output: github_document
always_allow_html: yes
---

# scienceverse <img src="man/figures/README-logo.png" align="right" alt="" width="120" />
<!-- rmarkdown v1 -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build status](https://travis-ci.org/scienceverse/scienceverse?branch=master)](https://travis-ci.org/scienceverse/scienceverse)
[![Coverage status](https://codecov.io/gh/debruine/faux/branch/master/graph/badge.svg)](https://codecov.io/github/scienceverse/scienceverse?branch=master)
<!-- badges: end -->



The goal of scienceverse is to generate and process machine-readable study descriptions to facilitate archiving, registration, checking, and meta-analysis.

## Installation

You can install scienceverse from [GitHub](https://github.com/scienceverse/scienceverse) with:

``` r
devtools::install_github("scienceverse/scienceverse")
```
## Example


```r
library(scienceverse)
```


```r
# create a new study object
iris_study <- study("Iris Petals")

# add a hypothesis
iris_study <- add_hypothesis(iris_study, description = "Petal length and width will be significantly correlated")

# add an analysis
iris_study <- add_analysis(
  iris_study, func = "cor.test", 
  params = list(
    x = ".data[1]$Petal.Length",
    y = ".data[1]$Petal.Width",
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


```r
# generate a post-registration report
study_report(iris_study, "postreg", "postreg.html")
```


```r
# output sections of the report
output_hypotheses(iris_study) 
```

## Hypotheses

### Hypothesis 1

Petal length and width will be significantly correlated

* Criterion 1 is confirmed if analysis yields p.value < 0.05   

If all criteria are met, this hypothesis is supported.


```r
output_analyses(iris_study) 
```

## Analyses

### 

We will run `cor.test(x = .data[1]$Petal.Length, y = .data[1]$Petal.Width, alternative = two.sided, method = pearson, conf.level = 0.95)`


```r
output_results(iris_study)
```

## Results

### Hypothesis 1

Petal length and width will be significantly correlated

* Criterion 1 was p.value < 0.05 in analysis 1.  
    The result was p.value = 0  

**Conclusion**: All criteria were met, this hypothesis was supported.


Now that you've saved the study framework as a JSON file, you can also load it in using the `study()` function.


```r
reloaded_iris_study <- study("iris.json")
```



### Create by Piping

You can also pipe together the steps to create a study object, save it, and generate a report.


```r
iris_study <- study("Iris Petals") %>%
  add_hypothesis("Petal length and width will be positively and significantly correlated") %>%
  add_analysis("cor.test", list(
    x = ".data[1]$Petal.Length",
    y = ".data[1]$Petal.Width",
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



