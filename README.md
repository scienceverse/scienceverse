---
output: github_document
always_allow_html: yes
---

# scienceverse <img src="man/figures/README-logo.png" align="right" alt="" width="120" />
<!-- rmarkdown v1 -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!--[![Travis build status](https://travis-ci.org/scienceverse/scienceverse?branch=master)](https://travis-ci.org/scienceverse/scienceverse)
[![Codecov test coverage](https://codecov.io/gh/scienceverse/scienceverse/branch/master/graph/badge.svg)](https://codecov.io/gh/scienceverse/scienceverse?branch=master)-->
<!-- badges: end -->



The increasingly digital workflow in science has made it possible to share almost all aspects of the research cycle, from pre-registered analysis plans and study materials to the data and analysis code that produce the reported results. Although the growing availability of research output is a positive development, most of this digital information is in a format that makes it difficult to find, access, and reuse. A major barrier is the lack of a framework to concisely describe every component of research in a machine-readable format: A grammar of science. 

The goal of scienceverse is to generate and process machine-readable study descriptions to facilitate archiving studies, pre-registering studies, finding variables and measures used in other research, meta-analyses of studies, and finding and re-using datasets in other ways.

## A Grammar of Science

A grammar is a formal system of rules that allow users to generate lawful statements. The goal of a grammar of science is to allow users to generate rich, standardized metadata describing experiments, materials, data, code, and any other research components that scholars want to share. Such standardization would facilitate reproducibility, cumulative science (e.g., meta-analysis) and reuse (e.g., finding datasets with specific measures). While many projects focus on making data [FAIR](https://www.go-fair.org/fair-principles/), Scienceverse aims to make every aspect of research findable, accessible, interoperable and reusable.

Developing a Grammar of Science, combined with a shared lexicon (e.g., standardized ways to reference manipulations, measures, and variables) aims to facilitate open research practices for researchers and journals. It is intended to mitigate several well-known problems that follow from the lack of organization of research output. 

First, it has been shown that even when data and code are shared, computational reproducibility is low (Hardwicke et al., 2018, Obels et al., 2019). Scienceverse improves computational reproducibility by providing a framework that explicitly links hypotheses, materials, data, and code. Scienceverse archive files can store any aspect of research in a systematic way, allowing, for example, automatic evaluation of results against machine-readable specifications of statistical hypotheses. Automated reproducibility allows journals to compare pre-registered hypotheses with the conclusions in the final manuscript. Scienceverse helps researchers to specify which analyses would confirm or falsify predictions in a structured and unambiguous manner. Journals can automatically check these predictions for the final submission, which will prevent problems with undeclared deviations from the protocol – a known problem in pre-registered studies.

Second, Scienceverse aims to make shared outputs easier to find and re-use. Good meta-data are essential to find research output, but there have been few attempts in health psychology, or social sciences in general, to summarize the structure of those aspects of the empirical endeavour that need to be findable. Scienceverse aims to create a well-structured grammar that provides a complete description of these components of the research cycle, including hypotheses, materials, methods, study design, measured variables, codebooks, analyses, and conclusions. Referenced against discipline-specific lexicons, this allows researchers to retrieve any information from archive files. For example, researchers can search for studies that use similar manipulations and retrieve relevant information about the effects these manipulations produce. This information can be used when choosing manipulations for future studies, to design well-powered experiments, or to easily perform meta-analyses. Given specific inclusion criteria, Scienceverse makes it possible to automatically update meta-analyses and share these with the scientific community.

## Installation

You can install scienceverse from [GitHub](https://github.com/scienceverse/scienceverse) with:

``` r
devtools::install_github("scienceverse/scienceverse")
```

[Using scienceverse for Registered Reports](https://scienceverse.github.io/scienceverse/articles/registration.html)

[Quick Demo](https://scienceverse.github.io/scienceverse/articles/demo.html)


