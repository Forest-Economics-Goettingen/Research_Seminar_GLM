Research Seminar - Logistic Regression
================
Johannes, Kai

- [1 Preamble](#1-preamble)
- [2 Data: `Forest Health`](#2-data-forest-health)
- [3 Literature and Material](#3-literature-and-material)

# 1 Preamble

This README file serves as an outline of today’s topics and contains all
required materials or links to the materials. Today’s aim is

- to explain the nature of the data for which a logistic regression can
  be performed,
- to explain the special properties of the logistic regression and
  thereby introduce the concept of generalized geression, and
- to perform and interpret a logistic regression in `R`.
- At the end, examples from current research projects are shown and
  discussed.

The martial contains texts and examples from the textbook Fahrmeir et
al. (2013) and also parts and didactic concepts are borrowed from the
corresponding lecture `GLM` by Prof. Thomas Kneib, Chair of Statistics.
Also parts of the lectures `Statistical Data Analysis with R` and
`Advanced Statistical Programming` are included.

We are using the following libraries:

Modelling survival probabilities, also known as survival analysis, is
one of the most important applications for which logistic regression is
used and which we will use as an example today. Another famous field of
application is the questionnaire analysis, which will be sketched in the
PdD examples at the end. The special property of survival data is that
the response variable can only have binary (or more generally
categorical) outcomes. This property is the reason, why the values of
the response that are to be estimated have different outcomes than the
observations of the response variable. In logistic regression, not the
outcome of an observation (e.g. dead or alive) is estmimated, but the
probability that an observation shows an outcome. Logistic regression is
a possibility to fit a survival probability curve over a continuous
variable of interest, e.g. the tree age. Logistic regression is
therefore an alternative to the well-known Weibull survival curves. The
logit curve resulting from the regression has similar properties as the
cumulative Weibull distribution. Also the interpretation is comparable.

# 2 Data: `Forest Health`

Consider the example data set `forest health` from Fahrmeir et
al. (2013). The data set consists of 16 variables with 1796 observations
on forest health to identify potential factors influencing the health
status of trees and therefore the vital status of the forest. In
addition to covariates characterizing a tree and its stand, the exact
locations of the trees are known. The interest is on detecting temporal
and spatial trends while accounting for further covariate effects in a
flexible manner. The data in our example come from a specific project in
the forest of Rothenbuch (Spessart), which has been carried out by Axel
Göttlein (Technical University, Munich) since 1982. Five tree species
are part of this survey: beech, oak, spruce, larch, and pine. Here we
will restrict ourselves to beech trees. Every year, the condition of
beech trees is categorized by the response variable “defoliation”
(`defol`) into nine ordinal categories 0 %, 12.5 %, 25 %, 37.5 %, 50 %,
62.5 %, 75 %, 87.5 %, and 100 %. Whereas the category 0 % signifies that
the beech tree is healthy, the category 100 % implies that the tree is
dead. See p. 9 of Fahrmeir et al. (2013) for more details.

``` r
load("Data/ForestHealth.rda")
library(ggplot2)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

# 3 Literature and Material

Johannes Signer, Kai Husmann (2024): Kursmaterial: Einführung in die
Datenanalyse mit R.
<https://github.com/Forest-Economics-Goettingen/KursskriptRBsc>

Ludwig Fahrmeir, Thomas Kneib, Stefan Lang, Brian Marx (2013):
Regression : Models, Methods and Applications. Berlin, Heidelberg.
Springer. <https://link.springer.com/book/10.1007/978-3-642-34333-9>