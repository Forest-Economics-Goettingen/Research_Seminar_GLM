Research Seminar - Logistic Regression
================
Kai & Johannes

- [1 Preamble](#1-preamble)
- [2 Introduction](#2-introduction)
- [3 Data: `ForestHealth`](#3-data-foresthealth)
- [4 Orinary LM vs. GLM](#4-orinary-lm-vs-glm)
- [5 Logistic regression](#5-logistic-regression)
  - [5.1 Theoretical background](#51-theoretical-background)
  - [5.2 Developing a GLM by hand](#52-developing-a-glm-by-hand)
    - [5.2.1 Global mean model](#521-global-mean-model)
    - [5.2.2 Stand type (mixed / pure)
      model](#522-stand-type-mixed--pure-model)
    - [5.2.3 Age model](#523-age-model)
  - [5.3 Multiple logistic regression](#53-multiple-logistic-regression)
  - [5.4 Interpretation](#54-interpretation)
    - [5.4.1 Using odds](#541-using-odds)
  - [5.5 Model diagnostics](#55-model-diagnostics)
- [6 PhD examples](#6-phd-examples)
- [7 Literature and Material](#7-literature-and-material)
  - [7.1 Primary literature](#71-primary-literature)
  - [7.2 Online resources](#72-online-resources)
  - [7.3 Books](#73-books)

# 1 Preamble

This README file serves as an outline of today’s topics and contains or
links to all materials required. This file and all materials can be
found on github. Today’s aims are

- to have a loot at the type of **data** for which a logistic regression
  can be performed and
- to discuss one of the most relevant examples for which logistic
  regression are often used. On the basis of this example,
- we take a deeper look the special **properties** of the logistic
  regression and thereby repeat the concept of generalized regression
  and
- **perform and interpret** a logistic regression in `R`.
- At the end, 2 short examples from **current research projects** are
  presented and discussed.

The martial contains contents, ideas and examples from the textbook
Fahrmeir et al. (2013) as well as parts as from the associated lecture
series `GLM` by Professor Thomas Kneib, Chair of Statistics.
Furthermore, parts of the lectures `Statistical Data Analysis with R`
and `Advanced Statistical Programming` of the forest faculty are
included. We ask you to prepare some tasks beforehand to save some time
in the seminar. Those are tagged as **Beforehand task**.

We are using the following libraries:

``` r
library(tidyverse)  # Data science
library(ggplot2)    # Visualization
library(ggpubr)     # Multiple plots
library(patchwork)  # Multiple plots
library(boot)       # For logit() and inv.logit()

# GLM itself is part of the stats package, which is loaded at start up
library(ggeffects)  # Visual interpretation of statistical models
library(emmeans)    # Testing linear hypotheses
library(DHARMa)     # For model validation
```

**Beforehand task 0:** Getting ready

- Connect to the repository and clone it to your laptop
- Get R and R Studio running
- Install the required libraries

# 2 Introduction

Modelling survival probabilities, also known as survival analysis, is
one of the most important and most suitable purposes for which logistic
regression is used and which we will use as an example today. (Another
widely used field of application is the analysis of questionnaires,
which will be sketched in the PhD examples at the end.) The specialty of
survival data is that the response variable can only have binary (or
more generally categorical) outcomes. The individuals survive until a
certain point in time occurs, or more generally, until a certain event
occurs. The simplest models only contain the time, i.e. the age of the
individuals. They give the average unconditional probability of
survival. More complex models can take multiple aspects into account.
The idea of survival modelling is to estimate this event and possibly
additionally inference the reasons survival (or non-survival
respectively). This 0/1 property is the reason, why the values of the
response that are to be estimated have different outcomes than the
observations of the response variable. In logistic regression, not the
outcome of an observation $y_i$ (e.g. dead or alive) is estimated, but
the **probability that an observation shows an outcome**, a fundamental
contrast to ordinary linear regression. Logistic regression is a
possibility to fit a survival probability curve over a continuous
variable of interest, e.g. the tree age. Fact for the economists:
Logistic regression is thus an alternative to the well-known Weibull
survival curves. The regression curve has similar properties as the
cumulative Weibull distribution and the the interpretation is
comparable. Simply spoken, the advantage of Weibull-function is the
straightforward parameter interpretation. Logistic regression comes more
from the field of statistical inference and thus has more
straightforward test procedures (multiple variable selection, linear
hypotheses, quality of fit, …) and is easy to estimate (in terms of
programming and also technically).

# 3 Data: `ForestHealth`

Consider the example data set `ForestHealth` from Fahrmeir et
al. (2013). The data set consists of 16 variables with 1796 observations
on forest health to identify potential factors influencing the health
status of trees and therefore the vital status of the forest. The data
in our example come from a project in the forest of Rothenbuch
(Spessart), which has been carried out by Axel Göttlein (Technical
University, Munich) since 1982. Five tree species are part of this
survey: beech, oak, spruce, larch, and pine. Here we will restrict
ourselves to beech trees. Every year, the condition of beech trees is
categorized by three ordinal categories $\[0 \% - 12.5\)$,
$\[12.5 \% - 50 \%\)$, and $\[50 \% - 75 \%\)$. The forth category
$>75 \%$ is not captured in the data. The category 0 % signifies that
the beech tree is healthy, the category 100 % implies that the tree is
dead. See Fahrmeir et al. (2013, p. 9) for more details.

**Beforehand Task 1:** Getting familiar with the data and with the
nature of binary responses

- Load `ForestHealth`, rename to `fh`
- Create a binary response variable that contains healthy (defoliation
  $\[0 \% - 12.5\)$) and unhealthy ($\[12.5 \% - 75 \%\)$) trees
- Perform relevant descriptive statistics to get an overview
- Which variables might have an impact on the tree health?
- Fit an ordinary linear model `lm` to estimate the health status over
  the age
- Add **one** another variable that seems to influence the tree health
  - Are these two models suitable to estimate the health status? Give
    some pro and contra arguments and underpin your arguments using
    common numbers or diagrams of (linear) statistical inference.

``` r
# Loading ForestHealth
load("Data/ForestHealth.rda")

fh <- ForestHealth

# Creating the binary variable diseased (0 = healthy, 1 = diseased)
fh <- fh %>% mutate(diseased = ifelse(defoliation %in% "[0, 12.5)", 0, 1))

# Overview
summary(fh)
```

    ##        id             year          defoliation         x         
    ##  Min.   : 1.00   Min.   :1983   [0, 12.5) :1113   Min.   : 0.700  
    ##  1st Qu.:21.00   1st Qu.:1988   [12.5, 50): 629   1st Qu.: 3.700  
    ##  Median :42.00   Median :1994   [50, 75]  :  51   Median : 8.100  
    ##  Mean   :41.71   Mean   :1994                     Mean   : 7.448  
    ##  3rd Qu.:62.00   3rd Qu.:1999                     3rd Qu.:10.300  
    ##  Max.   :83.00   Max.   :2004                     Max.   :16.100  
    ##        y              age            canopy        inclination      elevation  
    ##  Min.   :0.400   Min.   :  7.0   Min.   :0.0000   Min.   : 0.00   Min.   :250  
    ##  1st Qu.:2.100   1st Qu.: 65.0   1st Qu.:0.7000   1st Qu.: 6.00   1st Qu.:340  
    ##  Median :3.000   Median :112.0   Median :0.9000   Median :14.00   Median :390  
    ##  Mean   :3.306   Mean   :106.2   Mean   :0.7731   Mean   :15.45   Mean   :387  
    ##  3rd Qu.:4.100   3rd Qu.:148.0   3rd Qu.:1.0000   3rd Qu.:21.00   3rd Qu.:440  
    ##  Max.   :9.000   Max.   :234.0   Max.   :1.0000   Max.   :46.00   Max.   :480  
    ##       soil             ph                            moisture         alkali   
    ##  Min.   : 9.00   Min.   :3.280   moderately dry          :198   very low :352  
    ##  1st Qu.:16.00   1st Qu.:4.100   moderately moist        :989   low      :988  
    ##  Median :23.00   Median :4.250   moist or temporarily wet:606   high     :308  
    ##  Mean   :24.63   Mean   :4.295                                  very high:145  
    ##  3rd Qu.:31.00   3rd Qu.:4.440                                                 
    ##  Max.   :51.00   Max.   :6.050                                                 
    ##         humus           stand     fertilized    diseased     
    ##  [0cm, 1cm]:973   mixed    :891   no :1450   Min.   :0.0000  
    ##  (1cm, 2cm]:387   deciduous:902   yes: 343   1st Qu.:0.0000  
    ##  (2cm, 3cm]:266                              Median :0.0000  
    ##  (3cm, 4cm]:118                              Mean   :0.3793  
    ##  (4cm, 9cm]: 49                              3rd Qu.:1.0000  
    ##                                              Max.   :1.0000

``` r
# Linear model
fh %>% ggplot(aes(y = diseased, x = age)) + geom_point() +
  geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
lm_simple <- fh %>% lm(diseased ~ age, data = .)
summary(lm_simple)
```

    ## 
    ## Call:
    ## lm(formula = diseased ~ age, data = .)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.8864 -0.3389 -0.1009  0.4270  0.9547 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.0419314  0.0238913  -1.755   0.0794 .  
    ## age          0.0039671  0.0002026  19.584   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4406 on 1791 degrees of freedom
    ## Multiple R-squared:  0.1764, Adjusted R-squared:  0.1759 
    ## F-statistic: 383.5 on 1 and 1791 DF,  p-value: < 2.2e-16

``` r
# Multiple linear model: Age, stand
lm_multiple_1 <- fh %>% select(diseased, age, stand) %>%  lm(diseased ~ ., data = .)
summary(lm_multiple_1)
```

    ## 
    ## Call:
    ## lm(formula = diseased ~ ., data = .)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.8985 -0.3299 -0.1047  0.4162  0.9596 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.0008312  0.0247395  -0.034    0.973    
    ## age          0.0035764  0.0002120  16.869  < 2e-16 ***
    ## standmixed  -0.0625172  0.0108900  -5.741  1.1e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4367 on 1790 degrees of freedom
    ## Multiple R-squared:  0.1913, Adjusted R-squared:  0.1904 
    ## F-statistic: 211.7 on 2 and 1790 DF,  p-value: < 2.2e-16

``` r
fh %>% ggplot(aes(y = diseased, x = age, col = stand)) + geom_point() +
  geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
# Multiple linear model: Age, ph
lm_multiple_2 <- fh %>% select(diseased, age, ph) %>%  lm(diseased ~ ., data = .)
summary(lm_multiple_2)
```

    ## 
    ## Call:
    ## lm(formula = diseased ~ ., data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.88023 -0.35707 -0.09575  0.43448  1.03082 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.407705   0.134845   3.024  0.00253 ** 
    ## age          0.003969   0.000202  19.648  < 2e-16 ***
    ## ph          -0.104735   0.030916  -3.388  0.00072 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4393 on 1790 degrees of freedom
    ## Multiple R-squared:  0.1816, Adjusted R-squared:  0.1807 
    ## F-statistic: 198.6 on 2 and 1790 DF,  p-value: < 2.2e-16

``` r
# Marginal effects
p1 <- ggeffects::ggeffect(lm_multiple_2, terms = "age") %>% plot()
p2 <- ggeffects::ggeffect(lm_multiple_2, terms = "ph") %>% plot()

ggpubr::ggarrange(plotlist = list(p1, p2), ncol = 2)
```

![](README_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

# 4 Orinary LM vs. GLM

Why is the ordinary linear models usually not suited to model binary
responses (and under which conditions could a linear model might be
sufficient)? Consider the results from task 1 and elaborate the question
further in task 2.

**Beforehand Task 2:** What is the difference between linear models and
general linear models? What adds the G to LM?

- Read Lane (2002) and prepare for the following discussion:
  - Why do generalized linear model belong to the family of linear
    models even though their curvature is not linear?
  - Which properties distinguish a *generalized linear model* from a
    *linear* model? In particular: What is a link function?
  - Which properties distinguish a *generalized linear model* from a
    *non-linear* model?

# 5 Logistic regression

The GLM philosphy is explained directly using the example of logistic
regression. Generally, in contrast to LMs, GLMs have a link function and
allow arbitrary assumptions to be made about the residual distribution.
Other existing differences are not included.

## 5.1 Theoretical background

Instead of modelling $y_i$ directly, we define a regression model that
estimates the possibility of a discrete status 1 (e.g. diseased)

$$
\pi_i=\text{E}(y_i)=P(y_i=1),
$$

where $\pi_i$ shall vary continuously between 0 and 1 even tough $y_i$
is discrete. To achieve $\pi_i \in[0, 1]$, we apply a transformation (in
the sense of Lane (2002)) that transforms (“links”) the linear
regression formula to the deserved properties. The linear regression
formula (so to speak the linear core of the GLM)

$$
\eta_i=\beta_0+\beta_1 x_{i1}+ \dots + \beta_k x_{ik}
$$

is called linear predictor. Binomial regression requires a more
sophisticated transformation than the log-normal regression performed in
Lane (2002). However, the principle is the same.

$$
\pi_i=h(\eta_i).
$$

Along with some less familiar transformations, the logit transformation
leads to the desired properties of survival analysis, which is why the
binomial is sometimes referred to as logit regression. The term logistic
regression comes from the logistic (s shape) curvature that follows from
the logit transformation (In this sense, Weibull functions, Bertalanffy
growth functions and any other s shaped regression curves are also
logistic regressions). The logit response function $h$ relies on the
logistic cumulative distribution function

$$
\pi=h(\eta)=\frac{\text{exp}(\eta)}{1+\text{exp}(\eta)}.
$$

The respective inverse, also called link function $h$ reads as

$$
g(\pi)=\text{log} \left( \frac{\pi}{1 - \pi}\ \right).
$$

The response function follows a Bernoulli distribution. However,
grouping observation with same outcomes makes the solving process more
efficient and then leads to a Binomial distribution (Fahrmeir et al.,
2023, p. 270 - 277), which is why in practiced programming, the Binomial
or Quasibinomial distributions are usually taken. You should stick to
these standard combination of distributions of link functions, however,
you can freely decide for any of the distributions of the so called
exponential family if you know what you do.

## 5.2 Developing a GLM by hand

We want to develop the principle of the link function by a
self-programmed data transformation.

### 5.2.1 Global mean model

Let’s start simple. We firstly calculate the mean disease probability in
the sample without any covariates (even without `age`). Straightforward
calculation is to calculate the ratio of successes (diseased trees) in
relation to all trees as

``` r
sum(fh$diseased) / nrow(fh)
```

    ## [1] 0.3792526

``` r
mean(fh$diseased)
```

    ## [1] 0.3792526

``` r
# This is the same as a linear model
m1 <- lm(diseased ~ 1, data = fh)
coef(m1)
```

    ## (Intercept) 
    ##   0.3792526

Another option is to calculate an intercept only logistic regression.
This option appears to be too complicated to the simple problem. We use
it to underpin the how a GLM calculates the probabilities out of a
sample. We come back to this.

``` r
# GLM
glm_intercept_only <- glm(diseased ~ 1, family = binomial(), data = fh)
```

Result (prediction) is the average disease probability. Additionally, we
get the prediction of the link function. Generally, the GLM output
provides predictions for the response, the original scale. Look at the
Theoretical background, we have the result of the linear predictor $y_i$
(`link`) and for the response $\pi_i$. In Lane (2002), this response was
$y_o=\beta_0 \beta_1^{x_1}$, the link
$y_i=\beta_0+\beta_1\text{log(} x \text{)}$. The link, to conclude,
provides estimations on the scale of the linear predictor (here, the
logit, in Lane the log). Applying the inverse of the logit function
`exp(x)/(1+exp(x))` to the prediction of the link thus, again, gives the
response. The `GLM` output provides both estimates.

``` r
predict(glm_intercept_only, type = "response")[1]
```

    ##         1 
    ## 0.3792526

``` r
(x <- predict(glm_intercept_only, type = "link")[1])
```

    ##          1 
    ## -0.4927216

``` r
exp(x) / (1+exp(x)) # The response again
```

    ##         1 
    ## 0.3792526

Analogously to Lane (2002), we can also calculate the logits by hand (we
transform the data). Still for the case of a simple population mean, we
get

``` r
(p <- mean(fh$diseased))
```

    ## [1] 0.3792526

``` r
log(p / (1 - p))# logit(p)
```

    ## [1] -0.4927216

``` r
inv.logit(logit(p)) # short for exp(x) / (1+exp(x))
```

    ## [1] 0.3792526

### 5.2.2 Stand type (mixed / pure) model

Further developing the model, we now define groups of mixture type
`stand` with differing disease probabilities. We fit a model with one
discrete variable only:

``` r
# Firstly, the GLM considering one categorical variable
glm_stand <- glm(diseased ~ stand, family = binomial(), data = fh)
summary(glm_stand)
```

    ## 
    ## Call:
    ## glm(formula = diseased ~ stand, family = binomial(), data = fh)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -0.53077    0.05077  -10.45   <2e-16 ***
    ## standmixed  -0.53077    0.05077  -10.45   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2380  on 1792  degrees of freedom
    ## Residual deviance: 2266  on 1791  degrees of freedom
    ## AIC: 2270
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# Again, we fetch disease probabilities for the original scale (response) and
# for the logit-transformed scale (link)

nd <- data.frame(stand = c("deciduous", "mixed"))
predict(glm_stand, nd, type = "response")
```

    ##         1         2 
    ## 0.5000000 0.2570146

``` r
(x <- predict(glm_stand, nd, type = "link"))
```

    ##             1             2 
    ##  1.620926e-14 -1.061544e+00

``` r
inv.logit(x) # gives the response again
```

    ##         1         2 
    ## 0.5000000 0.2570146

``` r
# A linear model does no longer work
m2 <- lm(diseased ~ stand, data = fh)
coef(m2)
```

    ## (Intercept)  standmixed 
    ##   0.3785073  -0.1214927

And now the promised linear model with data transformation, the GLM by
hand:

``` r
# Do the logit transformation
d1 <- data.frame(
  stand = c("mixed", "deciduous"), 
  logit = table(fh$stand, fh$diseased) |> 
    apply(1, function(x) x[2] / sum(x)) |> 
    logit()
)
d1
```

    ##               stand     logit
    ## mixed         mixed -1.061544
    ## deciduous deciduous  0.000000

``` r
# ... and then estimate an ordinary linear model.
glm_stand_by_hand <- lm(logit ~ stand, data = d1)
coef(glm_stand_by_hand)
```

    ##   (Intercept)    standmixed 
    ## -7.850462e-17 -1.061544e+00

``` r
# Backtransformation gives us the same group means as the GLM.
inv.logit(coef(glm_stand_by_hand))
```

    ## (Intercept)  standmixed 
    ##   0.5000000   0.2570146

Interim interpretation: Just as in logarithmic regression, logistic
regression performs a “in-model” transformation. Doing data
transformation to linearize the data leads (in this example) to the same
results. Be aware that all models could also be calculated as simple
group means.

### 5.2.3 Age model

Now let’s derive the typical disease model, modelling the disease
probability over age to strengthen the advantages of GLM. We add the
continuous variable `age`. As any observation can be 0 or 1 only, we
cannot calculate $p$ for the single observations. A problem that not yet
occurred (not in Lane (2002)/ never in log-linear regression, not in our
simpler disease models). We need to group the original observations to
create observations of $p$ for our model. Best would be to have multiple
observations for each outcome of the continuous variable, which is,
however, not likely for continuous variables. We thus need to define
intervals of ages.

``` r
d <- fh |> mutate(g = age - (age %% 25)) |> 
  group_by(g) |> 
  summarize(logit = logit(mean(diseased))) |> # Calc. logits for groups
  mutate(age = g + 12.5)

head(d)
```

    ## # A tibble: 6 × 3
    ##       g   logit   age
    ##   <dbl>   <dbl> <dbl>
    ## 1     0 -4.05    12.5
    ## 2    25 -2.40    37.5
    ## 3    50 -0.931   62.5
    ## 4    75 -1.46    87.5
    ## 5   100 -0.0566 112. 
    ## 6   125  0.0531 138.

``` r
glm_age_by_hand <- lm(logit ~ age, data = d) # Fit a linear regression through the logits
coef(glm_age_by_hand)
```

    ## (Intercept)         age 
    ##  -2.5979539   0.0139126

The logit-transformed data is indeed linear (more or less) and the
back-transformed regression function shows indeed an s-shaped curve that
looks familiar to survival modelers.

``` r
p1 <- d %>% ggplot(aes(y = logit, x = age)) + geom_point() + 
  geom_abline(intercept = coef(glm_age_by_hand)["(Intercept)"],
              slope = coef(glm_age_by_hand)["age"])

p2 <- d %>% ggplot(aes(y = inv.logit(logit), x = age)) + geom_point() + geom_function(fun = function (x) {inv.logit(coef(glm_age_by_hand)["(Intercept)"] + coef(glm_age_by_hand)["age"] * x)})

# p1 + p2 # Altervative patchwork
ggpubr::ggarrange(plotlist = list(p1, p2), ncol = 2)
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
glm_age <- glm(diseased ~ age, data = fh, family = binomial(link = "logit"))
coef(glm_age)
```

    ## (Intercept)         age 
    ## -2.73169128  0.01991347

``` r
nd <- data.frame(age = c(10, 50, 100))
predict(glm_age_by_hand, nd)
```

    ##         1         2         3 
    ## -2.458828 -1.902324 -1.206694

``` r
predict(glm_age, nd)
```

    ##          1          2          3 
    ## -2.5325565 -1.7360175 -0.7403438

``` r
predict(glm_age_by_hand, nd) |> inv.logit()
```

    ##          1          2          3 
    ## 0.07879538 0.12984568 0.23028655

``` r
predict(glm_age, nd) |> inv.logit()
```

    ##          1          2          3 
    ## 0.07360713 0.14981949 0.32292897

``` r
predict(glm_age, nd, type = "response")
```

    ##          1          2          3 
    ## 0.07360713 0.14981949 0.32292897

We can do better by increasing the number of classes. And this already
gives the argument why to use `GLM` instead of *do it by hand*.

``` r
d <- fh |> mutate(g = age - (age %% 2)) |> 
  group_by(g) |> 
  summarize(logit = logit(mean(diseased))) |> 
  mutate(age = g + 1) |> 
  filter(is.finite(logit))

glm_age_by_hand_2 <- lm(logit ~ age, data = d)
coef(glm_age_by_hand_2)
```

    ## (Intercept)         age 
    ## -2.40947078  0.01648748

``` r
predict(glm_age_by_hand_2, nd) |> inv.logit()
```

    ##          1          2          3 
    ## 0.09581663 0.17007488 0.31848941

``` r
predict(glm_age, nd) |> inv.logit()
```

    ##          1          2          3 
    ## 0.07360713 0.14981949 0.32292897

We now have many groups with 0 or very few observations. If we would
include a second covariate, e.g. stand, the situations becomes even
worse.

## 5.3 Multiple logistic regression

GLM provides the ordinary procedures of hypotheses testing and
statistical inference, similar to a linear model but different in the
details. Let’s fit a multiple model. The code is similar to `lm`. We
here show basic principles of how a GLM can be interpreted and how its
model quality can be assessed.

``` r
glm_multiple <- fh %>% select(age, elevation, soil, ph, fertilized, diseased, stand) %>% 
  glm(diseased ~ ., data = .,  family = binomial(link = "logit"))
```

## 5.4 Interpretation

We now want to look at whether other covariates contribute more than age
to the likelihood of disease. First, we can interpret the summary in the
usual way. The p-values indicate the significance of the covariates.
However, unlike ordinary linear models, the parameter cannot be
interpreted directly. The summary gives the estimates of the linear
predictor, the “link” scale.

``` r
summary(glm_multiple)
```

    ## 
    ## Call:
    ## glm(formula = diseased ~ ., family = binomial(link = "logit"), 
    ##     data = .)
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   1.035e-01  9.311e-01   0.111   0.9115    
    ## age           1.938e-02  1.436e-03  13.493  < 2e-16 ***
    ## elevation     5.722e-05  9.923e-04   0.058   0.9540    
    ## soil         -3.100e-02  6.310e-03  -4.912 8.99e-07 ***
    ## ph           -5.608e-01  1.832e-01  -3.062   0.0022 ** 
    ## fertilizedno  5.208e-01  8.232e-02   6.327 2.50e-10 ***
    ## standmixed   -2.616e-01  6.391e-02  -4.094 4.24e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2380.0  on 1792  degrees of freedom
    ## Residual deviance: 1920.1  on 1786  degrees of freedom
    ## AIC: 1934.1
    ## 
    ## Number of Fisher Scoring iterations: 4

The model suggests that altitude might has no effect on the model,
whereas age, soil and fertilization fairly contribute to the model.
Also, we can read the direction from the summary in which the covariates
contribute to disease probability and we can derive rankings of the
effects from the parameter estimates. Positive contribution:
`fertilization` \> `age`. Negative contribution: `ph` \> `soil` \>
`elevation`. However, an interpretation in the sense of *if one
covariate changes by one unit, how does that alter the response
variable* is not possible with the data from the summary. Best practice
is to look at the curvature of the regression model. `ggeffects`
provides marginal effects plots, where one effect is displayed in
isolation keeping the others fixed (economists may know this principles
as *ceteris paribus*).

``` r
ggeffects::ggeffect(glm_multiple, terms = c("age")) %>% plot()
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
ggeffects::ggeffect(glm_multiple, terms = c("elevation")) %>% plot()
```

![](README_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
ggeffects::ggeffect(glm_multiple, terms = c("soil")) %>% plot()
```

![](README_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

``` r
ggeffects::ggeffect(glm_multiple, terms = c("age", "stand")) %>% plot()
```

![](README_files/figure-gfm/unnamed-chunk-15-4.png)<!-- -->

### 5.4.1 Using odds

If you want to interprete the coefficients, the odds ratio can be used.
The odds give the probability of an event occuring divided by the
probability that this event does not occurs ($odds = p / (1 - p)$).
Hence, if the odds are 1 its as likely that event will occur or will not
occur ($0.5 / (1 - 0.5) = 1$). If we now take the ratio of two odds
($OR$), we can say something about the coefficints of a logistic
regression.

Lets take `glm_multiple` as example:

``` r
summary(glm_multiple)
```

    ## 
    ## Call:
    ## glm(formula = diseased ~ ., family = binomial(link = "logit"), 
    ##     data = .)
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   1.035e-01  9.311e-01   0.111   0.9115    
    ## age           1.938e-02  1.436e-03  13.493  < 2e-16 ***
    ## elevation     5.722e-05  9.923e-04   0.058   0.9540    
    ## soil         -3.100e-02  6.310e-03  -4.912 8.99e-07 ***
    ## ph           -5.608e-01  1.832e-01  -3.062   0.0022 ** 
    ## fertilizedno  5.208e-01  8.232e-02   6.327 2.50e-10 ***
    ## standmixed   -2.616e-01  6.391e-02  -4.094 4.24e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2380.0  on 1792  degrees of freedom
    ## Residual deviance: 1920.1  on 1786  degrees of freedom
    ## AIC: 1934.1
    ## 
    ## Number of Fisher Scoring iterations: 4

We could calculate the OR for age, i.e., how does the OR change if age
increases by 1 unit (= 1 year). To see this, we will have to calculate
the odds for year = 10 and year = 11 (or any pair of values that differ
by 1 unit). All other covariates are held constant.

``` r
p1 <- inv.logit(c(1, 10, 0, 0, 0, 0, 0) %*% coef(glm_multiple))
p2 <- inv.logit(c(1, 11, 0, 0, 0, 0, 0) %*% coef(glm_multiple))

(p2 / (1 - p2)) / (p1 / (1 - p1))
```

    ##          [,1]
    ## [1,] 1.019569

This happens to be the `exp()` of the coefficient for age.

``` r
exp(coef(glm_multiple)[2])
```

    ##      age 
    ## 1.019569

There are few simple conclusions you can draw about any $\beta$:

- If $\beta = 0$, the OR will be 1.

- If $\beta < 0$, the OR will be \< 1 and vice versa.

- A OR of 2 means that the effects doubles, a OR of $0.5$ means that the
  effect is only half.

## 5.5 Model diagnostics

The distribution of residuals for GLMs is not obvious any longer. A
method that gained popularity recently is to simulate randomized
quantile residuals from your model. The `DHARMa` package provides
several functions to do exactly this. The easiest way is to plot the
residuals and check if they match. We can compare the linear model,
where the family is obviously wrong with a glm.

``` r
plot(simulateResiduals(lm_multiple_2))
```

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
plot(simulateResiduals(glm_multiple))
```

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

This looks all good.

# 6 PhD examples

- Henning
- Valeska

# 7 Literature and Material

This file as well as all other materials are uploaded on GitHub. You
should all have ssh access rights, such that you can use your version
control feature of RStudio to clone the GitHub repository:

<https://github.com/Forest-Economics-Goettingen/Research_Seminar_GLM>
<git@github.com>:Forest-Economics-Goettingen/Research_Seminar_GLM.git

## 7.1 Primary literature

Lane (2002): Generalized linear models in soil science. European Journal
of Soil Science, 53, 241-251.
<https://doi.org/10.1046/j.1365-2389.2002.00440.x>

## 7.2 Online resources

Johannes Signer, Kai Husmann (2024): Kursmaterial: Einführung in die
Datenanalyse mit R.
<https://github.com/Forest-Economics-Goettingen/KursskriptRBsc>

## 7.3 Books

Ludwig Fahrmeir, Thomas Kneib, Stefan Lang, Brian Marx (2013):
Regression : Models, Methods and Applications. Berlin, Heidelberg.
Springer. <https://link.springer.com/book/10.1007/978-3-642-34333-9>
