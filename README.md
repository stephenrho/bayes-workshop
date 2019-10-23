
# Workshop: Bayesian analysis with R using brms and Stan

Here are materials for a workshop on Bayesian analysis at the Rotman Research Institute

For this workshop you will need:

- [R](https://www.r-project.org/)
- [R Studio](https://www.rstudio.com/)
- The materials in this repository (click green "Clone or download" button above)
- Some `R` packages, by entering the code below in RStudio

```
install.packages(c("brms", "rstan", "bayesplot", "coda")) # this might take a while...
```

## Description

This workshop will introduce attendees to Bayesian data analysis and the `R` package [`brms`](https://cran.r-project.org/web/packages/brms/index.html). `brms` stands for 'Bayesian Regression Models using [`Stan`](https://mc-stan.org/)' and, as the name suggests, it provides a flexible interface to `Stan`, which is a powerful program for fitting Bayesian models. `brms` can handle a wide range of models and data types and this workshop will cover several example analyses, with a particular focus on (generalized) linear mixed effects models. These example analyses will cover the setting of prior distributions on model parameters, assessing model convergence and fit, and model comparison. The workshop will also introduce the `Stan` model language so attendees can start to implement their own bespoke models, as well as better understand what `brms` is doing 'under the hood'.

## Objectives

1. To give a conceptual understanding of Bayesian data analysis
2. Introduce the `brms` package for `R`
3. Work on several example analyses
    - specifying the model
    - choosing priors
    - evaluating convergence
    - posterior predictive checks
    - model comparison (including Bayes' factors)
4. Introduce the `Stan` model language and work through some examples

## Prerequisites

This workshop assumes familiarity with:

- The `R` language
- Some probability distributions
- Linear mixed effects models (users of `lme4` will have a head start)


