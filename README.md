
# Bayesian analysis with R using brms and Stan

- This is where the materials for the Rotman Research Institute workshop on Bayesian analysis will go.

- This is a work in progress but the materials will be available before the workshop on the 25th of October.

## Description

This workshop will introduce attendees to Bayesian data analysis and the `R` package [`brms`](https://cran.r-project.org/web/packages/brms/index.html). `brms` provides a flexible interface to the sampling routines in the [`Stan`](https://mc-stan.org/) program. It can handle a range of models and data types using `R`'s formula syntax. We will cover a range of examples with a particular focus on (generalized) linear mixed effects models. These examples will cover prior distributions, assessing model fit, and model comparison. The workshop will also introduce the `Stan` model language for more 'bespoke' models.

## Objectives

1. To give a conceptual understanding of Bayesian data analysis and its advantages
2. Introduce the `brms` package for `R`
3. Work on several example analyses
    - specifying the model
    - choosing priors
    - evaluating convergence
    - posterior predictive checks
    - model comparison (including Bayes' factors)
4. Introduce the `Stan` model language and work through some examples

## Requirements

(This will be added to)

- R
- R Studio

In RStudio type:

```
install.packages(c("brms", "rstan")) # this might take a while...
```

## Recommended Reading

Check back later...
