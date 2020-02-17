
# Workshop: Bayesian analysis with R and brms

Here are materials for a workshop on Bayesian analysis at Case Western Reserve University. An earlier version of this workshop was given that the Rotman Research Institute.

For this workshop you will need:

- [R](https://www.r-project.org/)
- [R Studio](https://www.rstudio.com/)
- The materials in this repository (click green "Clone or download" button above)
- Some `R` packages, by entering the code below in RStudio

```
install.packages(c("brms", "rstan", "bridgesampling", "loo", "bayesplot", "coda", "HDInterval", "lme4")) # this might take a while...
```

## Description

This workshop will introduce attendees to Bayesian data analysis and the `R` package [`brms`](https://cran.r-project.org/web/packages/brms/index.html). `brms` stands for 'Bayesian Regression Models using [`Stan`](https://mc-stan.org/)' and, as the name suggests, it provides a flexible interface to `Stan`, which is a powerful program for fitting Bayesian models. `brms` can handle a wide range of models and data types and this workshop will cover several example analyses, with a particular focus on (generalized) linear mixed effects models. These example analyses will cover the setting of prior distributions on model parameters, assessing model convergence and fit, and model comparison. Given responses to the pre-workshop questionnaire, we will also review linear and generalized mixed models. There are also slides and examples introducing the `Stan` model language so attendees can start to implement their own bespoke models, as well as better understand what `brms` is doing 'under the hood'.

## Objectives

1. Review linear and generalized mixed models
2. To give a conceptual understanding of Bayesian data analysis
3. Introduce the `brms` package for `R`
4. Work on several example analyses
    - specifying the model
    - choosing priors
    - evaluating convergence
    - posterior predictive checks
    - model comparison (including Bayes' factors)
5. Introduce the `Stan` model language and work through some examples (if there's time)

## Materials

### Slides

Main workshop slides:

- `intro.html` - how to get the materials and overview of the workshop
- `mixed.html` - primer on mixed models
- `bayes.html` - conceptual introduction to Bayesian analysis
- `brms.html` - introduction to `brms` with examples

Optional extras:

- `stan.html` - introduction to `Stan` with examples
- `mcmc.html` - introduction to MCMC sampling in Bayesian analysis

The `.Rmd` files used to create slides are also available (you may need extra packages to knit)

### Examples

Example analyses with `brms`:

- `brms-example1.R` - analysis of the `sleepstudy` dataset from the `lme4` package. Reaction times after different amounts of sleep deprivation
- `brms-example2.R` - analysis of a hypothetical memory study where two groups recall items under two different conditions. Outcome is recall accuracy (see `sim-brms-ex2.R` for code to simulate dataset)
- `brms-example3.R` - analysis of the `wine` dataset from the `ordinal` package. Ordinal ratings of wine betterness

Example analyses with `Stan`:

- `stan-example1.R` - analysis of the `sleepstudy` dataset with `Stan` (see `stan-models` folder)
- `stan-example2.R` - analysis of two-dimensional recall data that (I think) cannot be handled by `brms` (see `sim-recall-2D.R` for code to simulate dataset)

<!--
## Prerequisites

This workshop assumes familiarity with:

- The `R` language
- Some probability distributions
- Linear mixed effects models (users of `lme4` will have a head start)
-->

## Contact

Please send questions/ comments/ suggestions to: srhodes@research.baycrest.org
