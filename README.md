2024-10-07

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- <img src="vignettes/img/logo_huh.svg" align="right" width="20%"/> -->
<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-experimental-yellow.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/SigurdJanson/ccf21/branch/main/graph/badge.svg)](https://app.codecov.io/gh/SigurdJanson/ccf21?branch=main)
<!-- badges: end -->

# ccf21

The `ccf` function in R is an implementation for cross-correlations.
Cross-correlations aren’t such a difficult concept, but I still found it
difficult to understand how the `ccf` function works and how the
arguments affect the results.

[I](https://seifseit.de) first described the function in more detail.
You can find this in the white paper [“The R Cross Correlation
Function”](https://medium.com/@jan.seifert/the-r-cross-correlation-function-f5f426006425?source=friends_link&sk=60e3a85df26d2eebd0c47ab84c3407c0).
Then I wrote this extension, because giving the user a choice is an easy
way to make the behaviour more comprehensible. And I have also
documented this package in more detail.

## Goals

The new ccf should be an improvement but still be backwards compatible
with `stats::ccf`. These goals are being pursued with the updated
function of ccf21.

- Offer a switch to use simple correlations dropping the stationarity
  assumption of time series (see parameter `stationary`).
- New ways to handle sequences of different lengths: cut the longer
  sequence (today’s default) or “imprison” the shorter within the longer
  (see parameter `shiftaction`).
- Different ways to treat the vector positions that ‘become empty’
  through shifting: cut it (today’s default), wrap it back assuming the
  sequence is circular, or fill it with data (see parameter
  `shiftaction`).
- Offer adequate confidence intervals instead of the white noise
  solution that `ccf` uses.
- A second plot function that uses
  [ggplot2](https://ggplot2.tidyverse.org/ "ggplot2 graphics library").
- All this should be achieved without doing anything not compatible to
  the existing functions or S3 classes. New data structures should work
  with existing functions in the stats package and new functions should
  be able to read data structures created by `stats::ccf`.

## Stationarity Assumption

A stationary process has the property that the mean, variance and
autocorrelation structure do not change over time (NIST/SEMATECH, 2013).
Not always is that assumption desired when computing cross-correlations.
Therefore, the updated function `ccf` supports calculations under both
stationarity and non-stationarity assumption.

### Sequences of Different Lengths

When one sequence (y) is shorter than the other (Y) the function should
use this instead of simply cutting the longer sequence. That is what
`ccf` does at the time. Instead, ccf21 intends to move the shorter
sequence from the lower end to the upper end and correlate both
sequences at each step. This approach is called “imprison”.
Alternatively, “cut” shall be still available as option.

    Step 1
    Sequence Y: ####################
    Sequence y: ####

    Step 2
    Sequence Y: ####################
    Sequence y:   #####

    Step 3
    Sequence Y: ####################
    Sequence y:    ####

    ...

    Last step
    Sequence Y: ####################
    Sequence y:                 ####

## Shifting Vectors

Shifting two vectors against each other creates redundant vector
positions. Initial situation is this:

    Sequence x:   1##################N
    Sequence y:-> ###################M

Shifting y by 4 elements leaves 4 open positions on the right

    Sequence x: 1##################N
    Sequence y: ????1##################M

There are 3 solutions:

**Option 1**: simply cut redundant positions

    Sequence x:     ###############N
    Sequence y:     1###############    

**Option 2**: fill positions of y with ‘0’

    Sequence x: 1##################N
    Sequence y: 00001###############

**Option 3**: wrap y around

    Sequence x: 1##################N
    Sequence y: ###M1###############

## Confidence Intervals

The new `ccf` function returns confidence intervals as part of the
results. The classic R-function computes them when you request a plot.
Furthermore, R only returns a rather unspecific “white noise” confidence
for the cross-correlation and does not take the structure of the data
into account. The updated version supports confidence intervals around
the identified cross-correlation. It uses an approach suggested by
Bonett & Wright (2000) that transforms the correlations using [Fisher-z
transform](https://en.wikipedia.org/w/index.php?title=Fisher_transformation&oldid=946390163).
The Fisher z values are approximately normal distributed with a given
variance. Now, the function can easily determine the confidence range
and transform it back into correlations.

Note: these confidence intervals are not symmetrical because of the
characteristics of the probability distribution of correlations.

## Installation

You can install the development version of huh from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SigurdJanson/ccf21")
```

Please note: loading this package will mask `stats::ccf`.



## References

Bonett, D. G. & Wright, T. A. (2000). Sample Size Requirements for
Estimating Pearson, Kendall and Spearman Correlations.” Psychometrika,
65 (1), p. 23-28.

[NIST](http://www.nist.gov/ "National Institute of Standard and Technology")/[SEMATECH](http://www.sematech.org/ "SUNY Polytechnic Institute")
(2013). [e-Handbook of Statistical
Methods](http://www.itl.nist.gov/div898/handbook/) - [Chapter 6.4.4.2.
Stationarity](https://www.itl.nist.gov/div898/handbook/pmc/section4/pmc442.htm),
accessed 2020-03-27.

[Seifert, J.](https://twitter.com/usernaut) (2020). [“The R Cross
Correlation
Function”](https://medium.com/@jan.seifert/the-r-cross-correlation-function-f5f426006425?source=friends_link&sk=60e3a85df26d2eebd0c47ab84c3407c0).
[medium.com](https://medium.com)

Regards, Jan

> Doing this just out of curiosity.
