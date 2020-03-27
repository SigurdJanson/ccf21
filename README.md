# ccf21

An enhanced implementation of R's cross-correlation function ``ccf``. [I](https://twitter.com/usernaut) took a closer look at this function in the white paper ["The R Cross Correlation Function"](https://medium.com/@jan.seifert/the-r-cross-correlation-function-f5f426006425?source=friends_link&sk=60e3a85df26d2eebd0c47ab84c3407c0) and found out, that it is difficult to understand what it does. Therefore, [I](https://twitter.com/usernaut) created my own version that makes the computations more transparent by offering different options. You can now choose the function's behaviour. This is the best way to make it transparent. Furthermore, the documentation is intended to be more detailed to close any gaps.


## Goals

Main goal is to write a new function that is better than the old one by achieving these sub-goals.

* Offer a switch to use simple correlations dropping the stationarity assumption of time series.
* New ways to handle sequences of different lengths: cut the longer sequence (today's default) or "imprison" the shorter within the longer.
* Different ways to treat the vector positions that 'become empty' through shifting: cut it (today's default), wrap it back assuming the sequence is circular, or fill it with data.
* Offer adequate confidence intervals instead of the white noise solution that ``ccf`` uses.
* A second plot function that uses ggplots.

All this should be achieved without doing anything not compatible to the existing functions or S3 classes. New data structures should work with old functions and new functions should be able to read old data structures.


## Stationarity Assumption

A stationary process has the property that the mean, variance and autocorrelation structure do not change over time (NIST/SEMATECH, 2013). Not always is that assumption desired when computing cross-correlations. Therefore, the updated function ``ccf``  supports calculations under both stationarity and non-stationarity assumption.



## Sequences of Different Lengths

When one sequence (y) is shorter than the other (Y) the function should use this instead of simply cutting the longer sequence. That is what ccf does at the time. Instead it should move the shorter sequence from the lower end to the upper end. And, of course, correlate both sequences at each step. This approach is called "imprison". Alternatively, "cut" shall be still available as option.

Step 1\
Sequence Y: ####################\
Sequence y: ####

Step 2\
Sequence Y: ####################\
Sequence y:  ####

Step 3\
Sequence Y: ####################\
Sequence y:   ####

... 

Last step\
Sequence Y: ####################\
Sequence y:                 ####



## Shifting Vectors 

Sequence x:   1##################N\
Sequence y: ->1##################M

Shift y by 4 elements leaves 4 open positions on the right\
Sequence x: 1##################N\
Sequence y: ....1##################M

Option 1: simply cut\
Sequence x:     ###############N\
Sequence y:     1###############

Option 2: fill it with '0' gives y\
Sequence x: 1##################N\
Sequence y: 00001###############

Option 3: wrap y around\
Sequence x: 1##################N\
Sequence y: ###M1###############



## Confidence Intervals

The new ``ccf`` function returns confidence intervalsas part of the results. The classic R-function computes them when you request a plot. Furthermore, R only returns a rather unspecific "white noise" confidence for the cross-correlation being and does not take the structure of the data into account.

The updated version supports confidence intervals around the identified cross-correlation. It uses an approach suggested by Bonett & Wright (2000) that transforms the correlations using [Fisher-z transform](https://en.wikipedia.org/w/index.php?title=Fisher_transformation&oldid=946390163). The Fisher z values are approximately normal distributed with a given variance. Now, the function can easily determine the confidence range and transform it back into correlations.

Note: these confidence intervals are not symmetrical because of the characteristics of the probability distribution of correlations.


## References

Bonett, D. G. & Wright, T. A. (2000). Sample Size Requirements for Estimating Pearson, Kendall and Spearman Correlations.” Psychometrika, 65 (1), p. 23-28.

[NIST](http://www.nist.gov/ "National Institute of Standard and Technology")/[SEMATECH](http://www.sematech.org/ "SUNY Polytechnic Institute") (2013). [e-Handbook of Statistical Methods](http://www.itl.nist.gov/div898/handbook/) - [Chapter 6.4.4.2. Stationarity](https://www.itl.nist.gov/div898/handbook/pmc/section4/pmc442.htm), accessed 2020-03-27.

[Seifert, J.](https://twitter.com/usernaut) (2020). ["The R Cross Correlation Function"](https://medium.com/@jan.seifert/the-r-cross-correlation-function-f5f426006425?source=friends_link&sk=60e3a85df26d2eebd0c47ab84c3407c0). [medium.com](https://medium.com)

Regards,\
Jan




> Doing this just out of curiosity.

