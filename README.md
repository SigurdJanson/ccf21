# ccf21
An enhanced implementation of R's cross-correlation function ``ccf``.

##Goals

Main goal is to write a new function that is better than the old one by achieving these sub-goals.
* Offer a switch to use simple correlations dropping the stationarity assumption of time series.
* New ways to handle of sequences with different lengths.
* Offer adequate confidence intervals instead of the white noise solution that ``ccf`` uses.
* Maybe a second plot function that uses ggplots.

All this should be achieved without doing anything not compatible to the existing functions or S3 classes. New data structures should work with old functions and new functions should be able to read old data structures.

