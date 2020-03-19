# ccf21
An enhanced implementation of R's cross-correlation function ``ccf``.

== Goals

Main goal is to write a new function that is better than the old one by achieving these sub-goals.
* Offer a switch to use simple correlations dropping the stationarity assumption of time series.
* New ways to handle sequences of different lengths: cut the longer sequence (today's default) or "imprison" the shorter within the longer.
* Different ways to treat the vector positions that 'become empty' through shifting: cut it (today's default), wrap it back assuming the sequence is circular, or fill it with data.
* Offer adequate confidence intervals instead of the white noise solution that ``ccf`` uses.
* Maybe a second plot function that uses ggplots.

All this should be achieved without doing anything not compatible to the existing functions or S3 classes. New data structures should work with old functions and new functions should be able to read old data structures.


== Stationarity Assumption
TBD


== Sequences of Different Lengths

When one sequence (y) is shorter than the other (Y) the function should use this instead of simply cutting the longer sequence. That is what ccf does at the time. Instead it should move the shorter sequence from the lower end to the upper end. And, of course, correlate both sequences at each step. This approach is called "imprison". Alternatively, "cut" shall be still available as option.

Step 1
Sequence Y: ####################
Sequence y: ####

Step 2
Sequence Y: ####################
Sequence y:  ####

Step 3
Sequence Y: ####################
Sequence y:   ####

... 

Last step
Sequence Y: ####################
Sequence y:                 ####



== Shifting Vectors 

Sequence x:   1##################N
Sequence y: ->1##################M

Shift y by 4 elements leaves 4 open positions on the right
Sequence x: 1##################N
Sequence y: ....1##################M

Option 1: simply cut
Sequence x:     ###############N
Sequence y:     1###############

Option 2: fill it with '0' gives y
Sequence x: 1##################N
Sequence y: 00001###############

Option 3: wrap y around
Sequence x: 1##################N
Sequence y: ###M1###############
