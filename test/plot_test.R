
source("./R/ccf21.R")
source("./R/plotccf21.R")

x <- ts(sin(t), frequency = 0.34858)
y <- ts(sin(t - pi/4), frequency = 0.34858)

o <- ccf(x, y, plot = FALSE) # set stationary implicitely!
summary(o)
print(o)
g <- plot(o, main = "hohoho")
g
