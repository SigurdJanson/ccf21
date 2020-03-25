source("./R/ccf21.R")
source("./R/plotccf21.R")

t <- seq(0, 4*pi, length.out = 25)
x <- ts(sin(t), frequency = 0.34858)
y <- ts(sin(t - pi/4), frequency = 0.34858)
o <- ccf(x, y, ci = 0.95)

# Different title for all axes
# Pink confidence at ci p close to 1
g <- plot.ccf(o, ci = 0.99999999999, type = "b", 
              xlab = "XXXXXXX", ylab = "WhytestY", sub = "ci = 1, ma",
              ylim = c(-2, 2), main = "Rhein-Main", 
              ci.col = "#ff69b4", ci.type = "ma", 
              cex.main = 0.1, mar = c(5,5,5,5))
print(g)

# Different title for all axes
# Green confidence at p = 0.5
g <- plot.ccf(o, ci = 0.95, type = "h", 
              xlab = "H is the Thing", ylab = "WhytestY", sub = "ci = 0.95, white",
              ylim = c(-1, 1), main = "CI is Green", 
              ci.col = "#00ff00", ci.type = "white", 
              cex.main = 2, mar = c(1,5,1,5))
print(g)

t <- 0
for (TYPE in c("h", "l", "b", "s", "S")) {
  t   <- t + 1
  tci <- c("white", "ma", "white", "ma", "ma")[t]
  ci  <- c(0.5, 0.50, 0.95, 0.95, 0.99)[t]
  g <- plot.ccf(o, ci = ci, type = TYPE, 
                xlab = "X", ylab = TYPE, sub = paste("ci =", ci, tci),
                ylim = NULL, main = TYPE, 
                ci.col = "#00ff00", ci.type = tci, 
                cex.main = NULL, mar = NULL)
  print(g)
}

