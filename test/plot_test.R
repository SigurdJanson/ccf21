source("./R/ccf21.R")
source("./R/plotccf21.R")

t <- seq(0, 4*pi, length.out = 100)
x <- ts(sin(t), frequency = 0.34858)
y <- ts(sin(t - pi/4), frequency = 0.34858)

# Different title for all axes
# Pink confidence at p = 1
g <- plot.ccf(o, ci = 1, type = "b", 
              xlab = "XXXXXXX", ylab = "WhytestY", sub = "AnuthuSub",
              ylim = c(-2, 2), main = "Rhein-Main", 
              ci.col = "#ff69b4", ci.type = "white", 
              cex.main = 0.1, mar = c(5,5,5,5))
print(g)

# Different title for all axes
# Green confidence at p = 0.5
g <- plot.ccf(o, ci = 0.5, type = "h", 
              xlab = "H is the Thing", ylab = "WhytestY", sub = "AnuthuSub",
              ylim = c(-1, 1), main = "CI is Green", 
              ci.col = "#00ff00", ci.type = "white", 
              cex.main = 2, mar = c(1,5,1,5))
print(g)

for (TYPE in c("h", "l", "b", "s", "S")) {
  g <- plot.ccf(o, ci = 0.5, type = TYPE, 
                xlab = "X", ylab = "Y", sub = "S",
                ylim = NULL, main = TYPE, 
                ci.col = "#00ff00", ci.type = "white", 
                cex.main = NULL, mar = NULL)
  print(g)
}

