library(ggplot2)

#scatterplot
plot(midwest$area, midwest$poptotal)
plot(midwest$area, log(midwest$poptotal))

iniscatter <-ggplot(midwest, aes(area, poptotal)) +geom_point()
iniscatter
iniscatter + scale_y_continuous(trans="log")

#matrix plot
dummy<-midwest[,1:10]
plot(dummy)

#bubble chart
plot(midwest$area, midwest$poptotal, cex=midwest$percbelowpoverty)
plot(midwest$area, midwest$poptotal, cex=midwest$percbelowpoverty/10)

inibubble<-ggplot(midwest, aes(area,poptotal, size=percbelowpoverty)) + geom_point()
inibubble + scale_y_continuous(trans="log")
inibubblewarna<-inibubble<-ggplot(midwest, aes(area,poptotal, size=percbelowpoverty, color=percbelowpoverty)) + geom_point()
inibubblewarna+scale_y_continuous(trans="log")

#correlogram
midwest_numeric_column <- sapply(midwest, is.numeric)
View(midwest[midwest_numeric_column])
midwest_cor <- cor(midwest[midwest_numeric_column])
midwest_cor

round(midwest_cor, 2)
  #manual
library(reshape2)
midwest_cor_melt <- melt(midwest_cor)
midwest_cor_melt

myCorrPlot <- ggplot(data = midwest_cor_melt, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()
myCorrPlot

myCorrPlot +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme(axis.text.x = element_text(angle = 60))
  #otomatis
library(corrplot)
corrplot(midwest_cor)
corrplot(midwest_cor, method = "circle")
corrplot(midwest_cor, method = "number")
corrplot(midwest_cor, method = "number", tl.cex = .5, number.cex = .5)

#smoothing
plot(midwest$percwhite, midwest$percbelowpoverty, xlim = c(60, 100))

plot(midwest$perchsd, midwest$percbelowpoverty)
myScatter2 <- ggplot(midwest, aes(x = perchsd, y = percbelowpoverty)) +
  geom_point() + theme_bw()
myScatter2

#linear model
regresi<-lm(percbelowpoverty ~ perchsd, data = midwest)
regresi
  #manual
plot(midwest$perchsd, midwest$percbelowpoverty)
abline(regresi, col = "red", lwd = 2)
plot(midwest$perchsd, midwest$percbelowpoverty, xlim = c(0, 100), ylim = c(0, 60))
abline(regresi, col = "red", lwd = 2)
  #otomatis
myScatter2 + geom_smooth(method = "lm", color = "red")

#loess
myScatter2 + geom_smooth(method = "loess", color = "red")

myScatter2 + geom_smooth(method = "loess", span = 0.25, color = "red")
myScatter2 + geom_smooth(method = "loess", span = 0.75, color = "red")
myScatter2 + geom_smooth(method = "loess", span = 1.00, color = "red")

# Piece-wise Constant ####
pwc <- function (x, y, cutpoints) {
  cutpoints <- c(floor(min(x)), cutpoints, ceiling(max(x)))
  
  segments <- data.frame(start = cutpoints, stop = c(cutpoints[2:length(cutpoints)], NA))
  segments <- na.omit(segments)
  
  response <- c(NULL)
  
  for (each_row in segments) {
    y_chosen <- y[x > each_row[1] & x < each_row[2]]
    response <- c(response, mean(y_chosen))
  }
  
  segments <- cbind(segments, response)
  output <- segments
  
  last_point <- length(cutpoints) - 1
  last_stop <- output[last_point, "stop"]
  last_response <- output[last_point, "response"]
  
  output <- rbind(output,
                  c(last_stop, NA, last_response))
  return (output)
}

myPWC <- pwc(midwest$perchsd, midwest$percbelowpoverty, c(60, 70, 80))
myPWC

myScatter2 + geom_step(data = myPWC, aes(x = start, y = response), col = "blue", size = 1)

myScatter2 +
  geom_smooth(method = "lm", color = "darkgreen") +
  geom_smooth(method = "loess", span = 1.00, color = "red") +
  geom_step(data = myPWC, aes(x = start, y = response), col = "blue", size = 1)
