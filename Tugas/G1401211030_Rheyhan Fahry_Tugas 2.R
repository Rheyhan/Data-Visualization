library(ggplot2)
library(readxl)
df1<-read_excel("c:/Users/Rhey/Downloads/inidummy1.xlsx")
df1
inibubble1<-ggplot(df1, aes(Growth,Karyawan, size=Close, color=Sektor)) + geom_point()
inibubble1+scale_y_continuous(trans="log")
inibubble2<-ggplot(df1, aes(Growth,Marketcap, size=Karyawan,color=df1$`Market name`)) + geom_point()
inibubble2+scale_y_continuous(trans="log")

#2
library(ggplot2)
library(readxl)
df<-read_excel("c:/Users/Rhey/Downloads/inidummy.xlsx")
df
str(df$waktu)
df$waktu <- as.Date(df$waktu, format="%Y-%m-%d")

scatter<- ggplot(df, aes(x=waktu, y=harga)) +
  geom_point()
paketlengkap<- scatter+xlab("Hari") + ylab("Harga") + 
  ggtitle("Pegerakan Harga Saham BUMN20") + theme_light()

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

myPWC <- pwc(df$waktu, df$harga, c(25, 80, 190))
myPWC

paketlengkap +
  geom_smooth(method = "loess", span = 0.25, color = "red")+
  geom_step(data = myPWC, aes(x = start, y = response), col = "blue", size = 1)

