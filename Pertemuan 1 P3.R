# Introduction ####
library(ggplot2)

# Built-in Dataset
df <- midwest
df$popdensity

# Histogram ####
hist(df$popdensity)
??hist  # cari tahu lebih tentang parameter fungsi

ggplot(data = df, mapping = aes(x = popdensity)) + geom_histogram(binwidth = 5000)

# Boxplot ####
table(df$state)
boxplot(df$popdensity)
boxplot(log(df$popdensity))

# Stratified
boxplot(df$popdensity ~ df$state)
boxplot(log(df$popdensity) ~ df$state)

boxplot(log(df$popdensity))

ggplot(data = df, mapping = aes(x = popdensity, colour = state)) + geom_boxplot()

# Transformasi ####
log(df$popdensity)  # y* = log(y)

hist((-df$popdensity + 100000)**150)

# Scatter Plot ####
df$area
df$poptotal

hist(df$area,breaks=0.1)
hist(df$poptotal)

plot(df$area, df$poptotal)
plot(df$area, log(df$poptotal))

ggplot(data = df, mapping = aes(x = area, y = poptotal)) + geom_point() + scale_y_continuous(trans='log')

# GGPlot as a variable ####
ggplot(data = df, mapping = aes(x = popdensity, colour = state)) + geom_boxplot()

myGGPlot <- ggplot(data = df, mapping = aes(x = popdensity, colour = state))
myGGPlot + geom_boxplot()

