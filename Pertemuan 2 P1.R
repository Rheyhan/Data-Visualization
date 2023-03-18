library(ggplot2)

df <- read.csv("D:/Kuliah/!yes/R/Visdat/Dataframe/kor_rt_jabar_2020.csv")
df_wilayah <- read.csv("D:/Kuliah/!yes/R/Visdat/Dataframe/master_wilayah_survei_sosial_ekonomi_nasional_2020_maret__modul_konsumsi_dan_pengeluaran__.csv")
df_merge <- merge(df, df_wilayah, by.x = c("R101", "R102"), by.y = c("value_prov", "kab"), all.x = TRUE)
df_merge

df <- midwest

# Bar Chart ####
barplot(df$poptotal)
barplot(table(df$poptotal))
ggplot(df, aes(x = area)) + geom_bar()

# Stacked Bar Chart (basic & 100%) ####
myTable <- table(df$inmetro, df$state)

barplot(myTable, legend = rownames(myTable))
barplot(apply(myTable, 2, function(x){x*100/sum(x, na.rm=T)}))

ggplot(df, aes(x = state, fill = as.factor(inmetro))) + geom_bar()
ggplot(df, aes(x = state, fill = as.factor(inmetro))) + geom_bar(position="fill")

# Grouped Bar Chart ####
barplot(myTable, beside = T, legend = rownames(myTable))

ggplot(df, aes(x = state, fill = as.factor(inmetro))) + geom_bar(position="dodge")

# Lollipop Chart ####
ggplot(df, aes(x = PID, y = poptotal)) + geom_point() +
    geom_segment(aes(x = PID, xend = PID, y = 0, yend = poptotal))

# Violin Chart ####
ggplot(midwest, aes(x = state, y = poptotal)) + geom_violin()
ggplot(midwest, aes(x = state, y = poptotal)) + geom_violin() + geom_boxplot() +
    scale_y_continuous(trans='log')

ggplot(midwest, aes(x = state, y = poptotal, fill = state)) + geom_violin() +
    scale_y_continuous(trans='log') + geom_boxplot(width = .2, fill = "white")

# Density Plot ####
plot(density(midwest$poptotal))
plot(density(log10(df$poptotal), bw = .5))
plot(density(log10(df$poptotal), bw = .05))

ggplot(midwest, aes(x = poptotal)) + geom_density()
ggplot(midwest, aes(x = poptotal)) + geom_density() + scale_x_continuous(trans='log')

ggplot(data = midwest, mapping = aes(x = poptotal, color = state, fill = state)) +
    geom_density(adjust = 1, alpha = .5) + theme_bw() + scale_x_continuous(trans='log10')

library(RColorBrewer)
display.brewer.all()

brewer.pal(n = 5, "Spectral")

ggplot(data = midwest, mapping = aes(x = poptotal, color = state, fill = state)) +
    geom_density(adjust = 2, alpha = .8) + theme_bw() +
    scale_fill_manual(values = brewer.pal(n = 5, "Spectral")) +
    scale_colour_manual(values = brewer.pal(n = 5, "Spectral")) +
    scale_x_continuous(trans='log10')

