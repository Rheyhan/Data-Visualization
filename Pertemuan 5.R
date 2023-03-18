#Input data susenas
susenas<-read.csv("D:/Kuliah/!yes/R/Visdat/Dataframe/Data Susenas/Susenas Jawa Barat 2020.csv",sep = ";")
#Input data kabupaten
namakabupaten<-read.csv("D:/Kuliah/!yes/R/Visdat/Dataframe/Data Susenas/nama kabupaten.csv",sep = ",")
datakab <- namakabupaten[namakabupaten$nama_prov=="JAWA BARAT",]
datakab <- cbind(datakab$nama_kab,datakab$kab)
datakab <- as.data.frame(datakab)
colnames(datakab) <- c("Nama Kabupaten","KabKota")
#Merge dengan left join data susenas dan data kab
datadf <- merge(susenas, datakab, by="KabKota", all.x = TRUE)
head(datadf)
str(datadf)


#Donut Chart Sumber Air Minum

#Buat data dummy untuk menghitung sumber air minum
library(dplyr)
sumberair <- datadf %>% 
  count(Sumber_Air_Minum) %>% 
  mutate (per = n/sum(n)) %>%
  mutate(ratio=scales::percent(n/sum(n)))
sumberair


#Menghitung kumulatif percentage
sumberair$ymax = cumsum(sumberair$per)
sumberair$ymax

# Menghitung batas bawah tiap bagian
sumberair$ymin = c(0, head(sumberair$ymax, n=-1))
sumberair$ymin
sumberair


library(ggplot2)
ggplot(sumberair, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Sumber_Air_Minum)) + geom_rect() + coord_polar(theta="y") + xlim(c(2, 4))


#Posisi label ada di tengah
sumberair$labelPosition <- (sumberair$ymax + sumberair$ymin) / 2

ggplot(sumberair, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Sumber_Air_Minum)) + geom_rect() + coord_polar(theta="y") + xlim(c(2, 4)) + geom_text(x=3.5, aes(y=labelPosition, label=ratio), size = 3) + theme_void()


#Data label
sumberair$label <- paste0(sumberair$Sumber_Air_Minum, "\n value: ", sumberair$ratio)

ggplot(sumberair, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Sumber_Air_Minum)) + geom_rect() + coord_polar(theta="y") + xlim(c(-1, 4)) + geom_text(x=2, aes(y=labelPosition, label=label), size = 3) + theme_void() + theme(legend.position = "none")

#Area Plot Luas Lantai

p <- ggplot(datadf, aes(x=Luas_Lantai))
# Basic area plot
p + geom_area(stat = "bin", bins = 30)


#Change line and fill color
p + geom_area(stat = "bin", bins = 30, fill="lightblue", color="darkblue")
#Change line type
p + geom_area(stat = "bin", bins = 30, fill="lightblue", color="darkblue", linetype="dashed")
#Add Mean
p + geom_area(stat = "bin", bins = 30, fill="lightblue", color="darkblue", linetype="dashed") + geom_vline(aes(xintercept = mean(Luas_Lantai)), color = "darkblue", size= 1)



#Area Plot by group
q <- ggplot(datadf, aes(x=Luas_Lantai, fill=Status_Rawan))
# Basic area plot
q + geom_area(stat = "bin", bins = 30, alpha = 0.6) + theme_classic()


#Add mean line
#Buat data dummy
lantai <- datadf %>% 
  group_by(Status_Rawan) %>% 
  summarize(Mean=mean(Luas_Lantai, na.rm = TRUE))
lantai

#Add mean line and chaneg legend position
q + geom_area(stat = "bin", bins = 30, alpha = 0.6) + theme_classic() + geom_vline(data=lantai, aes(xintercept=Mean, color=Status_Rawan),
                                                                                   linetype="dashed") + theme(legend.position = "bottom")

#Area Chart

library(readxl)
butahuruf <- read_excel(path="D:/Kuliah/!yes/R/Visdat/Dataframe/databutahuruf.xlsx", col_names = TRUE)
butahuruf$Tahun <- as.Date(as.character(butahuruf$Tahun), format="%Y")
butahuruf



ggplot(butahuruf, aes(x= Tahun, y = Persen, fill= Usia)) + geom_area(color="black", size = 0.2, alpha = 0.8)


library(lubridate)
ggplot(butahuruf, aes(x= Tahun, y = Persen, fill= Usia)) + geom_area(color="black", size = 0.2, alpha = 0.8) + scale_x_date(date_breaks = "1 year", date_labels="%Y")