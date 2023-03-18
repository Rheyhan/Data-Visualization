#Data Susenas

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

#Bar Chart
##Status Rawan

library(ggplot2)
datadf$Status_Rawan
#Basic bar chart
ggplot(datadf, aes(x=Status_Rawan)) + geom_bar();
#Mewarnai bar chart dengan kondisi status rawan
e <- ggplot(datadf, aes(Status_Rawan, fill=Status_Rawan)) + geom_bar()
e
#Menghilangkan legend
e + theme(legend.position = "none")

##Pendidikan Kepala Rumah Tangga

x=reorder(datadf$Pendidikan_KRT, datadf$Pendidikan_KRT,function(x)-length(x))
x
#Basic Bar Chart
ggplot(datadf, aes(Pendidikan_KRT)) + geom_bar()
#Order Bar Chart from largest to smallest
ggplot(datadf, aes(x=reorder(Pendidikan_KRT, Pendidikan_KRT,function(x)-length(x)),fill=Pendidikan_KRT)) + geom_bar() + theme(legend.position = "none")
#Order Bar Chart from Smallest to Largest
ggplot(datadf, aes(x=reorder(Pendidikan_KRT, Pendidikan_KRT,function(x) length(x)),fill=Pendidikan_KRT)) + geom_bar() + theme(legend.position = "none")


#Mengubah count menjadi persentase
library(scales)
ggplot(datadf, aes(y=reorder(Pendidikan_KRT, Pendidikan_KRT,function(x)-length(x)),fill=Pendidikan_KRT)) + geom_bar(aes(x=((..count..)/sum(..count..)))) + scale_x_continuous(labels = percent_format()) + theme(legend.position = "none") + ylab("Persen") + xlab("Pendidikan KRT")

#Membuat data dummy

#Buat count untuk pendidikan KRT
library(dplyr)
datadf$Pendidikan_KRT
pendidikankrt <- datadf %>% 
  count(Pendidikan_KRT) %>% 
  mutate(ratio=(n/sum(n)))
pendidikankrt
#Buat basic bar chart
ggplot(pendidikankrt, aes(x=Pendidikan_KRT, y = ratio)) + geom_bar(stat="identity") + scale_y_continuous(labels=percent_format()) + theme(legend.position = "none")
#Reorder smallest to largest
a <- ggplot(pendidikankrt, aes(x=reorder(Pendidikan_KRT,ratio), y = ratio)) + geom_bar(stat="identity") + theme(legend.position = "none") + scale_y_continuous(labels = percent_format()) 
a
#Reorder smallest to largest dengan tambah text label
a + geom_text(aes(label=scales::percent(ratio)),col="black",vjust=-0.5)


#Reorder largest to smallest
b <- ggplot(pendidikankrt, aes(x=reorder(Pendidikan_KRT,-ratio), y = ratio)) + geom_bar(stat="identity") + theme(legend.position = "none") + scale_y_continuous(labels = percent_format())
b
#Reoder largest to smallest dengan tambah text label
b + geom_text(aes(label=scales::percent(ratio)),col="white",position =position_stack(vjust =0.5))

#Pie Chart Status Rawan

#Buat count untuk kondisi status rawan
library(dplyr)
jumlahrawan <- datadf %>% 
  count(Status_Rawan) %>% 
  mutate(ratio=scales::percent(n/sum(n)))
jumlahrawan
#Buat pie chart basic
rawanpie <- ggplot(jumlahrawan, aes(x="", y=n, fill=Status_Rawan)) + geom_bar(stat="identity") + coord_polar("y", start=0) + scale_fill_manual(values=c("green","red"))
rawanpie
#Hilangkan background
rawanpie + theme_void()
#Menambah text pada pie chart
rawanpie + theme_void() + geom_text(aes(label=(ratio)), vjust=0.5)

#Pie chart untuk pendidikan kepala rumah tangga (KRT)

#Latihan (JAM 11.00)
#Buat count untuk pendidikan KRT
library(dplyr)
pendidikankrt <- datadf %>% 
  count(Pendidikan_KRT) %>% 
  mutate(ratio=scales::percent(n/sum(n)))
pendidikankrt

#Buat pie chart 
pkrtpie <- ggplot(pendidikankrt, aes(x="", y=n, fill=Pendidikan_KRT)) + geom_bar(stat="identity") + coord_polar("y", start=0)
pkrtpie

#Panggil pie chart lengkap dengan text label dan tanpa background
pkrtpie + theme_void() + geom_text(aes(label=ratio), position = position_stack(vjust = 0.5))


Pertanyaan:
  1. visualisasi yang memberikan proporsi yang baik untuk menjelaskan status rawan pangan? Bar chart atau pie chart? Keduanya benar
2. visualisasi yang memberikan proporsi yang baik untuk menjelaskan pendidikan kepala rumah tangga? Bar chart atau pie chart? Bar Chart lebih mudah dimengerti

#Stacked bar chart pendidikan kepala rumah tangga berdasarkan status rawan

#Basic stacked bar chart
q <- ggplot(datadf, aes(Pendidikan_KRT, fill=Status_Rawan)) + geom_bar(position = "fill") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
q

#Data dummy untuk count pendidikan KRT berdasarkan status rawan
pendidikan <- datadf %>% 
  group_by(Pendidikan_KRT) %>% 
  count(Status_Rawan) %>% 
  mutate(ratio=scales::percent(n/sum(n)))

#Stacked bar chart dengan label
q + geom_text(data = pendidikan,aes(y=n,label=ratio),color="white",position=position_fill(vjust=0.5),size = 3)

#Stacked bar chart status rawan berdasarkan pendidikan kepala rumah tangga

#Basic stacked bar chart
o <- ggplot(datadf, aes(x=Status_Rawan, fill=Pendidikan_KRT)) + geom_bar(position = "fill")
o
#Data dummy untuk count status rawan berdasarkan pendidikan KRT
rawanpendidikan <- datadf %>% 
  group_by(Status_Rawan) %>% 
  count(Pendidikan_KRT) %>% 
  mutate(ratio=scales::percent(n/sum(n)))
#Stacked bar chart dengan label
o + geom_text(data=rawanpendidikan,aes(y=n,label=ratio),color="white",position=position_fill(vjust=0.5),size = 2)

Pertanyaan:
  1. Stacked bar chart mana yang lebih mudah dimengerti, status rawan dengan pendidikan terakhir krt atau pendidikan terakhir krt dengan status rawan?
  Jawaban: pendidikan terakhir krt dengan status rawan

#Mosaic Plot

library(ggmosaic)
table(datadf$Pendidikan_KRT)
#Jumlah pendidikan terbanyak SD, jumlah pendidikan sedikit Tidak sekolah
ggplot(data = datadf) + geom_mosaic(aes(x = product(Pendidikan_KRT), fill = Status_Rawan)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
