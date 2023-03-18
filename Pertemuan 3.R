#Data COVID (Histogram dan Boxplot)
#Input data covid
covid <- read.csv("D:/Kuliah/!yes/R/Visdat/Dataframe/Data COOVID-19/COVID-19 Coronavirus.csv", sep=";")
str(covid)



#Input data continent
library(readxl)
continent <- read_excel(path="D:/Kuliah/!yes/R/Visdat/Dataframe/Data COOVID-19/continent.xlsx",col_names = TRUE)
continent
#ambil ISO-alpha3 Code dan Continent saja
continent <- cbind(continent$`ISO-alpha3 Code`, continent$Continent)
continent 
#Ubah jadi data frame
continent <- as.data.frame(continent)
continent
#Ubah colnames menyesuaikan dengan data covid
colnames(continent) <- c("ISO.3166.1.alpha.3.CODE", "continent")
continent


#Gabungkan data covid dan data continent dengan left joint
??merge()
covid2 <- merge(covid, continent, by="ISO.3166.1.alpha.3.CODE", all.x = T)
head(covid2)



#Cek apakah terdapat negara yang belum memiliki continent
covid2[is.na(covid2$continent),]
#Input nama continent secara manual
covid2[21,10] <- "North America"
covid2[62,10] <- "Africa"
covid2[147,10] <- "Africa"
covid2[196,10] <- "North America"
covid2[206,10] <- "Asia"
covid2[219,10] <- "Asia"
#Missing value teratasi
covid2[is.na(covid2$continent),]


#Tipe Data
str(covid2)


#Statistik 5 serangkai
summary(covid2)

#Histogram

library(ggplot2)
#Histogram population
ggplot(covid2, aes(x=Population)) + geom_histogram();
#Histogram Total Cases
ggplot(covid2, aes(x=Total.Cases)) + geom_histogram();
#Histogram Total Deaths
ggplot(covid2, aes(x=Total.Deaths)) + geom_histogram();



#Histogram populasi dengan tambah warna manual
ggplot(covid2, aes(x=Population)) + geom_histogram(fill="green", color = "black", bins=30) + theme_classic() + ylab("") + xlab("Populasi") 

#Box Plot

#Boxplot populasi dengan warna manual
ggplot(covid2, aes(x = Population)) + geom_boxplot(color="black", fill="green") + theme_classic()


#Boxplot Populasi berdasarkan continent
ggplot(covid2, aes(y = Population, x = continent)) + geom_boxplot(color="black", fill="green") + theme_classic();


#Boxplot Total Cases dengan continent
ggplot(covid2, aes(y = covid2$Total.Cases, x = continent)) + geom_boxplot(color="black", fill="yellow") + theme_classic();
#Boxplot Total Cases per Populasi dengan continent
ggplot(covid2, aes(y = covid2$TotÂ.Cases..1M.pop, x = continent)) + geom_boxplot(color="black", fill="yellow") + theme_classic()



#Boxplot Total Deaths dengan continent
ggplot(covid2, aes(y = covid2$Total.Deaths, x = continent)) + geom_boxplot(color="black", fill="Pink") + theme_classic();
#Boxplot Total Deaths per Populasi dengan continent
ggplot(covid2, aes(y = covid2$TotÂ.Deaths.1M.pop, x = continent)) + geom_boxplot(color="black", fill="Pink") + theme_classic()


#Data Susenas (Density Plot)

susenas<-read.csv("D:/Kuliah/!yes/R/Visdat/Dataframe/Data Susenas/Susenas Jawa Barat 2020.csv",sep = ";")
namakabupaten<-read.csv("D:/Kuliah/!yes/R/Visdat/Dataframe/Data Susenas/nama kabupaten.csv",sep = ",")
datakab <- namakabupaten[namakabupaten$nama_prov=="JAWA BARAT",]
datakab <- cbind(datakab$nama_kab,datakab$kab)
datakab <- as.data.frame(datakab)
colnames(datakab) <- c("Nama Kabupaten","KabKota")
datadf <- merge(susenas, datakab, by="KabKota", all.x = TRUE)
head(datadf)


#Density Plot manual
ggplot(data = datadf, aes(x = Luas_Lantai)) + geom_density()


#Density Plot dengan warna manual
p <- ggplot(data = datadf, aes(x = Luas_Lantai)) + geom_density(fill="#B5D5C5", color="black")
p



#Density Plot lebih menarik
p + theme_classic() + xlab("Luas Lantai") + ylab("")



# Plot densitas dengan beberapa kategori
q <- ggplot(datadf, aes(x = Luas_Lantai)) + geom_density(aes(color = Status_Rawan))
q


# Plot densitas dengan beberapa kategori Lebih menarik dan warna manual
q + xlab("Luas Lantai") + theme_classic() + scale_color_manual(values=c("Red", "Blue"))

#Box Plot

#Buat Boxplot luas lantai keseluruhan
#Buat Boxplot luas lantai berdasarkan status rawan
#Buat Boxplot luas lantai berdasarkan nama kabupaten



#Boxplot luas lantai dengan kabupaten dan status rawan
ggplot(datadf, aes(x =`Nama Kabupaten`, y = Luas_Lantai, color = Status_Rawan)) +  geom_boxplot()+ theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#Violin Plot

# Basic violin plot
ggplot(datadf, aes(x=Status_Rawan, y=Luas_Lantai)) + geom_violin()


# Warna berdasarkan status rawan
ggplot(datadf, aes(x=Status_Rawan, y=Luas_Lantai)) + geom_violin(aes(fill=Status_Rawan))


# Warna berdasarkan status rawan secara manual
ggplot(datadf, aes(x=Status_Rawan, y=Luas_Lantai)) + geom_violin(aes(fill=Status_Rawan)) + scale_fill_manual(values=c("red", "blue"))
