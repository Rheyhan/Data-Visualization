library(readxl)
#df creation
  #importing data as df
indo<-read_excel(path="D:/Kuliah/!yes/R/Visdat/Dataframe/Indonesia.xlsx")
indo<-as.data.frame(indo)
malay<-read_excel(path="D:/Kuliah/!yes/R/Visdat/Dataframe/Malaysia.xlsx")
malay<-as.data.frame(malay)
  #date format edit
str(indo$date);str(malay$date)
indo$date <- as.Date(indo$date, format="%Y-%m-%d")
malay$date <- as.Date(malay$date, format="%Y-%m-%d")
str(indo$date);str(malay$date)
  #picking date interval
summary(indo$date);summary(malay$date)
hari <- data.frame(date=seq(as.Date("2020-04-01"), as.Date("2021-03-31"), by="day"))
nrow(hari)
covidindonesia <- merge(hari,indo,by="date",all.x=T)
covidmalaysia <- merge(hari,malay,by="date",all.x=T)
  #change to numeric
str(covidindonesia$new_cases);str(covidmalaysia$new_cases)
covidindonesia$new_cases <- as.numeric(covidindonesia$new_cases)
sum(is.na(covidindonesia$new_cases))
covidmalaysia$new_cases <- as.numeric(covidmalaysia$new_cases)
sum(is.na(covidmalaysia$new_cases))

#data visualization
library(ggplot2)
  #tanpa pemulusan
    #Plot harian jumlah kasus harian covid-19 di Indonesia
ggplot(covidindonesia, aes(x=date, y=new_cases)) + geom_point() +
  geom_line() + xlab("Tanggal") + ylab("Jumlah kasus baru harian") + 
  ggtitle("Indonesia") + theme_light()
    #Plot tanpa point
ggplot(covidindonesia, aes(x=date, y=new_cases)) +
  geom_line() + xlab("Tanggal") + ylab("Jumlah kasus baru harian") + 
  ggtitle("Indonesia") + theme_light()

    #Plot harian jumlah kasus harian covid-19 di Malaysia
ggplot(covidmalaysia, aes(x=date, y=new_cases)) + geom_point() +
  geom_line() + xlab("Tanggal") + ylab("Jumlah kasus baru harian") +
  ggtitle("Malaysia") + theme_light()
    #Plot tanpa point
ggplot(covidmalaysia, aes(x=date, y=new_cases)) + 
  geom_line() + xlab("Tanggal") + ylab("Jumlah kasus baru harian") +
  ggtitle("Malaysia") + theme_light()
  #pemulusan rolling mean
library(zoo)
    #Pemulusan dengan k = 3
a <- rollmean(covidindonesia$new_cases, k=3, fill=NA)
cbind(covidindonesia$new_cases,a)
covidindonesia$kasusbarupemulusan[2:365] <- rollmean(covidindonesia$new_cases, k=3, fill=NA)
cbind(covidindonesia$new_cases,covidindonesia$kasusbarupemulusan)
b <- rollmean(covidmalaysia$new_cases, k=3, fill=NA)
cbind(covidmalaysia$new_cases,b)
covidmalaysia$kasusbarupemulusan[2:365] <- rollmean(covidmalaysia$new_cases, k=3, fill=NA)
cbind(covidmalaysia$new_cases,covidmalaysia$kasusbarupemulusan)
      #plot
ggplot(covidindonesia, aes(x=date, y=kasusbarupemulusan)) + 
  geom_line() + xlab("Tanggal") + ylab("Jumlah kasus baru harian") +
  ggtitle("Indonesia dengan pemulusan k = 3") + theme_light()
ggplot(covidmalaysia, aes(x=date, y=kasusbarupemulusan)) + 
  geom_line() + xlab("Tanggal") + ylab("Jumlah kasus baru harian") +
  ggtitle("Malaysia") + theme_light()

    #Pemulusan dengan k = 3
c <- rollmean(covidindonesia$new_cases, k=14, fill=NA)
cbind(covidindonesia$new_cases,c)
covidindonesia$kasusbarupemulusan2[8:365] <- rollmean(covidindonesia$new_cases, k=14, fill=NA)
cbind(covidindonesia$new_cases,covidindonesia$kasusbarupemulusan2)
covidmalaysia$kasusbarupemulusan2[8:365] <- rollmean(covidmalaysia$new_cases, k=14, fill=NA)
cbind(covidmalaysia$new_cases,covidmalaysia$kasusbarupemulusan2)
d <- rollmean(covidindonesia$new_cases, k=10, fill=NA)
d
covidindonesia$kasusbarupemulusan10[6:365] <- rollmean(covidindonesia$new_cases, k=10, fill=NA)
cbind(covidindonesia$new_cases,covidindonesia$kasusbarupemulusan10)

#Indonesia
ggplot(covidindonesia, aes(x=date, y=kasusbarupemulusan)) + 
  geom_line() + xlab("Tanggal") + ylab("Jumlah kasus baru harian") +
  ggtitle("Indonesia dengan pemulusan k = 3") + theme_light()
ggplot(covidindonesia, aes(x=date, y=kasusbarupemulusan2)) + 
  geom_line() + xlab("Tanggal") + ylab("Jumlah kasus baru harian") +
  ggtitle("Indonesia dengan pemulusan k = 14") + theme_light()

#Malaysia
ggplot(covidmalaysia, aes(x=date, y=kasusbarupemulusan)) + 
  geom_line() + xlab("Tanggal") + ylab("Jumlah kasus baru harian") +
  ggtitle("Malaysia pemulusan k = 3") + theme_light()
ggplot(covidmalaysia, aes(x=date, y=kasusbarupemulusan2)) + 
  geom_line() + xlab("Tanggal") + ylab("Jumlah kasus baru harian") +
  ggtitle("Malaysia pemulusan k =14") + theme_light()

#time series with plot area
ggplot(covidindonesia, aes(x=date, y=kasusbarupemulusan2)) + 
  geom_line() + geom_area(fill="light blue", alpha=0.3) + xlab("Tanggal") + ylab("Jumlah kasus baru harian") +
  ggtitle("Indonesia pemulusan dengan k = 14") + theme_light()
ggplot(covidmalaysia, aes(x=date, y=kasusbarupemulusan2)) + 
  geom_line() + geom_area(fill = "orange", alpha=0.3) + xlab("Tanggal") + ylab("Jumlah kasus baru harian") +
  ggtitle("Malaysia pemulusan dengan k = 14") + theme_light()

#multiple time series plot
ggplot(data=covidindonesia, aes(x=date)) +
  geom_line(aes(y=kasusbarupemulusan2, color = "Indonesia"), lwd = 1.2) +
  geom_line(aes(y=covidmalaysia$kasusbarupemulusan2, color = "Malaysia"), lwd=1.2) +
  scale_color_manual(values=c("Indonesia"="red","Malaysia" = "blue")) +
  labs(x="Tanggal", y="Jumlah kasus baru harian", color = "Keterangan")

ggplot(data = covidindonesia, aes(x=date)) +
  geom_line(aes(y=kasusbarupemulusan2, col="Indonesia"), lwd=1.2) +
  geom_line(aes(y=covidmalaysia$kasusbarupemulusan2, col="Malaysia"), lwd=1.2) +
  scale_color_manual(values=c("Indonesia"="red","Malaysia"="blue")) + 
  labs(x="bulan-tahun", color="Keterangan")

min(covidindonesia$date);max(covidindonesia$date)+50
covidindonesia$kasusbarupemulusan2 #5316.3571
tail(covidindonesia$kasusbarupemulusan2,1)+25

#Plot
ggplot(data = covidindonesia, aes(x=date)) +
  geom_line(aes(y=kasusbarupemulusan2), col="red", lwd=1.2) +
  geom_line(aes(y=covidmalaysia$kasusbarupemulusan2), col="blue", lwd=1.2) +
  #atur xlim
  xlim(min(covidindonesia$date), max(covidindonesia$date)+50) + 
  #pengaturan text Indonesia
  geom_text(x=max(covidindonesia$date), y=tail(covidindonesia$kasusbarupemulusan2,1)+25,
            label='Indonesia', size=5, color="red", hjust = -0.1) +
  #pengaturan text malaysia
  geom_text(x=max(covidmalaysia$date), y=tail(covidmalaysia$kasusbarupemulusan2,1)-40,
            label='Malaysia', size=5, color="blue", hjust = -0.1) +
  xlab("Tanggal") + ylab("Jumlah kasus harian") + theme_light()

