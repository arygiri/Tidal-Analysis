# contoh pasut jakarta ----------------------------------------------------
setwd("D:/Documents/belajar R/PASUT")
#import dataset
library(oce)
library(readxl)
library(lubridate)
library(tidyr)
library(tidyverse)
library(zoo)
pasut <- read.csv("Data_Pasut_Jakarta.csv")
str(pasut)
view(pasut)
pasut$Tahun <- as.character(pasut$Tahun)
pasut$Bulan <- as.character(pasut$Bulan)
pasut$Hari <- as.character(pasut$Hari)
str(pasut)

#membuat variabel baru berisi tanggal dan jam
pasut2 <- unite(pasut, tanggal_pasut, Bulan, Hari, Tahun, sep="/")
#convert numerik menjadi jam
pasut2$jam2 <- hms::hms(lubridate::seconds_to_period(floor(pasut2$Jam*60*60)))
str(pasut2)
#convert string ke date
pasut2$tanggal_pasut <- as.Date(pasut2$tanggal_pasut, format = "%m/%d/%Y")
#merging date dan time
pasut2$tanggal_pasut <- as.POSIXct(paste
                                   (pasut2$tanggal_pasut, pasut2$jam2),
                                   format="%Y-%m-%d %H:%M:%S")
#menghilangkan time zone
strftime(pasut2$tanggal_pasut)
#menghapus kolom yg tidak perlu
pasut2 <- pasut2[,c(-1,-3,-5)]
view(pasut2)
str(pasut2)

tanggal_pasut2 <- pasut2$tanggal_pasut
elevasi2 <- pasut2$elevasi..m.

#buat line chart
par(mar = c(5,5,4,4))
par(cex.axis=1.2, cex.lab = 1.8)
oce.plot.ts(tanggal_pasut2, elevasi2, xlab="Month", ylab= "Elevation(m)",
            col = "black", mar = par("mar"))

#buat boxplot pasut
boxplot(pasut2$elevasi..m., main="Boxplot Data Water Level", ylab=
          "Water Level (m)", col="blue")
#cleaning data outlier
outliers <- boxplot.stats(pasut2$elevasi..m.)$out
outliers
#mencari baris yang mengandung outlier
outlier_raw <- which(pasut2$elevasi..m.%in% c(outliers))
outlier_raw
## Replace the outliers with NA
pasut2[pasut2$elevasi..m. %in% outliers, "elevasi..m."] = NA


#menghapus NA di data awal
pasut2 <- pasut2[-1:-169,]
head(pasut2)
tail(pasut2)
#menampilkan baris yang terdapat NA
pasut2[is.na(pasut2$elevasi..m.), ] 
#merubah label baris agar urut mulai dr 1 kembali
pasut2 <- pasut2 %>% as.data.frame(row.names = 1:nrow(.))
#mengganti nilai NA dengan interpolasi 
library(zoo)
pasut2$elevasi..m. <- transform(pasut2, elevasi..m. = na.approx(elevasi..m.))
str(pasut2)
plot(pasut2$elevasi..m.$tanggal_pasut, pasut2$elevasi..m.$elevasi..m.,  
     col="black", type= "l", ylab= "Water Level (m)",
     xlab= "Month")

# analisis pasut dengan package oce ---------------------------------------
#mencari nilai MSL (Mean Sea Level)
mean(pasut2$elevasi..m.$elevasi..m.)
#elevasi = water level-MSL
pasut2$elevasi <- pasut2$elevasi..m.$elevasi..m.-
  mean(pasut2$elevasi..m.$elevasi..m.)
view(pasut2)
#menghaspus kolom elevasi..m.
pasut2 <- pasut2[,-2]
plot(pasut2$tanggal_pasut, pasut2$elevasi,  col="black", type= "l", 
     ylab= "Elevation (m)", xlab= "Month")

#mencari konstanta harmonik dengan fungsi tidem()
tide <- tidem(pasut2$tanggal_pasut, pasut2$elevasi)

summary(tide)#untuk menampilkan konstanta harmonik


#mencari tipe pasut dengan bilangan formzhal
#F= K1 + O1/ M2+S2
F <- (0.269 + 0.155)/(0.0551+0.0507)
#F 4.007561
#F>3 maka tipe pasutnya diurnal (1 kali pasang dan 1 kali surut)

#membuat variabel model(pasut prediksi)
model <- predict(tide)
model
str(model)
#membuat variabel observasi
obs <- pasut2$elevasi
str(obs)

#membuat variabel waktu
waktu <- pasut2$tanggal_pasut

#plot ts obs-mode/residu
#jpeg("obs-mode.jpeg", quality = 250, width = 1100, height = 800)
par(mar = c(5,5,4,4))
par(cex.axis=1.2, cex.lab = 1.8)
oce.plot.ts(waktu, obs - model, xlab="Month", ylab= "obs-prection (m)",
            col = "black", mar = par("mar"))
#dev.off()

##menampilkan selisih obs-model terbesar
i <- which.max(obs-model)
waktu[i]#tertinggi saat tanggal 2004-08-22 01:00:00

#mencari nilai RMSE (root mean square error)
sqrt(mean((obs - model)^2))
#RMSE = 0.1309578



#membuat grafik elevasi model vs observasi
#jpeg("my_plot.jpeg", quality = 300, width = 1100, height = 800)
par(mar = c(5, 5,4,4))
par(cex.axis=1.2, cex.lab = 1.2)
plot(waktu, model,  col="chartreuse3", type= "l", ylab= "Elevation (m)", 
     xlab= "Month",ylim=c(-1,1)) # Draw first line

lines(waktu, obs, type = "l", col = "blue3") # Add second line

legend("topleft",  # Add legend to plot
       legend = c("model", "observasi"),
       col = c("chartreuse3", "blue3"), border= "black", 
       xjust= 0.5, yjust=0.5,
       lty = 1, horiz=T)
#dev.off()

#jpeg("elevasi.jpeg", quality = 300, width = 1100, height = 800)
par(mar = c(5, 5,4,4))
par(cex.axis=1.5, cex.lab = 1.5)
plot(waktu, obs, type="l", ylab="Elevation (m)", ylim=c(-1,1),
     xlab="Month", col= "black")
#dev.off()

#membuat grafik psut
LLWL <- rep(min(pasut2$elevasi), 6097)
MSL <- rep(mean(pasut2$elevasi), 6097)
HHWL <- rep(max(pasut2$elevasi), 6097)
pasut$elevasi..m.

pasut2 <- cbind(pasut2, LLWL, MSL,HHWL)

#grafik pasut

ggplot(pasut2, aes(x = tanggal_pasut)) + 
  geom_line(aes(y = elevasi), colour = 'blue') +
  geom_line(aes(y=LLWL), colour="#CC0000", lwd=1)+
  geom_line(aes(y=MSL), colour="black", lwd=1)+
  geom_line(aes(y=HHWL), colour="chartreuse3", lwd=1)+
  xlab("Date")+ ylab("Elevation (m)")+
  scale_x_datetime(breaks= seq(min(pasut2$tanggal_pasut), 
                               max(pasut2$tanggal_pasut), length=10), 
                   date_labels="%b-%y")+
  scale_y_continuous(limits = c(-0.9, 0.9)) +
  theme(legend.position = "top")+
  theme_bw()

#membuat grafik elevasi pasut dgn ggplot2
#ga usah
data1 <- as.data.frame(model)
pasut2 <- cbind(pasut2,data1)
str(pasut2)
ggplot(pasut2, aes(x = tanggal_pasut, y = model)) + 
  geom_point(colour= "#CC0000", size=1.15) + 
  geom_line(aes(y = elevasi), colour = 'blue') + 
  xlab("Date")+ ylab("Elevation (m)")+
  scale_x_datetime(breaks= seq(min(pasut2$tanggal_pasut), 
                               max(pasut2$tanggal_pasut), length=10), 
                   date_labels="%b-%y")+
  scale_y_continuous(limits = c(-0.9, 0.9)) +
  theme_bw() 
str(pasut2)
#
#mengkonversi data menjadi sealevel
data <- as.sealevel(elevation = pasut2$elevasi, time = pasut2$tanggal_pasut)
plot(data)


# Forcasting Pasut --------------------------------------------------------
#menentukan waktu yang akan di ramalkan 
tanggal_prediksi <- seq(as.POSIXct("2023-10-1 01:00"), 
                        as.POSIXct("2023-10-31 00:00"),
                        by = "hour")


tanggal_prediksi
#forcasting pasut dengan fungsi predict dari package oce dan konversi menjadi data frame
prediksi <-as.data.frame(predict(tide, tanggal_prediksi))
#merubah struktur data tanggal menjadi data frame dan dijadikan satu dengan prediksi pasut
tanggal_prediksi <- as.data.frame(tanggal_prediksi)
prediksi <- cbind(tanggal_prediksi,prediksi)
#cek struktur data
str(prediksi)
#membuat grafik pasut dengan base R
par(mar = c(5, 5,4,4))
par(cex.axis=1.5, cex.lab = 1.5)
plot(prediksi$tanggal_prediksi, prediksi$`predict(tide, tanggal_prediksi)`, 
     type="l", ylab="Elevation (m)", ylim=c(-1,1),
     xlab="Date", col= "black")
#membuat grafik pasut dengan ggplot2
ggplot(prediksi, aes(x = tanggal_prediksi, 
                     y = `predict(tide, tanggal_prediksi)`)) + 
  geom_line(colour = 'blue') + 
  xlab("Date")+ ylab("Elevation (m)")+
  scale_x_datetime(breaks= seq(min(prediksi$tanggal_prediksi),
                               max(prediksi$tanggal_prediksi), length=10), 
                   date_labels="%b-%d")+
  scale_y_continuous(limits = c(-0.7, 0.7)) +
  theme_bw() 
library(writexl)
write_xlsx(prediksi, "prediksipasutjakarta.xlsx")





library(rmarkdown)
rmarkdown::render("modulpasut2023.R", output_format = "word_document")

