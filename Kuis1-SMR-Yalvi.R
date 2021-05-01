#Nama : Yalvi Hidayat
#NPM  : 6181801044

#Nama : Michael Alvino W. D.
#NPM  : 6181801041

##############################

#install library
library(ggplot2)
library(datasets)
library(dplyr)
library(TeachingDemos)
library(mlbench)
library(mosaicData)

#dbInit
data("BostonHousing")
View(BostonHousing)
BostonHousing$rad <- as.factor(BostonHousing$rad)
factor(BostonHousing$rad, levels=c("1","2","3","4","5","6","7","8","24"))

str(BostonHousing)

#2

#eksploratif univariat

#eksplorasi dari data ZN ()
summary(BostonHousing$zn)
summary(BostonHousing$nox)
summary(BostonHousing$rad)
summary(BostonHousing$lstat)
summary(BostonHousing$chas)

#eksplorasi untuk melihat normalitas data

#eksplorasi ZN

# Asumsikan kita kelompokkan data proporsi tanahnya. Ada tanah yang ukurannya kecil, sedang ,dan besar
# Kecil adalah 0
# sedang adalah 12.50 - 50 (mean)
# Besar adalah 50 keatas

boston <- data.frame(BostonHousing)
boston$cat_zn = "a"
for (i in 1:dim(BostonHousing)[1]) {
  if(boston$zn[i] >= 0.0 & boston$zn[i] <= 12.50){
    boston$cat_zn[i] = "Kecil"
  } else if(boston$zn[i] > 12.50 & boston$zn[i] <= 35) {
    boston$cat_zn[i] = "Sedang"
  } else {
    boston$cat_zn[i] = "Besar"
  }
}

ggplot(boston, aes(x = "", y = cat_zn,fill=factor(cat_zn)))+
  geom_bar(stat = "identity", width = 1)+
  ggtitle("Piechart Proporsi Tempat Tinggal")+
  guides(fill=guide_legend("Tipe Proporsi Tanah"))+
  coord_polar("y",start=0)+
  theme_void()+
  scale_fill_brewer(palette="Set2")

attach(BostonHousing)
shapiro.test(zn)

#eksplorasi NOX
shapiro.test(nox)

#eksplorasi RAD
ggplot(boston, aes(x = factor(rad)))+
  geom_bar(fill="orange")+
  xlab("Indeks Aksesbilitas")+
  ylab("Freq")+
  ggtitle("Barplot Proporsi Tempat Tinggal")

#eksplorasi LSTAT
summary(lstat)

#eksplorasi chas
ggplot(boston, aes(x = chas))+
  geom_dotplot(binaxis='x', stackdir='center')

#3

str(boston)
boston$cat_zn <- as.factor(boston$cat_zn)

#interaksi plot

attach(boston)
interaction.plot(x.factor = cat_zn, 
                 trace.factor = chas, 
                 response = nox, 
                 main="Interaction Plot of Nitrite Oxide by Proportion Tanah*Sungai Charles", 
                 xlab = "Proprotion", ylab = "Nox (per 10 mil)",
                 col=c("red","blue","green"))

interaction.plot(x.factor = rad, 
                 trace.factor = chas, 
                 response = nox, 
                 main="Interaction Plot of Nitrite Oxide by Proportion Tanah*Sungai Charles", 
                 xlab = "Proprotion", ylab = "Nox (per 10 mil)",
                 col=c("red","blue","green"))

#4

#uji statistik
#dketahui 22 untuk niali diujinya. Perlu ada ui statistik t-test

# Hipotesis Nullnya adalaha apakah sama dengan 22.
# Hipotesis Alt adalah apakah lebih besar dari 22

attach(boston)
avg_medv <- mean(medv,na.rm = TRUE)
avg_medv

shapiro.test(medv)

par(mfrow = c(2,2))
h1 <- hist(medv, 
           main = "Histogram Rata-Rata Harga Rumah",
           xlab = "Harga Rata-rata Rumah Median")
xfit<-seq(min(medv),max(medv),length=40)
yfit<-dnorm(xfit,mean=mean(medv),sd=sd(medv))
yfit<-yfit*diff(h1$mids[1:2])*length(medv)
lines(xfit, yfit, col="red", lwd=2)

# Jika dengan a = 0.05
t.test(medv,mu=22,alternative = "greater")

# jika dengan a = 0.1
t.test(medv,mu=22,alternative = "greater",conf.level = 0.9)

# yang alpha 0.05 itu hipotesis nullnya diterima karena nilai p value nya lebih besar dari alphanya (0.09)
# yang alpha 0.1 itu hipotesis nullnya ditolak karena nilai p valuenya lebih kecil dari alphanya

#b
data_charles_near <- filter(boston, chas == 1)
data_charles_no_near <- filter(boston, chas == 0)

#diabaikan
shapiro.test(data_charles_near$medv)
shapiro.test(data_charles_no_near$medv)

#cek dahulu variansinya
var.test(data_charles_near$medv,data_charles_no_near$medv,alternative = "two.sided")

#karena hasil uji variansinya lebih kecil dari alpha, maka variansi kedua populasi itu berbeda
t.test(data_charles_near$medv,data_charles_no_near$medv)

#Hasilnya ternyata p value juga lebih rendah dari alphanya (0.05). Oleh karena itu H0 ditolak. Rumah yang dekat sungai charles daengan yang tidak dekat memang brbeda harganya

#c
corrected <- read.delim('corrected.txt')

#lakukan uji normalitas
shapiro.test(boston$medv)
shapiro.test(corrected$cmedv)

#asumsikan data sudah berpasangan dan hasilnya pun sama
var.test(boston$medv,corrected$cmedv,alternative = "two.sided")

#t.test berpasangan

# asumsikan h0 adalah apakah data tersebut sama
# ha adalah data tersebut berbeda

t.test(boston$medv,corrected$cmedv,alternative = "two.sided",paired = T)

#hasilnya nilai p valuenya lebih besar dari h0 nya yang berarti H0 diterima. Hal ini berarti data tersebut sama saja dan tidak berbeda sama sekali.

#5
#crim
ggplot(boston, aes(x=medv, y=crim))+ 
  geom_point()+
  xlab("Nilai Median")+
  ylab("Tingat Kriminalitas")+
  ggtitle("Scatterplot Hubungan Nilai Median Rumah dengan Tingkat Kriminalitas")

#nox
ggplot(boston, aes(x=medv, y=nox))+ 
  geom_point()+
  xlab("Nilai Median")+
  ylab("Tingat Kadar Nitrit Oxide")+
  ggtitle("Scatterplot Hubungan Nilai Median Rumah dengan Tingkat Kadar Nitrit Oxide")

#chas
ggplot(boston, aes(x=medv, y=chas))+
  geom_boxplot()+
  xlab("Nilai Median")+
  ylab("Jenis Rumah")+
  ggtitle("Scatterplot Hubungan Nilai Median Rumah dengan Dekatnya Rumah Sungai Charles")

#lstat
ggplot(boston, aes(x=medv, y=lstat))+
  geom_point()+
  xlab("Nilai Median")+
  ylab("Persentase Kalangan Rendah (dalam persen)")+
  ggtitle("Scatterplot Hubungan Nilai Median Rumah dengan Persentase Kalangan Rendah")

#b
ggplot(boston, aes(x=medv, y=b))+
  geom_point()+
  xlab("Nilai Median")+
  ylab("Proporsi Kalangan Ras Kulit Hitam")+
  ggtitle("Scatterplot Hubungan Nilai Median Rumah dengan Proporsi Kalangan Ras Kulit Hitam")

#tax
ggplot(boston, aes(x=medv, y=tax))+
  geom_point()+
  xlab("Nilai Median")+
  ylab("Indeks Pajak")+
  ggtitle("Scatterplot Hubungan Nilai Median Rumah dengan Indeks Pajak")

#rad
ggplot(boston, aes(x=medv, y=rad))+
  geom_boxplot()+
  xlab("Nilai Median")+
  ylab("Indeks Aksesbilitas ke Jalan Raya")+
  ggtitle("Scatterplot Hubungan Nilai Median Rumah dengan Indeks Aksesbilitas ke Jalan Raya")

#6

