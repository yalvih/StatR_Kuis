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

#1

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

ggplot(boston, aes(x = zn))+
  geom_density()+
  ggtitle("Piechart Proporsi Tempat Tinggal")

attach(BostonHousing)
shapiro.test(zn)

#eksplorasi NOX
summary(boston$nox)

boston$cat_nox = "a"
for (i in 1:dim(BostonHousing)[1]) {
  if(boston$nox[i] >= 0.0 & boston$nox[i] <= 0.53){
    boston$cat_nox[i] = "Rendah"
  } else if(boston$nox[i] > 0.53 & boston$nox[i] <= 0.62) {
    boston$cat_nox[i] = "Sedang"
  } else {
    boston$cat_nox[i] = "Tinggi"
  }
}

par(mfrow = c(2,2))
h1 <- hist(nox, 
           main = "Histogram Kadar Nitrat Oksida",
           xlab = "Tingkat Konsentrasi")
xfit<-seq(min(nox),max(nox),length=40)
yfit<-dnorm(xfit,mean=mean(nox),sd=sd(nox))
yfit<-yfit*diff(h1$mids[1:2])*length(nox)
lines(xfit, yfit, col="red", lwd=2)

#eksplorasi RAD
ggplot(boston, aes(x = "", y = rad,fill=factor(rad)))+
  geom_bar(stat = "identity", width = 1)+
  ggtitle("Piechart Proporsi Tempat Tinggal")+
  guides(fill=guide_legend("Tipe Proporsi Tanah"))+
  coord_polar("y",start=0)+
  theme_void()+
  scale_fill_brewer(palette="Set3")

#eksplorasi LSTAT
summary(lstat)

boston$cat_lstat = "a"
for (i in 1:dim(BostonHousing)[1]) {
  if(boston$lstat[i] >= 0.0 & boston$lstat[i] <= 11.36){
    boston$cat_lstat[i] = "Rendah"
  } else if(boston$lstat[i] > 11.36 & boston$lstat[i] <= 16.95) {
    boston$cat_lstat[i] = "Sedang"
  } else {
    boston$cat_lstat[i] = "Tinggi"
  }
}

ggplot(boston, aes(x=cat_lstat, y=lstat))+
  geom_boxplot()+
  xlab("Kategori Tingkat Status Rendah")+
  ylab("Tingat Angka Status Rendah")+
  ggtitle("Boxplot Tingkat Penduduk Berstatus Rendah")

#eksplorasi chas
ggplot(boston, aes(x = chas))+
  geom_dotplot(binaxis='x', stackdir='center')

ggplot(boston, aes(x = factor(chas)))+
  geom_bar(fill="orange")+
  xlab("Indeks Dekatnya Rumah dengan Sungai Charles")+
  ylab("Freq")+
  ggtitle("Barplot Jumlah Rumah dengan Dekatnya ")

#3

str(boston)
boston$cat_zn <- as.factor(boston$cat_zn)

#interaksi plot

attach(boston)
interaction.plot(x.factor = cat_zn, 
                 trace.factor = chas, 
                 response = nox, 
                 main="Interaction Plot of Nitrite Oxide by Proportion Tanah*Sungai Charles", 
                 xlab = "chas", ylab = "Nox (per 10 mil)",
                 col=c("red","blue","green"))

interaction.plot(x.factor = chas, 
                 trace.factor = cat_zn, 
                 response = nox, 
                 main="Interaction Plot of Nitrite Oxide by Proportion Tanah*Sungai Charles", 
                 xlab = "cat_zn", ylab = "Nox (per 10 mil)",
                 col=c("red","blue","green"))


interaction.plot(x.factor = rad, 
                 trace.factor = chas, 
                 response = medv, 
                 main="Interaction Plot of Median Harga Rumah by Rad*Sungai Charles", 
                 xlab = "rad", ylab = "Nox (per 10 mil)",
                 col=c("red","blue","green"))

interaction.plot(x.factor = chas, 
                 trace.factor = rad, 
                 response = medv, 
                 main="Interaction Plot of Median Harga Rumah by Rad*Sungai Charles", 
                 xlab = "chas", ylab = "medv",
                 col=c("red","blue","green"))

#4

str(boston)
#uji statistik
#dketahui 22 untuk niali diujinya. Perlu ada ui statistik t-test

# Hipotesis Nullnya adalaha apakah sama dengan 22.
# Hipotesis Alt adalah apakah lebih besar dari 22

attach(boston)

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

#asumsikan data sudah berpasangan dan hasilnya pun sama
var.test(boston$medv,corrected$cmedv,alternative = "two.sided")

#t.test berpasangan

# asumsikan h0 adalah apakah data tersebut sama dengan 10
# ha adalah data tersebut lebih kecil dari 10

t.test(corrected$cmedv,boston$medv,alternative = "less",var.equal = T,mu=10)

#hasilnya nilai p valuenya lebih kecil dari h0 nya yang berarti H0 ditolak. Ha diterima lebih kecil dari 10.

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

detach(boston)
#6

#a
attach(boston)
boston.in1 <- lm(medv ~ crim + cat_zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + b + lstat, data = boston)
summary(boston.in1)

#persamaan matematisnya adalah

# nilaiMed = 39.59 - 0.11*crim - 3.36*cat_znKecil - 3.05*cat_znSedang + 0.02*indus + 2.32chas1 - 17.52*nox + 3.72*rm + 0.001*age
#            - 1.46*dis + 1.39*rad2 + 4.62*rad3 + 2.16*rad4 + 2.77*rad5 + 1.24*rad6 + 5.61*rad7 + 4.82*rad8 + 7.84*rad24 - 0.01*tax
#            - 1.04*ptratio + 0.01*b - 0.52*lstat

boston.in2 <- lm(medv ~ crim + cat_zn + chas + nox + rm + dis + rad + ptratio + b + lstat, data = boston)
summary(boston.in2)

boston.in3 <- lm(medv ~ crim + chas + nox + rm + dis + ptratio + b + lstat, data = boston)
summary(boston.in3)

boston.in4 <- lm(medv ~ chas + nox + rm + dis + ptratio + lstat, data = boston)
summary(boston.in4)

boston.in5 <- lm(medv ~ nox + rm + dis + ptratio + lstat, data = boston)
summary(boston.in5)

boston.int1 <- lm(medv ~ crim*cat_zn, data = boston)
summary(boston.int1)

boston.int2 <- lm(medv ~ crim*nox, data = boston)
summary(boston.int2)

boston.int3 <- lm(medv ~ nox*rm, data = boston)
summary(boston.int3)

boston.int4 <- lm(medv ~ rm*lstat, data = boston)
summary(boston.int4)

boston.int5 <- lm(medv ~ b*dis, data = boston)
summary(boston.int5)

#aic
AIC(boston.in1)
AIC(boston.in2)
AIC(boston.in3)
AIC(boston.in4)
AIC(boston.in5)

#aic interaksi
AIC(boston.int1)
AIC(boston.int2)
AIC(boston.int3)
AIC(boston.int4)
AIC(boston.int5)

#r_squared
summary(boston.in1)
summary(boston.in2)
summary(boston.in3)
summary(boston.in4)
summary(boston.in5)
summary(boston.int1)
summary(boston.int2)
summary(boston.int3)
summary(boston.int4)
summary(boston.int5)

#hanya eksperimental saja
boston.in6 <- lm(medv ~ nox + rm*lstat + dis + ptratio, data = boston)
summary(boston.in6)

#b
stepwise_in1 <- step(boston.in1,direction = "both")
summary(stepwise_in1)
AIC(stepwise_in1)
extractAIC(stepwise_in1)

stepwise_int4 <- step(boston.int4,direction = "both")
summary(stepwise_int4)
AIC(stepwise_int4)
extractAIC(stepwise_int4)

backward_in1 <- step(boston.in1,direction = "backward")
summary(stepwise_in1)
AIC(stepwise_in1)
extractAIC(stepwise_in1)

backward_int4 <- step(boston.int4,direction = "backward")
summary(stepwise_int4)
AIC(stepwise_int4)
extractAIC(stepwise_int4)

