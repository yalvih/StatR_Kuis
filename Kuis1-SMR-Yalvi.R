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

str(BostonHousing)

#interaksi plot

attach(waist)
interaction.plot(Gender, Sport, Waistline, 
                 main="Interaction Plot of Waistline by Gender*Sport", 
                 xlab = "Gender", ylab = "Waistline (cm)",
                 col=c("red","blue","green"),
                 lty=c(1,2,3),pch=3,legend = F)

legend("topleft",inset = .1, 
       c("High","Low", "Medium"),
       col=c("red","blue","green"), 
       lty=c(1,2,3), pch=3, 
       title="Sport Intensity")

