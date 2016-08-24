install.packages("HistogramTools")
install.packages("forecast")
install.packages("scatterplot3d")
library(dplyr)
library(plyr)
library(ggplot2)
library(psych)
library(HistogramTools)
library(forecast)
library(plot3D)
library(scatterplot3d)

#############################################################
store <- read.csv("store_info.csv")                         #
mix <- read.csv("mixpanel_events.csv")                      #
mixc <- read.csv("mixpanel_events_type_and_combination.csv")#
#############################################################

#----------valid (0,1)------
po_st1 <- filter(store, Valid == 1)
id_1 <- select(po_st1, PoS)
po_st0 <- filter(store, Valid == 0)
id_0 <- select(po_st0, PoS)
an_1 <- po_st1$PoS
an_0 <- po_st0$PoS
#---------------------------
#--------Store - Mix (0,1)----
mix_1 <- filter(mix, PoS%in%an_1)
mix_0 <- filter(mix, PoS%in%an_0)
#-----------------------------
#--------conversión-----------
eve_1 <- as.numeric(mix_1$event)
eve_0 <- as.numeric(mix_0$event)
even_t1 <- data.frame(eve_1)
even_t0 <- data.frame(eve_0)
#-------------
mixt_1 <- mutate(mix_1, event=even_t1)
mixt_0 <- mutate(mix_0, event=even_t0)

#------------Visualización histogramas
PlotRelativeFrequency(hist(mixt_1$event%>%unlist),
                      xlab="Tipo de Evento (283)",main = "Tiendas Válidas", col="green")
lines(density(mixt_1$event%>%unlist), col="blue")
PlotRelativeFrequency(hist(mixt_0$event%>%unlist),
                      xlab="Tipo de Evento (283)",main = "Tiendas No Válidas", col="red")
lines(density(mixt_1$event%>%unlist), col="blue")
#-----------Visualización densidades
plot(density(mixt_1$event%>%unlist),
     xlab="Tipo de Evento (283)",main = "Comparación de Densidades",col="green")
lines(density(mixt_0$event%>%unlist), col="red")
#=======================
#---------correlación
evc <- mix$event
evc <- as.numeric(evc)
mixte <- mutate(mix,event=data.frame(evc))

ts <- mix$timestamp
ts <- as.numeric(ts)
mixts <- mutate(mixte,timestamp=data.frame(ts))

cor(mixts$timestamp, mixts$event)
cor.plot(cor(mixts$timestamp, mixts$event))

hist(mixts$event%>%unlist)
#--------------------
#---------------
med_1 <- numeric()
med_0 <- numeric()
sd_1 <- numeric()
sd_0 <- numeric()
#---------------
#----length
l_1 <- length(po_st1$PoS)
l_0 <- length(po_st0$PoS)
n <- l_1
m <- l_0
#-------------valid 1(
for(i in 1:n){
  st_1 <- filter(mix , PoS==an_1[i])
  sto_1 <- arrange(st_1 , timestamp)
  res_1 <- summary(sto_1$event)
  med_1[i] <- mean(res_1)
}

#-----------valid 0
for(j in 1:m){
  st_0 <- filter(mix , PoS==an_0[j])
  sto_0 <- arrange(st_0 , timestamp)
  res_0 <- summary(sto_0$event)
  med_0[j] <- mean(res_0)
}

mts_1 <- ts(med_1)
plot.ts(mts_1)
lines(mts_1)
qqnorm(mts_1, main = "Medias Categóricas Eventos Tiendas Válidas", col="green")
qqline(mts_1)
#---------------------------
mts_0 <- ts(med_0)
plot.ts(mts_0)
lines(mts_0)
qqnorm(mts_0,main = "Medias Categóricas Eventos Tiendas No Válidas", col="red")
qqline(mts_0)
#-----------------------------
#---------------------------
plot(forecast(med_1), col="green", main="Forecast Tiendas Válidas")
plot(forecast(med_0), col="red", main = "Forecast Tiendas No Válidas")

#------------------------------
size <- as.numeric(store$size)
hist(size)
store$Valid
meds <- c(med_1, med_0)

scatterplot3d(size,store$Valid,meds,box=FALSE, main="Perfiles de Usuario",
     xlab="x:Tamaño",ylab="y:Validez", zlab = "z:Medias Eventos")
