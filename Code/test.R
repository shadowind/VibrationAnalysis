rm(list=ls())
library(rJava)
library(RJDBC)
library(zoo)
library(sqldf)
library(VIF)
library(digest)
library(forecast)
library(survival)
library(foreach)
library(doParallel)
library(pracma)
require(MASS)


data_path <- 'C:/Users/I847777/Documents/Sherry_document/AAA-Work-SAP/5-Packaging/Saskpower/MockUpdata/';


#paths  Rafael
jdbcPath<-"C:/Program Files/sap/hdbclient/ngdbc.jar"
#csvPath <- "/Users/i836965/Documents/Project/2016/2017/VIRBRATION_DEMO/"
#dataFolder <- 'C:/Users/I836965/Documents/Project/2017/VIRBRATION_DEMO/'
#codeFolder <- 'C:/Users/I836965/Documents/Project/2017/VIRBRATION_DEMO/'
#resultFolder<- 'C:/Users/I836965/Documents/Project/2017/VIRBRATION_DEMO/'
#C:/Users/I836965/Documents/Project/2016/INSURANCE/Rscripts
# Connection to HANA database
#Host name: usphlhanapio01.phl.sap.corp
#Instance Number: 03
#User name: NAVISTAR
#Navistar: Navistar3


host <- "mo-77027f74b.mo.sap.corp"
port <- "30215"
uid <- "I836965"
pwd <- "ArDi2005"
jdbcDriver <- JDBC("com.sap.db.jdbc.Driver", jdbcPath,identifier.quote="'")
ch <- dbConnect(jdbcDriver, paste("jdbc:sap:", host, ":", port, sep=""), uid, pwd)
sqlW <- "select * from SASKPOWER.SPECTRA_JAN" ;
input_spectra <- dbGetQuery(ch, sqlW);
save(input_spectra,file=paste0(data_path,'spectra.RData'))


sqlW <- "select * from SASKPOWER.VELOCITY_JAN" ;
input_velocity <- dbGetQuery(ch, sqlW);
save(input_velocity,file=paste0(data_path,'velocity.RData'))


#### odbc Version
library(RODBC)
# pre-requisite: add ODBC connection in system

odbc_driver <- "SB2" # The ODBC name in odbc driver
uid <- "I836965"
pwd <- "ArDi2005"

ch<-odbcConnect(odbc_driver ,uid= uid,pwd=pwd)
# Find all tables in a schema
# table <- sqlTables(ch, schema = "SASKPOWER")

data<-sqlFetch(ch,"SASKPOWER.SPECTRA_JAN")





load(file=paste0(data_path,'velocity.RData'))
load(file=paste0(data_path,'spectra.RData'))

numero_columnas <- ncol(input_velocity)
numero <- numero_columnas - 1

tmax <- 6445.312
#Sample rate
rate = tmax/numero/1000
rate_inv <- 1/rate
#158.8752
# in milisecond so *1000, to simulate the website 6.5 second display, multiply 2
tmax <- rate*(numero)*1000 
# 6445.312
k <- 1:(numero)

input_velocity$TIEMPO <- as.POSIXct(input_velocity$TIEMPO,format="%Y-%m-%d %H:%M:%S")  
#input_velocity$TIEMPO <- as.POSIXct("2016-01-06 05:39:00.0",format="%Y-%m-%d %I:%M:%S")  
input_spectra$TIEMPO <- as.POSIXct(input_spectra$TIEMPO,format="%Y-%m-%d %H:%M:%S")  


vel.subset <- as.numeric(input_velocity[1,2:numero_columnas])

tiempo <- (1:numero)*rate*1000

plot(tiempo,vel.subset,type="l")
vel_temp_new <- data.frame("TIME" = tiempo,"VELOCITY"=vel.subset)





norder <- 2^11
velocity.fft <- data.frame(0:(norder/2-1))

frequencies <- (seq(1, norder/2, length.out=norder/2)-1) / (rate*(numero))
spec.subset <- as.numeric(input_spectra[1,2:numero_columnas])

plot(frequencies,spec.subset,type="l")


n.rows <- nrow(input_spectra)
final_velocity <- abs(input_velocity[,2:numero_columnas])
final_spectra <- abs(input_spectra[,2:numero_columnas])
base_amplitude.fft <- final_spectra[1,]
final_amplitude.fft <- final_spectra[n.rows,]


initial_final_output <- data.frame("FREQUENCY" = frequencies,"BASE" = base_amplitude.fft,"FINAL" =final_amplitude.fft)
#write.csv(initial_final_output,paste0(path,'MyOutputFrequency.csv'),row.names=FALSE)

par(mfrow=c(2,1))
plot(frequencies,base_amplitude.fft,type="l")
plot(frequencies,final_amplitude.fft,type="l")

tindex <- input_spectra$TIEMPO
power_velocity <- data.frame("TIME" = tindex,"MAX_VELOCITY"=double(length(tindex)))
power_spectra <- data.frame("TIME" = tindex,"POWER_SPECTRA"=double(length(tindex)))
amplitude_spectra_max <- data.frame("TIME" = tindex,"AMPLITUDE_SPECTRA"=double(length(tindex)))
l2_spectra_max <- data.frame("TIME" = tindex,"L2_SPECTRA"=double(length(tindex)))

titulo <- as.character("1A_DE_11A0")
par(mfrow=c(1,1))

for (i in 1:n.rows) {
  yyy <- input_spectra$TIEMPO[i]
  #xxx <- as.numeric(input_spectra[i,2:numero_columnas])
  amplitude.vel <- max(final_velocity[i,])
  power_velocity$MAX_VELOCITY[i]  = amplitude.vel
  
  amplitude.fft <- (final_spectra[i,])
  power_spectra$POWER_SPECTRA[i]  = sum(final_spectra[i,])
  amplitude_spectra_max$AMPLITUDE_SPECTRA[i]  = max(amplitude.fft)
  temp <- base_amplitude.fft-amplitude.fft
  l2_spectra_max$L2_SPECTRA[i] <- sqrt(sum((base_amplitude.fft-amplitude.fft)^2))
  
  if (i%%60 == 0) {
    par(mfrow=c(2,1))
    
    plot(frequencies,amplitude.fft,type="l",main = paste0('System ',titulo, '/n','Time:', tindex[i]) ,xlab = "Frequency (Hz)", ylab = "Velocity magnitude (mm/s)" )
    plot(frequencies,temp,t="l",main = paste0('System ',titulo, '/n','Time:', tindex[i]) ,xlab = "Frequency (Hz)", ylab = "Perturbation of Velocity (mm/s)" )
    print(c(i,yyy))
  }
  
}

power_velocity$TIME <- tindex
power_spectra$TIME <- tindex
amplitude_spectra_max$TIME <- tindex
l2_spectra_max$TIME <- tindex

par(mfrow=c(4,1))
plot(power_velocity,t="l",ylim=c(0,30),main=titulo)
plot(power_spectra,t="l",ylim=c(0,30),main=titulo)
plot(amplitude_spectra_max,t="l",ylim=c(0,4),main=titulo)
plot(l2_spectra_max,t="l",ylim=c(0,4),main=titulo)

par(mfrow=c(4,1))
plot(power_velocity,t="l",main=titulo)
plot(power_spectra,t="l",main=titulo)
plot(amplitude_spectra_max,t="l",main=titulo)
plot(l2_spectra_max,t="l",main=titulo)





for (i in 1:n.rows) {
  #yyy <- input_spectra$TIEMPO[i]
  #xxx <- as.numeric(input_spectra[i,2:numero_columnas])
  amplitude.vel <- max(final_velocity[i,])
  power_velocity$MAX_VELOCITY[i]  = amplitude.vel
  
  amplitude.fft <- (final_spectra[i,])
  power_spectra$POWER_SPECTRA[i]  = sum(final_spectra[i,])
  amplitude_spectra_max$AMPLITUDE_SPECTRA[i]  = max(amplitude.fft)
  temp <- base_amplitude.fft-amplitude.fft
  l2_spectra_max$L2_SPECTRA[i] <- sqrt(sum((base_amplitude.fft-amplitude.fft)^2))
  

}



n.col = ncol(input_spectra)
spectra <- as.matrix(input_spectra[,2:n.col])
velocity <- as.matrix(input_velocity[,2:n.col])
time <- as.POSIXct(input_velocity$TIEMPO,format="%Y-%m-%d %H:%M:%S")  

max_velocity <- apply(velocity, 1,max)
ampil_sqrt <- apply(spectra, 1,function(x) sqrt(max(x)))
power <- apply(spectra, 1,function(x) sum(x^2))
base <- as.vector(spectra[1,])
diff <- sweep(spectra , 2 , base)
l2_spectra_max <- apply(diff,1, function(x) sqrt(sum((x)^2)))

result<- data.frame(cbind(time, max_velocity,ampil_sqrt,power,l2_spectra_max))


l2_spectra_max$L2_SPECTRA[1:10]
perturbation[1:10]




time = 'test'
max_velocity=12.4
ampil_sqrt=32.3
power =32.22
l2_spectra_max =1.2 

result<- data.frame(cbind(time, max_velocity,ampil_sqrt,power,l2_spectra_max))
