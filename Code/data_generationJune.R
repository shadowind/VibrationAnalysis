# Clean workspace
rm(list=ls())

memory.limit(size=12000) 
library(zoo)

hrs <- function(u) {
  x <- u * 3600
  return(x)
}

mns <- function(m) {
  x <- m * 60
  return(x)
}

library(xts)

merge2=function(x,y) {
  as.zoo(merge(as.xts(x), as.xts(y)))
}

data_path <- 'C:/Users/i836965/Documents/Project/2017/VIRBRATION_DEMO/';


##
path <- 'C:/Users/i836965/Documents/Project/2016/SASKPower/VIBRATION_SASK/1A_DE_11A0/';
vibration_data1 <- read.csv(paste0(path,'MyOutputInterpolationJRP_June.csv'))
zs_data1 <- data.frame(vibration_data1[,2:ncol(vibration_data1)])
mean_data1 <- apply(zs_data1, 2, FUN=max)
time_data1 <- data.frame(vibration_data1[,1])

velocity_data1 <- read.csv(paste0(path,'MyOutputVelocityJRP.csv'))
vs_data1 <- data.frame(velocity_data1[,2:ncol(velocity_data1)])



zs_data11 <- zs_data1
vs_data11 <- vs_data1





nombres <- c("TIME",colnames(zs_data11))
data1 <- data.frame(time_data1,zs_data11)
colnames(data1) <- nombres



vel.data1 <- data.frame(time_data1,vs_data11)
colnames(vel.data1) <- nombres

vibration_data <- rbind(data1,data1,data1,data1,data1)
velocity_data <- rbind(vel.data1,vel.data1,vel.data1,vel.data1,vel.data1)




d = sample(nrow(vibration_data))    
vibration_data <- vibration_data[d,]    
velocity_data <- velocity_data[d,]    


#select training sample

#vibration_data <- rbind(data1,data3mean,data2,data1,data3mean,data2,data3)

vibration_data$TIME <- as.POSIXct(vibration_data$TIME,format="%Y-%m-%d %H:%M:%S")
velocity_data$TIME <- as.POSIXct(vibration_data$TIME,format="%Y-%m-%d %H:%M:%S")

#write.csv(vibration_data,file=paste0(data_path,'mymatrixOriginal.csv'),row.names=FALSE, col.names=TRUE)


## locale-specific version of date()
current_time <- Sys.time()

nrow(vibration_data)
start <- data.frame()
max_date <- current_time + hrs(18)

tindex <- vibration_data$TIME
start_time <- data.frame("TIME" = tindex)
nlast <- nrow(vibration_data)


for (i in 1:nlast) {
  ijk = nlast-i+1
  start_time$TIME[ijk] <- max_date-mns(i)
    print(start_time$TIME[ijk])
}



input_spectra <- vibration_data
input_velocity <- velocity_data


numero_columnas <- ncol(input_spectra)
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

#input_velocity$TIEMPO <- as.POSIXct("2016-01-06 05:39:00.0",format="%Y-%m-%d %I:%M:%S")  
#input_spectra$TIEMPO <- as.POSIXct(input_spectra$TIEMPO,format="%Y-%m-%d %H:%M:%S")  


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




par(mfrow=c(2,1))
plot(frequencies,base_amplitude.fft,type="l")
plot(frequencies,final_amplitude.fft,type="l")

tindex <- input_velocity$TIME
power_velocity <- data.frame("TIME" = tindex,"MAX_VELOCITY"=double(length(tindex)))
power_spectra <- data.frame("TIME" = tindex,"POWER_SPECTRA"=double(length(tindex)))
amplitude_spectra_max <- data.frame("TIME" = tindex,"AMPLITUDE_SPECTRA"=double(length(tindex)))
l2_spectra_max <- data.frame("TIME" = tindex,"L2_SPECTRA"=double(length(tindex)))

titulo <- as.character("1A_DE_11A0")
par(mfrow=c(1,1))



input_spectra$TIME <- start_time
input_velocity$TIME <- start_time

names(input_spectra) <- nombres
names(input_velocity) <- nombres

n.col = ncol(input_spectra)
spectra <- as.matrix(input_spectra[,2:n.col])
velocity <- as.matrix(input_velocity[,2:n.col])
TIEMPO <-  input_spectra$TIME  
#TIEMPO <- as.character(TIEMPO)
MAXVELOCITY <- apply(velocity, 1,max)
SQRT_FREQ_AMP <- apply(spectra, 1,function(x) sqrt(max(x)))
POWER <- apply(spectra, 1,function(x) sum(x^2))
base <- as.vector(spectra[1,])
diff <- sweep(spectra , 2 , base)
L2_SPECTRA_MAX <- apply(diff,1, function(x) sqrt(sum((x)^2)))

tindex <- nrow(input_spectra)
power_velocity$TIME <- start_time$TIME
power_spectra$TIME <- start_time$TIME
amplitude_spectra_max$TIME <-start_time$TIME
l2_spectra_max$TIME <-start_time$TIME

power_velocity$MAX_VELOCITY <- MAXVELOCITY
power_spectra$POWER_SPECTRA <- SQRT_FREQ_AMP
amplitude_spectra_max$AMPLITUDE_SPECTRA <- POWER
l2_spectra_max$L2_SPECTRA <-L2_SPECTRA_MAX


par(mfrow=c(4,1))
plot(power_velocity,t="l",ylim=c(0,6),main=titulo)
plot(power_spectra,t="l",ylim=c(0,2),main=titulo)
plot(amplitude_spectra_max,t="l",ylim=c(0,7),main=titulo)
plot(l2_spectra_max,t="l",ylim=c(0,4),main=titulo)



nn <- nrow(spectra)
mm <- ncol(spectra)


xxx <- (runif(nn, min=0, max=1)) 
xxx <- xxx - (runif(nn, min=0, max=1))
xxx <- 1 + 0.01*xxx 
xxx <- xxx*(1+0.005*sin(2*(1:nn)*pi/(60)))





spectra <- ((spectra) * xxx)
velocity <- ((velocity) * xxx)

TIEMPO <- start_time$TIME

nstart <- which(TIEMPO==current_time)
nend <- length(TIEMPO)

secuencia <- nend - nstart
hours_to_resonance <- 2
nexponential = nend - hours_to_resonance*60 
# two hours prior 
# y = 6E-08x5 - 1E-05x4 + 0.0012x3 - 0.0435x2 + 0.5473x + 2.4582
x <- 1:(hours_to_resonance*60)
y <- 0.15*(6e-8*x^5- 1e-05*x^4 + 0.0012*x^3 - 0.0435*x^2 + 0.5473*x) + 1.0
xyz <- y

#two hours
xxxmax <- 18 + hours_to_resonance
nlinear = nend - xxxmax*60
ymax <- xxx[nlinear] + 0.15 
x <- 1:(xxxmax*60)
#eighteen hours
y <- (ymax - xxx[nlinear])/(xxxmax*60)*x + xxx[x]



xxx1 <- xxx
ijk = 0
for (i in nlinear:(nend-1)) {
  ijk = ijk + 1
  xxx1[i] = xxx[i]*y[ijk] 
  
}



xxx2 <- xxx1
ijk = 0
for (i in nexponential:(nend-1)) {
    ijk = ijk + 1
    xxx2[i] = xxx1[i]*xyz[ijk] 
  
}

xxx2[length(xxx2)] = 2*xxx2[length(xxx2)-1]

spectra <- as.matrix(input_spectra[,2:n.col]) * xxx2
velocity <- as.matrix(input_velocity[,2:n.col]) * xxx2


TIEMPO <- start_time$TIME
#TIEMPO <- as.character(TIEMPO)
MAXVELOCITY <- apply(velocity, 1,max)
SQRT_FREQ_AMP <- apply(spectra, 1,function(x) sqrt(max(x)))
POWER <- apply(spectra, 1,function(x) sum(x^2))
base <- as.vector(spectra[1,])
diff <- sweep(spectra , 2 , base)
L2_SPECTRA_MAX <- apply(diff,1, function(x) sqrt(sum((x)^2)))

tindex <- nrow(input_spectra)
power_velocity$TIME <- start_time$TIME
power_spectra$TIME <- start_time$TIME
amplitude_spectra_max$TIME <-start_time$TIME
l2_spectra_max$TIME <-start_time$TIME

power_velocity$MAX_VELOCITY <- MAXVELOCITY
power_spectra$POWER_SPECTRA <- SQRT_FREQ_AMP
amplitude_spectra_max$AMPLITUDE_SPECTRA <- POWER
l2_spectra_max$L2_SPECTRA <-L2_SPECTRA_MAX



par(mfrow=c(4,1))
plot(power_velocity,t="l",main=titulo)
plot(power_spectra,t="l",main=titulo)
plot(amplitude_spectra_max,t="l",main=titulo)
plot(l2_spectra_max,t="l",main=titulo)

input_spectra <- data.frame( start_time$TIME, spectra)
input_velocity <- data.frame( start_time$TIME, velocity)

names(input_spectra) <- nombres
names(input_velocity) <- nombres


save(input_spectra,file=paste0(data_path,'spectraJUNE.RData'))
save(input_velocity,file=paste0(data_path,'velocityJUNE.RData'))

vibration_data <- data.frame(power_velocity,power_spectra,amplitude_spectra_max,l2_spectra_max)

write.csv(vibration_data,file=paste0(data_path,'Vibration_data.csv'),row.names=FALSE, col.names=TRUE)

write.csv(input_spectra,file=paste0(data_path,'input_spectra_HANA.csv'),row.names=FALSE, col.names=FALSE)
write.csv(input_velocity,file=paste0(data_path,'input_velocity_HANA.csv'),row.names=FALSE, col.names=FALSE)




####
####

load(file=paste0(data_path,'spectraJUNE.RData'))
load(file=paste0(data_path,'velocityJUNE.RData'))

input_spectra_june <- input_spectra
input_velocity_june <- input_velocity


load(file=paste0(data_path,'spectra.RData'))
load(file=paste0(data_path,'velocity.RData'))

nend_fail <- nrow(input_spectra)
nbeginning_fail <- nend_fail - (nend-nexponential)




input_spectra_june[nexponential:nend,2:ncol(input_spectra)] <- 0.3*input_spectra[nbeginning_fail:nend_fail,2:ncol(input_spectra)]
input_velocity_june[nexponential:nend,2:ncol(input_spectra)] <- 0.7*input_velocity[nbeginning_fail:nend_fail,2:ncol(input_spectra)]

input_spectra_june$TIME <- start_time
input_velocity_june$TIME <- start_time

input_spectra <- input_spectra_june
input_velocity <- input_velocity_june

n.rows <- nrow(input_spectra)
final_velocity <- abs(input_spectra[,2:numero_columnas])
final_spectra <- abs(input_spectra[,2:numero_columnas])
base_amplitude.fft <- final_spectra[1,]
final_amplitude.fft <- final_spectra[n.rows,]





par(mfrow=c(2,1))
plot(frequencies,base_amplitude.fft,type="l")
plot(frequencies,final_amplitude.fft,type="l")



spectra <- as.matrix(input_spectra_june[,2:n.col]) 
velocity <- as.matrix(input_velocity_june[,2:n.col]) 


TIEMPO <- start_time$TIME
#TIEMPO <- as.character(TIEMPO)
MAXVELOCITY <- apply(velocity, 1,max)
SQRT_FREQ_AMP <- apply(spectra, 1,function(x) sqrt(max(x)))
POWER <- apply(spectra, 1,function(x) sum(x^2))
base <- as.vector(spectra[1,])
diff <- sweep(spectra , 2 , base)
L2_SPECTRA_MAX <- apply(diff,1, function(x) sqrt(sum((x)^2)))

tindex <- nrow(input_spectra)
power_velocity$TIME <- start_time$TIME
power_spectra$TIME <- start_time$TIME
amplitude_spectra_max$TIME <-start_time$TIME
l2_spectra_max$TIME <-start_time$TIME

power_velocity$MAX_VELOCITY <- MAXVELOCITY
power_spectra$POWER_SPECTRA <- SQRT_FREQ_AMP
amplitude_spectra_max$AMPLITUDE_SPECTRA <- POWER
l2_spectra_max$L2_SPECTRA <-L2_SPECTRA_MAX



par(mfrow=c(4,1))
plot(power_velocity,t="l",main=titulo)
plot(power_spectra,t="l",main=titulo)
plot(amplitude_spectra_max,t="l",main=titulo)
plot(l2_spectra_max,t="l",main=titulo)

input_spectra <- data.frame( start_time$TIME, spectra)
input_velocity <- data.frame( start_time$TIME, velocity)

names(input_spectra) <- nombres
names(input_velocity) <- nombres


save(input_spectra,file=paste0(data_path,'spectraJUNE_FAIL.RData'))
save(input_velocity,file=paste0(data_path,'velocityJUNE_FAIL.RData'))



write.table(input_spectra,file=paste0(data_path,'input_spectra_HANA.dat'),row.names=FALSE, col.names=FALSE)
write.table(input_velocity,file=paste0(data_path,'input_velocity_HANA.dat'),row.names=FALSE, col.names=FALSE)



load(file=paste0(data_path,'spectraJUNE_FAIL.RData'))
load(file=paste0(data_path,'velocityJUNE_FAIL.RData'))

nn <- ncol(input_spectra)-1


paste(sprintf("Z_%04d", 1:nn))
      
      
nombres <- c("TIME",as.character(paste(sprintf("Z_%04d", 1:nn))))
colnames(input_spectra) <- nombres
colnames(input_velocity) <- nombres



library(tidyr)
library(reshape2)

gathered.messy_spectra <- gather(input_spectra, key, value, -TIME)
head(gathered.messy_spectra)
gathered.messy_velocity <- gather(input_velocity, key, value, -TIME)



write.csv(gathered.messy_spectra,file=paste0(data_path,'input_spectra_HANA.csv'),row.names=FALSE, col.names=FALSE)
write.csv(gathered.messy_velocity,file=paste0(data_path,'input_velocity_HANA.csv'),row.names=FALSE, col.names=FALSE)


