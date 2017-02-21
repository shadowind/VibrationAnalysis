
rm(list=ls())

data_path <- 'C:/Users/I847777/Documents/Sherry_document/AAA-Work-SAP/5-Packaging/Saskpower/MockUpdata/';


####### (2016,Jan-05) #######
load(file=paste0(data_path,'velocity.RData'))
load(file=paste0(data_path,'spectra.RData'))

## The format of Jan data and Jun data are not same for "TIME", prepare it before next step
input_spectra$TIME<- input_spectra$TIME$TIME
input_velocity$TIME<- input_velocity$TIME$TIME
############################################



###### (Jun)  ###############
load(file=paste0(data_path,'velocityJUNE.RData'))
load(file=paste0(data_path,'spectraJUNE.RData'))
input_spectra <- output_spectra
input_velocity <- output_velocity
############################################

n.col = ncol(input_spectra)
spectra <- as.matrix(input_spectra[,2:n.col])
velocity <- as.matrix(input_velocity[,2:n.col])
TIEMPO <-  input_spectra$TIME

MAXVELOCITY <- apply(velocity, 1,max)
SQRT_FREQ_AMP <- apply(spectra, 1,function(x) sqrt(max(x)))
POWER <- apply(spectra, 1,function(x) sum(x^2))
base <- as.vector(spectra[1,])
diff <- sweep(spectra , 2 , base)
L2_SPECTRA_MAX <- apply(diff,1, function(x) sqrt(sum((x)^2)))

tindex <- nrow(input_spectra)

power_velocity <- data.frame(TIEMPO, MAXVELOCITY)
power_spectra <- data.frame(TIEMPO,SQRT_FREQ_AMP)
amplitude_spectra_max <- data.frame(TIEMPO,POWER)
l2_spectra_max <-  data.frame(TIEMPO,L2_SPECTRA_MAX)

par(mfrow=c(2,2))

plot(power_velocity,t="l",ylim=c(0,6), main='power_velocity')
plot(power_spectra,t="l",ylim=c(0,2),main='power_spectra')
plot(amplitude_spectra_max,t="l",ylim=c(0,7),main='amplitude_spectra_max')
plot(l2_spectra_max,t="l",ylim=c(0,4),main='l2_spectra_max')
title(paste('4 norms at',TIEMPO[1]),outer='TRUE',line = -1)


### Zoom in to see last day, last 12 hours, last 6 hours, last hour
i_lastday = tindex - 24*60
i_last12hours = tindex - 12*60
i_last6hours = tindex - 6*60
i_lasthour = tindex - 60

# # Create a new plot
# #plot.new()
# #frame()
# par(mfrow=c(4,1))
# plot(power_velocity,t="l", xlim = c(TIEMPO[i_lastday], TIEMPO[tindex]),main=paste('Last day'))
# plot(power_velocity,t="l", xlim = c(TIEMPO[i_last12hours], TIEMPO[tindex]),main=paste('Last 12 hours'))
# plot(power_velocity,t="l", xlim = c(TIEMPO[i_last6hours], TIEMPO[tindex]),main=paste('Last 6 hours'))
# plot(power_velocity,t="l", xlim = c(TIEMPO[i_lasthour], TIEMPO[tindex]),main=paste('Last hour'))
# title("Max velocity zoom in", outer=TRUE,line = -1)
# 
# plot(power_spectra,t="l", xlim = c(TIEMPO[i_lastday], TIEMPO[tindex]),main=paste('Last day'))
# plot(power_spectra,t="l", xlim = c(TIEMPO[i_last12hours ], TIEMPO[tindex]),main=paste('Last 12 hours'))
# plot(power_spectra,t="l", xlim = c(TIEMPO[i_last6hours ], TIEMPO[tindex]),main=paste('Last 6 hours'))
# plot(power_spectra,t="l", xlim = c(TIEMPO[i_lasthour ], TIEMPO[tindex]),main=paste('Last hour'))
# title("Power spectra zoom in", outer=TRUE,line = -1)
# 
# 
# plot(amplitude_spectra_max,t="l", xlim = c(TIEMPO[i_lastday ], TIEMPO[tindex]),main=paste('Last day'))
# plot(amplitude_spectra_max,t="l", xlim = c(TIEMPO[i_last12hours ], TIEMPO[tindex]),main=paste('Last 12 hours'))
# plot(amplitude_spectra_max,t="l", xlim = c(TIEMPO[i_last6hours ], TIEMPO[tindex]),main=paste('Last 6 hours'))
# plot(amplitude_spectra_max,t="l", xlim = c(TIEMPO[i_lasthour ], TIEMPO[tindex]),main=paste('Last hour'))
# title("Amplitude zoom in", outer=TRUE,line = -1)
# 
# 
# plot(l2_spectra_max,t="l", xlim = c(TIEMPO[i_lastday ], TIEMPO[tindex]),main=paste('Last day'))
# plot(l2_spectra_max,t="l", xlim = c(TIEMPO[i_last12hours ], TIEMPO[tindex]),main=paste('Last 12 hours'))
# plot(l2_spectra_max,t="l", xlim = c(TIEMPO[i_last6hours ], TIEMPO[tindex]),main=paste('Last 6 hours'))
# plot(l2_spectra_max,t="l", xlim = c(TIEMPO[i_lasthour ], TIEMPO[tindex]),main=paste('Last hour'))
# title("L2 spectra zoom in", outer=TRUE,line = -1)


### After looking at the previous data, 6 hours before the failure are most to my interest
### Plot 4 invariants 
plot(power_velocity,t="l", xlim = c(TIEMPO[i_last6hours], TIEMPO[tindex]),main=paste('Power velocity'))
plot(power_spectra,t="l", xlim = c(TIEMPO[i_last6hours], TIEMPO[tindex]),main=paste('Power spectra'))
plot(amplitude_spectra_max,t="l", xlim = c(TIEMPO[i_last6hours ], TIEMPO[tindex]),main=paste('Amplitude spectra max'))
plot(l2_spectra_max,t="l", xlim = c(TIEMPO[i_last6hours ], TIEMPO[tindex]),main=paste('L2 spectra'))
title(paste(TIEMPO[i_last6hours ],' to ', TIEMPO[tindex]), cex.main = 1.5, outer=TRUE,line = -1.5)

