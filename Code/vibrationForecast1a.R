# Clean workspace
rm(list=ls())
### motors model every 15 minutes
library(forecast);
data_path <- 'C:/Users/I847777/Documents/Sherry_document/AAA-Work-SAP/5-Packaging/Saskpower/MockUpdata/';
rsctip_path <- 'C:/Users/I847777/Documents/Sherry_document/AAA-Work-SAP/5-Packaging/Saskpower/R/';

source(paste0(rsctip_path, 'functions2.R'), echo=TRUE)
#source('~/Project/2017/VIRBRATION_DEMO/Rscript/functions.R', echo=TRUE)

# load(file=paste0(data_path,'spectraJUNE_FAIL.RData'))
# load(file=paste0(data_path,'velocityJUNE_FAIL.RData'))

load(file=paste0(data_path,'spectraJUNE.RData'))
load(file=paste0(data_path,'velocityJUNE.RData'))

# 
#### load(file=paste0(data_path,'spectra.RData'))
#### load(file=paste0(data_path,'velocity.RData'))
# 

#doPlot = TRUE
# look at 20 hours before the failure
endTimeSeries <- 30;
nminutos <- 10
niterations <- as.integer(endTimeSeries/(nminutos/60))

#orecast should be 12 hours, maybe more.
hforecast <- 12;
holdout <- 12;
# current time
rightNow <- Sys.time();
## locale-specific version of date()
current_time <- rightNow;


start <- data.frame()
max_date <- current_time + hrs(endTimeSeries)

tindex <- input_spectra$TIME
start_time <- data.frame("TIME" = tindex)
nlast <- nrow(input_spectra)


for (i in 1:nlast) {
  ijk = nlast-i+1
  start_time$TIME[ijk] <- max_date-mns(i)
  #  print(start_time$TIME[ijk])
}


input_spectra$TIME <- start_time$TIME
input_velocity$TIME <- start_time$TIME


spectral_norms <- TimeSeries(rightNow,endTimeSeries,input_spectra,input_velocity)
names(spectral_norms)

input <- spectral_norms[spectral_norms$TIME <= rightNow,];

max_vel <- input$MAXVELOCITY
sqrt_freq_amp <- input$SQRT_FREQ_AMP
power <- input$POWER
max_spectra <-input$L2_SPECTRA_MAX

par(mfrow=c(4,1))
boxplot(max_vel)
boxplot(sqrt_freq_amp)
boxplot(power)
boxplot(max_spectra)



UB <- 1.5
LB <- 1.5


a_max_vel <- ClearOutlier(max_vel,UB,LB)
a_sqrt_freq_amp <- ClearOutlier(sqrt_freq_amp,UB,LB)
a_power <- ClearOutlier(power,UB,LB)
a_max_spectra <- ClearOutlier(max_spectra,UB,LB)
hminutes = 60*hforecast
frequency <- 60*6
tiempo<-NULL
#par(mfrow=c(2,2))
#x11()

#doPlot == FALSE
par(mfrow=c(4,2))



streamPlot<-function(){
  for (ijk in 1:niterations) {
    par(mfrow=c(4,2))
    i = ijk - 1
    
    
    newtime <- rightNow + mns(i*nminutos)
    print(newtime)
    startTime <- newtime - hrs(holdout)
    
    # Get the modeling 12 hours period [startTime, newtime ]
    input <- spectral_norms[spectral_norms$TIME >= startTime & spectral_norms$TIME <= newtime,];
    
    tiempo <-  input$TIME
    index <- seq(from = startTime, to = newtime, by = "min");
    
    
    tsData1 <- ts(input$MAXVELOCITY, freq = frequency)
    titulo1 = "Maximum Amplitude (cm/s)"
    a_max1 <- a_max_vel
    xlabel1 = "Time (hours)"
    ylabel1 = "Maximum Amplitude of Velocity (mm/s)"
    
    
    tsData2 <- ts(input$SQRT_FREQ_AMP, freq = frequency)
    titulo2 = "Maximum Spectral Amplitude (cm/s)"
    a_max2 <- a_sqrt_freq_amp
    xlabel2 = "Time (hours)"
    ylabel2 = "Maximum Amplitude of Spectra (mm/s)"
    
    
    tsData3 <- ts(input$POWER, freq = frequency)
    titulo3 = "Power Spectra (cm/s)"
    a_max3 <- a_power
    xlabel3 = "Time (hours)"
    ylabel3 = "Power Spectra (mm/s)"
    
    
    
    tsData4 <- ts(input$L2_SPECTRA_MAX, freq = frequency)
    titulo4 = "Perturbation Spectra (cm/s)"
    a_max4 <- a_max_spectra
    xlabel4 = "Time (hours)"
    ylabel4 = "Perturbation Spectra (mm/s)"
    

    #mypath <- file.path("C:","Users","I836965","Documents","Project","2017","VIRBRATION_DEMO","Figs",paste(sprintf("myplot_%03d", ijk), ".jpg", sep = ""))
#mypath <- file.path("C:/Users/I847777/Documents/Sherry_document/AAA-Work-SAP/5-Packaging/Saskpower/MockUpdata/Figs",paste(sprintf("myplot_%03d", ijk), ".jpg", sep = ""))
    
 #   if (doPlot == TRUE) {
    #  jpeg(file=mypath,width = 1000, height = 1000, units = "px")  
      
   # }
    
    LinearRegression(tiempo,newtime,index,hminutes,UB,LB,startTime,tsData1,titulo1,a_max1,xlabel1,ylabel1,tsData2,titulo2,a_max2,xlabel2,ylabel2,tsData3,titulo3,a_max3,xlabel3,ylabel3,tsData4,titulo4,a_max4,xlabel4,ylabel4)

    #if (doPlot == TRUE) {
      #dev.off() ### ADD THIS CODE AT THE END
      
    #}
    
    Sys.sleep(0)
  }
}

x11()
streamPlot()





