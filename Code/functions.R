LinearRegression <- function(tsData,index,hminutes,titulo,UB,LB,a_max,xlabel,ylabel,startTime,newtime,isPlot=TRUE){

#   
# fit<-stl(tsData,s.window="periodic",robust=TRUE)
# fit.trend<-fit$time.series[,"trend"]
# 
# res <- modelStlArimaMudpump(tsData, h = hminutes)
# 
# fit<-stl(tsData,s.window="periodic",robust=TRUE)
# m <- frequency(fit$time.series);
# n <- nrow(fit$time.series);
# lastseas <- rep(fit$time.series[n - (m:1) + 1, "seasonal"], trunc(1 + (h - 1)/m))[1:h];
# x.sa <- seasadj(fit);
# fcast <- forecast(x.sa, h = h);
# fcast$mean <- fcast$mean + lastseas;
# fcast$upper <- fcast$upper + lastseas;
# fcast$lower <- fcast$lower + lastseas;
# fcast$x <- ts(rowSums(fit$time.series));
# tsp(fcast$x) <- tsp(fit$time.series);
# fcast$method <- paste("STL + ", fcast$method);
# fcast$seasonal <- ts(lastseas[1:m], frequency = m, start = tsp(fit$time.series)[2] - 1 + 1/m);
# fcast$fitted <- fitted(fcast) + fit$time.series[, 1];
# fcast$residuals <- fcast$x - fcast$fitted;
# 
# # mmm <- data.frame("TIME_SERIES"=fcast$x,"FITTED" =fitted(fcast))
# # 
# # mmm1 <- data.frame("TIME_SERIES"=fcast$mean,"FITTED" =fcast$mean)
# # mmm <- rbind(mmm,mmm1)
# 
# if(isPlot) {
#   plot(fcast,ylim = c(a_max$LB_PR-IQ,IQ+a_max$UB_PR));
#   lines(fitted(fcast), col = "red");
#   text(paste("R2 = ", getRS(fcast$fitted, fcast$residuals), sep=""), x = 3, y = mean(fcast$fitted), col = "red");
#   
#   #row.names(fcast) <- index
#   # plot(res,t="l",main=titulo,xlab = xlabel,ylab = ylabel,ylim = c(a_max$LB_PR-IQ,IQ+a_max$UB_PR) )
#   #      xlim = c(startTime,newtime+mns(hminutes)))
#   lines(xxx$X,f.lm$fitted.values,col=4,lwd=3)
#   abline(h = a_max$LB_PR,  col = "blue")
#   abline(h = a_max$UB_PR,  col = "red")
#   abline(h = a_max$MEAN,  col = "green")
#   
#   
# }







index1 <- seq(from = startTime, to = newtime+mns(hminutes), by = "min");
  


IQ <- 0.5*(a_max$UB_PR-a_max$LB_PR)

xxx <- data.frame("X" = tiempo, "Y" = coredata(tsData))
xxx1 <- data.frame("X" = 1:length(tiempo), "Y" = coredata(tsData))


f.lm <- lm(xxx1$Y~xxx1$X)
intercept <-f.lm$coefficients[1]
slope <- f.lm$coefficients[2]
X1 <- slope*(1:length(index1)) +  intercept

xxx2 <- data.frame("X" = index1, "Y" = coredata(X1))


plot(xxx,t="l",main=titulo,xlab = xlabel,ylab = ylabel,ylim = c(a_max$LB_PR-IQ,IQ+a_max$UB_PR),
     xlim = c(startTime,newtime+mns(hminutes)))
#lines(xxx$X,f.lm$fitted.values,col=4,lwd=3)
lines(xxx2$X,xxx2$Y,col=4,lwd=3)
abline(a = a_max$LB_PR, b = 0, col = "blue")
abline(a = a_max$UB_PR, b = 0, col = "red")
abline(a = a_max$MEAN, b = 0, col = "green")



}


TimeSeries <- function(rightNow,endTimeSeries,input_spectra,input_velocity){
  onePeriod <- as.difftime(1, units = "mins");
  startTime <- min(input_spectra$TIME);
  
  ##############################################################################
  
  power_velocity <- data.frame("TIME" = tindex,"MAX_VELOCITY"=double(length(tindex)))
  power_spectra <- data.frame("TIME" = tindex,"POWER_SPECTRA"=double(length(tindex)))
  amplitude_spectra_max <- data.frame("TIME" = tindex,"AMPLITUDE_SPECTRA"=double(length(tindex)))
  l2_spectra_max <- data.frame("TIME" = tindex,"L2_SPECTRA"=double(length(tindex)))
  ##############
  
  
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
  
  power_velocity$TIME <- input_spectra$TIME
  power_spectra$TIME <- input_spectra$TIME
  amplitude_spectra_max$TIME <-input_spectra$TIME
  l2_spectra_max$TIME <-input_spectra$TIME
  
  power_velocity$MAX_VELOCITY <- MAXVELOCITY
  power_spectra$POWER_SPECTRA <- SQRT_FREQ_AMP
  amplitude_spectra_max$AMPLITUDE_SPECTRA <- POWER
  l2_spectra_max$L2_SPECTRA <-L2_SPECTRA_MAX
  
  
  
  
  titulo <- as.character("Vibration")
  
  par(mfrow=c(4,1))
  plot(power_velocity,t="l",main=titulo)
  plot(power_spectra,t="l",main=titulo)
  plot(amplitude_spectra_max,t="l",main=titulo)
  plot(l2_spectra_max,t="l",main=titulo)
  
  
  par(mfrow=c(1,1))
  spectral_norms <- data.frame(input_spectra$TIME,MAXVELOCITY,SQRT_FREQ_AMP,POWER,L2_SPECTRA_MAX)
  names(spectral_norms) <- c("TIME","MAXVELOCITY","SQRT_FREQ_AMP","POWER","L2_SPECTRA_MAX")
  
  return(spectral_norms)
  
}



ClearOutlier <- function(data_input,UB,LB){
  temp <- data_input
  LB_PR <- as.numeric(quantile(temp)[2] - LB*IQR(temp));
  UB_PR <- as.numeric(quantile(temp)[4] + UB*IQR(temp));
  xSD_before <- sd(temp);
  xInt_before <- (UB_PR - LB_PR)/sd(temp);
  xmean_before <-mean(temp);
  xmedian_before <-median(temp);
  bounds <- data.frame("LB_PR"=LB_PR,"UB_PR"=UB_PR,"MEAN" = xmean_before)
  return(bounds);
};


getRS <- function(f, r) {
  mss <- sum((f - mean(f))^2);
  rss <- sum(r^2);
  if(mss + rss == 0) {
    r.squared <- 0;
  } else {
    r.squared <- round(mss/(mss + rss), 5);
  }
  return(r.squared);
}

modelStlArimaMudpump <- function(tsData, h, isPlot = FALSE) {
  fit<-stl(tsData,s.window="periodic",robust=TRUE)
  m <- frequency(fit$time.series);
  n <- nrow(fit$time.series);
  lastseas <- rep(fit$time.series[n - (m:1) + 1, "seasonal"], trunc(1 + (h - 1)/m))[1:h];
  x.sa <- seasadj(fit);
  fcast <- forecast(x.sa, h = h);
  fcast$mean <- fcast$mean + lastseas;
  fcast$upper <- fcast$upper + lastseas;
  fcast$lower <- fcast$lower + lastseas;
  fcast$x <- ts(rowSums(fit$time.series));
  tsp(fcast$x) <- tsp(fit$time.series);
  fcast$method <- paste("STL + ", fcast$method);
  fcast$seasonal <- ts(lastseas[1:m], frequency = m, start = tsp(fit$time.series)[2] - 1 + 1/m);
  fcast$fitted <- fitted(fcast) + fit$time.series[, 1];
  fcast$residuals <- fcast$x - fcast$fitted;
  if(isPlot) {
    plot(fcast);
    lines(fitted(fcast), col = "red");
    text(paste("R2 = ", getRS(fcast$fitted, fcast$residuals), sep=""), x = 3, y = mean(fcast$fitted), col = "red");
  }
  return(fcast);
}

modelArimaNoSeason <- function(tsData, h, isPlot = FALSE) {
  n <- length(tsData);
  afit <- auto.arima(tsData);
  acast <- forecast(afit, h = h);
  if(isPlot) {
    plot(acast);
    lines(fitted(acast), col = "red");
    text(paste("R2 = ", getRS(acast$fitted, acast$residuals), sep=""), x = 3, y = mean(acast$fitted), col = "red");
  }
  return(acast);
}

modelArimaTrendMudpump <- function(tsData, h, isPlot = FALSE) {
  fit<-auto.arima(tsData)
  m <- frequency(fit$time.series);
  n <- nrow(fit$time.series);
  lastseas <- rep(fit$time.series[n - (m:1) + 1, "seasonal"], trunc(1 + (h - 1)/m))[1:h];
  fcast <- forecast(fit, h = h);
  fcast$mean <- fcast$mean ;
  fcast$upper <- fcast$upper ;
  fcast$lower <- fcast$lower ;
  #fcast$x <- ts(rowSums(fit$time.series));
  tsp(fcast$x) <- tsp(fit$time.series);
  fcast$method <- paste("STL + ", fcast$method);
  fcast$seasonal <- ts(lastseas[1:m], frequency = m, start = tsp(fit$time.series)[2] - 1 + 1/m);
  fcast$fitted <- fitted(fcast) + fit$time.series[, 1];
  fcast$residuals <- fcast$x - fcast$fitted;
  if(isPlot) {
    plot(fcast);
    lines(fitted(fcast), col = "red");
    text(paste("R2 = ", getRS(fcast$fitted, fcast$residuals), sep=""), x = 3, y = mean(fcast$fitted), col = "red");
  }
  return(fcast);
}



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
