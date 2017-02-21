LinearRegression <- function(tiempo,newtime,index,hminutes,UB,LB,startTime,tsData1,titulo1,a_max1,xlabel1,ylabel1,tsData2,titulo2,a_max2,xlabel2,ylabel2,tsData3,titulo3,a_max3,xlabel3,ylabel3,tsData4,titulo4,a_max4,xlabel4,ylabel4){

index1 <- seq(from = startTime, to = newtime+mns(hminutes), by = "min");
par(mfrow=c(4,2))

  
# fit1<-stl(tsData1,s.window="periodic",robust=TRUE)
# fit1.trend<-fit1$time.series[,"trend"]

res1 <- modelStlArimaMudpump(tsData1, h = hminutes)

xxx1 <- data.frame("X" = tiempo, "Y" = coredata(tsData1))
xxx1a <- data.frame("X" = 1:length(tiempo), "Y" = coredata(tsData1))
f1.lm <- lm(xxx1a$Y~xxx1a$X)
intercept <-f1.lm$coefficients[1]
slope <- f1.lm$coefficients[2]
X1 <- slope*(1:length(index1)) +  intercept
xxx1b <- data.frame("X" = index1, "Y" = coredata(X1))

fit2<-stl(tsData2,s.window="periodic",robust=TRUE)
fit2.trend<-fit2$time.series[,"trend"]

res2 <- modelStlArimaMudpump(tsData2, h = hminutes)
xxx2 <- data.frame("X" = tiempo, "Y" = coredata(tsData2))
xxx2a <- data.frame("X" = 1:length(tiempo), "Y" = coredata(tsData2))
f2.lm <- lm(xxx2a$Y~xxx2a$X)
intercept <-f2.lm$coefficients[1]
slope <- f2.lm$coefficients[2]
X2 <- slope*(1:length(index1)) +  intercept
xxx2b <- data.frame("X" = index1, "Y" = coredata(X2))

fit3<-stl(tsData3,s.window="periodic",robust=TRUE)
fit3.trend<-fit3$time.series[,"trend"]

res3 <- modelStlArimaMudpump(tsData3, h = hminutes)
xxx3 <- data.frame("X" = tiempo, "Y" = coredata(tsData3))
xxx3a <- data.frame("X" = 1:length(tiempo), "Y" = coredata(tsData3))
f3.lm <- lm(xxx3a$Y~xxx3a$X)
intercept <-f3.lm$coefficients[1]
slope <- f3.lm$coefficients[2]
X3 <- slope*(1:length(index1)) +  intercept
xxx3b <- data.frame("X" = index1, "Y" = coredata(X3))

fit4<-stl(tsData4,s.window="periodic",robust=TRUE)
fit4.trend<-fit4$time.series[,"trend"]

res4 <- modelStlArimaMudpump(tsData4, h = hminutes)
xxx4 <- data.frame("X" = tiempo, "Y" = coredata(tsData4))
xxx4a <- data.frame("X" = 1:length(tiempo), "Y" = coredata(tsData4))
f4.lm <- lm(xxx4a$Y~xxx4a$X)
intercept <-f4.lm$coefficients[1]
slope <- f4.lm$coefficients[2]
X4 <- slope*(1:length(index1)) +  intercept
xxx4b <- data.frame("X" = index1, "Y" = coredata(X4))

IQ <- 0.5*(a_max1$UB_PR-a_max1$LB_PR)

plot(res1,main=titulo1,ylim = c(a_max1$LB_PR-IQ,IQ+a_max1$UB_PR))
abline(a = a_max1$LB_PR, b = 0, col = "blue")
abline(a = a_max1$UB_PR, b = 0, col = "red")
abline(a = a_max1$MEAN, b = 0, col = "green")

plot(xxx1,t="l",main=titulo1,xlab = xlabel1,ylab = ylabel1,ylim = c(a_max1$LB_PR-IQ,IQ+a_max1$UB_PR),
     xlim = c(startTime,newtime+mns(hminutes)))
lines(xxx1b$X,xxx1b$Y,col=4,lwd=3)
abline(a = a_max1$LB_PR, b = 0, col = "blue")
abline(a = a_max1$UB_PR, b = 0, col = "red")
abline(a = a_max1$MEAN, b = 0, col = "green")

IQ <- 0.5*(a_max2$UB_PR-a_max2$LB_PR)
plot(res2,main=titulo2,ylim = c(a_max2$LB_PR-IQ,IQ+a_max2$UB_PR))
abline(a = a_max2$LB_PR, b = 0, col = "blue")
abline(a = a_max2$UB_PR, b = 0, col = "red")
abline(a = a_max2$MEAN, b = 0, col = "green")

plot(xxx2,t="l",main=titulo2,xlab = xlabel2,ylab = ylabel2,ylim = c(a_max2$LB_PR-IQ,IQ+a_max2$UB_PR),
     xlim = c(startTime,newtime+mns(hminutes)))
lines(xxx2b$X,xxx2b$Y,col=4,lwd=3)
abline(a = a_max2$LB_PR, b = 0, col = "blue")
abline(a = a_max2$UB_PR, b = 0, col = "red")
abline(a = a_max2$MEAN, b = 0, col = "green")

IQ <- 0.5*(a_max3$UB_PR-a_max3$LB_PR)
plot(res3,main=titulo3,ylim = c(a_max3$LB_PR-IQ,IQ+a_max3$UB_PR))
abline(a = a_max3$LB_PR, b = 0, col = "blue")
abline(a = a_max3$UB_PR, b = 0, col = "red")
abline(a = a_max3$MEAN, b = 0, col = "green")

plot(xxx3,t="l",main=titulo3,xlab = xlabel3,ylab = ylabel3,ylim = c(a_max3$LB_PR-IQ,IQ+a_max3$UB_PR),
     xlim = c(startTime,newtime+mns(hminutes)))
lines(xxx3b$X,xxx3b$Y,col=4,lwd=3)
abline(a = a_max3$LB_PR, b = 0, col = "blue")
abline(a = a_max3$UB_PR, b = 0, col = "red")
abline(a = a_max3$MEAN, b = 0, col = "green")

IQ <- 0.5*(a_max4$UB_PR-a_max4$LB_PR)
plot(res4,main=titulo4,ylim = c(a_max4$LB_PR-IQ,IQ+a_max4$UB_PR))
abline(a = a_max4$LB_PR, b = 0, col = "blue")
abline(a = a_max4$UB_PR, b = 0, col = "red")
abline(a = a_max4$MEAN, b = 0, col = "green")

plot(xxx4,t="l",main=titulo4,xlab = xlabel4,ylab = ylabel4,ylim = c(a_max4$LB_PR-IQ,IQ+a_max4$UB_PR),
     xlim = c(startTime,newtime+mns(hminutes)))
lines(xxx4b$X,xxx4b$Y,col=4,lwd=3)
abline(a = a_max4$LB_PR, b = 0, col = "blue")
abline(a = a_max4$UB_PR, b = 0, col = "red")
abline(a = a_max4$MEAN, b = 0, col = "green")


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
