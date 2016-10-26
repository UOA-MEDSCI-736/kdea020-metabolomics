#NoRAND functions for plotting and graphing
#K. Deane-Alder, 2016
#Licensed under CC-BY 4.0

library(qvalue)

y.lim <- c(-2, 3)

mkSingleGraphLog <- function(b) { #b = no. of compound you want to plot - log scale, and by injection order
  # - Plotting things out, inc. model
  cat("Plotting...")
  y<-LogMeasurements[Type=='C',b]
  x<-Lorder[Type=='C']
  mod1 <- lm(y~x)
  titleLog <- c(CompoundNames[,b], "R^2 = ", signif(summary(mod1)$r.squared,3) )
  plot(Lorder, LogMeasurements[,b], main = titleLog, xlab = "Injection Order", ylab = "Log Relative Intensity", col=Type)#, ylim = y.lim)
  abline(mod1)
  X <- subset(LogMeasurements[,b], Type == "C")
  Y <- subset(LogMeasurements[,b], Type == "S")
  sig <- t.test(X, Y)
  sig
  devAskNewPage(ask = TRUE)
}

DoCorrection.Linear <- function(i) { #this function performs corrections to the data based on the linear model (centering, scaling, subtraction of residuals)
  y <- LogMeasurements[Type=='C',i]  #placing the measurements for a single compound into a vector
  x <- Lorder[Type=='C'] #putting the injection order into another vector for model use
  mod1 <- lm(y ~ x) #recreating mod1 for JUST the compound being tested
  TotalDataForPredict=data.frame(x=Lorder) #prediction data frame init
  predict.mod1 <- predict(mod1, newdata=TotalDataForPredict) #outputting predict() into a vector
  finalvalues <- LogMeasurements[,i]  - predict.mod1 #subtracting the residuls (above output) to correct data based on subtracting residuals
  sig <- t.test(finalvalues~Type) #perform parametric t-test with welch's correction on the data to test for case/control differences in means
  title <- c(CompoundNames[,i], "Linear Model", "t.test p =", signif(sig$p.value,3)) #create a title vector inc. compound name and 
  plot(Lorder, finalvalues, main = title, xlab = "Injection Order", ylab = "Log Relative Intensity", col=Type, ylim = y.lim) #
  devAskNewPage(ask = TRUE) #ask for the user to press ENTER before producing the next graph
}

DoCorrection.Break <- function(i) { #as above, but for the stairstep model
  y <- LogMeasurements[Type=='C',i]
  x <- BreakSplit[Type=='C']
  mod2 <- lm(y ~ x)
  TotalDataForPredict = data.frame(x = BreakSplit)
  predict.mod2 <- predict(mod2, newdata = TotalDataForPredict)
  finalvalues2 <- LogMeasurements[,i]-predict.mod2
  sig <- t.test(finalvalues2~Type)
  title <- c(CompoundNames[,i], "Step Model", "t.test p =", signif(sig$p.value,3))
  plot(Lorder, finalvalues2, main = title, xlab = "Injection Order", ylab = "Log Relative Intensity", col=Type, ylim = y.lim)
  devAskNewPage(ask = TRUE)
}