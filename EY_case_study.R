rm(list=ls()) #Removes all items in Environment!
{sink("./arima.log")
  starting_time <- Sys.time()
  print(paste("Starting process at :",Sys.time()))
  print(paste("Starting processing from date :",Sys.Date()))
  ######### Install require Library and Import #############
  install.packages(c('readxl','data.table','dplyr','chron','xts','forecast','TSA','ggplot2'))
  
  ###============= Load library==========================
  library(readxl)      ## for import excel data
  library(data.table)  ## for data manipulation
  # library(plyr)
  library(dplyr)       ## for data manipulation
  library(chron)       ## for handle date and time
  library(xts)         ## for time series object and analysis
  library(forecast)    ## for time series forecasting
  library(TSA)         ## for time wise data handaling
  library(ggplot2)     ## for graphical presentation
  
  
  ###============ Load data & cleaning the data ==========
  ## Load the data file
  df <- read_excel("case_study_ML.xlsx")
  head(df)
  tail(df)
  dim(df)
  length(df)
  colnames(df)
  lapply(df, class)
  str(df)
  colSums(is.na(df))
  # sum(is.na(df))
  ## Check unique 'SKU'
  each_sku <- as.character(unique(df$SKU))
  length(each_sku)
  for( sku in 1:length(each_sku)){
    each_sku_df <- df[df$SKU==each_sku[sku],]
    ########## Duplicate Treatment ###########################
    ## check and remove duplicate data over SKU
    each_sku_df = distinct(each_sku_df, ISO_Week, .keep_all= T)
    len <- length(each_sku_df$ISO_Week)
    lapply(each_sku_df, class)
    ## Split ISOWEEK to year and week
    # list <- strsplit(each_sku_df$ISO_Week, "-")
    # each_sku_df <- ldply(list)
    # colnames(each_sku_df) <- c("Year", "Week")
    library(tidyr)
    each_sku_df$ISO_Week <- gsub("[()]", "", each_sku_df$ISO_Week)
    each_sku_df<- separate(each_sku_df, col = ISO_Week, into = c("Year","Week"), sep = "-")
    each_sku_df$Weekdate <- as.Date(paste(2018, each_sku_df$Week, 1, sep="-"), "%Y-%U-%u") ## convert year week to weekdate
    ## convert string to numeric
    each_sku_df$Sales <- as.numeric(as.character(each_sku_df$Sales))
    colSums(is.na(each_sku_df))
    # sum(is.na(each_sku_df))
    ## Replace NA by Mean
    Mean = as.numeric(mean(each_sku_df$Sales, na.rm = TRUE))
    Mean = as.integer(Mean)
    each_sku_df[,4][is.na(each_sku_df[,4])] <- Mean
    
    ## Replace zero and less than zero by mean, because zero is it means sales was not started after that period
    ## and less than zero means there are negative sales that could not be, so replace by mean value
    each_sku_df[each_sku_df <= 0] <- Mean
    ################ Outlier Treatment ####################
    ## Check Outliers and substitute by quantile ########
    capOutlier <- function(x){
      qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
      caps <- quantile(x, probs=c(.05, .95), na.rm = T)
      H <- 1.5 * IQR(x, na.rm = T)
      x[x < (qnt[1] - H)] <- caps[1]
      x[x > (qnt[2] + H)] <- caps[2]
      return(x)
    }
    each_sku_df$Sales = capOutlier(each_sku_df$Sales)
    
    ## Feature engeneering ##
    ## I have treat here time series analysis so taking only 'Weekdate' and 'Sales' variables of each 'SKU'
    ## create TS objective
    SalesTs <- xts(each_sku_df$Sales, order.by = each_sku_df$Weekdate)
    length(SalesTs)
    SalesTs1 <- ts(coredata(SalesTs))
    length(SalesTs1)
    ## split data set as you given the question
    y <- ts(SalesTs1[1:42])
    length(y)
    y_test <- ts(SalesTs1[(43:52)])
    
    ## Build predictive modelling
    ## Build ARIMA Model
    fit.arima <- auto.arima(y, d = NA, D = NA, max.p = 5, max.P = 5,
                            max.q = 5,max.Q = 5, max.d = 2,max.D = 1,max.order = 3,
                            start.p = 1,start.q = 1, start.P = 1,start.Q = 1,
                            stationary = FALSE,seasonal =TRUE, stepwise = TRUE)
    arima.frc <- forecast(fit.arima,h=10,level = 95) ## Forecasting the data, at 95% level of significance
    print(accuracy(fit.arima)) ## check traing accuracy
    # (accuracy(arima.frc$mean, y_test))
    ## View the graphical vizuligation
    # print(autoplot (arima.frc, alpha = 0.5,
    #                 shape = 3, xlab = "Time", ylab = "Sales",
    #                 title = "Sales demand forecast",
    #                 legendLabs = NULL, CI = TRUE, bands = TRUE,
    #                 pval = TRUE, plotTable = TRUE, divideTime = 1,
    #                 returnTable = FALSE))
    ## Create output file
    ## Forecast ISOWeek Index
    frcstweekdate <- c('2018-43','2018-44', '2018-45','2018-46','2018-47','2018-48','2018-49','2018-50','2018-51','2018-52' )
    
    arima.frc$mean <- as.numeric(arima.frc$mean)
    arima.frc$mean <- as.integer(arima.frc$mean)
    arima.frc$lower <- as.numeric(arima.frc$lower)
    arima.frc$lower <- as.integer(arima.frc$lower)
    arima.frc$upper <- as.numeric(arima.frc$upper)
    arima.frc$upper <- as.integer(arima.frc$upper)
    
    data <- data.frame(frcstweekdate,arima.frc$mean,arima.frc$lower,arima.frc$upper)
    colnames(data)<-c("frcstweekdate","Avg_PredictSales","Low_PredictSales","High_PredictSales")
    name <- paste(each_sku[sku],".csv",sep="")
    write.csv(data,file=name)
  }
  ending_time <- Sys.time()
  run_time <- ending_time - starting_time
  cat("\nOverall prosessing time is :",run_time)
  sink() ## sink() for checking log of script
}