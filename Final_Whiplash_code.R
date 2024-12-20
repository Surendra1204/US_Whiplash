library(ggplot2)
library(dplyr)
setwd("C:/Users/maharjan/OneDrive - Chapman University/Desktop/US_Whole-Whiplash/Future_Climate_Data/Final_rcps_for_whiplash_wwcra_mon")

list <- list.files()
print(list)


# 14/11/2023 Justin Li Chapman University

for (i in 1:length(list)) {
  
  original.df <- read.csv(list[i], stringsAsFactors = FALSE)
  
  # Select only the required columns
  original.df <- original.df %>% select(Year, Value, ID, Flag)
  
  print(original.df)
  
  # Define the meanValue for anomaly calculation, for 1981-2020 mean
  meanValue <- mean(original.df[original.df$Year %in% c(1981:2020),]$Value, na.rm=TRUE)
  #print(meanValue)
  if (meanValue > 10) { # the mean value has to > 10
    
  # x is the anomalies value
  original.df <- original.df[original.df$Year %in% c(1896:2099),]
  x <- original.df$Value - meanValue
  print(x)
  
  percentile20_80 <- quantile(original.df[original.df$Year %in% c(1981:2020),]$Value, probs = c(0.2,0.8))
  print(percentile20_80)
  upperLimit80 <- round(percentile20_80[2], 2)- meanValue # set as the threshold value 
  # lowerLimit20 <- round(percentile20_80[1], 2)- meanValue
  #print(upperLimit80)
  
  df <- data.frame(x)
  #print(df)
  
  length <- nrow(df)
  splitFactor = 1 # set the splitfactor 
  df$n <- NA
  df$ID <- NA
  df$AccumValue <- NA
  df$Indicator <- NA
  df$Year <- original.df$Year
  
  
  df[1,]$ID <- 1
  df[1,]$AccumValue <- df[1,]$x
  df[1,]$Indicator <- df[1,]$x
  df[1,]$n <- 1
  print(df)

  k <- 0
  
  
  tryCatch({    
  
  for (p in 1:(length-1)) {
    # case of same wet/dry condition in the following year
    if (df[p,]$Indicator * df[p+1,]$x >= 0) {
      df[p+1,]$ID <- df[p,]$ID # assign the same ID to previous one
      df[p+1,]$n <- df[p,]$n + 1 # update the consecutive years n number 
      
      df[p+1,]$Indicator <- (1-k)*df[p+1,]$x + k*df[p,]$Indicator
      if(df[p,]$x > upperLimit80) {df[p+1,]$Indicator <- (1-k)*df[p+1,]$x + k*upperLimit80}
      # if(df[p,]$x < lowerLimit20) {df[p+1,]$Indicator <- (1-k)*df[p+1,]$x + k*lowerLimit20}
      df[p+1,]$AccumValue <- df[p,]$AccumValue + df[p+1,]$Indicator # update the AccumValue with same sum
    }
    else{
      
      if ((1-k)*abs(df[p+1,]$x) < k*abs(df[p,]$Indicator)) { 
        #case of small shift that will not end the previous wet/dry periods
        df[p+1,]$ID <- df[p,]$ID # assign the same ID to previous one
        df[p+1,]$n <- df[p,]$n + 1 # update the consecutive years n number
        
        df[p+1,]$Indicator <- (1-k)*df[p+1,]$x + k*df[p,]$Indicator
        if(df[p,]$x > upperLimit80) {df[p+1,]$Indicator <- (1-k)*df[p+1,]$x + k*upperLimit80}
        # if(df[p,]$x < lowerLimit20) {df[p+1,]$Indicator <- (1-k)*df[p+1,]$x + k*lowerLimit20}
        df[p+1,]$AccumValue <- df[p,]$AccumValue + df[p+1,]$Indicator # update the AccumValue with same sum
      }
      #case of big shift that will end the previous wet/dry periods
      else{ 
        df[p+1,]$ID <- df[p,]$ID + 1 # assign a new ID
        df[p+1,]$n <-1
        
        df[p+1,]$Indicator <- (1-k)*df[p+1,]$x + k*df[p,]$Indicator
        if(df[p,]$x > upperLimit80) {df[p+1,]$Indicator <- (1-k)*df[p+1,]$x + k*upperLimit80}
        # if(df[p,]$x < lowerLimit20) {df[p+1,]$Indicator <- (1-k)*df[p+1,]$x + k*lowerLimit20}
        df[p+1,]$AccumValue <- df[p+1,]$Indicator # assign the accumulative value as the starting value of indicator
      }
    }
  }
  df$Condition <- NA

  # df$Condition <- ifelse(df$AccumValue >= 0, 'Wet', 'Dry')
  df$Condition <- ifelse(df$Indicator >= 0, 'Surplus', 'Deficit')
  
  
  percentile_WYT <- quantile(df[df$Year %in% c(1981:2020),]$Indicator, probs = c(0.15, 0.3, 0.5, 0.7))
  percentile_15 <- round(percentile_WYT[1], 2)
  percentile_30 <- round(percentile_WYT[2], 2)
  percentile_50 <- round(percentile_WYT[3], 2)
  percentile_70 <- round(percentile_WYT[4], 2)
  print(percentile_70)  
  
  df$WYT <- NA   
  df$WYT <- ifelse(df$Indicator <= percentile_15, 'C', 
                   ifelse(df$Indicator <= percentile_30 & df$Indicator > percentile_15, 'D', 
                          ifelse(df$Indicator <= percentile_50 & df$Indicator > percentile_30, 'BN', 
                                 ifelse(df$Indicator <= percentile_70 & df$Indicator > percentile_50, 'AN', 
                                        ifelse(df$Indicator > percentile_70, 'W', NA)))))
  
  
  df$meanValue <- meanValue
  df$ratio <- df$Indicator/meanValue
  print(df)
  
  export.df <- df
  names(export.df) <- c("Anomalies", 'PeriodLength', "PeriodID","AccumValue", 'Indicator','Year', "Condition", 'WYT', 'meanValue', 'ratio')

  std<- sd(export.df[export.df$Year %in% c(1981:2020),]$ratio, na.rm=TRUE) # Find standard deviation
  print(std)
  
  #append differences of ratio as new column
  export.df$Ratio_Diff <- c(0, diff(export.df$ratio))
  #append previous years condition count as new column
  export.df$Previous_PeriodLength <- c(0, export.df$PeriodLength[-length(export.df$PeriodLength)])
  export.df$Whiplash <- NA
  print(export.df)
  
  # Find the whiplash years
    percentile20_80 <- quantile(export.df[export.df$Year %in% c(1981:2020),]$ratio, probs = c(0.15,0.7))
    for (j in export.df$Year[2: nrow(export.df)]) {
      if ((export.df[export.df$Year == j-1, ]$ratio >= round(percentile20_80[2], 2)) &  
          (export.df[export.df$Year == j, ]$ratio <= round(percentile20_80[1], 2)) & 
          (export.df[export.df$Year == j-1, ]$Indicator * export.df[export.df$Year == j, ]$Indicator < 0)) {
        export.df[export.df$Year == j, ]$Whiplash <- 
          export.df[export.df$Year == j, ]$ratio
      }
      if ((export.df[export.df$Year == j, ]$ratio >= round(percentile20_80[2], 2)) &  
          (export.df[export.df$Year == j-1, ]$ratio <= round(percentile20_80[1], 2)) & 
          (export.df[export.df$Year == j-1, ]$Indicator * export.df[export.df$Year == j, ]$Indicator < 0)) {
        export.df[export.df$Year == j, ]$Whiplash <- 
          export.df[export.df$Year == j, ]$ratio
      }
      
    }
    print(export.df)
    ### Here ends the algorithm section
  # 
  # Below is to export the result
    export.df$ID <- original.df$ID
    export.df$Flag <- original.df$Flag
    export.df <- export.df %>%
      mutate(Flag = case_when(
        Flag == "P" ~ "Estimated",
        Flag == "A" ~ "Actual",
        TRUE ~ Flag  # This line keeps all other values unchanged
      ))
    write.csv(export.df, file = file.path("C:/Users/maharjan/OneDrive - Chapman University/Desktop/US_Whole-Whiplash/Future_Climate_Data/1.Version_1_Revised/2_Final_whiplash_wwcra/", list[i]), row.names = FALSE)
  }
  , error=function(e){cat("ERROR:",export.df$ID[1], "\n")})
  }
}

