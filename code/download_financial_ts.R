# R script for downloading multiple financial time series from yahoo finance


#install.packages("tidyquant")
#install.packages("rtsdata")
#install.packages("dplyr")

# tidyquant package which uses quantmod for the query
library(tidyquant)
# rtsdata package to save obtained datasets as csv
library(rtsdata)
# dplyr package for data wrangling 
library(dplyr)



# to download data from yahoo finance you need the ticker symbol. Go
# to finance.yahoo.com and search for assets of interest.

# here for example Apple: AAPL




# define start date in format "yyyy-mm-dd"

# end date is intia


###############################################################################
######################## User input ###########################################
###############################################################################

# example symbolvec for Apple and Tesla: 
# symbolvec <- c("AAPL","TSLA")

symbolvec <- c("MSCI",  "AAPL", "HG=F")

# example start date: "2018-06-07" format: "yyyy-mm-dd"
# Advice: Leave this at "1900-01-01" . This seems to lead to longest available
# series to be downloaded

startdate <- "1900-01-01"
enddate <- today()

# yahoo finance seems to have for each financial time series the following options:
# Open, High, Low, Close, Volume, Adjusted , choose one of them

# example type
# type <- "Close" 
type <- "Close"

# enddate is left open as this will lead to the most up to date data being 
# downloaded



###############################################################################
################### Now set the working directory #############################
################### then execute the entire code ##############################
###############################################################################


  symbolData <- new.env() 
  getSymbols(symbolvec, env = symbolData, src = "yahoo", from = startdate, to = enddate)
  
  # matching based on common dates
  common_dates <- as.Date(Reduce(intersect, eapply(symbolData, index)))
  # returns list of xts objects
  symbolData <- eapply(symbolData, `[`, i=common_dates)
  
  
  
  combined_series <- cbind(index(symbolData[[1]]),
                           as_tibble(symbolData[[1]]) %>% dplyr::select(dplyr::ends_with(type)))
  
  names(combined_series)[1] <- "Date"
  
  
  if (length(symbolData) >= 2){
    for (i in 2:length(symbolData)){
      combined_series <- cbind(combined_series,
                               as_tibble(symbolData[[i]]) %>% dplyr::select(dplyr::ends_with(type)))
    }
  }
  
  
  final_xts <- xts(combined_series[,-1],order.by = index(symbolData[[1]]))
  
  level_plot_name <- "../figures/level_"
  return_plot_name <- "../figures/logreturn_"
  csv_name <- "../data/time_series_"
  
  # create plot names
  
  all_assets_name <- paste(symbolvec[1])
  
  if (length(symbolData) >= 2){
    for (b in 2:length(symbolvec)){
      all_assets_name <- paste0(all_assets_name,"_",symbolvec[b])
    }
  }
  
  level_plot_name <- paste0(level_plot_name,all_assets_name,".pdf")
  return_plot_name <- paste0(return_plot_name,all_assets_name,".pdf")
  csv_name <- paste0(csv_name,all_assets_name,".csv")
  
  pdf(file=level_plot_name, width = 19, height = 9)
  plot(na.omit(final_xts),multi.panel = TRUE,type="l",yaxis.same = F, main = "Level")
  dev.off()
  
  pdf(file=return_plot_name, width = 19, height = 9)
  plot(diff(log(na.omit(final_xts))),multi.panel = TRUE,type="l",yaxis.same = T,
       main = "Log returns")
  dev.off()
  
  rtsdata::ds.storage.file.csv.save(na.omit(final_xts), csv_name, date.format = "%Y-%m-%d")
  




