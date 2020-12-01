library(tidyverse)
library(forecast)
library(copula)
library(scatterplot3d)
library(grid)
library(VineCopula)
library(readr)
library(rugarch)
library(lubridate)

medianswap <- function(data,Index){
  data[Index] <- NA
  for (i in 1:length(Index)) {
    data[Index[i]] <- median(data[(Index[i]-25):(Index[i]+25)],na.rm = TRUE)
  }
  return(data)
}
