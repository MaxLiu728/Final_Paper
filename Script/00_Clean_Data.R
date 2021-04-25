#### Workspace set-up ####
library(tidyverse)
library(tibble)
library(ggplot2)
library(readxl)
library(dplyr)
#install.packages("xlsx")
install.packages("writexl")
library(writexl)
library(DiagrammeR)

## Load Raw data ##
raw_data<- read_excel(here::here("Input/Raw_Data.xlsx"))
raw_data<- na.omit(raw_data[c(1,5,6,7,8,9)])

## Add dummy variable ##
### Price variable ###
raw_data$coal_price<- apply(raw_data[c(2,3,4,5)], 1, function(x) x=mean(x))
model_data<- raw_data[c(1,6,7)]
model_data<- rbind.data.frame(model_data, model_data)
for (i in 1:156){
  model_data$oil[i]= model_data$coal_price[i]
}
### Energy variable ###
model_data$Energy<- rep(c(1,0),each=156)
  
### Treat variable ###
model_data$Treat<- ifelse(model_data$Time>"2020-10-15",1,0)
write_xlsx(model_data, here::here("Input/model_data.xlsx"))
