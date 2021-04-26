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



Covariates<- read_excel(here::here("Input/Covariates.xlsx"))

ggplot(data= Covariates)+
  geom_line(aes(x= Time, y= Covariates$`Electricity consumption`))+
  geom_line(aes(x= Time, y= CBCFI))


## Density test ##
#Exp_data_RDit_2<- as.data.frame(Exp_data_RDit[-c(133),])

#model_desnity_test<- rddensity(Exp_data_RDit$Price, c= Exp_data_RDit$Price[132], p=1)
#summary(model_desnity_test



