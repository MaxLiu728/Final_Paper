#### Workspace set-up ####
library(tidyverse)
library(tibble)
library(ggplot2)
library(readxl)
library(dplyr)
#install.packages("xlsx")
#install.packages("writexl")
library(writexl)
library(scales)
library(DiagrammeR)
library(devtools)
install.packages(here::here("Packages/rddtools_1.4.0.tar.gz"), repos = NULL, type = "source")
#install.packages(rddtools)
library(rddtools)
#install.packages("rddensity")
#library(rddensity)


Exp_data<- read_excel(here::here("Input/model_data.xlsx"))
names(Exp_data)[2]<- 'Price'

Exp_data$Treat<- as.factor(Exp_data$Treat)
Exp_data$Energy<- as.factor(Exp_data$Energy)
################################## DID Model ############################################
## Parallel trend test## 
### Visualize ###
#### Average coal price without the policy effect as the base price##### 
Base_Group<- group_by(Exp_data, Energy)%>%
  filter(Time< "2020-10-17")%>%
  summarise(Base_price= mean(Price))

Percentage<- function(x){
  for (i in 1:nrow(Exp_data)){
    if (Exp_data$Energy[i]==0){
      Exp_data$Contribution_percentage= (Exp_data$Price- Base_Group$Base_price[1])/Base_Group$Base_price[1]
    }
    else{
      Exp_data$Contribution_percentage= (Exp_data$Price- Base_Group$Base_price[2])/Base_Group$Base_price[2]
    }
  }
  return(Exp_data)
}

Exp_data<- Percentage(Exp_data)
## Visualize parallel trend ##
Exp_data$Time<- as.Date(Exp_data$Time)
ggplot(data = Exp_data,
       aes(x= Time, y= Contribution_percentage, col= Energy))+
  geom_line()+
  geom_vline(xintercept = Exp_data$Time[132], linetype= 4,color="red",size=1)+
  labs(y="Coal Spot Price")+
  (scale_x_date(date_labels = "%m-%Y",
                breaks= date_breaks("3 months")))+
  theme(axis.text.x = element_text(angle = 90))


## Parallel trend test-- Event-study regression##
Exp_data_parallel<- filter(Exp_data, (Time > "2019-7-1") & (Time <"2021-4-6"))
Dtmin10<- function(Exp_data_parallel){
  for (i in 1:nrow(Exp_data_parallel)){
    if(i>0){
      if ((Exp_data_parallel$Time[i]>"2019-11-30") & (Exp_data_parallel$Time[i]<"2020-1-1")){
        Exp_data_parallel$Dtmin10[i]= 1
      }
      else{
        Exp_data_parallel$Dtmin10[i]= 0
      }
    }
    else{
      Exp_data_parallel$Dtmin10[i]= 0
    }
  }
  return(Exp_data_parallel)
}

Dtmin9<- function(Exp_data_parallel){
  for (i in 1:nrow(Exp_data_parallel)){
    if(i>0){
      if ((Exp_data_parallel$Time[i]>"2019-12-31") & (Exp_data_parallel$Time[i]<"2020-2-1")){
        Exp_data_parallel$Dtmin9[i]= 1
      }
      else{
        Exp_data_parallel$Dtmin9[i]= 0
      }
    }
    else{
      Exp_data_parallel$Dtmin9[i]= 0
    }
  }
  return(Exp_data_parallel)
}


Dtmin8<- function(Exp_data_parallel){
  for (i in 1:nrow(Exp_data_parallel)){
    if(i>0){
      if ((Exp_data_parallel$Time[i]>"2020-1-31") & (Exp_data_parallel$Time[i]<"2020-3-1")){
        Exp_data_parallel$Dtmin8[i]= 1
      }
      else{
        Exp_data_parallel$Dtmin8[i]= 0
      }
    }
    else{
      Exp_data_parallel$Dtmin8[i]= 0
    }
  }
  return(Exp_data_parallel)
}

Dtmin7<- function(Exp_data_parallel){
  for (i in 1:nrow(Exp_data_parallel)){
    if(i>0){
      if ((Exp_data_parallel$Time[i]>"2020-2-28") & (Exp_data_parallel$Time[i]<"2020-4-1")){
        Exp_data_parallel$Dtmin7[i]= 1
      }
      else{
        Exp_data_parallel$Dtmin7[i]= 0
      }
    }
    else{
      Exp_data_parallel$Dtmin7[i]= 0
    }
  }
  return(Exp_data_parallel)
}


Dtmin6<- function(Exp_data_parallel){
  for (i in 1:nrow(Exp_data_parallel)){
    if(i>0){
      if ((Exp_data_parallel$Time[i]<"2020-4-25") & (Exp_data_parallel$Time[i]>"2020-3-31")){
      Exp_data_parallel$Dtmin6[i]= 1
      }
      else{
        Exp_data_parallel$Dtmin6[i]= 0
      }
    }
    else{
      Exp_data_parallel$Dtmin6[i]= 0
     }
  }
  return(Exp_data_parallel)
}

Dtmin5<- function(Exp_data_parallel){
  for (i in 1:nrow(Exp_data_parallel)){
    if(i>0){
      if ((Exp_data_parallel$Time[i]>"2020-4-30") & (Exp_data_parallel$Time[i]<"2020-6-1")){
        Exp_data_parallel$Dtmin5[i]= 1
      }
      else{
        Exp_data_parallel$Dtmin5[i]= 0
      }
    }
    else{
      Exp_data_parallel$Dtmin5[i]= 0
    }
  }
  return(Exp_data_parallel)
}

Dtmin4<- function(Exp_data_parallel){
  for (i in 1:nrow(Exp_data_parallel)){
    if(i>0){
      if ((Exp_data_parallel$Time[i]>"2020-5-31") & (Exp_data_parallel$Time[i]<"2020-7-1")){
        Exp_data_parallel$Dtmin4[i]= 1
      }
      else{
        Exp_data_parallel$Dtmin4[i]= 0
      }
    }
    else{
      Exp_data_parallel$Dtmin4[i]= 0
    }
  }
  return(Exp_data_parallel)
}

Dtmin3<- function(Exp_data_parallel){
  for (i in 1:nrow(Exp_data_parallel)){
    if(i>0){
      if ((Exp_data_parallel$Time[i]>"2020-6-30") & (Exp_data_parallel$Time[i]<"2020-8-1")){
        Exp_data_parallel$Dtmin3[i]= 1
      }
      else{
        Exp_data_parallel$Dtmin3[i]= 0
      }
    }
    else{
      Exp_data_parallel$Dtmin3[i]= 0
    }
  }
  return(Exp_data_parallel)
}

Dtmin2<- function(Exp_data_parallel){
  for (i in 1:nrow(Exp_data_parallel)){
    if(i>0){
      if ((Exp_data_parallel$Time[i]>"2020-7-31") & (Exp_data_parallel$Time[i]<"2020-9-1")){
        Exp_data_parallel$Dtmin2[i]= 1
      }
      else{
        Exp_data_parallel$Dtmin2[i]= 0
      }
    }
    else{
      Exp_data_parallel$Dtmin2[i]= 0
    }
  }
  return(Exp_data_parallel)
}

Dtmin1<- function(Exp_data_parallel){
  for (i in 1:nrow(Exp_data_parallel)){
    if(i>0){
      if ((Exp_data_parallel$Time[i]>"2020-8-31") & (Exp_data_parallel$Time[i]<"2020-10-1")){
        Exp_data_parallel$Dtmin1[i]= 1
      }
      else{
        Exp_data_parallel$Dtmin1[i]= 0
      }
    }
    else{
      Exp_data_parallel$Dtmin1[i]= 0
    }
  }
  return(Exp_data_parallel)
}

Dtmin0<- function(Exp_data_parallel){
  for (i in 1:nrow(Exp_data_parallel)){
    if(i>0){
      if ((Exp_data_parallel$Time[i]>"2020-9-30") & (Exp_data_parallel$Time[i]<"2020-11-1")){
        Exp_data_parallel$Dtmin0[i]= 1
      }
      else{
        Exp_data_parallel$Dtmin0[i]= 0
      }
    }
    else{
      Exp_data_parallel$Dtmin0[i]= 0
    }
  }
  return(Exp_data_parallel)
}

Dtlag1<- function(Exp_data_parallel){
  for (i in 1:nrow(Exp_data_parallel)){
    if(i>0){
      if ((Exp_data_parallel$Time[i]>"2020-10-31") & (Exp_data_parallel$Time[i]<"2020-12-1")){
        Exp_data_parallel$Dtlag1[i]= 1
      }
      else{
        Exp_data_parallel$Dtlag1[i]= 0
      }
    }
    else{
      Exp_data_parallel$Dtlag1[i]= 0
    }
  }
  return(Exp_data_parallel)
}

Dtlag2<- function(Exp_data_parallel){
  for (i in 1:nrow(Exp_data_parallel)){
    if(i>0){
      if ((Exp_data_parallel$Time[i]>"2020-11-30") & (Exp_data_parallel$Time[i]<"2021-1-1")){
        Exp_data_parallel$Dtlag2[i]= 1
      }
      else{
        Exp_data_parallel$Dtlag2[i]= 0
      }
    }
    else{
      Exp_data_parallel$Dtlag2[i]= 0
    }
  }
  return(Exp_data_parallel)
}


Dtlag3<- function(Exp_data_parallel){
  for (i in 1:nrow(Exp_data_parallel)){
    if(i>0){
      if ((Exp_data_parallel$Time[i]>"2020-12-31") & (Exp_data_parallel$Time[i]<"2021-2-1")){
        Exp_data_parallel$Dtlag3[i]= 1
      }
      else{
        Exp_data_parallel$Dtlag3[i]= 0
      }
    }
    else{
      Exp_data_parallel$Dtlag3[i]= 0
    }
  }
  return(Exp_data_parallel)
}


Dtlag4<- function(Exp_data_parallel){
  for (i in 1:nrow(Exp_data_parallel)){
    if(i>0){
      if ((Exp_data_parallel$Time[i]>"2021-1-31") & (Exp_data_parallel$Time[i]<"2021-3-1")){
        Exp_data_parallel$Dtlag4[i]= 1
      }
      else{
        Exp_data_parallel$Dtlag4[i]= 0
      }
    }
    else{
      Exp_data_parallel$Dtlag4[i]= 0
    }
  }
  return(Exp_data_parallel)
}

Dtlag5<- function(Exp_data_parallel){
  for (i in 1:nrow(Exp_data_parallel)){
    if(i>0){
      if ((Exp_data_parallel$Time[i]>"2021-2-28") & (Exp_data_parallel$Time[i]<"2021-4-1")){
        Exp_data_parallel$Dtlag5[i]= 1
      }
      else{
        Exp_data_parallel$Dtlag5[i]= 0
      }
    }
    else{
      Exp_data_parallel$Dtlag5[i]= 0
    }
  }
  return(Exp_data_parallel)
}


Dtlag6<- function(Exp_data_parallel){
  for (i in 1:nrow(Exp_data_parallel)){
    if(i>0){
      if ((Exp_data_parallel$Time[i]>"2021-3-31") & (Exp_data_parallel$Time[i]<"2021-5-1")){
        Exp_data_parallel$Dtlag6[i]= 1
      }
      else{
        Exp_data_parallel$Dtlag6[i]= 0
      }
    }
    else{
      Exp_data_parallel$Dtlag6[i]= 0
    }
  }
  return(Exp_data_parallel)
}

Exp_data_parallel<- Dtmin10(Exp_data_parallel)
Exp_data_parallel<- Dtmin9(Exp_data_parallel)
Exp_data_parallel<- Dtmin8(Exp_data_parallel)
Exp_data_parallel<- Dtmin7(Exp_data_parallel)
Exp_data_parallel<- Dtmin6(Exp_data_parallel)
Exp_data_parallel<- Dtmin5(Exp_data_parallel)
Exp_data_parallel<- Dtmin4(Exp_data_parallel)
Exp_data_parallel<- Dtmin3(Exp_data_parallel)
Exp_data_parallel<- Dtmin2(Exp_data_parallel)
Exp_data_parallel<- Dtmin1(Exp_data_parallel)
Exp_data_parallel<- Dtmin0(Exp_data_parallel)
Exp_data_parallel<- Dtlag1(Exp_data_parallel)
Exp_data_parallel<- Dtlag2(Exp_data_parallel)
Exp_data_parallel<- Dtlag3(Exp_data_parallel)
Exp_data_parallel<- Dtlag4(Exp_data_parallel)
Exp_data_parallel<- Dtlag5(Exp_data_parallel)

#Exp_data_parallel_2<- Exp_data_parallel
#for (i in 1:90){
  #Exp_data_parallel_2$Contribution_percentage[i]<- log(Exp_data_parallel_2$Contribution_percentage[i])
#}
model_parallel<- lm(log(Price)~ Dtmin4+Dtmin5+Dtmin6+Dtmin7+Dtmin3+Dtmin2+Dtmin1+Dtmin0+
                      Dtlag1+Dtlag2+Dtlag3, data = Exp_data_parallel)

summary(model_parallel)
#
ggplot(data = Exp_data_parallel,
       aes(x= Time, y= log(Price), col= Energy))+
  geom_line()+
  labs(y="Coal Spot Price")+
  (scale_x_date(date_labels = "%m-%Y",
                breaks= date_breaks("3 months")))


## Model ##
model_1= lm(log(Price)~ Energy*Treat, data= Exp_data)
model_2= lm(log(Price)~ Energy+ Energy*Treat, data= Exp_data)
model_3= lm(Price~ Energy+Treat+ Energy*Treat, data= Exp_data)

summary(model_1)
summary(model_2)
summary(model_3)
########################################## DID Model ################################

########################################## RDiT Model ################################
Exp_data_RDit<- filter(Exp_data, Energy==1)
Exp_data_RDit$Cut_off<- ifelse(Exp_data_RDit$Time>
                                 "2020-10-15",1,0)
Exp_data_RDit$Period_week<- c(1:156)


Exp_data_RDit$Cut_off= as.factor(Exp_data_RDit$Cut_off)
ggplot(data= Exp_data_RDit, aes(x = Time, y = Price, color = Cut_off)) +
  geom_point() + 
  geom_smooth(formula = y~poly(x,1),method = "lm")

## RDD Tool ##
Exp_data_RDit$Period_week<- as.numeric(Exp_data_RDit$Period_week)

## Model 1##
rdd_data(Exp_data_RDit$Price, Exp_data_RDit$Period_week, cutpoint = 132) %>% 
  rdd_reg_lm(slope = "separate") %>% 
  summary()

## Model 2 Control variables ##
Controls<- read_excel(here::here("Input/Covariates.xlsx"))
### Standarization ## 
Controls_scaled<- Controls
Controls_scaled$`Electricity consumption`<- scale(Controls_scaled$`Electricity consumption`)[,1]
Controls_scaled$CBCFI<- scale(Controls_scaled$CBCFI)[,1]
Exp_data_RDit_control<- cbind.data.frame(Exp_data_RDit, Controls_scaled[c(2,3)])
#?rdd_data
model_RDiT_Control<- lm(Price~ Cut_off+CBCFI+Exp_data_RDit_control$`Electricity consumption`, 
                        data = Exp_data_RDit_control)
summary(model_RDiT_Control)

## Model 3 Narrow window ## 
Exp_data_RDit_Model3<- Exp_data_RDit_control[c(93:130, 136:148),]
### Model 3.1 ##
rdd_data(Exp_data_RDit_Model3$Price, Exp_data_RDit_Model3$Period_week, cutpoint = 130) %>% 
  rdd_reg_lm(slope = "separate") %>% 
  summary()


### Model 3.2 ##
model_RDiT_Control_2<- lm(Price~ Cut_off*(I-Period_week)+CBCFI+Exp_data_RDit_Model3$`Electricity consumption`, 
                        data = Exp_data_RDit_Model3)
summary(model_RDiT_Control_2)




)