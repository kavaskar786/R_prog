
#install.packages(c("ggpubr", "tidyverse", "broom", "AICcmodavg"))
#install.packages("ggpubr")
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)


crop.data = read.csv("C:\\Users\\kavas\\Downloads\\crop.data.csv")
summary(crop.data)

#Perform the ANOVA test
#One way ANOVA
one.way = aov(yield ~ fertilizer, data = crop.data)
summary(one.way)


#Two-way ANOVA
two.way = aov(yield ~ fertilizer + density, data = crop.data)
summary(two.way)


#Adding interactions between variables
interaction = aov(yield ~ fertilizer * density, data = crop.data)
summary(interaction)



df = data.frame(x=c(1,2,3,4,5),y=c(1,5,8,15,26))
df


linear_model = lm(y~x^2,data=df)
summary(linear_model)
plot(linear_model)

predict(linear_model,newdata=data.frame(x=c(15,16,17)))