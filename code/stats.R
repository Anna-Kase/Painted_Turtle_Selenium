

library(tidyverse)
setwd("~/Turtle_SE")


se_data <- read.csv("Turtle_Se_Data.csv", header=T)
se_data


ggplot(se_data, aes(x=Site, y=Se))+
  geom_point()

ggplot(se_data, aes(x=Site_Type, y=Se, col=Site))+
  geom_point()

ggplot(se_data, aes(x=Mass, y=Se, col=Site))+
  geom_point()+



#t-test comparing mean Se between Tile and Control Sites
t.test(se_data$Se ~ se_data$Site_Type, var.equal=FALSE)

#Anova comparing mean Se between sites
nova <- lm(Se ~ Site, data=se_data)
anova(nova)



