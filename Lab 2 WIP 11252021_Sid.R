library(tidyverse)
library(patchwork)
library(stargazer)
library(sandwich)
library(lmtest)
library(ggplot2)
library(dplyr)
# library(aods3)
library(stargazer)
theme_set(theme_bw())

# Load Data
library(readr)
Salesstore <- read_csv("Salesstore.csv")

# Initial Snapshot of Data
head(Salesstore)

# Scatter
initial_scatter_plot <- ggplot(
  Salesstore, aes(x = log(Order_Quantity), y = log(Profit + 5000))) + geom_point()

inital_scatter_with_title <- initial_scatter_plot + ggtitle("Scatter Plot of Profits by Order Quantity") +
  xlab("Log Order Quantity") + ylab("Profits")


# Model #1
model_1 <- lm(log(Salesstore$Profit + 5000) ~ log(Salesstore$Order_Quantity), na.action = na.exclude)
summary(model_1)

# Scatter with Model 1
plot(inital_scatter_with_title)
abline(lm(log(Salesstore$Profit + 5000) ~ log(Salesstore$Order_Quantity), data = Salesstore))


  
