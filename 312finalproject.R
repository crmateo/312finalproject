#Load packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(broom)

#Citations
citation(package = "ggplot2")
citation(package = "dplyr")
citation(package = "tidyverse")
citation(package = "broom")


#Read CM count by year csv
CM_yr_ct <- read.csv("Data/CM_yr_ct.csv")
head(CM_yr_ct)

#Plot CM count by year
CM_yr_ct.pl <- ggplot(CM_yr_ct) +
  geom_point(mapping = aes(x = Year, y = Count)) +
  geom_smooth(mapping = aes(x = Year, y = Count)) +
  labs(x = "Year", y = "Dungeness Count")
print(CM_yr_ct.pl)

#Linear model for CM count by year
lm_fit.CM_yr <- lm(Year ~ Count, data = CM_yr_ct)
summary(lm_fit.CM_yr)

#Regression line df for CM count by year
predicted.CM_yr <- data.frame(ct_pred = predict(lm_fit.CM_yr, CM_yr_ct), 
                              Count=CM_yr_ct$Count)

#Line of fit dataframe for CM count by year
fit.CM_yr <- tidy(lm_fit.CM_yr)
fit.CM_yr.R <- glance(lm_fit.CM_yr)

#Plot with regression line for CM count by year
CM_yr_ct.pl.R <- ggplot(CM_yr_ct) +
  geom_point(mapping = aes(x = Year, y = Count)) +
  geom_line(data = predicted.CM_yr, aes(x=ct_pred, y=Count)) +
  labs(x = "Year", y = "Dungeness Count") +
  annotate("text", label = paste(paste(round(fit.CM_yr[2,2], digits = 3), "x", 
                                       sep=""), round(fit.CM_yr[1,2]), sep="+"), 
           x = max(CM_yr_ct$Year)-17, 
           y = max(CM_yr_ct$Count)-2) +
  annotate("text", label = paste("R^2", round(fit.CM_yr.R[1,2], digits = 3), sep = "="), 
           x = max(CM_yr_ct$Year)-12, 
           y = max(CM_yr_ct$Count)-2)
print(CM_yr_ct.pl.R)

#Read PY count by year
PY_yr_ct <- read.csv("Data/PY_yr_ct.csv")
head(PY_yr_ct)

#Plot PY count by year
PY_yr_ct.pl <- ggplot(PY_yr_ct) +
  geom_point(mapping = aes(x = Year, y = Count)) +
  geom_smooth(mapping = aes(x = Year, y = Count)) +
  labs(x = "Year", y = "Prey Count")
print(PY_yr_ct.pl)

#Linear model for PY count by year
lm_fit.PY_yr <- lm(Year ~ Count, data = PY_yr_ct)
summary(lm_fit.PY_yr)

#Regression line df for PY count by year
predicted.PY_yr <- data.frame(ct_pred = predict(lm_fit.PY_yr, PY_yr_ct), 
                              Count=PY_yr_ct$Count)

#Line of fit dataframe for Cm count by year
fit.PY_yr <- tidy(lm_fit.PY_yr)
fit.PY_yr.R <- glance(lm_fit.PY_yr)

#Plot with regression line for PY count by year
PY_yr_ct.pl.R <- ggplot(PY_yr_ct) +
  geom_point(mapping = aes(x = Year, y = Count)) +
  geom_line(data = predicted.PY_yr, aes(x=ct_pred, y=Count)) +
  labs(x = "Year", y = "Prey Count") +
  annotate("text", label = paste(paste(round(fit.PY_yr[2,2], digits = 3), "x", 
                                       sep=""), round(fit.PY_yr[1,2]), sep="+"), 
           x = max(PY_yr_ct$Year)-17, 
           y = max(PY_yr_ct$Count)-2) +
  annotate("text", label = paste("R^2", round(fit.PY_yr.R[1,2], digits = 3), sep = "="), 
           x = max(PY_yr_ct$Year)-12, 
           y = max(PY_yr_ct$Count)-2)
print(PY_yr_ct.pl.R)

#Read CM and PY count by year csv
CM.PY_yr_ct <- read.csv("Data/CM.PY_yr_ct.csv")

#Plot CM count by PY count
CM.PY_ct.pl <- ggplot(CM.PY_yr_ct) +
  geom_point(mapping = aes(x = PY_ct, y = CM_ct)) +
  geom_smooth(mapping = aes(x = PY_ct, y = CM_ct)) +
  labs(x = "Prey Count", y = "Dungeness Count")
print(CM.PY_ct.pl)

#Linear model for CM by PY count by year
lm_fit.CM.PY_yr <- lm(PY_ct ~ CM_ct, data = CM.PY_yr_ct)
summary(lm_fit.CM.PY_yr)

#Regression line df for CM by PY count by year
predicted.CM.PY_yr <- data.frame(ct_pred = predict(lm_fit.CM.PY_yr, CM.PY_yr_ct), 
                              Count=CM.PY_yr_ct$CM_ct)

#Line of fit dataframe for Cm count by year
fit.CM.PY_yr <- tidy(lm_fit.CM.PY_yr)
fit.CM.PY_yr.R <- glance(lm_fit.CM.PY_yr)

#Plot with regression line for CM count by year
CM.PY_yr_ct.pl.R <- ggplot(CM.PY_yr_ct) +
  geom_point(mapping = aes(x = PY_ct, y = CM_ct)) +
  geom_line(data = predicted.CM.PY_yr, aes(x=ct_pred, y=Count)) +
  labs(x = "Prey Count", y = "Dungeness Count") +
  annotate("text", label = paste(paste(round(fit.CM_yr[2,2], digits = 3), "x", 
                                       sep=""), round(fit.CM.PY_yr[1,2]), sep="+"), 
           x = max(CM.PY_yr_ct$PY_ct)-40, 
           y = max(CM.PY_yr_ct$CM_ct)-2) +
  annotate("text", label = paste("R^2", round(fit.CM.PY_yr.R[1,2], digits = 3), sep = "="), 
           x = max(CM.PY_yr_ct$PY_ct)-30, 
           y = max(CM.PY_yr_ct$CM_ct)-2)
print(CM.PY_yr_ct.pl.R)
