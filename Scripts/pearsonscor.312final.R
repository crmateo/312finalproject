#Load required packages
library(ggpubr)

#Downloading required package from github
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

#Loading CM & PY data

CM.PY <- read.csv("Data/CM.PY_yr_ct.csv")
head(CM.PY)

#CM Q-Q plot
ggqqplot(CM.PY$CM_ct, ylab = "Dungeness Count")

#PY Q-Q plot
ggqqplot(CM.PY$PY_ct, ylab = "Prey Count")

#Correlation test
CM.PY.cor.test <- cor.test(CM.PY$PY_ct, CM.PY$CM_ct, method = "pearson")

#Correlation test interpretation
CM.PY.cor.test$p.value
CM.PY.cor.test$estimate
