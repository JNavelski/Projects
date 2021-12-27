
library(stats)
library(readxl)
library(lmtest)
library(MASS)
library(ggplot2)
library(patchwork)
library(distill)
library(dplyr)
library(nortest)
library(GGally)
library(gtsummary)
library(leaps)
library(arsenal)
library(car)
library(MVN)

library(stargazer)
library(broom)


setwd("/Users/JosephNavelski/Desktop/WSU PhD Economics/Econ Fall 2021/STAT 575 Multivariate Analysis/Project/")
# df <- read.table("project.txt",header = FALSE)
df <- read.csv('project.csv')

str(df)

# Drop Outliers for Better Predictions
(1:251)[min_rank(df$Height) == 1]
(1:251)[min_rank(df$Height) == 2]
(1:251)[min_rank(df$Height) == 4]
(1:251)[min_rank(df$Height) == 5]

(1:251)[rank(df$Weight) == 251]
(1:251)[rank(df$Weight) == 250]
(1:251)[rank(df$Weight) == 249]
(1:251)[rank(df$Weight) == 248]
(1:251)[rank(df$Weight) == 247]

(1:251)[rank(df$Ankle) == 251]
(1:251)[rank(df$Ankle) == 250]
(1:251)[rank(df$Ankle) == 249]
(1:251)[rank(df$Ankle) == 248]
(1:251)[rank(df$Ankle) == 247]

df <- df[-c(41,73,215,28,35,38,40,34,191,151,30,85,38,221,40),]
df <- df[,-1]

colnames(df)

#function for heatmap
my_fn <- function(data, mapping, method="p", use="pairwise", ...){
  
  # grab data
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  # calculate correlation
  corr <- cor(x, y, method=method, use=use)
  
  # calculate colour based on correlation value
  # Here I have set a correlation of minus one to blue, 
  # zero to white, and one to red 
  # Change this to suit: possibly extend to add as an argument of `my_fn`
  colFn <- colorRampPalette(c("blue", "white", "red"), interpolate ='spline')
  fill <- colFn(100)[findInterval(corr, seq(-1, 1, length=100))]
  
  ggally_cor(data = data, mapping = mapping, ...) + 
    theme_void() +
    theme(panel.background = element_rect(fill=fill))
}

# Function to return points and geom_smooth
# allow for the method to be changed
my_fn1 <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method,span = 0.95, ...)
  p
}

#combine
ggpairs(df, 
        upper = list(continuous = my_fn),
        lower = list(continuous = my_fn1)) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8))



# Regression and Regression Output
fit1=lm(df$pc_fat~df$Age+df$Weight+df$Height+df$Neck+df$Chest+df$Abdomen+df$Hip+df$Thigh+df$Knee+
         df$Ankle+df$Biceps+df$Forearm+df$Wrist, data=df)
summary(fit1)

# Stepwise Regression
step <- stepAIC(fit1, direction="both")
step$anova # display results

fit2=lm(df$pc_fat ~ df$Age + df$Height + df$Neck + df$Abdomen + df$Hip + 
         df$Thigh + df$Forearm + df$Wrist, data = df)
summary(fit2)

fit5=lm(df$pc_fat ~ df$Age+df$Abdomen, data = df)
summary(fit5)




# PCA Analysis
# Test for Multivariate Normality
library(QuantPsyc)

#create dataset
set.seed(0)

data <- data.frame(x1 = rnorm(50),
                   x2 = rnorm(50),
                   x3 = rnorm(50))

#perform Multivariate normality test
# Mardia’s Test (want to fail to reject the null for MVN)
mult.norm(data)$mult.test

mult.norm(df[,-1])$mult.test

R = cor(df[,-1])
print(R, digits = 3)
print(eigen(R)$values)

pcout_cov <- princomp(cov=R)
summary(pcout_cov)


# Want!!!
pcout <- princomp(x=df[,-1], cor = TRUE)
summary(pcout)
print(loadings(pcout),cutoff = 0, digits = 3)
plot(pcout)

nrow(pcout$scores)
PCs <- data.frame(pcout$scores)
# Get PCs to use for regresson

print(cor(PCs), digits = 1)

PCs <- data.frame(pcout$scores) # 1st component
cor(PCs)


fit3=lm(df$pc_fat ~ PCs$Comp.1)
summary(fit3)
fit4=lm(df$pc_fat ~ PCs$Comp.1+PCs$Comp.2+PCs$Comp.3)
summary(fit4)
fit5=lm(df$pc_fat ~ PCs$Comp.1+PCs$Comp.2+PCs$Comp.3+PCs$Comp.4+PCs$Comp.5)
summary(fit5)



fit_full =lm(df$pc_fat ~ PCs$Comp.1+PCs$Comp.2+PCs$Comp.3+PCs$Comp.4+PCs$Comp.5+PCs$Comp.6)
summary(fit_full)

shapiro.test(fit3$res)
bptest(fit3)
dwtest(fit3)

shapiro.test(fit4$res)
bptest(fit4)
dwtest(fit4)

shapiro.test(fit5$res)
bptest(fit5)
dwtest(fit5)

#combine
ggpairs(PCs, 
        upper = list(continuous = my_fn),
        lower = list(continuous = my_fn1)) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8))



# merge(model1_tidy, model2_tidy, by='term', all.x=T, all.y=T)  -> output
stargazer(fit1,fit2, type='latex', summary=FALSE, align = TRUE, title = 'Results',
          single.row = TRUE, font.size = 'small')

# merge(model1_tidy, model2_tidy, by='term', all.x=T, all.y=T)  -> output
stargazer(fit3,fit4,fit5, type='latex', summary=FALSE, align = TRUE, title = 'Results',
          single.row = TRUE, font.size = 'small')

loadings <- pcout$loadings[1:13,]

# merge(model1_tidy, model2_tidy, by='term', all.x=T, all.y=T)  -> output
stargazer(loadings, type='latex', summary=FALSE, align = TRUE, title = 'Results',
          single.row = TRUE, font.size = 'small')

# Summary Stats
stargazer(df, type='latex', summary=TRUE, align = TRUE, title = 'Results', digits = 2)






