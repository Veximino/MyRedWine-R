getwd()
setwd("C:/Users/-/Documents")
red_wine <- read.csv("C:/Users/-/Documents/winequality-red.csv")
View(red_wine)

sample_size = floor(0.75*nrow(red_wine))
sample_size

set.seed(123)
train_ind = sample(seq_len(nrow(red_wine)),size = sample_size)
train = red_wine[train_ind,]
dim(train)
test = red_wine[-train_ind,]

complete_model = lm(quality ~ ., data = train)
scope = formula(complete_model)
scope

#Histogram
install.packages("gridExtra")
install.packages("ggplot2")
library(gridExtra)
library(ggplot2)
draw_hist <- function(dataframe, variable)
{
  # Save histogram definition to the plot variable
  plot <- ggplot(data = dataframe, aes(x = variable)) + 
    geom_histogram(color = 'black', fill = '#099DD9') +
    xlab(deparse(substitute(variable)))
  return(plot)
}

grid.arrange(draw_hist(red_wine, red_wine$fixed.acidity),
             draw_hist(red_wine, red_wine$volatile.acidity),
             draw_hist(red_wine, red_wine$citric.acid),
             draw_hist(red_wine, red_wine$residual.sugar),
             draw_hist(red_wine, red_wine$chlorides),
             draw_hist(red_wine, red_wine$free.sulfur.dioxide),
             draw_hist(red_wine, red_wine$total.sulfur.dioxide),
             draw_hist(red_wine, red_wine$density),
             draw_hist(red_wine, red_wine$pH),
             draw_hist(red_wine, red_wine$sulphates),
             draw_hist(red_wine, red_wine$alcohol),
             draw_hist(red_wine, red_wine$quality),
             ncol = 3)

#correlation plot
red_wine_cor <- cor(red_wine[c(1:11, 12)])
# correlation matrix
install.packages("corrplot")
library(corrplot)
corrplot(red_wine_cor, method = 'square', order = "hclust", 
         tl.col = "black", tl.cex = 0.8, tl.offset = 1)

#Forward step

model_int = lm(quality ~ ., data = train)
forward_step = step(object = model_int, scope = scope, direction = "forward")

summary(forward_step)

forward_step_AIC = AIC(forward_step)
forward_step_AIC

#Backward step

backward_step = step(object = complete_model, scope = scope, direction = "backward")
summary(backward_step)

backward_step_AIC = AIC(backward_step)
backward_step_AIC

#Bidirectional Step
both_dir_step = step(object = complete_model,scope = scope, direction = "both")
summary(both_dir_step)

both_dir_step_AIC = AIC(both_dir_step)
both_dir_step_AIC

#AIC Values 

AIC_diff = data.frame(FwdSelection=forward_step_AIC, BackSelection=backward_step_AIC, 
                      BidirSelection=both_dir_step_AIC)
rownames(AIC_diff) = c("AIC")
AIC_diff

# AIC of forward selection is better among the backward as well as bidirectional
#variables selected are as follows:
# 1. volatile.acidity 2. chlorides 3. total.sulfur.dioxide 4. pH 5. sulphates 6. alcohol
# Partial least squares (PLS) regression is useful when you have very few 
# observations compared to the number of independent variables or when your 
#independent variables are highly correlated. PLS decreases the independent variables down to 
#a smaller number of uncorrelated components, similar to Principal Components Analysis. 
#Then, the procedure performs linear regression on these components rather the original data. 
#PLS emphasizes developing predictive models and is not used for screening variables. Unlike OLS, you can 
#include multiple continuous dependent variables. PLS uses the correlation structure to identify smaller 
#effects and model multivariate patterns in the dependent variables.

##ANOVA
#Test on Volatile Acidity
m1 <-lm(red_wine$quality ~ red_wine$chlorides + red_wine$free.sulfur.dioxide + 
     red_wine$total.sulfur.dioxide + red_wine$pH + red_wine$sulphates + 
     red_wine$alcohol)
m2<- lm(red_wine$quality ~ red_wine$volatile.acidity + red_wine$chlorides + 
     red_wine$free.sulfur.dioxide + red_wine$total.sulfur.dioxide + 
     red_wine$pH + red_wine$sulphates + red_wine$alcohol)

a1 <- anova(m1,m2)
a1

qf(1-.05, a1[1,1],a1[2,1])

#Test on chlorides

m3 <-lm(red_wine$quality ~ red_wine$volatile.acidity + red_wine$free.sulfur.dioxide + 
          red_wine$total.sulfur.dioxide + red_wine$pH + red_wine$sulphates + 
          red_wine$alcohol)
m4<- lm(red_wine$quality ~ red_wine$volatile.acidity + red_wine$chlorides + 
          red_wine$free.sulfur.dioxide + red_wine$total.sulfur.dioxide + 
          red_wine$pH + red_wine$sulphates + red_wine$alcohol)

a2 <- anova(m3,m4)
a2

#Test on Free sulphur dioxide

m5 <-lm(red_wine$quality ~ red_wine$volatile.acidity + red_wine$chlorides + 
          red_wine$total.sulfur.dioxide + red_wine$pH + red_wine$sulphates + 
          red_wine$alcohol)
m6<- lm(red_wine$quality ~ red_wine$volatile.acidity + red_wine$chlorides + 
          red_wine$free.sulfur.dioxide + red_wine$total.sulfur.dioxide + 
          red_wine$pH + red_wine$sulphates + red_wine$alcohol)

a3 <- anova(m5,m6)
a3

#Test on Total sulphur dioxide

m7 <-lm(red_wine$quality ~ red_wine$volatile.acidity + red_wine$chlorides + 
          red_wine$free.sulfur.dioxide + red_wine$pH + red_wine$sulphates + 
          red_wine$alcohol)
m8<- lm(red_wine$quality ~ red_wine$volatile.acidity + red_wine$chlorides + 
          red_wine$free.sulfur.dioxide + red_wine$total.sulfur.dioxide + 
          red_wine$pH + red_wine$sulphates + red_wine$alcohol)

a4 <- anova(m7,m8)
a4

#Test on pH

m9 <-lm(red_wine$quality ~ red_wine$volatile.acidity + red_wine$chlorides + 
          red_wine$free.sulfur.dioxide + red_wine$total.sulfur.dioxide + 
          red_wine$sulphates + red_wine$alcohol)
m10<- lm(red_wine$quality ~ red_wine$volatile.acidity + red_wine$chlorides + 
          red_wine$free.sulfur.dioxide + red_wine$total.sulfur.dioxide + 
          red_wine$pH + red_wine$sulphates + red_wine$alcohol)

a5 <- anova(m9,m10)
a5

#Test on Sulphates
m11 <-lm(red_wine$quality ~ red_wine$volatile.acidity + red_wine$chlorides + 
          red_wine$free.sulfur.dioxide + red_wine$total.sulfur.dioxide + 
          red_wine$pH + red_wine$alcohol)
m12<- lm(red_wine$quality ~ red_wine$volatile.acidity + red_wine$chlorides + 
           red_wine$free.sulfur.dioxide + red_wine$total.sulfur.dioxide + 
           red_wine$pH + red_wine$sulphates + red_wine$alcohol)

a6 <- anova(m11,m12)
a6

#Test on Alcohol

m13 <-lm(red_wine$quality ~ red_wine$volatile.acidity + red_wine$chlorides + 
           red_wine$free.sulfur.dioxide + red_wine$total.sulfur.dioxide + 
           red_wine$pH + red_wine$sulphates)
m14<- lm(red_wine$quality ~ red_wine$volatile.acidity + red_wine$chlorides + 
           red_wine$free.sulfur.dioxide + red_wine$total.sulfur.dioxide + 
           red_wine$pH + red_wine$sulphates + red_wine$alcohol)

a7 <- anova(m13,m14)
a7


install.packages("pls")
library(pls)
plsr_model <- plsr(quality ~ ., data = train)
summary(plsr_model)

install.packages("glmnet")
library(glmnet)
ridge_model <- cv.glmnet(x = as.matrix(train[,-12]), y = as.matrix(train$quality),
                         alpha = 0)
ridge_model

lasso_model <- glmnet(x = as.matrix(train[,-12]), y = as.matrix(train$quality),
                      alpha = 1) 
lasso_model

elastic_model <- glmnet(x = as.matrix(train[,-12]), y = as.matrix(train$quality),
                        alpha = 0.5)
elastic_model


forward_step_predict <- predict(forward_step, test[,-12])
backward_step_predict <- predict(backward_step, test[,-12])
both_dir_step_predict <- predict(both_dir_step, test[,-12])
plsr_model_predict <- predict(plsr_model, test[,-12], ncomp = 6)
ridge_model_predict <- predict(ridge_model, as.matrix(test[,-12]), s = 0.1)
lasso_model_predict <- predict(lasso_model, as.matrix(test[,-12]), s = 0.1)
elastic_model_predict <- predict(elastic_model, as.matrix(test[,-12]), s = 0.1)

results <- data.frame(forward_step_predict,backward_step_predict,
                      both_dir_step_predict,plsr_model_predict,
                      ridge_model_predict,lasso_model_predict,
                      elastic_model_predict)
colnames(results) <- c("Forward Step", "Backward Step", 
                       "Bidirectional Step", "PLSR", "Ridge", 
                       "Lasso", "Elastic")
head(results)


MSE = c()
for(i in 1:7){
  MSE <- rbind(MSE, c(names(results)[i], mean((test$quality - results[,i])^2)))
}
MSE <- data.frame(Model = MSE[,1], MSE = as.numeric(MSE[,2]))
MSE[order(MSE$MSE),]


install.packages("memisc")
library(memisc)
m1 <- lm(quality ~ alcohol,data = train)
m2 <- update(m1, ~ . + volatile.acidity)
m3 <- update(m2, ~ . + chlorides)
m4 <- update(m3, ~ . + total.sulfur.dioxide)
m5 <- update(m4, ~ . + pH)
m6 <- update(m5, ~ . + sulphates)
mtable(m1,m2,m3,m4,m5,m6)


# From the prediction, we can see that the mean square error of backward step as well as bidirection step is lowest. So these 2 models have best prediction performance among 7 models.

