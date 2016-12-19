library(AUC)
library(ROSE)
library(MASS)

#read the dataset into the R
data <- read.csv('total_sample_additional_V2.csv',header = TRUE)
data_train <- read.csv('data_train.csv',header = TRUE)
data_test <- read.csv('data_test.csv',header = TRUE)
str(data_train)
data <- data_train
str(data)
data <- BBmisc::dropNamed(data, "use")
data <- BBmisc::dropNamed(data, "id")
data <- BBmisc::dropNamed(data, "X")
data <- BBmisc::dropNamed(data, "bonus_credit_eligibility")
data <- BBmisc::dropNamed(data, "repayment_month")
data <- BBmisc::dropNamed(data, "group")
data <- BBmisc::dropNamed(data, "year")
data <- BBmisc::dropNamed(data, "month")
data <- BBmisc::dropNamed(data, "partner_id")
data <- BBmisc::dropNamed(data, "funded_amount")
data <- BBmisc::dropNamed(data, "themes")
data <- BBmisc::dropNamed(data, "activity")
data <- BBmisc::dropNamed(data, "language_1")
data <- BBmisc::dropNamed(data, "language_2")
data <- BBmisc::dropNamed(data, "currency_exchange_loss")
data <- BBmisc::dropNamed(data, "sector")
data <- BBmisc::dropNamed(data, "lender_count")
data <- BBmisc::dropNamed(data, "borrower_count")
data <- BBmisc::dropNamed(data, "location_country")
data$group <- as.factor(data$group)
for(number in 1:length(data[,'status'])){
  if (data[number,'bonus_credit_eligibility']=='UNKNOWN'){
    data[number,'bonus_credit_eligibility'] = '0.0'
  }
}
data$bonus_credit_eligibility  <- as.numeric(data$bonus_credit_eligibility)
data$bonus_credit_eligibility  <- as.factor(data$bonus_credit_eligibility)
data$status <- as.numeric(data$status)-1
str(data)

levels(data$bonus_credit_eligibility)
data_less <- BBmisc::dropNamed(data, "themes")
data_less <- BBmisc::dropNamed(data_less, "location_country")
data_less <- BBmisc::dropNamed(data_less, "activity")
data_less <- BBmisc::dropNamed(data_less, "partner_id")
data_less <- BBmisc::dropNamed(data_less, "year")

##try the logistic regression model
logistic_1 <- glm(status ~.,data_less,family = 'binomial')
summary(logistic_1)
predict(logistic_1,data,type = 'response')
stepAIC(logistic_1)
#paid is 1 and default is 0




## delete some of the categorical variables
pred_log <- predict(logistic_1,data_less,type = 'response')
y.test <- data_less$status
fmeasure <- accuracy.meas(y.test, pred_log, threshold = 0.82)
false_rate <- roc.curve(y.test, pred_log)
pred_log_1 <- ifelse(pred_log>=0.82,1,0)
error <- ifelse(pred_log_1==y.test,0,1)
error_rate <- sum(error)/length(data_less[,'status'])

sum(ifelse(pred_log_1==1,1,0))
##try the Xgboost model to predict the result
library(xgboost)
library(ggplot2)
library(Ckmeans.1d.dp)
str(data_x)

#change the parameter from category into the numerical
data_x <- BBmisc::dropNamed(data, "status")
data_y <- data[,'status']
head(data_y)
for(i in 1:length(data_x)){
  data_x[,i] <- as.numeric(unlist(data_x[,i]))-1
  
}

data_x <- as.matrix(data_x)
data_y <- as.matrix(data_y)
RF <- xgboost(data = data_x,label = data_y ,nrounds = 150 ,objective="binary:logistic",verbose=FALSE)
pred_xg <- predict(RF,data_x)
y.test <- as.factor(data_y)
false_rate <- roc.curve(y.test, pred_xg)

x.CV <- as.data.frame(data_x) 
y.CV <- as.data.frame(data_y) 

##tune the parameter of the xgboost
set.seed(123)
k <- sample(1:5,nrow(x.CV),replace = TRUE)
y.CV <- cbind(y.CV,k)
names(y.CV) <- c('y.CV','k')
x.CV$k <- k
x.CV <- as.data.frame(x.CV)
y.CV <- as.data.frame(y.CV)
y.CV$y.CV <- y.CV$y.CV
sum(ifelse(y.CV$y.CV==0,1,0))
head(x.CV)

nround <- (3:5)/0.02
# max.depth <- 1:4
eta <- (11:13)/100

subsample <- 0.8
max.depth <- c(2,4,10)
gamma <- c(1,10)
min_child_weight <- c(1,2,4)
colsample_bytree <- c(0.6,0.8)
lambda <- c(0,0.3,0.6)
lambda_bias <- c(0,0.6)
alpha <- c(0,0.3,0.6)
cost <- c(0.1,10)

length(nround)*length(max.depth)*length(eta)*length(subsample)*length(gamma)*
  length(min_child_weight)*length(colsample_bytree)*
  length(lambda)*length(lambda_bias)*length(alpha)

pa_auc <- data.frame(nround = 1:5832,max.depth = 1:5832, eta = 1:5832,auc = 1:5832,
                     subsample = 1:5832,gamma = 1:5832,min_child_weight = 1:5832,
                     colsample_bytree = 1:5832,lambda = 1:5832,lambda_bias = 1:5832,
                     alpha = 1:5832,fmeasure =1:5832 ,cost_1 =1:5832 ,cost_2 =1:5832 )

##tune the parameters to find the best fit model
count <- 1
for (nround in (3:5)/0.02){
  for (max.depth in c(2,4,10)){
    for (eta in (11:13)/100){
      for (gamma in c(1,10)){
        for (min_child_weight in c(1,2,4)){
          for (colsample_bytree in c(0.6,0.8)){
            for (lambda in c(0,0.3,0.6)){
              for (lambda_bias in c(0,0.6)){
                for (alpha in c(0,0.3,0.6)){

                  auc_set <- vector()
                  F_set <- vector()
                  cost_set_1 <- vector()
                  cost_set_2 <- vector()
                  print(count)
                  if(is.element(nround, result$nround) & is.element(max.depth, result$max.depth) &
                     is.element(eta, result$eta) & is.element(gamma, result$gamma) &
                     is.element(min_child_weight, result$min_child_weight) & is.element(colsample_bytree, result$colsample_bytree) &
                     is.element(lambda, result$lambda) & is.element(lambda_bias, result$lambda_bias) & 
                     is.element(alpha, result$alpha))
                    next
                  if(is.element(nround, result_1$nround) & is.element(max.depth, result_1$max.depth) &
                     is.element(eta, result_1$eta) & is.element(gamma, result_1$gamma) &
                     is.element(min_child_weight, result_1$min_child_weight) & is.element(colsample_bytree, result_1$colsample_bytree) &
                     is.element(lambda, result_1$lambda) & is.element(lambda_bias, result_1$lambda_bias) & 
                     is.element(alpha, result_1$alpha))
                    next
                  
                  for (i in 1:5){
                    
                    x.train <- as.matrix(x.CV[x.CV$k!=i,])
                    x.train <- as.data.frame(x.train)
                    x.train <- BBmisc::dropNamed(x.train, "k")
                    x.train <- as.matrix(x.train)
                    
                    
                    
                    y.train <- as.numeric(unlist(y.CV[y.CV$k!=i,'y.CV']))
                    y.train <- as.matrix(y.train)
                    x.test <- as.matrix(x.CV[x.CV$k==i,])
                    x.test <- as.data.frame(x.test)
                    x.test <- BBmisc::dropNamed(x.test, "k")
                    x.test <- as.matrix(x.test)
                    
                    
                    xgb.fit_1 <- xgboost(data=x.train, label=y.train, objective="binary:logistic",  nrounds=nround,max.depth=max.depth, eta=eta,subsample= subsample,gamma = gamma,
                                          min_child_weight = min_child_weight,
                                          colsample_bytree = 1,lambda = lambda,
                                          lambda_bias = lambda_bias,alpha,verbose=FALSE)
                    
#                    xgb.fit_1 <- xgboost(data=x.train, label=y.train, objective="binary:logistic",  nrounds=150,,verbose=FALSE)
                    
                    pred_xgb <- predict(xgb.fit_1,x.test)
                    y.test <- as.factor(y.CV[y.CV$k==i,]$y.CV)
                    #AUC calculation
                    roc_1 <- roc(pred_xgb,y.test)
                    auc_1 <- auc(roc_1,min=0,max=1)
                    auc_set <- append(auc_set,auc_1)
                    
                    #F-measure calculation
                    fmeasure <- accuracy.meas(y.test, pred_xgb, threshold = 0.82)
                    F_set <- append(F_set,fmeasure$F)
                    false_rate <- roc.curve(y.test, pred_xgb)
                    for(cost in c(0.1,10)){
                      cost_total <- false_rate$false.positive.rate*cost + (1-false_rate$true.positive.rate)
                      cost_min <- cost_total[which.min(cost_total)]
                      if (cost == 0.1){
                        cost_set_1 <- append(cost_set_1,cost_min)
                      }
                      else{
                        cost_set_2 <- append(cost_set_2,cost_min)
                      }
                      
                    }
                    
                    
                  }
                  pa_auc[count,'nround'] <- nround
                  pa_auc[count,'max.depth'] <- max.depth
                  pa_auc[count,'eta'] <- eta
                  pa_auc[count,'subsample'] <- subsample
                  pa_auc[count,'gamma'] <- gamma
                  pa_auc[count,'min_child_weight'] <- min_child_weight
                  pa_auc[count,'colsample_bytree'] <- colsample_bytree
                  pa_auc[count,'lambda'] <- lambda
                  pa_auc[count,'lambda_bias'] <- lambda_bias
                  pa_auc[count,'alpha'] <- alpha
                  
                  #the measure we use are AUC and cost function using different cost
                  
                  pa_auc[count,'fmeasure'] <- mean(F_set)
                  pa_auc[count,'cost_1'] <- mean(cost_set_1)
                  pa_auc[count,'cost_2'] <- mean(cost_set_2)
                  pa_auc[count,'auc'] <- mean(auc_set)
                  count <- count + 1
                }
              }
            }
          }
        }
        
        
      }
    }
  }
}

##find the most important features

feat.names <- dimnames(x.train)[[2]]
importanceMatrix <- xgb.importance(feature_names=feat.names, model = xgb.fit_1)  
# Importance graph
xgb.plot.importance(importanceMatrix)

###import the test data to test the model
x.train <- as.data.frame(x.train)
str(x.train)
predict_xgb <- ifelse(pred_xgb>0.1,1,0) 
table(predict_xgb,y.test)

##use the test dataset to predict the status
x.test <- as.matrix(x.CV)
y.CV <- as.data.frame(y.CV)
head(y.CV)
y.test <- y.CV$data_y
y.test <- as.factor(y.test)

pred_xg <- predict(xgb.fit_1,x.test)
false_rate <- roc.curve(y.test, pred_xg)


