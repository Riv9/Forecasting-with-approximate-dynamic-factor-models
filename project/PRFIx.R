rm(list = ls())
library(stats)
library(readr)
library(pracma)
library(writexl)
#library(fbi)
library(R.matlab)
library(dplyr)
library(rlang)
library(lmtest)
library(tseries)


load(file = "data\\act_data.Rda")
data2 <- act_data[1:52,] 
chi_rol <- readMat("data\\chi_rol.mat")
chi_rol <- as.data.frame((chi_rol))
xi_rol <- readMat("data\\xi_rol.mat")
xi_rol <- as.data.frame((xi_rol))

#r=5 q=3 
fcast_chi1 <- readMat("data\\fcast_chi1_new.mat")
fcast_chi1 <- as.data.frame(fcast_chi1)
fcast_chi4 <- readMat("data\\fcast_chi4_new.mat")
fcast_chi4 <- as.data.frame(fcast_chi4)

colnames(xi_rol) <- colnames(data2)
colnames(chi_rol) <- colnames(data2)
colnames(fcast_chi1) <- colnames(data2)
colnames(fcast_chi4) <- colnames(data2)


# data for graphs
data2 <- act_data[1:52,]
data2_PRFIx = data2[,10]
data2_PRFIx <- as.data.frame(data2_PRFIx)
data2_PRFIx <- data2_PRFIx %>% 
  rename(PRFIx = data2_PRFIx)

data2_4 <- act_data[1:55,]
data4_PRFIx = data2_4[,10]
data4_PRFIx <- as.data.frame(data4_PRFIx)
data4_PRFIx <- data4_PRFIx %>% 
  rename(PRFIx = data4_PRFIx)

##Residential Investments
############# AR(1) ############

PRFIx <- act_data$PRFIx
PRFIx <- as.data.frame(PRFIx)
train_PRFIx <- window(PRFIx$PRFIx, end = 52)
test_PRFIx <- window(PRFIx$PRFIx, start = 53)
fit <- arima(train_PRFIx, order = c(1,0,0))
forecast(fit, h = 4)
forecast(fit, h = 1)
w_size <- length(train_PRFIx)

#1-STEP
h <- 1
n <- length(test_PRFIx)-h+1
fcmat1 <- matrix(0, nrow = n, ncol = h)

for (i in 1:n){
  x <- PRFIx[i:(w_size-1+i),1]
  refit <- Arima(x, order = c(1,0,0))
  fcmat1[i,]<- forecast(refit, h = h)$mean
}

mean((fcmat1 - PRFIx[53:78,1])^2) #0.2066526

coeftest(refit)
#RES
mse.ar <- matrix(0, nrow = 26, ncol = 1) 
for (i in 1:26){
  mse.ar[i,] <- (PRFIx[52+i,] - fcmat1[i,])
}

#graph
PRFIx.g <- PRFIx[1:52,]
PRFIx.g <- as.data.frame(PRFIx.g)
PRFIx.g <- PRFIx.g %>%
  rename(PRFIx=PRFIx.g)
fcmat1 <- as.data.frame(fcmat1)
fcmat1 <- fcmat1 %>%
  rename(PRFIx=V1)

ar.g <- rbind(PRFIx.g, fcmat1)

plot.ts(ar.g, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PRFIx", xlab="Time (Quarters)")
points(act_data$PRFIx, type = "l", col = 1)
abline(v="52", col=8, lty=3, h=0)


#4-STEP
h <- 4
n <- length(test_PRFIx)-h+1
fcmat4 <- matrix(0, nrow = n, ncol = h)

for (i in 1:n){
  x <- PRFIx[i:(w_size-1+i),1]
  refit <- Arima(x, order = c(1,0,0))
  fcmat4[i,]<- forecast(refit, h = h)$mean
}

mean((fcmat4[,4] - PRFIx[56:78,1])^2) #0.2957257

coeftest(refit)

mse.ar4 <- matrix(0, nrow = n, ncol = 1) 
for (i in 1:n){
  mse.ar4[i,] <- (PRFIx[55+i,1] - fcmat4[i,4])
}

#graph
fcmat4 <- as.data.frame(fcmat4[,4])
fcmat4 <- fcmat4 %>%
  rename(PRFIx ='fcmat4[, 4]')

fcmat4 <- rbind(PRFIx.g, fcmat4)

plot.ts(fcmat4, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PRFIx", xlab="Time (Quarters)")
points(act_data$PRFIx, type = "l", col = 1)
abline(v="52", col=8, lty=3, h=0)


############# FHLR1 ############ 
#Only forecast through common part, i.e. chi

#1-STEP 
FHLR1 <- fcast_chi1 %>% 
  select(PRFIx) 

train_PRFIx <- as.data.frame(train_PRFIx)
train_PRFIx <- train_PRFIx %>% 
  rename(PRFIx = train_PRFIx)

# Graph
FHLR1 <- rbind(train_PRFIx, FHLR1)

plot.ts(FHLR1, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PRFIx", xlab="Time (Quarters)")
points(act_data$PRFIx, type = "l", col = 1)
abline(v="52", col=8, lty=3, h=0)

#MSE
mean((FHLR1[53:78,] - act_data[53:78,]$PRFIx)^2) #0.2256323

mse.f1 <- matrix(0, nrow = 26, ncol = 1) 
for (i in 1:26){
  mse.f1[i,] <- (PRFIx[52+i,] - FHLR1[52+i,])
}


#4-STEP 
FHLR1.4step <- fcast_chi4 %>% 
  select(PRFIx)
FHLR1.4step <- FHLR1.4step[4:26,]
FHLR1.4step <- as.data.frame(FHLR1.4step)
FHLR1.4step <- FHLR1.4step %>% 
  rename(PRFIx = FHLR1.4step)


#Graph
FHLR1.4step <- rbind(data4_PRFIx, FHLR1.4step)

plot.ts(FHLR1.4step, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PRFIx", xlab="Time (Quarters)")
points(act_data$PRFIx, type = "l", col = 1)
abline(v="52", col=8, lty=2, h=0)

#MSE
mean((FHLR1.4step[56:78,] - act_data[56:78,]$PRFIx)^2) #0.1801387

mse.f1.4 <- matrix(0, nrow = 23, ncol = 1) 
for (i in 1:23){
  mse.f1.4[i,] <- (PRFIx[55+i,] - FHLR1.4step[55+i,])
}

##ACCURACY  Diebold-Mariano test
dm.test(mse.ar, mse.f1, h = 1, alternative='g') #p-value = 0.7137 
dm.test(mse.ar4, mse.f1.4, h = 4, alternative='g') #p-value = 0.05944 


################ FHLR2 ############ 
#Forecast also idiosyncratic part, i.e. xi, through ar(1)

#1-STEP
xi_PRFIx <- xi_rol$PRFIx
xi_PRFIx <- as.data.frame(xi_PRFIx)
xi_PRFIx_train <- window(xi_PRFIx$xi_PRFIx, end = 52)
fit <- arima(xi_PRFIx_train, order = c(1,0,0))
w_size <- length(xi_PRFIx_train)

h <- 1
n <- 26-h+1
fcmat1.xi <- matrix(0, nrow = n, ncol = h)
xi_PRFIx.iter <- matrix(0, nrow = n, ncol = h)

for (i in 1:n){
  x <- xi_PRFIx[1+(w_size * (i-1)):(w_size * i),]
  refit <- Arima(x, order = c(1,0,0))
  xi_PRFIx.iter[i,] <- xi_PRFIx[(w_size  * (i+h)), 1] 
  fcmat1.xi[i,]<- forecast(refit, h = h)$mean[h]
}

mean((fcmat1.xi - xi_PRFIx.iter)^2) #0.1220759 MSE of the xi's forecasts

#x=chi+xi
PRFIx_fhlr2 = FHLR1[53:78,] + fcmat1.xi
PRFIx_fhlr2 <- as.data.frame(PRFIx_fhlr2)
PRFIx_fhlr2 <- PRFIx_fhlr2 %>% 
  rename( PRFIx = V1)

# Graph
FHLR2 <- rbind(train_PRFIx, PRFIx_fhlr2) 

plot.ts(FHLR2, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PRFIx", xlab="Time (Quarters)")
points(act_data$PRFIx, type = "l", col = 1)
abline(v="52", col=8, lty=3,h=0)

# MSE
mean((FHLR2[53:78,] - act_data[53:78,]$PRFIx)^2) #0.2202364 

#RES
mse.f2 <- matrix(0, nrow = 26, ncol = 1) 
for (i in 1:26){
  mse.f2[i,] <- (PRFIx[52+i,] - FHLR2[52+i,])
}


#4-STEP
h <- 4
n <- 26-h+1
fcmat4.xi <- matrix(0, nrow = n, ncol = h)
xi_PRFIx4.iter <- matrix(0, nrow = n, ncol = 1)


for (i in 1:n){
  x <- xi_PRFIx[1+(w_size * (i-1)):(w_size * i),]
  refit <- Arima(x, order = c(1,0,0))
  xi_PRFIx4.iter[i,] <- xi_PRFIx[(w_size  * (i+h)), 1]
  fcmat4.xi[i,]<- forecast(refit, h = h)$mean
}

# check whether we assigned the correct values into the test-set
#xi_gdp[260,]-xi_gdp4.iter[1,]
#xi_gdp[260+52,]-xi_gdp4.iter[2,] ##etc

mean((fcmat4.xi[,4] - xi_PRFIx4.iter)^2) #0.1195454 MSE for xi's

#x=chi+xi
PRFIx_fhlr2.4step = FHLR1.4step[56:78,] + fcmat4.xi[,4]
PRFIx_fhlr2.4step <- as.data.frame(PRFIx_fhlr2.4step)
PRFIx_fhlr2.4step <- PRFIx_fhlr2.4step %>% 
  rename( PRFIx = PRFIx_fhlr2.4step)

#Graph
FHLR2.4step <- rbind(data4_PRFIx, PRFIx_fhlr2.4step) 
plot.ts(FHLR2.4step, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PRFIx", xlab="Time (Quarters)")
points(act_data$PRFIx, type = "l", col = 1)
abline(v="52", col=8, lty=3,h=0)

#MSE
mean((PRFIx_fhlr2.4step[,1] - act_data[56:78,10])^2) #0.17591

#RES
mse.f2.4 <- matrix(0, nrow = 23, ncol = 1) 
for (i in 1:23){
  mse.f2.4[i,] <- (PRFIx[55+i,] - FHLR1.4step[55+i,])
}

##ACCURACY  Diebold-Mariano test
dm.test(mse.ar, mse.f2, h = 1, alternative='g') #p-value = 0.647 
dm.test(mse.ar4, mse.f2.4, h = 4, alternative='g') #p-value = 0.05944


######################## ML1 ##########################
# Forecast on xi_train with Lasso 

#1-STEP
xi_rol.mat <- as.matrix(xi_rol)


# 1-step
w_size <- 52
h <- 1
n <- 26-h+1
fcmat1.lasso <- matrix(0, nrow = n, ncol = h)
xi_testmat.lasso <- matrix(0, nrow = n, ncol = h) 

for (i in 1:n){
  x <- xi_rol.mat[1+(w_size * (i-1)):(w_size * i), -10]
  y <- xi_rol.mat[1+(w_size * (i-1)):(w_size * i), 10]
  testx <- xi_rol.mat[(w_size  * (i+h)), -10]
  xi_testmat.lasso[i,] <- xi_rol.mat[(w_size  * (i+1)), 10] 
  set.seed(1)
  cv.out <- cv.glmnet(x, y, alpha = 1)
  bestlam <- cv.out$lambda.min
  lasso.refit <- glmnet(x, y, alpha = 1, lambda = bestlam)
  fcmat1.lasso[i,] <- predict(lasso.refit, s = bestlam, newx = testx)
}

mean((fcmat1.lasso - xi_testmat.lasso[,1])^2) #MSE for predicting xi's (0.09367533)

## x = chi+ xi
FHLR1 <- fcast_chi1 %>% 
  select(PRFIx) 
ML1_1 <- FHLR1 + fcmat1.lasso

mean((ML1_1[,1] - act_data[53:78,10])^2) #0.1642313


#Graph
ML1_1g = rbind(data2_PRFIx, ML1_1)

plot.ts(ML1_1g, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PRFIx", xlab="Time (Quarters)")
points(act_data$PRFIx, type = "l", col = 1)
abline(v="52", col=8, lty=3,h=0)

mse.lasso1 <- matrix(0, nrow = n, ncol = h) 
for (i in 1:26){
  mse.lasso1[i,] <- (PRFIx[52+i,] - ML1_1[i,1])
}


# 4-step
w_size <- 52
h <- 4
n <- 26-h+1
fcmat4.lasso <- matrix(0, nrow = n, ncol = h)
xi_testmat.lasso <- matrix(0, nrow = n, ncol = 1) 

for (i in 1:n){
  x <- xi_rol.mat[1+(w_size * (i-1)):(w_size * i), -10]
  y <- xi_rol.mat[1+(w_size * (i-1)):(w_size * i), 10]
  testx <- xi_rol.mat[(w_size  * (i+h)), -10]
  xi_testmat.lasso[i,] <- xi_rol.mat[(w_size  * (i+h)), 10] 
  set.seed(1)
  cv.out <- cv.glmnet(x, y, alpha = 1)
  bestlam <- cv.out$lambda.min
  lasso.refit <- glmnet(x, y, alpha = 1, lambda = bestlam)
  fcmat4.lasso[i,] <- predict(lasso.refit, s = bestlam, newx = testx)
}

mean((fcmat4.lasso[,4] - xi_testmat.lasso[,1])^2) #MSE for predicting xi's (0.09643723)


## x = chi+ xi
FHLR1_4 <- fcast_chi4 %>% 
  select(PRFIx) 
ML1_4 = FHLR1_4[4:26,] + fcmat4.lasso[,4]

#MSE
mean((ML1_4 - act_data[56:78,10])^2) #0.1590205

#Graph

ML1_4 <- as.data.frame(ML1_4)
ML1_4 <- ML1_4 %>% 
  rename(PRFIx = ML1_4)

ML1_4g <- rbind(data4_PRFIx, ML1_4)


plot.ts(ML1_4g, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PRFIx", xlab="Time (Quarters)")
points(act_data$PRFIx, type = "l", col = 1)
abline(v="52", col=8, lty=3,h=0)

mse.lasso4 <- matrix(0, nrow = n, ncol = 1) 
for (i in 1:23){
  mse.lasso4[i,] <- (PRFIx[55+i,] - ML1_4[i,])
}

##ACCURACY  Diebold-Mariano test
dm.test(mse.ar, mse.lasso1, h=1, alternative='g') #p-value = 0.2561 
dm.test(mse.ar4, mse.lasso4, h=4, alternative = 'g') #p-value = 0.1437



############# ML 2 #############
#forecast on xi with Boosting
library(gbm)

#1 STEP AHEAD
w_size <- 52
h <- 1
n <- 26-h+1
fcmat1.boost <- matrix(0, nrow = n, ncol = h)
xi_testmat.boost <- matrix(0, nrow = n, ncol = h) 


for (i in 1:n){
  xi_testmat.boost[i,] <- xi_rol.mat[(w_size)*(i+1),10]
}


for (i in 1:n){
  ML2 <- gbm(PRFIx ~ ., data = xi_rol[1+(w_size * (i-1)):(w_size * i),],
             distribution = "gaussian", shrinkage = 0.01, cv.folds = 10,
             verbose=F, bag.fraction=0.7, n.trees = 5000, interaction.depth = 4)
  testx <- xi_rol[(w_size)*(i+1),]
  fcmat1.boost[i,] <- predict(ML2, newdata = testx, n.trees = 5000) #prediction with boosting
}

mean((fcmat1.boost[,1] - xi_testmat.boost[,1])^2) #MSE for predicting xi's (0.07939002)

#x = chi + xi
ML2_1 <- FHLR1 + fcmat1.boost[,1]

#MSE
mean((ML2_1[,1] - act_data[53:78,10])^2) # 0.1251423

#Graph
ML2_g = rbind(data2_PRFIx, ML2_1)

plot.ts(ML2_g, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PRFIx", xlab="Time (Quarters)")
points(act_data$PRFIx, type = "l", col = 1)
abline(v="52", col=8, lty=3,h=0)

mse.boost1 <- matrix(0, nrow = n, ncol = h) 
for (i in 1:26){
  mse.boost1[i,] <- (PRFIx[52+i,] - ML2_1[i,1])
}


#4 STEPS AHEAD 
w_size <- 52
h <- 4
n <- 26-h+1
fcmat4.boost <- matrix(0, nrow = n, ncol = h)
xi_testmat.boost <- matrix(0, nrow = n, ncol = 1) 


for (i in 1:n){
  xi_testmat.boost[i,] <- xi_rol.mat[(w_size)*(i+1),10]
}


for (i in 1:n){
  ML2 <- gbm(PRFIx ~ ., data = xi_rol[1+(w_size * (i-1)):(w_size * i),],
             distribution = "gaussian", shrinkage = 0.01, cv.folds = 10,
             verbose=F, bag.fraction=0.7, n.trees = 5000, interaction.depth = 4)
  testx <- xi_rol[(-3+ (w_size*(h+i))):((h+i)*(w_size)),]
  fcmat4.boost[i,] <- predict(ML2, newdata = testx, n.trees = 5000) #prediction with boosting
}

mean((fcmat4.boost[,4] - xi_testmat.boost[,1])^2) #MSE for predicting xi's (0.151323)



#x = chi + xi
ML2_4 = FHLR1_4[4:26,] + fcmat4.boost[,4]

# MSE
mean((ML2_4 - act_data[56:78,10])^2) #0.1500653

# Graph 

ML2_4 <- as.data.frame(ML2_4)
ML2_4 <- ML2_4 %>% 
  rename(PRFIx = ML2_4)

ML2_g <- rbind(data4_PRFIx, ML2_4)


plot.ts(ML2_g, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PRFIx", xlab="Time (Quarters)")
points(act_data$PRFIx, type = "l", col = 1)
abline(v="52", col=8, lty=3,h=0)

mse.boost4 <- matrix(0, nrow = n, ncol = 1) 
for (i in 1:23){
  mse.boost4[i,] <- (PRFIx[55+i,] - ML2_4[i,])
}

##ACCURACY  Diebold-Mariano test
dm.test(mse.ar, mse.boost1, h = 1, alternative='g') #p-value = 0.03572 ok
dm.test(mse.ar4, mse.boost4, h = 1, alternative = 'g') #p-value = 0.007284 ok




######## DGR1 ######
# Forecast on x with Lasso 

#1-STEP
act_data.mat <- as.matrix(act_data)


# 1-step
w_size <- 52
h <- 1
n <- 26-h+1
x_fcmat1.lasso <- matrix(0, nrow = n, ncol = h)
x_testmat.lasso <- matrix(0, nrow = n, ncol = h) 

for (i in 1:n){
  x <- act_data.mat[i: (w_size+i-1), -10]
  y <- act_data.mat[i: (w_size+i-1), 10]
  testx <- act_data.mat[w_size+i, -10]
  x_testmat.lasso[i,] <- act_data.mat[(w_size + i), 10] 
  set.seed(1)
  cv.out <- cv.glmnet(x, y, alpha = 1)
  bestlam <- cv.out$lambda.min
  lasso.refit <- glmnet(x, y, alpha = 1, lambda = bestlam)
  x_fcmat1.lasso[i,] <- predict(lasso.refit, s = bestlam, newx = testx)
}

#MSE
mean((x_fcmat1.lasso - x_testmat.lasso[,1])^2) #0.2266998


#Graph
DGR1_1 = x_fcmat1.lasso[,1]
DGR1_1 <- as.data.frame(DGR1_1)
DGR1_1 <- DGR1_1 %>% 
  rename(PRFIx = DGR1_1)
DGR1_1 = rbind(data2_PRFIx, DGR1_1)

plot.ts(DGR1_1, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PRFIx", xlab="Time (Quarters)")
points(act_data$PRFIx, type = "l", col = 1)
abline(v="52", col=8, lty=3,h=0)

mse.dgr1 <- matrix(0, nrow = n, ncol = 1) 
for (i in 1:26){
  mse.dgr1[i,] <- (PRFIx[55+i,] - DGR1_1[i,])
}


# 4-step
w_size <- 52
h <- 4
n <- 26-h+1
x_fcmat4.lasso <- matrix(0, nrow = n, ncol = h)
x_testmat.lasso <- matrix(0, nrow = n, ncol = 1) 

for (i in 1:n){
  x <- act_data.mat[i:(w_size+i-1), -10]
  y <- act_data.mat[i:(w_size+i-1), 10]
  testx <- act_data.mat[(w_size+i), -10]
  x_testmat.lasso[i,] <- act_data.mat[(w_size+i), 10] 
  set.seed(1)
  cv.out <- cv.glmnet(x, y, alpha = 1)
  bestlam <- cv.out$lambda.min
  lasso.refit <- glmnet(x, y, alpha = 1, lambda = bestlam)
  x_fcmat4.lasso[i,] <- predict(lasso.refit, s = bestlam, newx = testx)
}

#MSE
mean((x_fcmat4.lasso[,4] - x_testmat.lasso[,1])^2) #.....


#Graph
DGR1_4 = x_fcmat4.lasso[,4]
DGR1_4 <- as.data.frame(DGR1_4)
DGR1_4 <- DGR1_4 %>% 
  rename(PRFIx = DGR1_4)

DGR1_4g <- rbind(data4_PRFIx, DGR1_4)


plot.ts(DGR1_4g, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PRFIx", xlab="Time (Quarters)")
points(act_data$PRFIx, type = "l", col = 1)
abline(v="52", col=8, lty=3,h=0)


mse.dgr1.4 <- matrix(0, nrow = n, ncol = 1) 
for (i in 1:23){
  mse.dgr1.4[i,] <- (PRFIx[55+i,] - DGR1_4[i,])
}

##ACCURACY  Diebold-Mariano test
dm.test(mse.ar, mse.dgr1, h = 1, alternative='g') #p-value = 0.9707 ok
dm.test(mse.ar4, mse.dgr1.4, h = 4, alternative = 'g') #p-value = 0.8615 ns



######## DGR2 ######
#1STEP
w_size <- 52
h <- 1
n <- 26-h+1

fcmat1.boost <- matrix(0, nrow = n, ncol = h)

for (i in 1:n){
  testx <- act_data[w_size+i,]
  set.seed(12)
  DGR2 <- gbm(PRFIx ~ ., data = act_data[i:w_size+i-1,] , cv.folds = 10, 
              distribution = "gaussian", shrinkage = 0.01, verbose=F, 
              bag.fraction=0.9, n.trees = 1000, interaction.depth = 6)
  fcmat1.boost[i,] <- predict(DGR2, newdata = testx , n.trees = 1000)
}

#MSE
mean((fcmat1.boost[,1] - act_data[53:78,10])^2) #0.3683824

#Graph
fcmat1.boost <- as.data.frame(fcmat1.boost)
fcmat1.boost <- fcmat1.boost %>% 
  rename( PRFIx = V1
  )

DGR2 <- rbind(data2_PRFIx, fcmat1.boost)

plot.ts(DGR2, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PRFIx", xlab="Time (Quarters)")
points(act_data$PRFIx, type = "l", col = 1)
abline(v="52", col=8, lty=3, h=0)

mse.dgr2 <- matrix(0, nrow = n, ncol = 1) 
for (i in 1:26){
  mse.dgr2[i,] <- (PRFIx[52+i,] - fcmat1.boost[i,])
}


#4STEP
w_size <- 52
h <- 4
n <- 26-h+1

fcmat4.boost <- matrix(0, nrow = n, ncol = h)


for (i in 1:n){
  testx <- act_data[(w_size+i):(w_size+i+3),]
  set.seed(12)
  DGR2 <- gbm(PRFIx ~ ., data = act_data[i:w_size+i-1,], cv.folds = 10, 
              distribution = "gaussian", shrinkage = 0.01, verbose=F, 
              bag.fraction=0.9, n.trees = 1000, interaction.depth = 6)
  fcmat4.boost[i,] <- predict(DGR2, newdata = testx , n.trees = 1000)
}

#MSE
mean((fcmat4.boost[,4] - act_data[56:78,10])^2) #0.4295895

#Graph
fcmat4.boost <- as.data.frame(fcmat4.boost[,4])
fcmat4.boost <- fcmat4.boost %>% 
  rename(PRFIx = 'fcmat4.boost[, 4]')

#DGR2 <- rbind(data4_PRFIx[,1], fcmat4.boost[,1])

plot.ts(DGR2, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PRFIx", xlab="Time (Quarters)")
points(act_data$PRFIx, type = "l", col = 1)
abline(v="52", col=8, lty=3, h=0)

mse.dgr2.4 <- matrix(0, nrow = n, ncol = 1) 
for (i in 1:23){
  mse.dgr2.4[i,] <- (PRFIx[55+i,] - fcmat4.boost[i,])
}

##ACCURACY  Diebold-Mariano test
dm.test(mse.ar, mse.dgr2, h = 1, alternative='g') #p-value = 0.9351
dm.test(mse.ar4, mse.dgr2.4, h = 4, alternative = 'g') #p-value = 0.9928 for alternative=l p-val=0.007216

