rm(list = ls())
library(stats)
library(readr)
library(pracma)
library(writexl)
#library(fbi)
library(R.matlab)
library(dplyr)
library(rlang)
library(forecast)

load(file = "data\\act_data.Rda")
data2 <- act_data[1:52,] 

chi_rol <- readMat("data\\chi_rol.mat")
chi_rol <- as.data.frame((chi_rol))
xi_rol <- readMat("data\\xi_rol.mat")
xi_rol <- as.data.frame((xi_rol))

#r=5 q=3 
fcast_chi1 <- readMat("data\\fcast_chi1_NEW.mat")
fcast_chi1 <- as.data.frame(fcast_chi1)
fcast_chi4 <- readMat("data\\fcast_chi4_NEW.mat")
fcast_chi4 <- as.data.frame(fcast_chi4)

colnames(xi_rol) <- colnames(data2)
colnames(chi_rol) <- colnames(data2)
colnames(fcast_chi1) <- colnames(data2)
colnames(fcast_chi4) <- colnames(data2)

## AR(1)
library(forecast)

PCE <- act_data$PCECTPI
PCE <- as.data.frame(PCE)

train_PCE <- window(PCE$PCE, end = 52)
test_PCE <- window(PCE$PCE, start = 53)
fit <- arima(train_PCE, order = c(1,0,0))
w_size <- length(train_PCE)

#1-STEP
h <- 1
n <- length(test_PCE)-h+1
fcmat1 <- matrix(0, nrow = n, ncol = h)

for (i in 1:n){
  x <- PCE[i:(w_size-1+i),1]
  refit <- Arima(x, order = c(1,0,0))
  fcmat1[i,]<- forecast(refit, h = h)$mean
}

cor(data2[,89:93])

#install.packages('lmtest')
library(lmtest)
library(tseries)
coeftest(refit)
adf.test(act_data$PCECTPI)

#RES
mse.ar <- matrix(0, nrow = 26, ncol = 1) 
for (i in 1:26){
  mse.ar[i,] <- (act_data[52+i,]$PCECTPI - fcmat1[i,])
}

#MSE
mean((act_data[53:78,]$PCECTPI- fcmat1)^2) #0.3928518

#GRAPH
fcmat1 <- as.data.frame(fcmat1)
fcmat1 <- fcmat1 %>%
  rename(PCE=V1)

PCE.t <- PCE[1:52,]
PCE.t <- as.data.frame(PCE.t)
PCE.t <- PCE.t %>%
  rename(PCE=PCE.t)

fcmat1 <- rbind(PCE.t, fcmat1)

plot.ts(fcmat1, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PCE", xlab="Time (Quarters)")
points(act_data$PCECTPI, type = "l", col = 1)
abline(v="52", col=8, lty=3, h=0)

#4-STEP
h <- 4
n <- length(test_PCE)-h+1
fcmat4 <- matrix(0, nrow = n, ncol = h)

for (i in 1:n){
  x <- PCE[i:(w_size-1+i),1]
  refit <- Arima(x, order = c(1,0,0))
  fcmat4[i,]<- forecast(refit, h = h)$mean
}

coeftest(refit)
adf.test(act_data$PCECTPI)

fcmat4 <- as.data.frame(fcmat4)
fcmat4 <- fcmat4 %>%
  rename(PCE=V4)


#MSE
mean((fcmat4[,4] - act_data[56:78,]$PCECTPI)^2) #0.4421816

#RES
mse.ar4 <- matrix(0, nrow = 23, ncol = 1) #residuals FHLR1
for (i in 1:23){
  mse.ar4[i,] <- (act_data[55+i,]$PCECTPI - fcmat4[i,4])
}
#save(mse.ar4, file = "output\\mse_ar4_pce.Rda")
#save(mse.ar, file = "output\\mse_ar_pce.Rda")

#graph
fcmat4 <- as.data.frame(fcmat4[,4])
fcmat4 <- fcmat4 %>%
  rename(PCE ='fcmat4[, 4]')

fcmat4 <- rbind(PCE.t, fcmat4)

plot.ts(fcmat4, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PCE", xlab="Time (Quarters)")
points(act_data$PCECTPI, type = "l", col = 1)
abline(v="52", col=8, lty=3, h=0)


############# FHLR1 ############
#1-STEP 
FHLR1 <- fcast_chi1 %>% 
  select(PCECTPI) 
FHLR1 <- FHLR1 %>% 
  rename(PCE=PCECTPI) 

train_PCE <- as.data.frame(train_PCE)
train_PCE <- train_PCE %>% 
  rename(PCE = train_PCE)

FHLR1 <- rbind(train_PCE, FHLR1)

plot.ts(FHLR1, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PCE", xlab="Time (Quarters)")
points(act_data$PCECTPI, type = "l", col = 1)
abline(v="52", col=8, lty=3, h=0)

#MSE
mean((FHLR1[53:78,] - act_data[53:78,]$PCECTPI)^2) #0.3286766
#save(FHLR1, file = "output\\FHLR1_pce.Rda")

#RES
mse.h1 <- matrix(0, nrow = 26, ncol = 1) 
for (i in 1:26){
  mse.h1[i,] <- (act_data[52+i,]$PCECTPI - FHLR1[i,])
}

#4-STEP 
FHLR1.4step <- fcast_chi4 %>% 
  select(PCECTPI)
FHLR1.4step <- FHLR1.4step %>% 
  rename(PCE=PCECTPI)

FHLR1.4step <- rbind(train_PCE, FHLR1.4step)

plot.ts(FHLR1.4step, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PCE", xlab="Time (Quarters)")
points(act_data$PCECTPI, type = "l", col = 1)
abline(v="52", col=8, lty=2, h=0)

#MSE
mean((FHLR1.4step[56:78,] - act_data[56:78,95])^2) #0.480659
#save(FHLR1.4step, file = "output\\FHLR1_4steps_pce.Rda")

#RES
mse.h1.4 <- matrix(0, nrow = 23, ncol = 1) 
for (i in 1:23){
  mse.h1.4[i,] <- (act_data[55+i,]$PCECTPI - FHLR1.4step[i,])
}

##SIGNIFICANCE  Diebold-Mariano test
dm.test(mse.ar, mse.h1, h = 1, alternative='g')  #p-value = 0.922
dm.test(mse.ar4, mse.h1.4, h=4, alternative = 'g') #p-value = 0.997 but for H1=l p-value 0.003019


############# FHLR2 ############
#1-STEP
xi_pce <- xi_rol$PCECTPI
xi_pce <- as.data.frame(xi_pce)
xi_pce_train <- window(xi_pce$xi_pce, end = 52)
fit <- arima(xi_pce_train, order = c(1,0,0))
w_size <- length(xi_pce_train)

h <- 1
n <- 26-h+1
fcmat1.xi <- matrix(0, nrow = n, ncol = h)
xi_pce.iter <- matrix(0, nrow = n, ncol = h)

for (i in 1:n){
  x <- xi_pce[1+(w_size * (i-1)):(w_size * i),]
  refit <- Arima(x, order = c(1,0,0))
  xi_pce.iter[i,] <- xi_pce[(w_size  * (i+h)), 1] 
  fcmat1.xi[i,]<- forecast(refit, h = h)$mean[h]
}
coeftest(refit)
adf.test(xi_rol$PCECTPI)

mean((fcmat1.xi - xi_pce.iter)^2) #0.03569391 MSE of the xi's forecasts

#x=chi+xi
pce_fhlr2 = FHLR1[53:78,1] + fcmat1.xi[,1]
pce_fhlr2 <- as.data.frame(pce_fhlr2)
pce_fhlr2 <- pce_fhlr2 %>% 
  rename( PCE = pce_fhlr2)

FHLR2 <- rbind(train_PCE, pce_fhlr2) 

plot.ts(FHLR2, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PCE", xlab="Time (Quarters)")
points(act_data$PCECTPI, type = "l", col = 1)
abline(v="52", col=8, lty=3,h=0)

#MSE
mean((FHLR2[53:78,] - act_data[53:78,]$PCECTPI)^2) #0.3510767

#RES
mse.h2 <- matrix(0, nrow = 26, ncol = 1) 
for (i in 1:26){
  mse.h2[i,] <- (act_data[52+i,]$PCECTPI - FHLR2[i,])
}

#4-STEP
h <- 4
n <- 26-h+1
fcmat4.xi <- matrix(0, nrow = n, ncol = h)
xi_pce4.iter <- matrix(0, nrow = n, ncol = 1)

for (i in 1:n){
  x <- xi_pce[1+(w_size * (i-1)):(w_size * i),]
  refit <- Arima(x, order = c(1,0,0))
  xi_pce4.iter[i,] <- xi_pce[(w_size  * (i+h)), 1]
  fcmat4.xi[i,]<- forecast(refit, h = h)$mean
}
coeftest(refit)

mean((fcmat4.xi[,4] - xi_pce4.iter)^2) #0.02043501 MSE for xi's

#x=chi+xi
pce_fhlr2.4step = FHLR1.4step[56:78,] + fcmat4.xi[,4]
pce_fhlr2.4step <- as.data.frame(pce_fhlr2.4step)
pce_fhlr2.4step <- pce_fhlr2.4step %>% 
  rename( PCE = pce_fhlr2.4step)

FHLR2.4step <- rbind(train_PCE, pce_fhlr2.4step) 
plot.ts(FHLR2.4step, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PCE", xlab="Time (Quarters)")
points(act_data$PCECTPI, type = "l", col = 1)
abline(v="52", col=8, lty=3,h=0)

#MSE
mean((pce_fhlr2.4step[,1] - act_data[56:78,1])^2) #0.322541

#RES
mse.h2.4 <- matrix(0, nrow = 23, ncol = 1) 
for (i in 1:23){
  mse.h2.4[i,] <- (act_data[55+i,]$PCECTPI - FHLR2.4step[i,])
}

##SIGNIFICANCE  Diebold-Mariano test
dm.test(mse.ar, mse.h2, h = 1, alternative='g') #p-value = 0.9227
dm.test(mse.ar4, mse.h2.4, h=4, alternative='g') #p-value = 0.997 for H1=l p-value = 0.003019


######################## ML1 ##########################
library(glmnet)

#1-STEP
xi_rol.mat <- as.matrix(xi_rol)

w_size <- 52
h <- 1
n <- 26-h+1
fcmat1.lasso <- matrix(0, nrow = n, ncol = h)
xi_testmat.lasso <- matrix(0, nrow = n, ncol = h) 
bestlam.tot <- matrix(0, nrow = n, ncol = h)

for (i in 1:n){
  x <- xi_rol.mat[1+(w_size * (i-1)):(w_size * i), -95]
  y <- xi_rol.mat[1+(w_size * (i-1)):(w_size * i), 95]
  testx <- xi_rol.mat[(w_size  * (i+1)), -95]
  xi_testmat.lasso[i,] <- xi_rol.mat[(w_size  * (i+1)), 95] 
  set.seed(1)
  cv.out <- cv.glmnet(x, y, alpha = 1)
  bestlam <- cv.out$lambda.min
  bestlam.tot[i,] <- bestlam
  lasso.refit <- glmnet(x, y, alpha = 1, lambda = bestlam)
  fcmat1.lasso[i,] <- predict(lasso.refit, s = bestlam, newx = testx)
}

mean((fcmat1.lasso - xi_testmat.lasso[,1])^2) #MSE for predicting xi's

ML1 = fcast_chi1[,95] + fcmat1.lasso # x = chi+ xi

#MSE
mean((ML1 - act_data[53:78,95])^2) #0.3327508
c.lass0 <- coef(lasso.refit )
c.lass0[c.lass0 != 0]
min(bestlam.tot) 
max(bestlam.tot) 


#graph
ML1 <- as.data.frame(ML1)
ML1 <- ML1 %>%
  rename(PCECTPI = V1)
data_pce <- data2[,95]
data_pce <- as.data.frame(data_pce)
data_pce <- data_pce %>%
  rename(PCECTPI = data_pce)
ML1_g = rbind(data_pce, ML1)

plot.ts(ML1_g, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PCE", xlab="Time (Quarters)")
points(act_data$PCECTPI, type = "l", col = 1)
abline(v="52", col=8, lty=3,h=0)

#RES
mse.lasso1 <- matrix(0, nrow = 26, ncol = 1) 
for (i in 1:26){
  mse.lasso1[i,] <- (act_data[52+i,]$PCECTPI - ML1[i,])
}

#4-STEPS
xi_rol.mat <- as.matrix(xi_rol)

w_size <- 52
h <- 4
n <- 26-h+1
fcmat4.lasso <- matrix(0, nrow = n, ncol = h)
xi_testmat4.lasso <- matrix(0, nrow = n, ncol = h) 

for (i in 1:n){
  x <- xi_rol.mat[1+(w_size * (i-1)):(w_size * i), -95]
  y <- xi_rol.mat[1+(w_size * (i-1)):(w_size * i), 95]
  testx <- xi_rol.mat[(w_size  * (i+h)), -95]
  xi_testmat4.lasso[i,] <- xi_rol.mat[(w_size  * (i+h)), 95] 
  set.seed(1)
  cv.out <- cv.glmnet(x, y, alpha = 1)
  bestlam <- cv.out$lambda.min
  lasso.refit <- glmnet(x, y, alpha = 1, lambda = bestlam)
  fcmat4.lasso[i,] <- predict(lasso.refit, s = bestlam, newx = testx)
}

mean((fcmat4.lasso - xi_testmat4.lasso[,1])^2) #MSE for predicting xi's

ML1.4 = fcast_chi4[4:26,95] + fcmat4.lasso[,4] # x = chi+ xi
ML1.4 <- as.data.frame(ML1.4)

#MSE
mean((ML1.4[,1] - act_data[56:78,95])^2) #0.4677157

#graph
ML1.4 <- as.data.frame(ML1.4)
ML1.4 <- ML1.4 %>%
  rename(PCECTPI = ML1.4)
ML1.4_g = rbind(data_pce, ML1.4)

plot.ts(ML1.4_g, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PCE", xlab="Time (Quarters)")
points(act_data$PCECTPI, type = "l", col = 1)
abline(v="52", col=8, lty=3,h=0)



#RES
mse.lasso4 <- matrix(0, nrow = 23, ncol = 1) 
for (i in 1:23){
  mse.lasso4[i,] <- (act_data[55+i,]$PCECTPI - ML1.4[i,])
}

##SIGNIFICANCE  Diebold-Mariano test
dm.test(mse.ar, mse.lasso1, h = 1, alternative='g')  #p-value = 0.1619
dm.test(mse.ar4, mse.lasso4, h=4, alternative = 'g') #p-value = 0.6488


######################## ML2 ##########################
library(gbm)

#1 STEP AHEAD
w_size <- 52
h <- 1
n <- 26-h+1
fcmat1.boost <- matrix(0, nrow = n, ncol = h)
xi_testmat.boost <- matrix(0, nrow = n, ncol = h) 

for (i in 1:n){
  testx <- xi_rol[(w_size  * (i+1)), ]
  xi_testmat.boost[i,] <- xi_rol[(w_size  * (i+1)), 95] 
  ML2 <- gbm(PCECTPI ~ ., data = xi_rol[1+(w_size * (i-1)):(w_size * i), ], cv.folds = 10, 
             distribution = "gaussian", shrinkage = 0.01, verbose=F, 
             bag.fraction=0.7, n.trees = 5000)
  fcmat1.boost[i,] <- predict(ML2, newdata = testx)
}

min_MSE <- which.min(ML2$cv.error) #find index for n trees with minimum CV error
sqrt(ML2$cv.error[min_MSE]) # get MSE and compute RMSE
gbm.perf(ML2, method = "cv")
which.min(ML2$cv.error) #2735

ML2 = fcast_chi1[,95] + fcmat1.boost #x = chi+ xi

#graph
data2_pce <- data2[1:52,95]
data2_pce <- as.data.frame(data2_pce)
data2_pce <- data2_pce %>%
  rename(PCE='data2_pce')
ML2 <- as.data.frame(ML2)
ML2 <- ML2 %>%
  rename(PCE=V1)
ML2_g = rbind(data2_pce, ML2)

plot.ts(ML2_g, col=2, xlim = c(0, 78), ylim = c(-5,5), ylab="PCE", xlab="Time (Quarters)")
points(act_data$PCECTPI, type = "l", col = 1)
abline(v="52", col=8, lty=3,h=0)


#MSE
mean((ML2[,1] - act_data[53:78,95])^2) #0.2998482

#RES
mse.boost1 <- matrix(0, nrow = n, ncol = 1) 
for (i in 1:n){
  mse.boost1[i,] <- (act_data[52+i,95] - ML2[i,1])
}


#4 STEPS AHEAD 
w_size <- 52
h <- 4
n <- 26-h+1

ML2_4.boost <- matrix(0, nrow = n, ncol = h)
ML2_testmat4.boost <- matrix(0, nrow = n, ncol = h) 

for (i in 1:n){
  testx <- xi_rol[(w_size *(i+h)),]
  ML2_testmat4.boost[i,] <- xi_rol[(w_size  * (i+h)), 95] 
  ML2 <- gbm(PCECTPI ~ ., data = xi_rol[1+(w_size * (i-1)):(w_size * i), ], cv.folds = 10, 
             distribution = "gaussian", shrinkage = 0.01, verbose=F, 
             bag.fraction=0.7, n.trees = 4000)
  ML2_4.boost[i,] <- predict(ML2, newdata = testx)
}

mean((ML2_4.boost[,4] - ML2_testmat4.boost[,4])^2) #0.01146622

xi4_pce <- as.data.frame(ML2_4.boost[,4])
xi4_pce <- xi4_pce %>% 
  rename( PCE = 'ML2_4.boost[, 4]')

ML2.f4 = FHLR1.4step[53:75,] + xi4_pce[1:23,1] #(chi+xi) forecast
ML2.f4 <- as.data.frame(ML2.f4)
ML2.f4 <- ML2.f4 %>% 
  rename( PCE = ML2.f4)

#MSE
mean((ML2.f4[1:23,] - act_data[56:78,95])^2) #0.4711688

#RES
mse.boost4 <- matrix(0, nrow = n, ncol = 1) 
for (i in 1:23){
  mse.boost4[i,] <- (act_data[55+i,95] - ML2.f4[i,])
}

##SIGNIFICANCE  Diebold-Mariano test
dm.test(mse.ar, mse.boost1, h = 1, alternative = 'g') #p-value = 0.08704
dm.test(mse.ar4, mse.boost4, h=4, alternative = 'g') #p-value = 0.9021

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
  x <- act_data.mat[i: (w_size+i-1), -95]
  y <- act_data.mat[i: (w_size+i-1), 95]
  testx <- act_data.mat[w_size+i, -95]
  x_testmat.lasso[i,] <- act_data.mat[(w_size + i), 95] 
  set.seed(1)
  cv.out <- cv.glmnet(x, y, alpha = 1)
  bestlam <- cv.out$lambda.min
  lasso.refit <- glmnet(x, y, alpha = 1, lambda = bestlam)
  x_fcmat1.lasso[i,] <- predict(lasso.refit, s = bestlam, newx = testx)
}

#MSE
mean((x_fcmat1.lasso - x_testmat.lasso[,1])^2) #0.0007685898

## ???
#lasso.coef <- predict(lasso.train, type = "coefficients", s = bestlam)[,]
#lasso.coef[lasso.coef != 0]

mse.dgr1 <- matrix(0, nrow = n, ncol = 1) 
for (i in 1:26){
  mse.dgr1[i,] <- (PCE[55+i,] - x_fcmat1.lasso[i,])
}

# 4-step
w_size <- 52
h <- 4
n <- 26-h+1
x_fcmat4.lasso <- matrix(0, nrow = n, ncol = h)
x_testmat.lasso <- matrix(0, nrow = n, ncol = 1) 

for (i in 1:n){
  x <- act_data.mat[i:(w_size+i-1), -95]
  y <- act_data.mat[i:(w_size+i-1), 95]
  testx <- act_data.mat[(w_size+i), -95]
  x_testmat.lasso[i,] <- act_data.mat[(w_size+i), 95] 
  set.seed(1)
  cv.out <- cv.glmnet(x, y, alpha = 1)
  bestlam <- cv.out$lambda.min
  lasso.refit <- glmnet(x, y, alpha = 1, lambda = bestlam)
  x_fcmat4.lasso[i,] <- predict(lasso.refit, s = bestlam, newx = testx)
}

#MSE
mean((x_fcmat4.lasso[,4] - x_testmat.lasso[,1])^2) #0.0008499755

mse.dgr1.4 <- matrix(0, nrow = n, ncol = 1) 
for (i in 1:23){
  mse.dgr1.4[i,] <- (PCE[55+i,] - x_fcmat4.lasso[i,4])
}

##ACCURACY  Diebold-Mariano test
dm.test(mse.ar, mse.dgr1, h = 1, alternative='g') #p-value = 0.942 ns
dm.test(mse.ar4, mse.dgr1.4, h = 4, alternative = 'g') #p-value = 0.9979 ns



