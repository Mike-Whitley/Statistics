## Normal plots

x = seq(-4,5,length=100)
plot(x,
     0.7*dnorm(x, 0, sqrt(1)),
     pch=21,
     col="blue",
     cex=0.6,
     lwd = 4,
     type="l",
     xlab = "x",
     ylab = "pi_k*f_k",
     main = "Conditional densities multiplied by prior probabilities")
points(x,
       0.30*dnorm(x,1,sqrt(0.5)),
       pch=21,
       col="red",
       cex=0.6,
       lwd = 4,
       type="l")
legend("topright",
       legend = c("Class 0", "Class 1"),
       col = c("blue","red"),
       lwd = 4,
       text.col = "black",
       horiz = FALSE)

points(c(0.955,0.955),
       c(-0.1,0.3),
       lwd = 8,
       col = "black",
       type="l")

points(c(3.045,3.045),
       c(-0.1,0.3),
       lwd = 8,
       col = "black",
       type="l")

####################################################################################
data = read.csv('AutoTrain.csv')
data2 = read.csv('AutoTest.csv')

## STAT318/462 kNN regression function

kNN <- function(k,x.train,y.train,x.pred) {
  # 
  ## This is kNN regression function for problems with
  ## 1 predictor
  #
  ## INPUTS
  #
  # k       = number of observations in nieghbourhood 
  # x.train = vector of training predictor values
  # y.train = vector of training response values
  # x.pred  = vector of predictor inputs with unknown
  #           response values 
  #
  ## OUTPUT
  #
  # y.pred  = predicted response values for x.pred
  
  ## Initialize:
  n.pred <- length(x.pred);		y.pred <- numeric(n.pred)
  
  ## Main Loop
  for (i in 1:n.pred){
    d <- abs(x.train - x.pred[i])
    dstar = d[order(d)[k]]
    y.pred[i] <- mean(y.train[d <= dstar])		
  }
  ## Return the vector of predictions
  invisible(y.pred)
}

kNN2<-kNN(2, data$horsepower, data$mpg, data2$horsepower)
kNN2
TrainMSE2 = mean((data$mpg - kNN2)^2)
TestMSE2 = mean((data2$mpg - kNN2)^2)
TrainMSE2
TestMSE2

kNN5<-kNN(5, data$horsepower, data$mpg, data2$horsepower)
kNN5
TrainMSE5 = mean((data$mpg - kNN5)^2)
TestMSE5 = mean((data2$mpg - kNN5)^2)
TrainMSE5
TestMSE5

kNN10<-kNN(10, data$horsepower, data$mpg, data2$horsepower)
kNN10
TrainMSE10 = mean((data$mpg - kNN10)^2)
TestMSE10 = mean((data2$mpg - kNN10)^2)
TrainMSE10
TestMSE10

kNN20<-kNN(20, data$horsepower, data$mpg, data2$horsepower)
kNN20
TrainMSE20 = mean((data$mpg - kNN20)^2)
TestMSE20 = mean((data2$mpg - kNN20)^2)
TrainMSE20
TestMSE20

kNN30<-kNN(30, data$horsepower, data$mpg, data2$horsepower)
kNN30
TrainMSE30 = mean((data$mpg - kNN30)^2)
TestMSE30 = mean((data2$mpg - kNN30)^2)
TrainMSE30
TestMSE30

kNN50<-kNN(50, data$horsepower, data$mpg, data2$horsepower)
kNN50
TrainMSE50 = mean((data$mpg - kNN50)^2)
TestMSE50 = mean((data2$mpg - kNN50)^2)
TrainMSE50
TestMSE50

kNN100<-kNN(100, data$horsepower, data$mpg, data2$horsepower)
kNN100
TrainMSE100 = mean((data$mpg - kNN100)^2)
TestMSE100 = mean((data2$mpg - kNN100)^2)
TrainMSE100
TestMSE100

TestMSE2
TestMSE5
TestMSE10
TestMSE20
TestMSE30
TestMSE50
TestMSE100

lm1 = lm(mpg ~ horsepower, data = data)
lm2 = lm(mpg ~ horsepower, data = data2)
kNN20


plot(lm1,lm2,points(kNN20))

TestMSE100

