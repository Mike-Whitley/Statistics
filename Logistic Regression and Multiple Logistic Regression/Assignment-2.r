library(ISLR)
#####################QUESTION ONE#####################
#Is a bunch of math

#checj it is
#####################QUESTION TWO#####################

#a
dataq1train = read.csv('BankTrain.csv')
dataq1test = read.csv('BankTest.csv')

glmq1a <- glm(y~x1+x3,data= dataq1train, family = binomial )

summary(glmq1a)

#glmq1aa <- glm(y~x1+x3+I(x2^2),data= dataq1, family = binomial )
#summary(glmq1aa)
#This is interesting as the x2^2 value is significant. Ask if that is something we should talk about?

#Talk about the p values and whether it is significant. Intercept is close to 0.05 so will not be statistically significant if 
#using a confidence level of 0.99


#Need to make comments on the logitic regression

#bi

glmq1a.probs=predict(glmq1a,dataq1train,type="response")

glm.pred=rep("Real",960)

glm.pred[glmq1a.probs>.5]="Fake"


datameow = dataq1train[1:960,c(1,3,5)]

datameow = cbind(datameow,glm.pred)

plot(datameow$x3,
     datameow$x1,
     col=datameow$glm.pred,
     main = "Insert title",
     xlab = "x1",
     ylab = "x3")


abline(-(0.22041/-1.31489),-(-0.21738/-1.31489))


#bii

glmq1test.probs=predict(glmq1a,dataq1test,type="response")
glm.pred2=rep("Real",412)

glm.pred2[glmq1test.probs>.5]="Fake"


table(glm.pred2, dataq1test$y)
#Comment on the table output

#biii


glm.pred3=rep("Real",412)

glm.pred3[glmq1test.probs>.3]="Fake"


table(glm.pred3, dataq1test$y)





glm.pred4=rep("Real",412)

glm.pred4[glmq1test.probs>.6]="Fake"


table(glm.pred4, dataq1test$y)

#Comment on this. 







