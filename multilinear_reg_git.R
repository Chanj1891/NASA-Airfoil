## clearing the environmental variables
##rm(list=ls())
##loading necessary libraries for the model
install.packages("leaps")
install.packages("MASS")
library(MASS)
library(leaps)

##importing data
airfoil<-read.csv('D:/UTD/R work/AA/airfoil_self_noise.dat',sep="\t",header = TRUE)
head(airfoil)

##regular multilinear regression
model<-lm(SoundLev~.,data=airfoil)
summary(model)
fitted(model)
plot(model)

##prediction using the model
reg<-predict(model,newdata=airfoil)
head(reg)

stepmod<-stepAIC(model,direction="both")
stepmod$anova

##to check the model for different subsets of the regression
leaps<-regsubsets(SoundLev~.,data=airfoil,nbest=10)
summary(leaps)
plot(leaps,scale="adjr2")

################################
##introducing nonlinear variables
################################

## introducing square of the predictor variables
airfoil2<-airfoil
airfoil2$Freq2<-airfoil2$Freq*airfoil2$Freq
airfoil2$Angle2<-airfoil2$Angle*airfoil2$Angle
airfoil2$ChordLen2<-airfoil2$ChordLen*airfoil2$ChordLen
airfoil2$Velocity2<-airfoil2$Velocity*airfoil2$Velocity
airfoil2$Displace2<-airfoil2$Displace*airfoil2$Displace

head(airfoil2)

model2<-lm(SoundLev~.,data=airfoil2)
summary(model2)

##1st validation of model
model2<-lm(SoundLev~.-Velocity2,data=airfoil2)
summary(model2)

##2nd validation of model
leaps2<-regsubsets(SoundLev~.,data=airfoil2,nbest=50,really.big = T)
##summary(leaps2)
plot(leaps2,scale="adjr2")

##3rd validation of model
stepmod2<-stepAIC(model2,direction="both")
stepmod2$anova

##excluding velocity2 yields a significant model

##prediction using the model
reg2<-predict(model2,newdata=airfoil2)
head(reg2)

###############################
##using interation variables##
##############################

airfoil3<-airfoil2
airfoil3$FA<-airfoil2$Freq*airfoil2$Angle
airfoil3$AC<-airfoil2$Angle*airfoil2$ChordLen
airfoil3$CV<-airfoil2$ChordLen*airfoil2$Velocity
airfoil3$VD<-airfoil2$Velocity*airfoil2$Displace
airfoil3$DF<-airfoil2$Displace*airfoil2$Freq

head(airfoil3)

model3<-lm(SoundLev~.,data=airfoil3)
summary(model3)

##1st correction of model
model3<-lm(SoundLev~.-Velocity2,data=airfoil3)
summary(model3)

##2nd correction of model
model3<-lm(SoundLev~.-Velocity2-CV,data=airfoil3)
summary(model3)

##2nd validation of model
leaps3<-regsubsets(SoundLev~.,data=airfoil3,nbest=10,really.big = T)
##summary(leaps3)
plot(leaps3,scale="adjr2")

##3rd validation of model
stepmod3<-stepAIC(model3,direction="both")
stepmod3$anova

##prediction using the model
reg3<-predict(model3,newdata=airfoil3)
head(reg3)

##Model1
stepmod<-stepAIC(model,direction="both")
summary(model)
plot(reg,residuals(model),xlab='Predicted',ylab='Residuals')
abline(a=0,b=1)

##Model2
stepmod2<-stepAIC(model2,direction="both")
summary(model2)
plot(reg2,residuals(model2),xlab='Predicted',ylab='Residuals')

##Model3
stepmod3<-stepAIC(model3,direction="both")
summary(model3)
plot(reg3,residuals(model3),xlab='Predicted',ylab='Residuals')


