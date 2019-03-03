library(durham)
data(learndis)

#par(mfrow=c(1,1))
#par(mfrow=c(1,2))
#par(mfrow=c(2,1))

domdis <- subset(learndis, ACCOM=="DOM", select = c("PATIENT","COSTS.T1","COSTS.T2"))
hosdis <- subset(learndis, ACCOM=="HOS", select = c("PATIENT","COSTS.T1","COSTS.T2"))
rnhdis <- subset(learndis, ACCOM=="RNH", select = c("PATIENT","COSTS.T1","COSTS.T2"))
sghdis <- subset(learndis, ACCOM=="SGH", select = c("PATIENT","COSTS.T1","COSTS.T2"))

stem(domdis$COSTS.T1)
stem(domdis$COSTS.T2)

stem(hosdis$COSTS.T1)
stem(hosdis$COSTS.T2)

stem(rnhdis$COSTS.T1)
stem(rnhdis$COSTS.T2)

stem(sghdis$COSTS.T1)
stem(sghdis$COSTS.T2)

stem(learndis$COSTS.T1)
stem(learndis$COSTS.T2)

hist(domdis$COSTS.T1, freq=FALSE, main = 'DOM COST.T1', xlab = 'Cost')
x = seq(min(domdis$COSTS.T1), max(domdis$COSTS.T1), length = 100)
y = dnorm(x, mean = mean(domdis$COSTS.T1), sd = sd(domdis$COSTS.T1))
lines(x,y)
qqnorm(domdis$COSTS.T1, xlab = 'z', ylab = 'DOM COST.T1')
qqline(domdis$COSTS.T1, col='steelblue', lwd=2)

hist(domdis$COSTS.T2, freq=FALSE, main = 'DOM COST.T2', xlab = 'Cost')
x = seq(min(domdis$COSTS.T2), max(domdis$COSTS.T2), length = 100)
y = dnorm(x, mean = mean(domdis$COSTS.T2), sd = sd(domdis$COSTS.T2))
lines(x,y)
qqnorm(domdis$COSTS.T2, xlab = 'z', ylab = 'DOM COST.T2')
qqline(domdis$COSTS.T2, col='steelblue', lwd=2)

hist(hosdis$COSTS.T1, freq=FALSE, main = 'HOS COST.T1', xlab = 'Cost')
x = seq(min(hosdis$COSTS.T1), max(hosdis$COSTS.T1), length = 100)
y = dnorm(x, mean = mean(hosdis$COSTS.T1), sd = sd(hosdis$COSTS.T1))
lines(x,y)
qqnorm(hosdis$COSTS.T1, xlab = 'z', ylab = 'HOS COST.T1')
qqline(hosdis$COSTS.T1, col='steelblue', lwd=2)

hist(hosdis$COSTS.T2, freq=FALSE, main = 'HOS COST.T2', xlab = 'Cost')
x = seq(min(hosdis$COSTS.T2), max(hosdis$COSTS.T2), length = 100)
y = dnorm(x, mean = mean(hosdis$COSTS.T2), sd = sd(hosdis$COSTS.T2))
lines(x,y)
qqnorm(hosdis$COSTS.T2, xlab = 'z', ylab = 'HOS COST.T2')
qqline(hosdis$COSTS.T2, col='steelblue', lwd=2)


hist(rnhdis$COSTS.T1, freq=FALSE, main = 'RNH COST.T1', xlab = 'Cost')
x = seq(min(rnhdis$COSTS.T1), max(rnhdis$COSTS.T1), length = 100)
y = dnorm(x, mean = mean(rnhdis$COSTS.T1), sd = sd(rnhdis$COSTS.T1))
lines(x,y)
qqnorm(rnhdis$COSTS.T1, xlab = 'z', ylab = 'RNH COST.T1')
qqline(rnhdis$COSTS.T1, col='steelblue', lwd=2)

hist(rnhdis$COSTS.T2, freq=FALSE, main = 'RNH COST.T2', xlab = 'Cost')
x = seq(min(rnhdis$COSTS.T2), max(rnhdis$COSTS.T2), length = 100)
y = dnorm(x, mean = mean(rnhdis$COSTS.T2), sd = sd(rnhdis$COSTS.T2))
lines(x,y)
qqnorm(rnhdis$COSTS.T2, xlab = 'z', ylab = 'RNH COST.T2')
qqline(rnhdis$COSTS.T2, col='steelblue', lwd=2)

hist(sghdis$COSTS.T1, freq=FALSE, main = 'SGH COST.T1', xlab = 'Cost')
x = seq(min(sghdis$COSTS.T1), max(sghdis$COSTS.T1), length = 100)
y = dnorm(x, mean = mean(sghdis$COSTS.T1), sd = sd(sghdis$COSTS.T1))
lines(x,y)
qqnorm(sghdis$COSTS.T1, xlab = 'z', ylab = 'SGH COST.T1')
qqline(sghdis$COSTS.T1, col='steelblue', lwd=2)

hist(sghdis$COSTS.T2, freq=FALSE, main = 'SGH COST.T2', xlab = 'Cost')
x = seq(min(sghdis$COSTS.T2), max(sghdis$COSTS.T2), length = 100)
y = dnorm(x, mean = mean(sghdis$COSTS.T2), sd = sd(sghdis$COSTS.T2))
lines(x,y)
qqnorm(sghdis$COSTS.T2, xlab = 'z', ylab = 'SGH COST.T2')
qqline(sghdis$COSTS.T2, col='steelblue', lwd=2)

hist(learndis$COSTS.T1, freq=FALSE, main = 'Patient COST.T1', xlab = 'Cost')
x = seq(min(learndis$COSTS.T1), max(learndis$COSTS.T1), length = 100)
y = dnorm(x, mean = mean(learndis$COSTS.T1), sd = sd(learndis$COSTS.T1))
lines(x,y)
qqnorm(learndis$COSTS.T1, xlab = 'z', ylab = 'COST.T1')
qqline(learndis$COSTS.T1, col='steelblue', lwd=2)

hist(learndis$COSTS.T2, freq=FALSE, main = 'Patient COST.T2', xlab = 'Cost')
x = seq(min(learndis$COSTS.T2), max(learndis$COSTS.T2), length = 100)
y = dnorm(x, mean = mean(learndis$COSTS.T2), sd = sd(learndis$COSTS.T2))
lines(x,y)
qqnorm(learndis$COSTS.T2, xlab = 'z', ylab = 'COST.T2')
qqline(learndis$COSTS.T2, col='steelblue', lwd=2)

summary(domdis$COSTS.T1)
summary(domdis$COSTS.T2)

summary(hosdis$COSTS.T1)
summary(hosdis$COSTS.T2)

summary(rnhdis$COSTS.T1)
summary(rnhdis$COSTS.T2)

summary(sghdis$COSTS.T1)
summary(sghdis$COSTS.T2)

summary(learndis)
summary(learndis$COSTS.T1)
summary(learndis$COSTS.T2)
#           T1 Mean    T2 Mean
# WHOLE -    783.9      660.6
#DOM -       583.1      390.4 
#HOS -       725.2      612.5
#RNH -       798.4      571.8
#SGH -       853.4      825.3 

boxplot(domdis$COSTS.T1,domdis$COSTS.T2, names = c('DOM COST.T1','DOM COST.T2'), main = 'DOM COST', horizontal = TRUE)
boxplot(hosdis$COSTS.T1,hosdis$COSTS.T2, names = c('HOS COST.T1','HOS COST.T2'), main = 'HOS COST', horizontal = TRUE)
boxplot(rnhdis$COSTS.T1,rnhdis$COSTS.T2, names = c('RNH COST.T1','RNH COST.T2'), main = 'RNH COST', horizontal = TRUE)
boxplot(sghdis$COSTS.T1,sghdis$COSTS.T2, names = c('SGH COST.T1','SGH COST.T2'), main = 'SGH COST', horizontal = TRUE)
boxplot(learndis$COSTS.T1, learndis$COSTS.T2, names = c('Patient COST.T1','Patient COST.T2'), main = 'Patient COST', horizontal = TRUE)

accom <-factor(learndis$ACCOM,c(1,2,3,4), labels=c("DOM","HOS","RNH","SGH"))


lmdom <- lm(domdis$COSTS.T2~domdis$COSTS.T1)
lmhos <- lm(hosdis$COSTS.T2~hosdis$COSTS.T1)
lmrnh <- lm(rnhdis$COSTS.T2~rnhdis$COSTS.T1)
lmsgh <- lm(sghdis$COSTS.T2~sghdis$COSTS.T1)
lmdis <- lm(learndis$COSTS.T2~learndis$COSTS.T1)
#unclear if the multiple regression is valid
lmdis2 <- lm(learndis$COSTS.T2~learndis$COSTS.T1+learndis$ACCOM)

summary(lmdom)
summary(lmhos)
summary(lmrnh)
summary(lmsgh)
summary(lmdis)
#unclear if the multiple regression is valid
summary(lmdis2)

plot(domdis$COSTS.T2~domdis$COSTS.T1)
abline(lmdom)
plot(hosdis$COSTS.T2~hosdis$COSTS.T1)
abline(lmhos)
plot(rnhdis$COSTS.T2~rnhdis$COSTS.T1)
abline(lmrnh)
plot(sghdis$COSTS.T2~sghdis$COSTS.T1)
abline(lmsgh)
plot(learndis$COSTS.T2~learndis$COSTS.T1)
abline(lmdis)
#unclear if the multiple regression is valid
plot(learndis$COSTS.T2~learndis$COSTS.T1+learndis$ACCOM)
abline(lmdis2)


residdom <- resid(lmdom)
residhos <- resid(lmhos)
residrnh <- resid(lmrnh)
residsgh <- resid(lmsgh)
residdis <- resid(lmdis)
#unclear if the multiple regression is valid
residdis2 <- resid(lmdis2)

par(mfrow = c(1,2))
plot(domdis$COSTS.T1,residdom, ylab = 'Residuals')
qqline(y=0)
qqnorm(residdom, xlab = 'z', ylab = 'Residuals')
qqline(residdom, col='steelblue', lwd=2)

plot(hosdis$COSTS.T1,residhos, ylab = 'Residuals')
qqline(y=0)
qqnorm(residhos, xlab = 'z', ylab = 'Residuals')
qqline(residhos, col='steelblue', lwd=2)

plot(rnhdis$COSTS.T1,residrnh, ylab = 'Residuals')
qqline(y=0)
qqnorm(residrnh, xlab = 'z', ylab = 'Residuals')
qqline(residrnh, col='steelblue', lwd=2)

plot(sghdis$COSTS.T1,residsgh, ylab = 'Residuals')
qqline(y=0)
qqnorm(residsgh, xlab = 'z', ylab = 'Residuals')
qqline(residsgh, col='steelblue', lwd=2)

plot(learndis$COSTS.T1,residdis, ylab = 'Residuals')
qqline(y=0)
qqnorm(residdis, xlab = 'z', ylab = 'Residuals')
qqline(residdis, col='steelblue', lwd=2)

#unclear if the multiple regression is valid
plot(learndis$COSTS.T1,residdis2, ylab = 'Residuals')
qqline(y=0)
qqnorm(residdis2, xlab = 'z', ylab = 'Residuals')
qqline(residdis2, col='steelblue', lwd=2)
#get better as you go down, regression should work for whole data set
#lmdis2 is the best for resgression


