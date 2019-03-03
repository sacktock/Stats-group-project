library(durham)
data(learndis)

par(mfrow=c(2,2))
par(mfrow=c(1,1))
par(mfrow=c(1,2))
par(mfrow=c(2,1))

domdis <- subset(learndis, ACCOM=="DOM", select = c("PATIENT","COSTS.T1","COSTS.T2"))
hosdis <- subset(learndis, ACCOM=="HOS", select = c("PATIENT","COSTS.T1","COSTS.T2"))
rnhdis <- subset(learndis, ACCOM=="RNH", select = c("PATIENT","COSTS.T1","COSTS.T2"))
sghdis <- subset(learndis, ACCOM=="SGH", select = c("PATIENT","COSTS.T1","COSTS.T2"))


qqnorm(learndis$COSTS.T1)
qqnorm(learndis$COSTS.T2)
#Both show normality with no major flaws for whole set of data


qqnorm(domdis$COSTS.T1)
qqnorm(domdis$COSTS.T2)
#Both show questionable normality, costs.t2 shows better but still not massively viable


qqnorm(hosdis$COSTS.T1)
qqnorm(hosdis$COSTS.T2)
#Better than dom, but still variation from straight line


qqnorm(rnhdis$COSTS.T1)
qqnorm(rnhdis$COSTS.T2)
#Once again better, not quite clear normality but fit to the line with a few exceptions

qqnorm(sghdis$COSTS.T1)
qqnorm(sghdis$COSTS.T2)
#Both show a decent level of normality - could approximate 


stem(domdis$COSTS.T1)
stem(domdis$COSTS.T2)
#t1 shows normal curve with gap,t2 shows no normality

stem(hosdis$COSTS.T1)
stem(hosdis$COSTS.T2)
#t1 no normality, t2 decent evidence

stem(rnhdis$COSTS.T1)
stem(rnhdis$COSTS.T2)
#it would be a stretch to assume normality for t1, t2 has too many ups and downs 

stem(sghdis$COSTS.T1)
stem(sghdis$COSTS.T2)
#could both show vague normality as expected

stem(learndis$COSTS.T1)
stem(learndis$COSTS.T2)
#Confirms normality

hist(domdis$COSTS.T1, breaks=30, freq=FALSE, main = 'Domestic housing costs at time one', xlab = 'Costs of care per patient at five years')
x = seq(min(domdis$COSTS.T1), max(domdis$COSTS.T1), length = 100)
y = dnorm(x, mean = mean(domdis$COSTS.T1), sd = sd(domdis$COSTS.T1))
lines(x,y)
qqnorm(domdis$COSTS.T1)
qqline(domdis$COSTS.T1, col='steelblue', lwd=2)

hist(domdis$COSTS.T2, breaks=30, freq=FALSE, main = 'Domestic housing costs at time two', xlab = 'Costs of care per patient at ten years')
x = seq(min(domdis$COSTS.T2), max(domdis$COSTS.T2), length = 100)
y = dnorm(x, mean = mean(domdis$COSTS.T2), sd = sd(domdis$COSTS.T2))
lines(x,y)
qqnorm(domdis$COSTS.T2)
qqline(domdis$COSTS.T2, col='steelblue', lwd=2)



hist(hosdis$COSTS.T1, breaks = 30, freq=FALSE, main = 'Hostel costs at time one', xlab = 'Costs of care per patient at five years')
x = seq(min(hosdis$COSTS.T1), max(hosdis$COSTS.T1), length = 100)
y = dnorm(x, mean = mean(hosdis$COSTS.T1), sd = sd(hosdis$COSTS.T1))
lines(x,y)
qqnorm(hosdis$COSTS.T1)
qqline(hosdis$COSTS.T1, col='steelblue', lwd=2)

hist(hosdis$COSTS.T2, breaks = 30, freq=FALSE, main = 'Hostel costs at time one', xlab = 'Costs of care per patient at five years')
x = seq(min(hosdis$COSTS.T2), max(hosdis$COSTS.T2), length = 100)
y = dnorm(x, mean = mean(hosdis$COSTS.T2), sd = sd(hosdis$COSTS.T2))
lines(x,y)
qqnorm(hosdis$COSTS.T2)
qqline(hosdis$COSTS.T2, col='steelblue', lwd=2)


hist(rnhdis$COSTS.T1, breaks = 30, freq=FALSE, main = 'Residential and nursing home costs at time one', xlab = 'Costs of care per patient at five years')
x = seq(min(rnhdis$COSTS.T1), max(rnhdis$COSTS.T1), length = 100)
y = dnorm(x, mean = mean(rnhdis$COSTS.T1), sd = sd(rnhdis$COSTS.T1))
lines(x,y)
qqnorm(rnhdis$COSTS.T1)
qqline(rnhdis$COSTS.T1, col='steelblue', lwd=2)

hist(rnhdis$COSTS.T2, main = 'Residential and nursing home costs at time two', xlab = 'Costs of care per patient at ten years')
x = seq(min(rnhdis$COSTS.T2), max(rnhdis$COSTS.T2), length = 100)
y = dnorm(x, mean = mean(rnhdis$COSTS.T2), sd = sd(rnhdis$COSTS.T2))
lines(x,y)
qqnorm(rnhdis$COSTS.T2)
qqline(rnhdis$COSTS.T2, col='steelblue', lwd=2)



hist(sghdis$COSTS.T1, breaks = 30, freq=FALSE, main = 'Professionally staffed homes costs at time one', xlab = 'Costs of care per patient at five years')
x = seq(min(sghdis$COSTS.T1), max(sghdis$COSTS.T1), length = 100)
y = dnorm(x, mean = mean(sghdis$COSTS.T1), sd = sd(sghdis$COSTS.T1))
lines(x,y)
qqnorm(sghdis$COSTS.T1)
qqline(sghdis$COSTS.T1, col='steelblue', lwd=2)

hist(sghdis$COSTS.T2, main = 'Professionally staffed homes costs at time two', xlab = 'Costs of care per patient at ten years')
x = seq(min(sghdis$COSTS.T2), max(sghdis$COSTS.T2), length = 100)
y = dnorm(x, mean = mean(sghdis$COSTS.T2), sd = sd(sghdis$COSTS.T2))
lines(x,y)
qqnorm(sghdis$COSTS.T2)
qqline(sghdis$COSTS.T2, col='steelblue', lwd=2)



hist(learndis$COSTS.T1, main = 'Patient costs at time one', xlab = 'Costs of care per patient at five years')
x = seq(min(learndis$COSTS.T1), max(learndis$COSTS.T1), length = 100)
y = dnorm(x, mean = mean(learndis$COSTS.T1), sd = sd(learndis$COSTS.T1))
lines(x,y)
qqnorm(learndis$COSTS.T1)
qqline(learndis$COSTS.T1, col='steelblue', lwd=2)

hist(learndis$COSTS.T2, main = 'Patient costs at time two', xlab = 'Costs of care per patient at ten years')
x = seq(min(learndis$COSTS.T2), max(learndis$COSTS.T2), length = 100)
y = dnorm(x, mean = mean(learndis$COSTS.T2), sd = sd(learndis$COSTS.T2))
lines(x,y)
qqnorm(learndis$COSTS.T2)
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

boxplot(learndis$COSTS.T1)
boxplot(learndis$COSTS.T2)
boxplot(domdis$COSTS.T1)
boxplot(domdis$COSTS.T2)
boxplot(hosdis$COSTS.T1)
boxplot(hosdis$COSTS.T2)
boxplot(rnhdis$COSTS.T1)
boxplot(rnhdis$COSTS.T2)
boxplot(sghdis$COSTS.T1)
boxplot(sghdis$COSTS.T2)

dis <- learndis[,c(1,2,3)]
plot(dis)


plot(learndis)
plot(learndis$COSTS.T1, xlab="Index", ylab="Costs.T1")
plot(learndis$COSTS.T2, xlab="Index", ylab="Costs.T2")


plot(domdis)
plot(domdis$COSTS.T1, xlab="Index", ylab="Costs.T1 - DOM")
plot(domdis$COSTS.T2, xlab="Index", ylab="Costs.T2 - DOM")

plot(hosdis)
plot(hosdis$COSTS.T1, xlab="Index", ylab="Costs.T1 - HOS")
plot(hosdis$COSTS.T2, xlab="Index", ylab="Costs.T2 - HOS")

plot(rnhdis)
plot(rnhdis$COSTS.T1, xlab="Index", ylab="Costs.T1 - RNH")
plot(rnhdis$COSTS.T2, xlab="Index", ylab="Costs.T2 - RNH")

plot(sghdis)
plot(sghdis$COSTS.T2, xlab="Index", ylab="Costs.T1 - SGH")
plot(sghdis$COSTS.T1, xlab="Index", ylab="Costs.T2 - SGH")

cor(domdis$COSTS.T2,domdis$COSTS.T1)
cor(hosdis$COSTS.T2,hosdis$COSTS.T1)
cor(rnhdis$COSTS.T2,rnhdis$COSTS.T1)
cor(sghdis$COSTS.T2,sghdis$COSTS.T1)
cor(learndis$COSTS.T2,learndis$COSTS.T1)

accom <-factor(learndis$ACCOM,c(1,2,3,4), labels=c("DOM","HOS","RNH","SGH"))


lmdom <- lm(domdis$COSTS.T2~domdis$COSTS.T1)
lmhos <- lm(hosdis$COSTS.T2~hosdis$COSTS.T1)
lmrnh <- lm(rnhdis$COSTS.T2~rnhdis$COSTS.T1)
lmsgh <- lm(sghdis$COSTS.T2~sghdis$COSTS.T1)
lmdis <- lm(learndis$COSTS.T2~learndis$COSTS.T1)
lmdis2 <- lm(learndis$COSTS.T2~learndis$COSTS.T1+learndis$ACCOM)

summary(lmdom)
summary(lmhos)
summary(lmrnh)
summary(lmsgh)
summary(lmdis)
summary(lmdis2)

residdom <- resid(lmdom)
residhos <- resid(lmhos)
residrnh <- resid(lmrnh)
residsgh <- resid(lmsgh)
residdis <- resid(lmdis)
residdis2 <- resid(lmdis2)

qqnorm(residdom)
qqnorm(residhos)
qqnorm(residrnh)
qqnorm(residsgh)
qqnorm(residdis)
qqnorm(residdis2)
#get better as you go down, regression should work for whole data set
#lmdis2 is the best for resgression


