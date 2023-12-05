library("dplyr")
library("tseries")
library("readxl")
library(tidyverse)
library(GGally)
library(MASS)
library(car)
library(lmtest)
library(olsrr)
library(EnvStats)



install.packages("zoo")
install.packages("Quandl")
install.packages("stargazer")
library(Quandl)
library(stargazer)
library(zoo)

····VERSIÓN UNO····

#IMPORTAR VARIABLES

BTCdata=Quandl("BCHAIN/MKPRU", start_date=as.Date("2000-01-01"), end_date=as.Date("2023-03-22"))
BTCdata = BTC[-5193,]
colnames(BTCdata)[2] <-"BTC"
difficulty = Quandl("BCHAIN/DIFF", start_date=as.Date("2000-01-01"), end_date=as.Date("2023-03-22"))
difficulty = difficulty[-c(5193:5194),]
hashrate = Quandl("BCHAIN/HRATE", start_date=as.Date("2000-01-01"), end_date=as.Date("2023-03-23"))
hashrate = hashrate[-5193,]
mirev = Quandl("BCHAIN/MIREV", start_date=as.Date("2000-01-01"), end_date=as.Date("2023-03-23"))
mirev = mirev[-c(5193:5194),]
avgblock = Quandl("BCHAIN/AVBLS", start_date=as.Date("2000-01-01"), end_date=as.Date("2023-03-23"))
avgblock = avgblock[-c(5193:5194),]
blockchainsize = Quandl("BCHAIN/BLCHS", start_date=as.Date("2000-01-01"), end_date=as.Date("2023-03-23"))
blockchainsize = blockchainsize[-5193,]
costpertrans = Quandl("BCHAIN/CPTRA", start_date=as.Date("2000-01-01"), end_date=as.Date("2023-03-23"))
costpertrans = costpertrans[-c(5193:5194),]
totBTC = Quandl("BCHAIN/TOTBC", start_date=as.Date("2000-01-01"), end_date=as.Date("2023-03-23"))
totBTC = totBTC[-5193,]
numtransblck = Quandl("BCHAIN/NTRBL", start_date=as.Date("2000-01-01"), end_date=as.Date("2023-03-23"))
numtransblck = numtransblck[-5193,]
naddu = Quandl("BCHAIN/NADDU", start_date=as.Date("2000-01-01"), end_date=as.Date("2023-03-23"))
naddu = naddu[-5193,]
mirevPERhash<-as.data.frame(cbind(mirev$Value/hashrate$Value))
colnames(mirevPERhash)<-c("MinerRevenue/Hashrate")

data21=data.frame(Date=c(BTCdata$Date),BTCprice=c(BTCdata$BTC),Difficulty=c(difficulty$Value),Hashrate=c(hashrate$Value),MinerRevenue=c(mirev$Value),AvgBlockSize=c(avgblock$Value),BlockchainSize=c(blockchainsize$Value),CostPerTransaction=c(costpertrans$Value),TotalBitcoins=c(totBTC$Value),TransactionsPerBlock=c(numtransblck$Value),UniqueBitcoinAddysUsed=c(naddu$Value),MinRevPerHash=c(mirevPERhash$`MinerRevenue/Hashrate`))

data21.1 = data21[-c(4638:5194),]
btccarbon <- read.csv("bitcoincarbon.csv",header = FALSE)
data21.2=cbind(data21.1,CarbonEmissions=C(btccarbon$V3))

ESTADÍSTICA DESCRIPTIVA

plot(difficulty, type="l", main="Difficulty", col = "orange")
lines(blockchainsize, type="l", col = "orange")

plot(difficulty, BTCdata, type="l", main="Difficulty", col = "orange")
plot(log(difficulty$Value),(difficulty$Date),, main="log(Difficulty)")
plot(BTCdata, type="l", col = "orange", main = "Bitcoin Price")
lines(log(difficulty$Value), type="l", col = "blue")
ggplot(data=data21, aes(x=hashrate, y=difficulty))+geom_point()
plot(hashrate, type="l", main="Hashrate", col = "orange")
plot(log(hashrate$Value))
plot(mirev, type="l", main="Miner Revenue", col = "orange")
plot(avgblock, type="l", main="Average Block Size", col = "orange")
plot(blockchainsize, type="l", main="Blockchain Size", col = "orange")
plot(costpertrans, type="l", main="Cost Per Transaction", col = "orange")
plot(totBTC, type="l", main="Total Bitcoins", col = "orange")
plot(numtransblck, type="l", main="Transactions Per Block", col = "orange")
plot(naddu, type="l", main="Unique BTC Addresses Used", col = "orange")
plot(mirevPERhash)

mean(BTCprice, data=data21)

cor(data21$BTCprice, data21$Difficulty)
cor(data21$Hashrate, data21$Difficulty)
cor(data21$MinerRevenue, data21$Difficulty)
cor(data21$AvgBlockSize, data21$Difficulty)
cor(data21$BlockchainSize, data21$Difficulty)
cor(data21$CostPerTransaction, data21$Difficulty)
cor(data21$TotalBitcoins, data21$Difficulty)
cor(data21$TransactionsPerBlock, data21$Difficulty)
cor(data21$UniqueBitcoinAddysUsed, data21$Difficulty)

cor(data21$MinerRevenue, data21$Hashrate)
cor(data21$MinerRevenue, data21$BTCprice)
cor(data21$MinerRevenue, data21$Difficulty)
cor(data21$MinerRevenue, data21$AvgBlockSize)
cor(data21$MinerRevenue, data21$BlockchainSize)
cor(data21$MinerRevenue, data21$CostPerTransaction)
cor(data21$MinerRevenue, data21$TotalBitcoins)
cor(data21$MinerRevenue, data21$TransactionsPerBlock)
cor(data21$MinerRevenue, data21$UniqueBitcoinAddysUsed)

ggpairs(data21, columns = c(2,3), aes(color = "Red"))
ggpairs(data21, columns = c(4,3), aes(color = ""))
ggpairs(data21, columns = c(4:8,3), aes(col = ""))
ggpairs(data21, columns = c(3, 2), aes(color = country, alpha = 0.5),upper = list(continuous = wrap("cor", size = 2.5)))

ggplot(data=data21,aes(x=BTCprice,y=Difficulty))+geom_point()
ggplot(data=data21,aes(x=Difficulty,y=BTCprice))+geom_point()
ggplot(data=data21,aes(x=Hashrate,y=Difficulty))+geom_point()
ggplot(data=data21,aes(x=Difficulty,y=MinerRevenue))+geom_point()
ggplot(data=data21,aes(x=Hashrate,y=MinerRevenue))+geom_point()

REGRESIÓN LINEAL
-----------------------------------------------
model.data21 <- lm(Difficulty ~ Hashrate + BTCprice + Hashrate + BlockchainSize + AvgBlockSize + CostPerTransaction + TransactionsPerBlock, data=data21)
summary(btc_mining4)
anova(model.data21)

model.data21 <- lm(Difficulty ~ Hashrate + BTCprice + TransactionsPerBlock, data=data21)

data21.bw <- stepAIC(btc_mining4, trace=TRUE, direction="backward")
summary(data21.bw)
data21.bw$anova

empty.btc.model <- lm(Difficulty ~ 1, data=data21)
horizonte <- formula(Difficulty~BTCprice+Hashrate+TransactionsPerBlock)
data21.fw <- stepAIC(empty.btc.model, trace=FALSE, direction="forward", scope=horizonte)
data21.fw$anova
summary(data21.fw)

data21.sw <- stepAIC(empty.btc.model, trace=FALSE, direction="both", scope=horizonte)
data21.sw$anova
summary(data21.sw)

lmtest::bptest(btc_mining4)
gqtest(model.data21, order.by = ~Hashrate+BTCprice+MinerRevenue+BlockchainSize+AvgBlockSize, data = data21, fraction = 150)

durbinWatsonTest(btc_mining4,simulate = TRUE,reps = 1000)
dwtest(btc_mining4,alternative ="two.sided",iterations = 1000)
dwtest(btc_mining, alternative = "two.sided")

residuales21=as.data.frame(btc_mining4$residuals)
colnames(residuales21)=c("Residuales")

ggplot(residuales21, aes(sample = Residuales))+
  stat_qq() + stat_qq_line()+
  ggtitle("QQ Plot Residuales")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

ks_norm1<-ks.test(residuales21$Residuales,"pnorm")
ks_norm1$p.value

vif(model.data21)

jb_norm<-jarque.bera.test(residuales21$Residuales)
jb_norm


anova(btc_mining4)
summary(btc_mining4)
vif(btc_mining4)
data21.1.bw <- stepAIC(btc_mining4, trace=TRUE, direction="backward")
summary(data21.1.bw)
data21.1.bw$anova

lmtest::bptest(model.wls)
gqtest(model.wls, order.by = ~Hashrate+BTCprice+CostPerTransaction, data = data21, fraction = 150)

# load the car package
install.packages("car")
library(car)

# estimate the weights using varPower()
myweights <- varPower(btc_mining4)

# Compute the robust standard errors using coeftest
robust_se <- coeftest(btc_mining4, vcov = vcovHC(btc_mining4, type = "HC1"))
install.packages("sandwich")   # Install the sandwich package
library(sandwich)             # Load the sandwich package

# Print the results
summary(robust_se)

variance <- residuals(model.data21)^2
weights <- 1/variance
model.wls <- lm(Difficulty ~ Hashrate + BTCprice + TransactionsPerBlock, data = data21, weights = weights)
bptest(model.wls)

install.packages("orcutt")
library(orcutt)

coeftest(cochran.orcutt(model.data21))
cochran.orcutt(model.data21)
model.cochran <- cochran.orcutt(btc_mining4)

vcov <- NeweyWest(btc_mining4, lag = 1, prewhite = FALSE, sandwich = TRUE)
coef <- coef(btc_mining4)
se <- sqrt(diag(vcov))
results <- data.frame(Coefficient = coef, SE = se)
print(results)
dwtest(btc_mining4, alternative = "two.sided")

-------------------------------------------------
btc_mining <- lm(Difficulty ~ Hashrate + BTCprice , data=data21)
btc_mining2 <- lm(Difficulty ~ Hashrate, data=data21)
btc_mining3 <- lm(Difficulty ~ BTCprice , data=data21)
btc_mining4 <- lm(Difficulty ~ Hashrate + BTCprice + TransactionsPerBlock, data = data21)

anova_btcmining<-anova(btc_mining)
anova_btcmining
anova_btcmining2<-anova(btc_mining2)
anova_btcmining2
summ1<- summary(btc_mining)
summ1
summ2<- summary(btc_mining2)
summ2

miningdata_heteroscedasicidad<-as.data.frame(cbind(btc_mining$fitted.values,btc_mining$residuals))
colnames(miningdata_heteroscedasicidad)<-c("Y_hat","Residuales")

miningdata2_heteroscedasicidad<-as.data.frame(cbind(btc_mining2$fitted.values,btc_mining2$residuals))
colnames(miningdata2_heteroscedasicidad)<-c("Y_hat","Residuales")

ggplot(data=miningdata_heteroscedasicidad)+
  geom_point(aes(x=Y_hat,y=Residuales))+
  ggtitle("Y_hat vs Residuales")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data=miningdata2_heteroscedasicidad)+
  geom_point(aes(x=Y_hat,y=Residuales))+
  ggtitle("Y_hat vs Residuales")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


sqrt_mse1<-sqrt(anova_btcmining[2,3])
sqrt_mse1
sqrt_mse2<-sqrt(anova_btcmining2[2,3])
sqrt_mse2

base.train21<-sample_frac(data21,.8)

miningdata_ol<-as.data.frame(cbind(data21$Difficulty,btc_mining2$residuals/sqrt_mse2))

colnames(miningdata_ol)<-c("x","errores")

ggplot(data=miningdata_ol)+
  geom_point(aes(x=x,y=errores))+
  geom_line(aes(x=x,y=-4),col="Red",alpha=.5)+
  geom_line(aes(x=x,y=4),col="Red",alpha=.5)+
  ggtitle("x vs e/raiz(mse)")+
  ylab("e/raiz(mse)")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

residuales=as.data.frame(btc_mining$residuals)
colnames(residuales)=c("Residuales")
residuales2=as.data.frame(btc_mining2$residuals)
colnames(residuales2)=c("Residuales")

ggplot(residuales, aes(sample = Residuales))+
  stat_qq() + stat_qq_line()+
  ggtitle("QQ Plot Residuales")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data=residuales,aes(x=Residuales,y=..density..))+
  geom_histogram()+
  ggtitle("Histograma Residuales")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

ks_norm<-ks.test(residuales$Residuales,"pnorm")
ks_norm

ggplot(residuales2, aes(sample = Residuales))+
  stat_qq() + stat_qq_line()+
  ggtitle("QQ Plot Residuales")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data=residuales2,aes(x=Residuales,y=..density..))+
  geom_histogram()+
  ggtitle("Histograma Residuales")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

ks_norm2<-ks.test(residuales2$Residuales,"pnorm")
ks_norm2

#Rechazamos ho??

jb_norm<-jarque.bera.test(residuales$Residuales)
jb_norm
jb_norm2<-jarque.bera.test(residuales2$Residuales)
jb_norm2

confianza<-as.data.frame(predict.lm(object=model.data21,newdata=base.train21, interval = "confidence",level=.95))
confianza2<-as.data.frame(predict.lm(object=btc_mining2,newdata=base.train21, interval = "confidence",level=.95))

colnames(confianza)<-c("y_hat","lwr","upr")
colnames(confianza2)<-c("y_hat","lwr","upr")

head(confianza)
head(confianza2)

base.train3<-as.data.frame(c(base.train21,confianza))
base.train4<-as.data.frame(c(base.train21,confianza2))

head(base.train)


p <- ggplot(base.train3) +
  geom_point(aes(TransactionsPerBlock, Difficulty)) +
  geom_line(aes(x=TransactionsPerBlock,y =lwr), color = "red", linetype = "dashed")+
  geom_line(aes(x=TransactionsPerBlock,y = upr), color = "red", linetype = "dashed")+
  geom_line(aes(x=TransactionsPerBlock,y = y_hat), color = "blue")+
  ggtitle("Intervalo de confianza al 95%")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
p

p00<- ggplot(base.train4) +
  geom_point(aes(Hashrate, MinerRevenue)) +
  geom_line(aes(x=Hashrate,y =lwr), color = "red", linetype = "dashed")+
  geom_line(aes(x=Hashrate,y = upr), color = "red", linetype = "dashed")+
  geom_line(aes(x=Hashrate,y = y_hat), color = "blue")+
  ggtitle("Intervalo de confianza al 95%")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
p00


prediccion<-as.data.frame(predict.lm(object=model.data21,newdata=base.train21, interval = "prediction",level=.95))
colnames(prediccion)<-c("y_hat_pred","lwr_pred","upr_pred")
head(prediccion)

base.train5<-as.data.frame(c(base.train4,prediccion))

head(base.train5)

p1 <- ggplot(base.train3) +
  geom_point(aes(BTCprice, Difficulty)) +
  geom_line(aes(x=BTCprice,y =lwr), color = "red", linetype = "dashed")+
  geom_line(aes(x=BTCprice,y = upr), color = "red", linetype = "dashed")+
  geom_line(aes(x=BTCprice,y = y_hat), color = "blue")+
  geom_line(aes(x=BTCprice,y =lwr_pred), color = "#FF7F24", linetype = "dashed")+
  geom_line(aes(x=BTCprice,y = upr_pred), color = "#FF7F24", linetype = "dashed")+
  geom_line(aes(x=BTCprice,y = y_hat_pred), color = "black",, linetype = "dashed")+
  ggtitle("Intervalo de predicción y confianza al 95%")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

p1

base.train.btc<-sample_frac(data21,.8)
base.val21<-setdiff(data21,base.train.btc)
confianza<-as.data.frame(predict.lm(object=model.data21,newdata=base.val21, interval = "confidence",level=.95))

colnames(confianza)<-c("y_hat","lwr","upr")

head(confianza)

base.val21<-as.data.frame(c(base.val21,confianza))

head(base.val21)


p2 <- ggplot(base.val21) +
  geom_point(aes(Hashrate, Difficulty)) +
  geom_line(aes(x=Hashrate,y =lwr), color = "red", linetype = "dashed")+
  geom_line(aes(x=Hashrate,y = upr), color = "red", linetype = "dashed")+
  geom_line(aes(x=Hashrate,y = y_hat), color = "blue")+
  ggtitle("Intervalo de confianza al 95% validación")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
p2

prediccion<-as.data.frame(predict.lm(object=model.data21,newdata=base.train.btc, interval = "prediction",level=.95))

colnames(prediccion)<-c("y_hat_pred","lwr_pred","upr_pred")

head(prediccion)

base.val21<-as.data.frame(c(base.val,prediccion))

head(base.val)


p3 <- ggplot(base.train5) +
  geom_point(aes(CostPerTransaction, Difficulty)) +
  geom_line(aes(x=CostPerTransaction,y =lwr), color = "red", linetype = "dashed")+
  geom_line(aes(x=CostPerTransaction,y = upr), color = "red", linetype = "dashed")+
  geom_line(aes(x=CostPerTransaction,y = y_hat), color = "blue")+
  geom_line(aes(x=CostPerTransaction,y =lwr_pred), color = "#FF7F24", linetype = "dashed")+
  geom_line(aes(x=CostPerTransaction,y = upr_pred), color = "#FF7F24", linetype = "dashed")+
  geom_line(aes(x=CostPerTransaction,y = y_hat_pred), color = "black",, linetype = "dashed")+
  ggtitle("Intervalo de predicción y confianza al 95% validación")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

p3

plot(hpdata$bedrooms, hpdata$price)

stargazer(btc_mining3, btc_mining2, btc_mining4, type="text")

#Bitcoin Monetary Policy
for (i in 0:32)
totalsupply <- 210000*(50/2^i)
plot(totalsupply)

blockreward <- (50/2^i)
plot(blockreward)
