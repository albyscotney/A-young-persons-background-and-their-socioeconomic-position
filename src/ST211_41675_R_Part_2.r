# 41675


library(arm)
library(car)
library(cowplot)
library(dplyr)
library(forcats)
library(ggplot2)
library(gridExtra)
library(tidyr)


data.1<-read.csv("EOTST2112020.csv", header = T)
#data.1 is the original dataset 

head(data.1)
summary(data.1)
str(data.1)
dim(data.1)

#Delete NSID, W4Childck1YP, W6Childliv, W6NEETAct, W8DAGEYCH and W8PUSA
#These columns have less than 30% of the data
data.2<-data.1[-c(3054),-c(1,43,55,57,61,70)]

summary(data.2)
str(data.2)
dim(data.2)


#----------------------------------------------------------------------------------------------------------------------------- 

#When considering W8QDEB2 as the outcome-MLR 1

#Don't delete W8GROW,W8NETW,W8NETA and W8DINCW as they are predictors of income
#------(don't capture the same idea as debt)

#Reformat data

#For continuous predictors - Remove rows with missing data
missing_continuous<-c(which(data.2$W1GrssyrMP<0),which(data.2$W1GrssyrHH<0),
                      which(data.2$W1yschat1<0),which(data.2$W2ghq12scr<0),
                      which(data.2$W4schatYP<0),which(data.2$W6DebtattYP<0),
                      which(data.2$W8DGHQSC<0),which(data.2$W8GROW<0), 
                      which(data.2$W8NETW<0), which(data.2$W8NETA<0),
                      which(data.2$W8DINCW<0))
data.3<-data.2[-missing_continuous,]

summary(data.3)
str(data.3)
dim(data.3) #1827 rows therefore big enough

#For categorical predictors - merge the missing values into one category
data_index.1 <- c(1:63)
for(i in data_index.1){data.3[,i] <- ifelse(data.3[,i]<0,"missing",data.3[,i])}


#Centering continuous predictors where necessary
summary(data.4[,c(30,31,39,51,52,54,61,62,63,66,67)])
data.3$cent.W1GrssyrMP<-with(data.3,W1GrssyrMP-mean(W1GrssyrMP))
data.3$cent.W1GrssyrHH<-with(data.3,W1GrssyrHH-mean(W1GrssyrHH))

data.4<-data.3[,-c(1,2)]

str(data.4)
dim(data.4)

#Coding categorical variables as factors 

data_index.2<-c(1:63)
summary(data.4[,c(30,31,39,51,52,54,61,62,63,66,67)])
categorical_index<-data_index.2[-c(30,31,39,51,52,53,54,57,61,62,63,66,67)]
for (i in categorical_index){data.4[,i]<-as.factor(data.4[,i])}

cols <- c("W1wrk1aMP","W1condur5MP","W1hea2MP","W1NoldBroHS",
         "W1InCarHH", "W1hous12HH", "W1usevcHH","W1hiqualmum","W1wrkfulldad",
         "W1wrkfullmum", "W1empsmum", "W1empsdad","W1ch0_2HH","W1ch3_11HH",
         "W1ch12_15HH", "W1ch16_17HH", "IndSchool", "W1marstatmum","W1depkids",
         "W1famtyp2", "W1nssecfam", "W1ethgrpYP", "W1heposs9YP", "W1hwndayYP",
         "W1truantYP", "W1alceverYP", "W1bulrc", "W1disabYP",
         "W2disc1YP", "W2depressYP", "W4CannTryYP", "W4NamesYP",
         "W4RacismYP","W4empsYP","W5JobYP","W5EducYP",
         "W5Apprent1YP","W6JobYP","W6UnivYP","W6acqno","W6gcse",
         "W6als","W6OwnchiDV","W8DMARSTAT","W8DACTIVITYC",
         "W8DWRK","W8CMSEX","W8TENURE","W8DACTIVITY","W8QMAFI")
data.4[cols] <- lapply(data.4[cols], factor)


str(data.4)

#Changing the baseline for some predictors
#Most common is set as the reference level

data.4$W1hea2MP<-relevel(data.4$W1hea2MP,ref = 2)
data.4$W1hous12HH<-relevel(data.4$W1hous12HH,ref = 2)
data.4$W1hiqualmum<-relevel(data.4$W1hiqualmum,ref = 15)
data.4$W1hiqualdad<-relevel(data.4$W1hiqualdad,ref= "2") #most common is missing so second highest
data.4$W1wrkfullmum<-relevel(data.4$W1wrkfullmum,ref = 2)
data.4$W1empsmum<-relevel(data.4$W1empsmum,ref = 2)
data.4$W1ch12_15HH<-relevel(data.4$W1ch12_15HH,ref = 1)
data.4$W1marstatmum<-relevel(data.4$W1marstatmum, ref = 2)
data.4$W1depkids<-relevel(data.4$W1depkids,ref = 2)
data.4$W1nssecfam<-relevel(data.4$W1nssecfam,ref = 2)
data.4$W1hwndayYP<-relevel(data.4$W1hwndayYP,ref = 3)
data.4$W1truantYP<-relevel(data.4$W1truantYP,ref=2)
data.4$W1alceverYP<-relevel(data.4$W1alceverYP,ref = 1)
data.4$W1bulrc<-relevel(data.4$W1bulrc,ref = 2)
data.4$W1disabYP<-relevel(data.4$W1disabYP,ref = 3)#again
data.4$W2disc1YP<-relevel(data.4$W2disc1YP,ref = 2)
data.4$W4AlcFreqYP<-relevel(data.4$W4AlcFreqYP,ref = 2)
data.4$W4CannTryYP<-relevel(data.4$W4CannTryYP,ref = 2)#
data.4$W4NamesYP<-relevel(data.4$W4NamesYP,ref = 2)
data.4$W4RacismYP<-relevel(data.4$W4RacismYP,ref = 2)
data.4$W4empsYP<-relevel(data.4$W4empsYP,ref = 5)
data.4$W5Apprent1YP<-relevel(data.4$W5Apprent1YP,ref = 2)
data.4$W6UnivYP<-relevel(data.4$W6UnivYP,ref = 2)
data.4$W6EducYP<-relevel(data.4$W6EducYP,ref = 2)
data.4$W6Apprent1YP<-relevel(data.4$W6Apprent1YP,ref = 2)
data.4$W6acqno<-relevel(data.4$W6acqno,ref = 8)
data.4$W6gcse<-relevel(data.4$W6gcse,ref = 3)
data.4$W6als<-relevel(data.4$W6als,ref = 4)
data.4$W6OwnchiDV<-relevel(data.4$W6OwnchiDV,ref = 2)
data.4$W8CMSEX<-relevel(data.4$W8CMSEX,ref = 2)#
data.4$W8TENURE<-relevel(data.4$W8TENURE,ref = 4)
data.4$W8QMAFI<-relevel(data.4$W8QMAFI,ref = 2)

summary(data.4)


#--------------------------------------------------------------------------------------------------------------------------------------------------------------


#Initial model - W8QDEB2.lm.1 (66 predictors)

W8QDEB2.lm.1<-lm(W8QDEB2~., data = data.4)

display(W8QDEB2.lm.1,detail=TRUE)
summary(W8QDEB2.lm.1)
Anova(W8QDEB2.lm.1)
anova(W8QDEB2.lm.1)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#Second model - W8QDEB2.lm.2 (36 predictors)

#Removed predictors based on significance levels from the Anova table and intuition

data.5<-data.4[,-c(5,7,10,11,14,15,16,17,19,23,24,27,31,32,34,36,37,40,41,42,43,44,45,46,47,51,54,56,57,61)]
summary(data.5)
dim(data.5)


W8QDEB2.lm.2<-lm(W8QDEB2~., data = data.5)

display(W8QDEB2.lm.2,detail=TRUE)
summary(W8QDEB2.lm.2)
Anova(W8QDEB2.lm.2)
anova(W8QDEB2.lm.2)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#Outliers

show_outliers <- function(the.linear.model, topN) { # length of data
  n = length(fitted(the.linear.model))
  # number of parameters estimated
  p = length(coef(the.linear.model))
  # standardised residuals over 3
  res.out <- which(abs(rstandard(the.linear.model)) > 3) #sometimes >2
  # topN values
  res.top <- head(rev(sort(abs(rstandard(the.linear.model)))), topN)
  # high leverage values
  lev.out <- which(lm.influence(the.linear.model)$hat > 2 * p/n)
  # topN values
  lev.top <- head(rev(sort(lm.influence(the.linear.model)$hat)), topN)
  # high diffits
  dffits.out <- which(dffits(the.linear.model) > 2 * sqrt(p/n))
  # topN values
  dffits.top <- head(rev(sort(dffits(the.linear.model))), topN)
  # Cook's over 1
  cooks.out <- which(cooks.distance(the.linear.model) > 1)
  # topN cooks
  cooks.top <- head(rev(sort(cooks.distance(the.linear.model))), topN)
  # Create a list with the statistics -- cant do a data frame as different
  # lengths
  list.of.stats <- list(Std.res = res.out, Std.res.top = res.top, Leverage = lev.out,
                        Leverage.top = lev.top, DFFITS = dffits.out, DFFITS.top = dffits.top,
                        Cooks = cooks.out, Cooks.top = cooks.top) # return the statistics
  list.of.stats
}
show_outliers(W8QDEB2.lm.2, 25)

display(W8QDEB2.lm.2)

#Removed data point 1541 as it was an outlier in W8QDEB2 (£400,000 and next biggest was £275,000)
#Removed data point 3054 as we believe 12 younger siblings is an error and affects the diagnostics (second highest number of siblings was 5)

outliers_remove<-c(which(data.5$W8QDEB2>300000))
data.6 <- data.5[-outliers_remove,]
                   
W8QDEB2.lm.3<-lm(W8QDEB2~., data = data.6)

display(W8QDEB2.lm.3)
summary(W8QDEB2.lm.3)
Anova(W8QDEB2.lm.3)


vif(W8QDEB2.lm.3)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------

alias(W8QDEB2.lm.1)
alias(W8QDEB2.lm.2)
alias(W8QDEB2.lm.3)

#Remove W1empsmum and W1empsdad as they are colinear with W1hiqualmum and W1hiqualdad, which are also significant
#Remove W1NoOldBro as it was colinear with W1disabYP

data.7 <- data.6[,-c(4,8,9)]

W8QDEB2.lm.4<-lm(W8QDEB2~., data = data.7)

display(W8QDEB2.lm.4,detail=TRUE)
summary(W8QDEB2.lm.4)
Anova(W8QDEB2.lm.4)
anova(W8QDEB2.lm.4)

vif(W8QDEB2.lm.4)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#Remove W1hiqualmum (GVIF = 11.7), W1hiqualdad (GVIF = 38.1), W1depkids (GVIF = 21.3), W1famtyp2 (GVIF = 58.7)
#High VIF (over 10)

data.8 <- data.7[,-c(5,6,7,8)]

W8QDEB2.lm.5<-lm(W8QDEB2~., data = data.8)

dim(data.8)

display(W8QDEB2.lm.5,detail=TRUE)
summary(W8QDEB2.lm.5)
Anova(W8QDEB2.lm.5)
anova(W8QDEB2.lm.5)

vif(W8QDEB2.lm.5)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#Remove nssecfam, gcse, qmafi as they are non-significant predictors and the signs of their coefficients are 'wrong'

data.9 <- data.8[,-c(6,16)]

W8QDEB2.lm.6<-lm(W8QDEB2~., data = data.9)

display(W8QDEB2.lm.6,detail=TRUE)
summary(W8QDEB2.lm.6)
Anova(W8QDEB2.lm.6)
anova(W8QDEB2.lm.6)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#outliers

show_outliers <- function(the.linear.model, topN) { # length of data
  n = length(fitted(the.linear.model))
  # number of parameters estimated
  p = length(coef(the.linear.model))
  # standardised residuals over 3
  res.out <- which(abs(rstandard(the.linear.model)) > 3) #sometimes >2
  # topN values
  res.top <- head(rev(sort(abs(rstandard(the.linear.model)))), topN)
  # high leverage values
  lev.out <- which(lm.influence(the.linear.model)$hat > 2 * p/n)
  # topN values
  lev.top <- head(rev(sort(lm.influence(the.linear.model)$hat)), topN)
  # high diffits
  dffits.out <- which(dffits(the.linear.model) > 2 * sqrt(p/n))
  # topN values
  dffits.top <- head(rev(sort(dffits(the.linear.model))), topN)
  # Cook's over 1
  cooks.out <- which(cooks.distance(the.linear.model) > 1)
  # topN cooks
  cooks.top <- head(rev(sort(cooks.distance(the.linear.model))), topN)
  # Create a list with the statistics -- cant do a data frame as different
  # lengths
  list.of.stats <- list(Std.res = res.out, Std.res.top = res.top, Leverage = lev.out,
                        Leverage.top = lev.top, DFFITS = dffits.out, DFFITS.top = dffits.top,
                        Cooks = cooks.out, Cooks.top = cooks.top) # return the statistics
  list.of.stats
}
show_outliers(W8QDEB2.lm.6, 25)
display(W8QDEB2.lm.6)


#Noticed that the same points appeared as outliers in mutliple categories, namely DIFFTS and Standardised residuals
#Remove points 3111, 1541, 84, 1678



#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#Removed missing data from our outcome variable as these points

missing_debt<-c(which(data.9$W8QDEB2<0))
data.10 <- data.9[-missing_debt,]


W8QDEB2.lm.7<-lm(W8QDEB2~., data = data.10)

display(W8QDEB2.lm.7,detail=TRUE)
summary(W8QDEB2.lm.7)
Anova(W8QDEB2.lm.7)
anova(W8QDEB2.lm.7)

vif(W8QDEB2.lm.7)

par(mfrow=c(1,3))
plot(W8QDEB2.lm.7,which=c(1,2), cex=0.7, pch=".")
hist(W8QDEB2.lm.7$residuals, main="histogram", xlab="std. residuals")


#--------------------------------------------------------------------------------------------------------------------------------------------------------------


data.11<-data.9[-c(which(data.9$W8QDEB2<=0)),]


W8QDEB2.lm.8<- lm(log(W8QDEB2)~.,data=data.11)
par(mfrow=c(1,3))
plot(W8QDEB2.lm.8,which=c(1,2), cex=0.7, pch=".")
hist(W8QDEB2.lm.8$residuals, main="histogram", xlab="std. residuals")

display(W8QDEB2.lm.8,detail=TRUE)
summary(W8QDEB2.lm.8)
Anova(W8QDEB2.lm.8)
anova(W8QDEB2.lm.8)

vif(W8QDEB2.lm.8)


#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#Standardising continuous predictors to enable easier comparison

data.11$std.W1GrssyrMP <- with((cent.W1GrssyrMP-mean(cent.W1GrssyrMP))/sd(cent.W1GrssyrMP), data=data.11)
data.11$std.W1GrssyrHH <- with((cent.W1GrssyrHH-mean(cent.W1GrssyrHH))/sd(cent.W1GrssyrHH), data=data.11)
data.11$std.W1yschat1 <- with((W1yschat1-mean(W1yschat1))/sd(W1yschat1), data=data.11)
data.11$std.W4schatYP <- with((W4schatYP-mean(W4schatYP))/sd(W4schatYP), data=data.11)


data.12<-data.11[,-c(10,14,27,28)]

W8QDEB2.lm.9<- lm(log(W8QDEB2)~.,data=data.12)
par(mfrow=c(1,3))
plot(W8QDEB2.lm.9,which=c(1,2), cex=0.7, pch=".")
hist(W8QDEB2.lm.9$residuals, main="histogram", xlab="std. residuals")

display(W8QDEB2.lm.9,detail=TRUE)
summary(W8QDEB2.lm.9)
Anova(W8QDEB2.lm.9)
anova(W8QDEB2.lm.9)

vif(W8QDEB2.lm.9)

summary(data.12)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#Scatter plots of continuous predictors
summary(data.12)
p1<- ggplot(data.12, aes(x=log(W8QDEB2), y=W8DINCW)) +geom_point()
p1<-p1+geom_smooth(method = "lm", fill=NA)
p1

p2<- ggplot(data.12, aes(x=log(W8QDEB2), y=W8NETW)) +geom_point()
p2<-p2+geom_smooth(method = "lm", fill=NA)
p2

p3<- ggplot(data.12, aes(x=log(W8QDEB2), y=W8NETA)) +geom_point()
p3<-p3+geom_smooth(method = "lm", fill=NA)
p3

p4<- ggplot(data.12, aes(x=log(W8QDEB2), y=std.W1GrssyrMP)) +geom_point()
p4<-p4+geom_smooth(method = "lm", fill=NA)
p4

p5<- ggplot(data.12, aes(x=log(W8QDEB2), y=std.W1GrssyrHH)) +geom_point()
p5<-p5+geom_smooth(method = "lm", fill=NA)
p5

p6<- ggplot(data.12, aes(x=log(W8QDEB2), y=std.W1yschat1)) +geom_point()
p6<-p6+geom_smooth(method = "lm", fill=NA)
p6

p7<- ggplot(data.12, aes(x=log(W8QDEB2), y=std.W4schatYP)) +geom_point()
p7<-p7+geom_smooth(method = "lm", fill=NA)
p7

#Removed outlier
data.13<-data.12[-c(which(data.12$W8NETW>7500)),]
p8<- ggplot(data.13, aes(x=log(W8QDEB2), y=W8NETW)) +geom_point()
p8<-p8+geom_smooth(method = "lm", fill=NA)
p8


#--------------------------------------------------------------------------------------------------------------------------------------------------------------


#Boxplot of W8DDEGP 
summary(data.13)
p9<- ggplot(data.13, aes(y=log(W8QDEB2), x=W8DDEGP)) +geom_boxplot()
p9
#Removed as there was no significant difference between levels of the predictor


#Boxplot of W1hous12HH 
p10<- ggplot(data.13, aes(y=log(W8QDEB2), x=W1hous12HH)) +geom_boxplot()
p10
vif(W8QDEB2.lm.9)
#Removed as there was no significant difference between categories and the outcome + high vif (over 10)


data.14<-data.13[,-c(4,16)]

W8QDEB2.lm.10 <- lm(log(W8QDEB2)~.,data=data.14)
par(mfrow=c(1,3))
plot(W8QDEB2.lm.10,which=c(1,2), cex=0.7, pch=".")
hist(W8QDEB2.lm.10$residuals, main="histogram", xlab="std. residuals")

display(W8QDEB2.lm.10,detail=TRUE)
summary(W8QDEB2.lm.10)
Anova(W8QDEB2.lm.10)
anova(W8QDEB2.lm.10)

vif(W8QDEB2.lm.10)


#--------------------------------------------------------------------------------------------------------------------------------------------------------------

p11<- ggplot(data.14, aes(y=log(W8QDEB2), x=W1hea2MP, colour=factor(W1hea2MP))) +geom_boxplot()
p11

p12<- ggplot(data.14, aes(y=log(W8QDEB2), x=W1famtyp2, colour=factor(W1famtyp2))) +geom_boxplot()
p12

p13<- ggplot(data.14, aes(y=log(W8QDEB2), x=W1hwndayYP, colour=factor(W1hwndayYP))) +geom_boxplot()
p13

#Merge levels for W1hwndayYP

W1hwndayYP.1 = data.14$W1hwndayYP
data.14$W1hwndayYP.2<-with(data.14,Recode(W1hwndayYP.1,"c('1','2')='0-1'"))
data.14$W1hwndayYP.3<-with(data.14,Recode(W1hwndayYP.2,"c('3','4')='2-3'"))
data.14$W1hwndayYP.4<-with(data.14,Recode(W1hwndayYP.3,"c('5','6')='4-5'"))
data.14$W1hwndayYP.4<-factor(data.14$W1hwndayYP.4,
                             levels = c("0-1", "2-3", "4-5"))

p13<- ggplot(data.14, aes(y=log(W8QDEB2), x=W1hwndayYP.4, colour=factor(W1hwndayYP.4))) +geom_boxplot()
p13

p14<- ggplot(data.14, aes(y=log(W8QDEB2), x=W1truantYP, colour=factor(W1truantYP))) +geom_boxplot()
p14

p15<- ggplot(data.14, aes(y=log(W8QDEB2), x=W1bulrc,  colour=factor(W1bulrc))) +geom_boxplot()
p15

p16<- ggplot(data.14, aes(y=log(W8QDEB2), x=W1disabYP, colour=factor(W1disabYP))) +geom_boxplot()
p16

p17<- ggplot(data.14, aes(y=log(W8QDEB2), x=W2depressYP, colour=factor(W2depressYP))) +geom_boxplot()
p17

p18<- ggplot(data.14, aes(y=log(W8QDEB2), x=W4CannTryYP, colour=factor(W4CannTryYP))) +geom_boxplot()
p18

p19<- ggplot(data.14, aes(y=log(W8QDEB2), x=W4empsYP, colour=factor(W4empsYP))) +geom_boxplot()
p19

#Merge levels for W4empsYP

W4empsYP.1 = data.14$W4empsYP
data.14$W4empsYP.2<-with(data.14,Recode(W4empsYP.1,"c('1','2') ='Employed'"))
data.14$W4empsYP.3<-with(data.14,Recode(W4empsYP.2,"c('3')='Unemployed'"))
data.14$W4empsYP.4<-with(data.14,Recode(W4empsYP.3,"c('4')='Training Course'"))
data.14$W4empsYP.5<-with(data.14,Recode(W4empsYP.4,"c('5')='Full-Time Education'"))
data.14$W4empsYP.6<-with(data.14,Recode(W4empsYP.5,"c('6','7','8','9')='Other'"))
data.14$W4empsYP.6<-factor(data.14$W4empsYP.6,
                             levels = c("Employed", "Unemployed", "Training Course", "Full-Time Education", "Other" ))

p19<- ggplot(data.14, aes(y=log(W8QDEB2), x=W4empsYP.6, colour=factor(W4empsYP.6))) +geom_boxplot()
p19

p20<- ggplot(data.14, aes(y=log(W8QDEB2), x=W6als, colour=factor(W6als))) +geom_boxplot()
p20

p21<- ggplot(data.14, aes(y=log(W8QDEB2), x=W8CMSEX, colour=factor(W8CMSEX))) +geom_boxplot()
p21

p22<- ggplot(data.14, aes(y=log(W8QDEB2), x=W8DACTIVITY, colour=factor(W8DACTIVITY))) +geom_boxplot()
p22

#Merge levels for W8DACTIVITY

W8DACTIVITY.1 = data.14$W8DACTIVITY
data.14$W8DACTIVITY.2<-with(data.14,Recode(W8DACTIVITY.1,"c('1') ='Employed F/T'"))
data.14$W8DACTIVITY.3<-with(data.14,Recode(W8DACTIVITY.2,"c('2')='Employed P/T'"))
data.14$W8DACTIVITY.4<-with(data.14,Recode(W8DACTIVITY.3,"c('3','4')='Self-Employed'"))
data.14$W8DACTIVITY.5<-with(data.14,Recode(W8DACTIVITY.4,"c('13')='Other'"))
data.14$W8DACTIVITY.6<-with(data.14,Recode(W8DACTIVITY.5,"c('6','7','12')='In Education'"))
data.14$W8DACTIVITY.6<-factor(data.14$W8DACTIVITY.6,
                           levels = c("Employed F/T", "Employed P/T", "Self-Employed", "Other", "In Education" ))

p22<- ggplot(data.14, aes(y=log(W8QDEB2), x=W8DACTIVITY.6, colour=factor(W8DACTIVITY.6))) +geom_boxplot()
p22

summary(data.14)
p23<- ggplot(data.14, aes(y=log(W8QDEB2), x=W8QMAFI, colour=factor(W8QMAFI))) +geom_boxplot()
p23



#Remove W1hea2MP, W1hwndayYP, W1bulrc
#Remove data columns added when merging levels
data.15<-data.14[,-c(3,5,7,11,18,23,24,27:29,30:33,35:38)]

W8QDEB2.lm.11<- lm(log(W8QDEB2)~., data=data.15)

par(mfrow=c(1,3))
plot(W8QDEB2.lm.11,which=c(1,2), cex=0.7, pch=".")
hist(W8QDEB2.lm.11$residuals, main="histogram", xlab="std. residuals")

Anova(W8QDEB2.lm.11)
anova(W8QDEB2.lm.11)
summary(W8QDEB2.lm.11)
display(W8QDEB2.lm.11)


#--------------------------------------------------------------------------------------------------------------------------------------------------------------

vif(W8QDEB2.lm.11)

#Remove W8NETW as it captures the same idea as W8NETA, which is significant

data.16<-data.15[,-c(5:7,14,18:20)]

W8QDEB2.lm.12<- lm(log(W8QDEB2)~., data=data.16)

par(mfrow=c(1,3))
plot(W8QDEB2.lm.12,which=c(1,2), cex=0.7, pch=".")
hist(W8QDEB2.lm.12$residuals, main="histogram", xlab="std. residuals")

Anova(W8QDEB2.lm.12)
anova(W8QDEB2.lm.12)
summary(W8QDEB2.lm.12)
display(W8QDEB2.lm.12)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#Interactions

data.16$interaction.1 = interaction(data.16$W8CMSEX, data.16$W8DACTIVITY.6)
p24 = ggplot(aes(y = log(W8QDEB2), x = interaction.1, color=factor(W8DACTIVITY.6)), data = data.16) + geom_boxplot()
p24

data.16$interaction.2 = interaction(data.16$W8CMSEX, data.16$W8QMAFI)
p25 = ggplot(aes(y = log(W8QDEB2), x = interaction.2, color=factor(W8QMAFI)), data = data.16) + geom_boxplot()
p25

data.16$interaction.3 = interaction(data.16$W8CMSEX, data.16$W8TENURE)
p26 = ggplot(aes(y = log(W8QDEB2), x = interaction.3, color=factor(W8TENURE)), data = data.16) + geom_boxplot()
p26

data.16$interaction.4 = interaction(data.16$W8QMAFI, data.16$W8TENURE)
p26 = ggplot(aes(y = log(W8QDEB2), x = interaction.4, color=factor(W8TENURE)), data = data.16) + geom_boxplot()
p26

data.16$interaction.5 = interaction(data.16$W8CMSEX, data.16$W1truantYP)
p26 = ggplot(aes(y = log(W8QDEB2), x = interaction.5, color=factor(W1truantYP)), data = data.16) + geom_boxplot()
p26

data.16$interaction.6 = interaction(data.16$W8DACTIVITY, data.16$W8TENURE)
p26 = ggplot(aes(y = log(W8QDEB2), x = interaction.6, color=factor(W8TENURE)), data = data.16) + geom_boxplot()
p26

data.16$interaction.7 = interaction(data.16$W8DACTIVITY, data.16$W6als)
p27 = ggplot(aes(y = log(W8QDEB2), x = interaction.7, color=factor(W6als)), data = data.16) + geom_boxplot()
p27

data.16$interaction.8 = interaction(data.16$W8QMAFI, data.16$W6als)
p28 = ggplot(aes(y = log(W8QDEB2), x = interaction.8, color=factor(W6als)), data = data.16) + geom_boxplot()
p28

W8QDEB2.lm.13<- lm(log(W8QDEB2)~., data=data.16)

par(mfrow=c(1,3))
plot(W8QDEB2.lm.13,which=c(1,2), cex=0.7, pch=".")
hist(W8QDEB2.lm.13$residuals, main="histogram", xlab="std. residuals")

Anova(W8QDEB2.lm.13)
anova(W8QDEB2.lm.13)
summary(W8QDEB2.lm.13)
display(W8QDEB2.lm.13)

#Remove all interactions as they aren't 

data.17<-data.16[,-c(15,16,17,18,19,20,21,22)]

W8QDEB2.lm.14<- lm(log(W8QDEB2)~., data=data.17)

par(mfrow=c(1,3))
plot(W8QDEB2.lm.14,which=c(1,2), cex=0.7, pch=".")
hist(W8QDEB2.lm.14$residuals, main="histogram", xlab="std. residuals")

Anova(W8QDEB2.lm.14)
anova(W8QDEB2.lm.14)
summary(W8QDEB2.lm.14)
display(W8QDEB2.lm.14)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------

data.18<-data.17[,-c(4,5,7,8,13,14)]

W8QDEB2.lm.15<- lm(log(W8QDEB2)~., data=data.18)

par(mfrow=c(1,3))
plot(W8QDEB2.lm.15,which=c(1,2), cex=0.7, pch=".")
hist(W8QDEB2.lm.15$residuals, main="histogram", xlab="std. residuals")

Anova(W8QDEB2.lm.15)
anova(W8QDEB2.lm.15)
summary(W8QDEB2.lm.15)
display(W8QDEB2.lm.15)


#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#Cross-validation

data.18$W8QDEB2 <- with(log(W8QDEB2), data=data.18)
newdata<-data.18
model.1<-lm(W8QDEB2~., data=training.set)

#some cross validation on this final final model
#3 80/20 splits
for(i in 1:3){
  cross.val<-sample(1:nrow(newdata),0.8*nrow(newdata) , replace=FALSE)
  training.set<-newdata[cross.val,]
  test.set<-newdata[-cross.val,]
  model.1
  pred.val.set<-data.frame(predicted=predict(model.1,test.set), 
                           original=test.set$W8QDEB2,error=(predict(model.1,test.set)-test.set$W8QDEB2))
  if(i==1){
    p1<-ggplot(data=pred.val.set, aes(x=predicted,y=original))+geom_point()+theme_bw()
    p1<-p1+geom_smooth(method="lm", se=FALSE) 
    p1<-p1+geom_abline(slope=1,intercept=0, linetype="dashed")
    p2<-ggplot(data=pred.val.set, aes(x=predicted,y=error))+geom_point()+theme_bw()
  }else{
    if(i==2){
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="red")
      p1<-p1+geom_smooth(method="lm", se=FALSE, color="darkred") 
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="red")
    }else{
      p1<-p1+geom_point(data=pred.val.set, aes(x=predicted,y=original), color="green")
      p1<-p1+geom_smooth(method="lm", se=FALSE, color="darkgreen") 
      p2<-p2+geom_point(data=pred.val.set, aes(x=predicted,y=error), color="green")
      p2<-p2+geom_abline(slope=0,intercept=sd(pred.val.set$error), linetype="dashed")
      p2<-p2+geom_abline(slope=0,intercept=0)
      p2<-p2+geom_abline(slope=0,intercept=-sd(pred.val.set$error), linetype="dashed")
    }}}
grid.arrange(p1,p2,nrow=1)


#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#Final Model

final.model<-lm(W8QDEB2~., data=data.18)

par(mfrow=c(1,3))
plot(W8QDEB2.lm.15,which=c(1,2), cex=0.7, pch=".")
hist(W8QDEB2.lm.15$residuals, main="histogram", xlab="std. residuals")

Anova(W8QDEB2.lm.15)
vif(W8QDEB2.lm.15)

