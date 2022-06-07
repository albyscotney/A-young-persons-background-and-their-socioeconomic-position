# 41675


library(arm)
library(car)
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
# --------------------------------------------------------------------------------
sort(table(data.1$W8QMAFI))

# Remove missing values as these take up such a small amount of the data, then arrage the others into finacially positive and finacially negative

missing <- (which(data.1$W8QMAFI<0))
data.2<-data.1[-missing,]
missing

data.2

data.3 <- data.2
data.3$W8QMAFI<-with(data.3,Recode(W8QMAFI,"c('1','2')='1'"))
data.3$W8QMAFI<-with(data.3,Recode(W8QMAFI,"c('5','4','3')='0 '"))
# -------------------------------------------------------------------------------------

#Delete NSID,W4Childck1YP, W6Childliv,W6NEETAct,W8DAGEYCH and W8PUSA
#These columns have less than 30% of the data
summary(data.3[,c(1,43,55,57,61,70)])
data.4<-data.3[,-c(1,43,55,57,61,70)]

# Delete predictors heavily overlapping with others
data.5 <- data.4
summary(data.4)
dim(data.4)
head(data.5[,62,])
summary(data.4[,c(1,62,63,64)])
data.5 <- data.4[,-c(1,62,63,64)] #Include 64 cos its W8NETW

summary(data.5)

# Removing missing rows
missing_continuous<-c(which(data.5$W1GrssyrHH<0),
                      which(data.5$W1yschat1<0),which(data.5$W2ghq12scr<0),
                      which(data.5$W4schatYP<0),which(data.5$W6DebtattYP<0),
                      which(data.5$W8DINCW<0),which(data.5$W8DGHQSC<0),which(data.5$W8NETA<0),which(data.5$W8QDEB2<0))
data.6<-data.5[-missing_continuous,]

dim(data.6)
summary(data.6)

#For categorical predictors - merge the missing values into one category
data_index.1 <- c(1:63)#Changed from 66 to 63
for(i in data_index.1){data.6[,i] <- ifelse(data.6[,i]<0,"missing",data.6[,i])}

summary(data.6)


# code the catagorical predictors as factors
data_index.2<-c(1:63) #Changed to 63
summary(data.6[,c(1, 31, 32, 40,52,53,55,61,62)])
categorical_index<-data_index.2[-c(1, 31, 32, 40,52,53,55,61,62)]#Removed 64,65 and added 61,62
for (i in categorical_index){data.6[,i]<-as.factor(data.6[,i])}
summary(data.6)
str(data.6)
dim(data.6)

glm.all <- glm(W8QMAFI~., data = data.6, family = binomial(link = "logit"))
anova(glm.all, test="Chisq")

glm.sig <- glm(W8QMAFI~W1condur5MP+W1usevcHH+W1wrkfullmum+W1bulrc+W1disabYP+W1yschat1+W2ghq12scr+W4NamesYP+W4empsYP+W4schatYP+W6EducYP+W8DGHQSC+W8TENURE+W8NETA,data = data.6, family = binomial(link = "logit"))
anova(glm.sig, test="Chisq")


# Remove all those that aren't significant to 0.05
glm.sig.2 <- glm(W8QMAFI~W1condur5MP+W1usevcHH+W1bulrc+W1yschat1+W2ghq12scr+W4empsYP+W8DGHQSC+W8TENURE+W8NETA ,data = data.6, family = binomial(link = "logit"))
anova(glm.sig.2, test="Chisq")
display(glm.sig.2)


# Starting model

qchisq(0.99, 9)

summary(data.6$W8QMAFI)

predicted.data <- data.frame(
  probability.of.W8QMAFI=glm.sig.2$fitted.values,
  W8QMAFI=data.6$W8QMAFI)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.W8QMAFI, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)


ggplot(data=predicted.data, aes(x=rank, y=probability.of.W8QMAFI)) +
  geom_point(aes(color=W8QMAFI), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of W8QMAFI")



## ----function to create classification tables with %----------------------------------------------------------
#function
ct.op<-function(predicted,observed){ #arguments
  #create the data frame  
  df.op<-data.frame(predicted=predicted,observed=observed)
  #create a table 
  op.tab<-table(df.op)
  #use the prop.table function to obtain the proportions we need:
  #those who were correctly predicted as 0 
  #@position 1,1 in the table of proportions
  obs0.tab<-round(prop.table(op.tab,2)[1,1],2)
  #those who were correctly predicted as 1
  #@position 2,2 in the table of proportions
  obs1.tab<-round(prop.table(op.tab,2)[2,2],2)
  #and put them under the table 
  op.tab<-rbind(op.tab,c(obs0.tab,obs1.tab))
  #name the rows
  rownames(op.tab)<-c("pred=0","pred=1","%corr")
  #name the columns
  colnames(op.tab)<-c("obs=0","obs=1")
  #return the table
  op.tab
}

#------------------predict 0,1s with for pk----------------

pred.W8QMAFI<-as.numeric(glm.sig.2$fitted.values>0.5)
#pass the fitted values and the observed values to ct.op
ct.op(pred.W8QMAFI,data.6$W8QMAFI)


## --------------regression for APC----------------------------------------------------------
dmd.glm.apc<-glm(carrier~age+ck+h,data=dmd.dat,family=binomial)
display(dmd.glm.apc)

## ----data frame for risk ratio predictions--------------------------
new.dmd.dat<-data.frame(age=c(20,25,40,45),ck=mean(dmd.dat$ck),h=mean(dmd.dat$h))
new.dmd.dat

## --------------predicted logits----------------------------------------------------------
pred.logit<-predict(dmd.glm.apc,new.dmd.dat)
#pred.logit

## -----------------predicted probs-------------------------------------------------------
pred.probs<-invlogit(pred.logit)
#pred.probs

## ---------------combine all above into data frame---------------------------------------------------------
logreg<-data.frame(logits=pred.logit,probs=pred.probs)
cbind(new.dmd.dat,logreg)

## ----show results----------------------------------------------------------
print("risk ratio age:20-25, mean ck and h")
as.numeric(pred.probs[2]/pred.probs[1])
print("risk ratio age:40-45, mean ck and h")
as.numeric(pred.probs[4]/pred.probs[3])
#as.numeric means we don't get the data frame formatting

## ----summary for relevant ck,h,pk and age--------------------------
summary(dmd.dat[,c(1,3,4,5)])
library(jtools)
library(sandwich)
summ(glm.sig.2)
