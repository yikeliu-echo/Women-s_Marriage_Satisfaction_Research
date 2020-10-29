library(haven)
M<- read_dta("Desktop/09306-0001-Data.dta")
married<-subset(M,M$V44 == "1")
#************************************************************************************************
married$V259[which(married$V259=="1"|married$V259=="2")] <-1
married$V259[which(married$V259=="0"|married$V259=="3"|married$V259=="4"|married$V259=="5"|married$V259=="6"|
             married$V259=="7"|married$V259=="9")] <-0
#************************************************************************************************
#Education level
#married$V75[which(married$V75=="0"|married$V75=="1"|married$V75=="2"|married$V75=="3")] <-0
#married$V75[which(married$V75=="4"|married$V75=="5"|married$V75=="6")] <-1
married$V75[which(married$V75=="0"|married$V75=="1"|married$V75=="2"|married$V75=="3")] <-0
married$V75[which(married$V75=="4")] <-1
married$V75[which(married$V75=="5"|married$V75=="6")] <-2
married$V75<-as.factor(married$V75)
#Income
married$V398[which(married$V398=="1"|married$V398=="99")] <-0
married$V398[which(married$V398=="2"|married$V398=="3"|married$V398=="4"|married$V398=="5"|married$V398=="6")] <-1
married$V398[which(married$V398=="7"|married$V398=="8"|married$V398=="9"
                   |married$V398=="10"|married$V398=="11"|married$V398=="12"|married$V398=="13"
                   |married$V398=="14"|married$V398=="15"|married$V398=="16")] <-2
married$V398<-as.factor(married$V398)
#Age
married$V447<-married$V447/12 #interview
married$V444<-married$V444/12 #first wedding
#Race
married$V69[which(married$V69=="2"|married$V69=="3"|married$V69=="7")] <-0
married$V69[which(married$V69=="1")] <-1
married$V69<-as.factor(married$V69)
#V47 - Number of children
#V45 - Number of marriage

#************************************************************************************************

#Closeness of parents - mother
married$V85[which(married$V85=="0"|married$V85=="3"|married$V85=="4"|married$V85=="7"
                  |married$V85=="9")] <-0
married$V85[which(married$V85=="1"|married$V85=="2")] <-1
married$V85<-as.factor(married$V85)
#Closeness of parents - father
married$V98[which(married$V98=="0"|married$V98=="3"|married$V98=="4"|married$V98=="9")] <-0
married$V98[which(married$V98=="1"|married$V98=="2")] <-1
married$V98<-as.factor(married$V98)
#Parents marriage status
married$V165[which(married$V165=="1"|married$V98=="2"|married$V165=="3"|married$V165=="4")] <-0
married$V165[which(married$V165=="0")] <-1
married$V165<-as.factor(married$V165)
#Parents eductaion - father
married$V103[which(married$V103=="0"|married$V103=="1"|married$V103=="2"|married$V103=="3"
             |married$V103=="8"|married$V103=="9")] <-0
married$V103[which(married$V103=="4"|married$V103=="5"|married$V103=="6")] <-1
married$V103<-as.factor(married$V103)
#Parents eductaion - mother
married$V92[which(married$V92=="0"|married$V92=="1"|married$V92=="2"|married$V92=="3"
             |married$V92=="8")] <-0
married$V92[which(married$V92=="4"|married$V92=="5"|married$V92=="6")] <-1
married$V92<-as.factor(married$V92)
#Family class
married$V110<-as.factor(married$V110)
#************************************************************************************************
data<-data.frame("satisfy"=married$V259,"edu"=married$V75,"income"=married$V398,"child"=married$V47,
                 "agei"=married$V447,"agew"=married$V444,"marriaged"=married$V45,"race"=married$V69,
                 "closem"=married$V85,"closef"=married$V98,"pmarriage"=married$V165,
                 "peduf"=married$V103,"pedum"=married$V92,"class"=married$V110,"locat"=married$V63)
str(data)
data$satisfy<-as.factor(data$satisfy)
data$locat<-as.factor(data$locat)
attach(data)
#************************************************************************************************
logit.1 <- glm(satisfy~edu+income+child+race+closem+closef+peduf+pedum
               +class+agei, family=binomial(link="logit"))
summary(logit.1)

logit.2 <- glm(satisfy~edu+income+child+race+closem+closef+peduf+pedum, 
               family=binomial(link="logit"))
summary(logit.2)
logistic.display(logit.2)

probit.1 <- glm(satisfy~edu+income+child+race+closem+closef+peduf+pedum
                +class+agei,family=binomial(link="probit"))
probit.2 <- glm(satisfy~edu+income+child+race+closem+closef+peduf+pedum, 
               family=binomial(link="probit"))

logistic.display(probit.2)
logitOR.2 <- cbind(Estimate=round(coef(logit.2),4), OR=round(exp(coef(logit.2)),4))
library(stargazer)
stargazer(logit.1,probit.1,logit.2,probit.2,
          no.space = TRUE,type = "html", out="~/Desktop/table1.html")
library(car)
linearHypothesis(logit.2, c("closem1=0","closef1=0","peduf1=0","pedum1=0"))
linearHypothesis(logit.2, c("peduf1=0","pedum1=0"))
#************************************************************************************************
library(margins)
a<-summary(margins(logit.2))
summary(margins(logit.2,variables="edu", change="1"))
summary(margins(logit.2,variables="edu"))
summary(margins(logit.2,variables = "edu",at=list(income="0")))
summary(margins(logit.2,variables = "edu",at=list(income="1")))
summary(margins(logit.2,variables = "edu",at=list(income="2")))
summary(margins(logit.2,variables = "income",at=list(edu="0")))
summary(margins(logit.2,variables = "income",at=list(edu="1")))
summary(margins(logit.2,variables = "income",at=list(edu="")))

stargazer(a,
          no.space = TRUE,type = "html", out="~/Desktop/table7.html")
