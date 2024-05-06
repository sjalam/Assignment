#Topic-Group Assignment
#Submitted to-Dr. Devesh Birwal
#TOPIC: Monthly Expenditure on Milk product
#COLLECTED DATA FROM CLASSMATES BY MAKING  SHORT AND BRIEF QUESTIONNAIRE BY 
# USING GOOGLE FORM
 



#step1 : import dataset
library(readxl)
group_p <- read_excel("group_p.xlsx")
View(group_p)



#step2 : attach dataset, view and summarize the data
attach(group_p)
View(group_p)
summary(group_p)


#step3 : extracting data_3 using rows and columns
#row
group_p[10,]
group_p[3,]
#column
group_p[,5]
group_p[,2]
#both row & column
group_p[5,7]
#extract column/object by names
group_p$Age
group_p$AnnualIncome
group_p$kids_6
group_p$FamilyMember
group_p$MilkConsumptionltrs

dim(group_p)
ncol(group_p)
nrow(group_p)

head(group_p,5)
tail(group_p,4)
str(group_p)

#step4 : finding mean,median,mode,sd,var,max,min
mean(group_p$Age)
mean(group_p$INCOME)
mean(group_p$FamilyMember)
mean(group_p$MilkConsumptionltrs)

mode(group_p$Age)
mode(group_p$INCOME)
mode(group_p$FamilyMember)
mode(group_p$MilkConsumptionltrs)

median(group_p$Age)
median(group_p$INCOME)
median(group_p$FamilyMember)
median(group_p$MilkConsumptionltrs)

sd(group_p$Age)
sd(group_p$INCOME)
sd(group_p$FamilyMember)
sd(group_p$MilkConsumptionltrs)

var(group_p$Age)
var(group_p$INCOME)
var(group_p$INCOME)
var(group_p$MilkConsumptionltrs)

max(group_p$Age)
max(group_p$INCOME)
max(group_p$FamilyMember)
max(group_p$MilkConsumptionltrs)

min(group_p$Age)
min(group_p$INCOME)
min(group_p$FamilyMember)
min(group_p$MilkConsumptionltrs)

#step5 :simple linear regression in group_3
names(group_p)
class(FamilyMembers)
class(MilkConsumptionltrs)
plot(FamilyMember,MilkConsumptionltrs,main="Scatterplot")
cor(FamilyMember,MilkConsumptionltrs)
mod<-lm(FamilyMember~MilkConsumptionltrs)
summary(mod)
attributes(mod)
mod$coefficients
abline(mod)
abline(mod, col=2, lwd=3)

confint(mod)
confint(mod, level=0.99)
anova(mod)

#step6 : checking linear regression in group_p
plot(INCOME,MilkConsumptionltrs)
plot(mod)
par(mfrow=c(1,1))
plot(FamilyMember,MilkConsumptionltrs)
mod2<-lm(FamilyMember,MilkConsumptionltrs)
abline(mod2)

#step7 : hypothesis testing in group_p
FamilyMember = rnorm(10)
MilkConsumptionltrs = rnorm(10)
t.test(FamilyMember,MilkConsumptionltrs)
t.test(FamilyMember,MilkConsumptionltrs, var.equal=TRUE)
t.test(FamilyMember, mu =5)
t.test(MilkConsumptionltrs, mu=5)
t.test(FamilyMember,mu=5, alternative= 'greater')
t.test(MilkConsumptionltrs,mu=5, alternative= 'greater')
Sales:Profit
#TWO SAMPLE U-TEST
wilcox.test(FamilyMember,MilkConsumptionltrs)
#ONE SAMPLE U-TEST
wilcox.test(MilkConsumptionltrs, exact=FALSE)
boxplot(FamilyMember)
#Ho:mu<8
#one-sided 95% confidence interval for mu
t.test(MilkConsumptionltrs, mu=8,alternative="less",conf.level=0.95)
#two-sided
t.test(FamilyMember , mu=8,alt="two.sided",conf=0.99)
TEST<-t.test(FamilyMember, mu=8,alt="two.sided",conf=0.99)
TEST
attributes(TEST)
TEST$conf.int
TEST$p.value

#step8: plotting of graphs of group_p
hist(MilkConsumptionltrs)
hist(INCOME)
hist(FamilyMember)
set.seed(1)
Sales<-rnorm(300)
Profit<-rnorm(300)
group<-rep(1:2,each=150)
plot(Sales, Profit , col=group ,pch=16)
legend("topleft",
       legend=c("Group1","group2"),col=1:2,pch=16)
plot(Sales, Profit , col=group ,pch=16)
legend("topleft",
       legend=c("Group1","group2"),col=1:2,pch=16,
       box.col="orange",bg="green")

INCOME<-rnorm(300)
MilkConsumptionltrs<-rnorm(300)
group<-rep(1:2,each=150)
plot(INCOME,MilkConsumptionltrs, col=group ,pch=16)
legend("topleft",
       legend=c("Group1","group2"),col=1:2,pch=16)
plot(INCOME,MilkConsumptionltrs, col=group ,pch=16)
legend("topleft",
       legend=c("Group1","group2"),col=1:2,pch=16,
       box.col="orange",bg="green")
