-----
  #title: "WORLD BANK DATA FOR GGPLOTTING"
  #author: "SK JANE ALAM"
  #Date: "06/07/2022"
  #output: html_document
  -----



install.packages("ggplot2")
library(ggplot2)
library(gridExtra)
if(!file.exists("spm_lecture")){
  dir.create(("spm_lecture"))
}
getwd()
setwd("C:D:/NSSO level 1")
library(datasets)
data("mtcars")
str(mtcars)
View(mtcars)
ggplot2- data+aesthetics+ geometric expressions 

# basic one dimensional plots 
df<- as.data.frame(mtcars)
df$am <- as.factor(df$am)
df$gear <- as.factor(df$gear)
df$hp <- as.factor(df$hp)
df$cyl <- as.factor(df$cyl)

ggplot(mtcars,aes(mpg))+geom_histogram(bins = 50)
ggplot(mtcars,aes(mpg))+geom_density()
ggplot(mtcars,aes(x=mpg,fill=as.factor(am)))+geom_density()

# Two dimensional graphs with bar charts
ggplot(mtcars,aes(x=cyl,fill=gear))+geom_bar(position="dodge")
ggplot(mtcars,aes(x=as.factor(cyl),fill=as.factor(gear)))+geom_bar(position="stack")
ggplot(mtcars,aes(x=as.factor(cyl),fill=as.factor(gear)))+geom_bar(position="fill")

#two dimensional graph with column chart 

ggplot(mtcars,aes(x=as.factor(cyl),y=mpg))+geom_col(color="red",fill="yellow") 


#box plots and other additions 

ggplot(mtcars, aes(x=as.factor(cyl),y=mpg))+ geom_boxplot()
ggplot(mtcars, aes(x=as.factor(cyl),y=mpg))+ geom_boxplot(fill="red",color="yellow", notch = TRUE)
ggplot(mtcars, aes(x=as.factor(cyl),y=mpg))+ geom_boxplot(fill="red",color="black")+
  labs(title = "Box Plot of MPG", x="No. of cyl", y="Miles per Gallon")
ggplot(mtcars, aes(x=as.factor(cyl),y=mpg))+ geom_boxplot(fill="red",color="black")+
  labs(title = "Box Plot of MPG", x="no. of cyl", y="MPG")+
  scale_x_discrete(breaks=c("4","6","8"),labels=c("four","six","eight"))+
  scale_y_continuous(breaks = c(10,15,20,25,30,35),labels=c("ten","fif","twen","twenfiv","thirt","Thrtfv"))
ggplot(mtcars,aes(x=hp,y=mpg))+geom_point()
ggplot(mtcars,aes(x=hp,y=mpg))+geom_point(pch=16,color="red",size=3,alpha=0.9)
ggplot(mtcars,aes(x=hp,y=mpg,shape=as.factor(am)))+geom_point(color="red",size=3,alpha=0.9)

ggplot(mtcars,aes(x=hp,y=mpg,shape=as.factor(cyl),color=as.factor(am)))+geom_point(size=3,alpha=0.9)

ggplot(mtcars,aes(x=hp,y=mpg))+geom_point(pch=16,color="red",size=3,alpha=0.9)+
  facet_wrap(~as.factor(gear),ncol = 2)
ggplot(mtcars,aes(x=hp,y=mpg,fill=as.factor(cyl)))+geom_point(pch=16,color="red",size=3,alpha=0.9)+
  geom_smooth(method = "lm",color="Blue",linetype=2)+theme(legend.position = c(0.8,0.7))
 
g1 <-ggplot(mtcars, aes(mpg,fill=as.factor(am)))+geom_histogram()
g2 <-ggplot(mtcars, aes(mpg,fill=as.factor(gear)))+geom_density()
g3 <-ggplot(mtcars, aes(x=as.factor(cyl),y=mpg,fill=as.factor(cyl)))+geom_violin()
g4 <- ggplot(mtcars, aes(x=as.factor(cyl),y=mpg,fill=as.factor(carb)))+geom_boxplot()
grid.arrange(g1,g2,g3,g4,ncol=2)