library(stats)
library(ggplot2)
library(ggthemes)

source("tufte.R")


f<-function(x){
  return(sqrt(1-2*x))
}


area.calc<-function(n){
  angle<- ((n-2)*pi)/(n*2)
  c<-1/tan(angle)
  
  upper.limt<-0.5
  lower.limit<-(sqrt(c*c+1)-1)/(c*c)
  
  eat.area.a<-lower.limit*lower.limit*c/2
  eat.area.b<-integrate(f,lower.limit,upper.limt)$value
  t.eat.area<-eat.area.a+eat.area.b
  t.area<-1*c/2
  percent.eat.area<-t.eat.area/t.area
  return(percent.eat.area)
}


edges<-3:50
area<-sapply(edges,FUN=area.calc)

df<-data.frame(edges,area)

g<-ggplot(df,aes(x=edges,y=area))

g+theme_tufte()+geom_rangeframe()+
  geom_point(color='firebrick',size=rel(2),alpha=0.75)+
  geom_line(color='firebrick',size=rel(0.75),alpha=0.5)+
  labs(title='Proportion of Sandwich Eaten vs Bread Shape',
       x='Number of Edges',y='Proportion of Sandiwch Eaten')




