#1(1)
library(ggplot2)
prouction
product<- c( "apple", "strawberry", "watermelon")
cost<-c(1800,1500,3000)
sale<-c(24,38,13)
sales<-data.frame(product,cost,sale)
sales
#1 (2)
average(cost)
mean(sale)
#2. (1)
mpg<-as.data.frame(ggplot2::mpg)
ggplot(data = mpg, aes(x=manufacturer, y=cty)) + geom_boxplot()
#boxplot의 박스 속 선을 통해 toyota가 더 평균 도시연비가 높음을 알 수 있다.
#2. (2)

#2.(3)
library(dplyr)
total<- mpg$cty+mpg$hwy
average <- total/2
head(3)
#2 (4)


mpg <- mpg %>% mutate(compact = class(mpg))

mpg %>% arrange(desc(compact)) %>%
  

  select(county, compact) %>%

  head(5)
