# Sample Plots

library(ggplot2)

summary(iris)

box <- ggplot(data=iris, aes(x=Species, y=Sepal.Length))
box + geom_boxplot(aes(fill=Species)) + 
  ylab("Sepal Length") + ggtitle("Iris Boxplot") +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) 

airquality

ggplot(airquality, aes(x=Wind, fill=Month)) +
  geom_histogram(fill="white", color="black", binwidth=3)+
  geom_vline(aes(xintercept=mean(Wind)), color="blue",
             linetype="dashed")+
  labs(title="Wind histogram plot",x="Wind Speed/Mph", y = "Count")+
  theme_classic()

library(ggplot2)
library(readr)
prb<-read_csv(file = "https://raw.githubusercontent.com/coreysparks/data/master/PRB2008_All.csv")
ggplot(data=prb)+
  geom_point(mapping= aes(x=TFR, y=IMR, color=Continent))+
  ggtitle(label = "Relationship between Total Fertility and Infant Mortality", subtitle = "2008 Estimates")+
  xlab(label = "TFR")+
  ylab(label="IMR")

# Tukey Outliers

p <- ggplot(mpg, aes(class, hwy))
p + geom_boxplot()




