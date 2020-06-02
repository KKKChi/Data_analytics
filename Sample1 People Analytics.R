employee_ori = read.csv (file.choose())
general_ori = read.csv(file.choose())
manager_ori = read.csv(file.choose())
Merger1 = merge(employee_ori, general_ori, by = c("EmployeeID"))
Merger2 = merge(Merger1, manager_ori, by = c("EmployeeID"))

dataset_fin = Merger2
View(dataset_fin)
summary(dataset_fin)

dataset_fin1 = na.omit(dataset_fin)

#
str(dataset_fin1)
outvars= names(dataset_fin1)%in%c('EmployeeID','EmployeeCount','Over18','StandardHours')
dataset_fin2 = dataset_fin1[!outvars]

typeof(dataset_fin2)
hist(dataset_fin2)

for ( colnames in 1:25)
{dataset_fin2[,c(colnames:colnames)]= as.numeric(dataset_fin2[,c(colnames:colnames)])
hist(dataset_fin2[,c(colnames:colnames)])}


#CART method
library("rpart.plot") 
library("rpart")


n = colnames(dataset_fin2)
fullmodel = as.formula(paste("Attrition ~", paste(n[!n %in% "Attrition"], collapse = " + ")))
carfit = rpart(fullmodel, data = dataset_fin2, method = "class" )
print(carfit)
rpart.plot(carfit, main ="Attrition - Classification Tree",
           box.palette="Blues")
rpart.rules(carfit)
plotcp(carfit)
printcp(carfit)

#C50
library(C50)
ruleModel <- C5.0(fullmodel, data = dataset_fin1[,c(-1,-12,-19,-21)])
ruleModel
summary(ruleModel)
plot(ruleModel)

#conditional inference
library(party)
citree = ctree(fullmodel, data =  dataset_fin1[,c(-1,-12,-19,-21)])
plot(citree)

#Accuracy testing
dataset_fin3 = dataset_fin1[,c(-1,-12,-19,-21)]
library(Hmisc)  # Needed for %nin%
totalrows = nrow(dataset_fin3)
pickrows = round(runif(totalrows*.80, 1, totalrows),0)
traindataset = dataset_fin3[pickrows, ]
testdataset = dataset_fin3[-pickrows, ]
library(MLmetrics)
#CART acc test
carfit2 = rpart(fullmodel, data = traindataset, method = "class")
testdataset$predict = predict(carfit2, newdata=testdataset, type='class')
Accuracytable = table(testdataset$predict,testdataset$Attrition)
Accuracy(testdataset$predict, testdataset$Attrition)
#C50 acc test
rulemodel2 = C5.0(fullmodel, data =testdataset)
testdataset$predict2 = predict( rulemodel2, newdata = testdataset)
Accuracy(testdataset$predict2, testdataset$Attrition)
#conditional Inference acc test
citree1 = ctree(fullmodel, data=traindataset)
testdataset$predict3 = predict(citree1, newdata = testdataset)
Accuracy(testdataset$predict3, testdataset$Attrition)

#Final Assignment Question After this dash
ED=dataset_fin2
require(dplyr)
require(cowplot)
require(ggthemes)

# Gender analysis
require(dplyr)
require(cowplot)
require(ggthemes)
avg.age <- ED %>% select(Gender, Age) %>% group_by(Gender) %>% summarize(avg=mean(Age))
avg.age

options(repr.plot.width=8, repr.plot.height=6) 
dat_text <- data.frame(
  label = c("Mean = 37.4 \n Years Old", "Mean = 36.6 \n Years Old"),
  Gender   = c("Female", "Male")
)
gender.dist <- ED %>% select(Gender, Age) %>% filter(Gender == 'Male' | Gender== "Female") %>% 
  filter(!is.na(Age)) %>% group_by(Gender) %>% 
  ggplot(aes(x=Age)) + geom_density(aes(fill=Gender), alpha=0.8, show.legend=FALSE) + facet_wrap(~Gender) + theme_minimal() + 
  geom_vline(aes(xintercept=mean(Age)),
             color="red", linetype="dashed", size=1) + labs(title="Age Distribution") + 
  theme(plot.title=element_text(hjust=0.5)) + scale_fill_manual(values=c("#F781F3", "#819FF7")) + 
  geom_text(
    data    = dat_text,
    mapping = aes(x = 45, y = 0.03, label = label),
    hjust   = -0.1,
    vjust   = -1
  )

overall.dist <- ED %>% select(Gender, Age) %>% filter(!is.na(Age)) %>% 
ggplot(data=ED, mapping=aes(x=Age)) + geom_density(color="darkblue", fill="lightblue")+ 
geom_vline(aes(xintercept=mean(Age)),color="red", linetype="dashed", size=1)+ 
theme_minimal()+ 
labs(x="Overall Age")+ 
annotate("text", label = "Mean = 36.92 Years Old", x = 50, y = 0.03, color = "black")

options(repr.plot.width=8, repr.plot.height=6) 
box.attrition <- ED %>% select(Attrition, JobSatisfaction, Gender) %>% 
ggplot(aes(x=Attrition, y=JobSatisfaction)) + geom_boxplot(aes(color=Attrition)) + theme_minimal() + facet_wrap(~Gender) + 
scale_fill_manual(values=c("#9FF781", "#FA5858"))
dist.satisfaction <- ED %>% select(JobSatisfaction) %>%
ggplot(aes(x=JobSatisfaction)) + geom_density(color="Black", fill="Purple", trim=TRUE) + theme_tufte() + xlim(range(c(1,4)))
plot_grid(box.attrition, dist.satisfaction, nrow=1)


p <- ggplot(ED, aes(x=Gender, y=MonthlyIncome, color=Gender, fill=Gender)) + geom_boxplot() + 
  scale_fill_manual(values=c("#F5A9F2", "#5882FA")) + scale_color_manual(values=c("#FE2EF7", "#5858FA")) +
  coord_flip() + labs(title="Are there any Gender Discrimination in Income?")
p


options(repr.plot.width=10, repr.plot.height=8) 
gender.income <- ED %>% select(Gender, MonthlyIncome) %>% group_by(Gender) %>% summarise(avg_income=round(mean(MonthlyIncome), 2)) %>%
  ggplot(aes(x=Gender, y=avg_income)) + geom_bar(stat="identity", fill="#2E9AFE", width=0.5) + 
  geom_text(aes(x=Gender, y=0.01, label= paste0("$ ", avg_income)),
            hjust=-2, vjust=0, size=3, 
            colour="yellow", fontface="bold",
            angle=360) + labs(title="Average Salary by Gender", x="Gender",y="Salary") + coord_flip() + 
  theme_minimal() + theme(plot.title=element_text(size=14, hjust=0.5))
gender.department <- ED %>% group_by(Department, Gender) %>% summarise(amount=n()) %>%
  ggplot(aes(x=reorder(Department, -amount), y=amount, fill=Gender)) + geom_bar(stat="identity", position="dodge") + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5)) + scale_fill_manual(values=c("green", "orange")) + 
  labs(title="Number of Employees \n
       by Department",x="Department", y="Number of employees")
departments <- ED %>% group_by(Department, Gender) %>% summarise(amount=n()) %>%
  ggplot(aes(x="", y=amount, fill=Department), show.legend=FALSE, width=) + geom_bar(stat="identity", position="dodge") + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5), aspect.ratio=1) + 
  labs(title="Number of Employees \n
       by Department") + coord_polar() + scale_fill_manual(values=c("#FE642E", "#0080FF","#00FF40"))
plot_grid(gender.income, gender.department,ncol=2, nrow=1)

#Deep Leaning to pridect employee gender by characteristics
#clean the dataset before neuron network
D1=dataset_fin2
D2=as.data.frame(lapply(D1,min_max_normal))
D2$actual = D2[,'Gender']
D3=D2[,-11] #put the 'gender' column to the last of dataset and name as 'actual'
#pick up columns' names 
frm = 'actual ~ '
for (i in 1:24) {
  frm = paste(frm, names(D3)[i],sep="+")
  }
frm
#split dataset
D3_train = D3[1:3440,]
D3_test = D3[3441:4300,]
ncol(D3_train)
inputs = 24
outputs = 1
neuron_estimate  = ceiling(nrow(D3_train) / (2 * (inputs + outputs)))
neuron_estimate
#train the data
library(neuralnet)
AL1 = neuralnet(frm,data=D3_train,hidden=c(35,19,9,6),stepmax = 100000,threshold = 0.1, linear.output=F)
predict_Gen = round(compute(AL1, covariate = D3_test[,-25])$net.result)
D3_test$prediction=as.data.frame(predict_Gen)
Accuracy(D3_test$actual,D3_test$prediction)

CORFF = cor(D1)
RCORFF= round(CORFF,3)
corrplot::corrplot(RCORFF)

PCA_D1 = PCA(D1)
communality(PCA_D1)
display_pc(PCA_D1)
