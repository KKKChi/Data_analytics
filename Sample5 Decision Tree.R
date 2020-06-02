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
