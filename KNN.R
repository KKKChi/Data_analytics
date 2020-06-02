library(class)
library(fields) 

dataset=read.csv(file.choose())
summary(dataset)
str(dataset)
#There are some question marks in Bare_Nucleoli column
#Change the "?" into NA and clean the dataset
dataset[ as.character(dataset[,"Bare_Nuclei"]) == "?","Bare_Nuclei"] = NA
dataset1= dataset[complete.cases(dataset),]
dataset1$Bare_Nuclei = as.numeric(as.character(dataset1$Bare_Nuclei))
str(dataset1)
#remove code number rows
cancer = dataset1[c(2:11)]

library(ggplot2)
library(corrplot)
cancer_new=as.matrix(cancer) 
cancer_va = round(cor(cancer_new, use = "complete.obs"),3) 
corrplot(cancer_va)

cancer_knn = as.data.frame(cancer)
ggplot(cancer_knn, aes(x=Clump_Thickness, y=Uniformity_of_Cell_Size , colour=Class)) +
  geom_point() +
  ggtitle(" Clump_Thickness/Uniformity_of_Cell_Size (coded by type of Class)")

cancer_n2 = dataset1[,2:11]
cancer_n2$Class = as.numeric(as.character(cancer_n2$Class))
set.seed(101)
#spliting
sample = floor(0.8*nrow(cancer_n2))
train_total=sample(seq_len(nrow(cancer_n2)),size = sample)
View(train_total)


train1 = cancer_n2[train_total,]
test1 = cancer_n2[-train_total,]
#training
linear1 =lm(Class ~Clump_Thickness+Uniformity_of_Cell_Size+Uniformity_of_Cell_Shape+
              Marginal_Adhesion+Single_Epithelial_Cell_Size+
              Bland_Chromatin+Normal_Nucleoli,data=train1)
summary(linear1)

test1$pred=predict(linear1,newdata = test1)
#testing
library(MLmetrics) 
MSE(test1$pred,test1$Class) 
MAE(test1$pred,test1$Class) 
MAPE(test1$pred,test1$Class) 
RMSE(test1$pred,test1$Class)

#KNN
normalizedata = function(X){return((X-min(X))/(max(X)-min(X)))}

cancer_n2$Clump_Thickness=normalizedata(cancer_n2$Clump_Thickness)
cancer_n2$Uniformity_of_Cell_Size=normalizedata(cancer_n2$Uniformity_of_Cell_Size)
cancer_n2$Uniformity_of_Cell_Shape=normalizedata(cancer_n2$Uniformity_of_Cell_Shape)
cancer_n2$Single_Epithelial_Cell_Size=normalizedata(cancer_n2$Single_Epithelial_Cell_Size)
cancer_n2$Bare_Nuclei=normalizedata(cancer_n2$Bare_Nuclei)
cancer_n2$Bland_Chromatin=normalizedata(cancer_n2$Bland_Chromatin)
cancer_n2$Normal_Nucleoli=normalizedata(cancer_n2$Normal_Nucleoli)

cancer_n2$Class=as.factor(cancer_n2$Class)
set.seed(200)

sample1 = floor(0.8*nrow(cancer_n2))
knntrain = sample(seq_len(nrow(cancer_n2)),size = sample1)
knntrain1 = cancer_n2[knntrain,]
knntest1 = cancer_n2[-knntrain,] 
#ececute KNN 
knntrain1_target = knntrain1$Class
knntest1_target = knntest1$Class
knnmodel = knn(knntrain1, knntest1,knntrain1_target, k=5, prob = T)
knnmodel
#output result
knn_res= as.matrix(cbind(knntest1_target, knnmodel))
knn_res_frame = as.data.frame(knn_res)
#Error test
Accuracy(knnmodel,knntest1_target)
