source("http://blogs.fiveelementanalytics.com/RCode/min_max_normalization.R")
library(readr)
mnist_raw <- read_csv("https://pjreddie.com/media/files/mnist_train.csv",col_names = FALSE)

### SECTION 1 
# Deal all the columns withe min_max_normal fuction. However, Lapply returns to list, so
# change the result, which is a list into a data fram. So the dataset visionly not changed.
mnist_raw[,-1] = as.data.frame(lapply(mnist_raw[,-1], min_max_normal))
mnist_raw <- replace(mnist_raw, is.na(mnist_raw), 0)

### SECTION 2
#creat ten columns, name them "zero ... nine". 
#judge if the value in X1 column is 0, in the "Zero" column result Ture(1), if the value in X1 is not 0, in "Zero column" result False(0)
#Change the Zero" column resutes( either 0 or 1) in to numeric
#Do the same thing to other columns
mnist_raw[, "zero"] = as.numeric(mnist_raw[,"X1"] == 0)
mnist_raw[, "one"] = as.numeric(mnist_raw[,"X1"] == 1)
mnist_raw[, "two"] = as.numeric(mnist_raw[,"X1"] == 2)
mnist_raw[, "three"] = as.numeric(mnist_raw[,"X1"] == 3)
mnist_raw[, "four"] = as.numeric(mnist_raw[,"X1"] == 4)
mnist_raw[, "five"] = as.numeric(mnist_raw[,"X1"] == 5)
mnist_raw[, "six"] = as.numeric(mnist_raw[,"X1"] == 6)
mnist_raw[, "seven"] = as.numeric(mnist_raw[,"X1"] == 7)
mnist_raw[, "eight"] = as.numeric(mnist_raw[,"X1"] == 8)
mnist_raw[, "nine"] = as.numeric(mnist_raw[,"X1"] == 9)

### SECTION 3 
#creat a new column "actual". Set the value equal to the value of the first column
#delate first column and change the dataset into data frame.
mnist_raw$actual = mnist_raw[,1]
mnist_raw = as.data.frame(mnist_raw[,-1])

###SECTION 4
#extract the columns' names, and combined them with "+"
frm = 'zero + one + two + three + four + five + six + seven + eight + nine ~ '
for (i in 1:784) {
  
  frm = paste(frm, names(mnist_raw)[i],sep="+")
  
}
frm

#### SECTION 5
# extract the first 4000 row, and name the new dataset as mnist_raw_train
# show total column numbers
mnist_raw_train = mnist_raw[1:16000,]
ncol(mnist_raw_train)

### SECTION 6 
# estimate a neuron nod number

neuron_estimate  =  3 
neuron_estimate
#outputs = 1
#inputs = ncol(mnist_raw_train)
#neuron_estimate  = ceiling(nrow(mnist_raw_train) / (2 * (inputs + outputs)))

#### SECTION 7
#train the data, and got a model called nn
#use nn model to predit, name the predict result as net.result
#give the output dataset's columns new names " Actual zero ...nine".
library(neuralnet)
#nn1 = neuralnet(frm,data=mnist_raw_train[,-795],hidden=c(10,6),stepmax = 1000000,threshold = 0.04, linear.output=F)
#predict_num = compute(nn1, covariate = mnist_raw_train[,-(785:795)])$net.result

#names(mnist_raw_train)[783:795]

#tmp_output = cbind(mnist_raw_train[1:4000,"actual"], round(predict_num[1:4000,],2))
#names(tmp_output) = c("Actual", "Zero", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine")
#tmp_output

#train 4000, 8000, 12000, 16000 rows of data respectively
#16000 rows and accuracy test
mnist_raw_train1 = mnist_raw[1:16000,]　
mnist_raw_test1 = mnist_raw[16001:60000,]
nn1 = neuralnet(frm,data=mnist_raw_train1[,-795],hidden=c(7,3),stepmax = 1500000,threshold = 0.05, linear.output=F)
predict_num1 = compute(nn1, covariate = mnist_raw_test1[,-(785:795)])$net.result
mnist_raw_test1$predictresult=as.data.frame(max.col(predict_num1)-1)
accuracy(mnist_raw_test1$actual,mnist_raw_test1$predictresult)

#12000 rows
mnist_raw_train2 = mnist_raw[1:12000,]　
mnist_raw_test2 = mnist_raw[12001:60000,]
nn2 = neuralnet(frm,data=mnist_raw_train2[,-795],hidden=c(5,3),stepmax = 800000,threshold = 0.05, linear.output=F)
predict_num2 = compute(nn2, covariate = mnist_raw_test2[,-(785:795)])$net.result
mnist_raw_test2$predictresult=as.data.frame(max.col(predict_num2)-1)
accuracy(mnist_raw_test2$actual,mnist_raw_test2$predictresult)

#8000 rows
mnist_raw_train3 = mnist_raw[1:8000,]　
mnist_raw_test3 = mnist_raw[8001:60000,]
nn3 = neuralnet(frm,data=mnist_raw_train3[,-795],hidden=c(3,2),stepmax = 800000,threshold = 0.1, linear.output=F)
predict_num3 = compute(nn3, covariate = mnist_raw_test3[,-(785:795)])$net.result
mnist_raw_test3$predictresult=as.data.frame(max.col(predict_num3)-1)
accuracy(mnist_raw_test3$actual,mnist_raw_test3$predictresult)

#4000 rows
mnist_raw_train4 = mnist_raw[1:4000,]　
mnist_raw_test4 = mnist_raw[4001:60000,]
nn4 = neuralnet(frm,data=mnist_raw_train4[,-795],hidden=c(3),stepmax = 200000,threshold = 0.1, linear.output=F)
predict_num4 = compute(nn4, covariate = mnist_raw_test4[,-(785:795)])$net.result
mnist_raw_test4$predictresult=as.data.frame(max.col(predict_num4)-1)
accuracy(mnist_raw_test4$actual,mnist_raw_test4$predictresult)

## You can use these to display any image. 
## Run the function and then you can use it as below (just pass in a row)
## note that the function is 90 rotated, Ill work to fix this 
## YOU DO NOT NEED TO COMMENT THIS SECTION BELOW. 
display_digit = function( x ) {
  
  tmp = matrix(x,ncol=28, nrow=28, byrow = T) 
  tmp = apply(tmp,2,as.numeric)
  
  image(1:28,1:28,tmp, col = grey.colors(255))

} 


display_digit(mnist_raw_train[1, 1:784])

## Final Question 2 After this dash
# Split the dataset with 80%-20% for training and testing respectively
mnist_raw_train1 = mnist_raw[1:60000,]
#train the data
nn1 = neuralnet(frm,data=mnist_raw_train1[,-795],hidden=c(40),stepmax = 100000,threshold = 1, linear.output=F)

#Format the testing dataset
mnist_raw_test = read.csv(file.choose())
mnist_raw_test[,-1] = as.data.frame(lapply(mnist_raw_test[,-1], min_max_normal))
mnist_raw_test <- replace(mnist_raw_test, is.na(mnist_raw_test), 0)
mnist_raw_test[, "zero"] = as.numeric(mnist_raw_test[,"X1"] == 0)
mnist_raw_test[, "one"] = as.numeric(mnist_raw_test[,"X1"] == 1)
mnist_raw_test[, "two"] = as.numeric(mnist_raw_test[,"X1"] == 2)
mnist_raw_test[, "three"] = as.numeric(mnist_raw_test[,"X1"] == 3)
mnist_raw_test[, "four"] = as.numeric(mnist_raw_test[,"X1"] == 4)
mnist_raw_test[, "five"] = as.numeric(mnist_raw_test[,"X1"] == 5)
mnist_raw_test[, "six"] = as.numeric(mnist_raw_test[,"X1"] == 6)
mnist_raw_test[, "seven"] = as.numeric(mnist_raw_test[,"X1"] == 7)
mnist_raw_test[, "eight"] = as.numeric(mnist_raw_test[,"X1"] == 8)
mnist_raw_test[, "nine"] = as.numeric(mnist_raw_test[,"X1"] == 9)
mnist_raw_test$actual = mnist_raw_test[,1]
mnist_raw_test = as.data.frame(mnist_raw_test[,-1])
mnist_raw_test1=mnist_raw_test

#predict by algorithm and collect the pridicted data into a column
predict_num1 = compute(nn1, covariate = mnist_raw_test1[,-(785:795)])$net.result
mnist_raw_test1$predictresult=as.data.frame(max.col(predict_num1)-1)
#test accuracy
accuracy(mnist_raw_test1$actual,mnist_raw_test1$predictresult)

#KNN
mnist_raw_new=mnist_raw[sample(nrow(mnist_raw),60000),]
set.seed(666)
sample_size=floor(nrow(mnist_raw_new))
sample_size
train_ind=sample(seq_len(nrow(mnist_raw_new)),size = sample_size)
mnist_raw_new_train=mnist_raw_new[train_ind,]
mnist_raw_new_test=mnist_raw_new[-train_ind,]

mnist_raw_new_train_target=mnist_raw_new_train$X1
mnist_raw_new_test_target=mnist_raw_new_test$X1

mnist_raw_new_train=as.data.frame(apply(mnist_raw_new_train[,-1], 2, FUN =min_max_normal))
mnist_raw_new_train <- replace(mnist_raw_new_train, is.na(mnist_raw_new_train), 0)

library(class)
trueclass =mnist_raw_new_train_target
knnmdl <- knn(mnist_raw_new_train,
              mnist_raw_new_test[,-1],
              trueclass,
              k = 11,
              prob = TRUE)

output= as.matrix(cbind(mnist_raw_new_test_target,knnmdl ))
output= as.data.frame(output)
accuracy= sum(output$mnist_raw_new_test_target == output$knnmdl )/nrow(output)
accuracy



