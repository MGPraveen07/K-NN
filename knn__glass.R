
library(readr)
glass <- read_csv("C:/Users/Admin/Desktop/Assignments/k_nn/glass.csv")
View(glass)
str(glass)
summary(glass)
#Create a function to normalize the data
glass$Type <- factor(glass$Type, levels = c("1","2","3","4","5","6","7"), labels = c("Glass1","Glass2","Glass3","Glass4","Glass5","Glass6","Glass7"))
glass$Type
str(glass)

#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
glass_n <- as.data.frame(lapply(glass[1:9], norm))
set.seed(1234)
indexes = sample(2, nrow(glass), replace=TRUE, prob=c(0.7, 0.3))
indexes
glass_train = glass[indexes==1, 1:9]
glass_test = glass[indexes==2, 1:9]
glass_train_labels = glass[indexes==1, 10]
glass_test_labels = glass[indexes==2, 10]
glass_train_labels<-glass_train_labels[["Type"]]
glass_test_labels<-glass_test_labels[["Type"]]

library(class)
glass_test_pred = knn(train=glass_train, test=glass_test, cl=glass_train_labels, k=14)

table(glass_test_labels,glass_test_pred)
view(glass)
library(gmodels)
CrossTable( x =  glass_test_labels, y = glass_test_pred,prop.chisq = FALSE)

CM = table(glass_test_labels, glass_test_pred)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy

############# or
library(readr)
glass <- read_csv("C:/Users/Admin/Desktop/Assignments/k_nn/glass.csv")
View(glass)
str(glass)
summary(glass)
#Create a function to normalize the data
glass$Type <- factor(glass$Type, levels = c("1","2","3","4","5","6","7"), labels = c("Glass1","Glass2","Glass3","Glass4","Glass5","Glass6","Glass7"))
glass$Type
str(glass)
#Create a function to normalize the data
norm <- function(x){ 
    +   return((x-min(x))/(max(x)-min(x))) }
glass_n <- as.data.frame(lapply(glass[1:9], norm))
set.seed(1234)
indexes = sample(2, nrow(glass), replace=TRUE, prob=c(0.7, 0.3))
indexes

glass_train = glass[indexes==1, 1:9]
glass_test = glass[indexes==2, 1:9]
glass_train_labels = glass[indexes==1, 10]
glass_test_labels = glass[indexes==2, 10]
glass_train_labels<-glass_train_labels[["Type"]]
glass_test_labels<-glass_test_labels[["Type"]]
library(class)
glass_test_pred = knn(train=glass_train, test=glass_test, cl=glass_train_labels, k=16)
table(glass_test_labels,glass_test_pred)
View(glass)
str(glass)
library(gmodels)
CrossTable( x =  glass_test_labels, y = glass_test_pred,prop.chisq = FALSE)
CM = table(glass_test_labels, glass_test_pred)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy
