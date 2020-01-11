
library(readr)
Zoo <- read_csv("C:/Users/Admin/Desktop/Assignments/k_nn/Zoo.csv")
view(Zoo)
str(Zoo)
summary(Zoo)
#Create a function to normalize the data
Zoo$type <- factor(Zoo$type, levels = c("1","2","3","4","5","6","7"), labels = c("Animal 1","Animal 2","Animal 3","Animal 4","Animal 5","Animal 6","Animal 7"))
Zoo$type

#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

Zoo_n <- as.data.frame(lapply(Zoo[2:17], norm))
set.seed(1234)
indexes = sample(2, nrow(Zoo), replace=TRUE, prob=c(0.7, 0.3))
indexes
Zoo_train = Zoo[indexes==1, 2:17]
Zoo_test = Zoo[indexes==2, 2:17]
Zoo_train_labels = Zoo[indexes==1, 18]
Zoo_test_labels = Zoo[indexes==2, 18]
Zoo_train_labels<-Zoo_train_labels[["type"]]
Zoo_test_labels<-Zoo_test_labels[["type"]]

library(class)
Zoo_test_pred = knn(train=Zoo_train, test=Zoo_test, cl=Zoo_train_labels, k=14)

table(Zoo_test_labels,Zoo_test_pred)
view(Zoo)
library(gmodels)
CrossTable( x =  Zoo_test_labels, y = Zoo_test_pred,prop.chisq = FALSE)

CM = table(Zoo_test_labels, Zoo_test_pred)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy


############# or
library(readr)
Zoo <- read_csv("C:/Users/Admin/Desktop/Assignments/k_nn/Zoo.csv")
view(Zoo)
str(Zoo)
summary(Zoo)
#Create a function to normalize the data
Zoo$type <- factor(Zoo$type, levels = c("1","2","3","4","5","6","7"), labels = c("Animal 1","Animal 2","Animal 3","Animal 4","Animal 5","Animal 6","Animal 7"))
Zoo$type

#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

Zoo_n <- as.data.frame(lapply(Zoo[2:17], norm))

Zoo_train = Zoo_n[1:70,]
Zoo_test = Zoo_n[71:101,]
Zoo_train_labels = Zoo[1:70,18]
Zoo_test_labels = Zoo[71:101,18]
Zoo_train_labels<-Zoo_train_labels[["type"]]
Zoo_test_labels<-Zoo_test_labels[["type"]]
library(class)
Zoo_test_pred = knn(train=Zoo_train, test=Zoo_test, cl=Zoo_train_labels, k=8)

table(Zoo_test_labels,Zoo_test_pred)
view(Zoo)
library(gmodels)
CrossTable( x =  Zoo_test_labels, y = Zoo_test_pred,prop.chisq = FALSE)

CM = table(Zoo_test_labels, Zoo_test_pred)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy
