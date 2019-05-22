
##setting the directory
setwd('~/tensorflow_machine_learning/R_PROJECT/CANCER_DETECTION/')
## Load the Dataset
data <- read.csv('breast-cancer-wisconsin.csv')


train <- data[1:400,]
test <- data[401:699,]
## View The data
View(data)

## see the target

table(data$class)

## well there are 458 person beniegn and 241 malignant

## barplot

barplot(table(data$class),xlab = 'benign                  malignant',ylab = 'number of person')
##

barplot(table(data$clump_thickness))


##create a default decision tree depending on the clump_thickness
library('rpart')
library('rattle')
library('RColorBrewer')



tree1 <-rpart(class ~ clump_thickness,data=data,method = 'class')
fancyRpartPlot(tree1)


## lets predict with that

prediction_1=predict(tree1,test,type = 'class')


prediction_data_frame=data.frame(id=test$id,class=prediction_1)


write.csv(prediction_data_frame,file = "first_prediction.csv" ,row.names=FALSE)

View(prediction_data_frame)

table(prediction_1)


## lets take all in the considaretion


tree2 <-rpart(class ~ clump_thickness+unif_cell_size+unif_cell_shape+marg_adhesion+single_epith_cell_size+bare_nuclei+bland_chrom+norm_nucleoli+mitoses,data=data,method = 'class')
fancyRpartPlot(tree2)


## lets predict with that

prediction_2=predict(tree2,test,type = 'class')


prediction_data_frame=data.frame(id=test$id,class=prediction_2)


write.csv(prediction_data_frame,file = "second_prediction.csv" ,row.names=FALSE)

table(prediction_2)
## ok so the malignant increase

table(is.na(test))

## no absent data

# now apply rendom forest
library('mice')
library('lattice')


library('randomForest')
##

X_train1<-train[c('clump_thickness','unif_cell_size','unif_cell_shape','marg_adhesion','single_epith_cell_size','bare_nuclei','bland_chrom','norm_nucleoli','mitoses')]

##creating a subset of the data
Y_train1<-as.factor(train$class) 

View(Y_train1)

fit1<-randomForest(x = X_train1,y = Y_train1,importance = TRUE,ntree = 1000)
fit1


### thats fantastic OOB error rate 4.75%

X_test1<-test[c('clump_thickness','unif_cell_size','unif_cell_shape','marg_adhesion','single_epith_cell_size','bare_nuclei','bland_chrom','norm_nucleoli','mitoses')]

##creating a subset of the data
Y_test1<-as.factor(test$class) 
final_predict<-predict(fit1,X_test1)
View(final_predict)
table(final_predict)
## thats different

prediction_data_frame=data.frame(id=test$id,class=final_predict)


write.csv(prediction_data_frame,file = "final_random_forest_prediction.csv" ,row.names=FALSE)


