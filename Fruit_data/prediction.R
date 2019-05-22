### setting up the directory
setwd("/home/vagrant/tensorflow_machine_learning/R_PROJECT/Fruit_data/")
## import the data
DF <- read.table('data.txt',header = TRUE)
## you mast put The header=TRUE
View(DF)
table(DF$fruit_name)
barplot(table(DF$fruit_name))
table(is.na(DF))
## no null value
barplot(table(DF$fruit_subtype))
## adding
library('rpart')
library('rattle')
library('RColorBrewer')

df <-DF[c('fruit_label','mass','width','height','color_score')]

X_train <-DF[c('mass','width','height','color_score')]
Y_Train <-DF[c('fruit_label')]

DF$fruit_name =  as.factor(DF$fruit_name)
DF$fruit_subtype =  as.factor(DF$fruit_subtype)

mytree1 <-rpart(fruit_label ~ mass+width+height+color_score,data=df,method = 'class')
fancyRpartPlot(mytree1)

mytree2 <-rpart(fruit_label ~ color_score,data=df,method = 'class')
fancyRpartPlot(mytree2)

mytree3 <-rpart(fruit_label ~ width+height,data=df,method = 'class')
fancyRpartPlot(mytree3)

mytree4 <-rpart(fruit_name ~ mass,data=df,method = 'class')
fancyRpartPlot(mytree4)

prediction=predict(mytree1,X_train,type = 'class')
View(prediction)

barplot(table((DF$fruit_label)))
barplot(table(prediction))



View(X_train)
tree <-rpart(fruit_label ~ mass+width+height+color_score,data=df,method = 'class')
prediction1=predict(tree,X_train,type = 'class')
View(prediction1)
barplot(table(prediction1))



library('mice')
library('lattice')
library('randomForest')


### you mast change the target as factor before the random forest

Y_Train<-as.factor(Y_Train$fruit_label)








fit1<-randomForest(x = X_train,y = Y_Train,importance = TRUE,ntree = 1000)
fit1
