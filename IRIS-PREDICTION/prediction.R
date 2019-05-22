####
#set the directory
setwd('~/tensorflow_machine_learning/R_PROJECT/IRIS-PREDICTION/')
## import the data
df <- read.csv('Iris.csv')
View(df)

##checking id there is a null value
table(is.na(df))

barplot(table(df$SepalLengthCm),xlab = 'Sepal Length',ylab = 'number of flower')
barplot(table(df$SepalWidthCm),xlab = 'Sepal Width',ylab = 'number of flower')
barplot(table(df$PetalLengthCm),xlab = 'Petal Length',ylab = 'number of flower')
barplot(table(df$PetalWidthCm),xlab = 'Petal width',ylab = 'number of flower')



## EDA Starts here 
##classify the data

library('rpart')
library('rattle')
library('RColorBrewer')

mytree1 <-rpart(Species ~ SepalLengthCm,data=df,method = 'class')
fancyRpartPlot(mytree1)

mytree2 <-rpart(Species ~ SepalLengthCm+SepalWidthCm+PetalLengthCm+PetalWidthCm,data=df,method = 'class')
fancyRpartPlot(mytree2)

mytree3 <-rpart(Species ~ SepalLengthCm+SepalWidthCm,data=df,method = 'class')
fancyRpartPlot(mytree3)

