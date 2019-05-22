##SVM AND RANDOM FOREST ALGORITHM 
##set the directory
setwd('tensorflow_machine_learning/R_PROJECT/ABALONA_AGE_PREDICTION/')
## import data
df=read.csv('abalone.csv')
##
View(df)
## TARGET IS THE RING AND ACTUALLY THE AGE BUT WE NEED TO EDIT THIS
## IN THE DOCUMENTATION SYS THE THEIR AGE IS 1.5 YEARS MORE THAN THE RING COLUMN
df$Age <-df$Rings+1.5
## we got the age column
## explaratory data analysis starts here
pie(table(df$Sex))
View(df)
### data slicing and train and test data
df1= df[c("Sex","Length","Diameter","Height","Whole.weight","Shucked.weight","Viscera.weight","Shell.weight")]
X_train = df1[1:3000,]
## we gonna predict with these value and compare with the Y_test
X_test = df1[3001:4177,]
df2= df[c("Age")]
Y_train = df2[1:3000,]
Y_test = df2[3001:4177,]
## create a decision tree and make prediction
library('rpart')
library('rattle')
library('RColorBrewer')
df$Sex = as.factor(df$Sex)
tree1 = rpart(Age ~ Sex+Length+Diameter+Height+Whole.weight+Shucked.weight+Viscera.weight+Shell.weight,data = df,method = 'class')
fancyRpartPlot(tree1)
tree2 = rpart(Age ~ Sex,data = df,method = 'class')
fancyRpartPlot(tree2)
tree3 = rpart(Age ~ Sex+Length+Diameter+Height,data = df,method = 'class')
fancyRpartPlot(tree3)
first_prediction = predict(tree1,X_test,type = 'class')
##compare
plot(table(first_prediction))
plot(table(df$Age[3001:4177]))

View(Y_train)
library('mice')
library('lattice')
library('randomForest')
df$Age = as.factor(df$Age)
fit1<-randomForest(x = X_train,y = Y_train,importance = TRUE,ntree = 1000)
final_predict<-predict(fit1,X_test)
View(final_predict)
plot(Y_test)
View(Y_test)
plot(final_predict)
plot(fit1)





###### APPLYING KNN
library('pROC')
library('caret')
library('mlbench')
fit3 <-train(Age~ .,data=X_train,method="knn")
plot(fit3)

pr3<-predict(fit3,newdata = X_test)
plot(pr3)
plot(Y_test)



