
##set the directory first
setwd('~/tensorflow_machine_learning/R_PROJECT/google_stock/')

## take the data
data = read.csv('data/google_stock.csv')

View(data)
#we need specfic information about this raw data the data which we can count
#we create a new data frame
#we need openning price 
#closing  price 
#low price and high price 
data1<-data[c('Adj..Open','Adj..High','Adj..Low','Adj..Close','Adj..Volume')]
View(head(data1))

data1$volatility=(data1$Adj..High-data1$Adj..Close)/data1$Adj..Close
 
data1$PCT_Change=(data1$Adj..Close-data1$Adj..Open)/data1$Adj..Open

View(data1)


train = data1[1:3000,]

test = data1[3001:3424,]
#View(test)
#View(train)



##############################################################
#library('mice')
#train <- as.data.frame(train)
library('lattice')
library('randomForest')

X_test<-test[c('Adj..Open','Adj..High','Adj..Low','Adj..Volume','volatility','PCT_Change')]
Y_test <-test[c('Adj..Close')]
X_train<-train[c('Adj..Open','Adj..High','Adj..Low','Adj..Volume')]
Y_train <-train[c('Adj..Close')]

fit1<-randomForest(Adj..Close ~ .,data = train)
fit1
final_predict<-predict(fit1,X_test)
plot(final_predict)


require(Metrics)

#rmse <- sqrt(mean(as.matrix(log1p(y_train) - log1p(y_predictions))^2))
#print(paste('RMSE:', rmse))
# ... Metrics lib
#rmse <- rmse(log1p(as.matrix(y_train)), log1p(y_predictions))
#print(paste('RMSE:', rmse))





rmse1 <- sqrt(mean(as.matrix(log1p(Y_test) - log1p(final_predict))^2))
print(paste('RMSE:', rmse1))
# ... Metrics lib
rmse <- rmse(log1p(as.matrix(Y_test)), log1p(final_predict))
print(paste('RMSE:', rmse))
