## import the data
## social network ads data
###############

setwd('~/tensorflow_machine_learning/R_PROJECT/SOCIAL_MEDIA_PREDICTION/')

data  <- read.csv('data/Social_Network_Ads.csv')
View(data)

############################
##explanation of the dataset
#userid
#gender
#age
#salary
#purchaged or not
## we drop the user id train the next three and perform 
## analysis
## and predict parchaged or not
############################
##spliting the training and testing data
#train = data[1:300,]

X_train1<-data[1:300,c('Gender','Age','EstimatedSalary')]
y_train1<-data[1:300,c('Purchased')]
Xtrain<-data[1:400,c('Gender','Age','EstimatedSalary')]
ytrain<-data[1:400,c('Purchased')]
View(X_train1)
View(y_train1)

#test = data[301:400,]
X_test1<-data[301:400,c('Gender','Age','EstimatedSalary')]
y_test1<-data[301:400,c('Purchased')]
#############################
View(X_test1)
View(y_test1)
###############################
testy<-data[1:400,c('Purchased')]
#############################
##COMBINE THE DATA SET FIR EASY USE
#combined_set_X <- rbind(X_train1,X_test1)
#combined_set_Y <- rbind(y_train1,y_test1)
#
#not necessary
combined_set_X <- data[1:400,c('Gender','Age','EstimatedSalary','Purchased')]
combined_set_Y <- data[1:400,c('Purchased')]

View(combined_set_X)
View(combined_set_Y)


#### EDA of the combined datasets starts here

purchaged_or_not=table(data$Purchased)
barplot(purchaged_or_not)
###########################################
###########################################
gender_status=table(data$Gender)
barplot(gender_status)

#######################################

age_status=table(data$Age)
barplot(age_status)

#####################################

## how many people survived and death
## python command value_counts()
table(data$EstimatedSalary)
barplot(table(data$EstimatedSalary))
###### plotting 


View(combined_set_X)




###################feature Engineering  ###############
## adding the adult and the other younger column
combined_set_X$Age_status='Old'
combined_set_X$Age_status[combined_set_X$Age <18 & combined_set_X$Age >=10] <- 'Kid'
combined_set_X$Age_status[combined_set_X$Age <30 & combined_set_X$Age >=18] <- 'Mage1'
combined_set_X$Age_status[combined_set_X$Age <40 & combined_set_X$Age >=30] <- 'Mage2'


#########crazy right?


###################################

combined_set_X$salary_status='VVRich'
combined_set_X$salary_status[combined_set_X$EstimatedSalary <3700 & combined_set_X$EstimatedSalary >=15000] <- 'poor'
combined_set_X$salary_status[combined_set_X$EstimatedSalary <60000 & combined_set_X$EstimatedSalary >=3700] <- 'm_cls1'
combined_set_X$salary_status[combined_set_X$EstimatedSalary <82000 & combined_set_X$EstimatedSalary >=60000] <- 'm_cls2'
combined_set_X$salary_status[combined_set_X$EstimatedSalary <112000 & combined_set_X$EstimatedSalary >=82000] <- 'rich'
combined_set_X$salary_status[combined_set_X$EstimatedSalary <143000 & combined_set_X$EstimatedSalary >=112000] <- 'very_rich'

View(combined_set_X)


###################################
##################################

barplot(table(combined_set_X$Age_status))
barplot(table(combined_set_X$salary_status))


##################################
##################################





library('rpart')
library('rattle')
library('RColorBrewer')



##############################

##decision tree plotting
combined_set_X$Age_status<-factor(combined_set_X$Age_status)
combined_set_X$salary_status<-factor(combined_set_X$salary_status)


mytree1 <-rpart(Purchased ~ Age_status+salary_status,data=combined_set_X,method = 'class')
fancyRpartPlot(mytree1)


mytree2 <-rpart(Purchased ~ Age+EstimatedSalary+Gender,data=combined_set_X,method = 'class')
fancyRpartPlot(mytree2)


which(is.na(combined_set_X))


################################
################################


prediction=predict(mytree2,Xtrain,type = 'class')
View(prediction)
table(prediction)
table(data$Purchased)



######################
##first prediction


prediction_data_frame=data.frame(User.ID=data$User.ID,Purchased=prediction)

View(prediction_data_frame)

write.csv(prediction_data_frame,file = "oe.csv" ,row.names=FALSE)



library('lattice')
library('randomForest')


X <- data[1:400,c('Gender','Age','EstimatedSalary')]
Y <- data[1:400,c('Purchased')]


############
#######

# REMEMBER EVRY TIME YOU MAKE ANY PREDICTION IN RANDOM FOREST

### YOU MUST CHANGE TO A FACTOR EVEN IT IS A NUMERICAL
######
#########

Y=as.factor(Y)

fit1<-randomForest(x = X,y = Y,importance = TRUE,ntree = 1000)

final_predict<-predict(fit1,X)

View(final_predict)


table(final_predict)
table(data$Purchased)
