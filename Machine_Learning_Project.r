#--------------------------Question or problem definition.---------------------
#In this challenge, we ask you to complete the analysis of what sorts of people were 
#likely to survive. In particular, we ask you to apply 
#the tools of machine learning to predict which passengers survived the tragedy.

#On April 15, 1912, during her maiden voyage, the Titanic sank after colliding with an iceberg, killing 1502 out of 2224 passengers and crew. Translated 32% survival rate.
#One of the reasons that the shipwreck led to such loss of life was that there were not enough lifeboats for the passengers and crew.
#Although there was some element of luck involved in surviving the sinking, some groups of people were more likely to survive than others, such as women, children, and the upper-class.

#--------------------------Acquire training and testing data.------------------
#install.packages("ROCR")
library(ROCR)
rm(list=ls())
#setwd("C:\\Users\\abc\\Desktop\\ITU\\ML\\project\\Titanic")
#read test and train files
titanic.train <- read.csv(file="train.csv",stringsAsFactors = FALSE,header = TRUE)
titanic.test <- read.csv(file="test.csv",stringsAsFactors = FALSE,header = TRUE)
str(titanic.train)
str(titanic.test)
#Categorical: Survived, Sex, and Embarked. Ordinal: Pclass.
#Continous: Age, Fare. Discrete: SibSp, Parch.
#Ticket is a mix of numeric and alphanumeric data types. Cabin is alphanumeric.

#we combine test and train data set and clean it
titanic.train$IsTrainSet<-TRUE
titanic.test$IsTrainSet<-FALSE
ncol(titanic.train)
ncol(titanic.test)
#need to line up headers as well
names(titanic.train)
names(titanic.test)
#same headers so no need to line up
#now for survived column
#fill in colmn with NA
titanic.test$Survived <-NA
#now combine
titanic.full <- rbind(titanic.train,titanic.test)
tail(titanic.full)
table(titanic.full$IsTrainSet)

summary(titanic.full)
#null values
#Survived :418
#Age: 263
#Fare:1

#--------------------------Wrangle, prepare, cleanse the data.-----------------
#check missing values
#for passengerId
passengerId_null = sum(is.na(titanic.full$PassengerId))
sprintf("No of null values for passennger Id: %d",passengerId_null)
#no missing values found

#for Survived
survived_null = sum(is.na(titanic.full$Survived))
sprintf("No of null values for survied : %d",survived_null)
#for test set survived is fill with NULL

#for Pclass
pclass_null = sum(is.na(titanic.full$Pclass))
sprintf("No of null values for Pclass : %d",pclass_null)
#no missing values found

#for name
names_null = sum(is.na(titanic.full$Name))
sprintf("No of null values for names : %d",names_null)
#no missing values found

#for Sex
sex_null = sum(is.na(titanic.full$Sex))
sprintf("No of null values for gender : %d",sex_null)
#no missing values found

#for age
age_null = sum(is.na(titanic.full$Age))
sprintf("No of null values for age : %d",age_null)
#missing values found
#lets replace with median need to work on that
age.median<-median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age),"Age"] <- age.median
#(need to modifiy)
#replacing missing age by median age of the same sex

#for sibsp
sibsp_null = sum(is.na(titanic.full$SibSp))
sprintf("No of null values for sibsp : %d",sibsp_null)
#no missing values

#for Parch
parch_null = sum(is.na(titanic.full$Parch))
#no missing values
sprintf("No of null values for parch : %d",parch_null)
#no missing values

#for Ticket
ticke_null = sum(is.na(titanic.full$Ticket))
#no missing values
sprintf("No of null values for parch : %d",parch_null)

#for fare
fare_null = sum(is.na(titanic.full$Fare))
sprintf("No of null values for fare : %d",fare_null)
#1 missing values

## ## to know Which passenger has no fare information 
titanic.full[(which(is.na(titanic.full$Fare))) , 1] 
### Looks like Passenger number 1044 has no listed Fare
# Where did this passenger leave from? What was their class?
#Show row 1044
titanic.full[1044, ]
## Looks like he left from 'S' (Southampton) as a 3rd class passenger. 
## Let's see what other people of the same class and embarkment port paid for 
## their tickets.
titanic.full$Fare[1044] <- median(titanic.full[titanic.full$Pclass == '3' & titanic.full$Embarked == 'S', ]$Fare, na.rm = TRUE)
### Looks like the median cost for a 3rd class passenger leaving out of 
## Southampton was 8.05. That seems like a logical value for this passenger 
## to have paid.

#for cabin
titanic.full$Cabin[titanic.full$Cabin == ""] <- NA
cabin_null = sum(is.na(titanic.full$Cabin))
sprintf("No of null values for cabin : %d",cabin_null)
print("But since too much data is missing from cabin, it doesnt make sense to predict the cabin numbers based on too little data")
#There are  missing values 
#But since too much data is missing from cabin, 
#it doesnt make sense to predict the cabin numbers based on too little data.

#for Embarked
titanic.full$Embarked[titanic.full$Embarked == ""] <- NA 
embarked_null = sum(is.na(titanic.full$Embarked))
sprintf("No of null values for Embarked : %d",embarked_null)

#2 values are null
#just for now we replace it with 'S' we need to work on that
## ## To know which passengers have no listed embarkment port
titanic.full[(which(is.na(titanic.full$Embarked))), 1] 
# Explanation: Passengers 62 and 830 are missing Embarkment
titanic.full[c(62, 830), 'Embarked']

titanic.full$Embarked[c(62,830)] <- 'S'
#modification(need to work )------------
## ## To know which passengers have no listed embarkment port
#titanic.full[(which(is.na(titanic.full$Embarked))), 1] 
# Explanation: Passengers 62 and 830 are missing Embarkment
#titanic.full[c(62, 830), 'Embarked']
## So Passenger numbers 62 and 830 are each missing their embarkment ports.
## Let's look at their class of ticket and their fare.
#titanic.full[c(62, 830), c(1,3,10)]
## Both passengers had first class tickets that they spent 80 (pounds?) on. 
## Let's see the embarkment ports of others who bought similar kinds of tickets.
#median(titanic.full[titanic.full$Pclass == '1' & titanic.full$Embarked == 'S', ]$Fare, na.rm = TRUE)

#titanic.full%>%
#  group_by(Embarked, Pclass) %>%
#  filter(Pclass == "1") %>%
#  summarise(mfare = median(Fare),n = n())

#aggregate(titanic.full[, "Embarked":"Pclass"], list(titanic.full$Fare), mean)

#----------------------------Featured Engineering-------------------
## We can break down Passenger name into additional meaningful variables 
## which can feed predictions or be used in the creation of additional new 
## variables. For instance, passenger title is contained within the passenger 
## name variable and we can use surname to represent families. 

colnames(titanic.full)

### Retrieve title from passenger names

titanic.full$title<-gsub('(.*, )|(\\..*)', '', titanic.full$Name)

# Show title counts by sex

table(titanic.full$Sex, titanic.full$title)

## Convert title with low count into new title
unusual_title<-c('Capt', 'Col', 'Don','Dona', 'Dr', 'Jonkheer', 'Lady', 'Major', 'Rev', 'Sir', 'the Countess')

## Rename/reassign  Mlle, Ms, and Mme

titanic.full$title[titanic.full$title=='Mlle']<-'Miss'
titanic.full$title[titanic.full$title=='Ms']<-'Miss'
titanic.full$title[titanic.full$title=='Mme']<-'Mrs'
titanic.full$title[titanic.full$title %in% unusual_title]<-'Unusual Title'
## Check the title count again
table(titanic.full$Sex, titanic.full$title)


#create new attribute Child
titanic.full$child[titanic.full$Age<18]<-"Child"
titanic.full$child[titanic.full$Age>=18]<-"Adult"

#create new attribute Mother
titanic.full$isMother <- "Not Mother"
titanic.full$isMother[titanic.full$Sex=="female" & titanic.full$Parch >0 &titanic.full$Age >18 & titanic.full$title != "Miss"]<-"Mother"

#create new feature Family = Sibsp +parch +1 (for him.herself)
#(lets see if it give more probability)
titanic.full$familysize <- titanic.full$SibSp + titanic.full$Parch +1
family_null = sum(is.na(titanic.full$familysize))
#no missing values
sprintf("No of null values for familysize : %d",family_null)
#no missing values
#split into threee variables
titanic.full$fsizeD[titanic.full$familysize==1]<-'single'
titanic.full$fsizeD[titanic.full$familysize<5 & titanic.full$familysize>1]<-'small'
titanic.full$fsizeD[titanic.full$familysize>4]<-'large'

#----------------------------categorical casiting-------------------
titanic.full$Pclass<-as.factor(titanic.full$Pclass)
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)
#need to factor new attributes as well
titanic.full$fsizeD<-as.factor(titanic.full$fsizeD)
titanic.full$isMother<-as.factor(titanic.full$isMother)
titanic.full$child<-as.factor(titanic.full$child)
#copied from NB
titanic.full$SibSp = as.factor(titanic.full$SibSp)
titanic.full$Parch = as.factor(titanic.full$Parch)
#
str(titanic.full)


#---------------------separate test and train data set
titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE,]
str(titanic.train)
str(titanic.test)


#----------------------------Relation---------------------------

print("Relation between Pclass and Survived")
?aggregate()
aggregate(titanic.train["Survived"], by = list(Pclass=titanic.train$Pclass),FUN=mean,na.rm=TRUE)

print("Relation between Sex and Survived")
aggregate(titanic.train["Survived"], by = list(Sex=titanic.train$Sex),FUN=mean,na.rm=TRUE)

print("Correlation between sibsp and Survived")
#length(titanic.train$Survived)
#length(titanic.train$SibSp)
aggregate(titanic.train["Survived"], by = list(SibSp=titanic.train$SibSp),FUN=mean,na.rm=TRUE)

print("Correlation between parch and Survived")
aggregate(titanic.train["Survived"], by = list(Parch=titanic.train$Parch),FUN=mean,na.rm=TRUE)


print("Correlation between familysize and Survived")
aggregate(titanic.train["Survived"], by = list(fsizeD=titanic.train$fsizeD),FUN=mean,na.rm=TRUE)

print("Correlation between child and Survived")
aggregate(titanic.train["Survived"], by = list(child=titanic.train$child),FUN=mean,na.rm=TRUE)

print("Correlation between Mother and Survived")
aggregate(titanic.train["Survived"], by = list(isMother=titanic.train$isMother),FUN=mean,na.rm=TRUE)


print("Correlation between Embarked and Survived")
aggregate(titanic.train["Survived"], by = list(Embarked=titanic.train$Embarked),FUN=mean,na.rm=TRUE)

#need to categories survived but doing it after combine as there are NA values
titanic.train$Survived<-as.factor(titanic.train$Survived)


#----------------------------modeling----------------------
Survived.eqution<-"Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked "
Survived.formula <- as.formula(Survived.eqution)
y_train <- titanic.train$Survived
#Random Forest
library(randomForest)
titanic.model <- randomForest(formula=Survived.formula,data=titanic.train,ntree =500,mtry=3,nodesize=0.01*nrow(titanic.train))
features.eqution<-"Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

yhat<-predict(titanic.model, newdata =titanic.train)
submit <- data.frame(Survived = yhat)
rf_error<- 1-sum(submit$Survived==y_train)/length(y_train) # Train_error = 0.09561305
sprintf("Train error for Random forest %f",rf_error)

pred1<-prediction(as.numeric(submit$Survived), as.matrix(y_train))
perf1<-performance(pred1,"tpr","fpr")
rf_x.values= data.frame(perf1@x.values)
rf_y.values= data.frame(perf1@y.values)
plot(perf1,col="red", type = "l")
points(as.matrix(rf_x.values), as.matrix(rf_y.values),col="blue", type = "p") 
abline(0,1)
Survived<-predict(titanic.model, newdata =titanic.test)
PassengerId<-titanic.test$PassengerId
rf_result<-data.frame(PassengerId = titanic.test$PassengerId, Survived = Survived)
summary(rf_result)

#------------------------------random forest with new features added -----------------
Survived.equtionnew<-"Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + fsizeD + isMother + child"
Survived.formulanew <- as.formula(Survived.equtionnew)
y_trainnew <- titanic.train$Survived

#Random Forest- new features 
titanic.modelnew <- randomForest(formula=Survived.formulanew,data=titanic.train,ntree =500,mtry=3,nodesize=0.01*nrow(titanic.train))
features.equtionnew<-"Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + fsizeD + isMother + child"

yhatnew<-predict(titanic.model, newdata =titanic.train)
submit <- data.frame(Survived = yhatnew)
rf_errornew<- 1-sum(submit$Survived==y_trainnew)/length(y_trainnew) # Train_error = 0.09315376
sprintf("Train error for Random forest with new features(fsizeD,isMother and child) %f",rf_errornew)

pred2<-prediction(as.numeric(submit$Survived), as.matrix(y_train))
perf2<-performance(pred2,"tpr","fpr")
rfnew_x.values= data.frame(perf2@x.values)
rfnew_y.values= data.frame(perf2@y.values)
plot(perf2,col="red", type = "l")
points(as.matrix(rfnew_x.values), as.matrix(rfnew_y.values),col="blue", type = "p") 
abline(0,1)

Survived<-predict(titanic.modelnew, newdata =titanic.test)
PassengerId<-titanic.test$PassengerId
rf_resultnew<-data.frame(PassengerId = titanic.test$PassengerId, Survived = Survived)
summary(rf_resultnew)


#Decision tree
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=titanic.train,
             method="class")
plot(fit)
text(fit)

yhat_train<- predict(fit, titanic.train, type = "class")
yhat_test<- predict(fit, titanic.test, type = "class")
submit <- data.frame(Survived = yhat_train)
dt_error<- 1-sum(submit$Survived==y_train)/length(y_train) # train_error= 0.167604
sprintf("Train error for Decision tree %f",dt_error)

pred3<-prediction(as.numeric(submit$Survived), as.matrix(y_train))
perf3<-performance(pred3,"tpr","fpr")
dt_x.values= data.frame(perf3@x.values)
dt_y.values= data.frame(perf3@y.values)
plot(perf3,col="red", type = "l")
points(as.matrix(dt_x.values), as.matrix(dt_y.values),col="blue", type = "p") 
abline(0,1)

# For test data
df_result<-data.frame(PassengerId = titanic.test$PassengerId, Survived = yhat_test)
summary(df_result)

#----------------------decision tree with new features added ----------
fitnew <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked+fsizeD + isMother + child,
             data=titanic.train,
             method="class")
plot(fitnew)
text(fitnew)

yhat_trainnew<- predict(fitnew, titanic.train, type = "class")
yhat_testnew<- predict(fitnew, titanic.test, type = "class")
submitnew <- data.frame(Survived = yhat_trainnew)
dt_errornew<- 1-sum(submit$Survived==y_trainnew)/length(y_trainnew) # train_error= 0.1672278
sprintf("Train error for Decision tree with new features(fsizeD,isMother and child) %f",dt_errornew)

pred4<-prediction(as.numeric(submit$Survived), as.matrix(y_train))
perf4<-performance(pred4,"tpr","fpr")
dtnew_x.values= data.frame(perf4@x.values)
dtnew_y.values= data.frame(perf4@y.values)
plot(perf4,col="red", type = "l")
points(as.matrix(dtnew_x.values), as.matrix(dtnew_y.values),col="blue", type = "p") 
abline(0,1)
# For test data
df_resultnew<-data.frame(PassengerId = titanic.test$PassengerId, Survived = yhat_testnew)
summary(df_resultnew)


#Naive Bayes
library(klaR)
#install.packages("caret")
library(caret)
#titanic.full$SibSp = as.factor(titanic.full$SibSp)
#titanic.full$Parch = as.factor(titanic.full$Parch)
#titanic.full$Survived = as.factor(titanic.full$Survived)
#titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE,]
#titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE,]

nbModel = NaiveBayes(Survived~Pclass + Age+ Sex + SibSp + Fare+ Parch + Embarked, data=titanic.train, usekernel = FALSE, fL = 0)
yhat_train = predict(nbModel, titanic.train)
submit <- data.frame(Survived = yhat_train$class)
nb_error<- 1-sum(submit$Survived==y_train)/length(y_train) #train_error = 0.2401796
sprintf("Train error for Naive Bayes %f",nb_error)

pred5<-prediction(as.numeric(submit$Survived), as.matrix(y_train))
perf5<-performance(pred5,"tpr","fpr")
nb_x.values= data.frame(perf5@x.values)
nb_y.values= data.frame(perf5@y.values)
plot(perf5,col="red", type = "l")
points(as.matrix(nb_x.values), as.matrix(nb_y.values),col="blue", type = "p") 
abline(0,1)
yhat_test = predict(nbModel, titanic.test)
nb_result<-data.frame(PassengerId = titanic.test$PassengerId, Survived = yhat_test)
summary(nb_result)

#----------------------------Naive baise with new features
#titanic.full$SibSp = as.factor(titanic.full$SibSp)
#titanic.full$Parch = as.factor(titanic.full$Parch)
#titanic.full$Survived = as.factor(titanic.full$Survived)
#titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE,]
#titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE,]

nbModelnew = NaiveBayes(Survived~Pclass + Age+ Sex + SibSp + Fare+ Parch + Embarked+fsizeD + isMother + child
, data=titanic.train, usekernel = FALSE, fL = 0)
yhat_trainnew = predict(nbModelnew, titanic.train)
submitnew <- data.frame(Survived = yhat_trainnew$class)
nb_errornew<- 1-sum(submit$Survived==y_trainnew)/length(y_trainnew) #train_error = 0.2401796
sprintf("Train error for Naive Bayes with new features(fsizeD,isMother and child) %f",nb_errornew)

pred6<-prediction(as.numeric(submit$Survived), as.matrix(y_train))
perf6<-performance(pred6,"tpr","fpr")
nbnew_x.values= data.frame(perf6@x.values)
nbnew_y.values= data.frame(perf6@y.values)
plot(perf6,col="red", type = "l")
points(as.matrix(nbnew_x.values), as.matrix(nbnew_y.values),col="blue", type = "p") 
abline(0,1)
yhat_testnew = predict(nbModelnew, titanic.test)
nb_resultnew<-data.frame(PassengerId = titanic.test$PassengerId, Survived = yhat_testnew)
summary(nb_resultnew)

#SVM
library(e1071)
titanic.train<-na.omit(titanic.train)
titanic.test$Survived <- as.factor(titanic.test$Survived)
titanic.train$Survived <- as.factor(titanic.train$Survived)
x<-subset(titanic.train, select = -Survived)
titanic.train$Survived<- as.factor(titanic.train$Survived)
y<- titanic.train$Survived
Svm <- svm(Survived ~ Pclass + Age+ Sex + SibSp + Fare+ Parch + Embarked, data = titanic.train)#cost:  1 , gamma:  0.04761905  
yhat_train<-predict(Svm,x)
submit <- data.frame(Survived = yhat_train)
svm_error<- 1-sum(submit$Survived==y)/length(y) #0.2107843
sprintf("Train error for SVM %f",svm_error)

x_test <- subset(titanic.test, select = -Survived)
yhat_test<-predict(Svm,x_test)
svm_result<-data.frame(Survived = yhat_test)
svm_result
summary(svm_result)
pred7<-prediction(as.numeric(submit$Survived), as.matrix(y))
perf7<-performance(pred7,"tpr","fpr")
svm_x.values= data.frame(perf7@x.values)
svm_y.values= data.frame(perf7@y.values)
plot(perf7,col="red", type = "l")
points(as.matrix(svm_x.values), as.matrix(svm_y.values),col="blue", type = "p") 
abline(0,1)
#SVM with new features
x<-subset(titanic.train, select = -Survived)
x_test <- subset(titanic.test, select = -Survived)
svmModelnew = svm(Survived~Pclass + Age+ Sex + SibSp + Fare+ Parch + Embarked+fsizeD + isMother + child
                        , data=titanic.train, usekernel = FALSE, fL = 0)
yhat_trainnew = predict(svmModelnew, x)
submit <- data.frame(Survived = yhat_trainnew)
svm_errornew<- 1-sum(submit$Survived==y)/length(y)#train_error = 0.2205882
sprintf("Train error for SVM with new features(fsizeD,isMother and child) %f",svm_errornew)

yhat_testnew = predict(svmModelnew, x_test)
svm_resultnew<-data.frame(Survived = yhat_testnew)
svm_resultnew
summary(svm_resultnew)
pred8<-prediction(as.numeric(submit$Survived), as.matrix(y))
perf8<-performance(pred8,"tpr","fpr")
svmnew_x.values= data.frame(perf8@x.values)
svmnew_y.values= data.frame(perf8@y.values)
plot(perf8,col="red", type = "l")
points(as.matrix(svmnew_x.values), as.matrix(svmnew_y.values),col="blue", type = "p") 
abline(0,1)

#KNN
library(MASS)
library(class)
error <- rep(0, 20)
titanic.train <- na.omit(titanic.train)
x<- data.frame(Pclass=as.numeric(titanic.train$Pclass), Sex = as.numeric(titanic.train$Sex),
               Age = as.numeric(titanic.train$Age), SibSp = as.numeric(titanic.train$SibSp), 
               Fare = as.numeric(titanic.train$Fare),Embarked = as.numeric(titanic.train$Embarked))
titanic.train$Survived <- na.omit(titanic.train$Survived)
label <- as.numeric(titanic.train$Survived)
for (kk in seq(from = 1, to = 20)) {
  out <- knn.cv(x, label, k = kk)
  Error <- 1 - sum(abs(label == out))/length(out)
  error[kk] <- Error
}
best = which.min(error) #16
plot(error)
knn_error = error[best]
sprintf("Train error for KNN %f",knn_error) #0.306931
yhat_train <- knn.cv(x, label, k = best)
pred9<-prediction(as.numeric(yhat_train), as.matrix(label))
perf9<-performance(pred9,"tpr","fpr")
knn_x.values= data.frame(perf9@x.values)
knn_y.values= data.frame(perf9@y.values)
plot(perf9,col="red", type = "l")
points(as.matrix(knn_x.values), as.matrix(knn_y.values),col="blue", type = "p") 
abline(0,1)
# Predict the test data for k = best
x_test<- data.frame(Pclass=as.numeric(titanic.test$Pclass), Sex = as.numeric(titanic.test$Sex),
                    Age = as.numeric(titanic.test$Age), SibSp = as.numeric(titanic.test$SibSp), 
                    Fare = as.numeric(titanic.test$Fare),Embarked = as.numeric(titanic.test$Embarked))
titanic.test$Survived<- as.numeric(0)
label <- titanic.test$Survived 
yhat <- knn.cv(x_test, label, k = best)

# KNN with new features
error <- rep(0, 20)
titanic.train <- na.omit(titanic.train)
x<- data.frame(Pclass=as.numeric(titanic.train$Pclass), Sex = as.numeric(titanic.train$Sex),
               Age = as.numeric(titanic.train$Age), SibSp = as.numeric(titanic.train$SibSp), 
               Fare = as.numeric(titanic.train$Fare),Embarked = as.numeric(titanic.train$Embarked),
               fsizeD = as.numeric(titanic.train$fsizeD), isMother = as.numeric(titanic.train$isMother),
               child = as.numeric(titanic.train$child))
label <- as.numeric(titanic.train$Survived)
for (kk in seq(from = 1, to = 20)) {
  out <- knn.cv(x, label, k = kk)
  Error <- 1 - sum(abs(label == out))/length(out)
  error[kk] <- Error
}
best = which.min(error) #16
plot(error)
knn_errornew = error[best]
sprintf("Train error for KNN %f",knn_errornew) #0.297030
yhat_trainnew <- knn.cv(x, label, k = best)
pred9<-prediction(as.numeric(yhat_trainnew), as.matrix(label))
perf9<-performance(pred9,"tpr","fpr")
knnnew_x.values= data.frame(perf9@x.values)
knnnew_y.values= data.frame(perf9@y.values)
plot(perf9,col="red", type = "l")
points(as.matrix(knnnew_x.values), as.matrix(knnnew_y.values),col="blue", type = "p") 
abline(0,1)
x_test<- data.frame(Pclass=as.numeric(titanic.test$Pclass), Sex = as.numeric(titanic.test$Sex),
                    Age = as.numeric(titanic.test$Age), SibSp = as.numeric(titanic.test$SibSp), 
                    Fare = as.numeric(titanic.test$Fare),Embarked = as.numeric(titanic.test$Embarked))
label <- titanic.test$Survived 
yhatnew <- knn.cv(x_test, label, k = best)

#data frame to pr
Algorithm = c("Random forest","Decision tree","Naive Bayes","SVM", "KNN") 
TrainError = c(rf_error, dt_error, nb_error,svm_error, knn_error) 
#trainError(with new features) =c(rf_error, dt_error, nb_error,svm_error)
df = data.frame(Algorithm, TrainError)
df

#Data Frame of errors after adding new features
Algorithm = c("Random forest","Decision tree","Naive Bayes","SVM", "KNN") 
TrainErrorNew = c(rf_errornew, dt_errornew, nb_errornew,svm_errornew, knn_errornew) 
#trainError(with new features) =c(rf_error, dt_error, nb_error,svm_error)
df = data.frame(Algorithm, TrainErrorNew)
df
