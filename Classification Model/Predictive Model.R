# import dataset 
library(readr)
df <- read_csv("C:/Users/alex7/Downloads/LifeOwnershipData - NEW Sep 29.csv")
View(df)

#Prepocess data 
#1.drop poll columns 
library(dplyr)
df <- select(df, -contains('Pol'), -contains('Curr'), -contains('Prior'), -contains('DK'))

#2.drop assumption columns
df <- select(df, -contains('q38'), -contains('q39'), -contains('q40'), -contains('q34'), -contains('q21'))
df <- select(df, -c('q37','q1','q2','q3_1','q3_2','q3_3','q3_4'))

#3.drop redudant & unuseful columns
df <- select(df, -c('X1','YearsReplacement','TotalLife','GrpAmtsUnknown','TotalIndLifeFaceAmt','TotalGroup','PersonalIncome4Cat','Q15','AdultChild','AdultGender'))
df <- select(df, -c('qs2','qd9b','qd4b','qd7b','qd4a','qd4d'))
df <- select(df, -c('q4','q5','q6'))

#4.drop directly related with AnyLifePerson
df <- select(df, -c('q41','q16','q22','q30','Q18','Q18a'))
df <- select(df, -c('AnyGroupPerson',	'AnyIndividualPerson', 'AnyTerm',	'AnyPerm', 'respid','MidPtCalcPerson', 'Q14'))

#drop N/A rows
df_clean <-df[!is.na(df$PersonalIncome5Cat),]
df_clean <- df_clean[, colSums(is.na(df_clean))<1]

#rename columns
colnames(df_clean) <- c('FamilyIncome','FamilySize','Marital','Gender','Age','PersonalIncome5Cat','AnyLifePerson','STATE','LaborUnion','Ownership','FamilyInvestment','Mortgage')

# convert class of "Age" to numeric 
class(df_clean$Age)
df_clean$Age <- as.numeric(as.character(df_clean$Age))

# Combine categories
library(car)
df_clean$Marital <- recode(df_clean$Marital, "c('Single, Unknown','Single, never married') = 'Single, never married' ")

#modeling 
#Split train, test dataset   
set.seed(123)
df_clean$AnyLifePerson = ifelse(df_clean$AnyLifePerson=='No', 0, 1)
df_clean =df_clean %>% mutate_if(is.character, as.factor)

ind <- sample(2, nrow(df_clean), replace = TRUE, prob = c(0.8, 0.2))
train <- df_clean[ind==1,]
test <- df_clean[ind==2,]


#RF
library(e1071)
library(caret)
library(randomForest)
set.seed(223)
rf_mod <- randomForest(AnyLifePerson~.,data = train)
print(rf_mod)

rf_train <- predict(rf_mod,train)
rf_pred <- predict(rf_mod,test)

confusionMatrix(rf_train,train$AnyLifePerson)
confusionMatrix(rf_pred,test$AnyLifePerson)

# Variable Importance
varImpPlot(rf_mod,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf_mod)
varUsed(rf_mod)


#Logitisc Regression 
lr_mod <- glm(AnyLifePerson~FamilyIncome+FamilySize+Marital+Gender+Age+PersonalIncome5Cat+STATE+LaborUnion+Ownership+FamilyInvestment+Mortgage,
             family=binomial("logit"),data=train)
summary(lr_mod)

lr_train=predict(lr_mod,train)
lr_pred=predict(lr_mod,test)

lr_train_pred <- ifelse(lr_train>0.5, 1, 0)
lr_test_pred <- ifelse(lr_pred>0.5, 1, 0)

confusionMatrix(table(lr_train_pred,train$AnyLifePerson))
confusionMatrix(table(lr_test_pred,test$AnyLifePerson))


#SVM 
set.seed(333)
svm_mod=svm(AnyLifePerson~.,train)
summary(svm_mod)

svm_train=predict(svm_mod,train)
svm_pred=predict(svm_mod,test)

confusionMatrix(svm_train,train$AnyLifePerson)
confusionMatrix(table(svm_pred,test$AnyLifePerson))


