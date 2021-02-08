                                                                #Loading the CSV files
Titanic_train_file <- read.csv("C:/Users/parwa/Desktop/Northeastern/Fall 2017/ADS/Assignments/Week-3(10-01-2017)/Titanic train.csv", header = TRUE)
Titanic_train_file
Titanic_test_file <- read.csv("C:/Users/parwa/Desktop/Northeastern/Fall 2017/ADS/Assignments/Week-3(10-01-2017)/Titanic test.csv", header = TRUE)
Titanic_test_file

                                                             #Installing the required R packages
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("RColorBrewer")
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)


Titanic_train_file$Sex <- factor(Titanic_train_file$Sex, labels = c(1,2))
Titanic_train_file$Embarked <- factor(Titanic_train_file$Embarked, labels = c(1:4))

Titanic_test_file$Sex <- factor(Titanic_test_file$Sex, labels = c(1,2))
Titanic_test_file$Embarked <- factor(Titanic_test_file$Embarked, labels = c(1:3))

Titanic_train_file$Sex
Titanic_train_file$Embarked
Titanic_test_file$Sex
Titanic_test_file$Embarked

                                              #Checking the categorical variables and the associated levels
summary(Titanic_train_file)
summary(Titanic_test_file)
levels(Titanic_train_file$Embarked)
levels(Titanic_test_file$Sex)
 
                                              #Performing Linear regression for finding the p-value
var1.lm<- lm(Survived ~ PassengerId+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=Titanic_train_file)
summary(var1.lm)

                                               #Decision tree for prediction purpose
dec_tree_mod <- rpart(Survived ~PassengerId+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, Titanic_train_file, method = "class" )
p <- predict(dec_tree_mod, Titanic_test_file, type = "class", se.fit=FALSE)
p
                                               #Comparison of predicted values
table(p,Titanic_test_file[,2])
p
Titanic_test_file[,2]
Titanic_test_file
                                               #Plotting the data
print(dec_tree_mod)
plot(dec_tree_mod)
text(dec_tree_mod, pretty=0, use.n = TRUE, cex= 0.75)
fancyRpartPlot(dec_tree_mod)

printcp(dec_tree_mod)
plotcp(dec_tree_mod)

                                               #Pruning the tree
ptree<- prune(dec_tree_mod, cp= dec_tree_mod$cptable[which.min(dec_tree_mod$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree, uniform=TRUE, main="Category Tree")

