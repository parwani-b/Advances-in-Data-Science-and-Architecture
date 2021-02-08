#Question 1
LinearRegq1 <- read.csv("C:/Users/parwa/Desktop/Northeastern/Fall 2017/ADS/Assignments/Linear regressionCSV.csv")
lm(LinearRegq1)
str(LinearRegq1)
LinearModel <- lm(Yield~Factor.1+Factor.2, data = LinearRegq1)
LinearModel

summary(LinearModel)




#Question 2

loan <- read.csv(("C:/Users/parwa/Desktop/Northeastern/Fall 2017/ADS/Assignments/loanCSV.csv"))
LogRegModel <- glm(formula = Decision~Res_status+Occupation+Job_status+Liab_ref+Acc_ref,family = binomial(link='logit'),data = loan)
summary(LogRegModel)
loanData <- data.frame(Res_status="owner",Occupation="creative_",Job_status="governmen",Liab_ref="f",Acc_ref="given")
predict(LogRegModel,loanData,type="response")
loanData2 <- data.frame(Res_status="rent",Occupation="creative_",Job_status="governmen",Liab_ref="f",Acc_ref="given")
predict(LogRegModel,loanData2,type="response")
