install.packages("C50",repos = "http://cran.us.r-project.org")
install.packages("tree",repos = "http://cran.us.r-project.org")
install.packages("caret",repos = "http://cran.us.r-project.org")
install.packages("gmodels",repos = "http://cran.us.r-project.org")
install.packages("party",repos = "http://cran.us.r-project.org")
install.packages("knitr",repos = "http://cran.us.r-project.org")
install.packages("png",repos = "http://cran.us.r-project.org")
library(party)
library(caret)
library(C50)
library(tree)
library(gmodels)
library(knitr)
library(png)
FraudCheck <- read.csv(file.choose())
# Splitting data into training and testing.
# splitting the data based on Sales
hist(FraudCheck$Taxable.Income)
Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
FC = data.frame(FraudCheck,Risky_Good)
#CD <- CompanyData[,2:12]
# View(CD)

FC_train <- FC[1:300,]

# View(CD_train)
FC_test <- FC[301:600,]

# View(CD_test)

###Using Party Function 

png(file = "decision_tree.png")
FC$Risky_Good<-factor(FC$Risky_Good)
FC$Undergrad<-factor(FC$Undergrad)
FC$Marital.Status<-factor(FC$Marital.Status)
FC$Urban<-factor(FC$Urban)
opall_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = FC)
summary(opall_tree)
plot(opall_tree)
# From the above tree, It looks like the data has 20 % of Risky patients and 80 % good patients


# using the training Data 

png(file = "decision_tree.png")
FC_train$Risky_Good=factor(FC_train$Risky_Good)
FC_train$Undergrad=factor(FC_train$Undergrad)
FC_train$Marital.Status=factor(FC_train$Marital.Status)
FC_train$Urban=factor(FC_train$Urban)

FC_test$Risky_Good=factor(FC_test$Risky_Good)
FC_test$Undergrad=factor(FC_test$Undergrad)
FC_test$Marital.Status=factor(FC_test$Marital.Status)
FC_test$Urban=factor(FC_test$Urban)
op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(op_tree)
plot(op_tree)

pred_tree <- as.data.frame(predict(op_tree,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=FC_test)


mean(pred_test_df==FC_test$Risky_Good) # Accuracy = 82 %
CrossTable(FC_test$Risky_Good,pred_test_df)
confusionMatrix(FC_test$Risky_Good,pred_test_df)

