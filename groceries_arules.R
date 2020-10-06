#Prepare rules for the all the data sets 
#1) Try different values of support and confidence. Observe the change in number of rules for different support,confidence values
#2) Change the minimum length in apriori algorithm
#3) Visulize the obtained rules using different plots 

# Install packages required.
install.packages("arules")
library(arules)
library(arulesViz)
library(rmarkdown)
library(Matrix)
library(grid)

groceries <- read.csv("G:\\ExcelR Material DATA SCIENCE\\Assignments\\Association Rules\\groceries.csv")
View(groceries)
attach(groceries)
summary(groceries)
str(groceries)

# converting everything into character format 
groceries[] <- lapply(groceries,as.character)
View(groceries)

# Creating a custom fucntion to collapse all the items in a transaction into a single sentence .
paste_fun <- function(i){
  return (paste(as.character(i),collapse=" "))
}

# Applying the custom function
groceries["sentence"] <- apply(groceries,1,paste_fun)
View(groceries)

# tm package is used to do text manipulation and forming DTM and TDM matrices
install.packages("tm")
library(tm)

# Selecting the new column which contains all items of a transaction in a single sentence
x <- Corpus(VectorSource(groceries$sentence)) 
x <- tm_map(x,stripWhitespace)

# Creating a TDM matrix
dtm0 <- t(TermDocumentMatrix(x))

# Converting TDM matrix to data frame
dtm0_df <- data.frame(as.matrix(dtm0))
View(dtm0_df)

# Building model for Association Rules 
#Item Frequecy plot
windows()

# count of each item from all the transactions 
barplot(sapply(dtm0_df,sum),col=1:10)

# Applying apriori algorithm to get relevant rules
#######1########
rules <- apriori(as.matrix(dtm0_df),parameter = list(support=0.002,confidence=0.05,minlen=5))
inspect(rules)
inspect(head(sort(rules, by='lift')))

# Sorting rules by confidence 
rules_conf <- sort(rules,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
inspect(rules_lift)

plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "mosaic")

############2#############
rules1 <- apriori(as.matrix(dtm0_df),parameter = list(support=0.004,confidence=0.05,minlen=4))
inspect(rules1)
inspect(head(sort(rules1, by='lift')))

# Sorting rules by confidence 
rules_conf <- sort(rules1,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules1,by="lift")
inspect(rules_lift)

plot(rules1,method = "scatterplot")
plot(rules1,method = "grouped")
plot(rules1,method = "graph")
plot(rules1,method = "mosaic")

############3#############
rules2 <- apriori(as.matrix(dtm0_df),parameter = list(support=0.005,confidence=0.06,minlen=3))
inspect(rules2)
inspect(head(sort(rules2, by='lift')))

# Sorting rules by confidence 
rules_conf <- sort(rules2,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules2,by="lift")
inspect(rules_lift)

plot(rules2,method = "scatterplot")
plot(rules2,method = "grouped")
plot(rules2,method = "graph")
plot(rules2,method = "mosaic")

############4#############
rules3 <- apriori(as.matrix(dtm0_df),parameter = list(support=0.005,confidence=0.06,minlen=4))
inspect(rules3)
inspect(head(sort(rules3, by='lift')))

# Sorting rules by confidence 
rules_conf <- sort(rules3,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules3,by="lift")
inspect(rules_lift)

plot(rules3,method = "scatterplot")
plot(rules3,method = "grouped")
plot(rules3,method = "graph")
plot(rules3,method = "mosaic")

#########################

