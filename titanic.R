# Author: Peter Chen
# Titanic: Machine Learning from Disaster

train <- read.csv("train.csv")

# Survival rate
table(train$Survived)
prop.table(table(train$Survived))

# How does sex influence survival?
summary(train$Sex)
prop.table(table(train$Sex, train$Survived), 1)

# How does age influence survival?
summary(train$Age)
train$AgeRange <- "Adult" 
train$AgeRange[train$Age > 65] <- "Senior"
train$AgeRange[train$Age < 18 ] <- "Child"
prop.table(table(train$AgeRange, train$Survived), 1)

# How does class influence survival?
summary(train$Pclass)
prop.table(table(train$Pclass, train$Survived), 1)

aggregate(Survived ~ Wealth + AgeRange + Sex, data=train, FUN=length)
aggregate(Survived ~ Pclass + AgeRange + Sex, data=train, FUN=function(x){sum(x)/length(x)})

# How does title influence survival?
# Template provided by Trevor Stephens
train$Name <- as.character(train$Name) # Convert to string
train$Title <- sapply(train$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
train$Title <- sub(' ', '', train$Title)
table(train$Title)

# Combine similar titles
train$Title[train$Title %in% c('Ms', 'Mlle', 'Miss')] <- 'Miss'
train$Title[train$Title %in% c('Mme', 'Mrs')] <- 'Mrs'
train$Title[train$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
train$Title[train$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
train$Title <- factor(train$Title)
prop.table(table(train$Title, train$Survived),1)

#create data frame
test$Survived <- rep(0, 418)
test$Survived[test$Sex == 'female' & test$Pclass != 3] <- 1
result <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(result, file = "result.csv", row.names = FALSE)