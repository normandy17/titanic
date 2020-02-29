setwd("C:/ML/Titanic")

# Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a "Survived" variable to the test set to allow for combining data sets
test.Survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

# Combine data sets
data.combined <- rbind(train, test.Survived)

# A bit about R data types (e.g., factors)
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)


# Take a look at gross survival rates
table(data.combined$Survived)


# Distribution across classes
table(data.combined$Pclass)


# Load up ggplot2 packAge to use for visualizations
library(ggplot2)


# Hypothesis - Rich folks Survived at a higer rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 


# Examine the first few Names in the training data set
head(as.character(train$Name))

# How many unique Names are there across both train & test?
length(unique(as.character(data.combined$Name)))


# Two duplicate Names, take a closer look
# First, get the duplicate Names and store them as a vector
dup.Names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

# Next, take a look at the records in the combined data set
data.combined[which(data.combined$Name %in% dup.Names),]


# What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)

# Any correlation with other variables (e.g., SibSp)?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]


# Hypothesis - Name titles correlate with Age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")), ]
mrses[1:5,]

# Check out males to see if pattern continues
males <- data.combined[which(data.combined$Sex == "male"), ]
males[1:5,]


# Expand upon the realtionship between `Survived` and `Pclass` by adding the new `Title` variable to the
# data set and then explore a potential 3-dimensional relationship.

# Create a utility function to help with title extraction
extractTitle <- function(Name) {
  Name <- as.character(Name)
  
  if (length(grep("Miss.", Name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", Name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", Name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", Name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}
data.combined$title <- as.factor(titles)

# Since we only have Survived lables for the train set, only use the
# first 891 rows
ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")


# What's the distribution of females to males across train & test?
table(data.combined$Sex)


# Visualize the 3-way relationship of Sex, Pclass, and survival, compare to analysis of title
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")


# OK, Age and Sex seem pretty important as derived from analysis of title, let's take a closer 
# look at the distibutions of Age over entire data set
summary(data.combined$Age)
summary(data.combined[1:891,"Age"])

# Just to be thorough, take a look at survival rates broken out by Sex, Pclass, and Age
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")


# Validate that "Master." is a good proxy for male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)


# We know that "Miss." is more complicated, let's examine further
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None" & !is.na(misses$Age),], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass") + 
  xlab("Age") +
  ylab("Total Count")


# OK, appears female children may have different survival rate, 
# could be a candidate for feature engineering later
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))


# Move on to the SibSp variable, summarize the variable
summary(data.combined$SibSp)


# Can we treat as a factor?
length(unique(data.combined$SibSp))


data.combined$SibSp <- as.factor(data.combined$SibSp)


# We believe title is predictive. Visualize survival reates by SibSp, Pclass, and title
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Treat the Parch vaiable as a factor and visualize
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("Parch") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Let's try some feature engineering. What about creating a family size feature?
temp.SibSp <- c(train$SibSp, test$SibSp)
temp.Parch <- c(train$Parch, test$Parch)
data.combined$family.size <- as.factor(temp.SibSp + temp.Parch + 1)


# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
  

# Take a look at the Ticket variable
str(data.combined$Ticket)


# Based on the huge number of levels Ticket really isn't a factor variable it is a string. 
# Convert it and display first 20
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]


# There's no immediately apparent structure in the data, let's see if we can find some.
# We'll start with taking a look at just the first char for each
Ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(Ticket.first.char)


# OK, we can make a factor for analysis purposes and visualize
data.combined$Ticket.first.char <- as.factor(Ticket.first.char)

# First, a high-level plot of the data
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by Ticket.first.char") +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

# Ticket seems like it might be predictive, drill down a bit
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Lastly, see if we get a pattern when using combination of Pclass & title
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")




# Next up - the Fares Titanic passengers paid
summary(data.combined$Fare)
length(unique(data.combined$Fare))
str(data.combined$Fare)



# Can't make Fare a factor, treat as numeric & visualize with histogram
ggplot(data.combined, aes(x = Fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)


# Let's check to see if Fare has predictive power
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50) + 
  labs(fill = "Survived")




# Analysis of the Cabin variable
str(data.combined$Cabin)


# Cabin really isn't a factor, make a string and the display first 100
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]


# Replace empty Cabins with a "U"
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]


# Take a look at just the first char as a factor
Cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(Cabin.first.char)
levels(Cabin.first.char)


# Add to combined data set and plot 
data.combined$Cabin.first.char <- Cabin.first.char

# High level plot
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by Cabin.first.char") +
  xlab("Cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")

# Could have some predictive power, drill in
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by Cabin.first.char") +
  xlab("Pclass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# Does this feature improve upon Pclass + title?
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")


# What about folks with multiple Cabins?
data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = Cabin.multiple, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")


# Does survivability depend on where you got onboard the Titanic?
str(data.combined$Embarked)
levels(data.combined$Embarked)


# Plot data for analysis
ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


#### Exploratory modeling

library(randomForest)

rf.train.1 <- data.combined[1:891, c("Pclass","title")] 
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x= rf.train.1, y=rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

rf.train.2 <- data.combined[1:891, c("Pclass","title","SibSp")] 
set.seed(1234)
rf.2 <- randomForest(x= rf.train.2, y=rf.label, importance = TRUE, ntree = 1000)
rf.2d
varImpPlot(rf.2)

rf.train.3 <- data.combined[1:891, c("Pclass","title","Parch")] 
set.seed(1234)
rf.3 <- randomForest(x= rf.train.3, y=rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

rf.train.4 <- data.combined[1:891, c("Pclass","title","Parch","SibSp")] 
set.seed(1234)
rf.4 <- randomForest(x= rf.train.4, y=rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)

rf.train.5 <- data.combined[1:891, c("Pclass","title","family.size")] 
set.seed(1234)
rf.5 <- randomForest(x= rf.train.5, y=rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

rf.train.6 <- data.combined[1:891, c("Pclass","title","Parch","SibSp","family.size")] 
set.seed(1234)
rf.6 <- randomForest(x= rf.train.6, y=rf.label, importance = TRUE, ntree = 1000)
rf.6 
varImpPlot(rf.6)                              

# since Pclass, Title, and family size holds more weightage
rf.train.5 <- data.combined[1:891, c("Pclass","title","family.size")] 
set.seed(1234)
rf.5 <- randomForest(x= rf.train.5, y=rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

test.submit.df <-data.combined[892:1309, c("Pclass","title","family.size")]
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

submit.df <- data.frame(PassengerId = rep(892:1309), Survived=rf.5.preds)
write.csv(submit.df, file= "TIC_RF_20191022_1.csv", row.names= FALSE)

##
##Cross Validation##
##

library(caret)
library(doSNOW)

#10 fold cross validation repeated 10 times

set.seed(2345)
cv.10.folds <- createMultiFolds(rf.label, k=10, times =10)

table(rf.label)

table(rf.label[cv.10.folds[[33]]])
 
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.folds)
 
cl <- makeCluster(6, type="SOCK")
registerDoSNOW(cl)

set.seed(34324)
rf.5.cv1 <- train(x=rf.train.5, y=rf.label, method= "rf", tunelength=3, ntree=1000, trControl=ctrl.1)

stopCluster(cl)

rf.5.cv1

#5 fold cross validation repeated 10 times

set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k=5, times =10)

table(rf.label)

table(rf.label[cv.5.folds[[33]]])

ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10, index = cv.5.folds)

cl <- makeCluster(10, type="SOCK")
registerDoSNOW(cl)

set.seed(89472)
rf.5.cv2 <- train(x=rf.train.5, y=rf.label, method= "rf", tunelength=3, ntree=1000, trControl=ctrl.2)

stopCluster(cl)

rf.5.cv2

#3 fold cross validation repeated 10 times

set.seed(6013)
cv.3.folds <- createMultiFolds(rf.label, k=3, times =10)

table(rf.label)

table(rf.label[cv.10.folds[[33]]])

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.3.folds)

cl <- makeCluster(10, type="SOCK")
registerDoSNOW(cl)

set.seed(78945)
rf.5.cv3 <- train(x=rf.train.5, y=rf.label, method= "rf", tunelength=3, ntree=1000, trControl=ctrl.3)

warnings()
stopCluster(cl)

rf.5.cv3


#rpart and  rpart.plot

?
library(rpart)
library(rpart.plot)

rpart.cv <- function(seed,training,labels,ctrl)
{
  cl <- makeCluster(10, type="SOCK")
  registerDoSNOW(cl)
  
  
set.seed(seed)
rpart.cv <- train(x=training, y=labels, method="rpart", tuneLength=30, trControl=ctrl)

stopCluster(cl)

return(rpart.cv)

}

features <- c("Pclass", "title", "family.size")
rpart.train.1 <- data.combined[1:891, features]

rpart.1.cv.1 <- rpart.cv(9622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

#plot
prp(rpart.1.cv.1$finalModel, type=0, extra=1, under=TRUE)


##########


table(data.combined$title)
data.combined[1:25, "Name"]

#word split
name.splits <-  str_split(data.combined$Name, ",") 
name.splits[1]
last.names<- sapply(name.splits, "[", 1)
last.names[1:20]
data.combined$last.name <- last.names

name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles)

data.combined[which(titles == "the"),]


##wich(titles == "Dona." | titles =="the")
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles=="Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(titles)
table(data.combined$title)

data.combined$new.title <- as.factor(titles)

ggplot(data.combined[1:891,], aes(x = new.title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survival rates by new title") 
 
indexes <- which(data.combined$new.title == "Lady.")
data.combined$new.title[indexes] <- "Mrs."

indexes <- which(data.combined$new.title == "Dr." |
                   data.combined$new.title == "Rev." |
                   data.combined$new.title == "Sir." |
                   data.combined$new.title == "Officer")
data.combined$new.title[indexes] <- "Mr."
table(data.combined$new.title)
ggplot(data.combined[1:891,], aes(x = new.title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survival rates by new title") 

# new rpart with new.title

features <- c("Pclass", "new.title", "family.size")
rpart.train.2 <- data.combined[1:891, features]

rpart.2.cv.1 <- rpart.cv(9622, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

#plot
prp(rpart.2.cv.1$finalModel, type=0, extra=1, under=TRUE)

indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)
first.mr.df[first.mr.df$Sex== "female",] 

indexes <- which(data.combined$new.title == "Mr." & data.combined$Sex == "female")
data.combined$new.title[indexes] <- "Mrs."

#any other slip up
length(which(data.combined$Sex == "female" &
               data.combined$new.title == "Mr."&
               data.combined$new.title == "Master"))

indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)

summary(first.mr.df[first.mr.df$Survived== "1",])
View(first.mr.df[first.mr.df$Survived== "1",])

#view select
indexes<- which(data.combined$Ticket== "PC 17755" |
                data.combined$Ticket== "PC 17611" |
                data.combined$Ticket== "113760"  )
View(data.combined[indexes,])


ggplot(first.mr.df, aes(x = Fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival rates 1st class MR.") 

#sorting per person ticket fare
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep (0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)

length(tickets)

for (i in 1:length(tickets))  {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "Fare"]/length(party.indexes)
  
  for (k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare

first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)


ggplot(first.mr.df[first.mr.df$Survived != "None", ], aes(x = ticket.party.size, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival rates 1st class MR. by ticket party size") 

ggplot(first.mr.df[first.mr.df$Survived != "None", ], aes(x = avg.fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival rates 1st class MR. by ticket party size") 

summary(data.combined$avg.fare)
data.combined[is.na(data.combined$avg.fare),]

indexes <- with(data.combined, which(Pclass== "3" & new.title=="Mr." &
                                       family.size=="1" & Ticket != "3701" ))
similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$avg.fare)
data.combined[is.na(avg.fare), "avg.fare"] <- 7.840

################################################################

preproc.data.combined <- data.combined[, c("ticket.party.size", "avg.fare")]
preproc <- preProcess(preproc.data.combined, method = c("center","scale"))

postproc.data.combined <- predict(preproc, preproc.data.combined)


cor(postproc.data.combined$ticket.party.size, postproc.data.combined$avg.fare)

indexes<- which(data.combined$Pclass=="1")
cor(postproc.data.combined$ticket.party.size[indexes], postproc.data.combined$avg.fare[indexes])

### New Model with new features
features <- c("Pclass", "new.title", "family.size", "ticket.party.size", "avg.fare")
rpart.train.3 <- data.combined[1:891, features]

rpart.3.cv.1 <- rpart.cv(9622, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

#plot
prp(rpart.3.cv.1$finalModel, type=0, extra=1, under=TRUE)

test.submit.df <- data.combined[892:1309, features ]

rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type="class")
table(rpart.3.preds)

submit.df <- data.frame(PassengerId = rep(892:1309), Survived=rpart.3.preds)

write.csv(submit.df, file= "TIC_RPART_20191024_1.csv", row.names = FALSE)


##Random Forest

features <- c("Pclass", "new.title", "ticket.party.size", "avg.fare")
rf.train.temp <- data.combined[1:891, features]
  
set.seed(1234)
rf.temp<- randomForest(x=rf.train.temp, y=rf.label, ntree=1000)
rf.temp

test.submit.df <- data.combined[892:1309, features ]
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

submit.df <- data.frame(PassengerId = rep(892:1309), Survived=rf.preds)

write.csv(submit.df, file= "TIC_RF_20191024_1.csv", row.names = FALSE)

#####################
#############
#########
#####
##Infotheo/mutinformation

library(infotheo)
table(rf.label)

mutinformation(rf.label, data.combined$Pclass[1:891])
mutinformation(rf.label, data.combined$Sex[1:891])
mutinformation(rf.label, data.combined$SibSp[1:891])
mutinformation(rf.label, data.combined$Parch[1:891])
mutinformation(rf.label, discretize(data.combined$Fare[1:891]))
mutinformation(rf.label, data.combined$Embarked[1:891])
mutinformation(rf.label, data.combined$title[1:891])
mutinformation(rf.label, data.combined$family.size[1:891])
mutinformation(rf.label, data.combined$Ticket.first.char[1:891])
mutinformation(rf.label, data.combined$Cabin.multiple[1:891])
mutinformation(rf.label, data.combined$new.title[1:891])
mutinformation(rf.label, data.combined$ticket.party.size[1:891])
mutinformation(rf.label, discretize(data.combined$avg.fare[1:891]))


#####################
############
#######
###Rtnese/


















