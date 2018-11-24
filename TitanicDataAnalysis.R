#Load raw data
train <- read.csv("E:\\STUDY\\Kaggle\\titanic\\train.csv", header = TRUE)
test <- read.csv("E:\\STUDY\\Kaggle\\titanic\\test.csv", header = TRUE)

#add survived column to test table
test.survived <- data.frame(Survived = rep("None",nrow(test)), test[,])

data.combined <- rbind(train, test.survived)

str(data.combined)

data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)

table(data.combined$Survived)
table(data.combined$Pclass)

library(ggplot2)

#Hypothesis- Rich people survival rate more

train$Pclass <- as.factor(train$Pclass)

ggplot(train , aes(x = Pclass , fill = factor(Survived)))+
  geom_bar(width = 0.5)+
  xlab("Pclass")+
  ylab("Survived")+
  labs(fill="Survived")

head(as.character(train$Name))
length(unique(as.character(data.combined$Name)))
dup.name <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

data.combined[which(data.combined$Name %in% dup.name),]

library(stringr)

misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

males <- data.combined[which(data.combined$Sex == "male"),]
males[1:5,]


extractTitle <- function(name){
  name <- as.character(name)
  
  if(length(grep("Miss.",name))>0){
    return("Miss")
  }
  else if(length(grep("Master.",name))>0){
    return("Master.")
  }
  else if(length(grep("Mrs.",name))>0){
    return("Mrs.")
  }
  else if(length(grep("Mr.",name))>0){
    return("Mr.")
  }
  else{
    return("Other")
  }
}

titles <- NULL
for(i in 1:nrow(data.combined)){
  titles <- c(titles,extractTitle(data.combined[i,"Name"]))
}
data.combined$Title <- as.factor(titles)

ggplot(data.combined[1:891,] , aes(x = Title , fill = factor(Survived)))+
  geom_bar(width = 0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total count")+
  labs(fill="Survived")

#video 2

table(data.combined$Sex)

ggplot(data.combined[1:891,] , aes(x = Sex , fill = factor(Survived)))+
  geom_bar(width = 0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Sex")+
  ylab("Total count")+
  labs(fill="Survived")

summary(data.combined$Age)
summary(data.combined[1:891,"Age"])

ggplot(data.combined[1:891,] , aes(x = Age , fill = factor(Survived)))+
  facet_wrap(~Sex+Pclass)+
  geom_histogram(binwidth = 10)
  ggtitle("Age")+
  xlab("Age")+
  ylab("Total count")
  
boys <- data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age)

misses <- data.combined[which(data.combined$Title == "Miss"),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",] , aes(x = Age , fill = factor(Survived)))+
  facet_wrap(~Pclass)+
  geom_histogram(binwidth = 5)
  ggtitle("Age")+
  xlab("Age")+
  ylab("Total count")
  
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

summary(data.combined$SibSp)

length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) + 
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

data.combined$Parch <- as.factor(data.combined$Parch)

temp.sibsp <- c(train$SibSp,test$SibSp)
temp.parch <- c(train$Parch,test$Parch)

data.combined$FamilySize <- as.factor(temp.parch+temp.sibsp+1)


str(data.combined$Ticket)
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(ticket.first.char)

data.combined$Ticket <- as.factor(ticket.first.char)

ggplot(data.combined[1:891,], aes(x = Ticket, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = Ticket, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = Ticket, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) + 
  ggtitle("Pclass, Title") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")

summary(data.combined$Fare)

length(unique(data.combined$Fare))


# Can't make fare a factor, treat as numeric & visualize with histogram
ggplot(data.combined, aes(x = Fare)) +
 geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)

ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + Title) + 
  ggtitle("Pclass, Title") +
  xlab("fare") +
  ylab("Total Count") +
  ylim(0,50) + 
  labs(fill = "Survived")

str(data.combined$Cabin)


# Cabin really isn't a factor, make a string and the display first 100
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

# Replace empty cabins with a "U"
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]


# Take a look at just the first char as a factor
cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)

# Add to combined data set and plot 
data.combined$cabin.first.char <- cabin.first.char

# High level plot
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")

# Could have some predictive power, drill in
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by cabin.first.char") +
  xlab("Pclass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# Does this feature improve upon pclass + title?
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")


# What about folks with multiple cabins?
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")


# Does survivability depend on where you got onboard the Titanic?
str(data.combined$Embarked)
levels(data.combined$Embarked)


# Plot data for analysis
ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() +y
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
  

  library(randomForest)
  
  
  rf.train.1 <- data.combined[1:891, c("Pclass", "Title")]
  rf.label <- as.factor(train$Survived)

  set.seed(1234)
  rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
  rf.1
  varImpPlot(rf.1)
  
  rf.train.2 <- data.combined[1:891, c("Pclass", "Title", "FamilySize")]

  set.seed(1234)
  rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
  rf.2
  varImpPlot(rf.2)
  
  # Subset our test records and features
  test.submit.df <- data.combined[892:1309, c("Pclass", "Title", "FamilySize")]
  
  # Make predictions
  rf.2.preds <- predict(rf.2, test.submit.df)
  table(rf.2.preds)
  
  # Write out a CSV file for submission to Kaggle
  submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.2.preds)
  
  write.csv(submit.df, file = "E:\\STUDY\\Kaggle\\titanic\\submit.csv", row.names = FALSE)