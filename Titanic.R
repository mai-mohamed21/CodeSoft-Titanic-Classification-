#libraries
library(readr)
library(rpart)
library(rpart.plot)
library(caret)
library(ggplot2)
library(plotly)
Titanic <- read.csv("E:/Titanic-Dataset.csv")
Titanic$Survived <- as.factor(Titanic$Survived)
Titanic$Age[is.na(Titanic$Age)] <- median(Titanic$Age, na.rm = TRUE)
Titanic <- Titanic[!is.na(Titanic$Embarked), ]
Titanic$FamilySize <- Titanic$SibSp + Titanic$Parch + 1
Titanic$Pclass <- as.factor(Titanic$Pclass)
Titanic$Sex <- as.factor(Titanic$Sex)
Titanic$Embarked <- as.factor(Titanic$Embarked)
#spli the data
set.seed(123)
train_index <- createDataPartition(Titanic$Survived, p = 0.8, list = FALSE)
train_data <- Titanic[train_index, ]
test_data <- Titanic[-train_index, ]
#decision tree model
model <- rpart(Survived ~ Pclass + Sex + Age + Fare + Embarked + FamilySize,
               data = train_data,
               method = "class",
               control = rpart.control(minsplit = 20, cp = 0.01))

rpart.plot(model, type = 3, extra = 104, fallen.leaves = TRUE,
           main = "Decision Tree for Titanic Survival Prediction",
           box.palette = list("lightcoral", "lightgreen"),
           shadow.col = "gray")
#test prediction
predictions <- predict(model, test_data, type = "class")

test_data$Prediction <- predictions

ggplot_plot <- ggplot(test_data, aes(x = Prediction, fill = Prediction)) +
  geom_bar() +
  labs(title = "Distribution of Predicted Survival",
       x = "Predicted Survival",
       y = "Count",
       fill = "Predicted Survival") +
  theme_minimal() +
  scale_fill_manual(values = c("lightsalmon", "lightsteelblue"))

plotly_plot <- ggplotly(ggplot_plot)

#display plot
plotly_plot

confusion_matrix <- confusionMatrix(predictions, test_data$Survived)
print(confusion_matrix)