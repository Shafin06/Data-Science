install.packages(c("dplyr", "ggplot2", "tidyr"))
library(dplyr)
library(ggplot2)
library(tidyr)

url <- "https://raw.githubusercontent.com/Shafin06/Data-Science/61570c317223692a79b9186057c964b6dfbef40e/titanic.csv"
titanic <- read.csv(url, stringsAsFactors = FALSE)


head(titanic)


str(titanic)
summary(titanic)


colnames(titanic)


colSums(is.na(titanic) | titanic == "")


titanic <- distinct(titanic)


titanic$Age <- ifelse(is.na(titanic$Age), median(titanic$Age, na.rm = TRUE), titanic$Age)



titanic$Embarked[is.na(titanic$Embarked) | titanic$Embarked == ""] <- 
  names(sort(table(titanic$Embarked), decreasing = TRUE))[1]




titanic <- titanic %>%
  mutate(FamilySize = SibSp + Parch + 1)


titanic <- titanic %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked, FamilySize)




Q1 <- quantile(titanic$Fare, 0.25, na.rm = TRUE)
Q3 <- quantile(titanic$Fare, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

titanic$Fare <- ifelse(titanic$Fare < lower_bound, lower_bound,
                       ifelse(titanic$Fare > upper_bound, upper_bound, titanic$Fare))

 



titanic %>%
  summarise(
    mean_age = mean(Age, na.rm = TRUE),
    median_age = median(Age, na.rm = TRUE),
    sd_age = sd(Age, na.rm = TRUE),
    mean_fare = mean(Fare, na.rm = TRUE),
    median_fare = median(Fare, na.rm = TRUE),
    sd_fare = sd(Fare, na.rm = TRUE)
  )

ggplot(titanic, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Passenger Ages", x = "Age", y = "Count")

ggplot(titanic, aes(x = Fare)) +
  geom_histogram(binwidth = 10, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Passenger Fares", x = "Fare", y = "Count")

ggplot(titanic, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = "fill") +
  labs(title = "Survival Rate by Passenger Class", x = "Passenger Class", y = "Proportion", fill = "Survived")

ggplot(titanic, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  labs(title = "Survival Rate by Sex", x = "Sex", y = "Proportion", fill = "Survived")

ggplot(titanic, aes(x = Age, y = Fare, color = factor(Survived))) +
  geom_point(alpha = 0.6) +
  labs(title = "Age vs Fare Colored by Survival", x = "Age", y = "Fare", color = "Survived")

ggplot(titanic, aes(x = FamilySize, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  labs(title = "Survival Rate by Family Size", x = "Family Size", y = "Proportion", fill = "Survived")
