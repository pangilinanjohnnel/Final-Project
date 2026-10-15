#Can labor force observations 
#be classified into Low, Medium, and High levels 
#using sex and age group as predictors?

library(dplyr)
library(tidyr)
library(caret)

#LOAD
path="C:/Users/Johnnel/Desktop/TIP folder/2nd year 1st sem/business intellegence/FINAL/LaborForce.csv"
labor <- read.csv(path, skip=1, header=TRUE, fill=TRUE)

#CHECK
head(labor)
str(labor)

#CLEAN
labor[labor == "."] <- NA
labor[3:23] <- lapply(labor[3:23], as.numeric)

#CHECKING DISTRIBUTION<- for description
#BY SEX
labor_sex <- labor[, c(1, 2, 10, 17)]

#BY AGE GROUP
labor_age <- labor[, c(1:9)]

#CHECK
head(labor_age)
head(labor_sex)

#DISTRIBUTION BY SEX
barplot(
  c(sum(labor_sex$Male.Total, na.rm=TRUE),
    sum(labor_sex$Female.Total, na.rm=TRUE)),
  names.arg=c("Male", "Female"),
  main="Total Labor by Sex",
  ylab="Labor Value")

#MALE: approximately~4 Million
#FEMALE: approximately ~2.8 Million

#DISTRIBUTION BY SEX-PERCENTAGE %
sex_totals <- c(
  sum(labor_sex$Male.Total, na.rm=TRUE),
  sum(labor_sex$Female.Total, na.rm=TRUE))

barplot(
  sex_totals / sum(sex_totals) * 100,
  names.arg=c("Male", "Female"),
  main="Labor Distribution by Sex",
  ylab="Percentage (%)",
  ylim=c(0, 100))

#MALE: 60%
#FEMALE: 40%

#DISTRIBUTION BY AGE GROUP
barplot(
  colSums(labor_age[, 4:9], na.rm=TRUE),
  names.arg=c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  main="Total Labor by Age Group",
  xlab="Age Group",
  ylab="Labor Value")

#top 3
#25-34: approximately ~2 Million
#35-44: approximately ~1.5 Million 
#45-54: approximately ~1.2 Million
#least
#65+: approximately ~250,000

#DISTRIBUTION BY AGE GROUP-PERCENTAGE %
age_totals <- colSums(labor_age[, 4:9], na.rm=TRUE)

barplot(
  age_totals / sum(age_totals) * 100,
  names.arg=c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  main="Labor Distribution by Age Group",
  xlab="Age Group",
  ylab="Percentage (%)",
  ylim=c(0, 100))

#top 3
#25-34:  approximately ~25%
#35-44:  approximately ~22%
#45-54:  approximately ~19%
#least
#65+:  approximately ~5%

#CLASSIFICATION PREPARATION
labor_class <- labor %>%
  filter(Month != "Annual") %>%
  select(Year, Month, 
         starts_with("Male."),
         starts_with("Female.")) %>%
  pivot_longer(
    cols = starts_with(c("Male.", "Female.")),
    names_to = "Group",
    values_to = "Labor_Value"
  ) %>%
  filter(!grepl("Total", Group)) %>%
  mutate(
    Sex = ifelse(grepl("^Male", Group), "Male", "Female"),
    Age_Group = sub("^(Male|Female)\\.", "", Group)
  ) %>%
  select(Year, Month, Sex, Age_Group, Labor_Value) %>%
  na.omit() %>%
  mutate(Level = factor(ntile(Labor_Value, 3),
                        labels = c("Low", "Medium", "High")))

#check labels

#demographics
table(labor_class$Sex)
table(labor_class$Age_Group)
table(labor_class$Level)#<-target class

#split
set.seed(417)
train_id <- createDataPartition(labor_class$Level, p = 0.8, list = FALSE)
train <- labor_class[train_id, ]
test <- labor_class[-train_id, ]

#model
tree_model <- rpart::rpart(
  Level ~ Sex + Age_Group,
  data = train,
  method = "class")

#pred-using test
pred <- predict(tree_model, test, type = "class")

#eval
confusionMatrix(pred, test$Level)

#eval summary:
#Acc: 76.92% - decent accuracy
#Kappa: 0.6538 - substantial agreement beyond chance
#Best predicted class: Low (Sensitivity = 89.42%)
#Hardest predicted class: Medium (Sensitivity = 66.35%)
#Conclusion: Overall reasonable performance,
#but struggles to distinguish Medium from Low and High.



#decision tree rules
library(rpart.plot)

png("Figure5_DecisionTree.png",
    width = 3000,
    height = 2000,
    res = 300)

rpart.plot(tree_model,
           type = 3,
           extra = 104,
           fallen.leaves = TRUE,
           cex = 1.3,
           main = "Decision Tree for Labor Force Classification")

dev.off()
shell.exec(getwd())
