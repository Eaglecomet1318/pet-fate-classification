# DATA LINK: https://www.kaggle.com/aaronschlegel/austin-animal-center-shelter-outcomes-and

# READING FILE IN
library(dplyr)
bestia <- read.csv('../Documents/pets.csv') # load data in
str(bestia)
names(bestia)


# CLEANING DATA
animal<-bestia[1:21000,]
head(animal)


## Filter out NULL or empty cell values or any other values that hinder the model
animal <-filter(animal,outcome_type!='Transfer')
animal <-filter(animal,outcome_subtype!='Barn')
animal <-filter(animal,outcome_subtype!='Possible Theft')
animal <-filter(animal,animal_type!='Other')
animal <-filter(animal,animal_type!='Livestock')
animal <-filter(animal,animal_type!='Bird')
animal <-filter(animal,sex_upon_outcome != 'Unknown')
animal <-filter(animal,sex_upon_outcome != 'NULL')

nrow(animal)


## Convert Possible Fates to Binary Factors (0 = Alive, 1 = Dead)
lookup <- c("Adoption" = 0, "Died" = 1, "Missing" = 0, "Relocate" = 0, "Return to Owner" = 0,"Disposal" = 1, "Euthanasia" = 1, "Rto-Adopt" = 0)
animal$fate <- lookup[animal$outcome_type]


## Remove unwanted column frames
animal <- subset(animal, select = -c(1,2,4,5,6,7,8,9,11))
str(animal)

# DIVISION INTO TRAINING AND TESTING DATA
set.seed(2017) # to induce reproducibility
  data_Set <- sample(nrow(animal), nrow(animal)*0.75, replace=FALSE)
train <- animal[data_Set,] 
exam <- animal[-data_Set,]



# Logistic regression : ALGORITHM #1
aniglm = glm(fate~.,data=train, family=binomial)
summary(aniglm)

fortunas <- predict(aniglm, newdata=exam, type="response")
oracle <- ifelse(fortunas>0.5, 1, 0)
table(oracle, exam$fate)

mean(oracle==exam$fate) #0.73942...
