#################
# ML QZ #1
#################


# Install 'remotes' packages to import 'palmerpenguins' dataset
install.packages("remotes")
# Install 'palmerpenguins' package
remotes::install_github("allisonhorst/palmerpenguins")

# Import required libraries
library(palmerpenguins)
library(tidyverse)
  
# Glimpse of the penguins dataset.
glimpse(penguins)
# 344 observations
# 8 vars

############################3
# EDA
############################3
summary(penguins)

table(penguins$year)
table(penguins$sex)
table(penguins$species)
table(penguins$island)
table(penguins$species, penguins$island)
# All penguins in Torgersen island is 'Adelie'
table(penguins$sex, penguins$island)

plot(penguins$bill_length_mm, 
     penguins$bill_depth_mm)


#####
# missing values
library(VIM)
aggr(penguins)

# Variable with the most missing values: sex
sum(is.na(penguins$bill_length_mm))
sum(is.na(penguins$bill_depth_mm))
sum(is.na(penguins$flipper_length_mm))
sum(is.na(penguins$body_mass_g))

# Filter out when missings in 'bill_length_mm'
penguins %>% 
  filter(!is.na(bill_length_mm)) -> penguins_new

# Check the missing values again
aggr(penguins_new)

# Missing value handling with mice library and check again
library(mice)

penguins_new<-mice(penguins_new, method='rf', seed=1234)
penguins_imputed<-complete(penguins_new, 1)

aggr(penguins_imputed)

table(penguins$sex)

table(penguins_imputed$sex)


plot(penguins_imputed$bill_length_mm, 
     penguins_imputed$bill_depth_mm)

penguins_imputed %>% names
penguins_imputed %>% 
  ggplot() +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species))


penguins_imputed %>% 
  group_by(sex) %>% 
  summarise(bill_len=mean(bill_length_mm),
            bill_dep=mean(bill_depth_mm),
            mass=mean(body_mass_g),
            flipper_len=mean(flipper_length_mm)
            )

penguins_imputed %>% 
  group_by(island) %>% 
  summarise(bill_len=mean(bill_length_mm),
            bill_dep=mean(bill_depth_mm),
            mass=mean(body_mass_g),
            flipper_len=mean(flipper_length_mm)
  )
  
penguins_imputed %>% 
  group_by(species) %>% 
  summarise(bill_len=mean(bill_length_mm),
            bill_dep=mean(bill_depth_mm),
            mass=mean(body_mass_g),
            flipper_len=mean(flipper_length_mm)
  )



############################
# Modeling

glimpse(penguins_imputed)
table(penguins$species)


# 1. Decision Tree
penguins_imputed %>% select(-c("island", "sex", "year")) -> train

library(rpart)
dt_model<-rpart(species~., data = train, method = "class")

summary(dt_model)


library(rpart.plot)
rpart.plot(dt_model, type=4, extra=100, box.palette ="-YlGnBl", branch.lty = 2)




predict(dt_model, penguins_imputed[104,])


?randomForest

# 2. Random Forest

library(randomForest)
rf_model <- randomForest(species~.,
                         data = train, 
                         mtry = 3,
                         ntree = 200)
rf_model

varImpPlot(rf_model)

# 3. Naive Bayes

library(naivebayes)
nb_model <- naive_bayes(species ~ ., data=train)
summary(nb_model)

table(train$species)


# 4. kNN

# Normalization

nor <- function(x) { (x - min(x)) / (max(x) - min(x)) }

train %>% mutate(bill_length_mm=nor(bill_length_mm),
                 bill_depth_mm=nor(bill_depth_mm),
                 flipper_length_mm=nor(flipper_length_mm),
                 body_mass_g=nor(body_mass_g)) %>% 
  select(-species)-> train_nor

head(train_nor)





library(class)

kn_model <- knn(train_nor, 
                train_nor[101:120,], 
                cl=train$species, k=13)
tab <- table(kn_model,train[101:120,"species"])
tab
penguins
save(penguins_imputed, file="penguins_imputed.RData")
save(penguins, file="penguins.RData")

######################
# Score & Prediction 

# create a train dataset
test <- train[seq(1,300,3),]

# Prediction by using trained models
pred_dt <- predict(dt_model, test, type='class')
pred_rf <- predict(rf_model, test, type='class')
pred_nb <- predict(nb_model, test, type='class')
pred_kn <- knn(train_nor, train_nor[seq(1,300,3),], cl=train$species, k=13)


data.frame(truth=train[seq(1,300,3),"species"],
           dt=pred_dt, 
           rf=pred_rf, 
           nb=pred_nb, 
           kn=pred_kn) %>% 
  mutate(dt=ifelse(dt==truth, 1, 0),
         rf=ifelse(rf==truth, 1, 0),
         nb=ifelse(nb==truth, 1, 0),
         kn=ifelse(kn==truth, 1, 0)) -> score

apply(score[-1], 2, sum)

score %>% 
  mutate(ts=dt+rf+nb+kn) %>% 
  mutate(low=ifelse(ts==1, 1, 0)) %>% 
  filter(low==1)

penguins_imputed[295,]


library(caret)
confusionMatrix(pred_dt, test$species)

