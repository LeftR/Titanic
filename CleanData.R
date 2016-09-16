
### Missing column Survived on test data
test$Survived <- NA

test$DataSetName <- "Test"

train$DataSetName <- "Train"

### Connect train and test data 
full <- rbind(train, test)

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

table(full$Sex, full$Title)


full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title == 'Jonkheer']    <- 'Mr' 
full$Title[full$Title == 'Capt']        <- 'Mr'
full$Title[full$Title == 'Don']        <- 'Mr'
full$Title[full$Title == 'Capt']        <- 'Mr'
full$Title[full$Title == 'Dona']        <- 'Mrs'
full$Title[full$Title == 'Col']     <- 'Mr'
full$Title[full$Title == 'Major']     <- 'Mr'
full$Title[full$Title == 'Lady']     <- 'Mrs'
full$Title[full$Title == 'Sir']     <- 'Mr'
full$Title[full$Title == 'the Countess']     <- 'Mrs'
full$Title[full$Title == 'Rev']     <- 'Mr'
full$Title[full$Title == 'Dr' & full$Sex == "female" ]     <- 'Mrs'
full$Title[full$Title == 'Dr' & full$Sex == "male" ]     <- 'Mr'


prop.table(table(full$Sex, full$Title),1)


full$Surname <- gsub('(\\,.*)', "", full$Name)


Surename <- full[c("Surname","Ticket")]

Surename.group <- Surename %>% group_by("Ticket") 




# Get rid of our missing passenger IDs
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()
full$Embarked[c(62, 830)] <- 'C'

ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)


# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Surname','Survived')], method='rf') 
mice_output <- complete(mice_mod)

# Plot age distributions
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

# Replace Age variable from the mice model.
full$Age <- mice_output$Age

# Show new number of missing Age values
sum(is.na(full$Age))
