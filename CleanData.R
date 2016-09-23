
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

#Combining the Family Surname with the Family Size will set up a new feature
full$Surname <- sapply(as.character( full$Name), FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
full$FamilyID <- paste(as.character(full$FamilySize), full$Surname, sep="")
full$FamilyID[full$FamilySize <= 2] <- 'Small'
famIDs <- data.frame(table(full$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
full$FamilyID[full$FamilyID %in% famIDs$Var1] <- 'Small'
full$FamilyID <- factor(full$FamilyID)
table(full$FamilyID)

<<<<<<< HEAD
Surename.group <- Surename %>% group_by(Ticket) %>% tally()
=======

Surename <- full[c("Surname","Ticket")]

Surename.group <- Surename %>% group_by("Ticket") 

full$Fsize <- full$SibSp + full$Parch + 1


full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)

full$FsizeD <- factor(full$FsizeD)

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






full$Deck<-factor(sapply(as.character(full$Cabin), function(x) strsplit(x, NULL)[[1]][1]))

full$Child[full$Age < 10] <- 'Child'
full$Child[full$Age >= 10] <- 'Adult'

full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

#Extract Cabin Num from Cabin 
full$CabinNum<-sapply(as.character(full$Cabin),function(x) strsplit(x,'[A-Z]')[[1]][2])
full$CabinNum<-as.numeric(full$CabinNum)
full$CabinPos<-NA

#Categorize 1-50 as Front, 50-100 as Middle, >100 as End
full$CabinPos[full$CabinNum<50]<-'Front'
full$CabinPos[full$CabinNum>=50 & full$CabinNum<100]<-'Middle'
full$CabinPos[full$CabinNum>=100]<-'End'
#full<-full[!is.na(full$CabinNum),]
full$CabinPos<-factor(full$CabinPos)

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','FsizeD','Deck','Child', 'Mother','CabinPos')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Surname','Survived','FsizeD','FamilyID')], method='rf') 
mice_output <- complete(mice_mod)

# Plot age distributions
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

# Replace Age variable from the mice model.
full$Age <- mice_output$Age
full$Deck <- mice_output$Deck
full$Child <- mice_output$Child
full$CabinPos <- mice_output$CabinPos


# Show new number of missing Age values
sum(is.na(full$Age))

sum(is.na(full$Deck))
>>>>>>> e3d20c5548a4c50027d0a74cbd678d2653fd6f18

