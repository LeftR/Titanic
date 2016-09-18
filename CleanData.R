
### Missing column Survived on test data
test$Survived <- NA

test$DataSetName <- "Test"

train$DataSetName <- "Train"

### Connect trai and test data 
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
train$Surname <- gsub('(\\,.*)', "", train$Name)

Surename <- train[c("Surname","Ticket","")]

Surename.group <- Surename %>% group_by(Ticket) %>% tally()

