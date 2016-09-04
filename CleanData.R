
test$Survived <- NA

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


table(full$Sex, full$Title)