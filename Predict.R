train <- full[full$DataSetName=='Train',]
test <- full[full$DataSetName=='Test',]

# Set a random seed
set.seed(356)

sapply(train, function(x) sum(is.na(x)))

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title +Deck+FsizeD +CabinPos + QuotedName, 
                         data = train, ntree=350)

print(rf_model)

# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)


importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()


# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'titanic.csv', row.names = F)



#########################3333



fit.cf<-cforest(Survived~Pclass + Sex + Age + SibSp + Parch + 
                  Fare + Embarked + Title +Deck+CabinPos + FamilyID + Child+Mother,data=train,
                controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit.cf, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)


submit$Survived2<- solution$Survived

submit$SurvivedF <- (((as.numeric(submit$Survived2)-1)*((0.43+0.5)/2)+0.5)) * submit$Survived

submit$Survived2 <- NULL
submit$Survived <- submit$SurvivedF
submit$SurvivedF <- NULL
submit$Survived <- as.numeric (submit$Survived>0.5)
View(submit)
table(submit)
write.csv(submit, file = "TitanicConditionalforestsSub.csv", row.names = FALSE)




##################################33

library(arules)

factor_vars <- names(train)

train[factor_vars] <- lapply(train[factor_vars], function(x) as.factor(x))


rules <- apriori(train, parameter = list(minlen=2, supp=0.2,conf=0.90), 
                 appearance = list(rhs=c("Survived=0","Survived=1"),
                                   default="lhs"),
                                  control = list(verbose=F))

quality(rules) <- round(quality(rules), digits = 3)

rules.sorted <- sort(rules, by="lift")

    #inspect(rules.sorted)


subset.matrix<- is.subset(rules.sorted,rules.sorted)

subset.matrix[lower.tri(subset.matrix,diag = T)]<- NA

redundant <- colSums(subset.matrix,na.rm = T)>= 1

rules.pruned <- rules.sorted[!redundant]



inspect(rules.pruned)

library(arulesViz)
plot(rules.pruned,interactive = T)

plot(rules.pruned,method="grouped")

plot(rules.pruned,method="graph")

plot(rules.pruned,method="paracoord", control=list(reorder=TRUE))


