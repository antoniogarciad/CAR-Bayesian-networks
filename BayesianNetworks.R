
#Bayesian networks architectures: naive Bayes, semi-naive Bayes, tree-augmented naive Bayes

library(bnclassify)

#dataset
dataset = read.csv("dataset/featuresSelected5.csv", header = TRUE)
#Deleting na values
row.has.na <- apply(dataset, 1, function(x){any(is.na(x))})
dataset <- dataset[!row.has.na,]
#Converting multiple numeric variables to factor
dataset[sapply(dataset, is.numeric)] <- lapply(dataset[sapply(dataset, is.numeric)], as.factor)


#Bayesian network structures 
tn <- tan_cl('Class', dataset)

#Network parameters
tn <- lp(tn, dataset, smooth = 1) # Learn parameters

#Predicting with model
p = predict(tn, dataset) # Classify the entire data set
    
#Generating confusion matrix
cm <- table(predicted=p, true=dataset$Class)

#Confusion matrix accuracy
sum(cm * diag(1, nrow(cm), ncol(cm))) / sum(cm)

#Accuracy
bnclassify:::accuracy(p, dataset$Class)

