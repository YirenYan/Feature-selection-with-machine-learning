
# The code is to output the feature selection set based on forward step algorithm(every time we add one feature that can contribute the most to our dataset)
# The algorithm stops when the cross validation average accuracy cannot be improved 
# Result is the training dataset with category as the class variable and all the other columns representing features
# In "result" dataset, every column is one feature
# The ML algorithms implementation is based on caret packages, so you can change whatever ML algorithm based on research goal
# This code has been used in the author's proceding papaers.


library(CombMSC)
library(caret)
comb <- c()
result.train1 <- result
avg_bar <- 0
accuracy_bar <- 0
record <- c()
selected <- c()
n_var <- 600
for (i in 1:n_var){
  len <- length(subsets(n_var-i+1,1,colnames(result.train1[,!(colnames(result.train1) == "category")])))
  for (j in 1:len){
    set <- result[c(selected, subsets(n_var-i+1,1,colnames(result.train1[,!(colnames(result.train1) == "category")]))[j,])]
    fit.svml <- train(set,
                      y_train,
                      method = "svmLinear",
                      tuneGrid = expand.grid(C = 0.1),
                      trControl = train.control1)
    avg <- mean(fit.svml$results$Accuracy)
    if (avg > avg_bar){
      comb <- c(i, j, avg)
      avg_bar <- avg
    }
  }
  select_var <- subsets(n_var-i+1,1,colnames(result.train1[,!(colnames(result.train1) == "category")]))[comb[2],]
  selected <- c(selected, select_var)
  record <- rbind(record, c(select_var, avg_bar))
  result.train1 <- result.train1[,!(colnames(result.train1)==select_var)]
  if (i!=1 && avg_bar <= accuracy_bar){
    break
  }
  accuracy_bar <- avg_bar
}

# output the list of the variable
record