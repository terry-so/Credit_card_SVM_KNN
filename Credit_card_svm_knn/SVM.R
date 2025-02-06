
library('kernlab')

set.seed(1)


data <- read.table('data/credit_card_data.txt', header = FALSE)


df1 <- data.frame(C = numeric(), Accuracy = numeric())


for (i in 10^(-10:10)){
  invisible(capture.output({
  model <- ksvm(as.matrix(data[,1:10]), as.factor(data[,11]), type="C-svc", kernel = "vanilladot", C=i, scaled=TRUE)
  }))
  a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
  
  a0 <- -model@b
  
  pred <- predict(model,data[,1:10])
  
  pred_accuracy <- sum(pred == data[,11]) / nrow(data)
  
  df1 <- rbind(df1, data.frame(C=i, Accuracy = pred_accuracy))
  
  }

print(df1, row.names = FALSE)


df1 <- data.frame(C = numeric(), Accuracy = numeric())


for (i in 10^(-10:10)){
  model <- ksvm(as.matrix(data[,1:10]), as.factor(data[,11]), type="C-svc", kernel = "polydot", C=i, scaled=TRUE)
  
  a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
  
  a0 <- -model@b
  
  pred <- predict(model,data[,1:10])
  
  pred_accuracy <- sum(pred == data[,11]) / nrow(data)
  
  obj <- model@obj
  
  df1 <- rbind(df1, data.frame(C=i, Accuracy = pred_accuracy))
  
}

print(df1, row.names = FALSE)



