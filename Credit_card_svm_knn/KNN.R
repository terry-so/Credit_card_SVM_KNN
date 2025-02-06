

library('kknn')
credit_card_data <- read.table('data/credit_card_data.txt', header = FALSE)

set.seed(1)

m <- dim(credit_card_data)[1]

prediction_record <- rep(0,(m))

df <- data.frame(K =numeric(),Accuracy =  numeric())

for (k in 1:30){
  
  for (i in 1:m){
    
    train_data <- credit_card_data[-i, ]
    
    test_data <- credit_card_data[i, ]
    
    credit_card_kknn <- kknn(formula = V11~., train_data, test_data,k=k, scale=TRUE)
    
    prediction = fitted(credit_card_kknn)
    
    Binary.Prediction <- ifelse(prediction >= 0.5, 1, 0)
    
    prediction_record[i] <- Binary.Prediction
    
  }
  
  accuracy = sum((prediction_record) == credit_card_data[,11])/m
  df <- rbind(df, data.frame(K = k, Accuracy = accuracy))
  
}
print(df, row.names = FALSE)








i = sample(1:m, size = round(m/4), replace = FALSE, prob = rep(1/m, m))

train_data <- credit_card_data[-i, ]

test_data <- credit_card_data[i, ]

df <- data.frame(K =numeric(),Accuracy =  numeric())

for (k in 1:20){
  credit_card_kknn <- kknn(formula = V11~., train_data, test_data,k=k, scale=TRUE)
  prediction = fitted(credit_card_kknn)
  
  Binary.Prediction <- ifelse(prediction >= 0.5, 1, 0)
  
  
  confusion_matrix <- table(test_data$V11, Binary.Prediction)
  
  
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)


  df <- rbind(df, data.frame(K = k, Accuracy = accuracy))
  }

print(df, row.names = FALSE)


