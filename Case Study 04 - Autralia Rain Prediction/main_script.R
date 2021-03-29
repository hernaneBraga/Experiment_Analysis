library(caret)

fulldata <- read.csv("data/train.csv", header = TRUE)
fulldata_test <- read.csv("data/test.csv", header = TRUE)

## Necessário Remover Sydney

loc <- unique(fulldata$Location)
loc <- loc[!(loc == loc[4])]

##


a <- 1
rep <- 30
inst <- data.frame(
  "instancia" = c(1),
  "localidade" = c(1),
  "modelo" = c(1),
  "repeticao" = c(1),
  "acuracia" = c(1),
  "sensibilidade" = c(1),
  "especificidade" = c(1)
)
inst_line <- 1

for(i in loc){
  
  localdata <- fulldata[fulldata$Location == i,]
  localdata_test <- fulldata_test[fulldata_test$Location == i,]
  
  X_train <- localdata[,2:21]
  Y_train <- as.factor(localdata[,22])
  
  X_test <- localdata_test[,2:21]
  Y_test <- as.factor(localdata_test[,22])
  
  for(j in c(1:rep)){
    
    print(paste('Instância =',i,'modelo = RandomForest','repetição = ',j))
    
    # Modelo Random Forest
    randomf <- train(X_train, Y_train, method = 'ranger',
                     preProcess = 'scale',
                     trControl = trainControl(method = "none"),
                     tuneGrid = data.frame(mtry = 11,splitrule = 'extratrees', min.node.size = 1))
    yhat <- predict(randomf,X_test)
    cm <- confusionMatrix(data = yhat, reference = Y_test)
    acc <- cm$overall['Accuracy']
    sst <- cm$byClass['Sensitivity']
    spc <- cm$byClass['Specificity']
    
    inst[inst_line,] <- list(a,i,'RandomForest',j,acc,sst,spc)
    inst_line <- inst_line + 1
    
  }
  
  for(j in c(1:rep)){
    
    print(paste('Instância =',i,'modelo = MLP','repetição = ',j))
    
    # Multi Layer Perceptron
    mlperceptron <- train(X_train, Y_train, method = 'mlp', 
                          preProcess = 'scale',
                          trControl = trainControl(method = "none"),
                          tuneGrid = data.frame(size = 5))
    
    yhat <- predict(mlperceptron,X_test)
    cm <- confusionMatrix(data = yhat, reference = Y_test)
    acc <- cm$overall['Accuracy']
    sst <- cm$byClass['Sensitivity']
    spc <- cm$byClass['Specificity']
    
    inst[inst_line,] <- list(a,i,'MLP',j,acc,sst,spc)
    inst_line <- inst_line + 1
    
  }
  
  for(j in c(1:rep)){
    
    print(paste('Instância =',i,'modelo = SVM','repetição = ',j))
    
    # SVM Kernel Polinomial
    svmrbf <- train(X_train, Y_train, method = 'lssvmRadial',
                    preProcess = 'scale',
                    trControl = trainControl(method = "none"),
                    tuneGrid = data.frame(sigma = 0.01443098, tau = 0.25))
    
    yhat <- predict(svmrbf,X_test)
    cm <- confusionMatrix(data = yhat, reference = Y_test)
    acc <- cm$overall['Accuracy']
    sst <- cm$byClass['Sensitivity']
    spc <- cm$byClass['Specificity']
    
    inst[inst_line,] <- list(a,i,'SVM',j,acc,sst,spc)
    inst_line <- inst_line + 1
    
  }
  
  for(j in c(1:rep)){
    
    print(paste('Instância =',i,'modelo = Fuzzy','repetição = ',j))
    
    # Fuzzy Inference System
    fuzzy <- train(X_train, Y_train, method = 'FRBCS.CHI',
                   preProcess = 'scale',
                   trControl = trainControl(method = "none"),
                   tuneGrid = data.frame(num.labels = 7,type.mf = 'GAUSSIAN'))
    
    yhat <- predict(fuzzy,X_test)
    cm <- confusionMatrix(data = yhat, reference = Y_test)
    acc <- cm$overall['Accuracy']
    sst <- cm$byClass['Sensitivity']
    spc <- cm$byClass['Specificity']
    
    inst[inst_line,] <- list(a,i,'Fuzzy',j,acc,sst,spc)
    inst_line <- inst_line + 1
    
  }
  
  a <- a+1
  
}