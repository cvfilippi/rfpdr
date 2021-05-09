# browseURL("https://github.com/cvfilippi/rfpdr")

## References:
### http://r-statistics.co/Logistic-Regression-With-R.html
### https://stats.idre.ucla.edu/r/dae/logit-regression/

library(caret)
library(magrittr)
library(MLmetrics)

## choose working directory:
path = "<...>"

setwd(path)

## choose replicate letter:
for(o in letters[1:10]){

  rf <- readRDS(paste0("rf",toupper(o),"rd.RDS"))
  
  subset <- vector(mode = "list", length = length(rf))
  for(i in 1:length(rf)){
    # subset[[i]] <- NULL
    subset[[i]][["train"]] <- names(rf[[i]]$rf$y)
    subset[[i]][["test"]] <- names(rf[[i]]$rf$test$predicted)
  }
  
  X <- readRDS(paste0(o,".RDS"))
  names(X) <- make.names(names(X))
  keep <- read.table("keep.txt")$V1
  X <- X[, c(1, 2, which(names(X) %in% keep))]
  rownames(X) <- X[,1]; X[,-1] -> X
  X$tag <- as.factor(X$tag)
  levels(X$tag) <- c("nonR", "R")
  
  Y <- NULL
  
  for(i in 1:length(subset)){
    
    # x <- X[1:(4800-400*i),]
    y <- NULL
    
    # P = 0.8
    # train <- subset(x, split == TRUE)
    # train <- subset(x[,!grepl("residue0",names(x))], split == TRUE)
    train <- X[subset[[i]][["train"]],]
    # test <- subset(x, split == FALSE)
    # test <- subset(x[,!grepl("residue0",names(x))], split == FALSE)
    test <- X[subset[[i]][["test"]],]
    
    ## https://www.rdocumentation.org/packages/caret/versions/6.0-86/topics/trainControl
    library(caret)
    number <- 10
    repeats <- 1
    control <- trainControl(method = "repeatedcv", 
                            number = number , 
                            repeats = repeats, 
                            classProbs = TRUE, 
                            savePredictions = "final", 
                            index = createResample(train$tag, repeats*number), 
                            summaryFunction = multiClassSummary, 
                            allowParallel = TRUE)
    
    # set.seed()
    
    time <- proc.time()[3]
    glm <- train(tag ~ ., 
                   data = train, 
                   method = "glm", 
                   trControl = control)
    time <- proc.time()[3] - time
    
    prediction <- predict(glm, test, type = "raw")
    confusion <- confusionMatrix(prediction, reference = test$tag, positive = "R")
    
    # sum(coef(model1)>0)
    # sum(coef(model1)<0)
    # sum(coef(model1)==0)
    # length(coef(model1))
    # names(which(coef(model1)!=0))
    # 
    # time <- proc.time()[3]
    # library(magrittr)
    # model2 <- glm1path(as.logical(as.numeric(as.character(train$tag))),
    #                  as.matrix(train[,-1]), family = binomial(link = "logit"))
    # time2 <- proc.time()[3] - time
    # 
    # prediction <- predict(svm$best.model,newdata=test)
    # confusion <- confusionMatrix(prediction,test$tag,positive="1")
    
    # prob <- predict(rf, type = "prob")
    # pred <- prediction(prob[,2], train$tag)
    # ## get true positive and false positive rates:
    # perf <- performance(pred, "tpr","fpr")
    # ## plot the ROC curve:
    # # pdf(paste0("roc", O, "ratio1vs", 11 - i, "_FD.pdf"))
    # pdf(paste0("roc", O, "ratio1vs", 11 - i, "_RD.pdf"))
    # plot(perf, main = paste0("ROC curve for Random Forest 1:",11 - i," (", O, ")"), col = 2, lwd = 2)
    # abline(a = 0, b = 1, lwd = 2, lty = 2, col = "gray")
    # dev.off()
    # ## save the area under curve (AUC):
    # auc <- performance(pred, "auc")
    # 
    y[["model"]] <- glm
    y[["prediciton"]] <- prediction
    y[["confusion"]] <- confusion
    y[["time"]] <- time
    # y[["prob"]] <- prob
    # y[["pred"]] <- pred
    # y[["perf"]] <- perf
    # y[["auc"]] <- auc
    
    Y[[i]] <- y
    
    print(i)
  }
  
  saveRDS(Y, paste0("glm", toupper(o), "rd.RDS"))
  
}
