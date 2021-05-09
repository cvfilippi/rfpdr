# browseURL("https://github.com/cvfilippi/rfpdr")

## required package:
library(vioplot)

## choose working directory
path = "<...>"

setwd(path)

"rf" -> model; size <- "rd" # c("fd", "rd", "xr")
# "svmRadial" -> model; size <- "rd" # c("svmLinear", "svmRadial", "svmSigmoid")
# "glm" -> model; size <- "rd"

## Accuracy
# Accuracy = (TP+TN)/(TP+FP+FN+TN)
# Accuracy <- matrix(NA, nrow = 10, ncol = 10)
accuracy <- matrix(NA, nrow = 10, ncol = 10)

## Precision
# Precision = TP/(TP+FP)
# Precision <- matrix(NA, nrow = 10, ncol = 10)
precision <- matrix(NA, nrow = 10, ncol = 10)

## Recall (aka Sensitivity)
# Recall = TP/(TP+FN)
# Recall <- matrix(NA, nrow = 10, ncol = 10)
recall <- matrix(NA, nrow = 10, ncol = 10)

## F1-Score (aka F-Score / F-Measure)
# F1 score = 2*(Recall * Precision) / (Recall + Precision)
# F1 <- matrix(NA, nrow = 10, ncol = 10)
f1 <- matrix(NA, nrow = 10, ncol = 10)

## Specificity
# Specificity = TN/(TN+FP)
# Specificity <- matrix(NA, nrow = 10, ncol = 10)
specificity <- matrix(NA, nrow = 10, ncol = 10)

time <- matrix(NA, nrow = 10, ncol = 10)

files <- dir(pattern=paste0(size,".RDS$"))[grepl(model,dir(pattern=paste0(size,".RDS$")))]

for(i in 1:length(files)){
  
  Model <- readRDS(files[i])
  
  for(j in 1:length(Model)){
    
    ## "M" for training / "m" for test
    # M <- Model[[j]]$model$confusion
    # m <- Model[[j]]$rf$test$confusion # for RF (old code)
    m <- Model[[j]]$model$test$confusion # for RF
    # m <- Model[[j]]$confusion$table # for SVM and GLM
    
    # TN <- M[1]
    # FN <- M[2]
    # FP <- M[3]
    # TP <- M[4]
    
    # tn <- m[1]
    # fn <- m[2]
    # fp <- m[3]
    # tp <- m[4]
    
    tn <- m[1]
    fn <- m[3]
    fp <- m[2]
    tp <- m[4]
    
    # Accuracy[11 - j, i] <- (TP+TN)/(TP+FP+FN+TN)
    # Precision[11 - j, i] <- TP/(TP+FP)
    # Recall[11 - j, i] <- TP/(TP+FN)
    # F1[11 - j, i] <- 2*(Recall[11 - j, i] * Precision[11 - j, i]) / (Recall[11 - j, i] + Precision[11 - j, i])
    # Specificity[11 - j, i] <- TN/(TN+FP)
    
    accuracy[11 - j, i] <- (tp+tn)/(tp+fp+fn+tn)
    precision[11 - j, i] <- tp/(tp+fp)
    recall[11 - j, i] <- tp/(tp+fn)
    f1[11 - j, i] <- 2*(recall[11 - j, i] * precision[11 - j, i]) / (recall[11 - j, i] + precision[11 - j, i])
    specificity[11 - j, i] <- tn/(tn+fp)
    
    time[11 - j, i] <- Model[[j]]$time[[1]]
    
    print(paste(i, 11 - j, sep = "."))
    
  }
  
  # rm(Model)
  
}

if(any(grepl("auc",names(Model[[j]])))){
  
  auc <- matrix(NA, nrow = 10, ncol = 10)
  
  for(i in 1:length(files)){
    
    Model <- readRDS(files[i])
    
    for(j in 1:length(Model)){
      
      auc[11 - j, i] <- Model[[j]]$auc@y.values[[1]]
      
      # write.csv(Model[[j]]$model$importance,
      #           paste0("impo",i,11-j,model,size,"csv",sep="."),
      #           quote=FALSE,row.names=TRUE,col.names=TRUE)
      
    }
    rm(Model)
  }
  write.table(auc,paste("auc",model,size,"txt",sep="."),
              quote=FALSE,row.names=FALSE,col.names=FALSE)
}else{
  rm(Model)
}
         
# ## access metrics for the model performance estimations (for test sets only)
# 
# ## Accuracy
# round(rowMeans(accuracy),3)*100
# round(apply(accuracy, 1, sd),3)*100
# 
# ## Precision
# round(rowMeans(precision),3)*100
# round(apply(precision, 1, sd),3)*100
# 
# ## Recall (aka Sensitivity)
# round(rowMeans(recall),3)*100
# round(apply(recall, 1, sd),3)*100
# 
# ## F1-Score
# round(rowMeans(f1),3)*100
# round(apply(f1, 1, sd),3)*100
# 
# ## Specificity
# round(rowMeans(specificity),3)*100
# round(apply(specificity, 1, sd),3)*100

# capwords <- function(s, strict = FALSE) {
#   cap <- function(s) paste(toupper(substring(s, 1, 1)),
#                            {s <- substring(s, 2); if(strict) tolower(s) else s},
#                            sep = "", collapse = " " )
#   sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
# }

pdf(paste0(model, toupper(size), ".pdf"))

## Accuracy
round(rowMeans(accuracy),3)*100
round(apply(accuracy, 1, sd),3)*100
# boxplot(t(accuracy))
vioplot(t(accuracy), col = "grey", main = "Accuracy")
write.table(accuracy,paste("accuracy",model,size,"txt",sep="."),
            quote=FALSE,row.names=FALSE,col.names=FALSE)

## Precision
round(rowMeans(precision),3)*100
round(apply(precision, 1, sd),3)*100
# boxplot(t(precision))
vioplot(t(precision), col = "grey", main = "Precision")
write.table(precision,paste("precision",model,size,"txt",sep="."),
            quote=FALSE,row.names=FALSE,col.names=FALSE)

## Recall (aka Sensitivity)
round(rowMeans(recall),3)*100
round(apply(recall, 1, sd),3)*100
# boxplot(t(recall))
vioplot(t(recall), col = "grey", main = "Recall")
write.table(recall,paste("recall",model,size,"txt",sep="."),
            quote=FALSE,row.names=FALSE,col.names=FALSE)

## F1-Score
round(rowMeans(f1),3)*100
round(apply(f1, 1, sd),3)*100
# boxplot(t(f1))
vioplot(t(f1), col = "grey", main = "F1-Score")
write.table(f1,paste("f1",model,size,"txt",sep="."),
            quote=FALSE,row.names=FALSE,col.names=FALSE)

## Specificity
round(rowMeans(specificity),3)*100
round(apply(specificity, 1, sd),3)*100
# boxplot(t(specificity))
vioplot(t(specificity), col = "grey", main = "Specificity")
write.table(specificity,paste("specificity",model,size,"txt",sep="."),
            quote=FALSE,row.names=FALSE,col.names=FALSE)

## Time
round(rowMeans(time),3)
round(apply(time, 1, sd),3)
# boxplot(t(time))
vioplot(t(time), col = "grey", main = "Time")
write.table(time,paste("time",model,size,"txt",sep="."),
            quote=FALSE,row.names=FALSE,col.names=FALSE)

dev.off()
