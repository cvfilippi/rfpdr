#####
# browseURL("https://github.com/cvfilippi/rfpdr")

## required package:
library(vioplot)

## choose working directory
path = "..."

setwd(path)

## Accuracy
# Accuracy = (TP+TN)/(TP+FP+FN+TN)
Accuracy <- matrix(NA, nrow = 10, ncol = 10)
accuracy <- matrix(NA, nrow = 10, ncol = 10)

## Precision
# Precision = TP/(TP+FP)
Precision <- matrix(NA, nrow = 10, ncol = 10)
precision <- matrix(NA, nrow = 10, ncol = 10)

## Recall (aka Sensitivity)
# Recall = TP/(TP+FN)
Recall <- matrix(NA, nrow = 10, ncol = 10)
recall <- matrix(NA, nrow = 10, ncol = 10)

## F1-Score (aka F-Score / F-Measure)
# F1 score = 2*(Recall * Precision) / (Recall + Precision)
F1 <- matrix(NA, nrow = 10, ncol = 10)
f1 <- matrix(NA, nrow = 10, ncol = 10)

## Specificity
# Specificity = TN/(TN+FP)
Specificity <- matrix(NA, nrow = 10, ncol = 10)
specificity <- matrix(NA, nrow = 10, ncol = 10)

# files <- dir(pattern = "rf[A-J].RDS")
files <- dir(pattern = "rf[A-J]rd.RDS")

for(i in 1:length(files)){
  
  rf <- readRDS(files[i])
  
  for(j in 1:length(rf)){
    
    M <- rf[[j]]$rf$confusion
    m <- rf[[j]]$rf$test$confusion
    
    TN <- M[1]
    FN <- M[2]
    FP <- M[3]
    TP <- M[4]
    
    tn <- m[1]
    fn <- m[2]
    fp <- m[3]
    tp <- m[4]
    
    Accuracy[11 - j, i] <- (TP+TN)/(TP+FP+FN+TN)
    Precision[11 - j, i] <- TP/(TP+FP)
    Recall[11 - j, i] <- TP/(TP+FN)
    F1[11 - j, i] <- 2*(Recall[11 - j, i] * Precision[11 - j, i]) / (Recall[11 - j, i] + Precision[11 - j, i])
    Specificity[11 - j, i] <- TN/(TN+FP)
    
    accuracy[11 - j, i] <- (tp+tn)/(tp+fp+fn+tn)
    precision[11 - j, i] <- tp/(tp+fp)
    recall[11 - j, i] <- tp/(tp+fn)
    f1[11 - j, i] <- 2*(recall[11 - j, i] * precision[11 - j, i]) / (recall[11 - j, i] + precision[11 - j, i])
    specificity[11 - j, i] <- tn/(tn+fp)
    
    print(paste(i, 11 - j, sep = "."))
    
  }
  
  rm(rf)
  
}

#####

## access metrics for the model performance estimations (for test sets only)

## Accuracy
round(rowMeans(accuracy),3)*100
round(apply(accuracy, 1, sd),3)*100

## Precision
round(rowMeans(precision),3)*100
round(apply(precision, 1, sd),3)*100

## Recall (aka Sensitivity)
round(rowMeans(recall),3)*100
round(apply(recall, 1, sd),3)*100

## F1-Score
round(rowMeans(f1),3)*100
round(apply(f1, 1, sd),3)*100

## Specificity
round(rowMeans(specificity),3)*100
round(apply(specificity, 1, sd),3)*100

#####

# pdf("plotsFD.pdf")
pdf("plotsRD.pdf")

library(vioplot)

## Accuracy
round(rowMeans(accuracy),3)*100
round(apply(accuracy, 1, sd),3)*100
# boxplot(t(accuracy))
vioplot(t(accuracy), col = "grey", main = "Accuracy")

## Precision
round(rowMeans(precision),3)*100
round(apply(precision, 1, sd),3)*100
# boxplot(t(precision))
vioplot(t(precision), col = "grey", main = "Precision")

## Recall (aka Sensitivity)
round(rowMeans(recall),3)*100
round(apply(recall, 1, sd),3)*100
# boxplot(t(recall))
vioplot(t(recall), col = "grey", main = "Recall")

## F1-Score
round(rowMeans(f1),3)*100
round(apply(f1, 1, sd),3)*100
# boxplot(t(f1))
vioplot(t(f1), col = "grey", main = "F1-Score")

## Specificity
round(rowMeans(specificity),3)*100
round(apply(specificity, 1, sd),3)*100
# boxplot(t(specificity))
vioplot(t(specificity), col = "grey", main = "Specificity")

dev.off()

#####

# files <- dir(pattern = "rf[A-J].RDS")
files <- dir(pattern = "rf[A-J]rd.RDS")

time <- matrix(NA, nrow = 10, ncol = 10)
auc <- matrix(NA, nrow = 10, ncol = 10)

for(i in 1:length(files)){
  
  rf <- readRDS(files[i])
  
  for(j in 1:length(rf)){
    
    time[11 - j, i] <- rf[[j]]$time[[1]]
    auc[11 - j, i] <- rf[[j]]$auc@y.values[[1]]
    
    write.csv(rf[[j]]$rf$importance,
              paste0("impo",i,".",11-j,"FD.csv"), quote=FALSE)
              # paste0("impo",i,".",11-j,"RD.csv"), quote=FALSE)
    
  }
  
  rm(rf)
  
}


# write.table(time,"timeFD.txt",quote=FALSE)
write.table(time,"timeRD.txt",quote=FALSE)

# write.table(auc,"aucFD.txt",quote=FALSE)
write.table(auc,"aucRD.txt",quote=FALSE)
