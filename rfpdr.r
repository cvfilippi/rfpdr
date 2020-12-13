#####
# browseURL("https://github.com/cvfilippi/rfpdr")

## required package: 
library(caTools)
library(randomForest)
library(ROCR)

## choose working directory:
path = "..."

setwd(path)

## choose replicate letter:
## (e.g.; "a")
"a" -> o

O <- toupper(o)

X <- readRDS(paste0(o,".RDS"))
names(X) <- make.names(names(X))

## run this 4 lines only for RD-RFPDR!
## for FD-RFPDR, please omit this 4 lines below:
keep <- read.table("keep.txt")$V1
X <- X[, c(1:(1 + 20 + 20^2),
           which(names(X) %in% keep),
           (1 + 20 + 20^2 + 20^3 + 1):ncol(X))]

Y <- NULL

for(i in 1:10){

  x <- X[1:(6000-500*i),]
  y <- NULL
  
  P = 0.8
  split <- sample.split(rownames(x), SplitRatio = P)
  train <- subset(x, split == TRUE)
  test <- subset(x, split == FALSE)
  
  # set.seed()
  
  time <- proc.time()[3]
  rf <- randomForest(tag ~ .,
                     data = train,
                     xtest = test[, -1],
                     ytest = test[, 1],
                     keep.forest = TRUE,
                     importance = TRUE,
                     proximity = TRUE)
  time <- proc.time()[3] - time
  
  prob <- predict(rf, type = "prob")
  pred <- prediction(prob[,2], train$tag)
  ## get true positive and false positive rates:
  perf <- performance(pred, "tpr","fpr")
  ## plot the ROC curve:
  pdf(paste0("roc", O, "ratio1vs", 11 - i, ".pdf"))
  plot(perf, main = paste0("ROC curve for Random Forest 1:",11 - i," (", O, ")"), col = 2, lwd = 2)
  abline(a = 0, b = 1, lwd = 2, lty = 2, col = "gray")
  dev.off()
  ## save the area under curve (AUC):
  auc <- performance(pred, "auc")
  
  y[["rf"]] <- rf
  y[["time"]] <- time
  y[["prob"]] <- prob
  y[["pred"]] <- pred
  y[["perf"]] <- perf
  y[["auc"]] <- auc
  
  Y[[i]] <- y
  
  print(i)
}

# saveRDS(Y, paste0("rf", O, ".RDS"))
saveRDS(Y, paste0("rf", O, "rd.RDS"))
