# browseURL("https://github.com/cvfilippi/rfpdr")

## ref: https://educationalresearchtechniques.com/2017/05/03/numeric-prediction-with-support-vector-machines-in-r/#:~:text=SVM%20is%20used%20for%20both,summarizes%20this%20in%20the%20output

## required package: 
library(caTools)
library(randomForest)
library(ROCR)

## choose working directory:
path = "<...>"

setwd(path)

## choose replicate letter:
## (e.g.; "a")
"a" -> o

## run this line for FD-RFPDR
# X <- readRDS(paste0(o,"FD.RDS"))
# names(X) <- make.names(names(X))
# model = "FD"

## run these lines for RD-RFPDR
X <- readRDS(paste0(o,".RDS"))
names(X) <- make.names(names(X))
keep <- read.table("keep.txt")$V1
X <- X[, c(1, 2, which(names(X) %in% keep))]
model = "RD"

## or run these lines only for a XR-RFPDR,
## retaining top12 features:
# model = "XR"
# X <- readRDS(paste0(o,".RDS"))
# names(X) <- make.names(names(X))
# top <- c("length", "prop3.Tr1331", "GKT", "prop1.Tr1331", "L", "KTT", "CIDH920105.lag1", "VLD", "IGK", "normwaalsvolume.Group2", "CIDH920105.lag2", "LSY")
# X <- X[,c("name", "tag", as.character(top))]

rownames(X) <- X[,1]; X[,-1] -> X
X$tag <- as.factor(X$tag)

Y <- NULL

# set.seed(<...>)

for(i in 1:10){
  
  x <- X[1:(4800-400*i),]
  y <- NULL
  
  P = 0.8
  train <- subset(x, split == TRUE)
  test <- subset(x, split == FALSE)
  
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
  pdf(paste0("roc", toupper(o), "ratio1vs", 11 - i, tolower(model), ".pdf"))
  plot(perf, main = paste0("ROC curve for Random Forest 1:",11 - i," (", toupper(o), ")"), col = 2, lwd = 2)
  abline(a = 0, b = 1, lwd = 2, lty = 2, col = "gray")
  dev.off()
  ## save the area under curve (AUC):
  auc <- performance(pred, "auc")
  
  y[["model"]] <- rf
  y[["time"]] <- time
  y[["prob"]] <- prob
  y[["pred"]] <- pred
  y[["perf"]] <- perf
  y[["auc"]] <- auc
  
  Y[[i]] <- y
  
  print(i)
}

saveRDS(Y, paste0("rf", toupper(o), tolower(model), ".RDS"))
