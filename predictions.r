# browseURL("https://github.com/cvfilippi/rfpdr")

## required package: 
library(protr)
library(randomForest)

## choose working directory:
path = "<...>"

setwd(path)

## choose query fasta file(s):
file = "<...>.fasta(.gz)"

## choose the best RFPDR model,
## from environment or as RDS:
rfpdr = "rfpdr.RDS"
model <- readRDS(rfpdr)

query <- readFASTA(file)

## trim C-terminus "*":
for(i in names(query)){
  query[[i]] <- gsub(".\\*$", "", query[[i]])
}

## protcheck
check <- query[sapply(query[[i]], protcheck)]

newlist <- NULL

## Amino acid composition
## extractAAC
newlist[["aac"]] <- NULL
for(i in names(check)){
  newlist[["aac"]][[i]] <- lapply(check[[i]], extractAAC)
}

## Dipeptide composition
## extractDC
newlist[["dc"]] <- NULL
for(i in names(check)){
  newlist[["dc"]][[i]] <- lapply(check[[i]], extractDC)
}

## Tripeptide composition
## extractTC
newlist[["tc"]] <- NULL
for(i in names(check)){
  newlist[["tc"]][[i]] <- lapply(check[[i]], extractTC)
}

## Normalized Moreau-Broto autocorrelation
## extractMoreauBroto
newlist[["mb"]] <- NULL
for(i in names(check)){
  newlist[["mb"]][[i]] <- lapply(check[[i]], extractMoreauBroto)
}

## Moran autocorrelation
## extractMoran
newlist[["m"]] <- NULL
for(i in names(check)){
  newlist[["m"]][[i]] <- lapply(check[[i]], extractMoran)
}

## Geary autocorrelation
## extractGeary
newlist[["g"]] <- NULL
for(i in names(check)){
  newlist[["g"]][[i]] <- lapply(check[[i]], extractGeary)
}

## Composition
## extractCTDC
newlist[["ctdc"]] <- NULL
for(i in names(check)){
  newlist[["ctdc"]][[i]] <- lapply(check[[i]], extractCTDC)
}

## Transition
## extractCTDT
newlist[["ctdt"]] <- NULL
for(i in names(check)){
  newlist[["ctdt"]][[i]] <- lapply(check[[i]], extractCTDT)
}

## Distribution
## extractCTDD
newlist[["ctdd"]] <- NULL
for(i in names(check)){
    newlist[["ctdd"]][[i]] <- lapply(check[[i]], extractCTDD)
}

## Conjoint triad descriptors
## extractCTriad
newlist[["t"]] <- NULL
for(i in names(check)){
  newlist[["t"]][[i]] <- newlist[["t"]][[i]] <- lapply(check[[i]], extractCTriad)
}

# save(newlist, file="newlist.RData")

newdata <- NULL
for(i in names(newlist)){
  newdata[[i]] <- NULL
  for(j in names(check)){
    newdata[[i]] <- NULL
    temp <- NULL
    temp <- cbind(temp, t(do.call(data.frame, newlist[[i]])))
    names(temp) <- names(newlist[[i]][[1]])
    rownames(temp) <- names(check[[j]])
    newdata[[i]] <- temp
  }
}

# save(data, file="newdata.RData")

newfeats <- newdata[[1]]
for(i in 2:length(newdata)){
  newfeats <- cbind(newfeats, as.data.frame(newdata[[i]]))
}
newfeats <- data.frame(newfeats, "length"=sapply(check,nchar))

save(newfeats, file="newfeatures.RData")
# write.table(newfeats, "newfeatures.csv", sep=",",
#             quote=FALSE, row.names=FALSE, col.names=TRUE)

## predicting!

keep <- try(read.table("keep.txt")$V1)
new <- newfeats[, keep]
dim(new)
colnames(new) <- rownames(model$importance)
                 
predictions <- data.frame("name"=names(check),
                          "prediction"=as.character(predict(model, new)))
predictions
write.table(predictions,paste(file,".rfpdr.tsv"),sep="\t",
            quote=FALSE,row.names=FALSE,col.names=TRUE)

table(predictions$prediction)
