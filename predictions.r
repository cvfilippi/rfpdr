#####
# browseURL("https://github.com/cvfilippi/rfpdr")

## required package: 
library(protr)
library(randomForest)

## choose working directory:
path = "~/<...>"

setwd(path)

## choose query fasta file(s):
files = "<...>.fas"

# list.files(pattern = ".fa$|.fas$|.fasta$")

## choose the best RD-RFPDR model,
## from environment or as RDS:
rfpdr = "<...>.RDS"
## for example, model for 1:10 ratio:
# rfpdr = "rd-rfpdr_1vs10.RDS"

# list.files(pattern = ".RDS$")

#####

query <- NULL

for(i in files){
  query[[i]] <- readFASTA(i)
}

## trim C-terminus "*":
for(i in names(query)){
  for(j in names(query[[i]])){
    query[[i]][[j]] <- gsub(".\\*$", "", query[[i]][[j]])
  }
}

## protcheck
check <- NULL
for(i in names(query)){
  check[[i]] <- query[[i]][sapply(query[[i]], protcheck)]
}

#####

newlist <- NULL

## Amino acid composition
## extractAAC
newlist[["aac"]] <- NULL
for(i in names(check)){
  newlist[["aac"]][[i]] <- NULL
  for(j in names(check[[i]])){
    newlist[["aac"]][[i]][[j]] <- lapply(check[[i]][[j]], extractAAC)
  }
}

## Dipeptide composition
## extractDC
newlist[["dc"]] <- NULL
for(i in names(check)){
  newlist[["dc"]][[i]] <- NULL
  for(j in names(check[[i]])){
    newlist[["dc"]][[i]][[j]] <- lapply(check[[i]][[j]], extractDC)
  }
}

## Tripeptide composition
## extractTC
newlist[["tc"]] <- NULL
for(i in names(check)){
  newlist[["tc"]][[i]] <- NULL
  for(j in names(check[[i]])){
    newlist[["tc"]][[i]][[j]] <- lapply(check[[i]][[j]], extractTC)
  }
}

## Normalized Moreau-Broto autocorrelation
## extractMoreauBroto
newlist[["mb"]] <- NULL
for(i in names(check)){
  newlist[["mb"]][[i]] <- NULL
  for(j in names(check[[i]])){
    newlist[["mb"]][[i]][[j]] <- lapply(check[[i]][[j]], extractMoreauBroto)
  }
}

## Moran autocorrelation
## extractMoran
newlist[["m"]] <- NULL
for(i in names(check)){
  newlist[["m"]][[i]] <- NULL
  for(j in names(check[[i]])){
    newlist[["m"]][[i]][[j]] <- lapply(check[[i]][[j]], extractMoran)
  }
}

## Geary autocorrelation
## extractGeary
newlist[["g"]] <- NULL
for(i in names(check)){
  newlist[["g"]][[i]] <- NULL
  for(j in names(check[[i]])){
    newlist[["g"]][[i]][[j]] <- lapply(check[[i]][[j]], extractGeary)
  }
}

## Composition
## extractCTDC
newlist[["ctdc"]] <- NULL
for(i in names(check)){
  newlist[["ctdc"]][[i]] <- NULL
  for(j in names(check[[i]])){
    newlist[["ctdc"]][[i]][[j]] <- lapply(check[[i]][[j]], extractCTDC)
  }
}

## Transition
## extractCTDT
newlist[["ctdt"]] <- NULL
for(i in names(check)){
  newlist[["ctdt"]][[i]] <- NULL
  for(j in names(check[[i]])){
    newlist[["ctdt"]][[i]][[j]] <- lapply(check[[i]][[j]], extractCTDT)
  }
}

## Distribution
## extractCTDD
newlist[["ctdd"]] <- NULL
for(i in names(check)){
  newlist[["ctdd"]][[i]] <- NULL
  for(j in names(check[[i]])){
    newlist[["ctdd"]][[i]][[j]] <- lapply(check[[i]][[j]], extractCTDD)
  }
}

## Conjoint triad descriptors
## extractCTriad
newlist[["t"]] <- NULL
for(i in names(check)){
  newlist[["t"]][[i]] <- NULL
  for(j in names(check[[i]])){
    newlist[["t"]][[i]][[j]] <- lapply(check[[i]][[j]], extractCTriad)
  }
}

#####

newdata <- NULL

for(i in names(newlist)[]){
  newdata[[i]] <- NULL
  for(j in names(check)){
    newdata[[i]][[j]] <- NULL
    temp <- NULL
    temp <- cbind(temp, t(do.call(data.frame, newlist[[i]][[j]])))
    names(temp) <- names(newlist[[i]][[j]][[1]])
    rownames(temp) <- names(check[[j]])
    newdata[[i]][[j]] <- temp
  }
}

#####

newbind <- NULL

for(i in names(newdata[[1]])){
  # newbind[[i]] <- NULL
  temp <- newdata[[1]][[i]]
  for(j in 2:length(newdata)){
    temp <- cbind(temp, as.data.frame(newdata[[j]][[i]]))
  }
  newbind[[i]] <- temp
}

#####

model <- readRDS(rfpdr)

predictions <- NULL

for(i in names(newbind)){
  keep <- try(read.table("keep.txt")$V1)
  copy <- newbind[[i]]
  copy <- copy[, c(1:(20 + 20^2),
                   which(names(newbind[[i]]) %in% keep),
                   (20 + 20^2 + 20^3 + 1):ncol(newbind[[i]]))]
  dim(copy)
  colnames(copy) <- rownames(model$importance)
                   
  predictions[[i]] <- as.character(predict(model, copy))
                   
}

lapply(predictions, table)
