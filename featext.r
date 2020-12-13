#####
# browseURL("https://github.com/cvfilippi/rfpdr")

## required package: 
library(protr)

## choose working directory
path = "..."

setwd(path)

input <- NULL

input[["1"]] <- readFASTA("500ok.fas.gz")
input[["0"]] <- readFASTA("68166.fas.gz")

## protcheck
output <- NULL
for(i in names(input)){
  output[[i]] <- input[[i]][sapply(input[[i]], protcheck)]
}
input <- output; rm(output)

## nchar
n = 100
output <- NULL
for(i in names(input)){
  output[[i]] <- input[[i]][sapply(input[[i]], nchar) >= n]
}
input <- output; rm(output)

## sample
I = 10
R = 10
sample <- NULL
for(i in letters[1:I]){
  sample[[i]][["1"]] <- input[["1"]]
  sample[[i]][["0"]] <- input[["0"]][sample(1 : length(input[["0"]]), 500 * R)]
}

saveRDS(sample, "samples.RDS")

#####

list <- NULL

## Amino acid composition
## extractAAC
list[["aac"]] <- NULL
for(i in names(sample)){
  list[["aac"]][[i]] <- NULL
  time <- proc.time()[3]
  for(j in names(sample[[i]])){
    list[["aac"]][[i]][[j]] <- lapply(sample[[i]][[j]], extractAAC)
  }
  list[["aac"]][[i]][["T"]] <- proc.time()[3] - time
}

## Dipeptide composition
## extractDC
list[["dc"]] <- NULL
for(i in names(sample)){
  list[["dc"]][[i]] <- NULL
  time <- proc.time()[3]
  for(j in names(sample[[i]])){
    list[["dc"]][[i]][[j]] <- lapply(sample[[i]][[j]], extractDC)
  }
  list[["dc"]][[i]][["T"]] <- proc.time()[3] - time
}

## Tripeptide composition
## extractTC
list[["tc"]] <- NULL
for(i in names(sample)){
  list[["tc"]][[i]] <- NULL
  time <- proc.time()[3]
  for(j in names(sample[[i]])){
    list[["tc"]][[i]][[j]] <- lapply(sample[[i]][[j]], extractTC)
  }
  list[["tc"]][[i]][["T"]] <- proc.time()[3] - time
}

## Normalized Moreau-Broto autocorrelation
## extractMoreauBroto
list[["mb"]] <- NULL
for(i in names(sample)){
  list[["mb"]][[i]] <- NULL
  time <- proc.time()[3]
  for(j in names(sample[[i]])){
    list[["mb"]][[i]][[j]] <- lapply(sample[[i]][[j]], extractMoreauBroto)
  }
  list[["mb"]][[i]][["T"]] <- proc.time()[3] - time
}

## Moran autocorrelation
## extractMoran
list[["m"]] <- NULL
for(i in names(sample)){
  list[["m"]][[i]] <- NULL
  time <- proc.time()[3]
  for(j in names(sample[[i]])){
    list[["m"]][[i]][[j]] <- lapply(sample[[i]][[j]], extractMoran)
  }
  list[["m"]][[i]][["T"]] <- proc.time()[3] - time
}

## Geary autocorrelation
## extractGeary
list[["g"]] <- NULL
for(i in names(sample)){
  list[["g"]][[i]] <- NULL
  time <- proc.time()[3]
  for(j in names(sample[[i]])){
    list[["g"]][[i]][[j]] <- lapply(sample[[i]][[j]], extractGeary)
  }
  list[["g"]][[i]][["T"]] <- proc.time()[3] - time
}

## Composition
## extractCTDC
list[["ctdc"]] <- NULL
for(i in names(sample)){
  list[["ctdc"]][[i]] <- NULL
  time <- proc.time()[3]
  for(j in names(sample[[i]])){
    list[["ctdc"]][[i]][[j]] <- lapply(sample[[i]][[j]], extractCTDC)
  }
  list[["ctdc"]][[i]][["T"]] <- proc.time()[3] - time
}

## Transition
## extractCTDT
list[["ctdt"]] <- NULL
for(i in names(sample)){
  list[["ctdt"]][[i]] <- NULL
  time <- proc.time()[3]
  for(j in names(sample[[i]])){
    list[["ctdt"]][[i]][[j]] <- lapply(sample[[i]][[j]], extractCTDT)
  }
  list[["ctdt"]][[i]][["T"]] <- proc.time()[3] - time
}

## Distribution
## extractCTDD
list[["ctdd"]] <- NULL
for(i in names(sample)){
  list[["ctdd"]][[i]] <- NULL
  time <- proc.time()[3]
  for(j in names(sample[[i]])){
    list[["ctdd"]][[i]][[j]] <- lapply(sample[[i]][[j]], extractCTDD)
  }
  list[["ctdd"]][[i]][["T"]] <- proc.time()[3] - time
}

## Conjoint triad descriptors
## extractCTriad
list[["t"]] <- NULL
for(i in names(sample)){
  list[["t"]][[i]] <- NULL
  time <- proc.time()[3]
  for(j in names(sample[[i]])){
    list[["t"]][[i]][[j]] <- lapply(sample[[i]][[j]], extractCTriad)
  }
  list[["t"]][[i]][["T"]] <- proc.time()[3] - time
}

saveRDS(list, "list.RDS")

#####

data <- NULL

for(i in names(list)[]){
  data[[i]] <- NULL
  for(j in names(sample)){
    data[[i]][[j]] <- NULL
    temp <- NULL
    temp$tag <- as.factor(c(rep("1", 500), rep("0", 500 * R)))
    temp <- cbind(temp, do.call(rbind.data.frame,
                                c(list[[i]][[j]][["1"]],
                                  list[[i]][[j]][["0"]])))
    names(temp) <- c("tag",names(list[[i]][[j]][["1"]][[1]]))
    rownames(temp) <- c(names(sample[[j]][["1"]]),names(sample[[j]][["0"]]))
    data[[i]][[j]] <- temp
  }
}

saveRDS(data, "data.RDS")

#####

bind <- NULL

for(i in names(data[[1]])){
  bind[[i]] <- NULL
  temp <- data[[1]][[i]]
  for(j in 2:I){
    temp <- cbind(temp, data[[j]][[i]][,-1])
  }
  bind[[i]] <- temp
}

saveRDS(bind, "bind.RDS")

for(i in names(bind)){saveRDS(bind[[i]], paste0(i, ".RDS"))}
