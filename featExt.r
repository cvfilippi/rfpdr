# browseURL("https://github.com/cvfilippi/rfpdr")

## required package: 
library(protr)

## choose working directory
path = "<...>"

setwd(path)

input <- NULL

input[["1"]] <- readFASTA("positives.fasta.gz")
input[["0"]] <- readFASTA("negatives.fasta.gz")

## protcheck
output <- NULL
for(i in names(input)){
  output[[i]] <- input[[i]][sapply(input[[i]], protcheck)]
}
input <- output; rm(output)

## nchar
n = min(sapply(input[["1"]], nchar))
output <- NULL
for(i in names(input)){
  output[[i]] <- input[[i]][sapply(input[[i]], nchar) >= n]
}
input <- output; rm(output)

list <- vector(mode = "list", length = length(input)) -> time
names(list) <- names(input) -> names(time)

## Amino acid composition
## extractAAC
for(i in names(input)){
  t0 <- proc.time()[3]
  list[[i]][["aac"]] <- lapply(input[[i]], extractAAC)
  time[[i]][["aac"]] <- proc.time()[3] - t0
}

## Dipeptide composition
## extractDC
for(i in names(input)){
  t0 <- proc.time()[3]
  list[[i]][["dc"]] <- lapply(input[[i]], extractDC)
  time[[i]][["dc"]] <- proc.time()[3] - t0
}

## Tripeptide composition
## extractTC
for(i in names(input)){
  t0 <- proc.time()[3]
  list[[i]][["tc"]] <- lapply(input[[i]], extractTC)
  time[[i]][["tc"]] <- proc.time()[3] - t0
}

## Normalized Moreau-Broto autocorrelation
## extractMoreauBroto
for(i in names(input)){
  t0 <- proc.time()[3]
  list[[i]][["mb"]] <- lapply(input[[i]], extractMoreauBroto)
  time[[i]][["mb"]] <- proc.time()[3] - t0
}

## Moran autocorrelation
## extractMoran
for(i in names(input)){
  t0 <- proc.time()[3]
  list[[i]][["m"]] <- lapply(input[[i]], extractMoran)
  time[[i]][["m"]] <- proc.time()[3] - t0
}

## Geary autocorrelation
## extractGeary
for(i in names(input)){
  t0 <- proc.time()[3]
  list[[i]][["g"]] <- lapply(input[[i]], extractGeary)
  time[[i]][["g"]] <- proc.time()[3] - t0
}

## Composition
## extractCTDC
for(i in names(input)){
  t0 <- proc.time()[3]
  list[[i]][["ctdc"]] <- lapply(input[[i]], extractCTDC)
  time[[i]][["ctdc"]] <- proc.time()[3] - t0
}

## Transition
## extractCTDT
for(i in names(input)){
  t0 <- proc.time()[3]
  list[[i]][["ctdt"]] <- lapply(input[[i]], extractCTDT)
  time[[i]][["ctdt"]] <- proc.time()[3] - t0
}

## Distribution
## extractCTDD
for(i in names(input)){
  t0 <- proc.time()[3]
  list[[i]][["ctdd"]] <- lapply(input[[i]], extractCTDD)
  time[[i]][["ctdd"]] <- proc.time()[3] - t0
}

## Conjoint triad descriptors
## extractCTriad
for(i in names(input)){
  t0 <- proc.time()[3]
  list[[i]][["ct"]] <- lapply(input[[i]], extractCTriad)
  time[[i]][["ct"]] <- proc.time()[3] - t0
}

## Protein length
## nchar
for(i in names(input)){
  t0 <- proc.time()[3]
  list[[i]][["l"]] <- lapply(input[[i]], nchar)
  time[[i]][["l"]] <- proc.time()[3] - t0
}

# save(list, file="list.RData")
# save(time, file="time.RData")

data <- NULL

for(i in names(list[[1]])){
  temp <- NULL
  temp$tag <- as.factor(c(rep("1", length(input[[1]])), rep("0", length(input[[2]]))))
  temp <- cbind(temp, do.call(rbind.data.frame,
                              c(list[["1"]][[i]],
                                list[["0"]][[i]])))
  names(temp) <- c("tag",names(list[["1"]][[i]][[1]]))
  rownames(temp) <- c(names(input[["1"]]),names(input[["0"]]))
  data[[i]] <- temp
}

# save(data, file="data.RData")

feats <- data.frame("name" = c(names(input[["1"]]),names(input[["0"]])),
                    "tag" = as.factor(c(rep("1", length(input[[1]])), rep("0", length(input[[2]])))))

for(i in names(data)){
  feats <- cbind(feats, data[[i]][,-1])
}

names(feats)[length(feats)] <- "length"

save(feats, file="features.RData")
# write.table(feats, "features.csv", sep=",",
#             quote=FALSE, row.names=FALSE, col.names=TRUE)

## sampling

# set.seed(<...>)

r = 10 # max ratio
s = 10 # n samples

p = sum(feats$tag=="1")
q = p + 1

samples <- NULL
for(i in letters[1:s]){
  samples[[i]] <- rbind(feats[1:p,],
                        feats[sample(q:nrow(feats), p*r),])
}

# save(samples, file="samples.RData")
for(i in names(samples)){saveRDS(samples[[i]], paste0(i, ".RDS"))}
