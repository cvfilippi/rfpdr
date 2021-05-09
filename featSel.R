# browseURL("https://github.com/cvfilippi/rfpdr")

## choose working directory:
path = "<...>"

setwd(path)

# feats<-read.csv("features.csv")
load("features.RData")

# mw<-NULL
# for(i in names(feats)[-c(1:2)]){
#   mw[[i]]<-wilcox.test(feats[,i]~feats[,"tag"])
#   print(i)
# }
# 
# # saveRDS(wilcox,"mw.RDS")
# 
# pvalues <- matrix(NA, length(mw), 2)
# pvalues <- data.frame(pvalues)
# rownames(pvalues) <- names(mw)
# colnames(pvalues) <- c("variable","pvalue")
# pvalues$variable <- names(mw)
# for(i in names(mw)){
#   pvalues[i,"pvalue"] <- mw[[i]]$p.value
# }
# 
# write.table(bind, "mw.csv", sep=",",
#             quote=FALSE, row.names=FALSE, col.names=TRUE)

## subsetting

# set.seed(<...>)

s = 100 # n subsets

p = sum(feats$tag=="1")
q = p + 1

subsets <- NULL
for(i in 1:s){
  subsets[[i]] <- rbind(feats[1:p,],
                        feats[sample(q:nrow(feats), p),])
}

# save(subsets, file="subsets.RData")

pvalues <- matrix(NA, ncol(feats) - 2, s)
rownames(pvalues) <- names(feats)[-c(1:2)]
for(i in 1:s){
  for(j in rownames(pvalues)){
    pvalues[j,i]<-wilcox.test(subsets[[i]][,j]~subsets[[i]][,"tag"])$p.value
  }
  print(i)
}

# save(pvalues, file="pvalues.RData")
write.table(pvalues, "pvalues.csv", sep=",",
            quote=FALSE, row.names=TRUE, col.names=FALSE)

pvalues <- data.frame(pvalues)
rownames(pvalues) <- names(mw)
pvalues$variable <- names(mw)
for(i in names(mw)){
  pvalues[i,"pvalue"] <- mw[[i]]$p.value
}

write.table(bind, "mw.csv", sep=",",
            quote=FALSE, row.names=FALSE, col.names=TRUE)

pValues <- data.frame("var"=pvalues[[1]][,1],
                      "p.value_1"=as.numeric(pvalues[[1]][,2]))
for(i in 2:length(pvalues)){pValues[,paste0("p.value_",i)]<-as.numeric(pvalues[[i]][,2])}
write.table(pValues,"pValues.csv",sep=",",quote = F,row.names = F,col.names = F)

row.names(pValues)<-pValues$var
pValues<-pValues[,-1]

keep<-names((which(apply(pValues,1,max,na.rm=TRUE)<(0.05/nrow(pValues)))))[!grepl("residue0",names(which(apply(pValues,1,max,na.rm=TRUE)<(0.05/nrow(pValues)))))]
set1 <- (pvalues[pvalues$pvalue<=(0.05/nrow(pvalues)),])
set2 <- (pvalues[pvalues$pvalue<=(0.05/nrow(pvalues)^2),])
# set3 <- (pvalues[pvalues$pvalue<=(0.05/(nrow(pvalues)*nrow(all))),])
# set4 <- (pvalues[pvalues$pvalue<=(0.05/nrow(all)^2),])

0.001 -> raw
# keep <- names(which(apply(pvalues,1,max,na.rm=TRUE)<raw/nrow(pvalues)))
keep <- names(which(apply(pvalues,1,max,na.rm=TRUE)<.Machine$double.eps/raw))

write.table(keep[!grepl("residue0",keep)],"keep.txt",
            quote = F,row.names = F,col.names = F)
