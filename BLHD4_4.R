rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
########## Input data ###############
FilePath <- '../Google Drive/projects/filesystem_Spring2014/Data'
setwd(FilePath)
Filename <- 'all.file.systems-0324.txt'
columnnames = c("fullness", "num.chunks", "num.cores", 
                "chunk.order", "file.size", "sync", "dir.id", 
                "fsync", "disk.size", "disk.used", "dspan")
columnclasses = c("factor","factor","factor","factor","numeric","factor","factor",
                  "factor","numeric","factor","numeric")
names(columnclasses) = columnnames
data.sys <- read.table(file=Filename, header=T, quote="",
                       colClasses=columnclasses)

## normalized dspan and partition data into separate file systems
data.sys$dspan <- data.sys$dspan/data.sys$file.size
data.sys$file.size <- as.factor(data.sys$file.size/1024)
data.sys$disk.size <- as.factor(data.sys$disk.size/2^30)
data.sys <- as.data.frame(data.sys)
data.ext4 <- subset(data.sys, file.system == "ext4")
data.btrfs <- subset(data.sys, file.system == "btrfs")
data.xfs <- subset(data.sys, file.system == "xfs")

######### Analysis ######################
## fit model with only main effects
data = data.btrfs
model <- lm(log2(dspan) ~ num.chunks + file.size + fullness + dir.id + num.cores + disk.used
            + disk.size + num.chunks/fsync + num.chunks/sync + num.chunks/chunk.order ,data = data)
## throw away some insensitive factors
SumSq <- data.frame(anova(model)[2])
S.i <- SumSq[[1]][1:length(SumSq[[1]])]/sum(SumSq)
data.frame(SumSq,S.i*100)
## Optimal settings, model selection
null <- lm(log2(dspan) ~ 1 ,data = data)
smodel <- step(null, scope= list(lower=null, upper=model), direction="both", 
                   #k=log(dim(data)[1]))
                   k=2)
summary(smodel)
## fit model with 2-way interactions
model <- lm(log2(dspan) ~ (num.chunks + file.size + fullness + dir.id + num.cores + disk.used
                           + disk.size)^2 + num.chunks/fsync + num.chunks/sync  + num.chunks/chunk.order ,data = data.sys)
SumSq <- data.frame(anova(model)[2])
S.i <- SumSq[[1]][1:21]/sum(SumSq)
data.frame(SumSq,S.i)
