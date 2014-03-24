rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
########## Input data ###############
FilePath <- '../Google Drive/projects/filesystem_Spring2014/Data'
setwd(FilePath)
Filename <- 'nested4_4.txt'
data.sys <- read.table(file=Filename, header=T, quote="",
                       colClasses=c("factor","factor","factor","factor","numeric","factor","factor",
                                  "factor","factor","numeric","factor","numeric"))
## normalized dspan
data.sys$dspan <- data.sys$dspan/data.sys$file.size
data.sys$file.size <- as.factor(data.sys$file.size/1024)
data.sys$disk.size <- as.factor(data.sys$disk.size/2^30)
data.sys <- as.data.frame(data.sys)

######### Analysis ######################
## fit model with only main effects
model <- lm(log2(dspan) ~ num.chunks + file.size + fullness + dir.id + num.cores + disk.used
            + disk.size + num.chunks/fsync + num.chunks/sync + num.chunks/chunk.order ,data = data.sys)
SumSq <- data.frame(anova(model)[2])
S.i <- SumSq[[1]][1:length(SumSq[[1]])]/sum(SumSq)
data.frame(SumSq,S.i)
## fit model with 2-way interactions
model <- lm(log2(dspan) ~ (num.chunks + file.size + fullness + dir.id + num.cores + disk.used
            + disk.size)^2 + num.chunks/fsync + num.chunks/sync  + num.chunks/chunk.order ,data = data.sys)
SumSq <- data.frame(anova(model)[2])
S.i <- SumSq[[1]][1:length(SumSq[[1]])]/sum(SumSq)
data.frame(SumSq,S.i)

