rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
########## Input data ###############
FilePath <- '../Google Drive/projects/filesystem_Spring2014/Data'
setwd(FilePath)
Filename <- 'nested4_4.txt'
data.sys <- read.table(file=Filename, header=T, quote="",
                       colClasses=c("factor","factor","factor","factor","numeric","factor","factor",
                                  "factor","factor","factor","factor","numeric"))
## log2 (normalized dspan)
data.sys$dspan <- data.sys$dspan/data.sys$file.size
data.sys$file.size <- as.factor(data.sys$size/1024)
data.sys <- as.data.frame(data.sys)
