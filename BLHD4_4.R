rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
########## Input data ###############
FilePath <- '../Google Drive/projects/filesystem_Spring2014/Data'
setwd(FilePath)
Filename <- 'blhd4_4.txt'
data.sys <- read.table(file=Filename, header=T, quote="",
                       colClasses=c("factor","factor","factor","factor","numeric","factor","factor",
                                  "factor","numeric","factor","numeric"))
## normalized dspan
data.sys$dspan <- data.sys$dspan/data.sys$file.size
data.sys$file.size <- as.factor(data.sys$file.size/1024)
data.sys$disk.size <- as.factor(data.sys$disk.size/2^30)
data.sys <- as.data.frame(data.sys)
