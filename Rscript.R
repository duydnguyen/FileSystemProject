rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()


#### Initialize Header File ####
FilePath <- '~/projects/filesystem_Spring2014'
Filename.Header <- paste('~/RScripts/HeaderFile_HW.R', sep='')
source(Filename.Header)
source(paste(FilePath, 'fn_Library.R', sep=''))
################################

################ Input data #################
setwd(FilePath)
Filename <- 'experiment-table.txt'
data.sys <- read.table(file=Filename, header=TRUE, quote="'")
names(data.sys) <- c("size", "dspan", "chunk.order", "fsync", "sync", "chunk.number" )
# Should I consider size as continuous or catergorical?
# Consider log-transform dspan -> look much more normal
data.sys$size <- as.factor(data.sys$size)
data.sys$chunk.order <- as.factor(data.sys$chunk.order)
data.sys$fsync <- as.factor(data.sys$fsync)
data.sys$sync <- as.factor(data.sys$sync)
str(data.sys)
############### Exploratory data ###########
hist(log(data.sys$dspan))
layout(matrix(c(1:4), nrow = 2, ncol = 2))
plot(log(dspan) ~ size + chunk.order + fsync + sync,data = data.sys)
## Do we want factor size in our model? Size seems to have a problem with homogeineity. 
## Other factors look fine. Based on the boxplot, factors fsync, chunk.order, sync do not 
## significantly change log(dspan)

############### Initial Analysis:  ###########



