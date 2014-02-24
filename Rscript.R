rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()
install.packages("Rcmdr")
library(Rcmdr)


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
# Should I consider size as continuous or catergorical? Do we want to include size into model?
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
## plot mean log(dspan) for each group size
agg1 <- aggregate(log(data.sys$dspan), by=list(data.sys$size, data.sys$fsync, 
                                                data.sys$sync), FUN=mean)
names(agg1) <- c("size", "fsync", "sync", "mean")
data.12K <- agg1[agg1$size == 12288, 1:4]
data.12K[which(data.12K$mean < 9.5),1:4]
## smallest dspan config: sync=001 and fsync=000,001,011,101,111
data.24K <- agg1[agg1$size == 24576, 1:4]
min(data.24K$mean)
data.24K[which(data.24K$mean < 11),1:4]
## smallest dspan config: sync=001 and fsync=000,001,011,101,111

##6 Interaction plots: interactions are significant 
layout(matrix(c(1:6),nrow = 3, ncol = 2))
interaction.plot(data.sys$size, data.sys$chunk.order, log(data.sys$dspan))
interaction.plot(data.sys$size, data.sys$fsync, log(data.sys$dspan))
interaction.plot(data.sys$size, data.sys$sync, log(data.sys$dspan))
interaction.plot(data.sys$chunk.order, data.sys$fsync, log(data.sys$dspan))
interaction.plot(data.sys$chunk.order, data.sys$sync, log(data.sys$dspan))
interaction.plot(data.sys$fsync, data.sys$sync, log(data.sys$dspan))
############### Initial Analysis:  ###########
## ANOVA with no interactions
model.anova <- aov(log(dspan) ~ size + chunk.order + fsync + sync, data = data.sys) 
summary(model.anova)
## ANOVA with interactions
model.anova.int <- aov(log(dspan) ~ size * chunk.order * fsync * sync, data = data.sys) 
summary(model.anova.int)
# 3-way interactions are not significant. Refit the model by only including 2-way interactions
model.anova.int2 <- aov(log(dspan) ~ size + chunk.order + fsync + sync
                        + size:chunk.order + size:fsync + size:sync
                        + chunk.order:fsync + chunk.order:sync
                        + fsync:sync, data = data.sys) 
summary(model.anova.int2)
# All 2-way interactions are significant



