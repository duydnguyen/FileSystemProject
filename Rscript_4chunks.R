rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))

#### Initialize Header File ####
FilePath <- '~/projects/filesystem_Spring2014/Data/'
################################

################ Input data #################
setwd(FilePath)
Filename <- '4chunks.txt'
data.sys <- read.table(file=Filename, header=TRUE,sep=' ',
                      colClasses=c("factor",NA,"factor","factor","factor","character"))
names(data.sys) <- c("size", "dspan", "chunk.order", "fsync", "sync", "chunk.number" )
########### Full Model #########################################
model.int.lm <- lm(log(dspan) ~ size * chunk.order * fsync * sync, data = data.sys) 
summary(model.int.lm)
# Find some effects that are "significant" from FULL model
store <- model.int.lm$coef[ abs(model.int.lm$coef) >9.905 ]
#View(store)

########### ANOVA with 2 & 3 interactions; size as blocks ######
model.int3.lm <- lm(log(dspan) ~ size + chunk.order + fsync + sync
                    + size:chunk.order + size:fsync + size:sync
                    + chunk.order:fsync + chunk.order:sync + fsync:sync +
                      + size:chunk.order:fsync + size:chunk.order:sync + size:fsync:sync
                    + chunk.order:fsync:sync 
                    ,data = data.sys)
summary(model.int3.lm) 
# Pick the first 114 most significant effects
store.int3 <- model.int3.lm$coef[ abs(model.int3.lm$coef) >5 ]
## Write summary() to a file
out <- capture.output(summary(model.int3.lm))
cat(out,file="out_4chunks.txt",sep="\n",append=TRUE)



