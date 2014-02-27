rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()
library(ggplot2)


#### Initialize Header File ####
FilePath <- '~/projects/filesystem_Spring2014'
Filename.Header <- paste('~/RScripts/HeaderFile_HW.R', sep=' ')
source(Filename.Header)
source(paste(FilePath, 'fn_Library.R', sep=''))
################################

################ Input data #################
setwd(FilePath)
Filename <- 'experiment-table.txt'
data.sys <- read.table(file=Filename, header=TRUE,sep=' ',
              colClasses=c("factor","factor",NA,"factor","factor","factor","factor"))
names(data.sys) <- c("size", "dspan", "chunk.order", "fsync", "sync", "chunk.number" )
# Should I consider size as continuous or catergorical? Do we want to include size into model?
# Consider log-transform dspan -> look much more normal
data.sys$size <- as.factor(data.sys$size)
data.sys$chunk.order <- as.factor(data.sys$chunk.order)
data.sys$fsync <- as.factor(data.sys$fsync)
data.sys$sync <- as.factor(data.sys$sync)
str(data.sys)
############### Missing Values #############
attach(data.sys)
test=NULL
for (i in levels(size)) {
  for (j in levels(chunk.order)) {
    for (k in levels(fsync)) {
      for (l in levels(sync)) {
        ind1 = which(size == i)
        ind2 = which(chunk.order[ind1]==j)
        ind3 = which(fsync[ind2]==k)
        ind4 = which(sync[ind3]==l)
        test = c(test,paste(i,j,k,l,length(ind4),sep=' '))
      }
    }
  }
}
detach(data.sys)
############### Exploratory data ###########
hist(data.sys$dspan)
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
## using ggplot2: Plot mean
df <- data.frame(sync = data.12K$sync,fsync = data.12K$fsync, dspan =data.12K$mean)
df$sync.fsync <-interaction(df$sync, df$fsync)
cutoff <- data.frame(x = c(-Inf, Inf), y =min(data.12K$mean), cutoff = factor(data.12K$mean))
ggplot(aes(y = dspan, x = sync.fsync), data = df) + geom_boxplot()    
                  #+ geom_line(aes( x, y, linetype = cutoff ), cutoff)
ggplot(aes(y = dspan, x = fsync, fill = sync), data = df) + geom_boxplot() 
                  #+geom_line(aes( x, y, linetype = cutoff ), cutoff)
## boxplot 
index.12K <-which(data.sys$size == 12288)
data.sys.12K <- data.sys[index.12K, 1:5]
df <- data.frame(sync = data.sys.12K$sync,fsync = data.sys.12K$fsync, 
                 dspan =log(data.sys.12K$dspan))
df$sync.fsync <-interaction(df$sync, df$fsync)
#cutoff <- data.frame(x = c(-Inf, Inf), y =min(log(data.sys.12K$dspan)), cutoff = factor(data.12K$mean))
ggplot(aes(y = dspan, x = fsync, fill = sync), data = df) + geom_boxplot() 
          
#index.000.011 <-which(as.numeric(data.sys.12K$sync)== 2 & as.numeric(data.sys.12K$fsync)== 1) 
#boxplot(df$dspan[index.000.011])

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
model.int.lm <- lm(log(dspan) ~ size * chunk.order * fsync * sync, data = data.sys) 
summary(model.int.lm)
# 3-way interactions are not significant. Refit the model by only including 2-way interactions
model.anova.int2 <- aov(log(dspan) ~ size + chunk.order + fsync + sync
                        + size:chunk.order + size:fsync + size:sync
                        + chunk.order:fsync + chunk.order:sync
                        + fsync:sync, data = data.sys) 
summary(model.anova.int2)
model.int2.lm <- lm(log(dspan) ~ size + chunk.order + fsync + sync
                + size:chunk.order + size:fsync + size:sync
                + chunk.order:fsync + chunk.order:sync
                + fsync:sync, data = data.sys)
summary(model.int2.lm)
# All 2-way interactions are significant. R_squared = 0.6883
## Use halfnormal plot to indicate sifnificant effects. Need to do lenth's method as well
coef <- model.anova.int2$coefficients
str(coef)
max(coef[2:311])
library(faraway)
identify(Q <- qqnorm(coef, pch=21, bg="green3", cex=1.5) )
qqline(coef, lty=2, col="red")
# Identify 12, 13, 14, 15, 187 ; 5,10,11,26,159: 
# positive effects: size12582912, size25165824,size50331648, size100663296
View(coef[c(5,10,11,12,13,14,15,26,159,187)])
View(coef[c(48,62,75,76)])
# negative effects: size196608:chunk.order"102", size196608:chunk.order"120", size98304:chunk.order"201" 
#size196608:chunk.order"201" 

############# Orthogonal Array Designs ##################
install.packages("DoE.base")
library(DoE.base)
OA <- oa.design(nfactors = 4, nlevels=c(15,6,8,4), nruns=100)
dim(OA)
View(OA)
oa.design(nfactors = 3, nlevels = 2)


