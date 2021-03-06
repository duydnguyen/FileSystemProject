rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
library(ggplot2)


#### Initialize Header File ####
FilePath <- '~/projects/filesystem_Spring2014/Data/'
################################

################ Input data #################
setwd(FilePath)
Filename <- '3chunks.txt'
data.sys <- read.table(file=Filename, header=TRUE,sep=' ',
                       colClasses=c("character","factor",NA,"factor","factor","factor","character"))
names(data.sys) <- c("runs","size", "dspan", "chunk.order", "fsync", "sync", "chunk.number" )

# Should I consider size as continuous or catergorical? Do we want to include size into model?
# Consider log-transform dspan -> look much more normal
#data.sys$size <- as.factor(data.sys$size)
#data.sys$chunk.order <- as.factor(data.sys$chunk.order)
#data.sys$fsync <- as.factor(data.sys$fsync)
#data.sys$sync <- as.factor(data.sys$sync)
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
## Other factors look fine. Based on the boxplot, factors fsync, chunk.order, sync do not 
## significantly change log(dspan). Check resid R^2
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
############### Initial Analysis:  ##############
## ANOVA with no interactions
model.anova <- aov(log(dspan) ~ size + chunk.order + fsync + sync, data = data.sys) 
summary(model.anova)
## ANOVA with interactions
model.anova.int <- aov(log(dspan) ~ size * chunk.order * fsync * sync, data = data.sys) 
summary(model.anova.int)
model.int.lm <- lm(log(dspan) ~ size * chunk.order * fsync * sync, data = data.sys) 
summary(model.int.lm)
# Find some effects that are "significant" from FULL model
store <- model.int.lm$coef[ abs(model.int.lm$coef) >9.905 ]
View(store)
# 3-way interactions are not significant. Refit the model by only including 2-way interactions
## ANOVA with 2-interactions
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
coef <- model.anova.int2$coefficients
str(coef)
max(coef[2:311])
library(faraway)
identify(Q <- qqnorm(coef, pch=21, bg="green3", cex=1.5) )
qqline(coef, lty=2, col="red")
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

########### ANOVA with 2 & 3 interactions; size as blocks ######
model.anova.int3 <- aov(log2(dspan) ~ size + chunk.order + fsync + sync
                        + size:chunk.order + size:fsync + size:sync
                        + chunk.order:fsync + chunk.order:sync + fsync:sync
                        + size:chunk.order:fsync + size:chunk.order:sync + size:fsync:sync
                        + chunk.order:fsync:sync 
                        ,data = data.sys) 
summary(model.anova.int3)
model.int3.lm <- lm(log2(dspan) ~ size + chunk.order + fsync + sync
                     + size:chunk.order + size:fsync + size:sync
                     + chunk.order:fsync + chunk.order:sync + fsync:sync +
                     + size:chunk.order:fsync + size:chunk.order:sync + size:fsync:sync
                     + chunk.order:fsync:sync 
                     ,data = data.sys)
## Effect coding
model.int3.lm <- lm(log2(dspan) ~ size + chunk.order + fsync + sync
                    + size:chunk.order + size:fsync + size:sync
                    + chunk.order:fsync + chunk.order:sync + fsync:sync +
                      + size:chunk.order:fsync + size:chunk.order:sync + size:fsync:sync
                    + chunk.order:fsync:sync,
      contrasts = list(size = contr.sum, chunk.order = contr.sum, fsync = contr.sum,
                  sync = contr.sum),
                    data = data.sys)

summary(model.int3.lm) 
out <- capture.output(summary(model.int3.lm))
cat(out,file="out_3chunks.txt",sep="\n",append=TRUE)
# All main effects, 2-, 3- interactions are significant. Moving from full -> reduced 
#(2880 para -> 1410 para) R^2 only reduces to 0.9487
## Diagnotic Plots:
layout(matrix(c(1,2), nrow=1, ncol=2))
plot(model.int3.lm$fitted, model.int3.lm$res)
qqnorm(model.int3.lm$res)
qqline(model.int3.lm$res)
## Significant effects using qqplot
coef.int3 <- model.int3.lm$coefficients[2:length(model.int3.lm$coefficients)]
library(faraway)
qqline(coef.int3, lty=2, col="red")
identify(Q <- qqnorm(coef.int3, pch=21, bg="green3", cex=1.5) )
# Pick the first 114 most significant effects
store.int3 <- model.int3.lm$coef[ abs(model.int3.lm$coef) >5 ]
View(store.int3)
View(store)
#### Significant effects using Lenth's method
# Compute the median of |effect_i|
med <- median(abs(coef.int3))
# Compute s0
s0 <- 1.5*med
# trimming constant = 2.5 * s0
# compute pseudo standard error PSE
PSE <- 1.5 * median(abs(coef.int3[which(abs(coef.int3) < 2.5*s0)]))
# Compute t.PSE
t.PSE <- coef.int3 / PSE
# IER =???for I=1409 at level 0.01 (Appendix H in Wu&Hamada) : No available value
## Using BsMD package
library(BsMD)
Lenth <- LenthPlot(coef.int3, alpha = 0.01, plt = TRUE, limits = TRUE, 
          xlab = "factors", ylab = "effects")

effect.sig <- coef.int3[which(abs(t.PSE) > Lenth[4])]
length(effect.sig) #885
View(effect.sig)
############# Orthogonal Array Designs ##################
install.packages("DoE.base")
library(DoE.base)
OA <- oa.design(nfactors = 4, nlevels=c(15,6,8,4), nruns=100)
dim(OA)
View(OA)
oa.design(nfactors = 3, nlevels = 2)


