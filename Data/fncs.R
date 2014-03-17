################ Input data #################
Filename <- '4chunks_v1.txt'
data.sys <- read.table(file=Filename, header=TRUE,sep=' ',
                         colClasses=c("numeric",NA,"factor","factor","factor","character"))
names(data.sys) <- c("size", "dspan", "chunk.order", "fsync", "sync", "chunk.number" )
## normalize dspan
data.sys$dspan <- data.sys$dspan/data.sys$size
data.sys$size <- as.factor(data.sys$size/1024)
data.sys <- as.data.frame(data.sys)

############## Function to decompose factors ###################
EvalFac <- function(f, name){
    str <- as.matrix(f)
    n.chunk <- nchar(str[1])
    n.run <- length(str) 
    f.eval <- matrix(c(rep(0,n.chunk*n.run)), nrow = n.run, ncol = n.chunk)
    
    for (i in 1:n.run)
        for (j in 1:n.chunk)
            f.eval[i,j] <- as.numeric(substr(str[i],j,j)) 
    
    
    f.eval <- as.data.frame(f.eval)
    
    for (j in 1:n.chunk) names(f.eval)[j] <- paste(name,j,sep='')
    return(f.eval)
}
#factor <- EvalFac(f = data.12$fsync, "fsync")
############### Function to do model selection given filesize in KB #######
MSelect <- function(KB){
    data.size <- subset(data.sys, size == KB)
    factor.fsync <- EvalFac(f = data.size$fsync, "fsync")
    factor.sync <- EvalFac(f = data.size$sync, "sync")
    data.sizef <- data.frame(data.size$dspan, data.size$chunk.order, factor.fsync, factor.sync[,-dim(factor.sync)[2]])
    names(data.sizef)[1] <- c("ndspan")
    names(data.sizef)[2] <- c("c.order")
    data.sizef[,3:dim(data.sizef)[2]] <- lapply(data.sizef[,3:dim(data.sizef)[2]], factor)
    # Code Effects
    lm.str <- "ndspan ~ "
    fac.names <- names(data.sizef)[2:length(data.sizef)]
    for (i in 3:length(data.sizef)){
      lm.str <- paste(lm.str, fac.names[i-1],sep=" ")   
      lm.str <- paste(lm.str, " +", sep="")
    }
    lm.str <- paste(lm.str,fac.names[1], "+ .^2. + .^3.",sep=' ')
    # Fit model with 2 & 3-interactions
    model.sizef <- lm(lm.str, contrasts = list(c.order = contr.sum, fsync1 = contr.sum, fsync2 = contr.sum, 
                                      fsync3 = contr.sum, sync1 = contr.sum, sync2 = contr.sum),data = data.sizef)
    # Backward Stepwise BIC
    null <- lm(ndspan ~ 1 ,data = data.sizef)
    smodel.sizef <- step(null, scope= list(lower=null, upper=model.sizef), direction="both", k=log(dim(data.size)[1]))
    
    coef <- smodel.sizef$coefficients
    ##get anova()
    #return(smodel.sizef)
    return(coef)
    #return(smodel.sizef[1:length(smodel.sizef)])
    
}

#MSelect(12)



