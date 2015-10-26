library(dplyr)
library(stringr)

Opt.squad<-function(data,cost,n.d=1,n.k=1,n.qb=1,n.rb=2, n.wr=3, n.te=1) {
    
    ##Create dummy variables
    dat <- as.matrix(data)
    fact.only<-data[, which(sapply(data, class)=="factor")]
    non.fact<-data[, which(sapply(data, class)!="factor")]
    dat.dummy<-model.matrix(~ Position, data=fact.only, contrasts.arg=list(Position=contrasts(fact.only$Position, contrasts=F)))
    
    
    dat2<-dat.dummy[,-1]
    colnames(dat2)<-gsub("[^#A-Za-z0-9_]","",colnames(dat2))
    dat<-cbind(non.fact,dat2)
    
    ##Pick optimum team
    f.obj <- dat$Points
    nn<-length(f.obj)
    num_play  <- rep(1,nn)
    n.choose<-n.d+n.k+n.qb+n.rb+n.wr+n.te
    f.con <- cbind(Cost=dat$Cost,dat[,colnames(dat) %in% colnames(dat2)],num_play)
    f.con<-t(f.con)
    f.dir <- c("<=","=","=","=","=","=", "=", "=")
    f.rhs <- c(cost,n.d,n.k,n.qb,n.rb,n.te,n.wr,n.choose)

    
    out1  <- lp("max", f.obj, f.con, f.dir, f.rhs,all.bin=TRUE)
    
    if (out1$status == 2) {
        print ("There was no optimal solution. Try a different approach or increase/decrease your total cost")
    } else {
        sol1  <- out1$solution
        total1 <- out1$objval 
        ###Take numbers and take out 
        pick1a <- rep(0,15)
        k     <- 0
        f.obj2<-f.obj
        for (j in 1:nn) {
            if (sol1[j]==1) {
                k        <- k+1
                pick1a[k] <- j
            }
        }
        final.pick<-dat[pick1a, 1:3]
        print(total1)
        final.pick
    }
}

Opt.squad(nf, 60000)
