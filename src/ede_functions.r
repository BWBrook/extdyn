## Functions for data processing and EDE estimation, BBJ19 ede method ########### 
 
### Combine repeated sightings in a year and return dataframe ###################
dmy.cprob <- function(dd,plot=T) {
  dd <- dd[order(dd$year),] # ensure data is ordered by year 
  for(i in unique(dd$year)) {
    row.sel <- which(dd$year==i)
    if(length(row.sel) > 1) {
      dd$prob[row.sel[1]] <- 1-prod(1-dd[row.sel,"prob"]) # calculate combined probability for the year
      dd <- dd[-row.sel[-1],] # eliminate all but the first row for each year
    }
  }
  # Tablulate cumulative combinatorial prob. by year, versus last confirmed sighting (similar to model 0)
  dd$cum.prob <- NA
  for(i in 1:length(dd$prob)) {
    dd$cum.prob[i] <- round(1-prod(1-dd$prob[i:length(dd$prob)]),3)
  } # calculate the cumulative probability of persistence, from last 'confirmed' year to last observation
  MTE <- ifelse(max(dd$cum.prob<=0.5),dd$year[min(which(dd$cum.prob<=0.5))],NA) # estimate median TE based on cum.prob

  if(plot==T) {
    plot(dd$cum.prob~dd$year, main="Prob. Persistence based on uncertain records",ylim=c(0,1),
         xlab="Year",ylab="cumulative probability")
    lines(dd$year,dd$cum.prob); abline(v=c(min(dd$year),max(dd$year)),col="blue",lty=2); abline(v=MTE,col="red",lty=1)
    print(dd); print(paste("MTE =",MTE))
  } # Plot the annual vector of combinatorial probabilities

  return(dd)
}
#################################################################################

### Plot moving-average of PE based on sighting reliability #####################
ma.pd <- function(dd,n=5) {
  cx <- c(0,cumsum(dd$prob)); yr <- dd$year[n:length(dd$year)]
  rsum <- (cx[(n+1):length(cx)]-cx[1:(length(cx)-n)])/n # calculate moving average of probabilities
  plot((1-rsum)~yr, main="Moving average PE based on uncertain records",ylim=c(0,1), xlab="Year",ylab="MA probability of extinction")
  lines(yr,(1-rsum)); abline(v=c(min(yr),max(yr)),col="blue",lty=2)
}
#################################################################################

### Jaric & Roberts 2014 method applied to Solow 1993 EDE #######################
jr.2014 <- function(dd,ey=2018,a=0.05,m="LAD") {

  ts <- dd$year-dd$year[1]; rs <- dd$prob; n <- length(ts) # initialise vars
  tf <- ts[1]*rs[1] # Note: if prob(rs[1]) = 1 then tf==ts[1]
  for(i in n:2) tf <- tf + ts[i]*rs[i]*prod(1-rs[(i-1):1])
  t0 <- tf+dd$year[1] # most likely first sighting year, in original units

  ts <- ts[-min(which(rs>0))]; rs <- rs[-min(which(rs>0))]; n <- n-1
  tl <- ts[n]*rs[n] # Note: if prob(t.n) = 1 then tl==t.n
  for(i in 1:(n-1)) tl <- tl + ts[i]*rs[i]*prod(1-rs[(i+1):n])

  tr <- tl-tf # most likely sighting period
  r <- sum(rs) # most likely number of observations
  To <- ey-t0 # most likley total observation period

  switch(m,
   "so93"={mod <- "Solow 1993"; MTE <- (r+1)/r*tr
      UCI <- tr/(a^(1/r)); p <- (tr/To)^r},
   "mc06"={mod <- "McInerney et al. 06"; MTE <- ceiling(tr+log(0.5,1-(r/tr)))
      UCI <- ceiling(tr+log(a,1-(r/tr))); p <- (1-(r/tr))^(To-tr)},
   "ss89"={mod <- "Strauss & Sadler 1989"; MTE <- (r*tr)/(r-1)
      UCI <- tr+(a^(-1/(r-1))-1)*tr; p <- ((To-tr)/tr+1)^-(r-1)},
   {mod <- "Most likely endpoint (LAD)"; MTE <- tr; UCI <- NA; p <- NA})

  return(list(Method=mod, MTE=round(t0+MTE),
              UCI=ifelse(is.na(UCI),NA,round(t0+UCI)), p=p))
}
#################################################################################

### EDE functions for BBJ18 Method, returning TE (additions here) ###############
rw64 <- function(ts) {
  n <- length(ts)
  return(ts[n]+(ts[n]-ts[n-1]))
} # Robson & Whitlock 1964, returning TE

so93 <- function(ts) {
  sr <- ts-ts[1]; n <- length(sr)-1
  return(ts[1]+(n+1)/n*sr[n+1])
} # Solow 1993 (+ Strauss & Sadler 1989), returning TE

mc06 <- function(ts) {
  sr <- ts-ts[1]; n <- length(sr)-1
  return(ts[1]+ceiling(sr[n+1]+log(0.5,1-(n/sr[n+1]))))
} # McInerney et al. 2006, returning TE

ole <- function(ts) {
  gam.fit <- function(i,j,v) (gamma(2*v+i)*gamma(v+j))/(gamma(v+i)*gamma(j))
  sights <- rev(sort(ts)); k <- length(sights)
  v <- (1/(k-1))*sum(log((sights[1]-sights[k])/(sights[1]-sights[2:(k-1)])))
  lambda <- outer(1:k,1:k,gam.fit,v=v); lambda <- ifelse(lower.tri(lambda),lambda,t(lambda))
  e <- matrix(rep(1,k),ncol=1)
  a <- as.vector(solve(t(e)%*%solve(lambda)%*%e))*solve(lambda)%*%e
  return(round(sum(t(a)%*%sights)))
} # Roberts & Solow 2003 OLE, returning TE
#################################################################################

### BBJ 2018 computational method applied to LAD, Solow 1993 and OLE ############
bbj.2018 <- function(dd,iter=10000,ey=2018,m="LAD",plot=T,OS="W") {

  ifelse(exists(m), ede <- match.fun(m), ede <- function(ts) max(ts)+1)

  r.rec <- function(d) {
    sr <- d$year[which(d$prob>runif(length(d$year)))] # select random SR using reliability
    ext <- if(length(sr)>2) ede(sr[order(sr)])
    return(ifelse(length(ext)==0,NA,ext))
  } # generates a random sighting record and returns TE

  if(OS=="W") {
    cl <- makeCluster(detectCores()-1); dd # run loop with parallel computing
    ext.yr <- parSapply(cl=cl, 1:iter, function(x) r.rec(dd)) # vector of TE from random subsamples
    stopCluster(cl) # stop parallel computing
  } # parallel processing for Windows OS
    else {
      ext.yr <- mclapply(1:iter,mc.cores=detectCores()-1, function(x) r.rec(dd)) # Linux multi-core option
      ext.yr <- do.call(rbind,ext.yr)
    } # parallel processing for Linux

  ext.yr <- na.omit(ext.yr); min.y <- round(min(ext.yr)) # exclude non-convergence, calc. first ext yr
  if(sd(ext.yr)==0) return(list(MTE=round(ext.yr[1]),UCI=round(ext.yr[1]),
                                PP=ifelse(round(ext.yr[1])>=ey,1,0))) # for only physical records only

  freq.ext <- hist(ext.yr, breaks=round(max(ext.yr))-min.y, plot=F) # use hist to calc. freq per year
  bbj <- list(); bbj$Method <- m; x <- min.y:ey; pp.vec <- rep(NA,length(x)); index <- 1 # initialisation of variables
  for(i in min.y:ey) {
    pp.vec[index] <- 1 - (sum(freq.ext$counts[which(freq.ext$mids<i)])/length(ext.yr))
    index <- index + 1
  } # tally cumulative proportion of TE by year since first TE

  if(round(mean(ext.yr))>=ey) {
    bbj$MTE <- which(pp.vec>0.5)
    print("Median TE reported because mean TE is later than end year")
  } else bbj$MTE <- round(mean(ext.yr)) # Mean of distribution of TE from r.rec sampling
  bbj$UCI <- ifelse(x[max(which(pp.vec>0.05))]==ey,NA,x[max(which(pp.vec>0.05))]) # Upper 5th percentile of distribution
  bbj$end_year <- pp.vec[length(x)]; names(bbj$end_year) <- paste("PP",ey) # proportion of simulations with TE>ey

  if(plot==T) {
    par(mfrow=c(1,2))
    hist(ext.yr,main="Freq. of predicted TE", xlab="Year"); abline(v=ey,col="blue",lty=2)
    plot(pp.vec~x, main="Cumulative PP", xlab="Year", ylab="Prob. persistence",xlim=c(min(x),ey-1))
    lines(x,pp.vec); abline(v=bbj$MTE,col="blue",lty=2); abline(v=bbj$UCI,col="red",lty=3)
    par(mfrow=c(1,1))
  }
  return(bbj)
}
#################################################################################

### MTE by year method using year-cutoff or jackknifing #########################
bbj.mte_yr <- function(dd,iter=10000,ey=2018,m="LAD",jk=F,srm=0,plot=T) {
  dd <- dd[which(dd$prob>=srm),]; print(dd) # exclude sightings below minimum sighting reliability
  n <- dim(dd)[1]; if(n<3) return("Too few observations!")

  if(jk==F) { # assess impact of sequentially assuming all later years are false
    last.P <- max(which(dd$prob==max(dd$prob))) # start with last 'confirmed' record
    x <- dd$year[last.P:n]
    cull.pp <- rep(NA,length(x))
    for(i in last.P:n) {
      cull.pp[i-last.P+1] <- bbj.2018(dd[1:i,],iter,ey,m,plot=F)$MTE
      if(i%%5==0) print(paste("Last Data Year =", dd$year[i], "MTE =", cull.pp[i-last.P+1]))
    }
    if(plot==T) {
      plot(cull.pp~x, main="MTE by Year", xlab="Last Data Year", ylab="MTE at Year")
      lines(x[order(x)],cull.pp[order(x)]); abline(0,1,lty=3)
    }
    return(data.frame(year=x,MTE=cull.pp))
  }

  else { # assess impact of dropping each year in turn (jackknife)
    cull.pp <- rep(NA,n-1)
    print(paste(n,"observations, testing sensitivity of MTE to dropping each in turn"))
    for(i in 1:(n-1)) {
      cull.pp[i] <- bbj.2018(dd[-i,],iter,ey,m,plot=F)$MTE
      if(i%%5==0) print(i)
    }
    if(plot==T) hist(cull.pp, main="Jackknife freq. of predicted TE", xlab="Year")
    return(list(MTE=round(median(cull.pp)),UCI=round(quantile(cull.pp,0.95))))
  }
}
#################################################################################
