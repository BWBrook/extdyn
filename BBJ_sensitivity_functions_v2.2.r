lapply(c("parallel","sExtinct", "spatExtinct"),require,character.only=T)

### Selection of reliability vectors  - RANGES OF VALUES #############################################################################################
# A - whole reliability range (0,1), L - lower range (0,0.6), M - middle range (0.2,0.8), U - upper range (0.4,1); A is default.
set.rel <- function(rel="A") {
  return(switch(rel, "A"=c(0,1), "L"=c(0,0.6), "M"=c(0.2,0.8), "U"=c(0.4,1), c(0,1)))
} # return reliability vector, B (base) is default
###############################################################################################################################

###GENERATION OF DATASETS FOR SENSITIVITY ANALYSIS BASED ON JR2014 ##############################################################
# iter = number of simulated datasets generated, yr.rng = possible range for sighting record duration, sight.P = possible range for mean detectability
# min.S = minimum number of sightings allowed, min.rel = minimum total sighting reliability allowed
# rel = type of reliability vector, based on set.rel function, sight.type = type of sighting record (1 - constant detectability, 2 - declining detectability)
dataset.gen <- function(iter=1000, yr.rng=c(50,80), sight.P=c(0.1,0.3), rel.thresh=0.8, min.S=5, min.rel=2, rel="A", sight.type=1) {
  sight_records <- sight_reliab <- ext_year <- list() # creating empty lists for results (sight_records: sighting record, sight_reliab: sighting reliability record; ext_year <- true extinction year)
  counter <- 0 # setting the loop counter at 0
  while (counter < iter) {
    year <- round(runif(1, yr.rng[1], yr.rng[2])); endyear <- round(runif(1, (round(year/2)+1), year)) # random selection of the sighting record duration and the true extinction year, set to occur anywhere in the second part of the sighting period
    sightP <- runif(1, sight.P[1], sight.P[2]) # random selection of the sighting detectability
    ifelse(sight.type==1, sightings <- rbinom(year, 1, sightP), sightings <- rbinom(year, 1, (rep((sightP*2), times=year) - ((c(1:year)-1)/year*(sightP*2))))) # creation of the initial sighting record, based either on constant or declining detectability
    reliab <- runif(year,set.rel(rel)[1],set.rel(rel)[2])
    hypothtrue <- rbinom(year,1,reliab); alltrue <- sightings * hypothtrue # assignment of true sightings based on reliability
    allfalse <- sightings * (1 - hypothtrue)  # assignment of false sightings
    all <- allfalse; all[1:endyear] <- all[1:endyear] + alltrue[1:endyear]# combining true and false sightings, true sightings occurring after extinction year are removed from the dataset
    s.record <- all * c(1:year); s.record <- s.record[s.record>0] # final sighting record, 0's are excluded
    rel.record <- all * reliab; rel.record <- rel.record[s.record] # final reliability record, 0's are excluded
    if(length(s.record) >= min.S & sum(rel.record) >= min.rel & length(which(rel.record >= rel.thresh)) >= min.S) {
      # checkpoint whether there is sufficient number of sightings and overall reliability generated in a given iteration, if not iteration is skipped and replaced with a new one
      counter <- counter + 1
      sight_records <- c(sight_records, list(s.record))
      sight_reliab <- c(sight_reliab, list(rel.record))
      ext_year <- c(ext_year, list(endyear))
    } else {next}
  }
  return(list(sig.rec=sight_records, sig.rel=sight_reliab, e.yr=ext_year))
}
###############################################################################################################################

### TE INFERENCE - BASED ON BBJ2019 SCRIPT ####################################################################################
# iter = number of simulated datasets generated, iter.b = number of iterations per BBJ routine
# sig.rec = dataset wuth sighting records, sig.rel = dataset with sighting reliabilities, e.yr = dataset with the time of extinction for each sighting record
# alpha = significance level, ey = maximum year used for estimating confidence intervals in BBJ and BS14 methods
# rel.thresh = minimum sighting reliability accepted for standard extinction approaches, min.S = minimum number of sightings allowed
# rel = type of reliability vector, based on set.rel function
te.inference <- function(iter=100, iter.b=100, sig.rec=sight_records, sig.rel=sight_reliab, e.yr=ext_year, alpha=0.05, ey=1000, rel.thresh=0.8, min.S=5, rel="A") {

  sol.ext <- function(ts) {
    t0 <- ts[1] # record first sighting as start of observation perod
    tn <- ts[length(ts)]-t0 # last observation with respect to years since first
    n <- length(ts)-1 # exclude first sighting (start of observation period) from count
    return(round(t0+(n+1)/n*tn))
  } # Solow 1993, returning TE

  sol.ci <- function(ts) {
    t0 <- ts[1] # record first sighting as start of observation perod
    tn <- ts[length(ts)]-t0 # last observation with respect to years since first
    n <- length(ts)-1 # exclude first sighting (start of observation period) from count
    return(round(t0+(tn/(alpha^(1/n)))))
  } # Solow 1993, returning CI

  ole.ext <- function(ts) {
    gam.fit <- function(i,j,v) (gamma(2*v+i)*gamma(v+j))/(gamma(v+i)*gamma(j))
    sights <- rev(sort(ts)); k <- length(sights)
    v <- (1/(k-1))*sum(log((sights[1]-sights[k])/(sights[1]-sights[2:(k-1)])))
    lambda <- outer(1:k,1:k,gam.fit,v=v); lambda <- ifelse(lower.tri(lambda),lambda,t(lambda))
    e <- matrix(rep(1,k),ncol=1)
    a <- as.vector(solve(t(e)%*%solve(lambda)%*%e))*solve(lambda)%*%e
    return(round(sum(t(a)%*%sights)))
  } # Roberts & Solow 2003 (OLE), returning TE

  ole.ci <- function(ts) {
    sights <- rev(sort(ts)); k <- length(sights)
    v <- (1/(k-1))*sum(log((sights[1]-sights[k])/(sights[1]-sights[2:(k-1)])))
    c.alpha <- (k / -log(alpha))^(-v)
    return(round((sights[1]-(c.alpha*sights[k]))/(1-c.alpha)))
  } # Roberts & Solow 2003 (OLE), returning CI


  bbj18.sol.ext <- function(ts) {
    ext.yr <- c()
    r.rec.sol <- function(d) {
      sr <- d$year[which(d$prob>runif(length(d$year)))] # selects random time-series, based on sighting quality
      sr <- sr[order(sr)] # confirm that data are ordered sequentially
      if(length(sr)>=min.S) ext <- sol.ext(sr)
      return(ifelse(length(ext)==0,NA,ext))
    } # generates a random sighting record and returns TE (EDE depends on model-type chosen)
    for(ii in c(1:iter.b)) {ext.yr <- c(ext.yr, r.rec.sol(ts))}
    ext.yr <- na.omit(ext.yr); min.y <- round(min(unlist(ext.yr))) # exclude non-convergences, calc. first ext yr
    if(sd(ext.yr)==0) return(list(MTE=round(ext.yr[1]))) # for when only physical evidence is allowed
    ifelse((is.numeric(min.y)==FALSE | is.numeric(round(max(ext.yr)))==FALSE | min.y==round(max(ext.yr))), return(NA), freq.ext <- hist(ext.yr, breaks=round(max(ext.yr))-min.y, plot=F)) # use hist to calc. counts/freq per year
    bb <- list(); x <- min.y:ey; pp.vec <- rep(NA,length(x)); index <- 1 # initialisation of variables
    for(i in min.y:ey) {
      pp.vec[index] <- 1 - (sum(freq.ext$counts[which(freq.ext$mids<i)])/length(ext.yr))
      index <- index + 1
    } # tally cumulative proportion of TE by year since first TE
    if(round(mean(ext.yr))>=ey) {
      bbj.te <- which(pp.vec>0.5)
    } else bbj.te <- round(mean(ext.yr)) # Mean of distribution of TE
    bbj.ci <- ifelse(x[max(which(pp.vec>alpha))]==ey,NA,ifelse(length(x[max(which(pp.vec>0.05))])==0 | is.null(x[max(which(pp.vec>0.05))])==T,NA,x[max(which(pp.vec>0.05))])) # Upper 5th percentile of distribution
    return(data.frame(bbj.te,bbj.ci))
  } # BBJ computational method (Sol93 method), returning TE & CI

  bbj18.ole.ext <- function(ts) {
    ext.yr <- c()
    r.rec.ole <- function(d) {
      sr <- d$year[which(d$prob>runif(length(d$year)))] # selects random time-series, based on sighting quality
      sr <- sr[order(sr)] # confirm that data are ordered sequentially
      if(length(sr)>=min.S) ext <- ole.ext(sr)
      return(ifelse(length(ext)==0,NA,ext))
    } # generates a random sighting record and returns TE (EDE depends on model-type chosen)
    for(ii in c(1:iter.b)) {ext.yr <- c(ext.yr, r.rec.ole(ts))}
    ext.yr <- na.omit(ext.yr); min.y <- round(min(unlist(ext.yr))) # exclude non-convergences, calc. first ext yr
    if(sd(ext.yr)==0) return(list(MTE=round(ext.yr[1]))) # for when only physical evidence is allowed
    ifelse((is.numeric(min.y)==FALSE | is.numeric(round(max(ext.yr)))==FALSE | min.y==round(max(ext.yr))), return(NA), freq.ext <- hist(ext.yr, breaks=round(max(ext.yr))-min.y, plot=F)) # use hist to calc. counts/freq per year
    bb <- list(); x <- min.y:ey; pp.vec <- rep(NA,length(x)); index <- 1 # initialisation of variables
    for(i in min.y:ey) {
      pp.vec[index] <- 1 - (sum(freq.ext$counts[which(freq.ext$mids<i)])/length(ext.yr))
      index <- index + 1
    } # tally cumulative proportion of TE by year since first TE
    if(round(mean(ext.yr))>=ey) {
      bbj.te <- which(pp.vec>0.5)
    } else bbj.te <- round(mean(ext.yr)) # Mean of distribution of TE
    bbj.ci <- ifelse(x[max(which(pp.vec>alpha))]==ey,NA,ifelse(length(x[max(which(pp.vec>0.05))])==0 | is.null(x[max(which(pp.vec>0.05))])==T, NA,x[max(which(pp.vec>0.05))])) # Upper 5th percentile of distribution
    return(data.frame(bbj.te,bbj.ci))
  } # BBJ computational method (OLE method), returning TE & CI

  iter.results <- matrix(0, iter, 12)
  for (i in c(1:iter)) {
    dat.iter <- data.frame(year=unlist(sig.rec[i]),prob=unlist(sig.rel[i])) # select data
    ext <- unlist(e.yr[i])
    iter.results[i,1] <- (max(dat.iter$year)-min(dat.iter$year)); iter.results[i,2] <- ((ext-min(dat.iter$year))/(max(dat.iter$year)-min(dat.iter$year)))
    iter.results[i,3] <- length(unlist(dat.iter$year)); iter.results[i,4] <- mean(dat.iter$prob)
    if(length(which(dat.iter$prob>=rel.thresh))>=min.S) {
      dat.sel <- dat.iter$year[which(dat.iter$prob>=rel.thresh)]
      iter.results[i,5] <- sol.ext(dat.sel)-ext; iter.results[i,9] <- ifelse(sol.ci(dat.sel)>=ext, 1, 0); iter.results[i,6] <- ole.ext(dat.sel)-ext; iter.results[i,10] <- ifelse(ole.ci(dat.sel)>=ext, 1, 0)
    } else {
      iter.results[i,5:6] <- iter.results[i,12:13] <- c(NA, NA)
    }
    bbj_sol <- bbj18.sol.ext(dat.iter); bbj_ole <- bbj18.ole.ext(dat.iter)
    iter.results[i,7] <- bbj_sol[[1]]-ext; iter.results[i,11] <- ifelse(bbj_sol[[2]]>=ext, 1, 0)
    iter.results[i,8] <- bbj_ole[[1]]-ext; iter.results[i,12] <- ifelse(bbj_ole[[2]]>=ext, 1, 0)
    print(paste("progress:",i/iter*100,"%")) # computation progress indicator
  }
  colnames(iter.results) <- as.character(c("Years", "Ext_Year(%)", "N", "Mean_Realiab", "Sol_TE", "OLE_TE", "BBJ_Sol_TE", "BBJ_OLE_TE",
                                           "Sol_CI", "OLE_CI", "BBJ_Sol_CI", "BBJ_OLE_CI"))
  return(iter.results)
}

