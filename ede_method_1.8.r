## Inferring extinction with mixed-reliability data using resampling - BW Brook, May 2018 #############################
rm(list=ls()); options(scipen=999,warn=-1); require(parallel) # required for parallel computing
source('bbj18_functions_1.8.r'); source('bbj18_examples_1.8.r') # put these in the working directory

### Select sighting record data, reliability vector, observation year and upper confidence bound ######################
pv <- set.rel("B") # physical, expert, controversial: B=base(default), E=extreme, L=low, M=mid, U=upper, P=physical
sp <- "OA" # Specifiy species sighting record, as given in _examples file via two-letter code (defaults to dodo)
obs.year <- 2020; a <- 0.05 # set observation year and alpha for upper confidence interval

### Pre-process and display sighting record data ######################################################################
es <- load.ede.data(eg=sp,rel=pv) # load example data and impose selected reliability vector
dat <- dmy.cprob(data.frame(year=c(es$year.pe,es$year.ie,es$year.cs),prob=c(es$pe,es$ie,es$cs))) # pre-process data
ma.pd(dat) # plot moving-average of 1-rel as indication of persistence, based on a window of n sightings

### EDE estimates on mixed-certainty data for BBJ18 and JR14 ##########################################################
jr.2014(dat, ey=obs.year, m="so93") # Jaric & Roberts 2014 method for m = "so93", "mc06", "ss89" or "LAD" (default)

ede <- "ole" # Current methods are "rw64", so93", "mc06" and "ole" (f must take sighting record and return TE)
bbj.2018(dat, iter=10000, ey=obs.year, m=ede) # BBJ18 method: m = pre-defined EDE function to use (default: "LAD")

mte.yr <- bbj.mte_yr(dat,m=ede,jk=T); mte.yr # check sensitivity of cut-off by year, or jackknifing each obs
#######################################################################################################################
