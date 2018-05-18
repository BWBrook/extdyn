## Inferring extinction with mixed-reliability data using resampling - BW Brook, May 2018 ##
rm(list=ls()); options(scipen=999,warn=-1); require(parallel); source('ede_func_1.6.r'); source('ede_examples_1.6.r')

# Species examples are: BL = Barbary lion, BD = Bajj dolphin, AG = Alaotra grebe, JP = Jamaican petrel
# EC = Eskimo curlew, NU = Nukup'u, NP = Night parrot (through to 1989), IBW = Ivory-billed woodpecker (default)
# reliability vectors (physical,expert,controversial) are B=base (default), E=extreme, L=low, M=mid, H=high, P=physical only
sp <- "EC"; pv <- set.rel("B") 
end.year <- 2018; a <- 0.05 # set observation year and alpha for confidence intervals

es <- load.ede.data(eg=sp,rel=pv) # load data
dat <- dmy.cprob(data.frame(year=c(es$year.pe,es$year.ie,es$year.cs),prob=c(es$pe,es$ie,es$cs))) # pre-process data
ma.pd(dat,n=5) # plot moving-average of 1-rel as indication of persistence, based on a window of n sightings

### Use of mixed-certainty data ###
jr.2014(dat,ey=end.year,em=1) # Jaric & Roberts 2014 method for Solow 1993 (em=1) or McInerney et al 2006 (2)
bbj.2018(dat,iter=100000,em=3) # New resampling method, with TE(em=0), Solow 93 (1), McInerney et al 2006 (2) or Roberts & Solow 2003 (3)
mte.yr <- bbj.mte_yr(dat,em=3,jk=F); mte.yr # check impact of assuming all records false (true extinction) for each year after last confirmed obs (jk=F) or sensitivity of MTE to each individual sighitng (jk (jackknife)=T); sr.min = exclude obs below sr threshold

### Example of evaluation of a simulated record #########################################
# Where: sy = first observation year, exy = actual year of extinction, tsp/fsp = true/false sighting rates, pv = reliability scores
# p.type = proportion of sightings (true and false) that are based on physical or expert evidence (remainder are controversial)
s.sim <- ds.sim(exy=1950); s.sim; bbj.2018(dat, em=3)

# Note: sighting probabilities could be a function of time when there is a decline prior to extinction
s.sim <- ds.sim(sy=1900, ey=2018, exy=1950, tsp=0.2, fsp=0.1, tp=c(1,0), rel=c(1,0,0)); s.sim # example of no uncertain records
#########################################################################################
