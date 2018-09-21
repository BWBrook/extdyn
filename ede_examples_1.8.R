## Examples for EDE models with mixed-certainty data, and reliability vectors ###

### Set reliability vector, B (base) is default #################################
set.rel <- function(rel="B") {
  return(switch(rel,"E"=c(0.99,0.5,0.01),"L"=c(0.8,0.6,0.1),"M"=c(0.85,0.7,0.25),
                "U"=c(0.9,0.8,0.4),"P"=c(1,0,0),c(0.95,0.5,0.1)))
}
#################################################################################

### Load example data file based on physical, expert and controversial SR #######
load.ede.data <- function(eg="IW",rel=c(0.95,0.5,0.1)) {
  eg.dat <- list()
## Table 1 and Fig 2: four exemplar species
  if(eg=="BL") { # Barbary Lion (mammal, rare)
    eg.dat$year.pe <- c(1911,1917,1925,1934)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(1895,1900,1900,1910,1911,1911,1912,1912,1917,1920,1930,1935,1935,1942,1956)
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(1898,1901,1905,1920,1922,1929,1930,1930,1930,1935,1939,1943,1949)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  else if(eg=="BD") { # Baji dolphin (mammal, declining population)
    eg.dat$year.pe <- c(1978:1999,2001)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(rep(2000,4),rep(2001,9),rep(2002,7),2003,rep(2004,3))
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(2007,2016)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  else if(eg=="IW") { # Ivory-billed woodpecker (bird, famous example, possibly 2005 rediscovery)
    eg.dat$year.pe <- c(1897:1902,1904:1910,1913,1914,1917,1924,1925,1932,1935,1938,1939)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(1911,1916,1920,1921,1923,1926,1929,1930,1931,1933,1934,1936,1937,1941:1944)
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(1946,1948:1952,1955,1958,1959,1962,1966:1969,1971:1974,1976,1981,1982,1985:1988,1999,2004:2006)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  else if(eg=="NU") { # Nukupu'u o'ahu (bird, Hawaiian honeycreeper, over a century since last physical record)
    eg.dat$year.pe <- c(1837,1838,1888,1891:1897,1899)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(1879)
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(1960,1961,1965,1967,1968,1973:1976,1978,1979,1981,1983,1985:1996)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
## Lazarus Taxa examples (rediscoveries after long period of absence, good pre-disappearance sighting record)
  else if(eg=="NP") { # Night parrot to 1989 (confirmed 1990)
    eg.dat$year.pe <- c(1845,1854,1872,1880,1894,1912)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(1865,rep(1929,4),1935,1943,1948)
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(1897,1901,1909,1914,1954,1959,1963,rep(1969,3),1970,1972,1978,1979,1979,1980,1980,1981,1982,1983)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  else if(eg=="SB") { # Noisy scrub bird to 1960 (confirmed 1961)
    eg.dat$year.pe <- c(1842,1882,1889)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(1843,1866,1883)
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(1897,1909)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
## Extinct taxa (high chance of recent extinction as confirmed by factors beyond the sighting record)
  else if(eg=="BC") { # Bramble Cay melomys, extinct in 2014
    eg.dat$year.pe <- c(1845,1922,1924,1978,1998,2002,2004)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(1995)
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(2009)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
    else if(eg=="AG") { # Alaotra grebe, declared extinct in 2010
    eg.dat$year.pe <- c(1929,1960,1963,1969)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(1972,1982,1985)
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(1947,1970,1971,1986,1988)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
##########################################################################################################
## Below are other well-known examples with mixed-certainty records and some doubt about extinction status
  else if(eg=="PC") { # Scottish polecat (confirmed 2004?)
    eg.dat$year.pe <- c(1903,1905,1906,1907,1914)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(1912,1916,1928)
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c()
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  else if(eg=="CM") { # Caribbean monk seal
    eg.dat$year.pe <- c(1915,1922,1932,1948,1952)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c()
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(1985,1987,1996,1997)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  else if(eg=="EC") { # Eskimo curlew
    eg.dat$year.pe <- c(1900:1906,1908,1911:1914,1918,1932,1962,1963)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(1926,1929,1930,1933,1937,1939,1945,1947,1948,1950,1959:1961)
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(1956,1964,1968,1970,1972:1974,1976,1977,1980:1983,1985,1987,1990,1992,1996,2002,2006)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  else if(eg=="OA") { # O'ahu 'Alauahio
    eg.dat$year.pe <- c(1837,1888,1891:1893,1896,1897,1901:1903,1950,1968)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(1936,1937,1939,1940,1946:1949)
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(1952,1955:1958,1960,1961,1964,1966,1968,1969,1972:1978,1984,1985,1989:1991,1993,1996,1997,2000:2002)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  else if(eg=="JP") { # Jamaican petrel
    eg.dat$year.pe <- c(1829,1866,1879)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(1891)
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(1789,1847)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  else if(eg=="MS") { # Pohnpei mountain starling
    eg.dat$year.pe <- c(1930,1995)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c()
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(2008)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  else if(eg=="RC") { # Miss Waldron's Red Colobus monkey
    eg.dat$year.pe <- c(1912,1933,1934,1952,1953,1954,1954,2001)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c()
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c()
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  else if(eg=="BF") { # Black-footed ferret, time in months
    eg.dat$year.pe <- c(6,7,8,10,17,18,19,20,21,22,30,31,41,44,46,53,57,58,66,102,129,130,134,135,139,151,163,165)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c()
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c()
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  else { # Dodo (default if nothing selected for invalid species code given)
    eg.dat$year.pe <- c(1598,1601,1602,1607,1611,1628,1628,1631,1638,1662)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c()
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(1674,1688)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  return(eg.dat)
}
#################################################################################