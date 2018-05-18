## Examples for EDE models with mixed-certainty data, and reliability vectors
set.rel <- function(rel="B") {
  return(switch(rel,"E"=c(0.99,0.5,0.01),"L"=c(0.8,0.6,0.1),"M"=c(0.85,0.7,0.25),"U"=c(0.9,0.8,0.4),"P"=c(1,0,0),c(1,0.5,0.1)))
} # return reliability vector, B (base) is default

load.ede.data <- function(eg="IBW",rel=c(1,0.5,0.1)) {
  eg.dat <- list()
  if(eg=="BL") { # Barbary Lion
    eg.dat$year.pe <- c(1911,1917,1925,1934)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(1895,1900,1900,1910,1911,1911,1912,1912,1917,1920,1930,1935,1935,1942,1956)
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(1898,1901,1905,1920,1922,1929,1930,1930,1930,1935,1939,1943,1949)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  else if(eg=="BD") { # Baji dolphin
    eg.dat$year.pe <- c(1978:1999,2001)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(rep(2000,4),rep(2001,9),rep(2002,7),2003,rep(2004,3))
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(2007,2016)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  else if(eg=="AG") { # Alaotra grebe
    eg.dat$year.pe <- c(1929,1960,1963,1969)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(1972,1982,1985)
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(1947,1970,1971,1986,1988)
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
  else if(eg=="EC") { # Eskimo curlew
    eg.dat$year.pe <- c(1900:1906,1908,1911:1914,1918,1932,1962,1963)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(1926,1929,1930,1933,1937,1939,1945,1947,1948,1950,1959:1961)
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(1956,1964,1968,1970,1972:1974,1976,1977,1980:1983,1985,1987,1990,1992,1996,2002,2006)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  else if(eg=="NU") { # Nukup'u
    eg.dat$year.pe <- c(1837,1838,1888,1891:1897,1899)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(1879)
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(1960,1961,1965,1967,1968,1973:1976,1978,1979,1981,1983,1985:1996)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  else if(eg=="NP") { # Night parrot to 1989
    eg.dat$year.pe <- c(1845,1854,1872,1880,1894,1912)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(1865,rep(1929,4),1935,1943,1948)
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(1897,1901,1909,1914,1954,1959,1963,rep(1969,3),1970,1972,1978,1979,1979,1980,1980,1981,1982,1983)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  else { # Ivory-billed woodpecker
    eg.dat$year.pe <- c(1897:1902,1904:1910,1913,1914,1917,1924,1925,1932,1935,1938,1939)
    eg.dat$pe <- rep(rel[1],length(eg.dat$year.pe)) # physical
    eg.dat$year.ie <- c(1911,1916,1920,1921,1923,1926,1929,1930,1931,1933,1934,1936,1937,1941:1944)
    eg.dat$ie <- rep(rel[2],length(eg.dat$year.ie)) # expert
    eg.dat$year.cs <- c(1946,1948:1952,1955,1958,1959,1962,1966:1969,1971:1974,1976,1981,1982,1985:1988,1999,2004:2006)
    eg.dat$cs <- rep(rel[3],length(eg.dat$year.cs)) # controversial
  }
  return(eg.dat)
}

