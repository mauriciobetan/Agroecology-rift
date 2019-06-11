#This file has the code to generate the results reported in *text* in the results section.

##. ANALYSIS OF THE DATA
#load necessary libraries and custom functions
source("check_packages.R")
source("useful_functions.R")
#load the data from your output directory
load("output/analytical_data.RData")
## Figure 1. 
#. i. Fertilizer use in Cuba from 1961 to 1991.
#a) Cuba
cuba <-subset(final, country==“Cuba”)
mean(subset(cuba$fertuse, cuba$year<=1991), na.rm=TRUE)
sd(subset(cuba$fertuse, cuba$year<=1991), na.rm=TRUE)
#a.1) After USSR
mean(subset(cuba$fertuse, cuba$year>1991), na.rm=TRUE)
sd(subset(cuba$fertuse, cuba$year>1991), na.rm=TRUE)
#b) Control group
restla <-subset(final, country!=“Cuba”)
mean(subset(restla $fertuse, restla $year<=1991), na.rm=TRUE)
sd(subset(restla $fertuse, restla $year<=1991), na.rm=TRUE)
#b.1) After USSR
mean(subset(restla $fertuse, restla $year>1991), na.rm=TRUE)
sd(subset(restla $fertuse, restla $year>1991), na.rm=TRUE)
# ii. Fertilizer use through OLS regressions
#)a. Control group
model.fert <-lm(log(fertuse)~year, subset(final, country!="Cuba"))
#) a.1 Selected countries
model.ar <-lm(log(fertuse)~year, subset(final, country=="Argentina"))
model.br <-lm(log(fertuse)~year, subset(final, country=="Brazil"))
model.cr <-lm(log(fertuse)~year, subset(final, country=="Costa Rica"))
model.mx <-lm(log(fertuse)~year, subset(final, country=="Mexico"))
#) b. Cuba 
# Before USSR
model.pre <-lm(log(fertuse)~year, data=subset(final, country=="Cuba"&year<=1991))
## After USSR
model.post <-lm(log(fertuse)~year, data=subset(final, country=="Cuba"&year>1991))

## Figure 2
# a) Combined yield in Cuba
# Combine yields
final$yield <-final$ymaize+final$ybeans
cuba$yield <-cuba$ymaize + cuba$ybeans
#) a.1. Before 1991.
mean(subset(cuba$yield, cuba$year<=1991), na.rm=TRUE)
sd(subset(cuba$yield, cuba$year<=1991), na.rm=TRUE)
# a.2. After 1991
mean(subset(cuba$yield, cuba$year>1991), na.rm=TRUE)
sd(subset(cuba$yield, cuba$year>1991), na.rm=TRUE)
#  Yield trends through OLS regressions
# a) Control group
modely.cgy <-lm(log(yield)~year, data=subset(final, country!="Cuba"))
# a.1) Selected countries
modely.ary <-lm(log(yield)~year, data=subset(final, country=="Argentina"))
modely.bry <-lm(log(yield)~year, data=subset(final, country=="Brazil"))
modely.cry <-lm(log(yield)~year, data=subset(final, country=="Costa Rica"))
modely.mxy <-lm(log(yield)~year, data=subset(final, country=="Mexico"))
#) a.2) Cuba. 
# Overall 
modely.cuy <-lm(log(yield)~year, data=subset(final, country=="Cuba"))
# Before USSR
model.cuypre <-lm(log(yield)~year, data=subset(final, country=="Cuba"&year<=1991))
# After USSR 
model.cuypos <-lm(log(yield)~year, data=subset(final, country=="Cuba"&year>1991))

