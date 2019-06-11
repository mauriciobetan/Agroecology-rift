#' ---
#' title: "organize_data.R"
#' author: "Mauricio Betancourt"
#' ---

# This script will read in raw data from the input directory, clean it up to produce 
# the analytical dataset, and then write the analytical data to the output directory. 

#source in any useful functions
source("check_packages.R")
source("useful_functions.R")

#####################################################
#. FAO DATA. 
## I. SYNTHETIC NITROGEN FERTILIZER EMISSIONS (CO2EQ)

sfertCO2eeq <- read_excel("input/FAO/sfertCO2eeq.xlsx")

#) a.0) If needed, Change name "Area" for "Country"

colnames(sfertCO2eeq)[colnames(sfertCO2eeq)=="Area"] <- "Country"

#) a) change to relevant names

##) a.1. Bolivia
sfertCO2eeq$Country <-gsub("Bolivia (Plurinational State of)", "Bolivia", 
                           sfertCO2eeq$Country, fixed=TRUE)
##) a.2. Venezuela
sfertCO2eeq$Country <-gsub("Venezuela (Bolivarian Republic of)", "Venezuela", 
                           sfertCO2eeq$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
sfertCO2eeq <-subset(sfertCO2eeq, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                       Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                       Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                       Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                       Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                       Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                       Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

#) Remove unnecessary columns
sfertCO2eeq[,1:3]<- list(NULL)
sfertCO2eeq[,2:6]<- list(NULL)
sfertCO2eeq[,3]<- list(NULL)
sfertCO2eeq[,4:5]<- list(NULL)

##) Put final name to the column:

colnames(sfertCO2eeq) <-c("country","year","sfertCO2eeq")

## II. PRODUCTION. YIELD BY CROP 

foodyield <- read_excel("input/FAO/foodyield.xlsx")

#) a.0) If needed, Change name "Area" for "Country"

colnames(foodyield)[colnames(foodyield)=="Area"] <- "Country"

#) a) change to relevant names

##) a.1. Bolivia
foodyield$Country <-gsub("Bolivia (Plurinational State of)", "Bolivia", 
                         foodyield$Country, fixed=TRUE)
##) a.2. Venezuela
foodyield$Country <-gsub("Venezuela (Bolivarian Republic of)", "Venezuela", 
                         foodyield$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
foodyield <-subset(foodyield, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                     Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                     Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                     Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                     Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                     Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                     Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

#) Remove unnecessary columns
foodyield[,1:3]<- list(NULL)
foodyield[,2:4]<- list(NULL)
foodyield[,3]<- list(NULL)
foodyield[,4]<- list(NULL)
foodyield[,5:6]<- list(NULL)

##) C) Spread into item
foodyield <- spread(foodyield, Item, Value)

##) Put final name to the column:

colnames(foodyield) <-c("country","year","ybeans","ymaize","yrice","ysgcane")

## III. MACHINERY. AGRIGULTURAL TRACTORS. WB data format.

tractoruse <- read_excel("input/WB/tractorrel.xls")

##)A) Remove unwanted columns
tractoruse[,2:5]<- list(NULL)
##) B) Remove the firs three rows
tractoruse <- tractoruse[-c(1, 2, 3), ]
## C) Change "Data Source" for "Country"
colnames(tractoruse) <-c("Country", "1961","1962","1963","1964","1965","1966","1967","1968","1969","1970",
                           "1971","1972","1973","1974","1975","1976","1977","1978","1979","1980","1981",
                           "1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992",
                           "1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003",
                           "2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014",
                           "2015","2016","2017")

##) a.1. Bolivia
tractoruse$Country <-gsub("Bahamas, The", "Bahamas", 
                          tractoruse$Country, fixed=TRUE)
##) a.2. Venezuela
tractoruse$Country <-gsub("Venezuela, RB", "Venezuela", 
                            tractoruse$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
tractoruse <-subset(tractoruse, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                        Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                        Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                        Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                        Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                        Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                        Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

##) c) Put in the FAO's column format

tractoruse <-gather(tractoruse, Year, Value, 2:58)

## d) Colnames
colnames(tractoruse) <-c("country","year","tractoruse")

## IV. ENERGY USE IN AGRICULTURE 

Euseagri <- read_excel("input/FAO/Euseagri.xlsx")

#) a.0) If needed, Change name "Area" for "Country"

colnames(Euseagri)[colnames(Euseagri)=="Area"] <- "Country"

#) a) change to relevant names

##) a.1. Bolivia
Euseagri$Country <-gsub("Bolivia (Plurinational State of)", "Bolivia", 
                        Euseagri$Country, fixed=TRUE)
##) a.2. Venezuela
Euseagri$Country <-gsub("Venezuela (Bolivarian Republic of)", "Venezuela", 
                        Euseagri$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
Euseagri <-subset(Euseagri, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                    Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                    Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                    Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                    Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                    Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                    Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

#) Remove unnecessary columns
Euseagri[,1:3]<- list(NULL)
Euseagri[,2:6]<- list(NULL)
Euseagri[,3]<- list(NULL)
Euseagri[,4:5]<- list(NULL)

##) Put final name to the column:

colnames(Euseagri) <-c("country","year","Euseagri")

##. V FERTILIZER USE

fertuse <- read_excel("input/FAO/fertuse.xlsx")

#) a.0) If needed, Change name "Area" for "Country"

colnames(fertuse)[colnames(fertuse)=="Area"] <- "Country"

#) a) change to relevant names

##) a.1. Bolivia
fertuse$Country <-gsub("Bolivia (Plurinational State of)", "Bolivia", 
                       fertuse$Country, fixed=TRUE)
##) a.2. Venezuela
fertuse$Country <-gsub("Venezuela (Bolivarian Republic of)", "Venezuela", 
                       fertuse$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
fertuse <-subset(fertuse, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                   Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                   Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                   Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                   Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                   Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                   Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

#) Remove unnecessary columns
fertuse[,1:3]<- list(NULL)
fertuse[,2:6]<- list(NULL)
fertuse[,3]<- list(NULL)
fertuse[,4:5]<- list(NULL)

##) Put final name to the column:

colnames(fertuse) <-c("country","year","fertuse")

##. VI EMISSIONS CO2 AGRICULTURE 
agriCO2 <- read_excel("input/FAO/agriCO2.xlsx")

#) a.0) If needed, Change name "Area" for "Country"

colnames(agriCO2)[colnames(agriCO2)=="Area"] <- "Country"

#) a) change to relevant names

##) a.1. Bolivia
agriCO2$Country <-gsub("Bolivia (Plurinational State of)", "Bolivia", 
                       agriCO2$Country, fixed=TRUE)
##) a.2. Venezuela
agriCO2$Country <-gsub("Venezuela (Bolivarian Republic of)", "Venezuela", 
                       agriCO2$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
agriCO2 <-subset(agriCO2, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                   Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                   Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                   Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                   Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                   Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                   Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

#) Remove unnecessary columns
agriCO2[,1:3]<- list(NULL)
agriCO2[,2:6]<- list(NULL)
agriCO2[,3]<- list(NULL)
agriCO2[,4:5]<- list(NULL)

##) Put final name to the column:

colnames(agriCO2) <-c("country","year","agriCO2")

## VII. LANDSHARE

landshare <- read_excel("input/FAO/landshare.xlsx")

#) a.0) If needed, Change name "Area" for "Country"

colnames(landshare)[colnames(landshare)=="Area"] <- "Country"

#) a) change to relevant names

##) a.1. Bolivia
landshare$Country <-gsub("Bolivia (Plurinational State of)", "Bolivia", 
                         landshare$Country, fixed=TRUE)
##) a.2. Venezuela
landshare$Country <-gsub("Venezuela (Bolivarian Republic of)", "Venezuela", 
                         landshare$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
landshare <-subset(landshare, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                     Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                     Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                     Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                     Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                     Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                     Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

#) Remove unnecessary columns
landshare[,1:3]<- list(NULL)
landshare[,2:4]<- list(NULL)
landshare[,3]<- list(NULL)
landshare[,4]<- list(NULL)
landshare[,5:6]<- list(NULL)

##) C) Spread into item
landshare <- spread(landshare, Item, Value)

##) Put final name to the column:

colnames(landshare) <-c("country","year","agriland","forland")

##. VIII. EMISSIONS INTENSITY. CEREALS EXCLUDING RICE. kg CO2eq/kg product

emiintensity <- read_excel("input/FAO/emiintensity.xlsx")

#) a.0) If needed, Change name "Area" for "Country"

colnames(emiintensity)[colnames(emiintensity)=="Area"] <- "Country"

#) a) change to relevant names

##) a.1. Bolivia
emiintensity$Country <-gsub("Bolivia (Plurinational State of)", "Bolivia", 
                            emiintensity$Country, fixed=TRUE)
##) a.2. Venezuela
emiintensity$Country <-gsub("Venezuela (Bolivarian Republic of)", "Venezuela", 
                            emiintensity$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
emiintensity <-subset(emiintensity, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                        Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                        Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                        Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                        Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                        Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                        Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

#) Remove unnecessary columns
emiintensity[,1:3]<- list(NULL)
emiintensity[,2:6]<- list(NULL)
emiintensity[,3]<- list(NULL)
emiintensity[,4:5]<- list(NULL)

##) Put final name to the column:

colnames(emiintensity) <-c("country","year","emintensity")

##. IX MANURE APPLIED TO SOILS (N CONTENT). ALL ANIMALS. KG.  

manure <- read_excel("input/FAO/manure.xlsx")

#) a.0) If needed, Change name "Area" for "Country"

colnames(manure)[colnames(manure)=="Area"] <- "Country"

#) a) change to relevant names

##) a.1. Bolivia
manure$Country <-gsub("Bolivia (Plurinational State of)", "Bolivia", 
                      manure$Country, fixed=TRUE)
##) a.2. Venezuela
manure$Country <-gsub("Venezuela (Bolivarian Republic of)", "Venezuela", 
                      manure$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
manure <-subset(manure, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                  Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                  Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                  Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                  Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                  Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                  Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

#) Remove unnecessary columns
manure[,1:3]<- list(NULL)
manure[,2:6]<- list(NULL)
manure[,3]<- list(NULL)
manure[,4:5]<- list(NULL)

##) Put final name to the column:

colnames(manure) <-c("country","year","manure")

##. X FOOD SUPPLY. kcal/capita/day  

foodsupply <- read_excel("input/FAO/foodsupply.xlsx")

#) a.0) If needed, Change name "Area" for "Country"

colnames(foodsupply)[colnames(foodsupply)=="Area"] <- "Country"

#) a) change to relevant names

##) a.1. Bolivia
foodsupply$Country <-gsub("Bolivia (Plurinational State of)", "Bolivia", 
                          foodsupply$Country, fixed=TRUE)
##) a.2. Venezuela
foodsupply$Country <-gsub("Venezuela (Bolivarian Republic of)", "Venezuela", 
                          foodsupply$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
foodsupply <-subset(foodsupply, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                      Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                      Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                      Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                      Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                      Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                      Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

#) Remove unnecessary columns
foodsupply[,1:3]<- list(NULL)
foodsupply[,2:6]<- list(NULL)
foodsupply[,3]<- list(NULL)
foodsupply[,4:5]<- list(NULL)

##) Put final name to the column:

colnames(foodsupply) <-c("country","year","foodsup")

################################. WB DATA.
## XII. ARABLE LAND (HECTARES)

wbarableland <- read_excel("input/WB/wbarableland.xlsx")

##)A) Remove unwanted columns
wbarableland[,2:5]<- list(NULL)
##) B) Remove the firs three rows
wbarableland <- wbarableland[-c(1, 2, 3), ]
## C) Change "Data Source" for "Country"
colnames(wbarableland) <-c("Country", "1961","1962","1963","1964","1965","1966","1967","1968","1969","1970",
                           "1971","1972","1973","1974","1975","1976","1977","1978","1979","1980","1981",
                           "1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992",
                           "1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003",
                           "2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014",
                           "2015","2016","2017")

##) a.1. Bolivia
wbarableland$Country <-gsub("Bahamas, The", "Bahamas", 
                            wbarableland$Country, fixed=TRUE)
##) a.2. Venezuela
wbarableland$Country <-gsub("Venezuela, RB", "Venezuela", 
                            wbarableland$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
wbarableland <-subset(wbarableland, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                        Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                        Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                        Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                        Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                        Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                        Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

##) c) Put in the FAO's column format

wbarableland <-gather(wbarableland, Year, Value, 2:58)

## d) Colnames
colnames(wbarableland) <-c("country","year","arabland")

## XII. FOOD EXPORTS (% of merchandise exports)

wbfoodexports <- read_excel("input/WB/wbfoodexports.xlsx")

##)A) Remove unwanted columns
wbfoodexports[,2:5]<- list(NULL)
##) B) Remove the firs three rows
wbfoodexports <- wbfoodexports[-c(1, 2, 3), ]
## C) Change "Data Source" for "Country"
colnames(wbfoodexports) <-c("Country", "1961","1962","1963","1964","1965","1966","1967","1968","1969","1970",
                            "1971","1972","1973","1974","1975","1976","1977","1978","1979","1980","1981",
                            "1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992",
                            "1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003",
                            "2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014",
                            "2015","2016","2017")

##) a.1. Bolivia
wbfoodexports$Country <-gsub("Bahamas, The", "Bahamas", 
                             wbfoodexports$Country, fixed=TRUE)
##) a.2. Venezuela
wbfoodexports$Country <-gsub("Venezuela, RB", "Venezuela", 
                             wbfoodexports$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
wbfoodexports <-subset(wbfoodexports, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                         Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                         Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                         Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                         Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                         Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                         Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

##) c) Put in the FAO's column format

wbfoodexports <-gather(wbfoodexports, Year, Value, 2:58)

## d) Colnames
colnames(wbfoodexports) <-c("country","year","foodexp")

## XIII. FOOD IMPORTS (% of merchandise imports)

wbfoodimports <- read_excel("input/WB/wbfoodimports.xlsx")

##)A) Remove unwanted columns
wbfoodimports[,2:5]<- list(NULL)
##) B) Remove the firs three rows
wbfoodimports <- wbfoodimports[-c(1, 2, 3), ]
## C) Change "Data Source" for "Country"
colnames(wbfoodimports) <-c("Country", "1961","1962","1963","1964","1965","1966","1967","1968","1969","1970",
                            "1971","1972","1973","1974","1975","1976","1977","1978","1979","1980","1981",
                            "1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992",
                            "1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003",
                            "2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014",
                            "2015","2016","2017")

##) a.1. Bolivia
wbfoodimports$Country <-gsub("Bahamas, The", "Bahamas", 
                             wbfoodimports$Country, fixed=TRUE)
##) a.2. Venezuela
wbfoodimports$Country <-gsub("Venezuela, RB", "Venezuela", 
                             wbfoodimports$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
wbfoodimports <-subset(wbfoodimports, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                         Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                         Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                         Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                         Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                         Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                         Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

##) c) Put in the FAO's column format

wbfoodimports <-gather(wbfoodimports, Year, Value, 2:58)

## d) Colnames
colnames(wbfoodimports) <-c("country","year","foodimp")

## XIV. AGRICULTURE GDP

wbagrigdp <- read_excel("input/WB/wbagrigdp.xlsx")

##)A) Remove unwanted columns
wbagrigdp[,2:5]<- list(NULL)
##) B) Remove the firs three rows
wbagrigdp <- wbagrigdp[-c(1, 2, 3), ]
## C) Change "Data Source" for "Country"
colnames(wbagrigdp) <-c("Country", "1961","1962","1963","1964","1965","1966","1967","1968","1969","1970",
                        "1971","1972","1973","1974","1975","1976","1977","1978","1979","1980","1981",
                        "1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992",
                        "1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003",
                        "2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014",
                        "2015","2016","2017")

##) a.1. Bolivia
wbagrigdp$Country <-gsub("Bahamas, The", "Bahamas", 
                         wbagrigdp$Country, fixed=TRUE)
##) a.2. Venezuela
wbagrigdp$Country <-gsub("Venezuela, RB", "Venezuela", 
                         wbagrigdp$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
wbagrigdp <-subset(wbagrigdp, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                     Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                     Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                     Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                     Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                     Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                     Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

##) c) Put in the FAO's column format

wbagrigdp <-gather(wbagrigdp, Year, Value, 2:58)

## d) Colnames
colnames(wbagrigdp) <-c("country","year","agrigdp")

## XV. URBAN POPULATION GROWTH (annual %)

wburbanpgrowth <- read_excel("input/WB/wburbanpgrowth.xlsx")

##)A) Remove unwanted columns
wburbanpgrowth[,2:5]<- list(NULL)
##) B) Remove the firs three rows
wburbanpgrowth <- wburbanpgrowth[-c(1, 2, 3), ]
## C) Change "Data Source" for "Country"
colnames(wburbanpgrowth) <-c("Country", "1961","1962","1963","1964","1965","1966","1967","1968","1969","1970",
                             "1971","1972","1973","1974","1975","1976","1977","1978","1979","1980","1981",
                             "1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992",
                             "1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003",
                             "2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014",
                             "2015","2016","2017")

##) a.1. Bolivia
wburbanpgrowth$Country <-gsub("Bahamas, The", "Bahamas", 
                              wburbanpgrowth$Country, fixed=TRUE)
##) a.2. Venezuela
wburbanpgrowth$Country <-gsub("Venezuela, RB", "Venezuela", 
                              wburbanpgrowth$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
wburbanpgrowth <-subset(wburbanpgrowth, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                          Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                          Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                          Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                          Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                          Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                          Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

##) c) Put in the FAO's column format

wburbanpgrowth <-gather(wburbanpgrowth, Year, Value, 2:58)

## d) Colnames
colnames(wburbanpgrowth) <-c("country","year","gpurban")

### To get fertilizer use per hectare of arable land (fertuse*1,000)
## a) Transform from tonnes to kg
fertuse$fertuse <- fertuse$fertuse * 1000
##) b) Transform to numeric value in both datasets
as.numeric(fertuse$fertuse)
fertuse$fertuse <- as.numeric(fertuse$fertuse)
as.numeric(wbarableland$arabland)
wbarableland$arabland <- as.numeric(wbarableland$arabland)
##) C) Merge both datasets

fertuse <-merge(fertuse, wbarableland, all.x=FALSE, all.y=TRUE)
##) D) Use transform to divide one column by the other.

fertuse <- transform(fertuse, fertha = fertuse / arabland)

## XV. Fertilizer Use, WB. Kg/Ha

wbfertuse <- read_excel("input/WB/wbfertuse.xlsx")

##)A) Remove unwanted columns
wbfertuse[,2:5]<- list(NULL)
##) B) Remove the firs three rows
wbfertuse <- wbfertuse[-c(1, 2, 3), ]
## C) Change "Data Source" for "Country"
colnames(wbfertuse) <-c("Country", "1961","1962","1963","1964","1965","1966","1967","1968","1969","1970",
                        "1971","1972","1973","1974","1975","1976","1977","1978","1979","1980","1981",
                        "1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992",
                        "1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003",
                        "2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014",
                        "2015","2016","2017")

##) a.1. Bolivia
wbfertuse$Country <-gsub("Bahamas, The", "Bahamas", 
                         wbfertuse$Country, fixed=TRUE)
##) a.2. Venezuela
wbfertuse$Country <-gsub("Venezuela, RB", "Venezuela", 
                         wbfertuse$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
wbfertuse <-subset(wbfertuse, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                     Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                     Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                     Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                     Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                     Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                     Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

##) c) Put in the FAO's column format

wbfertuse <-gather(wbfertuse, Year, Value, 2:58)

## d) Colnames
colnames(wbfertuse) <-c("country","year","wbfert")

wbfertuse <-merge(fertuse, wbfertuse, all.x=FALSE, all.y=TRUE)
## e) Make the new column numeric
as.numeric(wbfertuse$year)
wbfertuse$year <- as.numeric(wbfertuse$year)

as.numeric(wbfertuse$wbfert)
wbfertuse$wbfert <- as.numeric(wbfertuse$wbfert)

as.numeric(wbfertuse$fertuse)
wbfertuse$fertuse <- as.numeric(wbfertuse$fertuse)

factor(wbfertuse$country)
wbfertuse$country <-factor(wbfertuse$country)

wbfertuse$wbfert <- ifelse(is.na(wbfertuse$wbfert), wbfertuse$fertha, wbfertuse$wbfert)
wbfertuse[,3:5]<- list(NULL)

# XVI. PRECIPITATION, WB. (mm)

precipitation <- read_excel("input/WB/precipitation.xls")
#Subset to relevant years
rain <-subset(precipitation, Year>1960)
# Remove irrelevant columns
rain[,5:6]<- list(NULL)
#transform to relevant values
as.numeric(rain$Year)
rain$Year <- as.numeric(rain$Year)
# Create 
rain <-aggregate(pr ~ Country+Year, data=rain, mean)
# Colnames
colnames(rain) <-c("country","year","rain")
factor(rain$country)
rain$country <-factor(rain$country)
# Change names
rain$country <-gsub("ARG", "Argentina",
                    rain$country, fixed=TRUE)
rain$country <-gsub("BLZ", "Belize",
                    rain$country, fixed=TRUE)
rain$country <-gsub("BOL", "Bolivia",
                    rain$country, fixed=TRUE)
rain$country <-gsub("BRA", "Brazil",
                    rain$country, fixed=TRUE)
rain$country <-gsub("CHL", "Chile",
                    rain$country, fixed=TRUE)
rain$country <-gsub("COL", "Colombia",
                    rain$country, fixed=TRUE)
rain$country <-gsub("CRI", "Costa Rica",
                    rain$country, fixed=TRUE)
rain$country <-gsub("CUB", "Cuba",
                    rain$country, fixed=TRUE)
rain$country <-gsub("DOM", "Dominican Republic",
                    rain$country, fixed=TRUE)
rain$country <-gsub("ECU", "Ecuador",
                    rain$country, fixed=TRUE)
rain$country <-gsub("GTM", "Guatemala",
                    rain$country, fixed=TRUE)
rain$country <-gsub("GUY", "Guyana",
                    rain$country, fixed=TRUE)
rain$country <-gsub("HND", "Honduras",
                    rain$country, fixed=TRUE)
rain$country <-gsub("HTI", "Haiti",
                    rain$country, fixed=TRUE)
rain$country <-gsub("JAM", "Jamaica",
                    rain$country, fixed=TRUE)
rain$country <-gsub("MEX", "Mexico",
                    rain$country, fixed=TRUE)
rain$country <-gsub("NIC", "Nicaragua",
                    rain$country, fixed=TRUE)
rain$country <-gsub("PAN", "Panama",
                    rain$country, fixed=TRUE)
rain$country <-gsub("PER", "Peru",
                    rain$country, fixed=TRUE)
rain$country <-gsub("PRY", "Paraguay",
                    rain$country, fixed=TRUE)
rain$country <-gsub("SLV", "El Salvador",
                    rain$country, fixed=TRUE)
rain$country <-gsub("URY", "Uruguay",
                    rain$country, fixed=TRUE)
rain$country <-gsub("VEN", "Venezuela",
                    rain$country, fixed=TRUE)

## XVII. Temperature, WB (°C). 

temperature <- read_excel("input/WB/temperature.xls")
#Subset to relevant years
temp <-subset(temperature, Year>1960)
# Remove irrelevant columns
temp[,5:6]<- list(NULL)
#transform to relevant values
as.numeric(temp$Year)
temp$Year <- as.numeric(temp$Year)
# Create 
temp <-aggregate(tas ~ Country+Year, data=temp, mean)
# Colnames
colnames(temp) <-c("country","year","temp")
factor(temp$country)
temp$country <-factor(temp$country)
# Change names
temp$country <-gsub("ARG", "Argentina",
                    temp$country, fixed=TRUE)
temp$country <-gsub("BLZ", "Belize",
                    temp$country, fixed=TRUE)
temp$country <-gsub("BOL", "Bolivia",
                    temp$country, fixed=TRUE)
temp$country <-gsub("BRA", "Brazil",
                    temp$country, fixed=TRUE)
temp$country <-gsub("CHL", "Chile",
                    temp$country, fixed=TRUE)
temp$country <-gsub("COL", "Colombia",
                    temp$country, fixed=TRUE)
temp$country <-gsub("CRI", "Costa Rica",
                    temp$country, fixed=TRUE)
temp$country <-gsub("CUB", "Cuba",
                    temp$country, fixed=TRUE)
temp$country <-gsub("DOM", "Dominican Republic",
                    temp$country, fixed=TRUE)
temp$country <-gsub("ECU", "Ecuador",
                    temp$country, fixed=TRUE)
temp$country <-gsub("GTM", "Guatemala",
                    temp$country, fixed=TRUE)
temp$country <-gsub("GUY", "Guyana",
                    temp$country, fixed=TRUE)
temp$country <-gsub("HND", "Honduras",
                    temp$country, fixed=TRUE)
temp$country <-gsub("HTI", "Haiti",
                    temp$country, fixed=TRUE)
temp$country <-gsub("JAM", "Jamaica",
                    temp$country, fixed=TRUE)
temp$country <-gsub("MEX", "Mexico",
                    temp$country, fixed=TRUE)
temp$country <-gsub("NIC", "Nicaragua",
                    temp$country, fixed=TRUE)
temp$country <-gsub("PAN", "Panama",
                    temp$country, fixed=TRUE)
temp$country <-gsub("PER", "Peru",
                    temp$country, fixed=TRUE)
temp$country <-gsub("PRY", "Paraguay",
                    temp$country, fixed=TRUE)
temp$country <-gsub("SLV", "El Salvador",
                    temp$country, fixed=TRUE)
temp$country <-gsub("URY", "Uruguay",
                    temp$country, fixed=TRUE)
temp$country <-gsub("VEN", "Venezuela",
                    temp$country, fixed=TRUE)

####### MERGE DATASETS 

final <-merge(manure, landshare, all.x=TRUE, all.y=TRUE)

final <-merge(final, sfertCO2eeq, all.x=TRUE, all.y=TRUE)

final <-merge(final, emiintensity, all.x =TRUE, all.y=TRUE)

final <-merge(final, Euseagri, all.x =TRUE, all.y=TRUE)

final <-merge(final, foodyield, all.x =TRUE, all.y=TRUE)

final <-merge(final, agriCO2, all.x =TRUE, all.y=TRUE)

final <-merge(final, foodsupply, all.x =TRUE, all.y=TRUE)

final <-merge(final, tractoruse, all.x =TRUE, all.y=TRUE)

final <-merge(final, wbfertuse, all.x =TRUE, all.y=TRUE)

final <-merge(final, wbagrigdp, all.x =TRUE, all.y=TRUE)

final <-merge(final, wbfoodimports, all.x =TRUE, all.y=TRUE)

final <-merge(final, wbfoodexports, all.x =TRUE, all.y=TRUE)

final <-merge(final, wburbanpgrowth, all.x =TRUE, all.y=TRUE)

final <-merge(final, rain, all.x =TRUE, all.y =TRUE)

final <-merge(final, temp, all.x=TRUE, all.y=TRUE)

### Remove Bahamas and French Guiana given the amount of missing values.

final <-subset(final, country!="Bahamas" & country!="French Guiana" & country!="Guyana" & country!="Suriname"
               & country!= "Trinidad and Tobago" & country!="Puerto Rico")
## Transform to numeric the WB columns that are charecters.
as.numeric(final$agrigdp)
final$agrigdp <- as.numeric(final$agrigdp)
as.numeric(final$year)
final$year <- as.numeric(final$year)
as.numeric(final$year)
final$foodimp <- as.numeric(final$foodimp)
as.numeric(final$foodexp)
final$foodexp <- as.numeric(final$foodexp)
as.numeric(final$gpurban)
final$gpurban <- as.numeric(final$gpurban)
factor(final$country)
final$country <-factor(final$country)
as.numeric(final$tractoruse)
final$tractoruse <- as.numeric(final$tractoruse)
## Rename wbfert for fertuse
colnames(final)[colnames(final)=="wbfert"] <- "fertuse"
## Remove some for Stats project. Might be relevant for master's paper.
final$agriCO2 <-NULL
final$sfertCO2eeq <-NULL
final$ysgcane <-NULL
final$foodsup <-NULL
final$foodimp <-NULL
final$foodexp <-NULL
final$manure <-NULL
final$agrigdp <-NULL
final$gpurban <-NULL
final$emintensity <-NULL
final$Euseagri <-NULL
## Remove years 2016 and 2017 due to missing values
final <-subset(final, year<2016)

## Include fertuse and yield (maize+beans) columns as percentage change(for main graph scales).Aaron correction.
# a) Fertilizer use
final$pfertuse <-ifelse(final$country=="Argentina", 0.8733129, 
                      ifelse(final$country=="Belize", 10,
                             ifelse(final$country=="Bolivia", 0.618238,
                                    ifelse(final$country=="Brazil", 12.2074329,
                                           ifelse(final$country=="Chile", 12.7054945,
                                                  ifelse(final$country=="Colombia", 20.1019253,
                                                         ifelse(final$country=="Costa Rica",65.5684211,
                                                                ifelse(final$country=="Cuba", 72.4137931,
                                                                       ifelse(final$country=="Dominican Republic", 19.4402778,
                                                                              ifelse(final$country=="Ecuador", 6.4498534,
                                                                                     ifelse(final$country=="El Salvador", 42.2356557,
                                                                                            ifelse(final$country=="Guatemala", 13.7045455,
                                                                                                   ifelse(final$country=="Haiti", 0.1111111,
                                                                                                          ifelse(final$country=="Honduras", 4.3289575,
                                                                                                                 ifelse(final$country=="Jamaica", 81.3614458,
                                                                                                                        ifelse(final$country=="Mexico", 9.7913569,
                                                                                                                               ifelse(final$country=="Nicaragua", 3.9466019,
                                                                                                                                      ifelse(final$country=="Panama", 11.4155251,
                                                                                                                                             ifelse(final$country=="Paraguay", 0.8571429,
                                                                                                                                                    ifelse(final$country=="Peru", 50.9136971,
                                                                                                                                                           ifelse(final$country=="Uruguay", 9.2502037,
                                                                                                                                                                  ifelse(final$country=="Venezuela", 6.5517241, 2))))))))))))))))))))))
final <- transform(final, pfertuse = fertuse / pfertuse)
#) B) Yield of maize + beans
#) B.1) add combined yield
final$yield <-final$ymaize+final$ybeans
# B.2) Nest
final$pyield <-ifelse(final$country=="Argentina", 28273, 
                        ifelse(final$country=="Belize", 11225,
                               ifelse(final$country=="Bolivia", 15834,
                                      ifelse(final$country=="Brazil", 19883,
                                             ifelse(final$country=="Chile", 29236,
                                                    ifelse(final$country=="Colombia", 16045,
                                                           ifelse(final$country=="Costa Rica", 14223,
                                                                  ifelse(final$country=="Cuba", 14241,
                                                                         ifelse(final$country=="Dominican Republic", 20860,
                                                                                ifelse(final$country=="Ecuador", 12018,
                                                                                       ifelse(final$country=="El Salvador", 14160,
                                                                                              ifelse(final$country=="Guatemala", 14717,
                                                                                                     ifelse(final$country=="Haiti", 14577,
                                                                                                            ifelse(final$country=="Honduras", 16381,
                                                                                                                   ifelse(final$country=="Jamaica", 11133,
                                                                                                                          ifelse(final$country=="Mexico", 14407,
                                                                                                                                 ifelse(final$country=="Nicaragua", 16137,
                                                                                                                                        ifelse(final$country=="Panama", 10746,
                                                                                                                                               ifelse(final$country=="Paraguay", 20009,
                                                                                                                                                      ifelse(final$country=="Peru", 22944,
                                                                                                                                                             ifelse(final$country=="Uruguay", 13717,
                                                                                                                                                                    ifelse(final$country=="Venezuela", 15707, 2))))))))))))))))))))))
final <- transform(final, pyield = yield / pyield)

#### Save data
save(final, file="output/analytical_data.RData")
