##This is the code to generate the tables and figures in the supplementary material section
## Supplementary material

#source in any useful functions
source("check_packages.R")
source("useful_functions.R")

## Download new dataset from FAO that has data on all crops. 

load("output/analytical_data.RData")
totalyields <- read_excel("input/supplementary/totalyields.xlsx")

#) a.0) If needed, Change name "Area" for "Country"

colnames(totalyields)[colnames(totalyields)=="Area"] <- "Country"

#) a) change to relevant names

##) a.1. Bolivia
totalyields$Country <-gsub("Bolivia (Plurinational State of)", "Bolivia", 
                           totalyields$Country, fixed=TRUE)
##) a.2. Venezuela
totalyields$Country <-gsub("Venezuela (Bolivarian Republic of)", "Venezuela", 
                           totalyields$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
totalyields <-subset(totalyields, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                       Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                       Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                       Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                       Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                       Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                       Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

#) Remove unnecessary columns
totalyields[,1:3]<- list(NULL)
totalyields[,2:4]<- list(NULL)
totalyields[,3]<- list(NULL)
totalyields[,4]<- list(NULL)
totalyields[,5:6]<- list(NULL)

##) C) Spread into item
totalyields <- spread(totalyields, Item, Value)

##) Put final name to the column:

colnames(totalyields) <-c("country","year","cereal","citrus","grain","fibre","fruit","oilcrop","pulses",
                          "roots","treenuts","vegetables")

### MERGE WITH ORIGINAL, FINAL DATASET
totalyields <-merge(final, totalyields, all.x=TRUE, all.y=TRUE)

# Create dummy variable for before and after the shock
totalyields$time <-ifelse(totalyields$year>= 1995, 1, 0)
# Create dummy variable for Cuba, country exposed to "treatment"
totalyields$cuba <-ifelse(totalyields$country =="Cuba", 1, 0)
### SUM ALL YIELDS. WITH NA.RM=TRUE
totalyields$yield <- rowSums(totalyields[,c("cereal","citrus","grain","fibre","fruit","oilcrop","pulses","roots",
                                            "treenuts","vegetables")], na.rm=TRUE)
### Remove Uruguay and Venezuela due to the lack of data
totalyields <-subset(totalyields, country!="Uruguay"&country!="Venezuela")


### Downloads variables that control for fuel (and energy use in general), and merges it to the original dataset 

energyagri <-read_excel("input/supplementary/energyagri.xlsx")

#) a) change to relevant names

##) a.1. Bolivia
energyagri$Country <-gsub("Bolivia (Plurinational State of)", "Bolivia", 
                          energyagri$Country, fixed=TRUE)
##) a.2. Venezuela
energyagri$Country <-gsub("Venezuela (Bolivarian Republic of)", "Venezuela", 
                          energyagri$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
energyagri <-subset(energyagri, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                      Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                      Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                      Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                      Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                      Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                      Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

#) Remove unnecessary columns
energyagri[,1:3]<- list(NULL)
energyagri[,2:4]<- list(NULL)
energyagri[,3]<- list(NULL)
energyagri[,4]<- list(NULL)
energyagri[,5:6]<- list(NULL)

##) C) Spread into item
energyagri <- spread(energyagri, Item, Value)

##) Put final name to the column:
colnames(energyagri) <-c("country","year","energy")

### ) Mrege with final dataset

ytotal<-merge(totalyields, energyagri, all.x =TRUE, all.y=FALSE)


#### only energy of fuel oil and for irrigation

energyagri2 <-read_excel("input/supplementary/energyagri2.xlsx")

#) a.1. Bolivia
energyagri2$Country <-gsub("Bolivia (Plurinational State of)", "Bolivia", 
                           energyagri2$Country, fixed=TRUE)
##) a.2. Venezuela
energyagri2$Country <-gsub("Venezuela (Bolivarian Republic of)", "Venezuela", 
                           energyagri2$Country, fixed=TRUE)
#) b) Subset to LA relevant countries
energyagri2 <-subset(energyagri2, Country=="Argentina"|Country=="Bahamas"|Country=="Belize"|Country=="Bolivia"|
                       Country=="Brazil"|Country=="Chile"|Country=="Colombia"|Country=="Costa Rica"|
                       Country=="Cuba"|Country=="Dominican Republic"|Country=="Ecuador"|Country=="El Salvador"|
                       Country=="French Guiana"|Country=="Guatemala"|Country=="Guyana"|Country=="Haiti"|
                       Country=="Honduras"|Country=="Jamaica"|Country=="Mexico"|Country=="Nicaragua"|
                       Country=="Panama"|Country=="Paraguay"|Country=="Peru"|Country=="Puerto Rico"|
                       Country=="Suriname"|Country=="Trinidad and Tobago"| Country=="Uruguay" | Country=="Venezuela")

#) Remove unnecessary columns
energyagri2[,1:3]<- list(NULL)
energyagri2[,2:4]<- list(NULL)
energyagri2[,3]<- list(NULL)
energyagri2[,4]<- list(NULL)
energyagri2[,5:6]<- list(NULL)

##) C) Spread into item
energyagri2 <- spread(energyagri2, Item, Value)
##) D) Add the irrigation and fuel oil use together

energyagri2$energy2 <- rowSums(energyagri2[,c("Energy for power irrigation", "Fuel oil")], na.rm=TRUE)


##) Put final name to the column:

colnames(energyagri2) <-c("country","year","irrigation","fuel","irrifuel")

### Merge

ytotal <-merge(ytotal, energyagri2, all.x =TRUE, all.y=FALSE)
ytotal <-subset(ytotal, country!="Puerto Rico"&country!="Bahamas"&country!="French Guiana"
                &country!="Guyana"&country!="Suriname")
# Turn into usable dataset
final <-ytotal
###########################################################################################################################
###### Tables and figures 
### code for generating table while controlling also for fuel (energy use in general).

##{r regression_template1, echo=FALSE, fig.show = "hold", results='asis', error=FALSE, message=FALSE}
# Code for generating Table 1. CONTROLLING FOR TOTAL ENERGY
model.maize <-panelAR(log(ymaize)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                        country+agriland+tractoruse+(rain*temp)+energy, panelVar="country", timeVar="year", 
                      autoCorr= "ar1", panelCorrMethod= "pcse", data=final)
model.beans <-panelAR(log(ybeans)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                        country+agriland+tractoruse+(rain*temp)+energy, panelVar="country", timeVar="year", 
                      autoCorr= "ar1", panelCorrMethod= "pcse", data=final)
## Model adding beans and maize yield
model.yield <-panelAR(log(yield)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                        country+agriland+tractoruse+(rain*temp)+energy, panelVar="country", timeVar="year", 
                      autoCorr= "ar1", panelCorrMethod= "pcse", data=final)
texreg(lapply(list(model.maize, model.beans, model.yield), convertpanelAR),
       custom.coef.names = c("Constant",
                             "Fertilizer use ($\\beta$)",
                             "Time ($\\ge$1995) ($\\gamma$)",
                             "Cuba",
                             "Agricultural land ($\\lambda$)",
                             "Tractor use ($\\mu$)",
                             "Rainfall",
                             "Temperature",
                             "Energy",
                             "Fertilizer use x Time ($\\ge$1995) ($\\delta$)",
                             "Fertilizer use x Cuba ($\\zeta$)",
                             "Time ($\\ge$1995) x Cuba ($\\eta$)",
                             "Rainfall x Temperature ($\\nu$)",
                             "Fertilizer Use x Time ($\\ge$1995) x Cuba ($\\theta$)"),
       omit.coef="country",
       reorder.coef=c(2,3,4,5,6,7,8,9,10,11,12,13,14,1),
       custom.model.names=c("\\textit{log} (maize)", "\\textit{log} (beans)", "\\textit{log} (maize+beans)"),
       digits=3,
       caption="OLS diff-in-diff-in-diff regressions estimating the relationship between industrial agriculture practices and yield in Cuba relative to the rest of LAC from 1961 to 1995, and assessing if the post-Soviet transition to agroecology in Cuba decoupled these practices from yield in comparison to the control group.",
       caption.above = TRUE)
###

### Code for generating table with yield of all crops, and controlling for energy use. 

###{r regression_template2, echo=FALSE, fig.show = "hold", results='asis', error=FALSE, message=FALSE}

model.cereal <-panelAR(log(cereal)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                         country+agriland+tractoruse+(rain*temp)+energy, panelVar="country", timeVar="year", 
                       autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
model.citrus <-panelAR(log(citrus)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                         country+agriland+tractoruse+(rain*temp)+energy, panelVar="country", timeVar="year", 
                       autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
model.grain <-panelAR(log(grain)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                        country+agriland+tractoruse+(rain*temp)+energy, panelVar="country", timeVar="year", 
                      autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
model.fibre<-panelAR(log(fibre)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                       country+agriland+tractoruse+(rain*temp)+energy, panelVar="country", timeVar="year", 
                     autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
model.fruit <-panelAR(log(fruit)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                        country+agriland+tractoruse+(rain*temp)+energy, panelVar="country", timeVar="year", 
                      autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
model.pulses <-panelAR(log(pulses)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                         country+agriland+tractoruse+(rain*temp)+energy, panelVar="country", timeVar="year", 
                       autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
model.roots <-panelAR(log(roots)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                        country+agriland+tractoruse+(rain*temp)+energy, panelVar="country", timeVar="year", 
                      autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
model.treenuts <-panelAR(log(treenuts)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                           country+agriland+tractoruse+(rain*temp)+energy, panelVar="country", timeVar="year", 
                         autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
model.vegetables <-panelAR(log(vegetables)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+
                             (fertuse*time*cuba)+
                             country+agriland+tractoruse+(rain*temp)+energy, panelVar="country", timeVar="year", 
                           autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
model.yield <-panelAR(log(yield)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                        country+agriland+tractoruse+(rain*temp)+energy, panelVar="country", timeVar="year", 
                      autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
texreg(scalebox = 0.7, lapply(list(model.cereal, model.citrus, model.grain, model.fibre, model.fruit,
                                   model.pulses, model.roots, model.treenuts, model.vegetables,
                                   model.yield), convertpanelAR),
       custom.coef.names = c("Constant",
                             "Fertilizer use ($\\beta$)",
                             "Time ($\\ge$1995) ($\\gamma$)",
                             "Cuba",
                             "Agricultural land ($\\lambda$)",
                             "Tractor use ($\\mu$)",
                             "Rainfall",
                             "Temperature",
                             "Energy",
                             "Fertilizer use x Time ($\\ge$1995) ($\\delta$)",
                             "Fertilizer use x Cuba ($\\zeta$)",
                             "Time ($\\ge$1995) x Cuba ($\\eta$)",
                             "Rainfall x Temperature ($\\nu$)",
                             "Fertilizer Use x Time ($\\ge$1995) x Cuba ($\\theta$)"),
       
       omit.coef="country",
       reorder.coef=c(2,3,4,5,6,7,8,9,10,11,12,13,14,1),
       custom.model.names=c("\\textit{log} Cereals","Citrus","Grain","Fiber","Fruit","Pulses","Roots","Treenuts","Vegetables", "Total yield"),
       digits=3,
       caption="OLS diff-in-diff-in-diff regressions estimating the relationship between industrial agriculture
       practices and yield in Cuba relative to the rest of LAC from 1961 to 1995, and assessing if the
       post-Soviet transition to agroecology in Cuba decoupled these practices from yield in comparison
       to the control group.",
       caption.above = TRUE)
###
### Code controlling for fuel and energy in irrigation only (not full energy use), for the original dependent variable

###{r regression_template3, echo=FALSE, fig.show = "hold", results='asis', error=FALSE, message=FALSE}
# Code for generating Table 1
model.maize <-panelAR(log(ymaize)~tractoruse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                        country+agriland+fertuse+(rain*temp)+irrifuel, panelVar="country", timeVar="year", 
                      autoCorr= "ar1", panelCorrMethod= "pcse", data=final)
model.beans <-panelAR(log(ybeans)~tractoruse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                        country+agriland+fertuse+(rain*temp)+irrifuel, panelVar="country", timeVar="year", 
                      autoCorr= "ar1", panelCorrMethod= "pcse", data=final)
## Model adding beans and maize yield
model.yield <-panelAR(log(yield)~tractoruse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                        country+agriland+fertuse+(rain*temp)+irrifuel, panelVar="country", timeVar="year", 
                      autoCorr= "ar1", panelCorrMethod= "pcse", data=final)
texreg(lapply(list(model.maize, model.beans, model.yield), convertpanelAR),
       custom.coef.names = c("Constant",
                             "Fertilizer use ($\\beta$)",
                             "Time ($\\ge$1995) ($\\gamma$)",
                             "Cuba",
                             "Agricultural land ($\\lambda$)",
                             "Fertuse",
                             "Rainfall",
                             "Temperature",
                             "Energy (Irrigation + fuel)",
                             "Fertilizer use x Time ($\\ge$1995) ($\\delta$)",
                             "Fertilizer use x Cuba ($\\zeta$)",
                             "Time ($\\ge$1995) x Cuba ($\\eta$)",
                             "Rainfall x Temperature ($\\nu$)",
                             "Fertilizer Use x Time ($\\ge$1995) x Cuba ($\\theta$)"),
       omit.coef="country",
       reorder.coef=c(2,3,4,5,6,7,8,9,10,11,12,13,14,1),
       custom.model.names=c("\\textit{log} (maize)", "\\textit{log} (beans)", "\\textit{log} (maize+beans)"),
       digits=3,
       caption="OLS diff-in-diff-in-diff regressions estimating the relationship between industrial agriculture practices and yield in Cuba relative to the rest of LAC from 1961 to 1995, and assessing if the post-Soviet transition to agroecology in Cuba decoupled these practices from yield in comparison to the control group.",
       caption.above = TRUE)
###

# Code for generating Table 1 with all crops included, without adding new variables

###{r regression_template4, echo=FALSE, fig.show = "hold", results='asis', error=FALSE, message=FALSE}

model.cereal <-panelAR(log(cereal)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                         country+agriland+tractoruse+(rain*temp), panelVar="country", timeVar="year", 
                       autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
model.citrus <-panelAR(log(citrus)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                         country+agriland+tractoruse+(rain*temp), panelVar="country", timeVar="year", 
                       autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
model.grain <-panelAR(log(grain)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                        country+agriland+tractoruse+(rain*temp), panelVar="country", timeVar="year", 
                      autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
model.fibre<-panelAR(log(fibre)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                       country+agriland+tractoruse+(rain*temp), panelVar="country", timeVar="year", 
                     autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
model.fruit <-panelAR(log(fruit)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                        country+agriland+tractoruse+(rain*temp), panelVar="country", timeVar="year", 
                      autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
model.pulses <-panelAR(log(pulses)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                         country+agriland+tractoruse+(rain*temp), panelVar="country", timeVar="year", 
                       autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
model.roots <-panelAR(log(roots)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                        country+agriland+tractoruse+(rain*temp), panelVar="country", timeVar="year", 
                      autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
model.treenuts <-panelAR(log(treenuts)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                           country+agriland+tractoruse+(rain*temp), panelVar="country", timeVar="year", 
                         autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
model.vegetables <-panelAR(log(vegetables)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+
                             (fertuse*time*cuba)+
                             country+agriland+tractoruse+(rain*temp), panelVar="country", timeVar="year", 
                           autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
model.yield <-panelAR(log(yield)~fertuse+time+(fertuse*time)+(fertuse*cuba)+(time*cuba)+(fertuse*time*cuba)+
                        country+agriland+tractoruse+(rain*temp), panelVar="country", timeVar="year", 
                      autoCorr= "ar1", panelCorrMethod= "pcse", data=ytotal)
texreg(scalebox = 0.7, lapply(list(model.cereal, model.citrus, model.grain, model.fibre, model.fruit,
                                   model.pulses, model.roots, model.treenuts, model.vegetables,
                                   model.yield), convertpanelAR),
       custom.coef.names = c("Constant",
                             "Fertilizer use ($\\beta$)",
                             "Time ($\\ge$1995) ($\\gamma$)",
                             "Cuba",
                             "Agricultural land ($\\lambda$)",
                             "Tractor use ($\\mu$)",
                             "Rainfall",
                             "Temperature",
                             "Fertilizer use x Time ($\\ge$1995) ($\\delta$)",
                             "Fertilizer use x Cuba ($\\zeta$)",
                             "Time ($\\ge$1995) x Cuba ($\\eta$)",
                             "Rainfall x Temperature ($\\nu$)",
                             "Fertilizer Use x Time ($\\ge$1995) x Cuba ($\\theta$)"),
       
       omit.coef="country",
       reorder.coef=c(2,3,4,5,6,7,8,9,10,11,12,13,1),
       custom.model.names=c("\\textit{log} Cereals","Citrus","Grain","Fiber","Fruit","Pulses","Roots","Treenuts","Vegetables", "Total yield"),
       digits=3,
       caption="OLS diff-in-diff-in-diff regressions estimating the relationship between industrial agriculture
       practices and yield in Cuba relative to the rest of LAC from 1961 to 1995, and assessing if the
       post-Soviet transition to agroecology in Cuba decoupled these practices from yield in comparison
       to the control group.",
       caption.above = TRUE)
###

##### CODE FOR SUPPLEMENTARY FIGURES
##### new graphs with total production (not just maize and beans)
## Figure 2 
ytotal$yield2 <-(ytotal$yield)/(10000)
g <-ggplot(subset(ytotal, country=="Argentina"|country=="Brazil"|country=="Costa Rica"|country=="Cuba"
                  |country=="Mexico"), aes(x=year, y=yield2)) + 
  scale_y_log10(breaks=c(1,10,20,30,40,50,60,70,80,90,100,300)) +
  geom_point(data=subset(ytotal, country=="Cuba"), color='#e41a1c', show.legend = FALSE) +  
  geom_line(aes(color=country), se=FALSE, show.legend = FALSE) +
  geom_text(data = subset(ytotal, year==2015&(country=="Argentina"|country=="Brazil"|
                                                country=="Costa Rica"|country=="Cuba"
                                              |country=="Mexico")), 
            aes(label=country, colour=country, x=year, y=yield2), show.legend = FALSE, hjust=.71, vjust=-.4) +
  scale_color_manual(values=c('grey35','grey35','grey35','#e41a1c','grey35'))
h <- g + scale_x_continuous(breaks=seq(1960,2015,5)) + 
  labs(x="Year",y=expression(~italic(log)~  'total yield ('*t~ha^-1*')')) + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "black", size=1) + 
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), # bg of the panel, 
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank())
suppressWarnings(print(h))


#######################################################
# Code for generating Figure 3, with the total yield of each country instead of just maize, beans, and both
## REMOVES URUGUAY AND VENEZUELA BECAUSE OF TOTAL MISSING VALUES 
final <-ytotal
################# every country
############# ARGENTINA ##############
argentina <-subset(final, country=="Argentina")
a <-ggplot(argentina, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text( size = 14),
    strip.text.x = element_text(size = 18))
argentina$yield2 <-(argentina$yield)/(10000)
b <-ggplot(argentina, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
arg1 <- ggplotGrob(a)
arg2 <- ggplotGrob(b)
# Grab the panels from g2 and overlay them onto the panels of g1
pp <- c(subset(arg1$layout, grepl("panel", arg1$layout$name), select = t:r))
arg <- gtable_add_grob(arg1, arg2$grobs[grepl("panel", arg1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(arg2$layout$name == "ylab-l") 
ylab <- arg2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
arg <- gtable_add_cols(arg, arg2$widths[arg2$layout[index, ]$l], pos = max(pp$r))
arg <-gtable_add_grob(arg,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(arg2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- arg2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(arg)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
arg <- gtable_add_cols(arg, arg2$widths[arg2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
arg <- gtable_add_grob(arg, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))
################ BELIZE ################
belize <-subset(final, country=="Belize")
c <-ggplot(belize, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 14),
    strip.text.x = element_text(size = 18))
belize$yield2 <-(belize$yield)/(10000)
d <-ggplot(belize, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
bel1 <- ggplotGrob(c)
bel2 <- ggplotGrob(d)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_bel <- c(subset(bel1$layout, grepl("panel", bel1$layout$name), select = t:r))
bel <- gtable_add_grob(bel1, bel2$grobs[grepl("panel", bel1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(bel2$layout$name == "ylab-l") 
ylab <- bel2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
bel <- gtable_add_cols(bel, bel2$widths[bel2$layout[index, ]$l], pos = max(pp$r))
bel <-gtable_add_grob(bel,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(bel2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- bel2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(bel)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
bel <- gtable_add_cols(bel, bel2$widths[bel2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
bel <- gtable_add_grob(bel, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))
############### BOLIVIA ####################
bolivia <-subset(final, country=="Bolivia")
e <-ggplot(bolivia, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
bolivia$yield2 <-(bolivia$yield)/(10000)
f <-ggplot(bolivia, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
bol1 <- ggplotGrob(e)
bol2 <- ggplotGrob(f)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_bol <- c(subset(bol1$layout, grepl("panel", bol1$layout$name), select = t:r))
bol <- gtable_add_grob(bol1, bol2$grobs[grepl("panel", bol1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(bol2$layout$name == "ylab-l") 
ylab <- bol2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
bol <- gtable_add_cols(bol, bol2$widths[bol2$layout[index, ]$l], pos = max(pp$r))
bol <-gtable_add_grob(bol,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(bol2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- bol2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(bol)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
bol <- gtable_add_cols(bol, bol2$widths[bol2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
bol <- gtable_add_grob(bol, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))
###################### BRAZIL #######################
brazil <-subset(final, country=="Brazil")
i <-ggplot(brazil, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
brazil$yield2 <-(brazil$yield)/(10000)
j <-ggplot(brazil, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
bra1 <- ggplotGrob(i)
bra2 <- ggplotGrob(j)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_bra <- c(subset(bra1$layout, grepl("panel", bra1$layout$name), select = t:r))
bra <- gtable_add_grob(bra1, bra2$grobs[grepl("panel", bra1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(bra2$layout$name == "ylab-l") 
ylab <- bra2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
bra <- gtable_add_cols(bra, bra2$widths[bra2$layout[index, ]$l], pos = max(pp$r))
bra <-gtable_add_grob(bra,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(bra2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- bra2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(bra)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
bra <- gtable_add_cols(bra, bra2$widths[bra2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
bra <- gtable_add_grob(bra, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))
################ CHILE #############################
chile <-subset(final, country=="Chile")
k <-ggplot(chile, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
chile$yield2 <-(chile$yield)/(10000)
l <-ggplot(chile, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
chi1 <- ggplotGrob(k)
chi2 <- ggplotGrob(l)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_chi <- c(subset(chi1$layout, grepl("panel", chi1$layout$name), select = t:r))
chi <- gtable_add_grob(chi1, chi2$grobs[grepl("panel", chi1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(chi2$layout$name == "ylab-l") 
ylab <- chi2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
chi <- gtable_add_cols(chi, chi2$widths[chi2$layout[index, ]$l], pos = max(pp$r))
chi <-gtable_add_grob(chi,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(chi2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- chi2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(chi)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
chi <- gtable_add_cols(chi, chi2$widths[chi2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
chi <- gtable_add_grob(chi, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))
############### COLOMBIA ###########################
colombia <-subset(final, country=="Colombia")
m <-ggplot(colombia, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
colombia$yield2 <-(colombia$yield)/(10000)
n <-ggplot(colombia, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
col1 <- ggplotGrob(m)
col2 <- ggplotGrob(n)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_col <- c(subset(col1$layout, grepl("panel", col1$layout$name), select = t:r))
col <- gtable_add_grob(col1, col2$grobs[grepl("panel", col1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(col2$layout$name == "ylab-l") 
ylab <- col2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
col <- gtable_add_cols(col, col2$widths[col2$layout[index, ]$l], pos = max(pp$r))
col <-gtable_add_grob(col,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(col2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- col2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(col)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
col <- gtable_add_cols(col, col2$widths[col2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
col <- gtable_add_grob(col, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))
################## COSTA RICA #######################
costarica <-subset(final, country=="Costa Rica")
o <-ggplot(costarica, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
costarica$yield2 <-(costarica$yield)/(10000)
p <-ggplot(costarica, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
cos1 <- ggplotGrob(o)
cos2 <- ggplotGrob(p)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_cos <- c(subset(cos1$layout, grepl("panel", cos1$layout$name), select = t:r))
cos <- gtable_add_grob(cos1, cos2$grobs[grepl("panel", cos1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(cos2$layout$name == "ylab-l") 
ylab <- cos2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
cos <- gtable_add_cols(cos, cos2$widths[cos2$layout[index, ]$l], pos = max(pp$r))
cos <-gtable_add_grob(cos,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(cos2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- cos2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(cos)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
cos <- gtable_add_cols(cos, cos2$widths[cos2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
cos <- gtable_add_grob(cos, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))
######################### CUBA ###################
cuba <-subset(final, country=="Cuba")
w <-ggplot(cuba, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
cuba$yield2 <-(cuba$yield)/(10000)
e <-ggplot(cuba, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
g1 <- ggplotGrob(w)
g2 <- ggplotGrob(e)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_cub <- c(subset(g1$layout, grepl("panel", g1$layout$name), select = t:r))
cub <- gtable_add_grob(g1, g2$grobs[grepl("panel", g1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(g2$layout$name == "ylab-l") 
ylab <- g2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
cub <- gtable_add_cols(cub, g2$widths[g2$layout[index, ]$l], pos = max(pp$r))
cub <-gtable_add_grob(cub,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(g2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- g2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(w)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
cub <- gtable_add_cols(cub, g2$widths[g2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
cub <- gtable_add_grob(cub, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))
################### DOMINICAN REPUBLIC ###################3
dominic <-subset(final, country=="Dominican Republic")
q <-ggplot(dominic, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
dominic$yield2 <-(dominic$yield)/(10000)
r <-ggplot(dominic, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
dom1 <- ggplotGrob(q)
dom2 <- ggplotGrob(r)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_dom <- c(subset(dom1$layout, grepl("panel", dom1$layout$name), select = t:r))
dom <- gtable_add_grob(dom1, dom2$grobs[grepl("panel", dom1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(dom2$layout$name == "ylab-l") 
ylab <- dom2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
dom <- gtable_add_cols(dom, dom2$widths[dom2$layout[index, ]$l], pos = max(pp$r))
dom <-gtable_add_grob(dom,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(dom2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- dom2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(dom)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
dom <- gtable_add_cols(dom, dom2$widths[dom2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
dom <- gtable_add_grob(dom, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))

################################# ECUADOR ##################################################
ecuador <-subset(final, country=="Ecuador")
s <-ggplot(ecuador, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
ecuador$yield2 <-(ecuador$yield)/(10000)
t <-ggplot(ecuador, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
ecu1 <- ggplotGrob(s)
ecu2 <- ggplotGrob(t)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_ecu <- c(subset(ecu1$layout, grepl("panel", ecu1$layout$name), select = t:r))
ecu <- gtable_add_grob(ecu1, ecu2$grobs[grepl("panel", ecu1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(ecu2$layout$name == "ylab-l") 
ylab <- ecu2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
ecu <- gtable_add_cols(ecu, ecu2$widths[ecu2$layout[index, ]$l], pos = max(pp$r))
ecu <-gtable_add_grob(ecu,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(ecu2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- ecu2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(ecu)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
ecu <- gtable_add_cols(ecu, ecu2$widths[ecu2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
ecu <- gtable_add_grob(ecu, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))
#################### EL SALVADOR ##################
salvador <-subset(final, country=="El Salvador")
v <-ggplot(salvador, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
salvador$yield2 <-(salvador$yield)/(10000)
x <-ggplot(salvador, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
sal1 <- ggplotGrob(v)
sal2 <- ggplotGrob(x)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_sal <- c(subset(sal1$layout, grepl("panel", sal1$layout$name), select = t:r))
sal <- gtable_add_grob(sal1, sal2$grobs[grepl("panel", sal1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(sal2$layout$name == "ylab-l") 
ylab <- sal2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
sal <- gtable_add_cols(sal, sal2$widths[sal2$layout[index, ]$l], pos = max(pp$r))
sal <-gtable_add_grob(sal,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(sal2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- sal2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(sal)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
sal <- gtable_add_cols(sal, sal2$widths[sal2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
sal <- gtable_add_grob(sal, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))
############# GUATEMALA #########################
guatemala <-subset(final, country=="Guatemala")
y <-ggplot(guatemala, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
guatemala$yield2 <-(guatemala$yield)/(10000)
z <-ggplot(guatemala, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
gua1 <- ggplotGrob(y)
gua2 <- ggplotGrob(z)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_gua <- c(subset(gua1$layout, grepl("panel", gua1$layout$name), select = t:r))
gua <- gtable_add_grob(gua1, gua2$grobs[grepl("panel", gua1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(gua2$layout$name == "ylab-l") 
ylab <- gua2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
gua <- gtable_add_cols(gua, gua2$widths[gua2$layout[index, ]$l], pos = max(pp$r))
gua <-gtable_add_grob(gua,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(gua2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- gua2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(gua)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
gua <- gtable_add_cols(gua, gua2$widths[gua2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
gua <- gtable_add_grob(gua, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))
##################### HAITI ##############################
haiti <-subset(final, country=="Haiti")
aa <-ggplot(haiti, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
haiti$yield2 <-(haiti$yield)/(10000)
bb <-ggplot(haiti, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
hai1 <- ggplotGrob(aa)
hai2 <- ggplotGrob(bb)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_hai <- c(subset(hai1$layout, grepl("panel", hai1$layout$name), select = t:r))
hai <- gtable_add_grob(hai1, hai2$grobs[grepl("panel", hai1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(hai2$layout$name == "ylab-l") 
ylab <- hai2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
hai <- gtable_add_cols(hai, hai2$widths[hai2$layout[index, ]$l], pos = max(pp$r))
hai <-gtable_add_grob(hai,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(hai2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- hai2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(hai)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
hai <- gtable_add_cols(hai, hai2$widths[hai2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
hai <- gtable_add_grob(hai, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))
##################### HONDURAS ##############################
honduras <-subset(final, country=="Honduras")
cc <-ggplot(honduras, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
honduras$yield2 <-(honduras$yield)/(10000)
dd <-ggplot(honduras, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
hon1 <- ggplotGrob(cc)
hon2 <- ggplotGrob(dd)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_hon <- c(subset(hon1$layout, grepl("panel", hon1$layout$name), select = t:r))
hon <- gtable_add_grob(hon1, hon2$grobs[grepl("panel", hon1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(hon2$layout$name == "ylab-l") 
ylab <- hon2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
hon <- gtable_add_cols(hon, hon2$widths[hon2$layout[index, ]$l], pos = max(pp$r))
hon <-gtable_add_grob(hon,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(hon2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- hon2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(hon)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
hon <- gtable_add_cols(hon, hon2$widths[hon2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
hon <- gtable_add_grob(hon, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))
################################# JAMAICA ##########################################
jamaica <-subset(final, country=="Jamaica")
ee <-ggplot(jamaica, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
jamaica$yield2 <-(jamaica$yield)/(10000)
ff <-ggplot(jamaica, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
jam1 <- ggplotGrob(ee)
jam2 <- ggplotGrob(ff)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_jam <- c(subset(jam1$layout, grepl("panel", jam1$layout$name), select = t:r))
jam <- gtable_add_grob(jam1, jam2$grobs[grepl("panel", jam1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(jam2$layout$name == "ylab-l") 
ylab <- jam2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
jam <- gtable_add_cols(jam, jam2$widths[jam2$layout[index, ]$l], pos = max(pp$r))
jam <-gtable_add_grob(jam,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(jam2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- jam2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(jam)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
jam <- gtable_add_cols(jam, jam2$widths[jam2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
jam <- gtable_add_grob(jam, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))
################################# MEXICO ##########################################
mexico <-subset(final, country=="Mexico")
gg <-ggplot(mexico, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
mexico$yield2 <-(mexico$yield)/(10000)
hh <-ggplot(mexico, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
mex1 <- ggplotGrob(gg)
mex2 <- ggplotGrob(hh)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_mex <- c(subset(mex1$layout, grepl("panel", mex1$layout$name), select = t:r))
mex <- gtable_add_grob(mex1, mex2$grobs[grepl("panel", mex1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(mex2$layout$name == "ylab-l") 
ylab <- mex2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
mex <- gtable_add_cols(mex, mex2$widths[mex2$layout[index, ]$l], pos = max(pp$r))
mex <-gtable_add_grob(mex,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(mex2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- mex2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(mex)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
mex <- gtable_add_cols(mex, mex2$widths[mex2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
mex <- gtable_add_grob(mex, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))
################################# NICARAGUA ##########################################
nicaragua <-subset(final, country=="Nicaragua")
ii <-ggplot(nicaragua, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
nicaragua$yield2 <-(nicaragua$yield)/(10000)
jj <-ggplot(nicaragua, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
nic1 <- ggplotGrob(ii)
nic2 <- ggplotGrob(jj)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_nic <- c(subset(nic1$layout, grepl("panel", nic1$layout$name), select = t:r))
nic <- gtable_add_grob(nic1, nic2$grobs[grepl("panel", nic1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(nic2$layout$name == "ylab-l") 
ylab <- nic2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
nic <- gtable_add_cols(nic, nic2$widths[nic2$layout[index, ]$l], pos = max(pp$r))
nic <-gtable_add_grob(nic ,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(nic2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- nic2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(nic)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
nic <- gtable_add_cols(nic, nic2$widths[nic2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
nic <- gtable_add_grob(nic, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))
################################# PANAMA ##########################################
panama <-subset(final, country=="Panama")
kk <-ggplot(panama, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    plot.margin=unit(c(.19,.14,.19,.14), "cm"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
panama$yield2 <-(panama$yield)/(10000)
ll <-ggplot(panama, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    plot.margin=unit(c(.19,.14,.19,.14), "cm"),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
pan1 <- ggplotGrob(kk)
pan2 <- ggplotGrob(ll)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_pan <- c(subset(pan1$layout, grepl("panel", pan1$layout$name), select = t:r))
pan <- gtable_add_grob(pan1, pan2$grobs[grepl("panel", pan1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(pan2$layout$name == "ylab-l") 
ylab <- pan2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
pan <- gtable_add_cols(pan, pan2$widths[pan2$layout[index, ]$l], pos = max(pp$r))
pan <-gtable_add_grob(pan ,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(pan2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- pan2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(pan)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
pan <- gtable_add_cols(pan, pan2$widths[pan2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
pan <- gtable_add_grob(pan, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))
################################# PARAGUAY ##########################################
paraguay <-subset(final, country=="Paraguay")
mm <-ggplot(paraguay, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
paraguay$yield2 <-(paraguay$yield)/(10000)
nn <-ggplot(paraguay, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
par1 <- ggplotGrob(mm)
par2 <- ggplotGrob(nn)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_par <- c(subset(par1$layout, grepl("panel", par1$layout$name), select = t:r))
par <- gtable_add_grob(par1, par2$grobs[grepl("panel", par1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(par2$layout$name == "ylab-l") 
ylab <- par2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
par <- gtable_add_cols(par, par2$widths[par2$layout[index, ]$l], pos = max(pp$r))
par <-gtable_add_grob(par ,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(par2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- par2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(par)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
par <- gtable_add_cols(par, par2$widths[par2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
par <- gtable_add_grob(par, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))
################################# PERU ##########################################
peru <-subset(final, country=="Peru")
oo <-ggplot(peru, aes(x=year, y=fertuse)) +
  geom_point(alpha=0.8, size=0.9) + geom_smooth(method="auto") + 
  xlab("Year") + ylab(bquote('Fertilizer use ( ' *kg~ha^-1*')')) + 
  theme_bw() + scale_x_continuous(breaks=seq(1960, 2020, 15)) + 
  facet_wrap(~country, scales="free") + 
  geom_vline(xintercept = 1991, linetype="dotted", color = "red", size=1) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid)
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
peru$yield2 <-(peru$yield)/(10000)
pp2 <-ggplot(peru, aes(x=year, y=yield2)) + 
  geom_point(alpha=0.8, color="green", size=0.9) + geom_smooth(method="auto", color="green3") + 
  xlab("Year") + scale_x_continuous(breaks=seq(1960, 2020, 15)) +
  theme_bw() +  ylab(bquote('Yield of maize + beans ( '*"t"~ha^-1*')')) +
  facet_wrap(~country, scales="free")  +
  labs(caption = "Based on data from the FAO and the WB.") +
  theme(
    panel.background = element_rect(fill = NA), # bg of the panel, 
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 18),
    axis.text = element_text( size = 14))
# Get the ggplot grobs
per1 <- ggplotGrob(oo)
per2 <- ggplotGrob(pp2)
# Grab the panels from g2 and overlay them onto the panels of g1
pp_per <- c(subset(per1$layout, grepl("panel", per1$layout$name), select = t:r))
per <- gtable_add_grob(per1, per2$grobs[grepl("panel", per1$layout$name)], 
                       pp$t, pp$l, pp$b, pp$l)
# Function to invert labels
hinvert_title_grob <- function(grob){
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}
# Get the y label from g2, and invert it
index <- which(per2$layout$name == "ylab-l") 
ylab <- per2$grobs[[index]]                # Extract that grob
# Put the y label into g, to the right of the right-most panel
# Note: Only one column and one y label
per <- gtable_add_cols(per, per2$widths[per2$layout[index, ]$l], pos = max(pp$r))
per <-gtable_add_grob(per ,ylab, t = min(pp$t), l = max(pp$r)+1, 
                      b = max(pp$b), r = max(pp$r)+1,
                      clip = "off", name = "ylab-r")
# Get the y axis from g2, reverse the tick marks and the tick mark labels, 
# and invert the tick mark labels 
index <- which(per2$layout$name == "axis-l-1-1")  # Which grob
yaxis <- per2$grobs[[index]]                    # Extract the grob
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}
tml <- plot_theme(per)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
yaxis$children[[2]] <- ticks
# Put the y axis into g, to the right of the right-most panel
# Note: Only one column, but two y axes - one for each row of the facet_wrap plot
per <- gtable_add_cols(per, per2$widths[per2$layout[index, ]$l], pos = max(pp$r))
nrows = length(unique(pp$t)) # Number of rows
per <- gtable_add_grob(per, rep(list(yaxis), nrows), 
                       t = unique(pp$t), l = max(pp$r)+1,
                       b = unique(pp$b), r = max(pp$r)+1, 
                       clip = "off", name = paste0("axis-r-", 1:nrows))

###### MERGING ALL GRIDS FOR FINAL GRAPH ############### 
# option a)
left_axis <-textGrob(expression(paste('Fertilizer use ( ' , kg~ha^{-1}*')')), gp=gpar(fontsize=26), rot=90)
right_axis <-textGrob(expression(paste('Total Yield ( ' , t~ha^{-1}*')')), 
                      gp=gpar(fontsize=26), rot=-90)
bottom <-textGrob(label="Year", gp=gpar(fontsize=26))
grid.arrange(arg, bel, bol, bra, chi, col, cos, cub, dom, ecu, sal, gua, hai, hon, jam, mex, nic, 
             pan, par, per, left=left_axis, right=right_axis, bottom=bottom)

