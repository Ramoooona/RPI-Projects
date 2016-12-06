rm(list=ls()) #reset work space

# Load library
library(readr)
library(ggplot2)
library(grid)
library(gridExtra)
library(reshape)
library("vegan")

# Import Dataset

# Original 3 months
july <- read_csv("C:/.../Data Files/july.csv")
aug <- read_csv("C:/.../Data Files/aug.csv")
sep <- read_csv("C:/.../Data Files/sep.csv")

# Env + 5 groups
allGroup <- read_csv("C:/.../Data Files/allGroup.csv")
augGroup <- read_csv("C:/.../Data Files/augGroup.csv")
sepGroup <- read_csv("C:/.../Data Files/sepGroup.csv")
julyGroup <- read_csv("C:/.../Data Files/julyGroup.csv")

# Average
avgGroup <- read_csv("C:/.../Data Files/avgGroup.csv")

# Significant 3 months
allSig <- read_csv("C:/.../Data Files/allSig.csv")


### Plots

## Plot 1: Species v.s. Salt concentration & fish
names(avgGroup)[names(avgGroup)=="salt concentration"] <- "salt.concentration"
avgfish <- avgGroup[which(avgGroup$chain=="Fish"),]
avgno <- avgGroup[which(avgGroup$chain=="No Fish"),]
avg1<-as.data.frame(avgfish[,c(3,12:14)])
avg2<-as.data.frame(avgno[,c(3,12:14)])
avg1 <- melt(avg1, id="salt.concentration")  # convert to long format
avg2 <- melt(avg2, id="salt.concentration")
# creatures change as the increase of salt concentration with fish
p1=ggplot(data=avg1,
          aes(x=salt.concentration, y=value, colour=variable)) +
  geom_line()+ylim(1,800)+ylab("Count")+geom_point(size=2)+
  ggtitle("Species Count with Fish")+ geom_text(aes(label=value),hjust = 0, nudge_x = 0.05)
# creatures change as the increase of salt concentration without fish
p2=ggplot(data=avg2,
          aes(x=salt.concentration, y=value, colour=variable)) +
  geom_line()+ylab("Count")+geom_point(size=2)+
  ggtitle("Species Count without Fish")+ geom_text(aes(label=value),hjust = 0, nudge_x = 0.05)
grid.arrange(p1, p2,ncol = 2)


## Plot 2: Species v.s. Salt concentration & fish
# Cladocerans count with/without fish
p1 = ggplot(avgGroup, aes(x = salt.concentration, y = cladoceran, shape = chain, colour = chain)) + 
  geom_point(size=4)+geom_line(aes(colour = chain))+ylab("Phytoplankton Count")+
  ggtitle("Salt vs cladoceran")+ geom_text(aes(label=phytoplankton),hjust = 1,vjust=2, size=3)
# Copepods count with/without fish
p2 = ggplot(avgGroup, aes(x = salt.concentration, y = copepods, shape = chain, colour = chain)) + 
  geom_point(size=4)+geom_line(aes(colour = chain))+ylab("Periphyton Count")+
  ggtitle("Salt vs Copepod")+ geom_text(aes(label=periphyton),hjust = 1, vjust=2, size=3)
# Routifer count with/without fish
p3 = ggplot(avgGroup, aes(x = salt.concentration, y = rotifer, shape = chain,colour=chain)) + 
  geom_point(size=4)+geom_line(aes(colour = chain))+ylab("Cladoceran Count")+
  ggtitle("Salt vs Rotifer")+ geom_text(aes(label=cladoceran),hjust = 1, vjust=2, size=3)
grid.arrange(p1,p2,p3,ncol = 3)


## Plot 3: Regression
plots <- list()  # new empty list
avgfish <- avgGroup[which(avgGroup$chain=="Fish"),]
avgno <- avgGroup[which(avgGroup$chain=="No Fish"),]
p1 = ggplot(avgfish, aes(x = salt.concentration, y = cladoceran))+geom_point() + stat_smooth(method = lm)+ggtitle("salt vs cladoceran with fish")
p2 = ggplot(avgfish, aes(x = salt.concentration, y =copepods))+geom_point() + stat_smooth(method = lm)+ggtitle("salt vs copepods with fish")
p3 = ggplot(avgfish, aes(x = salt.concentration, y = rotifer))+geom_point() + stat_smooth(method = lm)+ggtitle("salt vs rotifer with fish")
p4 = ggplot(avgno, aes(x = salt.concentration, y = cladoceran))+geom_point() + stat_smooth(method = lm)+ggtitle("salt vs cladoceran without fish")
p5 = ggplot(avgno, aes(x = salt.concentration, y = copepods))+geom_point() + stat_smooth(method = lm)+ggtitle("salt vs copepods without fish")
p6 = ggplot(avgno, aes(x = salt.concentration, y = rotifer))+geom_point() + stat_smooth(method = lm)+ggtitle("salt vs rotifer without fish")
source("http://peterhaschke.com/Code/multiplot.R")
multiplot(p1,p4,p2,p5,p3,p6, cols = 3)


## Plot 4: Correlation among Species when fish exist
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor)) 
    cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * (1 + r)/2)
}
panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "white", ...)
}
allfish <- allGroup[which(allGroup$chain=="Fish"),]
allno <- allGroup[which(allGroup$chain=="No Fish"),]
pairs(allfish[, 10:14], upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)


## Plot 5: Correlation among env factors with/without fish
pairs(allfish[, c(3,5,6,8)], upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
pairs(allno[, c(3,5,6,8)], upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)


## Plot 6:Salt - Phytoplankton - water quality
ggplot(avgGroup, aes(x = salt.concentration, y = predator, shape = chain, colour = chain)) + 
  geom_point(size=2)+ylab("Predator Count/Liter")+ xlab("Salt Concentration (milligram/liter)")+stat_smooth(method = lm,size=2)+
  ggtitle("Increased Salt Concentration Restricts Phytoplankton Predators")+ geom_text(aes(label=predator),hjust = 1,vjust=2, size=3)
ggplot(avgGroup, aes(x = salt.concentration, y = phytoplankton, shape = chain, colour = chain)) + 
  geom_point(size=2)+ylab("Phytoplankton Count/Liter")+ xlab("Salt Concentration (milligram/liter)")+ stat_smooth(method = lm,size=2)+
  ggtitle("Increased Salt Concentration Produces More Phytoplankton")+ geom_text(aes(label=phytoplankton),hjust = 1,vjust=2, size=3)


## Plot 7: Salt - Phytoplankton - time
names(julyGroup)[names(julyGroup)=="salt concentration"] <- "salt.concentration"
names(augGroup)[names(augGroup)=="salt concentration"] <- "salt.concentration"
names(sepGroup)[names(sepGroup)=="salt concentration"] <- "salt.concentration"
p1=ggplot(julyGroup, aes(x = salt.concentration, y = phytoplankton, shape = chain, colour = chain)) + 
  geom_point(size=2)+ylab("Phytoplankton Count/Liter")+ xlab("Salt Concentration (milligram/liter)")+ stat_smooth(method = lm,size=2)+
  ggtitle("Salt v.s. Phytoplankton in July")+geom_text(aes(label=phytoplankton),hjust = 1,vjust=2, size=3)
p2=ggplot(augGroup, aes(x = salt.concentration, y = phytoplankton, shape = chain, colour = chain)) + 
  geom_point(size=2)+ylab("Phytoplankton Count/Liter")+ xlab("Salt Concentration (milligram/liter)")+ stat_smooth(method = lm,size=2)+
  ggtitle("Salt v.s. Phytoplankton in August")+ geom_text(aes(label=phytoplankton),hjust = 1,vjust=2, size=3)
p3=ggplot(sepGroup, aes(x = salt.concentration, y = phytoplankton, shape = chain, colour = chain)) + 
  geom_point(size=2)+ylab("Phytoplankton Count/Liter")+ xlab("Salt Concentration (milligram/liter)")+ stat_smooth(method = lm,size=2)+
  ggtitle("Salt v.s. Phytoplankton in September")+ geom_text(aes(label=phytoplankton),hjust = 1,vjust=2, size=3)
grid.arrange(p1,p2,p3,ncol = 3)


### Vegan

# DCA
decorana(allSig[,3:14])

names(allSig)[names(allSig)=="salt concentration"] <- "salt.concentration"
names(allSig)[names(allSig)=="dissolved oxygen"] <- "dissolved.oxygen"

# Split allSig to alldata(species) and allenv(environmental factors)
alldata <- allSig[,10:28]
allenv <- allSig[,c(2,3,5,6,8)] # no tank number, start from chain(fish or not)
# set temperature as time

# Plot 8: CCA for all
cca <- cca(alldata,allenv[,2:5])
allenv$chain <- as.factor(allenv$chain)
with(allenv,levels(chain))
colvec <- c("red2", "green4")
plot(cca,type = "p",scaling = 3,main="CCA for All Sample Tanks")
with(allenv,points(cca,display = "sites",col=colvec[chain],
                   scaling = 3,pch=21,bg=colvec[chain]))
head(with(allenv,colvec[chain]))
text(cca,display = "species",scaling = 3,cex=0.75,col="black")
with(allenv,legend(2,2,legend=levels(chain),bty="n",col=colvec,
                   pch=21,pt.bg=colvec))


# Plot 9: Biplot for 3 months (compare)
par(mfrow=c(1,3)) 
# CCA of july 
julyenv <- julyGroup[,c(2,3,4,5,7)]
julydata <- julyGroup[,9:13]
ccaj <- cca(julydata,julyenv[,2:5])
ccaj # 38.4%
# CCA for august
augenv <- augGroup[,c(2,3,4,5,7)]
augdata <- augGroup[,9:13]
ccaa <- cca(augdata,augenv[,c(2,3,4,5)])
ccaa # 35.25%
# CCA for september
sepenv <- sepGroup[,c(2,3,4,5,7)]
sepdata <- sepGroup[,9:13]
ccas <- cca(sepdata,sepenv[,c(2,3,4,5)])
ccas # 22.271%

# Biplot for july
julyenv$chain <- as.factor(julyenv$chain)
with(julyenv,levels(chain))
plot(ccaj,type = "p",scaling = 3,main="July")
with(julyenv,points(ccaj,display = "sites",col=colvec[chain],
                    scaling = 3,pch=21,bg=colvec[chain]))
head(with(julyenv,colvec[chain]))
text(ccaj,display = "species",scaling = 3,cex=1.5,col="brown")

# Biplot for august
augenv$chain <- as.factor(augenv$chain)
with(augenv,levels(chain))
plot(ccaa,type = "p",scaling = 3,main="August")
with(augenv,points(ccaa,display = "sites",col=colvec[chain],
                   scaling = 3,pch=21,bg=colvec[chain]))
head(with(augenv,colvec[chain]))
text(ccaa,display = "species",scaling = 3,cex=1.5,col="brown")

# Biplot for September
sepenv$chain <- as.factor(sepenv$chain)
with(sepenv,levels(chain))
plot(ccas,scaling = 3,type = "p",main="September")
with(sepenv,points(ccas,display = "sites",col=colvec[chain],
                   scaling = 3,pch=21,bg=colvec[chain]))
head(with(sepenv,colvec[chain]))
text(ccas,display = "species",scaling = 3,cex=1.5,col="brown")
with(sepenv,legend(2,-2,legend=levels(chain),bty="n",col=colvec,pch=21,pt.bg=colvec,cex=1.5))
