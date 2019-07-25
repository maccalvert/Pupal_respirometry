###Below code analyzes the metabolic trajectories from respirometry experiments performed at the University of 
###Notre Dame 

##MBC 
##5/2/2019

library(ggplot2)

## Mount raglandlab drives before running scripts below 

### 1) Make mosaic plot of 2008 data 

setwd('/Volumes/denverlinux/ExtraDrive4/GJR_MacMirror/Rpom/RPom_2011Plus/OlderRespirometryData/Metabolic_rate_fall_2008')
dataAH<-read.table('HawAppleAllTrajectories_R.txt',sep="\t",header=T,row.names=NULL)
nA.shal<-sum(dataAH$pheno=='shal' &dataAH$fruit=='apple')
nA.nondia<-sum(dataAH$pheno=='nondia' &dataAH$fruit=='apple')
nA.dia<-sum(dataAH$pheno=='dia' &dataAH$fruit=='apple')
nA.tot<-sum(c(nA.shal,nA.nondia,nA.dia))

nH.shal<-sum(dataAH$pheno=='shal' &dataAH$fruit=='haw')
nH.nondia<-sum(dataAH$pheno=='nondia' &dataAH$fruit=='haw')
nH.dia<-sum(dataAH$pheno=='dia' &dataAH$fruit=='haw')
nH.tot<-sum(c(nH.shal,nH.nondia,nH.dia))

pA.shal<-nA.shal/nA.tot
pA.nondia<-nA.nondia/nA.tot
pA.dia <- nA.dia/nA.tot
pAs<-c(pA.shal,pA.nondia,pA.dia)

pH.shal<-nH.shal/nH.tot
pH.nondia<-nH.nondia/nH.tot
pH.dia <- nH.dia/nH.tot
pHs<-c(pH.shal,pH.nondia,pH.dia)


pupae_counts <- c(pA.shal, pA.nondia, pA.dia, pH.shal, pH.nondia, pH.dia)
race <- c(rep("Apple", 3), rep("Haw", 3))
class <- c(rep(c("shallow", "non-diapause", "diapause"), 2))
total <- c(rep(72, 3), rep(207, 3))

df <- data.frame(pupae_counts, race, class, total)


#code for mosaic plot adapted from post on stackexchange:
# https://stackoverflow.com/questions/19233365/how-to-create-a-marimekko-mosaic-plot-in-ggplot2/45688044#45688044
# User Z. lin's answer. 
library(ggplot2)
pdf("/media/raglandlab/ExtraDrive4/DiaClass/mosaic_2008.pdf")
ggplot(df,aes(x = race, y = pupae_counts, width = total, fill = class)) +
  geom_bar(stat = "identity", position = "fill", colour = "black") +
  # geom_text(aes(label = scales::percent(prop)), position = position_stack(vjust = 0.5)) + # if labels are desired
  facet_grid(~race, scales = "free_x", space = "free_x") +
  scale_fill_brewer(palette = "Dark2") +
  #scale_fill_brewer(palette = "RdYlGn") +
  # theme(panel.spacing.x = unit(0, "npc")) + # if no spacing preferred between bars
  #scale_y_continuous(sec.axis = sec_axis(~.*0.2, name = "Proportion diapause class"))+
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())+
  ylab("Propotion in diapause class")
dev.off()

### On to the trajectories 
#Need to rearrange the data set for use with ggplot2 
#9 measurements, all measurements need to go in the same column. 
met_rate <- unname(unlist(c(dataAH[,2:10])))
IDs <- rep(1:279, 9)
pheno <- rep(dataAH$pheno, 9)
block <- rep(dataAH$block, 9)
fruit <- rep(dataAH$fruit, 9)
days <- c(rep(5, 279), rep(9, 279), rep(12, 279), rep(16, 279), rep(19, 279), rep(23, 279), rep(26, 279), rep(30, 279), rep(33, 279))
met_traj_data <- data.frame(IDs, met_rate, days, pheno, block, fruit)
apple_traj <- met_traj_data[met_traj_data$fruit == "apple",]
haw_traj <- met_traj_data[met_traj_data$fruit == "haw",]
pdf("/media/raglandlab/ExtraDrive4/DiaClass/2008appletraj.pdf")
ggplot(data=apple_traj, aes(x=days, y=met_rate, group=IDs, color=pheno, linetype=pheno)) +
  geom_line() +
  scale_linetype_manual(values=c("twodash", "solid", "dashed"))+
  ylab("CO2/g/hr")+
  xlab("Days post pupation")+
  scale_color_brewer(palette = "Dark2")+
  theme_classic() 
dev.off()

pdf("/media/raglandlab/ExtraDrive4/DiaClass/2008hawtraj.pdf")
ggplot(data=haw_traj, aes(x=days, y=met_rate, group=IDs, color=pheno, linetype=pheno)) +
  geom_line() +
  scale_linetype_manual(values=c("twodash", "solid", "dashed"))+
  ylab("CO2/g/hr")+
  xlab("Days post pupation")+
  scale_color_brewer(palette = "Dark2")+
  theme_classic() 
dev.off()

setwd("../Metabolic_rate_fall_2009/RespirometryTrajectories/")
traj_2009 <- read.table('AllLiveFlys_R.txt',sep="\t",header=T,row.names=NULL)
met_rate <- unname(unlist(c(traj_2009[,2:12])))
IDs <- rep(1:197, 11)
pheno <- rep(traj_2009$pheno, 11)
block <- rep(traj_2009$block, 11)
#fruit <- rep(traj_2009$fruit, 11)
days <- c(rep(5, 197), rep(12, 197), rep(19, 197), rep(26, 197), rep(33, 197), rep(40, 197), rep(47, 197), rep(54, 197), rep(61, 197), rep(68, 197), rep(75, 197))
traj_2009 <- data.frame(met_rate, IDs, days, pheno, block)

pdf("~/Desktop/Grad_school/Clines_3/V7/supp_fig_1.pdf")
ggplot(data=traj_2009, aes(x=days, y=met_rate, group=IDs, color=pheno, linetype=pheno)) +
  geom_line() +
  scale_linetype_manual(values=c("twodash", "solid", "dashed"))+
  ylab("CO2/g/hr")+
  xlab("Days post pupation")+
  scale_color_brewer(palette = "Dark2")+
  theme_classic() 
dev.off()

#Glen's eclosion data. 
setwd('../../../MetRateFall2009_genotyping/')
eclData<-read.table('GlenPreWinterExp2009_R.txt',sep="\t",header=T,row.names=NULL)
eclData <- eclData[which(!is.na(eclData$PupationDate)), ]
eclDataSDND <- eclData[eclData$class == "F",]
#ND flies were sampled as those 29-31 days 
#SD were sampled after 50 days, so we will color accordingly by adding new 
#column to data frame above with ND, SD, and NS (for non sample )
ND <- sum(eclDataSDND$daysToEcl <= 31 & eclDataSDND$daysToEcl >= 29)
NS <- sum(eclDataSDND$daysToEcl >= 32 & eclDataSDND$daysToEcl <= 49)
SD <- sum(eclDataSDND$daysToEcl >= 50)
samp_vec <- vector(length = nrow(eclDataSDND))
for(i in 1:nrow(eclDataSDND)){
  if(eclDataSDND$daysToEcl[i] >= 29 & eclDataSDND$daysToEcl[i] <= 32){
    samp_vec[i] <- "ND"
  }else if(eclDataSDND$daysToEcl[i] >= 32 & eclDataSDND$daysToEcl[i] <= 49){
    samp_vec[i] <- "NS"
  }else if(eclDataSDND$daysToEcl[i] >= 50){
    samp_vec[i] <- "SD"
  }else if(eclDataSDND$daysToEcl[i] <= 28){
    samp_vec[i] <- "NS"
  }
}

eclDataSDND$samp <- samp_vec
library(scales)
library(RColorBrewer)
library(ggplot2)
pdf("~/Desktop/Grad_school/Clines_3/V7/Supp_fig2.pdf")
mypalette <- brewer_pal(9,"Dark2")
ggplot(eclDataSDND, aes(x=daysToEcl, fill=samp, color=samp))+
  geom_histogram(linetype="solid", binwidth = 1, position="identity", alpha=0.5)+
  scale_color_manual(values=c(mypalette(9)[2],"grey",mypalette(9)[3]))+
  scale_fill_manual(values=c(mypalette(9)[2],"grey",mypalette(9)[3]))+
  theme_classic()+
  ylab("Frequency")+
  xlab("Days to adult emergence")
dev.off()
