library(reprex)
library(tidyverse)
library(xlsx)
library(Cairo)
library(png)
library(grid)
library(directlabels)
library(svglite)

fancy <- theme(legend.justification = c("right", "centre"),
               legend.key = element_rect(fill="white", size = 3),
               legend.key.size = unit(1,"cm"),
               legend.text = element_text(size=30),
               text = element_text(size=20))+
  theme(legend.title = element_blank())

mydata <- read.xlsx("C:/Users/samue/OneDrive/Pictures/R/GermanPolls/GERNext.xlsx", 1)

sSize <- 0.07
kn <- 3

base <- ggplot(mydata) +
  geom_abline(color="grey50") +
  scale_y_continuous(minor_breaks = seq(0,50,1),breaks = seq(0, 50, 5), expand = c(0,0), limits = c(1,30)) +
  xlim(as.Date("2021-09-26"),as.Date("2025-09-30"))+
  #threshold
  geom_hline(yintercept=5, linetype="dashed")+
  #scatter
  geom_point(aes(Date, AfD, color="AfD"), alpha=0.8, size=0.5)+
  geom_point(aes(Date, FDP, color="FDP"), alpha=0.8, size=0.5)+
  geom_point(aes(Date, SPD, color="SPD"), alpha=0.8, size=0.5)+
  geom_point(aes(Date, GRN, color="GRUNE"), alpha=0.8, size=0.5)+
  geom_point(aes(Date, Union, color="Union"), alpha=0.8, size=0.5)+
  geom_point(aes(Date, Linke, color="Linke"), alpha=0.8, size=0.5)+
  scale_colour_manual(name="Parties", values=c("deepskyblue", "Yellow","forestgreen","mediumvioletred","red","black"))+
  geom_smooth(aes(Date, AfD),color="deepskyBlue", fill = "lightblue3",method="loess", size=2, se=FALSE,n=kn, span = sSize)+
  geom_smooth(aes(Date, SPD), color="Red",method="loess", size=2, se=FALSE,n=kn, span = sSize)+
  geom_smooth(aes(Date, FDP),color="Yellow",method="loess", size=2, se=FALSE,n=kn, span = sSize)+
  geom_smooth(aes(Date, GRN), color="forestgreen",method="loess", size=2, se=FALSE,n=kn, span = sSize)+
  geom_smooth(aes(Date, Union),color="black",method="loess", size=2, se=FALSE,n=kn, span = sSize)+
  geom_smooth(aes(Date, Linke),color="mediumvioletred",method="loess",n=kn, span = sSize,size=2, se=FALSE)+
  #Last Election
  geom_point(aes(as.Date("2021-09-26"),25.7), color= "red", size=4, shape=18, alpha=0.6)+
  geom_point(aes(as.Date("2021-09-26"),24.1), color= "black", size=4, shape=18, alpha=0.6)+
  geom_point(aes(as.Date("2021-09-26"),14.8), color= "forestgreen", size=4, shape=18, alpha=0.6)+
  geom_point(aes(as.Date("2021-09-26"),11.5), color= "yellow", size=4, shape=18, alpha=0.6)+
  geom_point(aes(as.Date("2021-09-26"),10.3), color= "deepskyblue", size=4, shape=18, alpha=0.6)+
  geom_point(aes(as.Date("2021-09-26"),4.9), color= "mediumvioletred", size=4, shape=18, alpha=0.6)+
  
guides(color = guide_legend(override.aes = list(size=10, stroke=NA, alpha=1)))+
  ylab("Support in %")+
  xlab("Date")

plot(base+fancy)

ggsave(file="polls.svg", plot=graph, width=18, height=8)

# workaround since svglite doesn't properly work in Wikipedia
aaa=readLines("polls.svg",-1)
bbb <- gsub(".svglite ", "", aaa)
writeLines(bbb,"polls.svg")