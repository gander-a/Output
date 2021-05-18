#Setup
rm(list=ls())
library(ggplot2)
library(reshape2)
library(ggalluvial)
library(dplyr)

#Setup directories
mainpath = "C:/Users/Armin/Desktop/Output/"
setwd(mainpath)

#Result files directory
folders = c("COHAfinal")

#Load result files
for (folder in folders) {
setwd(sprintf("%s/Results/%s", mainpath, folder))
f = list.files()

for (i in 1:length(f)) {
  if (i==1) {
    name = substr((f[i]), 1, 100)
    summary = read.csv(f[i], row.names=NULL, sep="")
    summary = summary[,-c(1)]
    summary$sim[is.na(summary$sim)] = 0
  } else {
    d = read.csv(f[i], row.names=NULL, sep="")
    d = d[,-c(1)]
    summary = rbind(summary, d)
  }
}

summary = summary[!duplicated(summary[,c(1,2,3)]),]
un = length(unique(summary$index))
print(un)

colnames(summary)[3] = c("year")
summary$year = as.character(summary$year)

#Add mirrored dataframe to account of symetry of text similarity metric
su = summary
su[,c(1,2)] = su[,c(2,1)]
colnames(su) = colnames(summary)
summary = rbind(summary, su)

#Aggregate
agg = summary %>% dplyr::group_by(year) %>% dplyr::summarise(meansim = mean(sim))
summary = merge(summary, agg, by.x = "year", by.y = "year")

#Plot boxplots

#Grouped boxplot
g = ggplot(summary, aes(x=year, y=sim, fill = meansim)) + 
  ggtitle("Intra-decade similarity") +
  geom_boxplot(outlier.shape = NA) +
  xlab("") + ylab("Similarity") +
  theme(panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=90),
        axis.text=element_text(size=30))+
  scale_fill_gradient(low="thistle1",high="orchid4") +
  theme(text = element_text(size = 40), plot.title = element_text(size=40))+
  scale_y_continuous(limits = c(4.5,7.5))+
  guides(fill=guide_legend(title="Mean similarity"))
  
plot(g)

#Store
pngname = sprintf("%sPlots/box_%s.png", mainpath, folder)
ggsave(pngname, width = 30, height = 20, units = "cm")

}
