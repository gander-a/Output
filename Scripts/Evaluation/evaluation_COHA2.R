#Setup
rm(list=ls())
library(ggplot2)
library(reshape2)
library(ggalluvial)
library(dplyr)
library(RColorBrewer)

#Setup directories
mainpath = "C:/Users/Armin/Desktop/Output/"
setwd(mainpath)

#Result folder directory
folders = c("COHAfinal")

#Load results
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

#Modify dataframe format for plotting
colnames(summary)[c(3,4)] = c("dec1", "dec2")
summary$dec1 = as.character(summary$dec1)
summary$dec2 = as.character(summary$dec2)
 
summary = summary[!duplicated(summary[,c(1,2,3)]),]
un = length(unique(summary$index))
print(un)

#Aggregate
agg = summary %>% dplyr::group_by(dec1, dec2) %>% dplyr::summarise(meansim = mean(sim))
agg_orig = agg

#Plot boxplots
#Define the number of colors you want
nb.cols <- length(unique(agg$dec1))
mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(nb.cols)

# Thick lines plot
g = ggplot(agg, aes(x=dec2, y=meansim, group = dec1, color = dec1)) + 
  geom_line(lwd = 3, alpha = 0.7) +
  geom_point(size = 5, alpha = 0.7) +
  ggtitle("Similarity between decades") +
  xlab("") + ylab("Average similarity") +
  theme(panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=90),
        axis.text=element_text(size=30))+
  scale_colour_viridis_d()+
  theme(text = element_text(size = 40), plot.title = element_text(size=40))+
  guides(color=guide_legend(title="Decade"))+
  theme(legend.text=element_text(size=20))
  
plot(g)

pngname = sprintf("%s/Plots/coha2_%s.png", mainpath, folder)
ggsave(pngname, width = 30, height = 20, units = "cm")

#Alternative plot inncluding only the 1810s
agg = agg[agg$dec1=="1810s",]
g = ggplot(agg, aes(x=dec2, y=meansim, group = dec1, color = dec1)) + 
  geom_smooth(lwd = 2, linetype = "dashed") +
  geom_line(lwd = 2, alpha = 0.7) +
  geom_point(size = 4) +
  ggtitle("Similarity with 1810s texts") +
  xlab("") + ylab("Average similarity") +
  theme(panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=90),
        axis.text=element_text(size=30))+
  scale_colour_viridis_d()+
  theme(text = element_text(size = 40), plot.title = element_text(size=40))+
  theme(legend.position = "none")

plot(g)

pngname = sprintf("%sPlots/coha2_alt_%s.png", mainpath, folder)
ggsave(pngname, width = 30, height = 20, units = "cm")

#Change per decade plot
data = agg_orig
data$year1 = as.numeric(substr(data$dec1, 1, 4))
data$year2 = as.numeric(substr(data$dec2, 1, 4))
datasame = data[data$dec1==data$dec2,]
datachange = data[data$year1 == data$year2-10,]

d = merge(datasame, datachange, by = c("dec1"))
d$diff = d$meansim.y - d$meansim.x
d$nxt = NA
d$nxt = datasame$meansim[2:nrow(datasame)]

g = ggplot(d, aes(x=year2.y, y=nxt -5, fill = diff)) + 
  geom_bar(aes(y = diff), stat = "identity") +
  ggtitle("Change compared to previous decade") +
  xlab("") + ylab("Difference in similarity") +
  theme(panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=90),
        axis.text=element_text(size=15))+
  scale_fill_continuous(type = "viridis") +
  scale_x_discrete(limits=d$year2.y)+
  theme(text = element_text(size = 25), plot.title = element_text(size=40)) +
  guides(fill=guide_legend(title="Change"))
  
plot(g)

pngname = sprintf("%sPlots/coha2_change_%s.png", mainpath, folder)
ggsave(pngname, width = 30, height = 20, units = "cm")

}
