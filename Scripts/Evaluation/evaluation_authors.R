#Setup
rm(list=ls())
library(ggplot2)
library(reshape2)
library(ggalluvial)
library(dplyr)

#Setup directories
mainpath = "C:/Users/Armin/Desktop/Output/"
setwd(mainpath)

#MULTIPLE RESULT FILES
folders = c("guten_authors_final")

#Load files
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

summary = summary[!duplicated(summary),]
summary$author1 = as.character(summary$author1)
summary$author2 = as.character(summary$author2)

#Add mirrored result dataframe to account for symmetry of text similarity scores
su = summary
su[,c(1,2)] = su[,c(2,1)]
colnames(su) = colnames(summary)
summary = rbind(summary, su)

#Aggregate
sameauthor = summary[as.character(summary$author1)==as.character(summary$author2),]
sameauthor_mean = sameauthor %>% dplyr::group_by(author1, author2) %>% dplyr::summarise(meansim = mean(sim))

diffauthor = summary[as.character(summary$author1)!=as.character(summary$author2),]
diffauthor_mean = diffauthor %>% dplyr::group_by(author1) %>% dplyr::summarise(meansim = mean(sim))

#Create adequate format for plot
variety=rep(LETTERS[1:7], each=40)
treatment=rep(c("high","low"),each=20)
note=seq(1:280)+sample(1:150, 280, replace=T)
data=data.frame(variety, treatment ,  note)

summary$type = "empty"
summary$type[summary$author1==summary$author2]="same"
summary$type[summary$author1!=summary$author2]="different"

authornames = strsplit((summary$author1),",")
authornames = unlist(lapply(authornames, `[[`, 1))
summary$author1 = authornames
authornames = strsplit((summary$author2),",")
authornames = unlist(lapply(authornames, `[[`, 1))
summary$author2 = authornames

#Plot boxplot
g = ggplot(summary, aes(x=reorder(author1, sim, mean), y=sim, fill=type)) + 
  geom_boxplot() +
  ggtitle("Similarity by author") +
  xlab("") + ylab("Average similarity") +
  theme(panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=90),
        axis.text=element_text(size=25))+
  theme(text = element_text(size = 40), plot.title = element_text(size=40))
  
plot(g)

#Store
pngname = sprintf("%sPlots/box_%s.png", mainpath, folder)
ggsave(pngname, width = 30, height = 20, units = "cm")

}
