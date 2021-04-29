rm(list=ls())
library(ggplot2)
library(reshape2)
library(ggalluvial)
library(dplyr)

#Setup directories
mainpath = "C:/Users/Armin/Desktop/Output/"
setwd(mainpath)

#MULTIPLE RESULT FILES
folders = c("guten_books_1final", "guten_books_2final", "guten_books_3final", "guten_books_4final","guten_books_5final")

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

un = length(unique(summary$index))
print(un)

summary$V1 = as.character(summary$V1)
summary$V2 = as.character(summary$V2)

su = summary
su[,c(1,2)] = su[,c(2,1)]
colnames(su) = colnames(summary)
summary = rbind(summary, su)

samebooks = summary[as.character(summary$V1)==as.character(summary$V2),]
samebooks_mean = samebooks %>% dplyr::group_by(V1, V2) %>% dplyr::summarise(meansim = mean(sim))

diffbook = summary[as.character(summary$V1)!=as.character(summary$V2),]
diffbook_mean = diffbook %>% dplyr::group_by(V1) %>% dplyr::summarise(meansim = mean(sim))

#Plot boxplots

# create a data frame
variety=rep(LETTERS[1:7], each=40)
treatment=rep(c("high","low"),each=20)
note=seq(1:280)+sample(1:150, 280, replace=T)
data=data.frame(variety, treatment ,  note)

summary$type = "empty"
summary$type[summary$V1==summary$V2]="same"
summary$type[summary$V1!=summary$V2]="different"

xlabels = rep('book',15)
xlabels = paste0(xlabels, " ", as.character(1:15))

# grouped boxplot
g = ggplot(summary, aes(x=reorder(V1, sim, mean), y=sim, fill=type)) + 
  geom_boxplot() +
  ggtitle("Similarity by book") +
  xlab("") + ylab("Average similarity") +
  theme(panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=90),
        axis.text=element_text(size=25))+
  scale_fill_brewer(palette="Set2")+
  scale_x_discrete(labels=xlabels)+
  theme(text = element_text(size = 40), plot.title = element_text(size=40))
  
plot(g)

pngname = sprintf("%sPlots/box_%s.png", mainpath, folder)
ggsave(pngname, width = 30, height = 20, units = "cm")

}
