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

# summary = summary %>% dplyr::group_by(V1, V2, author1, author2) %>% dplyr::summarise(meansim = mean(sim))
summary$gt = summary$V1==summary$V2
summary$id = paste0(summary$V1, summary$V2, summary$author1, summary$author2)
id_unique = unique(summary$id)

prop = 0.7
set.seed(11848230)
train = sample(id_unique, length(id_unique)*prop, replace = FALSE)
test = id_unique[!(id_unique %in% train)]

train = summary[summary$id %in% train, ]
test = summary[summary$id %in% test, ]

for (i in 1:nrow(test)) {
  print(i)
  row = test[i,]
  subset = train[train$V1 == row$V1,]
  false = sum(row$sim >= subset$sim[subset$gt==FALSE])/length(subset$sim[subset$gt==FALSE])
  row$score = false
  if (i == 1) {
    testres = row
  } else {
    testres = rbind(testres, row)
  }
}

testres$gt = as.numeric(testres$gt)

library(pROC)

#define object to plot
rocobj <- roc(testres$gt, testres$score)
auc = rocobj$auc

#create ROC plot
p = ggroc(rocobj, colour = "#7570b3", size = 3) +
  theme(panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=90),
        axis.text=element_text(size=40))+
  theme(text = element_text(size = 40), plot.title = element_text(size=40)) +
  ggtitle("ROC curve") +
  xlab("Sensitivity") + ylab("Specificity") +
  labs(subtitle = sprintf("AUC: %s", round(auc, 4))) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed")

plot(p)
pngname = sprintf("%sPlots/ROC_%s.png", mainpath, folder)
ggsave(pngname, width = 30, height = 20, units = "cm")

}
