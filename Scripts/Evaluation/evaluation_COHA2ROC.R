rm(list=ls())
library(ggplot2)
library(reshape2)
library(ggalluvial)
library(dplyr)
library(pROC)
library(tidyverse)

#Setup directories
mainpath = "C:/Users/Armin/Desktop/Output/"
setwd(mainpath)

#MULTIPLE RESULT FILES
folders = c("COHAfinal")

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

colnames(summary)[c(3,4)] = c("dec1", "dec2")
summary$dec1 = as.character(summary$dec1)
summary$dec2 = as.character(summary$dec2)
 
summary = summary[!duplicated(summary[,c(1,2,3)]),]
un = length(unique(summary$index))
print(un)

su = summary
su[,c(1,2,3,4)] = su[,c(2,1,4,3)]
colnames(su) = colnames(summary)
summary = rbind(summary, su)

set.seed(11848230)
summary = summary[sample(1:nrow(summary), nrow(summary)*1.0),]

#Split into train and test set
summary = summary %>% dplyr::group_by(path1, path2, dec1, dec2) %>% dplyr::summarise(meansim = mean(sim))
summary$gt = summary$dec1==summary$dec2
summary$id = paste0(summary$path1, summary$path2, summary$dec1, summary$dec2)
id_unique = unique(summary$id)

prop = 0.7
set.seed(11848230)
train = sample(id_unique, length(id_unique)*prop, replace = FALSE)
test = id_unique[!(id_unique %in% train)]

train = summary[summary$id %in% train, ]
test = summary[summary$id %in% test, ]

decades = unique(test$dec1)
decades = decades[order(decades)]
res = as.data.frame(matrix(NA, length(decades), length(decades)))
colnames(res) = decades
rownames(res) = decades

for (i in 1:(length(decades)-1)) {
  for (j in (i+1):length(decades)) {
    print(paste(i,j))
    
    subset = train[train$dec1==decades[i],]
    subset = subset[(subset$dec2==decades[j])|(subset$dec2==decades[i]),]
    
    subset_test = test[(test$dec1 == decades[i]),]
    subset_test = subset_test[(subset_test$dec2 == decades[j])|(subset_test$dec2 == decades[i]),]
    
    boxplot(subset$meansim[subset$gt==TRUE], subset$meansim[subset$gt==FALSE], names = c("Same", "Diff"))
    
    for (k in 1:nrow(subset_test)) {
      row = subset_test[k,]
      
      true = sum(row$meansim >= subset$meansim[subset$gt==TRUE])/length(subset$meansim[subset$gt==TRUE])
      false = sum(row$meansim <= subset$meansim[subset$gt==FALSE])/length(subset$meansim[subset$gt==FALSE])
      row$score = true/(true+false)
      if (k == 1) {
        testres = row
      } else {
        testres = rbind(testres, row)
      }
    }
    
    testres$gt = as.numeric(testres$gt)
    
    stripchart(score ~ gt, data = testres)
    
    rocobj <- roc(testres$gt, testres$score)
    
    if (i == 1) {
      assign(paste0("g",j), rocobj)
    }
    auc = rocobj$auc
    
    res[i,j] = auc
    print(auc)
  }
}

#create Heatmap
colnames(res) = decades[order(decades)]
rownames(res) = decades[order(decades)]
res = as.data.frame(t(res))
dt2 <- res %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
head(dt2)

p = ggplot(dt2, aes(x = rowname, y = colname, fill = value)) +
  geom_tile(color = "white", aes(colour = NA))+
  theme(panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=90),
        axis.text=element_text(size=25))+
  theme(text = element_text(size = 25), plot.title = element_text(size=40)) +
  ggtitle("Same/Different decades as Text 1") +
  xlab("Text 2") + ylab("Text 1") +
  labs() +
  scale_fill_continuous(low="white", high="black",guide="colorbar",na.value="white") +
  guides(fill=guide_legend(title="AUC"))

plot(p)
pngname = sprintf("%sPlots/HeatAUC_%s.png", mainpath, folder)
ggsave(pngname, width = 30, height = 20, units = "cm")


}
