#Setup
rm(list=ls())
library(ggplot2)
library(reshape2)
library(ggalluvial)
library(dplyr)
library(plyr)

#Setup directories
mainpath = "C:/Users/Armin/Desktop/Output/"
setwd(mainpath)

#Folder of results directory
folders = c("brown_final", "baseline_final")

#Load files
for (folder in folders) {
setwd(sprintf("%s/Results/%s", mainpath, folder))
f = list.files()

for (i in 1:length(f)) {
  if (i==1) {
    name = substr((f[i]), 1, 100)
    summary = read.csv(f[i], row.names=NULL, sep="")
    summary = summary[,-c(1)]
    rownames(summary) = colnames(summary)
    summary[is.na(summary)]=0
    check=as.matrix(summary)
    check[,]=NA
    check[summary!=0]=1
  } else {
    d = read.csv(f[i], row.names=NULL, sep="")
    d = d[,-c(1)]
    d[is.na(d)]=0
    summary=summary+d
    check[d!=0]=1
  }
}

m = as.matrix(summary)
diag(m) = 0
summary = as.data.frame(m)

#Format ground truth values
groundtruth = unlist(lapply(strsplit(colnames(summary), "[.]"), `[[`, 1))

#Set up matrices to store the results
knn = as.data.frame(matrix(NA, (nrow(summary)-1), (nrow(summary))))
predlabel = c()
colnames(knn) = rownames(summary)

summary_orig = summary
summary_orig$genre = groundtruth
summary_orig$index = c(1:nrow(summary_orig))

plot = 0

#Loop for bootstrap sample
maxiter = 120
for (iter in 1:maxiter){
  
print(iter)
set.seed(iter)
if (iter < maxiter) {
  rpt = TRUE
} else {
  rpt = FALSE
}
s = ddply(summary_orig,.(genre), function(x) x[sample(nrow(x), nrow(summary_orig)/length(unique(groundtruth)), replace = rpt),])
summary = s[,1:nrow(s)]
rownames(summary) = colnames(summary)

#Compute evaluation values
for (i in 1:nrow(summary)) {
  ordered = summary[order(summary[,i], decreasing = TRUE),]
  truelabel = strsplit(colnames(summary)[i], "[.]")[[1]][1]
  # print(truelabel)
  for (k in 1:(nrow(summary)-1)) {
    labels = rownames(ordered)[1:k]
    values = ordered[1:k,i]
    knn[k,i] = sum(grepl(truelabel, labels))/k
    if (k == 1) {
      predlabel = c(predlabel, strsplit(labels[k], "[.]")[[1]][1])
    }
  }
}

knn = knn[,order(colnames(knn))]
knnavg = as.data.frame(rowMeans(knn, na.rm = T))
colnames(knnavg) = c("Precision")

if (iter == 1) {
  res = knnavg
} else {
  res = cbind(res, knnavg)
}
}

res$average = rowMeans(res)
res = res[1:(nrow(summary)/2),]
assign(paste0("res_", folder), res[!is.na(res$average),])

if (maxiter > 1) {
#kNN development plot
maxpoint = (max(res$average))

assign(paste0(substr(folder, 4, 40),"maxpoint"), maxpoint)
assign(paste0(substr(folder, 4, 40),"data"), res$average)

res$sd = apply(res,1, sd, na.rm = TRUE)

g = ggplot() +
  geom_ribbon(aes(x = 1:nrow(res), ymin = res$average-2*res$sd, ymax = res$average+2*res$sd), fill = "grey70") +
  geom_line(aes(x = 1:nrow(res), y=res$average), size = 3) +
  geom_point(aes(x = which(maxpoint==res$average), y=maxpoint), size = 10) +
  geom_text(aes(x = which(maxpoint==res$average)[1]+round(nrow(res)*0.1), y=maxpoint+0.08, label = as.character(round(maxpoint, digits = 4))), size = 15) +
  labs(y="Accuracy",
       x="k nearest neighbors",
       title=sprintf("Average Accuracy"),
       subtitle = sprintf("k:%s - sd:%s" ,which(maxpoint==res$average)[1], round(res$sd[which(maxpoint==res$average)], digits = 4))) +
  theme(panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=90),
        axis.text=element_text(size=40))+
  theme(text = element_text(size = 40))+
  scale_x_continuous(breaks = c(0,25,50,75,100))+
  scale_y_continuous(breaks = c(0.20,0.30,0.40,0.50,0.60,0.70), limits = c(0.20,0.70))
plot(g)
pngname = sprintf("%sPlots/kNNa_%s.png", mainpath, folder)
ggsave(pngname, width = 30, height = 20, units = "cm")
}

#Bipartite graph
d = as.data.frame(matrix(NA, length(predlabel), 0))
d$Category = groundtruth
d$Predicted = predlabel
d$Weight = 1
d = d %>% dplyr::group_by(Category, Predicted) %>% dplyr::summarise(Weight = sum(Weight))

d$Category[d$Category=="belles_lettres"] = 'belles lettres'
d$Predicted[d$Predicted=="belles_lettres"] = 'belles lettres'

g = ggplot(as.data.frame(d),
           aes(y = Weight, axis1 = Category, axis2 = Predicted)) +
  geom_alluvium(aes(fill = Category), width = 1/12, knot.pos = 0.4) +
  geom_stratum(width = 1/5, fill = "white", color = "black") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)), size = 6) +
  scale_x_discrete(limits = c("Ground truth", "Predicted"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("Classification of labels") +
  theme(panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y=element_blank(), axis.title.y = element_blank(), 
        axis.line.x = element_blank(), axis.line.y = element_blank())+
  theme(text = element_text(size = 25))
plot(g)
pngname = sprintf("%sPlots/flow_%s.png",mainpath, folder)
ggsave(pngname, width = 30, height = 20, units = "cm")

}