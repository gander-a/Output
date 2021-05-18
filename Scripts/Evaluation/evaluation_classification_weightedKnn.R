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

#Results directory
folders = c("baseline_final")

#Load results
for (folder in folders) {

setwd(sprintf("%sResults/%s", mainpath, folder))
f = list.files()

for (i in 1:length(f)) {
  if (i==1) {
    name = substr((f[i]), 1, 100)
    summary = read.csv(f[i], row.names=NULL, sep="")
    summary = summary[,-c(1)]
    rownames(summary) = colnames(summary)
    summary[is.na(summary)]=0
    check=as.matrix(summary)
    check[,]=0
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

groundtruth = unlist(lapply(strsplit(colnames(summary), "[.]"), `[[`, 1))

#Set up dataframes to store results
knn = as.data.frame(matrix(NA, (nrow(summary)-1), (nrow(summary))))
predlabel = c()
colnames(knn) = rownames(summary)

summary_orig = summary
summary_orig$genre = groundtruth
summary_orig$index = c(1:nrow(summary_orig))

plot = 0

#Compute classification results
maxiter = 120
for (iter in 1:maxiter) {
print(iter)
  if (iter < maxiter) {
    rpt = TRUE
  } else {
    rpt = FALSE
  }

set.seed(iter)
s = ddply(summary_orig,.(genre), function(x) x[sample(nrow(x), nrow(summary_orig)/length(unique(groundtruth)), replace = rpt),])
summary = s[,1:nrow(s)]
rownames(summary) = colnames(summary)

knn = as.data.frame(matrix(NA, (nrow(summary)-1), (nrow(summary))))
predlabel = knn
colnames(knn) = rownames(summary)
for (i in 1:(nrow(summary))) {
  print(i)
  ordered = summary[order(summary[,i], decreasing = TRUE),]
  truelabel = strsplit(colnames(summary)[i], "[.]")[[1]][1]
  for (k in 1:(nrow(summary)-1)) {
    labels = rownames(ordered)[1:k]
    labels = (strsplit(labels, "[.]"))
    labels = as.data.frame(unlist(lapply(labels, `[[`, 1)))
    labels$values = ordered[1:k,i]
    colnames(labels)[1] = c("lab")
    freq = as.data.frame(table(labels[,1]))
    labels = suppressMessages(labels %>% dplyr::group_by(lab) %>% dplyr::summarise(values = mean(values)))

    labels = labels[order(labels$lab),]
    freq = freq[order(freq$Var1),]

    labels$score = labels$values*log(freq$Freq)
    labels = labels[order(labels[,3], decreasing = TRUE),]
    
    if (labels$score[1]==0) {
      labels = labels[order(labels[,2], decreasing = TRUE),]
    }
    
    knn[k,i] = sum(grepl(truelabel, labels$lab[1]))
    predlabel[k,i] = as.character(labels$lab[1])
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

#Compute average results and set up for plotting
res$average = rowMeans(res)
assign(paste0("res_", folder), res[!is.na(res$average),])

res$sd = apply(res[,c(1:(ncol(res)-1))],1, sd, na.rm = TRUE)
res = res[!is.na(res$sd),]

#kNN development plot
maxpoint = max(res$average)

assign(paste0(substr(folder, 4, 40),"maxpoint"), maxpoint)
assign(paste0(substr(folder, 4, 40),"data"), res$average)

g = ggplot() +
  geom_ribbon(aes(x = 1:nrow(res), ymin = res$average-2*res$sd, ymax = res$average+2*res$sd), fill = "mediumpurple1") +
  geom_line(aes(x = 1:nrow(res), y=res$average), size = 3, color = "mediumpurple4") +
  geom_point(aes(x = which(maxpoint==res$average), y=maxpoint), size = 10, color = "mediumpurple4") +
  geom_text(aes(x = which(maxpoint==res$average)[1]+round(nrow(res)*0.1), y=maxpoint+0.07, label = as.character(round(maxpoint, digits = 4))), size = 15) +
  labs(y="Accuracy",
       x="k nearest neighbors",
       title=sprintf("Average Accuracy"),
       subtitle = sprintf("k:%s" ,which(maxpoint==res$average)[1])) +
  theme(panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=90),
        axis.text=element_text(size=40))+
  theme(text = element_text(size = 40))+
  scale_x_continuous(breaks = c(0,25,50,75,100,125,150,175,200,225))+
  scale_y_continuous(breaks = c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70), limits = c(0,0.70))+
  geom_line(aes(x = c(1:nrow(res)), y = 0.20), linetype = "dashed", size = 2)
plot(g)
pngname = sprintf("%sPlots/kNNa_%s_weighted.png", mainpath, folder)
ggsave(pngname, width = 30, height = 20, units = "cm")

#Bipartite graph
maxindex = which.max(res$average)
predlabel_orig = predlabel
predlabel = predlabel[,maxindex[1]]
d = as.data.frame(matrix(NA, length(predlabel), 0))
d$Category = groundtruth
d$Predicted = as.character(predlabel)
d$Weight = 1
d = d %>% dplyr::group_by(Category, Predicted) %>% dplyr::summarise(Weight = sum(Weight))

g = ggplot(as.data.frame(d),
           aes(y = Weight, axis1 = Category, axis2 = Predicted)) +
  geom_alluvium(aes(fill = Category), width = 1/12, knot.pos = 0.4) +
  geom_stratum(width = 1/5, fill = "white", color = "black") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)), size = 6) +
  scale_x_discrete(limits = c("Ground truth", "Predicted"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Paired") +
  ggtitle("Classification of labels (weighted)") +
  theme(panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y=element_blank(), axis.title.y = element_blank(), 
        axis.line.x = element_blank(), axis.line.y = element_blank())+
  theme(text = element_text(size = 30))
plot(g)
pngname = sprintf("%sPlots/flow_%s_weighted.png", mainpath, folder)
ggsave(pngname, width = 30, height = 20, units = "cm")

}