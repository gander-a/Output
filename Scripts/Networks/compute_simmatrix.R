library(igraph)
library(xtable)
library(dplyr)
library(dils)
library(xlsx)

#Setup path
rm(list=ls())
mainpath = "C:/Users/Armin/Desktop/Data Science/CSH Project/Colex_vader/Output/"
setwd(paste0(mainpath,"Scripts"))

#Load network
net = "combined_clics3based"
fname = sprintf('%sFiles/%s.Rda', mainpath,net)
load(fname)
network_orig = network

#Select and format columns
network$weight = as.numeric(network$weight)
colnames(network) = c("from", "to", "weight")

#Create igraph graph
g=graph.data.frame(network)

#Get adjacency matrix
adjmat = get.adjacency(g,sparse=FALSE, attr='weight')
nodelist = colnames(adjmat)
colnames(adjmat) = nodelist
rownames(adjmat) = nodelist

#Diagonal entries are self loops -> sum of all ingoing and outgiong edges
for (i in 1:nrow(adjmat)) {
  adjmat[i,i] = sum(adjmat[i,]) + sum(adjmat[,i])
}

#Loop over different beta values
betalist = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8 ,0.9)
betalist = c(0.8)

#Load script where inverting happens
source('Networks/similarities_from_adjacency.R')

for (beta in betalist) {

#Run computation is other script
result1 = similarities_from_adjacency(adjmat, beta)
result2 = similarities_from_adjacency(t(adjmat), beta)
result = (result1 + result2)*0.5

colnames(result) = nodelist
rownames(result) = nodelist

#Store resulting adjacency matrices
storename = sprintf("%sSimilarities/simil_list_beta_%s.csv",mainpath, beta)
write.csv(result, storename)
}
