library(igraph)
library(xtable)
library(dplyr)
library(dils)
library(xlsx)

#Setup
rm(list=ls())
mainpath = "C:/Users/Armin/Desktop/Data Science/CSH Project/Colex_vader/Output/"
setwd(paste0(mainpath, "Scripts"))

#Load networks
for (net in c("clics3_extended", "omegawiki_extended", "freedict_extended")) {
  fname = sprintf('%sFiles/filt_%s.Rda', mainpath, net)
  load(fname)
  weight = "LanguageWeight"
  network = network[,c("from_word", "to_word", weight)]
  colnames(network) = c("from", "to", "weight")
  network$from = as.character(network$from)
  network$to = as.character(network$to)
  network$weight = as.numeric(network$weight)
  assign(net, network)
  rm(network)
}

#Create combined network with clics3 network as a base
network = clics3_extended
allwords = c(unique(clics3_extended$from), unique(clics3_extended$to))

#Iterate through omegawiki network: If an edges both words are in clics3, add this edge
for (i in 1:nrow(omegawiki_extended)) {
  currentrow = omegawiki_extended[i,]
  if (currentrow$from %in% allwords & currentrow$to %in% allwords) {
    network = rbind(network, currentrow)
  }
}

#Iterate through freedict network: If an edges both words are in clics3, add this edge
for (i in 1:nrow(freedict_extended)) {
  currentrow = freedict_extended[i,]
  if (currentrow$from %in% allwords & currentrow$to %in% allwords) {
    network = rbind(network, currentrow)
  }
}

#Store net network
fname = sprintf('%s/Files/combined_clics3based.Rda', mainpath)
save(network, file = fname)
