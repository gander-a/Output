library(xtable)
library(xlsx)
library(corpus)
library(arrangements)

#Setup
rm(list=ls())
mainpath = "C:/Users/Armin/Desktop/Data Science/CSH Project/Colex_vader/Output/"
setwd(paste0(mainpath,"Scripts"))

#Select network: "clics3", "omegawiki" or "freedict"
net = "clics3"
fname = sprintf('%sFiles/filt_%s.Rda', mainpath, net)

#Load network
network_orig<-readRDS(fname)
network_orig$from_word = gsub("[[:punct:]]", "", network_orig$from_word)
network_orig$to_word = gsub("[[:punct:]]", "", network_orig$to_word)
network = network_orig

#Add reverse edges (for algorithm)
n = network_orig[,c(1,2,3,5,4)]
colnames(n) = colnames(network_orig)
network_orig = rbind(network_orig, n)

#Prepare network dataframe
weight = "LanguageWeight"
network = network[,c("from_word", "to_word", weight)]
colnames(network) = c("from", "to", "weight")
network$from = as.character(network$from)
network$to = as.character(network$to)
network$weight = as.numeric(as.character(network$weight))

#Helper function for splitting nodes
stopwords =c("something","somebody","someone","(something)","(somebody)","(someone)","something)","(somebody", "(someone","(something","somebody)", "someone)","verb","(of","(a", "a","(", ")",",","not", "or", "of", "to", "(to", "i", "the", "(the", "with", "in", "for")
split_and_clean <- function(word, par = FALSE) {
  wordsplit = strsplit(word," ")[[1]]
  wordsplit = gsub('[0-9]+', '', wordsplit)
  wordsplit = gsub(',', '', wordsplit)
  wordsplit = gsub("mother's", "mother", wordsplit)
  wordsplit = gsub("father's", "father", wordsplit)
  sw = wordsplit %in% stopwords
  wordsplit = wordsplit[!sw]
  wordsplit = wordsplit[wordsplit!=""]
  if (par == TRUE) {
    wordsplit = wordsplit[(!grepl("\\(", wordsplit))&(!grepl("\\)", wordsplit))]
  }
  if (length(wordsplit)>0) {
    return(wordsplit)
  } else {
    return(NA)
  }

}

#Node splitting algorithm
allnew = network[0,]
for (i in 1:nrow(network_orig)) {
  if (net == "clics3") {
    currentrow = network_orig[i,c(4,5,3)]
  }
  if (net == "omegawiki") {
    currentrow = network_orig[i, c(3,4,2)]
  }
  if (net == "freedict") {
    currentrow = network_orig[i, c(1,2,3)]
  }
  
  #Select entry from original network and split nodes
  word_raw = currentrow$from_word
  wordfrom = split_and_clean(currentrow$from_word, FALSE)
  wordto = split_and_clean(currentrow$to_word, FALSE)
  
  #Add new nodes and edges
  if (length(wordfrom)==1&length(wordto)==1|(is.na(wordfrom))|(is.na(wordto))) {
    newrows = as.data.frame(matrix(c(split_and_clean(currentrow$from_word),split_and_clean(currentrow$to_word),currentrow$LanguageWeight), 1, 3))
    colnames(newrows) = colnames(allnew)
  } else {
    newrows <- network[0,]
    
    #Add new nodes/edges if the terms from concept 1 are more then 1
    if (length(wordfrom)>1) {
      connectsfrom = network_orig[network_orig$to_word==word_raw,]
      connectsto = network_orig[network_orig$from_word==word_raw,]
      weight1 = sum(as.numeric(connectsfrom$LanguageWeight))
      weight2 = sum(as.numeric(connectsto$LanguageWeight))
      weight = weight1 + weight2
      
      #Word permutations
      comb = combinations(wordfrom, 2)
      for (j in 1:nrow(comb)) {
        word1 = comb[j,1]
        word2 = comb[j,2]
        word1 = gsub("\\)", "", word1)
        word1 = gsub("\\(", "", word1)
        word2 = gsub("\\)", "", word2)
        word2 = gsub("\\(", "", word2)
        
        newrow = as.data.frame(matrix(c(word1,word2,weight), 1, 3))
        colnames(newrow) = colnames(network)
        newrows = rbind(newrows, newrow)
        print(newrow)
      }
      
      #New edges
      word = split_and_clean(currentrow$from_word, TRUE)
      if (word != wordto) {
        for (ind1 in 1:length(word)) {
          for (ind2 in 1:length(wordto)) {
            word1 = word[ind1]
            word2 = wordto[ind2]
            newrow = as.data.frame(matrix(c(word1,word2,currentrow$LanguageWeight), 1, 3))
            colnames(newrow) = colnames(network)
            newrows = rbind(newrows, newrow)
            print(newrow)
          }
        }
      }

    }
    
    #Add edges
    if (length(wordto)>1) {
      word = split_and_clean(currentrow$from_word, TRUE)
      if (!is.na(word)&!is.na(wordto)) {
        for (j in 1:length(word)) {
          word1 = word[j]
          for (k in 1:length(wordto)) {
            word2 = wordto[k]
            newrow = as.data.frame(matrix(c(word1,word2,currentrow$LanguageWeight), 1, 3))
            colnames(newrow) = colnames(network)
            newrows = rbind(newrows, newrow)
            print(newrow)
          }
        } 
      }
    } 
    
  }
  
  allnew = rbind(allnew, newrows)
}

#Post-process the dataframe, remove NAs, remove duplicates
allnew = allnew[!duplicated(allnew), ]
allnewbefore = allnew
allnew = allnew[complete.cases(allnew),]
allnew$weight = as.numeric(as.character(allnew$weight))
colnames(allnew) = c("from_word", "to_word", "LanguageWeight")
allnew <- aggregate(list(allnew$LanguageWeight), by = list(allnew$from_word, allnew$to_word), sum)
colnames(allnew) = c("from_word", "to_word", "LanguageWeight")

#Additional step to remove the stopwords from above once more (not sure if this is necessary)
allnew = allnew[!(allnew$from_word %in% stopwords),]
allnew = allnew[!(allnew$to_word %in% stopwords),]
allnew$from_word = gsub("\\)", "", allnew$from_word)
allnew$to_word = gsub("\\)", "", allnew$to_word)

#Store network
fname = sprintf('%sFiles/filt_%s_extended.Rda',mainpath, net)
network = allnew
save(network, file = fname)
