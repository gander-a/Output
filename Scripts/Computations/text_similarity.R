text_sim = function(mainpath, text1, text2, sim) {
  
  #Load libraries
  library(textstem)
  library(dplyr)
  library(stats)
  
  #Lemmatize concept names
  words = lemmatize_strings(colnames(sim))
  
  #Remove stopwords
  #https://gist.github.com/sebleier/554280
  stops = c("can"  , " "      ,"i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very", "s", "t", "can", "will", "just", "don", "should", "now")
  t1 = text1[!(text1%in%stops)]
  t2 = text2[!(text2%in%stops)]
  
  #Remove digits
  t1 = gsub('[[:digit:]]+', '', t1)
  t2 = gsub('[[:digit:]]+', '', t2)
  
  #Remove empty strings
  t1 = t1[nchar(t1)>0]
  t2 = t2[nchar(t2)>0]
  
  #Lemmatize
  t1 = unique(lemmatize_strings(t1))
  t2 = unique(lemmatize_strings(t2))
  
  #Find matches
  joint = c((t1), (t2))
  dupli = joint[duplicated(joint)]
  delta = length(dupli)
  
  #Keep only duplicates
  dup_t1 = t1[t1 %in% dupli]
  dup_t2 = t2[t2 %in% dupli]
  
  #Create dataframe of exact matches and annotate a similarity of 1
  matches = as.data.frame(as.character(dup_t1))
  matches$V2 = as.character(dup_t1)
  matches$V3 = 1
  matches$V4 = dup_t1
  matches$V5 = dup_t2
  
  labs = c("word1", "word2", "sim", "freq1", "freq2")
  colnames(matches) = labs
  
  #Remove duplicates
  t1 = t1[!(t1 %in% dupli)]
  t2 = t2[!(t2 %in% dupli)]
  
  #Filter for words in the colexification network
  t1 = t1[t1 %in% words]
  t2 = t2[t2 %in% words]
  
  #Get lengths of text vector
  n = length(t1)+delta
  m = length(t2)+delta
  
  #Create tables to store similarities
  t1 = as.data.frame(as.matrix(table(t1)))
  t1$word = as.character(rownames(t1))
  t2 = as.data.frame(as.matrix(table(t2)))
  t2$word = as.character(rownames(t2))
  
  bothtexts = rbind(t1, t2)
  bothtexts = bothtexts %>% dplyr::group_by(word) %>% dplyr::summarise(freq = sum(V1))
  
  #Retrieve subset of similarity matrix with words appearing in one of the two texts
  sim_red = sim[words%in%rownames(t1), words%in%rownames(t2)]
  sim_red[is.na(sim_red)]=0
  
  #Matching algorithm
  values = as.data.frame(matrix(NA,1,5))
  c = 0
  res = values
  sim_red = as.data.frame(na.omit(sim_red))
  
  #Iterate while similarity matrix subset is not empty
  while(nrow(sim_red)>0 & ncol(sim_red)>0) {
    
    #Identify indices of highest similarity value
    inds = which(sim_red == max(sim_red), arr.ind = TRUE)
    
    #Continue if there are entries greater then 0
    if (max(max(sim_red))>0) {
      
    #Store highest similarity value and remove corresponding row(s) and column(s)
    for (i in 1:nrow(inds)) {
      ind = inds[i,]
      
      #Store values in res dataframe
      if (c == 0) {
        res = values
        res$V1 = rownames(sim_red)[ind[1]]
        res$V2 = colnames(sim_red)[ind[2]]
        res$V3 = sim_red[ind[1], ind[2]]
        res$V4 = mean(bothtexts$freq[bothtexts$word==lemmatize_strings(rownames(sim_red)[ind[1]])])
        res$V5 = mean(bothtexts$freq[bothtexts$word==lemmatize_strings(colnames(sim_red)[ind[2]])])
      } else {
        v = values
        v$V1 = rownames(sim_red)[ind[1]]
        v$V2 = colnames(sim_red)[ind[2]]
        v$V3 = mean(sim_red[ind[1], ind[2]], na.rm = TRUE)
        v$V4 = mean(bothtexts$freq[bothtexts$word==lemmatize_strings(rownames(sim_red)[ind[1]])])
        v$V5 = mean(bothtexts$freq[bothtexts$word==lemmatize_strings(colnames(sim_red)[ind[2]])])
        res = rbind(res,v)
      }
      c = c+1
    }
    }
    sim_red = sim_red[-c(inds[,1]), -c(inds[,2]), drop = FALSE]
  }
  
  #Remove similarities of 0 (probably not necessary)
  res = res[res$V3>0,]
  colnames(res) = labs
  
  #Bind exact matches and matching algorithm result
  allsim = rbind(res, matches)
  allsim = allsim[length(as.character(allsim$word1))>0,]
  
  #Incorporate google ngram data
  ngram = readRDS(file = sprintf("%sDatasets/GoogleNgram/ngram.Rda", mainpath))
  freq = ngram[ngram$lemma %in% c(allsim$word1, allsim$word2),]
  freq = freq %>% dplyr::group_by(lemma) %>% dplyr::summarise(sum = sum(freq))
  
  #Get frequencies for word pair words
  colnames(freq) = c("word1", "sum")
  allsim = merge(allsim, freq, by = "word1")
  colnames(freq) = c("word2", "sum")
  allsim = merge(allsim, freq, by = "word2")
  
  #Invert document frequencies to get a score/weight
  allsim$sum.x = log(1/allsim$sum.x)
  allsim$sum.y = log(1/allsim$sum.y)
  
  #Multiply weights for word1 and word2
  allsim$weight2 = rowMeans(allsim[,c("sum.x", "sum.y")])
  
  d = allsim[,c("sim", "weight2")]
  d$sim = d$sim*d$weight2
  
  #Compute final similarity score
  similarity = (sum(d$sim))*(m+n)/(2*m*n)

  return(similarity)
}