find_similarity = function(data, sim) {
  library(svMisc)
  library(lsa)
  
  words = lemmatize_strings(colnames(sim))
  
  #Remove stopwords
  #https://gist.github.com/sebleier/554280
  stops = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very", "s", "t", "can", "will", "just", "don", "should", "now")
  # data = data[!(data$word1 %in% stops),]
  # data = data[!(data$word2 %in% stops),]
  
  
  for (k in 1:nrow(data)) {
    
    word1 = lemmatize_strings(data$word1[k])
    word2 = lemmatize_strings(data$word2[k])
    
    rowind = which(word1==words)
    colind = which(word2==words)
    
    if (length(colind)>0 & length(rowind)>0) {
      
      if (!is.na(sim[rowind[1], colind[1]])) {
        rat1 = sum(sum(sim[rowind, colind], na.rm = T))/(length(colind)*length(rowind))
        rat2 = sum(sum(sim[colind, rowind], na.rm = T))/(length(colind)*length(rowind))
        rat = 0.5*(rat1+rat2)
        data$sim[k] = rat
        row1 = 0.5*(as.vector(as.matrix(colMeans(as.data.frame(sim[rowind,]), na.rm = T)))+as.vector(as.matrix(rowMeans(as.data.frame(sim[,rowind]), na.rm = T))))
        row2 = 0.5*(as.vector(as.matrix(colMeans(as.data.frame(sim[colind,]), na.rm = T)))+as.vector(as.matrix(rowMeans(as.data.frame(sim[,colind]), na.rm = T))))
        check = (!is.na(row1)&!is.na(row2))
        data$cossim[k] = cosine(row1[check], row2[check])
        #print(sprintf("Similarity between %s and %s is: %s (cos: %s)", word1, word2, rat, data$cossim[k]))
        
      } else {
        data$sim[k] = -1
        data$cossim[k] = -1
      }
      
    } else {
      data$sim[k] = -1
      data$cossim[k] = -1
    }
    
  }
  
  return(data)
}