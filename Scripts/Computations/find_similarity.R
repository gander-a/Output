find_similarity = function(data, sim) {
  library(svMisc)
  library(lsa)
  
  #Get lemmatized adjacency matrix names
  words = lemmatize_strings(colnames(sim))
  
  #Algorithm to get matrix entry for word pairs
  for (k in 1:nrow(data)) {
    
    #Lemmatize
    word1 = lemmatize_strings(data$word1[k])
    word2 = lemmatize_strings(data$word2[k])
    
    #Find indices
    rowind = which(word1==words)
    colind = which(word2==words)
    
    #If both words are in the colexification network
    if (length(colind)>0 & length(rowind)>0) {
      
      #Get corresponding values (symmetric matrix) and aggregate it to get word similarity score
      if (!is.na(sim[rowind[1], colind[1]])) {
        rat1 = sum(sum(sim[rowind, colind], na.rm = T))/(length(colind)*length(rowind))
        rat2 = sum(sum(sim[colind, rowind], na.rm = T))/(length(colind)*length(rowind))
        rat = 0.5*(rat1+rat2)
        data$sim[k] = rat
        row1 = 0.5*(as.vector(as.matrix(colMeans(as.data.frame(sim[rowind,]), na.rm = T)))+as.vector(as.matrix(rowMeans(as.data.frame(sim[,rowind]), na.rm = T))))
        row2 = 0.5*(as.vector(as.matrix(colMeans(as.data.frame(sim[colind,]), na.rm = T)))+as.vector(as.matrix(rowMeans(as.data.frame(sim[,colind]), na.rm = T))))
        check = (!is.na(row1)&!is.na(row2))
        data$cossim[k] = cosine(row1[check], row2[check])
        
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