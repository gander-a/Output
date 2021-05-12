prepro_text = function(text) {
  #Change format to string vector
  text = paste(text, collapse = "")
  
  #Remove punctuation and digits
  text = gsub('[[:punct:] ]+',' ',text)
  text = gsub('[[:digit:]]+', '', text)
  
  #Unlist and return
  return(unlist(strsplit(tolower(text), " ")))
}