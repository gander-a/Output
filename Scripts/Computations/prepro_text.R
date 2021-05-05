prepro_text = function(text) {
  text = paste(text, collapse = "")
  text = gsub('[[:punct:] ]+',' ',text)
  text = gsub('[[:digit:]]+', '', text)
  return(unlist(strsplit(tolower(text), " ")))
}