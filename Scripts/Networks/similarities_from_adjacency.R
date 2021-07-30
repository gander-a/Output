similarities_from_adjacency = function(adjmat, beta) {
  #Compute similarity matrix from adjacency matrix
  library(reticulate)
  norm = scale(adjmat, center = FALSE, scale = colSums(adjmat))
  ones = matrix(0, nrow(adjmat), ncol(adjmat))
  diag(ones) = 1
  before_inv = ones - beta * norm
  write.table(before_inv, "Networks/before_inv.csv", row.names = FALSE, col.names = FALSE, sep = ",")
  write.table(before_inv, "before_inv.csv", row.names = FALSE, col.names = FALSE, sep = ",")
  print("Table saved")
  
  #Run python script
  print('Run python script')
  use_python("C:/Users/Armin/Appdata/Local/r-miniconda/python.exe", required = T)
  py_run_file("Networks/Invert.py")
  inverse = data.matrix(read.csv("after_inv.csv", header = FALSE))
  
  #Compute similarities
  print('Compute similarities')
  result = inverse %*% norm
  file.remove("before_inv.csv")
  file.remove("after_inv.csv")
  
  return(result)
}