#Setup
rm(list=ls())
#Load libraries
library(xtable)
library(readr)
library(textstem)
library(ggplot2)
library(ggrepel)
library(xlsx)
library(Hmisc)
library(lsa)
library(DescTools)
library(MASS)
library(ggpubr)
library(bestNormalize)
library(boot)

#Setup directories
mainpath = "C:/Users/Armin/Desktop/Output/"
setwd(mainpath)

#Chose which networks and datasets to use
nets = c("combined_clics3based")
# datasets = c("MEN", "SimLex", "SimVerb")
# datasets = c("Creativity")
datasets = c("Creativity")

#Choose b = beta parameter, l = lower_threshold parameter
b = c(0.8)
l = c(0.5)

c=1

#Run loop over all networks
for (i in 1:length(nets)) {
  
  source("Scripts/Computations/setup_sim_matrix.R")
  
  #Run loop over all beta parameter values
  for (beta in b) {
    
  #Run loop over all lower_threshold parameter values
  for (lower_th in l) {
  
  net = nets[i]
  sim = setup_sim_matrix(mainpath, net, beta, lower_th)
  
  #Get words in the colexification network
  words = lemmatize_strings(colnames(sim))
  
  #Load dataset
  for (j in 1:length(datasets)) {
    ds = datasets[j]
    allpearson = c()
    allspearman = c()
    
    if (ds == "Creativity") {
      allpearsonff = c()
      allspearmanff = c()
    }
    
    nmax = 200
    for (s in 1:nmax) {
      print(s)
      
    if (ds == "MEN") {
      data = read_table2("Datasets/Wordsimilarity/MEN/MEN_dataset_natural_form_full",col_names = FALSE)
      data$X3 = data$X3/50
      data$sim = NA
      data$cossim = NA
      colnames(data) = c("word1", "word2", "rating", "sim", "cossim")
      data_orig = data
      
    }
    if (ds == "SimVerb") {
      data = read.delim("Datasets/Wordsimilarity/SimVerb/SimVerb-3500.txt", header=FALSE)
      data = data[,c(1,2,4)]
      data$V4 = as.numeric(data$V4)/10
      data$sim = NA
      data$cossim = NA
      colnames(data) = c("word1", "word2", "rating", "sim", "cossim")
      data_orig = data
    }
    if (ds == "SimLex") {
      data = read_table2("Datasets/Wordsimilarity/SimLex-999/SimLex-999.txt")
      data = data[,c(1,2,4)]
      data$SimLex999 = as.numeric(data$SimLex999)/10
      data$sim = NA
      data$cossim = NA
      colnames(data) = c("word1", "word2", "rating", "sim", "cossim")
      data_orig = data
    }
    
    if (ds == "Creativity") {
      data = read.csv("Datasets/Wordsimilarity/FF/forwardflow.txt")
      data = data[,-c(1)]
      
      #Store original creativity ratings
      data$crea_orig = data$Creativity    
      
      #Scale the creavitiy score to [0,1] and subtract it from 1 such that higher rating is equal to higher similarity
      range01 <- function(x){(x-min(x))/(max(x)-min(x))}
      data$Creativity = range01(data$Creativity)
      data$Creativity = 1 - (data$Creativity)
      data$sim = NA
      data$cossim = NA
      colnames(data) = c("word1", "word2", "rating", "ff", "class", "id", "creativity_orig","sim", "cossim")
      
      data$word1 = tolower(data$word1)
      data$word2 = tolower(data$word2)
      
      #Load forward flow distance ratings
      data_ff = read.csv("Datasets/Forward_flow/FF_FF.csv", comment.char="#")
      data_ff$Flow = as.numeric(as.character(data_ff$Flow))
      data$ff_orig = data_ff$Flow
      data_orig = data
      
      data = data[!duplicated(data),]
      data = data[!is.na(data$creativity_orig),]
      data = data[!is.na(data$ff_orig),]
      
      #Get correlation between original creativity ratings and ff distance ratings
      cc_ff_crea_pearson = c()
      cc_ff_crea_spear = c()
      for (it in 1:200) {
        # print(it)
        d = data[sample(1:nrow(data),nrow(data), replace = TRUE),]
        cc_ff_crea_pearson = c(cc_ff_crea_pearson,cor(d$ff_orig, d$creativity_orig, method = "pearson"))
        cc_ff_crea_spear = c(cc_ff_crea_spear, cor(d$ff_orig, d$creativity_orig, method = "spearman"))
      }
      
      #Get confidence intervals for correlation scores between original creativity ratings and ff distance ratings
      print(mean(cc_ff_crea_pearson))
      print(quantile(cc_ff_crea_pearson, 0.025))
      print(quantile(cc_ff_crea_pearson, 0.975))

      print(mean(cc_ff_crea_spear))
      print(quantile(cc_ff_crea_spear, 0.025))
      print(quantile(cc_ff_crea_spear, 0.975))

    }
      
    #Sample with repetition from data
    set.seed(s)
    data = data[sample(1:nrow(data), nrow(data), replace = TRUE),]
    
    #Load and apply function to retrieve similarity values of word pairs
    source("Scripts/Computations/find_similarity.R")
    data = find_similarity(data, sim)
    
    #For results dataframe
    data$word1 = as.character(data$word1)
    data$word2 = as.character(data$word2)
    
    #Setup results dataframe
    results = as.data.frame(matrix(NA,length(datasets)*length(b)*length(l),16))
    colnames(results) = c("Network","Threshold","Number_Words","Dataset", "Unique_words","Number_WPs", "WPs_included","Coverage","Pearson", "Spearman", "Kendall", "Pearson_cos", "Spearman_cos", "Kendall_cos", "Pearson_Pvalue", "Spearman_Pvalue")
    
    #Fill results dataframe
    results$Network[c] = strsplit(net, "_")[[1]][1]
    results$Threshold[c] = as.character(lower_th)
    results$Number_Words[c] = length(words)
    results$Dataset[c] = ds
    results$Unique_words[c] = length(unique(c(unique(data$word1), unique(data$word2))))
    results$Number_WPs[c] = nrow(data)
    
    #Store word pairs and corresponding ratings in Excel
    data_all = data
    data_all$sim[data_all$sim==-1] = NA
    
    #Remove 'invalid' rows
    data = data[data$sim!=-1,]
    data = data[data$sim!=-Inf,]

    #Continue filling result statistics dataframe
    results$WPs_included[c] = nrow(data)
    results$Coverage[c] = as.numeric(results$WPs_included[c])/as.numeric(results$Number_WPs[c])
    results$Pearson[c] = cor(data$rating, data$sim, method = "pearson")
    
    allpearson = c(allpearson, results$Pearson[c])
    
    print(results$Pearson[c])
    
    #Perform hypothesis test to check if results are statistically significant
    a = cor.test(data$rating, data$sim, method = "pearson")
    results$Pearson_Pvalue[c] = a$p.value
    
    #Perform hypothesis test to check if results are statistically significant
    results$Spearman[c] = cor(data$rating, data$sim, method = "spearman")
    a = cor.test(data$rating, data$sim, method = "spearman", exact = FALSE)
    results$Spearman_Pvalue[c] = a$p.value
    
    allspearman = c(allspearman, results$Spearman[c])
    
    #Fill results statistics dataframe
    results$Kendall[c] = cor(data$rating, data$sim, method = "kendall")
    results$Pearson_cos[c] = cor(data$rating, data$cossim, method = "pearson")
    results$Spearman_cos[c] = cor(data$rating, data$cossim, method = "spearman")
    results$Kendall_cos[c] = cor(data$rating, data$cossim, method = "kendall")
    
    if (s == nmax) {
    
    #Select color of scatterplots
    if (ds=="MEN") {
      col = "#1b9e77"
    }
    if (ds == "SimVerb") {
      col = "#d95f02"
    }
    if (ds == "SimLex") {
      col = "#7570b3"
    }
    if (ds == "Creativity") {
      col = "#e7298a"
    }
    
    #Plot scatterplot
    g = ggplot(data, aes(x=rating, y = sim)) +
      geom_point(alpha = 0.6, position="identity", color = col, size = 8) +
      geom_smooth(color = col, fill = col, size = 3) +
      # geom_text_repel(hjust=0, vjust=0,size=10)+
      labs(y="Predicted values",
           x="Ground truth rating",
           title=sprintf("%s dataset", ds), 
           subtitle = sprintf("Pearson: %s, Spearman: %s", round(mean(allpearson), digits = 4), round(mean(allspearman), digits = 4))) +
      theme(panel.grid.minor = element_blank(),panel.background = element_blank(),
            axis.line = element_line(colour = "black"))+
      theme(text = element_text(size = 35)) +
      theme(legend.position = "none") +
      scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1), limits = c(0, 1))
    
    plot(g)
    pngname = sprintf("Plots/%s_%s_quant%s_similarity_%s.png", ds, net, as.character(lower_th), beta)
    ggsave(pngname, width = 30, height = 20, units = "cm")
    
    #Plot distributions of predicted and ground truth values
    g = ggplot() +
      geom_histogram(aes(x = data$sim, fill="b", colour="b"), alpha = 0.5) +
      geom_histogram(aes(x = data$rating, fill="g", colour="g") ,alpha = 0.5) +
      geom_smooth() +
      labs(y="Frequency",
           x="Similarity",
           title=sprintf("%s dataset", ds)) +
      theme(panel.grid.minor = element_blank(),panel.background = element_blank(),
            axis.line = element_line(colour = "black"))+
      theme(text = element_text(size = 35)) +
      scale_colour_manual(name="Similarity", values=c("g" = "gray", "b"=col, "r" = "red"), labels=c("b"="Predicted values", "g"="Ground truth rating", "r" = "Cosine sim")) +
      scale_fill_manual(name="Similarity", values=c("g" = "gray", "b"=col, "r" = "red"), labels=c("b"="Predicted values", "g"="Ground truth rating", "r" = "Cosine sim"))
    plot(g)
    pngname = sprintf("Plots/%s_%s_th%s_histogram_similarity_%s.png", ds, net, as.character(lower_th), beta)
    ggsave(pngname, width = 30, height = 20, units = "cm")
    
    }
    
    # c = c+1
    
    #Create output table for the creativity experiment
    if (ds == "Creativity") {
      cc_sim_crea_pearson = cor(1-data$sim, data$creativity_orig, method = "pearson")
      cc_sim_crea_spear = cor(1-data$sim, data$creativity_orig, method = "spearman")
      
      summary = t(as.data.frame(matrix(c(cc_sim_crea_pearson, cc_sim_crea_spear, cc_ff_crea_pearson, cc_ff_crea_spear))))
      colnames(summary) = c("Pearson Colex-based", "Spearman Colex-based", "Pearson FF", "Spearman FF")
    }
  }
  }
  }
  }
  
  #Get confidence intervals
  # print(ds)
  # print(quantile(allpearson, 0.025))
  # print(quantile(allpearson, 0.975))
  # 
  # print(quantile(allspearman, 0.025))
  # print(quantile(allspearman, 0.975))
  
  #Store final table of results
  suppressMessages(write.xlsx(results,sprintf("Tables/Word_similarity_summary_%s_%s.xlsx", net, ds) ,row.names = TRUE))
}

