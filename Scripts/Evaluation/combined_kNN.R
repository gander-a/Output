#Combined both weighted kNN plots into one

#Setup directories
mainpath = "C:/Users/Armin/Desktop/Output/"
setwd(mainpath)

#Load colexification-based method results
load("Plots/brown_knnweighted.Rdata")
max_brown = maxpoint
res_brown = res
res_brown$col = "Colexification"

#Load baseline method results
load("Plots/baseline_knnweighted.Rdata")
max_base = maxpoint
res_base = res
res_base$col = "Baseline"


#Plot
#kNN development plot
g = ggplot() +
  #Uncertainty bands
  geom_ribbon(aes(fill = "Colexification", x = 1:nrow(res_brown), ymin = res_brown$average-2*res_brown$sd, ymax = res_brown$average+2*res_brown$sd), alpha = 0.5) +
  geom_ribbon(aes(fill = "Baseline", x = 1:nrow(res_base), ymin = res_base$average-2*res_base$sd, ymax = res_base$average+2*res_base$sd) ,alpha = 0.35) +
  #Lines
  geom_line(aes(x = 1:nrow(res_brown), y=res_brown$average), size = 3, color = "mediumpurple4") +
  geom_line(aes(x = 1:nrow(res_base), y=res_base$average), size = 3, color = "orange") +
  #Label Colexification based
  geom_point(aes(x = which(max_brown==res_brown$average), y=max_brown), size = 10, color = "mediumpurple4") +
  geom_text(aes(x = which(max_brown==res_brown$average)[1]+round(nrow(res_brown)*0.1), y=max_brown+0.07, label = as.character(round(max_brown, digits = 4))), size = 15, color = "mediumpurple4") +
  #Label Baseline
  geom_point(aes(x = which(max_base==res_base$average), y=max_base), size = 10, color = "orange") +
  geom_text(aes(x = which(max_base==res_base$average)[1]+round(nrow(res_base)*0.1), y=max_base+0.07, label = as.character(round(max_base, digits = 4))), size = 15, color = "orange") +
  
  labs(y="Accuracy",
       x="k nearest neighbors",
       title=sprintf("Average Accuracy"))+
  theme(panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=90),
        axis.text=element_text(size=40), text = element_text(size = 40))+
  scale_x_continuous(breaks = c(0,25,50,75,100,125,150,175,200,225))+
  scale_y_continuous(breaks = c(0.20,0.30,0.40,0.50,0.60,0.70), limits = c(0.45,0.70))+
  
  scale_fill_manual(breaks = c("Colexification", "Baseline"), values = c("mediumpurple4", "orange"))+
  labs(fill='Method') 

#Plot and store
plot(g)
pngname = sprintf("%sPlots/kNNa_weighted_combined.png", mainpath)
ggsave(pngname, width = 30, height = 20, units = "cm")

