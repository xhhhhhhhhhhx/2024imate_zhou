#Figure-A
library(ggcharts)
setwd(readClipboard())
getwd()

plot_data <- read_excel("F1-11.xlsx",1)
names(plot_data)
ggplot(plot_data,aes(Microbes,Factor))+
  geom_point(size=3,aes(color=Group))+
  geom_errorbar(aes(xmin = Microbes - Error_bar, xmax = Microbes +Error_bar,color=Group),width = 0.25,cex=0.7)+
  labs(y="Traits", x="Responsivity (%)")+
  geom_vline(aes(xintercept =0), size=0.6, linetype="dashed", colour="gray2")+
  geom_hline(aes(yintercept =8.5), size=0.4, linetype="dashed", colour="gray52")+
  geom_hline(aes(yintercept =11.5), size=0.4, linetype="dashed", colour="gray52")+
  scale_x_continuous(expand = c(0, 0), limit = c(-100, 100))+
  scale_color_manual(values=c("#71959F","#3FBDA7","#F1606C","#3E516F"))+ 
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=10))
ggsave("Figure-A.pdf", dpi=1000, height =150,width=150,units="mm")


#Figure-B
library(ggplot2)  
library(ggsignif)  
library(ggdist)
library(readxl)

setwd(readClipboard())
getwd()

data <- read_excel("F2.xlsx",1)
names(data)  

Vec1 <- c("aCO2", "eCO2")  
comb_list <- list(c("aCO2", "eCO2"))  
Custom.color <- c("#d3838a", "#4a9a5b")  

P1 <- ggplot(data, aes(x = group, y = AP, fill = group)) +  
  geom_jitter(mapping = aes(color = group), width = .05, alpha = 0.5, size = 2) + 
  geom_boxplot(position = position_nudge(x = 0.14), width = 0.1, outlier.size = 0, outlier.alpha = 0) +  
  stat_halfeye(mapping = aes(fill = group), width = 0.2, .width = 0, justification = -1.2, point_colour = NA, alpha = 0.6) +  
  scale_fill_manual(values = Custom.color) + 
  scale_color_manual(values = Custom.color) +  
  ylab("Available P") +   
  theme(  
    axis.ticks.x = element_line(size = 0, color = "white"), 
    panel.background = element_rect(fill = "white", color = "white"),  
    panel.grid.minor.y = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
    legend.position = "none",  
    axis.title.x = element_blank(),  
    axis.title.y = element_text(size = 13,color = "black"), 
    axis.text.x = element_text(size = 10, hjust = 0.2,color = "black"),  
    axis.text.y = element_text(size = 10,color = "black"),   
    plot.title = element_text(hjust = 0.5)  
  ) +  
  geom_signif(comparisons = comb_list, step_increase = .1, map_signif_level = TRUE, vjust = 0.5, hjust = 0)

P1

P2 <- ggplot(data, aes(x = group, y = CAPI, fill = group)) +  
  geom_jitter(mapping = aes(color = group), width = .05, alpha = 0.5, size = 2) + 
  geom_boxplot(position = position_nudge(x = 0.14), width = 0.1, outlier.size = 0, outlier.alpha = 0) +  
  stat_halfeye(mapping = aes(fill = group), width = 0.2, .width = 0, justification = -1.2, point_colour = NA, alpha = 0.6) +  
  scale_fill_manual(values = Custom.color) + 
  scale_color_manual(values = Custom.color) +  
  ylab("Ca-Pi") +   
  theme(  
    axis.ticks.x = element_line(size = 0, color = "white"), 
    panel.background = element_rect(fill = "white", color = "white"),  
    panel.grid.minor.y = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
    legend.position = "none",  
    axis.title.x = element_blank(),  
    axis.title.y = element_text(size = 13,color = "black"), 
    axis.text.x = element_text(size = 10, hjust = 0.2,color = "black"),  
    axis.text.y = element_text(size = 10,color = "black"),   
    plot.title = element_text(hjust = 0.5)  
  ) +  
  geom_signif(comparisons = comb_list, step_increase = .1, map_signif_level = TRUE, vjust = 0.5, hjust = 0)

P2


P3 <- ggplot(data, aes(x = group, y = NaOHPO, fill = group)) +  
  geom_jitter(mapping = aes(color = group), width = .05, alpha = 0.5, size = 2) + 
  geom_boxplot(position = position_nudge(x = 0.14), width = 0.1, outlier.size = 0, outlier.alpha = 0) +  
  stat_halfeye(mapping = aes(fill = group), width = 0.2, .width = 0, justification = -1.2, point_colour = NA, alpha = 0.6) +  
  scale_fill_manual(values = Custom.color) + 
  scale_color_manual(values = Custom.color) +  
  ylab("NaOH-Po") +   
  theme(  
    axis.ticks.x = element_line(size = 0, color = "white"), 
    panel.background = element_rect(fill = "white", color = "white"),  
    panel.grid.minor.y = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
    legend.position = "none",  
    axis.title.x = element_blank(),  
    axis.title.y = element_text(size = 13,color = "black"), 
    axis.text.x = element_text(size = 10, hjust = 0.2,color = "black"),  
    axis.text.y = element_text(size = 10,color = "black"),   
    plot.title = element_text(hjust = 0.5)  
  ) +  
  geom_signif(comparisons = comb_list, step_increase = .1, map_signif_level = TRUE, vjust = 0.5, hjust = 0)

P3


P4 <- ggplot(data, aes(x = group, y = MBP, fill = group)) +  
  geom_jitter(mapping = aes(color = group), width = .05, alpha = 0.5, size = 2) + 
  geom_boxplot(position = position_nudge(x = 0.14), width = 0.1, outlier.size = 0, outlier.alpha = 0) +  
  stat_halfeye(mapping = aes(fill = group), width = 0.2, .width = 0, justification = -1.2, point_colour = NA, alpha = 0.6) +  
  scale_fill_manual(values = Custom.color) + 
  scale_color_manual(values = Custom.color) +  
  ylab("MBP") +   
  theme(  
    axis.ticks.x = element_line(size = 0, color = "white"), 
    panel.background = element_rect(fill = "white", color = "white"),  
    panel.grid.minor.y = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
    legend.position = "none",  
    axis.title.x = element_blank(),  
    axis.title.y = element_text(size = 13,color = "black"), 
    axis.text.x = element_text(size = 10, hjust = 0.2,color = "black"),  
    axis.text.y = element_text(size = 10,color = "black"),   
    plot.title = element_text(hjust = 0.5)  
  ) +  
  geom_signif(comparisons = comb_list, step_increase = .1, map_signif_level = TRUE, vjust = 0.5, hjust = 0)

P4


library(gridExtra)
P5 <- grid.arrange(P1, P2, P3, P4, ncol = 2)
print(P5)
ggsave("Figure-B.pdf", dpi = 1000, height = 150, width = 225, units = "mm")



#Figure-C
plot_data <- read_excel("Figure-B.xlsx",1)

cor1 <- cor(plot_data)

rt <- as.matrix(plot_data)
df <- rcorr(rt)
df

write.csv(df$r, "correlation_matrix.csv")   
write.csv(df$P, "p_values_matrix.csv")   
cor1[cor1==1]=0  

pos_col <- "lightblue"  
neg_col <- "lightpink" 
col1 <- matrix(ifelse(cor1 >= 0, pos_col, neg_col), nrow = nrow(cor1), ncol = ncol(cor1))  
par(mar=c(2,2,2,4))  
circos.par(gap.degree=rep(2, ncol(cor1)),  
           start.degree = 180,  
           track.margin = c(0.02, 0.02))    
chordDiagram(cor1, grid.col=NULL, col=col1, transparency = 0.5, symmetric = TRUE)





