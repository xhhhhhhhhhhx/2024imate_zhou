#Figure-3A
library(ggplot2)  
fit<- read_excel("Figure3-A.xlsx",1)
names(fit)

p1 <- ggplot(fit, aes(x=Treatment, y=Protozoa, color=Treatment)) +  
  geom_violin(trim=FALSE, width=0.8, fill=NA) + 
  stat_summary(aes(group=Treatment), fun.y=mean, geom="point", shape=15, size=2, color="gray") +  
  geom_jitter(width=0.2, height=0, alpha=0.5,size=1) +  
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +  
  xlab("") +   
  ylab("Abundance (*104 g-1 soil)") +  
  theme_bw() +   
  theme(panel.grid = element_blank(),  
        legend.position="none",  
        strip.text = element_text(size = rel(0.90)),  
        axis.title.x = element_blank(),  
        axis.title.y = element_text(size=10),  
        axis.text.x = element_text(vjust=0.5, hjust=0.5, size=8, color = "black"),  
        axis.text.y = element_text(vjust=0.5, hjust=0.5, size=8, color = "black"),  
        axis.line.x.bottom = element_line(color = 'black'),  
        axis.line.y.left = element_line(color = 'black'),  
        axis.line.y.right = element_line(color = 'black'),  
        axis.text.y.right = element_blank(),  
        axis.ticks.y.right = element_blank(),  
        panel.border = element_blank())  

p1
ggsave("F3-A1.pdf", dpi=1000, height =45,width=45,units="mm")


p2 <- ggplot(fit, aes(x=Treatment, y=Bacterivore, color=Treatment)) +  
  geom_violin(trim=FALSE, width=0.8, fill=NA) + 
  stat_summary(aes(group=Treatment), fun.y=mean, geom="point", shape=15, size=2, color="gray") +  
  geom_jitter(width=0.2, height=0, alpha=0.5,size=1) +  
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +  
  xlab("") +   
  ylab("Abundance (*104 g-1 soil)") +  
  theme_bw() +   
  theme(panel.grid = element_blank(),  
        legend.position="none",  
        strip.text = element_text(size = rel(0.90)),  
        axis.title.x = element_blank(),  
        axis.title.y = element_text(size=10),  
        axis.text.x = element_text(vjust=0.5, hjust=0.5, size=8, color = "black"),  
        axis.text.y = element_text(vjust=0.5, hjust=0.5, size=8, color = "black"),  
        axis.line.x.bottom = element_line(color = 'black'),  
        axis.line.y.left = element_line(color = 'black'),  
        axis.line.y.right = element_line(color = 'black'),  
        axis.text.y.right = element_blank(),  
        axis.ticks.y.right = element_blank(),  
        panel.border = element_blank())  

p2
ggsave("F3-A2.pdf", dpi=1000, height =45,width=45,units="mm")



p2 <- ggplot(fit, aes(x=Treatment, y=Fungivore, color=Treatment)) +  
  geom_violin(trim=FALSE, width=0.8, fill=NA) + 
  stat_summary(aes(group=Treatment), fun.y=mean, geom="point", shape=15, size=2, color="gray") +  
  geom_jitter(width=0.2, height=0, alpha=0.5,size=1) +  
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +  
  xlab("") +   
  ylab("Abundance (*104 g-1 soil)") +  
  theme_bw() +   
  theme(panel.grid = element_blank(),  
        legend.position="none",  
        strip.text = element_text(size = rel(0.90)),  
        axis.title.x = element_blank(),  
        axis.title.y = element_text(size=10),  
        axis.text.x = element_text(vjust=0.5, hjust=0.5, size=8, color = "black"),  
        axis.text.y = element_text(vjust=0.5, hjust=0.5, size=8, color = "black"),  
        axis.line.x.bottom = element_line(color = 'black'),  
        axis.line.y.left = element_line(color = 'black'),  
        axis.line.y.right = element_line(color = 'black'),  
        axis.text.y.right = element_blank(),  
        axis.ticks.y.right = element_blank(),  
        panel.border = element_blank())  

p2
ggsave("F3-A3.pdf", dpi=1000, height =45,width=45,units="mm")


#Figure-3B
library(readxl)
library(ggplot2)
library(grid)
library(ggpubr)
library(gridExtra)
library(tidyr)
library(RColorBrewer)

df <- read_excel("Figure3-B1.xlsx",1)
names(df)

df$Phyla<- factor(df$Phyla,levels=c("Others","Unclassified", "Apicomplexa","Ciliophora"
                                    ,"Lobosa","Conosa","Cercozoa"
),

labels = c("Others","Unclassified", "Apicomplexa","Ciliophora"
           ,"Lobosa","Conosa","Cercozoa"))


levels(df$Phyla)

p1<- ggplot(data=df, aes(x=Pro_Treatment, y=Pro,fill=Phyla))+
  geom_bar(stat="identity")+theme_bw()+
  scale_y_continuous(expand = c(0,0))+theme(legend.position="right")+
  
  scale_fill_manual(values =  rev(c("#E1177E","#1CA3E1","#00986E" ,"#7A6AAD",
                                    "#D56128",
                                    "#9F9F98","#000000"
  )))+
  theme(panel.grid =element_blank())+
  labs(x='',y='Relative abundance (%)')+
  theme(legend.title = element_text(size=12))+
  theme(legend.text = element_text(size=9))+
  theme(legend.key.size=unit(0.9,'cm'))+
  theme(strip.text.x = element_text(size =15),
        strip.background.x = element_rect(fill = "white", colour = "black"))+
  theme(axis.text.y=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.text.x=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.title.x = element_text(size=14,color = "black"),
        axis.title.y = element_text(size=14,color = "black"))

p1
ggsave("Figure3-B1.pdf", dpi=1000, height =75,width=100,units="mm")


####
df <- read_excel("Figure3-B2.xlsx",1)
names(df)

df$Genera<- factor(df$Genera,levels=c("Acrobeles","Wilsonema", "Cruznema","Rhabdolaimus"
                                      ,"Plectus","Alaimus","Cervidellus","Rhabditis","Cephalobus"
                                      ,"Acrobeloides","Mesorhabditis"
),

labels = c("Acrobeles","Wilsonema", "Cruznema","Rhabdolaimus"
           ,"Plectus","Alaimus","Cervidellus","Rhabditis","Cephalobus"
           ,"Acrobeloides","Mesorhabditis"))


levels(df$Genera)

p1<- ggplot(data=df, aes(x=BN_Treatment, y=BN,fill=Genera))+
  geom_bar(stat="identity")+theme_bw()+
  scale_y_continuous(expand = c(0,0))+theme(legend.position="right")+
  
  scale_fill_manual(values =  rev(c("#E1177E","#1CA3E1","#00986E" ,"#7A6AAD","#D56128","5ED89F",
                                    "#EEB606","#B340A9","#3CABB6","#31C047","#B0A640"
                                    
                                    
  )))+
  theme(panel.grid =element_blank())+
  labs(x='',y='Relative abundance (%)')+
  theme(legend.title = element_text(size=12))+
  theme(legend.text = element_text(size=9))+
  theme(legend.key.size=unit(0.9,'cm'))+
  theme(strip.text.x = element_text(size =15),
        strip.background.x = element_rect(fill = "white", colour = "black"))+
  theme(axis.text.y=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.text.x=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.title.x = element_text(size=14,color = "black"),
        axis.title.y = element_text(size=14,color = "black"))

p1
ggsave("Figure3-B2.pdf", dpi=1000, height =75,width=100,units="mm")



####
df <- read_excel("Figure3-B3.xlsx",1)
names(df)

df$Genera<- factor(df$Genera,levels=c("Diphtherophora","Tylencholaimus", "Ditylenchus","Paraphelenchus"
                                      ,"Aphelenchoides","Aphelenchus"
),

labels = c("Diphtherophora","Tylencholaimus", "Ditylenchus","Paraphelenchus"
           ,"Aphelenchoides","Aphelenchus"))


levels(df$Genera)

p1<- ggplot(data=df, aes(x=FN_Treatment, y=FN,fill=Genera))+
  geom_bar(stat="identity")+theme_bw()+
  scale_y_continuous(expand = c(0,0))+theme(legend.position="right")+
  
  scale_fill_manual(values =  rev(c("#E1177E","#1CA3E1","#00986E" ,"#7A6AAD","#D56128","5ED89F"
                                    
  )))+
  theme(panel.grid =element_blank())+
  labs(x='',y='Relative abundance (%)')+
  theme(legend.title = element_text(size=12))+
  theme(legend.text = element_text(size=9))+
  theme(legend.key.size=unit(0.9,'cm'))+
  theme(strip.text.x = element_text(size =15),
        strip.background.x = element_rect(fill = "white", colour = "black"))+
  theme(axis.text.y=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.text.x=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.title.x = element_text(size=14,color = "black"),
        axis.title.y = element_text(size=14,color = "black"))

p1
ggsave("Figure3-B3.pdf", dpi=1000, height =75,width=100,units="mm")  



###Figure 3C
library(ggplot2)  
library(ggsignif)  
library(ggdist)
library(readxl)

data <- read_excel("Figure3C.xlsx",1)
names(data)  
Vec1 <- c("aCO2", "eCO2")  
comb_list <- list(c("aCO2", "eCO2"))  
Custom.color <- c("#d3838a", "#4a9a5b")  

P1 <- ggplot(data, aes(x = group, y = Protozoa, fill = group)) +  
  geom_jitter(mapping = aes(color = group), width = .05, alpha = 0.5, size = 2) +  
  geom_boxplot(position = position_nudge(x = 0.14), width = 0.1, outlier.size = 0, outlier.alpha = 0) +  
  stat_halfeye(mapping = aes(fill = group), width = 0.2, .width = 0, justification = -1.2, point_colour = NA, alpha = 0.6) + 
  scale_fill_manual(values = Custom.color) +  
  scale_color_manual(values = Custom.color) + 
  ylab("Shannon index") +   
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


P2 <- ggplot(data, aes(x = group, y = Bacterivore, fill = group)) +  
  geom_jitter(mapping = aes(color = group), width = .05, alpha = 0.5, size = 2) +  
  geom_boxplot(position = position_nudge(x = 0.14), width = 0.1, outlier.size = 0, outlier.alpha = 0) +  
  stat_halfeye(mapping = aes(fill = group), width = 0.2, .width = 0, justification = -1.2, point_colour = NA, alpha = 0.6) + 
  scale_fill_manual(values = Custom.color) +  
  scale_color_manual(values = Custom.color) + 
  ylab("Shannon index") +   
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



P3 <- ggplot(data, aes(x = group, y = Fungivore, fill = group)) +  
  geom_jitter(mapping = aes(color = group), width = .05, alpha = 0.5, size = 2) +  
  geom_boxplot(position = position_nudge(x = 0.14), width = 0.1, outlier.size = 0, outlier.alpha = 0) +  
  stat_halfeye(mapping = aes(fill = group), width = 0.2, .width = 0, justification = -1.2, point_colour = NA, alpha = 0.6) + 
  scale_fill_manual(values = Custom.color) +  
  scale_color_manual(values = Custom.color) + 
  ylab("Shannon index") +   
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

library(cowplot)  
P <- P_combined <- plot_grid(P1, P2,P3, ncol = 3, align = "v", labels = c("A", "B", "C"))
ggsave("Figure3-C.pdf", dpi = 1000, height = 75, width = 225, units = "mm")


#####Co-occurrence of protozoa, and bacterivorous and fungivorous nematodes
setwd(readClipboard())
getwd()

library(igraph)
library(Hmisc)

bacteria<-read.table("otu.txt", header = T, check.names = F)
dim(bacteria)
head(bacteria)
bacteria <- bacteria[which(rowSums(bacteria) >= 0.001), ]
dim(bacteria)
bacteria1 <- bacteria
bacteria1[bacteria1>0] <- 1
bacteria<- bacteria[which(rowSums(bacteria1) >= 1), ]
dim(bacteria)
write.csv(bacteria,"selectbacteria.csv")

bacteria <- t(bacteria)
bac_corr <- rcorr(bacteria, type = 'spearman')
r <- bac_corr$r
r[abs(r) < 0.6] <- 0

p <- bac_corr$P
p <- p.adjust(p, method = 'BH') 
p[p>=0.05] <- -1
p[p<0.05 & p>=0] <- 1
p[p==-1] <- 0

z <- r * p
diag(z) <- 0
head(z)[1:6,1:6]

write.table(data.frame(z, check.names = FALSE), 'bac_corr.matrix.txt',
            col.names = NA, sep = '\t', quote = FALSE)



library(igraph)
igraph <- graph.adjacency(z, weighted = TRUE, mode = 'undirected')
igraph
vcount(igraph)
igraph <- delete.vertices(igraph, names(degree(igraph)[degree(igraph) == 0]))
vcount(igraph)
E(igraph)$correlation <- E(igraph)$weight
E(igraph)$weight <- abs(E(igraph)$weight)
plot(igraph)
write.graph(igraph, 'network.gml', format = 'gml')

adj_matrix <- as.matrix(get.adjacency(igraph, attr = 'correlation'))
write.table(data.frame(adj_matrix, check.names = FALSE), 'bac_network.adj_matrix.txt', col.names = NA, sep = '\t', quote = FALSE)

tax <- read.delim('taxonomy.txt', 
                  check.names = FALSE, stringsAsFactors = FALSE)

row.names(tax)<-make.names(tax[,1],TRUE)
tax<-tax[,-1]

dim(tax)
tax <- tax[as.character(V(igraph)$name), ]
write.csv(tax,"tax.csv")

V(igraph)$phylum <- tax$phylum
V(igraph)$class <- tax$class
V(igraph)$order <- tax$order
V(igraph)$family <- tax$family
V(igraph)$genus <- tax$genus
V(igraph)$specie<- tax$specie
V(igraph)$abundance <- tax$Abundance
#V(igraph)$type <- tax$Type

igraph
plot(igraph)

edge <- data.frame(as_edgelist(igraph))
edge_list <- data.frame(
  source = edge[[1]],
  target = edge[[2]],
  weight = E(igraph)$weight,
  correlation = E(igraph)$correlation
)

head(edge_list)
write.table(edge_list, 'network.edge_list.txt', sep = '\t', row.names = FALSE, quote = FALSE)



node_list <- data.frame(
  label = names(V(igraph)),
  phylum = V(igraph)$phylum,
  class = V(igraph)$class,
  order = V(igraph)$order,
  family = V(igraph)$family,
  genus = V(igraph)$genus,
  specie = V(igraph)$specie,
  abundance = V(igraph)$abundance,
  #type=V(igraph)$type
)


head(node_list)
write.table(node_list, 'network.node_list.txt', sep = '\t', row.names = FALSE, quote = FALSE)

#graphml format, which can be opened and visually edited using gephi software
write.graph(igraph, 'gephi_network.graphml', format = 'graphml')















