#Figure2-A
library(ggplot2)  
fit<- read_excel("Figure 2-1.xlsx",1)
names(fit)
p1 <- ggplot(fit, aes(x=Treatment, y=value, color=Treatment)) +  
  geom_violin(trim=FALSE, width=0.8, fill=NA) + 
  stat_summary(aes(group=Treatment), fun.y=mean, geom="point", shape=15, size=2, color="gray") +  
  geom_jitter(width=0.2, height=0, alpha=0.5,size=1) +  
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +  
  xlab("") +   
  ylab("AMF (nmol NLFA g-1 soil)") +  
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
ggsave("F2-A.pdf", dpi=1000, height =45,width=45,units="mm")

#Figure2-E
p1 <- ggplot(fit, aes(x=Treatment, y=ALPB, color=Treatment)) +  
  geom_violin(trim=FALSE, width=0.8, fill=NA) + 
  stat_summary(aes(group=Treatment), fun.y=mean, geom="point", shape=15, size=2, color="gray") +  
  geom_jitter(width=0.2, height=0, alpha=0.5,size=1) +  
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +  
  xlab("") +   
  ylab("ALP-producing bacteria (*105 copies g-1 soil)") +  
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
ggsave("F2-E.pdf", dpi=1000, height =45,width=45,units="mm")


#Figure-B
library(readxl)
library(ggplot2)
library(grid)
library(ggpubr)
library(gridExtra)
library(tidyr)
library(RColorBrewer)
df <- read_excel("Figure2-2.xlsx",1)
names(df)

df$Families<- factor(df$Families,levels=c("Ambisporaceae","Glomeraceae", "Paraglomeraceae","Archaeosporaceae"
),

labels = c("Ambisporaceae","Glomeraceae", "Paraglomeraceae","Archaeosporaceae"))


levels(df$Families)

p1<- ggplot(data=df, aes(x=AMF_Treatment, y=AMF,fill=Families))+
  geom_bar(stat="identity")+theme_bw()+
  scale_y_continuous(expand = c(0,0))+theme(legend.position="right")+
  scale_fill_manual(values =  rev(c("#E1177E","#1CA3E1","#00986E" ,"#7A6AAD"
  )))+
  theme(panel.grid =element_blank())+
  labs(x='',y='Relative abundance (%)')+
  theme(legend.title = element_text(size=12))+# legend labels
  theme(legend.text = element_text(size=9))+
  theme(legend.key.size=unit(0.9,'cm'))+
  theme(strip.text.x = element_text(size =15),
        strip.background.x = element_rect(fill = "white", colour = "black"))+
  theme(axis.text.y=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.text.x=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.title.x = element_text(size=14,color = "black"),
        axis.title.y = element_text(size=14,color = "black"))#angle =90,+

p1
ggsave("F2-B.pdf", dpi=1000, height =75,width=100,units="mm")

df <- read_excel("Figure2-3.xlsx",1)
names(df)

df$Families<- factor(df$Families,levels=c("Others","Unclassified", "Phyllobacteriaceae","Pseudomonadaceae"
                                          ,"Xanthomonadaceae","Rhizobiaceae","Bradyrhizobiaceae"
),

labels = c("Others","Unclassified", "Phyllobacteriaceae","Pseudomonadaceae"
           ,"Xanthomonadaceae" ,"Rhizobiaceae","Bradyrhizobiaceae"))


levels(df$Families)

p1<- ggplot(data=df, aes(x=PhoD_Treatment, y=PhoD,fill=Families))+
  geom_bar(stat="identity")+theme_bw()+
  scale_y_continuous(expand = c(0,0))+theme(legend.position="right")+
  
  scale_fill_manual(values =  rev(c("#E1177E","#1CA3E1","#00986E" ,"#7A6AAD",
                                    "#D56128",
                                    "#9F9F98","#000000"
  )))+
  theme(panel.grid =element_blank())+
  labs(x='',y='Relative abundance (%)')+
  theme(legend.title = element_text(size=12))+# legend labels
  theme(legend.text = element_text(size=9))+
  theme(legend.key.size=unit(0.9,'cm'))+
  theme(strip.text.x = element_text(size =15),
        strip.background.x = element_rect(fill = "white", colour = "black"))+
  theme(axis.text.y=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.text.x=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.title.x = element_text(size=14,color = "black"),
        axis.title.y = element_text(size=14,color = "black"))#angle =90,+

p1
ggsave("F2-F.pdf", dpi=1000, height =75,width=100,units="mm")


#Figure-C
library(ggplot2)  
library(ggsignif)  
library(ggdist)
library(readxl)

data <- read_excel("Figure2-4.xlsx",1)
names(data)  
Vec1 <- c("aCO2", "eCO2")  
comb_list <- list(c("aCO2", "eCO2"))  
Custom.color <- c("#d3838a", "#4a9a5b")  

P1 <- ggplot(data, aes(x = group, y = AMF, fill = group)) +  
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
ggsave("F2-C.pdf", dpi=1000, height =75,width=75,units="mm")


P2 <- ggplot(data, aes(x = group, y = ALPB, fill = group)) +  
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
ggsave("F2-G.pdf", dpi=1000, height =75,width=75,units="mm")


#Figure-I
setwd(readClipboard())
getwd()
library(vegan)
library(devtools)
library(linkET)
library(dplyr)
library(ggplot2)

s <-  read.csv("traits.csv",row.names = 1, check.names = FALSE)
env<-  read.csv("otu.csv",row.names = 1, check.names = FALSE)
varespec<-as.data.frame(t(s))
varechem<-as.data.frame(env)

mantel <- mantel_test(varespec, varechem ,
                      spec_select = list(
                        AMF = 398:408,
                        phoD= 1:397,
                                        )) %>%  
  mutate(rd = cut(r, breaks = c(-Inf, 0.1, 0.5, Inf),
                  labels = c("< 0.1", "0.1 - 0.5", ">= 0.5")), 
         pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05"))) 

qcorrplot(correlate(varechem), type = "lower", diag = FALSE) +
  geom_square() +
  geom_couple(aes(xend = .xend+ 1.25,
                  yend = .yend +0.5, 
                  colour = pd, 
                  size = rd), 
              data = mantel, curvature = 0.1) +
  geom_diag_label(mapping = aes(y = .y + 0.05),
                  hjust = 0.15) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  scale_size_manual(values = c(0.5, 1, 2)) +
  scale_colour_manual(values = color_pal(3)) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3)) +
  theme(axis.text.y = element_blank())
ggsave(paste("F2-I.pdf", sep=""), mantel, width = 18, height= 10)


#Figure-J
setwd(readClipboard())
getwd()
library(ComplexHeatmap)
library("ggtree")
library(circlize)
library(ComplexHeatmap)
mat<-read.csv('correlation_results.csv', header = T,row.names = 1)
mat=as.matrix(mat)
zz <- hclust(dist(t(mat)))
mat <- mat[,zz$order]   
col_fun1 = colorRamp2(c(-1, 0, 1), c("#4c93c9","white","#fe2016"))

circos.par(gap.after = c(40))
circos.heatmap(mat, col = col_fun1,cluster = F,rownames.cex = 0.8,rownames.side = "outside",track.height = 0.4)
circos.track(track.index = get.current.track.index(), panel.fun = function(x, y) {
  if(CELL_META$sector.numeric.index == 1) { 
    cn = colnames(mat)
    n = length(cn)
    circos.text(rep(CELL_META$cell.xlim[2], n) + convert_x(0.1, "mm"), 
                (1:n)*0.99,
                cn, 
                cex = 0.6, adj = c(0, 1), facing = "inside")
  }
}, bg.border = NA)
lgd = Legend(title = "mat1", col_fun = col_fun1)
grid.draw(lgd)

ggsave("F2-J.pdf", dpi = 1000, height = 75, width = 75, units = "mm")


#####Co-occurrence of AMF and ALPB
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




