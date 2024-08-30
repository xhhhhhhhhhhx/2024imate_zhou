#####Figure 4A-B
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


#####Figure4C-E
library(ggplot2)
library(ggpmisc)
library(readxl)
library(dplyr)
library(ggpubr)

fit<- read_excel("regression.xlsx",1)

head(fit)
names(fit)

##########Figure6C##########
p1<-ggplot(fit, aes(x=TOAA, y=Networkcomplexity),group=Group)+
  
  theme_bw()+theme(legend.position="top")+theme_bw() + theme(panel.grid =element_blank())+
  geom_point(aes(color=Group),size=4.355)+
  geom_smooth(method = 'lm',color="black",formula= y ~ x,se= T)+
  
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = '*\", \"*')), 
               formula = y ~ x, parse = T, label.y = "bottom", label.x = "right")+
  theme(axis.title= element_text(size=14))+
  ylab("Networkcom plexity")+xlab("Total organic acid anions (μmol g-1 RDW)")+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))
p1



##########Figure6D##########
p2<-ggplot(fit, aes(x=AP, y=Networkcomplexity),group=Group)+
  
  theme_bw()+theme(legend.position="top")+theme_bw() + theme(panel.grid =element_blank())+
  geom_point(aes(color=Group),size=4.355)+
  geom_smooth(method = 'lm',color="black",formula= y ~ x,se= T)+
  
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = '*\", \"*')), 
               formula = y ~ x, parse = T, label.y = "bottom", label.x = "right")+
  theme(axis.title= element_text(size=14))+
  ylab("Network complexity")+xlab("Total P accumulation(mg kg-1)")+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))
p2


##########Figure6E##########
p3<-ggplot(fit, aes(x=NaOH-Po, y=Networkcomplexity),group=Group)+
  
  theme_bw()+theme(legend.position="top")+theme_bw() + theme(panel.grid =element_blank())+
  geom_point(aes(color=Group),size=4.355)+
  geom_smooth(method = 'lm',color="black",formula= y ~ x,se= T)+
  
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = '*\", \"*')), 
               formula = y ~ x, parse = T, label.y = "bottom", label.x = "right")+
  theme(axis.title= element_text(size=14))+
  ylab("Network complexity")+xlab("NaOH-Po (mg kg-1)")+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))
p3



####Figure6F#######
setwd(readClipboard())
getwd()
plant_growth <- read.delim('plant growth.txt', row.names = 1)
otu <- read.delim('otu.txt', row.names = 1)
library(randomForest)
set.seed(123)
otu_forest <- randomForest(plant_growth~., data = otu, importance = TRUE, ntree = 500, nPerm = 1000)
otu_forest

importance_otu.scale <- data.frame(importance(otu_forest, scale = TRUE), check.names = FALSE)
importance_otu.scale

importance_otu.scale <- importance_otu.scale[order(importance_otu.scale$'%IncMSE', decreasing = TRUE), ]
importance_otu.scale

library(ggplot2)
importance_otu.scale$OTU_name <- rownames(importance_otu.scale)
importance_otu.scale$OTU_name <- factor(importance_otu.scale$OTU_name, levels = importance_otu.scale$OTU_name)

p <- ggplot(importance_otu.scale, aes(OTU_name, `%IncMSE`)) +
  geom_col(width = 0.5, fill = '#FFC068', color = NA) +
  labs(title = NULL, x = NULL, y = 'Increase in MSE (%)', fill = NULL) +
  theme(panel.grid = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = 'black')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(expand = c(0, 0), limit = c(0, 16))
p

p <- p +
  annotate('text', label = 'Total plant P accumulation', x = 9, y = 15, size = 4) +
  annotate('text', label = sprintf('italic(R^2) == %.2f', 45.6), x = 9, y = 13, size = 3, parse = TRUE)
p
