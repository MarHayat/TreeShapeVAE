library(magick)
library(dplyr)
library(reshape2)
library("wesanderson")
library("RColorBrewer")
library("ggplot2")
library(dplyr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(ggpubr)
library(RColorBrewer)
library(countrycode)

#figure 4:  the  distribution  of  the  species  across  the  clusters.  
names=c("Acinetobacter", "Campylobacter","Citrobacter_freundii", "Clostridioides_difficile", "Cronobacter", "Elizabethkingia", "Enterobacter", "Escherichia_coli_Shigella", 
        "Klebsiella", "Klebsiella_oxytoca", "Kluyvera_intermedia", "Legionella_pneumophila", "Listeria", 
        "Morganella", "Mycobacterium_tuberculosis", "Neisseria", "Providencia", "Pseudomonas_aeruginosa", 
        "Salmonella", "Serratia","Staphylococcus_pseudintermedius", "Vibrio_parahaemolyticus", "Vibrio_cholerae", 
        "Vibrio_vulnificus", "Clostridium_botulinum", "Clostridium_perfringens", "Corynebacterium_striatum", 
        "Photobacterium_damselae")

#tab_species is a table that each column shows the distribution of each species across the clusters!
tab_species = matrix(0,9,28)
colnames(tab_species) = names
for (i in c(0:8)) {
  allFiles = list.files(path = paste0("~/cluster",i), 
                        pattern = ".png")
  bacteria_name=sapply(allFiles, function(x) gsub( "\\d.*$", "", x ))
  #length(unique(bacteria_name))
  tab = table(bacteria_name)
  ind = match(names(tab),colnames(tab_species))
  tab_species[i+1,ind] = tab
}
tab_species = as.data.frame(tab_species)


cl = c("cluster 1","cluster 2","cluster 3","cluster 4","cluster 5","cluster 6","cluster 7",
       "cluster 8","cluster 9")
clust = as.vector(sapply(cl,function(x) rep(x,27)))

# the species "Kluyvera_intermedia" is not in any cluster!
ind = which(names ==  "Kluyvera_intermedia")
Species = rep(names[-ind],9)
Freq = as.matrix(tab_species)
Freq = Freq[,-which(colnames(Freq) ==  "Kluyvera_intermedia")]
Freq = as.vector(t(Freq))

data = data.frame(clust = clust,Species = Species, Freq = Freq)
data$clust = as.factor(data$clust)
data$Species = as.factor(data$Species)


ggplot(data, aes(x=Species, y=Freq, fill=clust)) +
  ggtitle("The distribution of the species across the clusters") +
  geom_bar(position="fill", stat="identity",width=0.6)+
  scale_fill_manual(values = c("cluster 1" = "gold2","cluster 2" = "#E69F00", 
                               "cluster 3" = "salmon3","cluster 4" = "#CC79A7", 
                               "cluster 5" = "steelblue3", "cluster 6" = "royalblue3",
                               "cluster 7" = "indianred3","cluster 8" = "yellowgreen",
                               "cluster 9" = "olivedrab"))+
  theme(axis.text.x = element_text(color="black", size=12, angle=90),
        axis.text.y = element_text(color="black", size=14),
        plot.title = element_text(color = "black", size = 20, face = "bold",hjust = 0.5,lineheight = 0.9))+
  ylab("")+xlab("")+ theme(legend.title = element_blank())

#*******************************************************************************
#figure 5: the composition of each cluster

type = as.vector(sapply(names,function(x) rep(x,10)))
clust = rep(c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5","Cluster 6","Cluster 7","Cluster 8","Cluster 9","Whole data"),28)

all_tab = as.matrix(rbind(tab_species,colSums(tab_species)))
Freq = as.vector(all_tab)
  
data = data.frame(type = type,clust = clust, Freq = Freq)
data$clust = as.factor(data$clust)
data$type = as.factor(data$type)

data = data[-which(data[,1]=="Kluyvera_intermedia"),]
ggplot(data, aes(x=clust, y=Freq, fill=type)) + 
  geom_bar(position="fill", stat="identity",width=0.6)+
  scale_fill_manual(values = c("Acinetobacter" = "gold2","Campylobacter" = "#E69F00", 
                               "Citrobacter_freundii" = "grey55", "Clostridioides_difficile" = 
                                 "lavenderblush3", "Cronobacter" = "lemonchiffon3", 
                               "Elizabethkingia"  = "peachpuff3", "Enterobacter" = 
                                 "salmon3","Escherichia_coli_Shigella" =  "#CC79A7","Klebsiella" 
                                 = "steelblue3","Klebsiella_oxytoca" = "steelblue4",
                               "Legionella_pneumophila" = "royalblue","Listeria"= "royalblue3",
                               "Morganella"= "orchid4", "Mycobacterium_tuberculosis"="indianred3",
                               "Neisseria" ="yellowgreen","Providencia" = "springgreen3", 
                               "Pseudomonas_aeruginosa" = "springgreen4", "Salmonella" = "olivedrab",
                               "Serratia"= "darkgreen","Staphylococcus_pseudintermedius"= "antiquewhite3",
                               "Vibrio_parahaemolyticus"= "antiquewhite4","Vibrio_cholerae" = "firebrick1",
                               "Vibrio_vulnificus" = "firebrick3", "Clostridium_botulinum" = "plum2",
                               "Clostridium_perfringens" = "plum4",  "Corynebacterium_striatum"  = "pink2", 
                               "Photobacterium_damselae" ="pink4"))+
  theme(axis.text.x = element_text(color="black", size=12, angle=90),
        plot.title = element_text(color = "black", size = 20, face = "bold",hjust = 0.5,lineheight = 0.9))+ylab("")+xlab("")

#*******************************************************************************
#figure 6: the  geographical  distributions  of  the  clusters 
df = read.csv("~/all_loc_region.csv")
df = df[,1:ncol(df)]
i=1
df$cluster = ifelse(df$cluster==paste0("cluster",i),paste0("cluster",i),"All datasets")

p <- ggplot(df, aes(x=Location,y = Freq,fill= cluster)) + theme_light()+
  geom_boxplot(width=0.50, color="black", alpha=0.7,size = 0.2, position=position_dodge(0.9),outlier.size=0.2)+
  theme(axis.text.x = element_text(color="black", size=12, angle=90) ,axis.text.y = element_text(color="black", size=15, angle=90))+
  theme(axis.title.x = element_text(size = 5),axis.title.y = element_text(size = 5))+ guides(fill=guide_legend(ncol=2))+ 
  ylab("")+xlab("")+ theme(legend.title = element_blank(),legend.position = "top",legend.text = element_text(size = 14),legend.key.size = unit(1, 'cm'))
p
ggsave(paste0("~/cls",i,"_region.pdf"),width =5, height =5)

#*******************************************************************************
#figure 7: the distribution of the species dates at each cluster.
df = read.csv("~/all_date.csv")
df = df[,2:ncol(df)]
i=1

df$cluster = ifelse(df$cluster==paste0("cluster",i),paste0("cluster",i),"All datasets")
df$tree = as.factor(df$tree)
df$Date = as.factor(df$Date)
df$cluster = as.factor(df$cluster)

p <- ggplot(df, aes(x=Date,y = Freq,fill= cluster)) + theme_light()+
  geom_boxplot(width=0.50, color="black", alpha=0.7,size = 0.2, position=position_dodge(0.9),outlier.size=0.2)+
  theme(axis.text.x = element_text(color="black", size=15, angle=90) ,axis.text.y = element_text(color="black", size=15, angle=90))+
  theme(axis.title.x = element_text(size = 5),axis.title.y = element_text(size = 5))+ guides(fill=guide_legend(ncol=2))+ 
  ylab("")+xlab("")+ theme(legend.title = element_blank(),legend.position = "top",legend.text = element_text(size = 14),legend.key.size = unit(1, 'cm'))
p
ggsave(paste0("~/cls",i,"_date.pdf"),width =5, height =5) 

#*******************************************************************************
#figure 8:  tree shape statistics
data = read.csv("~/whole_stats.csv")
data = data[,2:ncol(data)]

data$stats = as.factor(data$stats)
data$clusters = as.factor(data$clusters)

Name = c("sackin","colless","Variance","I2","B1","B2","avgLadder","ILnumber","pitchforks",
         "maxHeight","MaxWidth","DelW","Stairs1","Stairs2","Cherries","DoubleCherries","BS","descinm",
         "getstattest","skewness","kurtosis","diameter", "WienerIndex", 
         "betweenness", "closeness", "eigenvector","diameterW", "WienerIndexW", "betweennessW", "closenessW", 
         "eigenvectorW","minAdj","maxAdj","minLap","maxLap")

for (i in Name) {
  ind_1 = which(data$stats==i)
  data = data[c(ind_1),]
  p <- ggplot(data, aes(x=stats, y=values, fill=clusters)) + 
    geom_boxplot(width=0.30, color="black", alpha=0.7,size = 0.3, position=position_dodge(0.9)) +
    theme_minimal()+ xlab("")+  ylab("")+ theme(legend.position = "none")+
    theme(axis.text.x = element_text(color="black", size=27, angle=0) ,axis.text.y = element_text(color="black", size=24, angle=0),
          plot.title = element_text(color = "black", size = 14, face = "bold",hjust = 0.5))
  
  print(p)
  ggsave(paste0("~/",i,".pdf"),  device = "pdf",width = 7, height = 5, units = "in")
  dev.off()
}
#*******************************************************************************
