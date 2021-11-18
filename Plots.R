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
setwd("~/TreeShapeVAE/Clusters")

tab_species = matrix(0,9,28)
colnames(tab_species) = names
for (i in c(0:8)) {
  allFiles = list.files(path = paste0("cluster",i), 
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
df = read.csv("../all_loc_region.csv")
df = df[,1:ncol(df)]
i=1
df$cluster = ifelse(df$cluster==paste0("cluster",i),paste0("cluster",i),"All datasets")

p <- ggplot(df, aes(x=Location,y = Freq,fill= cluster)) + theme_light()+
  geom_boxplot(width=0.50, color="black", alpha=0.7,size = 0.2,
               position=position_dodge(0.9),outlier.size=0.2)+
  theme(axis.text.x = element_text(color="black", size=12, angle=90) ,
        axis.text.y = element_text(color="black", size=15, angle=90))+
  theme(axis.title.x = element_text(size = 5),axis.title.y = element_text(size = 5))+ 
  guides(fill=guide_legend(ncol=2))+ 
  ylab("")+xlab("")+ 
  theme(legend.title = element_blank(),legend.position = "top",
        legend.text = element_text(size = 14),legend.key.size = unit(1, 'cm'))
p
ggsave(paste0("~/cls",i,"_region.pdf"),width =5, height =5)

#*******************************************************************************
# new figure 6: the  geographical  distributions  of  the  clusters, with percents, from 
# leonid's metadata 
fullMeta = readr::read_csv("../fullMetadata.csv")
mydf = fullMeta %>% select(ID, Species, Region) 

# i need to use group and summarize to work out the portions of each ID in each location 
# i want the numbers in each ID -- summarize, by grouping on id, use n()  
# i want the numbers in each geo_loc_name in each id -- summarize, by grouping on id and geo_loc_name, use n()
# then divide them. no doubt there is a tidy way to do this. this isn't it. 
fullMeta$country = word(fullMeta$geo_loc_name, sep = "\\:")
#fullMeta$region = countrycode(fullMeta$country, origin="country.name",
#                                 destination = "region") 
# fullMeta$region[which(is.na(fullMeta$region))] = "Unknown"

df1 = fullMeta %>% group_by(ID) %>% summarise(treesize = n(), Cluster = Cluster[1], 
                                            Species=Species[1]  )
df2 = fullMeta %>% group_by(ID, Region) %>% summarise(locsize = n(),
                                                      Cluster = Cluster[1], 
                                                      Species=Species[1])
df2$locperc = 100*df2$locsize / df1$treesize[match(df2$ID, df1$ID)] 
df2$Cluster = paste("Cluster", df2$Cluster+1)
ggplot(data= df2, aes(x=Cluster, y=locperc, color=Region))+geom_boxplot() +
  facet_wrap(~Region, scales = "free_y") + theme(axis.text.x = 
                                element_text(color="black", size=12, angle=90,hjust=1), 
                              axis.title.x = element_blank()) + ylab("Percent of subtree")+ 
  guides(color=FALSE)
ggsave("cluster-geography.pdf", height = 6, width = 7)


#*******************************************************************************
# CC new figure 7: the distribution of the species dates at each cluster.
library(lubridate)
fullMeta$date = ymd(fullMeta$target_creation_date)
fullMeta$year = factor(year(fullMeta$date))
df1 = fullMeta %>% group_by(ID) %>% summarise(treesize = n(), Cluster = Cluster[1], 
                                              Species=Species[1]  )
df2 = fullMeta %>% group_by(ID, year) %>% summarise(yearsize = n(),
                                                      Cluster = Cluster[1], 
                                                      Species=Species[1])
df2$yearperc = 100*df2$yearsize / df1$treesize[match(df2$ID, df1$ID)] 
df2$Cluster = paste("Cluster", df2$Cluster+1)

dfall = df2; dfall$Cluster = All


 ggplot(data= df2, aes(x=year, y=yearperc, color=Cluster))+geom_boxplot(position="dodge") +
  facet_wrap(~Cluster, scales = "free_y") + theme(axis.text.x = 
                   element_text(color="black", size=12, angle=90,hjust=1), 
                   axis.title.x = element_blank()) + ylab("Percent of subtree") + 
  guides(color=FALSE)
 # p + geom_boxplot(data = subset(df2, select = -Cluster),
 #                 aes(x=year, y=yearperc), position = "dodge") 
 ggsave("cluster-year.pdf", height = 6, width = 7)
 
 #*******************************************************************************
 # CC new figure- percent of cluster by epi_type 
 df2 = fullMeta %>% group_by(ID, epi_type) %>% summarise(episize = n(),
                                                     Cluster = Cluster[1], 
                                                     Species=Species[1])
 df2$epiperc = 100*df2$episize / df1$treesize[match(df2$ID, df1$ID)] 
 df2$Cluster = paste("Cluster", df2$Cluster+1)
 
  ggplot(data= df2, aes(x=Cluster, y=epiperc, fill=epi_type))+
    geom_boxplot(position = position_dodge(0.8), width=0.5) +
   theme(axis.text.x =  element_text(color="black", size=12, angle=90,hjust=1), 
                    axis.title.x = element_blank(),
         legend.position = "bottom") + ylab("Percent of subtree") +
    labs(color="Data type")+ labs(fill="Data type")
  
  ggsave("cluster-epitype.pdf", width = 8, height = 6)
  
 
 
 

#*******************************************************************************
# old figure 7: the distribution of the species dates at each cluster.
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
