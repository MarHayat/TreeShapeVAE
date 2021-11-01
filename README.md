# TreeShapeVAE
This repository contains the material for the paper: "Deep clustering of bacterial tree images"

"Trees.zip" contains all the big trees which we extract the subtrees from them. The subtrees are named as "bigtree name x" which x is the node that the subtree is rooted at! For example "Campylobacter14080.png" is the image of one of the subtrees of Campylobacter which is rooted at node 14080.

"Clusters.zip" contains the 9 clusters of the images.

"Plots.R" contains the code for generating the figures 4-8 of the paper.

"all_date.csv" contains the date of all subtrees.
  The columns show the subtrees, dates, frequencies, and the clustre numbers respectively. For example, the first row shows that Acinetobacter1775 is in cluster 1 and has 1 tip in 2010. Acinetobacter1775 is the subtree of Acinetobacter tree that rooted at node 1775.
  
"all_loc_region.csv" contains the regions of all subtrees.
  The columns show the subtrees, location, frequencies, and the clustre numbers respectively. For example, the first row shows that Acinetobacter1775 is in cluster 1 and has has 5 tips in East Asia & Pacific. 

"tab_species.csv" shows the distribution of the species across the clusters. For example, column 1 shows the distribution of the Acinetobacter across the 9 clusters. The some of each row shows the numbers of subtrees at each cluster.

"metadata.csv" contains the metadata (including time and location) corresponding to each tip of the trees. I used this meta data to generate "all_loc_region.csv" and "all_date.csv".
