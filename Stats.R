library(tidyverse)
library(magrittr)
library(ape)
library(igraph)
library(mclust)
library(aricode)
library(lubridate)
library(apTreeshape)
library(phyloTop)
library(DescTools)
library(countrycode)

initDir = getwd()
setwd("~/Downloads/DownloadedSoftware/treeCentrality/R/")
for (fname in list.files()) { source(fname) }
setwd(initDir)
source("~/Downloads/DownloadedSoftware/TreeShapeStats/SpectralComparison.R")

STAT_NAMES = c("B1", "B2", "betweenness", "cherries", "closeness", "colless", "DelW", "I2",   "ILnumber", 
               "kurtosis", "maxHeight", "maxWidth", "pitchforks", "sackin", "skewness", "stairs1", "stairs2", "treeSize")
MIN_SIZE = 35
MAX_SIZE = 100
minFarness = rep(0, MAX_SIZE)
minFarness[1:2] = 1:2
curIncr = 2
for (index in 3:MAX_SIZE) {
  if ((index %% 3 == 0 && near(log(index/3, 2), round(log(index/3, 2)))) || 
      (index %% 3 == 1 && near(log((index - 1)/3, 2), round(log((index - 1)/3, 2))))) {
    curIncr = curIncr + 1
  }
  minFarness[index] = minFarness[index - 1] + curIncr
}

setwd("~/Downloads/PublicationsAndSubmissions/TreeShapeVAE/")

prepareClusters = function(inputFile = "metadata.csv", outputFile = "fullMetadata.csv") {
  Tab = read_csv(inputFile)
  initDir = getwd()
  fullTab = tibble()
  for (ind in 0:8) {
    setwd(paste0("Clusters/cluster", ind))
    LF = list.files() %>% 
      str_remove_all(".png")
    IDs = str_extract(LF, "[0-9]+")
    Specs = str_sub(LF, 1, nchar(LF) - nchar(IDs))
    fullTab = bind_rows(fullTab, tibble(Cluster = ind, ID = as.integer(IDs), Species = Specs))
    setwd(initDir)
  }
  uSpecs = unique(fullTab$Species)
  extraTab = tibble()
  for (spec in uSpecs) {
    fname = paste0("Trees/", spec, ".Rdata")
    e = new.env()
    load(fname, e)
    curNames = ls(e)
    stopifnot(length(curNames) == 1)
    curTree = get(curNames[1], e)
    curIDs = fullTab %>%
      filter(Species == spec) %>%
      pull(ID)
    useSubtrees = lapply(curIDs, function(x) {extract.clade(curTree, x, collapse.singles = TRUE)})
    useLabels = lapply(useSubtrees, function(x) {x$tip.label})
    extraTab = bind_rows(extraTab, tibble(Species = spec, ID = curIDs, labels = useLabels))
  }
  stopifnot(range(sapply(extraTab$labels, length)) == c(MIN_SIZE, MAX_SIZE))
  fullTab = inner_join(fullTab, extraTab, by = c("Species", "ID")) %>%
    unnest(labels)
  Tab = right_join(Tab, fullTab, by = c("Species" = "Species", "target_acc" = "labels"))
  write_csv(Tab, outputFile)
  Tab
}

prepareStats = function(inputFile = "fullMetadata.csv") {
  Tab = read_csv(inputFile)
  ### Preparing the stats for Figure 6
  Tab = Tab %>%
    mutate(Country = geo_loc_name %>% str_remove("\\(.*\\)$") %>% word(sep = "\\:")) %>%
    mutate(Region = countrycode(Country, origin = "country.name", destination = "region")) %>%
    mutate(Region = replace_na(Region, "Missing"))
  Table6 = Tab %>% 
    select(Cluster, Region)
  Table4 = Tab %>%
    distinct(Species, ID, .keep_all = TRUE) %>%
    select(Cluster, Species)
  Tab = Tab %>% 
    mutate(Year = year(target_creation_date)) %>%
    mutate(Year = replace_na(Year, "Missing"))
  Table7 = Tab %>%
    select(Cluster, Year)
  Tab = Tab %>%
    mutate(epi_type = replace_na(epi_type, "Missing"))
  Table8 = Tab %>%
    select(Cluster, epi_type)
  write_csv(Tab, inputFile)
  output = list(Table4, Table6, Table7, Table8)
  output
}

prepareTreeStats = function(inputFile = "fullMetadata.csv", outputFile = "fullTreeStats.csv", extraFile = "Subtrees.RData") {
  Tab = read_csv(inputFile)
  uCombos = Tab %>% 
    select(Species, ID) %>% 
    distinct()
  uSpecs = unique(uCombos$Species)
  allTrees = vector("list", length(uSpecs))
  print(paste("There are", length(uSpecs), "species to process"))
  if (file.exists(extraFile)) {
    load(extraFile)
  } else {
    for (spec in uSpecs) {
      print(spec)
      fname = paste0("Trees/", spec, ".Rdata")
      e = new.env()
      load(fname, e)
      curNames = ls(e)
      stopifnot(length(curNames) == 1)
      curTree = get(curNames[1], e)
      curIDs = uCombos %>%
        filter(Species == spec) %>%
        pull(ID)
      allTrees[[spec]] = lapply(curIDs, function(x) {extract.clade(curTree, x, collapse.singles = TRUE)})
      names(allTrees[[spec]]) = curIDs
    }
    allTrees = allTrees[names(allTrees) != ""]
    save(allTrees, file = extraFile)
  }
  allStats = matrix(0, 0, length(STAT_NAMES), dimnames = list(NULL, STAT_NAMES))
  trackStats = matrix(0, 0, 2, dimnames = list(NULL, c("Species", "ID")))
  for (spec in uSpecs) {
    print(spec)
    curTrees = allTrees[[spec]]
    print(paste("There are", length(curTrees), "trees to process"))
    for (ID in names(curTrees)) {
      curTree = curTrees[[ID]]
      allStats = allStats %>%
        rbind(computeAllStats(curTree))
      trackStats = trackStats %>%
        rbind(c(spec, ID))
    }
  }
  fullStats = cbind(trackStats, allStats) %>%
    as_tibble() %>%
    mutate_at(vars(!matches("Species")), as.double)
  write_csv(fullStats, outputFile)
  fullStats
}

computeAllStats = function(inputTree) {
  allFunctions = paste0("compute", StrCap(STAT_NAMES, method = "first"))
  inputTree = checkPhylogeneticTree(inputTree)
  N = Ntip(inputTree)
  output = sapply(allFunctions, function(x) { f = get(x); y = f(inputTree); out = max(y); out })
  names(output) = STAT_NAMES
  output
}

normalizeAllStats = function(inputFile = "fullTreeStats.csv") {
  Tab = read_csv(inputFile)
  Tab = Tab %>%
    mutate_at("B1",         ~{divide_by(., log(2) * (treeSize - 1))})  %>%
    mutate_at("B2",         ~{divide_by(., log2(treeSize))})           %>%
    mutate_at("betweenness",~{divide_by(., 4 * (treeSize - 1)^2 / 3)}) %>%
    mutate_at("cherries",   ~{divide_by(., treeSize / 2)})             %>%
    mutate_at("closeness",  ~{multiply_by(., minFarness[treeSize])})   %>%
    mutate_at("colless",    ~{divide_by(., choose(treeSize - 1, 2))})  %>%
    mutate_at("DelW",       ~{divide_by(., treeSize / 2)})             %>%
    mutate_at("I2",         ~{divide_by(., treeSize - 2)})             %>%
    mutate_at("ILnumber",   ~{divide_by(., treeSize - 2)})             %>%
    mutate_at("maxHeight",  ~{divide_by(., treeSize - 1)})             %>%
    mutate_at("maxWidth",   ~{divide_by(., treeSize)})                 %>%
    mutate_at("pitchforks", ~{divide_by(., treeSize / 3)})             %>%
    mutate_at("sackin",     ~{divide_by(., choose(treeSize + 1, 2) -1)})
  write_csv(Tab, file = str_replace(inputFile, ".csv", "Normalized.csv"))
  Tab
}

computeTreeSize = function(inputTree) {
  output = Ntip(inputTree)
  output
}

computeILnumber = function(inputTree) {
  output = ILnumber(inputTree, normalise = FALSE)
  output
}

computeKurtosis = function(inputTree) {
  goodEdges = which(inputTree$edge[,2] > Ntip(inputTree))
  output = Kurt(inputTree$edge.length[goodEdges], method = 1) + 3
  output
}

computeSkewness = function(inputTree) {
  goodEdges = which(inputTree$edge[,2] > Ntip(inputTree))
  output = Skew(inputTree$edge.length[goodEdges], method = 1)
  output
}

computeColless = function(inputTree) {
  output = colless.phylo(inputTree, normalise = FALSE)
  output
}

estimateAssociation = function(Factor1, Factor2) {
  ### Computes statistics for a two-way table; Cramer's V statistic, the uncertainty coefficient, the adjusted Rand index
  xTable = table(Factor1, Factor2)
  V = CramerV(xTable, correct = FALSE, conf.level = 0.95)
  Vcorr = CramerV(xTable, correct = TRUE, conf.level = 0.95)
  # comp = clustComp(Factor1, Factor2)
  ARI = adjustedRandIndex(Factor1, Factor2)
  NMI = NMI(Factor1, Factor2)
  AMI = AMI(Factor1, Factor2)
  # cs = colSums(xTable)
  # rs = rowSums(xTable)
  # Sum = sum(xTable)
  # rowFracs = rs/Sum
  # rowEntropy = -sum(rowFracs * log(rowFracs))
  # colFracs = cs/Sum
  # colEntropy = -sum(colFracs * log(colFracs))
  # jointFracs = xTable/Sum
  # jointEntropy = -sum(jointFracs * log(jointFracs))
  # mutualInfo = rowEntropy + colEntropy - jointEntropy
  # uncertaintyRow = mutualInfo/rowEntropy
  # uncertaintyCol = mutualInfo/colEntropy
  # uncertaintySym = mutualInfo/((rowEntropy + colEntropy)/2)
  output = c(V = V[1], V_L = V[2], V_U = V[3], cV = Vcorr[1], cv_L = Vcorr[2], cv_R = Vcorr[3], ARI = ARI, NMI = NMI, AMI = AMI)
  output
}

compareStats = function(inputFile = "fullTreeStats.csv", metadataFile = "fullMetadata.csv") {
  statTable = read_csv(inputFile)
  metaTable = read_csv(metadataFile) %>%
    select(Cluster, ID, Species) %>%
    distinct()
  statTable = statTable %>%
    inner_join(metaTable, by = c("Species", "ID")) %>%
    select(-Species, -ID) %>%
    select(Cluster, everything())
  N = range(statTable$Cluster)
  numComparisons = choose(N[2] - N[1] + 1, 2)
  allComps = tibble()
  M = ncol(statTable) - 1
  totalComparisons = M * numComparisons
  output = tibble()
  print(paste("There are", M, "statistics to process"))
  for (ind in 2:ncol(statTable)) {
    curName = colnames(statTable)[ind]
    print(paste(ind - 1, curName))
    curStats = statTable %>% 
      select(Cluster, all_of(curName))
    for (index1 in N[1]:(N[2] - 1)) {
      curStats1 = curStats %>%
        filter(Cluster == index1) %>%
        select(all_of(curName))
      for (index2 in (index1 + 1):N[2]) {
        curStats2 = curStats %>%
          filter(Cluster == index2) %>%
          select(all_of(curName))
        curTest = stats::t.test(unlist(curStats1), unlist(curStats2), alternative = "two.sided", var.equal = FALSE)
        output = output %>%
          bind_rows(tibble(C1 = index1, C2 = index2, stat = curName, p_value = curTest$p.value))
      }
    }
  }
  output = output %>%
    arrange(p_value) %>%
    mutate(cutoff = 0.05/totalComparisons) %>%
    mutate(significant = (p_value < cutoff))
  outFile = str_replace(inputFile, ".csv", "Tested.csv")
  write_csv(output, outFile)
  output
}

clusterStats = function(inputFile = "fullTreeStatsNormalized.csv", metadataFile = "fullMetadata.csv") {
  statTable = read_csv(inputFile)
  metaTable = read_csv(metadataFile) %>%
    select(Cluster, ID, Species) %>%
    distinct()
  statTable = statTable %>%
    inner_join(metaTable, by = c("Species", "ID")) %>%
    select(-Species, -ID) %>%
    select(Cluster, everything())
}

# Tab0 = prepareClusters(inputFile = "metadata.csv", outputFile = "fullMetadata.csv")
# Res  = prepareStats(inputFile = "fullMetadata.csv")
# Assoc = lapply(Res, function(x) { estimateAssociation(x[[1]], x[[2]]) })
# treeStats = prepareTreeStats(inputFile = "fullMetadata.csv", outputFile = "fullTreeStats.csv", extraFile = "Subtrees.RData")
# normTreeStats = normalizeAllStats(inputFile = "fullTreeStats.csv")
# treeAssocUnnorm = compareStats(inputFile = "fullTreeStats.csv", metadataFile = "fullMetadata.csv")
# treeAssocNorm   = compareStats(inputFile = "fullTreeStatsNormalized.csv", metadataFile = "fullMetadata.csv")
# N1 = treeAssocUnnorm %>% filter(stat=="treeSize") %>% pull(significant) %>% sum
# N2 = treeAssocNorm %>% group_by(stat) %>% mutate(N=sum(significant)) %>% slice(1) %>% ungroup %>% arrange(-N)
# N3 = treeAssocNorm %>% group_by(C1, C2) %>% mutate(N = sum(significant)) %>% slice(1) %>% ungroup %>% filter(N == 0) %>% nrow
# N4 = treeAssocNorm %>% group_by(C1, C2) %>% mutate(N = sum(significant)) %>% slice(1) %>% ungroup %>% filter(N == 1) %>% nrow
# numDist = rep(NA, 9)
# for (ind in 0:8) {
#   numDist[ind + 1] = treeAssocNorm %>% filter(C1 == ind | C2 == ind) %>% mutate(other = C1 + C2 - ind) %>% group_by(other) %>%
#     mutate(N = sum(significant)) %>% slice(1) %>% ungroup %>% filter(N > 0) %>% nrow
# }
