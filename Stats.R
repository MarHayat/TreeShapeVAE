library(tidyverse)
library(magrittr)
library(ape)
library(mclust)
library(aricode)
library(lubridate)
library(apTreeshape)
library(phyloTop)
library(DescTools)

initDir = getwd()
setwd("~/Downloads/DownloadedSoftware/treeCentrality/R/")
for (fname in list.files()) { source(fname) }
setwd(initDir)
source("~/Downloads/DownloadedSoftware/TreeShapeStats/SpectralComparison.R")

STAT_NAMES = c("B1", "B2", "betweenness", "cherries", "closeness", "colless", "DelW", "descinm", "getstattest", "I2",   "ILnumber", "kurtosis", "maxHeight", "maxWidth", "pitchforks", "sackin", "skewness", "stairs1", "stairs2")
MIN_SIZE = 35
MAX_SIZE = 100

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

findSignificantPairs = function(Factor1, Factor2, N = 999) {
  ### Computes statistics for a two-way table; Cramer's V statistic, the uncertainty coefficient, the adjusted Rand index
  xTable = table(Factor1, Factor2)
  V = cramerV(xTable)
  cs = colSums(xTable)
  rs = rowSums(xTable)
  Sum = sum(xTable)
  rowFracs = rs/Sum
  rowEntropy = -sum(rowFracs * log(rowFracs))
  colFracs = cs/Sum
  colEntropy = -sum(colFracs * log(colFracs))
  jointFracs = xTable/Sum
  jointEntropy = -sum(jointFracs * log(jointFracs))
  mutualInfo = rowEntropy + colEntropy - jointEntropy
  uncertaintyRow = mutualInfo/rowEntropy
  uncertaintyCol = mutualInfo/colEntropy
  uncertaintySym = mutualInfo/((rowEntropy + colEntropy)/2)
  ARI = adjustedRandIndex(Factor1, Factor2)
  set.seed(1234567890L)
  ARIperm = rep(NA, N)
  for (ind in 1:N) {
    if (ind %% 100 == 0) { print(ind) }
    ARIperm[ind] = adjustedRandIndex(Factor1, sample(Factor2, replace = FALSE))
  }
  p = (sum(ARIperm > ARI) + 1) / (N + 1)
  output = clustComp(Factor1, Factor2)
  output = c(V, uncertaintyRow = uncertaintyRow, uncertaintyCol = uncertaintyCol, uncertaintySym = uncertaintySym, ARI = ARI, p = p)
  output
}

prepareStats = function(inputFile = "fullMetadata.csv") {
  Tab = read_csv(inputFile)
  ### Preparing the stats for Figure 6
  ### Country classification: http://databank.worldbank.org/data/download/site-content/CLASS.xlsx opened and saved as csv file
  Tab = Tab %>%
    separate(geo_loc_name, into = c("Country", "Location"), sep = ":", remove = FALSE)
  countries = read_csv("CLASS.csv") %>%
    select(Economy, Region) %>%
    rename(Country = Economy)
  Tab = Tab %>%
    mutate(Region = countries$Region[match(Country, countries$Country)])
  uCountryNoMatch = Tab %>%
    filter(is.na(Region)) %>%
    select(Country) %>%
    distinct(Country) %>%
    mutate(CC = str_remove(Country, "stan$") %>% str_remove("\\(.*\\)$") %>% str_trim()) %>%
    mutate(CC = recode(CC,`Gaza Strip`="Gaza",`Laos`="Lao",`Slovakia`="Slovak",`USA`="United States",`Viet Nam`="Vietnam"))%>%
    mutate(minCountry = str_extract(CC, "[ ]?[A-Za-z]+$") %>% str_trim() %>% recode(`Guiana`="Guyana")) %>%
    filter(nchar(minCountry) > 3) %>%
    mutate(correctCountry = map_chr(minCountry, ~{countries$Country[str_detect(countries$Country, .)][1]})) %>%
    filter(!is.na(correctCountry)) %>%
    select(Country, correctCountry)
  extraMatch = match(Tab$Country, uCountryNoMatch$Country)
  goodPos = which(!is.na(extraMatch))
  Tab$Country[goodPos] = uCountryNoMatch$correctCountry[extraMatch[goodPos]]
  Tab = Tab %>%
    mutate(Region = countries$Region[match(Country, countries$Country)]) %>%
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
  output = list(Tab, Table4, Table6, Table7, Table8)
  write_csv(Tab, inputFile)
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
    save(allTrees, extraFile)
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
  output = sapply(allFunctions, function(x) { f = get(x); y = f(inputTree); out = ifelse(x == "computeMaxHeight", y/(N-1), 
                  ifelse(x == "computePitchforks", 3*y/N, ifelse(x == "computeSackin", y/(choose(N+1,2)-1), max(y)))); out })
  names(output) = STAT_NAMES
  output
}

### From https://github.com/MarHayat/flu_prediction/blob/master/Tree_Statistics.R
getstattest <- function(tree) {
  if (class(tree) != "phylo") { tree = as(tree, "phylo") }
  # lttbelow gives times, nodeids, and number lineages just below each node
  # for each node, i want to know whether one of its descendants is the NEXT one to branch (after it)
  lttb <- lttbelow(tree)
  IND <- which(lttb$nodeids > length(tree$tip.label))
  intonly = lttb$nodeids[IND] # internal nodes in chron order
  nlins = lttb$lttbelow[IND] # number of lineages below each internal node, ch order
  nexttobranch = c(intonly[-1], 0) # next one to branch after each intl node, ch order
  declist <- vapply(intonly, function(x) {
    tree$edge[which(tree$edge[,1]==x),2]
  }, FUN.VALUE=c(1,1)) # each internal node's 2 descendants
  colnames(declist) = as.character(intonly)     
  # For each node in intonly, I want to know: is one of its descendants next?
  sresult = vapply(1:length(intonly), function(x) {
    is.element(nexttobranch[x],declist[,as.character(intonly[x])])
  }, FUN.VALUE=0.5)
  probs= 2/nlins
  probs[length(probs)] = 0 # the last one can't have a descendant that branches next
  W = (sum(sresult - probs))/sqrt((sum(probs*(1 - probs))))
  L = length(probs)
  return(list(details = data.frame(nodeids = intonly, s = sresult, nlins = nlins, probs = probs, topdepths
                                   = lttb$topdepths[IND], times = lttb$times[IND]), W = W))
}

### From https://github.com/MarHayat/flu_prediction/blob/master/Tree_Statistics.R
#' is a descendant of the node among the next m to branch? 
#' @param tree object of class phylo
#' @param m integer; we ask whether each node's desc is among the next m 
#' @return A list. details: a data frame with success (was descn in the next m), 
#' probabilities (how probable was that), nodeids (as in tree); mW: W values for all 
#' offsets (eg 1, 3, 5, .. for m=2, and 2, 4, 6 ...). W: mean of mW. 
#' @examples
#' descinm(rtree(12))
descinm <- function(tree, m = 2) {
  if (!inherits(tree, "phylo")) { stop("Tree should be an object of class \"phylo\".") }
  lttb  <- lttbelow(tree)
  IND <- lttb$nodeids > length(tree$tip.label) # LOG
  intonly = lttb$nodeids[IND] # internal nodes in chron order
  nlins = lttb$lttbelow[IND]
  L = length(intonly)
  # init outputs
  success = rep(0, L) # success: was descendant in next m branching events? 
  probs = rep(0, L) # probability of success
  # to get next m to branch, do intonly[ thisone:(thisone+m)]. nlines[same]
  for (j in 1:(L-1)) {
    dj = tree$edge[which(tree$edge[,1] == intonly[j]), 2]
    nextm = intonly[(j+1):min(j+m,L)] # next m to branch
    nlj = nlins[j:min(j+m-1, L)] # nlins now, next, next .. m-1 after
    success[j] = any(is.element(dj, nextm))
    probs[j] = prod(1 - 1/nlj)^2
  }
  mW = rep(0, m)
  for (Offset in 0:(m-1)) {
    Filter = seq(from = 1 + Offset, by = m, to = L - 1) # ignore last entry, success 0, prob 0.
    mW[Offset + 1] = (sum(success[Filter] - probs[Filter]))/sqrt(sum(probs[Filter]*(1 - probs[Filter]))) 
  }
  return(list(details = data.frame(success = success, probs = probs, nodeids = intonly), mW = mW, W = mean(mW)))
}

### From https://github.com/MarHayat/flu_prediction/blob/master/Tree_Statistics.R
#' Compute times of nodes and tips and arrange in chronological order along with LTT
#' @param tree Object of class phylo, or convertible with as(tree,"phylo")
#' @return data frame with times, topdepths (topological depth from root, in number of edges, 
#' lttbelow: the number of lineages in the tree just below the current node, and nodeids: the ID of the node. 
#' 1: number tips are the tips. Order corresponds to the order in the phylo tree. 
#' @examples
#' lttbelow(rtree(10))
lttbelow <- function(tree) {
  if (class(tree) != "phylo") { tree = as(tree, "phylo") }
  N = 2 * tree$Nnode + 1 # total number of nodes. Each will get a time. 
  Ntips = (N+1)/2 # number of tips
  times = vector(mode = "numeric", length = N)
  times[Ntips+1] = 0 # roots has time 0. this line's not needed (already 0). for clarity
  topdepths = times # initialize to all 0s. 
  # set both times and topological depths for all nodes by traversing edge list
  for (k in 1:(N-1)) {
    times[tree$edge[k,2]] = tree$edge.length[k] + times[tree$edge[k,1]]
    topdepths[tree$edge[k,2]] = 1 + topdepths[tree$edge[k,1]]
  }
  # times now has all node's times and it is in the order 1,2,3,.. n-1 where first are tips, then internals. the Ntips+1'st one is 0 because it is the root. order(times) is the index of them ordered increasingly. times[order(times)] is just like sort(times)
  nodeids = order(times)
  sorttimes = times[nodeids]
  # now I want a vector that I create by : start with 1. at each time, in order, add 1 to the previous entry if the point was an internal node. otherwise subtract 1. 
  contribs = -1 + 2 * as.numeric(order(times) > Ntips) # each contribution to ltt
  ltt = 1 + cumsum(contribs)
  return(data.frame(times = sorttimes, topdepths = topdepths[nodeids], lttbelow = ltt, nodeids = nodeids))
}

computeDescinm = function(inputTree) {
  output = descinm(inputTree, m = 2)$W
  output
}

computeGetstattest = function(inputTree) {
  output = getstattest(inputTree)$W
  output
}

computeILnumber = function(inputTree) {
  output = ILnumber(inputTree, normalise = TRUE)
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

