### No longer used!
### "descinm"
### "getstattest"

prepareStats = function(inputFile = "fullMetadata.csv") {
  Tab = read_csv(inputFile)
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
