library(ape)
library(phangorn)
library(apTreeshape)
library(igraph)
library(phyloTop)
library(moments)
library(devtools)
devtools::install_github('Leonardini/treeCentrality')
library(treeCentrality)
library(seqinr)


### This function factory recursively computes values for subtrees rooted at internal nodes  
computeLRValues = function(tree, FUN) {
  n = Ntip(tree)
  N = 2 * n - 1
  Tab = matrix(NA, n - 1, 2)
  edges = tree$edge
  for (ind in (N - 1):1) {
    curRow = edges[ind,] - n
    pos = Tab[curRow[1], 1]
    Tab[curRow[1], 2 - is.na(pos)] = 1 + ifelse(curRow[2] <= 0, 0, FUN(Tab[curRow[2],]))
  }
  Tab
}


### This function computes the sizes of the left and right subtrees rooted at internal nodes
computeLRSizes = function(tree) {
  return(computeLRValues(tree, sum))
}

### This function computes the I2 statistic of a tree
computeI2 = function(tree) {
  n = Ntip(tree)
  LRMat = computeLRSizes(tree)
  values = abs(LRMat[,1] - LRMat[,2]) / (rowSums(LRMat) - 2)
  output = sum(values[is.finite(values)])
  output
}

### This function computes the B1 statistic of a tree
computeB1 = function(tree) {
  n = Ntip(tree)
  depths = node.depth(tree, method = 2)
  output = sum(1/(depths[-(1:(n+1))] - 1))
  output
}

### This function computes the B2 statistic of a tree
computeB2 = function(tree) {
  n = Ntip(tree)
  depths = node.depth.edgelength(tree)[1:n]
  output = sum(depths/2^depths)
  output
}

#Branching speed feature
BS=function(clade){
  tips=length(clade$tip.label)
  L=mean(node.depth.edgelength(clade)[1:tips])
  return(tips/L)
}

lttbelow <- function(tree) {
  if (class(tree)!="phylo") tree=as(tree,"phylo")
  N=2*tree$Nnode+1 # total number of nodes. Each will get a time. 
  Ntips=(N+1)/2 # number of tips
  times=vector(mode="numeric",length=N)
  times[Ntips+1]=0; # roots has time 0. this line's not needed (already 0). for clarity
  topdepths=times # initialize to all 0s. 
  # set both times and topological depths for all nodes by traversing edge list
  for (k in 1:(N-1)) {
    times[tree$edge[k,2]]=tree$edge.length[k]+ times[tree$edge[k,1]]
    topdepths[tree$edge[k,2]]=1+topdepths[tree$edge[k,1]]
  }
  # times now has all node's times. and it is in the order 1,2,3,.. n-1 where first are tips, then internals. the Ntips+1'st one is 0 because it is the root. order(times) is the index of them ordered increasingly. times[order(times)] is jjust like sort(times)
  nodeids=order(times)
  sorttimes=times[nodeids]
  
  # now I want a vector that I create by : start with 1. at each time, in order, add 1 to the previous entry if the point was an internal node. otherwise subtract 1. 
  contribs=-1+2*as.numeric(order(times)>Ntips) # each contribution to ltt
  ltt=1+cumsum(contribs)
  return(data.frame(times=sorttimes,topdepths=topdepths[nodeids], lttbelow=ltt,nodeids=nodeids))
}

getstattest <- function(tree) {
  if (class(tree)!="phylo") tree=as(tree,"phylo")
  #    
  # lttbelow gives times, nodeids, and number lineages just below each node
  # for each node, i want to know whether one of its descendants is the NEXT one to branch (after it)
  lttb<-lttbelow(tree)
  IND<-which(lttb$nodeids > length(tree$tip.label))
  intonly=lttb$nodeids[IND] # internal nodes in chron order
  nlins=lttb$lttbelow[IND] # number of lineages below each internal node, ch order
  nexttobranch=c(intonly[-1],0) # next one to branch after each intl node, ch order
  declist <- vapply(intonly, function(x) {tree$edge[which(tree$edge[,1]==x),2]},FUN.VALUE=c(1,1)) # each internal node's 2 descendants
  colnames(declist)=as.character(intonly)     
  # For each node in intonly, I want to know: is one of its descendants next? 
  
  sresult=vapply(1:length(intonly),function(x) {is.element(nexttobranch[x],declist[,as.character(intonly[x])])},FUN.VALUE=0.5)
  
  probs=2/nlins; probs[length(probs)]=0 # the last one can't have a descendant that branches next
  W=(sum(sresult-probs))/sqrt((sum(probs*(1-probs))))
  L=length(probs); # L=round(reducefrac*L)
  return(list(details=data.frame(nodeids=intonly,s=sresult,nlins=nlins,probs=probs,topdepths=lttb$topdepths[IND],times=lttb$times[IND]),W=W))
}

descinm <- function(tree,m=2) {
  if (!inherits(tree, "phylo")) 
    stop("Tree should be an object of class \"phylo\".")
  lttb  <- lttbelow(tree)
  IND <- lttb$nodeids > length(tree$tip.label) # LOG
  intonly=lttb$nodeids[IND] # internal nodes in chron order
  nlins=lttb$lttbelow[IND]
  L=length(intonly)
  # init outputs
  success=0*intonly # success: was descendant in next m branching events? 
  probs=0*intonly # probability of success
  # to get next m to branch, do intonly[ thisone:(thisone+m)]. nlines[same]
  for (j in 1:(L-1)) {
    dj=tree$edge[which(tree$edge[,1]==intonly[j]),2]
    nextm=intonly[ (j+1):min(j+m,L)] # next m to branch
    nlj=nlins[j:min(j+m-1,L)] # nlins now, next, next .. m-1 after
    success[j]=any(is.element(dj,nextm))
    probs[j]=prod(1 - 1/nlj)^2
  }
  mW=0*(1:m)
  for (Offset in 0:(m-1)) {
    Filter=seq(from=1+Offset,by=m,to=L-1) # ignore last entry, success 0, prob 0.
    mW[Offset+1]=(sum(success[Filter]-probs[Filter]))/sqrt(sum(probs[Filter]*(1-probs[Filter]))) 
  }
  return(list(details=data.frame(success=success,probs=probs,nodeids=intonly),mW=mW,W=mean(mW)))
}

i_bl <- function(tree) {
  if (class(tree)!="phylo") tree=as(tree,"phylo")
  
  
  n<- length(tree$tip.label)
  ibl <-  tree$edge.length[tree$edge[,2]>n]
  #mean_ibl <-mean(ibl)
  #median_ibl <- median(ibl)
  #var_ibl <-var(ibl)
  #stdev_ibl <- sqrt(var_ibl)
  
  return(ibl)
}



Clade_featurs=function(tr){
  features=numeric(35)
  features[1]=sackin(as.treeshape(tr),"pda")
  features[2]=colless(as.treeshape(tr),"pda")
  features[3]=var(node.depth.edgelength(tr)[1:length(tr$tip.label)])
  features[4]=computeI2(tr)
  features[5]=computeB1(tr)
  features[6]=computeB2(tr)
  features[7]=avgLadder(tr, normalise = TRUE)
  features[8]=ILnumber(tr, normalise = TRUE)
  features[9]=pitchforks(tr, normalise = TRUE)
  features[10]=maxHeight(tr, normalise = TRUE)
  features[11]=computeMaxWidth(tr)
  features[12]=computeDelW(tr)
  features[13]=computeStairs1(tr)
  features[14]=computeStairs2(tr)
  features[15]=computeCherries(tr, DOUBLE = FALSE)
  features[16]=computeCherries(tr, DOUBLE = TRUE)
  features[17]=BS(tr)
  features[18]=descinm(tr,m=2)$W
  features[19]=getstattest(tr)$W
  features[20]=skewness(i_bl(tr))
  features[21]=kurtosis(i_bl(tr))
  features[22:26]=computeNetworkStats(tr, weight = FALSE, meanpath = FALSE, maxOnly = TRUE)
  features[27:31]=computeNetworkStats(tr, weight = TRUE, meanpath = FALSE, maxOnly = TRUE)
  features[32:35]=computeSpectralStats(tr, weight = c(FALSE, TRUE), adj = c(FALSE,TRUE), norm = FALSE, dist = FALSE, full = FALSE,
                                       maxOnly = TRUE, unitMean = FALSE)
  return(features)
}

i = "Salmonella"
load(paste0("/Users/maryam/Desktop/Research/nn/Trees/",i,".Rdata"))
tr = extract.clade(tree,56210)
cluster_2 =  Clade_featurs(tr)
