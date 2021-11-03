library(tidyverse)
library(magrittr)
library(ape)

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
    useSubtrees = lapply(curIDs, function(x) {extract.clade(curTree, x, collapse.singles = FALSE)})
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