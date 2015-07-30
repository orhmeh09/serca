library(dplyr)
library(data.table)
group_by(Target, )


load_faset <- function(fileName) {
  t.base <- fread(fileName, header=TRUE, stringsAsFactors=FALSE)
  t <- t.base %>% group_by(Target) %>% mutate(Rank=dense_rank(-n), Probability=round((1/(sum(n)/n)), 3))
  return (t)
}

mk_faset_meta <- function(t) {
  info <- list()
  info$n.targets <- t %>% summarise(length(unique(Target)))
  return(info)
}

faset_target_meta <- function(t, Target.name) {
  st <- t %>% filter(Target==Target.name)
  info <- list()
  info$subtable <- st
  info$setsize <- st %>% summarise(length(Assoc))
  info$total <- st %>% summarise(sum(n))
  return(info)
}




