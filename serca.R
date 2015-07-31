library(dplyr)
library(tools)
write.tdelim <- function(x, file) {
  write.table(x, file, sep="\t", row.names=F, fileEncoding="UTF-8", quote=F)
}

read.tdelim <- function(file) {
  read.delim(file, stringsAsFactors = F, fileEncoding = "utf8")
}

load_faset <- function(fileName) {
  t.base <- tbl_df(read.tdelim(fileName))
  t <- t.base %>% group_by(Target) %>% mutate(Rank=dense_rank(-n), Rank_Max=rank(n, ties.method="max"), P=round((1/(sum(n)/n)), 3))
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


Sys.setlocale("LC_CTYPE", "Turkish_Turkey.1254")
Sys.setlocale("LC_COLLATE", "Turkish_Turkey.1254")
#setwd('/data/frequency')
filenames <- list.files(pattern='*.txt')
names <- sapply(filenames, file_path_sans_ext)
fs <- sapply(filenames, load_faset, simplify = F)


