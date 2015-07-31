library(dplyr)
library(tools)
library(magrittr)
write.tdelim <- function(x, file) {
  write.table(x, file, sep="\t", row.names=F, fileEncoding="UTF-8", quote=F)
}

read.tdelim <- function(file) {
  read.delim(file, stringsAsFactors = F, fileEncoding = "utf8")
}

load_faset <- function(path) {
  t.base <- tbl_df(read.tdelim(path))
  t <- t.base %>% group_by(Target) %>% mutate(Rank=dense_rank(-n), Rank_Max=rank(n, ties.method="max"), P=round((1/(sum(n)/n)), 3))
  f = list()
  f$table <- t
  f$path <- path
  f$abspath <- tools::file_path_as_absolute(path)
  f$name <- tools::file_path_sans_ext(path)
  f$tag <- substring(f$name, 1, 3)
  f
}

mk_faset_meta <- function(t) {
  info <- list()
  info$n.targets <- t %>% summarise(length(unique(Target)))
  return(info)
}

faset_target_meta <- function(f, Target.name) {
  t <- f$table
  st <- t %>% filter(Target==Target.name)
  info <- list()
  info$top3 <- st %>% top_n(3, n)
  info$setsize <- length(st$Assoc)
  info$total <- sum(st$n)
  return(info)
}


Sys.setlocale("LC_CTYPE", "Turkish_Turkey.1254")
Sys.setlocale("LC_COLLATE", "Turkish_Turkey.1254")


frequency_path = 'data/frequency/'
paths <- list.files(frequency_path, pattern='*.txt', full.names = TRUE)


onl <- load_faset('data/frequency/onl_freq.txt')
onl$nsubj <- 52

tek <- load_faset('data/frequency/tek_freq.txt')
tek$nsubj <- 100

umi <- load_faset('data/frequency/umi_freq.txt')
umi$nsubj <- 104


