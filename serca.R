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
  f$orig.table <- t
  f$path <- path
  f$abspath <- tools::file_path_as_absolute(path)
  f$name <- tools::file_path_sans_ext(path)
  f$tag <- substring(f$name, 1, 3)
  f$table <- f$orig.table
  f$n.targets <- length(unique(t$Target))
  return (f)
}


faset_target_meta <- function(f, Target.name) {
  t <- f$orig.table
  st <- t %>% filter(Target==Target.name)
  info <- list()
  info$top3 <- st %>% top_n(3, n)
  info$setsize <- length(st$Assoc)
  info$total <- sum(st$n)
  return (info)
}


idio <- function(t) {
  t %>% filter(n == 1)
}

nonidio <- function(t) {
  t %>% filter(n > 1)
}

nonq <- function(t) {
  t %>% filter(Assoc != '?')
}

get_setsizes <- function(t) {
  t %>% nonidio() %>% nonq() %>% group_by(Target) %>% summarise(setsize=n())
}


Sys.setlocale("LC_CTYPE", "Turkish_Turkey.1254")
Sys.setlocale("LC_COLLATE", "Turkish_Turkey.1254")

onl <- load_faset('data/frequency/onl_freq.txt')
onl$n.subjects <- 52
onl$setsizes <- get_setsizes(onl$table)

tek <- load_faset('data/frequency/tek_freq.txt')
tek$n.subjects <- 100
tek$setsizes <- get_setsizes(tek$table)

umi <- load_faset('data/frequency/umi_freq.txt')
umi$n.subjects <- 104
umi$setsizes <- get_setsizes(umi$table)

# umi.fas <- umi$table %>% nonidio() %>% nonq() %>% group_by(Target) %>% summarise(set_size=n())
# tum.fas <- semi_join(tek.fas, umi.fas, by='Target')

tum = list()
tum$setsizes <- semi_join(tek$setsizes, umi$setsizes, by='Target')

ss <-  onl$setsizes %>% merge(umi$setsizes, by = 'Target', suffixes = c('.onl', '.umi')) %>% merge(tum$setsizes)
#names(ss)[names(ss) == 'setsize'] <- 'setsize.tek'
colnames(ss) <- c('Target', 'SS.onl', 'SS.umi', 'SS.tek')
