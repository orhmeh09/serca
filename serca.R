library(tools)
library(iterators)
library(magrittr)
library(dplyr)
library(tidyr)
library(foreach)
library(reshape2)

set_os_locales <- function() {
  if(.Platform$OS.type != "unix") {
    if(Sys.getlocale("LC_CTYPE") != "en_US.UTF-8") {
      Sys.setlocale("LC_CTYPE", "Turkish_Turkey.1254")
      Sys.setlocale("LC_COLLATE", "Turkish_Turkey.1254")
    }
  }
}

omega_sq <- function(aovm){
  sum_stats <- summary(aovm)[[1]]
  SSm <- sum_stats[["Sum Sq"]][1]
  SSr <- sum_stats[["Sum Sq"]][2]
  DFm <- sum_stats[["Df"]][1]
  MSr <- sum_stats[["Mean Sq"]][2]
  W2 <- (SSm-DFm*MSr)/(SSm+SSr+MSr)
  return(W2)
}

omega_manual <- function(SSm, SSr, DFm, MSr) {
  (SSm-DFm*MSr)/(SSm+SSr+MSr)
}
load_meta <- function(path) {
 tbl_df(read.delim(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8"))
}


get_labels <- function(meta) {
  unique(select(fmeta, Label))
}



load_one_frequency_table <- function(path, label) {
  tbl_df(read.delim(path, stringsAsFactors = FALSE, fileEncoding="UTF-8"))  %>%
    arrange(Target, desc(n), Assoc) %>%
    mutate(Label = label)
}

rank_frequencies <- function(frequencies) {
  frequencies %>% group_by(Label, Target) %>%
    mutate(Rank = dense_rank(desc(n)), 
           RankMax=rank(n, ties.method = "max"),
           Probability=round((1/(sum(n)/n)), 3)
    )
}

nsubjects <- function(meta, label) {
  meta %>% filter(Label==label) %>% select(Subjects)
}
get_setsizes <- function(frequencies) {
  setsizes <- frequencies %>% filter(n > 1, Assoc != '?') %>% group_by(Label, Target) %>% summarise(SetSize=n()) %>% arrange(Target, Label)
  idios <- frequencies %>% filter(n == 1) %>% group_by(Label, Target) %>% summarise(Idio=n()) %>% arrange(Target, Label)
  setsizes <- setsizes %>% left_join(idios)
  setsizes <- setsizes %>% mutate(Totals=SetSize+Idio)
  setsizes <- setsizes %>% arrange(Target, Label, SetSize)
  return(setsizes)
}

get_setsizes_stats <- function(setsizes) {
  setsizes %>% group_by(Label) %>% summarise(N = n(),
                                             Mean = round(mean(SetSize), 2),
                                             SD = round(sd(SetSize), 2), 
                                             Median = round(median(SetSize, 2)))
}

get_blanks <- function(frequencies) {
  frequencies %>% filter(Assoc == '?') %>% group_by(Target, Label) %>% summarise(Blanks=sum(n))
}

get_top_frequencies <- function(frequencies, rank) {
  frequencies %>%
    filter(Rank == rank) %>% 
    group_by(Label, Target) %>% 
    filter(row_number() == 1) %>%
    select(Target, Label, Probability) %>% 
    spread(Label, Probability) %>% 
    select(Target, tem, umi, onl) %>% 
    arrange(Target)
}

do_anova_top6 <- function(frequencies) {
  t <- frequencies %>% 
    filter(Rank <= 6) %>%
    group_by(Label, Target) %>%
    filter(row_number() <= 6) %>%
    mutate(RowNo = row_number()) %>%
    ungroup() %>%
    arrange(Target, Label, desc(Probability)) %>%
    select(RowNo, Label, Target, Probability)
  t$RowNo <- factor(t$RowNo)
  t$Label <- factor(t$Label)
  t$Target <- factor(t$Target)
  t <- tbl_df(t %>% mutate(RowNo = paste('R', RowNo, sep='')) %>% dcast(Label + Target ~ RowNo))

  
  #a <- aov(Probability ~ Label + Error(RowNo/Target), data=top6) # <- lm(Probability ~ Label + (RowNo/Target), data=top6) # ez <- lmer(Probability ~ Label + Target + RowNo + (1 | Target), data=top6)
  a <- aov(Probability ~ RowNo * Label + Error(Target/RowNo), t)
  return (a)
}

do_new_words <- function(frequencies) {
  f.onl <- frequencies %>% filter(Label=="onl")
  f.umi <- frequencies %>% filter(Label=="umi")
  f.tem <- frequencies %>% filter(Label=="tem")
  
  news <- rbind(f.umi, f.onl) %>% filter(n > 1 & Assoc != '?') %>% group_by(Label) %>%
    anti_join(f.tem, by="Assoc") %>% ungroup() %>%
    arrange(desc(Probability)) %>% select(Label, Target, Assoc, n, Strength=Probability)
  
  news.counts <- news %>% select(Label, Target, Assoc, n) %>% spread(Label, n, fill=0)
  news.strengths <- news %>% select(Label, Target, Assoc, Strength) %>% spread(Label, Strength, fill=0.0)
  news.counts <- news.counts %>% rename(n.onl = onl, n.umi = umi)
  news.strengths <- news.strengths %>% rename(p.onl = onl, p.umi = umi)
  news <- inner_join(news.counts, news.strengths) %>% select(Target, Assoc, p.onl, n.onl, p.umi, n.umi) %>% arrange(Target, Assoc)
  return (news)
}
#set_os_locales()

