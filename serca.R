library(tools)
library(iterators)
library(magrittr)
library(dplyr)
library(tidyr)
library(foreach)


set_os_locales <- function() {
  if(.Platform$OS.type != "unix") {
    if(Sys.getlocale("LC_CTYPE") != "en_US.UTF-8") {
      Sys.setlocale("LC_CTYPE", "Turkish_Turkey.1254")
      Sys.setlocale("LC_COLLATE", "Turkish_Turkey.1254")
    }
  }
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
get_setsizes <- function(frequencies, meta) {
  setsizes <- frequencies %>% filter(n > 1, Assoc != '?') %>% group_by(Label, Target) %>% summarise(SetSize=n()) %>% arrange(Target, Label)
  idios <- frequencies %>% filter(n == 1) %>% group_by(Label, Target) %>% summarise(Idio=n()) %>% arrange(Target, Label)
  setsizes <- setsizes %>% left_join(idios)

  
  setsizes <- setsizes %>% mutate(Total=SetSize+Idio)
  
  
  frequencies %>% left_join(select(meta, Label, Subjects))
  blanks <- setsizes %>% mutate(Blanks =  )
  setsizes <- setsizes %>% left_join(blanks) %>% mutate(Blanks= replace(Blanks, is.na(Blanks), 0))
  setsizes <- setsizes %>% arrange(Target, Label, SetSize, )
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
set_os_locales()

