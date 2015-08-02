library(dplyr)
library(magrittr)
library(foreach)
library(data.table)

faset_target_meta <- function(f, Target.name) {
  t <- f$orig.table
  st <- t %>% filter(Target==Target.name)
  info <- list()
  info$top3 <- st %>% top_n(3, n)
  info$setsize <- length(st$Assoc)
  info$total <- sum(st$n)
  return (info)
}

load_frequencies <- function(path) {
  f <- fread(path, stringsAsFactors = FALSE)
  f <- f %>% arrange(Target, desc(n), Assoc)
  f <- f %>% mutate(Rank = dense_rank(-n), 
                    RankMax=rank(n, ties.method = "max"),
                    Probability=round((1/(sum(n)/n)), 3))
  return (f)
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

get_asetsizes <- function(t) {
  t %>% nonidio() %>% nonq() %>% group_by(Target) %>% summarise(setsize=n())
}


#Sys.setlocale("LC_CTYPE", "Turkish_Turkey.1254")
#Sys.setlocale("LC_COLLATE", "Turkish_Turkey.1254")

frequencies.meta <- tbl_dt(read.delim('data/meta.txt', stringsAsFactors = TRUE, fileEncoding = "UTF-8"))
#frequencies.paths <- lapply(fas.meta$Label, function(label){paste('data/frequency/', label, '.txt', sep='')})
frequencies <- frequencies.meta %>%
  transmute(Label=Label, Path = paste('data/frequency/', Label, '.txt', sep=''))

frequencies <- frequencies %>% mutate(Frequencies = tbl_dt(read.delim(Path, stringsAsFactors = FALSE)))

#setsizes <- lapply(frequencies, get_asetsizes)


#targets <- transmute(fa, )
#fa <- mutate(fa, NumTargets = length(unique(frequencies$Target)))
#fa <- mutate(fa, AssocSetSize = get_asetsizes(frequencies))
#fa <- mutate(fa, NumIdio = nrow(idio(frequencies)))

                    
# umi.fas <- umi$table %>% nonidio() %>% nonq() %>% group_by(Target) %>% summarise(set_size=n())
# tum.fas <- semi_join(tek.fas, umi.fas, by='Target')

#tum = list()
#tum$setsizes <- semi_join(tek$setsizes, umi$setsizes, by='Target')

#ss <-  onl$setsizes %>% merge(umi$setsizes, by = 'Target', suffixes = c('.onl', '.umi')) %>% merge(tum$setsizes)
#names(ss)[names(ss) == 'setsize'] <- 'setsize.tek'
#colnames(ss) <- c('Target', 'SS.onl', 'SS.umi', 'SS.tek')
