library(dplyr)
library(data.table)


cols2utf8 <- function(raw) {
  for (name in colnames(raw[,sapply(raw, is.character), with=F])){
       Encoding(raw[[name]]) <- "UTF-8"}
}


load_faset <- function(fileName) {
  t.base <- fread(fileName, header=TRUE, stringsAsFactors=FALSE)
  t <- t.base %>% group_by(Target) %>% mutate(Rank=dense_rank(-n), Probability=round((1/(sum(n)/n)), 3))
  return (t)
}
f = fread(list.files()[1])
d <- f %>% group_by(Target, Assoc) %>% summarise(n=n()) %>% arrange(Target, desc(n), Assoc)
write.table(d, 'online freq new.txt', sep = '\t', fileEncoding = 'utf8')