#!/usr/bin/env Rscript
library('data.table')
rawb <- read.csv("./bibletaxonomy.csv", stringsAsFactors = FALSE, header = FALSE, col.names = c('book','chapter','verse'))

books <- unique(rawb$book)
master <- c()
years <- 1800:2200
sepchars <- c('@', '#', '$', '-', '&', '.', ' ', '+', ':', '_', '~')
list3 <- c()
listS <- c()
list2 <- c('jesus', 'Jesus', 'jesus!', 'Jesus!')
for (i in 1:length(books)){
  b <- books[i]
  setb <- rawb[rawb$book == b,]
  b <- gsub(' ', '', b)
  list <- c(b, tolower(b), paste(b, '!', sep=''), paste(tolower(b), '!', sep=''))
  for (y in years) {
    list3 <- c(list3, paste(b, toString(y), sep=''))
    list3 <- c(list3, paste(b, toString(y), '!', sep=''))
    list3 <- c(list3, paste(tolower(b), toString(y), sep=''))
    list3 <- c(list3, paste(tolower(b), toString(y), '!', sep=''))
    for (sc in sepchars) {
      list3 <- c(list3, paste(b, sc, toString(y), sep=''))
      list3 <- c(list3, paste(b, sc, toString(y), '!', sep=''))
      list3 <- c(list3, paste(tolower(b), sc, toString(y), sep=''))
      list3 <- c(list3, paste(tolower(b), sc, toString(y), '!', sep=''))
    }
  }
  if (b == 'Revelation' || b == 'Psalm') {
    b4 <- paste(b, 's', sep="")
    for (y in years) {
      list3 <- c(list3, paste(b4, toString(y), sep=''))
      list3 <- c(list3, paste(b4, toString(y), '!', sep=''))
      list3 <- c(list3, paste(tolower(b4), toString(y), sep=''))
      list3 <- c(list3, paste(tolower(b4), toString(y), '!', sep=''))
      for (sc in sepchars) {
        list3 <- c(list3, paste(b4, sc, toString(y), sep=''))
        list3 <- c(list3, paste(b4, sc, toString(y), '!', sep=''))
        list3 <- c(list3, paste(tolower(b4), sc, toString(y), sep=''))
        list3 <- c(list3, paste(tolower(b4), sc, toString(y), '!', sep=''))
      }
    }
  }
  for (c in unique(setb$chapter)){
    list <- c(list, paste(b, toString(c), sep=''))
    list <- c(list, paste(b, toString(c), '!', sep=''))
    list <- c(list, paste(tolower(b), toString(c), sep=''))
    list <- c(list, paste(tolower(b), toString(c), '!', sep=''))
    for (sc in sepchars) {
      listS <- c(listS, paste(b, sc, toString(c), sep=''))
      listS <- c(listS, paste(b, sc, toString(c), '!', sep=''))
      listS <- c(listS, paste(tolower(b), sc, toString(c), sep=''))
      listS <- c(listS, paste(tolower(b), sc, toString(c), '!', sep=''))
    }
    for (v in setb[setb$chapter == c, 'verse']) {
      list <- c(list, paste(b, toString(c), toString(v), sep=''))
      list <- c(list, paste(b, toString(c), ':', toString(v), sep=''))
      list <- c(list, paste(b, toString(c), toString(v), '!', sep=''))
      list <- c(list, paste(b, toString(c), ':', toString(v), '!', sep=''))
      list <- c(list, paste(tolower(b), toString(c), toString(v), sep=''))
      list <- c(list, paste(tolower(b), toString(c), ':', toString(v), sep=''))
      list <- c(list, paste(tolower(b), toString(c), toString(v), '!', sep=''))
      list <- c(list, paste(tolower(b), toString(c), ':', toString(v), '!', sep=''))
      for (sc in sepchars) {
        listS <- c(listS, paste(b, sc, toString(c), toString(v), sep=''))
        listS <- c(listS, paste(b, sc, toString(c), ':', toString(v), sep=''))
        listS <- c(listS, paste(b, sc, toString(c), toString(v), '!', sep=''))
        listS <- c(listS, paste(b, sc, toString(c), ':', toString(v), '!', sep=''))
        listS <- c(listS, paste(tolower(b), sc, toString(c), toString(v), sep=''))
        listS <- c(listS, paste(tolower(b), sc, toString(c), ':', toString(v), sep=''))
        listS <- c(listS, paste(tolower(b), sc, toString(c), toString(v), '!', sep=''))
        listS <- c(listS, paste(tolower(b), sc, toString(c), ':', toString(v), '!', sep=''))
      }
      list2 <- c(list2, paste('jesus', toString(c), toString(v), sep=''))
      list2 <- c(list2, paste('jesus', toString(c), ':', toString(v), sep=''))
      list2 <- c(list2, paste('Jesus', toString(c), toString(v), sep=''))
      list2 <- c(list2, paste('Jesus', toString(c), ':', toString(v), sep=''))
      list2 <- c(list2, paste('jesus', toString(c), toString(v), '!', sep=''))
      list2 <- c(list2, paste('jesus', toString(c), ':', toString(v), '!', sep=''))
      list2 <- c(list2, paste('Jesus', toString(c), toString(v), '!', sep=''))
      list2 <- c(list2, paste('Jesus', toString(c), ':', toString(v), '!', sep=''))
      if (grepl('\\d', b)) {
        b3 <- gsub('1', 'I', b)
        b3 <- gsub('2', 'II', b3)
        b3 <- gsub('3', 'III', b3)
        list <- c(list, paste(b3, toString(c), toString(v), sep=''))
        list <- c(list, paste(b3, toString(c), ':', toString(v), sep=''))
        list <- c(list, paste(b3, toString(c), toString(v), '!', sep=''))
        list <- c(list, paste(b3, toString(c), ':', toString(v), '!', sep=''))
        list <- c(list, paste(tolower(b3), toString(c), toString(v), sep=''))
        list <- c(list, paste(tolower(b3), toString(c), ':', toString(v), sep=''))
        list <- c(list, paste(tolower(b3), toString(c), toString(v), '!', sep=''))
        list <- c(list, paste(tolower(b3), toString(c), ':', toString(v), '!', sep=''))
        for (sc in sepchars) {
          listS <- c(listS, paste(b3, sc, toString(c), toString(v), sep=''))
          listS <- c(listS, paste(b3, sc, toString(c), ':', toString(v), sep=''))
          listS <- c(listS, paste(b3, sc, toString(c), toString(v), '!', sep=''))
          listS <- c(listS, paste(b3, sc, toString(c), ':', toString(v), '!', sep=''))
          listS <- c(listS, paste(tolower(b3), sc, toString(c), toString(v), sep=''))
          listS <- c(listS, paste(tolower(b3), sc, toString(c), ':', toString(v), sep=''))
          listS <- c(listS, paste(tolower(b3), sc, toString(c), toString(v), '!', sep=''))
          listS <- c(listS, paste(tolower(b3), sc, toString(c), ':', toString(v), '!', sep=''))
        }
      }
      if (b == 'Revelation' || b == 'Psalm') {
        b2 <- paste(b, 's', sep="")
        list <- c(list, b2, tolower(b2), paste(b2, '!', sep=''), paste(tolower(b2), '!', sep=''))
        list <- c(list, paste(b2, toString(c), toString(v), sep=''))
        list <- c(list, paste(b2, toString(c), ':', toString(v), sep=''))
        list <- c(list, paste(b2, toString(c), toString(v), '!', sep=''))
        list <- c(list, paste(b2, toString(c), ':', toString(v), '!', sep=''))
        list <- c(list, paste(tolower(b2), toString(c), toString(v), sep=''))
        list <- c(list, paste(tolower(b2), toString(c), ':', toString(v), sep=''))
        list <- c(list, paste(tolower(b2), toString(c), toString(v), '!', sep=''))
        list <- c(list, paste(tolower(b2), toString(c), ':', toString(v), '!', sep=''))
        for (sc in sepchars) {
          listS <- c(listS, paste(b2, sc, toString(c), toString(v), sep=''))
          listS <- c(listS, paste(b2, sc, toString(c), ':', toString(v), sep=''))
          listS <- c(listS, paste(b2, sc, toString(c), toString(v), '!', sep=''))
          listS <- c(listS, paste(b2, sc, toString(c), ':', toString(v), '!', sep=''))
          listS <- c(listS, paste(tolower(b2), sc, toString(c), toString(v), sep=''))
          listS <- c(listS, paste(tolower(b2), sc, toString(c), ':', toString(v), sep=''))
          listS <- c(listS, paste(tolower(b2), sc, toString(c), toString(v), '!', sep=''))
          listS <- c(listS, paste(tolower(b2), sc, toString(c), ':', toString(v), '!', sep=''))
        }
      }
    }
  }
  list <- unique(list)
  master <- c(master, list)
  write.table(data.table(list), paste('./Lists/', b, ".txt", sep=""), col.names = FALSE, row.names = FALSE, quote = FALSE)
}
list2 <- sort(unique(list2))
write.table(data.table(list2), paste('./Lists/Jesus.txt', sep=""), col.names = FALSE, row.names = FALSE, quote = FALSE)
write.table(data.table(list3), paste('./Lists/Years.txt', sep=""), col.names = FALSE, row.names = FALSE, quote = FALSE)
write.table(data.table(listS), paste('./Lists/SpecialMaster.txt', sep=""), col.names = FALSE, row.names = FALSE, quote = FALSE)
master <- c(master, list2, list3, listS)
write.table(data.table(master), paste('./Master.txt', sep=""), col.names = FALSE, row.names = FALSE, quote = FALSE)