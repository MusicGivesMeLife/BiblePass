#!/usr/bin/env Rscript
library('data.table')
library('english')
library('stringr')
library('stringi')
library('Hmisc')

numbers_only <- function(x) !grepl("\\D", x)

num_to_stri <- function(numl) {
  numlist <- c()
  numchar <- unlist(strsplit(numl, NULL))
  for(numi in 1:length(numchar)) {
    if(numchar[numi] == "0") {
      numchar[numi] = "zero"
    }
    else if(numchar[numi] == "1") {
      numchar[numi] = "one"
    }
    else if(numchar[numi] == "2") {
      numchar[numi] = "two"
    }
    else if(numchar[numi] == "3") {
      numchar[numi] = "three"
    }
    else if(numchar[numi] == "4") {
      numchar[numi] = "four"
    }
    else if(numchar[numi] == "5") {
      numchar[numi] = "five"
    }
    else if(numchar[numi] == "6") {
      numchar[numi] = "six"
    }
    else if(numchar[numi] == "7") {
      numchar[numi] = "seven"
    }
    else if(numchar[numi] == "8") {
      numchar[numi] = "eight"
    }
    else if(numchar[numi] == "9") {
      numchar[numi] = "nine"
    }
  }
  numlist <- c(numlist, paste(numchar, sep="", collapse=""))
  numlist <- c(numlist, toupper(paste(numchar, sep="", collapse="")))
  numchar <- unlist(strsplit(numl, NULL))
  for(numi in 1:length(numchar)) {
    if(numchar[numi] == "0") {
      numchar[numi] = "Zero"
    }
    else if(numchar[numi] == "1") {
      numchar[numi] = "One"
    }
    else if(numchar[numi] == "2") {
      numchar[numi] = "Two"
    }
    else if(numchar[numi] == "3") {
      numchar[numi] = "Three"
    }
    else if(numchar[numi] == "4") {
      numchar[numi] = "Four"
    }
    else if(numchar[numi] == "5") {
      numchar[numi] = "Five"
    }
    else if(numchar[numi] == "6") {
      numchar[numi] = "Six"
    }
    else if(numchar[numi] == "7") {
      numchar[numi] = "Seven"
    }
    else if(numchar[numi] == "8") {
      numchar[numi] = "Eight"
    }
    else if(numchar[numi] == "9") {
      numchar[numi] = "Nine"
    }
  }
  numlist <- c(numlist, paste(numchar, sep="", collapse=""))
  if (numbers_only(numl)) {
    numl <- as.integer(numl)
    to_cap_str <- strsplit(str_replace(toString(as.english(numl)), "[-]", ' '), ' ', fixed=TRUE)
    tcrfinal <- c()
    for(tcr in to_cap_str) {
      tcrfinal <- c(tcrfinal, capitalize(tcr))
    }
    numlist <- c(numlist, toString(paste(tcrfinal, sep="", collapse="")))
    numlist <- c(numlist, toupper(toString(paste(tcrfinal, sep="", collapse=""))))
    numlist <- c(numlist, tolower(toString(paste(tcrfinal, sep="", collapse=""))))
  }
  return(numlist)
}

rawb <- read.csv("./bibletaxonomy.csv", stringsAsFactors = FALSE, header = FALSE, col.names = c('book','chapter','verse'))

books <- unique(rawb$book)
master <- c()
years <- 1800:2200
years2 <- 0:99
sepchars_y <- c('@', '!', '$', '-', '.', ':', '_')
sepchars <- c('_', '.', '-', '!', '@', ':')
favs <- c('Genesis', 'Exodus', 'Job', 'Psalm', 'Proverbs', 'Matthew', 'Mark', 'Luke', 'John', 'Romans', '1 Corinthians', 'Acts', 'Revelation')
list3 <- c()
listS <- c()
list2 <- c('jesus', 'Jesus', 'jesus!', 'Jesus!', 'JESUS', 'JESUS!')
looptimes <- c()
if (file.exists(paste('./Lists/NumbersOnly.txt', sep=""))) 
  #Delete file if it exists
  file.remove(paste('./Lists/NumbersOnly.txt', sep=""))
if (file.exists(paste('./Lists/SpecialMaster.txt', sep=""))) 
  #Delete file if it exists
  file.remove(paste('./Lists/SpecialMaster.txt', sep=""))
if (file.exists(paste('./Lists/Years.txt', sep=""))) 
  #Delete file if it exists
  file.remove(paste('./Lists/Years.txt', sep=""))
for (i in 1:length(books)){
  b <- books[i]
  setb <- rawb[rawb$book == b,]
  b <- gsub(' ', '', b)
  list <- c(b, tolower(b), paste(b, '!', sep=''), paste(tolower(b), '!', sep=''))
  numlist <- c()
  fn <- paste('./Lists/', b, ".txt", sep="")
  if (file.exists(fn)) 
    #Delete file if it exists
    file.remove(fn)
  for (y in years) {
    list3 <- c(list3, paste(b, toString(y), sep=''))
    list3 <- c(list3, paste(b, toString(y), '!', sep=''))
    list3 <- c(list3, paste(tolower(b), toString(y), sep=''))
    list3 <- c(list3, paste(tolower(b), toString(y), '!', sep=''))
    list3 <- c(list3, paste(toupper(b), toString(y), sep=''))
    list3 <- c(list3, paste(toupper(b), toString(y), '!', sep=''))
    for (sc in sepchars_y) {
      list3 <- c(list3, paste(b, sc, toString(y), sep=''))
      list3 <- c(list3, paste(b, sc, toString(y), '!', sep=''))
      list3 <- c(list3, paste(tolower(b), sc, toString(y), sep=''))
      list3 <- c(list3, paste(tolower(b), sc, toString(y), '!', sep=''))
      list3 <- c(list3, paste(toupper(b), sc, toString(y), sep=''))
      list3 <- c(list3, paste(toupper(b), sc, toString(y), '!', sep=''))
    }
  }
  write.table(data.table(list3), paste('./Lists/Years.txt', sep=""), col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
  list3 <- c()
  for (y in years2) {
    if (y < 10) {
      list3 <- c(list3, paste(b, paste0('0', toString(y)), sep=''))
      list3 <- c(list3, paste(b, paste0('0', toString(y)), '!', sep=''))
      list3 <- c(list3, paste(tolower(b), paste0('0', toString(y)), sep=''))
      list3 <- c(list3, paste(tolower(b), paste0('0', toString(y)), '!', sep=''))
      list3 <- c(list3, paste(toupper(b), paste0('0', toString(y)), sep=''))
      list3 <- c(list3, paste(toupper(b), paste0('0', toString(y)), '!', sep=''))
      for (sc in sepchars_y) {
        list3 <- c(list3, paste(b, sc, toString(y), sep=''))
        list3 <- c(list3, paste(b, sc, toString(y), '!', sep=''))
        list3 <- c(list3, paste(tolower(b), sc, paste0('0', toString(y)), sep=''))
        list3 <- c(list3, paste(tolower(b), sc, paste0('0', toString(y)), '!', sep=''))
        list3 <- c(list3, paste(toupper(b), sc, paste0('0', toString(y)), sep=''))
        list3 <- c(list3, paste(toupper(b), sc, paste0('0', toString(y)), '!', sep=''))
      }
    }
    else {
      list3 <- c(list3, paste(b, toString(y), sep=''))
      list3 <- c(list3, paste(b, toString(y), '!', sep=''))
      list3 <- c(list3, paste(tolower(b), toString(y), sep=''))
      list3 <- c(list3, paste(tolower(b), toString(y), '!', sep=''))
      list3 <- c(list3, paste(toupper(b), toString(y), sep=''))
      list3 <- c(list3, paste(toupper(b), toString(y), '!', sep=''))
      for (sc in sepchars_y) {
        list3 <- c(list3, paste(b, sc, toString(y), sep=''))
        list3 <- c(list3, paste(b, sc, toString(y), '!', sep=''))
        list3 <- c(list3, paste(tolower(b), sc, toString(y), sep=''))
        list3 <- c(list3, paste(tolower(b), sc, toString(y), '!', sep=''))
        list3 <- c(list3, paste(toupper(b), sc, toString(y), sep=''))
        list3 <- c(list3, paste(toupper(b), sc, toString(y), '!', sep=''))
      }
    }
  }
  write.table(data.table(list3), paste('./Lists/Years.txt', sep=""), col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
  list3 <- c()
  if (b == 'Revelation' || b == 'Psalm') {
    b4 <- paste(b, 's', sep="")
    for (y in years) {
      list3 <- c(list3, paste(b4, toString(y), sep=''))
      list3 <- c(list3, paste(b4, toString(y), '!', sep=''))
      list3 <- c(list3, paste(tolower(b4), toString(y), sep=''))
      list3 <- c(list3, paste(tolower(b4), toString(y), '!', sep=''))
      list3 <- c(list3, paste(toupper(b4), toString(y), sep=''))
      list3 <- c(list3, paste(toupper(b4), toString(y), '!', sep=''))
      for (sc in sepchars_y) {
        list3 <- c(list3, paste(b4, sc, toString(y), sep=''))
        list3 <- c(list3, paste(b4, sc, toString(y), '!', sep=''))
        list3 <- c(list3, paste(tolower(b4), sc, toString(y), sep=''))
        list3 <- c(list3, paste(tolower(b4), sc, toString(y), '!', sep=''))
        list3 <- c(list3, paste(toupper(b4), sc, toString(y), sep=''))
        list3 <- c(list3, paste(toupper(b4), sc, toString(y), '!', sep=''))
      }
    }
    for (y in years2) {
      if (y < 10) {
        list3 <- c(list3, paste(b4, paste0('0', toString(y)), sep=''))
        list3 <- c(list3, paste(b4, paste0('0', toString(y)), '!', sep=''))
        list3 <- c(list3, paste(tolower(b4), paste0('0', toString(y)), sep=''))
        list3 <- c(list3, paste(tolower(b4), paste0('0', toString(y)), '!', sep=''))
        list3 <- c(list3, paste(toupper(b4), paste0('0', toString(y)), sep=''))
        list3 <- c(list3, paste(toupper(b4), paste0('0', toString(y)), '!', sep=''))
        for (sc in sepchars_y) {
          list3 <- c(list3, paste(b4, sc, toString(y), sep=''))
          list3 <- c(list3, paste(b4, sc, toString(y), '!', sep=''))
          list3 <- c(list3, paste(tolower(b4), sc, paste0('0', toString(y)), sep=''))
          list3 <- c(list3, paste(tolower(b4), sc, paste0('0', toString(y)), '!', sep=''))
          list3 <- c(list3, paste(toupper(b4), sc, paste0('0', toString(y)), sep=''))
          list3 <- c(list3, paste(toupper(b4), sc, paste0('0', toString(y)), '!', sep=''))
        }
      }
      else {
        list3 <- c(list3, paste(b4, toString(y), sep=''))
        list3 <- c(list3, paste(b4, toString(y), '!', sep=''))
        list3 <- c(list3, paste(tolower(b4), toString(y), sep=''))
        list3 <- c(list3, paste(tolower(b4), toString(y), '!', sep=''))
        list3 <- c(list3, paste(toupper(b4), toString(y), sep=''))
        list3 <- c(list3, paste(toupper(b4), toString(y), '!', sep=''))
        for (sc in sepchars_y) {
          list3 <- c(list3, paste(b4, sc, toString(y), sep=''))
          list3 <- c(list3, paste(b4, sc, toString(y), '!', sep=''))
          list3 <- c(list3, paste(tolower(b4), sc, toString(y), sep=''))
          list3 <- c(list3, paste(tolower(b4), sc, toString(y), '!', sep=''))
          list3 <- c(list3, paste(toupper(b4), sc, toString(y), sep=''))
          list3 <- c(list3, paste(toupper(b4), sc, toString(y), '!', sep=''))
        }
      }
    }
  }
  for (c in unique(setb$chapter)){
    list <- c(list, paste(b, toString(c), sep=''))
    list <- c(list, paste(b, toString(c), '!', sep=''))
    list <- c(list, paste(tolower(b), toString(c), sep=''))
    list <- c(list, paste(tolower(b), toString(c), '!', sep=''))
    list <- c(list, paste(toupper(b), toString(c), sep=''))
    list <- c(list, paste(toupper(b), toString(c), '!', sep=''))
    c2list <- num_to_stri(toString(c))
    for (c2 in c2list) {
      list <- c(list, paste(b, toString(c2), sep=''))
      list <- c(list, paste(b, toString(c2), '!', sep=''))
      list <- c(list, paste(tolower(b), toString(c2), sep=''))
      list <- c(list, paste(tolower(b), toString(c2), '!', sep=''))
      list <- c(list, paste(toupper(b), toString(c2), sep=''))
      list <- c(list, paste(toupper(b), toString(c2), '!', sep=''))
    }
    for (sc in sepchars) {
      listS <- c(listS, paste(b, sc, toString(c), sep=''))
      listS <- c(listS, paste(b, sc, toString(c), '!', sep=''))
      listS <- c(listS, paste(tolower(b), sc, toString(c), sep=''))
      listS <- c(listS, paste(tolower(b), sc, toString(c), '!', sep=''))
      listS <- c(listS, paste(toupper(b), sc, toString(c), sep=''))
      listS <- c(listS, paste(toupper(b), sc, toString(c), '!', sep=''))
    }
    ptm <- proc.time()
    for (v in setb[setb$chapter == c, 'verse']) {
      numlist <- c(numlist, paste(toString(c), toString(v), sep=''))
      numlist <- c(numlist, paste(toString(c), ':', toString(v), sep=''))
      numlist <- c(numlist, paste(toString(c), toString(v), '!', sep=''))
      numlist <- c(numlist, paste(toString(c), ':', toString(v), '!', sep=''))
      list <- c(list, paste(b, toString(c), toString(v), sep=''))
      list <- c(list, paste(b, toString(c), ':', toString(v), sep=''))
      list <- c(list, paste(b, toString(c), toString(v), '!', sep=''))
      list <- c(list, paste(b, toString(c), ':', toString(v), '!', sep=''))
      list <- c(list, paste(tolower(b), toString(c), toString(v), sep=''))
      list <- c(list, paste(tolower(b), toString(c), ':', toString(v), sep=''))
      list <- c(list, paste(tolower(b), toString(c), toString(v), '!', sep=''))
      list <- c(list, paste(tolower(b), toString(c), ':', toString(v), '!', sep=''))
      list <- c(list, paste(toupper(b), toString(c), toString(v), sep=''))
      list <- c(list, paste(toupper(b), toString(c), ':', toString(v), sep=''))
      list <- c(list, paste(toupper(b), toString(c), toString(v), '!', sep=''))
      list <- c(list, paste(toupper(b), toString(c), ':', toString(v), '!', sep=''))
      v2list <- num_to_stri(toString(v))
      for (c2 in c2list) {
        for (v2 in v2list) {
          list <- c(list, paste(b, toString(c2), toString(v2), sep=''))
          list <- c(list, paste(b, toString(c2), ':', toString(v2), sep=''))
          list <- c(list, paste(b, toString(c2), toString(v2), '!', sep=''))
          list <- c(list, paste(b, toString(c2), ':', toString(v2), '!', sep=''))
          list <- c(list, paste(tolower(b), toString(c2), toString(v2), sep=''))
          list <- c(list, paste(tolower(b), toString(c2), ':', toString(v2), sep=''))
          list <- c(list, paste(tolower(b), toString(c2), toString(v2), '!', sep=''))
          list <- c(list, paste(tolower(b), toString(c2), ':', toString(v2), '!', sep=''))
          list <- c(list, paste(toupper(b), toString(c2), toString(v2), sep=''))
          list <- c(list, paste(toupper(b), toString(c2), ':', toString(v2), sep=''))
          list <- c(list, paste(toupper(b), toString(c2), toString(v2), '!', sep=''))
          list <- c(list, paste(toupper(b), toString(c2), ':', toString(v2), '!', sep=''))
        }
      }
      if (b %in% favs) {
        for (sc in sepchars) {
          listS <- c(listS, paste(b, sc, toString(c), toString(v), sep=''))
          listS <- c(listS, paste(b, sc, toString(c), ':', toString(v), sep=''))
          listS <- c(listS, paste(b, sc, toString(c), toString(v), '!', sep=''))
          listS <- c(listS, paste(b, sc, toString(c), ':', toString(v), '!', sep=''))
          listS <- c(listS, paste(tolower(b), sc, toString(c), toString(v), sep=''))
          listS <- c(listS, paste(tolower(b), sc, toString(c), ':', toString(v), sep=''))
          listS <- c(listS, paste(tolower(b), sc, toString(c), toString(v), '!', sep=''))
          listS <- c(listS, paste(tolower(b), sc, toString(c), ':', toString(v), '!', sep=''))
          listS <- c(listS, paste(toupper(b), sc, toString(c), toString(v), sep=''))
          listS <- c(listS, paste(toupper(b), sc, toString(c), ':', toString(v), sep=''))
          listS <- c(listS, paste(toupper(b), sc, toString(c), toString(v), '!', sep=''))
          listS <- c(listS, paste(toupper(b), sc, toString(c), ':', toString(v), '!', sep=''))
        }
      }
      list2 <- c(list2, paste('jesus', toString(c), toString(v), sep=''))
      list2 <- c(list2, paste('jesus', toString(c), ':', toString(v), sep=''))
      list2 <- c(list2, paste('Jesus', toString(c), toString(v), sep=''))
      list2 <- c(list2, paste('Jesus', toString(c), ':', toString(v), sep=''))
      list2 <- c(list2, paste('JESUS', toString(c), toString(v), sep=''))
      list2 <- c(list2, paste('JESUS', toString(c), ':', toString(v), sep=''))
      list2 <- c(list2, paste('jesus', toString(c), toString(v), '!', sep=''))
      list2 <- c(list2, paste('jesus', toString(c), ':', toString(v), '!', sep=''))
      list2 <- c(list2, paste('Jesus', toString(c), toString(v), '!', sep=''))
      list2 <- c(list2, paste('Jesus', toString(c), ':', toString(v), '!', sep=''))
      list2 <- c(list2, paste('JESUS', toString(c), toString(v), '!', sep=''))
      list2 <- c(list2, paste('JESUS', toString(c), ':', toString(v), '!', sep=''))
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
        list <- c(list, paste(toupper(b3), toString(c), toString(v), sep=''))
        list <- c(list, paste(toupper(b3), toString(c), ':', toString(v), sep=''))
        list <- c(list, paste(toupper(b3), toString(c), toString(v), '!', sep=''))
        list <- c(list, paste(toupper(b3), toString(c), ':', toString(v), '!', sep=''))
        for (c2 in c2list) {
          for (v2 in v2list) {
            list <- c(list, paste(b3, toString(c2), toString(v2), sep=''))
            list <- c(list, paste(b3, toString(c2), ':', toString(v2), sep=''))
            list <- c(list, paste(b3, toString(c2), toString(v2), '!', sep=''))
            list <- c(list, paste(b3, toString(c2), ':', toString(v2), '!', sep=''))
            list <- c(list, paste(tolower(b3), toString(c2), toString(v2), sep=''))
            list <- c(list, paste(tolower(b3), toString(c2), ':', toString(v2), sep=''))
            list <- c(list, paste(tolower(b3), toString(c2), toString(v2), '!', sep=''))
            list <- c(list, paste(tolower(b3), toString(c2), ':', toString(v2), '!', sep=''))
            list <- c(list, paste(toupper(b3), toString(c2), toString(v2), sep=''))
            list <- c(list, paste(toupper(b3), toString(c2), ':', toString(v2), sep=''))
            list <- c(list, paste(toupper(b3), toString(c2), toString(v2), '!', sep=''))
            list <- c(list, paste(toupper(b3), toString(c2), ':', toString(v2), '!', sep=''))
          }
        }
        if (b %in% favs) {
          for (sc in sepchars) {
            listS <- c(listS, paste(b3, sc, toString(c), toString(v), sep=''))
            listS <- c(listS, paste(b3, sc, toString(c), ':', toString(v), sep=''))
            listS <- c(listS, paste(b3, sc, toString(c), toString(v), '!', sep=''))
            listS <- c(listS, paste(b3, sc, toString(c), ':', toString(v), '!', sep=''))
            listS <- c(listS, paste(tolower(b3), sc, toString(c), toString(v), sep=''))
            listS <- c(listS, paste(tolower(b3), sc, toString(c), ':', toString(v), sep=''))
            listS <- c(listS, paste(tolower(b3), sc, toString(c), toString(v), '!', sep=''))
            listS <- c(listS, paste(tolower(b3), sc, toString(c), ':', toString(v), '!', sep=''))
            listS <- c(listS, paste(toupper(b3), sc, toString(c), toString(v), sep=''))
            listS <- c(listS, paste(toupper(b3), sc, toString(c), ':', toString(v), sep=''))
            listS <- c(listS, paste(toupper(b3), sc, toString(c), toString(v), '!', sep=''))
            listS <- c(listS, paste(toupper(b3), sc, toString(c), ':', toString(v), '!', sep=''))
          }
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
        list <- c(list, paste(toupper(b2), toString(c), toString(v), sep=''))
        list <- c(list, paste(toupper(b2), toString(c), ':', toString(v), sep=''))
        list <- c(list, paste(toupper(b2), toString(c), toString(v), '!', sep=''))
        list <- c(list, paste(toupper(b2), toString(c), ':', toString(v), '!', sep=''))
        for (c2 in c2list) {
          for (v2 in v2list) {
            list <- c(list, paste(b2, toString(c2), toString(v2), sep=''))
            list <- c(list, paste(b2, toString(c2), ':', toString(v2), sep=''))
            list <- c(list, paste(b2, toString(c2), toString(v2), '!', sep=''))
            list <- c(list, paste(b2, toString(c2), ':', toString(v2), '!', sep=''))
            list <- c(list, paste(tolower(b2), toString(c2), toString(v2), sep=''))
            list <- c(list, paste(tolower(b2), toString(c2), ':', toString(v2), sep=''))
            list <- c(list, paste(tolower(b2), toString(c2), toString(v2), '!', sep=''))
            list <- c(list, paste(tolower(b2), toString(c2), ':', toString(v2), '!', sep=''))
            list <- c(list, paste(toupper(b2), toString(c2), toString(v2), sep=''))
            list <- c(list, paste(toupper(b2), toString(c2), ':', toString(v2), sep=''))
            list <- c(list, paste(toupper(b2), toString(c2), toString(v2), '!', sep=''))
            list <- c(list, paste(toupper(b2), toString(c2), ':', toString(v2), '!', sep=''))
          }
        }
        for (sc in sepchars) {
          listS <- c(listS, paste(b2, sc, toString(c), toString(v), sep=''))
          listS <- c(listS, paste(b2, sc, toString(c), ':', toString(v), sep=''))
          listS <- c(listS, paste(b2, sc, toString(c), toString(v), '!', sep=''))
          listS <- c(listS, paste(b2, sc, toString(c), ':', toString(v), '!', sep=''))
          listS <- c(listS, paste(tolower(b2), sc, toString(c), toString(v), sep=''))
          listS <- c(listS, paste(tolower(b2), sc, toString(c), ':', toString(v), sep=''))
          listS <- c(listS, paste(tolower(b2), sc, toString(c), toString(v), '!', sep=''))
          listS <- c(listS, paste(tolower(b2), sc, toString(c), ':', toString(v), '!', sep=''))
          listS <- c(listS, paste(toupper(b2), sc, toString(c), toString(v), sep=''))
          listS <- c(listS, paste(toupper(b2), sc, toString(c), ':', toString(v), sep=''))
          listS <- c(listS, paste(toupper(b2), sc, toString(c), toString(v), '!', sep=''))
          listS <- c(listS, paste(toupper(b2), sc, toString(c), ':', toString(v), '!', sep=''))
        }
      }
      looptimes <- c(looptimes, as.double(proc.time() - ptm)[3])
      print(paste0(b,toString(c),':',toString(v), ", Average per verse is ", round(mean(looptimes), digits = 3), "secs."))
      list <- unique(list)
      write.table(data.table(numlist), paste('./Lists/NumbersOnly.txt', sep=""), col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      write.table(data.table(list), fn, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      write.table(data.table(listS), paste('./Lists/SpecialMaster.txt', sep=""), col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
      list <- c()
      listS <- c()
      numlist <- c()
    }
  }
}
master <- c()
for (i in 1:length(books)) {
  b <- books[i]
  setb <- rawb[rawb$book == b,]
  b <- gsub(' ', '', b)
  fn <- paste('./Lists/', b, ".txt", sep="")
  master <- c(master, read.table(fn, header = FALSE, stringsAsFactors = FALSE))
  print(paste0('Read ',b))
}
write.table(data.table(unique(unlist(read.table(paste('./Lists/NumbersOnly.txt', sep=""), header = FALSE, stringsAsFactors = FALSE), use.names = FALSE))), paste('./Lists/NumbersOnly.txt', sep=""), col.names = FALSE, row.names = FALSE, quote = FALSE)
master <- unlist(master, use.names = FALSE)
list2 <- sort(unique(list2))
write.table(data.table(list2), paste('./Lists/Jesus.txt', sep=""), col.names = FALSE, row.names = FALSE, quote = FALSE)
master <- c(master, unlist(read.table(paste('./Lists/SpecialMaster.txt', sep=""), header = FALSE, stringsAsFactors = FALSE), use.names = FALSE), unlist(read.table(paste('./Lists/Years.txt', sep=""), header = FALSE, stringsAsFactors = FALSE), use.names = FALSE), unlist(read.table(paste('./Lists/NumbersOnly.txt', sep=""), header = FALSE, stringsAsFactors = FALSE), use.names = FALSE), list2)
master <- unique(master)
write.table(data.table(master), paste('./Master.txt', sep=""), col.names = FALSE, row.names = FALSE, quote = FALSE)