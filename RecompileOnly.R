#!/usr/bin/env Rscript
# Dependencies
library('data.table')
library('english')
library('stringr')
library('stringi')
library('Hmisc')
library('NCmisc')

rm(list=ls())

rawb <- read.csv("./bibletaxonomy.csv", stringsAsFactors = FALSE, header = FALSE, col.names = c('book','chapter','verse'))    #Read main list

books <- unique(rawb$book)

master <- c()
for (i in 1:length(books)) {
  b <- books[i]
  setb <- rawb[rawb$book == b,]
  b <- gsub(' ', '', b)
  fn <- paste('./Lists/', b, ".txt", sep="")
  master <- c(master, read.table(fn, header = FALSE, stringsAsFactors = FALSE))
  print(paste0('Read ',b))
}

master <- unlist(master, use.names = FALSE)

master <- c(master, unlist(read.table(paste('./Lists/SpecialMaster.txt', sep=""), header = FALSE, stringsAsFactors = FALSE), use.names = FALSE), unlist(read.table(paste('./Lists/Years.txt', sep=""), header = FALSE, stringsAsFactors = FALSE), use.names = FALSE), unlist(read.table(paste('./Lists/NumbersOnly.txt', sep=""), header = FALSE, stringsAsFactors = FALSE), use.names = FALSE), unlist(read.table(paste('./Lists/Jesus.txt', sep=""), header = FALSE, stringsAsFactors = FALSE), use.names = FALSE))
master <- unique(master)
write.table(data.table(master), paste('./BiblePass.txt', sep=""), col.names = FALSE, row.names = FALSE, quote = FALSE)
file.split(paste('./BiblePass.txt', sep=""), size=900000, same.dir=TRUE, verbose=FALSE)
