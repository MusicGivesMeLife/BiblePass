# BiblePass
Generates wordlists of possible passwords based off of Bible verses (e.g. john316, Romans8:28, Ephesians2:8, Revelation@2012, GenesisOneOne, 8:5!, etc.)

## Design
* Generates different variations of book-chapter-verse, Book Capitalized/not capitalized/ALL CAPS
* Generates list of book + year
* Generates different variations of book-separating character-chapter-verse, also C/nc/AC
* Tries singular and plural forms for Revelation/Revelations and Psalm/Psalms
* Uses both numeral and word forms for chapter/verse numbers

## Input
* ./bibleTaxonomy.csv - CSV list of every book-chapter-verse of the bible in order

## Output
* ./Lists/`book of the Bible`.txt - Simple wordlist from each book of the Bible
* ./Lists/Jesus.txt - Variations C/nc/AC of "jesus" combined with bible chapter/verse numbers
* ./Lists/Years.txt - Books of the Bible + years (1800-2200 and 00-99) w/ and wo/ separating characters
* ./Lists/SpecialMaster.txt - Master list of all books of the Bible with special characters
* ./Lists/NumbersOnly.txt - Compilation of chapter and verse numbers without books
* ./Master.txt - All of the above
