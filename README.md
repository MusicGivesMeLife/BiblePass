# BiblePass
Generates wordlists of possible passwords based off of Bible verses (e.g. john316, Ephesians2:8, Revelation@2012, etc.)

## Design
* Generates different variations of book-chapter-verse, Book Capitalized/not capitalized/ALL CAPS
* Generates list of book + year
* Generates different variations of book-separating character-chapter-verse, also C/nc/AC
* Also tries singular and plural forms for Revelation/Revelations and Psalm/Psalms

## Output
* ./Lists/<book of the Bible>.txt - Simple wordlist from each book of the Bible
* ./Lists/Jesus.txt - Variations C/nc/AC of "jesus" combined with bible chapter/verse numbers
* ./Lists/Years.txt - Books of the Bible + years (1800-2200) w/ and wo/ separating characters
* ./Lists/SpecialMaster.txt - Master list of all books of the Bible with special characters
* ./Master.txt - All of the above