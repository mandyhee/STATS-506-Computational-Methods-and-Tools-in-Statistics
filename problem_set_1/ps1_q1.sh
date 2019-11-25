#!/bin/bash
echo stat506 problem set 1 question 1
#How many rows are there for region 3 in the RECS 2015 data set? 2011
cut -d ',' -f 2 recs2015_public_v3.csv | grep 3 | wc -l #2020
#cut: -d: specify delimiter csv = ','; -f 2: read line 2
#grep 3: search for 3
#wc: word count; -l: line

#Write a one-liner to create a compressed data set containing only the variables: DOEID, NWEIGHT, and BRRWT1-BRRWT96.
cut -d',' -f 1,475-571 recs2015_public_v3.csv | gzip > q1_2.gz
#cut -d ',' -f 1,475,476-571 recs2015_public_v3.csv > q1_2.csv && gzip q1_2.csv
#Write a Bash for loop to count and print the number of observations within each region.
for i in 1 2 3 4
  do
    cut -d ',' -f 2 recs2015_public_v3.csv | grep $i | wc -l
  done
     #794
     #1327
     #2010
     #1555

#Produce a file region_division.txt providing a sorted list showing unique combinations of values from REGIONC and DIVISION.
#Include the contents of that file in your solution. Hint: See man uniq.
cut -d ',' -f 2,3 recs2015_public_v3.csv | sort | uniq -c > region_division.txt
