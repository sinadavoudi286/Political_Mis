library(RSQLite)
library(sqldf)
library(data.table)

##set path to store sqlite database| You will need to create directory if it
##doesn't exist.
path <- '~/dime_sqlite/'

con <- dbConnect(RSQLite::SQLite(), paste0(path,"dime_v3.sqlite3"))

dbListTables(con)

cand.vars <- dbGetQuery(con,"select * from candDB limit 10")
print(cand.vars)

dbGetQuery(con,"select * from itemized_conts limit 10")


##tasks... 
##1) calculate percentage of money coming form each congressional district in 2016; do this in R and SQL. 

##2) Identify specific candidate and calculate total funds raised. 



##unzip file | This will only work on Mac and Linux machines. You may need to unzip it manually on Windows.
if(!file.exists('dime_1979_2014.sqlite3.gz')){
    system('gunzip dime_1979_2014.gz')
}

##Connecting to SQLite DB
con <- dbConnect(RSQLite::SQLite(), paste0(path,"dime.sqlite3"))

##list tables
dbListTables(con)

##show variables in candDB
cand.vars <- dbGetQuery(con,"select * from candDB limit 10")
print(cand.vars)


##show variables in contribDB
contrib.vars <- dbGetQuery(con,"select * from contribDB limit 10")
print(contrib.vars)


##show variables in donorDB
disb.vars <- dbGetQuery(con,"select * from donorDB limit 10")
print(disb.vars)


###############################################################################
##Example queries | recipient file
###############################################################################

##pull all california cands
ca.cands <- dbGetQuery(con,"select * from candDB where state ='CA'")

##pull all california cands running in 2020
ca.cands.2014 <- dbGetQuery(con,"select * from candDB where state ='CA' and cycle == 2020")

##pull house california cands running in 2020
ca.cands.2014.house <- dbGetQuery(con,"select * from candDB where state ='CA' and cycle == 2020 and seat == 'federal:house'")

###############################################################################
##Example queries | contrib file
###############################################################################
by.last.and.first.name <- dbGetQuery(con,"select * from contribDB where contributor_lname = 'trump' and contributor_fname ='donald'")

by.cid <- dbGetQuery(con,"select * from contribDB where bonica_cid = 3110056714")

by.gender <- dbGetQuery(con,"select contributor_gender, amount, bonica_cid, recipient_name from contribDB where cycle == 2020 and contributor_gender= 'F'")




