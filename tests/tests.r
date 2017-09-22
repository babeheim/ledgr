
library(devtools)

devtools::load_all()

ledgr::create_books('./test-books')

setwd('./test-books')

# make some dummy accounts
date_shifts <- data.frame(tid=character(), 
  original_date=character(), presentation_date=character())
exchange_rates <- data.frame(date=character(), 
  denominator=character(), numerator=character(), price=character())
general_ledger <- data.frame(date=character(), 
  amount=character(),  tag=character(), notes=character(), 
  account=character(), currency=character(), checksum=character(), 
  tid=character(), balance=character())
d <- general_ledger
n_trans <- 100
add <- matrix(NA, nrow=n_trans, ncol=ncol(d))
colnames(add) <- colnames(d)
d <- rbind(d, add)
d$date <- as.Date(as.numeric(as.Date("2000-01-01")) + round(rnorm(n_trans, 0, 100)), origin="1970-01-01")
account_list <- id_maker(5, nchar=3)
tag_list <- id_maker(20, nchar=4)
currency_list <- c("usd", "eur", "yen")
d$account <- sample(account_list, nrow(d), prob=c(1:5), replace=TRUE)
d$tag <- sample(tag_list, nrow(d), prob=c(1:20), replace=TRUE)
d$amount <- round(rnorm(nrow(d), 0, 20), 2)
d$currency <- sample(currency_list, nrow(d), prob=c(0.9, 0.05, 0.05), replace=TRUE) # all unique
o <- order(d$date)
d <- d[o,]
d$tid <- id_maker(nrow(d), nchar=5)
write.csv(d, './csv/general_ledger.csv', row.names=FALSE)

ledgr::summarize_accounts()

ledgr::balance_books()

ledgr::seperate_accounts()

ledgr::merge_accounts()



# import new


dir_init("./primary_sources")



# primary_sources

input_file <- data.frame(date=character(), 
  amount=character(),  tag=character(), notes=character(), 
  account=character(), currency=character(), checksum=character(), 
  tid=character(), balance=character())
d <- input_file
n_trans <- 20
add <- matrix(NA, nrow=n_trans, ncol=ncol(d))
colnames(add) <- colnames(d)
d <- rbind(d, add)
d$date <- as.Date(as.numeric(as.Date("2001-01-01")) + round(rnorm(n_trans, 0, 100)), origin="1970-01-01")
tag_list <- id_maker(20, nchar=4)
currency_list <- c("usd", "eur", "yen")
d$account <- "input_test"
d$tag <- sample(tag_list, nrow(d), prob=c(1:20), replace=TRUE)
d$amount <- round(rnorm(nrow(d), 0, 20), 2)
d$currency <- sample(currency_list, nrow(d), prob=c(0.9, 0.05, 0.05), replace=TRUE) # all unique
o <- order(d$date)
d <- d[o,]
d$tid <- NA
write.csv(d, './primary_sources/input_test.csv', row.names=FALSE)



ledgr::absorb_entries()

ledgr::summarize_accounts()

ledgr::seperate_accounts() # note: currently requires an entry in the accounts list first!
# good design IMO b/c wont make a random file with bad account name

ledgr::prepare_journal()


# two tasks
# - calculate balances at a given moment, for a given account or set of accounts
# - calculate flows between two moments in time, for a given account or set of accounts
# then scale those operations up to produce all desired reports, etc.

# now use this for two common operations:
# 1. summarize all income and expenses accounts, yearly, quarterly, monthly, with different sublevel resolutions
# 2. summarize the ending balances for all accounts, yearly, quarterly, monthly, with different sublevel resolutions

# output should be:
# 1. csv tables
# 2. latex
# 3. csv tables bound together into an XLS file
# 4. some kind of rmarkdown script?


d <- read.csv('./csv/journal.csv', stringsAsFactors=FALSE)

# time_target <- rev(sort(unique(d$ym)))[1]
time_target <- '2000'

# account_target <- c('expenses', 'income')
# net_balance_tx <- journal_tabulator(d, account_target, time_target, as.tex=TRUE)
# writeLines(net_balance_tx, './temp/net_balance_week.txt')

d$ym <- substr(d$date, 1, 7)
d$y <- substr(d$date, 1, 4)
d$week <- floor((as.numeric(substr(d$date, 9, 10)) / 7)) + 1

d$account1 <- colon_reducer(d$account,1)
d$account2 <- colon_reducer(d$account,2)
d$account3 <- colon_reducer(d$account,3)

account_target <- unique(d$account)
net_flow_tx <- journal_tabulator(d, account_target, time_target, as.tex=TRUE)

d <- exchanger(d, xe, 'eur')

tar <- which(d$tid %in% dateshift$tid)
if(length(tar)>0) d$date[tar] <- dateshift$presentation_date[match(d$tid[tar], dateshift$tid)]

# drop <- which(d$date < '2015-06-01')
# d <- d[-drop,] # the beginning of time?

d$amount <- round(d$amount, 2)
