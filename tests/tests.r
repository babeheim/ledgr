
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
n_trans <- 1000
add <- matrix(NA, nrow=n_trans, ncol=ncol(d))
colnames(add) <- colnames(d)
d <- rbind(d, add)
d$date <- as.Date(as.numeric(as.Date("2000-01-01")) + round(rnorm(n_trans, 0, 1000)), origin="1970-01-01")
account_list <- c("assets:deutsche_bank", "assets:paypal", "liabilities:chase_freedom_cc")
tag_list <- c("expenses:food:restaurant", "expenses:food:groceries", "expenses:rent", "income:mpi:salary", "income:mpi:grants")
currency_list <- c("usd", "eur", "yen")
d$account <- sample(account_list, nrow(d), prob=c(1:length(account_list)), replace=TRUE)
d$tag <- sample(tag_list, nrow(d), prob=c(1:length(tag_list)), replace=TRUE)
d$amount <- round(rnorm(nrow(d), 0, 20), 2)
d$currency <- sample(currency_list, nrow(d), prob=c(0.9, 0.05, 0.05), replace=TRUE) # all unique
o <- order(d$date)
d <- d[o,]
d$tid <- id_maker(nrow(d), nchar=5)
write.csv(d, './csv/general_ledger.csv', row.names=FALSE)


dir_init("./primary_sources")

input_file <- data.frame(date=character(), 
  amount=character(),  tag=character(), notes=character(), 
  account=character(), currency=character(), checksum=character(), 
  tid=character(), balance=character())
d <- input_file
n_trans <- 200
add <- matrix(NA, nrow=n_trans, ncol=ncol(d))
colnames(add) <- colnames(d)
d <- rbind(d, add)
d$date <- as.Date(as.numeric(as.Date("2001-01-01")) + round(rnorm(n_trans, 0, 1000)), origin="1970-01-01")
tag_list <- c("expenses:food:restaurant", "expenses:food:groceries", "expenses:rent", "income:mpi:salary", "income:mpi:grants")
currency_list <- c("usd", "eur", "yen")
d$account <- "assets:roth_IRA"
d$tag <- sample(tag_list, nrow(d), prob=c(1:length(tag_list)), replace=TRUE)
d$amount <- round(rnorm(nrow(d), 0, 20), 2)
d$currency <- sample(currency_list, nrow(d), prob=c(0.9, 0.05, 0.05), replace=TRUE) # all unique
o <- order(d$date)
d <- d[o,]
d$tid <- NA

my_filename <- id_maker(1, nchar=5)
my_filename <- paste0(my_filename, ".csv")

write.csv(d, file.path('./primary_sources', my_filename), row.names=FALSE)



# add some dateshifting

d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=FALSE)

shift_tid <- sample(d$tid, 10)
original_date <- d$date[match(shift_tid, d$tid)]
presentation_date <- as.Date(as.numeric(as.Date("2001-01-01")) + round(rnorm(length(original_date), 0, 1000)), origin="1970-01-01")

date_shifts <- data.frame(tid=shift_tid, original_date, presentation_date)
write.csv(date_shifts, "./csv/date_shifts.csv", row.names=FALSE)


# add some currency conversion

exchange_rates <- data.frame(date=character(), 
  denominator=character(), numerator=character(), price=character())

ex_dates <- c("1999-01-01", "2000-01-01", "2001-01-01")
denom <- c("eur", "eur", "eur")
num <- c("usd", "usd", "usd")
price <- c("1.1", "1.3", "1.5")

ex <- data.frame(date=ex_dates, denominator=denom, numerator=num, price=price)

write.csv(ex, "./csv/exchange_rates.csv", row.names=FALSE)



# and some multi-currency transactions!

input_file <- data.frame(date=character(), 
  amount=character(),  tag=character(), notes=character(), 
  account=character(), currency=character(), checksum=character(), 
  tid=character(), balance=character())
d <- input_file
n_trans <- 6
add <- matrix(NA, nrow=n_trans, ncol=ncol(d))
colnames(add) <- colnames(d)
d <- rbind(d, add)
d$date <- rep(c("1999-10-01", "1999-08-15", "2000-08-03"), each=2)

d$currency <- c("eur", "usd", "eur", "usd", "eur", "usd")

d$account <- c("assets:deutsche_bank", "assets:petty_cash", "assets:deutsche_bank",
 "assets:petty_cash", "assets:deutsche_bank", "assets:c1_360")
d$tag <- c("assets:petty_cash", "assets:deutsche_bank",
 "assets:petty_cash", "assets:deutsche_bank", "assets:c1_360", "assets:deutsche_bank")

d$amount <- c(-100, 100*1.12, -100, 100*1.05, 100, -100*1.23)

o <- order(d$date)
d <- d[o,]

d$tid <- rep(c("test_one", "test_two", "test_three"), each=2)

my_filename <- id_maker(1, nchar=5)
my_filename <- paste0(my_filename, ".csv")

write.csv(d, file.path('./primary_sources', my_filename), row.names=FALSE)








ledgr::format_exchange_rates()

ledgr::summarize_accounts() 

ledgr::balance_accounts()

d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=FALSE)
table(d$tid)

ledgr::seperate_accounts()

ledgr::merge_accounts()


# ledgr::audit_accounts() 
# looks through the accounts for trouble:
# - balances in assets go into negatives, imporrible
# - balances in liabilities go into positives, impossible
# - unbalanced transactions
# - accounts that aren't in the accounts.csv file (or are NA)
# - 


# every function has to have a path...or it works with the files in memory perhaps?



# generate some multi=currency transactions with a fixed ratio
# that goes into the time no problemo


ledgr::absorb_entries()
# im really gonna need a timestamp...hmmm...

ledgr::summarize_accounts()

ledgr::seperate_accounts() 

# note: currently requires an entry in the accounts list first!
# good design IMO b/c wont make a random file with bad account name

ledgr::prepare_journal()



d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=FALSE)
table(table(d$tid))


d <- read.csv('./csv/journal.csv', stringsAsFactors=FALSE)
table(table(d$tid))

ledgr::absorb_entries()

ledgr::summarize_accounts()

ledgr::balance_accounts()

ledgr::prepare_journal() 
# journal should drop bad tids


d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=FALSE)
table(table(d$tid))


d <- read.csv('./csv/journal.csv', stringsAsFactors=FALSE)
table(table(d$tid))

 d <- exchange(d, xe, 'eur')

# balance accounts has to take into account currency exchanges first!! thats how it decides there's something missing....


ledgr::prepare_reports()

# add a 'depth' flag so you can reduce subaccounts easily

# also "data" instead of csv for every path

# incorporate date_shift in the report prep
# incorporate currency exchange in report prep

# specify output format for easy access
# 1. csv tables
# 2. latex
# 3. csv tables bound together into an XLS file
# 4. some kind of rmarkdown script?

d <- read.csv('./csv/journal.csv', stringsAsFactors=FALSE)



# drop <- which(d$date < '2015-06-01')
# d <- d[-drop,] # the beginning of time?

d$amount <- round(d$amount, 2)
# this should happen automatically...

# sum_amounts(stop_date="2001-01-01")


# readxl

setwd('..')








