
rm(list=ls())

library(devtools)

devtools::load_all()

# install_github("babeheim/ledgr")

ledgr::create_books('./currency_tests')

setwd("./currency_tests")

library(testthat)

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
d$date <- as.Date(as.numeric(as.Date("2000-01-01")) + round(rnorm(n_trans, 0, 1000)), origin="1970-01-01")
account_list <- c("assets:deutsche_bank", "assets:paypal", "liabilities:chase_freedom_cc")
tag_list <- c("expenses:food:restaurant", "expenses:food:groceries", "expenses:rent", "income:mpi:salary", "income:mpi:grants")
currency_list <- c("yen", "eur")
d$account <- sample(account_list, nrow(d), prob=c(1:length(account_list)), replace=TRUE)
d$tag <- sample(tag_list, nrow(d), prob=c(1:length(tag_list)), replace=TRUE)
d$amount <- round(rnorm(nrow(d), 0, 20), 2)
d$currency <- sample(currency_list, nrow(d), prob=c(0.9, 0.1), replace=TRUE) # all unique
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
currency_list <- c("yen", "eur")
d$account <- "assets:roth_IRA"
d$tag <- sample(tag_list, nrow(d), prob=c(1:length(tag_list)), replace=TRUE)
d$amount <- round(rnorm(nrow(d), 0, 20), 2)
d$currency <- sample(currency_list, nrow(d), prob=c(0.1, 0.9), replace=TRUE) # all unique

d$amount[d$currency=="yen"] <- d$amount[d$currency=="yen"]*1000

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
num <- c("yen", "yen", "yen")
price <- c("110", "130", "150")

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

d$currency <- c("eur", "yen", "eur", "yen", "eur", "yen")

d$account <- c("assets:deutsche_bank", "assets:petty_cash", "assets:deutsche_bank",
 "assets:petty_cash", "assets:deutsche_bank", "assets:c1_360")
d$tag <- c("assets:petty_cash", "assets:deutsche_bank",
 "assets:petty_cash", "assets:deutsche_bank", "assets:c1_360", "assets:deutsche_bank")

d$amount <- c(-100, 100*110, -100, 100*105, 100, -100*123)

o <- order(d$date)
d <- d[o,]

d$tid <- rep(c("test_one", "test_two", "test_three"), each=2)

my_filename <- id_maker(1, nchar=5)
my_filename <- paste0(my_filename, ".csv")

write.csv(d, file.path('./primary_sources', my_filename), row.names=FALSE)






##############


ledgr::absorb_entries()
# im really gonna need a timestamp...hmmm...

test_that("test entries are absorbed", {

  d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=TRUE)
  expect_true(all(c("test_one", "test_two", "test_three") %in% d$tid))

})


# unit test: try to exchange currencies that have no pairings
# unit test: try to exchange currencies when last observed price is REALLY FAR AWAY

test_that("exchange rates fail if not formatted", {

  d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=FALSE)
  xe <- read.csv('./csv/exchange_rates.csv', stringsAsFactors=FALSE)

  expect_error(exchange(d, xe, "eur"))

  expect_true( length(unique(d$currency))==2 )

  d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=FALSE)
  xe <- read.csv('./csv/exchange_rates.csv', stringsAsFactors=FALSE)

#   expect_error(exchange(d, xe, "yen"))
  # needs more testing....

})


ledgr::format_exchange_rates()

test_that("exchange rates go both directions", {
  ex <- read.csv('./csv/exchange_rates.csv', stringsAsFactors=FALSE)
  expect_true(all(table(ex$date)==2))
})



test_that("exchange rates work once formatted", {

  d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=FALSE)
  xe <- read.csv('./csv/exchange_rates.csv', stringsAsFactors=FALSE)

  d <- exchange(d, xe, "eur") 
  # needs more testing....

  expect_true( length(unique(d$currency))==2 )
  expect_true( abs(sum(d$amount[d$tid=="test_one"])/d$amount[d$tid=="test_one"][1]) < 0.3 )
  expect_true( abs(sum(d$amount[d$tid=="test_two"])/d$amount[d$tid=="test_two"][1]) < 0.3 )
  expect_true( abs(sum(d$amount[d$tid=="test_three"])/d$amount[d$tid=="test_three"][1]) < 0.3 )

  d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=FALSE)
  xe <- read.csv('./csv/exchange_rates.csv', stringsAsFactors=FALSE)

  d <- exchange(d, xe, "yen") 
  # needs more testing....

  expect_true( length(unique(d$currency))==2 )
  expect_true( abs(sum(d$amount[d$tid=="test_one"])/d$amount[d$tid=="test_one"][1]) < 0.3 )
  expect_true( abs(sum(d$amount[d$tid=="test_two"])/d$amount[d$tid=="test_two"][1]) < 0.3 )
  expect_true( abs(sum(d$amount[d$tid=="test_three"])/d$amount[d$tid=="test_three"][1]) < 0.3 )

})





ledgr::summarize_accounts() 


ledgr::audit_accounts()


ledgr::balance_accounts()

test_that("accounts balance", {
  d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=FALSE)
  xe <- read.csv('./csv/exchange_rates.csv', stringsAsFactors=FALSE)
  d <- exchange(d, xe, 'eur')
  expect_true(abs(sum(d$amount)) < 100) # lame
})


ledgr::seperate_accounts()

test_that("all valid accounts were seperated", {

  d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=FALSE)
  account_list <- sort(unique(c(d$account, d$tag)))
  account_files <- list.files('./csv', pattern="^account_", full.names=TRUE)
  expect_equal( length(account_list), length(account_files) )

})

ledgr::merge_accounts()
# eliminates all NA's..not good!

# ledgr::audit_accounts() 



# every function has to have a path...or it works with the files in memory perhaps?

# generate some multi=currency transactions with a fixed ratio
# that goes into the time no problemo

# note: currently requires an entry in the accounts list first!
# good design IMO b/c wont make a random file with bad account name

ledgr::prepare_journal()

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

# readxl

setwd('..')








