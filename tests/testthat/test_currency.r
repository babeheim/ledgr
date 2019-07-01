
rm(list=ls())

# library(devtools)
# devtools::load_all()
# install_github("babeheim/ledgr")

dir_init('./currency_tests')
setwd('./currency_tests')

wb <- init_workbook()

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

wb$ledger <- d

# add some dateshifting

d <- wb$ledger

shift_tid <- sample(d$tid, 10)
original_date <- d$date[match(shift_tid, d$tid)]
presentation_date <- as.Date(as.numeric(as.Date("2001-01-01")) + round(rnorm(length(original_date), 0, 1000)), origin="1970-01-01")

date_shifts <- data.frame(tid=shift_tid, original_date, presentation_date)
wb$dates <- date_shifts 


# add some currency conversion
my_dates <- as.character(as.Date(seq(7305, 14610, by = 50), origin = "1970-01-01"))
exchange_rates <- data.frame(
  date=my_dates,
  denominator="eur",
  numerator="usd",
  price=rnorm(length(my_dates), 130, 10)
)
wb$exchange <- exchange_rates

#####


dir_init('./csv')
save_workbook(wb)



##################


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

wb <- load_workbook(".")

inputs <- list.files("./primary_sources", pattern="*.csv", full.names=TRUE)

add <- data.frame(date=character(), 
amount=character(),  tag=character(), notes=character(), 
account=character(), currency=character(), checksum=character(), 
tid=character(), balance=character()) 

if(length(inputs)>0){
  for(i in 1:length(inputs)){
    add <- rbind(add, read.csv(inputs[i], stringsAsFactors=FALSE))
  }
}


test_that("test entries are absorbed", {

  test <- absorb_entries(wb, add)
  expect_true(all(c("test_one", "test_two", "test_three") %in% test$tid))

})

wb$ledger <- absorb_entries(wb, add)


# unit test: try to exchange currencies that have no pairings
# unit test: try to exchange currencies when last observed price is REALLY FAR AWAY

test_that("exchange rates fail if not formatted", {

  d <- wb$ledger
  xe <- wb$exchange

  expect_error(exchange(d, xe, "eur"))

  expect_true( length(unique(d$currency))==2 )

  # d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=FALSE)
  # xe <- read.csv('./csv/exchange_rates.csv', stringsAsFactors=FALSE)

#   expect_error(exchange(d, xe, "yen"))
  # needs more testing....

})


wb$exchange <- format_exchange_rates(wb$exchange)

test_that("exchange rates go both directions", {
  ex <- wb$exchange
  expect_true(all(table(ex$date)==2))
})

test_that("exchange rates work once formatted", {

  d <- wb$ledger
  xe <- wb$exchange

  d <- exchange(d, xe, "eur") 
  # needs more testing....

  expect_true( length(unique(d$currency))==2 )
  expect_true( abs(sum(d$amount[d$tid=="test_one"])/d$amount[d$tid=="test_one"][1]) < 0.3 )
  expect_true( abs(sum(d$amount[d$tid=="test_two"])/d$amount[d$tid=="test_two"][1]) < 0.3 )
  expect_true( abs(sum(d$amount[d$tid=="test_three"])/d$amount[d$tid=="test_three"][1]) < 0.3 )

  d <- wb$ledger
  xe <- wb$exchange

  d <- exchange(d, xe, "yen") 
  # needs more testing....

  expect_true( length(unique(d$currency))==2 )
  expect_true( abs(sum(d$amount[d$tid=="test_one"])/d$amount[d$tid=="test_one"][1]) < 0.3 )
  expect_true( abs(sum(d$amount[d$tid=="test_two"])/d$amount[d$tid=="test_two"][1]) < 0.3 )
  expect_true( abs(sum(d$amount[d$tid=="test_three"])/d$amount[d$tid=="test_three"][1]) < 0.3 )

})

wb$accounts <- summarize_accounts(wb)

wb$ledger <- balance_accounts(wb)

test_that("accounts balance", {
  d <- wb$ledger
  xe <- wb$exchange
  d <- exchange(d, xe, 'eur')
  expect_true(abs(sum(d$amount)) < 100) # lame
})

wb$accounts <- summarize_accounts(wb) 
wb$journal <- prepare_journal(wb)

save_workbook(wb, format = "yaml")
save_workbook(wb, format = "csv")

prepare_reports(wb)


setwd('..')








