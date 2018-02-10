
library(devtools)
devtools::load_all()
# install_github("babeheim/ledgr")

library(ledgr)

dir_init('./missing_tags')
setwd('./missing_tags')

wb <- ledgr::init_workbook()

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
currency_list <- c("usd", "eur")
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

exchange_rates <- data.frame(date=character(), 
  denominator=character(), numerator=character(), price=character())

ex_dates <- c("1999-01-01", "2000-01-01", "2001-01-01")
denom <- c("eur", "eur", "eur")
num <- c("usd", "usd", "usd")
price <- c("110", "130", "150")

ex <- data.frame(date=ex_dates, denominator=denom, numerator=num, price=price)

wb$exchange <- ex

#####

dir_init('./csv')
save_workbook(wb)


###################

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
currency_list <- c("usd", "eur")
d$account <- "assets:roth_IRA"
d$tag <- sample(tag_list, nrow(d), prob=c(1:length(tag_list)), replace=TRUE)
bad <- sample(nrow(d), 10)
d$tag[bad] <- NA
d$amount <- round(rnorm(nrow(d), 0, 20), 2)
d$currency <- sample(currency_list, nrow(d), prob=c(0.9, 0.1), replace=TRUE) # all unique


o <- order(d$date)
d <- d[o,]
d$tid <- NA

my_filename <- id_maker(1, nchar=5)
my_filename <- paste0(my_filename, ".csv")

write.csv(d, file.path('./primary_sources', my_filename), row.names=FALSE)



##############


wb <- load_workbook(".")

ex <- ledgr::format_exchange_rates(ex)

wb$exchange <- ledgr::format_exchange_rates(ex)



# test that: new entries with no tag are ignored with warning 

# im really gonna need a timestamp...hmmm...

test_that("test entries are absorbed with issues", {

  d <- wb$ledger

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

  expect_warning(test <- ledgr::absorb_entries(d, add))

})

test_that("accounts balance", {
  wb$accounts <- ledgr::summarize_accounts(wb) 
  test <- ledgr::balance_accounts(wb)
  expect_true(abs(sum(test$amount)) < 10)
})


test_that("balancing fails with missing tags balance", {
  wb$accounts <- ledgr::summarize_accounts(wb) 
  bad <- sample(nrow(wb$ledger), 10)
  wb$ledger$tag[bad] <- NA
  expect_error(test <- ledgr::balance_accounts(wb))
})


setwd('..')








