
# library(devtools)
# devtools::load_all()
# install_github("babeheim/ledgr")

dir_init('./simple_test')
setwd('./simple_test')

wb <- init_workbook()

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
my_dates <- as.character(as.Date(seq(7305, 14610, by = 50), origin = "1970-01-01"))
exchange_rates <- data.frame(
  date=my_dates,
  denominator="eur",
  numerator="usd",
  price=rnorm(length(my_dates), 130, 10)
)
wb$exchange <- exchange_rates

save_workbook(wb)



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
d$amount <- round(rnorm(nrow(d), 0, 20), 2)
d$currency <- sample(currency_list, nrow(d), prob=c(0.9, 0.1), replace=TRUE) # all unique


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

d$currency <- c("eur", "usd", "eur", "usd", "eur", "usd")

d$account <- c("assets:deutsche_bank", "assets:petty_cash", "assets:deutsche_bank",
 "assets:petty_cash", "assets:deutsche_bank", "assets:c1_360")
d$tag <- c("assets:petty_cash", "assets:deutsche_bank",
 "assets:petty_cash", "assets:deutsche_bank", "assets:c1_360", "assets:deutsche_bank")

d$amount <- c(-100, 100*1.1, -100, 100*1.05, 100, -100*1.23)

o <- order(d$date)
d <- d[o,]

d$tid <- rep(c("test_one", "test_two", "test_three"), each=2)


my_filename <- id_maker(1, nchar=5)
my_filename <- paste0(my_filename, ".csv")

write.csv(d, file.path('./primary_sources', my_filename), row.names=FALSE)






##############


wb <- load_workbook(".")


test_that("fix_dates works", {

  d <- wb$ledger
  ds <- wb$dates

  x <- as.Date(fix_dates(ds$original_date))  
  expect_true( class(x)=="Date" )
  expect_true( sum(is.na(x))==0 )

  x <- as.Date(fix_dates(ds$presentation_date))
  expect_true( class(x)=="Date" )
  expect_true( sum(is.na(x))==0 )

  x <- as.Date(fix_dates(d$date))
  expect_true( class(x)=="Date" )
  expect_true( sum(is.na(x))==0 )

})



test_that("shift_dates works", {

  d <- wb$ledger
  ds <- wb$dates

  d <- shift_dates(d, ds)

  expect_true(all(ds$presentation_date==d$date[match(ds$tid, d$tid)]))

})



test_that("exchange rates go both directions once formatted", {
  xe <- format_exchange_rates(wb$exchange)
  expect_true(all(table(xe$date)==2))
})



test_that("exchange rates work once formatted", {

  d <- wb$ledger
  xe <- format_exchange_rates(wb$exchange)

  d <- exchange(d, xe, "eur") 
  # needs more testing....

  expect_true( length(unique(d$currency))==2 )

  d <- wb$ledger
  xe <- format_exchange_rates(wb$exchange)

  d <- exchange(d, xe, "usd") 
  # needs more testing....

  expect_true( length(unique(d$currency))==2 )

})

wb$exchange <- format_exchange_rates(wb$exchange)



# test that: new entries with no tag are ignored with warning 

# im really gonna need a timestamp...hmmm...

test_that("test entries are absorbed", {

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

  test <- absorb_entries(wb, add)
  expect_true(all(c("test_one", "test_two", "test_three") %in% test$tid))

})






test_that("accounts balance", {
  wb$accounts <- summarize_accounts(wb) 
  test <- balance_accounts(wb)
  expect_true(abs(sum(test$amount)) < 10)
})





# every function has to have a path...or it works with the files in memory perhaps?

# generate some multi=currency transactions with a fixed ratio
# that goes into the time no problemo

# note: currently requires an entry in the accounts list first!
# good design IMO b/c wont make a random file with bad account name


wb$accounts <- summarize_accounts(wb) 
wb$journal <- prepare_journal(wb)

save_workbook(wb, format = "csv")
save_workbook(wb, format = "yaml")
prepare_reports(wb)

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








