
library(devtools)

devtools::load_all()

# install_github("babeheim/ledgr")

# ledgr::create_books('./simple_test')

setwd('./tests')
setwd("./bret_books")



##############

  
test_that("fix_dates works", {

  d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=FALSE)
  ds <- read.csv('./csv/date_shifts.csv', stringsAsFactors=FALSE)

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

  d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=FALSE)
  ds <- read.csv('./csv/date_shifts.csv', stringsAsFactors=FALSE)

  ds$original_date <- as.Date(fix_dates(ds$original_date))  
  ds$presentation_date <- as.Date(fix_dates(ds$presentation_date))  

  d <- shift_dates(d, ds)

  expect_true(all(ds$presentation_date==d$date[match(ds$tid, d$tid)]))

})




ledgr::format_exchange_rates() # bug?

test_that("exchange rates go both directions", {
  ex <- read.csv('./csv/exchange_rates.csv', stringsAsFactors=FALSE)
  expect_true(all(table(ex$date) %% 2 == 0))
})



test_that("exchange rates work once formatted", {

  d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=FALSE)
  xe <- read.csv('./csv/exchange_rates.csv', stringsAsFactors=FALSE)

  d <- exchange(d, xe, "eur") 
  # needs more testing....

  expect_true( length(unique(d$currency))==2 )

  d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=FALSE)
  xe <- read.csv('./csv/exchange_rates.csv', stringsAsFactors=FALSE)

  d <- exchange(d, xe, "usd") 
  # needs more testing....

  expect_true( length(unique(d$currency))==2 )

})




ledgr::absorb_entries()
# im really gonna need a timestamp...hmmm...

ledgr::summarize_accounts() 
# wanring!

ledgr::audit_accounts()


ledgr::balance_accounts()

test_that("accounts balance", {
  d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=FALSE)
  expect_true(abs(sum(d$amount)) < 10)
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

# oh here's a bug: it will print the accounts without the truncation
# as rows...need to simplify!
# 

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








