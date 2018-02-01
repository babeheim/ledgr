

init_workbook <- function(){
  
  dates <- data.frame(tid=character(), 
    original_date=character(), presentation_date=character())

  exchange <- data.frame(date=character(), 
    denominator=character(), numerator=character(), 
    price=character())

  ledger <- data.frame(date=character(), 
    amount=character(),  tag=character(), 
    notes=character(), account=character(), 
    currency=character(),  checksum=character(), 
    tid=character(), balance=character())

  output <- list(ledger, exchange, dates)
  names(output) <- c("ledger", "exchange", "dates")
  return(output)

}

load_workbook <- function(path){

  files <- list.files( file.path(path, "csv") )
  if( ! all( c("date_shifts.csv", "general_ledger.csv", 
    "exchange_rates.csv") %in% files )){
    stop("not a valid workbook, check that it has the three necessary files!")
  }
  wb <- list()
  wb$ledger <- read.csv( file.path(path, "csv", "general_ledger.csv"), stringsAsFactors=FALSE)
  wb$exchange <- read.csv( file.path(path, "csv", "exchange_rates.csv"), stringsAsFactors=FALSE)
  wb$dates <- read.csv( file.path(path, "csv", "date_shifts.csv"), stringsAsFactors=FALSE)
  if("journal.csv" %in% files) wb$journal <- read.csv( file.path(path, "csv", "journal.csv"), stringsAsFactors=FALSE)
  if("accounts.csv" %in% files) wb$accounts <- read.csv( file.path(path, "csv", "accounts.csv"), stringsAsFactors=FALSE)
  
  wb$ledger$date <- fix_dates(wb$ledger$date)
  wb$exchange$date <- fix_dates(wb$exchange$date)

  return(wb)

}