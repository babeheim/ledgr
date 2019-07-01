


adjust_tags <- function (patterns, tag, ledger) {
  hits <- intersect(
    unlist(lapply(patterns, function(z) grep(z, ledger$notes))),
    which(ledger$tag == "expenses:misc")
  )
  if(length(hits) > 0) {
    ledger$tag[hits] <- my_tag
    hit_doubles <- which(ledger$account == "expenses:misc" & ledger$tid %in% ledger$tid[hits])
    if(length(hit_doubles) > 0) ledger$account[hit_doubles] <- my_tag
    print(paste(length(hits), "postings updated"))
  } else {
    print("no postings match these patterns")
  }
  return(ledger)
}


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

save_workbook <- function(wb, format = "csv"){

  if (format == "csv") {
    dir_init("./csv")
    write.csv(wb$ledger, "./csv/general_ledger.csv", row.names=FALSE)
    write.csv(wb$dates, "./csv/date_shifts.csv", row.names=FALSE)
    write.csv(wb$exchange, "./csv/exchange_rates.csv", row.names=FALSE)
    if("journal" %in% names(wb)) write.csv(wb$journal, "./csv/journal.csv", row.names=FALSE)
    if("accounts" %in% names(wb)) write.csv(wb$accounts, "./csv/accounts.csv", row.names=FALSE)
  }

  if (format == "yaml") {
    dir_init("./yaml")
    extract_journal(wb$ledger, "./yaml/general_ledger.yaml")
    extract_dateshifts(wb$dates, "./yaml/date_shifts.yaml")
    extract_exchanges(wb$exchange, "./yaml/exchange_rates.yaml")
    if("accounts" %in% names(wb)) extract_accounts(wb$accounts, "./yaml/accounts.yaml")
  }

  return("workbook saved to disk")

}


audit_accounts <- function(wb){

  a <- wb$accounts
  d <- wb$ledger

  if(any(is.na(d$currency))) stop("some currencies are missing")
  if(any(is.na(as.numeric(d$amount)))) stop("some amounts are missing")
  if(any(is.na(d$date))) stop("some amounts are missing")
  if(any(is.na(d$tag))) stop("some tags are missing")
  if(any(is.na(d$account))) stop("some accounts are missing")
  
# looks through the accounts for trouble:
# - checksums
# - balances in assets go into negatives, imporrible
# - balances in liabilities go into positives, impossible
# - unbalanced transactions
# - accounts that aren't in the accounts.csv file (or are NA)
# - currency cannot be NA
# - date cannot be NA or an invalid Date (turns NA if as.Date'd)
# - tag and account cannot be NA

  output <- list()

  output$n_bad_dates <- sum(is.na(as.Date(d$date)))
  
  output$n_bad_amounts <- sum(is.na(as.numeric(d$amount)))
  
  account_list <- sort(unique(c(d$account, d$tag)))

  output$n_bad_accounts <- sum(!account_list %in% a$account)
  
  xe <- wb$exchange
  d <- exchange(d, xe, "eur")

  trans_check <- abs(tapply(d$amount, d$tid, sum))

  output$n_bad_transactions <- sum( trans_check > 2 )

  if(output$n_bad_transactions>0) output$bad_transactions <- d[which(d$tid %in% names(trans_check[trans_check>2])),]

  return(output)

}


prepare_reports <- function(wb, account_depth = 2, currency="eur"){

  if(!"accounts" %in% names(wb)) stop("accounts table has not been created; run summarize_accounts first")
  if(!"journal" %in% names(wb)) stop("journal table has not been created; run prepare_journal first")

  a <- wb$accounts
  d <- wb$journal

  dateshift <- wb$dates

  d <- shift_dates(d, dateshift)

  d$account <- truncate_accounts(d$account, level=account_depth)
  a$account <- truncate_accounts(a$account, level=account_depth)

  drop <- which(duplicated(a$account))
  if(length(drop)>0) a <- a[-drop,]

  # convert currencies
  xe <- wb$exchange
  d <- exchange(d, xe, currency)

  drop <- which(is.na(d$date))
  if(length(drop)>0) d <- d[-drop,]

  dir_init("./reports", overwrite=FALSE)

  # inverse values

  d$amount <- (-1)*d$amount
 
  # income expense

  min_year <- as.numeric(substr(min(d$date), 1, 4))
  max_year <- as.numeric(substr(max(d$date), 1, 4))

  year_range <- min_year : max_year

  target_accounts <- unique(a$account[grep("^expenses|^income", a$account)])

  ## annual

  out <- matrix(NA, nrow=length(target_accounts), ncol=length(year_range))

  colnames(out) <- year_range
  rownames(out) <- target_accounts

  for(i in 1:length(target_accounts)){
    for(j in 1:length(year_range)){
      start_date <- paste0(year_range[j], "-01-01")
      stop_date <- paste0(year_range[j], "-12-31")
      out[i,j] <- sum_amounts(journal=d, start_date=start_date, stop_date=stop_date, 
        accounts=target_accounts[i])
    }
  }
  
  write.csv(out, "./reports/income_expense_annual.csv", row.names=TRUE) 
  # if it ever gets to the point i want to break apart by decade, revisit this code

  ## months, broken up by years

  for(i in 1:length(year_range)){
    out <- matrix(NA, nrow=length(target_accounts), ncol=12)
    colnames(out) <- c("January", "February", "March", "April", "May", 
        "June", "July", "August", "September", "October", "November", "December")
    rownames(out) <- target_accounts
    for(month in 1:12){
      first_day <- paste(year_range[i], sprintf("%02d", month), "01", sep="-")

      if(month==12){
        last_day <- paste(year_range[i], sprintf("%02d", month), "31", sep="-")
      } else {
        last_day <- as.Date(paste(year_range[i], sprintf("%02d", month+1), "01", sep="-"))-1
      }

      for(j in 1:length(target_accounts)){
        out[j,month] <- sum_amounts(journal=d, start_date=first_day, stop_date=last_day, 
          accounts=target_accounts[j])
      }
    }
    my_name <- paste0("./reports/income_expenses_monthly_", year_range[i], ".csv")
    write.csv(out, my_name, row.names=TRUE)
  }

  # inverse values

  d$amount <- (-1)*d$amount

  # account_balances

  min_year <- as.numeric(substr(min(d$date), 1, 4))
  max_year <- as.numeric(substr(max(d$date), 1, 4))

  year_range <- min_year : max_year

  target_accounts <- a$account[grep("^assets|^liabilities|^equity", a$account)]

  ## annual

  out <- matrix(NA, nrow=length(target_accounts), ncol=length(year_range))

  colnames(out) <- year_range
  rownames(out) <- target_accounts

  for(i in 1:length(target_accounts)){
    for(j in 1:length(year_range)){
      stop_date <- paste0(year_range[j], "-12-31")
      out[i,j] <- sum_amounts(journal=d, stop_date=stop_date, 
        accounts=target_accounts[i])
    }
  }
  
  write.csv(out, "./reports/account_balances_annual.csv", row.names=TRUE) 
  # if it ever gets to the point i want to break apart by decade, revisit this code

  ## months, broken up by years

  for(i in 1:length(year_range)){
    out <- matrix(NA, nrow=length(target_accounts), ncol=12)
    colnames(out) <- c("January", "February", "March", "April", "May", 
        "June", "July", "August", "September", "October", "November", "December")
    rownames(out) <- target_accounts
    for(month in 1:12){

      if(month==12){
        last_day <- paste(year_range[i], sprintf("%02d", month), "31", sep="-")
      } else {
        last_day <- as.Date(paste(year_range[i], sprintf("%02d", month+1), "01", sep="-"))-1
      }

      for(j in 1:length(target_accounts)){
        out[j,month] <- sum_amounts(journal=d, stop_date=last_day, 
          accounts=target_accounts[j])
      }
    }
    my_name <- paste0("./reports/account_balances_monthly_", year_range[i], ".csv")
    write.csv(out, my_name, row.names=TRUE)
  }

  # having populated the reports/ folder with csv's, now we create an xlsx
  prepare_excel(wb)

}


absorb_entries <- function (wb, add) {
    if (class(wb) != "list" | !"ledger" %in% names(wb)) 
        stop("first argument is not a valid ledgr workbook")
    d <- wb$ledger
    drop <- which(is.na(add$tag) | is.na(add$date) | is.na(add$amount) | 
        is.na(add$account))
    if (length(drop) > 0) {
        warning(paste0(length(drop), " records were ignored due to incomplete information (e.g. tagging)"))
        add <- add[-drop, ]
    }
    ledger_key <- paste(d$date, d$amount, d$account)
    add_key <- paste(add$date, add$amount, add$account)
    keep <- which(!add_key %in% ledger_key)
    if (length(keep) > 0) {
        d <- rbind(d, add[keep, ])
        o <- order(d$date)
        d <- d[o, ]
    }
    return(d)
}


prepare_journal <- function(wb){

  d <- wb$ledger

  d$date <- fix_dates(d$date)
  d$date <- as.character(d$date)

  drop <- which(d$tid=="" | is.na(d$tid))
  if(length(drop)>0) d <- d[-drop,]

  drop <- which(colnames(d) %in% c('tag', 'checksum'))
  if(length(drop)>0) d <- d[,-drop]

  tid_list <- unique(d$tid)
  tid_date <- d$date[match(tid_list, d$tid)]

  treg <- data.frame(tid=tid_list, date=tid_date)
  o <- rev(order(treg$date))
  treg <- treg[o,]

  # now order the entries in d by the order here!
  o <- order(match(d$tid, treg$tid))
  d <- d[o,]

  return(d)

}


balance_accounts <- function(wb){

  d <- wb$ledger

  if(!"accounts" %in% names(wb)) stop("no accounts table in the workbook")
  a <- wb$accounts

  d$date <- fix_dates(d$date)

  if(any(is.na(d$tag) | is.na(d$account))) stop("some accounts or tags are NA")
  
  tar <- which(is.na(d$tid))
  if(length(tar)>0) d$tid[tar] <- id_maker(length(tar), nchar=5, reserved=d$tid)

  transaction_list <- sort(unique(d$tid))

  transaction_balance <- rep(NA, length(transaction_list))
  transaction_posts <- rep(NA, length(transaction_list))
  valid_tags <- rep(FALSE, length(transaction_list))
  
  for(i in 1:length(transaction_list)){

    tid_rows <- which(d$tid==transaction_list[i])

    transaction_posts[i] <- length(tid_rows)
    transaction_balance[i] <- sum(d$amount[tid_rows])

    valid_tags[i] <- all(d$tag[tid_rows] %in% a$account)
    # excludes multi-account tags, e.g. 'misc'

    # have to convert into a single currency first to confirm balances
    # if it doesn't balance, it might be b/c....

  }

  # mirror_these <- transaction_list[which( abs(transaction_balance) > 0.05 & valid_tags )]
  mirror_these <- transaction_list[which( transaction_posts==1 & valid_tags )]

  if(length(mirror_these)>0){
     
    tar <- which(d$tid %in% mirror_these)

    add <- d[tar,]

    add$tag <- d$account[tar]
    add$account <- d$tag[tar]
    add$amount <- (-1)*add$amount

    d <- rbind(d, add)

  }
  # order ledger by transaction dates

  tid_list <- unique(d$tid)
  tid_date <- d$date[match(tid_list, d$tid)]
  treg <- data.frame(tid=tid_list, date=tid_date)
  o <- rev(order(treg$date))
  treg <- treg[o,]
  o <- order(match(d$tid, treg$tid))
  d <- d[o,]

  return(d)

}


summarize_accounts <- function (wb, currency = "eur") 
{
    d <- wb$ledger
    xe <- wb$exchange

    d <- exchange(d, xe, currency)

    d$date <- fix_dates(d$date)
    account_list <- sort(unique(c(d$account, d$tag)))
    d$amount <- as.numeric(d$amount)
    n_postings <- rep(NA, length(account_list))
    start_date <- rep(NA, length(account_list))
    last_date <- rep(NA, length(account_list))
    net_flow <- rep(NA, length(account_list))
    n_checksums <- rep(NA, length(account_list))
    for (i in 1:length(account_list)) {
        involved <- which(d$account == account_list[i] | d$tag == 
            account_list[i])
        n_postings[i] <- length(involved)
        start_date[i] <- as.character(min(d$date[involved]))
        last_date[i] <- as.character(max(d$date[involved]))
        net_flow[i] <- round(sum(d$amount[involved]), 2)
        n_checksums[i] <- sum(!is.na(d$checksum[d$account == 
            account_list[i]]))
    }
    output <- data.frame(account = account_list, n_postings, 
        start_date, last_date, net_flow, currency, n_checksums)
    return(output)
}


format_exchange_rates <- function(xe){
 # takes every date-numerator-denominator pair, inverts price and switches drop dpulicates
  if(nrow(xe) > 0){

    xe$date <- fix_dates(xe$date)
    xe$price <- as.numeric(xe$price)

    xe2 <- xe
    xe2$denominator <- xe$numerator
    xe2$numerator <- xe$denominator
    xe2$price <- 1/xe$price

    xe <- rbind(xe, xe2)

    xe$price <- round(xe$price, 4)

    o <- rev(order(xe$date))
    xe <- xe[o,]

    drop <- which(duplicated(xe[,c("denominator", "numerator", "date")]))
    if(length(drop)>0) xe <- xe[-drop,]

    return(xe)

  }

}
