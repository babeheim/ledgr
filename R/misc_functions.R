

truncate_accounts <- function(string, level=NA){
    output <- string
    output[is.na(output)] <- ''
    for(i in 1:length(string)){
        break_pos <- gregexpr(':', output[i])[[1]]
        if(!is.na(break_pos[level]) & break_pos[level]!='-1') output[i] <- substr(output[i], 1, break_pos[level]-1) 
    }
    return(output)
}

prepare_reports <- function(account_depth=2){
  
  if(!file.exists('./csv/accounts.csv')) stop("accounts.csv has not been created; run summarize_accounts first")
  if(!file.exists('./csv/journal.csv')) stop("journal.csv has not been created; run prepare_journal first")
      
  a <- read.csv('./csv/accounts.csv', stringsAsFactors=FALSE)
  d <- read.csv('./csv/journal.csv', stringsAsFactors=FALSE)

  d$account <- truncate_accounts(d$account, level=account_depth)
  a$account <- truncate_accounts(a$account, level=account_depth)

  dir_init("./reports", overwrite=FALSE)

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

}


absorb_entries <- function(){

  d <- read.csv("./csv/general_ledger.csv", stringsAsFactors=FALSE)

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

  # detect if entries are not present in the general ledger, timestamp and amount!
  # do we require that the tags be completed on the primary sources? no!

  # need time here too..
  ledger_key <- paste(d$date, d$amount, d$account)
  add_key <- paste(add$date, add$amount, add$account)

  keep <- which(!add_key %in% ledger_key)

  if(length(keep)>0){

  d <- rbind(d, add[keep,])

  o <- order(d$date)
  d <- d[o,]

  write.csv(d, './csv/general_ledger.csv', row.names=FALSE)

  }

}


# can be used for flows or balances depending on what you pass in
sum_amounts <- function(journal, start_date=NA, stop_date=NA, accounts=NA){
  d <- journal
  if(is.na(start_date)) start_date <- min(d$date)
  if(is.na(stop_date)) stop_date <- max(d$date)
  if(all(is.na(accounts))) accounts <- sort(unique(d$account))
  tar <- which(d$date >= start_date & d$date <= stop_date & d$account %in% accounts)
  output <- 0
  if(length(tar)>0) output <- sum(d$amount[tar])
  output <- round(output, 2)
  return(output)
}



texttab <- function(input.matrix, alignment=NA, hlines=NA, caption="", scale=NA){
  output <- character(nrow(input.matrix))
  for(i in 1:nrow(input.matrix)){
    add.amps <- paste(input.matrix[i,], collapse=" & ")
    output[i] <- paste(add.amps, "\\\\", sep=" ")
  }
  if(all(!is.na(hlines))){
    for(i in 1:length(hlines)) output <- append(output, "\\hline", hlines[i]+(i-1))
  }
  return(output)
}


fix_dates <- function(dates){
  if(length(grep('/', dates))>0){
    last_four <- substr(dates, nchar(dates)-3, nchar(dates))
    lower_y <- grep('/', last_four)
    upper_y <- which(!1:nrow(d) %in% lower_y)
    dates[upper_y] <- as.character(as.Date(dates, '%m/%d/%Y'))
    dates[lower_y] <- as.character(as.Date(dates, '%m/%d/%y'))
  }
  dates <- as.Date(dates)
  return(dates)
}

exchange <- function(journal, prices, currency){
  journal$date <- fix_dates(journal$date)
  prices$date <- fix_dates(prices$date)
  for(i in 1:nrow(journal)){
    if(journal$currency[i]!=currency & journal$currency[i] %in% prices$numerator){
      currency_rows <- which(prices$numerator==journal$currency[i])
      currency_dates <- prices$date[currency_rows]
      diffs <- abs(journal$date[i] - currency_dates)
      best_row <- currency_rows[which.min(diffs)]
      journal$amount[i] <- prices$price[best_row]*journal$amount[i]
      journal$currency[i] <- paste0(currency,'-eqv')
    }
  } # ok good, but what if the numerare isn't in numerare commodity?
  return(journal)
}



prepare_journal <- function(){

  d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=TRUE)

  d$date <- fix_dates(d$date)
  d$date <- as.character(d$date)

  drop <- which(d$tid=='')
  if(length(drop)>0) d <- d[-drop,]

  drop <- which(colnames(d) %in% c('tag', 'checksum'))
  if(length(drop)>0) d <- d[,-drop]

  # d$notes <- substr(1, 50, d$notes)

  tid_list <- unique(d$tid)
  tid_date <- d$date[match(tid_list, d$tid)]

  treg <- data.frame(tid=tid_list, date=tid_date)
  o <- rev(order(treg$date))
  treg <- treg[o,]

  # now order the entries in d by the order here!
  o <- order(match(d$tid, treg$tid))
  d <- d[o,]

  write.csv(d, './csv/journal.csv', row.names=FALSE)

}


balance_accounts <- function(){

  d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=FALSE)
  a <- read.csv('./csv/accounts.csv', stringsAsFactors=FALSE)

  tar <- which(is.na(d$tid))
  if(length(tar)>0) d$tid[tar] <- id_maker(length(tar), nchar=5)

  transaction_list <- sort(unique(d$tid))

  transaction_balance <- rep(NA, length(transaction_list))
  valid_tags <- rep(FALSE, length(transaction_list))
  
  for(i in 1:length(transaction_list)){

  tid_rows <- which(d$tid==transaction_list[i])

  transaction_balance[i] <- sum(d$amount[tid_rows])

  valid_tags[i] <- all(d$tag[tid_rows] %in% a$account)
  # excludes multi-account tags, e.g. 'misc'

  # have to convert into a single currency first to confirm balances
  # if it doesn't balance, it might be b/c....

  }

  mirror_these <- transaction_list[which( abs(transaction_balance) > 0.05 & valid_tags )]

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

  write.csv(d, "./csv/general_ledger.csv", row.names=FALSE)

}


summarize_accounts <- function(){

  d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=FALSE)

  account_list <- sort(unique(c(d$account, d$tag))) # alphabetical order

  n_postings <- rep(NA, length(account_list))
  start_date <- rep(NA, length(account_list))
  last_date <- rep(NA, length(account_list))
  net_flow <- rep(NA, length(account_list))
  n_checksums <- rep(NA, length(account_list))

  for(i in 1:length(account_list)){

  involved <- which(d$account == account_list[i] | d$tag==account_list[i])
  n_postings[i] <- length(involved)
  start_date[i] <- min(d$date[involved])
  last_date[i] <- min(d$date[involved])
  net_flow[i] <- sum(d$amount[involved])
  n_checksums[i] <- sum(!is.na(d$checksum[d$account==account_list[i]]))

  }

  # also count how many other accounts these accounts share a tid with...hmmm

  output <- data.frame(account=account_list, n_postings, start_date, 
  last_date, net_flow, n_checksums)
 
  write.csv(output, "./csv/accounts.csv", row.names=FALSE)

}

seperate_accounts <- function(){

  # key element: only accounts in the 'accounts' column get counted
  # should have a force flag and detects if postings are alreayd in these accounts
  # that are not in the general ledger

  d <- read.csv("./csv/general_ledger.csv", stringsAsFactors=FALSE)
  a <- read.csv("./csv/accounts.csv", stringsAsFactors=FALSE)

  if(!all(d$accounts %in% a$account)) stop("not all ledger accounts are in the account list") # check

  account_names <- paste0("account_", a$account, ".csv")
  account_names <- gsub(":", "_", account_names)
  account_names <- file.path("./csv", account_names)

  for(i in 1:length(account_names)){

  tar <- which(d$account==a$account[i])
  write.csv(d[tar,], account_names[i], row.names=FALSE)

  }

}

merge_accounts <- function(){

  # should have a force flag and detects if postings are in gl not in account ledgers

  account_tables <- list.files("./csv", pattern="^account_", full.names=TRUE)

  output <- read.csv(account_tables[1], stringsAsFactors=FALSE)

  for(i in 2:length(account_tables)) output <- rbind( output, read.csv(account_tables[i], stringsAsFactors=FALSE) )

  o <- order(output$date)
  output <- output[o,]

  write.csv(output, "./csv/general_ledger.csv", row.names=FALSE)

}



id_maker <- function(n, reserved='', seed=NA, nchars=NA){
  my_let <- letters 
  my_num <- 0:9 
  if(is.na(seed) | !is.numeric(seed)) set.seed(as.numeric(as.POSIXlt(Sys.time())))
  if(!is.na(seed) & is.numeric(seed)) set.seed(seed)
  output <- replicate(n, paste(sample(c(my_let, my_num), nchars, replace=TRUE), 
    collapse=''))
  rejected <- duplicated(output) | output %in% reserved | substr(output, 1, 1) %in% my_num
  while(any(rejected)){
    output <- output[-which(rejected)]
    remaining <- n-length(output)
    output <- c(output, replicate(remaining, paste(sample(c(my_let, my_num), nchars, 
      replace=TRUE), collapse='')))
    rejected <- duplicated(output) | output %in% reserved | substr(output, 1, 1) %in% my_num
  }
  output
}

dir_init <- function(path, verbose=FALSE, overwrite=TRUE){
  if(substr(path, 1, 2)!='./') stop('path argument must be formatted
    with "./" at beginning')
  contents <- dir(path, recursive=TRUE)
  if(dir.exists(path)){
    if(overwrite){
      if(verbose){
        if(length(contents)==0) print(paste('folder ', path, ' created.', sep=""))
        if(length(contents)>0) print(paste('folder ', path, ' wiped of ', length(contents), ' files/folders.', sep=""))
      }
      if(dir.exists(path)) unlink(path, recursive=TRUE)
      dir.create(path)
    }
  } else {
    if(verbose){
      print(paste('folder ', path, ' created.', sep=""))
    }
    dir.create(path)
  }
}
