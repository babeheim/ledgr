
# balance transactiosn shoudl fail if any are NA

scrub_text <- function(input_string){
  d <- input_string
  d <- iconv(d, "latin1", "ASCII", sub="") # strip out non-ASCII entirely
  # pgn_tag <- stringi::stri_trans_general(pgn_tag, "latin-ascii") # convert non-ASCII to closest ascii
  d <- gsub("[\x01-\x1F]", "", d) # takes care of non-printing ASCII
  return(d)
}

prepare_excel <- function(){
 
  account_balances <- list()
  my_files <- list.files("./reports", pattern = "^account_balances_monthly*", full.names = TRUE)
  my_filenames <- list.files("./reports", pattern = "^account_balances_monthly*", full.names = FALSE)
  my_filenames <- paste("account balances", substr(my_filenames, 26, 29))
  for(i in 1:length(my_files)) account_balances[[i]] <- read.csv(my_files[i], stringsAsFactors = FALSE)
  names(account_balances) <- my_filenames

  income_expenses <- list()
  my_files <- list.files("./reports", pattern = "^income_expenses_monthly*", full.names = TRUE)
  my_filenames <- list.files("./reports", pattern = "^income_expenses_monthly*", full.names = FALSE)
  my_filenames <- paste("income expense", substr(my_filenames, 25, 28))
  for(i in 1:length(my_files)) income_expenses[[i]] <- read.csv(my_files[i], stringsAsFactors = FALSE)
  names(income_expenses) <- my_filenames

  output <- list(
    accounts = read.csv("./csv/accounts.csv", stringsAsFactors = FALSE),
    `general ledger` = read.csv("./csv/general_ledger.csv", stringsAsFactors = FALSE),
    `income expense annual` = read.csv("./reports/income_expense_annual.csv", stringsAsFactors = FALSE),
    `account balances annual` = read.csv("./reports/account_balances_annual.csv", stringsAsFactors = FALSE)
  )

  output <- c(output, income_expenses, account_balances)
 
  date <- gsub("-", "", substr(Sys.time(), 1, 10))
  names <- paste0("account_overview_", date,".xlsx")

  write.xlsx(output, file.path("./reports", names), colWidths = "auto")

}

truncate_accounts <- function(string, level = 2){
  string <- as.character(string)
  output <- string
  output[is.na(output)] <- ''
  for(i in 1:length(string)){
    break_pos <- gregexpr(':', output[i])[[1]]
    if(!is.na(break_pos[level]) & break_pos[level] != '-1') output[i] <- substr(output[i], 1, break_pos[level]-1)
  }
  return(output)
}


shift_dates <- function(journal, dateshift){
  dateshift$original_date <- fix_dates(dateshift$original_date)
  dateshift$presentation_date <- fix_dates(dateshift$presentation_date)
  shift_tar <- which(journal$tid %in% dateshift$tid)
  if(length(shift_tar)>0){
    journal$date[shift_tar] <- as.character(dateshift$presentation_date[match(journal$tid[shift_tar], dateshift$tid)])
  }    
  return(journal)
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
    upper_y <- which(!1:length(dates) %in% lower_y)
    if(length(upper_y)>0) dates[upper_y] <- as.character(as.Date(dates[upper_y], '%m/%d/%Y'))
    if(length(lower_y)>0) dates[lower_y] <- as.character(as.Date(dates[lower_y], '%m/%d/%y'))
  }
  dates <- as.Date(dates)
  return(dates)
}

exchange <- function(journal, prices, out_currency){
  journal$date <- fix_dates(journal$date)
  prices$date <- fix_dates(prices$date)
  prices2 <- select(prices, date = date, numerator = denominator, denominator = numerator, price = price)
  prices2$price <- 1 / prices2$price
  prices <- rbind(prices, prices2)
  for(i in 1:nrow(journal)){
    if(journal$currency[i]!=out_currency & !is.na(journal$date[i])){
      ex_rows <- which(prices$denominator==journal$currency[i] & prices$numerator==out_currency)
      if(length(ex_rows)>0){
        ex_dates <- prices$date[ex_rows]
        diffs <- abs(journal$date[i] - ex_dates)
        best_row <- ex_rows[which.min(diffs)]
        if(diffs[which.min(diffs)] > 365) warning(paste0("transaction ", journal$tid[i], " does not have a recent exchange rate"))
        journal$amount[i] <- prices$price[best_row]*journal$amount[i]
        journal$currency[i] <- paste0(out_currency,'-eqv')
      } else {
        stop( paste(journal$currency[i], "and", out_currency, "have no pairing
          in the exchange rate table") ) 
      }
    }
  } 
  return(journal)
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
