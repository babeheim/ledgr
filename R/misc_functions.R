

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
sum_amounts <- function(start_date=NA, stop_date=NA, accounts=NA){
  d <- read.csv('./csv/journal.csv', stringsAsFactors=FALSE)
  if(is.na(start_date)) start_date <- min(d$date)
  if(is.na(stop_date)) stop_date <- max(d$date)
  if(is.na(accounts)) accounts <- sort(unique(d$account))
  tar <- which(d$date >= start_date & d$date <= stop_date & d$account %in% accounts)
  return(sum(d$amount[tar]))
}


# unnecessary with grep involved!
# surely this is simpler using regexpr???
colon_reducer <- function(string, level=NA){
    output <- string
    output[is.na(output)] <- ''
    for(i in 1:length(string)){
        break_pos <- gregexpr(':', output[i])[[1]]
        if(!is.na(break_pos[level]) & break_pos[level]!='-1') output[i] <- substr(output[i], 1, break_pos[level]-1) 
    }
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

# deprecated IMO
journal_tabulator <- function( data, account_target, time_target, as.tex=FALSE, na.blank=TRUE ){
    valid_rows <- which( ( data$y %in% time_target | data$ym %in% time_target ) & 
            data$account1 %in% account_target )
    data <- data[valid_rows,]
    if( any( time_target %in% data$y ) ) x <- tapply( data$amount, list( data$account2, data$y ), sum )
    if( any( time_target %in% data$ym ) ) x <- tapply( data$amount, list( data$account2, data$ym ), sum )
    if( length(time_target) == 1 & time_target %in% data$ym ) x <- tapply( data$amount, list( data$account2, data$week ), sum )
    if( nrow(x) == 0 ) stop('no entries found')
    for( i in 1:ncol(x) ) x[,i] <- round(x[,i])
    total <- apply( x, 2, function(z) sum(z, na.rm=TRUE) )
    x <- rbind( x, total )
    total <- apply( x, 1, function(z) sum(z, na.rm=TRUE) )
    x <- cbind( x, total )
    x <- cbind( rownames(x), x )
    colnames(x)[1] <- 'account'
    if(na.blank) x[is.na(x)] <- '.'
    if(as.tex){
        net_balance_tx <- x
        net_balance_tx <- rbind( colnames(net_balance_tx), net_balance_tx )
        net_balance_tx <- texttab(net_balance_tx, hline=c(1, nrow(net_balance_tx)-1, nrow(net_balance_tx)) )
        net_balance_tx <- gsub( 'NA', '.', net_balance_tx )
        x <- net_balance_tx
    }
    return(x)
}

exchanger <- function(journal, prices, currency){
    d <- journal
    d$date <- as.Date(d$date)
    prices$date <- as.Date(prices$date)
    for(i in 1:nrow(d)){
        if(d$currency[i]!=currency & d$currency[i] %in% prices$numerator){
            currency_rows <- which(prices$numerator==d$currency[i])
            currency_dates <- prices$date[currency_rows]
            diffs <- abs(d$date[i] - currency_dates)
            best_row <- currency_rows[which.min(diffs)]
            d$amount[i] <- prices$price[best_row]*d$amount[i]
            d$currency[i] <- paste0(currency,'-eqv')
        }
    } # ok good, but what if the numerare isn't in numerare commodity?
    return(d)
}



prepare_journal <- function(){

  d <- read.csv('./csv/general_ledger.csv', stringsAsFactors=TRUE)

  if(length(grep('/', d$date))>0){
      last_four <- substr(d$date, nchar(d$date)-3, nchar(d$date))
      lower_y <- grep('/', last_four)
      upper_y <- which(!1:nrow(d) %in% lower_y)
      d$date[upper_y] <- as.character(as.Date(d$date, '%m/%d/%Y'))
      d$date[lower_y] <- as.character(as.Date(d$date, '%m/%d/%y'))
  }
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


balance_books <- function(){

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
    n_postings[i] <- sum(involved)
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

dir_init <- function(path, verbose=FALSE){
    if(substr(path, 1, 2)!='./') stop('path argument must be formatted
        with "./" at beginning')
    contents <- dir(path, recursive=TRUE)
    if(verbose){
        if(length(contents)==0) print(paste('folder ', path, ' created.', sep=""))
        if(length(contents)>0) print(paste('folder ', path, ' wiped of ', length(contents), ' files/folders.', sep=""))
    }
    if(dir.exists(path)) unlink(path, recursive=TRUE)
    dir.create(path)
}
