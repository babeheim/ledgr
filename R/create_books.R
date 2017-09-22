

create_books <- function(path){

  dir_init(path)
  dir_init(file.path(path, 'csv'))

  # aid isn't really necessary, is it? no, its not! neither is "accounts"
  
  date_shifts <- data.frame(tid=character(), 
    original_date=character(), presentation_date=character())

  exchange_rates <- data.frame(date=character(), 
    denominator=character(), numerator=character(), 
    price=character())

  general_ledger <- data.frame(date=character(), 
    amount=character(),  tag=character(), 
    notes=character(), account=character(), 
    currency=character(),  checksum=character(), 
    tid=character(), balance=character())

  write.csv(general_ledger, file.path(path, 'csv', 'general_ledger.csv'), row.names=FALSE)
  write.csv(exchange_rates, file.path(path, 'csv', 'exchange_rates.csv'), row.names=FALSE)
  write.csv(date_shifts, file.path(path, 'csv', 'date_shifts.csv'), row.names=FALSE)

}

