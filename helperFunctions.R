create_cancellation_email <- function(customer_name, 
                                      service, 
                                      date, 
                                      time, 
                                      guests, 
                                      ref_number) {
 email <- compose_email(
   body = md(glue::glue("
                        Hello {customer_name}, 
                        
                        We regret to inform you that your booking has been canceled for the following
                        booking details: 
                        
                        Reference Number: {ref_number}
                        Service: {service}
                        Date: {date}
                        Time: {time}
                        Number of Guests: {guests}
                        
                        If you have any questions, or if you would like to reschedule your booking with us, 
                        please contact us by replying to this email! 
                        
                        We hope to see you soon. 
                        
                        Best Regards, 
                        Kil'n Time Team
                        "))
 ) 
 return(email)
}

create_email <- function(customer_name, 
                         service, 
                         date, 
                         time, 
                         guests,
                         ref_number) {
  email <- compose_email(
    body = md(glue::glue("
                         Hello {customer_name}, 
                         
                         Thank you so much for your booking! We're looking forward 
                         to Kil'n Time with you! 
                         
                         Your booking reference number is {ref_number}
                         
                         Your appointment is scheduled for {service} on {date} at {time} for 
                         {guests} guest(s). 
                         
                         
                         
                         Please contact us as soon as possible if you have any questions,
                         or if you'd like to make changes to your booking. 
                         "))
  )
  return(email)
}


admin_booking_confirmation_email <- function(date, time, guests, remaining_capacity) {
  email <- compose_email(
    body = md(glue::glue("
                         A new booking has been made for {date} at {time} for {guests} guest(s).
                         
                         This time slot now has {remaining_capacity} spaces available."))
  )
  return(email)
}
generate_time_slots <- function(start_time, end_time, interval = 15) {
  start <- as.POSIXct(start_time, format = "%I:%M %p")
  end <- as.POSIXct(end_time, format = "%I:%M %p")
  slots <- seq(from = start, to = end, by = paste(interval, "mins"))
  return(format(slots, "%I:%M %p"))
}

# create_smtp_creds_key(
#   id = "email_creds_key", 
#   user = "kilntimecafe@gmail.com", 
#   provider = "gmail"
# )
#  create_smtp_creds_file(
#    file = "email_creds", 
#    user = "kilntimecafe@gmail.com",
#    provider = "gmail", 
#    use_ssl = TRUE
# )
# 
# fileConn <- file("email_creds")
# writeLines(c(
#   "user=kilntimecafe@gmail.com", 
#   "password=mlag fprn iojw bftn", 
#   "provider=gmail", 
#   "use_ssl=TRUE"
# ), fileConn)
# 
# close(fileConn)
calculate_total_guests <- function(date, timeSlot) {
  conn <- dbConnect(SQLite(), dbname ="ChristineKilnTime.db", timeout = 10)
  on.exit(dbDisconnect(conn), add = TRUE)
  query <- sprintf("SELECT SUM(guests) AS total_guests FROM bookings 
                   WHERE date = '%s' AND time = '%s'", date, timeSlot)
  result <- dbGetQuery(conn, query)$total_guests
  print(paste("Total Guests Query Result:", result))
  if (is.na(result)) {
    return(0)
  }
  return(result)
}

# fetch_upcoming_bookings <- function() {
#   conn <- dbConnect(SQLite(), dbname ="ChristineKilnTime.db", timeout = 10)
#   on.exit(dbDisconnect(conn), add = TRUE)
#   query <- "SELECT rowid AS id, * FROM bookings WHERE date >= DATE('now')"
#   upcoming_bookings <- dbGetQuery(conn, query)
#   
#   output$upcoming_bookings <- renderDT({
#     datatable(upcoming_bookings, selection = 'single')
#   }, options = list(pageLength = 10, autoWidth = TRUE))
# }

reset_db_connection <- function() {
  if (dbIsValid(conn)) {
    dbDisconnect(conn)
  }
  conn <<- dbConnect(SQLite(), "ChristineKilnTime.db")
  print("Database Connection Reset Successfully.")
    
}

get_db_connection <- function() {
  conn <- dbConnect(SQLite(), dbname = "ChristineKilnTime.db", timeout = 10)
  print("Database connection established.")
  return(conn)
}

ensure_latest_db <- function() {
  file_path <- "ChristineKilnTime.db"
  file_info <- file.info(file_path)
  print(paste("Database file last modified:", file_info$mtime))
}



potteryPainting <- ""