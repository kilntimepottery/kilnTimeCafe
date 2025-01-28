library(RSQLite)
library(sodium)

#Create a connection to the database (create database if it doesn't exist)
conn <- dbConnect(SQLite(), "ChristineKilnTime.db")

#Create tables for availability and bookings
dbExecute(conn, "CREATE TABLE IF NOT EXISTS availability (
                  id INTEGER PRIMARY KEY, 
                  day TEXT, 
                  time TEXT, 
                  available BOOLEAN)")
dbExecute(conn, "CREATE TABLE IF NOT EXISTS bookings (
                  id INTEGER PRIMARY KEY, 
                  ref_number INTEGER,
                  service TEXT, 
                  date TEXT, 
                  time TEXT, 
                  customer_name TEXT, 
                  email TEXT, 
                  phone_number TEXT, 
                  guests INTEGER)")
dbExecute(conn, "CREATE TABLE IF NOT EXISTS users (
                  id INTEGER PRIMARY KEY, 
                  username TEXT, 
                  password_hash TEXT)")
dbExecute(conn, "CREATE TABLE IF NOT EXISTS closures (
                  id INTEGER PRIMARY KEY, 
                  closure_date TEXT, 
                  closure_time TEXT)")

dbExecute(conn, "CREATE TABLE IF NOT EXISTS events (
                  id INTEGER PRIMARY KEY, 
                  event_name TEXT, 
                  event_description TEXT,
                  event_date TEXT, 
                  event_start_time TEXT, 
                  event_duration INTEGER, 
                  event_capacity INTEGER)")

dbExecute(conn, "CREATE TABLE IF NOT EXISTS event_bookings (
                  id INTEGER PRIMARY KEY, 
                  event_name TEXT, 
                  event_date TEXT, 
                  event_time TEXT, 
                  customer_name TEXT, 
                  email TEXT, 
                  phone_number TEXT, 
                  guests INTEGER)")


tuesday_to_saturday_timeslots <- generate_time_slots("10:00 AM", "05:00 PM")
sunday_slots <- generate_time_slots("11:00 AM", "05:00 PM")

availability_data <- data.frame(
  day = c(rep("Tuesday", length(tuesday_to_saturday_timeslots)), 
          rep("Wednesday", length(tuesday_to_saturday_timeslots)), 
          rep("Thursday", length(tuesday_to_saturday_timeslots)), 
          rep("Friday", length(tuesday_to_saturday_timeslots)), 
          rep("Saturday", length(tuesday_to_saturday_timeslots)), 
          rep("Sunday", length(sunday_slots))), 
  time = c(rep(tuesday_to_saturday_timeslots, 5), sunday_slots),
  available = TRUE
)

admin_password <- "kilnTimePottery123"
admin_password_hash <- sodium::password_store(admin_password)

admin_username <- "christineCarr"

dbExecute(conn, "INSERT INTO users (username, password_hash) VALUES (?, ?)", 
          params = list(admin_username, admin_password_hash))


new_admin_username <- "katie"
new_admin_password <- "P@nt@l00n"
new_admin_password_hash <- sodium::password_store(new_admin_password)
dbExecute(conn, "INSERT INTO users (username, password_hash) VALUES (?, ?)",
          params = list(new_admin_username, new_admin_password_hash))
print(availability_data)

dbWriteTable(conn, "availability", availability_data, append = TRUE, row.names = FALSE )


availability_exists <- dbExistsTable(conn, "availability")

