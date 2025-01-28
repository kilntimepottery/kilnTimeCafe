library(shiny)
library(shinydashboard)
library(RSQLite)
library(sodium)
library(DT)
library(blastula)
library(glue)
library(bslib)
library(shinyjs)
library(shinyBS)
library(DBI)
library(gmailr)
library(dplyr)



source("helperFunctions.R")


calculate_remaining_capacity <- function(date, timeSlot) {
  conn <- dbConnect(SQLite(), dbname = "ChristineKilnTime.db", timeout = 10)
  on.exit(dbDisconnect(conn), add = TRUE)
  
  query <- "SELECT SUM(guests) AS total FROM bookings WHERE date = ? AND time = ?"
  result <- dbGetQuery(conn, query, params = list(as.character(date), timeSlot))
  total_guests <- result$total[1]
  
  remaining_capacity <- 19 - total_guests
  return(remaining_capacity)
}

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(
    bg = "#04192e",
    fg = "#cae6df",
    primary = "#52bf90",
    base_font = font_google("Grandstander")
  ),
  tags$style(HTML("
                  .centered-content {
                    display: flex;
                    justify-content: center;
                    align-items: center;
                    height: 80vh;
                  }
                  .btn.btn-default.action-button.shiny-bound-input {
                    background-color: #52bf90;
                    color: white; 
                  }
                  .admin-view-box {
                    margin-bottom: 20px;
                    padding: 10px;
                  }
                  .navbar {
                    margin-bottom: 20px;
                  }
                  .tab-content {
                    padding: 20px;
                  }
                  .nav.nav-tabs.shiny-tab-input.shiny-bound-input {
                    margin-bottom: 20px;
                  }
                  .col-sm-3 {
                    flex: 0 0 10%;
                    max-width: 10%;
                  }
                  h2 {
                    padding-top: 20px;
                  }
                  .box {
                    padding: 20px;
                    border: 2px solid #52bf90;
                  }
                  
                  .nav-tabs .nav-item a:not(.btn) {
                    pointer-events: none;
                  }
                 
      ")),
  tags$head(
    tags$script(HTML("
                     var idleTime = 0;
                     $(document).ready(function () {
                      var idleInterval = setInterval(timerIncrement, 60000);
                      $(this).mousemove(function (e) {
                        idleTime = 0;
                      });
                      $(this).keypress(function (e) {
                        idleTime = 0;
                      });
                    });
                     
                     function timerIncrement() {
                      idleTime = idleTime + 1;
                      if (idleTime > 9) {
                        Shiny.setInputValue('idle_timeout', true, {priority: 'event'});
                     }
                     }
                     "))
  ),
  titlePanel("Book Your Visit!"), 
  sidebarLayout(
    sidebarPanel(
      width = 3,
      actionButton("admin_login", "Admin Login")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs", 
        tabPanel("Select Service",
                 tags$head(
                   tags$script(
                     HTML("
                          $(document).ready(function() {
                            $('[data-toggle=\"tooltip\"]').tooltip({html: true});
                            
                            $('#pottery_painting').on('click', function() {
                              Shiny.onInputChange('pottery_painting', Math.random());
                            });
                            $('#evening_event').on('click', function() {
                              Shiny.onInputChange('evening_event', Math.random());
                            });
                          });
                          ")
                   )
                 ),
                 div(class = "jumbotron text-center", 
                     h1("Choose a Service For Your Visit:")),
                 fluidRow(column(width = 6,
                                 div(class = "centered-content",
                                     tags$button(id = "pottery_painting", type = "button", class = "btn btn-default",
                                                 tags$img(src = "pottery_painting.jpg", 
                                                          `data-toggle` = "tooltip", 
                                                          title = paste0("<b><u>Description:</u></b> Join us for a relaxing paint-your-own-pottery experience! ",
                                                                         "Choose from our range of individually-priced bisques and enjoy our comfy studio space while you unleash your creativity. ",
                                                                         "Pieces will be fired in our kiln and will be available for collection within 7 days of painting.<br><br>",
                                                                         "<b><u>Duration:</u></b> 2 Hours.<br><br>",
                                                                         "<b><u>Price:</u></b> Varies depending on bisque selection."),
                                                          style = "width: 100%; height: auto;"))
                                    
                                 )
                              ),
                          column(width = 6, 
                                 div(class = "centered-content", 
                                     tags$button(id = "evening_event", type = "button", class = "btn btn-default", 
                                                 tags$img(src = "evening_event.jpg", 
                                                          `data-toggle` = "tooltip", 
                                                          title = paste0("<b><u>Description:</u></b> Register for one of our upcoming special events! ",
                                                                         "Bring your friends and join us for a themed craft night experience. ",
                                                                         "Events typically include a complimentary glass of Prosecco (or an alcohol-free alternative) to enjoy while you paint. ",
                                                                         "From instructor-guided projects to relaxed paint-and-sip nights, we have events suited for all age groups and interests. ",
                                                                         "Register while there are still spaces available! <br><br>", 
                                                                         "<b><u>Duration:</u></b> 1.5 - 3 hours.<br><br>",
                                                                         "<b><u>Price:</u></b> We will collect a £5 deposit per guest upon registration. ",
                                                                         "This will be deducted from your total at the end of the event."),
                                                          style = "width: 100%; height: auto;"))
                                     )
                                 )
                              )
                           ),
          
          tabPanel("Select Date and Time",
                   conditionalPanel(
                     condition = "input.pottery_painting",
                     fluidRow(
                       column(width = 12, style = "display: flex; flex-direction: column; align-items: center; height: 400px;",
                       div(style = "margin-bottom: 20px;", uiOutput("dateUI")), 
                       div(style = "margin-bottom: 20px;", uiOutput("timeSlotUI")),
                       actionButton("next2", "Enter Details")
                                        
                                         )
                 ))
              ),
          tabPanel("Select Event",
                   conditionalPanel(
                     condition = "input.evening_event",
                     fluidRow(
                        column(width = 12,
                               div(class = "centered-content",
                                   uiOutput("eventsTable"),
                                   actionButton("next3", "Next")
                               )
                          ))
                 )),
          tabPanel("Event Booking Info",
                 fluidRow(
                   column(width = 12,
                          div(class = "centered-content",
                              textInput("customer_name", "Name:"),
                              textInput("email", "Email:"),
                              textInput("phone_number", "Phone Number:"),
                              numericInput("guests", "Number of Guests", value = NULL, min = 1),
                              actionButton("next4", "Next")
                              ))
                 )),
          tabPanel("Enter Customer Information", 
                   conditionalPanel(
                     condition = "input.next2 > 0 || input.next3 > 0",
                        fluidRow(
                          column(width = 12, style = "display: flex; flex-direction: column; align-items: center; height: 400px;",
                                 div(stlye = "margin-bottom: 20px;", textInput("customer_name", "Name: ")), 
                                 div(stlye = "margin-bottom: 20px;", textInput("email", "Email: ")), 
                                 div(style = "margin-bottom: 20px;", textInput("phone_number", "Phone Number: ")), 
                                 div(style = "margin-bottom: 20px;", numericInput("guests", "Number of Guests: ", value = NULL, min = 1)), 
                                 actionButton("book", "Book Now", class = "btn-primary")
                                
                          )
                     ))),
          tabPanel("Event Confirmation",
                 fluidRow(
                   column(width = 12,
                          div(class = "centered-content",
                          box(
                            title = "Event Booking Confirmation",
                            h3("Your event booking has been submitted!"),
                            p("You will soon receive an email from our staff regarding your booking deposit.")
                          ))
                          )
                   )
                 ), 
          tabPanel("Booking Confirmation",
                   conditionalPanel(
                     condition = "input.book > 0",
                     fluidRow(
                          column(width = 12, 
                                 div(class = "centered-content",
                            box(
                              title = "Booking Confirmation",
                              h3(textOutput("confirmation_msg")), 
                              h4("Reference Number: ", textOutput("ref_number")), 
                              h4("Service: ", textOutput("conf_service")),
                              h4("Name: ", textOutput("conf_name")), 
                              h4("Date: ", textOutput("conf_date")),
                              h4("Time: ", textOutput("conf_time"))
                              ))
                            )
                          )
                      )),
            tabPanel("Admin Login", 
                          fluidRow(
                            column(width = 12, 
                                   div(class = "centered-content",
                                      box(
                                        title = "Admin Login",
                                        textInput("admin_user", "Username:"), 
                                        passwordInput("admin_pass", "Password:"), 
                                        actionButton("login", "Login")
                                      )
                                    ))
                               )), 
            tabPanel("Admin Dashboard", 
                          fluidRow(
                              box(
                                title = "Manage Bookings", 
                                status = "primary",
                                width = 11,
                                selectInput("time_frame", "View Bookings For:", 
                                        choices = c("Next 24 Hours" = "24_hours", 
                                                    "Next 7 Days" = "7_days",
                                                    "Next 14 Days" = "14_days", 
                                                    "Next 28 Days" = "28_days")),
                              DTOutput("upcoming_bookings"), 
                              actionButton("add_booking", "Add Booking")
                          ), 
                          box(title = "Admin Management", 
                              width = 6, 
                              DTOutput("usersTable"),
                              actionButton("create_admin", "New Admin Profile"),
                              status = "primary"
                          ),
                            box(
                                title = "Business Closures", 
                                status = "primary",
                                width = 6,
                                dateInput("closure_date", "Choose a date:"),
                                selectInput("closure_start_time", "Choose start time:",
                                            choices = c("All Day", format(seq(strptime("00:00", "%H:%M"), strptime("23:45", "%H:%M"), by = "15 mins"), "%I:%M %p")),
                                            selected = "All Day"),
                                selectInput("closure_end_time", "Choose End Time:", 
                                            choices = c("All Day", format(seq(strptime("00:00", "%H:%M"), strptime("23:45", "%H:%M"), by = "15 mins"), "%I:%M %p")), 
                                            selected = "All Day"), 
                                actionButton("add_closure", "Add Closure"), 
                                DTOutput("closuresTable")
                              ), 
                            box(
                              title = "Create an Upcoming Event", 
                              width = 6,
                              textInput("event_name", "Event Name:"), 
                              textAreaInput("event_description", "Event Description:"), 
                              dateInput("event_date", "Event Date:"), 
                              selectInput("event_start_time", "Event Start Time:", 
                                          choices = format(seq(strptime("00:00", "%H:%M"), strptime("23:45", "%H:%M"), by = "15 mins"), "%I:%M %p")), 
                              numericInput("event_duration", "Event Duration (minutes):", value = 240, min = 1), 
                              numericInput("event_capacity", "Event Capacity:", value = 24, min = 1), 
                              actionButton("admin_event", "Add Event")
                            )
                  )
            )
      )
    )
  ), 
  bsModal("addBookingModal", "Add New Booking", "add_booking", size = "small", 
          dateInput("booking_date", "Date:"),
          textInput("booking_time", "TimeSlot:"), 
          textInput("booking_name", "Name:"), 
          textInput("booking_email", "Email:"), 
          textInput("booking_phone", "Phone Number:"),
          numericInput("booking_guests", "Number of Guests:", value = NULL, min = 1), 
          actionButton("confirm_add_booking", "Add Booking"), 
          actionButton("confirm_add_booking_email", "Add Booking and Send Confirmation Email")
          )
  

)


server <- function(input, output, session) {
  
  observeEvent(input$idle_timeout, {
    session$close()
  })
  
  fetch_upcoming_bookings <- function() {
    ensure_latest_db()
    conn <- get_db_connection()
    
    query <- "SELECT rowid AS id, * FROM bookings WHERE date >= DATE('now')"
    upcoming_bookings <- dbGetQuery(conn, query)
    dbDisconnect(conn)
    
    print(upcoming_bookings)
    
    output$upcoming_bookings <- renderDT({
      datatable(upcoming_bookings, selection = 'single', options = list(pageLength = 10, autoWidth = TRUE))
    })
  }
  
  observeEvent(input$pottery_painting, {
    updateTabsetPanel(session, "tabs", selected = "Select Date and Time")
  })
  
  observeEvent(input$evening_event, {
    updateTabsetPanel(session, "tabs", selected = "Select Event")
  })
  
  output$dateUI <- renderUI({
    if (!is.null(input$pottery_painting)) {
      dateInput("date", "Choose a date:")
    }
  })
  
 
  
  output$timeSlotUI <- renderUI({
    req(input$date)
    conn <- dbConnect(SQLite(), dbname ="ChristineKilnTime.db", timeout = 10)
    on.exit(dbDisconnect(conn), add = TRUE)
    if (!is.null(input$pottery_painting) && !is.null(input$date)) {
      selected_day <- weekdays(as.Date(input$date))
      query <- sprintf("SELECT time FROM availability
                       WHERE day = '%s' AND available = TRUE", selected_day)
      
      closures_query <- sprintf("Select closure_time FROM closures WHERE closure_date = '%s'", input$date)
      closures <- dbGetQuery(conn, closures_query)$closure_time
      available_slots <- dbGetQuery(conn, query)$time
      available_slots <- setdiff(available_slots, closures)
      
      
      if (length(available_slots) == 0) {
        h3("Sorry, there are no more available bookings for this date.")
      } else {
        selectInput("timeSlot", "Choose a time slot:", choices = available_slots)
      }
    }
  })
  
  observe({
    shinyjs::toggleState(id  = "next2", condition = !is.null(input$date) && input$date != "" && !is.null(input$timeSlot) && input$timeSlot != "")
  })
  
  observeEvent(input$next2, {
    if (!is.null(input$date) && !is.null(input$timeSlot)) {
    updateTabsetPanel(session, "tabs", selected = "Enter Customer Information")
    }
  })
  
  observe({
    shinyjs::toggleState(id = "next3", condition = !is.null(input$eventsTable_rows_selected) && length(input$eventsTable_rows_selected) > 0)
      
  })
  
  observeEvent(input$next3, {
    if (!is.null(input$events_Table_rows_selected)) {
      updateTabsetPanel(session, "tabs", selected = "Event Booking Info")
    }
  })
  
  observe({
    toggleState("next4", !is.null(input$customer_name) && input$customer_name != "" &&
                !is.null(input$email) && input$email != "" &&
                !is.null(input$guests) && input$guests > 0)
  })
  
  observeEvent(input$next4, {
    if (!is.null(input$customer_name) && input$customer_name != "" &&
        !is.null(input$email) && input$email != "" &&
        !is.null(input$guests) && input$guests > 0) {
      updateTabsetPanel(session, "tabs", selected = "Event Confirmation")
    }
  })
  

  observe({
    all_fields_filled <- !is.null(input$customer_name) && input$customer_name != "" &&
                         !is.null(input$email) && input$email != "" &&
                         !is.null(input$phone_number) && input$phone_number != "" &&
                         !is.null(input$guests) && input$guests > 0
    shinyjs::toggleState(id = "book", condition = "all_fields_filled")
  })
  
  observeEvent(input$book, {
    
    all_fields_filled <- !is.null(input$customer_name) && input$customer_name != "" &&
      !is.null(input$email) && input$email != "" &&
      !is.null(input$phone_number) && input$phone_number != "" &&
      !is.null(input$guests) && input$guests > 0
    
    if(!all_fields_filled) {
      showModal(modalDialog(
        title = "Incomplete Information", 
        "Please fill in all the required fields before proceeding.",
        easyClose = TRUE, 
        footer = NULL
      ))
    } else {
      
 
    conn <- dbConnect(SQLite(), dbname ="ChristineKilnTime.db", timeout = 10)
    on.exit(dbDisconnect(conn), add = TRUE)
    
    total_guests <- calculate_total_guests(input$date, input$timeSlot)
    
    service <- if (!is.null(input$pottery_painting)) {
      "Pottery Painting"
    } else if (!is.null(input$evening_event)) {
      "Evening Event Registration"
    } else {
      ""
    }
    
    print(paste("Total Guests Calculated:", total_guests))
    print(paste("Service:", service))
    print(paste("Date:", input$date))
    print(paste("Time:", input$timeSlot))
    print(paste("Customer Name:", input$customer_name))
    print(paste("Email:", input$email))
    print(paste("Phone Number:", input$phone_number))
    print(paste("Guests:", input$guests))
    
    
    if (total_guests + input$guests <= 19) {
      
      ref_number <- sample(1:1e6, 1)
      print(paste("Reference Number: ", ref_number))
      tryCatch({
        dbBegin(conn)
        dbExecute(conn, "INSERT INTO bookings (service, date, time, customer_name, email, phone_number, guests, ref_number)
                              VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
                      params = list(service, as.character(input$date), input$timeSlot, input$customer_name, input$email, input$phone_number, input$guests, ref_number))
        dbCommit(conn)
       
        print("Booking inserted into the database")
        
        verify_query <- "SELECT * FROM bookings WHERE date >= DATE('now')"
        verify_booking <- dbGetQuery(conn, verify_query)
        print("Verification Query Results:")
        print(verify_booking)
        
        all_bookings_query <- "SELECT * FROM bookings"
        all_bookings <- dbGetQuery(conn, all_bookings_query)
        print("All Bookings Table")
        print(all_bookings)
      }, error = function(e) {
        dbRollback(conn)
        print(paste("Error inserting booking:", e$message))
        

      })
      output$confirmation_msg <- renderText("Thank you for your booking! We're looking forward to Kil'n Time with you!")
      output$ref_number <- renderText(ref_number)
      output$conf_name <- renderText(input$customer_name)
      output$conf_service <- renderText(service)
      output$conf_date <- renderText(input$date)
      output$conf_time <- renderText(input$timeSlot)
            
      updateTabsetPanel(session, "tabs", selected = "Booking Confirmation")
      
      fetch_upcoming_bookings()
      
      
      email <- create_email(input$customer_name, 
                            service, 
                            input$date, 
                            input$timeSlot, 
                            input$guests, 
                            ref_number)
      smtp_send(
        email,
        from = "kilntimecafe@gmail.com", 
        to = input$email, 
        subject = "Booking Confirmation", 
        credentials = creds_key('email_creds_key'))
  
      remaining_capacity <- calculate_remaining_capacity(input$date, input$timeSlot)
      admin_email <- admin_booking_confirmation_email(input$date, 
                                                      input$timeSlot, 
                                                      input$guests, 
                                                      remaining_capacity)
      smtp_send(
        admin_email, 
        from = "kilntimecafe@gmail.com", 
        to = "kilntimecafe@gmail.com", 
        subject = "New Booking", 
        credentials = creds_key('email_creds_key')
      )
    } else {
      showModal(modalDialog(
        title = "Booking Failed", 
        paste("Sorry, this time slot is full. Please choose another time slot, 
              or give us a call to arrange your visit."), 
        easyClose = TRUE, 
        footer = NULL
      ))
    }
    }
  })
  
  observeEvent(input$login, {
    conn <- dbConnect(SQLite(), dbname ="ChristineKilnTime.db", timeout = 10)
    on.exit(dbDisconnect(conn), add = TRUE)
    user <- dbGetQuery(conn, "SELECT * FROM users WHERE username = ?", params = list(input$admin_user))
    if (nrow(user) == 1 && sodium::password_verify(user$password_hash, input$admin_pass)) {
      updateTabsetPanel(session, "tabs", selected = "Admin Dashboard")
      
      fetch_upcoming_bookings()
      
      users_data <- dbGetQuery(conn, "SELECT username FROM users")
      output$usersTable <- renderDT({
        datatable(users_data, options = list(pageLength = 10, autoWidth = TRUE))
      })
    } else {
      showModal(modalDialog(
        title = "Login Failed",
        "Invalid username or password",
        easyClose = TRUE, 
        footer = NULL
      ))
    }
  })
  
  output$eventsTable <- renderUI({
    conn <- dbConnect(SQLite(), dbname = "ChristineKilnTime.db", timeout = 10)
    on.exit(dbDisconnect(conn), add = TRUE)
    
    events_query <- "SELECT * FROM events WHERE event_date >= DATE('now')"
    events <- dbGetQuery(conn, events_query)
    
    if (nrow(events) == 0) {
      div(
        class = "jumbotron text-center", 
        h1("Sorry, we don't have any upcoming events availble for booking!"), 
        p("Check back soon, as we are always adding new events to the schedule."), 
        actionButton("backtoBeginning", "Back to Beginning", class = "btn btn-primary"), 
        actionButton("closePage", "Close Page", class = "btn btn-danger")
      )
    } else {
      DT::renderDataTable({
        DT::datatable(events)
    })
    }
  })
  
  observeEvent(input$backtoBeginning, {
    session$reload()
  })
  
  observeEvent(input$closePage, {
    stopApp()
  })
  
  observeEvent(input$add_closure, {
    conn <- dbConnect(SQLite(), dbname ="ChristineKilnTime.db", timeout = 10)
    on.exit(dbDisconnect(conn), add = TRUE)
    formatted_closure_date <- format(as.Date(input$closure_date), "%Y-%m-%d")
    
    if (input$closure_start_time != "All Day" & input$closure_end_time != "All Day") {
      start_time <- strptime(input$closure_start_time, "%I:%M %p")
      end_time <- strptime(input$closure_end_time, "%I:%M %p")
      time_slots <- format(seq(start_time, end_time, by = "15 mins"), "%I:%M %p")
      for (time_slot in time_slots) {
        dbExecute(conn, "INSERT INTO closures (closure_date, closure_time) VALUES (?, ?)", 
                  params = list(formatted_closure_date, time_slot))
      }
    } else {
      dbExecute(conn, "INSERT INTO closures (closure_date, closure_time) VALUES (?, ?)", 
                params = list(formatted_closure_date, "All Day"))
    }
    
    showModal(modalDialog(
      title = "Closure Added", 
      paste("Closure added for", input$closure_date, "from", input$closure_start_time, "to", input$closure_end_time), 
      easyClose = TRUE, 
      footer = NULL
    ))
  })
  output$closuresTable <- renderDT({
    
    conn <- dbConnect(SQLite(), dbname ="ChristineKilnTime.db", timeout = 10)
    on.exit(dbDisconnect(conn), add = TRUE)
    dbGetQuery(conn, "SELECT closure_date, closure_time FROM closures WHERE closure_date >= DATE('now')")
  }, options = list(pageLength = 10, autoWidth = TRUE))
  
  observeEvent(input$admin_event, {
    conn <- dbConnect(SQLite(), dbname = "ChristineKilnTime.db", timeout = 10)
    on.exit(dbDisconnect(conn), add = TRUE)
    formatted_event_date <- format(as.Date(input$event_date), "%Y-%m-%d")
    dbExecute(conn, "INSERT INTO events (event_name, event_description, event_date, event_start_time, event_capacity, event_duration)
                    VALUES (?, ?, ?, ?, ?, ?)", 
              params = list(input$event_name, input$event_description, as.character(input$event_date), input$event_start_time, input$event_capacity, input$event_duration))
    showModal(modalDialog(
      title = "Event Added", 
      paste("Event", input$event_name, "on", input$event_date, "at", input$event_start_time, "has been added."), 
      easyClose = TRUE, 
      footer = NULL
    ))
  })
  
  observeEvent(input$admin_login, {
    updateTabsetPanel(session, "tabs", selected = "Admin Login")
  })
  observe({
    conn <- dbConnect(SQLite(), dbname = "ChristineKilnTime.db", timeout = 10)
    on.exit(dbDisconnect(conn), add = TRUE)
    print("Database connection established.")
    
    tables <- dbListTables(conn)
    print(paste("Tables in the database:", paste(tables, collapse = ", ")))
  })
  observeEvent(input$time_frame, {
    conn <- dbConnect(SQLite(), dbname = "ChristineKilnTime.db", timeout = 10)
    on.exit(dbDisconnect(conn), add = TRUE)
    
    time_frame <- switch(input$time_frame,
                         "24_hours" = 1, 
                         "7_days" = 7, 
                         "14_days" = 14,
                         "28_days" = 28)
    
    query <- sprintf("SELECT * FROM bookings WHERE date BETWEEN DATE('now') AND DATE('now', '+%d days')", time_frame)
    upcoming_bookings <- dbGetQuery(conn, query)
    
    output$upcoming_bookings <- renderDT({
      renamed_bookings <- upcoming_bookings %>%
        rename(
          `Booking Ref` = ref_number, 
          `Customer Name` = customer_name, 
          `Customer Email` = email, 
          `Phone Number` = phone_number
          
        )
      datatable(renamed_bookings, selection = 'single', options = list(pageLength = 10, autoWidth = TRUE))
    })
  })
  
  observe({
    updateSelectInput(session, "time_frame", selected = "7_days")
  })
 
  

  output$bookingTable <- renderDT({
    conn <- dbConnect(SQLite(), dbname = "ChristineKilnTime.db", timeout = 10)
    on.exit(dbDisconnect(conn), add = TRUE)
    bookings <- dbGetQuery(conn, "SELECT * FROM bookings")
    bookings <- bookings %>% rename(
      `Booking Reference` = booking_reference, 
      `Customer Name` = customer_name, 
      `Phone Number` = phone_number
    )
    
   
      
    
      
    
    datatable(bookings)
  })
  
  observeEvent(input$add_booking, {
    showModal(
      modalDialog(
        title = "Add New Booking", 
        dateInput("booking_date", "Date:"), 
        selectInput("booking_time", "TimeSlot:", 
                    choices = format(seq(strptime("00:00", "%H:%M"), strptime("23:45", "%H:%M"), by = "15 mins"), "%I:%M %p")),
        textInput("booking_name", "Name:"), 
        textInput("booking_email", "Email:"), 
        textInput("booking_phone", "Phone Number:"), 
        numericInput("booking_guests", "Number of Guests:", value = NULL, min = 1), 
        actionButton("confirm_add_booking", "Add Booking"), 
        actionButton("confirm_add_booking_email", "Add Booking and Send Confirmation Email"), 
        easyClose = TRUE, 
        footer = tagList(
          modalButton("Close"))
      )
    )
  })
  
  observeEvent(input$confirm_add_booking, {
    ref_number <- sample(1:1e6, 1)
    conn <- dbConnect(SQLite(), dbname = "ChristineKilnTime.db", timeout = 10)
    on.exit(dbDisconnect(conn), add = TRUE)
    
    dbBegin(conn)
    dbExecute(conn, "INSERT INTO bookings (service, date, time, customer_name, email, phone_number, guests, ref_number)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?)", 
              params = list("Admin Booking", as.character(input$booking_date), input$booking_time, input$booking_name, input$booking_email, input$booking_phone, input$booking_guests, ref_number))
    dbCommit(conn)
    print("Booking inserted into database")
    removeModal()
    
    fetch_upcoming_bookings()
  })
  
  observeEvent(input$confirm_add_booking_email, {
    ref_number <- sample(1:1e6, 1)
    conn <- dbConnect(SQLite(), dbname = "ChristineKilnTime.db", timeout = 10)
    on.exit(dbDisconnect(conn), add =TRUE)
    message <- ""
    dbExecute(conn, "INSERT INTO bookings (service, date, time, customer_name, email, phone_number, guests, ref_number)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?)", 
              params = list("Admin Booking", as.character(input$booking_date), input$booking_time, input$booking_name, input$booking_email, input$booking_phone, input$booking_guests, ref_number))
   email <- create_email(input$booking_name, 
                         "Pottery Painting",
                         input$booking_date,
                         input$booking_time, 
                         input$booking_guests, 
                         message, 
                         ref_number)
   smtp_send(
     email, 
     from = "kilntimecafe@gmail.com",
     to = input$booking_email, 
     subject = "Booking Confirmation", 
     credentials = creds_key('email_creds_key')
   )
   
   removeModal()
   fetch_upcoming_bookings()
    
  })
  
  observeEvent(input$upcoming_bookings_rows_selected, {
    selected_row <- input$upcoming_bookings_rows_selected
    print(paste("Selected row for viewing details:", selected_row))
    if(length(selected_row) > 0) {
      conn <- dbConnect(SQLite(), dbname = "ChristineKilnTime.db", timeout = 10)
      on.exit(dbDisconnect(conn), add = TRUE)
      query <- sprintf("SELECT rowid AS id, * FROM bookings WHERE rowid = %d", selected_row)
      booking <- dbGetQuery(conn, query)
      
      print("Booking Details for the selected row:")
      print(booking)
      
      showModal(
        modalDialog(
          title = "Booking Details", 
          p(paste("Service:", booking$service)), 
          p(paste("Reference Number:", booking$ref_number)),
          p(paste("Date:", booking$date)), 
          p(paste("Time:", booking$time)), 
          p(paste("Customer Name:", booking$customer_name)), 
          p(paste("Email:", booking$email)), 
          p(paste("Phone Number:", booking$phone_number)), 
          p(paste("Guests:", booking$guests)), 
          actionButton("email_customer", "Email Customer"), 
          actionButton("cancel_booking", "Cancel Booking"), 
          actionButton("change_booking", "Change Booking"),
          easyClose = TRUE, 
          footer = tagList(
            modalButton("Close")
        )
      ))
    }
  })
  
  observeEvent(input$email_customer, {
    showModal(
      modalDialog(
        title = "Email Customer", 
        textInput("email_message", "Message to Customer:"), 
        actionButton("send_contact", "Contact Customer"), 
        easyClose = TRUE, 
        footer = NULL
        
      )
    )
  })
  
  observeEvent(input$send_contact, {
    conn <- dbConnect(SQLite(), dbname = "ChristineKilnTime.db", timeout = 10)
    on.exit(dbDisconnect(conn), add = TRUE)
    
    selected_row <- input$upcoming_bookings_rows_selected
    query <- sprintf("SELECT * FROM bookings WHERE rowid = %d", selected_row)
    booking <- dbGetQuery(conn, query)
    
    email <- create_email(booking$customer_name, 
                          booking$service, 
                          booking$date, 
                          booking$time, 
                          booking$guests, 
                          input$email_message,
                          booking$ref_number)
    smtp_send(
      email, 
      from = "kilntimecafe@gmail.com", 
      to = booking$email, 
      subject = "Your Kil'n Time Booking", 
      credentials = creds_key('email_creds_key')
    )
    removeModal()
  })
  
  observeEvent(input$cancel_booking, {
    showModal(
      modalDialog(
        title = "Cancel Booking", 
        p("Are you sure you wish to cancel this booking?"), 
        actionButton("confirm_cancel_booking", "Yes"), 
        actionButton("ncel", "No"), 
        easyClose = TRUE, 
        footer = NULL
      )
    )
  })
  
  observeEvent(input$confirm_cancel_booking, {
    ensure_latest_db()
    conn <- get_db_connection()
    on.exit(dbDisconnect(conn), add = TRUE)
    
    
    selected_row <- input$upcoming_bookings_rows_selected
    print(paste("Selected Row ID: ", selected_row))
    
    query <- sprintf("SELECT rowid AS id, * FROM bookings WHERE rowid = %d", selected_row)
    booking <- dbGetQuery(conn, query)
    
    print("Selected Booking for Cancellation:")
    print(booking)
    
    if(nrow(booking) == 1) {
    
      dbExecute(conn, "DELETE FROM bookings WHERE rowid = ?", params = list(selected_row))
      
      print("Booking Deleted from Database")
      
      tryCatch({
        email <- create_cancellation_email(booking$customer_name, booking$service, booking$date, booking$time, booking$guests, booking$ref_number)
          smtp_send(
            email, 
            from = "kilntimecafe@gmail.com", 
            to = booking$email, 
            subject = "Booking Cancellation", 
            credentials = creds_key('email_creds_key')
          )
          print("Email sent successfully")
      }, error = function(e) {
      print(paste("Error sending email:", e$message))
    })
      removeModal()
      fetch_upcoming_bookings()
    } else {
      print("Error: Booking Not Found or Multiple Bookings Returned.")
    }
    })
  
  observeEvent(input$change_booking, {
    selected_row <- input$upcoming_bookings_rows_selected
    conn <- dbConnect(SQLite(), dbname = "ChristineKilnTime.db", timeout = 10)
    on.exit(dbDisconnect(conn), add = TRUE)
    query <- sprintf("SELECT * FROM bookings WHERE rowid = %d", selected_row)
    booking <- dbGetQuery(conn, query)
    
    showModal(
      modalDialog(
        title = "Change Booking", 
        dateInput("change_booking_date", "Date:", value = as.Date(booking$date)), 
        textInput("change_booking_time", "TimeSlot:", value = booking$time), 
        textInput("change_booking_name", "Name:", value = booking$customer_name), 
        textInput("change_booking_email", "Email:", value = booking$email), 
        textInput("change_booking_phone", "Phone Number", value = booking$phone_number), 
        numericInput("change_booking_guests", "Number of Guests:", value = booking$guests, min = 1), 
        actionButton("confirm_change_booking", "Save Changes"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  observeEvent(input$confirm_change_booking, {
    selected_row <- input$upcoming_bookings_rows_selected
    conn <- dbConnect(SQLite(), dbname = "ChristineKilnTime.db", timeout = 10)
    on.exit(dbDisconnect(conn), add = TRUE)
    
    dbBegin(conn)
    dbExecute(conn, "UPDATE bookings SET service = ?, date = ?, time = ?, customer_name = ?, email = ?, phone_number = ?, guests = ?
                    WHERE rowid = ?", 
              params = list("Admin Booking", as.character(input$change_booking_date), input$change_booking_time, input$change_booking_name, 
                            input$change_booking_email, input$change_booking_phone, input$change_booking_guests, selected_row))
    dbCommit(conn)
    
    print("Booking Updated in Database")
    removeModal()
    fetch_upcoming_bookings()
    
  })
  
  observeEvent(input$create_admin, {
    showModal(
      modalDialog(
        title = "Create New Admin Profile:", 
        textInput("new_admin_username", "New Admin Username:"), 
        passwordInput("new_admin_password", "New Admin Password:"), 
        footer = tagList(
          modalButton("Cancel"), 
          actionButton("save_admin", "Save")
        ), 
        easyClose = TRUE
      )
    )
  })
  
  observeEvent(input$save_admin, {
    req(input$new_admin_username, input$new_admin_password)
    
    conn <- dbConnect(SQLite(), dbname = "ChristineKilnTime.db", timeout = 10)
    on.exit(dbDisconnect(conn), add = TRUE)
    
    new_admin_password_hash <- sodium::password_store(input$new_admin_password)
    dbExecute(conn, "INSERT INTO users (username, password_hash) VALUES (?, ?)",
              params = list(input$new_admin_username, new_admin_password_hash))
    
    showModal(
      modalDialog(
        title = "Success", 
        "New admin profile has been created successfully.", 
        easyClose = TRUE, 
        footer = NULL
      ))
    
    users_data <- dbGetQuery(conn, "SELECT username FROM users")
    output$usersTable <- renderDT({
      datatable(users_data, options = list(pageLength = 10, autoWidth = TRUE))
    })
  })
  
  
  
  
}

shinyApp(ui, server)  