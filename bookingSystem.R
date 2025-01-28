library(shiny)
library(shinydashboard)
library(RSQLite)
library(sodium)
library(DT)
library(blastula)
library(glue)
library(bslib)

setwd("C:/Users/bear/source/repos/KilnTimeBookingSystem/KilnTimeBookingSystem")

conn <- dbConnect(SQLite(), "ChristineKilnTime.db")

ui <- fluidPage(
  theme = bs_theme(
    bg = "#202123",
    fg = "#B8BCC2",
    primary = "#EA80FC",
    base_font = font_google("Grandstander")
  ),
  dashboardPage(
  dashboardHeader(
      title = "Book Your Visit!", 
      tags$li(class = "dropdown", 
              actionButton("admin_login", "Admin Login"))), 
  dashboardSidebar(disable = TRUE, 
                   sidebarMenu(
                     id = "tabs", 
                     menuItem("Select Service", tabName = "serviceSelect", selected = TRUE), 
                     menuItem("Select Date and Time", tabName = "dateTimeSelect"), 
                     menuItem("Select Event", tabName = "selectEvent"), 
                     menuItem("Event Booking Info", tabName = "eventBookingInfo"),
                     menuItem("Enter Customer Information", tabName = "customerInfo"), 
                     menuItem("Event Confirmation", tabName = "eventConfirmation"),
                     menuItem("Booking Confirmation", tabName = "bookingConfirmation"), 
                     menuItem("Admin Login", tabName = "adminLogin"), 
                     menuItem("Admin View", tabName = "adminView")
                   )), 
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"), 
      # tags$style(HTML("
      #                 body {
      #                   background-color: black;
      #                 }
      #                 nav.navbar.navbar-static-top {
      #                   background-color: black;
      #                   color: white;
      #                   font-weight: bold;
      #                 }
      #                 .navbar-title {
      #                   color: white;
      #                   font-weight: bold;
      #                 }
      #                 .service-selection {
      #                   display: flex;
      #                   justify-content: space-around;
      #                   flex-wrap: wrap;
      #                   background-color: black;
      #                   color: white;
      #                   text-align: center;
      #                   font-size: 24px;
      #                   font-weight: bold;
      #                 }
      #                 .service-btn {
      #                   width: 100%;
      #                   max-width: 600px;
      #                   margin: 10px;
      #                   border: none;
      #                   padding: 0;
      #                   overflow: hidden;
      #                   position: relative;
      #                   cursor: pointer;
      #                   flex: 1;
      #                 }
      #                 .service-btn-box {
      #                   width: 100%;
      #                   height: 1000px;
      #                   display: flex;
      #                   justify-content: center;
      #                   align-items: center;
      #                 }
      #                 .service-btn img {
      #                   width: 100%;
      #                   height: auto;
      #                   display: block;
      #                 }
      #                 .box-title {
      #                   text-align: center;
      #                   font-size: 24px;
      #                 }
      #                 nav.navbar.navbar-static-top {
      #                   color: white;
      #                   
      #                 }
      #                 .box-header {
      #                   text-align: center;
      #                   font-size: 24px;
      #                   background-color: black;
      #                   color: white;
      #                 }
      #                 .box-body {
      #                   text-align: center;
      #                   font-size: 24px;
      #                   background-color: black;
      #                   color: white;
      #                 }
      #                 .navbar navbar-static-top {
      #                   text-align: center;
      #                   font-size: 24px;
      #                   background-color: black;
      #                   color: white;
      #                 }
      #                 header.main-header {
      #                   background-color: black;
      #                   color: white;
      #                 }
      #                 "))
    ),
    tabItems(
      tabItem(tabName = "serviceSelect", 
              fluidRow(
                column(width = 12, 
                  box(
                    title = div(class = "box-title", "Select a service"), 
                    width = 12,
                    div(class = "service-selection",
                        column(width = 6,
                              actionLink("pottery_painting",
                                  div(class = "service-btn service-btn-box",
                                      img(src = "cowplate.png")
                                      ))
                              ),
                        column(width = 6,
                       actionLink("evening_event",
                                  div(class = "service-btn service-btn-box",
                                      img(src = "privateparty.png")
                                      ))
                        )
                    )
                  )
                
                
            )
          )),
      tabItem(tabName = "dateTimeSelect", 
              fluidRow(
                column(width = 8, offset = 2, 
                uiOutput("dateUI"), 
                uiOutput("timeSlotUI"), 
                actionButton("next2", "Enter Details")
                )
              )), 
      tabItem(tabName = "selectEvent", 
              fluidRow(
                column(width = 8, offset = 2,
                box(
                  title = "Select an Event", 
                  DTOutput("eventsTable"), 
                  actionButton("next3", "Next")
                )
                )
              )),
      tabItem(tabName = "eventBookingInfo", 
              fluidRow(
                column(width = 8, offset = 2,
                textInput("customer_name", "Name:"), 
                textInput("email", "Email:"), 
                textInput("phone_number", "Phone Number (optional):"), 
                numericInput("guests", "Number of Guests:", value = 1, min = 1), 
                actionButton("next4", "Next")
                )
              )),
      tabItem(tabName = "customerInfo", 
              fluidRow(
                column(width = 8, offset = 2,
                textInput("customer_name", "Name:"), 
                textInput("email", "Email:"), 
                textInput("phone_number", "Phone Number (Optional)"), 
                numericInput("guests", "Number of Guests:", value = 1, min = 1), 
                actionButton("book", "Book Now")
                )
              )),
      tabItem(tabName = "eventConfirmation", 
              fluidRow(
                column(width = 8, offset = 2,
                box(
                  title = "Event Booking Confirmation", 
                  h3("Your event booking has been submitted!"), 
                  p("You will soon receive an email from our staff regarding your booking fee.")
                )
                )
              )), 
      tabItem(tabName = "bookingConfirmation", 
              fluidRow(
                column(width = 8, offset = 2,
                box(
                  title = "Booking Confirmation", 
                  h3(textOutput("confirmation_msg")), 
                  h4("Reference Number: ", textOutput("ref_number")), 
                  h4("Name: ", textOutput("conf_name")), 
                  h4("Service: ", textOutput("conf_service")), 
                  h4("Date: ", textOutput("conf_date")), 
                  h4("Time: ", textOutput("conf_time"))
                  )
                )
              )), 
      tabItem(tabName = "adminLogin", 
              fluidRow(
                column(width = 8, offset = 2,
                box(
                  title = "Admin Login", 
                  textInput("admin_user", "Username:"), 
                  passwordInput("admin_pass", "Password:"), 
                  actionButton("login", "Login")
                )
                )
              )), 
      tabItem(tabName = "adminView", 
              fluidRow(
                column(width = 8, offset = 2,
                box(
                  title = "Admin - View Bookings", 
                  tableOutput("bookingTable")
                ), 
                box(
                  title = "Search Bookings By Reference Number:", 
                  textInput("search_ref_number", "Reference Number:"), 
                  actionButton("search_booking", "Search")
                ),
                box(
                  title = "Business Closures", 
                  dateInput("closure_date", "Choose a date:"), 
                  selectInput("closure_start_time", "Choose start time:", 
                              choices = c("All Day", format(seq(strptime("00:00", "%H:%M"), strptime("23:45", "%H:%M"), by = "15 mins"), "%I:%M %p")),
                              selected = "All Day"), 
                  selectInput("closure_end_time", "Choose end time:", 
                              choices = c("All Day", format(seq(strptime("00:00", "%H:%M"), strptime("23:45", "%H:%M"), by = "15 mins"), "%I:%M %p")),
                              selected = "All Day"),
                  actionButton("add_closure", "Add Closure"), 
                  DTOutput("closuresTable")
                ), 
                box(
                  title = "Create an Upcoming Event", 
                  textInput("event_name", "Event Name:"), 
                  textAreaInput("event_description", "Event Description:"), 
                  dateInput("event_date", "Event Date:"), 
                  selectInput("event_start_time", "Event Start Time:", 
                              choices = format(seq(strptime("00:00", "%H:%M"), strptime("23:45", "%H:%M"), by = "15 mins"), "%I:%M %p")), 
                  numericInput("event_duration", "Event Duration (minutes):", value = 240, min = 1), 
                  numericInput("event_capacity", "Event Capacity:", value = 24, min = 1),
                  actionButton("add_event", "Add Event")
                )
              ))
      )
    
      )
    )
  )
)
server <- function(input, output, session) {
  
  observeEvent(input$pottery_painting, {
    updateTabItems(session, "tabs", "dateTimeSelect")
    updateSelectInput(session, "service", selected = "Pottery Painting")
  })
  
  observeEvent(input$evening_event, {
    updateTabItems(session, "tabs", "selectEvent")
    updateSelectInput(session, "service", selected = "Evening Event Registration")
  })
  output$dateUI <- renderUI({
    
    if (input$service == "Pottery Painting") {
      dateInput("date", "Choose a date:")
    }
  })
  
  output$timeSlotUI <- renderUI({
    if (input$service == "Pottery Painting" && !is.null(input$date)) {
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
  
  observeEvent(input$next1, {
    if (input$service == "Pottery Painting") {
      updateTabItems(session, "tabs", "dateTimeSelect")
    } else if (input$service == "Evening Event Registration") {
      updateTabItems(session, "tabs", "selectEvent")
    }
  })  
  
  output$eventsTable <- renderDT({ 
    dbGetQuery(conn, "SELECT event_name AS 'Event Name', event_date AS 'Date', event_start_time AS 'Time', event_description AS 'Description' FROM events WHERE event_date >= DATE('now')")
    }, options = list(pageLength = 10, autoWidth = TRUE))
  
  observeEvent(input$next3, {
    updateTabItems(session, "tabs", "eventBookingInfo")
  })
  
  observeEvent(input$next4, {
    selected_event <- dbGetQuery(conn, "SELECT event_name, event_date, event_start_time, event_capacity FROM events WHERE event_name = ?", 
                                 params = list(input$service))
    
    total_guests <- dbGetQuery(conn, "SELECT SUM(guests) as total_guests FROM event_bookings WHERE event_name = ? AND event_date = ? AND event_time = ?", 
                               params = list(selected_event$event_name, selected_event$event_date, selected_event$event_start_time))$total_guests
    
    if (is.na(total_guests)) {
      total_guests <- 0
    }
    
    remaining_capacity <- selected_event$event_capacity - total_guests
    
    if (input$guests <= remaining_capacity) {
      dbExecute(conn, "INSERT INTO event_bookings (event_name, event_date, event_time, customer_name, email, phone_number, guests)
                      VALUES (?, ?, ?, ?, ?, ?, ?)",
                params = list(selected_event$event_name, selected_event$event_date, selected_event$event_start_time, 
                              input$customer_name, input$email, input$phone_number, input$guests))
      updateTabItems(session, "tabs", "eventConfirmation")
    } else {
      showModal(modalDialog(
        title = "Booking Failed", 
        paste("Sorry, there are only", remaining_capacity, "spaces remaining for this event."), 
        easyClose = TRUE, 
        footer = NULL
      ))
    }
  })
  observeEvent(input$next2, {
      updateTabItems(session, "tabs", "customerInfo")
  })
  
  
    
  
  
  #Record the new booking: 
  observeEvent(input$book, {
    
    total_guests <- calculate_total_guests(input$date, input$timeSlot)
    
    if (total_guests + input$guests <= 19) {
    
      ref_number <- sample(1:1e6, 1)
      dbExecute(conn, "INSERT INTO bookings (service, date, time, customer_name, email, phone_number, guests)
                        VALUES (?, ?, ?, ?, ?, ?, ?)", 
                params = list(input$service, input$date, input$timeSlot, input$customer_name, input$email, input$phone_number, input$guests, ref_number))
      
      output$confirmation_msg <- renderText({"Thank you for your booking! We're looking forward to Kil'n Time with you!"})
      output$ref_number <- renderText({ref_number})
      output$conf_name <- renderText({input$customer_name})
      output$conf_service <- renderText({input$service})
      output$conf_date <- renderText({input$date})
      output$conf_time <- renderText({input$timeSlot})
      
      updateTabItems(session, "tabs", "bookingConfirmation")
      
      email <- create_email(input$customer_name, input$date, input$service, input$time, input$guests)
      smtp_send(
        email,
        from = "kilntimecafe@gmail.com", 
        to = input$email, 
        subject = "Booking Confirmation", 
        creds_file = "email_creds"
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
  })
    
  output$bookingTable <- renderTable({
    dbGetQuery(conn, "SELECT * FROM bookings")
  })
  
  observeEvent(input$add_closure, {
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
    dbGetQuery(conn, "SELECT closure_date, closure_time FROM closures WHERE closure_date >= DATE('now')")
  }, options = list(pageLength = 10, autoWidth = TRUE))
  
  observeEvent(input$admin_event, {
    formatted_event_date <- format(as.Date(input$event_date), "%Y-%m-%d")
    dbExecute(conn, "INSERT INTO events (event_name, event_description, event_date, event_start_time, event_capacity, event_duration)
                    VALUES (?, ?, ?, ?, ?, ?)", 
              params = list(input$event_name, event_description, event_date, event_start_time, event_capacity, event_duration))
    showModal(modalDialog(
      title = "Event Added", 
      paste("Event", input$event_name, "on", input$event_date, "at", input$event_start_time, "has been added."), 
      easyClose = TRUE, 
      footer = NULL
    ))
  })
  
  observeEvent(input$admin_login, {
    updateTabItems(session, "tabs", "adminLogin")
  })
  observeEvent(input$login, {
    user <- dbGetQuery(conn, "SELECT * FROM users WHERE username = ?", params = list(input$admin_user))
    if (nrow(user) == 1 && password_verify(user$password_hash, input$admin_pass)) {
      updateTabItems(session, "tabs", "adminView")
    } else{
      showModal(modalDialog(
        title = "Login Failed", 
        "Invalid username or password.", 
        easyClose = TRUE, 
        footer = NULL
      ))
    }
  })
  
  observeEvent(input$search_booking, {
    query <- sprintf("SELECT * FROM bookings WHERE ref_number = %d", input$search_ref_number)
    result <- dbGetQuery(conn, query)
    output$bookingTable <- renderDT({
      result
    })
  })
  output$bookingTable <- renderDT({
    dbGetQuery(conn, "SELECT * FROM bookings")
  })
}

shinyApp(ui, server)