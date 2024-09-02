library(serial)
library(shiny)
library(ggplot2)

# Initialize an empty df in server context
# Serial connection will be handled in server scope to ensure proper lifecycle management

ui <- fluidPage(
  # Application title
  titlePanel("Serial Connection Data"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
      actionButton("terminate_app", "Terminate App")
    ),
    mainPanel(
      verbatimTextOutput("serial_output"),
      plotOutput("scatter")
    )
  )
)

server <- function(input, output, session) {
  # Initialize plot log dataframe
  df <- data.frame(x = numeric(0), y = numeric(0))

  # Setup serial connection once
  con <- serialConnection(
    name = "con1",
    port = "COM3",
    mode = "9600,n,8,1",
    buffering = "none",
    newline = 0,
    eof = "",
    translation = "auto",
    handshake = "none",
    buffersize = 4096
  )

  open(con)

  # Function to read serial data
  read_serial_data <- function() {
    tryCatch({
      data <- read.serialConnection(con)  # Assuming you want to read 1 byte of data
      as.integer(data)
    }, error = function(e) {
      NA  # Return NA if there's an error
    })
  }

  # Reactive value to store serial data
  serial_data <- reactiveVal(NA)

  # Setup a reactive timer to trigger every 500 ms
  autoInvalidate <- reactiveTimer(500)

  #set global count variable - which should activate when nrow(df) exceeds 10
  count <<- 0

  observe({
    autoInvalidate()
    new_data <- read_serial_data()

    if (!is.na(new_data)) {
        # Calculate new x and y values
        new_x <- new_data * sin(36 * (nrow(df)+count) * pi / 180)
        new_y <- new_data * cos(36 * (nrow(df)+count) * pi / 180)

        # Update the dataframe
        df <<- rbind(df, data.frame(x = new_x, y = new_y))
        
        if (nrow(df)==11){
            df <<- df[-1,]
            #when dataframe reaches size of 10, next angle will consider count
            count <<- count + 1
        }

      serial_data(new_data)
    }
  })

  output$serial_output <- renderText({
    # Display data as text
    paste(serial_data(), collapse = ", ")
  })

  output$scatter <- renderPlot({
    autoInvalidate()

    ggplot(df, aes(x, y)) +
      geom_point() +
      labs(x = "lol", y = "distance") +
      scale_y_continuous(limits = c(-200, 200), breaks = seq(-200, 200, by = 100)) +
      scale_x_continuous(limits = c(-200, 200), breaks = seq(-200, 200, by = 100))
  })

  observeEvent(input$terminate_app, {
    close(con)
    stopApp()
  })
}

shinyApp(ui = ui, server = server)