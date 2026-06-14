library(shiny)
library(mongolite)

# Environment variables
app_password <- Sys.getenv("correct_password")

# get connection string from environment (prefer MONGO_URI, then mongodb_uri, then localhost)
mongo_uri <- Sys.getenv("MONGO_URI")

# if your URI does NOT include the database, set db name here:
db_name <- "wedding"          # change to your DB
collection_name <- "rsvp"

# create connection


# Diagnostic: test MongoDB connection early and print helpful message on failure
conn_ok <- FALSE
tryCatch({
  tmp <- mongo(collection = collection_name, db = db_name, url = mongo_uri)
  n <- tmp$count()
  tmp$disconnect()
  conn_ok <- TRUE
  message("MongoDB connection OK; collection '", collection_name, "' count: ", n)
}, error = function(e) {
  message("MongoDB connection failed: ", e$message)
  message("Tried URI: ", substr(mongo_uri, 1, 200))
})

# Create the collection handle used by the app (this may still error if auth is incorrect)
rsvp_collection <- tryCatch(
  mongo(collection = "rsvps", db = "wedding", url = mongo_uri),
  error = function(e) {
    stop("Failed to create 'rsvp_collection': ", e$message)
  }
)

ui <- fluidPage(

  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Cormorant+Garamond:wght@300;400;600&family=Montserrat:wght@300;400;500&display=swap"
    ),

    tags$style(HTML("
      body {
        background-color: #ffffff;
        color: #000000;
        font-family: 'Montserrat', sans-serif;
      }

      .container-fluid {
        max-width: 1200px;
        margin: auto;
      }

      .main-title {
        font-family: 'Cormorant Garamond', serif;
        font-size: 72px;
        letter-spacing: 5px;
        font-weight: 300;
        margin-top: 35px;
        margin-bottom: 5px;
      }

      .subtitle {
        font-family: 'Cormorant Garamond', serif;
        font-size: 30px;
        letter-spacing: 2px;
        font-weight: 300;
      }

      .small-caps {
        font-size: 14px;
        letter-spacing: 3px;
        text-transform: uppercase;
      }

      .divider {
        width: 220px;
        border-top: 1px solid #000000;
        margin: 28px auto;
      }

      .hero-img {
        max-width: 850px;
        width: 100%;
        border-radius: 3px;
        margin-top: 25px;
        margin-bottom: 20px;
      }

      .section-title {
        font-family: 'Cormorant Garamond', serif;
        font-size: 38px;
        letter-spacing: 2px;
        font-weight: 400;
        margin-bottom: 20px;
      }

      .card {
        background: #ffffff;
        border: 1px solid #d9d9d9;
        padding: 32px;
        border-radius: 4px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.05);
        margin-bottom: 30px;
      }

      .details-card {
        border-left: 1px solid #000000;
        padding-left: 35px;
      }

      .form-control {
        border-radius: 0;
        border: 1px solid #bfbfbf;
        box-shadow: none;
        font-family: 'Montserrat', sans-serif;
      }

      .form-control:focus {
        border-color: #000000;
        box-shadow: none;
      }

      label {
        font-weight: 400;
        letter-spacing: 1px;
        text-transform: uppercase;
        font-size: 12px;
      }

      .btn-default {
        background-color: #000000;
        color: #ffffff;
        border: 1px solid #000000;
        border-radius: 0;
        padding: 11px 26px;
        letter-spacing: 2px;
        text-transform: uppercase;
        font-size: 12px;
        margin-top: 10px;
      }

      .btn-default:hover,
      .btn-default:focus {
        background-color: #ffffff;
        color: #000000;
        border: 1px solid #000000;
      }

      .password-box {
        max-width: 420px;
        margin: 100px auto;
        text-align: center;
        border: 1px solid #d9d9d9;
        padding: 40px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.05);
      }

      .footer {
        text-align: center;
        margin-top: 50px;
        margin-bottom: 30px;
        font-family: 'Cormorant Garamond', serif;
        font-size: 22px;
        letter-spacing: 2px;
      }

      table {
        font-family: 'Montserrat', sans-serif;
      }

      @media only screen and (max-width: 768px) {
        .main-title {
          font-size: 46px;
          letter-spacing: 3px;
        }

        .subtitle {
          font-size: 24px;
        }

        .details-card {
          border-left: none;
          padding-left: 0;
          margin-top: 30px;
        }
      }
    "))
  ),

  uiOutput("password_ui"),

  conditionalPanel(
    condition = "output.authenticated == true",

    fluidRow(
      column(
        width = 12,
        div(
          style = "text-align:center;",

          tags$img(
            src = "pic.png",
            class = "hero-img"
          ),

          div(class = "main-title", "MICHELA & STEPHEN"),

          div(class = "divider"),

          div(class = "subtitle", "Together with their families"),

          br(),

          div(class = "small-caps", "January 16, 2027"),

          br(),

          tags$p(
            "Mill Street Brew Pub · Ottawa",
            style = "font-size:18px; letter-spacing:2px;"
          ),

          div(class = "divider")
        )
      )
    ),

    fluidRow(
      column(
        width = 5,

        div(
          class = "card",

          div(class = "section-title", "RSVP"),

          textInput("guest_name", "Your Name"),

          selectInput(
            "attendance",
            "Will You Attend?",
            choices = c(
              "Joyfully Accept",
              "Regretfully Decline"
            )
          ),

          numericInput(
            "guest_count",
            "Number Attending",
            value = 1,
            min = 1,
            max = 10
          ),

          selectInput(
            "meal",
            "Meal Preference",
            choices = c(
              "No Preference",
              "Vegetarian",
              "Vegan",
              "Gluten-Free",
              "Other"
            )
          ),

          textAreaInput(
            "message",
            "Message or Dietary Notes",
            height = "110px"
          ),

          actionButton("submit_rsvp", "Submit RSVP")
        )
      ),

      column(
        width = 7,

        div(
          class = "details-card",

          div(class = "section-title", "Wedding Details"),

          tags$p(class = "small-caps", "Ceremony"),
          tags$p("3:00 PM"),

          tags$br(),

          tags$p(class = "small-caps", "Cocktail Hour"),
          tags$p("3:15 PM – 5:00 PM"),

          tags$br(),

          tags$p(class = "small-caps", "Dinner & Dancing"),
          tags$p("3:00 PM – Midnight"),

          tags$br(),

          tags$p(class = "small-caps", "Location"),
          tags$p("Mill Street Brew Pub"),
          tags$p("555 Wellington St, Ottawa, ON K1R 1C5"),

          div(class = "divider"),

          div(class = "section-title", "Your RSVP"),
          tableOutput("rsvp_preview")
        )
      )
    ),

    div(class = "footer", "We cannot wait to celebrate with you.")
  )
)

server <- function(input, output, session) {

  auth <- reactiveVal(FALSE)
  latest_rsvp <- reactiveVal(NULL)

  output$password_ui <- renderUI({
    if (!auth()) {
      div(
        class = "password-box",

        div(class = "section-title", "Michela & Stephen"),

        tags$p(
          "Please enter the wedding website password.",
          style = "letter-spacing:1px;"
        ),

        passwordInput("password", "Password"),

        actionButton("login", "Enter")
      )
    }
  })

  observeEvent(input$login, {
    if (input$password == app_password && app_password != "") {
      auth(TRUE)
    } else {
      showModal(
        modalDialog(
          title = "Access Denied",
          "Incorrect password. Please try again.",
          easyClose = TRUE
        )
      )
    }
  })

  output$authenticated <- reactive({
    auth()
  })

  outputOptions(output, "authenticated", suspendWhenHidden = FALSE)

  observeEvent(input$submit_rsvp, {
    req(input$guest_name)

    new_rsvp <- data.frame(
      name = input$guest_name,
      attendance = input$attendance,
      guest_count = input$guest_count,
      meal = input$meal,
      notes = input$message,
      submitted_at = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )

    tryCatch({

      rsvp_collection$insert(new_rsvp)

      latest_rsvp(new_rsvp)

      showModal(
        modalDialog(
          title = "Thank You",
          "Your RSVP has been received.",
          easyClose = TRUE
        )
      )

    }, error = function(e) {

      showModal(
        modalDialog(
          title = "Something went wrong",
          paste("Your RSVP could not be saved:", e$message),
          easyClose = TRUE
        )
      )

    })
  })

  output$rsvp_preview <- renderTable({
    latest_rsvp()
  })
}

shinyApp(ui = ui, server = server)
