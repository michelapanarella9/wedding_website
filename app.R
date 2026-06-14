# Entrée mapping (short label -> full description)
entree_map <- list(
  "Sirloin" = "Grilled top sirloin — peppercorn gravy, fried onions, mashed potato & asparagus",
  "Chicken" = "Cider brined roasted chicken supreme — herb mustard sauce, mashed potato & asparagus",
  "Salmon" = "Seared maple & Organic lager glazed salmon — tarragon cream sauce, mashed potato & asparagus",
  "Portobello" = "Portobello mushroom schnitzel — mushroom 'demi', fresh herb, mashed potato & asparagus"
)
# Entrée choices (full descriptions)
entree_choices <- c(
  "Grilled top sirloin — peppercorn gravy, fried onions, mashed potato & asparagus",
  "Cider brined roasted chicken supreme — herb mustard sauce, mashed potato & asparagus",
  "Seared maple & Organic lager glazed salmon — tarragon cream sauce, mashed potato & asparagus",
  "Portobello mushroom schnitzel — mushroom \"demi\", fresh herb, mashed potato & asparagus"
)
library(shiny)
library(mongolite)

app_password <- Sys.getenv("correct_password")
mongo_uri <- Sys.getenv("MONGO_URI")
print(Sys.getenv("MONGO_URI"))
db_name <- "wedding"
collection_name <- "rsvps"

make_mongo_connection <- function() {
  if (mongo_uri == "") {
    stop("MONGO_URI is empty. Set it in Posit Cloud environment variables.")
  }

  mongo(
    collection = collection_name,
    db = db_name,
    url = mongo_uri
  )
}

test_mongo_connection <- function() {
  tryCatch({
    con <- make_mongo_connection()
    n <- con$count()
    con$disconnect()
    message("MongoDB connection OK. Collection count: ", n)
    TRUE
  }, error = function(e) {
    message("MongoDB connection failed: ", e$message)
    FALSE
  })
}

conn_ok <- test_mongo_connection()

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

          tags$img(src = "pic.png", class = "hero-img"),

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
            choices = c("Joyfully Accept", "Regretfully Decline")
          ),

          numericInput(
            "guest_count",
            "Number Attending",
            value = 1,
            min = 1,
            max = 10
          ),

          selectInput(
            "entree",
            "Entrée Choice",
            choices = entree_choices,
            selected = NULL
          ),

          checkboxGroupInput(
            "dietary",
            "Dietary Restrictions (check all that apply)",
            choices = c("Vegetarian", "Vegan", "Gluten-Free", "Other")
          ),

          conditionalPanel(
            condition = "input.dietary && input.dietary.indexOf('Other') !== -1",
            textInput("dietary_other", "Other dietary restrictions (please specify)")
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
          tags$p("4:00 PM"),

          tags$br(),

          tags$p(class = "small-caps", "Cocktail Hour"),
          tags$p("4:15 PM – 6:00 PM"),

          tags$br(),

          tags$p(class = "small-caps", "Dinner & Dancing"),
          tags$p("6:00 PM – 11:45 PM"),

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
    if (app_password == "") {
      showModal(
        modalDialog(
          title = "Password not set",
          "The correct_password environment variable is missing.",
          easyClose = TRUE
        )
      )
    } else if (input$password == app_password) {
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
    req(input$entree)

    dietary_sel <- if (!is.null(input$dietary)) paste(input$dietary, collapse = "; ") else "None"
    dietary_other <- if (!is.null(input$dietary_other) && nzchar(input$dietary_other)) input$dietary_other else NA

    new_rsvp <- data.frame(
      name = input$guest_name,
      attendance = input$attendance,
      guest_count = input$guest_count,
      entree = input$entree,
      dietary_restrictions = dietary_sel,
      dietary_other = dietary_other,
      notes = input$message,
      submitted_at = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )

    tryCatch({
      con <- make_mongo_connection()
      con$insert(new_rsvp)
      con$disconnect()

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
