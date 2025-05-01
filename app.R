library(shiny)
library(DBI)
library(RSQLite)
library(readxl)
library(digest)

# Initialize database -----------------------------------------
db_path <- "data/users.db"
dir.create("data", showWarnings = FALSE)

initialize_db <- function(db_path) {
  con <- dbConnect(SQLite(), db_path)
  
  # Create accounts table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS accounts (
      account_id TEXT PRIMARY KEY,
      passport TEXT,
      format TEXT,
      withdrawn BOOLEAN DEFAULT FALSE,
      withdraw_date TIMESTAMP DEFAULT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Create admin table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS admin (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      password_hash TEXT NOT NULL
    )
  ")
  
  # Insert default admin if not exists
  existing_admin <- dbGetQuery(con, "SELECT * FROM admin WHERE username = 'admin'")
  if (nrow(existing_admin) == 0) {
    hashed_password <- digest("admin123", algo = "sha256")
    dbExecute(con, "
      INSERT INTO admin (username, password_hash) VALUES (?, ?)
    ", params = list("admin", hashed_password))
  }
  
  dbDisconnect(con)
}

initialize_db(db_path)

# UI ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Weekly Account Management"),
  
  uiOutput("login_ui"),   # Login UI shown conditionally
  uiOutput("app_ui")      # Main app shown only if authenticated
)

# Server --------------------------------------------------------
server <- function(input, output, session) {
  db_path <- "data/users.db"
  user_session <- reactiveVal(NULL)
  data_trigger <- reactiveVal(Sys.time())
  
  # Login UI ----------------------------------------------------
  output$login_ui <- renderUI({
    if (is.null(user_session())) {
      tagList(
        h3("Admin Login"),
        textInput("username", "Username"),
        passwordInput("password", "Password"),
        actionButton("login_btn", "Log In"),
        verbatimTextOutput("login_status")
      )
    }
  })
  
  # Main App UI --------------------------------------------------
  output$app_ui <- renderUI({
    if (!is.null(user_session())) {
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            fileInput("file", "Upload Excel File", accept = ".xlsx"),
            textInput("format", "Format Mention", placeholder = "e.g., Week 12"),
            actionButton("upload", "Upload Accounts"),
            hr(),
            actionButton("withdraw", "Withdraw My Account"),
            actionButton("logout_btn", "Log Out")
          ),
          mainPanel(
            h3("Available Accounts This Week"),
            tableOutput("available_accounts"),
            verbatimTextOutput("status")
          )
        )
      )
    }
  })
  
  # Login logic --------------------------------------------------
  observeEvent(input$login_btn, {
    req(input$username, input$password)
    
    con <- dbConnect(SQLite(), db_path)
    user_data <- dbGetQuery(con, "SELECT * FROM admin WHERE username = ?", params = list(input$username))
    dbDisconnect(con)
    
    if (nrow(user_data) == 1) {
      input_hash <- digest(input$password, algo = "sha256")
      if (input_hash == user_data$password_hash[1]) {
        user_session(user_data$username[1])
        output$login_status <- renderText("")
      } else {
        output$login_status <- renderText("Incorrect password.")
      }
    } else {
      output$login_status <- renderText("User not found.")
    }
  })
  
  # Logout logic -------------------------------------------------
  observeEvent(input$logout_btn, {
    user_session(NULL)
    output$status <- renderText("You have been logged out.")
  })
  
  # Upload accounts ----------------------------------------------
  observeEvent(input$upload, {
    req(input$file, input$format, !is.null(user_session()))
    
    tryCatch({
      df <- read_excel(input$file$datapath)
      if (!all(c("Konami", "Steam") %in% colnames(df))) {
        output$status <- renderText("Error: Missing 'Konami' or 'Steam' columns.")
        return()
      }
      
      con <- dbConnect(SQLite(), db_path)
      for (i in 1:nrow(df)) {
        dbExecute(con, "
          INSERT INTO accounts (account_id, passport, format, withdrawn, withdraw_date, created_at)
          VALUES (?, ?, ?, FALSE, NULL, CURRENT_TIMESTAMP)
          ON CONFLICT(account_id) DO UPDATE SET
            passport = excluded.passport,
            format = excluded.format,
            withdrawn = FALSE,
            withdraw_date = NULL,
            created_at = CURRENT_TIMESTAMP
        ", params = list(df$Konami[i], df$Steam[i], input$format))
      }
      dbDisconnect(con)
      data_trigger(Sys.time())
      output$status <- renderText("Accounts uploaded successfully.")
    }, error = function(e) {
      output$status <- renderText(paste("Upload failed:", e$message))
    })
  })
  
  # Withdraw account ---------------------------------------------
  observeEvent(input$withdraw, {
    req(!is.null(user_session()))
    
    con <- dbConnect(SQLite(), db_path)
    start_of_week <- as.character(as.POSIXct(cut(Sys.time(), "week")))
    
    result <- dbGetQuery(con, "
      SELECT account_id FROM accounts
      WHERE withdrawn = FALSE AND created_at >= ?
      LIMIT 1
    ", params = list(start_of_week))
    
    if (nrow(result) > 0) {
      dbExecute(con, "
        UPDATE accounts
        SET withdrawn = TRUE, withdraw_date = CURRENT_TIMESTAMP
        WHERE account_id = ?
      ", params = list(result$account_id[1]))
      output$status <- renderText(paste(" You have withdrawn:", result$account_id[1]))
    } else {
      output$status <- renderText("⚠️ No available accounts to withdraw this week.")
    }
    
    dbDisconnect(con)
    data_trigger(Sys.time())
  })
  
  # Show available accounts --------------------------------------
  output$available_accounts <- renderTable({
    req(!is.null(user_session()))
    
    con <- dbConnect(SQLite(), db_path)
    start_of_week <- as.character(as.POSIXct(cut(Sys.time(), "week")))
    
    df <- dbGetQuery(con, "
      SELECT account_id, passport, format, created_at
      FROM accounts
      WHERE withdrawn = FALSE AND created_at >= ?
    ", params = list(start_of_week))
    
    dbDisconnect(con)
    df
  })
}

# Run the app ------------------------------------------------------
shinyApp(ui = ui, server = server)