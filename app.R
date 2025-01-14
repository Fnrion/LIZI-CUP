library(shiny)
library(DBI)
library(RSQLite)
library(digest)

source("setup_admin.R")
source("setup_db.R")
source("update_db.R")

db_path <- "data/users.db"

# UI Section
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .navbar-custom {
        display: flex;
        justify-content: space-between;
        align-items: center;
        width: 100%;
      }
      .navbar-custom .navbar-header {
        flex-grow: 1;
      }
      .language-dropdown {
        position: absolute;
        top: 10px;
        right: 20px;
      }
    "))
  ),
  uiOutput("app_title"),
  
  navbarPage(
    title = div(class = "navbar-custom",
                div(style = "margin-right: 20px;",
                    selectInput("lang", NULL, 
                                choices = c("English" = "en", "中文" = "zh"), 
                                selected = "en",
                                width = "150px")
                )
    ),
    tabsetPanel(
      tabPanel(uiOutput("tab_signup"),
               textInput("signup_user", uiOutput("signup_user_label")),
               passwordInput("signup_pass", uiOutput("signup_pass_label")),
               actionButton("signup_btn", uiOutput("signup_btn_label")),
               textOutput("signup_status")
      ),
      tabPanel(uiOutput("tab_login"),
               textInput("login_user", uiOutput("login_user_label")),
               passwordInput("login_pass", uiOutput("login_pass_label")),
               actionButton("login_btn", uiOutput("login_btn_label")),
               textOutput("login_status")
      ),
      tabPanel(uiOutput("tab_withdraw"),
               uiOutput("restricted_ui")
      ),
      tabPanel(uiOutput("tab_recheck"),
               actionButton("recheck_btn", uiOutput("recheck_btn_label")),
               tableOutput("withdraw_history")
      ),
      tabPanel(uiOutput("tab_admin"),
               textInput("admin_user", uiOutput("admin_user_label")),
               passwordInput("admin_pass", uiOutput("admin_pass_label")),
               actionButton("admin_login_btn", uiOutput("admin_login_btn_label")),
               textOutput("admin_login_status"),
               h4("Admin Actions"),
               fileInput("admin_upload_file", "Upload New Accounts Dataset", accept = c(".xlsx")),
               actionButton("upload_accounts_btn", "Upload Dataset"),
               textOutput("upload_status"),
               actionButton("view_taken_accounts_btn", "View Taken Accounts"),
               tableOutput("taken_accounts_table")
      )
    )
  )
)

# Server Section
server <- function(input, output, session) {
  user_session <- reactiveValues(logged_in = FALSE, username = NULL, is_admin = FALSE)
  
  # Translation dictionary
  translations <- list(
    "en" = list(
      app_title = "LIZICUP",
      tab_signup = "Sign-Up",
      tab_login = "Login",
      tab_withdraw = "Withdraw Account",
      tab_recheck = "Recheck Withdrawals",
      tab_admin = "Admin Login",
      signup_user = "Choose a Username:",
      signup_pass = "Choose a Password:",
      signup_btn = "Sign-Up",
      login_user = "Username:",
      login_pass = "Password:",
      login_btn = "Login",
      recheck_btn = "Recheck Withdrawal History",
      admin_user = "Admin Username:",
      admin_pass = "Admin Password:",
      admin_login_btn = "Admin Login",
      restricted_msg = "Please log in to withdraw an account.",
      withdraw_title = "Withdraw Your Konami and Steam Account"
    ),
    "zh" = list(
      app_title = "栗子杯",
      tab_signup = "注册",
      tab_login = "登录",
      tab_withdraw = "领取账号",
      tab_recheck = "查看已有账号",
      tab_admin = "管理员登录",
      signup_user = "用户名：",
      signup_pass = "密码：",
      signup_btn = "注册",
      login_user = "用户名：",
      login_pass = "密码：",
      login_btn = "登录",
      recheck_btn = "查看历史账号",
      admin_user = "用户名：",
      admin_pass = "密码：",
      admin_login_btn = "管理员登录",
      restricted_msg = "请登录以查看。",
      withdraw_title = "提取账号"
    )
  )
  
  # Dynamic translations
  selected_lang <- reactive({ req(input$lang); translations[[input$lang]] })
  
  # Dynamic title
  output$app_title <- renderUI({ titlePanel(selected_lang()$app_title) })
  
  # Dynamic tab labels
  output$tab_signup <- renderText(selected_lang()$tab_signup)
  output$tab_login <- renderText(selected_lang()$tab_login)
  output$tab_withdraw <- renderText(selected_lang()$tab_withdraw)
  output$tab_recheck <- renderText(selected_lang()$tab_recheck)
  output$tab_admin <- renderText(selected_lang()$tab_admin)
  
  # Dynamic input labels
  output$signup_user_label <- renderText(selected_lang()$signup_user)
  output$signup_pass_label <- renderText(selected_lang()$signup_pass)
  output$signup_btn_label <- renderText(selected_lang()$signup_btn)
  output$login_user_label <- renderText(selected_lang()$login_user)
  output$login_pass_label <- renderText(selected_lang()$login_pass)
  output$login_btn_label <- renderText(selected_lang()$login_btn)
  output$recheck_btn_label <- renderText(selected_lang()$recheck_btn)
  output$admin_user_label <- renderText(selected_lang()$admin_user)
  output$admin_pass_label <- renderText(selected_lang()$admin_pass)
  output$admin_login_btn_label <- renderText(selected_lang()$admin_login_btn)
  
  # Sign-Up Logic
  observeEvent(input$signup_btn, {
    username <- input$signup_user
    password <- input$signup_pass
    
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    
    if (username == "" || password == "") {
      output$signup_status <- renderText("Please fill out both fields.")
      return()
    }
    
    existing_user <- dbGetQuery(con, "SELECT * FROM users WHERE username = ?", params = list(username))
    if (nrow(existing_user) > 0) {
      output$signup_status <- renderText("Username already exists.")
    } else {
      hashed_password <- digest(password, algo = "sha256")
      dbExecute(con, "INSERT INTO users (username, password_hash) VALUES (?, ?)", 
                params = list(username, hashed_password))
      output$signup_status <- renderText("Sign-Up successful!")
    }
  })
  
  # User Login Logic
  observeEvent(input$login_btn, {
    username <- input$login_user
    password <- input$login_pass
    
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    
    hashed_password <- digest(password, algo = "sha256")
    user <- dbGetQuery(con, "SELECT * FROM users WHERE username = ? AND password_hash = ?", 
                       params = list(username, hashed_password))
    
    if (nrow(user) > 0) {
      user_session$logged_in <- TRUE
      user_session$username <- username
      output$login_status <- renderText("Login successful!")
    } else {
      output$login_status <- renderText("Invalid username or password.")
    }
  })
  
  # Admin Login Logic
  observeEvent(input$admin_login_btn, {
    username <- input$admin_user
    password <- input$admin_pass
    
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    
    hashed_password <- digest(password, algo = "sha256")
    admin <- dbGetQuery(con, "SELECT * FROM admin WHERE username = ? AND password_hash = ?", 
                        params = list(username, hashed_password))
    
    if (nrow(admin) > 0) {
      user_session$logged_in <- TRUE
      user_session$username <- username
      user_session$is_admin <- TRUE
      output$admin_login_status <- renderText("Admin login successful!")
    } else {
      output$admin_login_status <- renderText("Invalid admin credentials.")
    }
  })
  
  # Withdraw Account Logic
  observeEvent(input$withdraw_btn, {
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    
    username <- user_session$username
    recent_withdrawal <- dbGetQuery(con, "
      SELECT * FROM accounts WHERE withdrawn_by = ? AND withdraw_date >= date('now', '-7 days')",
                                    params = list(username))
    
    if (nrow(recent_withdrawal) > 0) {
      output$withdraw_status <- renderTable(data.frame(
        Message = "You can only withdraw one account per week."
      ))
    } else {
      result <- dbGetQuery(con, "
        SELECT account_id, passport FROM accounts
        WHERE withdrawn_by IS NULL AND withdrawn = FALSE LIMIT 1")
      
      if (nrow(result) > 0) {
        account_id <- result$account_id
        dbExecute(con, "
          UPDATE accounts SET withdrawn_by = ?, withdrawn = TRUE, withdraw_date = date('now')
          WHERE account_id = ?", params = list(username, account_id))
        
        output$withdraw_status <- renderTable(result)
      } else {
        output$withdraw_status <- renderTable(data.frame(
          Message = "No accounts available for withdrawal."
        ))
      }
    }
  })
  
  # Admin Upload Logic
  observeEvent(input$upload_accounts_btn, {
    if (!user_session$is_admin) {
      showNotification("Admin login required to upload datasets.", type = "error")
      return()
    }
    
    if (is.null(input$admin_upload_file)) {
      output$upload_status <- renderText("No file uploaded. Please select a file.")
      return()
    }
    
    # Get the file path of the uploaded file
    file_path <- input$admin_upload_file$datapath
    
    # Call `update_weekly_accounts` and handle errors
    tryCatch({
      format_mention <- "Admin Upload"  # Optional label for admin uploads
      update_weekly_accounts(db_path, file_path, format_mention)
      output$upload_status <- renderText("Dataset uploaded and accounts updated successfully.")
    }, error = function(e) {
      output$upload_status <- renderText(paste("Error during upload:", e$message))
    })
  })
  
  # Recheck Withdrawals
  observeEvent(input$recheck_btn, {
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    
    username <- user_session$username
    history <- dbGetQuery(con, "
      SELECT account_id, passport, withdraw_date
      FROM accounts WHERE withdrawn_by = ? ORDER BY withdraw_date DESC",
                          params = list(username))
    
    output$withdraw_history <- renderTable({
      if (nrow(history) > 0) {
        history
      } else {
        data.frame(Message = "No past withdrawals found.")
      }
    })
  })
  
  # View Taken Accounts (Admin-Only)
  observeEvent(input$view_taken_accounts_btn, {
    if (!user_session$is_admin) {
      showNotification("Admin login required to view taken accounts.", type = "error")
      return()
    }
    
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))
    
    taken_accounts <- dbGetQuery(con, "
      SELECT account_id, passport, withdrawn_by, withdraw_date
      FROM accounts WHERE withdrawn = TRUE ORDER BY withdraw_date DESC")
    
    output$taken_accounts_table <- renderTable({
      if (nrow(taken_accounts) > 0) {
        taken_accounts
      } else {
        data.frame(Message = "No accounts have been withdrawn yet.")
      }
    })
  })
  
  # Withdraw Account UI
  output$restricted_ui <- renderUI({
    if (user_session$logged_in) {
      tagList(
        h3(paste(selected_lang()$withdraw_title, "for", user_session$username)),
        actionButton("withdraw_btn", "Withdraw Account"),
        tableOutput("withdraw_status")
      )
    } else {
      h3(selected_lang()$restricted_msg)
    }
  })
}

shinyApp(ui = ui, server = server)