library(DBI)
library(RSQLite)
library(readxl)

# Path to the database file
db_path <- "data/users.db"

# Function to update the database with weekly accounts from Excel
update_weekly_accounts <- function(db_path, excel_file, format_mention) {
  # Connect to the SQLite database
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))  # Ensure the connection is closed
  
  # Check if the Excel file exists
  if (!file.exists(excel_file)) {
    stop("Error: The specified Excel file does not exist: ", excel_file)
  }
  
  # Read the Excel file
  tryCatch({
    account_data <- read_excel(excel_file)
  }, error = function(e) {
    stop("Error reading the Excel file: ", e$message)
  })
  
  # Validate that the required columns are present
  required_columns <- c("Konami", "Steam")
  missing_columns <- setdiff(required_columns, colnames(account_data))
  if (length(missing_columns) > 0) {
    stop("Error: Missing required column(s): ", paste(missing_columns, collapse = ", "))
  }
  
  # Insert or update each row into the database
  for (i in 1:nrow(account_data)) {
    konami <- account_data$Konami[i]
    steam <- account_data$Steam[i]
    
    # Perform an upsert (insert or update)
    dbExecute(con, "
      INSERT INTO accounts (account_id, passport, format, withdrawn, withdraw_date)
      VALUES (?, ?, ?, FALSE, NULL)
      ON CONFLICT(account_id) DO UPDATE SET
        passport = excluded.passport,
        format = excluded.format,
        withdrawn = excluded.withdrawn,
        withdraw_date = NULL
    ", params = list(konami, steam, format_mention))
  }
  
  message("Weekly accounts updated successfully.")
}

