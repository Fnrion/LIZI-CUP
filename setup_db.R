library(DBI)
library(RSQLite)

db_path <- "data/users.db"

initialize_database <- function(db_path) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))  # Ensure the connection is closed after running
  
  # Create users table if it does not exist
  dbExecute(con, "
  CREATE TABLE IF NOT EXISTS users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT UNIQUE NOT NULL,
    password_hash TEXT NOT NULL,
    withdrawn_account TEXT DEFAULT NULL
  )")
  
  # Create accounts table if it does not exist
  dbExecute(con, "
  CREATE TABLE IF NOT EXISTS accounts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    account_id TEXT UNIQUE NOT NULL,
    passport TEXT,
    withdrawn_by TEXT DEFAULT NULL,
    format TEXT DEFAULT 'weekly',
    withdrawn BOOLEAN DEFAULT FALSE,
    withdraw_date DATE DEFAULT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )")
  
  message("Database initialized successfully.")
}

# Run the initialization
initialize_database(db_path)

