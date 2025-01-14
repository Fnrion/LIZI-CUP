library(DBI)
library(RSQLite)
library(digest)

db_path <- "data/users.db"

initialize_admin_table <- function(db_path) {
  # Ensure the directory exists
  dir_path <- dirname(db_path)
  if (!dir.exists(dir_path)) {
    stop("Error: Directory does not exist: ", dir_path)
  }
  
  # Connect to the database
  con <- tryCatch({
    dbConnect(SQLite(), db_path)
  }, error = function(e) {
    stop("Error: Failed to connect to the database. ", e$message)
  })
  
  on.exit({
    if (dbIsValid(con)) dbDisconnect(con)
  })  # Ensure connection is closed
  
  # Create the admin table if it doesn't exist
  tryCatch({
    dbExecute(con, "
    CREATE TABLE IF NOT EXISTS admin (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      password_hash TEXT NOT NULL
    )")
  }, error = function(e) {
    stop("Error: Failed to create admin table. ", e$message)
  })
  
  # Add default admin if not present
  tryCatch({
    existing_admin <- dbGetQuery(con, "SELECT * FROM admin WHERE username = 'admin'")
    if (nrow(existing_admin) == 0) {
      hashed_password <- digest("admin123", algo = "sha256")
      dbExecute(con, "
      INSERT INTO admin (username, password_hash) VALUES (?, ?)",
                params = list("admin", hashed_password))
      message("Admin user created (username: admin, password: 123)")
    } else {
      message("Admin user already exists.")
    }
  }, error = function(e) {
    stop("Error: Failed to insert or verify admin user. ", e$message)
  })
  
  message("Admin table initialized successfully.")
}

# Run the admin table initialization
initialize_admin_table(db_path)
