# Konfigurasi koneksi database
db_config <- list(
  dbname = "logindummy",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "12345678"
)

# Fungsi untuk mendapatkan koneksi database
get_db_connection <- function() {
  con <- dbConnect(
    PostgreSQL(),
    dbname = db_config$dbname,
    host = db_config$host,
    port = db_config$port,
    user = db_config$user,
    password = db_config$password
  )
  return(con)
}