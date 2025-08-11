source("utils.R")
source("configs/config_db.R")
source("ui.R")
library(DT)

# Fungsi untuk memeriksa apakah akun admin sudah ada
check_admin_exists <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  query <- "SELECT COUNT(*) FROM users WHERE role = 'admin'"
  result <- dbGetQuery(con, query)
  
  return(result$count > 0)
}

# Fungsi untuk membuat akun admin
create_admin <- function(username, password) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  # Pastikan tidak ada admin lain
  if (check_admin_exists()) {
    return(FALSE)
  }
  
  # Hash password sebelum menyimpan
  hashed_password <- hashpw(password, gensalt())
  query <- "INSERT INTO users (username, password, role) VALUES ($1, $2, 'admin')"
  tryCatch({
    dbExecute(con, query, params = list(username, hashed_password))
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Fungsi untuk memeriksa kredensial dan mengembalikan role
check_credentials <- function(username, password) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  query <- "SELECT password, role FROM users WHERE username = $1"
  result <- dbGetQuery(con, query, params = list(username))
  
  if (nrow(result) > 0 && checkpw(password, result$password[1])) {
    return(list(is_valid = TRUE, role = result$role[1]))
  }
  return(list(is_valid = FALSE, role = NULL))
}

# Fungsi untuk menambah pengguna baru (hanya role 'user')
add_user <- function(username, password) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  # Hash password sebelum menyimpan
  hashed_password <- hashpw(password, gensalt())
  query <- "INSERT INTO users (username, password, role) VALUES ($1, $2, 'user')"
  tryCatch({
    dbExecute(con, query, params = list(username, hashed_password))
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Fungsi untuk menghapus pengguna
delete_user <- function(user_id, current_username) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  # Jangan izinkan menghapus akun admin atau pengguna yang sedang login
  query <- "SELECT username, role FROM users WHERE id = $1"
  user <- dbGetQuery(con, query, params = list(user_id))
  
  if (nrow(user) > 0 && (user$role[1] == "admin" || user$username[1] == current_username)) {
    return(FALSE) # Tidak boleh menghapus admin atau diri sendiri
  }
  
  query <- "DELETE FROM users WHERE id = $1"
  tryCatch({
    dbExecute(con, query, params = list(user_id))
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Fungsi untuk mendapatkan daftar pengguna
get_users <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  query <- "SELECT id, username, role, created_at FROM users"
  dbGetQuery(con, query)
}

server <- function(input, output, session) {
  # Reactive values untuk status login, role admin, username saat ini, dan ID pengguna untuk penghapusan
  logged_in <- reactiveVal(FALSE)
  admin_exists <- reactiveVal(check_admin_exists())
  is_admin <- reactiveVal(FALSE)
  current_username <- reactiveVal(NULL)
  user_to_delete <- reactiveVal(NULL)
  
  # Handle pembuatan akun admin
  observeEvent(input$create_admin_button, {
    if (input$admin_password != input$admin_password_confirm) {
      showNotification("Passwords do not match", type = "error")
    } else if (nchar(input$admin_username) == 0 || nchar(input$admin_password) == 0) {
      showNotification("Username and password cannot be empty", type = "error")
    } else {
      success <- create_admin(input$admin_username, input$admin_password)
      if (success) {
        admin_exists(TRUE)
        showNotification("Admin account created successfully", type = "message")
      } else {
        showNotification("Failed to create admin account. Username may already exist or another admin exists.", type = "error")
      }
    }
  })
  
  # Handle login button click
  observeEvent(input$login_button, {
    credentials <- check_credentials(input$username, input$password)
    if (credentials$is_valid) {
      logged_in(TRUE)
      is_admin(credentials$role == "admin")
      current_username(input$username)
    } else {
      showNotification("Invalid username or password", type = "error")
    }
  })
  
  # Handle logout button click
  observeEvent(input$logout_button, {
    logged_in(FALSE)
    is_admin(FALSE)
    current_username(NULL)
    showNotification("Logged out successfully", type = "message")
  })
  
  # Handle penambahan pengguna
  observeEvent(input$add_user_button, {
    if (!is_admin()) {
      showNotification("Only admins can add users", type = "error")
      return()
    }
    if (input$new_password != input$new_password_confirm) {
      showNotification("Passwords do not match", type = "error")
    } else if (nchar(input$new_username) == 0 || nchar(input$new_password) == 0) {
      showNotification("Username and password cannot be empty", type = "error")
    } else {
      success <- add_user(input$new_username, input$new_password)
      if (success) {
        showNotification("User added successfully", type = "message")
        # Perbarui tabel pengguna
        output$user_table <- renderDT({
          users <- get_users()
          users$Actions <- sapply(users$id, function(id) {
            paste0('<button onclick="Shiny.setInputValue(\'delete_user_button\', ', id, ', {priority: \'event\'})">Delete</button>')
          })
          datatable(users, escape = FALSE, options = list(pageLength = 10))
        })
        # Kosongkan field input
        updateTextInput(session, "new_username", value = "")
        updateTextInput(session, "new_password", value = "")
        updateTextInput(session, "new_password_confirm", value = "")
      } else {
        showNotification("Failed to add user. Username may already exist.", type = "error")
      }
    }
  })
  
  # Handle klik tombol delete untuk menampilkan konfirmasi
  observeEvent(input$delete_user_button, {
    if (!is_admin()) {
      showNotification("Only admins can delete users", type = "error")
      return()
    }
    # Simpan ID pengguna yang akan dihapus
    user_to_delete(input$delete_user_button)
    # Ambil informasi pengguna untuk ditampilkan di konfirmasi
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    query <- "SELECT username FROM users WHERE id = $1"
    user <- dbGetQuery(con, query, params = list(input$delete_user_button))
    username <- if (nrow(user) > 0) user$username[1] else "Unknown"
    
    # Tampilkan modal konfirmasi
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete user", username, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_button", "Confirm", class = "btn-danger")
      )
    ))
  })
  
  # Handle konfirmasi penghapusan pengguna
  observeEvent(input$confirm_delete_button, {
    user_id <- user_to_delete()
    if (!is.null(user_id)) {
      success <- delete_user(user_id, current_username())
      if (success) {
        showNotification("User deleted successfully", type = "message")
        # Perbarui tabel pengguna
        output$user_table <- renderDT({
          users <- get_users()
          users$Actions <- sapply(users$id, function(id) {
            paste0('<button onclick="Shiny.setInputValue(\'delete_user_button\', ', id, ', {priority: \'event\'})">Delete</button>')
          })
          datatable(users, escape = FALSE, options = list(pageLength = 10))
        })
      } else {
        showNotification("Failed to delete user. Cannot delete admin or self.", type = "error")
      }
    }
    # Kosongkan user_to_delete dan tutup modal
    user_to_delete(NULL)
    removeModal()
  })
  
  # Render UI berdasarkan status admin dan login
  output$ui <- renderUI({
    if (!admin_exists()) {
      admin_setup_ui()
    } else if (!logged_in()) {
      login_ui()
    } else {
      dashboard_ui(is_admin())
    }
  })
  
  # Render tabel pengguna untuk admin
  output$user_table <- renderDT({
    users <- get_users()
    users$Actions <- sapply(users$id, function(id) {
      paste0('<button onclick="Shiny.setInputValue(\'delete_user_button\', ', id, ', {priority: \'event\'})">Delete</button>')
    })
    datatable(users, escape = FALSE, options = list(pageLength = 10))
  })
}