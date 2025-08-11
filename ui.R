source("utils.R")
source("configs/config_db.R")

# UI untuk halaman pembuatan akun admin
admin_setup_ui <- function() {
  fluidPage(
    div(class = "admin-setup",
        style = "width: 300px; max-width: 100%; margin: 0 auto; padding: 20px;",
        wellPanel(
          h3("Create Admin Account"),
          textInput("admin_username", "Username/Email"),
          passwordInput("admin_password", "Password"),
          passwordInput("admin_password_confirm", "Confirm Password"),
          actionButton("create_admin_button", "Create Admin")
        )
    )
  )
}

# UI untuk halaman login
login_ui <- function() {
  fluidPage(
    div(class = "login",
        style = "width: 300px; max-width: 100%; margin: 0 auto; padding: 20px;",
        wellPanel(
          h3("Login"),
          textInput("username", "Username/Email"),
          passwordInput("password", "Password"),
          actionButton("login_button", "Log in")
        )
    )
  )
}

# UI untuk dashboard
dashboard_ui <- function(is_admin = FALSE) {
  # 1. Siapkan daftar tab yang selalu ada
  tabs <- list(
    tabItem(tabName = "home",
            h2("Welcome to the Dashboard!"),
            p("This is your home page after successful login.")
    ),
    tabItem(tabName = "data",
            h2("Data Section"),
            p("This is where you can display your data.")
    )
  )
  
  # 2. Jika user adalah admin, tambahkan tab "User Management" ke dalam daftar
  if (is_admin) {
    admin_tab <- tabItem(tabName = "user_management",
                         h2("User Management"),
                         fluidRow(
                           box(
                             title = "Add New User",
                             width = 4,
                             textInput("new_username", "Username/Email"),
                             passwordInput("new_password", "Password"),
                             passwordInput("new_password_confirm", "Confirm Password"),
                             actionButton("add_user_button", "Add User")
                           ),
                           box(
                             title = "User List",
                             width = 8,
                             DTOutput("user_table")
                           )
                         )
    )
    tabs <- append(tabs, list(admin_tab))
  }
  
  # 3. Buat UI Dasbor
  dashboardPage(
    dashboardHeader(
      title = "My Dashboard",
      tags$li(class = "dropdown", 
              actionButton("logout_button", "Logout", icon = icon("sign-out-alt"), 
                           style = "background-color: #dd4b39; color: white; border-color: #d73925; margin-top: 8px; margin-right: 5px;")
      )
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Data", tabName = "data", icon = icon("table")),
        # Logika 'if' di sini aman dan tidak menyebabkan error
        if (is_admin) {
          menuItem("User Management", tabName = "user_management", icon = icon("users"))
        }
      )
    ),
    # 4. Gunakan do.call untuk memanggil tabItems dengan daftar tab yang sudah dibuat
    dashboardBody(
      do.call(tabItems, tabs)
    )
  )
}

# UI utama
ui <- fluidPage(
  uiOutput("ui")
)