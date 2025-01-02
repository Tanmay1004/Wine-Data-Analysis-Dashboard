# Required Libraries
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(plotly)
library(randomForest)
library(caret)
library(corrplot)
library(reshape2)
library(DBI)
library(RSQLite)
library(sodium)
library(gargle)





# Database Setup Function
setup_database <- function() {
  con <- dbConnect(RSQLite::SQLite(), "users.db")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS users (
    username TEXT PRIMARY KEY,
    password TEXT,
    email TEXT
  )")
  dbDisconnect(con)
}

# User Authentication Functions
register_user <- function(username, password, email) {
  con <- dbConnect(RSQLite::SQLite(), "users.db")
  
  # Check if username already exists
  existing_user <- dbGetQuery(con, "SELECT * FROM users WHERE username = ?", params = list(username))
  
  if (nrow(existing_user) > 0) {
    dbDisconnect(con)
    return(FALSE)
  }
  
  # Hash password before storing
  hashed_password <- hash_password(password)
  
  tryCatch({
    dbExecute(con, "INSERT INTO users (username, password, email) VALUES (?, ?, ?)", 
              params = list(username, hashed_password, email))
    dbDisconnect(con)
    TRUE
  }, error = function(e) {
    dbDisconnect(con)
    FALSE
  })
}

validate_login <- function(username, password) {
  con <- dbConnect(RSQLite::SQLite(), "users.db")
  
  user <- dbGetQuery(con, "SELECT * FROM users WHERE username = ?", params = list(username))
  dbDisconnect(con)
  
  if (nrow(user) == 0) return(FALSE)
  
  # Verify hashed password
  verify_password(user$password, password)
}

# Password Hashing Functions
hash_password <- function(password) {
  # Use sodium for secure password hashing
  pw_hash <- password_store(password)
  return(pw_hash)
}

verify_password <- function(stored_hash, input_password) {
  password_verify(stored_hash, input_password)
}

# Initialize Database
setup_database()

# UI Definition
ui <- navbarPage(
  theme = shinytheme("flatly"),
  title = "Wine Quality Explorer",
  id = "nav",
  
  # Remove Login and Signup tabs
  
  
  # About Project Page - Professional and Informative
  tabPanel("About Project",
           fluidPage(
             tags$div(
               class = "jumbotron text-center",
               tags$h1("Wine Quality Analysis", style = "color: #2c3e50;"),
               tags$p("A comprehensive data science approach to understanding wine characteristics", 
                      style = "color: #34495e;")
             ),
             fluidRow(
               column(6, 
                      tags$div(
                        class = "well",
                        tags$h3("Project Insights"),
                        tags$ul(
                          tags$li("Advanced data visualization of wine properties"),
                          tags$li("Machine learning prediction of wine quality"),
                          tags$li("Comparative analysis of red and white wines")
                        )
                      )
               ),
               column(6,
                      tags$div(
                        class = "well",
                        tags$h3("Methodology"),
                        tags$ul(
                          tags$li("Random Forest Machine Learning Algorithm"),
                          tags$li("Comprehensive Chemical Property Analysis"),
                          tags$li("Interactive Data Exploration Platform")
                        )
                      )
               )
             ),
             fluidRow(
               column(12,
                      tags$div(
                        class = "well text-center",
                        tags$h3("Research Objectives"),
                        tags$p("Our research aims to develop a sophisticated machine learning model 
                               that can predict wine quality based on various chemical properties, 
                               providing insights into the complex factors influencing wine characteristics.", 
                               style = "color: #34495e;")
                      )
               )
             )
           )
  ),
  
  # Enhanced Dashboard with More Visualizations
  tabPanel("Wine Dashboard",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("wine_type", "Select Wine Type", 
                             choices = c("All", "Red", "White")),
                 sliderInput("quality_range", "Quality Range", 
                             min = 3, max = 8, 
                             value = c(3, 8)),
                 
               ),
               mainPanel(
                 fluidRow(
                   column(6, 
                          plotlyOutput("alcohol_boxplot"),
                          tags$p("Alcohol Content Distribution Across Wine Types", 
                                 class = "text-muted text-center")
                   ),
                   column(6, 
                          plotlyOutput("quality_doughnut"),
                          tags$p("Distribution of Wine Quality Ratings", 
                                 class = "text-muted text-center")
                   ),
                 ),
                 fluidRow(
                   column(12, 
                          plotlyOutput("double_sided_bar"),
                          tags$p("Comparison of Chemical Properties Between Wine Types", 
                                 class = "text-muted text-center")
                   )
                 ),
                 fluidRow(
                   column(12, 
                          plotlyOutput("radar_chart"),
                          tags$p("Comparative Radar Chart of Wine Properties", 
                                 class = "text-muted text-center")
                   )
                 )
               )
             )
           )
  ),
  
  # Enhanced Prediction Page
  tabPanel("Quality Prediction",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 sliderInput("alcohol", "Alcohol Content (%)", 
                             min = 8, max = 15, value = 10, step = 0.1),
                 sliderInput("residual_sugar", "Residual Sugar (g/L)", 
                             min = 0, max = 50, value = 3, step = 0.1),
                 sliderInput("volatile_acidity", "Volatile Acidity (g/L)", 
                             min = 0, max = 2, value = 0.5, step = 0.01),
                 sliderInput("pH", "pH Level", 
                             min = 2.5, max = 4, value = 3.3, step = 0.1),
                 actionButton("predict", "Predict Quality", 
                              class = "btn-primary")
               ),
               mainPanel(
                 wellPanel(
                   h3("Prediction Results"),
                   verbatimTextOutput("prediction_output")
                 ),
                 plotlyOutput("feature_importance"),
                 tags$p("Feature Importance in Quality Prediction", 
                        class = "text-muted text-center")
               )
             )
           )
  ),
  
  # About Us Page with Professional Layout
  tabPanel("About Us",
           fluidPage(
             tags$div(
               class = "container",
               tags$h1("Project Team", style = "color: #2c3e50;"),
               fluidRow(
                 column(6,
                        tags$div(
                          class = "well",
                          tags$h3("Taniya Shrivastava"),
                          tags$ul(
                            tags$li("Student ID: 22ESKCX117"),
                            tags$li("Phone: 7851906400"),
                            tags$li("Email: ts.taniyashrivastava@gmail.com")
                          )
                        )
                 ),
                 column(6,
                        tags$div(
                          class = "well",
                          tags$h3("Tanmay Sharma"),
                          tags$ul(
                            tags$li("Student ID: 22ESKCX118"),
                            tags$li("Phone: +918949668558"),
                            tags$li("Email: tanmaysharma1004@gmail.com")
                          )
                        )
                 )
               ),
               tags$p("Data Science Specialization - Rajasthan Technical University", 
                      style = "color: #34495e; text-align: center;")
             )
           )
  ),
  
  # Logout button on the right
  tags$li(
    class = "navbar-right", 
    style = "margin-right: 10px; margin-top: 8px;",
    uiOutput("logout_button")
  )
)

# Server Logic
server <- function(input, output, session) {
  # User Authentication State
  USER <- reactiveVal(NULL)
  
  # Initial page load redirects to login
  observe({
    if (is.null(USER())) {
      # Create a login modal
      showModal(modalDialog(
        title = "Login Required",
        textInput("login_username", "Username"),
        passwordInput("login_password", "Password"),
        footer = tagList(
          actionButton("login_button", "Login", class = "btn-primary"),
          actionButton("signup_button", "Sign Up", class = "btn-success")
        )
      ))
    }
  })
  
  # Login Logic
  observeEvent(input$login_button, {
    req(input$login_username, input$login_password)
    
    if (validate_login(input$login_username, input$login_password)) {
      USER(input$login_username)
      removeModal()
    } else {
      showNotification("Invalid username or password", type = "error")
    }
  })
  
  # Signup Logic (in modal)
  observeEvent(input$signup_button, {
    # Replace login modal with signup modal
    showModal(modalDialog(
      title = "Create Account",
      textInput("new_username", "Username"),
      textInput("new_email", "Email"),
      passwordInput("new_password", "Password"),
      passwordInput("confirm_password", "Confirm Password"),
      footer = actionButton("create_account", "Create Account", class = "btn-success")
    ))
  })
  
  # Account Creation Logic
  observeEvent(input$create_account, {
    req(input$new_username, input$new_email, 
        input$new_password, input$confirm_password)
    
    if (input$new_password != input$confirm_password) {
      showNotification("Passwords do not match", type = "error")
      return()
    }
    
    success <- register_user(
      input$new_username, 
      input$new_password, 
      input$new_email
    )
    
    if (success) {
      showNotification("Account created successfully", type = "message")
      removeModal()
      # Automatically show login modal again
      showModal(modalDialog(
        title = "Login",
        textInput("login_username", "Username"),
        passwordInput("login_password", "Password"),
        footer = actionButton("login_button", "Login", class = "btn-primary")
      ))
    } else {
      showNotification("Username already exists", type = "error")
    }
  })
  
  # Logout Logic
  output$logout_button <- renderUI({
    req(USER())
    actionButton("logout", paste("Logout (", USER(), ")", class = "btn-danger"))
  })
  
  # Logout Action
  observeEvent(input$logout, {
    USER(NULL)
  })
  # Add Logout button in UI
  output$logout_button <- renderUI({
    req(USER())
    actionButton("logout", "Logout", class = "btn-danger navbar-btn")
  })
  
  # Existing server logic from your original code goes here
  # ... (paste your entire previous server function, keeping all existing visualizations and predictions)
  # 
  # Reactive filtered data
  filtered_data <- reactive({
    data <- wine_data
    
    if (input$wine_type != "All") {
      data <- data %>% filter(wine_type == input$wine_type)
    }
    
    data %>% 
      filter(quality >= input$quality_range[1] & quality <= input$quality_range[2])
  })
  
  # Alcohol Boxplot
  output$alcohol_boxplot <- renderPlotly({
    ggplotly(
      ggplot(filtered_data(), aes(x = wine_type, y = alcohol, fill = wine_type)) +
        geom_boxplot(alpha = 0.7) +
        scale_fill_manual(values = c("Red" = "#bf4830", "White" = "#a1a45a")) +
        labs(title = "Alcohol Content Distribution", 
             x = "Wine Type", y = "Alcohol (%)") +
        theme_minimal()
    )
  })
  
  
  # In the server section, replace the ph_violin output with this new doughnut chart logic:
  output$quality_doughnut <- renderPlotly({
    # Get the filtered data based on quality range
    data <- filtered_data()
    
    if(input$wine_type == "All") {
      # For "All" option, create a double doughnut chart comparing red and white
      red_counts <- data %>%
        filter(wine_type == "Red") %>%
        count(quality) %>%
        mutate(percentage = n/sum(n) * 100,
               label = paste("Quality", quality))
      
      white_counts <- data %>%
        filter(wine_type == "White") %>%
        count(quality) %>%
        mutate(percentage = n/sum(n) * 100,
               label = paste("Quality", quality))
      
      # Create the double doughnut chart
      plot_ly() %>%
        add_pie(data = red_counts,
                labels = ~label,
                values = ~percentage,
                hole = 0.3,
                hoverinfo = 'label+percent',  # Specify exact hover information
                domain = list(x = c(0, 0.48), y = c(0, 1)),
                marker = list(colors = colorRampPalette(c("#bf4830", "#722b1c"))(nrow(red_counts)),
                              showscale = FALSE)) %>%
        add_pie(data = white_counts,
                labels = ~label,
                values = ~percentage,
                hole = 0.3,
                hoverinfo = 'label+percent',  # Specify exact hover information
                domain = list(x = c(0.52, 1), y = c(0, 1)),
                marker = list(colors = colorRampPalette(c("#a1a45a", "#555730"))(nrow(white_counts)),
                              showscale = FALSE)) %>%
        layout(title = "Quality Distribution by Wine Type",
               showlegend = FALSE,
               annotations = list(
                 list(x = 0.24, y = 0.5, text = "Red", showarrow = FALSE, font = list(size = 14)),
                 list(x = 0.76, y = 0.5, text = "White", showarrow = FALSE, font = list(size = 14))
               ))
    } else {
      # For single wine type selection
      quality_counts <- data %>%
        count(quality) %>%
        mutate(percentage = n/sum(n) * 100,
               label = paste("Quality", quality))  # Create label with "Quality" prefix
      
      colors <- if(input$wine_type == "Red") {
        colorRampPalette(c("#bf4830", "#722b1c"))(nrow(quality_counts))
      } else {
        colorRampPalette(c("#a1a45a", "#555730"))(nrow(quality_counts))
      }
      
      plot_ly() %>%
        add_pie(data = quality_counts,
                labels = ~label,  # Use the new label with "Quality" prefix
                values = ~percentage,
                hole = 0.3,
                hoverinfo = 'label+percent',  # Specify hover information
                marker = list(colors = colors)) %>%
        layout(title = paste(input$wine_type, "Wine Quality Distribution"),
               showlegend = TRUE)  # Keep legend for single wine type views
    }
  })
  
  # Add these columns of interest at the start of your server function
  columns_of_interest <- c(
    "fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar",
    "density", "pH", "sulphates", "alcohol", "quality"
  )
  
  # Double-sided bar plot
  output$double_sided_bar <- renderPlotly({
    # Get the filtered data based on wine type selection
    if(input$wine_type == "All") {
      # Calculate means for both wine types
      red_means <- wine_data %>%
        filter(wine_type == "Red",
               quality >= input$quality_range[1],
               quality <= input$quality_range[2]) %>%
        summarise(across(all_of(columns_of_interest), mean, na.rm = TRUE))
      
      white_means <- wine_data %>%
        filter(wine_type == "White",
               quality >= input$quality_range[1],
               quality <= input$quality_range[2]) %>%
        summarise(across(all_of(columns_of_interest), mean, na.rm = TRUE))
      
      # Combine data for plotting
      plot_data <- data.frame(
        Feature = rep(columns_of_interest, 2),
        Mean = c(-as.numeric(red_means), as.numeric(white_means)),
        Type = rep(c("Red Wine", "White Wine"), each = length(columns_of_interest))
      )
      
      # Create double-sided bar plot
      plot_ly(
        data = plot_data,
        x = ~Mean,
        y = ~Feature,
        type = 'bar',
        orientation = 'h',
        color = ~Type,
        colors = c("#bf4830", "#a1a45a")
      ) %>%
        layout(
          barmode = "relative",
          xaxis = list(
            title = "Mean Values",
            zeroline = TRUE,
            tickvals = c(-max(abs(plot_data$Mean)), 0, max(abs(plot_data$Mean))),
            ticktext = c("Higher in Red Wine", "0", "Higher in White Wine")
          ),
          yaxis = list(
            title = "Features",
            categoryorder = "total ascending"
          ),
          title = "Comparison of Mean Values for Red and White Wines"
        )
    } else {
      # For single wine type selection, show regular bar plot
      selected_means <- wine_data %>%
        filter(wine_type == input$wine_type,
               quality >= input$quality_range[1],
               quality <= input$quality_range[2]) %>%
        summarise(across(all_of(columns_of_interest), mean, na.rm = TRUE))
      
      plot_data <- data.frame(
        Feature = columns_of_interest,
        Mean = as.numeric(selected_means)
      )
      
      # Create single-sided bar plot
      plot_ly(
        data = plot_data,
        x = ~Mean,
        y = ~Feature,
        type = 'bar',
        orientation = 'h',
        marker = list(
          color = if(input$wine_type == "Red") "#bf4830" else "#a1a45a"
        )
      ) %>%
        layout(
          xaxis = list(
            title = "Mean Values"
          ),
          yaxis = list(
            title = "Features",
            categoryorder = "total ascending"
          ),
          title = paste("Mean Values for", input$wine_type, "Wine")
        )
    }
  })
  
  # Static Radar Chart with pre-calculated values
  output$radar_chart <- renderPlotly({
    # Pre-compute average values for each wine type (static data)
    static_data <- wine_data %>%
      group_by(wine_type) %>%
      summarise(
        avg_alcohol = mean(alcohol),
        avg_sugar = mean(residual.sugar),
        avg_pH = mean(pH),
        avg_quality = mean(quality),
        avg_density = mean(density)
      )
    
    # Create radar chart
    plot_ly(
      type = 'scatterpolar',
      mode = 'lines+markers'
    ) %>%
      add_trace(
        r = unlist(static_data[static_data$wine_type == "Red", 2:6]),
        theta = c('Alcohol', 'Sugar', 'pH', 'Quality', 'Density'),
        name = 'Red Wine',
        fill = 'toself',
        line = list(color = '#bf4830')
      ) %>%
      add_trace(
        r = unlist(static_data[static_data$wine_type == "White", 2:6]),
        theta = c('Alcohol', 'Sugar', 'pH', 'Quality', 'Density'),
        name = 'White Wine',
        fill = 'toself',
        line = list(color = '#a1a45a')
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, max(static_data[,2:6]) * 1.2)  # Dynamic range based on data
          )
        ),
        title = "Comparative Radar Chart of Wine Properties"
      )
  })
  
  
  # Prediction Model
  rf_model <- reactive({
    set.seed(123)
    train_index <- createDataPartition(wine_data$quality, p = 0.8, list = FALSE)
    train_data <- wine_data[train_index, ]
    
    randomForest(
      quality ~ alcohol + residual.sugar + volatile.acidity + 
        pH + chlorides + sulphates + density,
      data = train_data,
      ntree = 500,
      importance = TRUE
    )
  })
  
  # Prediction Logic
  prediction <- eventReactive(input$predict, {
    predict_data <- data.frame(
      alcohol = input$alcohol,
      residual.sugar = input$residual_sugar,
      volatile.acidity = input$volatile_acidity,
      pH = input$pH,
      chlorides = 0.045,  # Default value
      sulphates = 0.8,    # Default value
      density = 0.99      # Default value
    )
    
    round(predict(rf_model(), newdata = predict_data), 2)
  })
  
  # Prediction Output
  output$prediction_output <- renderText({
    paste("Predicted Wine Quality:", prediction())
  })
  
  # Enhanced Feature Importance Plot
  output$feature_importance <- renderPlotly({
    imp_df <- as.data.frame(importance(rf_model()))
    imp_df$Feature <- rownames(imp_df)
    library(plotly)
    
    # Assuming imp_df is your data frame
    plot_ly(
      data = imp_df,
      x = ~Feature,
      y = ~`%IncMSE`, # Use backticks for `%IncMSE`
      type = 'bar',
      text = ~round(`%IncMSE`, 2), # Use backticks here as well
      textposition = 'auto',
      marker = list(color = '#a1a45a')
    ) %>%
      layout(
        title = "Feature Importance in Wine Quality Prediction",
        xaxis = list(title = "Features"),
        yaxis = list(title = "% Increase in MSE"),
        plot_bgcolor = 'rgba(245, 246, 249, 1)',
        paper_bgcolor = 'rgba(245, 246, 249, 1)'
      )
    
  })
  
  # [Rest of your existing server logic goes here]
  # ... (paste your previous server function implementation)
}

# Run the application 
shinyApp(ui = ui, server = server)