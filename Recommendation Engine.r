# Load necessary libraries
library(shiny)
library(dplyr)
library(recommenderlab)

# Load the dataset from a CSV file
# Modify the file path according to your file location
grant_opportunities_df <- read.csv("D:/Data/Class Materials/Advanced R Programming/R Project/TEST Funding Opportunities.csv")
colnames(grant_opportunities_df) <- c("grant_id", "grant_name", "industry")

# Create a user DataFrame
n_users <- 100
users_df <- data.frame(user_id = 1:n_users)

# Create random ratings
set.seed(123) # For reproducibility
ratings <- sample(c(-1, 0, 1), size = n_users * nrow(grant_opportunities_df), replace = TRUE, prob = c(0.1, 0.8, 0.1))

# Create a ratings DataFrame
ratings_df <- data.frame(
  user_id = rep(users_df$user_id, each = nrow(grant_opportunities_df)),
  grant_name = rep(grant_opportunities_df$grant_name, n_users),
  rating = ratings
)

# Convert the ratings DataFrame to a realRatingMatrix
ratings_matrix <- as(ratings_df, "realRatingMatrix")

# Create an evaluation scheme for splitting data
split_data <- evaluationScheme(ratings_matrix, method = "split", train = 0.7, given = -1, goodRating = 1)

# Get the training and test data
train_data <- getData(split_data, "train")
test_data <- getData(split_data, "unknown")

# Create a UBCF model
model_ubcf <- Recommender(train_data, method = "UBCF", param = list(normalize = "Z-score"))

# Define the Shiny UI
ui <- fluidPage(
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Authentication",
      fluidPage(
        titlePanel("User Authentication"),
        sidebarLayout(
          sidebarPanel(
            numericInput("user_id", "User ID (1-100):", value = NULL, min = 1, max = 100),
            selectInput("industry", "Preferred Industry:", choices = unique(grant_opportunities_df$industry)),
            actionButton("authenticate", "Authenticate")
          ),
          mainPanel(
            textOutput("auth_message")
          )
        )
      )
    ),
    tabPanel("Recommendations",
      fluidPage(
        titlePanel("Top 10 Recommended Grants"),
        sidebarLayout(
          sidebarPanel(),
          mainPanel(
            tableOutput("recommendations_table")
          )
        )
      )
    )
  )
)

# Define the Shiny server
server <- function(input, output, session) {
  observeEvent(input$authenticate, {
    # Validate user ID and industry inputs
    if (is.null(input$user_id) || input$industry == "") {
      output$auth_message <- renderText("Please enter a valid user ID and preferred industry.")
      return()
    }

    # Redirect to the recommendations tab
    updateTabsetPanel(session, "main_tabs", selected = "Recommendations")
  })

  # Generate recommendations for the given user ID and industry preference
  output$recommendations_table <- renderTable({
    # Get user ID and preferred industry from input
    user_id <- input$user_id
    preferred_industry <- input$industry

    # Make predictions using the UBCF model
    predicted_ratings <- predict(model_ubcf, test_data, type = "ratings")

    # Convert predicted ratings to a matrix
    predicted_matrix <- as(predicted_ratings, "matrix")

    # Extract predictions for the specific user ID
    user_predictions <- predicted_matrix[user_id,]

    # Create a data frame of predictions and grant names
    recommendations <- data.frame(
      grant_name = names(user_predictions),
      predicted_rating = user_predictions
    )

    # Join with grant opportunities data frame to get industry information
    recommendations <- recommendations %>%
      inner_join(grant_opportunities_df, by = "grant_name") %>%
      filter(industry == preferred_industry) %>%
      arrange(desc(predicted_rating))

    # Select the top 10 recommendations
    top_10_recommendations <- recommendations[1:10, ]

    # Display the top 10 recommended grants in a table format
    top_10_recommendations %>%
      select(grant_name, predicted_rating) %>%
      rename("Grant Title" = grant_name, "Predicted Rating" = predicted_rating)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
