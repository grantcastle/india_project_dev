# Website of this published shiny app:
# https://bella-huang.shinyapps.io/india-political-support-and-fiscal-rewards/

# This R script contains the code for an interactive Shiny app with three parts 
# (UI, server, calling the app) and three tabs:
# 1."Fiscal Allocation" tab
#   A choropleth plot where the color of the state indicating amount of fiscal transfer,
#   and the size of the bubble indicating the percentage of BJP vote share
# 2. "Demographics" tab:
#   Two choropleth plots displaying state-wise Hindu population or BJP vote share.
# 3. "Election Outcome" tab:
#   A choropleth plot for constituency-wise BJP vote share (1984-2019).

# Please make sure to edit your path file at the beginning of the server side

# Load required libraries
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(sf)
library(DT)

################################## UI SIDE #####################################
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    title = "India - Political Support & Fiscal Rewards",
    titleWidth = 400
  ),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Fiscal Allocation", tabName = "fundtransfer", icon = icon("hand-holding-dollar")),
      menuItem("Demographics", tabName = "demvote", icon = icon("magnifying-glass")),
      menuItem("Election Outcome", tabName = "yearvote", icon = icon("bullhorn"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab: vote share and funding
      tabItem(tabName = "fundtransfer",
              h2("Hello!"),
              p("Welcome to the 'Political Support and Fiscal Rewards in India' Shiny App! ",
                "This Shiny app is part of a research project conducted by Grant Castle, Bella Huang, and Varun Thampi from the University of Chicago.",
                "The project delves into the political dynamics of fiscal allocations in India, focusing on whether victorious political parties reward their voters through fiscal allocations.",
                "Specifically, it explores if the Bharatiya Janata Party (BJP) redirects central government funding toward states where they received more support.",
                "The research utilizes data on funding allocations, demographics, and election outcomes to uncover patterns and relationships.",
                "Explore three distinct tabs to delve into different aspects of Indian politics."),
              hr(),
              h3("State-wise Fiscal Allocation"),
              p("This tab provides insights into the relationship between fiscal allocation and political support in India. Explore the choropleth plot to understand the amount of fiscal transfer to each state, and observe the percentage of BJP vote share represented by bubble sizes."),
              fluidRow(
                column(width = 3,
                       box(width = NULL,
                           radioButtons("program_type", 
                                        "Please choose a program",
                                        choices = c("PM Kisan Beneficiaries Std" = "pm_kisan", 
                                                    "JDY Beneficiaries Std" = "jdy_beneficiaries",
                                                    "MGNGEGA Funding Std" = "mgnrga", 
                                                    "PMAY-G Funding Std" = "pmayg", 
                                                    "SMB-G Funding Std" = "sbmg",
                                                    "JJM Funding Std" = "jjm", 
                                                    "Total Funding Std" = "total"))),
                ),
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           plotlyOutput("fund_plot"))
                )
              ),
              fluidRow(
                column(width = 3),
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           dataTableOutput("fund_table")))
              )
              
      ),
      # Second tab: Hindu population and BJP vote share by state
      tabItem(tabName = "demvote",
              h3("State-wise Demographics"),
              p("Welcome to the Demographics tab! This tab allows you to delve into the demographics of each state and understand the correlation with political support. Choose between visualizing the percentage of Hindu population or BJP vote share through choropleth plots. Gain insights into the diverse demographic landscape across states."),
              fluidRow(
                column(width = 3,
                       box(width = NULL,
                           radioButtons("plot_type", 
                                        "Please choose a plot to display",
                                        choices = c("Percentage of Hindu population",
                                                    "BJP vote share")))
                ),
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           plotlyOutput("demvote_plot")))
              )
      ),
      # Third tab: Constituency vote share by year
      tabItem(tabName = "yearvote",
              h3("Constituency Vote Share by Year (1984-2019)"),
              p("Welcome to the Election Outcome tab! This tab provides insights into the constituency-wise election outcomes from 1984 to 2019. Select a specific year to visualize the BJP vote share for each parliamentary constituency. Analyze how political landscapes have evolved over the years."),
              fluidRow(
                column(width = 2,
                       box(width = NULL,
                           selectInput("year",
                                       "Please choose a year",
                                       choices = c(1984, 1989, 1991, 1996, 1998,
                                                   1999, 2004, 2009, 2014, 2019) 
                           ))),
                column(width = 10,
                       box(width = NULL, solidHeader = TRUE,
                           plotOutput("yearvote_plot")))
              ),
              fluidRow(
                column(width = 2),
                column(width = 10,
                       box(width = NULL, solidHeader = TRUE,
                           dataTableOutput("cons_table"))))
      ))
  )
)

################################ SERVER SIDE ###################################

server <- function(input, output) {
  
  # Set your own path here
  path <- "/Users/grantcastle/Documents/final-project-castle-huang-thampi/data"
  
  ############################### Data Preprocessing ##############################
  
  # Read the shape files
  india_sf <- st_read(file.path(path, "shape_files/india_sf.shp"))
  boundaries <- st_read(file.path(path, "shape_files/state_boundaries_clean.shp"))
  
  # Map the user input of funding programs with column names in india_sf
  program <- data.frame("pm_kisan" = "pm_kisan_beneficiaries_pgpc", 
                        "jdy_beneficiaries" = "jdy_beneficiaries_pgpc",
                        "mgnrga" = "mgnrega_funding_pgpc",
                        "pmayg" = "pmayg_funding_pgpc",
                        "sbmg" = "sbmg_funding_pgpc",
                        "jjm" = "jjm_funding_pgpc",
                        "total" = "total_funding_pgpc")
  
  # Define the column names for india_sf as shape file rename column names automatically
  col_names <- c("state", "pm_kisan_beneficiaries_pgpc", "jdy_beneficiaries_pgpc",
                 "mgnrega_funding_pgpc", "pmayg_funding_pgpc", "sbmg_funding_pgpc", "jjm_funding_pgpc",
                 "total_funding_pgpc", "UT", "BJP_votes", "eligible_voters",
                 "BJP_vote_share", "hindu_pop", "pct_hindus", "geometry" )
  
  colnames(india_sf) <- col_names
  
  # Create reactive data for fund transfer plot and table
  fund_sf <- reactive({
    selected_program <- program[, as.character(input$program_type)]
    
    india_sf |> 
      select(state, !!selected_program, BJP_vote_share, geometry) |> 
      st_sf()
  })
  
  # Load the cleaned elections df
  elections_clean <- read.csv(file.path(path, "cleaned_datasets/elections_clean.csv"))
  
  # Load the shapefiles for Indian parliamentary constituencies
  cons <- st_read(file.path(path, "shape_files/india_pc_2019_simplified.geojson"))
  
  # Change the state names within the shapefile to lowercase and removing any 
  # symbols/spaces to enable easier merging with theelections dataset
  # Set the CRS for the shapefile
  cons <- cons |> 
    mutate(st_name = tolower(gsub("[[:space:][:punct:]]", "", st_name))) |> 
    st_transform(cons, crs = 4326)
  
  # Create reactive df for constituency vote share plot
  election_sf <- reactive({
    selected_year <- input$year
    
    elections_clean |> 
      filter(Year == selected_year)
  })
  
  ############################## Fund Transfer Plot ##############################
  # Create interactive plotly object for fund transfer
  output$fund_plot <- renderPlotly({
    
    fund_sf <- fund_sf()
    
    selected_program <- program[, as.character(input$program_type)]
    
    # Create centroid sf
    fund_centroid <- fund_sf |> 
      mutate(geometry = st_centroid(geometry)) 
    
    # Create choropleth plot for fund transfer from central to state government
    # Size of the bubble indicates the BJP vote share by state
    gg <- ggplot() +
      geom_sf(data = boundaries, fill = "white") +
      geom_sf(data = fund_sf, aes_string(fill = colnames(fund_sf)[2])) +
      geom_sf(data = fund_centroid,
              pch = 21, fill = "orange", col = "black",
              aes(fill = BJP_vote_share, size = BJP_vote_share,
                  text = paste0("State: ", state, "<br>",
                                "Fiscal transfer: ", round(get(selected_program), 2), "<br>",
                                "BJP vote share: ", round(BJP_vote_share, 2), "%"))) +
      scale_size_continuous(range = c(1, 5)) +
      scale_fill_continuous(low = "lightblue", high = "navy") +
      theme_minimal() +
      labs(fill = "Transfer (pgpc)") 
    
    # Convert ggplot to plotly
    gg_plotly <- ggplotly(tooltip = 'text') |> 
      style(hoverlabel = list(bgcolor = "white"), hoveron = "fill")
    
    # Print the plotly object
    print(gg_plotly)
  })
  
  ################### Hindu Population & Vote Share Plot ##########################
  output$demvote_plot <- renderPlotly(
    # Use if else statement to decide which plot to display
    if (input$plot_type == "Percentage of Hindu population") {
      # Choropleth of hindu population by state
      gg <- ggplot() +
        geom_sf(data = boundaries, fill = "white") +
        geom_sf(data = india_sf, aes(fill = pct_hindus,
                                     text = paste0("State: ", state, "<br>",
                                                   "Hindu population: ", round(pct_hindus, 2), "%"))) +
        scale_fill_gradient(low = "white", high = "steelblue") +
        labs(title = "Hindu Population by State",
             subtitle = "(according to the 2011 census)",
             fill = "Percentage") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "right",
          plot.caption = element_text(hjust = 0.5)
        )
      
      # Convert ggplot to plotly
      gg_plotly <- ggplotly(tooltip = 'text') |> 
        style(hoverlabel = list(bgcolor = "white"), hoveron = "fill")
      
      # Print the plotly object
      print(gg_plotly)
      
    } else if (input$plot_type == "BJP vote share") {
      # Choropleth of BJP vote share by state
      gg <- ggplot() +
        geom_sf(data = boundaries, fill = "white") +
        geom_sf(data = india_sf, aes(fill = BJP_vote_share,
                                     text = paste0("State: ", state, "<br>",
                                                   "BJP vote share: ", round(BJP_vote_share, 2), "%"))) +
        scale_fill_gradient(low = "white", high = "maroon") +
        labs(title = "BJP Vote Share by State (2019)",
             fill = "Percentage") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "right",
          plot.caption = element_text(hjust = 0.5)
        )
      
      # Convert ggplot to plotly
      gg_plotly <- ggplotly(tooltip = 'text') |> 
        style(hoverlabel = list(bgcolor = "white"), hoveron = "fill")
      
      # Print the plotly object
      print(gg_plotly)
    }
  )
  
  ########################## Vote Share by Year Plot ##############################
  
  output$yearvote_plot <- renderPlot({
    
    # Assign the reactive dataframe to a new df
    elections_con_year <- election_sf() %>% 
      na.omit()
    
    # Merge the election results dataset with the constituency shapefile using state names
    # and constituency numbers as merging points 
    elections_year_shapes <- elections_con_year |> 
      merge(cons,
            by.x = c("State_Name", "Constituency_No"),
            by.y = c("st_name", "pc_no"), all = TRUE) |> 
      st_as_sf()
    
    # Create a choropleth plot of the BJP's vote share by parliamentary constituency in 2019
    ggplot() +
      geom_sf(data = elections_year_shapes, aes(fill = BJP_vote_share)) +
      scale_fill_gradient(low = "orange", high = "maroon", na.value = "white", limits = c(0, 100)) +
      labs(fill = "BJP vote share") +
      theme_minimal()
  })
  
  ############################## Fund Transfer Table ##############################
  # Table output of fiscal allocation
  output$fund_table <- renderDataTable({
    
    # Display only the state name, fund amount, and BJP vote share
    fund_sf <-  as.data.frame(fund_sf()) |> 
      select(1:3) 
  })
  
  output$cons_table <- renderDataTable({
    
    elections_con_year <- election_sf() %>% 
      na.omit()
    
    # Merge the election results dataset with the constituency shapefile using state names
    # and constituency numbers as merging points 
    elections_year_shapes <- elections_con_year |> 
      merge(cons,
            by.x = c("State_Name", "Constituency_No"),
            by.y = c("st_name", "pc_no"), all = TRUE) |> 
      st_as_sf()
    
    # Display on the state name, constituency name and BJP vote share
    as.data.frame(elections_year_shapes) |> 
      select(State_Name, Constituency_Name, BJP_vote_share)
  })
  
}

############################# CALL THE SHINY APP ###################################
# Call the shiny app
shinyApp(ui, server)
