# Force binary packages to avoid compilation issues
options(install.packages.compile.from.source = "never")

# app_with_simple_map.R
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(leaflet)
library(tidyr)  # Added for complete() function

# Your existing data loading code remains the same...
file_path <- "water_data_clean.csv"
water_data <- read.csv(file_path, stringsAsFactors = FALSE, 
                       fileEncoding = "UTF-8", check.names = FALSE)

if(nrow(water_data) == 0) {
  water_data <- read.csv(file_path, stringsAsFactors = FALSE, 
                         fileEncoding = "Latin1", check.names = FALSE)
}

manual_colnames <- c(
  "STN_code", "Monitoring_Location", "Year", "Type_Water_Body", "State_Name",
  "Temperature_C_Min", "Temperature_C_Max", "Dissolved_Min", "Dissolved_Max",
  "pH_Min", "pH_Max", "Conductivity_Min", "Conductivity_Max",
  "BOD_mg_L_Min", "BOD_mg_L_Max", "NitrateN_mg_L_Min", "NitrateN_mg_L_Max",
  "Fecal_Coliform_MPN_100ml_Min", "Fecal_Coliform_MPN_100ml_Max",
  "Total_Coliform_MPN_100ml_Min", "Total_Coliform_MPN_100ml_Max"
)

if(ncol(water_data) == length(manual_colnames)) {
  colnames(water_data) <- manual_colnames
  cat("Using manual column names\n")
} else {
  colnames(water_data) <- gsub("[^a-zA-Z0-9_]", "_", colnames(water_data))
  colnames(water_data) <- make.names(colnames(water_data), unique = TRUE)
  cat("Using cleaned column names\n")
}

numeric_cols <- c("Year", "Temperature_C_Min", "Temperature_C_Max", "Dissolved_Min", "Dissolved_Max",
                  "pH_Min", "pH_Max", "Conductivity_Min", "Conductivity_Max", "BOD_mg_L_Min", "BOD_mg_L_Max",
                  "NitrateN_mg_L_Min", "NitrateN_mg_L_Max", "Fecal_Coliform_MPN_100ml_Min", 
                  "Fecal_Coliform_MPN_100ml_Max", "Total_Coliform_MPN_100ml_Min", "Total_Coliform_MPN_100ml_Max")

for(col in numeric_cols) {
  if(col %in% colnames(water_data)) {
    water_data[[col]] <- as.numeric(water_data[[col]])
  }
}

water_data$Avg_Temperature <- (water_data$Temperature_C_Min + water_data$Temperature_C_Max) / 2
water_data$Avg_pH <- (water_data$pH_Min + water_data$pH_Max) / 2
water_data$Avg_Dissolved_Oxygen <- (water_data$Dissolved_Min + water_data$Dissolved_Max) / 2
water_data$Avg_Conductivity <- (water_data$Conductivity_Min + water_data$Conductivity_Max) / 2
water_data$Avg_BOD <- (water_data$BOD_mg_L_Min + water_data$BOD_mg_L_Max) / 2
water_data$Avg_Nitrate <- (water_data$NitrateN_mg_L_Min + water_data$NitrateN_mg_L_Max) / 2
water_data$Avg_Fecal_Coliform <- (water_data$Fecal_Coliform_MPN_100ml_Min + water_data$Fecal_Coliform_MPN_100ml_Max) / 2
water_data$Avg_Total_Coliform <- (water_data$Total_Coliform_MPN_100ml_Min + water_data$Total_Coliform_MPN_100ml_Max) / 2

water_data$Type_Water_Body <- gsub("WATER TREATMENT PLANT.*RAW WATER.*", "RAW WATER", water_data$Type_Water_Body)

# Color palettes
color_palettes <- list(
  primary = c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', 
              '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf'),
  pastel = c('#aec7e8', '#ffbb78', '#98df8a', '#ff9896', '#c5b0d5',
             '#c49c94', '#f7b6d2', '#c7c7c7', '#dbdb8d', '#9edae5'),
  vibrant = c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3', '#ff7f00',
              '#ffff33', '#a65628', '#f781bf', '#999999', '#66c2a5'),
  water_colors = c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd',
                   '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf'),
  wqi_gradient = c('#d73027', '#fc8d59', '#fee090', '#e0f3f8', '#91bfdb', '#4575b4')
)

# State coordinates and information
state_data <- data.frame(
  State_Name = c("ANDHRA PRADESH", "ASSAM", "GOA", "GUJARAT", "HARYANA", 
                 "HIMACHAL PRADESH", "MADHYA PRADESH", "MAHARASHTRA", "ODISHA",
                 "PUNJAB", "RAJASTHAN", "TAMIL NADU", "TELANGANA", "UTTAR PRADESH",
                 "KARNATAKA", "KERALA", "WEST BENGAL", "BIHAR", "JHARKHAND"),
  lat = c(15.9129, 26.2006, 15.2993, 22.2587, 29.0588, 
          31.1048, 22.9734, 19.7515, 20.9517,
          31.1471, 27.0238, 11.1271, 18.1124, 26.8467,
          15.3173, 10.8505, 22.9868, 25.0961, 23.6102),
  lng = c(79.7400, 92.9376, 74.1240, 71.1924, 76.0856, 
          77.1734, 78.6569, 75.7139, 85.0985,
          75.3412, 74.2179, 78.6569, 79.0193, 80.9462,
          75.7139, 76.2711, 88.3639, 85.3131, 85.2799),
  radius = c(12, 8, 4, 10, 6, 5, 12, 12, 8, 6, 14, 10, 8, 14, 10, 6, 8, 8, 6)
)

# Available parameters for analysis - UPDATED WITH TOTAL COLIFORM
available_params <- c("Temperature", "pH", "Dissolved Oxygen", 
                      "Conductivity", "BOD", "Nitrate", "Fecal Coliform", "Total Coliform")

# CORRECTED WQI Calculation Function - More realistic for drains/canals
calculate_wqi <- function(data) {
  # More realistic WQI calculation that gives lower scores for polluted water bodies
  # This uses sub-index calculations based on actual parameter values
  
  # Remove rows with missing critical parameters
  data <- data[!is.na(data$Avg_pH) & !is.na(data$Avg_Dissolved_Oxygen) & 
                 !is.na(data$Avg_BOD) & !is.na(data$Avg_Total_Coliform), ]
  if(nrow(data) == 0) return(data.frame())
  
  # pH sub-index (ideal range 6.5-8.5)
  data$pH_Index <- ifelse(data$Avg_pH >= 6.5 & data$Avg_pH <= 8.5, 100,
                          ifelse(data$Avg_pH >= 6.0 & data$Avg_pH < 6.5 | data$Avg_pH > 8.5 & data$Avg_pH <= 9.0, 80,
                                 ifelse(data$Avg_pH >= 5.5 & data$Avg_pH < 6.0 | data$Avg_pH > 9.0 & data$Avg_pH <= 9.5, 60, 40)))
  
  # Dissolved Oxygen sub-index (higher is better)
  data$DO_Index <- ifelse(data$Avg_Dissolved_Oxygen >= 7, 100,
                          ifelse(data$Avg_Dissolved_Oxygen >= 5, 80,
                                 ifelse(data$Avg_Dissolved_Oxygen >= 4, 60,
                                        ifelse(data$Avg_Dissolved_Oxygen >= 3, 40, 20))))
  
  # BOD sub-index (lower is better) - Made stricter
  data$BOD_Index <- ifelse(data$Avg_BOD <= 2, 100,
                           ifelse(data$Avg_BOD <= 3, 80,
                                  ifelse(data$Avg_BOD <= 6, 60,
                                         ifelse(data$Avg_BOD <= 8, 40, 20))))
  
  # Total Coliform sub-index (lower is better) - Made much stricter
  data$TC_Index <- ifelse(data$Avg_Total_Coliform <= 50, 100,
                          ifelse(data$Avg_Total_Coliform <= 500, 80,
                                 ifelse(data$Avg_Total_Coliform <= 2500, 60,
                                        ifelse(data$Avg_Total_Coliform <= 5000, 40, 20))))
  
  # Nitrate sub-index (lower is better)
  data$Nitrate_Index <- ifelse(data$Avg_Nitrate <= 5, 100,
                               ifelse(data$Avg_Nitrate <= 10, 80,
                                      ifelse(data$Avg_Nitrate <= 20, 60,
                                             ifelse(data$Avg_Nitrate <= 30, 40, 20))))
  
  # Fecal Coliform sub-index (lower is better) - Made much stricter
  data$FC_Index <- ifelse(data$Avg_Fecal_Coliform <= 50, 100,
                          ifelse(data$Avg_Fecal_Coliform <= 250, 80,
                                 ifelse(data$Avg_Fecal_Coliform <= 1000, 60,
                                        ifelse(data$Avg_Fecal_Coliform <= 2000, 40, 20))))
  
  # Calculate WQI using weighted arithmetic mean
  # Adjusted weights to give more importance to pollution indicators
  weights <- c(0.15, 0.25, 0.20, 0.20, 0.10, 0.10)  # pH, DO, BOD, TC, Nitrate, FC weights
  
  # Calculate weighted WQI
  data$WQI <- (data$pH_Index * weights[1] + 
                 data$DO_Index * weights[2] + 
                 data$BOD_Index * weights[3] + 
                 data$TC_Index * weights[4] + 
                 data$Nitrate_Index * weights[5] +
                 data$FC_Index * weights[6])
  
  # Apply water body type penalty - drains and canals get lower scores
  data$WQI <- ifelse(grepl("DRAIN|CANAL", data$Type_Water_Body, ignore.case = TRUE),
                     data$WQI * 0.7,  # 30% penalty for drains/canals
                     data$WQI)
  
  # Ensure WQI is between 0-100
  data$WQI <- pmin(pmax(data$WQI, 0), 100)
  
  # UPDATED WQI Classification with new ranges
  data$WQI_Class <- cut(data$WQI,
                        breaks = c(0, 30, 60, 75, 90, 100),
                        labels = c("Very Poor", "Poor", "Modarate", "Good", "Excellent"),
                        include.lowest = TRUE)
  
  return(data)
}

# Calculate WQI for the entire dataset
water_data_with_wqi <- calculate_wqi(water_data)

# UI with corrected sidebar
ui <- dashboardPage(
  dashboardHeader(title = "Indian Water Quality Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("info-circle")),
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Spatial Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Correlation Analysis", tabName = "correlation", icon = icon("scatter-chart")),
      menuItem("WQI Analysis", tabName = "wqi", icon = icon("tint")),
      menuItem("Clean Data", tabName = "data", icon = icon("table")),
      id = "sidebar"
    ),
    
    # Conditional sidebar panels
    conditionalPanel(
      condition = "input.sidebar == 'overview' || input.sidebar == 'analysis' || input.sidebar == 'correlation' || input.sidebar == 'wqi'",
      selectInput("state", "Select State:", 
                  choices = c("All", unique(water_data$State_Name)))
    ),
    
    conditionalPanel(
      condition = "input.sidebar == 'overview' || input.sidebar == 'analysis' || input.sidebar == 'correlation' || input.sidebar == 'wqi'",
      selectInput("year", "Select Year:",
                  choices = c("All", unique(water_data$Year)))
    ),
    
    conditionalPanel(
      condition = "input.sidebar == 'overview' || input.sidebar == 'analysis'",
      selectInput("water_type", "Select Water Body Type:",
                  choices = c("All", unique(water_data$Type_Water_Body)))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box { margin-bottom: 10px; }
        .small-box { height: 80px; }
        .leaflet-container { background: #f8f9fa; }
        .info.legend { background: white; padding: 10px; border-radius: 5px; font-size: 12px; }
        .collapsible-box .box-header { cursor: pointer; }
        .collapsible-box .box-header .fa { transition: transform 0.3s; }
        .collapsible-box.collapsed .box-header .fa-chevron-down { transform: rotate(-90deg); }
        .param-header { background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px; }
        /* Custom teal color for the box plot section */
        .box.box-teal {
          border-color: #20c997;
        }
        .box.box-teal > .box-header {
          background-color: #20c997 !important;
          color: white;
        }
        .wqi-excellent { background-color: #4575b4; color: white; }
        .wqi-good { background-color: #91bfdb; color: black; }
        .wqi-Modarate { background-color: #e0f3f8; color: black; }
        .wqi-poor { background-color: #fee090; color: black; }
        .wqi-very-poor { background-color: #fc8d59; color: black; }
        .btn-kaggle {
          background-color: #20BEFF;
          border-color: #20BEFF;
          color: white;
        }
        .btn-kaggle:hover {
          background-color: #1a9cd6;
          border-color: #1a9cd6;
          color: white;
        }
      ")),
      tags$script(HTML("
        $(document).ready(function() {
          $('.collapsible-box .box-header').click(function() {
            var $box = $(this).closest('.collapsible-box');
            var $content = $box.find('.box-body');
            var $icon = $box.find('.fa-chevron-down');
            
            if ($box.hasClass('collapsed')) {
              $content.slideDown(300);
              $box.removeClass('collapsed');
            } else {
              $content.slideUp(300);
              $box.addClass('collapsed');
            }
          });
        });
      "))
    ),
    
    tabItems(
      # NEW: Introduction Tab
      tabItem(tabName = "introduction",
              fluidRow(
                div(
                  class = "collapsible-box",
                  box(
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    title = tagList(
                      icon("chevron-down"), 
                      "Introduction (Click to expand/collapse)"
                    ),
                    collapsible = FALSE,
                    div(
                      class = "box-body",
                      HTML("
                        <div style='font-size: 14px; line-height: 1.6;'>
                          <h3>Indian Water Quality Dashboard</h3>
                          <p>This interactive dashboard provides comprehensive analysis and visualization of water quality data 
                          collected from various water bodies across India between 2021-2023. The platform enables researchers, 
                          policymakers, and the general public to explore spatial patterns, temporal trends, and correlations 
                          between different water quality parameters.</p>
                          
                          <p><strong>Key Features:</strong></p>
                          <ul>
                            <li>Spatial analysis of water quality parameters across Indian states</li>
                            <li>Correlation analysis between different water quality indicators</li>
                            <li>Water Quality Index (WQI) calculation and classification</li>
                            <li>Interactive visualizations and downloadable data</li>
                            <li>Comparative analysis by water body types</li>
                          </ul>
                          
                          <p>The dashboard integrates data from rivers, lakes, ponds, canals, drains, marine waters, and beaches 
                          to provide a holistic view of India's water quality landscape.</p>
                        </div>
                      ")
                    )
                  )
                )
              ),
              
              fluidRow(
                div(
                  class = "collapsible-box collapsed",
                  box(
                    width = 12,
                    status = "info",
                    solidHeader = TRUE,
                    title = tagList(
                      icon("chevron-down"), 
                      "Data Description (Click to expand/collapse)"
                    ),
                    collapsible = FALSE,
                    div(
                      class = "box-body",
                      HTML("
                        <div style='font-size: 14px; line-height: 1.6;'>
                          <h3>Data Description</h3>
                          <p><strong>Data Source:</strong> This dataset was originally collected from 
                          <a href='https://www.kaggle.com/datasets/rishabchitloor/indian-water-quality-data-2021-2023' target='_blank'>
                          Kaggle - Indian Water Quality Data 2021-2023</a></p>
                          
                          <p><strong>Data Processing:</strong> 79.4% of the original data was retained for analysis, 
                          ensuring complete cases for robust statistical evaluation. The dataset underwent comprehensive 
                          cleaning and preprocessing to handle missing values and standardize parameter measurements.</p>
                          
                          <p><strong>Key Variables:</strong></p>
                          <ul>
                            <li><strong>STN Code:</strong> Station identification code</li>
                            <li><strong>Monitoring Location:</strong> Specific location where samples were collected</li>
                            <li><strong>Year:</strong> Year of data collection (2021-2023)</li>
                            <li><strong>Type Water Body:</strong> Classification of water body (River, Lake, Pond, Canal, Drain, Marine, Beach, etc.)</li>
                            <li><strong>State Name:</strong> Indian state where monitoring occurred</li>
                            <li><strong>Physical Parameters:</strong> Temperature (Min/Max)</li>
                            <li><strong>Chemical Parameters:</strong> pH, Dissolved Oxygen, Conductivity, BOD, Nitrate</li>
                            <li><strong>Biological Parameters:</strong> Fecal Coliform, Total Coliform</li>
                          </ul>
                          
                          <p><strong>Data Structure:</strong> The dataset contains minimum and maximum values for each parameter, 
                          with calculated averages used for analysis. Data spans multiple states and various types of water bodies, 
                          providing comprehensive coverage of India's water quality monitoring efforts.</p>
                        </div>
                      ")
                    )
                  )
                )
              ),
              
              fluidRow(
                div(
                  class = "collapsible-box collapsed",
                  box(
                    width = 12,
                    status = "success",
                    solidHeader = TRUE,
                    title = tagList(
                      icon("chevron-down"), 
                      "Acknowledgements (Click to expand/collapse)"
                    ),
                    collapsible = FALSE,
                    div(
                      class = "box-body",
                      HTML("
                        <div style='font-size: 14px; line-height: 1.6;'>
                          <h3>Acknowledgements</h3>
                          <p>This dashboard was developed as part of a visualization project on Indian water quality data. 
                          We extend our sincere gratitude to:</p>
                          
                          <p><strong>Dr. Sourish Das</strong> - For his invaluable guidance, expertise in statistical analysis, 
                          and continuous support throughout this project.</p>
                          
                          <p><strong>Dr. Anish Rai</strong> - For his insightful suggestions and mentorship in environmental 
                          data interpretation and visualization techniques.</p>
                          
                          <p>We also acknowledge the original data contributors and the Kaggle community for making this 
                          valuable dataset publicly available for research and educational purposes.</p>
                          
                          <p><em>Note: This dashboard is intended for educational and research purposes. 
                          For official water quality assessments, please refer to authorized government sources.</em></p>
                        </div>
                      ")
                    )
                  )
                )
              )
      ),
      
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_locations", width = 3),
                valueBoxOutput("total_states", width = 3),
                valueBoxOutput("total_years", width = 3),
                valueBoxOutput("data_status", width = 3)
              ),
              fluidRow(
                box(plotlyOutput("water_type_donut"), width = 6,
                    status = "primary", solidHeader = TRUE,
                    title = "Water Body Types Distribution"),
                box(plotlyOutput("year_plot"), width = 6,
                    status = "info", solidHeader = TRUE,
                    title = "Data Collection by Year")
              ),
              fluidRow(
                box(plotlyOutput("state_plot"), width = 12,
                    status = "success", solidHeader = TRUE,
                    title = "Monitoring Locations by State")
              )
      ),
      
      # Spatial Analysis Tab
      tabItem(tabName = "analysis",
              fluidRow(
                box(
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  title = "Select Parameter for Spatial Analysis",
                  fluidRow(
                    column(12,
                           selectInput("analysis_parameter", "Choose Parameter:",
                                       choices = available_params,
                                       selected = "Temperature",
                                       width = "100%")
                    )
                  )
                )
              ),
              fluidRow(
                box(leafletOutput("simple_map", height = "500px"), width = 8,
                    status = "success", solidHeader = TRUE,
                    title = "State-wise Parameter Distribution"),
                box(
                  width = 4,
                  status = "info",
                  solidHeader = TRUE,
                  title = "State Statistics",
                  DTOutput("state_stats_table")
                )
              ),
              fluidRow(
                div(
                  class = "collapsible-box",
                  box(
                    width = 12,
                    class = "box-teal",
                    solidHeader = TRUE,
                    title = tagList(
                      icon("chevron-down"), 
                      "Parameter Distribution by Water Body Type (Click to expand/collapse)"
                    ),
                    collapsible = FALSE,
                    plotlyOutput("param_by_type")
                  )
                )
              )
      ),
      
      # Correlation Analysis Tab
      tabItem(tabName = "correlation",
              fluidRow(
                box(
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  title = "Select Variables for Correlation Analysis",
                  fluidRow(
                    column(6,
                           selectInput("x_var", "X-Axis Variable:",
                                       choices = available_params,
                                       selected = "Temperature")
                    ),
                    column(6,
                           selectInput("y_var", "Y-Axis Variable:",
                                       choices = available_params,
                                       selected = "pH")
                    )
                  )
                )
              ),
              fluidRow(
                box(plotlyOutput("correlation_plot"), width = 8,
                    status = "primary", solidHeader = TRUE,
                    title = "Scatter Plot"),
                box(
                  width = 4,
                  status = "success",
                  solidHeader = TRUE,
                  title = "Correlation Statistics",
                  verbatimTextOutput("correlation_stats")
                )
              ),
              fluidRow(
                div(
                  class = "collapsible-box collapsed",
                  box(
                    width = 12,
                    status = "warning",
                    solidHeader = TRUE,
                    title = tagList(
                      icon("chevron-down"), 
                      "Correlation by Water Body Type (Click to expand/collapse)"
                    ),
                    collapsible = FALSE,
                    plotlyOutput("correlation_by_type")
                  )
                )
              )
      ),
      
      # WQI Analysis Tab
      tabItem(tabName = "wqi",
              fluidRow(
                box(
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  title = "Water Quality Index (WQI) Analysis",
                  HTML("
                    <div style='font-size: 14px; line-height: 1.6;'>
                      <p><strong>What is WQI?</strong> The Water Quality Index is a single number that expresses the overall water quality 
                      at a certain location and time based on several water quality parameters. It provides a simple and 
                      understandable tool for decision-makers and the public to assess water quality.</p>
                      
                      <p><strong>Calculation Method:</strong> We use a weighted arithmetic mean approach:</p>
                      <ul>
                        <li><strong>WQI = (pH_Index × 0.15) + (DO_Index × 0.25) + (BOD_Index × 0.20) + (TC_Index × 0.20) + (Nitrate_Index × 0.10) + (FC_Index × 0.10)</strong></li>
                        <li>Each parameter is converted to a sub-index (0-100) based on water quality standards</li>
                        <li>Weights reflect the relative importance of each parameter</li>
                        <li><strong>Note:</strong> Drains and canals receive a 30% penalty to reflect their typically poorer water quality</li>
                      </ul>
                      
                      <p><strong>WQI Classification:</strong></p>
                      <ul>
                        <li><span style='color: #4575b4; font-weight: bold;'>91-100: Excellent</span> - Very clean, safe for all uses</li>
                        <li><span style='color: #91bfdb; font-weight: bold;'>76-90: Good</span> - Clean, safe for swimming and fishing</li>
                        <li><span style='color: #e0f3f8; font-weight: bold;'>61-75: Fair</span> - Moderately polluted, suitable for irrigation</li>
                        <li><span style='color: #fee090; font-weight: bold;'>31-60: Poor</span> - Polluted, poses health risk</li>
                        <li><span style='color: #fc8d59; font-weight: bold;'>0-30: Very Poor</span> - Heavily polluted, severely degraded</li>
                      </ul>
                      
                      <p><strong>Note:</strong> Analysis includes only water body types with sufficient data (≥10% of total samples).</p>
                    </div>
                  ")
                )
              ),
              fluidRow(
                box(
                  width = 8,
                  status = "primary",
                  solidHeader = TRUE,
                  title = "WQI Distribution by Water Body Type",
                  plotlyOutput("wqi_by_type")
                ),
                box(
                  width = 4,
                  status = "success",
                  solidHeader = TRUE,
                  title = "WQI Statistics Summary",
                  DTOutput("wqi_summary_table")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  status = "warning",
                  solidHeader = TRUE,
                  title = "WQI Classification Distribution",
                  plotlyOutput("wqi_classification_plot")
                )
              )
      ),
      
      # Data Table Tab - UPDATED NAME AND ADDED DOWNLOAD BUTTONS
      tabItem(tabName = "data",
              fluidRow(
                box(
                  width = 12,
                  status = "primary", 
                  solidHeader = TRUE,
                  title = "Water Quality Data",
                  fluidRow(
                    column(6,
                           downloadButton("download_clean_data", "Download Clean Data", 
                                          class = "btn-primary")
                    ),
                    column(6,
                           actionButton("redirect_kaggle", "Download Raw Data from Kaggle",
                                        class = "btn-kaggle",
                                        icon = icon("external-link"),
                                        onclick = "window.open('https://www.kaggle.com/datasets/rishabchitloor/indian-water-quality-data-2021-2023', '_blank')")
                    )
                  ),
                  br(),
                  DTOutput("data_table")
                )
              )
      )
    )
  )
)

# Enhanced Server with Simple Map and Working Collapsible Section
server <- function(input, output, session) {
  
  # Get current active tab
  active_tab <- reactive({
    input$sidebar
  })
  
  # FIXED: Properly handle water body type filtering
  filtered_data <- reactive({
    data <- water_data
    
    # Apply state filter
    if(input$state != "All") {
      data <- data[data$State_Name == input$state, ]
    }
    
    # Apply year filter
    if(input$year != "All") {
      data <- data[data$Year == input$year, ]
    }
    
    # Apply water body type filter - only for tabs where it's available
    if(active_tab() %in% c("overview", "analysis") && input$water_type != "All") {
      data <- data[data$Type_Water_Body == input$water_type, ]
    }
    
    data
  })
  
  # NEW: Download handlers for data files
  output$download_clean_data <- downloadHandler(
    filename = function() {
      "water_data_clean.csv"
    },
    content = function(file) {
      write.csv(water_data, file, row.names = FALSE)
    }
  )
  
  # Reactive for WQI data
  wqi_data <- reactive({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(data.frame())
    }
    
    # Calculate WQI for filtered data
    data_with_wqi <- calculate_wqi(data)
    
    if(nrow(data_with_wqi) == 0) {
      return(data.frame())
    }
    
    # Filter water body types with sufficient data (≥10% of total samples)
    total_samples <- nrow(data_with_wqi)
    type_counts <- table(data_with_wqi$Type_Water_Body)
    valid_types <- names(type_counts)[type_counts >= total_samples * 0.1]
    
    # Filter data to only include valid water body types
    data_with_wqi <- data_with_wqi[data_with_wqi$Type_Water_Body %in% valid_types, ]
    
    return(data_with_wqi)
  })
  
  # [Rest of your existing server code remains exactly the same...]
  # WQI by Water Body Type Plot
  output$wqi_by_type <- renderPlotly({
    data <- wqi_data()
    
    if(nrow(data) == 0) {
      return(plotly_empty() %>% layout(title = "No data available for WQI calculation"))
    }
    
    plot_ly(data, x = ~Type_Water_Body, y = ~WQI, type = "box",
            color = ~Type_Water_Body, 
            colors = color_palettes$vibrant,
            marker = list(
              line = list(color = '#000000', width = 1.5)
            ),
            line = list(color = '#000000', width = 1.5)) %>%
      layout(
        title = "",
        xaxis = list(title = "Water Body Type"),
        yaxis = list(title = "Water Quality Index (WQI)", range = c(0, 100)),
        showlegend = FALSE
      )
  })
  
  # WQI Summary Table
  output$wqi_summary_table <- renderDT({
    data <- wqi_data()
    
    if(nrow(data) == 0) {
      return(datatable(data.frame(Message = "No data available for WQI calculation")))
    }
    
    summary_data <- data %>%
      group_by(Type_Water_Body) %>%
      summarise(
        Avg_WQI = round(mean(WQI, na.rm = TRUE), 2),
        Min_WQI = round(min(WQI, na.rm = TRUE), 2),
        Max_WQI = round(max(WQI, na.rm = TRUE), 2),
        Samples = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(Avg_WQI)) %>%
      rename(
        `Water Body` = Type_Water_Body,
        `Average WQI` = Avg_WQI,
        `Minimum WQI` = Min_WQI,
        `Maximum WQI` = Max_WQI,
        `Samples` = Samples
      )
    
    datatable(
      summary_data,
      options = list(
        pageLength = 10,
        dom = 't',
        scrollY = "300px"
      ),
      rownames = FALSE
    )
  })
  
  # WQI Classification Plot
  output$wqi_classification_plot <- renderPlotly({
    data <- wqi_data()
    
    if(nrow(data) == 0) {
      return(plotly_empty() %>% layout(title = "No data available for WQI calculation"))
    }
    
    classification_counts <- data %>%
      group_by(Type_Water_Body, WQI_Class) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      complete(Type_Water_Body, WQI_Class, fill = list(Count = 0))
    
    wqi_colors <- c("Very Poor" = "#fc8d59", 
                    "Poor" = "#fee090", 
                    "Fair" = "#e0f3f8", 
                    "Good" = "#91bfdb", 
                    "Excellent" = "#4575b4")
    
    plot_ly(classification_counts, x = ~Type_Water_Body, y = ~Count, 
            type = "bar", color = ~WQI_Class, colors = wqi_colors,
            marker = list(
              line = list(color = '#000000', width = 1)
            )) %>%
      layout(
        title = "",
        xaxis = list(title = "Water Body Type"),
        yaxis = list(title = "Number of Samples"),
        barmode = "stack",
        showlegend = TRUE
      )
  })
  
  # [The rest of your existing server code remains unchanged...]
  # Reactive for parameter data
  param_data <- reactive({
    data <- filtered_data()
    param <- input$analysis_parameter
    
    if(is.null(param) || nrow(data) == 0) {
      return(list(values = numeric(0), unit = "", title = "Parameter", color = "#808080"))
    }
    
    if(param == "Temperature") {
      list(values = data$Avg_Temperature, unit = "°C", title = "Temperature", color = "#FF6B6B")
    } else if(param == "pH") {
      list(values = data$Avg_pH, unit = "", title = "pH", color = "#4ECDC4")
    } else if(param == "Dissolved Oxygen") {
      list(values = data$Avg_Dissolved_Oxygen, unit = "mg/L", title = "Dissolved Oxygen", color = "#45B7D1")
    } else if(param == "Conductivity") {
      list(values = data$Avg_Conductivity, unit = "µS/cm", title = "Conductivity", color = "#96CEB4")
    } else if(param == "BOD") {
      list(values = data$Avg_BOD, unit = "mg/L", title = "BOD", color = "#FFEAA7")
    } else if(param == "Nitrate") {
      list(values = data$Avg_Nitrate, unit = "mg/L", title = "Nitrate", color = "#DDA0DD")
    } else if(param == "Fecal Coliform") {
      list(values = data$Avg_Fecal_Coliform, unit = "MPN/100ml", title = "Fecal Coliform", color = "#98D8C8")
    } else if(param == "Total Coliform") {
      list(values = data$Avg_Total_Coliform, unit = "MPN/100ml", title = "Total Coliform", color = "#FFA07A")
    } else {
      list(values = numeric(0), unit = "", title = "Parameter", color = "#808080")
    }
  })
  
  # State-wise summary for map
  state_summary <- reactive({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(data.frame())
    }
    
    summary <- data %>%
      group_by(State_Name) %>%
      summarise(
        Avg_Temperature = mean(Avg_Temperature, na.rm = TRUE),
        Avg_pH = mean(Avg_pH, na.rm = TRUE),
        Avg_Dissolved_Oxygen = mean(Avg_Dissolved_Oxygen, na.rm = TRUE),
        Avg_Conductivity = mean(Avg_Conductivity, na.rm = TRUE),
        Avg_BOD = mean(Avg_BOD, na.rm = TRUE),
        Avg_Nitrate = mean(Avg_Nitrate, na.rm = TRUE),
        Avg_Fecal_Coliform = mean(Avg_Fecal_Coliform, na.rm = TRUE),
        Avg_Total_Coliform = mean(Avg_Total_Coliform, na.rm = TRUE),
        Locations = n(),
        .groups = 'drop'
      )
    
    param <- input$analysis_parameter
    
    if(is.null(param)) {
      return(data.frame())
    }
    
    if(param == "Temperature") {
      summary$Avg_Value <- summary$Avg_Temperature
      summary$Min_Value <- min(data$Avg_Temperature, na.rm = TRUE)
      summary$Max_Value <- max(data$Avg_Temperature, na.rm = TRUE)
    } else if(param == "pH") {
      summary$Avg_Value <- summary$Avg_pH
      summary$Min_Value <- min(data$Avg_pH, na.rm = TRUE)
      summary$Max_Value <- max(data$Avg_pH, na.rm = TRUE)
    } else if(param == "Dissolved Oxygen") {
      summary$Avg_Value <- summary$Avg_Dissolved_Oxygen
      summary$Min_Value <- min(data$Avg_Dissolved_Oxygen, na.rm = TRUE)
      summary$Max_Value <- max(data$Avg_Dissolved_Oxygen, na.rm = TRUE)
    } else if(param == "Conductivity") {
      summary$Avg_Value <- summary$Avg_Conductivity
      summary$Min_Value <- min(data$Avg_Conductivity, na.rm = TRUE)
      summary$Max_Value <- max(data$Avg_Conductivity, na.rm = TRUE)
    } else if(param == "BOD") {
      summary$Avg_Value <- summary$Avg_BOD
      summary$Min_Value <- min(data$Avg_BOD, na.rm = TRUE)
      summary$Max_Value <- max(data$Avg_BOD, na.rm = TRUE)
    } else if(param == "Nitrate") {
      summary$Avg_Value <- summary$Avg_Nitrate
      summary$Min_Value <- min(data$Avg_Nitrate, na.rm = TRUE)
      summary$Max_Value <- max(data$Avg_Nitrate, na.rm = TRUE)
    } else if(param == "Fecal Coliform") {
      summary$Avg_Value <- summary$Avg_Fecal_Coliform
      summary$Min_Value <- min(data$Avg_Fecal_Coliform, na.rm = TRUE)
      summary$Max_Value <- max(data$Avg_Fecal_Coliform, na.rm = TRUE)
    } else if(param == "Total Coliform") {
      summary$Avg_Value <- summary$Avg_Total_Coliform
      summary$Min_Value <- min(data$Avg_Total_Coliform, na.rm = TRUE)
      summary$Max_Value <- max(data$Avg_Total_Coliform, na.rm = TRUE)
    }
    
    summary <- merge(summary, state_data, by = "State_Name", all.x = TRUE)
    
    return(summary)
  })
  
  # Color palette for map
  map_palette <- reactive({
    summary <- state_summary()
    
    if(nrow(summary) == 0) {
      return(colorBin("Blues", domain = c(0, 1)))
    }
    
    param <- input$analysis_parameter
    
    if(is.null(param)) {
      return(colorBin("Blues", domain = c(0, 1)))
    }
    
    if(param == "Temperature") {
      colorBin("RdYlBu", domain = summary$Avg_Value, reverse = TRUE, bins = 5)
    } else if(param == "pH") {
      colorBin("viridis", domain = summary$Avg_Value, bins = 5)
    } else if(param == "Dissolved Oxygen") {
      colorBin("Blues", domain = summary$Avg_Value, bins = 5)
    } else if(param == "Conductivity") {
      colorBin("YlOrRd", domain = summary$Avg_Value, bins = 5)
    } else if(param == "BOD") {
      colorBin("OrRd", domain = summary$Avg_Value, bins = 5)
    } else if(param == "Nitrate") {
      colorBin("Purples", domain = summary$Avg_Value, bins = 5)
    } else if(param == "Fecal Coliform") {
      colorBin("Reds", domain = summary$Avg_Value, bins = 5)
    } else if(param == "Total Coliform") {
      colorBin("Oranges", domain = summary$Avg_Value, bins = 5)
    } else {
      colorBin("Spectral", domain = summary$Avg_Value, reverse = TRUE, bins = 5)
    }
  })
  
  # Simple Map
  output$simple_map <- renderLeaflet({
    summary <- state_summary()
    p_data <- param_data()
    
    if(nrow(summary) == 0) {
      return(leaflet() %>% 
               addTiles() %>%
               setView(lng = 78.9629, lat = 22.5937, zoom = 4) %>%
               addControl("No data available", position = "topright"))
    }
    
    pal <- map_palette()
    
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 78.9629, lat = 22.5937, zoom = 4)
    
    for(i in 1:nrow(summary)) {
      state <- summary[i, ]
      
      if(!is.na(state$lat) && !is.na(state$lng)) {
        map <- map %>% addCircleMarkers(
          lng = state$lng,
          lat = state$lat,
          radius = state$radius,
          color = "black",
          fillColor = pal(state$Avg_Value),
          fillOpacity = 0.8,
          weight = 2,
          opacity = 1,
          label = state$State_Name,
          popup = paste(
            "<div style='min-width: 250px;'>",
            "<h4>", state$State_Name, "</h4>",
            "<hr>",
            "<b>Average ", p_data$title, ":</b> ", round(state$Avg_Value, 2), " ", p_data$unit, "<br>",
            "<b>Range:</b> ", round(state$Min_Value, 2), " - ", round(state$Max_Value, 2), " ", p_data$unit, "<br>",
            "<b>Monitoring Locations:</b> ", state$Locations,
            "</div>"
          )
        ) %>%
          addLabelOnlyMarkers(
            lng = state$lng,
            lat = state$lat + 0.3,
            label = state$State_Name,
            labelOptions = labelOptions(
              noHide = TRUE,
              textOnly = TRUE,
              style = list(
                "color" = "black",
                "font-weight" = "bold",
                "font-size" = "11px",
                "text-shadow" = "1px 1px 2px white"
              )
            )
          )
      }
    }
    
    map %>% addLegend(
      position = "bottomright",
      pal = pal,
      values = summary$Avg_Value,
      title = paste(p_data$title, "<br>", "(", p_data$unit, ")"),
      opacity = 1,
      labFormat = labelFormat(
        transform = function(x) round(x, 2)
      )
    )
  })
  
  # State Statistics Table
  output$state_stats_table <- renderDT({
    summary <- state_summary()
    p_data <- param_data()
    
    if(nrow(summary) == 0) {
      return(datatable(data.frame(Message = "No data available")))
    }
    
    display_data <- summary %>%
      select(State_Name, Avg_Value, Min_Value, Max_Value, Locations) %>%
      arrange(desc(Avg_Value)) %>%
      rename(
        State = State_Name,
        Average = Avg_Value,
        Minimum = Min_Value,
        Maximum = Max_Value,
        Locations = Locations
      )
    
    datatable(
      display_data,
      options = list(
        pageLength = 10,
        dom = 't',
        scrollY = "300px"
      ),
      rownames = FALSE
    ) %>%
      formatRound(c('Average', 'Minimum', 'Maximum'), 2)
  })
  
  # Value boxes
  output$total_locations <- renderValueBox({
    valueBox(nrow(filtered_data()), "Locations", icon = icon("map-marker"), color = "blue")
  })
  
  output$total_states <- renderValueBox({
    valueBox(length(unique(filtered_data()$State_Name)), "States", icon = icon("flag"), color = "green")
  })
  
  output$total_years <- renderValueBox({
    valueBox(length(unique(filtered_data()$Year)), "Years", icon = icon("calendar"), color = "purple")
  })
  
  output$data_status <- renderValueBox({
    valueBox("Real Data", "Status", icon = icon("database"), color = "orange")
  })
  
  # Donut chart for Water Body Types
  output$water_type_donut <- renderPlotly({
    data <- filtered_data()
    counts <- as.data.frame(table(data$Type_Water_Body))
    colnames(counts) <- c("Type", "Count")
    
    if(nrow(counts) == 0) {
      return(plotly_empty() %>% layout(title = "No data available"))
    }
    
    counts$Percentage <- round(counts$Count / sum(counts$Count) * 100, 1)
    counts$Label <- paste0(counts$Type, "\n", counts$Count, " (", counts$Percentage, "%)")
    
    plot_ly(counts, labels = ~Type, values = ~Count, type = 'pie',
            hole = 0.6,
            textinfo = 'label+percent',
            hoverinfo = 'text',
            text = ~paste(Type, '\nCount:', Count, '\nPercentage:', Percentage, '%'),
            marker = list(colors = color_palettes$water_colors,
                          line = list(color = '#000000', width = 2))) %>%
      layout(title = "",
             showlegend = TRUE,
             legend = list(orientation = "v", x = 1.1, y = 0.5),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             annotations = list(
               list(text = paste("Total:", sum(counts$Count)), 
                    x = 0.5, y = 0.5, 
                    font = list(size = 16, color = 'black'), 
                    showarrow = FALSE)
             ))
  })
  
  # Year Plot
  output$year_plot <- renderPlotly({
    data <- filtered_data()
    counts <- as.data.frame(table(data$Year))
    colnames(counts) <- c("Year", "Count")
    
    if(nrow(counts) == 0) {
      return(plotly_empty() %>% layout(title = "No data available"))
    }
    
    max_count <- max(counts$Count)
    min_count <- min(counts$Count)
    
    counts$color <- sapply(counts$Count, function(x) {
      intensity <- (x - min_count) / (max_count - min_count)
      if(is.na(intensity)) intensity <- 0.5
      rgb(173, 216, 230 + (255 - 230) * intensity, maxColorValue = 255)
    })
    
    plot_ly(counts, x = ~Year, y = ~Count, type = "bar",
            marker = list(color = ~color,
                          line = list(color = '#000000', width = 1.5)),
            text = ~paste("Year:", Year, "<br>Count:", Count),
            hoverinfo = 'text') %>%
      layout(title = "",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Number of Locations"),
             showlegend = FALSE)
  })
  
  # State Plot
  output$state_plot <- renderPlotly({
    data <- filtered_data()
    counts <- as.data.frame(table(data$State_Name))
    colnames(counts) <- c("State", "Count")
    
    if(nrow(counts) == 0) {
      return(plotly_empty() %>% layout(title = "No data available"))
    }
    
    counts <- counts[order(counts$Count, decreasing = FALSE), ]
    
    max_count <- max(counts$Count)
    min_count <- min(counts$Count)
    
    counts$color <- sapply(counts$Count, function(x) {
      intensity <- (x - min_count) / (max_count - min_count)
      if(is.na(intensity)) intensity <- 0.5
      rgb(144, 238, 144 + (34 - 144) * intensity, maxColorValue = 255)
    })
    
    plot_ly(counts, x = ~Count, y = ~State, type = "bar", orientation = "h",
            marker = list(color = ~color,
                          line = list(color = '#000000', width = 1.5)),
            text = ~paste("State:", State, "<br>Count:", Count),
            hoverinfo = 'text') %>%
      layout(title = "",
             xaxis = list(title = "Number of Locations"),
             yaxis = list(title = "State", 
                          categoryorder = "array",
                          categoryarray = counts$State),
             showlegend = FALSE)
  })
  
  output$param_by_type <- renderPlotly({
    p_data <- param_data()
    data <- filtered_data()
    
    if(nrow(data) == 0 || length(p_data$values) == 0 || all(is.na(p_data$values))) {
      return(plotly_empty() %>% layout(title = "No data available"))
    }
    
    plot_data <- data.frame(Type = data$Type_Water_Body, Value = p_data$values)
    plot_data <- plot_data[!is.na(plot_data$Value), ]
    
    if(nrow(plot_data) == 0) {
      return(plotly_empty() %>% layout(title = "No data available"))
    }
    
    unique_types <- unique(plot_data$Type)
    type_colors <- color_palettes$vibrant[1:length(unique_types)]
    
    plot_ly(plot_data, x = ~Type, y = ~Value, type = "box",
            color = ~Type, colors = type_colors) %>%
      layout(title = "",
             xaxis = list(title = "Water Body Type"),
             yaxis = list(title = paste(p_data$title, p_data$unit)),
             showlegend = FALSE)
  })
  
  # Correlation functions
  get_param_values <- function(param_name, data) {
    switch(param_name,
           "Temperature" = data$Avg_Temperature,
           "pH" = data$Avg_pH,
           "Dissolved Oxygen" = data$Avg_Dissolved_Oxygen,
           "Conductivity" = data$Avg_Conductivity,
           "BOD" = data$Avg_BOD,
           "Nitrate" = data$Avg_Nitrate,
           "Fecal Coliform" = data$Avg_Fecal_Coliform,
           "Total Coliform" = data$Avg_Total_Coliform)
  }
  
  get_param_unit <- function(param_name) {
    switch(param_name,
           "Temperature" = "°C",
           "pH" = "",
           "Dissolved Oxygen" = "mg/L",
           "Conductivity" = "µS/cm",
           "BOD" = "mg/L",
           "Nitrate" = "mg/L",
           "Fecal Coliform" = "MPN/100ml",
           "Total Coliform" = "MPN/100ml")
  }
  
  output$correlation_plot <- renderPlotly({
    data <- filtered_data()
    x_var <- input$x_var
    y_var <- input$y_var
    
    if(nrow(data) == 0) {
      return(plotly_empty() %>% layout(title = "No data available"))
    }
    
    x_values <- get_param_values(x_var, data)
    y_values <- get_param_values(y_var, data)
    
    complete_cases <- complete.cases(x_values, y_values)
    x_values <- x_values[complete_cases]
    y_values <- y_values[complete_cases]
    
    if(length(x_values) == 0 || length(y_values) == 0) {
      return(plotly_empty() %>% layout(title = "No complete data available for correlation"))
    }
    
    correlation <- cor(x_values, y_values, use = "complete.obs")
    
    plot_ly(data = data[complete_cases, ], 
            x = ~x_values, 
            y = ~y_values,
            type = 'scatter',
            mode = 'markers',
            marker = list(
              size = 8,
              opacity = 0.7,
              color = '#1f77b4',
              line = list(color = 'darkblue', width = 1)
            ),
            text = ~paste(Monitoring_Location, 
                          "<br>", x_var, ": ", round(x_values, 2), get_param_unit(x_var),
                          "<br>", y_var, ": ", round(y_values, 2), get_param_unit(y_var)),
            hoverinfo = 'text') %>%
      layout(
        title = paste("Correlation:", x_var, "vs", y_var),
        xaxis = list(title = paste(x_var, "(", get_param_unit(x_var), ")")),
        yaxis = list(title = paste(y_var, "(", get_param_unit(y_var), ")")),
        annotations = list(
          x = 0.05, y = 0.95, xref = "paper", yref = "paper",
          text = paste("Correlation:", round(correlation, 3)),
          showarrow = FALSE,
          bgcolor = "white",
          bordercolor = "black",
          borderwidth = 1
        )
      )
  })
  
  output$correlation_stats <- renderPrint({
    data <- filtered_data()
    x_var <- input$x_var
    y_var <- input$y_var
    
    if(nrow(data) == 0) {
      cat("No data available")
      return()
    }
    
    x_values <- get_param_values(x_var, data)
    y_values <- get_param_values(y_var, data)
    
    complete_cases <- complete.cases(x_values, y_values)
    x_values <- x_values[complete_cases]
    y_values <- y_values[complete_cases]
    
    if(length(x_values) == 0 || length(y_values) == 0) {
      cat("No complete data available")
      return()
    }
    
    correlation <- cor(x_values, y_values, use = "complete.obs")
    correlation_test <- cor.test(x_values, y_values)
    
    cat("=== CORRELATION STATISTICS ===\n\n")
    cat("Variables:", x_var, "vs", y_var, "\n")
    cat("Sample size:", length(x_values), "\n")
    cat("Correlation coefficient:", round(correlation, 4), "\n")
    cat("R-squared:", round(correlation^2, 4), "\n")
    cat("P-value:", format.pval(correlation_test$p.value, digits = 4), "\n")
    cat("\nInterpretation:\n")
    if(abs(correlation) > 0.7) {
      cat("Strong correlation\n")
    } else if(abs(correlation) > 0.5) {
      cat("Moderate correlation\n")
    } else if(abs(correlation) > 0.3) {
      cat("Weak correlation\n")
    } else {
      cat("Very weak or no correlation\n")
    }
  })
  
  output$correlation_by_type <- renderPlotly({
    data <- filtered_data()
    x_var <- input$x_var
    y_var <- input$y_var
    
    if(nrow(data) == 0) {
      return(plotly_empty() %>% layout(title = "No data available"))
    }
    
    water_type_counts <- table(data$Type_Water_Body)
    valid_water_types <- names(water_type_counts)[water_type_counts >= 3]
    
    if(length(valid_water_types) == 0) {
      return(plotly_empty() %>% layout(title = "Not enough data for water type analysis"))
    }
    
    plot_list <- list()
    
    for(water_type in valid_water_types) {
      type_data <- data[data$Type_Water_Body == water_type, ]
      x_values <- get_param_values(x_var, type_data)
      y_values <- get_param_values(y_var, type_data)
      
      complete_cases <- complete.cases(x_values, y_values)
      x_values <- x_values[complete_cases]
      y_values <- y_values[complete_cases]
      
      if(length(x_values) >= 3) {
        correlation <- cor(x_values, y_values, use = "complete.obs")
        
        plot_list[[water_type]] <- list(
          x = x_values,
          y = y_values,
          type = 'scatter',
          mode = 'markers',
          name = paste(water_type, "(r =", round(correlation, 2), ")"),
          marker = list(size = 6, opacity = 0.6)
        )
      }
    }
    
    if(length(plot_list) == 0) {
      return(plotly_empty() %>% layout(title = "No sufficient data for water type analysis"))
    }
    
    p <- plot_ly()
    for(trace in names(plot_list)) {
      p <- p %>% add_trace(
        x = plot_list[[trace]]$x,
        y = plot_list[[trace]]$y,
        type = plot_list[[trace]]$type,
        mode = plot_list[[trace]]$mode,
        name = plot_list[[trace]]$name,
        marker = plot_list[[trace]]$marker
      )
    }
    
    p %>% layout(
      title = paste("Correlation by Water Body Type:", x_var, "vs", y_var),
      xaxis = list(title = paste(x_var, "(", get_param_unit(x_var), ")")),
      yaxis = list(title = paste(y_var, "(", get_param_unit(y_var), ")")),
      showlegend = TRUE
    )
  })
  
  # Data table
  output$data_table <- renderDT({
    datatable(filtered_data(), 
              options = list(scrollX = TRUE, pageLength = 10),
              filter = "top") %>%
      formatStyle(colnames(filtered_data()), backgroundColor = '#f9f9f9')
  })
}

# Run the app
shinyApp(ui, server)