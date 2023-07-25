# IAVI Assignment for RA-Quantitative 
# Load required packages
library("pacman")
pacman::p_load(shiny,
               sf,
               sp,
               rgdal,
               tidyverse,
               ggplot2,
               terra,
               readxl
)
# Data Import
covid_data <- read_excel(file.choose())

# Calculate the coverage percentage
covid_data = covid_data %>%
  mutate(P18_First_DoseP = (P18_First_Dose/Target_18P)*100,
         P18_Second_DoseP = (P18_Second_Dose/Target_18P)*100,
         B1214_First_DoseP = (B1214_First_Dose/Target_1214)*100,
         B1214_Second_DoseP = (B1214_Second_Dose/Target_1214)*100,
         B1518_First_DoseP = (B1518_First_Dose/Target_1518)*100,
         B1518_Second_DoseP = (B1518_Second_Dose/Target_1518)*100,
         B1859_YearP = (B1859_Year/Target_1859)*100,
         P60_Years_HCWFLW_P = (P60_Years_HCWFLW/Target_60P)*100
  )
# Read shapefile of India from Github 
url="https://github.com/Anup-droid/India_Map/raw/main/Shape_files.zip"
download.file(url,"Shape_files.zip")
unzip("Shape_files.zip")
#Load state shape file of India
ind_shape <- st_read("Shape_files/state.shp")

# Define UI for app
ui <- fluidPage(
  titlePanel("Covid-19 Vaccination Coverage in India"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", 
                  label = "Select Covid-19 vaccination dose:",
                  choices = c("P18_First_DoseP", 
                              "P18_Second_DoseP",
                              "B1214_First_DoseP",
                              "B1214_Second_DoseP",
                              "B1518_First_DoseP",
                              "B1518_Second_DoseP",
                              "B1859_YearP",
                              "P60_Years_HCWFLW_P"
                              ),
                  selected = "P18_First_DoseP")
    ),
    mainPanel(
      plotOutput("ind_map"),
      br(),
      h4("Top performer states"),
      tableOutput("top_states")
    )
  )
)

# Define server for app
server <- function(input, output) {
  
  # Merge data with state shapefile of India by common id
  ind_map_data <- merge(ind_shape, covid_data, by.x = "OBJECTID", by.y = "OBJECTID")
  
  # Generate map
  output$ind_map <- renderPlot({
    ggplot(ind_map_data, aes(fill = !!sym(input$var))) +
      geom_sf(color = "black") +
      scale_fill_viridis_b() +
      labs(title = "Covid-19 dose coverage in India",
           subtitle = paste0(input$var, " Coverage"),
           caption = "Source: MoHFW - Data upto 31Mar-2023") +
      theme(title = element_text(face = "bold"),
            legend.position = "left") +
      theme_void()
  })
  
  # Calculate top performer states
  top_states_data <- reactive({
    covid_data %>% 
      arrange(desc(!!sym(input$var))) %>% 
      select(STATE, !!sym(input$var)) %>% 
      head(5)
  })
  
  # Generate top performer states table
  output$top_states <- renderTable({
    top_states_data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)


