library(shiny)
library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(shinythemes)
library(tidyquant)

mortality_tbl <- read_excel("C:/Users/gabor_szabo/OneDrive - Edwards Lifesciences/Documents/R/UseR group contest/global_mortality.xlsx")

# Transform tibble (pivot longer, str_extract and str_trim)
mortality_tbl_long <- mortality_tbl %>% 
    
    # pivot tibble to long format
    pivot_longer(ends_with("(%)"), names_to = "cause", values_to = "proportion")


mortality_tbl_long <- mortality_tbl_long %>% 
    mutate(cause = str_extract(mortality_tbl_long$cause, "[^(%)]+")) %>% 
    mutate(cause = cause %>% str_trim())



# Countries vector
countries <- mortality_tbl_long %>% 
    distinct(country) %>% 
    pull()

# Causes vector
causes <- mortality_tbl_long %>% 
    distinct(cause) %>% 
    pull()


# UI
ui <- navbarPage("Global Mortality App",
    
    theme = shinytheme("sandstone"),

    tabPanel("Between countries by cause",
    # Application title

    # Sidebar with numeric entry panels
    fluidRow(
        
        # first column
        column(5,
               hr(),
               sidebarPanel(width = 6,
                            
                            selectInput(inputId = "country",
                                     label      = "Select countries", 
                                     choices    = countries, 
                                     selected   = c("United States", "Canada", "Hungary", "Italy", "Japan", "China"), 
                                     multiple   = TRUE, 
                                     selectize  = FALSE,
                                     size = 30)
                ),
            
            sidebarPanel(width = 6,
                         
                         selectInput(inputId    = "cause",
                                     label      = "Select cause", 
                                     choices    = causes, 
                                     selected   = "Cancers", 
                                     multiple   = FALSE, 
                                     selectize  = FALSE,
                                     size = 30)
            )
            ),

        # second column
        column(6, 
               offset = 0,
               
               # small multiples plot
               plotOutput("small_multiples", width = "800px", height = "660px")
               
        )
    )
),

    tabPanel("Between countries vs. rest of the world by cause",
             # Application title
             
             # Sidebar with numeric entry panels
             fluidRow(
                 
                 # first column
                 column(5,
                        hr(),
                        sidebarPanel(width = 6,
                                     
                                     selectInput(inputId = "1",
                                                 label      = "Select countries", 
                                                 choices    = countries, 
                                                 selected   = c("United States", "Canada", "Hungary", "Italy", "Japan", "China"), 
                                                 multiple   = TRUE, 
                                                 selectize  = FALSE,
                                                 size = 30)
                        ),
                        
                        sidebarPanel(width = 6,
                                     
                                     selectInput(inputId    = "2",
                                                 label      = "Select cause", 
                                                 choices    = causes, 
                                                 selected   = "Cancers", 
                                                 multiple   = FALSE, 
                                                 selectize  = FALSE,
                                                 size = 30)
                        )
                 ),
                 
                 # second column
                 column(6, 
                        offset = 0,
                        
                        # small multiples plot
                        plotOutput("plot_output_here", width = "800px", height = "500px")
                        
                 )
             )
    ),

    tabPanel("Tab 3")
)

# SERVER
server <- function(input, output) {
    

    
    create_small_multiples <- function(countries, cause_of_death) {
        
        mortality_tbl_long %>% 
            
            filter(country %in% countries) %>%
            filter(cause == cause_of_death) %>% 
            
            ggplot(aes(x = year, y = proportion)) +
            
            geom_line(aes(group = cause), color ="grey", size = 0.9) +
            
            facet_wrap(~ country, ncol = 4) +
            
            theme_tq() +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()) +
            theme(strip.background = element_rect(fill = "#cfd8e6")) +
            theme(strip.text = element_text(colour = "grey30", size = 13)) +
            coord_cartesian(ylim = c(0, NA)) +
            labs(
                title    = "Small Multiples Plot", 
                subtitle = "Between-country comparisons", 
                x        = "Year", 
                y        = "Proportion (% of overall deaths)") +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 15))
        
    }
    
    
    
    # reactive function - CAN DELETE
    selected_countries <- reactive({
        
        req(input$countries)
        
        selected_countries <- input$countries
        
    })
        
        
        
    
    output$small_multiples <- renderPlot({
        
        
        create_small_multiples(input$country, input$cause)
        
        
    })
    
    }

    
    
    


# Run the application 
shinyApp(ui = ui, server = server)


