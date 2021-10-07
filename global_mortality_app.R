library(shiny)
library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(shinythemes)
library(tidyquant)
library(gghighlight)

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
    
    # Tab 1 ----
    tabPanel("Between countries by cause",
    # Application title

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

    # Tab 2 ----
    tabPanel("Between countries vs. rest of the world by cause",
             # Application title
             
             fluidRow(
                 
                 # first column
                 column(5,
                        hr(),
                        sidebarPanel(width = 6,
                                     
                                     selectInput(inputId = "country_2",
                                                 label      = "Select countries", 
                                                 choices    = countries, 
                                                 selected   = c("United States", "Canada", "Hungary", "Italy", "Japan", "China"), 
                                                 multiple   = TRUE, 
                                                 selectize  = FALSE,
                                                 size = 30)
                        ),
                        
                        sidebarPanel(width = 6,
                                     
                                     selectInput(inputId    = "cause_2",
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
                        
                        # plot
                        plotOutput("btw_countries_by_cause", width = "800px", height = "500px")
                        
                 )
             )
    ),


    # Tab 3 ----
    tabPanel("Between causes vs. rest of the causes by country",
             # Application title
             
             fluidRow(
                 
                 # first column
                 column(5,
                        hr(),
                        sidebarPanel(width = 6,
                                     
                                     selectInput(inputId = "country_3",
                                                 label      = "Select country", 
                                                 choices    = countries, 
                                                 selected   = c("United States", "Canada", "Hungary", "Italy", "Japan", "China"), 
                                                 multiple   = FALSE, 
                                                 selectize  = FALSE,
                                                 size = 30)
                        ),
                        
                        sidebarPanel(width = 6,
                                     
                                     selectInput(inputId    = "cause_3",
                                                 label      = "Select causes", 
                                                 choices    = causes, 
                                                 selected   = c("Cardiovascular diseases", "Cancers"), 
                                                 multiple   = TRUE, 
                                                 selectize  = FALSE,
                                                 size = 30)
                        )
                 ),
                 
                 # second column
                 column(6, 
                        offset = 0,
                        
                        # plot
                        plotOutput("btw_cause_by_country", width = "800px", height = "500px")
                        
                 )
             )
    )
)

# SERVER
server <- function(input, output) {
    

# Small multiples function and plot ----    
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
            theme(plot.title    = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 15)) +
            scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015))
        
    }
    

    output$small_multiples <- renderPlot({
        
        
        create_small_multiples(input$country, input$cause)
        
        
    })
 

    
    
# Between countries vs. rest of the world function and plot ----
    
    create_btw_country_by_cause <- function(countries, cause_of_death) {
        
        mortality_tbl_long %>% 
            
            filter(cause == cause_of_death) %>% 
            
            
            ggplot(aes(x = year, y = proportion, color = country)) +
            
            geom_line() +
            gghighlight(country %in% countries,
                        unhighlighted_params = list(size = 1, colour = alpha("lightgrey", 0.18))) +
            
            theme_tq() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            labs(
                title    = "Comparison Between Countries vs. Rest of The World by Cause",
                subtitle = input$cause_2,
                x        = "Year", 
                y        = "Proportion (% of overall deaths)") +
            theme(
                plot.title    = element_text(size = 20, face = "bold"),
                plot.subtitle = element_text(size = 15, face = "bold")) +
            scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) +
            scale_color_viridis_d()
        
    }
    
    output$btw_countries_by_cause <- renderPlot({
        
        
        create_btw_country_by_cause(input$country_2, input$cause_2)
        
        
    })
    
   
# Between selected causes vs. rest of causes by country function and plot ----   
    
    create_btw_cause_by_country <- function(cause_of_death, ctr) {
        
        mortality_tbl_long %>% 
            
            filter(country == ctr) %>% 
            
            ggplot(aes(x = year, y = proportion)) +
            
            geom_line(aes(group = cause, color = cause)) +
            gghighlight(cause %in% cause_of_death,
                        unhighlighted_params = list(size = 1, colour = alpha("grey", 0.2))) +
            
            theme_tq() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            labs(
                title    = "Comparison Between Selected Causes vs. The Rest of The Causes by Country",
                subtitle = input$country_3,
                x        = "Year", 
                y        = "Proportion (% of overall deaths)") +
            theme(
                plot.title    = element_text(size = 20, face = "bold"),
                plot.subtitle = element_text(size = 15, face = "bold")) +
            scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) +
            
            scale_color_viridis_d()
        
    }
    
    
     
    output$btw_cause_by_country <- renderPlot({
        
        
        create_btw_cause_by_country(input$cause_3, input$country_3)
    
    })
    
    
    
    
       
    }

    


# reactive function - CAN DELETE
selected_countries <- reactive({
    
    req(input$countries)
    
    selected_countries <- input$countries
    
})
  
    


# Run the application 
shinyApp(ui = ui, server = server)


