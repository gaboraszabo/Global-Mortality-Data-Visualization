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
    mutate(cause = cause %>% str_trim()) %>% 
    mutate(proportion = proportion / 100)



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
                        plotOutput("btw_countries_by_cause",
                                   width  = "800px",
                                   height = "400px",
                                   brush = brushOpts(id         = "btw_countries_by_cause_brush",
                                                     resetOnNew = TRUE
                                                     )
                        ),
                        plotOutput("btw_countries_by_cause_zoom",
                                   width  = "800px",
                                   height = "300px")
                        
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
            
            theme_light() +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.title       = element_text(color = "grey30"),
                strip.background = element_rect(fill = "#cfd8e6"),
                strip.text       = element_text(colour = "grey30", size = 13)
                ) +
            coord_cartesian(ylim = c(0, NA)) +
            labs(
                title    = "Comparison Between Countries - Small Multiples Plot", 
                subtitle = input$cause, 
                x        = "Year", 
                y        = "Proportion (% of overall deaths)") +
            theme(plot.title    = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 15, face = "bold")) +
            scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) +
            scale_y_continuous(labels = scales::percent_format())
        
    }
    

    output$small_multiples <- renderPlot({
        
        
        create_small_multiples(input$country, input$cause)
        
        
    })
 

    
    
# Between countries vs. rest of the world function and plot ----
    
    # range variable for secondary plot for zooming
    ranges2 <- reactiveValues(x = NULL, y = NULL)
    
    
    output$btw_countries_by_cause <- renderPlot({
        
        
        mortality_tbl_long %>% 
            
            filter(cause == input$cause_2) %>% 
            
            
            ggplot(aes(x = year, y = proportion, color = country)) +
            
            geom_line() +
            gghighlight(country %in% input$country_2,
                        unhighlighted_params = list(size = 1, colour = alpha("lightgrey", 0.2))) +
            
            theme_light() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.title       = element_text(color = "grey30"),
                  plot.title    = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 15, face = "bold"),
                  plot.caption = element_text(color = "grey30", size = 12, hjust = 0.5)
            ) +
            labs(
                title    = "Comparison Between Countries vs. Rest of The World by Cause",
                subtitle = input$cause_2,
                x        = "Year", 
                y        = "Proportion (% of overall deaths)",
                caption = "Select a specific part of the upper plot to zoom in on relevant details and have it displayed in the lower plot") +
            scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) +
            scale_y_continuous(labels = scales::percent_format()) +
            scale_color_brewer(palette = "Dark2")
        
    })
    
    
    output$btw_countries_by_cause_zoom <-  renderPlot({
        
        mortality_tbl_long %>% 
            
            filter(cause == input$cause_2) %>% 
            
            
            ggplot(aes(x = year, y = proportion, color = country)) +
            
            geom_line() +
            gghighlight(country %in% input$country_2,
                        unhighlighted_params = list(size = 1, colour = alpha("lightgrey", 0.2))) +
            
            theme_light() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.title       = element_text(color = "grey30"),
                  plot.title    = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 15, face = "bold")
            ) +
            labs(
                x        = "Year", 
                y        = "Proportion (% of overall deaths)") +
            scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) +
            scale_y_continuous(labels = scales::percent_format()) +
            scale_color_brewer(palette = "Dark2") +
            
            coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
        
    })
    
    #create_btw_country_by_cause(input$country_2, input$cause_2)
    
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observe({
        brush <- input$btw_countries_by_cause_brush
        if (!is.null(brush)) {
            ranges2$x <- c(brush$xmin, brush$xmax)
            ranges2$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges2$x <- NULL
            ranges2$y <- NULL
        }
    })
    
    
    
    
    
    
   
# Between selected causes vs. rest of causes by country function and plot ----   
    
    create_btw_cause_by_country <- function(cause_of_death, ctr) {
        
        mortality_tbl_long %>% 
            
            filter(country == ctr) %>% 
            
            ggplot(aes(x = year, y = proportion)) +
            
            geom_line(aes(group = cause, color = cause)) +
            gghighlight(cause %in% cause_of_death,
                        unhighlighted_params = list(size = 1, colour = alpha("grey", 0.24))) +
            
            theme_light() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.title       = element_text(color = "grey30"),
                  plot.title    = element_text(size = 20, face = "bold"),
                  plot.subtitle = element_text(size = 15, face = "bold")
                  ) +
            labs(
                title    = "Comparison Between Selected Causes vs. The Rest of The Causes by Country",
                subtitle = input$country_3,
                x        = "Year", 
                y        = "Proportion (% of overall deaths)") +
            scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) +
            scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
            scale_color_brewer(palette = "Dark2")
        
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


