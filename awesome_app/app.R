library(shiny)
library(tidyverse)

theme_set(theme_minimal())

mismanaged_vs_gdp <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv") %>% 
    janitor::clean_names() %>%
    filter(!is.na(per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day)) %>% 
    rename(
        gdp = gdp_per_capita_ppp_constant_2011_international_rate,
        mismanaged = per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day,
        pop = total_population_gapminder
    )

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput(
                inputId = "countries",
                label = "Select some countries",
                choices = mismanaged_vs_gdp$entity,
                multiple = TRUE
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("misPlt"),
            plotOutput("map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$misPlt <- renderPlot({
        plt <- mismanaged_vs_gdp %>% 
            ggplot(aes(gdp,
                       mismanaged,
                       label = entity)) +
            geom_point(aes(size = pop)) +
            scale_x_log10(labels = scales::dollar_format()) +
            scale_y_log10(labels = scales::percent_format()) +
            labs(x = "GDP per capita",
                 y = "Percent mismanaged waste per capita",
                 subtitle = "Size represents total population",
                 title = "Test",
                 caption = "test") +
            guides(size = "none")
        
        label_data <- mismanaged_vs_gdp %>% 
            filter(entity %in% input$countries)
        
        plt +
            geom_point(data = label_data, aes(size = pop), color = "red") +
            geom_text(data = label_data)
    })
    
    output$map <- renderPlot({
        world <- map_data("world") %>% 
            as_tibble() %>% 
            filter(region != "Antarctica")
        
        
        world %>% 
            left_join(rename(mismanaged_vs_gdp, region = entity)) %>% 
            ggplot(aes(long, lat, group = group, fill = mismanaged)) +
            geom_polygon() +
            coord_map(xlim = c(-180, 180))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
