# inspo: https://github.com/ronammar/baby_names_shiny_appy
# state and territory data here: https://www.ssa.gov/oact/babynames/limits.html

library(viridis) 
library(tidyverse)
library(ggrepel)
library(shinythemes)
library(shiny)


options(stringsAsFactors=FALSE)

babyNames <- readRDS("baby_names.rds")

ui <- fluidPage(theme = shinytheme("cerulean"),
# Define UI for application that draws a histogram
                  
  h1("Popularity of baby names in the USA since 1880"),
  h4("(data from ", a("SSA", href="https://www.ssa.gov/oact/babynames/limits.html", target="_blank"), ")"),
  h2("Search for popularity of specific names"),

navlistPanel(
  "Analysis options",
  tabPanel("Names over time",
             mainPanel( textInput("names",
                                  "Lookup names (comma separated):",
                                  value="Taylor, Travis, Jim, Pam, Dwight")),
               plotOutput("density"),
               width=11),
  
  
  tabPanel("Top Names by year",
             mainPanel(
               plotOutput("hist"),
               width=11)
  ),
  tabPanel("Least Popular Names Hist",
           
  mainPanel(
    plotOutput("histLeast"),
    width=11)), 
  
  tabPanel("Least Popular Names Table",
           fluidRow(
             column(12,
                    dataTableOutput('table')
             )))),

  
 column(5,             # Input: Slider for the number of bins ----
                     sliderInput("year", "Birth year",
                                 min=min(babyNames$year),
                                 max=max(babyNames$year), value = 1989), style = "margin-top: -80px;")
)
  

# set up plotting
# Histogram plotter
makeHistogram <- function(inputYear, topNum = 10, topNames=TRUE) {
  # Plot a histogram of name counts for the top names for a given year
  d <- babyNames %>%
    mutate(name=str_to_title(name)) %>%
    filter(year == inputYear) %>%
    group_by(sex)
  
  if (topNames) {
    d <- d %>% 
      arrange(desc(count)) %>%
      top_n(topNum, count)
  } else {  # bottom
    d <- d %>% 
      arrange(count) %>%
      top_n(-topNum, count) %>%
      # there are more than 20 with ties for lowest count, sample 20 random
      sample_n(topNum)
  }
  
  # Below we use custom factor levels to preserve ordering when plotted
  d$name <- factor(d$name, levels=d$name)
  
  ggplot(d, aes(x=name, y=count, fill=sex)) +
    facet_wrap(~ sex, scale="free") +
    geom_bar(stat="identity") +
    labs(x="Baby name", y="Number of babies", fill="Sex") +
    scale_fill_grey() + 
    theme_bw(17) +
    theme(axis.text.x = element_text(angle=60, hjust=1))
}

                
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$hist <- renderPlot({
    makeHistogram(input$year)
    })
  
  output$histLeast <- renderPlot({
    makeHistogram(input$year, topNames=FALSE)})
  
  output$density <- renderPlot({
    # Density plot of specific name popularity over the years
    specificNames <- unlist(str_split(str_to_lower(input$names), ","))
    # Trim any whitespace between names
    specificNames <- str_trim(specificNames)
    
    d <- filter(babyNames, name %in% specificNames) %>% 
      mutate(label = if_else(year == max(year), as.character(name), NA_character_))
    
    ggplot(d, aes(x=year, y=count, color=name)) +
      facet_wrap(~ sex, scales="free", nrow = 2) +
      geom_line(size=1) + 
      geom_label_repel(aes(label = label), show_guide  = FALSE,
                       nudge_x = 1,  nudge_y = 1, na.rm = TRUE) +
      scale_color_viridis_d(option = "magma", end = 0.9) +
      theme_bw(15)
  })
  
  output$table <- renderDataTable(
    babyNames %>%
      mutate(name=str_to_title(name)) %>% 
      filter(year == input$year) %>%
      group_by(sex) %>%
      arrange(count) %>%
      top_n(-15, count),
    options = list(pageLength = 50))
  }
                
# Run the application 
shinyApp(ui=ui, server=server)


