#
#
library(plotly)
library(shiny)

prov.select <- readRDS("data/file1.rds")



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Canada Employment Income by Occupation (NOC), 2011 National Household Survey"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(position = "left",
    sidebarPanel(
      p("This Shiny app allows you to:"),
      p("- compare the Median salary for males and females
across all the Canadian provinces  for a given occupation."),
      p("- see the salary adjusted by inflation for the selected occupation, province,
        and gender"),
      p("- compare the salary of the selected occupation relative to others in the 
        same province"),
      br(),
      strong("Make your selections and see what happens!"),
      br(),
      textInput("occup", label = h3("Occupation"), value = "Enter Occupation"),
      uiOutput("choose_occupation"),
      em("Use the textbox to filter the occupations in the selection list and 
         select the occupation of interest", style = "color:blue"),
      selectInput("province", label = h3("Province"), 
                  choices = prov.select, 
                  selected = "AB"),
      em("Select the province and gender to see how the median wage for the selected 
         occupation compares to other in the same province", style = "color:blue"),
      selectInput("sex", label = h3("Gender"), 
                  choices =setNames(as.list(c("male","female")),c("male","female")), 
                  selected = "female"),
      em("The median wage for the selected occupation, province, and gender is
         adjusted using the annual Consumer Price Index for the provinces"),
      br(),
      br(),
      strong("Data Source: Statistics Canada, 2011 National Household Survey,
         Statistics Canada Catalogue no. 99-014-X2011042.")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      br(),
      br(),
      textOutput("text1"),
      tags$head(tags$style("#text1{font-size: 20px;
                           font-style: italic;
                           }"
      )),
      br(),
      plotOutput("Plot1"),
      br(),
      br(),
      plotlyOutput("Plot3"),
      br(),
      br(),
      plotlyOutput("Plot2")
    )
  )
))
