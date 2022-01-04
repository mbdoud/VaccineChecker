library(shiny)

# read in the data
vaccine_components <- read.csv("vaccine_components.csv", header=FALSE, row.names=1) # dataframe with each row named by vaccine name

# define functions to process ingredient lists from the data frame
get_ingredients <- function(vacname) { # returns a list of ingredients for a vaccine name (in lower case, empty elements removed)
  this_list <- tolower(as.list(vaccine_components[vacname,2:ncol(vaccine_components)]))
  this_list <- this_list[this_list != ""] # remove empty elements from the list
  return(trimws(this_list)) # return after trimming leading or ending white space from list elements
} 
get_intersect <- function(vacname1, vacname2) {intersect(get_ingredients(vacname1),get_ingredients(vacname2))} # returns the intersection of ingredients for two vaccine names
formatted_ingredients <- function(vacname) {paste(get_ingredients(vacname), collapse=", ")}
formatted_intersect <- function(vacname1, vacname2) {paste(get_intersect(vacname1, vacname2), collapse=", ")} # formatting the list to print separated by comma

# shiny ui
ui <- fluidPage(
  selectInput("vaccine1", "Select first vaccine", choices = row.names(vaccine_components)),
  selectInput("vaccine2", "Select second vaccine", choices = row.names(vaccine_components)),
  br(),
  textOutput(outputId="vaccine1_ingredients"),
  br(),
  textOutput(outputId="vaccine2_ingredients"),
  br(),
  textOutput(outputId="common_ingredients"),
  br(),
  textOutput(outputId="pegwarning")
)

# shiny server
server <- function(input,output) {
  output$vaccine1_ingredients <- renderText({ paste("The ingredients in the first vaccine are: ", formatted_ingredients(input$vaccine1)) })
  output$vaccine2_ingredients <- renderText({ paste("The ingredients in the second vaccine are: ", formatted_ingredients(input$vaccine2)) })
  
  output$common_ingredients <- renderText({ 
    if(length(get_intersect(input$vaccine1, input$vaccine2)) > 0)
    { paste("The ingredients in common are: ", formatted_intersect(input$vaccine1, input$vaccine2)) }
    else
    { paste("No ingredients in common identified.") }
    })

  output$pegwarning <- renderText({
    
    # Checking to see if PEG and polysorbate are separately present in the selected vaccines
    peg_in_v1 <- grepl("peg", formatted_ingredients(input$vaccine1), ignore.case = TRUE)
    peg_in_v2 <- grepl("peg", formatted_ingredients(input$vaccine2), ignore.case = TRUE)
    poly_in_v1 <- grepl("polysorbate", formatted_ingredients(input$vaccine1), ignore.case = TRUE)
    poly_in_v2 <- grepl("polysorbate", formatted_ingredients(input$vaccine2), ignore.case = TRUE)
    
    if((peg_in_v1 && poly_in_v2) || (peg_in_v2 && poly_in_v1) || (peg_in_v1 && peg_in_v2) || (poly_in_v1 && poly_in_v2))
    { paste("Caution: Some form of polyethylene glycol (PEG) and/or polysorbate are present in both of the selected vaccines any may exhibit allergic cross-reactivity.")}
    else
    { paste("")}
  })
}

# run the shiny app
shinyApp(ui=ui,server=server)