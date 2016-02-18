


shinyServer(function(input, output,session) {
  
  ## sidebar options
  
  output$a <- renderUI({
  if (input$sbMenu=="migration") { 
    inputPanel(
      selectInput("mig_Category", label="Select Category",migCats),
      sliderInput("mig_years", label="Select Year(s)", min=minYear,max=maxYear,
      value=maxYear, step=1,sep="")
      )
  }
  })
  
  
  # code for individual items
  source("code/migration.R", local = TRUE)
  
  
  
 
  
})