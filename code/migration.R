## relook at this in plotly
## 

# sel_data <-migration_df %>% 
#   select(-attrs.df, -OBS_STATUS) %>%  # remove unnecessary columns
#   filter(VAR=="B11") %>% # reduce to variable of interest
#   spread(GEN,obsValue) %>%  # obtain male data
#   mutate(MEN=ifelse(is.na(WMN),TOT,TOT-WMN)) %>% # some women data is NA
#   gather(key=GEN,value=obsValue,TOT,MEN,WMN) %>% 
#   mutate(Year=as.integer(obsTime)) %>%  # further tidying up
#   select(Year,From=CO2,Gender=GEN,To=COU,Count=obsValue)



data <- reactive({
  
  print("enter reactive")
  req(input$mig_Category)
  req(input$mig_years)
  
  print(input$mig_Category)
  print("that was category")
  
  sel_data <-migration_df %>% 
    select(-TIME_FORMAT, -OBS_STATUS) %>%  # remove unnecessary columns
    filter(VAR==input$mig_Category) %>% # reduce to variable of interest
    spread(GEN,obsValue) %>%  # obtain male data
    mutate(MEN=ifelse(is.na(WMN),TOT,TOT-WMN)) %>% # some women data is NA
    gather(key=GEN,value=obsValue,TOT,MEN,WMN) %>% 
    mutate(Year=as.integer(obsTime)) %>%  # further tidying up
    select(Year,From=CO2,Gender=GEN,To=COU,Count=obsValue) #
  
  
  
  
  allImmsbyCountry2000 <-sel_data %>% 
    tbl_df() %>% 
    filter(Gender=="TOT"&Year==input$mig_years) %>% 
    group_by(To) %>% 
    summarize(Total=sum(Count,na.rm=T))
  
  
  allImmsbyCountry2000 <- allImmsbyCountry2000 %>% 
    left_join(countries,by=c("To"="iso3c"))
  
  allImmsbyCountry2000$hover <- with(allImmsbyCountry2000, paste(country.name, '<br>', "Immigrants", Total))
  
  info=list(df=allImmsbyCountry2000,sel_data=sel_data)
  return(info)
  
})


output$mapTo <- renderPlotly({
  
  
  # specify some map projection/options
  
  theTitle <- paste0('Total Immigrants into OECD Countries ',input$mig_years)
  
  plot_ly(data()$df, z = log10(Total),  locations = To, hoverinfo = "text",
          text = paste(country.name,"<br>",Total),
          type = 'choropleth',  ##To is abbrev eg CAN
          color = Total, colors = 'Greens', showlegend = FALSE,
          colorbar = list(title = "Immigrants")) %>% #marker = list(line = l),
    layout(title = theTitle, geo = g)
  
})

mapData <- reactive({
  
  
  
  s <- event_data("plotly_click")
  
  
  
  point <-  s[["pointNumber"]] # as initial index is 0
  
})

output$mapFrom <- renderPlotly({
  
  if (is.null(mapData())) return()
  # print(data())
  
  df <-data()$df
  sel_data <- data()$sel_data
  
  inCountry <-df$To[[mapData()+1]]
  
  countryName <- df$country.name[[mapData()+1]]
  
  mapdf <- sel_data %>% 
    filter(Year==input$mig_years&To==inCountry)
  # 
  # print(glimpse(df))
  # #inCountry
  # 
  # print(df$Count)
  # print(df$From)
  
  theTitle = paste0("Immigrants by Country into ",countryName, " ",input$mig_years)
  
  plot_ly(mapdf, z = log10(Count),  locations = From,hoverinfo = "text",
          text = paste(From,"<br>",Count), type = 'choropleth',  showlegend= FALSE, # not working
          color = Count, colors = 'YlOrRd', colorbar = list(title = "Immi")) %>% 
    #  marker = list(line = l), colorbar = list(title = "Immi")) %>%  ## not sure what line=l does removing marker actually looks better
    layout(title = theTitle, geo = g ) 
  
})


