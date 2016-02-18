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
    select(Year,From=CO2,Gender=GEN,To=COU,Count=obsValue) %>%  # link to pops
    left_join(countryPops,by=c("From"="countryId","Year"="year")) %>% 
    rename(Population=count)
  
  
  
  
  byYear <-sel_data %>%
    tbl_df() %>%
    filter(Gender=="TOT"&Year==input$mig_years) %>%
    group_by(To) %>%
    summarize(Total=sum(Count,na.rm=T)) %>%
    left_join(countries,by=c("To"="iso3c"))

  #allImmsbyCountry2000$hover <- with(allImmsbyCountry2000, paste(country.name, '<br>', "Immigrants", Total))
  
  # byYear <-sel_data %>% 
  #   tbl_df() %>% 
  #   filter(Gender=="TOT"&Year==input$mig_years) %>% 
  #   group_by(To) %>% 
  #   summarize(Total=sum(Count,na.rm=T)) %>% 
  #   left_join(countries,by=c("To"="iso3c"))
  # 
 #  print(glimpse(byYear))
  
  
  info=list(df=byYear,sel_data=sel_data)
  return(info)
  
})


output$tableTo <- DT::renderDataTable({
  
  
data()$df %>% 
  arrange(desc(Total)) %>% 
  select(Country=country.name,Immigrants=Total) %>% 
    mutate(OECD_pc=round(Immigrants*100/sum(Immigrants,na.rm=T),1)) %>% 
  arrange(desc(Immigrants)) %>%
  DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = TRUE, searching = FALSE,info=FALSE))
  
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
  
  # mapdf <- sel_data %>% 
  #   filter(Year==input$mig_years&To==inCountry) %>%
  #   left_join(countries,by=c("From"="iso3c"))
  
  mapdf <- sel_data %>% 
    filter(Year==input$mig_years&To==inCountry) 
  
  print(glimpse(mapdf))
  # #inCountry
  # 
  # print(df$Count)
  # print(df$From)
  
  theTitle = paste0("Immigrants by Country into ",countryName, " ",input$mig_years)
  
  plot_ly(mapdf, z = log10(Count),  locations = From,hoverinfo = "text",
          text = paste(Country,"<br>",Count), type = 'choropleth',  showlegend= FALSE, # not working
          color = Count, colors = 'YlOrRd', colorbar = list(title = "Immi")) %>% 
    #  marker = list(line = l), colorbar = list(title = "Immi")) %>%  ## not sure what line=l does removing marker actually looks better
    layout(title = theTitle, geo = g ) 
  
})

output$tableFrom <- DT::renderDataTable({
  
  if (is.null(mapData())) return()
  # print(data())
  
  df <-data()$df
  sel_data <- data()$sel_data
  
  inCountry <-df$To[[mapData()+1]]
  
  countryName <- df$country.name[[mapData()+1]]
  
  sel_data %>% 
    arrange(desc(Count)) %>% 
    filter(Gender=="TOT"&Year==input$mig_years&To==inCountry&From!="TOT") %>%
    select(Country,Immigrants=Count,Population) %>% 
    mutate(Imm_pc=round(Immigrants*100/sum(Immigrants,na.rm=T),1),Country_pc=round(Immigrants*100/Population,3)) %>% 
    arrange(desc(Immigrants)) %>%
    DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = TRUE, searching = FALSE,info=FALSE))
  
})
