library('shiny')
library('rCharts')
library('reshape2')

research_data <- read.csv("./data/14_Topic_en_csv_v2.csv", skip = 1)

ind_over_years <- function(country, indicator){
      country_data <- research_data[research_data$Country.Name %in% country,]
      ctry_ind <- country_data[country_data$Indicator.Name == indicator,]
      cMelt <- melt(ctry_ind, id = c('Country.Name', 'Indicator.Name'), measure.vars = 5:59)
      cMelt$date <- gsub('X', '', as.character(cMelt$variable))
      cMelt <- cMelt[!is.na(cMelt$value),]
      return(cMelt)
}

shinyServer(
      function(input, output){

            output$oindicator <- renderText({input$indicator})
            output$ocountry <- renderText({(input$country)})
            
            
            output$plot <- renderChart({
                  
                  data <- data.frame(ind_over_years(input$country, input$indicator))
            
                  m1 <- mPlot(x = 'date', 
                              y = 'value', 
                              data = data, 
                              type = 'Line', 
                              group = 'Country.Name')
                  
                  m1$set(pointSize = 0, 
                         linewidth = 1)
                  
                  m1$set(dom = "plot")

                  return(m1)})
      
      }
)