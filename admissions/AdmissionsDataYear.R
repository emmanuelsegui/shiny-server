AdmissionsDataYearUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("adata"))
  
}

AdmissionsDataYear <- function(input, output, session, year, admissions) {
  
  staterep<- read.xlsx(admissions, sheet = 1)
  ethnicity<- read.xlsx(admissions, sheet = 2)
  data<- read.xlsx(admissions, sheet = 3)

  output$adata  <- renderUI({
    Sys.sleep(0.5)
    ethnicity = filter(ethnicity, class==year)
    data = filter(data, class==year)
    tagList(
      column(4,
             box(title="Admissions",
                 infoBox(data$category[1], prettyNum(data$number[1], big.mark=","), icon = icon("window-restore"),color = "aqua", width = 12, fill = TRUE),
                 infoBox(data$category[2], prettyNum(data$number[2], big.mark=","), icon = icon("paperclip"),color = "green", width = 12, fill = TRUE),
                 infoBox(data$category[3], prettyNum(data$number[3], big.mark=","), icon = icon("clipboard-check"),color = "yellow", width = 12, fill = TRUE),
                 infoBox(data$category[4], prettyNum(data$number[4], big.mark=","), icon = icon("user-graduate"),color = "red", width = 12, fill = TRUE)
             ),
             box(title="Matriculants",
                 infoBox(data$category[5], paste0(data$number[5], "%"), icon = icon("male"),color = "aqua", width = 12, fill = TRUE),
                 infoBox(data$category[6], paste0(data$number[6], "%"), icon = icon("female"),color = "green", width = 12, fill = TRUE),
                 infoBox(data$category[7], data$number[7], icon = icon("clipboard-check"),color = "yellow", width = 12, fill = TRUE),
                 infoBox(data$category[8], data$number[8], icon = icon("user-graduate"),color = "red", width = 12, fill = TRUE)
             ),
             box( width=12,
                  output$plot <- renderPlotly({
                    plot_ly(ethnicity, labels = ~group, values = ~value, type = 'pie') %>%
                      layout(title = "Ethnicity", showlegend=FALSE)
                    
                  })
             )
             
      ),
      column(8,
             output$map <- renderLeaflet({
               staterep = filter(staterep, class==year)
               leaflet(data = staterep) %>% addTiles() %>%
                 addMarkers(~Longitude, ~Latitude, popup = ~paste("Number of Students", "<br>", "Coming From", state, ": ", number))

             }),
             output$text <- renderText({
               if (year==2017) {
               HTML(paste("Note: 1 Student represents Hawaii"))
               } else
                 if (year==2019 | year==2021 | year==2022) {
                   HTML(paste("Note: 1 Student represents Alaska"))
                 }  
             })
      )
    )
    
  })
  
}