#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
## app.R ##

library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(conflicted)
library(dplyr)
library(glue)
library(shinyauthr)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(ggmosaic)
library(stringr)
library(raster)
library(tidyr)
library(ggplot2)
library(tigerstats)
library(shinycssloaders)
library(shinyjs)
library(DT)
library(rhandsontable)
library(knitr)
library(tinytex)
library(openxlsx)
library(sf)
library(lubridate)
library(purrr)
library(rlist)
library(kableExtra)
library(formattable)
source("AdmissionsDataYear.R")
source("data/data.R")
source("webadmitAPI.R")
source("AdmissionsReports.R")

conflict_prefer("select", "dplyr")
conflict_prefer("box", "shinydashboard")
conflict_prefer("filter", "dplyr")
conflict_prefer("layout", "plotly")
conflict_prefer("mean", "mosaic")
conflict_prefer("show", "shinyjs")
conflict_prefer("lag", "dplyr")

sidebar <- dashboardSidebar(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
  sidebarMenu(
    menuItem("Admissions", icon = icon("calendar"), startExpanded = TRUE,
             menuSubItem("Overall Data", tabName = "admissions"),
             menuSubItem("Enrollement Report", tabName = "enroll"),
             menuSubItem("Upload Casper Data", tabName = "uploadcasper")
    )
))

body <- dashboardBody(
  shinyDashboardThemes(
  theme = "onenote"
),
tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(tabName = "admissions", 
            tabsetPanel(
             id = "a_tabs",
             tabPanel("All Classes", 
                      column(6,
                             box(width=12,
                                 withSpinner(plotlyOutput("admissionsall"))
                                ),
                             box( width=4, title = "Ethnicity",
                                  withSpinner(plotlyOutput("aethnicityall", height=400, width=400))
                             ),
                             box( width=8,
                                  withSpinner(plotlyOutput("agenderall"))
                             )),
                      column(6, 
                             box(width=12, title = "State Representation",
                             leafletOutput("astateall", height=700),
                             textOutput("adatalltext")
                             ))
                      ),
             tabPanel("Class of 2017", AdmissionsDataYearUI("2017")),
             tabPanel("Class of 2018", AdmissionsDataYearUI("2018")),
             tabPanel("Class of 2019", AdmissionsDataYearUI("2019")),
             tabPanel("Class of 2020", AdmissionsDataYearUI("2020")),
             tabPanel("Class of 2021", AdmissionsDataYearUI("2021")),
             tabPanel("Class of 2022", AdmissionsDataYearUI("2022"))
             #tabPanel("Class of 2023", AdmissionsDataYearUI("2023"))
             
           )
            
    ),
    tabItem(tabName = "enroll", useShinyjs(),
            div(id="reportsintro", width=5, 
                  HTML("<h1> Admissions Report Generation </h1>"),
                  HTML("<p> This dashboard will allow you to create the following admissions reports</p>"),
                  HTML("<p> To start generating any report, you will need to click the button below to generate the template that will be available to you </p>"),
                
                  actionBttn(
                    inputId = "webadmitapi",
                    label = "Prepare Report Generation Templates", 
                    style = "material-flat",
                    icon = icon("sliders"),
                    color = "danger"
                  ),
                  hidden
                  (
                    actionBttn(
                    inputId = "webadmitapistarting",
                    label = "Preparing Templates.... ", 
                    style = "material-flat",
                    icon = icon("sliders"),
                    color = "success"
                      )
                  )
            ),
            
            hidden (
              div(id = "reports_tabs_div",
              tabsetPanel(
                id = "reports_tabs",
              tabPanel(id="defaultreport", title= "Generate an Admission Report",
                
                column(3,
                dateInput("date", "Date:", value = Sys.Date(), width=150, format = "mm/dd/yyyy"),
                numericInput("ApplicationsDownloaded", "Applications Downloaded:", 10, width=200),
                numericInput("SupplementalRequested", "Supplemental Requested:", 10, width=200),
                numericInput("SupplementalReceived", "Supplemental Received:", 10, width=200),
                numericInput("CompletedPackages", "Completed Packages:", 10, width=200),
                actionButton("saveadmissionsreport", label = "Save Data to File"),
                downloadButton("downloadreport", "Generate report"),
                verbatimTextOutput("debug")
                ),

                column(2,
                numericInput("Interviewed", "Interviewed:", 10, width=200),
                numericInput("Accepted", "Accepted:", 10, width=200),
                numericInput("Waitlisted", "Waitlisted:", 10, width=200),
                numericInput("Rejected", "Rejected:", 10, width=200),
                numericInput("DepositsDeceived", "Deposits Deceived:", 10, width=200)

                )
           )
           )
           )
           )),
    tabItem(tabName = "uploadcasper", useShinyjs(),
            div(id="casperdiv",
                box(width=3,
                fileInput("casperfile", "Choose Casper Excel File",
                      multiple = TRUE,
                      accept = c(".xls",".XLS", ".xlsx")),
                
                hidden
                (
                  uiOutput("casperprint"),
                  actionBttn(
                    inputId = "casperput",
                    label = "Upload zScores for Applicants ", 
                    style = "material-flat",
                    icon = icon("sliders"),
                    color = "warning"
                    ),
                  actionBttn(
                    inputId = "casperputstarting",
                    label = "Uploading zScores... ", 
                    style = "material-flat",
                    icon = icon("sliders"),
                    color = "success"
                    ),
                  uiOutput("casperDTafterupload")
                )
                
                  ),
            box(width=9,
                uiOutput("caspercontents"),
                
                hidden
                ( DTOutput("caspercontentsupdate"),
                  uiOutput("casperprintafterupload")
                  
                )
                  )
            )
            )

    
  )
)


ui <- dashboardPage(
    dashboardHeader(title = "ACOM"),
  sidebar,
  body
)

################## S E R V E R    S I D E ############################


server <- function(input, output, session) {
  ## READ MASTER DATA
  master <- read.xlsx(xlsMaster, sheet = 1)
  
  ## READ ADMISSION DATA
  admissions <- loadWorkbook(xlsAdmissions)
  a_staterep<- read.xlsx(admissions, sheet = 1)
  a_ethnicity<- read.xlsx(admissions, sheet = 2)
  a_data<- read.xlsx(admissions, sheet = 3)
  a_report <- read.xlsx(admissions, sheet = 4)
  
#### ALL CLASSES ADMISSION DATA  

  output$admissionsall <- renderPlotly({
    a_data <-
      filter(a_data,
             category == c("AACOMAS", "SUPPLEMENTAL", "INTERVIEW", "STUDENTS"))
    a_data$class <- as.factor(a_data$class)
    a_data$category <-
      factor(
        a_data$category,
        levels = c(
          "AACOMAS",
          "SUPPLEMENTAL",
          "INTERVIEW",
          "STUDENTS",
          "AVERAGE GPA",
          "AVERAGE MCAT",
          "MALE",
          "FEMALE"
        )
      )
    p <-
      ggplot(data = a_data,
             aes(
               x = class,
               y = number,
               group = category,
               color = category
             )) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      theme_minimal() +
      ggtitle("Admissions Data") +
      scale_x_discrete(name = "Classes") +
      scale_y_continuous(name = "") +
      theme(
        axis.title.x = element_text(
          face = "bold",
          colour = "#990000",
          size = 14
        ),
        plot.title = element_text(
          color = "black",
          size = 12,
          face = "bold",
          hjust = 0.5
        ),
        axis.text.x  = element_text(size = 10)
      ) +
      labs(colour = "")
    ggplotly(p, tooltip = c("group", "y"))
  })
  
  
  output$aethnicityall <- renderPlotly({
    m <- list(
      l = 0,
      r = 200,
      b = 100,
      t = 0,
      pad = 0
    )
    plot_ly(
      a_ethnicity,
      labels = ~ group,
      values = ~ value,
      type = 'pie'
    ) %>%
      layout(
        
        margin = m,
        showlegend = FALSE
      )
    
  })
  
  output$agenderall <- renderPlotly({
    a_data <- filter(a_data, category == c("MALE", "FEMALE"))
    a_data$class <- as.factor(a_data$class)
    p <-
      ggplot(data = a_data,
             aes(
               x = class,
               y = number,
               group = category,
               color = category
             )) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      theme_minimal() +
      ggtitle("Gender (in percentage)") +
      scale_x_discrete(name = "Classes") +
      scale_y_continuous(name = "") +
      theme(
        axis.title.x = element_text(
          face = "bold",
          colour = "#990000",
          size = 14
        ),
        plot.title = element_text(
          color = "black",
          size = 12,
          face = "bold",
          hjust = 0.5
        ),
        axis.text.x  = element_text(size = 10)
      ) +
      labs(colour = "")
    ggplotly(p, tooltip = c("group", "y"))  %>% layout(hovermode = "closest")
  })
  

  output$astateall <- renderLeaflet({
    a_staterepsum <-
      a_staterep %>% group_by(state) %>% summarize(total = sum(number, na.rm = T))
    a_staterep <-
      inner_join(a_staterep, a_staterepsum, by = c("state" = "state"))
    leaflet(data = a_staterep) %>% addTiles() %>%
      addMarkers(
        ~ Longitude,
        ~ Latitude,
        popup = ~ paste("Number of Students", "<br>", "Coming From", state, ": ", total)
      )
    
  })
  output$adatalltext <- renderText({
    HTML(
      paste(
        "Note: 1 Student from Class of 2017 represents Hawaii and 5 students from classes of 2019, 2021 and 2022 represent Alaska"
      )
    )
  })

## ADMISSION DATA PER YEAR
  callModule(AdmissionsDataYear, "2017", 2017, admissions)
  callModule(AdmissionsDataYear, "2018", 2018, admissions)
  callModule(AdmissionsDataYear, "2019", 2019, admissions)
  callModule(AdmissionsDataYear, "2020", 2020, admissions)
  callModule(AdmissionsDataYear, "2021", 2021, admissions)
  callModule(AdmissionsDataYear, "2022", 2022, admissions)
  #callModule(AdmissionsDataYear, "2023", 2023, admissions)
  
##### ADMISSIONS REPORT ####################  
  
  observeEvent(input$saveadmissionsreport, {
    wb <- loadWorkbook(xlsAdmissions)
    a_report <- read.xlsx(wb, sheet = 5)
    a_report <- a_report %>%
                   add_row(Date=as_date(input$date),
                          Applications.Downloaded = input$ApplicationsDownloaded,
                          Supplemental.Requested = input$SupplementalRequested,
                          Supplemental.Received = input$SupplementalReceived,
                          Completed.Packages = input$CompletedPackages,
                          Interviewed = input$Interviewed,
                          Accepted = input$Accepted,
                          Waitlisted = input$Waitlisted,
                          Rejected = input$Rejected,
                          Deposits.Deceived = input$DepositsDeceived
                          )
    names(a_report)<-str_replace_all(names(a_report), c("[.]" = " "))
    boldHeader <- createStyle(textDecoration = 'bold') # Makes first row bold
    if (!('Supplemental Table 1' %in% names(wb))) addWorksheet(wb, 'Supplemental Table 1')
    writeData(wb, 'Supplemental Table 1', a_report, headerStyle = boldHeader)
    setColWidths(wb, 'Supplemental Table 1', cols = 1:ncol(a_report), widths = 'auto')
    saveWorkbook(wb, xlsAdmissions, overwrite = T)
  })
  
  

  observeEvent(input$webadmitapi,{
      hide("webadmitapi")
      show("webadmitapistarting")
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Computing data", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      # Create a callback function to update progress.
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      webadmitAPI(updateProgress)
      hide("reportsintro")
      show("reports_tabs_div")
    }
  )

    output$downloadreport <- downloadHandler(
      
      filename = "report.pdf",
      content = function(file) {
        print("create ls")
        ls <- localstatusreport(APIdata)
        print("ls created, calling finalreport function now")
        final<- finalreport(ls)
        #table<- finaltable(ls, plots[[3]])
        #params <- list(plots = plots, table = table)
        print("finalreport done, build params")
        print(final)
        params <- list(final = final)
        rmarkdown::render(
          "AdmissionsReport.Rmd",
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )
    
    
############ CASPER #################    
    casper <- reactiveValues(data = NULL)
    
    observe({
      
      shiny::validate(
        need(!is.null(input$casperfile), "Input data")
      )
      
      casper$data = read.xlsx(input$casperfile$datapath)
    })
    

    output$caspercontents <- renderUI({
      if(is.null(casper$data)) return(NULL)
      #Casperdata <<- read.xlsx(input$casperfile$datapath)
      if ("AACOMAS" %in% names(casper$data))
      {
        show("casperprint")
        show("casperput")
        DTOutput("aa")
        
      } else { 
        uiOutput("wrongfile") 
      }
    })
    
    output$caspercontentsupdate <- renderDT({
      if(is.null(casper$data)) return(NULL)
      CasperdataUploaded 
    },
    selection = list(target = 'column', selected = 4),
    extensions = c('KeyTable', 'Responsive', 'ColReorder', 'AutoFill'),
    rownames = FALSE,
    class = 'compact row-border stripe',
    options = list(
      lengthChange=TRUE,
      searching=TRUE,
      autoWidth=FALSE,
      paging=TRUE,
      #dom = 'Bfrtip',
      keys = TRUE,
      colReorder = TRUE,
      autoFill = TRUE),
    
    ) 
    
    output$aa <-renderDT({
      
      Casperdata<<- casper$data  %>% select(AACOMAS, firstName, lastName, zScore)
    },
    extensions = c('KeyTable', 'Responsive', 'ColReorder', 'AutoFill'),
    rownames = FALSE,
    class = 'compact row-border stripe',
    options = list(
      lengthChange=TRUE,
      searching=TRUE,
      autoWidth=FALSE,
      paging=TRUE,
      #dom = 'Bfrtip',
      keys = TRUE,
      colReorder = TRUE,
      autoFill = TRUE)
    
    )
    
    output$wrongfile <-renderUI(HTML("The file you uploaded doesn't contain any AACOMAS ids.<br> Click 'browse' to upload another file"))
    output$casperprint <- renderUI({
      HTML(paste("<h1></h1>", "The zScores of the <b>", nrow(casper$data) ,"following applicants </b> found in the Casper file are going to be uploded to WebAdmit. <br><br> Each zScore is going to be uploaded in the custom field, to its corresponding AACOMAS (CAS ID) Number. <br><br>Click the button below to start uploading.",
                 "<p><p>")) 
    })
    
    output$casperprintafterupload <- renderUI({
      HTML(paste( "<br>The zScores of the <b>", nrow(casper$data) ,"applicants </b> found in the Casper file have been uploded to WebAdmit. <h4> The last column of the table checks if the zScore was uploaded successfully for each applicant. </h4>")) 
    })
    
    
    
    observeEvent(input$casperput,{
      hide("casperput")
      show("casperputstarting")
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Computing data", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      # Create a callback function to update progress.
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      Casperdata <- casper$data %>% select(AACOMAS, firstName, lastName, zScore)
      data <- map2(Casperdata$AACOMAS, Casperdata$zScore, ~ waput(cfanswer, cycle_2019_2020_id,cfidCasper, updateProgress, .x, .y))
      dfb=list.rbind(data)
      CasperdataUploaded <<- bind_cols(Casperdata,dfb)
      hide("casperprint")
      hide("casperputstarting")
      hide("caspercontents")
      show("caspercontentsupdate")
      show("casperprintafterupload")

    }
    )
    

    

}

shinyApp(ui, server)
