########### API Functions ############

webadmitAPI <- function(updateProgress = NULL) {
  #List all exports
  export_list <- waget(exportlist,cycle_2019_2020_id)
  # Isolate EVERYONE report
  report <- export_list$exports %>% filter(name=="EVERYONE")
  reportid <- report$id
  #Generate EVERYONE report
  export_files <- wapost(runreport,cycle_2019_2020_id, reportid)
  # Find EVERYONE Report ID
  exportid <- export_files$export_files$id
  #Check EVERYONE report Status every 2 seconds until available
  repeat {
    
    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      text <- " - Preparing Templates (Please Wait)"
      updateProgress(detail = text)
    }
    statuslist <- waget(exportlist_status,cycle_2019_2020_id)
    reportstatus <- statuslist$export_files %>% filter(id==exportid)
    if (reportstatus$status == "Available"){
      break
    }
  }
  # Download EVERYONE report to PC
  wad(reportstatus$href)
  # Open EVERYONE report
  csvMaster <- "data/EVERYONE.csv"
  masterapi <- read.csv(csvMaster)
  APIdata <<- masterapi
}



########## REPORTS FUNCTIONS ########################

localstatusreport <- function(data = NULL) {
  localstatuses <- tibble("local_status" = c("None",
                                             "AACOMAS Application Received",
                                             "Supplemental Application Requested",
                                             "Received Supplemental App",
                                             "Complete and In Review",
                                             "Reviewed-No Interview",
                                             "Interview Waitlist - Hold",
                                             "Interview Requested",
                                             "Interview Scheduled",
                                             "Interview Complete",
                                             "Accepted-Waiting Deposit",
                                             "Accept & Deposit",
                                             "Declined Accept Offer",
                                             "Rescinded Deposit",
                                             "Waitlisted",
                                             "Rejected",
                                             "Deferred Admissions",
                                             "Withdrew Application"),
                          "total" = rep(0, 18)
  )
  # Calculations 
  LocalStatus_Totals <- APIdata %>% group_by(local_status) %>% filter(local_status!="") %>% tally()
  
  LocalStatus_Totals <- left_join(localstatuses, LocalStatus_Totals, by="local_status") %>% 
    replace(., is.na(.), 0) %>% 
    select(local_status,n) 
  
  AACOMAS.Application.Received <- sum(LocalStatus_Totals$n)
  
  Accept.and.Deposit <- LocalStatus_Totals %>% 
    filter(local_status %in% c("Accept & Deposit", 
                               "Rescinded Deposit")) 
  Accept.and.Deposit <- sum(Accept.and.Deposit$n)
  
  Complete.and.In.Review <- LocalStatus_Totals %>% 
    filter(local_status %in% c("Accept & Deposit",
                               "Complete and In Review",
                               "Declined Accept Offer",
                               "Interview Requested",
                               "Interview Waitlist - Hold",
                               "Rejected",
                               "Rescinded Deposit",
                               "Reviewed-No Interview",
                               "Waitlisted",
                               "Accepted-Waiting Deposit",
                               "Withdrew Application")) 
  Complete.and.In.Review <- sum(Complete.and.In.Review$n)
  
  Interview.Requested <- LocalStatus_Totals %>% 
    filter(local_status %in% c("Accept & Deposit",
                               "Declined Accept Offer",
                               "Interview Requested",
                               "Rejected",
                               "Rescinded Deposit",
                               "Waitlisted")) 
  Interview.Requested <- sum(Interview.Requested$n)
  
  Interviewed <- LocalStatus_Totals %>% 
    filter(local_status %in% c("Accepted-Waiting Deposit",
                               "Accept & Deposit",
                               "Declined Accept Offer",
                               "Rescinded Deposit",
                               "Waitlisted",
                               "Rejected"))
  Interviewed <- sum(Interviewed$n)
  
  Accept <- LocalStatus_Totals %>% 
    filter(local_status %in% c("Accept & Deposit",
                               "Declined Accept Offer",
                               "Rescinded Deposit",
                               "Accepted-Waiting Deposit"))
  Accept <- sum(Accept$n)
  
  Waitlisted <- LocalStatus_Totals %>% 
    filter(local_status == "Waitlisted")
  Waitlisted <- Waitlisted$n
  
  
  localstatuses <- tibble("status" = c("AACOMAS Application Received",
                                             "Complete and In Review",
                                             "Interview Requested",
                                             "Interviewed",
                                             "Accept",
                                             "Deposit",
                                             "Waitlisted"),
                          "total" = c(AACOMAS.Application.Received,
                                      Complete.and.In.Review,
                                      Interview.Requested,
                                      Interviewed,
                                      Accept,
                                      Accept.and.Deposit,
                                      Waitlisted)
  )
  cycle <- year(Sys.Date())
  cycle <- paste("Cycle of ", cycle, "-", cycle+1, sep="")
  lo <- tibble("Date" = c(cycle),
               "Applications.Received" = c(AACOMAS.Application.Received),
               "Completed" = c(Complete.and.In.Review),
               "Interview.Requested" = c(Interview.Requested),
               "Interviewed" = c(Interviewed),
               "Accepted" = c(Accept),
               "Deposited" = c(Accept.and.Deposit),
               "Waitlisted" = c(Waitlisted))
  
   return(list(localstatuses,lo))
}


#################################

finalreport <- function(data=NULL) {
  
  #  csvMaster <- "data/EVERYONE.csv"
  #  masterapi <- read.csv(csvMaster)
  #  APIdata <<- masterapi
   ls <- localstatusreport(APIdata)
  admissions <- loadWorkbook(xlsApplicationNumbers)
  l <- length(sheets(admissions))
  cycle = list()
  for (i in 1:4) {
    print(paste("loop", i))
    cycle[[i]] <- read.xlsx(admissions, sheet = l - 1)
    # convert numeric from excel to Date format
    cycle[[i]]$Date <- as.Date(cycle[[i]]$Date, origin = "1899-12-30")
    # subset dataframe where month of Sys.Date is equal to month of dataframe
    cycle[[i]] <-
      cycle[[i]][which(month(Sys.Date()) == month(cycle[[i]]$Date)), , drop = FALSE]
    #subset dataframe where day is closest to day of Sys.Date
    cycle[[i]] <-
      cycle[[i]][which.min((day(cycle[[i]]$Date) - day(Sys.Date())) ^ 2), , drop = FALSE]
    l = l - 1
  }
  cycles = list.rbind(cycle)
  cycle <- year(cycles$Date)
  cycle <- paste("Cycle of ", cycle, "-", cycle + 1, sep = "")
  cycles$Date <- cycles$Date %>% format("%m/%d/%Y") %>% replace(, cycle)
  cycles <- rbind(data[[2]], cycles)
  # make sure it's a factor
  cycles$Date <- factor(cycles$Date)
  cycles <-
    gather(cycles,
           status,
           value,
           Applications.Received:Waitlisted,
           factor_key = TRUE)
 
  cyclesplot1 <- cycles %>% filter(status == "Applications.Received")
  cyclesplot2 <- cycles %>% filter(status != "Applications.Received")
  
  print("create final cycles")
  cycles <- cycles %>% group_by(status) %>% 
    arrange(Date, .by_group = TRUE) %>%
    mutate(pctchange = round((value/lag(value) - 1) * 100), digits=0) %>%
    filter(Date=="Cycle of 2019-2020") %>% 
    replace(is.na(.), 0)
  print("create final ls")
  finaltable <- ls[[1]]
  finaltable <- finaltable %>% 
    bind_cols(cycles) %>%
    select(status, total, pctchange) 
  #%>%
  #  rename('Change From Last Cycle (in %)'=pctchange)
  print("after create final ls")
return(list(cyclesplot1,cyclesplot2,finaltable))
}
  
finaltable <- function(localstatuses, data=NULL) {
  data <- data %>% group_by(status) %>% 
    arrange(Date, .by_group = TRUE) %>%
    mutate(pctchange = (value/lag(value) - 1) * 100) %>%
    filter(Date=="Cycle of 2019-2020") %>% 
    replace(is.na(.), 0)
  
  localstatuses <- localstatuses %>% 
    bind_cols(cycles) %>%
    select(status, Total, pctchange) %>%
    rename('Percent Change'=pctchange)
  return(localstatuses)
}


##### SAVE DATA INTO EXCEL

# savedata <- function() {
#   admissions <- loadWorkbook("data/Emmanuel Lenz Report.xlsx")
#   l<-length(sheets(admissions))
#   a_report <- read.xlsx(wb, sheet = 5)
#   a_report <- a_report %>%
#     add_row(Date=as_date(input$date),
#             Applications.Downloaded = input$ApplicationsDownloaded,
#             Supplemental.Requested = input$SupplementalRequested,
#             Supplemental.Received = input$SupplementalReceived,
#             Completed.Packages = input$CompletedPackages,
#             Interviewed = input$Interviewed,
#             Accepted = input$Accepted,
#             Waitlisted = input$Waitlisted,
#             Rejected = input$Rejected,
#             Deposits.Deceived = input$DepositsDeceived
#     )
#   names(a_report)<-str_replace_all(names(a_report), c("[.]" = " "))
#   boldHeader <- createStyle(textDecoration = 'bold') # Makes first row bold
#   if (!('Supplemental Table 1' %in% names(wb))) addWorksheet(wb, 'Supplemental Table 1')
#   writeData(wb, 'Supplemental Table 1', a_report, headerStyle = boldHeader)
#   setColWidths(wb, 'Supplemental Table 1', cols = 1:ncol(a_report), widths = 'auto')
#   saveWorkbook(wb, xlsAdmissions, overwrite = T)
# }


