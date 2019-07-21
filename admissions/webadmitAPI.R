############### WEBADMIT API ######################

library(httr)
library(jsonlite)
library(lubridate)
library(stringr)
library(openxlsx)
library(conflicted)
options(stringsAsFactors = FALSE)
conflict_prefer("filter", "dplyr")

baseurl  <- "https://acomedu.webadmit.org"
apiKey <- "ecd5b40312803093a9b1ae2bdec937be"
headers = c(
  `Content-Type` = 'application/json',
  `x-api-key` = apiKey
)


#List of ":user_identity"
cycle_2015_2016_id <- 244779
cycle_2016_2017_id <- 154615
cycle_2017_2018_id <- 206003
cycle_2018_2019_id <- 256498
cycle_2019_2020_id <- 376806
# ACOM program id 
programid_cycle_2019_2020 <- 8906049343345191927 
programid_cycle_2018_2019 <- 4649827613237463850 

#Custom Field ID for CASPER zscore
#cycle 19-20
cfidCasper <- 761781
# cycle 18-19 Optional String
cfidCasperS <- 760846
# cycle 18-19 Optional number
cfidCasperN <- 760847
  
  
### user scoped endpoints #####
# Export/Report LIST
exportsall <- "api/v1/exports"
# Export/Report Files (check status)
exportstatus <- "/api/v1/exports/:export_id/export_files/:export_file_id"

### User Identity-Scoped Endpoints
userids <-  "/api/v1/user_identities"
decisions <- "/api/v1/user_identities/:user_identity_id/decisions"
orglist <- "/api/v1/user_identities/:user_identity_id/organizations"
pdftemplatelist <- "/api/v1/user_identities/:user_identity_id/pdf_manager_templates"
programlist <- "/api/v1/user_identities/:user_identity_id/programs"
exportlist <- "/api/v1/user_identities/:user_identity_id/exports"
#Export/Report Files (check status in batch) List
exportlist_status <- "/api/v1/user_identities/:user_identity_id/export_files"
#Export/Report Files (initiate a run) POST
runreport <- "/api/v1/user_identities/:user_identity_id/exports/:export_id/export_files"
# Applicant Details Update Applicant Details
appdetails <- "/api/v1/user_identities/:user_identity_id/applicant_details"
# Display the designation for the applicant with the given applicant CAS ID and program ID.
designation <- "/api/v1/user_identities/:user_identity_id/programs/:program_id/applicants_by_cas_id/:applicant_cas_id/designation"
# List of Programs
program <- "/api/v1/user_identities/:user_identity_id/programs"
# Custom Field List # 8906049343345191927 is programid
customfields <- "/api/v1/user_identities/:user_identity_id/programs/8906049343345191927/custom_fields"
# Custom Field ID 4649827613237463850 is program id for cycle 18-19
cfanswer <- "/api/v1/user_identities/:user_identity_id/programs/8906049343345191927/applicants_by_cas_id/:applicant_cas_id/custom_field_answers/:custom_field_id"

########## GET ###############
waget <- function(path, userid = NULL) {
  if(!is.null(userid)) {
   path<- sub(":user_identity_id", userid, path)
  }
  if(!str_detect(path,baseurl)) {
  url <- modify_url(baseurl, path = path)
  } else {url=path}
  WebAdmit_GET <- GET(url, 
                      add_headers(.headers=headers))
  WebAdmit_GET <- rawToChar(WebAdmit_GET$content)
  WebAdmit_GET <- fromJSON(WebAdmit_GET)
  data <- length(WebAdmit_GET)
  return(WebAdmit_GET[data])
}

############### POST ################
wapost <- function(path, userid = NULL, exportid=NULL) {
  if(!is.null(userid) & !is.null(exportid)) {
    path<- sub(":user_identity_id", userid, path)
    path<- sub(":export_id", exportid, path)
  }
  url <- modify_url(baseurl, path = path)
  WebAdmit_POST <- POST(url, 
                        add_headers(.headers=headers))
  WebAdmit_POST <- rawToChar(WebAdmit_POST$content)
  WebAdmit_POST <- fromJSON(WebAdmit_POST)
  return(WebAdmit_POST)
}

############## DOWNLOAD REPORT ####################
wad <- function(path) {
  url <- modify_url(baseurl, path = path)
  WebAdmit_GET <- GET(url, 
                      add_headers(.headers=headers))
  WebAdmit_GET <- rawToChar(WebAdmit_GET$content)
  WebAdmit_GET <- fromJSON(WebAdmit_GET)
  WebAdmit_GET <- WebAdmit_GET$export_files
  reporturl <- length(WebAdmit_GET)
  url <- WebAdmit_GET[reporturl]
  
  download <- GET(url, 
                  encode="json", 
                  add_headers(`X-Api-Key`=apiKey),
                  write_disk("data/EVERYONE.csv", overwrite=TRUE))
}

########### PUT #######################
waput <- function(path, userid = NULL, cfid=NULL, updateProgress = NULL, casid=NULL, zscore=NULL,i=NULL) {
  if(!is.null(userid) & !is.null(casid) & !is.null(cfid)) {
    path<- sub(":user_identity_id", userid, path)
    path<- sub(":applicant_cas_id", casid, path)
    path<- sub(":custom_field_id", cfid, path)
  }
   url <- modify_url(baseurl, path = path)
   #zscore= 1.73
   #data = paste0('{\n  "custom_field_answer": {\n    "field_type": "string",\n    "value": "', zscore,'"\n  }\n}', sep="")
   data = paste0('{\n  "custom_field_answer": {\n    "field_type": "number",\n    "value": ', zscore,'\n  }\n}', sep="")
   WebAdmit_PUT <- PUT(url, 
                       add_headers(.headers=headers),
                       body = data)
   # If we were passed a progress update function, call it
   if (is.function(updateProgress)) {
     text <- paste0(" - zScore being uploaded for applicant ", which(Casperdata$AACOMAS == casid), "/", nrow(Casperdata), " (Please Wait)")
     updateProgress(detail = text)
     }
   if(WebAdmit_PUT$status_code == 200) {
   Result = "Upload Successful"
   } else {
     Result = "Upload Failure"
     }
  df= data.frame(Result)
}

################ CASPER ###################
# casid1 <- 6467043101 # Emmanuel Segui esdc76@gmail.com
# casid2 <- 4655146148 # Joe Donzel esdc76l@gmail.com
# casid3 <- 1661848995 # Sariah Mongas esdc76mturk@gmail.com
# casid4 <- 6751872359 # Renee Lutz esdc767@gmail.com
# casid5 <- 1581575825 # Julie Teava juliesegui45@gmail.com
# casid6 <- 6163124995 # trin tran
# curl to r https://curl.trillworks.com/#r
#
# # 742459   Optional     string
# customfielda <- "/api/v1/user_identities/:user_identity_id/programs/:program_id/applicants_by_cas_id/:applicant_cas_id/custom_field_answers/:custom_field_id"
# customfielda <- "/api/v1/user_identities/376806/programs/8906049343345191927/applicants_by_cas_id/:applicant_cas_id/custom_field_answers/742459"
# 
 
#custom_fields <- waget(customfields, cycle_2019_2020_id)

#  cf <- "/api/v1/user_identities/:user_identity_id/programs/8906049343345191927/applicants_by_cas_id/:applicant_cas_id/custom_field_answers/:custom_field_id"
# cf <- "/api/v1/user_identities/256498/programs/4649827613237463850/applicants_by_cas_id/7277666467/custom_field_answers/760847"
# 
# customfieldid<- 742459
#  casid <- 6163124995
#  zscore <- 145.25368
#  waput(cfanswer, cycle_2019_2020_id,cfidCasper, 6163124995, 145.25368)
#  waget(cf)
# 
# ~ waput(cfanswer, cycle_2019_2020_id,cfidCasper, .x, .y)
# data <- Casperdata %>% select(AACOMAS,zScore) %>% pmap(~ waput(cfanswer, cycle_2019_2020_id,cfidCasper, .x, .y))