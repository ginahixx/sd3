#CBSA Frame Suggestions

suppressMessages(library(shiny))
suppressMessages(library(shinyjs))
suppressMessages(library(RMySQL))

debugmode <- FALSE
source("decisions.R")
source("BT_global.R")
source("ST_global.R")
source("Sample Tool.R")

options(mysql = list(
  "host" = "sbwdb.cwnbzgtflvcd.us-west-2.rds.amazonaws.com",
  "port" = 3306,
  "user" = "gina",
  "password" = "CBSA4Funtimes!"
))
databaseName <- "CBSA"
issuestablename <- "CBSADesign"
desisiontablename <- "DesignDecisions"
choicestablename <- "DesignChoices"
#commenttablename <- "FrameComments"
#commenttablename_neea <- "FrameComments_NEEA"
userstablename <- "users"
filteredcommenttable <- 1
#commentchoicestable <- 1
downloadtable <- 1
#issuestable is created below
onestage <- "1 Stage"
twostage <- "2 Stage"


# Get table column info.
GetTableMetadata <- function(tablename) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  # Construct the insert query
  query <- sprintf("SHOW columns FROM %s", tablename)
  # Submit the fetch query and disconnect
  fields <- dbGetQuery(db, query)
  dbDisconnect(db)
  result <- list(fields = fields$Field)
  return (result)
}
#change below to read cols from table
desisiontablefields <- GetTableMetadata(desisiontablename)
#issuestablefields <- c("CBSAID", "DesignIssue", "Status", "Resolution","IssueTitle", "Question", "Choices", "DecisionProcess", "HasDefinedChoices")
#commenttablefields <- c("idCBSAComments", "CBSAIssueID", "Commenter", "organization", "email", "comment", "Response", "Focus", "PickChoice")
commenttablefields <- c("DCID", "DecisionID", "MeetingID",  "email", "Comment")
commentformfields <- c("comment", "PickChoice")
usersqlfields <- c("email", "firstname", "organization")
usertablefields <- c("loginemail", "Commenter", "organization")
  
questionprompt <- "Question Prompt"

registeredEmails <- function() {
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  emails <- tolower(as.character(dbReadTable(db, "users")$email))
  dbDisconnect(db)
  emails
}

getUserInfo <- function(email) {
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  users <- dbReadTable(db, "users")
  users <- users[tolower(email) == tolower(users$email), ]
  dbDisconnect(db)
  users
}

CreateData <- function(data, tablename) {
  #Only works for a single row of data
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  # Construct the query by looping over the data fields
  sqldata <- data#[-1]
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    tablename,
    paste(names(sqldata), collapse = ", "),
    paste(gsub("'", "\\\\'",sqldata), collapse = "', '")
  )
  if(debugmode){
    cat(file=stderr(), "create sql is ", query, ".\n")
  }
  # Submit the query and disconnect
  rowsaffected <- dbExecute(db, query)
  #dbGetQuery(db, query)
  dbDisconnect(db)
  rowsaffected
}

CreateDataMulti <- function(data, tablename){
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  # Construct the query by looping over the data fields
  rowsaffected <- 0
  sqldata <- data
  #very messy way to combine the data rowwise, but works
  rowdata <- do.call(paste,c(sqldata, sep = "', '"))
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES %s",
    tablename,
    paste(names(sqldata), collapse = ", "),
    paste0("('", paste(rowdata, collapse = "'), ('"), "')") 
  )
  if(debugmode){
    cat(file=stderr(), "in CreateDataMulti insert sql is ", query, ".\n")
  }
  try(rowsaffected <- dbExecute(db, query))
  dbDisconnect(db)
  rowsaffected
}

ReadData <- function(tablename) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  query <- sprintf("SELECT * FROM %s", tablename)
  # Submit the fetch query and disconnect
  #cat(file=stderr(), "ReadData sql is ", query, ".\n")
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

LookupFilteredID <- function(tablename, columnname, row){
  # cat(file=stderr(), "LookupFilteredID data is ", unlist(filteredcommenttable), ".\n")
  # cat(file=stderr(), "LookupFilteredID parameters are table:", tablename, ", column:", columnname, ", row:", row, ".\n")
  #data <- tablename[row,c(columnname)]
  #cat(file=stderr(), "LookupFilteredID data is ", unlist(data), ".\n")
   #cat(file=stderr(), "LookupFilteredID call is ", tablename[row,c(columnname)], ".\n")
  #return (filteredcommenttable[row,c("idCBSAComments")])
  return (filteredcommenttable[row,c(columnname)])
  #return (tablename[row,c(columnname)])
}

LoadFilteredData <- function(tablename, key, value){
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  # Construct the query
  #cat(file=stderr(), "filter data has table as ", tablename, " key as ", key, ", and value as ", (value), ".\n")
  query <- sprintf("SELECT * FROM %s Where %s = '%s'", tablename, key, value)
  #cat(file=stderr(), "filter query is ", query, ".\n")
  #cat(file=stderr(), "in loadfilter, query has been assigned.\n")
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

#returns a comment if it already exits
#title is Title string (not ID) from DesignDecisions
LoadUserComment <- function(table, meetingid, title, email){
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  if(debugmode){
    cat(file=stderr(), "LoadUserComment has title as ", title, " email as ", email, ".\n")
  }
  query <- sprintf(paste0("SELECT * FROM ", desisiontablename, 
                          " INNER JOIN %s ON DDID = DecisionID Where Title = '%s' AND email = '%s' AND MeetingID = %s"),
                   table, title, email, meetingid)
  if(debugmode){
    cat(file=stderr(), "filter query is ", query, ".\n")
  }
  #cat(file=stderr(), "in loadfilter, query has been assigned.\n")
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

UpdateData <- function(data, id, tablename, field) {
  #limit data to not include id which is null anyway for some reason
  #assumes id is first field in data
  sqldata <- data[-1] #c("DesignIssue", "Status", "IssueTitle")]
  settext<- paste0(names(sqldata), " = '", gsub("'", "\\\\'", sqldata), "'", collapse=",")
  #cat(file=stderr(), "set text is  ", (settext),".\n")
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "UPDATE %s SET %s WHERE %s = %s ",
    tablename,
    settext, 
    field,
    id
  )
  #query <- dbEscapeStrings(db, query)  #Yikes, it escapes all string delimeters. How is that helpful?
  if(debugmode){
    cat(file=stderr(), "UpdateData sql is ", query, ".\n")
  }
  # Submit the update query and disconnect
  rowsaffected <-dbExecute(db, query)
  dbDisconnect(db)
  rowsaffected
}

DeleteData <- function(id, tablename, key) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  query <- sprintf(
    "DELETE FROM %s WHERE %s = %s",
    tablename, key, id
  )
  if(debugmode){
    cat(file=stderr(), "delete sql is ", query, ".\n")
  }
  # Submit the update query and disconnect
  rowsaffected <-dbExecute(db, query)
  dbDisconnect(db)
  rowsaffected
}

# Cast from Inputs to a one-row data.frame
CastData <- function(data) {
  datar <- data.frame(id = data["CBSAID"],
                      IssueTitle = data["IssueTitle"],
                      DesignIssue = data["DesignIssue"],
                      Status = data["Status"],
                      Resolution = data["Resolution"],
                      #Choices = data["Choices"],
                      Question = data["Question"],
                      DesignProcess = data["DecisionProcess"],
                      HasDefinedChoices = data["HasDefinedChoices"],
                      stringsAsFactors = FALSE)
  rownames(datar) <- data["CBSAID"]
  cat(file=stderr(), "in CastData rows = ", nrow(datar), ".\n")
  return (datar)
}

# Cast from Inputs to a one-row data.frame
CastCommentData <- function(data) {
  datar <- data.frame(cbsaid = data["CBSAIssueID"],
                      comment = data["comment"],
                      Response = data["Response"],
                      PickChoice = data["PickChoice"],
                      stringsAsFactors = FALSE)
  rownames(datar) <- data["CBSAIssueID"]
  return (datar)
}

# Return an empty, new record
#change this to pull fields from a table
CreateDefaultRecord <- function() {
  mydefault <- CastData(list(CBSAID = "0", IssueTitle = "", DesignIssue = "", Resolution = "", Status = "", 
                             Question = "", DecisionProcess = "" 
                             ))
  cat(file=stderr(), "in CreateDefaultRecord rows = ", nrow(mydefault), ".\n")
  return (mydefault)
}

# Fill the input fields with the values of the selected record in the table
UpdateInputs <- function(data, session) {
  cat(file=stderr(), "in UpdateInputs data struct is ", unlist(names(data)), ".\n")
  updateTextInput(session, "id", value = unname(data["CBSAID"]))
  updateTextInput(session, "CBSAIssueID", value = unname(data["CBSAID"]))
  updateTextInput(session, "HasDefinedChoices", value = unname(data["HasDefinedChoices"]))
  updateTextAreaInput(session, "Question", value = unname(data["Question"]))
  updateTextAreaInput(session, "DesignIssue", value = unname(data["DesignIssue"]))
}



#update the comments table with comments  
UpdateComments <- function(tablename, data, session){
  #cat(file=stderr(), "in update comments value is ", unlist(data["CBSAIssueID"]), ".\n")
  data <- LoadFilteredData(tablename, "CBSAIssueID", unlist(data["CBSAIssueID"]))
  return (data)
}

#Get Option List for Issue
GetOptions <- function(issue, session){
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  cat(file=stderr(), "get options passed issue is ", issue, ".\n")
  query <- "Select * from IssueChoices"
  cat(file=stderr(), "get options sql is ", query, ".\n")
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

#load data that doesn't change
choices <- ReadData(choicestablename)
desisiontable <- ReadData(desisiontablename)