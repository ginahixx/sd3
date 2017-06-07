#functions for Design Decisions section
settingstablename <- "DDSettings"
controlTableName <- "DesignDecisions"
versionflag <- "version"
appflag <- "app"

#functions ---------------------------------
GetMeetingIDs <- function() {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  query <- paste0("SELECT MeetingID FROM ", settingstablename, " Group By MeetingID") #group by in case table structure changes from one line per
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  return (data)
}

GetSettingsTable <- function(id) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  query <- paste0("SELECT * FROM ", settingstablename, " where MeetingID = ", id)
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  return (data)
}

LoadControlData <- function(id){
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  #select * from DesignDecisions inner join MeetingGroups on DDID = DecisionID where MeetingID = 1
  query <- paste0("SELECT * FROM ", controlTableName, " inner join MeetingGroups on DDID = DecisionID WHERE MeetingID = ", id)
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  return (data)  
}

LoadComments <- function(tablename, meetingid){
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  query <- paste0("SELECT * FROM ", tablename, "  WHERE MeetingID = ", id)
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  return (data)  
}

GetCommentData <- function(tablename, selecteditem, meetingid){
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  
  #cat(file=stderr(), "GetCommentData data has myemail as ", email, ".\n")
  downloadfields <- paste0(" comment, PickChoice, concat_ws(', ', firstname, organization) as 'Submitted by' ")
  if (selecteditem==""){
    query <- paste0("SELECT ", downloadfields, " FROM users INNER JOIN (", desisiontablename, " INNER JOIN ", 
                    tablename, " ON DDID = DecisionID) ON users.email = ", tablename, ".email WHERE MeetingID = ", meetingid)
  }else if (selecteditem=="%"){
    query <- sprintf(paste0("SELECT %s FROM users INNER JOIN (", desisiontablename, " INNER JOIN ", tablename, 
                            " ON DDID = DecisionID) ON users.email = ", tablename, ".email Where Title  like '%s' and MeetingID = ", meetingid),
                     downloadfields, selecteditem, meetingid) 
  }else{
    query <- sprintf(paste0("SELECT %s FROM users INNER JOIN (", desisiontablename, " INNER JOIN ", tablename, 
                            " ON DDID = DecisionID) ON users.email = ", tablename, ".email Where Title  = '%s' and MeetingID = ", meetingid),
                     downloadfields, selecteditem, meetingid) 
  }
  if(debugmode){
    cat(file=stderr(), "in GetCommentData, query is ", query, ".\n")
  }
  data <- dbGetQuery(db, query)
  downloadtable <<- data
  dbDisconnect(db)
  data
}

#AssembleCommentData(commentsTableName(), MeetingID(), topic, userInfo$user$email, input$comment)
AssembleCommentData <- function(tablename, meetingid, decisiontitle, decisionid, email, comment, pickchoice){
  #lookup up commentID
  commentCheck <- LoadUserComment(tablename, meetingid, decisiontitle, email)
  if(debugmode){
    cat(file=stderr(), "in AssembleCommentData nrow(commentCheck) is ", nrow(commentCheck), ", length is ",length(commentCheck), ".\n")
  }
  #build data
  if(nrow(commentCheck)> 0){
    if(debugmode){
      cat(file=stderr(), "in AssembleCommentData nrow >0 section, nrow(commentCheck) is ", nrow(commentCheck), ".\n")
    }
    data <- as.list(commentCheck$DCID)
  }else {
    data <- as.list("")
  }
  names(data) <- "DCID"
  data$DecisionID <- decisionid
  data$MeetingID <- meetingid
  data$email <-email
  data$Comment <- comment
  pickoptions <- pickchoice
  pickstr <- paste(pickoptions, collapse = ", ")
  data$PickChoice <- pickstr
  if(debugmode){
    cat(file=stderr(), "leaving AssembleCommentData.\n")
  }
  return(data)
}

# Text retrieval functions  ----------------------------------------
#pull together parts for modal message
getmodalHTML <- function(data){
  c(data$ModalIntro, data$ContactInfo, "<br><br>")
}

GetInstructionsHTML <- function(data){
  c(data$InstructionsText, data$ContactInfo, "<br><br>")
}

GetIntroHTML <- function(data){
  c(data$IntroText, "<br>")
}

GetExcelLinkHTML <- function(data){
  c(data$ExcelFileLink, "<br>")
}




