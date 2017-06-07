#Building Type

library(ggplot2)
btsettingtablename <-"BldgTypesSettings"

commentformfieldsBT <- c("commentBT")
frame <-1
# updateDataTables <- function() {
#   if(debugmode){
#     cat(file=stderr(), "*** Reloading data.\n")
#   }
#   buildingtypes <- read.csv("buildingtypes.csv", strip.white=TRUE)
#   detailedBT <- read.csv("detailedBT.csv")
#   detailedBT$detail <- paste0(detailedBT$SBW_Frame_Detailed_BT, " [", detailedBT$Count,"]")
#   load("frame.rda")
#   btfieldname <<- names(frame)[1]
#   sffieldname <<- names(frame)[3]
#   
#   blankBT <- sum(length(which(frame$BLD_TYP_2009.CBSA=="")))
#   blankSQ <- sum(is.na(frame$SBW_Frame_Floor_Area))
#   blankBoth <- sum(length(which(frame$BLD_TYP_2009.CBSA=="" & is.na(frame$SBW_Frame_Floor_Area))))
#   totalcount <- nrow(frame)
#   tblMissing <- format(c(totalcount, blankBT, blankSQ, blankBoth), big.mark=",",scientific=FALSE)
#   names(tblMissing) <- c("Total Properties", "Missing Building Type", "Missing Square Footage", "Missing Both BT and SF")
#   tblMissing <<- as.data.frame(t(tblMissing))
#   
#   renameblanks <- "Missing Bldg Type, Possible Duplicates"
#   levels(frame$BLD_TYP_2009.CBSA) <- c(levels(frame$BLD_TYP_2009.CBSA),renameblanks)
#   levels(buildingtypes$BLD_TYP_2009.CBSA) <- c(levels(buildingtypes$BLD_TYP_2009.CBSA),renameblanks)
#   levels(detailedBT$BLD_TYP_2009.CBSA) <- c(levels(detailedBT$BLD_TYP_2009.CBSA),renameblanks)
#   frame$BLD_TYP_2009.CBSA[frame$BLD_TYP_2009.CBSA == ""] <- renameblanks
#   buildingtypes$BLD_TYP_2009.CBSA[buildingtypes$BLD_TYP_2009.CBSA == ""] <- renameblanks
#   detailedBT$BLD_TYP_2009.CBSA[detailedBT$BLD_TYP_2009.CBSA == ""] <- renameblanks
#   buildingtypes <<- buildingtypes
#   detailedBT <<- detailedBT
#   frame <<- frame
#   
# }
#BTtablename <- 



LoadUserCommentBT <- function(table, meetingid, title, email){
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  if(debugmode){
    cat(file=stderr(), "LoadUserCommentBT has title as ", title, " email as ", email, ".\n")
  }
  query <- sprintf(paste0("SELECT * FROM %s Where DetailID = '%s' AND email = '%s' AND MeetingID = %s"),
                   table, title, email, meetingid)
  if(debugmode){
    cat(file=stderr(), "LoadUserCommentBT query is ", query, ".\n")
  }
  #cat(file=stderr(), "in LoadUserCommentBT, query has been assigned.\n")
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

GetCommentDataBT <- function(tablename, selecteditem, meetingid){
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  
  downloadfields <- paste0(" comment, concat_ws(', ', firstname, organization) as 'Submitted by' ")
  if (selecteditem==""){
    query <- paste0("SELECT ", downloadfields, " FROM users INNER JOIN ", tablename, " ON users.email = ", tablename, ".email WHERE MeetingID = ", meetingid)
  }else if (selecteditem=="%"){
    query <- sprintf(paste0("SELECT %s FROM users INNER JOIN ", tablename, " ON users.email = ", tablename, ".email Where DetailID  like '%s' and MeetingID = ", meetingid),
                     downloadfields, selecteditem, meetingid) 
  }else{
    query <- sprintf(paste0("SELECT %s FROM users INNER JOIN ", tablename, " ON users.email = ", tablename, ".email Where DetailID = '%s' and MeetingID = ", meetingid),
                     downloadfields, selecteditem, meetingid) 
  }
  if(debugmode){
    cat(file=stderr(), "in GetCommentData, query is ", query, ".\n")
  }
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

#AssembleCommentDataBT(commentsTableNameBT(), MeetingID(), topic, userInfo$user$email, input$commentBT)
AssembleCommentDataBT <- function(tablename, meetingid, detail,  email, comment){
  #lookup up commentID
  commentCheck <- LoadUserCommentBT(tablename, meetingid, detail, email)
  if(debugmode){
    cat(file=stderr(), "in AssembleCommentDataBT nrow(commentCheck) is ", nrow(commentCheck), ", length is ",length(commentCheck), ".\n")
  }
  #build data
  if(nrow(commentCheck)> 0){
    if(debugmode){
      cat(file=stderr(), "in AssembleCommentDataBT nrow >0 section, nrow(commentCheck) is ", nrow(commentCheck), ".\n")
    }
    data <- as.list(commentCheck$FrID)
    #data$FrID <- commentCheck$FrID
    #data <- data[-1] #There's got to be a better way to create this list, but for now this works
  }else {
    data <- as.list("")
    #names(data) <- "FrID"
  }
  names(data) <- "FrID"
  data$DetailID <- detail
  data$MeetingID <- meetingid
  data$email <-email
  data$Comment <- comment
  if(debugmode){
    cat(file=stderr(), "leaving AssembleCommentData.\n")
  }
  return(data)
}

plotSummaryBT <- function(table, filterfield, criteria, valuefield, valuelabel){
  
  #vec <- unlist(siteSum$data[[varName]])
  #vec <- unlist(table[table$BLD_TYP_2009.CBSA==detail, 3])
  vec <- unlist(table[table[[filterfield]]==criteria, c(valuefield)])
  
  vecClass <- class(table[,c(valuefield)])
  nMissing <- sum(as.character(vec) == "NA" | as.character(vec) == "" | is.na(vec))
  #nBlanks <- length(table[table$SBW_Frame_Floor_Area=="",])
  #nZero <- length(table[table$SBW_Frame_Floor_Area==0,])
  nTotal <- length(vec)
  pTitle <- ggtitle(paste("Missing Values: ", nMissing, ", Total Properties: ", nTotal, sep = ""))
  if(vecClass %in% c("Numeric", "integer")) {
    vec <- as.numeric(vec)
  }
  
  class(vec)
  if(inherits(vec, c("factor", "character", "logical"))) {
      #build later
  }else { # numeric
    nMinus1 <- sum(vec == -1, na.rm = TRUE)
    nNa <- sum(is.na(vec))
    nUnknown <- nMinus1 + nNa
    nTotal <- length(vec)
    vec[vec == -1] <- NA
    vecMin <- min(vec[vec > 0], na.rm = TRUE)
    vecMax <- max(vec, na.rm = TRUE) 
    range <- vecMax / vecMin 
    p <- ggplot(data.frame("x" = vec)) + theme_bw() +
      geom_histogram(aes(x), na.rm = TRUE, bins = 30) +
      pTitle +
      xlab(valuelabel) +
      ylab("Count of Properties")
    if(range > 100) {
      roundDigits <- 0
      if(max(vec, na.rm = TRUE) < 10) roundDigits <- 2
      if(max(vec, na.rm = TRUE) < 1) roundDigits <- 3
      p <- p + scale_x_log10(breaks = round(exp(seq(from = log(vecMin), to = log(vecMax), length.out = 6)), roundDigits))
    }
  } #end else
  p
  # plotdata <- table[table$BLD_TYP_2009.CBSA==detail & table$SBW_Frame_Floor_Area>0, 3]
  # hist(plotdata, freq = FALSE)
  # ggplot(data=chol, aes(chol$AGE)) + 
  #   geom_histogram(breaks=seq(20, 50, by = 2), 
  #                  col="red", 
  #                  fill="green", 
  #                  alpha = .2) + 
  #   labs(title="Histogram for Age") +
  #   labs(x="Age", y="Count")
  # 
  # ggplot(data=chol, aes(chol$AGE)) + 
  #   geom_histogram(breaks=seq(20, 50, by =2), 
  #                  col="red", 
  #                  aes(fill=..count..)) +
  #   scale_fill_gradient("Count", low = "green", high = "red") +
  #   labs(title="Histogram for Age") +
  #   labs(x="Age", y="Count")
  #   
}
