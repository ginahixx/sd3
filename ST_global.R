#Stuff for developemnt of the sample tool.
library(rhandsontable)
library(data.table)
library(plyr)
library(XLConnect)

#setwd("P:\\Projects\\NEEA22 (CBSA Phase 1 Planning)\\_04 Assessment of Alternative Designs\\Tool\\SD2")
debugmode <- TRUE
sdsettingtablename  <-"BldgTypesSettings"
SampleDesignsTableName <- "SampleDesigns"
SampleDesignSummaryTableName <- "DesignsSummaries"
CustomBldgTypestablename <- "CustomBuildingTypes"
oversamplesettingtablename <- sdsettingtablename
OversampleDesignsTableName <- "OverSampleSampleDesigns"
#onestage <- "1 Stage"
#twostage <- "2 Stage"
summaryBlank <-data.frame("Summary"="Compute sample sizes to see summary")
domainflags <- data.table(RecordID=1,domain=1, domainstring=1)
tblmin <- 250
#saveddesign <-NULL

updateDataTables <- function() {
  if(debugmode){
    cat(file=stderr(), "*** Reloading data.\n")
  }
  defaultDesign <<- read.csv("design.csv" )
  #summaryBlank <<- read.csv("summarytable.csv")
  summaryData <<- summaryBlank
  #moved to server so each user has their own copy
  #framedt <<- data.table(readRDS("sim_frame.rds"))
  designframe <<- data.table(readRDS("sim_frame.rds"))
  #turn off until next round
  #frame2stagedt <<- data.table(readRDS("frame_2.rds"))
  load("lookups.rda")
  states <<- states
  utilities <<- utilities
  densities <<-  densities
  #lookup user bldgtypes
  bldgtypesonly <<- bldgtypesonly
  bldgtypes <<- bldgtypes
  # states <<- c("All","Remaining", sort(c("WA", "OR", "ID", "MT")))
  # utilities <<- c("All","Remaining", sort(unique(as.character(framedt[,OverSampleUtility]))))
  # densities <<- c("All","Remaining", "Rural", "Urban")
  # bldgtypesonly <<- sort(unique(as.character(framedt[,bldg.type])))
  # bldgtypes <<- c("All","Remaining", bldgtypesonly)
  UtilType <<- c("All","Remaining", "Public", "Private")
  analysisTypes <<- c("Precision", "Cost")
  heightval <<- 100
  ConfidenceOptions <<- seq(0.8, 0.95, by=0.05)
  PrecisionOps <<- seq(0.05, 0.3, by=0.05)
  #adjfacors <<- ReadData("AdjFactors")
  
  #alter this section to work in sd2 environment
  buildingtypes <- read.csv("buildingtypes.csv", strip.white=TRUE)
  detailedBT <- read.csv("detailedBT.csv")
  detailedBT$detail <- paste0(detailedBT$SBW_Frame_Detailed_BT, " [", detailedBT$Count,"]")
  load("frame.rda")
  btfieldname <<- names(frame)[1]
  sffieldname <<- names(frame)[3]

  blankBT <- sum(length(which(frame$BLD_TYP_2009.CBSA=="")))
  blankSQ <- sum(is.na(frame$SBW_Frame_Floor_Area))
  blankBoth <- sum(length(which(frame$BLD_TYP_2009.CBSA=="" & is.na(frame$SBW_Frame_Floor_Area))))
  totalcount <- nrow(frame)
  tblMissing <- format(c(totalcount, blankBT, blankSQ, blankBoth), big.mark=",",scientific=FALSE)
  names(tblMissing) <- c("Total Properties", "Missing Building Type", "Missing Square Footage", "Missing Both BT and SF")
  tblMissing <<- as.data.frame(t(tblMissing))

  renameblanks <- "Missing Bldg Type, Possible Duplicates"
  levels(frame$BLD_TYP_2009.CBSA) <- c(levels(frame$BLD_TYP_2009.CBSA),renameblanks)
  levels(buildingtypes$BLD_TYP_2009.CBSA) <- c(levels(buildingtypes$BLD_TYP_2009.CBSA),renameblanks)
  levels(detailedBT$BLD_TYP_2009.CBSA) <- c(levels(detailedBT$BLD_TYP_2009.CBSA),renameblanks)
  frame$BLD_TYP_2009.CBSA[frame$BLD_TYP_2009.CBSA == ""] <- renameblanks
  buildingtypes$BLD_TYP_2009.CBSA[buildingtypes$BLD_TYP_2009.CBSA == ""] <- renameblanks
  detailedBT$BLD_TYP_2009.CBSA[detailedBT$BLD_TYP_2009.CBSA == ""] <- renameblanks
  buildingtypes <<- buildingtypes
  detailedBT <<- detailedBT
  frame <<- frame

}

# design <- defaultDesign
#x<- 7

#data table version
UpdateValuesRegional <- function(userdesign, email, designname){
  #update passed table with population count
  userdesign <- converttotablenames (userdesign)
  designframe[,domain := NULL]
  if (debugmode){
    #cat(file=stderr(), "in UpdateValuesRegional userdesign is a ", typeof(userdesign) , ".\n")
  }
  # if(is.null(userdesign)){
  #   if (debugmode){
  #     cat(file=stderr(), "in UpdateValuesRegional userdesign is null.\n")
  #   }   
  # }else{
  #couldn't get sapply to work so using a for loop
  for (x in 1:nrow(userdesign)){
    # write function to translate all and remaining into criteria
    # if (debugmode){
    #   #cat(file=stderr(), "in UpdateValuesDT utility is ",  as.character(userdesign[x, "Utility"]) , " for ", x, ".\n")
    #   cat(file=stderr(), "* checking state. for x=", x, " State is ", unlist(userdesign[x, "State"]), ".\n")
    # }
    #check to abort  process for blank row
    if(!is.na(userdesign[x, "State"])){
        
      usedstates <- switch(as.character(userdesign[x, "State"]),
                              All = states,
                              Remaining =  unique((userdesign$State)),
                              as.character(userdesign[x, "State"]))
      
      usedutilities <- switch(as.character(userdesign[x, "Utility"]),
                              All = utilities,
                              Remaining =  unique((userdesign$Utility)),
                              as.character(userdesign[x, "Utility"]))
      
      useddensities <- switch(as.character(userdesign[x, "Density"]),
                              All = densities,
                              Remaining =  unique((userdesign$Density)),
                              as.character(userdesign[x, "Density"]))
      
      usedstates <- as.character(usedstates[!usedstates =="Remaining"])
      usedutilities <- as.character(usedutilities[!usedutilities =="Remaining"])
      useddensities <- as.character(useddensities[!useddensities =="Remaining"])
      
      #why only bldg tpe gets this special treament?
      usedbldgtypes <- as.character(unlist(unique(userdesign[userdesign$State %in% usedstates & if(as.character(userdesign[x, "Utility"])=="Remaining")(!userdesign$Utility %in% usedutilities) else (userdesign$Utility %in% usedutilities) & userdesign$Density %in% useddensities , "BuildingType"])))
      usedbldgtypes <- as.character(usedbldgtypes[!usedbldgtypes =="Remaining"])
      if (debugmode){
        #cat(file=stderr(), "in UpdateValuesDT usedstates is ",  unlist(usedstates) , " used utlis are ",  unlist(usedutilities), " used dens ",  unlist(useddensities) , " and used bld ",  unlist(usedbldgtypes) , ".\n")
        #cat(file=stderr(), "in UpdateValuesRegional  used bld ",  unlist(usedbldgtypes) , ".\n")
      }
      usedbldgtypes <- ExpandBldgTypes(usedbldgtypes, email, designname)
      if (debugmode){
        #cat(file=stderr(), "in UpdateValuesDT usedstates is ",  unlist(usedstates) , " used utlis are ",  unlist(usedutilities), " used dens ",  unlist(useddensities) , " and used bld ",  unlist(usedbldgtypes) , ".\n")
        #cat(file=stderr(), "in UpdateValuesRegional  expanded used bld ",  unlist(usedbldgtypes) , ".\n")
      }
      
      domaintext <- paste0(if(as.character(userdesign[x, "State"])=="Remaining") "not in ", paste(usedstates, collapse = ", "), "-", 
                           if(as.character(userdesign[x, "Utility"])=="Remaining") "not in ", paste(usedutilities, collapse = ", "), "-", 
                           if(as.character(userdesign[x, "Density"])=="Remaining") "not in ", paste(useddensities, collapse = ", "), "-", 
                           if(as.character(userdesign[x, "BuildingType"])=="Remaining") "not in ", paste(usedbldgtypes, collapse = ", ")
                           )
      
      #tmplist <- copy(framedt)
      #designframe <- copy(framedt)
      
      designframe[,incState :=switch(as.character(userdesign[x, "State"]),
                                 All = TRUE, 
                                 Remaining = !designframe$stabbr %in% usedstates,
                                 (stabbr == as.character(userdesign[x, "State"])))]
      # if (stage=="2 stage"){
      #   designframe[,incUtility := switch(as.character(userdesign[x, "Utility"]),
      #                                     All = TRUE, 
      #                                     Remaining = !designframe$OverSampleUtility %in% usedutilities,
      #                                     (OverSampleUtility == as.character(userdesign[x, "Utility"])))]
      # }else{
        designframe[,incUtility := switch(as.character(userdesign[x, "Utility"]),
                                   All = TRUE, 
                                   Remaining = !designframe$Pub.Priv %in% usedutilities,
                                   (Pub.Priv == as.character(userdesign[x, "Utility"])))]
      # }
      designframe[, incDensity := switch(as.character(userdesign[x, "Density"]),
                                   All = TRUE, 
                                   Remaining = !designframe$Density %in% useddensities,
                                   (Density == as.character(userdesign[x, "Density"])))]
      
      btlist <- ExpandBldgTypes(userdesign[x, "BuildingType"], email, designname)
      
      ###!Change to use %in% bldg type list
      # designframe[, incBldgType  := bldg.type %in% unlist(btlist)]
      # tmp <- designframe[bldg.type %in% unlist(btlist)]
      # tmp <- designframe[bldg.type %in% c("Grocery", "Hospital")]
      # tmp <- designframe[incBldgType==TRUE, bldg.type]
      designframe[, incBldgType  := switch(as.character(userdesign[x, "BuildingType"]),
                                    All = TRUE, 
                                    Remaining = !designframe$bldg.type %in% unlist(usedbldgtypes),
                                    (bldg.type %in% unlist(btlist)))]
                                    #(bldg.type == as.character(userdesign[x, "BuildingType"])))]

          # domainflags <<- designframe
      # framedt <<- framedt[designframe,on = colnames(framedt)][incState & incUtility & incDensity & incBldgType,domain := paste(as.character(userdesign[x, "State"]),
      #            as.character(userdesign[x, "Utility"]), as.character(userdesign[x, "Density"]),as.character(userdesign[x, "BuildingType"]),sep = "-")]
      #designframe <- designframe[incState & incUtility & incDensity & incBldgType][, colnames(designframe)[(ncol(designframe)-3):ncol(designframe)]:=NULL]
      # designframe[,domain := paste(as.character(userdesign[x, "State"]),as.character(userdesign[x, "Utility"]), 
      #                          as.character(userdesign[x, "Density"]),as.character(userdesign[x, "BuildingType"]),sep = "-")]
      designframe[incState & incUtility & incDensity & incBldgType,domain := paste(as.character(userdesign[x, "State"]),as.character(userdesign[x, "Utility"]), 
                                   as.character(userdesign[x, "Density"]),as.character(userdesign[x, "BuildingType"]),sep = "-")]
      designframe[incState & incUtility & incDensity & incBldgType,domainstring:=domaintext]
      # if(debugmode){
      #   cat(file=stderr(), "*in updatevaluesregional inside loop , frame domains are", unlist(unique(designframe$domain)),".\n")
      # }

      # if (debugmode){
      #   cat(file=stderr(), ">>>in UpdateValuesDT  for x= ", x, " btlist is  ",  unlist(btlist) , " used bt's are ", unlist(usedbldgtypes), 
      #       " bt's with incbldgtype are:", unlist(unique(designframe[designframe$incBldgType==TRUE, "bldg.type"])), ".\n")
      # }
      
      #domainflags <<- rbind(domainflags, designframe[,c("RecordID","domain", "domainstring"), with=FALSE])
      #userdesign[x,"Population"] <- format(nrow(designframe[incState & incUtility & incDensity & incBldgType]), big.mark = ",")
      userdesign[x,"Population"] <- format((designframe[incState & incUtility & incDensity & incBldgType, .N]), big.mark = ",")
      userdesign[x,"SquareFootage"] <- format(sum(as.numeric(designframe[incState & incUtility & incDensity & incBldgType,sim.sf]), 
                                                  na.rm = TRUE), digits = 0, scientific = FALSE, big.mark = ",")
      userdesign[x,"AvgSF"] <- format(mean(as.numeric(designframe[incState & incUtility & incDensity & incBldgType,sim.sf]), 
                                           na.rm = TRUE), digits = 0, scientific = FALSE, big.mark = ",")
      userdesign[x,"CV"] <- format(sd(as.numeric(designframe[incState & incUtility & incDensity & incBldgType,sim.sf]), 
                              na.rm = TRUE)/mean(as.numeric(designframe[incState & incUtility & incDensity & incBldgType,sim.sf]), na.rm = TRUE), digits = 2, scientific = FALSE, big.mark = ",")
    } #end has state
  } #end for loop
  #}
  # if(debugmode){
  #   cat(file=stderr(), "*in updatevaluesregional , frame domains are", unlist(unique(designframe$domain)),".\n")
  # }
  userdesign <- converttouinames(userdesign)
}

createDefaultRow <- function(design){
  cols <- length(design)
  rem <- cols - 11
  if(debugmode){
    cat(file=stderr(), "in createDefaultRow deisgn has ", length(design), " for a remainder of ", rem, ".\n")
  }
  #defrow <- c("Remaining", "Remaining","Remaining","Remaining", "", "", "", "", 0.9, "Precision",  0.1, "", "", "", "", "", "")
  defrow <- c("Remaining", "Remaining","Remaining","Remaining", "", "", "", "", 0.9, "Precision",  0.1, c(rep("",rem), recursive = TRUE))
  defrow <- as.data.frame(t(defrow), stringsAsFactors = FALSE)
  defrow
}

LoadUserCommentTab2 <- function(table, meetingid, email, designname){
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  if(debugmode){
    cat(file=stderr(), "LoadUserCommentTab2 has email as ", email, ", table :", table, ", mtg:", meetingid, ", design:", designname, ".\n")
  }
  query <- sprintf("SELECT * FROM %s Where email = '%s' AND MeetingID = %s AND SampleDesign = '%s'",
                   table, email, meetingid, cleanApos(designname))
  if(debugmode){
    cat(file=stderr(), "LoadUserCommentTab2 query is ", query, ".\n")
  }
  #cat(file=stderr(), "in LoadUserCommentTab2, query has been assigned.\n")
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

GetCommentDataTab2 <- function( commenttblName,  meetingid, email, appversion, design){
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  
  downloadfields <- paste0(" SampleDesign, comment, firstname as 'Submitted by' ")
  #make separate email where for neea
  if(appversion == "neea") {
    emailcriteria <- paste0("(users.email like '%@neea.org' or users.email = 'design')")
  }else{
    emailcriteria <- paste0("(users.email = '", email, "' or users.email = 'design')")
  }
  #FROM users INNER JOIN SampleDesignComments ON users.email = SampleDesignComments.email  
  query <- paste0("SELECT ", downloadfields, " FROM users INNER JOIN ", commenttblName, 
                  " ON users.email = ", commenttblName, ".email WHERE ", commenttblName, ".MeetingID = ", meetingid
                  ,
                  #" and ", emailcriteria
                   " AND SampleDesign = '", cleanApos(design), "'"
                  ) 
                  #" Group by ", designtblName, ".DesignName, ", designtblName, ".email order by ", designtblName, ".DateUpdated desc")

  data <- dbGetQuery(db, query)
  if(debugmode){
    cat(file=stderr(), "in GetCommentDataTab2, query is ", query, ", results are ", unlist(data), ".\n")
  }
  dbDisconnect(db)
  data
}

GetDesignsTable <- function(designtblName,meetingid, email, appversion){
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  
  downloadfields <- paste0(" DesignName, DesignDescription, firstname as 'Submitted by' ")
  #make separate email where for neea
  if(appversion == "neea") {
    emailcriteria <- paste0("(users.email like '%@neea.org' or users.email = 'design')")
  }else{
    emailcriteria <- paste0("(users.email = '", email, "' or users.email = 'design')")
  }
  #FROM users INNER JOIN (SampleDesigns INNER JOIN SampleDesignComments ON DesignName = SampleDesign) ON users.email = SampleDesigns.email    
  query <- paste0("SELECT ", downloadfields, " FROM users INNER JOIN ", designtblName, 
                  " ON users.email = ", 
                  designtblName, ".email WHERE ", designtblName, ".MeetingID = ", meetingid, " and ", emailcriteria, 
                  " Group by ", designtblName, ".DesignName, ", designtblName, ".email order by ", designtblName, ".DateUpdated desc")
  if(debugmode){
    cat(file=stderr(), "in GetDesignsTable, query is ", query, ".\n")
  }
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}
AssembleCommentDataTab2 <- function(tablename, meetingid,  email, design, comment){
  if(debugmode){
    cat(file=stderr(), "AssembleCommentDataTab2 has email as ", email, ", table:", tablename, ", mtg:", meetingid, ", design:", design, ".\n")
  }
  #lookup designid
  #designid <- GetSampleDesignID(SampleDesignsTableName,meetingid,  email, design)

  #lookup up commentID
  commentCheck <- LoadUserCommentTab2(tablename, meetingid, email, design)
  if(debugmode){
    cat(file=stderr(), "in AssembleCommentDataTab2 nrow(commentCheck) is ", nrow(commentCheck), ", length is ",length(commentCheck), ".\n")
  }
  #build data
  if(nrow(commentCheck)> 0){
    if(debugmode){
      cat(file=stderr(), "in AssembleCommentDataTab2 nrow >0 section, nrow(commentCheck) is ", nrow(commentCheck), ".\n")
    }
    data <- as.list(commentCheck$SDCID)
  }else {
    data <- as.list("")
  }
  names(data) <- "SDCID"
  data$MeetingID <- meetingid
  data$email <-email
  data$SampleDesign <- design
  data$Comment <- comment
  if(debugmode){
    cat(file=stderr(), "leaving AssembleCommentDataTab2\n")
  }
  return(data)
}

RunQuery <- function(queryString) {
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  # if(debugmode){
  #   cat(file=stderr(), "in RunQuery sql is ", queryString, ".\n")
  # }
  # Submit the query and disconnect
  rowsaffected <-dbExecute(db, queryString)
  dbDisconnect(db)
  rowsaffected
}

ReturnQuery <- function(queryString) {
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  if(debugmode){
    #cat(file=stderr(), "in ReturnQuery sql is ", queryString, ".\n")
  }
  # Submit the query and disconnect
  results <-dbGetQuery(db, queryString)
  dbDisconnect(db)
  if (nrow(results)==0) {
    results <- NULL
  }
  results
}

ReadDataParam <- function(fromstr, fieldstr = NULL, wherestr = NULL) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  if(is.null(fieldstr)){
    fieldstr = "*"
  }
  if (!is.null(wherestr)){
    wherestr <- paste0(" WHERE ", wherestr)
  }
  query <- sprintf("SELECT %s FROM %s %s", fieldstr, fromstr, wherestr)
  # Submit the fetch query and disconnect
  if(debugmode){
    cat(file=stderr(), "ReadDataParam sql is ", query, ".\n")
  }
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

#not finished
#data <- defaultDesign
#table <- SampleDesignsTableName
writedesign <- function(data, table, email, name, meeting, designdesc, budget){
  data <- converttotablenames(data)
  #pull in old design if any for backup
  wherestr <- paste0(" email = '", email, "' and DesignName = '", cleanApos(name), "'  and MeetingID = '", meeting, "' ")
  backupdata <- ReadDataParam(table, NULL, wherestr)
  #delete prior entries
  if (length(backupdata)>0) {
    #confirm they want to write over their old design
    
    #delete prior records
    query <- sprintf("DELETE from %s WHERE %s ", table, wherestr)
    if(debugmode){
      cat(file=stderr(), "in writedesign delete sql is ", query, ".\n")
    }
        rowsaffected <- RunQuery(query)
  }
  #strip commas
  data$Population <- as.numeric(gsub(",", "", data$Population))
  data$SquareFootage <- as.numeric(gsub(",", "", data$SquareFootage))
  data$AvgSF <- as.numeric(gsub(",", "", data$AvgSF))
  
  #insert new entries
  data$email <- email
  data$DesignName <- cleanApos(name)
  data$MeetingID <- meeting
  data$DesignDescription <- designdesc
  data$budget <- budget
  #designdata <- AssembleDesignData()
  sqldata <- data
  # df_srgs <- c(sqldata, sep = "', '")
  # newdata <- do.call(paste, df_srgs)
  #very messy way to combine the data rowwise, but works
  rowdata <- do.call(paste,c(sqldata, sep = "', '"))
  #below works, but don't want to specify every column
  #rowdata <- paste(sqldata$State, sqldata$Utility, sep = "', '")
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES %s",
    table,
    paste(names(sqldata), collapse = ", "),
    paste0("('", paste(rowdata, collapse = "'), ('"), "')") 
  )
  # settext<- paste0(names(sqldata), " = '", gsub("'", "\\\\'", sqldata), "'", collapse=",")
  # query <- sprintf("Update %s Set %s WHERE %s", table, settext, wherestr)
  if(debugmode){
    cat(file=stderr(), "in writedesign insert ql is ", query, ".\n")
  }
    trySend <- try(rowsaffected <-RunQuery(query))
  #restore backup if inserts fail
  
  if (rowsaffected == 0){
    if(nrow(backupdata)>0){
      #put deleted data back
      dbWriteTable(conn = dbcon, name = table, value = as.data.frame(backupdata), row.names = FALSE, overwrite = FALSE, append = TRUE)
    }
    msg = "Error in Saving Design"
  } else(
    msg = "Design Saved"
  )
  if(inherits(trySend, "try-error")) {
    showModal(modalDialog("Error in Saving Design", easyClose =  TRUE))
  } else {
    showModal(modalDialog(
      msg,
      easyClose = TRUE))
  }
}

writesummary <- function(data, table, email, name, meeting){
  data <- converttosummarytablenames(data)
  #pull in old design if any for backup
  wherestr <- paste0(" email = '", email, "' and SampleDesignName = '", cleanApos(name), "'  and MeetingID = '", meeting, "' ")
  #delete prior records
  query <- sprintf("DELETE from %s WHERE %s ", table, wherestr)
  # if(debugmode){
  # #  cat(file=stderr(), "in writesummary delete sql is ", query, ".\n")
  #   cat(file=stderr(), "in writesummary data cols are ", unlist(colnames(data)), ".\n")
  # }
  rowsaffected <- RunQuery(query)

  #strip commas
  for (i in 1:ncol(data)){
    data[,i]<-gsub(",", "", data[ , i])
  }

  #insert new entries
  data$email <- email
  data$SampleDesignName <- cleanApos(name)
  data$MeetingID <- meeting
  if(debugmode){
    cat(file=stderr(), "in writesummary data cols are ", unlist(colnames(data)), ".\n")
  }
  sqldata <- data
  #very messy way to combine the data rowwise, but works
  rowdata <- do.call(paste,c(sqldata, sep = "', '"))
  #below works, but don't want to specify every column
  #rowdata <- paste(sqldata$State, sqldata$Utility, sep = "', '")
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES %s",
    table,
    paste(names(sqldata), collapse = ", "),
    paste0("('", paste(rowdata, collapse = "'), ('"), "')") 
  )
  if(debugmode){
    cat(file=stderr(), "in writesummary insert ql is ", query, ".\n")
  }
  trySend <- try(rowsaffected <-RunQuery(query))

  if (rowsaffected == 0){
    msg = "Error in Saving Design"
  } else(
    msg = "Design Saved"
  )
  if(inherits(trySend, "try-error")) {
    showModal(modalDialog("Error in Saving Design", easyClose =  TRUE))
  } else {
    showModal(modalDialog(
      msg,
      easyClose = TRUE))
  }
}

#update the buliding type lists with cusomt BT's and leftovers
updateBT <- function(bldglist, email, design){
  designemail <- email
  CustomBT <- as.character(unlist(ReadDataParam(CustomBldgTypestablename, "BldgGrpName", paste0(" email = '", email, "' AND SampleDesignName = '", cleanApos(design), "'"))))
  if(length(CustomBT)==0){
    CustomBT <- as.character(unlist(ReadDataParam(CustomBldgTypestablename, "BldgGrpName", paste0(" email = 'design' AND SampleDesignName = '", cleanApos(design), "'"))))
    designemail <- "design"
  }
  #   #pull out any used DTs
  if(nchar(design)==0){
    leftovers <- bldglist 
  }else {
    usedlist <- (ExpandBldgTypes(CustomBT, designemail, design))
    leftovers <- bldglist[!bldglist %in% unlist(usedlist)]
    if(debugmode){
      cat(file=stderr(), "*in updateBT custombt is ", unlist(CustomBT), ", used list is  ", unlist(usedlist), ",  leftovers is ", unlist(leftovers), ".\n")
      #cat(file=stderr(), "in updateBT bldglist is a ", typeof(bldglist), " with  ", unlist(bldglist), ".\n")
    }
  }
    newlist <- unname(c(CustomBT, leftovers, recursive = TRUE))
}

BTsLeft <- function(bldglist, email, design, currentgrp){
  CustomBT <- as.character(unlist(ReadDataParam(CustomBldgTypestablename, "BldgGrpName", paste0(" email = '", email, "' AND SampleDesignName = '", design, "'"))))
  CustomBT <- CustomBT[!CustomBT %in% currentgrp]
  # 
  #   #pull out any used DTs
  if(nchar(design)==0){
    leftovers <- bldglist 
  }else {
    usedlist <- (ExpandBldgTypes(CustomBT, email, design))
    leftovers <- bldglist[!bldglist %in% unlist(usedlist)]
    if(debugmode){
      cat(file=stderr(), "*in BTsLeft currentgrp is ", currentgrp, ", custombt is ", unlist(CustomBT), ", used list is  ", unlist(usedlist), ",  leftovers is ", unlist(leftovers), ".\n")
      #cat(file=stderr(), "in updateBT bldglist is a ", typeof(bldglist), " with  ", unlist(bldglist), ".\n")
    }
  }
  newlist <- leftovers
}

BTsUsed <- function(bldglist, email, design, currentgrp){
  CustomBT <- as.character(unlist(ReadDataParam(CustomBldgTypestablename, "BldgGrpName", paste0(" email = '", email, "' AND SampleDesignName = '", design, "'"))))
  CustomBT <- CustomBT[CustomBT %in% currentgrp]
  # 
  #   #pull out any used DTs
  if(nchar(design)==0){
    usedlist <- NULL 
  }else {
    usedlist <- (ExpandBldgTypes(CustomBT, email, design))
    #leftovers <- bldglist[!bldglist %in% unlist(usedlist)]
    if(debugmode){
      cat(file=stderr(), "*in BTsLeft currentgrp is ", currentgrp, ", custombt is ", unlist(CustomBT), ", used list is  ", unlist(usedlist), ".\n")
      #cat(file=stderr(), "in updateBT bldglist is a ", typeof(bldglist), " with  ", unlist(bldglist), ".\n")
    }
  }
  newlist <- usedlist
}

#ExpandBldgTypes(usedbldgtypes, email)
#x <-2
ExpandBldgTypes <- function(usedbtList, email, design){
  #pass in list of used bldg types, any that are group names are swapped out with the BTs in the group
  # if(debugmode){
  #   cat(file=stderr(), "in ExpandBldgTypes usedbtList length is ",length(usedbtList), " with ", 
  #       unlist(usedbtList), " email is ", email, ", design is ", design, ".\n")
  # }
  if (length(usedbtList)==0){
    explist <- usedbtList
  }else {
    explist <- lapply(1:length(usedbtList), function(x){
      query <- sprintf("SELECT BldgTypeList FROM %s WHERE BldgGrpName = '%s' AND email = '%s' AND SampleDesignName = '%s'", 
                       CustomBldgTypestablename, usedbtList[x], email, cleanApos(design))
    btlist <- ReturnQuery(query)
      # if(debugmode){
      #   cat(file=stderr(), "in ExpandBldgTypes btlist is ", unlist(btlist), " for query ", query, ".\n")
      # }

      if(is.null(btlist)){
        btlist <- usedbtList[x]
      }else{
        #btlist <-strsplit(as.character(btlist), ", ")
        btlist <- unlist(unname(btlist))
      }
      return (btlist)
    })
    explist <- paste(explist, sep = ", ", collapse = ", ")
    explist <- strsplit(explist, ", ")
    explist <- unique(explist)
    }
  return(explist)
}

#Return if the passed design uses grouped BT's which aren't in the table
isGroupedDesign <- function(designdata, email, designname, designtable, meetingid){
#isGroupedDesign <- function(designdata, email, designname, designtable, meetingid, designintable){
  if(debugmode){
    cat(file=stderr(), "in isGroupedDesign data is a ", typeof(designdata), " bldgtypes is a ", typeof(bldgtypes), ", designname is a ", typeof(designname),
        ", design table is ", designtable, ", length of desingname is ", nchar(designname), ", design name is ", unlist(designname), 
        ", nrow(designname) is ", nrow(designname) , ".\n")
        #", nrow(designname) is ", nrow(designname) , ", and saveddesign is ", saveddesign, ".\n")
  }
  
  extras <-  designdata[! designdata$"Building Type" %in% as.list(bldgtypes)]
  if(length(extras) >0 && nchar(designname)>1){
    
  
  #if (!is.null(designname)){
  #if (nchar(designname)>1){
    query <- sprintf("Select SDID FROM %s where (email = '%s' OR email = 'design') and MeetingID = '%s' AND DesignName = '%s'", designtable, email, meetingid, designname)
    if(debugmode){
      cat(file=stderr(), "in isGroupedDesign query is ", query, ".\n")
    }
  
    designlist <- ReturnQuery(query)
    designexists <- !is.null(designlist)
    
  #} else{
  #   designlist <-0
  #   designexists <- TRUE
  # }
    if(debugmode){
      cat(file=stderr(), "in isGroupedDesign designexists is ", designexists, ", designlist is ", unlist(designlist), ", nrow(designlist) is ", nrow(designlist), ", length of extras is ", length(extras), 
          #" and extras are ", unlist(extras),
          ".\n")
    }
    return(is.null(designlist))
  } else{
    if(debugmode){
      cat(file=stderr(), " 0 in isGroupedDesign blank design name or there are no groupings Return FALSE .\n")
    }
    return(FALSE)
    
  }
  
  # if(length(extras) >0 && !designexists){
  #   if(debugmode){
  #     cat(file=stderr(), " + in isGroupedDesign Return TRUE .\n")
  #   }
  #   return(TRUE)
  # }else{
  #   if(debugmode){
  #     cat(file=stderr(), " 0 in isGroupedDesign Return FALSE .\n")
  #   }
  #   return(FALSE)
  # }
}

#copyBldgGroups
copyBldgGroups <- function(Srcdesign, designname, email, table){
  if(debugmode){
    cat(file=stderr(), "^^^in copyBldgGroups source design is: ", unlist(Srcdesign), ".\n")
  }
  query <- sprintf("Select * FROM %s WHERE email = '%s' AND SampleDesignName = '%s'", table, email, cleanApos(Srcdesign))
  grplist <- ReturnQuery(query)
  if(is.null(grplist)){
    query <- sprintf("Select * FROM %s WHERE email = 'design' AND SampleDesignName = '%s'", table, cleanApos(Srcdesign))
    grplist <- ReturnQuery(query)
  }
  if(is.null(grplist)){
    #can't copy
    return(FALSE)
  }else{
    query <- sprintf("Delete FROM %s WHERE email = '%s' AND SampleDesignName = '%s'", table, email, cleanApos(designname))
    rowsaff <- RunQuery(query)
    grplist$email <- email
    grplist$SampleDesignName <- designname
    
    rowsaff <- CreateDataMulti(grplist, table)
    if(debugmode){
      cat(file=stderr(), "^^^in copyBldgGroups created data, list is: ", unlist(grplist), ", rows affected was ", rowsaff, ".\n")
    }
    if(rowsaff > 0){
      return(TRUE)
    } else{
      return(FALSE)
    }
  }
}

#Add adjustment factors to design table
addadjfactors <- function(designdata, email, designname){
  adjfacors <- ReadData("AdjFactors")
  for (x in 1:nrow(designdata)){
    btlist <- ExpandBldgTypes(designdata[x, "BuildingType"], email, designname)
    designfactors <- adjfacors[adjfacors$BldgType %in% btlist, "AjustmentFactor"]
    factor2nd <- mean(designfactors)
    designdata$adjfactor                         
  }
  designdata
}
#change table names to be user friendly versions
converttouinames <- function(designdata){
  colnames(designdata)[which(names(designdata) == "Utility")] <- "Utility Type"
  colnames(designdata)[which(names(designdata) == "BuildingType")] <- "Building Type"
  colnames(designdata)[which(names(designdata) == "SquareFootage")] <- "Floor Area (SF)"
  colnames(designdata)[which(names(designdata) == "AvgSF")] <- "Avg (SF)"
  colnames(designdata)[which(names(designdata) == "CV")] <- "CV (SF)"
  colnames(designdata)[which(names(designdata) == "AnalysisType")] <- "Target Type"
  colnames(designdata)[which(names(designdata) == "Value")] <- "Target Value"
  colnames(designdata)[which(names(designdata) == "SampleSize")] <- "1 Stg n"
  colnames(designdata)[which(names(designdata) == "RP_E_EUI")] <- "RP Elect EUI"
  colnames(designdata)[which(names(designdata) == "RP_G_EUI")] <- "RP Gas EUI"
  colnames(designdata)[which(names(designdata) == "Cost")] <- "1 Stg $"
  colnames(designdata)[which(names(designdata) == "Cost2nd")] <- "2 Stg $"
  colnames(designdata)[which(names(designdata) == "SampleSize2nd")] <- "2 Stg n"
  colnames(designdata)[which(names(designdata) == "SRSn")] <- "SRS n"
  colnames(designdata)[which(names(designdata) == "CBGn")] <- "CBG n"
  colnames(designdata)[which(names(designdata) == "CBGcost")] <- "CBG var $"
  colnames(designdata)[which(names(designdata) == "RP_vintage")] <- "RP Vintage"
  colnames(designdata)[which(names(designdata) == "RP_kWh")] <- "RP kWh"
  colnames(designdata)[which(names(designdata) == "RP_therm")] <- "RP therm"
  designdata
}

converttotablenames <- function(designdata){
  colnames(designdata)[which(names(designdata) == "Utility Type")] <- "Utility"
  colnames(designdata)[which(names(designdata) == "Building Type")] <- "BuildingType"
  colnames(designdata)[which(names(designdata) == "Floor Area (SF)")] <- "SquareFootage"
  colnames(designdata)[which(names(designdata) == "Avg (SF)")] <- "AvgSF"
  colnames(designdata)[which(names(designdata) == "CV (SF)")] <- "CV"
  colnames(designdata)[which(names(designdata) == "Target Type")] <- "AnalysisType"
  colnames(designdata)[which(names(designdata) == "Target Value")] <- "Value"
  colnames(designdata)[which(names(designdata) == "RP Elect EUI")] <- "RP_E_EUI"
  colnames(designdata)[which(names(designdata) == "RP Gas EUI")] <- "RP_G_EUI"
  colnames(designdata)[which(names(designdata) == "1 Stg n")] <- "SampleSize"
  colnames(designdata)[which(names(designdata) == "1 Stg $")] <- "Cost"
  colnames(designdata)[which(names(designdata) == "2 Stg $")] <- "Cost2nd"
  colnames(designdata)[which(names(designdata) == "2 Stg n")] <- "SampleSize2nd"
  colnames(designdata)[which(names(designdata) == "SRS n")] <- "SRSn"
  colnames(designdata)[which(names(designdata) == "CBG n")] <- "CBGn"
  colnames(designdata)[which(names(designdata) == "CBG var $")] <- "CBGcost"
  colnames(designdata)[which(names(designdata) == "RP Vintage")] <- "RP_vintage"
  colnames(designdata)[which(names(designdata) == "RP kWh")] <- "RP_kWh"
  colnames(designdata)[which(names(designdata) == "RP therm")] <- "RP_therm"
  designdata
}

converttosummarytablenames <- function(designdata){
  colnames(designdata)[which(names(designdata) == "1 or 2 Stage Design")] <- "Stage"
  colnames(designdata)[which(names(designdata) == "Census Block Group (CBG) Sample Size (n)")] <- "CBGn"
  colnames(designdata)[which(names(designdata) == "Stratified Sample Size (n)")] <- "Stratn"
  colnames(designdata)[which(names(designdata) == "Simple Random Sample (SRS) Size (n)")] <- "SRSn"
  colnames(designdata)[which(names(designdata) == "Fixed $")] <- "Fixed"
  colnames(designdata)[which(names(designdata) == "Variable $")] <- "variable"
  colnames(designdata)[which(names(designdata) == "Total $")] <- "totalcost"
  colnames(designdata)[which(names(designdata) == "Frame Enhancement $")] <- "FrameE"
  colnames(designdata)[which(names(designdata) == "Total $ w/ Frame Enhancement")] <- "TotalFrameE"
  designdata
}

converttosummaryuinames <- function(designdata){
  colnames(designdata)[which(names(designdata) == "Stage")] <- "1 or 2 Stage Design"
  colnames(designdata)[which(names(designdata) == "CBGn")] <- "Census Block Group (CBG) Sample Size (n)"
  colnames(designdata)[which(names(designdata) == "Stratn")] <- "Stratified Sample Size (n)"
  colnames(designdata)[which(names(designdata) == "SRSn")] <- "Simple Random Sample (SRS) Size (n)"
  colnames(designdata)[which(names(designdata) == "Fixed")] <- "Fixed $"
  colnames(designdata)[which(names(designdata) == "Variable")] <- "Variable $"
  colnames(designdata)[which(names(designdata) == "totalcost")] <- "Total $"
  colnames(designdata)[which(names(designdata) == "FrameE")] <- "Frame Enhancement $"
  colnames(designdata)[which(names(designdata) == "TotalFrameE")] <- "Total $ w/ Frame Enhancement"
  designdata
}
# convertsmrytouinames <- function(thedata){
#   colnames(thedata)[which(names(thedata) == "Fixed Cost")] <- "BaseCost"
#   colnames(thedata)[which(names(thedata) == "Variable Cost")] <- "VariableCost"
#   colnames(thedata)[which(names(thedata) == "Total Cost")] <- "TotalCost"
#   
#   thedata
#   
# }

cleanApos <- function(cx) {
  gsub("'", "\\\\'", cx)
}

updateDataTables()
