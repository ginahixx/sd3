#CBSA 

shinyServer(function(input, output, session) {

  # Session stuff -----------------------------------------------------------
  saveddesign <- NULL
  designer <- "Design Team"
  #savegrp <- FALSE

  # Reactives ----
  MeetingID <- reactive({
    #come up with default id?
    defaultid <- 6
    flag <- appflag
    tmp <- parseQueryString(session$clientData$url_search)
    if(debugmode){
      cat(file=stderr(), "meeting id from url ", tmp[[flag]], ".\n")
    }
    allowableList <- GetMeetingIDs()
    if(!is.null(tmp[[flag]])) {
      if(!(tmp[[flag]] %in% allowableList)) {
        defaultid # if invalid input, default to nothing
      }
      tmp[[flag]]
    } else {
      defaultid
    }
  })
  
  SettingsTable <- reactive({
    if(debugmode){
      cat(file=stderr(), "In SettingsTable reactive, mtgID is ", MeetingID(), ".\n")
    }
    GetSettingsTable(MeetingID())
  })
  
  DecisionData <- reactive({
    LoadControlData(MeetingID())
  })
  
  userInfo <- reactiveValues()
  userName <- reactiveValues(name = "Not Logged In to Comment")
  
  output$landingPage <- renderUI(HTML(getmodalHTML(SettingsTable())))
  output$instructions <- renderUI(HTML(GetInstructionsHTML(SettingsTable())))
  output$introText <- renderUI(HTML(SettingsTable()$IntroText))
  output$contactText <- renderUI(HTML(SettingsTable()$ContactInfo))
  output$webtab <- renderText(HTML(SettingsTable()$WebTabName))
  output$startText <- renderUI(HTML(SettingsTable()$StartScreenMessage))
  
  # special version control --------------------------------------------------
  # Let NEEA have their own comments for private review. Should be a version=... in the url string
  specialVersion <- reactive({
    allowableVersions <- c("neea", "public")
    flag <- versionflag
    tmp <- parseQueryString(session$clientData$url_search)
    if(!is.null(tmp[[flag]])) {
      if(!(tmp[[flag]] %in% allowableVersions)) {
        "public" # if invalid input, default to the public designs
      }
      tmp[[flag]]
    } else {
      "public"
    }
  })

  commentsTableName <- reactive({
    if(debugmode){
      cat(file=stderr(), "In commentsTableName reactive, version is ", specialVersion(), ", table name is ", SettingsTable()$CommentsTableName, ".\n")
    }
    
    if(specialVersion() == "neea") {
      paste0(SettingsTable()$CommentsTableName, "_NEEA")
    } else {
      SettingsTable()$CommentsTableName
    }
  })
  
  appTitle <- reactive({
    if(specialVersion() == "neea") {
      paste0(SettingsTable()$AppTitle, ", NEEA Internal Version")
    } else {
      SettingsTable()$AppTitle
    }   
  })
  output$appTitle <- renderText(appTitle())
  
  
  observe({
    if(is.null(ControlSelected())) {
      updateCheckboxInput(session, "showStartInstr", value = TRUE)
    } else {
      updateCheckboxInput(session, "showStartInstr", value = FALSE)
    }
  })
  
  # Login Handling --------------------------------------------------------------
  # # Functions to bring up dialogs with checks on the inputs
  logindata <- reactive({
    data<- sapply(usertablefields, function(x) input[[x]])
    names(data) <- usersqlfields
    #cat(file=stderr(), "in logindata reactive, ",unlist(data), ".\n")
    data
  })
  
  loginBox <- function(emailWarning = FALSE, neeaUserWarning = FALSE) {
    modalDialog(
      title = "Log In to Participate ",
      htmlOutput("landingPage"),
      textInput("loginemail", "Please Enter Your Email Address"),
      if(emailWarning) 
        div(tags$b("Please enter your email address", style = "color: red;")),
      if(neeaUserWarning)
        div(tags$b("Error: only NEEA personnel may use the NEEA app version", style = "color: red;")),
      size = "l",
      footer = tagList(actionButton("login", "Login"))
    )
  }
  
  registrationBox <- function(nameOrgWarning = FALSE) {
    modalDialog(
      title = "Please Register",
      p("It looks like this is your first time logging in, please provide your name and organization."),
      br(), p("If this is not your first visit, hit cancel and please check the spelling of your email address"),
      textInput(inputId = "Commenter", label = "Name"),
      textInput(inputId = "organization", label = "Organization"),
      if(nameOrgWarning)
        div(tags$b("Please enter your name and organization", style = "color: red;")),
      footer = tagList(
        actionButton("returnToLogin", "Cancel"),
        actionButton("register", "Register"))
    )
  }
  
  # Set up reactive values that govern whether the user needs to log in
  needToLogin <- reactiveValues(loginBox = TRUE)
  observe({
    if(needToLogin$loginBox) {
      showModal(loginBox())
    }
  })
  # Save the email address in case they want to hit cancel at the registration page
  login2 <- reactive({
    input$loginemail
  })
  
  # Bring up the login dialog
  observeEvent(input$login, {
    needToLogin$loginBox <- FALSE
    if(!nchar(input$loginemail)) {
      showModal(loginBox(emailWarning = TRUE))
    # } else if(commentsTableName() == commenttablename_neea & !length(grep("@neea\\.org$", input$loginemail, ignore.case = TRUE))) {
    } else if(specialVersion() == "neea" & !length(grep("@neea\\.org$", input$loginemail, ignore.case = TRUE))) {
      showModal(loginBox(neeaUserWarning = TRUE))
    } else if(!(tolower(input$loginemail) %in% registeredEmails())) {
      # Not found. Set up a registration box
      showModal(registrationBox())
    } else {
      # Email Found. Proceed
      userInfo$user <- getUserInfo(tolower(input$loginemail))
      userName$name <- userInfo$user$firstname
      #move 
      # commentdatatmp <- LoadUserCommentTab2(commentsTableNameTab2(), MeetingID(), userInfo$user$email)
      # updateTextAreaInput(session, "commentTab2", value = unname(commentdatatmp["Comment"]))
      summaryData <<- summaryBlank
      if(debugmode){
        cat(file=stderr(), "in input login email found, name is ", userName$name, ".\n")
      }
      removeModal()
    }    
  })
  
  observeEvent(input$returnToLogin, {
    updateTextInput(session, "loginemail", value = login2())
    needToLogin$loginBox <- TRUE
  })
  
  # When the register button is pressed, add a credential to the database
  observeEvent(input$register, {
    if(!nchar(input$Commenter) | !nchar(input$organization)) {
      showModal(registrationBox(nameOrgWarning = TRUE))
    } else {
      logindata <- sapply(usertablefields, function(x) input[[x]])
      names(logindata) <- usersqlfields
      rows <- CreateData(logindata, userstablename)
      userInfo$user <- getUserInfo(input$loginemail)
      userName$name <- userInfo$user$firstname
      removeModal() 
    }
  })
  
  observeEvent(input$Logout, {
    userName$name <- "Not Logged In to Comment"
    #updateTextInput(session, "loginemail", value = "")
    updateDataTables() #force reload of data, I hope
    needToLogin$loginBox <- FALSE
    needToLogin$loginBox <- TRUE
  })
  
  loggedInStr <- reactive({
    if(userName$name == "Not Logged In to Comment") {
      "Not Logged In To Comment"
    } else {
      if(debugmode){
        cat(file=stderr(), "in loggedInStr name is ", userName$name, ".\n")
      }
      paste("Logged in as", userName$name)
    }
  })
  
  output$loggedinas <- renderText(loggedInStr())
  
  output$urlx <- renderText({
    as.character(session$clientData$url_pathname)
  })
  
  
  # General reactives ------------------------------------------------
  # input fields are treated as a group
  formData <- reactive({
    #data<- sapply(names(GetTableMetadata(issuestablename)), function(x) input[[x]])
    data<- sapply(commentformfields, function(x) input[[x]])
    data
  })
  
  # commentData <- reactive({
  #   #cat(file=stderr(), "in commentData reactive.\n")
  #   data <- sapply(commenttablefields, function(x) input[[x]])
  #   pickoptions <- input$PickChoice
  #   pickstr <- paste(pickoptions, collapse = ", ")
  #   data$PickChoice_text <- pickstr
  #   data$PickChoice <- pickstr
  #   #cat(file=stderr(), "in commentData reactive, pick results are: ",pickstr, ".\n")
  #   #cat(file=stderr(), "in commentData reactive, ",unlist(data), ".\n")
  #   data
  # })
  
  # dataToDownload <- reactive({
  #   GetCommentData("")
  #   #GetEmailCommentData(input$email)
  #   #"downloadtable"
  # })
  
  # Select row in table -> show details in inputs and pull up related comments
  observeEvent(input$controlList_rows_selected, {
    if (length(input$controlList_rows_selected) > 0) {
      if(debugmode){
        cat(file=stderr(), "row selected values  ", unlist(input$controlList_cell_clicked), ".\n")
      }
      id <- unlist(input$controlList_cell_clicked[3])
      # data <- LoadFilteredData(issuestablename, "IssueTitle", id)
      # UpdateInputs(data, session)
      #questionprompt <- input$Question
      commentdata <- LoadUserComment(commentsTableName(), MeetingID(), id, userInfo$user$email)
      #line below doesn't seem to work.
      #data <- LoadUserComment(id, tolower(input$loginemail))
      #cat(file=stderr(), "selected row data is ", unlist(data), ".\n")
      updateTextAreaInput(session, "comment", value = unname(commentdata["Comment"]))
      updateCheckboxInput(session, "showpickchoice", value = SelectedData()$HasDefinedChoices)
      if(debugmode){
        cat(file=stderr(), "value of  excellink:  ", (SelectedData()$ExcelFileLink), ".\n")
      }
      #updateCheckboxInput(session, "showexceldownload", value = (!is.na(SelectedData()$ExcelFileLink)&& !SelectedData()$ExcelFileLink)=="NA")
      if(debugmode){
       cat(file=stderr(), "row selected done updating inputs\n")
      }
    }
  })

  # Handle Decision Comment Submission --------------------------------------------------------------
  # Submit a comment 
  observeEvent(input$submitcomment, {
    # cat(file=stderr(), "in submit selected is", ControlSelected(), ".\n")
    # cat(file=stderr(), "in submit comment !nchar(input$email) is ", !nchar(input$email), ".\n")
    topic <- ControlSelected()
    if (is.null(topic)){
      msg<- "Please make sure you have selected a decision to comment on before submitting comments"
      showModal(modalDialog(title = "No Decision Selected", msg, easyClose = TRUE)) 
      #} else if(!nchar(input$Commenter) & !nchar(input$organization) & !nchar(input$email)) {
      #} else if(!nchar(input$Commenter) & !nchar(input$organization)) {
    } else {
      # Check if this is a fresh comment or a modification
      #topic <- unlist(input$controlList_cell_clicked[3])
      # cat(file=stderr(), "in submit comment comment is ", input$comment, ".\n")
      #clear pickchoice if not needed
      if(SelectedData()$HasDefinedChoices){
        pick <- input$PickChoice
      } else {
        pick <- NULL
      }
      usercommentdata <- AssembleCommentData(commentsTableName(), MeetingID(), topic, SelectedData()$DDID, userInfo$user$email, input$comment, pick)
      # cat(file=stderr(), "in submit comment names are", names(usercommentdata), ", usercommentdata is ", unlist(usercommentdata), ", length is ",length(usercommentdata), ".\n")
      if(debugmode){
        cat(file=stderr(), "in submit comment usercommentdata is ", names(usercommentdata), " with ", unlist(usercommentdata), ".\n")
        cat(file=stderr(), "in submit comment length(usercommentdata$DCID) is ", length(usercommentdata$DCID), " and !usercommentdata$DCID =='' is",!usercommentdata$DCID =="", ".\n")
        cat(file=stderr(), "in submit comment nchar(input$comment) is ", nchar(input$comment), " and is.null(input$PickChoice) is ", is.null(input$PickChoice),".\n")
      }
      # cat(file=stderr(), "in submit comment usercommentdata$DecisionID ", usercommentdata$DecisionID, ".\n")
      if(!usercommentdata$DCID =="" & nchar(input$comment)) {
        trySend <- try(UpdateData(usercommentdata, usercommentdata$DCID, commentsTableName(), "DCID"))
        msg <- "Comment Updated!"
      } else if(!usercommentdata$DCID=="" & !nchar(input$comment) & if(is.null(pick)) TRUE else !nchar(pick)){
      #} else if((!is.null(usercommentdata$DCID)) & !nchar(input$comment) & !nchar(input$PickChoice)){
        trySend <- try(DeleteData(usercommentdata$DCID, commentsTableName(), "DCID"))
        msg <- "Comment Removed!"
      } else {
        if(debugmode){
          cat(file=stderr(), "in submit comment about to add new comment.\n")
        }
        trySend <- try(CreateData(usercommentdata, commentsTableName()))  
        msg <- "Comment Saved!"
      }
      if(inherits(trySend, "try-error")) {
        showModal(modalDialog("Error in Comment Submission", easyClose =  TRUE))
      } else {
        showModal(modalDialog(
          msg, 
          easyClose = TRUE))
      }
      #Update comments table
      ###Not working :(
      commentsTable$data <- LoadFilteredData(commentsTableName(), "MeetingID", MeetingID())
      # updateTextInput(session, inputId = "elementComment", label = "Comment", value = "") 
    }
  })
  

  # display control table ----------------
  output$controlList <- DT::renderDataTable({
    as.data.frame(DecisionData()$Title)
    }, selection = "single", rownames = FALSE, colnames = SettingsTable()$ControlTableTitle,
    options = list(
      pageLength = min(10, nrow(DecisionData())),
      dom = if(nrow(DecisionData())>10) 'tp' else 't'
      #dom =  'tp' 
    )
  )     
  
  # Set up the reactivity for selected control and comments --------------------------------------------------------------
  # Update data and comments whenever the row selected changes
  ControlSelected <- reactive({
    if(!is.null(input$controlList_rows_selected)) {
      #cx <- as.character(categoriesDf$category[input$controlList_rows_selected])
      decision <- unlist(input$controlList_cell_clicked[3])
      decision
    } else {
      NULL
    }
  })
  #limit data to the selected item
  SelectedData <- reactive({
    if(is.null(ControlSelected())){
      NULL
    } else{
      DecisionData()[DecisionData()$Title == ControlSelected(), ]
    }
  })
  
  # Make a table of element comments. This gets loaded/reloaded whenever the user submits a comment
  commentsTable <- reactiveValues()
  observe({
    if(debugmode){
      cat(file=stderr(), "in commentstable observe table name is ", unlist(commentsTableName()), ", mtg id is ",  MeetingID(), ".\n")
    }
    #commentsTable$data <- LoadFilteredData(commentsTableName(), "MeetingID", MeetingID())
    commentsTable$data <- GetCommentData(commentsTableName(), "%", MeetingID())
  })
  
  commentsTable2 <- reactive({
    if(!is.null(input$controlList_rows_selected)) {
      #To get it to refresh after a comment is submitted
      input$submitcomment
      decisionID <- DecisionData()$DDID[DecisionData()$Title ==ControlSelected()]
      if(debugmode){
        cat(file=stderr(), "in commentstable2 reactive id is ", decisionID, ".\n")
      }
      if(decisionID >0) {
        #commentsTableElement$tx[commentsTableElement$tx$category == cx, ]
        commentsTable$data <- GetCommentData(commentsTableName(), ControlSelected(), MeetingID())
        #ReadCommentIssueData(SelectedData()$DDID)
        #commentsTable$data[commentsTable$data$DDID == decisionID, ]
      }
    } else {
      commentsTable$data
    }    
  })
  
  output$commentsTable <- DT::renderDataTable(commentsTable2(), selection = "single", rownames = FALSE, filter = 'top')
  output$CommentHdrOutput <- renderText(paste("All Stakeholder Comments about ", ControlSelected()))
  
  # Decision outputs --------------------------------
  output$title <- renderText(ControlSelected())
  output$background <- renderText(paste0(SelectedData()$Background))
  output$proposedDecision <- renderText(paste0(SelectedData()$ProposedDecision))
  output$pros <- renderText(SelectedData()$Pros)
  output$cons <- renderText(SelectedData()$Cons)
  output$excelLink <- renderUI(HTML(SelectedData()$ExcelFileLink))
  
  output$PickChoiceLabel <- renderText(SelectedData()$PickLabel)
  output$CommentLabel_Open <- renderText(SelectedData()$CommentLabel)

  #Render the Pick choice input
  output$choiceinput <- renderUI({
    #id <- unlist(input$controlList_cell_clicked[3])
    if(length(SelectedData())==0) {
      p("")
    }else if(!SelectedData()$HasDefinedChoices ==1){
      #try to clear pickchoice when it's not being used
      selectInput("PickChoice", label = textOutput("PickChoiceLabel"), 
                  choices = c("Make a selection" = ""),selectize =  TRUE)
      p("")
    }else{
      commentdata <- LoadUserComment(commentsTableName(), MeetingID(), ControlSelected(), userInfo$user$email)
      
      if(SelectedData()$PickMultiple ==1){
        placeholdertext <- "Select all that apply"
      } else{
        placeholdertext <- "Select Top Choice"
      }
      dMultiple <- SelectedData()$PickMultiple ==1
      userselection <- unlist(strsplit(commentdata[,c("PickChoice")], ", "))
      if(debugmode){
        cat(file=stderr(), "in render choiceinput placeholdertext is ", placeholdertext, " multiple is ", dMultiple, " userselection:", userselection, ", decisionid: ", SelectedData()$DecisionID, ".\n")
        cat(file=stderr(), "in render choiceinput, choices are ", unlist(choices$DecisionID), ".\n") 
      }
      selectInput("PickChoice", label = textOutput("PickChoiceLabel"), 
                        choices = c("Make a selection" = "", choices[choices$DecisionID == SelectedData()$DecisionID,c("Option")]), 
                        selected = userselection, multiple = dMultiple, selectize =  TRUE)
    }
  })

  #***NEEA Frame  start---------------------------------
  SettingsTableBT <- reactive({
    if(debugmode){
      cat(file=stderr(), "In SettingsTableBT reactive, mtgID is ", MeetingID(), ".\n")
    }
    LoadFilteredData(sdsettingtablename, "TabID", paste0(MeetingID(), "-NEEA"))
  })
  
  commentsTableNameBT <- reactive({
    if(debugmode){
      cat(file=stderr(), "In commentsTableNameBT reactive, version is ", specialVersion(), ", table name is ", SettingsTableBT()$CommentsTableName, ".\n")
    }
    
    if(specialVersion() == "neea") {
      paste0(SettingsTableBT()$CommentsTableName, "_NEEA")
    } else {
      SettingsTableBT()$CommentsTableName
    }
  })
  
  ControlSelectedBT <- reactive({
    if(!is.null(input$controlListBT_rows_selected)) {
      if(debugmode){
        cat(file=stderr(), "In commentsTableNameBT reactive, data is ", unlist(input$controlListBT_cell_clicked), ".\n")
      }
      decision <- unlist(input$controlListBT_cell_clicked[3])
      decision
    } else {
      NULL
    }
  })
  
  #limit data to the selected item
  SelectedDataBT <- reactive({
    if(is.null(ControlSelectedBT())){
      NULL
    } else{
      #buildingtypes[buildingtypes$DetailID == ControlSelectedBT(), ]
      DecisionDataBT[DecisionDataBT$DetailID == ControlSelectedBT(), ]
      
    }
  })
  
  # Make a table of element comments. This gets loaded/reloaded whenever the user submits a comment
  commentsTableBT <- reactiveValues()
  observe({
    if(debugmode){
      cat(file=stderr(), "in commentstableBT observe table name is ", unlist(commentsTableNameBT()), ", mtg id is ",  MeetingID(), ".\n")
    }
    #commentsTable$data <- LoadFilteredData(commentsTableName(), "MeetingID", MeetingID())
    commentsTableBT$data <- GetCommentDataBT(commentsTableNameBT(), "%", MeetingID())
  })
  
  commentsTable2BT <- reactive({
    if(!is.null(input$controlListBT_rows_selected)) {
      #To get it to refresh after a comment is submitted
      input$submitcommentBT
      if(debugmode){
        cat(file=stderr(), "in commentstable2 reactive id is ", ControlSelectedBT(), ".\n")
      }
      if(ControlSelectedBT() >0) {
        commentsTableBT$data <- GetCommentDataBT(commentsTableNameBT(), ControlSelectedBT(), MeetingID())
      }
    } else {
      commentsTableBT$data
    }    
  })
  
  output$commentsTableBT <- DT::renderDataTable(commentsTable2BT(), selection = "single", rownames = FALSE, filter = 'top')
  output$CommentHdrOutputBT <- renderText(paste("All Stakeholder Comments about ", ControlSelectedBT()))
  
  output$detailTabTitle <- renderText(SettingsTableBT()$DetailTabTitle)
  output$introTextBT <- renderText(SettingsTableBT()$IntroText)
  output$BuildingType <- renderText(ControlSelectedBT())
  output$startTextBT <- renderText(SettingsTableBT()$StartScreenMessage)
  output$detailedlist <- renderText(paste(detailedBT[detailedBT$BLD_TYP_2009.CBSA==ControlSelectedBT(), 4], collapse = " | "))
  output$countsText <- renderText(sprintf("The NEEA Frame contains informations on %s sites. There are %s sites that are missing building type, %s sites missing square footage, and %s sites missing both.", 
                                          tblMissing[1,1], tblMissing[1,2], tblMissing[1,3], tblMissing[1,4]))
  
  # Handle Comment Submission --------------------------------------------------------------
  # Submit a comment 
  observeEvent(input$submitcommentBT, {
    # cat(file=stderr(), "in submit selected is", ControlSelected(), ".\n")
    # cat(file=stderr(), "in submit comment !nchar(input$email) is ", !nchar(input$email), ".\n")
    topic <- ControlSelectedBT()
    if (is.null(topic)){
      msg<- "Please make sure you have selected a building type to comment on before submitting comments"
      showModal(modalDialog(title = "No BuildingType Selected", msg, easyClose = TRUE)) 
      #} else if(!nchar(input$Commenter) & !nchar(input$organization) & !nchar(input$email)) {
      #} else if(!nchar(input$Commenter) & !nchar(input$organization)) {
    } else {
      # Check if this is a fresh comment or a modification
      #topic <- unlist(input$controlList_cell_clicked[3])
      # cat(file=stderr(), "in submit comment comment is ", input$comment, ".\n")
      usercommentdataBT <- AssembleCommentDataBT(commentsTableNameBT(), MeetingID(), topic, userInfo$user$email, input$commentBT)
      # cat(file=stderr(), "in submit comment names are", names(usercommentdataBT), ", usercommentdataBT is ", unlist(usercommentdataBT), ", length is ",length(usercommentdataBT), ".\n")
      if(debugmode){
        cat(file=stderr(), "in submit commentBT usercommentdataBT is ", names(usercommentdataBT), " with ", unlist(usercommentdataBT), ".\n")
        cat(file=stderr(), "in submit commentBT length(usercommentdataBT$FrID) is ", length(usercommentdataBT$FrID), " and !usercommentdataBT$FrID =='' is",!usercommentdataBT$FrID =="", ".\n")
        cat(file=stderr(), "in submit commentBT nchar(input$comment) is ", nchar(input$comment), " and is.null(input$PickChoice) is ", is.null(input$PickChoice),".\n")
      }
      # cat(file=stderr(), "in submit comment usercommentdataBT$DecisionID ", usercommentdataBT$DecisionID, ".\n")
      if(!usercommentdataBT$FrID =="" & nchar(input$commentBT)) {
        trySend <- try(UpdateData(usercommentdataBT, usercommentdataBT$FrID, commentsTableNameBT(), "FrID"))
        msg <- "Comment Updated!"
      } else if(!usercommentdataBT$FrID=="" & !nchar(input$commentBT)){
        trySend <- try(DeleteData(usercommentdataBT$FrID, commentsTableNameBT(), "FrID"))
        msg <- "Comment Removed!"
      } else {
        if(debugmode){
          cat(file=stderr(), "in submit comment about to add new comment.\n")
        }
        trySend <- try(CreateData(usercommentdataBT, commentsTableNameBT()))  
        msg <- "Comment Saved!"
      }
      if(inherits(trySend, "try-error")) {
        showModal(modalDialog("Error in Comment Submission", easyClose =  TRUE))
      } else {
        showModal(modalDialog(
          msg, 
          easyClose = TRUE))
      }
    }
  })
  
  observe({
    updateCheckboxInput(session, "showStartInstrBT", value = is.null(ControlSelectedBT()))
  })
  
  # display control table ----------------
  output$controlListBT <- DT::renderDataTable({
    buildingtypes
  }, selection = "single", rownames = FALSE,colnames = c("CBSA 2009 Building Type", "Sites", "% of Sites"),
  options = list(
    pageLength = 20,
    #dom = if(nrow(DecisionData())>10) 'tp' else 't'
    dom =  't' 
  )
  )     
  
  # Select row in table -> show details in inputs and pull up related comments
  observeEvent(input$controlListBT_rows_selected, {
    if (length(input$controlListBT_rows_selected) > 0) {
      if(debugmode){
        cat(file=stderr(), "row selected values  ", unlist(input$controlListBT_cell_clicked), ".\n")
      }
      
      id <- unlist(input$controlListBT_cell_clicked[3])
      commentdataBT <- LoadUserCommentBT(commentsTableNameBT(), MeetingID(), id, userInfo$user$email)
      updateTextAreaInput(session, "commentBT", value = unname(commentdataBT["Comment"]))
      
    }
  })
  
  # Plot
  sfplot <- eventReactive(ControlSelectedBT(), {
    #function(table, filterfield, criteria, valuefield, valuelabel){
    xlabel <- paste0("Square Footage of ", ControlSelectedBT(), " Properties")
    plotSummaryBT(frame, btfieldname, ControlSelectedBT(), sffieldname, xlabel)
  })
  #output$elementPlot <- renderPlot(eplot(), res = 110)
  
  output$SFplot <- renderPlot(sfplot())
  
  # end NEEA Frame stuff -----------
  
      
  # ** Sample Design stuff  start---------------------------------
  tblData <- reactiveValues(data = defaultDesign)

  SettingsTableTab2 <- reactive({
    if(debugmode){
      cat(file=stderr(), "In SettingsTableTab2 reactive, mtgID is ", MeetingID(), ".\n")
    }
    #LoadFilteredData(sdsettingtablename, "MeetingID", MeetingID())
    LoadFilteredData(sdsettingtablename, "TabID", paste0(MeetingID(), "-regional"))
  })
  
  SettingsTableHowTo <- reactive({
    if(debugmode){
      cat(file=stderr(), "In SettingsTableHowTo reactive, mtgID is ", MeetingID(), ".\n")
    }
    #LoadFilteredData(sdsettingtablename, "MeetingID", MeetingID())
    LoadFilteredData(sdsettingtablename, "TabID", paste0(MeetingID(), "-howto"))
  })
  
  DesignsTableNameTab2 <- reactive({
    if(debugmode){
      cat(file=stderr(), "In DesignsTableNameTab2 reactive, version is ", specialVersion(), ", table name is ", SampleDesignsTableName, ".\n")
    }
    
    if(specialVersion() == "neea") {
      paste0(SampleDesignsTableName, "_NEEA")
    } else {
      SampleDesignsTableName
    }
  })
  
  
  DesignSummaryTableNameTab2 <- reactive({
    if(debugmode){
      cat(file=stderr(), "In DesignSummaryTableNameTab2 reactive, version is ", specialVersion(), ", table name is ", SampleDesignSummaryTableName, ".\n")
    }
    
    if(specialVersion() == "neea") {
      paste0(SampleDesignSummaryTableName, "_NEEA")
    } else {
      SampleDesignSummaryTableName
    }
  })
  
  
  commentsTableNameTab2 <- reactive({
    if(debugmode){
      cat(file=stderr(), "In commentsTableNameSD reactive, version is ", specialVersion(), ", table name is ", SettingsTableTab2()$CommentsTableName, ".\n")
    }
    
    if(specialVersion() == "neea") {
      paste0(SettingsTableTab2()$CommentsTableName, "_NEEA")
    } else {
      SettingsTableTab2()$CommentsTableName
    }
  })
  
  observe({
    if(debugmode){
      cat(file=stderr(), "In observe bldgtypesonly, design is ", input$designname, ",  empty design is ", input$emptydesign, ".\n")
    }
    bldgtypesonly <- updateBT(bldgtypesonly, input$loginemail, input$designname)
  })
  
  observe({
    if(debugmode){
      cat(file=stderr(), "In observe designnamechange, design is ", input$designname, ",  empty design is ", isolate(input$emptydesign), ".\n")
    }
    #if(isGroupedDesign(hot_to_r(input$sampledesign), input$loginemail,saveddesign)){
    #if(isGroupedDesign(tblData$data, input$loginemail, input$designname, DesignsTableNameTab2(), MeetingID()) && !savegrp ){
    if(isGroupedDesign(isolate(tblData$data), isolate(input$loginemail), input$designname, DesignsTableNameTab2(), MeetingID())){
      if(debugmode){
        cat(file=stderr(), " +++ in design name changed for grouped design.\n")
      }
      showModal(copygrpsdialog())
    } else{
      if(debugmode){
        cat(file=stderr(), " --- in design name changed *NOT* a grouped design.\n")
      }
    }
    #bldgtypes <- updateBT(bldgtypes, input$loginemail,  input$designname)
  })  
  
  # Make a table of comments. This gets loaded/reloaded whenever the user submits a comment
  commentsTableTab2 <- reactiveValues()
  
  #hmm, what is this supposed to be doing. What is supposed to trigger this
  observe({
    if(debugmode){
      cat(file=stderr(), "in commentsTableTab2 observe table name is ", unlist(commentsTableNameTab2()), ", mtg id is ",  MeetingID(), ".\n")
    }
    commentsTableTab2$data <- GetCommentDataTab2(commentsTableNameTab2(), MeetingID(), input$loginemail,specialVersion(),input$designname )
  })
  
  commentsTable2Tab2 <- reactive({
    if(debugmode){
      cat(file=stderr(), "In commentsTable2Tab2 reactive. design is ", input$designname, " with length of ", length(input$designname), ".\n")
    }
    if(length(input$designname)>0){
    #if(!is.null(input$DesignsTable_rows_selected)){
      input$designname
      commentsTableTab2$data <- GetCommentDataTab2(commentsTableNameTab2(), MeetingID(), input$loginemail,specialVersion(),input$designname )
      if(debugmode){
        cat(file=stderr(), "In commentsTable2Tab2 reactive. pulled in comment length is ", length(commentsTableTab2$data), " with nrow of ", 
          nrow(commentsTableTab2$data), ", and data of ", unlist(commentsTableTab2$data), ".\n")
      }
    }
    commentsTableTab2$data
    
  })

  # Make a table of designs This gets loaded/reloaded whenever the user submits a design
  DesignsTable <- reactiveValues()
  
  #hmm, what is this supposed to be doing. What is supposed to trigger this
  observe({
    if(debugmode){
      cat(file=stderr(), "in DesignsTable observe table name is ", unlist(commentsTableNameTab2()), ", mtg id is ",  MeetingID(), ".\n")
    }
    DesignsTable$data <- GetDesignsTable(DesignsTableNameTab2(), MeetingID(), input$loginemail, specialVersion() )
  })
  
  DesignsTable2 <- reactive({
    if(debugmode){
      cat(file=stderr(), "In DesignsTable2 reactive.\n")
    }
    DesignsTable$data
  })
  
  # Select row in table -> load design ---- 
  observeEvent(input$DesignsTable_rows_selected, {
    if (length(input$DesignsTable_rows_selected) > 0) {
      summaryData <<- summaryBlank
      design <- DesignsTable$data[input$DesignsTable_rows_selected,1]
      designer <<- DesignsTable$data[input$DesignsTable_rows_selected,3]
      fieldlist <-  paste0(paste(names(defaultDesign), collapse = ", "), ", DesignDescription, budget")
      query <- sprintf("Select %s from %s LEFT JOIN users on users.email = %s.email WHERE DesignName = '%s' AND firstname = '%s' AND MeetingID = %s", 
                       fieldlist, DesignsTableNameTab2(), DesignsTableNameTab2(), cleanApos(design), DesignsTable$data[input$DesignsTable_rows_selected,3], MeetingID())
      if(debugmode){
        cat(file=stderr(), "row selected values  ", unlist(design), " for ", unlist(names(design)), ", length: ", length(design),
            ", default fields are ", unlist(names(defaultDesign)), " and query is ", query, ".\n")
      }
      
      trySend <- try(defaultDesign <- ReturnQuery(query))
      if(debugmode){
        cat(file=stderr(), "saved design row selected design row count is   ", nrow(defaultDesign), " length is: ", length(defaultDesign) , ".\n")
      }
      
      if(length(defaultDesign)==0){ #load without email restriction
        query <- sprintf("Select %s from %s WHERE DesignName = '%s' AND MeetingID = %s", 
                         fieldlist, DesignsTableNameTab2(), cleanApos(design), MeetingID())
        trySend <- try(defaultDesign <- ReturnQuery(query))
      }
      heightval <<- max(tblmin, nrow(defaultDesign)*24 +50)
      designdesc <- defaultDesign[1,"DesignDescription"]
      budget <- defaultDesign[1,"budget"]
      #need to drop the last 2 columns so they don't show in table
      defaultDesign <<- defaultDesign[,c(1:(ncol(defaultDesign)-2))]
      
      #load summary
      query <- sprintf("Select %s.* from %s LEFT JOIN users on users.email = %s.email WHERE SampleDesignName = '%s' AND firstname = '%s' AND MeetingID = %s", 
                       DesignSummaryTableNameTab2(), DesignSummaryTableNameTab2(), DesignSummaryTableNameTab2(), cleanApos(design), DesignsTable$data[input$DesignsTable_rows_selected,3], MeetingID())
      try(DesSummary <- ReturnQuery(query))
      if(length(DesSummary)>0){
        DesSummary <- DesSummary[,4:ncol(DesSummary)]
        #DesSummary[,c(3, 5:ncol(DesSummary))] <- format(as.numeric(DesSummary[,c(3, 5:ncol(DesSummary))]),big.mark =",")
        DesSummary$CBGn <- format(as.numeric(DesSummary$CBGn),big.mark =",")
        DesSummary$Stratn <- format(as.numeric(DesSummary$Stratn),big.mark =",")
        DesSummary$SRSn  <- format(as.numeric(DesSummary$SRSn ),big.mark =",")
        DesSummary$Fixed <- format(as.numeric(DesSummary$Fixed),big.mark =",")
        DesSummary$variable <- format(as.numeric(DesSummary$variable),big.mark =",")
        DesSummary$totalcost <- format(as.numeric(DesSummary$totalcost),big.mark =",")
        DesSummary$FrameE <- format(as.numeric(DesSummary$FrameE),big.mark =",")
        DesSummary$TotalFrameE <- format(as.numeric(DesSummary$TotalFrameE),big.mark =",")
        summaryData <<- converttosummaryuinames(DesSummary)
      }
      updateTextInput(session, "designname", value = as.character(design))
      updateTextAreaInput(session, "designdesc", value = as.character(designdesc))
      updateTextInput(session, "budget", value = as.character(budget))
      updateCheckboxInput(session, "emptydesign", value = FALSE)
      #update comment
      commentdatatmp <- LoadUserCommentTab2(commentsTableNameTab2(), MeetingID(), userInfo$user$email, design)
      updateTextAreaInput(session, "commentTab2", value = unname(commentdatatmp["Comment"]))
      
      #commentsTableTab2$data <- GetCommentDataTab2(commentsTableNameTab2(),  MeetingID(), input$loginemail, specialVersion(), input$designname)
      saveddesign <<- as.character(design)
      #designer <<- 
      if(debugmode){
        cat(file=stderr(), "row selected done updating inputs, cmt list is ", unlist(commentsTable2Tab2()), ", saveddesign is ", saveddesign, ".\n")
      }
    }
  })
  
  customBTTable <- reactive({
    input$cbtName
    input$designname
    input$loginemail
    input$groupedbtchanged
    #DesignsTable$data[input$DesignsTable_rows_selected,3]
    input$DesignsTable_rows_selected
    #determine if it's a design team design
    
    if (designer == "Design Team"){
      query <- sprintf("SELECT BldgGrpName, BldgTypeList from %s WHERE email = 'design' AND SampleDesignName = '%s'", CustomBldgTypestablename,  cleanApos(input$designname))
      btlist <- ReturnQuery(query)
      if(is.null(btlist)){
        query <- sprintf("SELECT BldgGrpName, BldgTypeList from %s WHERE email = '%s' AND SampleDesignName = '%s'", CustomBldgTypestablename, input$loginemail, cleanApos(input$designname))
        btlist <- ReturnQuery(query)
      }      
    }else{
      query <- sprintf("SELECT BldgGrpName, BldgTypeList from %s WHERE email = '%s' AND SampleDesignName = '%s'", CustomBldgTypestablename, input$loginemail, cleanApos(input$designname))
      btlist <- ReturnQuery(query)
    }
    #query <- sprintf("SELECT BldgGrpName, BldgTypeList from %s WHERE (email = '%s' OR email = 'design') AND SampleDesignName = '%s'", CustomBldgTypestablename, input$loginemail, cleanApos(input$designname))
    #query <- sprintf("SELECT BldgGrpName, BldgTypeList from %s WHERE SampleDesignName = '%s'", CustomBldgTypestablename, input$designname)
    #btlist <- ReturnQuery(query)
    if(debugmode){
      cat(file=stderr(), "***in customBTTable query is ", query, ", is.null(btlist) is ", is.null(btlist), " row count is ", nrow(btlist), ", designer is ", designer, " with status of ", designer == "Design Team", ".\n")
    }
    
    as.data.frame(btlist)
  })
  
  summaryTbl <- reactive({
      if(debugmode){
        cat(file=stderr(), "--in observe summaryData.\n")
      }
    input$sampledesign
    #summaryData <- convertsmrytouinames(summaryData)
    summaryData
  })
  
  # observe({
  #   if(debugmode){
  #     cat(file=stderr(), "in observe SummaryOut.\n")
  #   }
  #       summaryOut
  # })
  
    # outputs ----
  output$deleterow <- renderText(paste0("Delete row ", input$rowtodelete))
  output$nametoSave <- renderText(paste0("Save Design as  ", input$designname))
  output$framecols <- renderText(paste(nrow(domainflags), unlist(head(domainflags)), sep = "-"))
  output$Tab2Title <- renderText(SettingsTableTab2()$DetailTabTitle)
  output$introTextTab2 <- renderText(SettingsTableTab2()$IntroText)
  output$totalcost <- renderText(paste0("Total cost (variable) + base cost $",format(sum(as.numeric(tblData$data$Cost)) + Cost.OH,big.mark =",")))
  output$totalsample <- renderText(paste0("Total sample count is ",format(sum(as.numeric(tblData$data$'Sample Size')),big.mark =",")))
  output$helpbutton <- renderText(SettingsTableHowTo()$ControlTableTitle)
  output$summaryOut <- renderTable(summaryTbl(), rownames=FALSE)
  #output$commentsTableTab2 <- renderTable(commentsTable2Tab2()$data)

  output$commentsTableTab2 <- DT::renderDataTable(commentsTable2Tab2(), selection = "single", rownames = FALSE, filter = 'top',
                                                  options = list(
                                                    #pageLength = max(10, nrow(commentsTable2Tab2())), #keeping it from showing for some reason
                                                    dom = if(nrow(commentsTable2Tab2())>10) 'tp' else 't'
                                                  ))
  output$DesignsTable <- DT::renderDataTable(DesignsTable2(), selection = "single", rownames = FALSE, filter = 'top', 
                                                  options = list(
                                                    pageLength = min(5, nrow(DesignsTable2())),
                                                    dom = if(nrow(DesignsTable2())>5) 'tp' else 't'
                                                  ))
  
  # display Grouped BT table ----------------
  output$custombt <- DT::renderDataTable({
    if(debugmode){
      cat(file=stderr(), "%%in render custombt rows: ", unlist(nrow(customBTTable())), ", data:", unlist(customBTTable()), ".\n")
    }
    customBTTable()
    }, selection = "single", rownames = FALSE, colnames = c("Group", "Building Types"),
    options = list(
      #pageLength = min(5, nrow(customBTTable())),
      dom = if(nrow(customBTTable())>5) 'tp' else 't'
      #dom =  'tp' 
    )
  )

  # Select row in table -> update inputs
  observeEvent(input$custombt_rows_selected, {
    if (length(input$custombt_rows_selected) > 0) {
      #userselection <- unlist(strsplit(commentdata[,c("PickChoice")], ", "))
      userselection <- unlist(strsplit(customBTTable()[input$custombt_rows_selected,2], ", "))
      #design <- selected$value
      if(debugmode){
        cat(file=stderr(), "row selected values  ", unlist(userselection), ".\n")
      }
      leftover <- BTsLeft(bldgtypesonly, input$loginemail, input$designname, unname(customBTTable()[input$custombt_rows_selected,1]))
      updateSelectInput(session,"custombldgtype", selected = userselection, choices = leftover)
      isolate(updateTextInput(session, "cbtName", value = unname(customBTTable()[input$custombt_rows_selected,1])))
    }
  })       
  
  output$debug <- renderText(paste(colnames(designframe),collapse= ", "))
  output$designlength <- renderText(length(isolate(input$sampledesign$data)))
  
  # File download of current design -----------------------------------------------
  output$downloadDesign <- downloadHandler(
    filename <- function() {paste("design_", gsub(" ", "", input$designname, fixed = TRUE), ".xlsx", sep = "")},
    content = function(file) {
      fname <- paste(file,"xlsx",sep=".")
      wb <- loadWorkbook(fname, create = TRUE)
      shtName = "Settings"
      createSheet(wb, name = shtName)
      writeWorksheet(wb, data.frame("Design Name"= input$designname, "Design description" = input$designdesc, "Budget" =input$budget), sheet = shtName) 
      shtName = "Design"
      createSheet(wb, name = shtName)
      writeWorksheet(wb, tblData$data, sheet = shtName)
      shtName = "Summary"
      createSheet(wb, name = shtName)
      writeWorksheet(wb, summaryTbl(), sheet = shtName)
      saveWorkbook(wb)
      file.rename(fname,file)

    }
  )
  
  # File download of all designs -----------------------------------------------
  output$downloadAllDesigns <- downloadHandler(
    filename <- "Alldesigns.xlsx",
    content = function(file) {
      #fname <- paste(file,"xlsx",sep=".")
      nameroot <- "DesignCosts"
      fname <- paste0(nameroot, ".xlsx")
      #wb <- loadWorkbook(fname, create = TRUE)
      wb <- loadWorkbook(fname)
      shtName = "Designs"
      createSheet(wb, name = shtName)
      query <- sprintf("SELECT * from %s where MeetingID = %s and (email = '%s' or email = 'design')", DesignsTableNameTab2(), MeetingID(), input$loginemail)
      datatable <- ReturnQuery(query)
      writeWorksheet(wb, datatable, sheet = shtName)
      shtName = "Summaries"
      createSheet(wb, name = shtName)
      query <- sprintf("SELECT * from %s where MeetingID = %s and (email = '%s' or email = 'design')", DesignSummaryTableNameTab2(), MeetingID(), input$loginemail)
      datatable <- ReturnQuery(query)
      writeWorksheet(wb, datatable, sheet = shtName)
      setStyleAction(wb, XLC$"STYLE_ACTION.NONE")
      shtName = "CostPossibleDesigns"
      #createSheet(wb, name = shtName)
      query <- paste0("Select DesignDescription as Design, Domains, RE, format(s1.Stratn,0) as '1 Stage Sites', format(s2.CBGn,0) as 'Block Groups',",
          " format(s2.Stratn,0) as '2 Stage Sites', format(s1.totalcost,0) as '1 Stage', format(s2.totalcost,0) as '2 Stage', DesignDescription as Design2 ",
          " from DesignsSummaries s1 inner join DesignsSummaries s2 on s1.SampleDesignName = s2.SampleDesignName AND s1.email=s2.email ",
          " AND s1.MeetingID = s2.MeetingID inner join (Select DesignName, email, MeetingID, DesignDescription, Count(*) as Domains, ",
          " concat(round(Confidence*100,0), '/', round(Value*100, 0)) as 'RE' from SampleDesigns group by MeetingID, email, DesignName) d1 ",
          " on s1.SampleDesignName = d1.DesignName AND s1.email=d1.email AND s1.MeetingID = d1.MeetingID ",
          " where s1.stage like '1%' AND s2.Stage like '2%' and d1.email = 'design' order by Design, RE desc")
      # query <- paste0("Select DesignDescription as Design, Domains, RE, s1.Stratn as '1 Stage Sites', s2.CBGn as 'Block Groups',",
      #     " s2.Stratn as '2 Stage Sites', s1.totalcost as '1 Stage', s2.totalcost as '2 Stage', DesignDescription as Design2 ",
      #     " from DesignsSummaries s1 inner join DesignsSummaries s2 on s1.SampleDesignName = s2.SampleDesignName AND s1.email=s2.email ",
      #     " AND s1.MeetingID = s2.MeetingID inner join (Select DesignName, email, MeetingID, DesignDescription, Count(*) as Domains, ",
      #     " concat(round(Confidence*100,0), '/', round(Value*100, 0)) as 'RE' from SampleDesigns group by MeetingID, email, DesignName) d1 ",
      #     " on s1.SampleDesignName = d1.DesignName AND s1.email=d1.email AND s1.MeetingID = d1.MeetingID ",
      #     " where s1.stage like '1%' AND s2.Stage like '2%' and d1.email = 'design' order by Design, RE desc")
      datatable <- ReturnQuery(query)
      #trim design descriptions
      datatable$Design <- sub(' -.*$', '', datatable$Design)
      #datatable <- rbind(c("", "", "", "Sample Size", "", "", "Total Cost", ""), colnames(datatable), datatable)
      datatable <- rbind(datatable, c("   * Assumes 1 Stage Option B - Incomplete Frame", rep("", 7)))
      writeWorksheet(wb, datatable, sheet = shtName, header = FALSE, startRow = 3)
      fname <- paste0(nameroot, "Fly.xlsx")
      saveWorkbook(wb, fname)
      file.rename(fname,file)
    }
  )
  # test code ----
  observeEvent(input$runtest, {
    #designframe[,new:=stabbr=="WA"]
    if(debugmode){
      cat(file=stderr(), "in runtest colnames are ", unlist(paste(colnames(summaryData),collapse= ", ")), ".\n")
    }    
    
  })
  observeEvent(input$updatetest, {
    updateSelectInput(session,"fields",choices = colnames(designframe))
    if(debugmode){
      cat(file=stderr(), "in updatetest colnames are ", unlist(paste(colnames(designframe),collapse= ", ")), ".\n")
    }    
    update
  })
  
  # observe({
  #   if(input$numstages == twostage){
  #     msg<- "Two stage design is not implemented yet. It will be available for the next round of imput."
  #     showModal(modalDialog(title = "Two Stage not available yet.", msg, easyClose = TRUE)) 
  #     isolate(updateTextInput(session, "numstages", value = onestage))
  #     #designframe<<-copy(frame2stagedt) #[,c("RecordID","stabbr","Pub.Priv","Density","bldg.type","BLD_SF")])
  #   }else{
  #     #designframe<<-copy(framedt) #[,c("RecordID","stabbr","Pub.Priv","Density","bldg.type","BLD_SF")])
  #   }
  # })
  
  observe({
    #defaultDesign <- isolate(UpdateValues(tblData$data))
    
    #to get it to refresh after selection changes
     input$DesignsTable_rows_selected
    # 
    defaultDesign <- converttouinames (defaultDesign)
    tblData$data <-(defaultDesign)
  })
  
  observe({
    input$cbtName
    if(debugmode){
      cat(file=stderr(), "in observe input$cbtName.\n")
    }
    userselection <- unlist(BTsUsed(bldgtypesonly, isolate(input$loginemail), isolate(input$designname), isolate(input$cbtName)))
    leftover <- BTsLeft(bldgtypesonly, isolate(input$loginemail), isolate(input$designname), isolate(input$cbtName))
    if(debugmode){
      cat(file=stderr(), "in observe input$cbtName userselection is ", unlist(userselection), ".\n")
    }
    updateSelectInput(session,"custombldgtype", selected = userselection, choices = leftover)
    #updateSelectInput(session, "custombldgtype", choices = leftover)
  })
  
  # observe HOT----
  observe({
    if(debugmode){
      # cat(file=stderr(), "in observe hot_to_r input$sampledesign$data length is ", isolate(length((input$sampledesign$data))), " is.null is ", 
      #     isolate(is.null((input$sampledesign))), " nrow is ", isolate(nrow(input$sampledesign$data)), " row.names length is ", isolate(length(row.names(input$sampledesign))), ".\n")
       #isolate(cat(file=stderr(), "in observe hot_to_r tblData$data nrow is ", nrow((tblData$data)), " is.null is ", is.null((tblData$data)), ", !is.null(input$sampledesign) is ", !is.null(input$sampledesign), ".\n"))
      cat(file=stderr(), "in observe hot_to_r.\n")
    }
    
    bldgtypesR$data
    
    #with isolate on the line below it is a little behind on the height.
    heightval <<- max(tblmin, (length((input$sampledesign$data)))*24 +50)
    #if(!is.null(input$sampledesign) & isolate(nrow(tblData$data) > 0)){
    if(!is.null(input$sampledesign) & length(input$sampledesign$data)){      
        #input$sampledesign <- isolate(UpdateValues(tblData$data))
      tmpdesign <- hot_to_r(input$sampledesign)
      #tmpdesign <- isolate(UpdateValuesRegional(hot_to_r(input$sampledesign), isolate(input$loginemail), isolate(input$designname)))
      if(debugmode){
        cat(file=stderr(), "in observe hot_to_r assigment. tmpdesign is a ", typeof(tmpdesign),"\n")
      }
      # updateCheckboxInput(session, "emptydesign", value = isolate(length(tmp$data))==0)
      #tmpdesign <- (UpdateValuesRegional(isolate(tmpdesign), isolate(input$loginemail), isolate(input$designname)))
    }else{
      tmpdesign <- isolate(tblData$data)
      # if(debugmode){
      #   cat(file=stderr(), "in observe input$sampledesign length is ", isolate(length(tmp$data)), ".\n")
      # }
      # updateCheckboxInput(session, "emptydesign", value = isolate(length(tmp$data))==0)
    }
    tblData$data <- tmpdesign
    #updateCheckboxInput(session, "emptydesign", value = isolate(length(tblData$data))==0)
    if(debugmode){
      #cat(file=stderr(), "in observe hot_to_r emptydesign could be ", isolate(nrow(tblData$data)), ".\n")
    }
    
  })
  
  #make reactive so HOT table keeps current on list
  bldgtypesR <- reactiveValues(data = c("All","Remaining", bldgtypesonly))
  
  observe({
    bldgtypesR$data <- c("All","Remaining", unname(updateBT(bldgtypesonly, input$loginemail, input$designname)))
    })
  
  #Render HOT ----  
  output$sampledesign <- ({
    if(debugmode){
      cat(file=stderr(), ">> in output$sampledesign Data cols is ", isolate(unlist(colnames(tblData$data))), ".\n")
    }
    # 
     #renderUI({
    # if (length(tblData$data) > 0){
    renderRHandsontable(
    #rhandsontable(df,selectCallback = TRUE,readOnly = FALSE)
    rhandsontable((tblData$data),  width = "100%", height = heightval, selectCallback = TRUE, readOnly = TRUE, 
                  fillHandle = FALSE) #%>%
                #fillHandle = list(direction='vertical', autoInsertRow=FALSE)) #%>%
    #rhandsontable((defaultDesign),  width = "100%", height = heightval, selectCallback = TRUE, readOnly = TRUE) #%>%
    #causes an error :(
    #%>%  hot_table(contextMenu = FALSE)
    #%>%  hot_table(contextMenu : ['undo', 'redo'])
    #%>%  hot_context_menu(contextMenu =list('undo', 'redo'))
    #%>%  hot_table(highlightCol = TRUE)
    %>%  hot_col(col = "State", type = "dropdown", source = states, readOnly = FALSE)  #%>% Tried setting colWidth, but didn't work
    %>%  hot_col(col = "Utility Type", type = "dropdown", source = UtilType, readOnly = FALSE) 
    %>%  hot_col(col = "Density", type = "dropdown", source = densities, readOnly = FALSE) 
    %>%  hot_col(col = "Building Type", type = "dropdown", source = bldgtypesR$data, readOnly = FALSE)
    %>%  hot_col(col = "Target Type", type = "dropdown", source = analysisTypes, readOnly = FALSE)
    %>%  hot_col("Confidence", format = "0%", source = ConfidenceOptions, readOnly = FALSE)
    %>%  hot_col("Target Value", format = "0,0.00", readOnly = FALSE)
    %>%  hot_col("1 Stg $", type = "numeric", format = "$0,0.")
    %>%  hot_col("2 Stg $", type = "numeric", format = "$0,0.")
    %>%  hot_col("Population", type = "numeric", format = "0,0.") 
    %>%  hot_col("Floor Area (SF)", type = "numeric", format = "0,0.")
    %>%  hot_col("Avg (SF)", type = "numeric", format = "0,0.")
    %>%  hot_col("RP Vintage", type = "numeric", format = "0.000")
    %>%  hot_col("CBG var $", type = "numeric", format = "$0,0.")
    %>%  hot_col("SRS n", type = "numeric", format = "0,0.")
    %>%  hot_col("2 Stg n", type = "numeric", format = "0,0.")
    %>%  hot_col("CBG n", type = "numeric", format = "0,0.")
    
    %>% hot_validate_numeric(cols = "Confidence", min = 0, max = 1)
    %>% hot_cell(1, 11, "For Precision use values between 0.05 and .3. For Costs enter values greater than 0")
    #works, but interferes with the drop down boxes
    # %>% hot_cols(renderer = "
    #       function (instance, td, row, col, prop, value, cellProperties) {
    #                Handsontable.renderers.TextRenderer.apply(this, arguments);
    #                if (col> 3 && col < 8) {
    #                td.style.background = 'lightgrey';
    #                } }")
    #%>%    hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    
    )  #ends renderRHandsontable
    
    # }else{
    #   renderText("Select a design from below to begin")
    # }
       
   })
 # })
  
  # #temporary for debugging
  # output$domainflagstbl <- renderTable({
  #   summary(domainflags)
  # })
  # 
  # #currently turned off. May not need it.
  # output$userdesign <- renderTable({ 
  #   #(tblData$data)
  # })
  
  # creade how to modal ------
  observeEvent(input$designhowto, {
    msg <- HTML(SettingsTableHowTo()$IntroText)
    showModal(modalDialog(title = SettingsTableHowTo()$StartScreenMessage, msg, easyClose = TRUE)) 
  })
  
  observeEvent(input$addrow, {
    if(debugmode){
      cat(file=stderr(), "in addrow default is", unlist(createDefaultRow(isolate(tblData$data))), ".\n")
    }    
    tblData$data[nrow(tblData$data)+1,] <- createDefaultRow((tblData$data))
  })
  
  observeEvent(input$deleterow, {
    if(grepl('-', input$rowtodelete)){
      rng <- as.numeric(unlist(strsplit(input$rowtodelete, "-")))
      dellist <- seq.int(rng[1], rng[2])
    }else{
      dellist <- as.numeric(unlist(strsplit(input$rowtodelete, ",")))
    }

    if(nrow(isolate(tblData$data))<= length(dellist)){
      msg<- "You cannot delete all the rows. You must leave at least one"
      showModal(modalDialog(title = "Can't Delete All Rows", msg, easyClose = TRUE)) 
    }  else if(!length(which(rownames(isolate(tblData$data)) %in% dellist))==0){
      tblData$data<-isolate(tblData$data[-which(rownames(tblData$data) %in% dellist) ,])
    } else {
      msg<- sprintf("There is no row %s to delete.", input$rowtodelete)
      showModal(modalDialog(title = "Can't Delete Row", msg, easyClose = TRUE)) 
    }
  })
  
  # Submit an SD comment ----
  observeEvent(input$submitcommentTab2, {
    #check to see if design name is filled out
    if (is.null(input$designname)){
      msg<- "Please make sure you have named the design before submitting comments"
      showModal(modalDialog(title = "No Design Name", msg, easyClose = TRUE)) 
      #} else if(!nchar(input$Commenter) & !nchar(input$organization) & !nchar(input$email)) {
      #} else if(!nchar(input$Commenter) & !nchar(input$organization)) {
    } else  {
      query <- sprintf("Select DesignName from %s WHERE MeetingID = %s AND (email = '%s' OR email = 'design') and DesignName = '%s'", 
                       DesignsTableNameTab2(), MeetingID(),  userInfo$user$email, cleanApos(input$designname))
      design <- ReturnQuery(query)
      #designid <- GetSampleDesignID(DesignsTableNameTab2(), MeetingID(),  userInfo$user$email, input$designname) 
      if(debugmode){
        cat(file=stderr(), "in submitcommentTab2 is.null(design):", is.null(design), ", design is:", unlist(design),", length of design", length(design), ".\n")
      }
      #if (length(design)==0){
      if (is.null(design)){
        msg<- "You have to save your design before submitting comments"
        showModal(modalDialog(title = "Unsaved Design", msg, easyClose = TRUE)) 
      }else{
      # Check if this is a fresh comment or a modification
         usercommentdata <- AssembleCommentDataTab2(commentsTableNameTab2(), MeetingID(),  userInfo$user$email, input$designname,  input$commentTab2)
        if(debugmode){
          cat(file=stderr(), "in submit comment usercommentdata is ", names(usercommentdata), " with ", unlist(usercommentdata), ".\n")
        }
        if(!usercommentdata$SDCID =="" & nchar(input$commentTab2)) {
          trySend <- try(UpdateData(usercommentdata, usercommentdata$SDCID, commentsTableNameTab2(), "SDCID"))
          msg <- "Comment Updated!"
        } else if(!usercommentdata$SDCID=="" & !nchar(input$commentTab2)){
          trySend <- try(DeleteData(usercommentdata$SDCID, commentsTableNameTab2(), "SDCID"))
          msg <- "Comment Removed!"
        } else {
          if(debugmode){
            cat(file=stderr(), "in submit comment about to add new comment.\n")
          }
          trySend <- try(CreateData(usercommentdata, commentsTableNameTab2()))  
          msg <- "Comment Saved!"
        }
        if(inherits(trySend, "try-error")) {
          showModal(modalDialog("Error in Comment Submission", easyClose =  TRUE))
        } else {
          showModal(modalDialog(
            msg, 
            easyClose = TRUE))
        }
        #Update comments table
        commentsTableTab2$data <- GetCommentDataTab2(commentsTableNameTab2(),  MeetingID(), input$loginemail, specialVersion(), input$designname)
      }
    }
  })
  
  # Save a Design ----
  observeEvent(input$saveDesign, {
    #check that name has been filled out
    if(!nchar(isolate(input$designname))) {
      msg<- "You must specify a name before saving a design."
      showModal(modalDialog(title = "No Design Name Entered", msg, easyClose = TRUE)) 
    }else if(length(isolate(input$sampledesign$data))==0){
      msg<- "You must create at least one row before saving a design."
      showModal(modalDialog(title = "No Design Created", msg, easyClose = TRUE)) 
    } else{
      currentdesign <- isolate(tblData$data)
      isolate(writedesign(currentdesign, DesignsTableNameTab2(), input$loginemail, input$designname, MeetingID(), input$designdesc, input$budget))
      currentdesign <- isolate(summaryTbl())
      if(ncol(currentdesign)>1){
        isolate(writesummary (currentdesign, DesignSummaryTableNameTab2(), input$loginemail, input$designname, MeetingID()))
      }
      #Copy any grouped BTs
      success <- copyBldgGroups(saveddesign,isolate(input$designname), isolate(input$loginemail), CustomBldgTypestablename)
      
      #Update Designs table
      isolate(DesignsTable$data <- isolate(GetDesignsTable(DesignsTableNameTab2(),  MeetingID(), input$loginemail, specialVersion())))
    }#end name,design check
  })

  # Delete a Design ----
  observeEvent(input$deletedesign, {
    #check that name has been filled out
    if(!nchar(input$designname)) {
      msg<- "You must specify a name before deleting a design."
      showModal(modalDialog(title = "No Design Name Entered", msg, easyClose = TRUE)) 
    } else{
      query <- sprintf("Delete from %s WHERE email = '%s' AND DesignName = '%s' AND MeetingID = %s", DesignsTableNameTab2(), input$loginemail, cleanApos(input$designname), MeetingID())
      trySend <- try(rowsaff<-RunQuery(query))
      if(debugmode){
        cat(file=stderr(), "in deletedesign query is ", query, ". and rows deleted is:", rowsaff, ".\n")
      }
      if (rowsaff == 0){
        msg <- "Unable to delete that design. You cannot delete the design team designs."
      }else{
        msg <- "Design Deleted"
      }
      #delete summary if it exists
      query <- sprintf("Delete from %s WHERE email = '%s' AND SampleDesignName = '%s' AND MeetingID = %s", DesignSummaryTableNameTab2(), input$loginemail, cleanApos(input$designname), MeetingID())
      try(rowsaff<-RunQuery(query))
      
      if(inherits(trySend, "try-error")) {
        showModal(modalDialog("Error in Deleting Design", easyClose =  TRUE))
      } else {
        #Update comments table
        DesignsTable$data <- isolate(GetDesignsTable(DesignsTableNameTab2(),  MeetingID(), input$loginemail, specialVersion()))
        #load default design or show nothing
        updateCheckboxInput(session, "emptydesign", value = TRUE)
        showModal(modalDialog(
          msg, 
          easyClose = TRUE))
      }
      #delete any grouped BT's
      query <- sprintf("Delete from %s WHERE email = '%s' AND SampleDesignName = '%s'", CustomBldgTypestablename, input$loginemail, cleanApos(input$designname))
      try(rowsaff<-RunQuery(query))
      
    }#end name check
    
  })
  
  #Copy building groups ----
  observeEvent(input$copygrp, {
    success <- copyBldgGroups(saveddesign,isolate(input$designname), isolate(input$loginemail), CustomBldgTypestablename)
    
    removeModal()
    if (!success){
      msg <- "Unable to copy groups"
      showModal(modalDialog(
        msg, 
        easyClose = TRUE))
    }
    else{
      # updateTextInput(session, "designname", value = input$designname)
      #show Grouped BT table
      updateCheckboxInput(session, "groupedbtchanged", value = TRUE)
      
      #refresh HOT table BT List
      combbldgtypes <- unname(updateBT(bldgtypesonly, input$loginemail, input$designname))
      bldgtypesR$data <- c("All","Remaining", combbldgtypes)
    }
  })
  
  #Compute pop values ----
  observeEvent(input$computepops, {
    tblData$data <- UpdateValuesRegional(hot_to_r(input$sampledesign), isolate(input$loginemail), isolate(input$designname))
    })
    
    
    #tmp<-as.data.table(defaultDesign)
  # Compute size stratification----
  observeEvent(input$sizestrat, {
    #compute inputs
    #tblData$data <- UpdateValuesRegional(hot_to_r(input$sampledesign), isolate(input$loginemail), isolate(input$designname))
    #check for design overlap
    ###!!write this function to check design
    #msg and quit if design bad
    #designok <- checkDesign(tblData$data)
    designok <- TRUE
    
    if (!designok){
      msg <- "There is a problem with your design. Check to see that there is no overlap"
      showModal(modalDialog(
        msg, 
        easyClose = TRUE))
    }else{
      #check to see if domains have been set
      #test below is inadequate, so just always run it
      # if(!"domain" %in% names(designframe)){ #run popcounts
        userdesign <-as.data.table(converttotablenames(UpdateValuesRegional(hot_to_r(input$sampledesign),
                                                      isolate(input$loginemail), isolate(input$designname))))
        if(debugmode){
          cat(file=stderr(), "**in input$sizestrat after userdesign cols are", unlist(names(designframe)), ".\n")
          cat(file=stderr(), "**in input$sizestrat after update pop results are", unlist(designframe[,.N, by = domain]), ".\n")
        }
        
      # }else{
      #   userdesign <- as.data.table(converttotablenames(tblData$data))
      # }
      
      userdesign[,domain := paste0(State,"-",Utility,'-', Density,'-',BuildingType)]
      #  setnames(tmp, "State", "stabbr")
      # setnames(tmp, "BuildingType", "bldg.type")
      #setnames(tmp, "SampleSize", "Sample_Size")
      setnames(userdesign, "Confidence", "targ.conf")
      setnames(userdesign, "AnalysisType", "opt.param")
      setnames(userdesign, "Value", "value")
      if(debugmode){
        cat(file=stderr(), "in input$sizestrat design has ", nrow(userdesign), " rows, cols are ", unlist(paste(colnames(userdesign),collapse=", ")), " the data is ", unlist(userdesign), ".\n")
        #cat(file=stderr(), "in input$sizestrat str frame is  ", unlist(str(designframe)), ", str design is  ", unlist(str(userdesign)), ".\n")
      }
      
      #write.csv(designframe, "test_designframe.csv")
      #save(designframe, "test_designframe.rda")
      #write.csv(userdesign, "test_userdesign.csv")
      userdesign[,targ.conf := as.numeric(targ.conf)]
      userdesign[,value := as.numeric(value)]
      userdesign[,opt.param := as.character(opt.param)]
      if(debugmode){
        cat(file=stderr(), "*in input$sizestrat , frame domains are", unlist(unique(designframe$domain)),".\n")
      }
      trySend <- try(
        stratified <- get.adj.factor(copy(designframe[!is.na(domain)]),copy(userdesign[,c("domain", "targ.conf", "opt.param", "value")]),input$budget)
        #stratified <- size.stratify(copy(designframe[!is.na(domain)]),copy(userdesign[,c("domain", "targ.conf", "opt.param", "value")]))
        )
      
      if(inherits(trySend, "try-error")) {
        msg <- "Problem computing size. Make sure that each of your domains has a population greater than zero and that you are not overassigning the domains."
        showModal(modalDialog(msg, easyClose =  TRUE))
        userdesign <- tblData$data
      } else {
        
        # if(debugmode){
        #   cat(file=stderr(), "*in input$sizestrat After call cols are ", unlist(paste(colnames(stratified),collapse=", ")), " the data is ", unlist(stratified), ".\n")
        # }
        
          #set names to table version
        setnames(userdesign, "targ.conf", "Confidence" )
        setnames(userdesign, "opt.param", "AnalysisType" )
        setnames(userdesign, "value", "Value")
        
        userdesign <- userdesign[,c("domain","State","Utility","Density", "BuildingType", "Population", "SquareFootage", "AvgSF", "CV", "Confidence", "AnalysisType", "Value")]
        setkey (userdesign, domain)
        setkey(stratified, domain)
        if(debugmode){
          cat(file=stderr(), "*in input$sizestrat before join cols in stratified are ", unlist(paste(colnames(stratified),collapse=", ")),
              " the cols in userdesign are ", unlist(paste(colnames(userdesign),collapse=", ")), ".\n")
        }
        
        userdesign <- userdesign[stratified, on = "domain"]
        #set names to table version
        setnames(userdesign, "RP_Elect.EUI", "RP_E_EUI")
        setnames(userdesign, "RP_Gas.EUI", "RP_G_EUI")
        setnames(userdesign, "n.2", "SampleSize2nd")
        setnames(userdesign, "cost.2", "Cost2nd")
        setnames(userdesign, "n.srs", "SRSn")
        setnames(userdesign, "cb.n", "CBGn")
        setnames(userdesign, "cb.var.cst", "CBGcost")
        
        #userdesign <- addadjfactors(userdesign, isolate(input$loginemail), isolate(input$designname))
        
        #drop domain
        userdesign[, domain := NULL]
        if(debugmode){
          cat(file=stderr(), "*in input$sizestrat after join cols in userdesign are ", unlist(paste(colnames(userdesign),collapse=", ")), ".\n")
        }
        #Build summary table
        totals <- stratified[,.(cb.n=head(cb.n,1),n.1.stage=sum(SampleSize),n.srs=sum(n.srs),var.cost.1.stg=sum(Cost),
                                base.cost.1.stg=Cost.OH,tot.cost.1.stage=sum(Cost)+Cost.OH,
                                frm.enh.1.stg=Cost.1.FE,tot.w.fe.1.stg=sum(Cost)+Cost.OH+Cost.1.FE,n.2.stage=sum(n.2),var.cost.2.stg=sum(cost.2)+head(cb.var.cst,1),
                                base.cost.2.stg=Cost.OH+Cost.OH.1st.stage,tot.cost.2.stage=sum(cost.2)+Cost.OH+Cost.OH.1st.stage,frm.enh.2.stg=Cost.2.FE,
                                tot.w.fe.2.stg=sum(cost.2)+Cost.OH+Cost.OH.1st.stage+Cost.1.FE)]
        
        # totals <- stratified[,.(cb.n=head(cb.n,1),n.1.stage=sum(SampleSize),n.srs=head(n.srs,1),var.cost.1.stg=sum(Cost),base.cost.1.stg=Cost.OH,tot.cost.1.stage=sum(Cost)+Cost.OH,
        #                      frm.enh.1.stg=Cost.1.FE,tot.w.fe.1.stg=sum(Cost)+Cost.OH+Cost.1.FE,n.2.stage=sum(n.2),var.cost.2.stg=sum(cost.2),
        #                      base.cost.2.stg=Cost.OH+Cost.OH.1st.stage,tot.cost.2.stage=sum(cost.2)+Cost.OH+Cost.OH.1st.stage,
        #                      frm.enh.2.stg=Cost.2.FE,tot.w.fe.2.stg=sum(cost.2)+Cost.OH+Cost.OH.1st.stage+Cost.1.FE)]
        
        stg1cost <- format(sum(as.numeric(totals$base.cost.1.stg)),big.mark =",")
        stg2cost <- format(sum(as.numeric(totals$base.cost.2.stg)),big.mark =",")
        stg1costvar <- format(totals$var.cost.1.stg,big.mark =",")
        stg2costvar <- format(sum(as.numeric(totals$var.cost.2.stg)),big.mark =",")
        stg1totalcost <- format(totals$tot.cost.1.stage,big.mark =",")
        stg2totalcost <- format(totals$tot.cost.2.stage,big.mark =",")
        stg1n <- format(sum(as.numeric(totals$n.1.stage)),big.mark =",")
        stg2n <- format(sum(as.numeric(totals$n.2.stage)),big.mark =",")
        stg1srs <- format(sum(as.numeric(totals$n.srs)),big.mark =",")
        cbn2 <- format(sum(as.numeric(totals$cb.n)),big.mark =",")
        stg1framecost <- format(sum(as.numeric(totals$frm.enh.1.stg)),big.mark =",")
        stg2framecost <- format(sum(as.numeric(totals$frm.enh.2.stg)),big.mark =",")
        stg1allcost <- format(sum(as.numeric(totals$tot.w.fe.1.stg)),big.mark =",")
        stg2allcost <- format(sum(as.numeric(totals$tot.w.fe.2.stg)),big.mark =",")
        
        #userdesign[, adjfactor := NULL]
        
        costsummary <- data.frame("1 or 2 Stage Design"=c("1 Stage (stg)", "2 Stage (stg)"), "Census Block Group (CBG) Sample Size (n)"=c("NA", cbn2), 
                                  "Stratified Sample Size (n)"=c(stg1n, stg2n), "Simple Random Sample (SRS) Size (n)"=c(stg1srs, "NA"),
                                  "Fixed $"=c(stg1cost, stg2cost), "Variable $"=c(stg1costvar,stg2costvar), "Total $"=c(stg1totalcost,stg2totalcost), 
                                  "Frame Enhancement $"=c(stg1framecost,stg2framecost), "Total $ w/ Frame Enhancement"=c(stg1allcost,stg2allcost),
                                  #row.names=c("1 Stage (stg)", "2 Stage (stg)"),
                                  stringsAsFactors=FALSE, check.names = FALSE)    
        #costsummary["Total" ,] <- colSums(costsummary)
        summaryData <<- costsummary
        #summaryData <<- totals
        if(debugmode){
          cat(file=stderr(), "-*-in input$sizestrat after summary data update, cols are ", unlist(colnames(summaryData)), ".\n")
        }
      } #close else

      tblData$data <- as.data.frame(converttouinames(userdesign))
      msg <- "Done Computing Sample Sizes"
      showModal(modalDialog(
        msg, 
        easyClose = TRUE))
    } #end if design ok
  })  
  
  #
  # Add a custom building type ----
  observeEvent(input$addCBT, {
    #check that name is populated and not in bldytypes
    if(!nchar(input$designname)) {
      msg<- "You must specify a design name before creating groupings."
      showModal(modalDialog(title = "No Design Name Entered", msg, easyClose = TRUE)) 
    }else if (!nchar(input$cbtName)>0){
      msg<- "Please make sure you have named the grouped building type before trying to save"
      showModal(modalDialog(title = "No Grouped Name", msg, easyClose = TRUE))
    }else if (input$cbtName %in% bldgtypes){
      msg<- "The group name must be unique (not the same as an existing building type)"
      showModal(modalDialog(title = "Group Name Already in Use", msg, easyClose = TRUE))
    }else{
      wherepart <- sprintf("BldgGrpName = '%s' AND email = '%s' AND SampleDesignName = '%s'",  input$cbtName, input$loginemail, input$designname)
      bldglist <- paste(input$custombldgtype, collapse = ", ")
      if(debugmode){
        cat(file=stderr(), "in input$addCBT input$custombldgtype is:", bldglist, ", with length ", length(bldglist), " and nchar:", nchar(bldglist), ", nchar input:", nchar(input$custombldgtype), ".\n")
      }
      
      query <- sprintf("SELECT * From %s WHERE %s", CustomBldgTypestablename, wherepart)
      GrpExists <- ReturnQuery(query)
      if(!is.null(GrpExists) & nchar(bldglist)>0) {
        query <- sprintf("UPDATE %s SET BldgGrpName = '%s', BldgTypeList = '%s' WHERE %s", 
                         CustomBldgTypestablename, input$cbtName, bldglist, wherepart)
        #CustomBldgTypestablename, input$cbtName, input$loginemail, bldglist, wherepart)
        trySend <- try(RunQuery(query))
        action <- "edit"
        msg <- "Grouping Updated!"
      #} else if(!is.null(GrpExists) & !nchar(input$custombldgtype)>0){        
      } else if(!is.null(GrpExists) & nchar(bldglist)==0){
        query <- sprintf("DELETE From %s WHERE %s", CustomBldgTypestablename, wherepart)
        trySend <- try(RunQuery(query)) 
        action <- "delete"
        msg <- "Grouping Removed!"
      } else {
        if(debugmode){
          cat(file=stderr(), "in submit Grouping about to add new Grouping\n")
        }
        query <- sprintf("INSERT INTO %s (BldgGrpName, email, BldgTypeList, SampleDesignName ) VALUES ('%s', '%s', '%s', '%s')",
                         CustomBldgTypestablename, input$cbtName, input$loginemail, bldglist, input$designname)
        trySend <- try(RunQuery(query))
        action <- "add"
        msg <- "Grouping Saved!"
      }
      if(inherits(trySend, "try-error")) {
        showModal(modalDialog("Error in Grouping Submission", easyClose =  TRUE))
      } else {
        combbldgtypes <- unname(updateBT(bldgtypesonly, input$loginemail, input$designname))
        if(debugmode){
          cat(file=stderr(), "in input$addCBT msg(10) is ", msg[10], ".\n")
        }
        # if(debugmode){
        #   cat(file=stderr(), "in input$addCBT passed params to BTsLeft are email:", unlist(input$loginemail), ", design:", unlist(input$designname), ",  bldglist:", unlist(bldglist), ".\n")
        # }
        #if(action=="delete"){
          updateTextInput(session, "cbtName", value = "")
        #}         
        leftover <- BTsLeft(bldgtypesonly, input$loginemail, input$designname, input$cbtName)
        updateSelectInput(session, "custombldgtype", choices = leftover)
        updateCheckboxInput(session, "groupedbtchanged", value = FALSE)

        #refresh table
        bldgtypesR$data <- c("All","Remaining", combbldgtypes)
        showModal(modalDialog(
          msg, 
          easyClose = TRUE))
      }
    } #end if 
    
  })
  
  #Save grped design prompt
  copygrpsdialog <- function(copy = FALSE) {
    modalDialog(
      title = "Copy Grouped Designs",
      "You have changed a design name that has grouped building types. Would you like to copy the Building Type groupings?",
      footer = tagList(
        modalButton ("No"),
        actionButton("copygrp", "Copy Groups"))
    )
  }
  
  #* Start Oversample Tab ----
  SettingsTableTab3 <- reactive({
    if(debugmode){
      cat(file=stderr(), "In SettingsTableTab3 reactive, mtgID is ", MeetingID(), ".\n")
    }
    #LoadFilteredData(sdsettingtablename, "MeetingID", MeetingID())
    LoadFilteredData(oversamplesettingtablename, "TabID", paste0(MeetingID(), "-Oversample"))
  })
  
  DesignsTableNameTab3 <- reactive({
    if(debugmode){
      cat(file=stderr(), "In DesignsTableNameTab3 reactive, version is ", specialVersion(), ", table name is ", OversampleDesignsTableName, ".\n")
    }
    
    if(specialVersion() == "neea") {
      paste0(OversampleDesignsTableName, "_NEEA")
    } else {
      OversampleDesignsTableName
    }
  })
  
  commentsTableNameTab3 <- reactive({
    if(debugmode){
      cat(file=stderr(), "In commentsTableNameOverSD reactive, version is ", specialVersion(), ", table name is ", SettingsTableTab3()$CommentsTableName, ".\n")
    }
    
    if(specialVersion() == "neea") {
      paste0(SettingsTableTab3()$CommentsTableName, "_NEEA")
    } else {
      SettingsTableTab3()$CommentsTableName
    }
  })
  
  output$Tab3Title <- renderText(SettingsTableTab3()$DetailTabTitle)
  output$introTextTab3 <- renderText(SettingsTableTab3()$IntroText)
  
} #server close
)