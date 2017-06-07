#CBSA 

shinyUI(fluidPage(
  #is shinyjs being used anywhere?
  shinyjs::useShinyjs(),
  #title = textOutput("webtab"), #can't find right combination to get this to work :()
  title = "CBSA",
  fluidRow(
    column(8, titlePanel(textOutput("appTitle"))),
    column(2,textOutput("loggedinas")),
    column(2,actionButton("Logout", "Logout")) 
  ), 
  br(),
    tabsetPanel(
      selected = "Instructions",
      #selected = "Sample Design",
      tabPanel("Design Decisions", 
         br(),
         htmlOutput("introText"),br(),
         br(),
         fluidRow(
          column(2, DT::dataTableOutput("controlList")
                  ,checkboxInput("showCommentsTable", "Show Saved Comments", value = FALSE)
                 ), 
          column(10,
                 conditionalPanel(
                   condition = "input.showStartInstr",
                   htmlOutput("startText")
                 ),
                 conditionalPanel(
                   condition = "!input.showStartInstr",
                   fluidRow(
                   h4(textOutput("title"))
                     ),
                   p(h4("Background"), htmlOutput("background")),
                   p(h4("Proposed Decision"), htmlOutput("proposedDecision")),
                   fluidRow(
                     column(5, h4("Pros"), p(textOutput("pros"))),
                     column(5, h4("Cons"), p(textOutput("cons")))
                   ),
                   tags$hr()
                   , 
                   h4("My Comment"),
                  
                   fluidRow(
                     column(4, 
                          textAreaInput("comment","Comment", width = '100%', rows = 8, resize="both", placeholder="Enter your response up to 65,000 characters...") 
                     )
                      , #add file input/download here
                     conditionalPanel(
                        condition = "input.showpickchoice",
                        column(4,
                              uiOutput("choiceinput")
                        )
                     )
                    ,
                    #Add conditional Panel based on presence of excel input
                    column(4,
                           conditionalPanel(
                             condition = "input.showexceldownload",
                             htmlOutput("excelLink")
                             #HTML("<p>To submit detailed comments <a href = 'https://sbwconsulting.sharefile.com/d-s35b4dd287d445539' target = '_blank'>download the excel file.</a> You can add comments to the file and upload it below to submit your comments.</p>"),
                             #works but not for this project
                             # fileInput("file1", "Upload your Excel comment file",
                             #           accept = c("xlsx", "Excel Spreadsheet",".xlsx")
                             # ),tableOutput("contents")
                           )
                   )
                   ),
                   actionButton("submitcomment", "Save My Comment")
                ) #end Comment area conditional 
          ) #end column
      ), #end fluid row
        conditionalPanel(
          condition = "input.showCommentsTable",
          style = "padding:5px",
          fluidRow(
          tags$hr(), #Line        
          #Center this?
          h4(textOutput("CommentHdrOutput")),
          p("The boxes at the top of the table below can be used to filter the comments."),
          #actual comments
          DT::dataTableOutput("commentsTable"), 
          br(),br(),
          br(), br()
          )  #end fluid row
        ) #End conditional panel
        ,  conditionalPanel(
          #condition = sprintf("input['%s'} == 'gina@'", ns("loginemail")),
          # condition = sprintf("input['%s'] == 'xyz'",ns("showpickchoice")),
          condition = "input.showpickchoice == 'xyz'",
          #condition = FALSE,
          fluidRow(
            checkboxInput("showpickchoice", "Show pickchoice", value = FALSE),
            checkboxInput("showexceldownload", "Show exceldownload", value = FALSE),
            checkboxInput("showStartInstr", "Show startup", value = TRUE),
            checkboxInput("groupedbtchanged", "Show groupedbt", value = FALSE),
            textInput("LoadedDesign", "Loaded Design")
            #checkboxInput("emptydesign", "no entries in design", value = TRUE)
          )
      )
    ), #end tab panel
    #* NEEA Frame tab panel start -----
    tabPanel(
      textOutput("detailTabTitle"),
      br(),
      htmlOutput("introTextBT"),br(),
      textOutput("countsText"),
      br(),
      fluidRow(
        sidebarLayout(
          sidebarPanel(
            DT::dataTableOutput("controlListBT")
            ,checkboxInput("showCommentsTableBT", "Show Saved Comments", value = FALSE)
          ),
          mainPanel(
            conditionalPanel(
              condition = "input.showStartInstrBT",
              htmlOutput("startTextBT")
            ),
            conditionalPanel(
              condition = "!input.showStartInstrBT",
              fluidRow(
                h4(textOutput("BuildingType"))
              ),
              #Add conditional Panel based on presence of excel input
              column(4,
                     conditionalPanel(
                       condition = "input.showexceldownloadBT",
                       htmlOutput("excelLinkBT")
                     )
              ),
              #comma separated list of detailed
              br(),
              p("List of NEEA Frame 'Descriptions of Use'"),
              textOutput("detailedlist"),
              h4("Histogram of Square Footage"),
              #put in plot
              plotOutput("SFplot"),
              tags$hr(),
              h4("My Comment"),
              textAreaInput("commentBT","Comment", width = '100%', rows = 8, resize="both", placeholder="Enter your response up to 65,000 characters..."), 
              actionButton("submitcommentBT", "Save My Comment")
            ) #end Comment area conditional
          ) #end main panel
        ) #end sidebar layout
      ) #end fluid row
      , conditionalPanel(
        condition = "input.showCommentsTableBT",
        fluidRow(
          tags$hr(), #Line
          #Center this?
          h4(textOutput("CommentHdrOutputBT")),
          p("The boxes at the top of the table below can be used to filter the comments."),
          #actual comments
          DT::dataTableOutput("commentsTableBT"),
          br(),br(),
          br(), br()
        )  #end fluid row
      ) #End conditional panel
      ,  conditionalPanel(
        #condition = sprintf("input['%s'} == 'gina@'", ns("loginemail")),
        # condition = sprintf("input['%s'] == 'xyz'",ns("showpickchoice")),
        condition = "input.showpickchoice == 'xyz'",
        #condition = FALSE,
        fluidRow(
          checkboxInput("showpickchoiceBT", "Show pickchoice", value = FALSE),
          checkboxInput("showexceldownloadBT", "Show exceldownload", value = FALSE),
          checkboxInput("showStartInstrBT", "Show startup", value = TRUE)
        )
      )
    ),
    
    ### End NEEA Frame tab panel -- -- --
    
        # * Sample Design tab panel start -----
  tabPanel(
    textOutput("Tab2Title"),
    br(),
    htmlOutput("introTextTab2"),br(),
    actionButton("designhowto", textOutput("helpbutton")),br(),
    conditionalPanel(
      condition = "input.loginemail",
      style = "padding-left:20px", 
      fluidRow(
        tags$hr(), #Line        
        h4(textOutput("DesisgnsHdr")),
        fluidRow(
          column(8,"Select a design name to load that design. The boxes at the top of the table below can be used to filter the designs."),
          column(4,downloadButton("downloadAllDesigns", "Download All Designs"))
        ),
        DT::dataTableOutput("DesignsTable"), 
        br()
      )  #end fluid row
    ), #End conditional panel
    #selectInput("numstages", "Select the Number of stages", choices = c(onestage, twostage)),
    fluidRow(
      column(3, textInput("designname", "Design Name"), textAreaInput("designdesc", "Design Description"), 
             textInput("budget", "Stage 1 Budget", value = "500000")),
      column(3, 
             fluidRow(actionButton("saveDesign", htmlOutput("nametoSave")), br(), br(), 
                      actionButton("deletedesign", "Delete Design"), br(), br(),
                      actionButton("sizestrat", "Compute sample sizes"), br(), br(),
                      #checkboxInput("groupedbtchanged", "Show groupedbt", value = FALSE),
                      downloadButton("downloadDesign", "Download Design and Summary"))),
      # column(2,
      #         actionButton("computepops", "Compute Population Data"), br(), br(),
      #        actionButton("sizestrat", "Compute sample sizes")),
      column(4, 
             tableOutput("summaryOut")
             #textOutput("totalcost"), textOutput("totalsample")
             )
    ),
    
    fluidRow(
      style = "padding:5px", 
      #br(),
      #uiOutput("sampledesign"),
      conditionalPanel(
        condition = "input.loginemail == 'gina@'",
        textOutput("debug"),
        actionButton("runtest", "run test"),
        #selectInput("fields", "fieldlist", choices = colnames(designframe)),
        actionButton("updatetest", "force update")
      ),
      conditionalPanel(
        #condition = "! input.emptydesign",
        condition = "input.designname",
            rHandsontableOutput('sampledesign'),
            br(),
        fluidRow(
          column(2, br(), actionButton("addrow", "Add Row")),
          column(2, textInput("rowtodelete", "Row(s) to Delete")),
          column(2, br(), actionButton("deleterow", htmlOutput("deleterow")))
        )
      ),
      conditionalPanel(
        condition = "!input.designname",
        h3("Select a saved design or enter a design name to get started")
      ),
      tags$hr()
      , 
      fluidRow(
        column(4,textAreaInput("commentTab2","My Comment on Sample Design", width = '100%', 
                               rows = 5, resize="both", placeholder="Enter your response up to 65,000 characters...")),
        column(4, textInput("cbtName", "Name for building type grouping"),
               selectInput("custombldgtype", "Select Building Types to group together", choices = bldgtypesonly, multiple = TRUE), 
               actionButton("addCBT", "Add Custom Building Type")), 
        column(4, DT::dataTableOutput("custombt"))
      ),
      actionButton("submitcommentTab2", "Save My Comment"), 
      checkboxInput("showCommentsTableTab2", "Show Saved Comments", value = TRUE)

    )#end Fluid row 
    ,  conditionalPanel(
      condition = "input.showCommentsTableTab2",
        fluidRow(
            h4(textOutput("CommentHdrOutputTab2")),
          p("The boxes at the top of the table below can be used to filter the comments"),
          #actual comments
          DT::dataTableOutput("commentsTableTab2"), 
          br()
        )
    )
  ),
  # # End Sample Design  tab panel 
  # # * Reporting Tab ----
  # tabPanel(
  #   textOutput("Tab3Title"),
  #   br(),
  #   htmlOutput("introTextTab3"),
  #   br()
  #   #     "Regional Summary",
  #   # br(), 
  #   # h4("Regional Summary")
  #   #htmlOutput("instructions")
  # ),     
  # # End Reporting tab panel ---
  tabPanel(
    "Instructions",
    br(), htmlOutput("instructions")
  )      
    )#end tabset panel
  ) #end fluidpage
  ) #end ui