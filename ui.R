#--------------------------------------------------------------------------------------------------------------
#   This application was developed based on Snelder's cardiovascular model.
#
#   Yu Fu
#   March 6th, 2019
#--------------------------------------------------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyWidgets)
library(shinyjs)
library(V8)

# javascript code to collapse box
jscode <- "shinyjs.collapse = function(boxid) {
           $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
          }"

body <- dashboardBody(
                      # Including Javascript
                      useShinyjs(),
                      extendShinyjs(text = jscode),
                      
                      fluidRow(setSliderColor(c(rep("#f46e32",3), rep("#001158",12)), 1:15),
                               setShadow("box"),
                               tags$head(tags$style(HTML(".skin-blue .main-header > .logo { background-color: #001158;
                                                                                            font-weight: bold;
                                                                                            font-size: 28px}
                                                          .skin-blue .main-header .logo:hover {background-color: #001158;}

                                                          .skin-blue .main-header .navbar { background-color: #001158;} 

                                                         .box-primary .box-header>.logo{
                                                          font-weight: bold;}
            
                                                         .nav-tabs-custom .nav-tabs li.active {
                                                            border-top-color: #001158;
                                                          }
                                                         .box.box-solid.box-primary>.box-header {
                                                              color: #fff;
                                                              background: #001158;}
                                                          
                                                         .box.box-solid.box-primary{
                                                              border-bottom-color: #ccc;
                                                              border-left-color: #ccc;
                                                              border-right-color: #ccc;
                                                              border-top-color: #ccc;
                                                              
                                                          }
                                                          .box.box-solid.box-success>.box-header {background: #f46e32}
                                                          .box.box-solid.box-success{
                                                               border-bottom-color: #ccc;
                                                               border-left-color: #ccc;
                                                               border-right-color: #ccc;
                                                               border-top-color: #ccc;
                                                          
                                                          }
                                                         #titleID0{background-color:#001158}
                                                         #titleID1{background-color:#f46e32}
                                                         #titleID2{background-color:#001158}
                                                         #titleID3{background-color:#f46e32}
                                                         #titleID4{background-color:#001158}
                                                         #titleID5{background-color:#001158}
                                                         #titleID6{background-color:#001158}
                                                         #titleID7{background-color:#001158}"))),
                               
                               column(width = 4,
                                      box(width = NULL,
                                          id = "box0",
                                          collapsible = TRUE,collapsed = TRUE,
                                          status = "primary",
                                          solidHeader = TRUE,
                                          title = actionLink("titleID0",span(icon("paw"),span("Species",style = "font-weight:bold;font-size:18px"))),
                                          awesomeRadio("specie","Select Specie for Simulation:",choices = c("Rat","Dog"), selected = "Rat", inline = TRUE),
                                          conditionalPanel(
                                            condition = "input.specie == 'Rat'",
                                            awesomeRadio("rattype","Select rat strain:",choices = c("spontaneously hypertensive rats (SHR)","normotensive Wistar-Kyoto rats (WKY)"),
                                                                                                    selected = "spontaneously hypertensive rats (SHR)", inline = TRUE)
                                          ),
                                          actionButton("info1","Species Information",icon = icon("info-circle"))),
                                      
                                      box(width = NULL,
                                          id = "box1",
                                          title = actionLink("titleID1",span(icon("book"),span("Reference Drug (Optional)",style = "font-weight:bold"))),
                                          collapsible = TRUE,collapsed = TRUE,
                                          status = "success",
                                          solidHeader = TRUE,
                              
                                          pickerInput("drugname", "Drug:",
                                                      choices = c("Amiloride",
                                                                  "Amlodipine",
                                                                  "Atropine",
                                                                  "Enalapril",
                                                                  "Fasudil",
                                                                  "HCTZ",
                                                                  "Prazosin"),
                                                      options = list(title = "Reference Drugs")),
                                          
                                          conditionalPanel(
                                            condition = "input.drugname != '' ",
                                            
                                            actionButton("info2","Drug Information",icon = icon("info-circle")),
                                            hr(),
                                            fluidRow(align ="center",span("Dose Regimen",style = "font-weight:bold;font-style:italic;font-size:18px;color:grey;")),
                                            sliderInput("nd2", "Number of doses:", value=1, min=1, max = 10, step = 1),
                                            sliderInput("ii2", "Interdose interval (day):", value = 1, min = 0.5, max = 14, step = 0.5),
                                            awesomeRadio("amountunit2","Amount Unit:",choices = c("mg/kg","ug/kg","ng/kg"), selected = "mg/kg", inline = TRUE),
                                            sliderInput("amt2","Amount:",min = 0, max = 100, value = 10),
                                            materialSwitch("plotswitch",span("Show plot:",style="font-weight:bold;color:#f46e32"),value = FALSE, status="primary")
                                          )
                                          ),
 
                                      box(width = NULL,
                                          id = "box2",
                                          title = actionLink("titleID2",span(icon("search"),span("Investigational Drug",style = "font-weight:bold"))),
                                          collapsible = TRUE,collapsed = TRUE,
                                          status = "primary",
                                          solidHeader = TRUE,
                                          
                                          pickerInput("cmt", "Select PK model:",
                                                       choices = c("one-compartmental", "two-compartmental", "three-compartmental"),
                                                       options = list(title = "PK model")),
                                          
                                          conditionalPanel(
                                            condition = "input.cmt =='one-compartmental'| input.cmt == 'two-compartmental'| input.cmt =='three-compartmental'" ,
                                            hr(),
                                            fluidRow(align="center",span("Dose Regimen",style = "font-weight:bold;font-style:italic;font-size:18px;color:grey;")),
                                            awesomeRadio("timeunit","Time Unit:",
                                                         choices = c("hour","day","week"), selected = "hour", inline = TRUE),
                                            
                                            sliderInput("obs","Observation Timescale:",min = 1,max = 100,value = 100),
                                            sliderInput("nd", "Number of doses:", value=1, min=1, max = 10, step=1),
                                            sliderInput("ii", "Interdose interval (day):", value = 1, min = 0.5, max = 14, step=0.5),
                                            awesomeRadio("amountunit","Amount Unit:",choices = c("mg/kg","ug/kg","ng/kg"), selected = "mg/kg", inline = TRUE),
                                            awesomeRadio("concunit","Concentration Unit:",choices = c("mg/ml","ug/ml","ng/ml"), selected = "ng/ml", inline = TRUE),
                                            sliderInput("amt","Amount:",min = 0, max = 100, value = 10),
                                            hr(),
                                            fluidRow(align="center",span("PK Parameters",style = "font-weight:bold;font-style:italic;font-size:18px;color:grey;")),
                                            sliderInput("V1","V1 (L/kg):",value = 10, min = 0, max = 100, step = 0.1),
                                            sliderInput("k10",HTML("k10 (h<sup>-1</sup>):"),value = 0.1, min = 0, max = 10, step = 0.1)
                                          ),
                                          
                                          conditionalPanel(
                                            condition = "input.cmt =='two-compartmental'|input.cmt =='three-compartmental'",
                                            sliderInput("k12",HTML("k12 (h<sup>-1</sup>):"),value = 0.1, min = 0, max = 10, step = 0.1),
                                            sliderInput("k21",HTML("k21 (h<sup>-1</sup>):"),value = 0.1, min = 0, max = 10, step = 0.1)
                                          ),
                                          
                                          conditionalPanel(
                                            condition = "input.cmt =='three-compartmental'",
                                            sliderInput("k13",HTML("k13 (h<sup>-1</sup>):"),value = 0.1, min = 0, max = 10, step = 0.1),
                                            sliderInput("k31",HTML("k31 (h<sup>-1</sup>):"),value = 0.1, min = 0, max = 10, step = 0.1)
                                          ),
                                          conditionalPanel(
                                            condition = "input.cmt =='one-compartmental'| input.cmt == 'two-compartmental'| input.cmt =='three-compartmental'" ,
                                            hr(),
                                            fluidRow(align ="center",span("PD Parameters",style = "font-weight:bold;font-style:italic;font-size:18px;color:grey;")),
                                            awesomeRadio("mode","Mode of Action:",
                                                       choices = c("Heart Rate","Stroke Volume","Total Peripheral Resistance"),
                                                       selected = "Heart Rate", inline = TRUE),
                                        
                                            materialSwitch("cr",span("Circadian Rhythm Switch:",style = "font-weight:bold"),value = FALSE, status="primary"),
                                            sliderInput("emax","Emax:",min = 0, max = 2, value = 1, step = 0.01),
                                            sliderInput("ec50","EC50 (ng/ml):", min = 1, max = 1000, value = 100, step = 1))),
                                      
                                      box(width = NULL,
                                          id = "box3",
                                          title = actionLink("titleID3",span(icon("upload"),span("Input your data (Optional)",style = "font-weight:bold"))),
                                          collapsible = TRUE,collapsed = TRUE,
                                          status = "success",
                                          solidHeader = TRUE,
                                          
                                          fileInput("file1","Input your dataset (.xls, .xlsx, .csv):",
                                                    accept = c(
                                                               ".xls",
                                                               ".csv",
                                                               ".xlsx")
                                                   ),
                                          actionButton("template","Dataset Template",icon = icon("table"))
                                         ),
                                      box(width = NULL,
                                          title = span("Model Information",style = "font-weight:bold"),
                                          tags$img(src = "model.png",style = "display:block;max-width:100%;max-height:100%;width:auto;height:auto;
                                                                        margin-left:auto;margin-right:auto;margin-bottom:auto;margin-top:auto"),
                                          span(tags$a(href="https://bpspubs.onlinelibrary.wiley.com/doi/full/10.1111/bph.12824", 
                                                      "[1] Snelder, N. et al. Br J Pharmacol (2014)."),
                                               style = "font-size:16px;font-style: italic;"),
                                          
                                          conditionalPanel(condition = "input.plotswitch",
                                                           hr(),
                                                           fluidRow(align = "center",span("Parameters for Reference Drug",style = "font-weight:bold;font-style:italic;font-size:18px;color:grey;"))),
                                          fluidRow(align = "center",
                                                   tableOutput("parameters2")),
                                          conditionalPanel(condition = "input.cmt != ''",
                                                           hr(),
                                                           fluidRow(align = "center",span("Parameters for Investigational Drug",style = "font-weight:bold;font-style:italic;font-size:18px;color:grey;"))),
                                          fluidRow(align = "center",
                                                   tableOutput("parameters1"))
                                          
                                          )
                               ),
                               
                                column(width = 4,
                                       box(width = NULL, 
                                           id = "box4", collapsible = TRUE, 
                                           plotOutput("PK",height="400px"), 
                                           title = actionLink("titleID4",span(icon("prescription-bottle-alt"),span("PK - Pharmacokinetics",style = "font-weight:bold"))), 
                                           status = "primary", solidHeader = TRUE),
                                       box(width = NULL, 
                                           id = "box5", collapsible = TRUE, 
                                           plotOutput("CO",height="400px"),
                                           title = actionLink("titleID5",span(icon("tint"),span("CO - Cardiac Output",style = "font-weight:bold"))), 
                                           status = "primary", solidHeader = TRUE)
                                       ),
                                column(width = 4,
                                       box(width = NULL, 
                                           id = "box6", collapsible = TRUE, 
                                           plotOutput("HR",height="400px"),
                                           title = actionLink("titleID6",span(icon("heartbeat"),span("HR - Heart Rate",style = "font-weight:bold"))), 
                                           status = "primary", solidHeader = TRUE),
                                       box(width = NULL, 
                                           id = "box7", collapsible = TRUE, 
                                           plotOutput("MAP",height="400px"), 
                                           title = actionLink("titleID7",span(icon("bolt"),span("MAP - Mean Arterial Pressure", style = "font-weight:bold"))), 
                                           status = "primary", solidHeader = TRUE)
                                       )
                               ),
                      fluidRow(align = "center",
                               span("Version 1.0.0, Made by",
                                    tags$a(href="mailto:y.fu@lacdr.leidenuniv.nl", "Yu Fu"),
                                    ", ",
                                    tags$a(href="https://www.universiteitleiden.nl/en/staffmembers/coen-van-hasselt#tab-1", "J.G.C. van Hasselt"),
                                    style = "font-size:18px;font-style: italic")),
                      fluidRow(align = "center",
                               img(src = "LU_logo.png", height = 120),
                               img(src = "LACDR_logo.png",height = 80),
                               img(src = "imi_logo.png",height = 80),
                               img(src = "transQST_logo.png",height = 100))
)

ui <- dashboardPage(skin="blue",
      dashboardHeader(title = "Hemodynamic-simulator",
                      tags$li(class = "dropdown", downloadBttn("report", span("Generate Report",style = "font-weight:bold;color:#fff"),size = "sm",style = "bordered"),
                              style = "padding-top:8px; padding-bottom:8px;padding-right:10px"),
                      titleWidth = "500px"),
      dashboardSidebar(disable = TRUE),
      body
)

