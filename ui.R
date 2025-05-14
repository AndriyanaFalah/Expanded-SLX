#Packages used in Expanded Spatial Lag of Exogenous Model Using RShiny

library(shiny)          # Package RShiny
library(shinythemes)    # RShiny theme package
library(DT)             # Package Table in RShiny
library(matlib)         # Package for matrix calculation
library(ggplot2)        # Package for plotting of graph
library(r2symbols)      # Package for math symbols
library(shinyjs)
library(MASS)

# Code UI or Front End Display in RShiny 
title <- tags$div(style="display:block;margin-bottom:10px;",
  span("Expanded Spatial Lag of Exogenous (ESLX) Model", style="font-size: 20px;"))


ui <- fluidPage(theme = shinytheme("flatly"),
  withMathJax(),
  useShinyjs(),
  tags$head(
    tags$style(
      HTML("
        .italic {
          font-style: italic;
        }
      ")
    )
  ),
  headerPanel("Expanded Spatial Lag of Exogenous (ESLX) Model",
              title = title),
  tabsetPanel( id="tab",
    tabPanel("Description of Model", class="tab",
             br(),
             div(
               p("The Spatial Lag of Exogenous (also called SLX model) refers to the inclusion of spatially lagged explanatory variables in a regression model. It accounts for potential spatial spillover effects of independent variables.", style="text-align: justify; text-justify: inter-word;padding: 10px 10px;"),
               p("The Spatial Expansion model was introduced (Casetti, 1972) in (LeSage, 1999) as an extension of the exogenous variables with the aim of describing spatial heterogeneity. Heterogeneity is used to describe different parameter values for each spatial observation through the distance between locations. The distance between two locations is measured by the Euclidean distance involving location coordinates. In general, the Spatial Expansion model is formulated with a linear regression model approach as follows:", style="text-align: justify; text-justify: inter-word;padding: 10px 10px;"),
               p(HTML(paste0(tags$b("y"),"=", tags$b("X"), tags$b("\u03B2"), "+", tags$b("\u03b5"), " with ", tags$b("\u03B2"), " = ", tags$b("ZJ"), tags$b("\u03B2"),tags$b(tags$sub("0")))), style="text-align:center"),
               p("The integration of Spatial Lag of Exogenous and Spatial Expansion with Casetti's Approach can be formulated as follows:", style="text-align: justify; text-justify: inter-word;padding: 10px 10px;"),
               p(HTML(paste0(tags$b("y"),"=",tags$b("X"), tags$b("ZJ"), tags$b("\u03B2"), tags$b(tags$sub("0")), "+",tags$b("W"),tags$b("X"),tags$sub("tilde"), tags$b("\u03B8"), "+", tags$b("\u03b5"))), style="text-align:center"),
               p("Suppose", tags$b("A=XZJ"), "then:", style="text-align: justify; text-justify: inter-word;padding: 10px 10px;"),
               p(HTML(paste0(tags$b("y"),"=", tags$b("A"), tags$b("\u03B2"), tags$b(tags$sub("0")), "+",tags$b("W"),tags$b("X"),tags$sub("tilde"), tags$b("\u03B8"), "+", tags$b("\u03b5"))), style="text-align:center"),
               
               p("with:", style="text-align: justify; text-justify: inter-word;padding: 10px 10px;"),
               p(HTML(paste0(tags$b("y")," \t:vector of dependent variables of size ", tags$em("n"), " &times; ", tags$em("1"))), style="padding: 10px 10px;"),
               p(HTML(paste0(tags$b("X")," \t:matrix of independent variables of size ", tags$em("n"), " &times; ", tags$em("nk"))), style="padding: 10px 10px;"),
               p(HTML(paste0(tags$b("X"),tags$sub("tilde"), " \t:matrix of independent variables of size  ", tags$em("n"), " &times; ", tags$em("k"))), style="padding: 10px 10px;"),
               p(HTML(paste0(tags$b("W")," \t:a spatial weight matrix of size ", tags$em("n"), " &times; ", tags$em("n"))), style="padding: 10px 10px;"),
               p(HTML(paste0(tags$b("\u03B2")," \t:matrix of size ", tags$em("nk"), " &times; ", tags$em("1"), " contains parameter estimators for all explanatory ", tags$em("k"), " variables at each observation")), style="padding: 10px 10px;"),
               p(HTML(paste0(tags$b("Z")," \t:location information that contains elements ", tags$em("Z"),tags$em(tags$sub("xi")), ",",  tags$em("Z"),tags$em(tags$sub("yi")), "with ", tags$em("i = 1, 2, ..., n "), "representing the latitude and longitude of each observation,  of size ", tags$em("nk"), " &times; ", tags$em("2nk"))), style="padding: 10px 10px;"),
               p(HTML(paste0(tags$b("J")," \t:expansion of the identity matrix of size ", tags$em("2nk"), " &times; ", tags$em("2k"))), style="padding: 10px 10px;"),
               p(HTML(paste0(tags$b("\u03B2"), tags$sub("0")," \t:parameter expressed by ", tags$b("\u03B2"),tags$sub(tags$em("lat")), ",", tags$b("\u03B2"),tags$sub(tags$em("long")), " of size ", tags$em("2k"), " &times; ", tags$em("1"))), style="padding: 10px 10px;"),
               p(HTML(paste0(tags$b("\u03B8"), " \t:spatial lag parameter vector of covariate variable of size ", tags$em("k"), " &times; ", tags$em("1"))), style="padding: 10px 10px;"),
               p(HTML(paste0(tags$b("\u03b5")," \t:error vector of size ", tags$em("n"), " &times; ", tags$em("1"))), style="padding: 10px 10px;"),
             )
             ),
    
    tabPanel("Import Data", class="tab",
             br(), br(),
             column(4,
                    fileInput("upload", "Please Choose a File CSV", accept= c(".csv"), multiple = FALSE),
                    span("The data should contain the coordinate of location (latitude and longitude), dependent variable and exogenous variables",style="color:blue"),
                    hr(),
                    radioButtons("separator", "Separator",
                                 choices = c(Semicolon = ";",
                                             Comma = ",",
                                             Tab = "\t"),
                                 selected = ";", inline = TRUE),
                    checkboxInput('header', 'Header', TRUE),
             ),
             column(8,
                    DTOutput("printData")
             )
             
             
    ),
    
    tabPanel("Vector & Matrix", class="tab",
             tabsetPanel(
               tabPanel("Vector of Y",
                        uiOutput("optionColumnY"),
                        uiOutput("inputrowY"),
                        br(),
                        dataTableOutput("vectorY")
               ),
               tabPanel('Matrix of X',
                        uiOutput("OptionColumnX"),
                        uiOutput("inputrowX"),
                        br(),
                        uiOutput("inputcolX"),
                        hr(),
                        DTOutput("matrixX")
               ),
               tabPanel('Matrix of XTilde',
                        uiOutput("OptionColumnXTilde"),
                        uiOutput("inputrowXTilde"),
                        br(),
                        uiOutput("inputcolXTilde"),
                        hr(),
                        DTOutput("matrixXTilde")
               ),
               tabPanel('Matrix of Z',
                        uiOutput("optionColumnZ"),
                        uiOutput("inputrowZ"),
                        br(),
                        uiOutput("inputcolZ"),
                        hr(),
                        DTOutput("matrixZ")
               ),
               tabPanel('Matrix of J',
                        uiOutput("optionColumnJ"),
                        uiOutput("inputrowJ"),
                        br(),
                        uiOutput("inputcolJ"),
                        hr(),
                        DTOutput("matrixJ")
               ),
               tabPanel('Matrix of W',
                        uiOutput("optionColumnW"),
                        uiOutput("inputrowW"),
                        br(),
                        uiOutput("inputcolW"),
                        hr(),
                        DTOutput("matrixW")
               ),
               tabPanel("Kronecker of Z and I",
                        br(),
                        verbatimTextOutput("warningZI"),
                        uiOutput("inputrowZI"),
                        br(),
                        uiOutput("inputcolZI"),
                        hr(),
                        DTOutput("matrixZI")
               ),

               tabPanel("Matrix of A",
                        br(),
                        verbatimTextOutput("warningA"),
                        uiOutput("inputrowA"),
                        br(),
                        uiOutput("inputcolA"),
                        hr(),
                        DTOutput("matrixA")
               )

             ),
    ),
    tabPanel("Result of Prediction", class="tab",
             tabsetPanel(
               tabPanel("\u03B8",
                        br(),
                        verbatimTextOutput("warningthetaMLE"),
                        uiOutput("inputrowthetaMLE"),
                        hr(),
                        DTOutput("ThetaMLE")
               ),
               tabPanel(HTML(paste0("\u03B2",tags$sub("0"))),
                        br(),
                        verbatimTextOutput("warningb0MLE"),
                        uiOutput("inputrowb0MLE"),
                        hr(),
                        DTOutput("b0MLE")
               ),
               tabPanel("\u03B2",
                        br(),
                        verbatimTextOutput("warningbetaMLE"),
                        uiOutput("inputrowbetaMLE"),
                        hr(),
                        DTOutput("betaMLE")
               ),
               tabPanel("\u03B2 w.r.t. X",
                        br(),
                        verbatimTextOutput("warningbetawrtX"),
                        uiOutput("inputrowbetawrtX"),
                        br(),
                        uiOutput("inputcolbetawrtX"),
                        hr(),
                        DTOutput("betawrtX")
               ),
               tabPanel(HTML("Y&#770;"),
                        br(),
                        verbatimTextOutput("warningYHatMLE"),
                        uiOutput("inputrowYHatMLE"),
                        hr(),
                        DTOutput("YHatMLE")
               ),
               tabPanel("Error",
                        br(),
                        verbatimTextOutput("warningErrorMLE"),
                        uiOutput("inputrowErrorMLE"),
                        hr(),
                        DTOutput("ErrorMLE")
               ),
               tabPanel("RMSE",
                        br(),
                        verbatimTextOutput("warningRMSE"),
                        verbatimTextOutput("RMSEMLE")
               ),
               tabPanel("MAPE",
                        br(),
                        verbatimTextOutput("warningMAPE"),
                        verbatimTextOutput("MAPEMLE")
               ),
             )

    ),
    tabPanel("Download Data", class="tab",
             br(),
             selectInput("downloaddata", "Choose Vector/Matrix", choices = c("Vector of Y" = "VY",
                                                                          "Matrix of X" = "MX",
                                                                          "Matrix of XTilde" = "MXT",
                                                                          "Matrix of Z" = "MZ",
                                                                          "Matrix of J" = "MJ",
                                                                          "Matrix of W" = "MW",
                                                                          "Matrix of ZI" = "MKZ",
                                                                          "Matrix of A" = "MA",
                                                                          "Vector \u03B20" = "VB0",
                                                                          "Vector \u03B2" = "VB",
                                                                          "Vector \u03B2 with respect to X" = "VBX",
                                                                          "Vector \u03B8" = "VT",
                                                                          "Vector Y Hat;" = "VBY",
                                                                          "Vector Error" = "VBE",
                                                                          "Aggregated Data" = "AD"
             )),
             downloadButton('downloadData', 'Download Data'),
             uiOutput("optionColumnDownloadData"),
             br(),
             DTOutput("showDownloadData")
    ),


    tabPanel("Created By",
             br(),
             div(
               p(tags$b("Dr. Annisa Nur Falah, S.Si., M.Mat."), style="text-align: center;"),
               p("Post-Doctoral Program, Research Center for Climate and Atmosphere, National Research and Innovation Agency (BRIN), Indonesia", style="text-align: center;"),
               p("e-mail: annisanurfalah02@gmail.com", style="text-align: center;"),
               br(),
               p(tags$b("Yudhie Andriyana, M.Sc., PhD."), style="text-align: center;"),
               p("Department of Statistics, Faculty of Mathematics and Natural Sciences, Universitas Padjadjaran, Indonesia", style="text-align: center;"),
               p("e-mail: y.andriyana@unpad.ac.id", style="text-align: center;"),
               br(),
               p(tags$b("Arjun Hasibuan, M.Mat."), style="text-align: center;"),
               p("Doctoral Program of Mathematics, Faculty of Mathematics and Natural Sciences, Universitas Padjadjaran, Indonesia", style="text-align: center;"),
               p("e-mail: arjun17001@mail.unpad.ac.id", style="text-align: center;"),
               br(),
             )
    ),

    
  ) # Tutup kurung Tabset panel
  
  
)# tutup kurung fluidPage


