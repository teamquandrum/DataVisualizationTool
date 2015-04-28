library(shiny) 

shinyUI(fluidPage(  
  headerPanel("Ordell Ugo Visualization Tool"),
  sidebarPanel(
    conditionalPanel(condition="input.tabs == 'Home Page'",       
     helpText("Home Page"),
     p("This tool was created using R as the backend and Shiny as the frontend"),
     br(),
     p("This tool was made by Aman Achpal and Adithya Abraham Philip"),
     p("under the Mentorship and Tutelage of"),
     h3("Professor Nitin V Pujari")

     ),    

    conditionalPanel(condition="input.tabs == 'Stack Overflow'", 
     helpText("Stack Overflow"),
     numericInput("num",label = "Enter your Stack Overflow User ID",value = 1),
     radioButtons(inputId = "public_graph", label = "Select the graph to download", choices = list("Score Distribution" = 1,"Answering patterns over time" = 2,"Daily answering patterns" = 3,"Top Tags Histogram" = 4,"Word Cloud of Top Tags" = 5)),
     radioButtons(inputId = "public_type", label = "Select the download type", choices = list("png", "pdf")),
     downloadButton(outputId = "down_public", label = "Download the plot")

     ),
    conditionalPanel(condition="input.tabs == 'Table Plot'", 
     helpText("Table Plot"),
     fileInput("tp_file","Upload the file"), 
     helpText("Default max. file size is 5MB"),
     selectInput("tp_column", "Select the Column to Sort By", ""),
     radioButtons(inputId = "ftype_tableplot", label = "Select the download type", choices = list("png", "pdf")),
     downloadButton(outputId = "down_tableplot", label = "Download the plot")

     ),

    conditionalPanel(condition="input.tabs == 'Pareto Chart'", 
     helpText("Pareto Chart"),
     fileInput("pareto_file","Upload the file"), 
     helpText("Default max. file size is 5MB"),
     radioButtons(inputId = "pareto_metric", label = "Select the metric", choices = list("max", "min", "mean", "median", "mode")),
     radioButtons(inputId = "ftype_pareto", label = "Select the download type", choices = list("png", "pdf")),
     downloadButton(outputId = "down_pareto", label = "Download the plot")

     ),

    conditionalPanel(condition="input.tabs == 'Histogram'", 
     fileInput("hist_file","Upload the file"),

     helpText("Default max. file size is 5MB"),
     tags$hr(),
     h5(helpText("Select the read.table parameters below")),
     checkboxInput(inputId = 'hist_header', label = 'Header', value = TRUE),
     checkboxInput(inputId = "hist_stringAsFactors", "stringAsFactors", TRUE),
     br(),
     radioButtons(inputId = 'hist_sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),


     selectInput("column", "Select the Column to visualize", ""),

     sliderInput("bin", "2. Select the number of histogram BINs by using the slider below", min=5, max=25, value=15),

     radioButtons("colour", label = "3. Select the color of histogram",
       choices = c("Green", "Red",
        "Yellow"), selected = "Green"),
     radioButtons(inputId = "ftype_histogram", label = "Select the download type", choices = list("png", "pdf")),
     downloadButton(outputId = "down_histogram", label = "Download the plot")
     ),

conditionalPanel(condition="input.tabs == 'QQ Plot'", helpText("Quantile Plots"),
 fileInput("qq_file","Upload the file"),

 helpText("Default max. file size is 5MB"),
 tags$hr(),
 h5(helpText("Select the read.table parameters below")),
 checkboxInput(inputId = 'qq_header', label = 'Header', value = TRUE),
 checkboxInput(inputId = "qq_stringAsFactors", "stringAsFactors", TRUE),
 br(),
 radioButtons(inputId = 'qq_sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
 selectInput("qqcolumn", "Select the Column to visualize", ""),
 radioButtons(inputId = "ftype_qq", label = "Select the download type", choices = list("png", "pdf")),
 downloadButton(outputId = "down_qq", label = "Download the plot")
 ),

conditionalPanel(condition="input.tabs == 'PP Plot'", helpText("Probability Plots"),
                 fileInput("pp_file","Upload the file"),
                 
                 helpText("Default max. file size is 5MB"),
                 tags$hr(),
                 h5(helpText("Select the read.table parameters below")),
                 checkboxInput(inputId = 'pp_header', label = 'Header', value = TRUE),
                 checkboxInput(inputId = "pp_stringAsFactors", "stringAsFactors", TRUE),
                 br(),
                 radioButtons(inputId = 'pp_sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                 selectInput("ppcolumn", "Select the Column to visualize", ""),
                 radioButtons(inputId = "ftype_pp", label = "Select the download type", choices = list("png", "pdf")),
                 downloadButton(outputId = "down_pp", label = "Download the plot")
),

conditionalPanel(condition="input.tabs == 'Correlation'",helpText("Scatter Plots, Line Graph, Box Plot"),
  fileInput("scat_file","Upload the file"),

  helpText("Default max. file size is 5MB"),
  tags$hr(),
  h5(helpText("Select the read.table parameters below")),
  checkboxInput(inputId = 'scat_header', label = 'Header', value = TRUE),
  checkboxInput(inputId = "scat_stringAsFactors", "stringAsFactors", FALSE),
  br(),
  radioButtons(inputId = 'scat_sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),


  selectInput("cx", "Select Column X", ""),
  selectInput("cy", "Select Column Y", ""),
  selectInput("geom","Select Geometry",c("Scatter Plot"=1,"Line Graph"=2,"Box Plot"=3)),
  radioButtons(inputId = "ftype_others", label = "Select the download type", choices = list("png", "pdf")),
  downloadButton(outputId = "down_others", label = "Download the plot")

  )
),

mainPanel(
  tabsetPanel(
    tabPanel("Home Page",
     fluidRow(
       column(1,
        img(src="ou.jpg", height = 480, width = 750)
        )
       )

     ),
    tabPanel("Stack Overflow",
     h4("Raw Data from Query"),
     dataTableOutput("dataraw"),
     br(),
     h4("Distribution of scores on your answers"),
     plotOutput("score"),
     br(),
     h4("Answering patterns over time"),
     plotOutput("time"),
     br(),
     h4("Daily answering patterns"),
     plotOutput("day"),
     br(),
     h4("Top tags answered"),
     plotOutput("tags"),
     br(),
     h4("Word cloud of top tags"),
     plotOutput("wordcloud")
     ),
    tabPanel("Table Plot",
     tableOutput("tp_sum"),
     br(),
     plotOutput("tp_output")
     ),

    tabPanel("Pareto Chart",
      plotOutput("pareto")
      ),

    tabPanel("Histogram",
     plotOutput("myhist")
     ),
    tabPanel("QQ Plot",
     plotOutput("myqq")
     ),
    
    tabPanel("PP Plot",
             plotOutput("mypp")
    ),
    tabPanel("Correlation",
      plotOutput("myscat")
      ),
    id = "tabs"    
    )
  )
)  
)