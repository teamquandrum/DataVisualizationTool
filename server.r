library(shiny)
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(rjson)
library(lubridate)
library(qcc)

#Global Variables
compact <- function(x) Filter(function(e) length(e) > 0, x)

shinyServer(
  function(input,output,session) {

    library(reshape2)
    library(plyr)
    library(dplyr)
    library(ggplot2)
    library(rjson)
    library(lubridate)
    

    #TABLE PLOT VISUALIZER
    library(tabplot)
    output$down_tableplot <- downloadHandler(
      filename =  function() {
        paste("Tableplot", input$ftype_tableplot, sep=".")
        },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        if(input$ftype_tableplot == "png")
          png(file) # open the png device
          else
          pdf(file) # open the pdf device
          indexy<<-input$tp_column
          tableplot(tp_data(),sortCol = indexy)
        dev.off()  # turn the device off
        
      } 
      )
    
    
    
    
    tp_data <- reactive({
      file1 <<- input$tp_file
      if(is.null(file1)){return()}
      else {
        dat<-read.csv(file=file1$datapath)
        updateSelectInput(session, "column", NA)
        return(dat)
      }
      })
    
    observe({
      updateSelectInput(
        session,
        "tp_column",
        choices=colnames(tp_data())
        )
      })
    
    output$tp_sum <- renderTable({
      if(is.null(tp_data())){return ()}
      summary(tp_data())
      
      })
    
    
    
    output$tp_output <- renderPlot({
      indexy<<-input$tp_column
      if(!is.null(tp_data()))
      tableplot(tp_data(),sortCol = indexy)
      })
    
    #HISTOGRAM PLOTTING CODE
    
    output$down_histogram <- downloadHandler(
      filename =  function() {
        paste("Histogram", input$ftype_histogram, sep=".")
        },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        if(input$ftype_histogram == "png")
          png(file) # open the png device
          else
          pdf(file) # open the pdf device
          df<- myData()
          colm = grep(input$column, colnames(df))
          hist(df[,colm], col =input$colour, xlim = c(0, max(df[,colm])), main = "Histogram of Selected Dataset", breaks = seq(0, max(df[,colm]),l=input$bin+1), xlab = names(df[colm]))
        dev.off()  # turn the device off
        
      } 
      )
    
    inFile <- reactive({
      if (is.null(input$hist_file)) {
        return(NULL)
        } else {
          input$hist_file
        }
        })


    myData <<- reactive({
      if (is.null(inFile())) {return(NULL)} 

      else {
       updateNumericInput(session, "column", NA)
       dat <-  read.table(file=inFile()$datapath, sep=input$hist_sep, header = input$hist_header, stringsAsFactors = input$hist_stringAsFactors)
       #dat <- read.csv(inFile()$datapath)
       
       return(dat)
     }
     })

    observe({
     updateSelectInput(
      session,
      "column",
      choices=c(names(myData()[sapply(myData(), is.numeric)]) )
      )
     })

    output$myhist <- renderPlot(

    {
      df<- myData()
      colm = grep(input$column, colnames(df))
      colm <- as.numeric(as.character(colm)) 
      colm <- na.omit(colm)
      if(!is.null(df))
      hist(df[,colm], col =input$colour, xlim = c(0, max(df[,colm])), main = "Histogram of Selected Dataset", breaks = seq(0, max(df[,colm]),l=input$bin+1), xlab = names(df[colm]))

      })    

	#END HISTOGRAM PLOTTING

  #QQ PLOTTING CODE

  output$down_qq <- downloadHandler(
    filename =  function() {
      paste("QQ Plot", input$ftype_qq, sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        if(input$ftype_qq == "png")
          png(file) # open the png device
          else
          pdf(file) # open the pdf device
          Qdf<- myDataQ()
          colm = grep(input$qqcolumn, colnames(Qdf))
          if(!is.null(Qdf))
          qqnorm(Qdf[,colm])
          qqline(Qdf[,colm])
          dev.off()  # turn the device off

        } 
        )

  inFileQ <- reactive({
    if (is.null(input$qq_file)) {
      return(NULL)
      } else {
        input$qq_file
      }
      })


  myDataQ <<- reactive({
    if (is.null(inFileQ())) {return(NULL)} 

    else {
     updateNumericInput(session, "qqcolumn", NA)
     datQ <-  read.table(file=inFileQ()$datapath, sep=input$qq_sep, header = input$qq_header, stringsAsFactors = input$qq_stringAsFactors)
       #datQ <- read.csv(inFileQ()$datapath)
       
       return(datQ)
     }
     })


  observe({
   updateSelectInput(
    session,
    "qqcolumn",
    choices=c(names(myDataQ()[sapply(myDataQ(), is.numeric)]) )
    )
   })

  output$myqq <- renderPlot(

  {
    Qdf<- myDataQ()
    colm = grep(input$qqcolumn, colnames(Qdf))
    if(!is.null(Qdf)){
      qqnorm(Qdf[,colm])
      qqline(Qdf[,colm])
    }
    })    

  #END QQ PLOTTING CODE

    #PP PLOTTING CODE

    output$down_pp <- downloadHandler(
      filename =  function() {
        paste("PP Plot", input$ftype_pp, sep=".")
        },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        if(input$ftype_pp == "png")
          png(file) # open the png device
          else
          pdf(file) # open the pdf device
          Pdf<- myDataP()
          colm = grep(input$ppcolumn, colnames(Pdf))
          newdf<-Pdf[,colm]
          pdist<-pnorm(newdf)
          if(!is.null(newdf)){
            plot(ppoints(length(newdf)), sort(pdist), main = "PP Plot", xlab = "Observed Probability", ylab = "Expected Probability")
          }
            dev.off()  # turn the device off
      }
        )

    inFileP <- reactive({
      if (is.null(input$pp_file)) {
        return(NULL)
        } else {
          input$pp_file
        }
        })


    myDataP <<- reactive({
      if (is.null(inFileP())) {return(NULL)} 

      else {
       updateNumericInput(session, "ppcolumn", NA)
       datP <-  read.table(file=inFileP()$datapath, sep=input$pp_sep, header = input$pp_header, stringsAsFactors = input$pp_stringAsFactors)
       #datQ <- read.csv(inFileQ()$datapath)
       
       return(datP)
     }
     })


    observe({
     updateSelectInput(
      session,
      "ppcolumn",
      choices=c(names(myDataP()[sapply(myDataP(), is.numeric)]) )
      )
     })

    output$mypp <- renderPlot(

    {
      
      Pdf<- myDataP()
      if(!is.null(Pdf)){
      colm = grep(input$ppcolumn, colnames(Pdf))
      newdf<-Pdf[,colm]
      pdist<-pnorm(newdf)
      
        plot(ppoints(length(newdf)), sort(pdist), main = "PP Plot", xlab = "Observed Probability", ylab = "Expected Probability")
      }
      })    

  #END PP PLOTTING CODE

  #SCATTER PLOT AND OTHER PLOT CODE

  inFileSc <- reactive({
    if (is.null(input$scat_file)) {
      return(NULL)
      } else {
        input$scat_file
      }
      })


  myDataSc <- reactive({
    if (is.null(inFileSc())) {return(NULL)} 

    else {
     updateNumericInput(session, "cx", NA)
     updateNumericInput(session, "cy", NA)
     dat <-  read.table(file=inFileSc()$datapath, sep=input$scat_sep, header = input$scat_header, stringsAsFactors = input$scat_stringAsFactors)
     return(dat)
   }
   })


  observe({
   updateSelectInput(
    session,
    "cx",
    choices=c(names(myDataSc()[sapply(myDataSc(), is.numeric)]) )
    )
   })

  observe({
   updateSelectInput(
    session,
    "cy",
    choices=c(names(myDataSc()[sapply(myDataSc(), is.numeric)]) )
    )
   })

  output$myscat <- renderPlot(

  {

    datasc<<-myDataSc()
    p <- ggplot(datasc, aes_string(x=input$cx, y=input$cy))
    if(input$geom==1)
    p<- p + geom_point()
    if(input$geom==2)
    p<- p + geom_line()
    if(input$geom==3)
    p<- p + geom_boxplot()
    
    glo <<- p
    if(!is.null(datasc))  
    print(p)

    })    
  
  output$down_others <- downloadHandler(
    filename =  function() {
      paste("Plot", input$ftype_others, sep=".")
      },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$ftype_others == "png")
        png(file) # open the png device
        else
        pdf(file) # open the pdf device
        print(glo)
      dev.off()  # turn the device off
      
    } 
    )

  # END SCATTER PLOT

  #STACK OVERFLOW PUBLIC DATA VISUALIZER


  library(stackr)
  library(lubridate)
  library(wordcloud)
  
  answers <- reactive({
    stack_users(as.numeric(input$num), "answers", num_pages = 10, pagesize = 100)
    })
  
  
  output$dataraw <- renderDataTable({
    t<-answers()
    t <- tbl_df(t)
    show_vars<-c("owner_display_name","question_id","answer_id","creation_date","is_accepted","score","owner_reputation","last_activity_date")
    t[, show_vars, drop = FALSE]
    },options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  
  output$score <- renderPlot({
    pl<<-answers()
    score_plot <<- ggplot(pl, aes(score)) + geom_histogram(binwidth = 1)
    print(score_plot)
    })
  
  output$time <- renderPlot({
    pl1<<-answers()
    time_plot <<- pl1 %>% mutate(month = round_date(creation_date, "month")) %>% count(month) %>% ggplot(aes(month, n)) + geom_line()
    print(time_plot)
    })
  
  output$day <- renderPlot({
    pl2<<-answers()
    day_plot <<- pl2 %>% mutate(hour = hour(creation_date)) %>%
    count(hour) %>%
    ggplot(aes(hour, n)) + geom_line()
    print(day_plot)
    })
  
  output$tags <- renderPlot({
    top_tags <- stack_users(as.numeric(input$num), "top-answer-tags", pagesize = 100)
    tags_plot<<- top_tags %>% mutate(tag_name = reorder(tag_name, -answer_score)) %>%
    head(20) %>%
    ggplot(aes(tag_name, answer_score)) + geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
    print(tags_plot)
    })
  
  output$wordcloud <- renderPlot({
    top_tags <- stack_users(as.numeric(input$num), "top-answer-tags", pagesize = 100)
    cloud_plot<<-wordcloud(top_tags$tag_name, top_tags$answer_count)
    print(cloud_plot)
    })
  
  output$down_public <- downloadHandler(
    filename =  function() {
      paste("StackOverflowPlot", input$public_type, sep=".")
      },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$public_type == "png")
        png(file) # open the png device
        else
        pdf(file) # open the pdf device

        getgraph<-as.numeric(input$public_graph)

        if(getgraph == 1)
        print(score_plot)
        else if(getgraph == 2)
        print(time_plot)
        else if(getgraph == 3)
        print(day_plot)
        else if(getgraph == 4)
        print(tags_plot)
        else if(getgraph == 5){
          top_tags <- stack_users(as.numeric(input$num), "top-answer-tags", pagesize = 100)
          wordcloud(top_tags$tag_name, top_tags$answer_count)
        }

      dev.off()  # turn the device off
      
    } 
    )
  
  #END STACK OVERFLOW PUBLIC DATA VISUALIZER
  
  #PARETO PLOT VISUALIZER
  
  output$down_pareto <- downloadHandler(
    filename =  function() {
      paste("Pareto", input$ftype_pareto, sep=".")
      },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$ftype_pareto == "png")
        png(file) # open the png device
        else
        pdf(file) # open the pdf device
        x<-data()
        pareto.chart(x, col=heat.colors(length(x)), main="Pareto Chart")
      dev.off()  # turn the device off
      
    } 
    )
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  data <- reactive({
    file1 <- input$pareto_file
    if(is.null(file1)){return()} 
    dat<-read.csv(file=file1$datapath)
    nums <- sapply(dat, is.numeric)
    dat <- dat[,nums]
    if(input$pareto_metric=="mean")
    dat<-colMeans(dat)
    else if(input$pareto_metric=="median")
    dat<-apply(dat, 2, FUN = median)
    else if(input$pareto_metric=="max")
    dat<-apply(dat, 2, FUN = max)
    else if(input$pareto_metric=="min")
    dat<-apply(dat, 2, FUN = min)
    else if(input$pareto_metric=="mode")
    dat<-apply(dat, 2, FUN = Mode)
    return(dat)
    })

  output$pareto <- renderPlot(

  {
    x<-data()
    if(!is.null(x))
    pareto.chart(x, col=heat.colors(length(x)), main="Pareto Chart")

    })


  
  #END PARETO PLOT VISUALIZER  


}
)