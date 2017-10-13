library(DT)
library(ggplot2)
library(plotly)
library(Hmisc) #%nin%
library(scales)
#library(doBy)
library(data.table)
library(gdata)
library(plyr)#rbind.fill

options(shiny.sanitize.errors = TRUE)

source("global.R")

options(shiny.maxRequestSize=10000000000*1024^2) #pour les data du grande taille

server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  
  
  ##################### Importer la base ################
  dat <- reactive({
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1
    
    # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
    # and                              write.csv(iris, "iris.csv")
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote, na.strings=c("NA","NaN", " ",""))
    
    
    
    # Update inputs (you could create an observer with both updateSel...)
    # You can also constraint your choices. If you wanted select only numeric
    # variables you could set "choices = sapply(df, is.numeric)"
    # It depends on what do you want to do later on.
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = c(names(df), selected = names(df)[2]))
    
    
    return(df)
  })
  
  ########### minimiser la base ###############
  mini<-reactive({
    
    bbb <- subset(dat(),!(is.na(input$colonne)),select= c(input$vars))
    #View(bbb)
    return(bbb)
  })
  
  
  ############### modifier notre base ###############
  supp<-reactive({
    
    #aa <- subset(mini(),mini()[,input$etat] %nin% c(input$eta)) #pour choisir les colonnes quon va utiliser#
    bb <- subset(mini(),mini()[,input$rb] > input$rbb)  #pour controler RB#
    cc <- subset(bb,bb[,input$column] %in% c(input$control))  #pour controler les annees#
    dd <- subset(cc,cc[,input$bilan] %in% c(input$biilan)) #pour controler le bilan#
    
    return(dd)
    
  })
 
  
  ############## maximum RB ##############
  
  maxx<- reactive({
    m=max(as.numeric(mini()[,input$rb]), na.rm = TRUE)
    return(m)
  })
  
  
  ########### fonction Time #################
  
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("The current time is", Sys.time())
  })
  
  ########## filtration pour les secteurs #############
  filteredData <- reactive({ 
    
    kk <- subset(supp(),supp()[,input$colonne] %in% c(input$sector))
    
    return(kk)
  })
 
  
  ##################### INPUTS ###########################
  
  
  output$sector = renderUI(
    awesomeCheckboxGroup('sector', 'Choose a sector',
                         c(
                           levels(as.factor(mini()[,input$colonne])),"NONE","SECTOR"
                         ),c("NONE","SECTOR"), inline = TRUE, status = "warning")
    
  )
  
  output$rb <- renderUI(
    selectInput("rb","Choose the RB column",choices = c(names(mini())),"RB_31_12_N")
  )
  
  output$rbb <- renderUI(
    sliderInput("rbb",
                "Value of RB",
                min = 0,
                max=250000,
                #max = maxx(),
                value = 100000)
  )
  
  
  
  output$bilan= renderUI(
    selectInput('bilan','Choose the balance sheet: BILAN',
                c(names(dat())),"BILAN")
  )
  
  output$biilan = renderUI(
    awesomeCheckboxGroup(inputId = "biilan", 
                         label = "Choose type of balance sheet", choices = c("With balance sheet" = "1","Without balance sheet" = "0"), selected = c("With balance sheet" = "1","Without balance sheet" = "0"), 
                         inline = TRUE, status = "warning")
  )
  
  output$control = renderUI(
    awesomeCheckboxGroup(inputId = "control", 
                         label = "Choose years", choices = c(levels(as.factor(mini()[,input$column]))), selected = c(levels(as.factor(mini()[,input$column]))), 
                         inline = TRUE, status = "warning")
  )
  
  output$vars =renderUI(
    selectInput('vars','Choose only columns you need',
                c(names(dat())),selected=c(names(dat())),multiple = TRUE)
  )    
  
  
  #output$etat =renderUI(
  #  selectInput('etat','Choose the status column: ETAT',
  #              c(names(dat())),"ETAT")
  #)         
  
  #output$eta = renderUI(
  #  if (is.null(input$etat) || input$etat == "pick one"){return()
  #  }else selectInput('eta',
  #                   'Choose one or more states to be eliminated',
  #                    c(levels(as.factor(dat()[,input$etat])),"pick one"),
  #                    "pick one",multiple = TRUE))
  
  output$colonne = renderUI(
    
    selectInput('colonne',
                'Choose the sectors column: SECTEUR',
                c(names(mini()),"pick one"),
                "SECTEUR_NOTATION"))
  
 # output$sec = renderUI(
#    if (input$secteur == "No" || is.null(input$colonne) || input$colonne == "pick one"){return()
 #   }else selectInput('sec',
#                      'Choose a sector',
 #                     c(levels(as.factor(mini()[,input$colonne])),"pick one"),
#                      "pick one",multiple = TRUE))
  
  output$askctx= renderUI(
    radioButtons("askctx" , "CTX", c("no","yes"),inline = T)
  )
  output$askimp= renderUI(
    radioButtons("askimp" , "IMP", c("no","yes"),inline = T)
  )
  output$askcls= renderUI(
    radioButtons("askcls" , "Quarterly Class", c("no","yes"),inline = T)
  )
  output$askclsn= renderUI(
    radioButtons("askclsn" , "Year-end class", c("no","yes"),inline = T)
  )


  output$definition = renderUI(   checkboxGroupInput("definition", "Choose the definition",
                                                     c(
                                                       #"CTX" = "CTX",
                                                       "IMP" = "IMP",
                                                       "CLS" = "CLS",
                                                       "CLSF" = "CLSF")))
  
  
  
  output$column = renderUI(selectInput('column',
                                       'Choose the column of years: ANNEE',
                                       c(names(mini()),"pick one"),
                                       "ANNEE_N"))
  
  output$year = renderUI(
    if (is.null(input$column) || input$column == "pick one"){return()
    }else selectInput('year',
                      'Choose a year',
                      c(levels(as.factor(mini()[,input$column])),"pick one"),
                      "pick one"))
  
  
  
  output$CTX_N = renderUI(
    
      selectInput('CTX_N',
                  'CTX_N',
                  c(names(mini()),"pick one"),
                  "CTX_31_12_N"))
  
  output$IMP_N = renderUI(
    
      selectInput('IMP_N',
                  'IMP_N',
                  c(names(mini()),"pick one"),
                  "ANC_IMP_31_12_N"))
  
  output$CLS_N = renderUI(
    
      selectInput('CLS_N',
                  'CLS_N',
                  c(names(mini()),"pick one"),
                  "CLS_31_12_N"))
  
  
  output$CLSn_N = renderUI(
   
      selectInput('CLSn_N',
                  'CLSn_N',
                  c(names(mini()),"pick one"),
                  "CLS_31_12_N"))
  
  output$CTX_N1 = renderUI(
    if (is.null(input$CTX_N) || input$CTX_N == "pick one"){return()}
    else 
      selectInput('CTX_N1',
                  'CTX_N1',
                  c(names(mini()),"pick one"),
                  "CTX_N1"))
  
  output$IMP_N1 = renderUI(
    if (is.null(input$IMP_N) || input$IMP_N == "pick one"){return()}
    else 
      selectInput('IMP_N1',
                  'IMP_N1',
                  c(names(mini()),"pick one"),
                  "MAX_ANC_IMP_N1"))
  
  output$CLS_N1 = renderUI(
    if (is.null(input$CLS_N) || input$CLS_N == "pick one"){return()}
    else 
      selectInput('CLS_N1',
                  'CLS_N1',
                  c(names(mini()),"pick one"),
                  "MAX_CLS_N1"))
  
  output$CLSn_N1 = renderUI(
    if (is.null(input$CLSn_N) || input$CLSn_N == "pick one"){return()}
    else 
      selectInput('CLSn_N1',
                  'CLSn_N1',
                  c(names(mini()),"pick one"),
                  "CLS_31_12_N1"))
  
  ############### ######## la table ##########
  output$contents <- renderDataTable({
    head(dat(), n = isolate(input$obs))
  })
  
  ######## la table sans letat E ###########
  output$cont <- renderDataTable({
    
    head(supp(), n = input$obs)
  })
  
  
  
  output$MyPlot1 <- renderPlot({
    x <- supp();
    ggplot(x,aes(as.factor(x[,input$xcol]),x [,input$ycol]))+geom_boxplot()
    
  })
  
  
  
  #############la base des secteurs###########
  base<-reactive({
    dd=subset(supp(),supp()[,input$colonne]==input$sec)
    return(dd)
  })
  
  output$secteurrr <- renderPrint(base())
  
  
  
  ############quelle base on va choisir ################
  dataaa<- reactive({
    if( any(input$secteur == "Yes" )){
      da=base()
    } else
      da=supp()
    return(da)
  })
  
  output$dattt<- renderPrint(dataaa())
  
  
  
  ############## table des taux  ###########
  
  output$ptauxd<- renderDataTable({ 

      
      a=c()
      b=c()
      d=c()
      years=as.vector(levels(as.factor(supp()[,input$column])))
      for(i in years){
        
        a[i]<-obs(supp(),input$column,i,input$CTX_N,input$IMP_N,input$CLS_N,input$CLSn_N,input$CTX_N1,input$IMP_N1,input$CLS_N1,input$CLSn_N1,input$askctx,input$askimp,input$askcls,input$askclsn)
        b[i]<-defaut(supp(),input$column,i,input$CTX_N,input$IMP_N,input$CLS_N,input$CLSn_N,input$CTX_N1,input$IMP_N1,input$CLS_N1,input$CLSn_N1,input$askctx,input$askimp,input$askcls,input$askclsn)
        d[i]<-tauxx(supp(),input$column,i,input$CTX_N,input$IMP_N,input$CLS_N,input$CLSn_N,input$CTX_N1,input$IMP_N1,input$CLS_N1,input$CLSn_N1,input$askctx,input$askimp,input$askcls,input$askclsn)
        e<-percent(round(d,4))
      }
      k=cbind(observation=a,defaut=b,taux=e)
  
    
  })

  ################### LA TABLE QU'ON VA UTILISER POUR NOS PLOTS ####################
  #cette fonction nous permet d'avoir une table qui contient les annees , les taux , les secteurs
  table=reactive({
    data=supp()
    L<-list()
    tables<-list()
    vect<-as.character(unique(data[,input$colonne]))
    years=as.vector(levels(as.factor(data[,input$column])))
    
    for (i in 1:length(vect))
      L[[i]]<-subset(data,data[,input$colonne]==vect[i])
      names(L)<-vect
    
         tables=lapply(L,function(x) {
             tx=c()
               for(j in years){
                  tx[j]=tauxx(x,input$column,j,input$CTX_N,input$IMP_N,input$CLS_N,input$CLSn_N,input$CTX_N1,input$IMP_N1,input$CLS_N1,input$CLSn_N1,input$askctx,input$askimp,input$askcls,input$askclsn)
               }
 
             h=data.frame(years,taux=tx)
                        })
         
    ma_table<- rbind.fill(tables)
    
      d=c()
      years=as.vector(levels(as.factor(supp()[,input$column])))
         for(j in years){
             d[j]<-tauxx(supp(),input$column,j,input$CTX_N,input$IMP_N,input$CLS_N,input$CLSn_N,input$CTX_N1,input$IMP_N1,input$CLS_N1,input$CLSn_N1,input$askctx,input$askimp,input$askcls,input$askclsn)
         }
         g=data.frame(years,taux=d)
   
   ma_table<- rbind.fill(g,ma_table)
   
   e=c()
   years=as.vector(levels(as.factor(supp()[,input$column])))
   for(j in years){
     e[j]<-tauxx(filteredData(),input$column,j,input$CTX_N,input$IMP_N,input$CLS_N,input$CLSn_N,input$CTX_N1,input$IMP_N1,input$CLS_N1,input$CLSn_N1,input$askctx,input$askimp,input$askcls,input$askclsn)
   }
   h=data.frame(years,taux=e)
   
   ma_table<- rbind.fill(h,ma_table)
   
     sect=c()
     none=c()
     nv_list= c()
     n=length(years)
     m=length(vect)
  
      none=rep("NONE",times=n)
      sect= rep("SECTOR",times=n)
      nv_list= rep(vect,each=n)
      
       my_list= c(sect,none,nv_list)
       
  ma_table<- cbind(ma_table,secteur=my_list)
   

     return(ma_table)
  })
  
  
  ################# Filtration du table selon les secteurs pour les plots ##################
  new_data<- reactive({
    new=subset(table(),table()[,3] %in% c(input$sector))
    return(new)
  })
 
  
  ####################### PLOT DES COURBES DES SECTEURS ###############
 output$ptaux= renderPlotly({
   
   
   plot_ly(
     new_data(),  
     x = ~years,
     y = ~taux,
     split = ~secteur,
     mode = "lines + markers",
     type = 'scatter',
     hoverinfo = "y"   
 
   )
 })
  
  
  #########################################################
  
})