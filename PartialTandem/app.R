#############
#setup and loading data

library(shiny)
library(DT)
library(bslib)
library(proporz)

#read language file
#csv is transposed, so that the required texts can easily referenced by their id(column name) and used language(rows)
localisation <- t(read.csv("localisation.csv", header=TRUE, sep=","))
colnames(localisation)<-localisation[1,]
localisation<-as.data.frame(localisation[-1,])

#votes data
Votes2024 <- read.csv("data/votes 2024.csv", header=TRUE, sep=";")
Votes2019 <- read.csv("data/votes 2019.csv", header=TRUE, sep=";")

#country codes
CC<-c("AT","BE-NED","BE-FRA","BE-DEU","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")

#election law data (national seat distribution method, threshold, seats)
law.matrix19<- read.csv("data/laws2019.csv", sep=";")
law.matrix24<- read.csv("data/laws2024.csv", sep=";")
ep22.law.matrix19<- read.csv("data/laws2019.csv", sep=";")
ep22.law.matrix19$threshold[13]<-0.035 #Germany
ep22.law.matrix24<- read.csv("data/laws2024.csv", sep=";")
ep22.law.matrix24$threshold[13]<-0.035 #Germany
ep22.law.matrix24$threshold[28]<-0.035 #Spain
mueller.law.matrix19<- read.csv("data/muellerlaws2019.csv", sep=";")
mueller.law.matrix24<- read.csv("data/muellerlaws2024.csv", sep=";")
law.matrix19<-cbind(CC,law.matrix19[,-1])
law.matrix24<-cbind(CC,law.matrix24[,-1])
ep22.law.matrix19<-cbind(CC,ep22.law.matrix19[,-1])
ep22.law.matrix24<-cbind(CC,ep22.law.matrix24[,-1])
mueller.law.matrix19<-cbind(CC,mueller.law.matrix19[,-1])
mueller.law.matrix24<-cbind(CC,mueller.law.matrix24[,-1])

#data frame which decides who participates (yes: "Founding Six",Spain, Portugal, Poland, no: rest)
tandem_participation_ini<-matrix(rep("yes",29),29,1)
tandem_participation_ini[c(1,5,6,7,8,9,10,11,14,15,16,18,19,21,25,26,27,29)]<-"no"
tandem_participation_ini<-cbind(CC,tandem_participation_ini)
colnames(tandem_participation_ini)<-c("CC","participation")
tandem_participation_ini<-as.data.frame(tandem_participation_ini)

#coalition names are the party ids from the language file, these are paired with their colors (same order)
coalition_names<-c("FPP","INITIATIVE","LEFT","APEU","PES","DiEM25","GREEN","EGP","EFA","EUPP","VOLT","ALDE","RENEW","EDP","EPP","ECPM","ECR","PfE","ID","ESN","inds","none","tech")
coalition_colors<-c("seagreen","red4","red3","lawngreen","red","orangered","forestgreen","green3","purple4","black","purple","gold","skyblue","orange","mediumblue","turquoise","midnightblue","peru","peru","saddlebrown","white","grey","grey")


#############
ui <- navbarPage("",
                 windowTitle = "Partial Tandem",
                 theme = bs_theme(bootswatch = "flatly"),#base_font = font_collection(font_scale = .5)), #bs_theme(base_font = font_collection("system-ui", "-apple-system", "Segoe UI", font_google("Roboto"), "Helvetica Neue",  font_google("Noto Sans"), "Liberation Sans", "Arial", "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol", font_google("Noto Color Emoji")), font_scale = 0.5),
                 tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/flag-icon-css/3.5.0/css/flag-icon.min.css")),
                 
                 tabPanel( 
                   #main panel
                   textOutput("main0"),
                   # Application title
                   titlePanel(textOutput("title0")),
                   #language support
                   p(textOutput("t0")),
                   #short explanation
                   h5(textOutput("title0.0")),
                   p(textOutput("t0.1")),
                   #switch between 2019 and 2024
                   h5(textOutput("title0.1")),
                   p(textOutput("t1")),
                   #reset to initial values
                   actionButton("reset_2019", textOutput("b1")),
                   actionButton("reset_2024", textOutput("b2")),
                   p(textOutput("t2")),
                   
                   #accordion with all settings
                   accordion(
                     #main settings for the upper apportionment
                     accordion_panel(
                       strong(textOutput("a1title")),
                       p(textOutput("a1t1")),
                       #seat distribution method
                       selectInput(inputId = "eu_method",
                                   label =  NULL,
                                   choices = c("d'Hondt","Sainte-Lague"),
                                   selected  =  "d'Hondt",
                                   width = "100%"),
                       hr(),
                       #technical list
                       p(textOutput("a1t2")),
                       checkboxInput(inputId = "use_technical_list",
                                     label = textOutput("a1t3",inline = TRUE),
                                     value = TRUE,
                                     width = "100%"),
                       hr(),
                       #electoral diversity (= vote share of biggest national party inside a list coalitions)
                       p(textOutput("a1t4")),
                       sliderInput(inputId = "numdiv",
                                   label = NULL,
                                   min = 30,
                                   max = 90,
                                   step = 5,
                                   value = 70,
                                   width = "100%"),
                       p(textOutput("a1t5")),
                       value = "accp1"
                     ),
                     accordion_panel(
                       #threshold settings
                       strong(textOutput("a2title")),
                       p(textOutput("a2t1")),
                       #introduction of European threshold
                       selectizeInput(inputId = "quorum_type",
                                      label = NULL,
                                      choices = NULL,
                                      width = "100%"
                       ),
                       #size of a European quorum
                       p(textOutput("a2t2")),
                       numericInput(inputId =  "eu_quorum",
                                    label = NULL,
                                    value =  .01,
                                    min = 0,
                                    step = .01,
                                    width = "100%"),
                       #threshold explanations 
                       strong(textOutput("a2t3")),
                       p(textOutput("a2t4")),
                       p(textOutput("a2t5")),
                       p(textOutput("a2t6")),
                       p(textOutput("a2t7")),
                       p(textOutput("a2t8")),
                       hr(),
                       #respect 5%-Maximum
                       strong(textOutput("a2t9")),
                       p(textOutput("a2t10")),
                       selectizeInput(inputId = "respect5",
                                      label = NULL,
                                      choices = NULL,
                                      width = "100%"),
                       value = "accp2"
                     ),
                     accordion_panel(
                       #tandem participation
                       strong(textOutput("a3title")),
                       p(textOutput("a3t1")),
                       checkboxInput(inputId = "all",
                                     label = textOutput("a3t2",inline = TRUE),
                                     value = FALSE,
                                     width = "100%"),
                       DTOutput("tandem_participation"),
                       value = "accp3"
                     ),
                     accordion_panel(
                       #national laws, thresholds, seats
                       strong(textOutput("a4title")),
                       p(textOutput("a4t1")),
                       DTOutput("laws_matrix"),
                       p(textOutput("a4t2")),
                       value = "accp4"
                     ),
                     accordion_panel(
                       strong(textOutput("a5title")),
                       p(textOutput("a5t1")),
                       p(textOutput("a5t2")),
                       #decision on the Green coalition
                       selectizeInput("GC",
                                      label = NULL,
                                      choices = NULL,
                                      width = "100%"),
                       p(),
                       #list of national parties, their affiliation and votes
                       DTOutput("votes_list"),
                       value = "accp5"
                     ),
                     accordion_panel(
                       #transnational lists
                       strong(textOutput("a6title")),
                       p(textOutput("a6t1")),
                       p(textOutput("a6t2")),
                       selectizeInput(inputId = "transversion",
                                      label = NULL,
                                      choices = NULL,
                                      width = "100%"),
                       hr(),
                       #without pan-European constituency
                       strong(textOutput("a6t3")),
                       p(textOutput("a6t4")),
                       p(textOutput("a6t5")),
                       #options with req to change text output
                       numericInput(inputId = "transseats1",
                                    label = NULL,
                                    min = 1,
                                    step = 1,
                                    value = 5,
                                    width = "100%"),
                       checkboxInput(inputId = "transoption1",
                                     label = textOutput("a6t6",inline = TRUE),
                                     value = FALSE,
                                     width = "100%"),
                       p(textOutput("a6t7")),
                       hr(),
                       #with pan-European constituency
                       strong(textOutput("a6t8")),
                       p(textOutput("a6t9")),
                       p(textOutput("a6t10")),
                       #options with req to change text output
                       numericInput(inputId = "transseats2",
                                    label = NULL,
                                    min = 1,
                                    step = 1,
                                    value = 28,
                                    width = "100%"),
                       checkboxInput(inputId = "transoption2",
                                     label = textOutput("a6t11",inline = TRUE),
                                     value = FALSE,
                                     width = "100%"),
                       p(textOutput("a6t12")),
                       value = "accp6"),
                     accordion_panel(
                       #special scenarios
                       strong(textOutput("a7title")),
                       p(textOutput("a7t1")),
                       hr(),
                       #no right wing parties
                       strong(textOutput("a7o1t")),
                       p(textOutput("a7o1t1")),
                       actionButton("nofarright", textOutput("a7o1t2")),
                       hr(),
                       #full tandem
                       strong(textOutput("a7o2t")),
                       p(textOutput("a7o2t1")),
                       actionButton("Pukelsheim", textOutput("a7o2t2")),
                       hr(),
                       #EP2022
                       strong(textOutput("a7o3t")),
                       p(textOutput("a7o3t1")),
                       actionButton("EP2022", textOutput("a7o3t2")),
                       hr(),
                       #Müller 2021
                       strong(textOutput("a7o4t")),
                       p(textOutput("a7o4t1")),
                       actionButton("Mueller", textOutput("a7o4t2")),
                       value = "accp7")
                   ),
                   
                   #output
                   p(),
                   h3(textOutput("res1"), align = "center"),
                   p(
                     plotOutput("Europlot",inline = F,width = "100%"),
                     align = "center"
                   ),
                   uiOutput("results"),
                   #accordion(id="results",accordion_panel("test"))
                   #tableOutput("biproporz_partial_result")
                   
                   value = "tab1"
                 ),
                 tabPanel(
                   #references tab
                   textOutput("ref0"),
                   titlePanel(textOutput("reftitle")),
                   p(textOutput("ref1")),
                   h3(textOutput("ref2")),
                   p(textOutput("ref3")), 
                   p(class="hangingindent","Müller, M. (2021). Making the Most of Transnational Lists - Electoral Equality at EU Level Through Proportional Compensation. FES Policy Paper: https://brussels.fes.de/e/new-fes-policy-paper-making-the-most-of-transnational-lists.html"),
                   p(class="hangingindent","Leinen, J., & Pukelsheim, F. (2021). The tandem system: A new electoral frame for the European Parliament. Zeitschrift für Parteienwissenschaften, (2), 115-124."),
                   p(textOutput("ref3a")),
                   h3(textOutput("ref4")),
                   p(class="hangingindent","Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2022). _shiny: Web Application Framework for R_. R package version 1.7.4, <https://CRAN.R-project.org/package=shiny>"),
                   p(class="hangingindent","Poletti F (2023). _proporz: Proportional Apportionment_. R package version 1.2."),
                   p(class="hangingindent","R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/."),
                   p(class="hangingindent","Schloerke B, Chang W, Stagg G, Aden-Buie G (2024). _shinylive: Run 'shiny' Applications in the Browser_. R package version 0.2.0, https://CRAN.R-project.org/package=shinylive."),             
                   
                   p(class="hangingindent","Sievert C, Cheng J, Aden-Buie G (2023). _bslib: Custom  'Bootstrap' 'Sass' Themes for 'shiny' and 'rmarkdown'_. R package version 0.5.0, https://CRAN.R-project.org/package=bslib"),
                   p(class="hangingindent","Xie Y, Cheng J, Tan X (2024). _DT: A Wrapper of the JavaScript Library 'DataTables'_. R package version 0.33, https://CRAN.R-project.org/package=DT"),
                   
                   h3(textOutput("translatorThankYou")),
                   p(htmlOutput("translators")),
                   
                   h3(textOutput("ref5")),
                   p(textOutput("ref6")),
                   a(href="https://choffmann.eu","choffmann.eu"),
                   value = "tab2"
                   
                 ),
                 
                 nav_spacer(),
                 tabPanel(
                   selectizeInput(
                     inputId = "lang",
                     label =  NULL,
                     choices = c("English" =1,
                                 "Deutsch" =2,
                                 "Français"=3,
                                 "Italiano"=4,
                                 "Español" =5),
                     width = "110px"
                   ),
                   p("The page will update after selecting another language."),
                   value = "tab4"
                 ),
                 id="allsites",
                 footer = p()#some space at the bottom
)
#############

server <- function(input, output,session=session) {
  #save vales to restore settings if language is changed
  saved<-reactiveVal("EUT")
  saved2<-reactiveVal("tog")
  saved3<-reactiveVal("yes")
  saved_trans<-reactiveVal("no")
  
  #data
  tandem_participation<-reactiveVal(as.data.frame(tandem_participation_ini))
  laws_matrix<-reactiveVal(as.data.frame(law.matrix24))
  votes_list<-reactiveVal(as.data.frame(Votes2024[,-7]))
  year<-reactiveVal(2024)
  
  # diagram title function with following outputs
  # p - partial tandem (initial value)
  # f - full tandem
  # e - EP proposal 
  # m - Müller
  title.fun<-function(x,year){
    lang<-as.numeric(input$lang)
    switch(x,
           p = paste0(localisation$res1a[lang],year,collapse = " "),
           f = paste0(localisation$res1b[lang],year,collapse = " "),
           e = paste0(localisation$res1c[lang],year,collapse = " "),
           m = paste0(localisation$res1d[lang],year,collapse = " "))
  }
  dia.title<-reactiveVal("p")
  
  #change of language
  observeEvent(input$lang,{
    #get language
    lang<-as.numeric(input$lang)
    #switch row in localisation
    lapply(1:ncol(localisation),function(i){
      output[[colnames(localisation)[i]]]<-renderText({
        localisation[lang,i]
      })
    })
    #update diagram title
    output$res1<-renderText({title.fun(dia.title(),year())})
    #update select inputs
    updateSelectizeInput(session,"quorum_type",
                         choices = {choi<-list("EUT","NAT","ENT","NOT")
                         names(choi)<-c(localisation$a2s1[lang],
                                        localisation$a2s2[lang],
                                        localisation$a2s3[lang],
                                        localisation$a2s4[lang])
                         choi},
                         selected = saved())
    
    updateSelectizeInput(session,"GC",
                         choices = {choi<-list("tog","sep")
                         names(choi)<-c(localisation$a5s1[lang],
                                        localisation$a5s2[lang])
                         choi},
                         selected = saved2())
    
    updateSelectizeInput(session,"respect5",
                         choices = {choi<-list("yes","no")
                         names(choi)<-c(localisation$yes[lang],
                                        localisation$no[lang])
                         choi},
                         selected = saved3())
    
    updateSelectizeInput(session,"transversion",
                         choices = {choi<-list("no","without","with")
                         names(choi)<-c(localisation$a6s1[lang],
                                        localisation$a6s2[lang],
                                        localisation$a6s3[lang])
                         choi},
                         selected = saved_trans())
    
    #yes & no translation
    edited_data <- tandem_participation()
    for (i in 1:nrow(edited_data)) {
      if (edited_data$participation[i] %in% localisation$no){
        edited_data$participation[i]<-localisation$no[lang]
      }else{
        edited_data$participation[i]<-localisation$yes[lang]
      }
    }
    tandem_participation(edited_data)
    #list coalitions translation
    edited_data <- votes_list()
    party.names<-rbind(localisation$FPP,
                       localisation$INITIATIVE,
                       localisation$LEFT,
                       localisation$APEU,
                       localisation$PES,
                       localisation$DiEM25,
                       localisation$GREEN,
                       localisation$EGP,
                       localisation$EFA,
                       localisation$EUPP,
                       localisation$VOLT,
                       localisation$ALDE,
                       localisation$RENEW,
                       localisation$EDP,
                       localisation$EPP,
                       localisation$ECPM,
                       localisation$ECR,
                       localisation$PfE,
                       localisation$ID,
                       localisation$ESN,
                       localisation$inds,
                       localisation$none,
                       localisation$coal,
                       localisation$noalt)
    for (i in 1:nrow(edited_data)) {
      #edited_data<-Votes2024
      rr<-which(edited_data$European.list.coalition[i]==party.names,arr.ind = TRUE)[1]
      edited_data$European.list.coalition[i]<-party.names[rr,lang]
      rr<-which(edited_data$alternative[i]==party.names,arr.ind = TRUE)[1]
      edited_data$alternative[i]<-party.names[rr,lang]      
    }
    votes_list(edited_data)
    #go to main page
    updateNavbarPage(session,"allsites","tab1")
  })
  #translator names
  output$translators<-renderUI({HTML(paste(localisation$translatorNames[3], collapse = '<br/>'))})
  
  #update of saved values
  observeEvent(input$quorum_type,{
    saved(input$quorum_type)
  })
  observeEvent(input$GC,{
    saved2(input$GC)
  })
  observeEvent(input$respect5,{
    saved3(input$respect5)
  })
  observeEvent(input$transversion,{
    saved_trans(input$transversion)
  })
  
  #define editable data.frame inputs
  output$tandem_participation <- renderDT({
    req(!input$all)
    lang<-as.numeric(input$lang)
    rnames<-c(localisation$AT[lang],
              localisation$BENED[lang],
              localisation$BEFRA[lang],
              localisation$BEDEU[lang],
              localisation$BG[lang],
              localisation$HR[lang],
              localisation$CY[lang],
              localisation$CZ[lang],
              localisation$DK[lang],
              localisation$EE[lang],
              localisation$FI[lang],
              localisation$FR[lang],
              localisation$DE[lang],
              localisation$EL[lang],
              localisation$HU[lang],
              localisation$IE[lang],
              localisation$IT[lang],
              localisation$LV[lang],
              localisation$LT[lang],
              localisation$LU[lang],
              localisation$MT[lang],
              localisation$NL[lang],
              localisation$PL[lang],
              localisation$PT[lang],
              localisation$RO[lang],
              localisation$SK[lang],
              localisation$SI[lang],
              localisation$ES[lang],
              localisation$SE[lang])
    cnames<-c(localisation$col1[lang],localisation$col2[lang])
    datatable(tandem_participation(),
              options=list(language = list(
                lengthMenu = localisation$com1[lang],
                info = localisation$com2[lang],
                `search` = localisation$com3[lang],
                paginate = list(
                  previous = localisation$com4[lang],
                  `next` = localisation$com5[lang]
                )
              )),
              rownames = rnames,
              colnames = cnames,
              editable = TRUE)})
  output$laws_matrix <- renderDT({
    lang<-as.numeric(input$lang)
    rnames<-c(localisation$AT[lang],
              localisation$BENED[lang],
              localisation$BEFRA[lang],
              localisation$BEDEU[lang],
              localisation$BG[lang],
              localisation$HR[lang],
              localisation$CY[lang],
              localisation$CZ[lang],
              localisation$DK[lang],
              localisation$EE[lang],
              localisation$FI[lang],
              localisation$FR[lang],
              localisation$DE[lang],
              localisation$EL[lang],
              localisation$HU[lang],
              localisation$IE[lang],
              localisation$IT[lang],
              localisation$LV[lang],
              localisation$LT[lang],
              localisation$LU[lang],
              localisation$MT[lang],
              localisation$NL[lang],
              localisation$PL[lang],
              localisation$PT[lang],
              localisation$RO[lang],
              localisation$SK[lang],
              localisation$SI[lang],
              localisation$ES[lang],
              localisation$SE[lang])
    cnames<-c(localisation$col1[lang],
              localisation$col3[lang],
              localisation$col4[lang],
              localisation$col5[lang])
    datatable(laws_matrix(),
              options=list(language = list(
                lengthMenu = localisation$com1[lang],
                info = localisation$com2[lang],
                `search` = localisation$com3[lang],
                paginate = list(
                  previous = localisation$com4[lang],
                  `next` = localisation$com5[lang]
                )
              )),
              rownames = rnames,
              colnames = cnames,
              editable = TRUE)})
  output$votes_list <- renderDT({
    lang<-as.numeric(input$lang)
    cnames<-c(localisation$col1[lang],
              localisation$col6[lang],
              localisation$col7[lang],
              localisation$col8[lang],
              localisation$col9[lang],
              localisation$col10[lang])
    datatable(votes_list(),options=list(language = list(
      lengthMenu = localisation$com1[lang],
      info = localisation$com2[lang],
      `search` = localisation$com3[lang],
      paginate = list(
        previous = localisation$com4[lang],
        `next` = localisation$com5[lang]
      )
    )),colnames = cnames,escape = -c(4), editable = FALSE,rownames = FALSE)})
  
  #changes in editable data.frames
  observeEvent(input$tandem_participation_cell_edit, {
    #update data.frame
    info <- input$tandem_participation_cell_edit
    edited_data <- tandem_participation()
    edited_data[info$row, info$col] <- info$value
    tandem_participation(edited_data)
    #change diagram title
    if (all(edited_data$participation%in%localisation$yes|input$all)) {
      dia.title("f")
      output$res1<-renderText({title.fun(dia.title(),year())})
    }else{
      dia.title("p")
      output$res1<-renderText({title.fun(dia.title(),year())})
    }
  })
  observeEvent(input$all,{
    #change diagram title
    tandem_partici<-tandem_participation()
    if(input$all){
      dia.title("f")
      output$res1<-renderText({title.fun(dia.title(),year())})
    }else if (any(tandem_partici$participation%in%localisation$yes)){
      dia.title("p")
      output$res1<-renderText({title.fun(dia.title(),year())})
    }
  })
  observeEvent(input$laws_matrix_cell_edit, {
    #update data.frame
    info <- input$laws_matrix_cell_edit
    edited_data <- laws_matrix()
    edited_data[info$row, info$col] <- info$value
    laws_matrix(edited_data)
  })
  observeEvent(input$votes_list_cell_edit, {
    #update data.frame
    info <- input$votes_list_cell_edit
    edited_data <- votes_list()
    edited_data[info$row, info$col+1] <- info$value
    votes_list(edited_data)
  })
  
  #define reactive to calculate tandem result
  tandem_result<-reactive({
    partial_tandem(election.year = year(),
                   votes_list = votes_list(),
                   threshold_type = input$quorum_type,
                   european_quorum = ifelse(input$quorum_type=="EUT"|input$quorum_type=="ENT",as.numeric(input$eu_quorum),0),
                   tandem_method = input$eu_method,
                   participation_all = input$all,
                   tandem_participation = tandem_participation(),
                   fivepercentrule = ifelse(input$respect5=="yes",TRUE,FALSE),
                   min_list_div = input$numdiv/100,
                   national_laws_matrix = laws_matrix(),
                   use_technical_list = input$use_technical_list,
                   transversion = input$transversion,
                   transseats1 = input$transseats1,
                   transoption1 = input$transoption1,
                   transseats2 = input$transseats2,
                   transoption2 = input$transoption2,
                   language = as.numeric(input$lang)
    )
  })
  
  #output accordion by European parties
  output$results <- renderUI({
    #aggregate result by European parties
    votes_list<-as.data.frame(tandem_result())
    results.tab<-aggregate(tandem_seats~European.list.coalition,data = votes_list,FUN=sum)
    results.tab<-results.tab[(which(results.tab$tandem_seats!=0)),]
    results.tab<-results.tab[order(results.tab$tandem_seats, decreasing = TRUE),]
    results.tab$European.list.coalition.long<-NA
    #get long name
    lang<-as.numeric(input$lang)
    for (i in 1:nrow(results.tab)) {
      nameofparty<-paste(colnames(localisation)[which(results.tab$European.list.coalition[i]==localisation[lang,])[1]],"l",sep = "")
      results.tab$European.list.coalition.long[i]<-localisation[lang,which(colnames(localisation)==nameofparty)]
    }
    #create accordion panels
    panel_list <- lapply(1:nrow(results.tab), function(i){
      #find transnational list
      if (any(votes_list$European.list.coalition==results.tab$European.list.coalition[i]&votes_list$CC=="EU"&votes_list$tandem_seats>0)) {
        reu<-which(votes_list$European.list.coalition==results.tab$European.list.coalition[i]&votes_list$CC=="EU"&votes_list$tandem_seats>0)
      }else{
        reu<-c()
      }
      #find member parties with more than one seat
      rr<-which(votes_list$European.list.coalition==results.tab$European.list.coalition[i]&votes_list$tandem_seats>0&votes_list$CC!="EU")
      #order by number of seats
      rr<-rr[order(votes_list$tandem_seats[rr],decreasing = TRUE)]
      #combine
      if (length(reu)>0) {
        rr<-c(reu,rr)
      }
      panel_content<- paste(
        paste(
          sprintf("%s - %s - %d %s",votes_list$CC[rr],votes_list$national.party[rr],votes_list$tandem_seats[rr],ifelse(votes_list$tandem_seats[rr]>1,localisation$res3[lang],localisation$res4[lang])),
          collapse = "<br>"
        )
        ,sep = "<br>"
      )
      accordion_panel(
        title = paste(results.tab$European.list.coalition.long[i]," - ",results.tab$tandem_seats[i],ifelse(results.tab$tandem_seats[i]>1,localisation$res3[lang],localisation$res4[lang]) ),
        #paste(votes_list$national.party[rr], " - ",votes_list$tandem_seats[rr],"seats",collapse = " \n "),
        HTML(panel_content),
        value= paste0("panel",i)
      )
    })
    #create accordion with panels
    accordion(id = "results_acc", !!!panel_list, open = FALSE)
  })
  
  #create plot
  output$Europlot<-renderPlot({
    #create data table
    votes_list<-as.data.frame(tandem_result())
    results.tab<-aggregate(tandem_seats~European.list.coalition,data = votes_list,FUN=sum)
    results.tab<-results.tab[(which(results.tab$tandem_seats!=0)),]
    
    lang<-as.numeric(input$lang)
    begin<-which(colnames(localisation)==coalition_names[1])
    end<-which(colnames(localisation)==coalition_names[23])
    results.tab<-results.tab[na.omit(match(c(localisation[lang,c(begin:end)],setdiff(results.tab$European.list.coalition,localisation[lang,c(begin:end)])),results.tab$European.list.coalition)),]
    results.tab$European.list.coalition<-factor(results.tab$European.list.coalition,levels = c(localisation[lang,c(begin:end)],setdiff(results.tab$European.list.coalition,localisation[lang,c(begin:end)])))
    results.tab$start<-cumsum(c(0, results.tab$tandem_seats[-nrow(results.tab)]) / sum(results.tab$tandem_seats))
    results.tab$end<-cumsum(results.tab$tandem_seats / sum(results.tab$tandem_seats))
    results.tab$color<-coalition_colors[match(results.tab$European.list.coalition,localisation[lang,c(begin:end)])]
    results.tab$color[which(is.na(results.tab$color))]<-"grey"
    
    par(mar=c(0,0,0,0))
    plot(1, type = "n", xlim = c(-1.6, 1.6), ylim = c(-0.7, 1.6), asp = 1, axes = FALSE, xlab = "", ylab = "")
    
    # Function to draw arc segments
    draw_arc <- function(x0, y0, r0, r, start, end, col) {
      theta <- seq(start, end, length.out = 100)
      x_outer <- x0 + r * sin(theta)
      y_outer <- y0 + r * cos(theta)
      x_inner <- x0 + r0 * sin(rev(theta))
      y_inner <- y0 + r0 * cos(rev(theta))
      polygon(c(x_outer, x_inner), c(y_outer, y_inner), col = col, border = NA)
    }
    
    # Draw arcs based on the data
    for (i in 1:nrow(results.tab)) {
      start <- results.tab$start[i] * pi - 0.5 * pi
      end <- results.tab$end[i] * pi - 0.5 * pi
      draw_arc(0, 0, 0.5, 1.6, start, end, results.tab$color[i])
    }
    # Add legend
    num.parties<-nrow(results.tab)
    num.cols<-ceiling(num.parties/3)
    num.rows<-3
    if (num.cols>4) {
      par(mar=c(0,0,0,0),cex=.85)
      num.cols<-ceiling(num.parties/4)
      num.rows<-4
    }
    for (i in 1:num.cols) {
      legend(x=-1.6+(i-1)*3.2/num.cols,y=-.1, legend = results.tab$European.list.coalition[(1+(num.rows*i)-num.rows):min(num.rows+(num.rows*i)-num.rows,num.parties)], fill = results.tab$color[(1+(num.rows*i)-num.rows):min(num.rows+(num.rows*i)-num.rows,num.parties)], bty = "n", cex = 1.2,horiz = F,plot = T,y.intersp = 1.5,x.intersp = .5)
    }
  },height="auto",width="auto")
  
  #reset settings to election 2019
  observeEvent(input$reset_2019,{
    updateNumericInput(session,"eu_quorum", value =  .01)
    updateSelectInput(session,"eu_method", selected =  "d'Hondt")
    updateCheckboxInput(session,"use_technical_list",value = TRUE)
    updateSliderInput(session,"numdiv", value = 70)
    updateCheckboxInput(session,"all", value = FALSE)
    tandem_participation(as.data.frame(tandem_participation_ini))
    laws_matrix(as.data.frame(law.matrix19))
    votes_list(as.data.frame(Votes2019[,-7]))
    year(2019)
    lang<-as.numeric(input$lang)
    updateSelectizeInput(session,"quorum_type",
                         choices = {choi<-list("EUT","NAT","ENT","NOT")
                         names(choi)<-c(localisation$a2s1[lang],
                                        localisation$a2s2[lang],
                                        localisation$a2s3[lang],
                                        localisation$a2s4[lang])
                         choi},
                         selected = "EUT")
    
    updateSelectizeInput(session,"GC",
                         choices = {choi<-list("tog","sep")
                         names(choi)<-c(localisation$a5s1[lang],
                                        localisation$a5s2[lang])
                         choi},
                         selected = "tog")
    
    updateSelectizeInput(session,"respect5",
                         choices = {choi<-list("yes","no")
                         names(choi)<-c(localisation$yes[lang],
                                        localisation$no[lang])
                         choi},
                         selected = "yes")
    
    updateNumericInput(session,"transseats1",value = 5)
    updateNumericInput(session,"transseats2",value = 28)
    updateCheckboxInput(session,"transoption1",value = TRUE)
    updateCheckboxInput(session,"transoption2",value = TRUE)
    updateSelectizeInput(session,"transversion",
                         choices = {choi<-list("no","without","with")
                         names(choi)<-c(localisation$a6s1[lang],
                                        localisation$a6s2[lang],
                                        localisation$a6s3[lang])
                         choi},
                         selected = "no")
    
    #yes & no translation
    edited_data <- tandem_participation()
    for (i in 1:nrow(edited_data)) {
      if (edited_data$participation[i] %in% localisation$no){
        edited_data$participation[i]<-localisation$no[lang]
      }else{
        edited_data$participation[i]<-localisation$yes[lang]
      }
    }
    tandem_participation(edited_data)
    #list coalitions translation
    edited_data <- votes_list()
    party.names<-rbind(localisation$FPP,
                       localisation$INITIATIVE,
                       localisation$LEFT,
                       localisation$APEU,
                       localisation$PES,
                       localisation$DiEM25,
                       localisation$GREEN,
                       localisation$EGP,
                       localisation$EFA,
                       localisation$EUPP,
                       localisation$VOLT,
                       localisation$ALDE,
                       localisation$RENEW,
                       localisation$EDP,
                       localisation$EPP,
                       localisation$ECPM,
                       localisation$ECR,
                       localisation$PfE,
                       localisation$ID,
                       localisation$ESN,
                       localisation$inds,
                       localisation$none,
                       localisation$coal,
                       localisation$noalt)
    for (i in 1:nrow(edited_data)) {
      #edited_data<-Votes2024
      rr<-which(edited_data$European.list.coalition[i]==party.names,arr.ind = TRUE)[1]
      edited_data$European.list.coalition[i]<-party.names[rr,lang]
      rr<-which(edited_data$alternative[i]==party.names,arr.ind = TRUE)[1]
      edited_data$alternative[i]<-party.names[rr,lang]      
    }
    votes_list(edited_data)
    dia.title("p")
    output$res1<-renderText({title.fun(dia.title(),year())})
  })
  #reset settings to election 2024
  observeEvent(input$reset_2024,{
    updateNumericInput(session,"eu_quorum", value =  .01)
    updateSelectInput(session,"eu_method", selected =  "d'Hondt")
    updateCheckboxInput(session,"use_technical_list",value = TRUE)
    updateSliderInput(session,"numdiv", value = 70)
    updateCheckboxInput(session,"all", value = FALSE)
    tandem_participation(as.data.frame(tandem_participation_ini))
    laws_matrix(as.data.frame(law.matrix24))
    votes_list(as.data.frame(Votes2024[,-7]))
    year(2024)
    lang<-as.numeric(input$lang)
    updateSelectizeInput(session,"quorum_type",
                         choices = {choi<-list("EUT","NAT","ENT","NOT")
                         names(choi)<-c(localisation$a2s1[lang],
                                        localisation$a2s2[lang],
                                        localisation$a2s3[lang],
                                        localisation$a2s4[lang])
                         choi},
                         selected = "EUT")
    
    updateSelectizeInput(session,"GC",
                         choices = {choi<-list("tog","sep")
                         names(choi)<-c(localisation$a5s1[lang],
                                        localisation$a5s2[lang])
                         choi},
                         selected = "tog")
    updateSelectizeInput(session,"respect5",
                         choices = {choi<-list("yes","no")
                         names(choi)<-c(localisation$yes[lang],
                                        localisation$no[lang])
                         choi},
                         selected = "yes")
    
    updateNumericInput(session,"transseats1",value = 5)
    updateNumericInput(session,"transseats2",value = 28)
    updateCheckboxInput(session,"transoption1",value = TRUE)
    updateCheckboxInput(session,"transoption2",value = TRUE)
    updateSelectizeInput(session,"transversion",
                         choices = {choi<-list("no","without","with")
                         names(choi)<-c(localisation$a6s1[lang],
                                        localisation$a6s2[lang],
                                        localisation$a6s3[lang])
                         choi},
                         selected = "no")
    
    #yes & no translation
    edited_data <- tandem_participation()
    for (i in 1:nrow(edited_data)) {
      if (edited_data$participation[i] %in% localisation$no){
        edited_data$participation[i]<-localisation$no[lang]
      }else{
        edited_data$participation[i]<-localisation$yes[lang]
      }
    }
    tandem_participation(edited_data)
    #list coalitions translation
    edited_data <- votes_list()
    party.names<-rbind(localisation$FPP,
                       localisation$INITIATIVE,
                       localisation$LEFT,
                       localisation$APEU,
                       localisation$PES,
                       localisation$DiEM25,
                       localisation$GREEN,
                       localisation$EGP,
                       localisation$EFA,
                       localisation$EUPP,
                       localisation$VOLT,
                       localisation$ALDE,
                       localisation$RENEW,
                       localisation$EDP,
                       localisation$EPP,
                       localisation$ECPM,
                       localisation$ECR,
                       localisation$PfE,
                       localisation$ID,
                       localisation$ESN,
                       localisation$inds,
                       localisation$none,
                       localisation$coal,
                       localisation$noalt)
    for (i in 1:nrow(edited_data)) {
      #edited_data<-Votes2024
      rr<-which(edited_data$European.list.coalition[i]==party.names,arr.ind = TRUE)[1]
      edited_data$European.list.coalition[i]<-party.names[rr,lang]
      rr<-which(edited_data$alternative[i]==party.names,arr.ind = TRUE)[1]
      edited_data$alternative[i]<-party.names[rr,lang]      
    }
    votes_list(edited_data)
    dia.title("p")
    output$res1<-renderText({title.fun(dia.title(),year())})
  })
  #set settings to no far right scenario
  observeEvent(input$nofarright,{
    lang<-as.numeric(input$lang)
    updateNumericInput(session,"eu_quorum", value =  .05)
    updateSelectInput(session,"eu_method", selected =  "d'Hondt")
    updateCheckboxInput(session,"use_technical_list",value = TRUE)
    updateSliderInput(session,"numdiv", value = 40)
    tandem_participation(as.data.frame(tandem_participation_ini))
    updateSelectizeInput(session,"quorum_type",
                         choices = {choi<-list("EUT","NAT","ENT","NOT")
                         names(choi)<-c(localisation$a2s1[lang],
                                        localisation$a2s2[lang],
                                        localisation$a2s3[lang],
                                        localisation$a2s4[lang])
                         choi},
                         selected = "EUT")
    
    updateSelectizeInput(session,"GC",
                         choices = {choi<-list("tog","sep")
                         names(choi)<-c(localisation$a5s1[lang],
                                        localisation$a5s2[lang])
                         choi},
                         selected = "tog")
    updateSelectizeInput(session,"respect5",
                         choices = {choi<-list("yes","no")
                         names(choi)<-c(localisation$yes[lang],
                                        localisation$no[lang])
                         choi},
                         selected = "no")
    tandem_partici<-tandem_participation()
    if(input$all|all(tandem_partici$participation%in%localisation$yes)){
      dia.title("f")
      output$res1<-renderText({title.fun(dia.title(),year())})
    }else if(!input$all&any(tandem_partici$participation%in%localisation$yes)){
      dia.title("p")
      output$res1<-renderText({title.fun(dia.title(),year())})
    }
  })
  #set settings to no full tandem scenario
  observeEvent(input$Pukelsheim,{
    lang<-as.numeric(input$lang)
    updateNumericInput(session,"eu_quorum", value =  0)
    updateSelectInput(session,"eu_method", selected =  "Sainte-Lague")
    updateCheckboxInput(session,"use_technical_list",value = FALSE)
    updateSliderInput(session,"numdiv", value = 90)
    updateCheckboxInput(session,"all", value = TRUE)
    updateSelectizeInput(session,"quorum_type",
                         choices = {choi<-list("EUT","NAT","ENT","NOT")
                         names(choi)<-c(localisation$a2s1[lang],
                                        localisation$a2s2[lang],
                                        localisation$a2s3[lang],
                                        localisation$a2s4[lang])
                         choi},
                         selected = "NOT")
    
    updateSelectizeInput(session,"GC",
                         choices = {choi<-list("tog","sep")
                         names(choi)<-c(localisation$a5s1[lang],
                                        localisation$a5s2[lang])
                         choi},
                         selected = "sep")
    updateSelectizeInput(session,"respect5",
                         choices = {choi<-list("yes","no")
                         names(choi)<-c(localisation$yes[lang],
                                        localisation$no[lang])
                         choi},
                         selected = "no")
    updateSelectizeInput(session,"transversion",
                         choices = {choi<-list("no","without","with")
                         names(choi)<-c(localisation$a6s1[lang],
                                        localisation$a6s2[lang],
                                        localisation$a6s3[lang])
                         choi},
                         selected = "no")
    dia.title("f")
    output$res1<-renderText({title.fun(dia.title(),year())})
    
  })
  #set settings to EP proposal scenario
  observeEvent(input$EP2022,{
    lang<-as.numeric(input$lang)
    updateNumericInput(session,"eu_quorum", value =  0)
    updateSelectInput(session,"eu_method", selected =  "d'Hondt")
    updateCheckboxInput(session,"use_technical_list",value = FALSE)
    updateSliderInput(session,"numdiv", value = 70)
    updateCheckboxInput(session,"all", value = FALSE)
    tandem_participation(data.frame(CC=CC,participation="no"))
    updateSelectizeInput(session,"quorum_type",
                         choices = {choi<-list("EUT","NAT","ENT","NOT")
                         names(choi)<-c(localisation$a2s1[lang],
                                        localisation$a2s2[lang],
                                        localisation$a2s3[lang],
                                        localisation$a2s4[lang])
                         choi},
                         selected = "NOT")
    
    updateSelectizeInput(session,"GC",
                         choices = {choi<-list("tog","sep")
                         names(choi)<-c(localisation$a5s1[lang],
                                        localisation$a5s2[lang])
                         choi},
                         selected = "tog")
    if (year()==2019) {
      laws_matrix(as.data.frame(ep22.law.matrix19))
    }
    if (year()==2024) {
      laws_matrix(as.data.frame(ep22.law.matrix24))
    }
    #updateNumericInput(session,"transseats1",value = 5)
    updateNumericInput(session,"transseats2",value = 28)
    #updateCheckboxInput(session,"transoption1",value = TRUE)
    updateCheckboxInput(session,"transoption2",value = FALSE)
    updateSelectizeInput(session,"transversion",
                         choices = {choi<-list("no","without","with")
                         names(choi)<-c(localisation$a6s1[lang],
                                        localisation$a6s2[lang],
                                        localisation$a6s3[lang])
                         choi},
                         selected = "with")
    dia.title("e")
    output$res1<-renderText({title.fun(dia.title(),year())})
  })
  #set settings to Müller's proposal scenario
  observeEvent(input$Mueller,{
    updateNumericInput(session,"eu_quorum", value =  0)
    updateSelectInput(session,"eu_method", selected =  "Sainte-Lague")
    updateCheckboxInput(session,"use_technical_list",value = FALSE)
    updateSliderInput(session,"numdiv", value = 70)
    updateCheckboxInput(session,"all", value = FALSE)
    tandem_participation(data.frame(CC=CC,participation="no"))
    lang<-as.numeric(input$lang)
    updateSelectizeInput(session,"quorum_type",
                         choices = {choi<-list("EUT","NAT","ENT","NOT")
                         names(choi)<-c(localisation$a2s1[lang],
                                        localisation$a2s2[lang],
                                        localisation$a2s3[lang],
                                        localisation$a2s4[lang])
                         choi},
                         selected = "NOT")
    
    updateSelectizeInput(session,"GC",
                         choices = {choi<-list("tog","sep")
                         names(choi)<-c(localisation$a5s1[lang],
                                        localisation$a5s2[lang])
                         choi},
                         selected = "tog")
    
    if (year()==2019) {
      laws_matrix(as.data.frame(mueller.law.matrix19))
    }
    if (year()==2024) {
      laws_matrix(as.data.frame(mueller.law.matrix24))
    }
    updateNumericInput(session,"transseats1",value = 5)
    updateNumericInput(session,"transseats2",value = 76)
    updateCheckboxInput(session,"transoption1",value = TRUE)
    updateCheckboxInput(session,"transoption2",value = TRUE)
    updateSelectizeInput(session,"transversion",
                         choices = {choi<-list("no","without","with")
                         names(choi)<-c(localisation$a6s1[lang],
                                        localisation$a6s2[lang],
                                        localisation$a6s3[lang])
                         choi},
                         selected = "with")
    dia.title("m")
    output$res1<-renderText({title.fun(dia.title(),year())})
  })
  
  #switch for green coalition
  observeEvent(input$GC,{
    lang<-as.numeric(input$lang)
    if (input$GC=="sep"){
      vl<-votes_list()
      rr<-which(vl$European.list.coalition==localisation$GREEN[lang])
      vl$European.list.coalition[rr]<-vl$alternative[rr]
      vl$alternative[rr]<-localisation$GREEN[lang]
    } else {#if(input$GC=="tog")
      vl<-votes_list()
      rr<-which(vl$alternative==localisation$GREEN[lang])
      vl$alternative[rr]<-vl$European.list.coalition[rr]
      vl$European.list.coalition[rr]<-localisation$GREEN[lang]
    }
    votes_list(as.data.frame(vl))
  })
  
  #####################
  # internal function #
  #####################
  
  partial_tandem<-function(election.year=2024,
                           votes_list,
                           threshold_type,
                           european_quorum,
                           tandem_method,
                           participation_all,
                           tandem_participation,
                           fivepercentrule,
                           min_list_div,
                           national_laws_matrix,
                           use_technical_list,
                           transversion,
                           transseats1,
                           transoption1,
                           transseats2,
                           transoption2,
                           language=1){
    #add undisplayed historic seat distribution
    if (election.year==2024) {
      votes_list<-cbind(votes_list,seats=Votes2024$seats)
    }else{
      votes_list<-cbind(votes_list,seats=Votes2019$seats)
    }
    
    #remove coalitions on national level
    if (!identical(which(votes_list[,2]%in%localisation$coal), integer(0))) {
      votes_list<-votes_list[-which(votes_list[,2]%in%localisation$coal),]
    }
    #to lower case for yes + translations
    tandem_participation$participation<-tolower(tandem_participation$participation)
    for (i in 1:nrow(tandem_participation)) {
      if (tandem_participation$participation[i] %in% tolower(localisation$yes)) {
        tandem_participation$participation[i]<-"Yes"
      }else{
        tandem_participation$participation[i]<-"No"
      }
    }
    #translate none and inds
    none<-localisation$none[language]
    inds<-localisation$inds[language]
    tech.list.coal<-localisation$tech[language]
    
    #overwrite participation if checkbox is selected
    if (participation_all) {
      tandem_participation$participation="Yes"
    }
    
    #calculate national thresholds in votes
    nat_thresholds<-national_laws_matrix[,c(1,3)]
    nat_thresholds<-cbind(nat_thresholds,threshold5=0,nat_natural_thresholds=0)
    for (i in 1:nrow(nat_thresholds)) {
      #temporary national vote sum
      temp<-sum(votes_list$votes[votes_list$CC==nat_thresholds$CC[i]])
      
      #if threshold is given in percent
      if (nat_thresholds[i,2]<1) {
        #
        
        #which rows belong to country i
        nat_thresholds[i,2]<-ceiling(temp*nat_thresholds[i,2])
      }
      nat_thresholds[i,3]<-ceiling(temp*0.05)
      #determine natural threshold
      country_votes<-data.frame(votes=votes_list$votes[votes_list$CC==nat_thresholds$CC[i]])
      country_votes$seats<-proporz::proporz(country_votes$votes,national_laws_matrix$seats[i],method = "d'Hondt",0)
      country_votes<-country_votes[which(country_votes$seats>0),]
      nat_thresholds[i,4]<-min(country_votes$votes)
      
      #national threshold at least at natural level to not inflate technical list
      nat_thresholds[i,2]<-max(nat_thresholds[i,2],nat_thresholds[i,4])
      #national threshold at least at natural level to not inflate technical list
      nat_thresholds[i,2]<-min(nat_thresholds[i,2],nat_thresholds[i,3])
    }
    #application of thresholds
    #add columns with values whether a threshold dismisses a party
    votes_list<-cbind(votes_list,EU.threshold=FALSE,Nat.threshold=FALSE,Fivepercent=FALSE)
    
    #replace none, inds 
    for (i in 1:nrow(votes_list)) {
      if (votes_list$European.list.coalition[i]==none|votes_list$European.list.coalition[i]==inds) {
        votes_list$European.list.coalition[i]=paste(none," - ",votes_list$national.party[i])
      }        
    }
    #find propective coalitions
    coalitions<-unique(votes_list$European.list.coalition)
    
    #check wether coalitions are diverse enough
    for (i in coalitions) {
      if (max(votes_list$votes[which(votes_list$European.list.coalition==i)])>min_list_div*sum(votes_list$votes[which(votes_list$European.list.coalition==i)])) {
        votes_list$European.list.coalition[which(votes_list$European.list.coalition==i)]<-none
      }
    }
    for (i in 1:nrow(votes_list)) {
      if (votes_list$European.list.coalition[i]==none) {
        votes_list$European.list.coalition[i]=paste(none," - ",votes_list$national.party[i])
      }        
    }    
    #find true coalitions
    coalitions<-unique(votes_list$European.list.coalition)    
    
    #determine natural European threshold
    coalitions_votes<-aggregate(votes~European.list.coalition,data=votes_list,FUN=sum)
    coalitions_votes$seats<-proporz::proporz(coalitions_votes$votes,sum(national_laws_matrix$seats),method = tandem_method,0)
    coalitions_votes<-coalitions_votes[coalitions_votes$seats>0,]
    european_natural_quorum<-coalitions_votes[which.min(coalitions_votes[,2]),2]
    
    #European threshold in votes
    if(european_quorum<1){european_quorum<-ceiling(european_quorum*sum(votes_list$votes))}
    european_quorum<-max(european_quorum,european_natural_quorum)
    
    #check whether coalitions met the European (natural) threshold 
    for (i in coalitions) {
      #calculate coalitions votes
      coal.votes<-sum(votes_list$votes[which(votes_list$European.list.coalition==i)])
      #if coalition is below quorum move to none
      if (coal.votes<european_quorum) {
        votes_list$European.list.coalition[which(votes_list$European.list.coalition==i)]<-none
      }else{
        votes_list$EU.threshold[which(votes_list$European.list.coalition==i)]<-TRUE
      }
    }
    for (i in 1:nrow(votes_list)) {
      if (votes_list$European.list.coalition[i]==none) {
        votes_list$European.list.coalition[i]=paste(none," - ",votes_list$national.party[i])
      }        
    }      
    
    for (i in nrow(votes_list):1) {
      #check whether national parties met national threshold
      if(votes_list$votes[i]>nat_thresholds$threshold[which(nat_thresholds$CC==votes_list$CC[i])]){
        votes_list$Nat.threshold[i]<-TRUE
      }
      
      #check whether national party got more than 5%
      if (fivepercentrule) {
        if(votes_list$votes[i]>nat_thresholds$threshold5[which(nat_thresholds$CC==votes_list$CC[i])]){
          votes_list$Fivepercent[i]<-TRUE
        }
      }
    }
    
    #add seats column
    votes_list<-cbind(votes_list,tandem_seats=0)
    
    #transnational lists with pan-European constituency
    if (transversion=="with") {
      translist<-localisation$translist[language]
      #which votes should count for European result
      if (threshold_type=="EUT") {
        rr<-which(votes_list$EU.threshold)
      }else if (threshold_type=="ENT"){
        rr<-which(votes_list$EU.threshold|votes_list$Nat.threshold)
      }else if (threshold_type=="NAT") {
        rr<-which(votes_list$Nat.threshold)
      }else{
        rr<-which(votes_list$EU.threshold)
      }
      if (fivepercentrule) {
        rr<-sort(union(rr,which(votes_list$Fivepercent)))
      }
      
      if (transoption2) {
        #distribute seats as if all countries have opt-out
        for (i in tandem_participation$CC) {
          #which rows belong to the state
          rr2<-which(votes_list$CC==i)#&votes_list$Nat.threshold)
          #check whether number of seats has changed for a country
          if (sum(votes_list$seats[rr2])==national_laws_matrix$seats[which(national_laws_matrix$CC==i)]) {
            #copy seats from real result
            votes_list$tandem_seats[rr2]<-votes_list$seats[rr2]
          }else{
            #remove below threshold
            rr2<-which(votes_list$CC==i&votes_list$Nat.threshold)
            #check if STV or Quota
            if (national_laws_matrix$method[which(national_laws_matrix$CC==i)]=="Quota"|national_laws_matrix$method[which(national_laws_matrix$CC==i)]=="Hare-Niemeyer"|national_laws_matrix$method[which(national_laws_matrix$CC==i)]=="STV") {
              votes_list$tandem_seats[rr2]<-largest_remainder_method(votes=votes_list$votes[rr2],
                                                                     national_laws_matrix$seats[which(national_laws_matrix$CC==i)],
                                                                     quorum = 0)
              ##all other member states
            }else{
              votes_list$tandem_seats[rr2]<-proporz::proporz(votes=votes_list$votes[rr2],
                                                             national_laws_matrix$seats[which(national_laws_matrix$CC==i)],
                                                             method = national_laws_matrix$method[which(national_laws_matrix$CC==i)],
                                                             quorum = 0)
            }
          }
        }
        #determine European result with additional seats
        #aggregate by votes and seats
        EU.nat.seats<-aggregate(tandem_seats~European.list.coalition,data = votes_list,FUN=sum,subset = c(tandem_seats>0))
        EU.votes<-aggregate(votes~European.list.coalition,data = votes_list[rr,],FUN=sum)
        #combine both
        EU.votes<-merge(EU.nat.seats,EU.votes,by="European.list.coalition",all = TRUE)
        EU.votes[is.na(EU.votes)]=0
        #remove parties without list coalition + note their seats
        if (any(substr(EU.votes$European.list.coalition,1,3)==substr(none,1,3))) {
          nocoal.seats<-sum(EU.votes$tandem_seats[which(substr(EU.votes$European.list.coalition,1,3)==substr(none,1,3))])
          EU.votes<-EU.votes[-(which(substr(EU.votes$European.list.coalition,1,3)==substr(none,1,3))),]
        }else{
          nocoal.seats<-0
        }
        #determine EU result
        EU.votes$seats<-proporz::proporz(EU.votes$votes,sum(national_laws_matrix$seats)+transseats2-nocoal.seats,tandem_method,0)
        #calculate gap for transnational seats
        EU.votes$trans_seats<-EU.votes$seats-EU.votes$tandem_seats
        #if gap is negative for party -> exclude party + determine European result again
        while (any(EU.votes$trans_seats<0)) {
          nocoal.seats<-nocoal.seats+sum(EU.votes$tandem_seats[which(EU.votes$trans_seats<0)])
          EU.votes<-EU.votes[-(which(EU.votes$trans_seats<0)),]
          EU.votes$seats<-proporz::proporz(EU.votes$votes,sum(national_laws_matrix$seats)+transseats2-nocoal.seats,tandem_method,0)
          EU.votes$trans_seats<-EU.votes$seats-EU.votes$tandem_seats
        }
        #set tandem seats back to 0
        votes_list$tandem_seats=0
      }else{
        #aggregate by votes
        EU.votes<-aggregate(votes~European.list.coalition,data = votes_list[rr,],FUN=sum)
        #remove parties without trans list
        if (any(substr(EU.votes$European.list.coalition,1,3)==substr(none,1,3))) {
          EU.votes<-EU.votes[-(which(substr(EU.votes$European.list.coalition,1,3)==substr(none,1,3))),]
        }
        #determine European result
        EU.votes$trans_seats<-proporz::proporz(EU.votes$votes,transseats2,tandem_method,0)
      }
      #add to votes_list as seats not tandem_seats
      votes_list<-rbind(votes_list,data.frame(CC="EU",European.list.coalition=EU.votes$European.list.coalition,alternative="no alternative",national.party=paste0(EU.votes$European.list.coalition," ",translist),votes=EU.votes$votes,comment="",seats=EU.votes$trans_seats,EU.threshold=FALSE,Nat.threshold=FALSE,Fivepercent=FALSE,tandem_seats=EU.votes$trans_seats)) 
      #add to laws matrix
      national_laws_matrix<-rbind(national_laws_matrix,data.frame(CC="EU",seats=transseats2,threshold=0,method=tandem_method))
      #add to participation matrix
      tandem_participation<-rbind(tandem_participation,data.frame(CC="EU",participation="No"))
    }
    
    #distribution of seats in non-tandem states
    for (i in tandem_participation$CC[1:29][which(tandem_participation$participation[1:29]=="No")]) {
      #which rows belong to the state
      rr<-which(votes_list$CC==i)#&votes_list$Nat.threshold)
      #check whether number of seats, method or threshold has changed for a country
      seatcheck<-(sum(votes_list$seats[rr])==national_laws_matrix$seats[which(national_laws_matrix$CC==i)])
      methodcheck<-(national_laws_matrix$method[which(national_laws_matrix$CC==i)]==ifelse(election.year==2024,law.matrix24$method[which(national_laws_matrix$CC==i)],law.matrix19$method[which(national_laws_matrix$CC==i)]))
      thresholdcheck<-(national_laws_matrix$threshold[which(national_laws_matrix$CC==i)]==ifelse(election.year==2024,law.matrix24$threshold[which(national_laws_matrix$CC==i)],law.matrix19$threshold[which(national_laws_matrix$CC==i)]))
      if (seatcheck&methodcheck&thresholdcheck) {
        #copy seats from real result
        votes_list$tandem_seats[rr]<-votes_list$seats[rr]
      }else{
        #remove below threshold
        rr<-which(votes_list$CC==i&votes_list$Nat.threshold)
        #check if STV or Quota
        if (national_laws_matrix$method[which(national_laws_matrix$CC==i)]=="Quota"|national_laws_matrix$method[which(national_laws_matrix$CC==i)]=="Hare-Niemeyer"|national_laws_matrix$method[which(national_laws_matrix$CC==i)]=="STV") {
          votes_list$tandem_seats[rr]<-largest_remainder_method(votes=votes_list$votes[rr],
                                                                national_laws_matrix$seats[which(national_laws_matrix$CC==i)],
                                                                quorum = 0)
          ##all other member states
        }else{
          votes_list$tandem_seats[rr]<-proporz::proporz(votes=votes_list$votes[rr],
                                                        national_laws_matrix$seats[which(national_laws_matrix$CC==i)],
                                                        method = national_laws_matrix$method[which(national_laws_matrix$CC==i)],
                                                        quorum = 0)
        }
      }
    }
    #if no country participates
    if (all(tandem_participation$participation=="No")) {
      #rename to none
      for (i in 1:nrow(votes_list)){
        if (votes_list$European.list.coalition[i]==paste(none," - ",votes_list$national.party[i])) {
          votes_list$European.list.coalition[i]=none
        }
      }
      
      #return
      return(votes_list)
    }
    
    #introduce technical coalition
    if (use_technical_list) {
      for (i in 1:nrow(votes_list)) {
        if (votes_list$European.list.coalition[i]==paste(none," - ",votes_list$national.party[i])) {
          votes_list$European.list.coalition[i]<-tech.list.coal
        }
      }
    }
    
    #already distributed seats by European party family
    given_seats<-aggregate(tandem_seats~European.list.coalition,data=votes_list,FUN=sum)
    given_seats<-cbind(given_seats,hang.seats=FALSE)
    
    #add further exclusion in upper apportionment rule for edge cases (all list seats are already distributed with overhang or no further leveling is possible )
    votes_list<-cbind(votes_list,exclusion=FALSE)    
    
    #check whether EU party runs only in Opt out states
    for (i in given_seats$European.list.coalition) {
      #in which states has the EU party received votes
      statesV<-unique(votes_list$CC[which(votes_list$European.list.coalition==i)])
      statesP<-tandem_participation$CC[which(tandem_participation$participation=="No")]
      if (all(statesV%in%statesP)) {
        given_seats$hang.seats[which(given_seats$European.list.coalition==i)]=TRUE
        votes_list$exclusion[which(votes_list$European.list.coalition==i)]=TRUE
      }
    }
    
    #seats in tandemstates that fall out the lower apportionment due to an overhang which can't be leveled
    seat.adjust<-matrix(0,sum(tandem_participation$participation=="Yes"),nrow(given_seats))
    colnames(seat.adjust)<-given_seats$European.list.coalition
    rownames(seat.adjust)<-tandem_participation$CC[tandem_participation$participation=="Yes"]
    
    #tandem function with limited overhang management
    eu_tandem<-function(votes_list,threshold_type,seats,given_seats,seat.adjust){
      #which votes should count for upper apportionment
      if (threshold_type=="EUT") {
        rr<-which(votes_list$EU.threshold)
      }else if (threshold_type=="ENT"){
        rr<-which(votes_list$EU.threshold|votes_list$Nat.threshold)
      }else if (threshold_type=="NAT") {
        rr<-which(votes_list$Nat.threshold)
      }else{
        rr<-which(votes_list$EU.threshold)
      }
      if (fivepercentrule) {
        rr<-sort(union(rr,which(votes_list$Fivepercent)))
      }
      rr<-setdiff(rr,which(votes_list$exclusion))
      #set votes of cleared to zero
      votes_list$votes[-rr]<-0
      
      #determine free seats usable for the upper apportionment
      free.seats<-seats-sum(given_seats$tandem_seats[which(given_seats$hang.seats)])
      
      #aggregate European result
      upper.votes<-aggregate(votes~European.list.coalition,data = votes_list,FUN=sum)
      #determine European seat distribution
      upper.votes$eu.result<-proporz::proporz(upper.votes$votes,free.seats,method = tandem_method,0)
      
      #check for overhang
      overhang<-FALSE
      for (i in given_seats$European.list.coalition) {
        if (given_seats$tandem_seats[which(given_seats$European.list.coalition==i)]>upper.votes$eu.result[which(upper.votes$European.list.coalition==i)]&!given_seats$hang.seats[which(given_seats$European.list.coalition==i)]) {
          given_seats$hang.seats[which(given_seats$European.list.coalition==i)]=TRUE
          votes_list$exclusion[which(votes_list$European.list.coalition==i)]=TRUE
          overhang<-TRUE
        }
      }
      if (overhang) {
        #run again from beginning with adjusted seats
        res<-eu_tandem(votes_list=votes_list,threshold_type=threshold_type,seats,given_seats=given_seats,seat.adjust=seat.adjust)
        return(res)
      }
      
      #seats that must be distributed by the lower apportionment
      open.party.seats<-data.frame(upper.votes$European.list.coalition,seats=0)
      colnames(open.party.seats)<-c("European.list.coalition","seats")
      for (i in upper.votes$European.list.coalition) {
        open.party.seats$seats[which(open.party.seats$European.list.coalition==i)]<-max(upper.votes$eu.result[which(upper.votes$European.list.coalition==i)]-given_seats$tandem_seats[which(given_seats$European.list.coalition==i)],0)
      }
      #remove European.list.coalition with 0 seats
      open.party.seats<-open.party.seats[which(open.party.seats$seats>0),]
      
      open.state.seats<-data.frame(tandem_participation$CC[which(tandem_participation$participation=="Yes")],seats=0)
      colnames(open.state.seats)<-c("CC","seats")
      for (i in open.state.seats$CC) {
        open.state.seats$seats[which(open.state.seats$CC==i)]<-national_laws_matrix$seats[which(national_laws_matrix$CC==i)]-sum(seat.adjust[which(rownames(seat.adjust)==i),])
      }
      #open seats in a tandem state are reduced if a European list coalition must be fixed due to an overhang which cannot be leveled
      
      #lower apportionment
      opt_in<-tandem_participation$CC[which(tandem_participation$participation=="Yes")]
      #create states x list coalitions matrix for all included votes
      votes_matrix<-aggregate(votes~CC+European.list.coalition,data=votes_list,FUN=sum,subset = c(CC %in% opt_in)) 
      colnames(votes_matrix)<-c("CC","European.list.coalition","votes")
      #remove list coalitions without seats
      votes_matrix<-votes_matrix[which(votes_matrix$European.list.coalition %in% open.party.seats$European.list.coalition),]
      
      votes_matrix<-as.matrix(xtabs(votes~CC+European.list.coalition,votes_matrix))
      votes_matrix<-votes_matrix[,order(colSums(votes_matrix),decreasing = TRUE)]
      
      #match seat.adjust to votes_matrix
      seat.adjust<-seat.adjust[match(rownames(votes_matrix),rownames(seat.adjust)),match(colnames(votes_matrix),colnames(seat.adjust))]
      
      open.state.seats<-open.state.seats[match(rownames(votes_matrix),open.state.seats$CC),]
      open.party.seats<-open.party.seats[match(colnames(votes_matrix),open.party.seats$European.list.coalition),]
      
      #check whether lower_apportionment runs
      lower.res<-tryCatch({
        setTimeLimit(15)
        t(lower_apportionment(t(votes_matrix),open.state.seats$seats,open.party.seats$seats))
      },error = function(e){
        stop(print(localisation$error[language])) #new upper apportionment must be destermined to account for non-compensatable over- and underhangs
      })
      #return list with seats by European list coalition with no overhang/underhang and country
      res<-list(lower.res=lower.res,seat.adjust=seat.adjust)
      return(res)
    }
    
    #apply tandem function 
    tandem.result<-eu_tandem(votes_list=votes_list,threshold_type=threshold_type,seats=sum(national_laws_matrix$seats),given_seats=given_seats,seat.adjust=seat.adjust)
    #combine list coalitions with and without overhang/underhang
    seats.to.dis<-tandem.result$lower.res+tandem.result$seat.adjust
    
    #further distribution onto national parties, especially relevant, if European list coalition has multiple member parties in one member state
    for (i in 1:nrow(seats.to.dis)) {
      for (j in 1:ncol(seats.to.dis)) {
        if (seats.to.dis[i,j]>0) {
          #find national parties above the respective thresholds in one country
          if (threshold_type=="EUT") {
            rr<-which(votes_list$European.list.coalition==colnames(seats.to.dis)[j]&
                        votes_list$CC==rownames(seats.to.dis)[i]&
                        votes_list$EU.threshold)
          }else if (threshold_type=="ENT"){
            rr<-which(votes_list$European.list.coalition==colnames(seats.to.dis)[j]&
                        votes_list$CC==rownames(seats.to.dis)[i]&
                        (votes_list$EU.threshold|votes_list$Nat.threshold))
          }else if (threshold_type=="NAT") {
            rr<-which(votes_list$European.list.coalition==colnames(seats.to.dis)[j]&
                        votes_list$CC==rownames(seats.to.dis)[i]&
                        votes_list$Nat.threshold)
          }else{ #natural European threshold always applied
            rr<-which(votes_list$European.list.coalition==colnames(seats.to.dis)[j]&
                        votes_list$CC==rownames(seats.to.dis)[i]&
                        votes_list$EU.threshold)
          }
          if (fivepercentrule) {
            rr<-sort(union(rr,which(votes_list$European.list.coalition==colnames(seats.to.dis)[j]&
                                      votes_list$CC==rownames(seats.to.dis)[i]&
                                      votes_list$Fivepercent)))
          }
          #distribute seats to national parties
          votes_list$tandem_seats[rr]<-divisor_round(votes_list$votes[rr],seats.to.dis[i,j])
        }
      }
    }
    
    #rename to none
    for (i in 1:nrow(votes_list)){
      if (votes_list$European.list.coalition[i]==paste(none," - ",votes_list$national.party[i])) {
        votes_list$European.list.coalition[i]=none
      }
    }
    
    
    #transnational lists
    if (transversion=="without") {
      #opt-in countries of trans lists
      if (transoption1){opt_in<-tandem_participation$CC}else{
        opt_in<-tandem_participation$CC[which(tandem_participation$participation=="Yes")]
      } 
      
      #translations
      translist<-localisation$translist[language]
      GREEN<-localisation$GREEN[language]
      EGP<-localisation$EGP[language]
      EFA<-localisation$EFA[language]
      EUPP<-localisation$EUPP[language]
      VOLT<-localisation$VOLT[language]
      DiEM25<-localisation$DiEM25[language]
      #non-implemented option to split Renew
      # RENEW<-localisation$RENEW[language]
      # ALDE<-localisation$ALDE[language]
      # EDP<-localisation$EDP[language]
      
      #going through list coalitions
      for (i in unique(votes_list$European.list.coalition)){
        #ignore none and technical
        if (i==none) next
        if (i==tech.list.coal) next
        #non-implemented option to split Renew
        #split RENEW
        # if (i==RENEW){
        #   for (j in c(ALDE,EDP)) {
        #     rr<-which(votes_list$CC %in% opt_in & votes_list$European.list.coalition==i & votes_list$alternative==j)
        #     if (sum(votes_list$tandem_seats[rr])<transseats1) {
        #       votes_list$tandem_seats[rr]<-0
        #       votes_list<-rbind(votes_list,c("EU",i,j,paste0(j," ",translist,collapse = " "),0,"",TRUE,TRUE,TRUE,sum(votes_list$tandem_seats),TRUE))
        #     }else{
        #       votes_list$tandem_seats[rr]<-votes_list$tandem_seats[rr]-divisor_round(votes_list$tandem_seats[rr],transseats1)
        #       votes_list<-rbind(votes_list,c("EU",i,j,paste0(j," ",translist,collapse = " "),0,"",TRUE,TRUE,TRUE,transseats1,TRUE))
        #     }
        #   }
        #   next
        # }
        #split GREEN if existent
        if (i==GREEN) {
          for (j in c(EGP,EFA,EUPP,VOLT,DiEM25)) {
            #find national parties
            rr<-which(votes_list$CC %in% opt_in & votes_list$European.list.coalition==i & votes_list$alternative==j)
            #how many seats these parties have
            tandem_seats<-as.numeric(votes_list$tandem_seats)[rr]
            #vote shares to find unique result to distribute transnational candidates list onto national party lists (just using the seats wouldn't work, the addition of vote shares leads to more seats from big member parties)
            votes_shares<-as.numeric(votes_list$votes)[rr]/sum(as.numeric(votes_list$votes)[rr])
            #remove member parties without seats
            votes_shares[which(tandem_seats==0)]<-0
            #if less seats are won than there are transnational candidates
            if (sum(tandem_seats)<transseats1) {
              #add transnational list
              votes_list<-rbind(votes_list,data.frame(CC="EU",European.list.coalition=i,alternative=j,national.party=paste0(j," ",translist,collapse = " "),votes=0,comment="",seats=0,EU.threshold=TRUE,Nat.threshold=TRUE,Fivepercent=TRUE,tandem_seats=sum(tandem_seats),exclusion=TRUE))
              #remove national parties
              votes_list$tandem_seats[rr]<-0
            }else{ #more seats than transnational candidates
              #add transnational list
              votes_list$tandem_seats[rr]<-tandem_seats-divisor_round(tandem_seats+votes_shares,transseats1)
              #remove seats from national parties
              votes_list<-rbind(votes_list,data.frame(CC="EU",European.list.coalition=i,alternative=j,national.party=paste0(j," ",translist,collapse = " "),votes=0,comment="",seats=0,EU.threshold=TRUE,Nat.threshold=TRUE,Fivepercent=TRUE,tandem_seats=transseats1,exclusion=TRUE))
            }
          }
          next
        }
        #similar to GREEN coalition but without split by European parties in one coalition
        rr<-which(votes_list$CC %in% opt_in & votes_list$European.list.coalition==i)
        tandem_seats<-as.numeric(votes_list$tandem_seats)[rr]
        votes_shares<-as.numeric(votes_list$votes)[rr]/sum(as.numeric(votes_list$votes)[rr])
        votes_shares[which(tandem_seats==0)]<-0
        #check whether enough seats are available
        if (sum(as.numeric(votes_list$tandem_seats)[rr])<transseats1) {
          #add transnational list to European list coalition
          votes_list<-rbind(votes_list,data.frame(CC="EU",European.list.coalition=i,alternative="no alternative",national.party=paste0(i," ",translist,collapse = " "),votes=0,comment="",seats=0,EU.threshold=TRUE,Nat.threshold=TRUE,Fivepercent=TRUE,tandem_seats=sum(tandem_seats),exclusion=TRUE))
          #remove national parties
          votes_list$tandem_seats[rr]<-0
        }else{
          #add transnational list to European list coalition
          votes_list<-rbind(votes_list,data.frame(CC="EU",European.list.coalition=i,alternative="no alternative",national.party=paste0(i," ",translist,collapse = " "),votes=0,comment="",seats=0,EU.threshold=TRUE,Nat.threshold=TRUE,Fivepercent=TRUE,tandem_seats=transseats1,exclusion=TRUE))
          #remove seats from national parties
          votes_list$tandem_seats[rr]<-tandem_seats-divisor_round(tandem_seats+votes_shares,transseats1)
        }
        
      }
    }
    #return
    return(votes_list)
  }
}
# Run the application 
shinyApp(ui = ui, server = server )

