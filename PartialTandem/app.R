#############
#setup and loading data

library(shiny)
library(DT)
library(bslib)
library(proporz)
library(ggplot2)
library(ggforce)

localisation <- read.csv("localisation.csv", header=TRUE, sep=";")

Votes2024 <- read.csv("data/votes 2024.csv", header=TRUE, sep=";")
Votes2019 <- read.csv("data/votes 2019.csv", header=TRUE, sep=";")

CC<-c("AT","BE-NED","BE-FRA","BE-DEU","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")

law.matrix19<- read.csv("data/laws2019.csv", sep=";")
law.matrix24<- read.csv("data/laws2024.csv", sep=";")
law.matrix19<-cbind(CC,law.matrix19[,-1])
law.matrix24<-cbind(CC,law.matrix24[,-1])

tandem_participation_ini<-matrix(rep("yes",29),29,1)
tandem_participation_ini[c(1,c(3,4,5,6,7,8,9,12,13,14,16,17,19,23,24,25,27)+2)]<-"no"
tandem_participation_ini<-cbind(CC,tandem_participation_ini)
colnames(tandem_participation_ini)<-c("CC","participation")
tandem_participation_ini<-as.matrix(tandem_participation_ini)

coalition_names<-c("FPP","INITIATIVE","LEFT","APEU","PES","DiEM25","GREEN","EGP","EFA","EUPP","VOLT","ALDE","RENEW","EDP","EPP","ECPM","ECR","PfE","ID","ESN","inds","none")
coalition_colors<-c("seagreen","red4","red3","lawngreen","red","orangered","forestgreen","green3","purple4","black","purple","gold","skyblue","orange","mediumblue","turquoise","midnightblue","peru","peru","saddlebrown","white","grey")


#############
ui <- navbarPage("",
  theme = bs_theme(bootswatch = "flatly"),#base_font = font_collection(font_scale = .5)), #bs_theme(base_font = font_collection("system-ui", "-apple-system", "Segoe UI", font_google("Roboto"), "Helvetica Neue",  font_google("Noto Sans"), "Liberation Sans", "Arial", "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol", font_google("Noto Color Emoji")), font_scale = 0.5),
  tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/flag-icon-css/3.5.0/css/flag-icon.min.css")),
  
    tabPanel(textOutput("main0"),
             # Application title
             titlePanel(textOutput("title0")),
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
             
             accordion(
               accordion_panel(
                 textOutput("a1title"),
                 p(textOutput("a1t1")),
                 selectInput(inputId = "eu_method",
                             label =  NULL,
                             choices = c("d'Hondt","Sainte-Lague"),
                             selected  =  "d'Hondt"),
                 #technical list
                 p(textOutput("a1t2")),
                 checkboxInput(inputId = "use_technical_list",
                               label = textOutput("a1t3",inline = TRUE),
                               value = TRUE), 
                 #minimum electoral diversity
                 p(textOutput("a1t4")),
                 sliderInput(inputId = "numdiv",
                             label = NULL,
                             min = 30,
                             max = 90,
                             step = 5,
                             value = 70),
                 p(textOutput("a1t5")),
                 value = "accp1"
               ),
               accordion_panel(
                 textOutput("a2title"),
                 p(textOutput("a2t1")),
                 #introduction of european threshold
                 selectizeInput(inputId = "quorum_type",
                             label = NULL,
                             choices = NULL
                             ),
                 #european quorum
                 p(textOutput("a2t2")),
                 numericInput(inputId =  "eu_quorum",
                              label = NULL,
                              value =  .01),
                 p(textOutput("a2t3")),
                 p(textOutput("a2t4")),
                 p(textOutput("a2t5")),
                 p(textOutput("a2t6")),
                 p(textOutput("a2t7")),
                 p(textOutput("a2t8")),
                 #respect 5%-Maximum
                 checkboxInput(inputId = "respect5",
                               label = textOutput("a2t9",inline = TRUE),
                               value = TRUE),
                 p(textOutput("a2t10")),
                 value = "accp2"
               ),
               accordion_panel(textOutput("a3title"),
                 p(textOutput("a3t1")),
                 checkboxInput(inputId = "all",
                               label = textOutput("a3t2",inline = TRUE),
                               value = FALSE),
                 DTOutput("tandem_participation"),
                 value = "accp3"
               ),
               accordion_panel(textOutput("a4title"),
                 p(textOutput("a4t1")),
                 DTOutput("laws_matrix"),
                 p(textOutput("a4t2")),
                 value = "accp4"
               ),
               accordion_panel(textOutput("a5title"),
                 p(textOutput("a5t1")),
                 p(textOutput("a5t2")),
                 selectizeInput("GC",
                             label = NULL,
                             choices = NULL),
                 p(),
                 DTOutput("votes_list"),
                 #p("The list will be extended by one row if you click on this button.")#,
                 #actionButton(inputId = "add.national.party", "Add party"),
                 value = "accp5"
               )
             ),
             

             #transnational list coaltion requirements (number of countries and tandem countries)
             #output
             p(),
             h3(textOutput("res1")),
             plotOutput("Europlot"),
             uiOutput("results"),
             #accordion(id="results",accordion_panel("test"))
             #tableOutput("biproporz_partial_result")
             
             value = "tab1"
    ),
    tabPanel(textOutput("ref0"),
             titlePanel(textOutput("reftitle")),
             p(textOutput("ref1")),
             h3(textOutput("ref2")),
             p(textOutput("ref3")), 
             h3(textOutput("ref4")),
             p(class="hangingindent","R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>."),
             p(class="hangingindent","Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2022). _shiny: Web Application Framework for R_. R package version 1.7.4, <https://CRAN.R-project.org/package=shiny>"),
             p(class="hangingindent","Sievert C, Cheng J, Aden-Buie G (2023). _bslib: Custom  'Bootstrap' 'Sass' Themes for 'shiny' and 'rmarkdown'_. R package version 0.5.0, <https://CRAN.R-project.org/package=bslib>"),
             p(class="hangingindent","Xie Y, Cheng J, Tan X (2024). _DT: A Wrapper of the JavaScript Library 'DataTables'_. R package version 0.33, <https://CRAN.R-project.org/package=DT>"),
             p(class="hangingindent","Poletti F (2023). _proporz: Proportional Apportionment_. R package version 1.2."),
             p(class="hangingindent","H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016."),
             p(class="hangingindent","Pedersen T (2024). _ggforce: Accelerating 'ggplot2'_. R package version 0.4.2,  <https://CRAN.R-project.org/package=ggforce>"),
             value = "tab2"
    ),
    tabPanel("Impressum",
             p("Clemens Hoffmann"),
             p("Wissentschaftlicher Mitarbeiter am Lehrstuhl für Agrarpolitik der Georg-August-Universität Göttingen"),
             p("Platz der Göttinger Sieben 5"),
             p("37073 Göttingen"),
             p("Germany"),
             p("clemens_hoffmann[at]yahoo.de"),
             value = "tab3"
             ),
    nav_spacer(),
    tabPanel(
      selectizeInput(
        inputId = "lang",
        label =  NULL,
        choices = c("English" =2,"Deutsch"=3,"Français"=4),
        width = "110px"
      ),
      p("The page will update after selecting another language."),
      value = "tab4"
    ),
    id="allsites",
  footer = p()
  
)
#############

server <- function(input, output,session=session) {
  saved<-reactiveVal("EUT")
  saved2<-reactiveVal("tog")
  #data
  tandem_participation<-reactiveVal(as.data.frame(tandem_participation_ini))
  laws_matrix<-reactiveVal(as.data.frame(law.matrix24))
  votes_list<-reactiveVal(as.data.frame(Votes2024))
  year<-reactiveVal(2024)
  
  observeEvent(input$lang,{
    lapply(1:nrow(localisation),function(i){
      output[[localisation$id[i]]]<-renderText({
        localisation[i,as.numeric(input$lang)]
      })
    })
    output$res1<-renderText({paste0(localisation[130,as.numeric(input$lang)],year(),collapse = " ")})
    updateSelectizeInput(session,"quorum_type",
                         choices = {choi<-list("EUT","NAT","ENT","NOT")
                         names(choi)<-c(localisation[88:91,as.numeric(input$lang)])
                         choi},
                         selected = saved())
    
    updateSelectizeInput(session,"GC",
                         choices = {choi<-list("tog","sep")
                         names(choi)<-c(localisation[110:111,as.numeric(input$lang)])
                         choi},
                         selected = saved2())

    #yes & no translation
    edited_data <- tandem_participation()
    for (i in 1:nrow(edited_data)) {
      if (edited_data$participation[i] %in% localisation[127,]){
        edited_data$participation[i]<-localisation[127,as.numeric(input$lang)]
      }else{
        edited_data$participation[i]<-localisation[128,as.numeric(input$lang)]
      }
    }
    tandem_participation(edited_data)
    #list coalitions translation
    edited_data <- votes_list()
    party.names<-localisation[c(52:73,134,129),-1]
    for (i in 1:nrow(edited_data)) {
      #edited_data<-Votes2024
      rr<-which(edited_data$European.list.coalition[i]==party.names,arr.ind = TRUE)[1]
      edited_data$European.list.coalition[i]<-party.names[rr,as.numeric(input$lang)-1]
      rr<-which(edited_data$alternative[i]==party.names,arr.ind = TRUE)[1]
      edited_data$alternative[i]<-party.names[rr,as.numeric(input$lang)-1]      
    }
    votes_list(edited_data)
    #go to main page
    updateNavbarPage(session,"allsites","tab1")
  })
  
  observeEvent(input$quorum_type,{
    saved(input$quorum_type)
  })
  observeEvent(input$quorum_type,{
    saved2(input$GC)
  })
  
  #define data.frame inputs
  output$tandem_participation <- renderDT({
    req(!input$all)
    rnames<-c(localisation[1:29,as.numeric(input$lang)])
    cnames<-c(localisation[117:118,as.numeric(input$lang)])
    datatable(tandem_participation(),
              options=list(language = list(
                lengthMenu = localisation[112,as.numeric(input$lang)],
                info = localisation[113,as.numeric(input$lang)],
                `search` = localisation[114,as.numeric(input$lang)],
                paginate = list(
                  previous = localisation[115,as.numeric(input$lang)],
                  `next` = localisation[116,as.numeric(input$lang)]
                )
              )),
              rownames = rnames,
              colnames = cnames,
              editable = TRUE)})
  output$laws_matrix <- renderDT({
    rnames<-c(localisation[1:29,as.numeric(input$lang)])#as.character(country_full_names())
    cnames<-c(localisation[c(117,119:121),as.numeric(input$lang)])
    datatable(laws_matrix(),options=list(language = list(
      lengthMenu = localisation[112,as.numeric(input$lang)],
      info = localisation[113,as.numeric(input$lang)],
      `search` = localisation[114,as.numeric(input$lang)],
      paginate = list(
        previous = localisation[115,as.numeric(input$lang)],
        `next` = localisation[116,as.numeric(input$lang)]
      )
    )),
    rownames = rnames,
    colnames = cnames,
    editable = TRUE)})
  output$votes_list <- renderDT({
    cnames<-c(localisation[c(117,122:126),as.numeric(input$lang)])
    datatable(votes_list(),options=list(language = list(
    lengthMenu = localisation[112,as.numeric(input$lang)],
    info = localisation[113,as.numeric(input$lang)],
    `search` = localisation[114,as.numeric(input$lang)],
    paginate = list(
      previous = localisation[115,as.numeric(input$lang)],
      `next` = localisation[116,as.numeric(input$lang)]
    )
  )),colnames = cnames,escape = -c(4), editable = FALSE,rownames = FALSE)})
  
  observeEvent(input$tandem_participation_cell_edit, {
    info <- input$tandem_participation_cell_edit
    edited_data <- tandem_participation()
    edited_data[info$row, info$col] <- info$value
    tandem_participation(edited_data)
  })
  
  observeEvent(input$laws_matrix_cell_edit, {
    info <- input$laws_matrix_cell_edit
    edited_data <- laws_matrix()
    edited_data[info$row, info$col] <- info$value
    laws_matrix(edited_data)
  })
  
  observeEvent(input$votes_list_cell_edit, {
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
                   fivepercentrule = input$respect5,
                   min_list_div = input$numdiv/100,
                   national_laws_matrix = laws_matrix(),
                   use_technical_list = input$use_technical_list,
                   use_european_quorum = input$use_european_quorum,
                   language = as.numeric(input$lang)
    )
  })

  output$results <- renderUI({
    #create
    votes_list<-as.data.frame(tandem_result())
    results.tab<-aggregate(nat.seats~European.list.coalition,data = votes_list,FUN=sum)
    results.tab<-results.tab[(which(results.tab$nat.seats!=0)),]
    results.tab<-results.tab[order(results.tab$nat.seats, decreasing = TRUE),]
    results.tab$European.list.coalition.long<-NA
    #get long name
    for (i in 1:nrow(results.tab)) {
      if (results.tab$European.list.coalition[i] %in% localisation[135,as.numeric(input$lang)]){
        results.tab$European.list.coalition.long[i]<-localisation[which(results.tab$European.list.coalition[i]==localisation[,as.numeric(input$lang)])+1,as.numeric(input$lang)]
      }else{
        results.tab$European.list.coalition.long[i]<-localisation[which(results.tab$European.list.coalition[i]==localisation[,as.numeric(input$lang)])-22,as.numeric(input$lang)] 
      }
    }
    #create accordion panels
    panel_list <- lapply(1:nrow(results.tab), function(i){
      rr<-which(votes_list$European.list.coalition==results.tab$European.list.coalition[i]&votes_list$nat.seats>1)
      #order by number of seats
      rro<-rr[order(votes_list$nat.seats[rr],decreasing = TRUE)]
      rr1<-which(votes_list$European.list.coalition==results.tab$European.list.coalition[i]&votes_list$nat.seats==1)
      panel_content<- paste(
        paste(
        sprintf("%s - %s - %d %s",votes_list$CC[rro],votes_list$national.party[rro],votes_list$nat.seats[rro],localisation[132,as.numeric(input$lang)]),
        collapse = "<br>"
        ),
        paste(
          sprintf("%s - %s - %d %s",votes_list$CC[rr1],votes_list$national.party[rr1],votes_list$nat.seats[rr1],localisation[133,as.numeric(input$lang)]),
          collapse = "<br>" 
        ),sep = "<br>"
      )
      
      accordion_panel(
        title = paste(results.tab$European.list.coalition.long[i]," - ",results.tab$nat.seats[i],ifelse(results.tab$nat.seats[i]>1,localisation[132,as.numeric(input$lang)],localisation[133,as.numeric(input$lang)]) ),
        #paste(votes_list$national.party[rr], " - ",votes_list$nat.seats[rr],"seats",collapse = " \n "),
        HTML(panel_content),
        value= paste0("panel",i)
      )
    })
    
    accordion(id = "results_acc", !!!panel_list, open = FALSE)
  })
  
  #create plot
  output$Europlot<-renderPlot({
    #create data table
    votes_list<-as.data.frame(tandem_result())
    results.tab<-aggregate(nat.seats~European.list.coalition,data = votes_list,FUN=sum)
    results.tab<-results.tab[(which(results.tab$nat.seats!=0)),]

    #reformat for correct order
    
    results.tab<-results.tab[na.omit(match(c(localisation[c(52:73),as.numeric(input$lang)],setdiff(results.tab$European.list.coalition,localisation[c(52:73),as.numeric(input$lang)])),results.tab$European.list.coalition)),]
    results.tab$European.list.coalition<-factor(results.tab$European.list.coalition,levels = c(localisation[c(52:73),as.numeric(input$lang)],setdiff(results.tab$European.list.coalition,localisation[c(52:73),as.numeric(input$lang)])))
    results.tab$start<-cumsum(c(0, results.tab$nat.seats[-nrow(results.tab)]) / sum(results.tab$nat.seats))
    results.tab$end<-cumsum(results.tab$nat.seats / sum(results.tab$nat.seats))
    results.tab$label_position<-(results.tab$start+results.tab$end)
    results.tab$color<-coalition_colors[match(results.tab$European.list.coalition,localisation[c(52:73),as.numeric(input$lang)])]
    results.tab$color[which(is.na(results.tab$color))]<-"grey"

    ggplot(results.tab)+geom_arc_bar(aes(x0 = 0, y0 = 0, fill=European.list.coalition,
                                         r0 = .5, r = 1.5,
                                         start = start/1 * pi - 0.5 * pi,
                                         end = end / 1 * pi - 0.5 * pi))+
      scale_fill_manual(values = results.tab$color,
                        name = "")+#localisation[131,as.numeric(input$lang)])+
      theme(aspect.ratio = 0.5,
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.text = element_text(size = 12))+
      guides(color=guide_legend(ncol = 1,byrow = TRUE))
  })
  
  #

  
  observeEvent(input$reset_2019,{
    #updateSelectInput(session,"quorum_type", "EUT")
    updateNumericInput(session,"eu_quorum", value =  .01)
    updateCheckboxInput(session,"respect5", value = TRUE)
    updateSelectInput(session,"eu_method", selected =  "d'Hondt")
    updateCheckboxInput(session,"use_technical_list",value = TRUE)
    updateSliderInput(session,"numdiv", value = 70)
    #updateSelectInput(session,"GC","tog")
    tandem_participation(as.data.frame(tandem_participation_ini))
    laws_matrix(as.data.frame(law.matrix19))
    votes_list(as.data.frame(Votes2019))
    year(2019)
    updateSelectizeInput(session,"quorum_type",
                         choices = {choi<-list("EUT","NAT","ENT","NOT")
                         names(choi)<-c(localisation[88:91,as.numeric(input$lang)])
                         choi},
                         selected = "EUT")
    
    updateSelectizeInput(session,"GC",
                         choices = {choi<-list("tog","sep")
                         names(choi)<-c(localisation[110:111,as.numeric(input$lang)])
                         choi},
                         selected = "tog")
    #yes & no translation
    edited_data <- tandem_participation()
    for (i in 1:nrow(edited_data)) {
      if (edited_data$participation[i] %in% localisation[127,]){
        edited_data$participation[i]<-localisation[127,as.numeric(input$lang)]
      }else{
        edited_data$participation[i]<-localisation[128,as.numeric(input$lang)]
      }
    }
    tandem_participation(edited_data)
    #list coalitions translation
    edited_data <- votes_list()
    party.names<-localisation[c(52:73,134,129),-1]
    for (i in 1:nrow(edited_data)) {
      #edited_data<-Votes2024
      rr<-which(edited_data$European.list.coalition[i]==party.names,arr.ind = TRUE)[1]
      edited_data$European.list.coalition[i]<-party.names[rr,as.numeric(input$lang)-1]
      rr<-which(edited_data$alternative[i]==party.names,arr.ind = TRUE)[1]
      edited_data$alternative[i]<-party.names[rr,as.numeric(input$lang)-1]      
    }
    votes_list(edited_data)
  })
  
  observeEvent(input$reset_2024,{
    #updateSelectInput(session,"quorum_type", "EUT")
    updateNumericInput(session,"eu_quorum", value =  .01)
    updateCheckboxInput(session,"respect5", value = TRUE)
    updateSelectInput(session,"eu_method", selected =  "d'Hondt")
    updateCheckboxInput(session,"use_technical_list",value = TRUE)
    updateSliderInput(session,"numdiv", value = 70)
    #updateSelectInput(session,"GC","tog")
    tandem_participation(as.data.frame(tandem_participation_ini))
    laws_matrix(as.data.frame(law.matrix24))
    votes_list(as.data.frame(Votes2024))
    year(2024)
    updateSelectizeInput(session,"quorum_type",
                         choices = {choi<-list("EUT","NAT","ENT","NOT")
                         names(choi)<-c(localisation[88:91,as.numeric(input$lang)])
                         choi},
                         selected = "EUT")
    
    updateSelectizeInput(session,"GC",
                         choices = {choi<-list("tog","sep")
                         names(choi)<-c(localisation[110:111,as.numeric(input$lang)])
                         choi},
                         selected = "tog")
    #yes & no translation
    edited_data <- tandem_participation()
    for (i in 1:nrow(edited_data)) {
      if (edited_data$participation[i] %in% localisation[127,]){
        edited_data$participation[i]<-localisation[127,as.numeric(input$lang)]
      }else{
        edited_data$participation[i]<-localisation[128,as.numeric(input$lang)]
      }
    }
    tandem_participation(edited_data)
    #list coalitions translation
    edited_data <- votes_list()
    party.names<-localisation[c(52:73,134,129),-1]
    for (i in 1:nrow(edited_data)) {
      #edited_data<-Votes2024
      rr<-which(edited_data$European.list.coalition[i]==party.names,arr.ind = TRUE)[1]
      edited_data$European.list.coalition[i]<-party.names[rr,as.numeric(input$lang)-1]
      rr<-which(edited_data$alternative[i]==party.names,arr.ind = TRUE)[1]
      edited_data$alternative[i]<-party.names[rr,as.numeric(input$lang)-1]      
    }
    votes_list(edited_data)
  })
  
  observeEvent(input$GC,{
    if (input$GC=="sep"){
      vl<-votes_list()
      rr<-which(vl$European.list.coalition==localisation[58,as.numeric(input$lang)])
      vl$European.list.coalition[rr]<-vl$alternative[rr]
      vl$alternative[rr]<-localisation[58,as.numeric(input$lang)]
    } else {#if(input$GC=="tog")
      vl<-votes_list()
      rr<-which(vl$alternative==localisation[58,as.numeric(input$lang)])
      vl$alternative[rr]<-vl$European.list.coalition[rr]
      vl$European.list.coalition[rr]<-localisation[58,as.numeric(input$lang)]
    }
    votes_list(as.data.frame(vl))
  })
  
  ###### internal function
  partial_tandem<-function(election.year,
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
                           use_european_quorum,
                           language=2){
    #remove coalitions
    if (!identical(which(votes_list[,2]%in%localisation[134,-1]), integer(0))) {
      votes_list<-votes_list[-which(votes_list[,2]%in%localisation[134,-1]),]
    }
    #to lower case for yes + translations
    tandem_participation$participation<-tolower(tandem_participation$participation)
    for (i in 1:nrow(tandem_participation)) {
      if (tandem_participation$participation[i] %in% tolower(localisation[127,])) {
        tandem_participation$participation[i]<-"Yes"
      }else{
        tandem_participation$participation[i]<-"No"
      }
    }
    #translate none and inds
    none<-localisation[73,language]
    inds<-localisation[72,language]
    tech.list.coal<-localisation[135,language]
    
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
    votes_list<-cbind(votes_list,nat.seats=0)
    
    #distribution of seats in non-tandem states
    for (i in tandem_participation$CC[which(tandem_participation$participation=="No")]) {
      #which rows belong to the state
      rr<-which(votes_list$CC==i&votes_list$Nat.threshold)
      #check if STV
      if (national_laws_matrix$method[which(national_laws_matrix$CC==i)]=="STV") {
        #check if 2019
        if (election.year==2019) {##check names !!!!!!
          if (i=="IE") {
            votes_list$nat.seats[which(votes_list$national.party=="Fine Gael")]<-5
            votes_list$nat.seats[which(votes_list$national.party=="Fianna Fáil")]<-2
            votes_list$nat.seats[which(votes_list$national.party=="Sinn Féin")]<-1
            votes_list$nat.seats[which(votes_list$national.party=="Green")]<-2
            votes_list$nat.seats[which(votes_list$national.party=="Inds. 4 Change")]<-2
            votes_list$nat.seats[which(votes_list$national.party=="Independents (IE)")]<-1 
          }else if(i=="MT"){
            votes_list$nat.seats[which(votes_list$national.party=="Partit Laburista (PL)")]<-4
            votes_list$nat.seats[which(votes_list$national.party=="Partit Nazzjonalista (PN)")]<-2
          }
        }else{
          if (i=="IE") {
            votes_list$nat.seats[which(votes_list$national.party=="Fine Gael")]<-4
            votes_list$nat.seats[which(votes_list$national.party=="Fianna Fáil")]<-4
            votes_list$nat.seats[which(votes_list$national.party=="Sinn Féin")]<-2
            votes_list$nat.seats[which(votes_list$national.party=="Independent Ireland")]<-1
            votes_list$nat.seats[which(votes_list$national.party=="Labour")]<-1
            votes_list$nat.seats[which(votes_list$national.party=="Independent (IE)")]<-2            
          }else if(i=="MT"){
            votes_list$nat.seats[which(votes_list$national.party=="Partit Laburista")]<-3
            votes_list$nat.seats[which(votes_list$national.party=="Partit Nazzjonalista")]<-3            
          }
        }
        ## member states with quota method
      }else if (national_laws_matrix$method[which(national_laws_matrix$CC==i)]=="Quota"|national_laws_matrix$method[which(national_laws_matrix$CC==i)]=="Hare-Niemeyer") {
        votes_list$nat.seats[rr]<-largest_remainder_method(votes=votes_list$votes[rr],
                                                    national_laws_matrix$seats[which(national_laws_matrix$CC==i)],
                                                    quorum = 0)
        ##all other member states
      }else{
        votes_list$nat.seats[rr]<-proporz::proporz(votes=votes_list$votes[rr],
                                                        national_laws_matrix$seats[which(national_laws_matrix$CC==i)],
                                                        method = national_laws_matrix$method[which(national_laws_matrix$CC==i)],
                                                        quorum = 0)
      }
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
    given_seats<-aggregate(nat.seats~European.list.coalition,data=votes_list,FUN=sum)
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
      
    # data.frame(tandem_participation$CC[which(tandem_participation$participation=="Yes")],seats=0)
    # colnames(seat.adjust)<-c("CC","seats")
    # seats = sum(law.matrix24$seats)
    
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
      free.seats<-seats-sum(given_seats$nat.seats[which(given_seats$hang.seats)])
      
      #aggregate European result
      upper.votes<-aggregate(votes~European.list.coalition,data = votes_list,FUN=sum)
      #determine European seat distribution
      upper.votes$eu.result<-proporz::proporz(upper.votes$votes,free.seats,method = tandem_method,0)
      
      #check for overhang
      overhang<-FALSE
      for (i in given_seats$European.list.coalition) {
        if (given_seats$nat.seats[which(given_seats$European.list.coalition==i)]>upper.votes$eu.result[which(upper.votes$European.list.coalition==i)]&!given_seats$hang.seats[which(given_seats$European.list.coalition==i)]) {
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
        open.party.seats$seats[which(open.party.seats$European.list.coalition==i)]<-max(upper.votes$eu.result[which(upper.votes$European.list.coalition==i)]-given_seats$nat.seats[which(given_seats$European.list.coalition==i)],0)
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
      #create states x list coalitions matrix for all included votes
      votes_matrix<-aggregate(votes~CC+European.list.coalition,data=votes_list,FUN=sum) 
      colnames(votes_matrix)<-c("CC","European.list.coalition","votes")
      #remove list coalitions without seats
      votes_matrix<-votes_matrix[which(votes_matrix$European.list.coalition %in% open.party.seats$European.list.coalition),]
      
      votes_matrix<-as.matrix(xtabs(votes~CC+European.list.coalition,votes_matrix))
      votes_matrix<-votes_matrix[,order(colSums(votes_matrix),decreasing = TRUE)]
      
      #remove non tandem states
      for (i in CC) {
        if (tandem_participation[which(tandem_participation[,1]==i),2]=="No" ) {
          votes_matrix<-votes_matrix[-which(rownames(votes_matrix)==i),]
        }
      }
      
      #match seat.adjust to votes_matrix
      seat.adjust<-seat.adjust[match(rownames(votes_matrix),rownames(seat.adjust)),match(colnames(votes_matrix),colnames(seat.adjust))]
      

      open.state.seats<-open.state.seats[match(rownames(votes_matrix),open.state.seats$CC),]
      open.party.seats<-open.party.seats[match(colnames(votes_matrix),open.party.seats$European.list.coalition),]
      
      #test.tandem<-lower_apportionment(votes_matrix, open.party.seats$seats,open.state.seats$seats)
      #underhang because only list coalition only in optout states or not enough free seats in tandemstates

      #check whether lower_apportionment runs
      lower.res<-tryCatch({
        setTimeLimit(15)
        t(lower_apportionment(t(votes_matrix),open.state.seats$seats,open.party.seats$seats))
      },error = function(e){
        stop("Lower apportionment couldn't determined probably because too many seats had to distributed in the big EU member states. There are ways to deal with this problem such as a seat cap for national party without European list coalition or a fall back to the old electoral system with 27 indepedent elections. An inbetween solution between the later and the tandem system is also thinkable. Here neither of this options are implemented.")
      })
      #if the lower apportionment doesn't converge into a result, ones are added to zero cells in votes_matrix
      # if (all(lower.res=="error")) {
      #   #cells affected
      #   cells<-which(votes_matrix==0,arr.ind = FALSE)
      #   votes_matrix[cells]<-1#round(runif(length(cells),0,100))
      #   lower.res<-lower_apportionment(votes_matrix,open.party.seats$seats,open.state.seats$seats)
      #   #find the seats that aren't actually attributable
      #   non.attributable<-intersect(which(lower.res>0,arr.ind = FALSE),cells)
      #   if (identical(non.attributable, integer(0))) {
      #     res<-list(lower.res=lower.res,seat.adjust=seat.adjust)
      #     return(res)
      #   }
      #   non.attributable<-arrayInd(non.attributable,dim(votes_matrix))
      #   
      #   #grant list coalitions the other seats and remove it than from the apportionment
      #   #put grantable seats into seat.adjust
      #   seat.adjust[,unique(non.attributable[,2])]<-lower.res[,unique(non.attributable[,2])]
      #   seat.adjust[non.attributable]<-0
      # 
      #   for (i in colnames(seat.adjust)[unique(non.attributable[,2])]) {
      #     #add to given.seats and set European list coalition to hang seats
      #     given_seats$nat.seats[which(given_seats$European.list.coalition==i)]<-given_seats$nat.seats[which(given_seats$European.list.coalition==i)]+sum(seat.adjust[,which(colnames(seat.adjust)==i)])
      #     given_seats$hang.seats[which(given_seats$European.list.coalition==i)]<-TRUE
      #     #votes_list to zero for coalition
      #     votes_list$votes[which(votes_list$European.list.coalition==i)]<-0
      #   }
      #   #run again
      #   res<-eu_tandem(votes_list=votes_list,threshold_type=threshold_type,seats=seats,given_seats=given_seats,seat.adjust=seat.adjust)
      #   return(res)
      # }else{
        res<-list(lower.res=lower.res,seat.adjust=seat.adjust)
        return(res)
      #}
    }
    
    #don't run if no seats to distribute 
    tandem.result<-eu_tandem(votes_list=votes_list,threshold_type=threshold_type,seats=sum(national_laws_matrix$seats),given_seats=given_seats,seat.adjust=seat.adjust)
    
    seats.to.dis<-tandem.result$lower.res+tandem.result$seat.adjust
    #distribution onto national parties
    for (i in 1:nrow(seats.to.dis)) {
      for (j in 1:ncol(seats.to.dis)) {
        if (seats.to.dis[i,j]>0) {
          #find member parties with votes
          #rr<-which(votes_list$European.list.coalition==colnames(seats.to.dis)[j]&votes_list$CC==rownames(seats.to.dis)[i])
          #apply thresholds
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
          
          votes_list$nat.seats[rr]<-divisor_round(votes_list$votes[rr],seats.to.dis[i,j])
        }
      }
    }
    
    #rename to none
    for (i in 1:nrow(votes_list)){
      if (votes_list$European.list.coalition[i]==paste(none," - ",votes_list$national.party[i])) {
        votes_list$European.list.coalition[i]=none
      }
    }
    
    #return
    return(votes_list)
  }
}
# Run the application 
shinyApp(ui = ui, server = server )

