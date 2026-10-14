# package needed
library(shiny)

options(encoding="UTF-8")
library(reshape)
library(kableExtra)
library (tidyverse)
library(data.table)
library(DT)
library(httr)
library(rjson)
library(RJSONIO)
# use rjstat JSON-stat library
library(rjstat)
library(checkmate)
#library(backports)
library(ggplot2)
library(shinydashboard)
library(htmltools)
library(shiny)
library(shinyjs)
library(httpuv)
library(xtable)
library(rsdmx)
# bruk dplyr for CB% rydde tabeller og data
library(dplyr)
# bruk writexl for CB% eksportere til excel
#library(xlsx)
# Adress to eksport   JSON-Stat dataset for eksoprtstat - Index
url <- "https://data.ssb.no/api/v0/no/table/08817/"
data <- '{
"query":
[
  {
  "code": "Region",
  "selection": {
  "filter": "item",
  "values": [
  "0",
  "30",
  "01",
  "02",
  "03",
  "34",
  "04",
  "05",
  "06",
  "38",
  "07",
  "08",
  "42",
  "09",
  "10",
  "11",
  "46",
  "12",
  "14",
  "15",
  "50",
  "16",
  "17",
  "18",
  "54",
  "19",
  "20",
  "21",
  "22",
  "NVFF",
  "PUDT",
  "99"
  ]
  }
  },
  {
  "code": "SITC",
  "selection": {
  "filter": "item",
  "values": [
  "SITC0-1",
  "SITC03u",
  "SITC2_4",
  "SITC3b",
  "SITC5-9",
  "SITCT"
  ]
  }
  }
  ],
  "response": {
  "format": "json-stat"
  }
  }
  '
    eksport <- data.frame()

    d.tmp <- POST(url , body = data, encode = "json", verbose())

    eksport <- fromJSONstat(content(d.tmp, "text"))
    Sys.setlocale(category = "LC_ALL", locale = "Norwegian.UTF-8")

    eksport2 <- data.frame(eksport)

    colnames(eksport2) <- c("Region","Varegruppe","Variabel","MC%ned","BelC8p")

   # save(eksport2, file = "C:/Users/EIBRE/OneDrive - Innovasjon Norge/DVH/Eksportanalyse/Eksportdata/eksport.csv")
   # eksport2 <- load("C:/Users/EIBRE/OneDrive - Innovasjon Norge/DVH/Eksportanalyse/Eksportdata/eksport.csv")


    aar <- data.frame()
    as.integer(aar <- substr(eksport2$MC%ned, 1, 4))
    as.Date.character(aar, "%y")

    samleteksport <- cbind(aar, eksport2)

    sort_aar <- group_by(samleteksport, aar)
    samleteksport$Varegruppe[samleteksport$Varegruppe=="B, Fisk"] <- "Fisk"

    I<-(samleteksport$Varegruppe=="Fisk")
    fisk<-samleteksport[I, ]

    fisk<-samleteksport[(samleteksport$Varegruppe=="Fisk"), ]

    mat<-samleteksport[(samleteksport$Varegruppe=="Matvarer, drikkevarer, tobakk"),]


    mat_fisk<-merge(fisk,mat, by=c("aar","Region","MC%ned","Variabel"))

    mat_fisk$BelC8p<-mat_fisk$BelC8p.y-mat_fisk$BelC8p.x

    mat_fisk<-mat_fisk[,c("aar","Region","MC%ned","Variabel", "BelC8p")]

    mat_fisk$Varegruppe<-"Matvarer, drikkevarer, tobakk (excl fisk)"

    samleteksport_excl_fisk_mat<-samleteksport[(samleteksport$Varegruppe!="Matvarer, drikkevarer, tobakk"|samleteksport$Varegruppe!="Varer i alt"),]


    mat_fisk <- mat_fisk[(mat_fisk$Varegruppe!= "Matvarer, drikkevarer, tobakk"),]

    new<-merge(mat_fisk,samleteksport_excl_fisk_mat,
               by=c("aar","Region","MC%ned","Variabel", "BelC8p","Varegruppe"),all.x=TRUE,all.y=TRUE)

    new$MC%ned <- substr(new$MC%ned, 6, 7)

   #new = new %>%
  #    group_by(aar, MC%ned, Region, Varegruppe) %>%
  #    summarise(sum = sum(BelC8p, na.rm = TRUE))

   new <- subset(new, new$BelC8p!=0)

    new$Region[new$Region=="Hordaland (-2019)"] <- "Vestland"
    new$Region[new$Region=="Sogn og Fjordane (-2019)"] <- "Vestland"
    new$Region[new$Region=="Aust-Agder (-2019)"] <- "Agder"
    new$Region[new$Region=="Vest-Agder (-2019)"] <- "Agder"
    new$Region[new$Region=="Finnmark - FinnmC!rku (-2019)"] <- "Troms og Finnmark - Romsa ja FinnmC!rku"
    new$Region[new$Region=="Troms - Romsa (-2019)"] <- "Troms og Finnmark - Romsa ja FinnmC!rku"
    new$Region[new$Region=="Vestfold (-2019)"] <- "Vestfold og Telemark"
    new$Region[new$Region=="Telemark (-2019)"] <- "Vestfold og Telemark"
    new$Region[new$Region=="Hedmark (-2019)"] <- "Innlandet"
    new$Region[new$Region=="Oppland (-2019)"] <- "Innlandet"
    new$Region[new$Region=="Nord-TrC8ndelag (-2017)"] <- "TrC8ndelag - TrC6C6ndelage"
    new$Region[new$Region=="SC8r-TrC8ndelag (-2017)"] <- "TrC8ndelag - TrC6C6ndelage"
    new$Region[new$Region=="Akershus (-2019)"] <- "Viken"
    new$Region[new$Region=="Buskerud (-2019)"] <- "Viken"
    new$Region[new$Region=="Cstfold (-2019)"] <- "Viken"





    samleteksport_hittil <- filter(new, Variabel == "Verdi hittil i C%r")
    samleteksport_mnd <- filter(new, Variabel == "Verdi")

    url <- "https://data.norges-bank.no/api/data/EXR/M.USD+EUR.NOK.SP?format=sdmx-generic-2.1&startPeriod=1999-01-31&endPeriod=2020-05-31&locale=no"

    dataset <- readSDMX(url)
    stats <- as.data.frame(dataset)




    # Define user interface

    ui <- fluidPage(
        titlePanel(
            h1("MC%nedlige eksportdata fra SSB - Velg hvilke regioner du vil summere", allign = "center")
            # h2("Eksportdata fra SSB")


             ),

        downloadButton('download',"Last ned tabellen i csv-format"),
        fluidRow(column(10,dataTableOutput('dto'))),

        #downloadButton("report", "Generate report"),



        selectInput(inputId = "Region", "Region:",
                    c("Agder" = "Agder",
                       "Hele landet" = "Hele landet",
                      "Innlandet" = "Innlandet",
                      "Jan Mayen" ="Jan Mayen" ,
                      "MC8re og Romsdal" = "MC8re og Romsdal",
                      "Nordland" = "Nordland" ,
                      "Norske varer fra flere fylker" ="Norske varer med opprinnelse fra flere fylker",
                      "Oslo" = "Oslo" ,
                      "Reeksport av utenl. varer" = "Reeksport av varer produsert i utlandet" ,
                      "Rogaland" ="Rogaland"  ,
                      "Svalbard " ="Svalbard" ,
                      "Troms og Finnmark - Romsa ja FinnmC!rku" = "Troms og Finnmark - Romsa ja FinnmC!rku",
                      "TrC8ndelag" ="TrC8ndelag - TrC6C6ndelage" ,
                      "Uoppgitt" ="Uoppgitt fylke" ,
                      "Vestland" = "Vestland",
                      "Viken" = "Viken"),

                    multiple = TRUE),

        selectInput(inputId = "aar", "Crstall:",
                    c("1999" = "1999",
                      "2000"="2000",
                      "2001"="2001",
                      "2002"="2002",
                      "2003"="2003",
                      "2004"="2004",
                      "2005"="2005",
                      "2006"="2006",
                      "2007"="2007",
                      "2008"="2008",
                      "2009"="2009",
                      "2010"="2010",
                      "2011"="2011",
                      "2012"="2012",
                      "2013"="2013",
                      "2014"="2014",
                      "2015"="2015",
                      "2016"="2016",
                      "2017"="2017",
                      "2018"="2018",
                      "2019"="2019",
                      "2020"="2020",
                      "2021" = "2021",
                      "2022" = "2022",
                      "2023" = "2023",
                      "2024" = "2024",
                      "2025" = "2025"),

                    multiple = TRUE
        ),


        selectInput(inputId = "Varegruppe", "Varegruppe:",
                    c( "Brenselsstoffer" = "Brenselsstoffer",
                       "Matvarer, drikkevarer, tobakk" = "Matvarer, drikkevarer, tobakk (excl fisk)",
                       "RC%varer, unntatt brenselsstoffer" = "RC%varer, unntatt brenselsstoffer" ,
                       "Fisk"="Fisk",
                       "Matvarer, drikkevarer, tobakk" = "Matvarer, drikkevarer, tobakk" ,
                       "Bearbeidde varer, unntatt matvarer, drikkevarer, tobakk"= "Bearbeidde varer, unntatt matvarer, drikkevarer, tobakk",
                       "Varer i alt"   = "Varer i alt"   ),
                    multiple = TRUE
        ),


        selectInput(inputId = "Variabel","Hittil i C%r / MC%ned",
                    c("MC%ned" = "Verdi",
                      "Verdi hittil i C%r" = "Verdi hittil i C%r")),


        DT::dataTableOutput("tabell"),
        plotOutput("plot1")


    )



    # Define server logic

    server <- function(input, output) {

        tabell <- reactive({
            filter(new, (Region %in% input$Region) &

                       (aar %in% input$aar)&

                       (Varegruppe %in% input$Varegruppe) &

                       (Variabel %in% input$Variabel)

            )

        })


        output$tabell <- DT::renderDataTable({
            DT::datatable(data = tabell(), options = list(pageLength = 12),

                          rownames = FALSE, class = 'display', escape = FALSE)

        })

        output$plot1 <- renderPlot({

          tabell2 <- reactive({
            filter(new, (Region %in% input$Region) &

                     (aar %in% input$aar)&

                     (Varegruppe %in% input$Varegruppe) &

                     (Variabel %in% input$Variabel)

            )

          })


#         ggplot(data=tabell2())+
#           geom_col( aes (x= aar, y = BelC8p))+
#           geom_col(aes(x=Region, y = BelC8p))

          ggplot(data=tabell2(), aes(x = aar, y = BelC8p, fill = Region, label = aar)) +
            geom_bar(stat = "identity") #+
           # geom_text(size = 3, position = position_stack(vjust = 0.5))

       } )



        output$download <- downloadHandler(
            filename = function(){"nedlast.csv"},
            content = function(fname){
                write.csv(tabell(), fname)
            }
        )





        #'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        table_rmd <- reactive({
            filter(new, (Region %in% input$Region) &

                       (aar %in% input$aar)&

                       (Varegruppe %in% input$Varegruppe) &
                       (Variabel %in% input$Variabel)

            )

        })



        output$report <- downloadHandler(
            filename = "report.pdf",
            content = function(file) {
                tempReport <- file.path(tempdir(), "report.pdf")
                file.copy("report.pdf", tempReport, overwrite = TRUE)

                params <- list(tabell = table_rmd())

                rmarkdown::render(input = tempReport, output_file = file,
                                  params = params,
                                  envir = new.env(parent = globalenv())
                )
            }
        )
    }



    #    output$download = downloadHandler(
    #      filename = function() {"report.pdf"},
    #      content = function(file) {
    #        pdf(file, onefile = TRUE)
    #        # pdf(tabell(), onefile = TRUE)
    #       tabell()
    #        #grid.arrange(output$tabell)
    #        dev.off()
    #      }
    #    )















    #.............................................................


    # output$tabell = DT::renderDataTable({new})

    #output$tabell = DT::renderDataTable(samleteksport_hittil[, input$inputID, drop = FALSE])




    # Run the app
    shinyApp(ui = ui, server = server)
