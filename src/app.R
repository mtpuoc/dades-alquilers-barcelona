# install.packages("shiny")
# install.packages("shinydashboard")

library(shiny)
library(shinydashboard)

#alquiler_barcelona <- read.csv2("./data/alquilers-barcelona", sep=";", fileEncoding = "UTF-8")

df_2022 <- alquiler_barcelona[alquiler_barcelona$Any==2022,]

#preus
mitjana <- round(mean(df_2022$Preu),2)

mitjana_t2 <- round(mean(df_2022[df_2022$Trimestre==2,"Preu"]),2)
mitjana_t3 <- mean(df_2022[df_2022$Trimestre==3,"Preu"])
diff <- round(mitjana_t3*100/mitjana_t2-100,2)

mitjana_2021 <- mean(alquiler_barcelona[alquiler_barcelona$Any==2021,]$Preu)
diff_any <- round(mitjana*100/mitjana_2021-100,2)


barri_max <- df_2022[df_2022$Preu==max(df_2022$Preu),][1,]
barri_min <- df_2022[df_2022$Preu==min(df_2022$Preu),][1,]

# nombres compra-venta
cv_max <- df_2022[df_2022$compra_venta==max(df_2022$compra_venta),][1,]
cv_min <- df_2022[df_2022$compra_venta==min(df_2022$compra_venta),][1,]

# nombres lloguer
ll_max <- df_2022[df_2022$lloguers==max(df_2022$lloguers),][1,]
ll_min <- df_2022[df_2022$lloguers==min(df_2022$lloguers),][1,]



ui <- dashboardPage(
  dashboardHeader(title = "Evolució alquilers Barcelona", titleWidth = 300),
  dashboardSidebar(width = 300,sidebarMenu(
    menuItem("Resum", tabName = "overview"),
    menuItem("Preus", tabName = "preus"),
    menuItem("Compra venta", tabName = "compra-venta"),
    menuItem("Lloguers", tabName = "lloguers"),
    menuItem("Taula dataset", tabName = "taula")
  )),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "overview",
              h1("Resum"),
              div("En aquest espai es mostra l'evolució dels preus del m2 de Barcelona i dels nombre de lloguers i compra-venta que s'han fet trimestralment a partir del 2014."),
              hr(),
              div("Tota aquesta informació s'ha tret de la web de Barcelona de opensource on hi ha hagut un data-quality on s'han omes els registres que hi havia algún valor 
              sense informar ja que per a la realització dels calculs es necesari tenir els valors informats."),
              h2("Evolució dels preus de Barcelona"),
              fluidRow(
                # A static valueBox
                valueBox(paste(mitjana,"e/m2"), width = 4, "Preu del m2 a la provincia de Barcelona durant el any 2022.", icon = icon("home")),
                valueBox(paste("+",diff,"%"), width = 4, "Evolució del m2 del segon trimestre respecte al tercer de l'any 2022", icon = icon("home")),
                valueBox(paste("+",diff_any,"%"), width = 4, "Evolució del m2 de l'any 2022 respecte al 2021", icon = icon("home"))
              ),
              fluidRow(
                # A static valueBox
                valueBox(paste(barri_max$Preu,"e/m2"), width = 4, paste("Es el preu més alt del 2022 i es troba al districte:",barri_max$Nom_Districte,"-",barri_max$Nom_Barri), icon = icon("arrow-up")),
                valueBox(paste(barri_min$Preu,"e/m2"), width = 4, paste("Es el preu més baix del 2022 i es troba al districte:",barri_min$Nom_Districte,"-",barri_min$Nom_Barri), icon = icon("arrow-down")),
              ),
              h2("Nombres de compra-venta a Barcelona"),
              fluidRow(
                # A static valueBox
                valueBox(cv_max$Nom_Districte, width = 4, paste("Es el districte amb més compra-venta(",cv_max$compra_venta,"), el barri:",cv_max$Nom_Barri), icon = icon("repeat"),color = "green"),
                valueBox(cv_min$Nom_Districte, width = 4, paste("Es el districte amb menys compra-venta(",cv_min$compra_venta,"), el barri:",barri_max$Nom_Barri), icon = icon("repeat"),color = "green"),
              ),
              h2("Nombres de lloguer a Barcelona"),
              fluidRow(
                # A static valueBox
                valueBox(ll_max$Nom_Districte, width = 4, paste("Es el districte amb més lloguer(",ll_max$lloguers,"), el barri:",ll_max$Nom_Barri), icon = icon("check"),color = "teal"),
                valueBox(ll_min$Nom_Districte, width = 4, paste("Es el districte amb menys lloguer(",ll_min$lloguers,"), el barri:",ll_min$Nom_Barri), icon = icon("check"), color = "teal"),
              )
      ),
      
      # Second tab content
      tabItem(tabName = "preus",
          fluidRow(
            h2("Preus d'alquilers"),
            box(title = "Filtrar", width = 10, status = "warning", solidHeader = TRUE,
                column(4,selectInput("districte", "Selecciona el nom del districte", append("Tots",alquiler_barcelona$Nom_Districte))),
                column(5,uiOutput("selectionPreusBarri"))
            ),
            plotOutput("plot_preus"),
          )
      ),
      tabItem(tabName = "compra-venta",
        fluidRow(
          h2("Compra venta"),
          box(title = "Filtrar",width = 10, status = "warning", solidHeader = TRUE,
              column(4,selectInput("districteCV", "Selecciona el nom del districte", append("Tots",alquiler_barcelona$Nom_Districte))),
              column(5,uiOutput("selectionCVBarri"))
          ),
          plotOutput("barplot_cv")
        )
      ),
      tabItem(tabName = "lloguers",
        fluidRow(
          h2("Lloguers"),
          box(title = "Filtrar",width = 10, status = "warning", solidHeader = TRUE,
              column(4,selectInput("districteLL", "Selecciona el nom del districte", append("Tots",alquiler_barcelona$Nom_Districte))),
              column(5,uiOutput("selectionLloguersBarri"))
          ),
          plotOutput("barplot_lloguer")
        )
      ),
      tabItem(tabName = "taula",
        fluidRow(
          h2("Taula"),
          box(title = "Filtrar",width = 11, status = "warning", solidHeader = TRUE,
              column(3,selectInput("anyTaula", "Selecciona l'any", append("Tots",alquiler_barcelona$Any))),
              column(3,selectInput("districteT", "Selecciona el nom del districte", append("Tots",alquiler_barcelona$Nom_Districte))),
              column(5,uiOutput("selectionTaulaBarri"))
          ),
          dataTableOutput('tableAlquiler'),
          downloadButton("downloadData", "Download")
        )
      )
    )
  )
)

server <- function(input, output) {
  output$selectionPreusBarri <- renderUI({
    if(input$districte == "Tots"){
      selectInput("barri", "Selecciona el barri","Has de seleccionar un districte primer")
    }else{
      selectInput("barri", "Selecciona el barri",alquiler_barcelona[alquiler_barcelona$Nom_Districte == input$districte,]$Nom_Barri)
    }
  })
  
  output$selectionCVBarri <- renderUI({
    if(input$districteCV == "Tots"){
      selectInput("barriCV", "Selecciona el barri","Has de seleccionar un districte primer")
    }else{
      selectInput("barriCV", "Selecciona el barri",alquiler_barcelona[alquiler_barcelona$Nom_Districte == input$districteCV,]$Nom_Barri)
    }
  })
  
  output$selectionLloguersBarri <- renderUI({
    if(input$districteLL == "Tots"){
      selectInput("barriLL", "Selecciona el barri","Has de seleccionar un districte primer")
    }else{
      selectInput("barriLL", "Selecciona el barri",alquiler_barcelona[alquiler_barcelona$Nom_Districte == input$districteLL,]$Nom_Barri)
    }
  })
  
  output$selectionTaulaBarri <- renderUI({
    if(input$districteT == "Tots"){
      selectInput("barriTaula", "Selecciona el barri","Has de seleccionar un districte primer")
    }else{
      selectInput("barriTaula", "Selecciona el barri",alquiler_barcelona[alquiler_barcelona$Nom_Districte == input$districteT,]$Nom_Barri)
    }
  })
  
  output$plot_preus <- renderPlot({
    if(input$districte == "Tots"){
      tmp <- alquiler_barcelona
      main <- "Evolució preus alquilers"
    }else{
      tmp <- alquiler_barcelona[alquiler_barcelona$Nom_Districte == input$districte,]
      main <- paste("Evolució preus alquilers. Districte:",input$districte)
      
      if(input$barri != ""){
        tmp <- tmp[tmp$Nom_Barri == input$barri,]
        main <- paste(main,"::",input$barri)
      }
    }
    a <-aggregate(tmp$Preu,by=list(tmp$Any),FUN=mean)
    p <- plot(a$Group.1,a$x,type = "b",col="blue",xlab="Any",ylab="preu mitja",main=main)
    text(a$Group.1, a$x,round(a$x,2), pos=2,col="red")
  })
  output$barplot_cv <- renderPlot({
    if(input$districteCV == "Tots"){
      tmp_cv <- alquiler_barcelona
      main <- "Nombre total de moviments de compra-venta"
    }else{
      tmp_cv <- alquiler_barcelona[alquiler_barcelona$Nom_Districte == input$districteCV,]
      main <- paste("Nombre total de moviments de compra-venta. Districte:",input$districteCV)
      
      if(input$barriCV != ""){
        tmp_cv <- tmp_cv[tmp_cv$Nom_Barri == input$barriCV,]
        main <- paste(main,"::",input$barriCV)
      }
    }
    a <- aggregate(tmp_cv$compra_venta,by=list(tmp_cv$Any),FUN=sum)
    bp <- barplot(a$x,names.arg=a$Group.1,xlab="Any",ylab="Nombre",main=main, col = "green")
    text(bp, a$x/2,labels=round(a$x,digits=2))
  })
  output$barplot_lloguer <- renderPlot({
    if(input$districteLL == "Tots"){
      tmp_ll <- alquiler_barcelona
      main <- "Nombre total de lloguers"
    }else{
      tmp_ll <- alquiler_barcelona[alquiler_barcelona$Nom_Districte == input$districteLL,]
      main <- paste("Nombre total de lloguers. Districte:",input$districteLL)
      if(input$barriLL != ""){
        tmp_ll <- tmp_ll[tmp_ll$Nom_Barri == input$barriLL,]
        main <- paste(main,"::",input$barriLL)
      }
    }
    a <-aggregate(tmp_ll$lloguers,by=list(tmp_ll$Any),FUN=sum)
    bp <- barplot(a$x,names.arg=a$Group.1,xlab="Any",ylab="Nombre",main=main, col="#008080")
    text(bp, a$x/2,labels=round(a$x,digits=2))
  })
  
  output$tableAlquiler <- renderDataTable({
    if(input$anyTaula == "Tots"){
      df_taula <- alquiler_barcelona
    }else{
      df_taula <- alquiler_barcelona[alquiler_barcelona$Any ==input$anyTaula,]
    }
    if(input$districteT != "Tots"){
      df_taula <- df_taula[df_taula$Nom_Districte == input$districteT,]
      if(input$barriTaula != ""){
        df_taula <- df_taula[df_taula$Nom_Barri == input$barriTaula,]
      }
    }
    df_taula
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(output$tableAlquiler, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)