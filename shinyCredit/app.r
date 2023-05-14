

library(shiny)
library(shinyjs)
library(DT)

ui <- pageWithSidebar(
  headerPanel(""),
  sidebarPanel(
    
    shinyjs::useShinyjs(),
    
    ### param
    numericInput("taux", "Taux (%):",value = 1.5),
    numericInput("n.mensu", "Nombre mensualites credit:",value = 300),
    textOutput("mensu"),
    br(),
    numericInput("n.assu", "Nombre de mensualites assurance:",value = 300),
    numericInput("assu", "Montant mensualites assurance:",value = 0),
    br(),
    numericInput("notaire", "Frais de notaire (%):",value = 7),
    br(),
    textInput("new","Nouvelle ligne:"),
    actionButton("add_b","Ajout"),
    actionButton("del_b","Suppr. selection")
  ),
  mainPanel(
    DTOutput("tabCredit"),
    plotOutput("graphCredit"),
    plotOutput("graphRest")
  )
)

server <- function(input, output, session) {

  ### index table
  values <- reactiveValues()
  values[["DF"]] <- data.frame(label = c("Prix","Apport","Interets","Assurance","Remboursement","Frais de notaire","Garantie","Frais de dossier","Frais de courtage"),
                               debit = c(1e5,NA,NA,NA,NA,NA,NA,NA,NA),
                               credit = c(NA,1e4,NA,NA,NA,NA,NA,NA,NA),
                               stringsAsFactors = F)
  
  output$tabCredit <- renderDT({
    DT::datatable(values[["DF"]], editable = TRUE)
  })
  
  ComputeCapital <- reactive({
      k = which(!values[["DF"]]$label %in% c("Interets","Assurance","Remboursement"))
      return(sum(values[["DF"]][k,"debit"],na.rm = T) - sum(values[["DF"]][k,"credit"],na.rm = T))
    })
  
  ComputeInterets <- reactive({
    tm = input$taux/(1200) # 100% * 12 months
    K  = ComputeCapital()
    mensu = ComputeMensu()
    
    interets = c()
    for(i in 1:input$n.mensu){
      interets = c(interets, tm * K[length(K)])
      K  = c(K, K[length(K)] - mensu + interets[length(interets)])
    }
    
    return(list(int = interets,K = K))
  })
  ComputeMensu <- reactive({
    tm = input$taux/(1200) # 100% * 12 months
    K  = ComputeCapital()
    
    mensu = (K * tm)/(1-(1 + tm)^(-(input$n.mensu)))
    return(mensu)
  })
  
  ComputeRemb <- reactive({
    return(input$n.mensu * ComputeMensu() + input$n.assu * input$assu)
  })

  ComputeNotaire <- reactive({
    return(input$notaire * values[["DF"]][which(values[["DF"]]$label == "Prix"),"debit"] / 100)
  })
  
  observeEvent(input$tabCredit_cell_edit, {
    info = input$tabCredit_cell_edit
    i = info$row
    j = info$col
    k = info$value

    if(j != which(colnames(values[["DF"]]) == "label")) values[["DF"]][i,j] = k
    
    values[["DF"]]$label  = as.character(values[["DF"]]$label)
    values[["DF"]]$debit  = as.numeric  (values[["DF"]]$debit)
    values[["DF"]]$credit = as.numeric  (values[["DF"]]$credit)
  })
  
  observeEvent(input$add_b, {
    if(!input$new %in% values[["DF"]]$label) {
      values[["DF"]] = rbind(values[["DF"]],
                             data.frame(label  = input$new,
                                        debit  = NA,
                                        credit = NA,
                                        stringsAsFactors = F))
    }
  })
  observeEvent(input$del_b, {
    if(!is.null(input$tabCredit_rows_selected))
      if(length(intersect(c("Prix","Interets","Assurance","Remboursement"),
                          values[["DF"]][input$tabCredit_rows_selected,"label"])) == 0) {
      values[["DF"]] = values[["DF"]][-as.numeric(input$tabCredit_rows_selected),]
    }
  })
  
  editTable <- reactive({list(
    input$tabCredit_cell_edit,
    input$add_b,
    input$del_b,
    input$n.mensu,
    input$taux,
    input$n.assu,
    input$assu,
    input$notaire
  )})

  observeEvent(editTable(), {
    values[["DF"]][which(values[["DF"]]$label == "Interets"),"debit"] = round(sum(ComputeInterets()$int),2)
    values[["DF"]][which(values[["DF"]]$label == "Assurance"),"debit"] = input$n.assu * input$assu
    values[["DF"]][which(values[["DF"]]$label == "Remboursement"),"credit"] = round(ComputeRemb(),2)
    if("Frais de notaire" %in% values[["DF"]]$label)
      values[["DF"]][which(values[["DF"]]$label == "Frais de notaire"),"debit"] = round(ComputeNotaire(),2)
  })
  
  output[["mensu"]] <- shiny::renderText({
    paste0("Mensualites credit = ",round(ComputeMensu(),2))
  })
  
  output$graphCredit <- renderPlot({
    
    remb = rep(ComputeMensu(),input$n.mensu)
    remba = remb
    remba[1:input$n.assu] = remb[1:input$n.assu] + input$assu
    seq.x = seq(from = 0, to = input$n.mensu, by = 12)

    plot(remba, col = "blue", lwd = 3,
         type="l" ,axes = F,
         ylim = c(0,max(remba)),
         main="Remboursement",
         xlab = "annees",ylab = "")
    legend("right",pch = rep(19,3), col = c("blue","black","red"),legend = c("Total","HA","interets"),bty="n")
    box()
    axis(side=1, at = seq.x, label = seq.x/12)
    axis(side=2)
    lines(remb, col = "black", lwd = 3)
    lines(ComputeInterets()$int[-1], col = "red", lwd = 3)
  })
  
  output$graphRest <- renderPlot({
    
    seq.x = seq(from = 0, to = input$n.mensu, by = 12)
    
    plot(ComputeInterets()$K, col = "black", lwd = 3,
         type="l" ,axes = F,
         main="Restant du",
         xlab = "annees",ylab = "")
    box()
    axis(side=1, at = seq.x, label = seq.x/12)
    axis(side=2)
  })
}

shinyApp(ui, server)
