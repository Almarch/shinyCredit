

library(shiny)
library(shinyjs)
library(DT)

ui <- pageWithSidebar(
  headerPanel(""),
  sidebarPanel(
    
    shinyjs::useShinyjs(),
    
    ### param
    numericInput("taux", "Taux (%):",value = 1.5),
    numericInput("n_mensu", "Nombre mensualites credit:",value = 300),
    textOutput("mensu1"),
    textOutput("mensu2"),
    br(),

    numericInput("t_assu", "Assurance (%):",value = 0),
    textOutput("assurance1"),
    textOutput("assurance2"),
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

  calculMensu = function(K,t,n){
    x = (K * t)/(1-(1 + t)^(-n))
    return(x)
  }

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
    for(i in 1:input$n_mensu){
      interets = c(interets, tm * K[length(K)])
      K  = c(K, K[length(K)] - mensu + interets[length(interets)])
    }
    
    return(list(int = interets, K = K))
  })

  ComputeMensu <- reactive({
    tm = input$taux/(1200) # 100% * 12 months
    K  = ComputeCapital()
    
    mensu = calculMensu(K,tm,input$n_mensu)
    return(mensu)
  })

  ComputeTAEG <- reactive({
    tm = input$taux/(1200) # 100% * 12 months
    ta = input$t_assu/(1200)
    K  = ComputeCapital()
    
    taeg = calculMensu(K,tm + ta,input$n_mensu)
    return(taeg)
  })

  ComputeAssurance <- reactive({
    mensu = ComputeTAEG() - ComputeMensu()
    return(mensu)
  })
  
  ComputeRemb <- reactive({
    return(input$n_mensu * ComputeMensu() + input$n_mensu * ComputeAssurance())
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
    input$n_mensu,
    input$taux,
    input$t_assu,
    input$notaire
  )})

  observeEvent(editTable(), {
    values[["DF"]][which(values[["DF"]]$label == "Interets"),"debit"] = round(sum(ComputeInterets()$int),2)
    values[["DF"]][which(values[["DF"]]$label == "Assurance"),"debit"] = round(ComputeAssurance() * input$n_mensu,2)
    values[["DF"]][which(values[["DF"]]$label == "Remboursement"),"credit"] = round(ComputeRemb(),2)
    if("Frais de notaire" %in% values[["DF"]]$label)
      values[["DF"]][which(values[["DF"]]$label == "Frais de notaire"),"debit"] = round(ComputeNotaire(),2)
  })
  
  output[["mensu1"]] <- shiny::renderText({
    paste0("Total emprunté = ",round(ComputeCapital(),2))
  })
    output[["mensu2"]] <- shiny::renderText({
    paste0("Mensualites hors assurance = ",round(ComputeMensu(),2))
  })

  output[["assurance1"]] <- shiny::renderText({
    paste0("Mensualites asurance = ",round(ComputeAssurance(),2))
  })

  output[["assurance2"]] <- shiny::renderText({
    paste0("Soit une mensualité totale = ",round(ComputeTAEG(),2))
  })
  
  output$graphCredit <- renderPlot({

    remb  = rep(ComputeMensu(),input$n_mensu)
    remba = rep(ComputeTAEG(), input$n_mensu)
    seq.x = seq(from = 0, to = input$n_mensu, by = 12)

    plot(remba, col = "blue", lwd = 3,
         type="l" ,axes = F,
         ylim = c(0,max(remba)),
         main="Remboursement",
         xlab = "annees",ylab = "")
    legend("right",
           pch = rep(19,3),
           col = c("blue",
                   "black",
                   "red"),
           legend = c("Total",
                      "HA",
                      "interets"),
            bty="n")
    box()
    axis(side=1, at = seq.x, label = seq.x/12)
    axis(side=2)
    lines(remb, col = "black", lwd = 3)
    lines(ComputeInterets()$int[-1], col = "red", lwd = 3)
  })
  
  output$graphRest <- renderPlot({
    
    seq.x = seq(from = 0, to = input$n_mensu, by = 12)
    
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
