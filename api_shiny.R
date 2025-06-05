library(shiny)

# Carregando o modelo salvo (ajuste o caminho conforme o seu arquivo)
modelo_credito <- readRDS("C://Users//202204211696//Documents//credit_default_model.rds")

# Função para prever inadimplência
predict_default <- function(LIMIT_BAL, SEX, EDUCATION, MARRIAGE, AGE,
                            PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6,
                            BILL_AMT1, BILL_AMT2, BILL_AMT3, BILL_AMT4, BILL_AMT5, BILL_AMT6,
                            PAY_AMT1, PAY_AMT2, PAY_AMT3, PAY_AMT4, PAY_AMT5, PAY_AMT6) {
  
  input_data <- data.frame(
    LIMIT_BAL = as.numeric(LIMIT_BAL),
    SEX = factor(as.numeric(SEX), levels = c(1, 2), labels = c("Male", "Female")),
    EDUCATION = factor(as.numeric(EDUCATION), 
                       levels = c(1, 2, 3, 4, 5, 6),
                       labels = c("Graduate School", "University", "High School", 
                                  "Others", "Unknown", "Unknown")),
    MARRIAGE = factor(as.numeric(MARRIAGE), levels = c(1, 2, 3),
                      labels = c("Married", "Single", "Others")),
    AGE = as.numeric(AGE),
    PAY_0 = as.numeric(PAY_0),
    PAY_2 = as.numeric(PAY_2),
    PAY_3 = as.numeric(PAY_3),
    PAY_4 = as.numeric(PAY_4),
    PAY_5 = as.numeric(PAY_5),
    PAY_6 = as.numeric(PAY_6),
    BILL_AMT1 = as.numeric(BILL_AMT1),
    BILL_AMT2 = as.numeric(BILL_AMT2),
    BILL_AMT3 = as.numeric(BILL_AMT3),
    BILL_AMT4 = as.numeric(BILL_AMT4),
    BILL_AMT5 = as.numeric(BILL_AMT5),
    BILL_AMT6 = as.numeric(BILL_AMT6),
    PAY_AMT1 = as.numeric(PAY_AMT1),
    PAY_AMT2 = as.numeric(PAY_AMT2),
    PAY_AMT3 = as.numeric(PAY_AMT3),
    PAY_AMT4 = as.numeric(PAY_AMT4),
    PAY_AMT5 = as.numeric(PAY_AMT5),
    PAY_AMT6 = as.numeric(PAY_AMT6)
  )
  
  prob <- predict(modelo_credito, newdata = input_data, type = "response")
  classe_prevista <- ifelse(prob > 0.5, "Yes", "No")
  
  list(
    probabilidade_default = round(prob, 4),
    classe_prevista_default = classe_prevista
  )
}

# UI
ui <- fluidPage(
  titlePanel("Previsão de Inadimplência de Cartão de Crédito"),
  
  tags$div(
    style = "background-color:#f9f9f9; padding: 10px; margin-bottom: 15px; border: 1px solid #ddd;",
    tags$h5("Descrição das Variáveis:"),
    tags$ul(
      tags$li("LIMIT_BAL: Amount of given credit in NT dollars (includes individual and family/supplementary credit)"),
      tags$li("SEX: Gender (1=male, 2=female)"),
      tags$li("EDUCATION: (1=graduate school, 2=university, 3=high school, 4=others, 5=unknown, 6=unknown)"),
      tags$li("MARRIAGE: Marital status (1=married, 2=single, 3=others)"),
      tags$li("AGE: Age in years"),
      tags$li("PAY_0 to PAY_6: Repayment status from September to April, 2005 (scale: -1=pay duly, 1=payment delay for one month, ... 9=payment delay for nine months and above)"),
      tags$li("BILL_AMT1 to BILL_AMT6: Amount of bill statement from September to April, 2005 (NT dollar)"),
      tags$li("PAY_AMT1 to PAY_AMT6: Amount of previous payment from September to April, 2005 (NT dollar)"),
      tags$li("default.payment.next.month: Default payment (1=yes, 0=no)")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("LIMIT_BAL", "Limite de Crédito", 50000),
      helpText("Amount of given credit in NT dollars"),
      
      selectInput("SEX", "Sexo", choices = list("Masculino" = 1, "Feminino" = 2)),
      helpText("Gender (1=male, 2=female)"),
      
      selectInput("EDUCATION", "Educação", choices = list(
        "Graduate School" = 1,
        "University" = 2,
        "High School" = 3,
        "Others" = 4,
        "Unknown (5)" = 5,
        "Unknown (6)" = 6
      )),
      helpText("(1=graduate school, 2=university, 3=high school, 4=others, 5=unknown, 6=unknown)"),
      
      selectInput("MARRIAGE", "Estado Civil", choices = list(
        "Casado(a)" = 1,
        "Solteiro(a)" = 2,
        "Outros" = 3
      )),
      helpText("Marital status (1=married, 2=single, 3=others)"),
      
      numericInput("AGE", "Idade", 30, min = 18, max = 100),
      helpText("Age in years"),
      
      numericInput("PAY_0", "PAY_0", 0),
      helpText("Repayment status em Setembro, 2005 (-1=pay duly, 1=delay 1 mês, … 9=delay 9+ meses)"),
      
      numericInput("PAY_2", "PAY_2", 0),
      helpText("Repayment status em Agosto, 2005"),
      
      numericInput("PAY_3", "PAY_3", 0),
      helpText("Repayment status em Julho, 2005"),
      
      numericInput("PAY_4", "PAY_4", 0),
      helpText("Repayment status em Junho, 2005"),
      
      numericInput("PAY_5", "PAY_5", 0),
      helpText("Repayment status em Maio, 2005"),
      
      numericInput("PAY_6", "PAY_6", 0),
      helpText("Repayment status em Abril, 2005"),
      
      numericInput("BILL_AMT1", "BILL_AMT1", 20000),
      helpText("Valor da fatura em Setembro, 2005 (NT dollar)"),
      
      numericInput("BILL_AMT2", "BILL_AMT2", 18000),
      helpText("Valor da fatura em Agosto, 2005"),
      
      numericInput("BILL_AMT3", "BILL_AMT3", 15000),
      helpText("Valor da fatura em Julho, 2005"),
      
      numericInput("BILL_AMT4", "BILL_AMT4", 12000),
      helpText("Valor da fatura em Junho, 2005"),
      
      numericInput("BILL_AMT5", "BILL_AMT5", 10000),
      helpText("Valor da fatura em Maio, 2005"),
      
      numericInput("BILL_AMT6", "BILL_AMT6", 8000),
      helpText("Valor da fatura em Abril, 2005"),
      
      numericInput("PAY_AMT1", "PAY_AMT1", 2000),
      helpText("Pagamento anterior em Setembro, 2005 (NT dollar)"),
      
      numericInput("PAY_AMT2", "PAY_AMT2", 1800),
      helpText("Pagamento anterior em Agosto, 2005"),
      
      numericInput("PAY_AMT3", "PAY_AMT3", 1500),
      helpText("Pagamento anterior em Julho, 2005"),
      
      numericInput("PAY_AMT4", "PAY_AMT4", 1200),
      helpText("Pagamento anterior em Junho, 2005"),
      
      numericInput("PAY_AMT5", "PAY_AMT5", 1000),
      helpText("Pagamento anterior em Maio, 2005"),
      
      numericInput("PAY_AMT6", "PAY_AMT6", 800),
      helpText("Pagamento anterior em Abril, 2005"),
      
      actionButton("predict", "Prever Inadimplência")
    ),
    
    mainPanel(
      h4("Resultado da Previsão"),
      verbatimTextOutput("result")
    )
  )
)

# Server
server <- function(input, output) {
  observeEvent(input$predict, {
    resultado <- predict_default(
      input$LIMIT_BAL, input$SEX, input$EDUCATION, input$MARRIAGE, input$AGE,
      input$PAY_0, input$PAY_2, input$PAY_3, input$PAY_4, input$PAY_5, input$PAY_6,
      input$BILL_AMT1, input$BILL_AMT2, input$BILL_AMT3, input$BILL_AMT4, input$BILL_AMT5, input$BILL_AMT6,
      input$PAY_AMT1, input$PAY_AMT2, input$PAY_AMT3, input$PAY_AMT4, input$PAY_AMT5, input$PAY_AMT6
    )
    
    output$result <- renderText({
      paste0("Probabilidade de inadimplência: ", resultado$probabilidade_default * 100, "%\n",
             "Classe prevista: ", resultado$classe_prevista_default)
    })
  })
}

shinyApp(ui = ui, server = server)
