install.packages("plumber")
library(plumber)
modelo_credito <- readRDS("C://Users//202204211696//Documents//credit_default_model.rds")

#* @apiTitle API de Previsão de Inadimplência de Cartão de Crédito
#* @apiDescription Esta API permite prever a probabilidade de inadimplência no próximo mês
#* com base em dados demográficos e histórico de pagamento do cliente.

#* Previsão de Inadimplência
#* @param LIMIT_BAL Valor do crédito concedido (ex: 50000)
#* @param SEX Gênero (1 para Masculino, 2 para Feminino - conforme seu dataset original antes da fatoração, ou use "Male", "Female" e converta)
#* @param EDUCATION Nível de educação (1: Pós-graduação, 2: Universidade, 3: Ensino Médio, 4: Outros - conforme seu dataset original)
#* @param MARRIAGE Estado civil (1: Casado, 2: Solteiro, 3: Outros - conforme seu dataset original)
#* @param AGE Idade em anos (ex: 30)
#* @param PAY_0 Status de pagamento em Setembro (ex: -1, 0, 1, 2...)
#* @param PAY_2 Status de pagamento em Agosto (ex: -1, 0, 1, 2...)
#* @param PAY_3 Status de pagamento em Julho (ex: -1, 0, 1, 2...)
#* @param PAY_4 Status de pagamento em Junho (ex: -1, 0, 1, 2...)
#* @param PAY_5 Status de pagamento em Maio (ex: -1, 0, 1, 2...)
#* @param PAY_6 Status de pagamento em Abril (ex: -1, 0, 1, 2...)
#* @param BILL_AMT1 Valor da fatura em Setembro (ex: 20000)
#* @param BILL_AMT2 Valor da fatura em Agosto (ex: 18000)
#* @param BILL_AMT3 Valor da fatura em Julho (ex: 15000)
#* @param BILL_AMT4 Valor da fatura em Junho (ex: 12000)
#* @param BILL_AMT5 Valor da fatura em Maio (ex: 10000)
#* @param BILL_AMT6 Valor da fatura em Abril (ex: 8000)
#* @param PAY_AMT1 Valor pago em Setembro (ex: 2000)
#* @param PAY_AMT2 Valor pago em Agosto (ex: 1800)
#* @param PAY_AMT3 Valor pago em Julho (ex: 1500)
#* @param PAY_AMT4 Valor pago em Junho (ex: 1200)
#* @param PAY_AMT5 Valor pago em Maio (ex: 1000)
#* @param PAY_AMT6 Valor pago em Abril (ex: 800)
#* @post /predict_default
function(LIMIT_BAL, SEX, EDUCATION, MARRIAGE, AGE,
         PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6,
         BILL_AMT1, BILL_AMT2, BILL_AMT3, BILL_AMT4, BILL_AMT5, BILL_AMT6,
         PAY_AMT1, PAY_AMT2, PAY_AMT3, PAY_AMT4, PAY_AMT5, PAY_AMT6) {
  
  input_data <- data.frame(
    LIMIT_BAL = as.numeric(LIMIT_BAL),
    SEX = factor(as.numeric(SEX), levels = c(1, 2), labels = c("Male", "Female")),
    EDUCATION = factor(as.numeric(EDUCATION), levels = c(1, 2, 3, 4, 5, 6),
                       labels = c("Graduate School", "University", "High School", "Others", "Unknown", "Unknown")), # Adicionado exclude = NULL para lidar com NAs se necessário e manter todos os níveis
    MARRIAGE = factor(as.numeric(MARRIAGE), levels = c(1, 2, 3), # Adicionado nível 0, que pode existir no dataset original
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
