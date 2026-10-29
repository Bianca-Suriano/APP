# ============================================================
# PAINEL SHINY GENÉRICO COM 4 TELAS (para aprender)
# Só precisa do pacote shiny: install.packages("shiny")
# Rodar: abrir no RStudio e clicar em "Run App"
#        (ou shiny::runApp("painel_generico.R"))
#
# Todo app Shiny tem 3 partes:
#   1) ui     -> o visual (o que aparece na tela)
#   2) server -> a lógica (calcula e preenche os outputs)
#   3) shinyApp(ui, server) -> junta tudo e roda
#
# Regra de ouro: cada input$nome na UI vira um valor que o server lê;
# cada output$nome no server aparece na UI com o *Output() correspondente.
# ============================================================

library(shiny)

# ---------------- UI ----------------
# navbarPage() cria o menu no topo; cada tabPanel() é uma "tela".
ui <- navbarPage(
  title = "Meu primeiro painel",
  
  # ---------- TELA 1: Início ----------
  # Só conteúdo estático (texto) + um output simples.
  tabPanel("Início",
           h2("Bem-vinda ao painel"),
           p("Este painel usa o conjunto de dados 'mtcars' (carros), que já vem no R."),
           p("Use o menu no topo para navegar entre as telas."),
           h4("Resumo dos dados"),
           verbatimTextOutput("resumo")      # mostra texto do server
  ),
  
  # ---------- TELA 2: Gráfico ----------
  # sidebarLayout = barra lateral (controles) + painel principal (resultado).
  tabPanel("Gráfico",
           sidebarLayout(
             sidebarPanel(
               selectInput("var_x", "Variável do eixo X:",
                           choices = c("Peso" = "wt", "Potência" = "hp", "Cilindrada" = "disp")),
               selectInput("var_y", "Variável do eixo Y:",
                           choices = c("Consumo (mpg)" = "mpg", "Potência" = "hp", "Peso" = "wt")),
               radioButtons("tipo", "Tipo de gráfico:",
                            choices = c("Pontos" = "p", "Linhas" = "l", "Ambos" = "b"))
             ),
             mainPanel(plotOutput("grafico"))
           )
  ),
  
  # ---------- TELA 3: Tabela ----------
  # Filtro com slider + checkbox; a tabela reage a eles.
  tabPanel("Tabela",
           sidebarLayout(
             sidebarPanel(
               sliderInput("cil", "Número de cilindros:", min = 4, max = 8, value = c(4, 8), step = 2),
               checkboxInput("so_manual", "Só câmbio manual", value = FALSE)
             ),
             mainPanel(
               textOutput("contagem"),
               tableOutput("tabela")
             )
           )
  ),
  
  # ---------- TELA 4: Simulação ----------
  # Mostra o uso de botão (actionButton) e números aleatórios.
  tabPanel("Simulação",
           sidebarLayout(
             sidebarPanel(
               numericInput("n", "Tamanho da amostra:", value = 100, min = 10, max = 5000),
               numericInput("media", "Média:", value = 0),
               numericInput("dp", "Desvio padrão:", value = 1, min = 0.1),
               actionButton("gerar", "Gerar amostra")
             ),
             mainPanel(
               plotOutput("hist"),
               verbatimTextOutput("estat")
             )
           )
  )
)

# ---------------- SERVER ----------------
# input  = tudo que o usuário mexeu na UI
# output = tudo que o server devolve para a UI
server <- function(input, output, session) {
  
  # --- Tela 1 ---
  # render*() + output$nome conecta com o *Output("nome") da UI.
  output$resumo <- renderPrint({
    summary(mtcars[, c("mpg", "hp", "wt")])
  })
  
  # --- Tela 2 ---
  # O gráfico refaz sozinho sempre que um input usado aqui muda.
  output$grafico <- renderPlot({
    plot(mtcars[[input$var_x]], mtcars[[input$var_y]],
         type = input$tipo, pch = 19, col = "steelblue",
         xlab = input$var_x, ylab = input$var_y,
         main = paste(input$var_y, "vs", input$var_x))
  })
  
  # --- Tela 3 ---
  # reactive() guarda um cálculo reaproveitável (aqui, os dados filtrados).
  # Usamos dados_filtrados() COM parênteses, como se fosse função.
  dados_filtrados <- reactive({
    d <- mtcars
    d <- d[d$cyl >= input$cil[1] & d$cyl <= input$cil[2], ]
    if (input$so_manual) d <- d[d$am == 1, ]
    d
  })
  
  output$contagem <- renderText({
    paste("Carros encontrados:", nrow(dados_filtrados()))
  })
  
  output$tabela <- renderTable({
    dados_filtrados()[, c("mpg", "cyl", "hp", "wt", "am")]
  }, rownames = TRUE)
  
  # --- Tela 4 ---
  # eventReactive() só recalcula quando o botão é clicado.
  amostra <- eventReactive(input$gerar, {
    rnorm(input$n, mean = input$media, sd = input$dp)
  }, ignoreNULL = FALSE)   # roda uma vez ao abrir o app
  
  output$hist <- renderPlot({
    hist(amostra(), col = "tomato", border = "white",
         main = "Histograma da amostra", xlab = "valor")
  })
  
  output$estat <- renderPrint({
    summary(amostra())
  })
}

# ---------------- RODAR ----------------
shinyApp(ui, server)