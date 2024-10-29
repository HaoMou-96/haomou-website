#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# 加载所需的库
library(shiny)
library(datasets)

# 加载数据集
data("mtcars")
data("USArrests")
data("uspop")

# 定义 UI
ui <- fluidPage(
  
  # 自定义样式
  tags$style(HTML("
    body {
      background-color: white;
      color: black;
    }
    h1 {
      font-family: 'Palatino', sans-serif;
    }
    .shiny-input-container {
      color: #000000;
    }
  ")),
  
  # 应用程序标题
  titlePanel("Data Visualization App"),
  
  # 侧边栏布局
  sidebarLayout(
    
    # 侧边栏：输入
    sidebarPanel(
      
      # 文本输入：用于显示标题
      textInput(inputId = "caption",
                label = "Caption:",
                value = "Data Summary"),
      
      # 数据集选择下拉菜单
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("mtcars", "USArrests", "uspop")),
      
      # 数值输入：选择显示的观察数
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   min = 1,
                   value = 10),
      
      # 选择变量用于绘图（仅适用于 mtcars 数据集）
      conditionalPanel(
        condition = "input.dataset == 'mtcars'",
        selectInput("variable", "Variable to plot against mpg:",
                    choices = c("Cylinders" = "cyl",
                                "Transmission" = "am",
                                "Gears" = "gear")),
        
        # 是否显示异常值
        checkboxInput("outliers", "Show outliers", TRUE)
      )
    ),
    
    # 主面板：输出
    mainPanel(
      
      # 显示标题
      h3(textOutput("caption", container = span)),
      
      # 显示数据集摘要
      verbatimTextOutput("summary"),
      
      # 显示数据表
      tableOutput("view"),
      
      # 显示绘图（仅适用于 mtcars 数据集）
      conditionalPanel(
        condition = "input.dataset == 'mtcars'",
        plotOutput("hpPlot")
      )
    )
  )
)

# 定义服务器逻辑
server <- function(input, output) {
  
  # 选择数据集的反应式表达式
  datasetInput <- reactive({
    switch(input$dataset,
           "mtcars" = mtcars,
           "USArrests" = USArrests,
           "uspop" = as.data.frame(uspop))
  })
  
  # 返回标题
  output$caption <- renderText({
    input$caption
  })
  
  # 数据集摘要
  output$summary <- renderPrint({
    summary(datasetInput())
  })
  
  # 显示前 n 行数据
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  # 绘制与 mpg 相关的图（仅适用于 mtcars 数据集）
  formulaText <- reactive({
    paste("hp ~", input$variable)
  })
  
  output$hpPlot <- renderPlot({
    if (input$dataset == "mtcars") {
      boxplot(as.formula(formulaText()),
              data = mtcars,
              outline = input$outliers,
              col = "#75AADB", pch = 19,
              main = paste("Plot of", input$variable, "vs hp"))
    }
  })
}

# 创建 Shiny 应用
shinyApp(ui = ui, server = server)


# 安装并加载 rsconnect 包
install.packages("rsconnect")
library(rsconnect)

# 配置账户信息（从 shinyapps.io 获取 token 和 secret）
rsconnect::setAccountInfo(name = 'l8ty54-hao-mou',
                          token = 'D5487C6CA52A2E51ACFC8270853D12E4',
                          secret = '+uLzQY0EEyw4PnHqKZr9yjS45Hz7eZdNggk/ryCT')

# 部署应用
rsconnect::deployApp(appName = "hw6_app")
