# 安裝必要套件
install.packages(c("shiny", "shinydashboard", "data.table", "dplyr", "DT", "plotly"))

library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(DT)
library(plotly)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Loan Data 互動儀表板"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("上傳資料", tabName = "upload", icon = icon("upload")),
      menuItem("總覽分析", tabName = "overview", icon = icon("chart-pie")),
      menuItem("互動分析", tabName = "analysis", icon = icon("filter"))
    )
  ),
  dashboardBody(
    tabItems(
      # 上傳資料頁面
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "請上傳 Loan Data CSV 檔", status = "primary", solidHeader = TRUE, width = 6,
                    fileInput("file1", "選擇檔案", accept = ".csv")
                )
              )
      ),
      # 總覽頁面
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_loans"),
                valueBoxOutput("paid_rate")
              ),
              fluidRow(
                box(plotlyOutput("status_plot"), width = 6),
                box(plotlyOutput("pastdue_plot"), width = 6)
              )
      ),
      # 分析頁面
      tabItem(tabName = "analysis",
              fluidRow(
                box(title = "篩選", status = "warning", solidHeader = TRUE, width = 4,
                    selectInput("status_filter", "選擇貸款狀態",
                                choices = c("All"), selected = "All"),
                    selectInput("education_filter", "選擇教育程度",
                                choices = c("All"), selected = "All"),
                    selectInput("gender_filter", "選擇性別",
                                choices = c("All"), selected = "All"),
                    selectInput("age_filter", "選擇年齡區間",
                                choices = c("All", "18-25", "26-35", "36-45", "46-60"),
                                selected = "All")
                ),
                box(plotlyOutput("scatter_plot"), width = 8)
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  loan_data <- reactiveVal(NULL)
  
  # 上傳資料
  observeEvent(input$file1, {
    df <- fread(input$file1$datapath, stringsAsFactors = FALSE)
    loan_data(df)
    
    # 更新篩選選項
    updateSelectInput(session, "status_filter",
                      choices = c("All", unique(df$loan_status)),
                      selected = "All")
    updateSelectInput(session, "education_filter",
                      choices = c("All", unique(df$education)),
                      selected = "All")
    updateSelectInput(session, "gender_filter",
                      choices = c("All", unique(df$Gender)),
                      selected = "All")
  })
  
  # 總貸款筆數
  output$total_loans <- renderValueBox({
    df <- loan_data()
    validate(need(!is.null(df), "請先上傳資料"))
    valueBox(nrow(df), "總貸款筆數", icon = icon("file-invoice"), color = "blue")
  })
  
  # 已償還比例
  output$paid_rate <- renderValueBox({ 
    df <- loan_data() 
    validate(need(!is.null(df), "請先上傳資料")) 
    paid_pct <- mean(df$loan_status == c("PAIDOFF", "COLLECTION_PAIDOFF")) * 100 
    valueBox(paste0(round(paid_pct, 1), "%"), "已償還比例", icon = icon("dollar-sign"), color = "green") 
  })
  
  # 貸款狀態長條圖
  output$status_plot <- renderPlotly({ 
    df <- loan_data() 
    validate(need(!is.null(df), "請先上傳資料")) 
    plot_ly(df, x = ~loan_status, type = "histogram", color = ~loan_status) 
  })
  
  # 逾期天數箱型圖
  output$pastdue_plot <- renderPlotly({
    df <- loan_data()
    validate(need(!is.null(df), "請先上傳資料"))
    if (!"past_due_days" %in% names(df)) return(NULL)
    plot_ly(df, x = ~past_due_days, type = "box", name = "逾期天數", boxpoints = "all")
  })
  
  # 互動分析 scatter plot
  output$scatter_plot <- renderPlotly({
    df <- loan_data()
    validate(need(!is.null(df), "請先上傳資料"))
    df_f <- df
    
    # 篩選條件
    if (input$status_filter != "All") {
      df_f <- df_f %>% filter(loan_status == input$status_filter)
    }
    if (input$education_filter != "All") {
      df_f <- df_f %>% filter(education == input$education_filter)
    }
    if (input$gender_filter != "All") {
      df_f <- df_f %>% filter(Gender == input$gender_filter)
    }
    if (input$age_filter != "All") {
      df_f <- df_f %>% filter(
          (input$age_filter == "18-25" & age >= 18 & age <= 25) |
          (input$age_filter == "26-35" & age >= 26 & age <= 35) |
          (input$age_filter == "26-35" & age >= 26 & age <= 35) |
          (input$age_filter == "36-45" & age >= 36 & age <= 45) |
          (input$age_filter == "46-60" & age >= 46 & age <= 60) 
            
      )
    }
    
    # 確認欄位存在
    validate(
      need("Principal" %in% names(df_f), "資料缺少 Principal 欄位"),
      need("loan_status" %in% names(df_f), "資料缺少 loan_status 欄位"),
      need("age" %in% names(df_f), "資料缺少 age 欄位")
    )
    
    # 繪圖
    plot_ly(df_f, x = ~loan_status, y = ~Principal, color = ~loan_status,
            type = "scatter", mode = "markers",
            hoverinfo = "text",
            text = ~paste("Loan Status:", loan_status,
                          "<br>Principal:", Principal,
                          "<br>Age:", age,
                          "<br>Education:", education,
                          "<br>Gender:", Gender))
  })
}

# 啟動 App
shinyApp(ui, server)
