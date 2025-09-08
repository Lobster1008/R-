#install.packages(c("shiny", "leaflet", "sp", "geojsonio", 'shinyWidgets'))
library(shiny)
library(leaflet)
library(jsonlite)
library(geojsonio)
library(shinyWidgets)

  
# 讀取台灣 geojson
geojson_url <- "https://raw.githubusercontent.com/g0v/twgeojson/master/json/twCounty2010.geo.json"
taiwan_map <- geojson_read(geojson_url, what = "sp")

# 模擬人口資料
set.seed(123)
taiwan_map@data$population <- sample(100000:3000000, nrow(taiwan_map@data), replace = TRUE)

pal <- colorNumeric("YlOrRd", domain = taiwan_map@data$population, na.color = "gray")

ui <- fluidPage(
  titlePanel("台灣各縣市人口熱像圖"),
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "selected_counties",
        label = "選擇縣市著色（可搜尋）",
        choices = taiwan_map@data$COUNTYNAME,
        selected = NULL,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          `none-selected-text` = "請選擇縣市"
        )
      ),
      actionButton("clear_btn", "清除全部選取", icon = icon("eraser")),
      br(), br(),
      # 動態產生已選縣市標籤及取消按鈕
      uiOutput("selected_tags_ui")
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

server <- function(input, output, session) {
  
  clicked_state <- reactiveValues(selected = character(0))
  
  output$map <- renderLeaflet({
    leaflet(taiwan_map) %>%
      addTiles() %>%
      addPolygons(
        fillColor = "transparent",
        color = "black",
        weight = 1,
        layerId = ~COUNTYNAME
      ) %>%
      addLegend("bottomright", pal = pal,
                values = ~population, title = "人口數")
  })
  
  # 地圖點擊互動（同步更新 selected_counties）
  observeEvent(input$map_shape_click, {
    county <- input$map_shape_click$id
    
    isolate({
      selected <- input$selected_counties
      if (county %in% selected) {
        # 已選 → 移除
        selected <- setdiff(selected, county)
      } else {
        # 未選 → 新增
        selected <- c(selected, county)
      }
      updatePickerInput(session, "selected_counties", selected = selected)
    })
  })
  
  # 根據 selected_counties 著色
  observe({
    selected <- input$selected_counties
    # 先清除所有形狀再重繪，避免多餘重複繪製
    leafletProxy("map") %>% clearShapes()
    
    # 全部還原成透明底圖
    leafletProxy("map") %>%
      addPolygons(
        data = taiwan_map,
        fillColor = "transparent",
        fillOpacity = 1,
        color = "black", weight = 1,
        label = NULL,
        layerId = ~COUNTYNAME
      )
    
    if (length(selected) > 0) {
      for (county in selected) {
        county_row <- which(taiwan_map@data$COUNTYNAME == county)
        pop_value <- taiwan_map@data$population[county_row]
        
        leafletProxy("map") %>%
          removeShape(layerId = county) %>%
          addPolygons(
            data = taiwan_map[county_row, ],
            fillColor = pal(pop_value),
            fillOpacity = 0.8,
            color = "black", weight = 1,
            label = paste0(county, ": ", format(pop_value, big.mark = ","), " 人"),
            layerId = county
          )
      }
    }
  })
  
  # 清除全部選取按鈕
  observeEvent(input$clear_btn, {
    updatePickerInput(session, "selected_counties", selected = character(0))
  })
  
  # 動態產生已選縣市標籤與刪除按鈕
  output$selected_tags_ui <- renderUI({
    selected <- input$selected_counties
    if (length(selected) == 0) return(NULL)
    
    tagList(
      lapply(selected, function(county) {
        fluidRow(
          column(9, tags$span(style = "padding: 4px 8px; background-color: #d9edf7; border-radius: 4px; display: inline-block;",
                              county)),
          column(3, actionButton(inputId = paste0("remove_", gsub("\\s", "_", county)),
                                 label = "取消", class = "btn-xs btn-danger"))
        )
      })
    )
  })
  
  # 動態觀察所有取消按鈕，逐一處理移除縣市
  observe({
    selected <- input$selected_counties
    lapply(selected, function(county) {
      btn_id <- paste0("remove_", gsub("\\s", "_", county))
      observeEvent(input[[btn_id]], {
        new_selected <- setdiff(input$selected_counties, county)
        updatePickerInput(session, "selected_counties", selected = new_selected)
      }, ignoreInit = TRUE)
    })
  })
  
}

shinyApp(ui, server)
        
       

