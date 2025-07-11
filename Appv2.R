# GLOBAL FUNCTION TO RUN AT STARTUP
global = function(){   
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(shiny)
  library(shinyWidgets)
  library(shinycssloaders)
  library(tidyr)
  library(purrr)
  library(broom)
  library(shinydashboard)
  library(viridis)
  library(ggspatial)
}

###########################################
#                   USER INTERFACE       #
###########################################
ui = function(){
  jp_solar <- read.csv('jp_solar.csv')
  jp_solar$date <- as.Date(jp_solar$date)
  jp_solar$muni_code <- as.character(jp_solar$muni_code)
  muni_names_df <- read.csv('municipality_names.csv')
  muni_names_df$muni_code <- as.character(muni_names_df$muni_code)
  jp_solar <- left_join(jp_solar, muni_names_df, by = 'muni_code' )
  
  muni_list <- unique(jp_solar$muni)
  muni_choices <- c("All", as.character(muni_list))
  area_choices <- c("Municipalities", "Prefectures", "Clusters")
  
  fluidPage(theme = "style.css",
            div(style = "padding: 1px 0px; width: '100%'",
                titlePanel(title = "", windowTitle = "Japanese Municipalities Solar Installations")
            ),
            navbarPage(
              title = div(span("Japanese Municipalities Solar Installations",
                               style = "position: relative; top: 50%; transform: translateY(-50%);")),
              
              ###################
              #     App V1      #
              ###################
              tabPanel("App V1",
                       tabsetPanel(
                         type = "tabs",
                         #########
                         #Total #
                         #########
                         tabPanel("Total Solar Rate",
                                  sidebarPanel(
                                    pickerInput("municipality1Picker", "Filter to one municipality:", choices = muni_choices, selected = "All", multiple = FALSE),
                                    radioButtons("ci1Picker", "Toggle Confidence Interval:", choices = c("On" = "on", "Off" = "off")),
                                    radioButtons("outlier1Picker", "Filter to outlier only or all graphs:", choices = c("On" = "on", "Off" = "off")),
                                    textOutput("meanText1"),
                                    br(),
                                    valueBoxOutput("sdValueBox1", width = 12),
                                    valueBoxOutput("diffFromMeanBox1", width = 12)
                                  ),
                                  mainPanel(
                                    plotOutput("total_sr_by_muni_plot") %>% withSpinner(),
                                    textOutput("endcaption1")
                                  )
                         ),
                         #########
                         #Monthly #
                         #########
                         tabPanel("Installs Per Month",
                                  sidebarPanel(
                                    pickerInput("municipality2Picker", "Filter to one municipality:", choices = muni_choices, selected = "All", multiple = FALSE),
                                    radioButtons("ci2Picker", "Toggle Confidence Interval:", choices = c("On" = "on", "Off" = "off")),
                                    radioButtons("outlier2Picker", "Filter to outlier only or all graphs:", choices = c("On" = "on", "Off" = "off")),
                                    textOutput("meanText2"),
                                    br(),
                                    valueBoxOutput("sdValueBox2", width = 12),
                                    valueBoxOutput("diffFromMeanBox2", width = 12)
                                  ),
                                  mainPanel(
                                    plotOutput("sr_by_muni_plot") %>% withSpinner(),
                                    textOutput("endcaption2")
                                  )
                         )
                       )
              ),
              
              ###################
              #     App V2      #
              ###################
              tabPanel("App V2",
                       tabsetPanel(
                         type = "tabs",
                         #########
                         #Total #
                         #########
                         tabPanel("Mean Monthly Installations",
                                  sidebarPanel(
                                    pickerInput("area_map1", "Filter by area:", choices = area_choices),
                                    checkboxInput("include_na_munis", "Include municipalities with no data", value = FALSE),
                                    pickerInput("selected_muni", "Select Municipality:", choices = muni_choices, selected = "All", multiple = FALSE),
                                    textOutput("selected_muni_data"),
                                    textOutput("highText_map1"),
                                    textOutput("lowText_map1")
                                    #radioButtons("disaster1Picker", "Toggle Disaster Effect on Graph:", choices = c("On" = "on", "Off" = "off"))
                                  ),
                                  mainPanel(plotOutput("mean_monthly_map", width = "540px", height = "720px") %>% withSpinner(),
                                            textOutput("endcaption3"))
                         ),
                         #########
                         #Monthly #
                         #########
                         tabPanel("Total Solar Rate at Time t",
                                  sidebarPanel(
                                    selectInput("date_picker", "Select Date:", choices = NULL),
                                    pickerInput("area_map2", "Filter by area:", choices = area_choices),
                                    checkboxInput("include_na_munis_2", "Include municipalities with no data", value = FALSE),
                                    pickerInput("selected_muni_2", "Select Municipality:", choices = muni_choices, selected = "All", multiple = FALSE),
                                    textOutput("selected_muni_data_2"),
                                    textOutput("highText_map2"),
                                    textOutput("lowText_map2")
                                    #radioButtons("disaster1Picker", "Toggle Disaster Effect on Graph:", choices = c("On" = "on", "Off" = "off"))
                                  ),
                                  mainPanel(plotOutput("rate_time_map", width = "540px", height = "720px") %>% withSpinner(),
                                            textOutput("endcaption4"))
                         )
                       )
              )
            )
  )
}






###########################################
#                   SERVER                #
###########################################
server = function(input, output, session){
  
  ###################################################################
  #                                   Data Manipulation             #
  ###################################################################
  ######App V1#####
  jp_solar <- read.csv('jp_solar.csv')
  jp_solar$date <- as.Date(jp_solar$date)
  jp_solar$muni_code <- as.character(jp_solar$muni_code)
  muni_names_df <- read.csv('municipality_names.csv')
  muni_names_df$muni_code <- as.character(muni_names_df$muni_code)
  jp_solar <- left_join(jp_solar, muni_names_df, by = 'muni_code' )
  
  
  monthly_solar_total <- jp_solar %>%
    filter(date <= '2017-03-25') %>%
    mutate(total_solar_rate = solar_under_10kw / pop * 1000)
  
  monthly_solar_total <- monthly_solar_total %>%
    arrange(date) %>%
    mutate(month_num = as.integer(as.factor(date)))
  
  # STEP 2: Fit Linear Model
  # Model solar_rate as a function of time
  modelMonth <- lm(solar_rate ~ month_num, data = monthly_solar_total)
  modelTotal <- lm(total_solar_rate ~ month_num, data = monthly_solar_total)
  
  # STEP 3: Summarize Model
  model_summary_month <- summary(modelMonth)
  model_summary_total <- summary(modelTotal)
  tidy_model_month <- tidy(modelMonth, conf.int = TRUE)
  tidy_model_total <- tidy(modelTotal, conf.int = TRUE)
  
  summary_stats_month <- monthly_solar_total %>%
    group_by(date) %>%
    summarize(
      mean_solar_rate = mean(solar_rate, na.rm = TRUE),
      sd_solar_rate = sd(solar_rate, na.rm = TRUE),
      se_solar_rate = sd_solar_rate / sqrt(n()),
      ci_lower = mean_solar_rate - 1.96 * se_solar_rate,
      ci_upper = mean_solar_rate + 1.96 * se_solar_rate,
      median_solar_rate = median(solar_rate, na.rm = TRUE),
      iqr_solar_rate = IQR(solar_rate, na.rm = TRUE)
    )
  
  summary_stats_total <- monthly_solar_total %>%
    group_by(date) %>%
    summarize(
      mean_solar_rate = mean(total_solar_rate, na.rm = TRUE),
      sd_solar_rate = sd(total_solar_rate, na.rm = TRUE),
      se_solar_rate = sd_solar_rate / sqrt(n()),
      ci_lower = mean_solar_rate - 1.96 * se_solar_rate,
      ci_upper = mean_solar_rate + 1.96 * se_solar_rate,
      median_solar_rate = median(total_solar_rate, na.rm = TRUE),
      iqr_solar_rate = IQR(total_solar_rate, na.rm = TRUE)
    )
  
  
  
  ######################################################################
  #                                   RENDER - App V1              
  ######################################################################
  #########################
  ##App1 REACTIVE ELEMENT #
  #########################
  monthly_solar <- reactive({
    data <- monthly_solar_total
    
    if (!is.null(input$municipality1Picker) && input$municipality1Picker != "All") {
      data <- data %>% filter(muni == input$municipality1Picker)
    }
    
    if (input$outlier1Picker == "on") {
      data <- data %>% filter(muni_code != "4362")
    }
    
    data
  })
  
  monthly_solar_2 <- reactive({
    data <- monthly_solar_total
    
    if (!is.null(input$municipality2Picker) && input$municipality2Picker != "All") {
      data <- data %>% filter(muni == input$municipality2Picker)
    }
    
    if (input$outlier2Picker == "on") {
      data <- data %>% filter(muni_code != "4362")
    }
    
    data
  })
  
  
  ##################
  #App1 PLOT1 - Total Solar Rate#
  ##################
  output$total_sr_by_muni_plot <- renderPlot({
    p <- ggplot() +
      geom_line(data = monthly_solar(), 
                aes(x = date, y = total_solar_rate, group = muni_code),
                alpha = 0.4)
    
    if (input$ci1Picker == "on") {
      p <- p +
        geom_line(data = summary_stats_total, aes(x = date, y = mean_solar_rate, color = "Mean Solar Rate"), size = 1) +
        geom_ribbon(data = summary_stats_total, aes(x = date, ymin = ci_lower, ymax = ci_upper, fill = "95% CI"), alpha = 0.5)
    }
    
    # replace with disaster code
    # if (input$disaster1Picker == "on") {
    #   p <- p +
    #     geom_line(data = summary_stats_total, aes(x = date, y = mean_solar_rate, color = "Mean Solar Rate"), size = 1) +
    #     geom_ribbon(data = summary_stats_total, aes(x = date, ymin = ci_lower, ymax = ci_upper, fill = "95% CI"), alpha = 0.5)
    # }
    
    p + labs(
      title = "Total Solar Rate Over Time by Municipality",
      x = "Date",
      y = "Total Solar Rate (solars per 1000 people")
  })
  
  output$sdValueBox1 <- renderValueBox({
    std_dev <- round(sd(monthly_solar()$total_solar_rate, na.rm = TRUE), 3)
    valueBox(value = std_dev, subtitle = "Std Dev (Selected Muni)", color = "blue")
    
    # replace with disaster
    # if (input$disaster1Picker == "on") {
    #   beta <- tidy_model_total$estimate[2]
    #   p_val <- tidy_model_total$p.value[2]
    #   sprintf("Beta: %.4f \n p-value: %.4f\n", beta,p_val)
    # }
  })
  
  output$diffFromMeanBox1 <- renderValueBox({
    data <- monthly_solar()
    latest_date <- max(data$date, na.rm = TRUE)
    selected_value <- mean(data %>% filter(date == latest_date) %>% pull(total_solar_rate), na.rm = TRUE)
    mean_value <- summary_stats_total %>% filter(date == latest_date) %>% pull(mean_solar_rate)
    diff_value <- round(selected_value - mean_value, 3)
    valueBox(value = diff_value, subtitle = "Latest Value − Mean")
  })
  
  #########################
  #App1 PLOT2 - Installs per month     #
  #########################
  output$sr_by_muni_plot <- renderPlot({
    p <- ggplot() +
      geom_line(data = monthly_solar_2(), 
                aes(x = date, y = solar_rate, group = muni_code),
                alpha = 0.4)
    
    if (input$ci2Picker == "on") {
      p <- p +
        geom_line(data = summary_stats_month, aes(x = date, y = mean_solar_rate, color = "Mean Solar Rate"), size = 1) +
        geom_ribbon(data = summary_stats_month, aes(x = date, ymin = ci_lower, ymax = ci_upper, fill = "95% CI"), alpha = 0.5)
    }
    
    # replace with disaster code
    # if (input$disaster2Picker == "on") {
    #   p <- p +
    #     geom_line(data = summary_stats_month, aes(x = date, y = mean_solar_rate, color = "Mean Solar Rate"), size = 1) +
    #     geom_ribbon(data = summary_stats_month, aes(x = date, ymin = ci_lower, ymax = ci_upper, fill = "95% CI"), alpha = 0.5)
    # }
    
    p + labs(
      title = "Solar Installation Rate Over Time by Municipality",
      x = "Date",
      y = "Solar Installation Rate (installations per 1000 people each month)")
  })
  
  output$sdValueBox1 <- renderValueBox({
    std_dev <- round(sd(monthly_solar()$total_solar_rate, na.rm = TRUE), 3)
    valueBox(value = std_dev, subtitle = "Std Dev (Selected Muni)", color = "blue")
  })
  
  output$diffFromMeanBox1 <- renderValueBox({
    data <- monthly_solar()
    latest_date <- max(data$date, na.rm = TRUE)
    selected_value <- mean(data %>% filter(date == latest_date) %>% pull(total_solar_rate), na.rm = TRUE)
    mean_value <- summary_stats_total %>% filter(date == latest_date) %>% pull(mean_solar_rate)
    diff_value <- round(selected_value - mean_value, 3)
    valueBox(value = diff_value, subtitle = "Latest Value − Mean")
  })
  
  output$sdValueBox2 <- renderValueBox({
    std_dev <- round(sd(monthly_solar_2()$solar_rate, na.rm = TRUE), 3)
    valueBox(value = std_dev, subtitle = "Std Dev (Selected Muni)", color = "blue")
  })
  
  output$diffFromMeanBox2 <- renderValueBox({
    data <- monthly_solar_2()
    latest_date <- max(data$date, na.rm = TRUE)
    selected_value <- mean(data %>% filter(date == latest_date) %>% pull(solar_rate), na.rm = TRUE)
    mean_value <- summary_stats_month %>% filter(date == latest_date) %>% pull(mean_solar_rate)
    diff_value <- round(selected_value - mean_value, 3)
    valueBox(value = diff_value, subtitle = "Latest Value − Mean")
  })
  
  #//////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  
  
  
  ######################################################################
  #                                   RENDER - App V2              
  ######################################################################
  
  #####App 2 Data Manipulation #####
  # Load data
  munis <- read.csv("municipality_names.csv")
  municipalities <- st_read("municipalities.geojson", quiet = TRUE)
  prefectures <- st_read("prefectures.geojson", quiet = TRUE)
  
  jp_solar$date <- as.Date(jp_solar$date)
  jp_solar$muni_code <- as.integer(jp_solar$muni_code)
  municipalities$muni_code <- as.integer(municipalities$muni_code)
  
  monthly_solar_data <- jp_solar %>%
    filter(date <= '2017-03-25') %>%
    mutate(total_solar_rate = solar_under_10kw / pop * 1000) %>%
    left_join(munis, by = 'muni_code')
  
  updateSelectInput(session, "date_picker", choices = unique(monthly_solar_data$date))
  
  muni_geo_joined <- municipalities %>%
    left_join(monthly_solar_data %>% 
                group_by(muni_code) %>%
                summarise(mean_solar_rate = mean(solar_rate, na.rm = TRUE)),
              by = "muni_code") %>%
    left_join(munis, by = "muni_code")
  
  observe({
    muni_names <- unique(na.omit(muni_geo_joined$muni))
    updateSelectInput(session, "selected_muni", choices = c("All", muni_names))
    updateSelectInput(session, "selected_muni_2", choices = c("All", muni_names))
  })
  
  filtered_geo_data <- reactive({
    data <- muni_geo_joined %>%
      mutate(muni = muni.x)
    if (!input$include_na_munis) {
      data <- data %>% filter(!is.na(mean_solar_rate))
    }
    data
  })
  
  filtered_geo_data_2 <- reactive({
    data <- municipalities %>%
      left_join(jp_solar %>% filter(date == as.Date(input$date_picker)) %>%
                  mutate(total_solar_rate = solar_under_10kw / pop * 1000),
                by = "muni_code") %>%
      left_join(munis, by = 'muni_code')
    if (!input$include_na_munis_2) {
      data <- data %>% filter(!is.na(total_solar_rate))
    }
    data
  })
  
  processed_geo_data <- reactive({
    data <- filtered_geo_data()
    joined <- st_join(data, prefectures, left = FALSE)
    mean_pref <- joined %>% group_by(pref_name) %>% summarise(mean_solar_rate = mean(mean_solar_rate, na.rm = TRUE))
    
    with_centroids <- joined %>%
      mutate(centroid = st_point_on_surface(st_geometry(.))) %>%
      mutate(lon = st_coordinates(centroid)[, 1], lat = st_coordinates(centroid)[, 2])
    
    clustering_data <- with_centroids %>%
      st_drop_geometry() %>%
      filter(!is.na(mean_solar_rate), !is.na(lon), !is.na(lat)) %>%
      select(mean_solar_rate, lon, lat) %>% scale()
    
    set.seed(42)
    kmeans_result <- kmeans(clustering_data, centers = 3, nstart = 25)
    with_centroids$cluster <- factor(kmeans_result$cluster[1:nrow(with_centroids)])
    
    list(pref = mean_pref, cluster = with_centroids)
  })
  
  
  
  #########################
  ##App2 REACTIVE ELEMENT #
  #########################
  
  ##################
  #App2 PLOT1 - Total Solar Rate#
  ##################
  #########Render Plot############
  output$mean_monthly_map <- renderPlot({
    data <- filtered_geo_data()
    highlight <- input$selected_muni
    
    if (input$area_map1 == "Municipalities") {
      p <- ggplot()+
        geom_sf(data = data, aes(fill = mean_solar_rate), color = "white", alpha = 0.5)
      if (highlight != "All") {
        data <- data %>% filter(muni == input$selected_muni)
        p <- p +
          geom_sf(data = data, aes(fill = mean_solar_rate), color = "white", alpha = 0.5)+
          geom_sf(data = subset(data, muni == highlight), aes(fill = mean_solar_rate), color = "black")
      } else{
        p <- p+
          geom_sf(data = data, aes(fill = mean_solar_rate), color = "gray80", alpha = 1)
      }
      
      p +
        scale_fill_viridis(name = "Solar Rate") +
        labs(title = "Mean Monthly Solar Installation Rate by Municipality") +
        annotation_scale(location = "bl") +
        annotation_north_arrow(location = "br", which_north = "true")+
        theme_minimal()
    } else if (input$area_map1 == "Prefectures") {
      ggplot(processed_geo_data()$pref) +
        geom_sf(aes(fill = mean_solar_rate)) +
        scale_fill_viridis(name = "Mean Solar Rate") +
        labs(title = "Mean Monthly Solar Installation Rate by Prefecture") +
        annotation_scale(location = "bl") +
        annotation_north_arrow(location = "br", which_north = "true")+
        theme_minimal()
    } else {
      ggplot(processed_geo_data()$cluster) +
        geom_sf(aes(fill = cluster)) +
        scale_fill_viridis_d(name = "Cluster") +
        labs(title = "Solar Rate Clustering (K=3)") +
        annotation_scale(location = "bl") +
        annotation_north_arrow(location = "br", which_north = "true")+
        theme_minimal()
    }
  })
  
  #########Render Text##############
  output$highText_map1 <- renderText({
    data <- filtered_geo_data()
    top <- data %>% filter(mean_solar_rate == max(mean_solar_rate, na.rm = TRUE))
    sprintf("Highest: %s, mean monthly solar installations = %.3f", top$muni[1], top$mean_solar_rate[1])
  })
  
  output$lowText_map1 <- renderText({
    data <- filtered_geo_data()
    bottom <- data %>% filter(mean_solar_rate == min(mean_solar_rate, na.rm = TRUE))
    sprintf("Lowest: %s, mean monthly solar installations = %.3f", bottom$muni[1], bottom$mean_solar_rate[1])
  })
  
  output$selected_muni_data <- renderText({
    req(input$selected_muni)
    if (input$selected_muni == "All") return("")
    selected_data <- filtered_geo_data() %>% filter(muni == input$selected_muni)
    if (nrow(selected_data) == 0) return("No data available for selected municipality")
    rate <- round(selected_data$mean_solar_rate, 3)
    paste("Selected Municipality:", input$selected_muni, "\nAverage Solar Rate:", rate)
  })
  
  
  
  
  
  
  
  
  
  
  #########################
  #App2 PLOT2 - Installs per month     #
  #########################
  #########Render Plot############
  output$rate_time_map <- renderPlot({
    data <- filtered_geo_data_2()
    highlight <- input$selected_muni_2
    
    if (input$area_map2 == "Municipalities") {
      p <- ggplot()+
        geom_sf(data = data, aes(fill = total_solar_rate), color = "white", alpha = 0.5)
      if (highlight != "All") {
        data <- data %>% filter(muni == input$selected_muni_2)
        p <- p +
          geom_sf(data = data, aes(fill = total_solar_rate), color = "white", alpha = 0.5)+
          geom_sf(data = subset(data, muni == highlight), aes(fill = total_solar_rate), color = "black")
      } else{
        p <- p+
          geom_sf(data = data, aes(fill = total_solar_rate), color = "gray80", alpha = 1)
      }
      
      p +
        scale_fill_viridis(name = "Total Solar Rate") +
        labs(title = paste("Total Solar Rate on", input$date_picker)) +
        annotation_scale(location = "bl") +
        annotation_north_arrow(location = "br", which_north = "true")+
        theme_minimal()
    } else if (input$area_map2 == "Prefectures") {
      ggplot(processed_geo_data()$pref) +
        geom_sf(aes(fill = mean_solar_rate)) +
        scale_fill_viridis(name = "Mean Solar Rate") +
        labs(title = "Mean Monthly Solar Installation Rate by Prefecture") +
        annotation_scale(location = "bl") +
        annotation_north_arrow(location = "br", which_north = "true")+
        theme_minimal()
    } else {
      ggplot(processed_geo_data()$cluster) +
        geom_sf(aes(fill = cluster)) +
        scale_fill_viridis_d(name = "Cluster") +
        labs(title = "Solar Rate Clustering (K=3)") +
        annotation_scale(location = "bl") +
        annotation_north_arrow(location = "br", which_north = "true")+
        theme_minimal()
    }
    
  })
  #########Render Text##############
  output$highText_map2 <- renderText({
    data <- filtered_geo_data_2()
    top <- data %>% filter(total_solar_rate == max(total_solar_rate, na.rm = TRUE))
    sprintf("Highest: %s, total solar rate = %.3f", top$muni[1], top$total_solar_rate[1])
  })
  
  output$lowText_map2 <- renderText({
    data <- filtered_geo_data_2()
    bottom <- data %>% filter(total_solar_rate == min(total_solar_rate, na.rm = TRUE))
    sprintf("Lowest: %s, total solar rate = %.3f", bottom$muni[1], bottom$total_solar_rate[1])
  })
  
  output$selected_muni_data_2 <- renderText({
    req(input$selected_muni_2)
    if (input$selected_muni_2 == "All") return("")
    selected_data <- filtered_geo_data_2() %>% filter(muni == input$selected_muni_2)
    if (nrow(selected_data) == 0) return("No data available for selected municipality")
    rate <- round(selected_data$total_solar_rate, 3)
    paste("Selected Municipality:", input$selected_muni_2, "\nTotal Solar Rate:", rate)
  })
  
  
  
  
  output$endcaption1 <- renderText(
    paste("This Shiny app is designed to visualize solar energy installation trends across 147 Japanese",
          "municipalities using the jp_solar dataset. This dataset includes variables such as date, municipality code (muni_code), ",
          "installed solar capacity under 10kW (solar_under_10kw), population (pop), and derived metrics like total_solar_rate ",
          "(calculated as solar capacity per 1,000 people). Users can interact with the app via dropdowns and radio buttons to ",
          "filter the data by a specific municipality or view all, exclude an identified outlier municipality (4362), and ",
          "toggle the display of 95% confidence intervals. The app outputs a line graph that shows the the total solar installation rate over time, ",
          "optional overlays of mean trend lines and confidence ribbons based on the full dataset",
          "rather than individual municipalities.")
  )
  output$endcaption2 <- renderText(
    paste("This Shiny app is designed to visualize solar energy installation trends across 147 Japanese",
          "municipalities using the jp_solar dataset. This dataset includes variables such as date, municipality code (muni_code), ",
          "installed solar capacity under 10kW (solar_under_10kw), population (pop), and derived metrics like total_solar_rate ",
          "(calculated as solar capacity per 1,000 people). Users can interact with the app via dropdowns and radio buttons to ",
          "filter the data by a specific municipality or view all, exclude an identified outlier municipality (4362), and ",
          "toggle the display of 95% confidence intervals. The app outputs a line graph that shows the monthly installation trends, ",
          "with optional overlays of mean trend lines and confidence ribbons based on the full dataset",
          "rather than individual municipalities.")
  )
  output$endcaption3 <- renderText(
    paste("This Shiny app is designed to visualize solar energy installation trends across 147 Japanese",
          "municipalities using the jp_solar dataset and japanese .geojson files. This dataset includes variables such as date, municipality code (muni_code), ",
          "installed solar capacity under 10kW (solar_under_10kw), population (pop), and derived metrics like total_solar_rate ",
          "(calculated as solar capacity per 1,000 people). Users can interact with the app via dropdowns and radio buttons to ",
          "filter the data by municipality, prefectures, or clusters. The app also allows for the ability for users to select ",
          "municipalities as well as whether a disaster will affect the mean solar rate. ",
          "Municipalities on this map light up in relation to the mean monthly solar rate.")
  )
  output$endcaption4 <- renderText(
    paste("This Shiny app is designed to visualize solar energy installation trends across 147 Japanese",
          "municipalities using the jp_solar dataset and japanese .geojson files. This dataset includes variables such as date, municipality code (muni_code), ",
          "installed solar capacity under 10kW (solar_under_10kw), population (pop), and derived metrics like total_solar_rate ",
          "(calculated as solar capacity per 1,000 people). Users can interact with the app via dropdowns and radio buttons to ",
          "filter the data by municipality, prefectures, or clusters. The app also allows for the ability for users to select ",
          "municipalities as well as whether a disaster will affect the mean solar rate. ",
          "Municipalities on this map light up in relation to the mean monthly solar rate.")
  )
  
  
  
}

shinyApp(ui = ui, server = server, onStart = global)