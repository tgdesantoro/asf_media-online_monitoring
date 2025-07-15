
  library(dplyr)
  library(tidyr)
  library(DT)
  library(plotly)
  library(markdown)

  server <- function(input, output, session) {
    
    # ==== Summary on ASF About Tab ====
    output$summary_table_asf <- renderTable({
      get_summary <- function(df, country) {
        data.frame(
          Country = country,
          `Number of News Articles` = length(unique(df$doc_id)),
          `Publication Range` = paste0(
            format(min(as.Date(df$date_publish, format = "%d/%m/%Y")), "%d %b %Y"),
            " - ",
            format(max(as.Date(df$date_publish, format = "%d/%m/%Y")), "%d %b %Y")
          ),
          check.names = FALSE
        )
      }
      
      indonesia <- get_summary(asf_indonesia, "Indonesia")
      malaysia  <- get_summary(asf_malaysia,  "Malaysia")
      
      rbind(indonesia, malaysia)
    }, striped = TRUE, bordered = TRUE, align = "c")
    
    
    # ==== Static UI Tab Content ====
    output$about_asf_text <- renderUI({ includeMarkdown("about_asf.md") })
    output$metode_text    <- renderUI({ includeMarkdown("method.md") })
    
    
    # ==== Filter Bertingkat: Indonesia ====
    observe({
      updateSelectInput(session, "province", choices = c("All", sort(unique(asf_indonesia$province))), selected = "All")
      updateSelectInput(session, "regency", choices = c("All", sort(unique(asf_indonesia$regency))), selected = "All")
      updateSelectInput(session, "subdistrict", choices = c("All", sort(unique(asf_indonesia$subdistrict))), selected = "All")
      updateSelectInput(session, "species", choices = c("All", sort(unique(asf_indonesia$species))), selected = "All")
    })
    
    observeEvent(input$province, {
      filtered <- if (input$province == "All") unique(asf_indonesia$regency)
      else unique(asf_indonesia$regency[asf_indonesia$province == input$province])
      updateSelectInput(session, "regency", choices = c("All", sort(filtered)), selected = "All")
    })
    
    observeEvent(input$regency, {
      filtered <- if (input$regency == "All") {
        if (input$province == "All") unique(asf_indonesia$subdistrict)
        else unique(asf_indonesia$subdistrict[asf_indonesia$province == input$province])
      } else unique(asf_indonesia$subdistrict[asf_indonesia$regency == input$regency])
      updateSelectInput(session, "subdistrict", choices = c("All", sort(filtered)), selected = "All")
    })
    
    observeEvent(input$subdistrict, {
      filtered <- if (input$subdistrict == "All") unique(asf_indonesia$species)
      else unique(asf_indonesia$species[asf_indonesia$subdistrict == input$subdistrict])
      updateSelectInput(session, "species", choices = c("All", sort(filtered)), selected = "All")
    })
    
    
    # ==== Grafik Indonesia: Filtered Plot ====
    output$filteredPlot <- renderPlot({
      df <- asf_indonesia
      if (input$province != "All")     df <- df[df$province == input$province, ]
      if (input$regency != "All")      df <- df[df$regency == input$regency, ]
      if (input$subdistrict != "All")  df <- df[df$subdistrict == input$subdistrict, ]
      if (input$species != "All")      df <- df[df$species == input$species, ]
      
      if (nrow(df) == 0) {
        plot.new(); title("Tidak ada data"); return()
      }
      
      barplot(height = df$kasus, names.arg = df$year,
              main = paste("Jumlah Kasus", input$species),
              ylab = "Jumlah Kasus", xlab = "Tahun", col = "darkred", las = 2)
    })
    
    # ==== Histogram Indonesia: Tahun x Spesies ====
    output$speciesHistogram <- renderPlot({
      df <- asf_indonesia
      if (input$province != "All")     df <- df[df$province == input$province, ]
      if (input$regency != "All")      df <- df[df$regency == input$regency, ]
      if (input$subdistrict != "All")  df <- df[df$subdistrict == input$subdistrict, ]
      
      df$year <- format(as.Date(df$date_publish, format = "%d/%m/%Y"), "%Y")
      df <- df[df$year %in% as.character(2019:2025), ]
      
      all_years <- as.character(2019:2025)
      all_species <- sort(unique(asf_indonesia$species))
      grid <- expand.grid(year = all_years, species = all_species, stringsAsFactors = FALSE)
      
      count_data <- df %>%
        count(year, species, name = "jumlah") %>%
        right_join(grid, by = c("year", "species")) %>%
        mutate(jumlah = ifelse(is.na(jumlah), 0, jumlah)) %>%
        pivot_wider(names_from = species, values_from = jumlah, values_fill = 0) %>%
        arrange(year)
      
      mat <- as.matrix(count_data[, -1])
      rownames(mat) <- count_data$year
      
      bp <- barplot(
        t(mat),
        beside = TRUE,
        col = rainbow(ncol(mat)),
        ylim = c(0, max(mat) + 5),
        main = "Jumlah Artikel per Tahun per Spesies (2019–2025)",
        ylab = "Jumlah Artikel",
        xlab = "Tahun",
        names.arg = rownames(mat),
        las = 1
      )
      values <- as.vector(t(mat))
      text(x = bp, y = values / 2, labels = values, cex = 0.8, col = "black")
      legend("topright", legend = colnames(mat), fill = rainbow(ncol(mat)), bty = "n")
    })
    
    
    # ==== Filter Bertingkat: Malaysia ====
    observe({
      req(asf_malaysia)
      updateSelectInput(session, "state", choices = c("All", sort(unique(asf_malaysia$state))), selected = "All")
      updateSelectInput(session, "species_malaysia", choices = c("All", sort(unique(asf_malaysia$species))), selected = "All")
    })
    
    observeEvent(input$state, {
      filtered_district <- if (input$state == "All") {
        unique(asf_malaysia$district)
      } else {
        unique(asf_malaysia$district[asf_malaysia$state == input$state])
      }
      updateSelectInput(session, "district", choices = c("All", sort(filtered_district)), selected = "All")
    })
    
    observeEvent(input$district, {
      filtered_mukim <- asf_malaysia
      if (input$state != "All") {
        filtered_mukim <- filtered_mukim[filtered_mukim$state == input$state, ]
      }
      if (input$district != "All") {
        filtered_mukim <- filtered_mukim[filtered_mukim$district == input$district, ]
      }
      updateSelectInput(session, "mukim", choices = c("All", sort(unique(filtered_mukim$mukim))), selected = "All")
    })
    
    
    
    # ==== Grafik Malaysia: Filtered Plot ====
    output$filteredPlot_malaysia <- renderPlot({
      df <- asf_malaysia
      if (input$state != "All")        df <- df[df$state == input$state, ]
      if (input$district != "All")     df <- df[df$district == input$district, ]
      if (input$mukim != "All")        df <- df[df$mukim == input$mukim, ]
      
      
      if (nrow(df) == 0) {
        plot.new()
        title("No data available for selected filters")
        return()
      }
      
      df %>%
        count(subdistrict = mukim, name = "jumlah") %>%
        ggplot(aes(x = reorder(subdistrict, -jumlah), y = jumlah, fill = subdistrict)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        coord_flip() +
        labs(title = "Jumlah Kasus Berdasarkan Filter (Malaysia)",
             x = "Mukim", y = "Jumlah Artikel") +
        theme_minimal()
    })
    
    # ==== Histogram Malaysia ====
    output$speciesHistogram_malaysia <- renderPlot({
      df <- asf_malaysia
      df$year <- format(as.Date(df$date_publish, format = "%d/%m/%Y"), "%Y")
      df <- df[df$year %in% as.character(2019:2025), ]
      
      all_years <- as.character(2019:2025)
      all_species <- sort(unique(asf_malaysia$species))
      grid <- expand.grid(year = all_years, species = all_species, stringsAsFactors = FALSE)
      
      count_data <- df %>%
        count(year, species, name = "jumlah") %>%
        right_join(grid, by = c("year", "species")) %>%
        mutate(jumlah = ifelse(is.na(jumlah), 0, jumlah)) %>%
        pivot_wider(names_from = species, values_from = jumlah, values_fill = 0) %>%
        arrange(year)
      
      mat <- as.matrix(count_data[, -1])
      rownames(mat) <- count_data$year
      
      bp <- barplot(
        t(mat),
        beside = TRUE,
        col = rainbow(ncol(mat)),
        ylim = c(0, max(mat) + 5),
        main = "Jumlah Artikel per Tahun per Spesies (Malaysia, 2019–2025)",
        ylab = "Jumlah Artikel",
        xlab = "Tahun",
        names.arg = rownames(mat),
        las = 1
      )
      
      values <- as.vector(t(mat))
      text(x = bp, y = values / 2, labels = values, cex = 0.8, col = "black")
      legend("topright", legend = colnames(mat), fill = rainbow(ncol(mat)), bty = "n")
    })
    
    
    # ==== Tabel dan Download ====
    output$Tab1 <- DT::renderDataTable({ datatable(asf_indonesia) })
    output$Tab2 <- DT::renderDataTable({ datatable(asf_malaysia) })
    
    output$download_indonesia <- downloadHandler(
      filename = function() { "ASF_Indonesia.csv" },
      content = function(file) { write.csv(asf_indonesia, file, row.names = FALSE) }
    )
    
    output$download_malaysia <- downloadHandler(
      filename = function() { "ASF_Malaysia.csv" },
      content = function(file) { write.csv(asf_malaysia, file, row.names = FALSE) }
    )
  }
  


shinyApp(ui = ui, server = server)
    