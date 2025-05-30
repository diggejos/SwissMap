# Load libraries
library(shiny)
library(tidyverse)
library(sf)
library(viridis)
library(ggiraph)
library(shinycssloaders)
library(patchwork)
library(scales)

Sys.setenv(R_MAX_MEM_SIZE = 512000000)  # 512MB


# --- Login credentials ---
credentials <- list(username = "Tamedia", password = "Jodi_9878")

# === Load data ===
data <- read.csv("input/data_2.csv", sep = ";", fileEncoding = "ISO-8859-1") %>%
  mutate(across(c(Bevoelkerung, TA, BS, BZ, BU, h24, TG, FW, SZ, mean),
                ~ suppressWarnings(as.numeric(gsub("'", "", .)))))

gemeinde_kanton <- read.csv("input/kanton_gemeinde_mapping.csv", sep = ";", fileEncoding = "ISO-8859-1") %>%
  dplyr::select(BFS_Nr, Kantonskuerzel) %>%
  distinct()

data <- data %>% 
  left_join(gemeinde_kanton, by = c("bfs_id" = "BFS_Nr")) 

# === Load spatial data ===
canton_geo <- read_sf("input/g2k15.shp")
country_geo <- read_sf("input/g2l15.shp")
lake_geo <- read_sf("input/g2s15.shp")
municipality_geo <- read_sf("input/gde-1-1-15.shp")

# === Relief raster ===
relief <- raster("input/02-relief-ascii.asc") %>%
  mask(country_geo) %>%
  as("SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(value = X02.relief.ascii)

# # === Load preprocessed relief ===
# relief <- readRDS("input/relief_df.rds") %>%
#   rename(value = X02.relief.ascii)

# === Theme ===
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "arial", color = "black"),
      axis.text = element_blank(), axis.ticks = element_blank(),
      axis.title = element_blank(), panel.grid = element_blank(),
      plot.background = element_rect(fill = "#e7dfdd", color = NA),
      panel.background = element_rect(fill = "#e7dfdd", color = NA),
      legend.background = element_rect(fill = "#e7dfdd", color = NA),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      legend.text = element_text(size=10),
      legend.title = element_text(size=10),
      legend.position = "top",
      ...
    )
}

# === UI ===
ui <- fluidPage(
  tags$style(HTML("
    body, .container-fluid {
      background-color: #e7dfdd !important;
    }
    .girafe-container, .girafe-output-container, .girafe, svg {
      margin: 0 !important;
      padding: 0 !important;
      background-color: #e7dfdd !important;
    }
    .row > .col-sm-2, .row > .col-sm-10 { padding-left: 0 !important; padding-right: 0 !important; }
    .well { margin-left: 15px; }
  ")),
  uiOutput("auth_ui")
)

# === Server ===
server <- function(input, output, session) {
  user_logged_in <- reactiveVal(FALSE)
  selected_id <- reactiveVal(NULL)
  
  output$auth_ui <- renderUI({
    if (user_logged_in()) {
      fluidPage(
        titlePanel("Streudichte nach Marke"),
        fluidRow(
          column(2,
                 wellPanel(
                   selectInput("selected_brands", "Marken wählen:",
                               choices = c("TA", "BS", "BZ", "BU", "h24", "TG", "FW", "SZ"),
                               selected = "TA", multiple = TRUE),
                   radioButtons("value_mode", "Anzeigemodus:",
                                choices = c("Streudichte (%)" = "percent", "Absolute Anzahl" = "absolute"),
                                selected = "percent")
                 ),
                 strong(textOutput("total_abos"))
          ),
          column(10, withSpinner(girafeOutput("brand_map")))
        )
      )
    } else {
      fluidPage(
        h3("Login erforderlich"),
        textInput("user", "Benutzername"),
        passwordInput("pass", "Passwort"),
        actionButton("login", "Einloggen"),
        textOutput("login_message")
      )
    }
  })
  
  observeEvent(input$login, {
    if (input$user == credentials$username && input$pass == credentials$password) {
      user_logged_in(TRUE)
    } else {
      output$login_message <- renderText("❌ Falscher Benutzername oder Passwort.")
    }
  })
  
  map_data <- reactive({
    req(input$selected_brands)
    data %>%
      mutate(abos_sum = rowSums(across(all_of(input$selected_brands)), na.rm = TRUE)) %>%
      mutate(percent = abos_sum / Bevoelkerung * 100) %>%
      transmute(
        bfs_id,
        abs_value = abos_sum,
        percent,
        mean,
        municipality,
        Bevoelkerung,
        Kantonskuerzel
      ) %>%
      left_join(municipality_geo, by = c("bfs_id" = "BFS_ID")) %>%
      st_as_sf()
  })
  
  output$total_abos <- renderText({
    req(map_data())
    paste("Gesamtanzahl Abos:", format(sum(map_data()$abs_value, na.rm = TRUE), big.mark = "'"))
  })
  
  output$brand_map <- renderGirafe({
    req(map_data())  # <-- sehr wichtig!
    
    value_col <- if (input$value_mode == "percent") "percent" else "abs_value"
    label_unit <- if (input$value_mode == "percent") " %" else " Abos"
    
    df_all <- map_data() %>% st_drop_geometry()
    
    top_cantons <- df_all %>%
      group_by(Kantonskuerzel) %>%
      summarise(total = sum(.data[[value_col]], na.rm = TRUE)) %>%
      arrange(desc(total)) %>%
      slice_head(n = 5) %>%
      pull(Kantonskuerzel)
    
    df <- df_all %>%
      filter(Kantonskuerzel %in% top_cantons) %>%
      arrange(desc(.data[[value_col]])) %>%
      slice_head(n = 300)
    
    sf_data <- map_data()
    
    # Map
    p_map <- ggplot(sf_data) +
      geom_raster(data = relief, aes(x = x, y = y, alpha = value), inherit.aes = FALSE) +
      scale_alpha(range = c(0.6, 0), guide = "none") +
      geom_sf_interactive(
        aes(
          geometry = geometry,
          fill = .data[[value_col]],
          tooltip = paste0("Gemeinde: ", municipality, "\n", round(.data[[value_col]], 2), label_unit),
          data_id = bfs_id
        ),
        color = "white", size = 0.1
      ) +
      scale_fill_viridis_c(
        option = "plasma", name = label_unit, begin = 0.1, end = 0.9, direction = 1,
        guide = guide_colorbar(barheight = unit(0.4, "cm"), barwidth = unit(4, "cm"), direction = "horizontal")
      ) +
      geom_sf(data = canton_geo, fill = NA, color = "black", linewidth = 0.15) +
      geom_sf(data = lake_geo, fill = "#D6F1FF", color = NA) +
      theme_map()
    
    # Scatterplot
    p_scatter <- ggplot(df, aes(x = mean, y = .data[[value_col]])) +
      geom_point_interactive(
        aes(
          tooltip = paste0(municipality, "\n", round(.data[[value_col]], 2), label_unit, "\nØ Einkommen: ", mean),
          data_id = bfs_id,
          fill = Kantonskuerzel
        ),
        size = 5, alpha = 0.3, shape = 21, stroke = 0
      ) +
      scale_fill_viridis_d(option = "plasma", name = "Kanton") +
      scale_x_log10(labels = comma_format()) +
      geom_smooth_interactive(se = FALSE, method = "lm", color = "black", linetype = 2) +
      labs(
        x = "log(Ø Einkommen)",
        y = label_unit,
        caption = paste("Top 5 Kantone nach", label_unit, ":", paste(top_cantons, collapse = ", "))
      ) +
      theme_classic() +
      theme(
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.line = element_line(linewidth = 0.1),
        legend.position = "bottom",
        plot.background = element_rect(fill = "#e7dfdd", color = NA),
        panel.background = element_rect(fill = "#e7dfdd", color = NA),
        legend.background = element_rect(fill = "#e7dfdd", color = NA),
        panel.grid = element_blank(),
        plot.margin = unit(c(0, 2, 0, 2), "cm")
      )
    
    scatter_wrapped <- plot_spacer() + p_scatter + plot_spacer() +
      plot_layout(ncol = 3, widths = c(1, 2.5, 1))
    
    combined_plot <- p_map / scatter_wrapped +
      plot_layout(nrow = 2, heights = c(5, 1)) &
      theme(plot.background = element_rect(fill = "#e7dfdd", color = NA))
    # combined_plot <- p_map
    
    girafe(
      ggobj = combined_plot,
      bg = "#e7dfdd",
      width_svg = 20,
      height_svg = 15,
      options = list(
        opts_hover(css = "fill:red;stroke:none;stroke-width:2px;"),
        opts_selection(type = "none"),
        opts_toolbar(saveaspng = TRUE),
        opts_sizing(rescale = TRUE, width = 1),
        opts_zoom(min = 1, max = 8)
      )
    )
  })
}

# Launch app
shinyApp(ui, server)



