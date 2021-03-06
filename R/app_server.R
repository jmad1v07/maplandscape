#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import leaflet
#' @importFrom dplyr select_if
#'
#' @noRd
#'

options(shiny.maxRequestSize = 5000 * 1024^2)

app_server <- function(input, output, session) {
  # move to data tab when action button pressed
  observeEvent(input$enter, {
    updateNavbarPage(session, "navbar", selected = "Data")
  })
  
  
  # Data Sync ---------------------------------------------------------------
  
  # sync forms with database / template
  sync_waiter <- waiter::Waiter$new(
    html = sync_screen,
    color = "rgba(44,62,80,.6)"
  )
  
  # select template db to sync forms to
  template <- mod_get_layers_server(id = "template_db")
  
  # forms to sync to template db
  forms <- mod_get_layers_server(id = "forms_db")
  
  # returns 4 element list
  # element 1 is file name and path to temporary geopackage
  # element 2 is date time string for creation of temporary geopackage
  # element 3 is a data frame in the same format as returned by fileUpload
  # element 4 is a log file
  gpkg_path <- reactive({
    req(template(), forms())
    
    sync_waiter$show()
    gpkg_path <- sync_forms(template = template(), forms = forms())
    sync_waiter$hide()
    gpkg_path
  })
  
  # download raw synced data as a zip file
  output$download_sync_forms <- downloadHandler(
    filename = function() {
      req(gpkg_path()[[1]])
      
      paste("synced_forms_", gpkg_path()[[2]], ".zip", sep = "")
    },
    content = function(file) {
      req(gpkg_path()[[1]])
      
      zip(
        zipfile = file,
        files = c(gpkg_path()[[1]], gpkg_path()[[4]]),
        flags = "-r9Xj"
      )
    },
    contentType = "application/zip"
  )
  
  # sync forms modal
  observeEvent(input$sync_forms, {
    showModal(
      modalDialog(
        tags$h4("Template or central database"),
        mod_get_layers_ui(
          id = "template_db",
          label = "Select template .gpkg",
          multiple = FALSE,
          accept = c(".gpkg")
        ),
        tags$h4("Completed forms"),
        mod_get_layers_ui(
          id = "forms_db",
          label = "Select forms .gpkg",
          multiple = TRUE,
          accept = c(".gpkg")
        ),
        hr(),
        checkboxInput("add_synced_forms", label = "add synced forms to active layer", value = TRUE),
        downloadButton("download_sync_forms", "Download"),
        hr(),
        modalButton("Go to app"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  # Data Upload -------------------------------------------------------------
  
  # Upload data and select active layer
  # reactiveValues object to hold dataframe of layers a user can select as the active layer
  data_file <-
    reactiveValues(
      data_file = data.frame(),
      map_drawn = 0,
      joined_df = list()
    )
  
  # add synced files to app
  sync_file <- reactive({
    req(gpkg_path()[[1]], input$add_synced_forms)
    
    sync_file <- gpkg_path()[[3]]
    sync_file
  })
  
  # update reactiveValues object holding dataframe of layers a user can select as active layer
  observe({
    req(sync_file())
    
    sync_file <- isolate(sync_file())
    isolate({
      df <- dplyr::bind_rows(data_file$data_file, sync_file)
      # unique number id next to each layer to catch uploads of tables with same name
      rows <- nrow(df)
      row_idx <- 1:rows
      df$layer_disp_name_idx <-
        paste0(df$layer_disp_name, "_", row_idx, sep = "")
      data_file$data_file <- df
    })
  })
  
  # user uploaded files
  # return table of files and file paths of data loaded to the server
  upload_file <- mod_get_layers_server(id = "qfield_data")
  
  # update reactiveValues object holding dataframe of layers a user can select as active layer
  observe({
    req(upload_file())
    
    upload_file <- isolate(upload_file())
    isolate({
      df <- dplyr::bind_rows(data_file$data_file, upload_file)
      # unique number id next to each layer to catch uploads of tables with same name
      rows <- nrow(df)
      row_idx <- 1:rows
      df$layer_disp_name_idx <-
        paste0(df$layer_disp_name, "_", row_idx, sep = "")
      data_file$data_file <- df
    })
  })
  
  # select one table as active layer from files loaded to the server
  observe({
    df <- data_file$data_file
    joined_df <- data_file$joined_df
    nm_jdf <- names(joined_df)
    choices <- unique(df$layer_disp_name_idx)
    choices <- c(choices, nm_jdf)
    updateSelectInput(session, "active_layer", choices = choices)
  })
  
  # active df - use this df for summarising and generating raw tables for display
  active_df <- reactive({
    req(input$active_layer)
    
    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))
    
    if (any(jdf == input$active_layer)) {
      active_df <- data_file$joined_df[[input$active_layer]]
    } else {
      active_df <- read_tables(df, input$active_layer)
    }
    
    active_df
  })
  
  # render active df as raw data table
  mod_render_dt_server(id = "data_raw", dt = active_df)
  
  # Summary Tables ----------------------------------------------------------
  
  # Select input for grouping and summarising variables
  grouping_vars <-
    mod_multiple_input_server(id = "grouping_var", m_df = active_df)
  
  # filter out selected grouping variables in list of variables which can be summarised
  s_active_df <- reactive({
    req(active_df(), grouping_vars())
    
    tmp_df <- active_df() %>%
      select_if(is.numeric)
    choices <- names(tmp_df)
    s_intersect <- intersect(choices, grouping_vars())
    choices <- choices[!choices %in% s_intersect]
    
    choices
  })
  
  summarising_vars <-
    mod_multiple_input_server(id = "summarising_var", m_df = s_active_df)
  
  # perform group by and summarise operation
  summarised_df <- reactive({
    req(active_df())
    
    summarised_df <-
      group_by_summarise(active_df(), grouping_vars(), summarising_vars())
    
    summarised_df
  })
  
  # render summarised_df as data table
  mod_render_dt_server(id = "data_summary", dt = summarised_df)
  
  # Joining Tables ----------------------------------------------------------
  
  observe({
    df <- data_file$data_file
    joined_df <- data_file$joined_df
    choices <- unique(df$layer_disp_name_idx)
    nm_jdf <- names(joined_df)
    choices <- c(choices, nm_jdf)
    data_file$table_left <- choices
    
    updateSelectInput(session, "table_left", choices = choices)
  })
  
  left_df <- reactive({
    req(input$table_left)
    
    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))
    
    if (any(jdf == input$table_left)) {
      left_df <- isolate(data_file$joined_df[[input$table_left]])
    } else {
      left_df <- read_tables(df, input$table_left)
    }
    
    left_df
  })
  
  observe({
    df <- data_file$table_left
    
    choices <- unique(df)
    choices <- choices[choices != input$table_left]
    
    updateSelectInput(session, "table_right", choices = choices)
  })
  
  right_df <- reactive({
    req(input$table_right)
    
    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))
    
    if (any(jdf == input$table_right)) {
      right_df <- isolate(data_file$joined_df[[input$table_right]])
    } else {
      right_df <- read_tables(df, input$table_right)
    }
    
    right_df
  })
  
  # update select input for table left primary key
  p_key <-
    mod_multiple_input_server(id = "joining_p_key_left", m_df = left_df)
  
  # update select input for table right foreign key
  f_key <-
    mod_multiple_input_server(id = "joining_f_key_right", m_df = right_df)
  
  join_waiter <- waiter::Waiter$new(
    html = join_screen,
    color = "rgba(44,62,80,.6)"
  )
  
  # join left table to right table
  observeEvent(input$table_join_button, {
    req(left_df(), right_df(), input$key_join_type)
    
    join_waiter$show()
    if (input$key_join_type == "col_inner" |
        input$key_join_type == "col_left") {
      joined_table <-
        join_tables(
          left_df(),
          right_df(),
          input$key_join_type,
          p_key(),
          f_key()
        )
    }
    join_waiter$hide()
    
    data_file$joined_df[[input$join_tbl_name]] <- joined_table
  })
  
  # select tables for spatial joins
  observe({
    df <- data_file$data_file
    joined_df <- data_file$joined_df
    choices <- unique(df$layer_disp_name_idx)
    nm_jdf <- names(joined_df)
    choices <- c(choices, nm_jdf)
    data_file$spatial_table_left <- choices
    
    updateSelectInput(session, "spatial_table_left", choices = choices)
  })
  
  spatial_left_df <- reactive({
    req(input$spatial_table_left)
    
    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))
    
    if (any(jdf == input$spatial_table_left)) {
      left_df <- isolate(data_file$joined_df[[input$spatial_table_left]])
    } else {
      left_df <- read_tables(df, input$spatial_table_left)
    }
    
    shinyFeedback::feedbackWarning(
      "spatial_table_left",
      !("sf" %in% class(left_df)),
      "Not a spatial layer"
    )
    
    left_df
  })
  
  observe({
    req(input$spatial_table_left)
    df <- data_file$spatial_table_left
    
    choices <- unique(df)
    choices <- choices[choices != input$spatial_table_left]
    
    updateSelectInput(session, "spatial_table_right", choices = choices)
  })
  
  spatial_right_df <- reactive({
    req(input$spatial_table_right)
    
    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))
    
    if (any(jdf == input$spatial_table_right)) {
      right_df <-
        isolate(data_file$joined_df[[input$spatial_table_right]])
    } else {
      right_df <- read_tables(df, input$spatial_table_right)
    }
    
    shinyFeedback::feedbackWarning(
      "spatial_table_right",
      !("sf" %in% class(right_df)),
      "Not a spatial layer"
    )
    
    right_df
  })
  
  spatial_join_waiter <- waiter::Waiter$new(
    html = join_screen,
    color = "rgba(44,62,80,.6)"
  )
  
  # join left table to right table
  observeEvent(input$spatial_join_button, {
    req(
      spatial_left_df(),
      spatial_right_df(),
      "sf" %in% class(spatial_left_df()),
      "sf" %in% class(spatial_right_df()),
      input$spatial_join_type
    )
    
    spatial_join_waiter$show()
    if ("sf" %in% class(spatial_left_df()) &
        "sf" %in% class(spatial_right_df()) &
        input$spatial_join_type == "spatial_inner" |
        input$spatial_join_type == "spatial_left") {
      joined_table <-
        spatial_join_tables(
          spatial_left_df(),
          spatial_right_df(),
          input$spatial_join_type
        )
    }
    spatial_join_waiter$hide()
    
    data_file$joined_df[[input$spjoin_tbl_name]] <- joined_table
  })
  
  # Filter Rows -------------------------------------------------------------
  
  # filter modal
  observeEvent(input$filter, {
    showModal(
      modalDialog(
        tags$h4("Filter Options"),
        textInput(inputId = "filter_conditions", label = "Conditions to filter rows"),
        textInput(
          inputId = "filter_tbl_name",
          "Table name",
          value = "",
          placeholder = "enter table name for output"
        ),
        tags$p(
          "DEMO SNIPPET:"
        ),
        tags$code(
          "crop == 'dalo'"
        ),
        tags$p(
          "Filter conditions must be specified using dplyr syntax. Some tips:"
        ),
        tags$ul(
          tags$li("Quotes for strings - \"string\""),
          tags$li("Escape apostrophes within strings - \"vava\\'u\""),
          tags$li("Specify column names without quotes"),
          tags$li("== - equal to"),
          tags$li("!= - not equal to"),
          tags$li("<, >, <=, >= - greater than / less than comparisons"),
          tags$li("& - and"),
          tags$li("| - or")
        ),
        tags$p("Example: crop_number > 25"),
        tags$p("Example: island == \"vava\'u\""),
        actionButton("execute_filter", "Filter"),
        modalButton("close"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  
  # select tables for row filtering
  observe({
    df <- data_file$data_file
    joined_df <- data_file$joined_df
    choices <- unique(df$layer_disp_name_idx)
    nm_jdf <- names(joined_df)
    choices <- c(choices, nm_jdf)
    
    updateSelectInput(session, "table_filter", choices = choices)
  })
  
  
  filter_df <- reactive({
    req(input$table_filter)
    
    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))
    
    if (any(jdf == input$table_filter)) {
      filter_df <- isolate(data_file$joined_df[[input$table_filter]])
    } else {
      filter_df <- read_tables(df, input$table_filter)
    }
    
    filter_df
  })
  
  # execute filter and add filtered table to active layers
  observeEvent(input$execute_filter, {
    req(filter_df())
    req(input$filter_conditions)
    
    filter_df <- isolate(filter_df())
    
    filter_expr <- tryCatch(
      error = function(cnd) {
        NULL
      },
      {
        filter_expr <-
          call2(
            dplyr::filter,
            rlang::parse_expr("filter_df"),
            rlang::parse_expr(input$filter_conditions)
          )
      }
    )
    
    filter_out <- tryCatch(
      error = function(cnd) {
        NULL
      },
      {
        filter_out <- eval(filter_expr)
      }
    )
    
    if (length(filter_out) > 0) {
      data_file$joined_df[[input$filter_tbl_name]] <- filter_out
    }
  })
  
  # Add Columns -------------------------------------------------------------
  
  # add column modal
  observeEvent(input$add_column, {
    showModal(
      modalDialog(
        tags$h4("Add New Column"),
        textInput(inputId = "col_name", label = "New column name"),
        textInput(inputId = "mutate_conditions", label = "Function to add new column"),
        tags$p(
          "DEMO SNIPPET:"
        ),
        tags$code(
          "area.x * (area.y / 100)"
        ),
        tags$p("Function to add new column must use dplyr syntax."),
        tags$p("Example: acres * 4046.86"),
        tags$p("Example: tree_number > 0"),
        actionButton("execute_mutate", "Create column"),
        modalButton("close"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  # select table to add column to
  observe({
    df <- data_file$data_file
    joined_df <- data_file$joined_df
    choices <- unique(df$layer_disp_name_idx)
    nm_jdf <- names(joined_df)
    choices <- c(choices, nm_jdf)
    
    updateSelectInput(session, "table_mutate", choices = choices)
  })
  
  mutate_df <- reactive({
    req(input$table_mutate)
    
    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))
    
    if (any(jdf == input$table_mutate)) {
      mutate_df <- isolate(data_file$joined_df[[input$table_mutate]])
    } else {
      mutate_df <- read_tables(df, input$table_mutate)
    }
    
    mutate_df
  })
  
  # execute mutate and add column to selected table
  observeEvent(input$execute_mutate, {
    req(mutate_df())
    req(input$mutate_conditions)
    mutate_df <- isolate(mutate_df())
    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))
    col_name <- input$col_name
    
    mutate_expr <- tryCatch(
      error = function(cnd) {
        NULL
      },
      {
        mutate_expr <-
          call2(
            dplyr::mutate,
            rlang::parse_expr("mutate_df"),
            !!col_name := parse_expr(input$mutate_conditions)
          )
      }
    )
    
    mutate_out <- tryCatch(
      error = function(cnd) {
        NULL
      },
      {
        mutate_out <- eval(mutate_expr)
      }
    )
    
    if (length(mutate_out) > 0) {
      if (any(jdf == input$table_mutate)) {
        data_file$joined_df[[input$table_mutate]] <- mutate_out
      } else {
        tryCatch(
          error = function(cnd) {
            print("problem")
          },
          {
            a_lyr <- df %>%
              dplyr::filter(layer_disp_name_idx == input$table_mutate)
            layer <- a_lyr$layers
            sf::st_write(
              mutate_out,
              dsn = a_lyr$file_path,
              layer = layer,
              append = FALSE
            )
          }
        )
      }
    }
  })
  
  # Data Download -----------------------------------------------------------
  
  # Date stamp for downloading files
  dt <- reactive({
    d <- Sys.time()
    d <- stringr::str_replace_all(d, ":", "-")
    d <- stringr::str_replace(d, " ", "-")
    d
  })
  
  # download raw data
  output$download_data_raw <- downloadHandler(
    filename = function() {
      paste("raw_data_", dt(), ".csv", sep = "")
    },
    content = function(file) {
      req(active_df())
      
      readr::write_csv(active_df(), file)
    }
  )
  
  # download summarised data
  output$download_data_summarised <- downloadHandler(
    filename = function() {
      paste("summarised_data_", dt(), ".csv", sep = "")
    },
    content = function(file) {
      req(summarised_df())
      
      readr::write_csv(summarised_df(), file)
    }
  )
  
  # Web Map -----------------------------------------------------------------
  
  # Map options
  # update select input for mapping layer
  observe({
    df <- data_file$data_file
    joined_df <- data_file$joined_df
    nm_jdf <- names(joined_df)
    choices <- unique(df$layer_disp_name_idx)
    choices <- c(choices, nm_jdf)
    updateSelectInput(session, "map_active_layer", choices = choices)
  })
  
  # active df - use this df for rendering on web map
  map_active_df <- reactive({
    req(input$map_active_layer)
    
    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))
    
    if (any(jdf == input$map_active_layer)) {
      map_active_df <-
        isolate(data_file$joined_df[[input$map_active_layer]])
    } else {
      map_active_df <- read_tables(df, input$map_active_layer)
    }
    
    if (nrow(map_active_df) > 0) {
      map_active_df$layer_id <- as.character(1:nrow(map_active_df))
    }
    
    if (nrow(map_active_df) > 5000) {
      map_active_df <- map_active_df[1:5000, ]
      id <-
        showNotification(
          "Only drawing first 5000 features!",
          duration = 5,
          type = c("warning")
        )
    }
    
    # show warning if map active layer has no records
    shinyFeedback::feedbackWarning(
      "map_active_layer",
      !(nrow(map_active_df) > 0),
      "Not updating options - no records in selected table"
    )
    
    # show warning if map active layer is not spatial (class sf)
    shinyFeedback::feedbackWarning(
      "map_active_layer",
      !("sf" %in% class(map_active_df)),
      "Not updating options - not a spatial layer"
    )
    
    # don't update options if selected layer has no records
    req(nrow(map_active_df) > 0, "sf" %in% class(map_active_df))
    
    map_active_df
  })
  
  map_var <-
    mod_single_input_server(id = "map_var", s_df = map_active_df)
  
  label_vars <-
    mod_multiple_input_server(id = "label_vars", m_df = map_active_df)
  
  # Create web map
  output$web_map <- leaflet::renderLeaflet({
    base_map <- leaflet::leaflet() %>%
      leaflet::addTiles(group = "OSM (default)") %>%
      leaflet::addProviderTiles(
        providers$Esri.WorldImagery,
        options = providerTileOptions(maxZoom = 17),
        group = "ESRI Satellite"
      ) %>%
      leaflet::setView(0, 0, 3) %>%
      leaflet::addLayersControl(
        baseGroups = c("OSM (default)", "ESRI Satellite"),
        options = leaflet::layersControlOptions(collapsed = FALSE),
        position = c("bottomright")
      ) %>%
      leaflet::addMeasure(
        position = "bottomright",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479"
      )
    
    base_map
  })
  
  map_waiter <- waiter::Waiter$new(
    html = map_screen,
    color = "rgba(44,62,80,.6)"
  )
  
  # add spatial data to map
  observeEvent(input$create_map, {
    req(map_active_df())
    
    # map_active_df <- isolate(map_active_df())
    
    if ("sf" %in% class(map_active_df()) &
        is.atomic(map_active_df()[[map_var()]]) &
        nrow(map_active_df()) > 0) {
      data_file$map_drawn <- 1
      print(data_file$map_drawn)
      add_layers_leaflet(
        map_object = "web_map",
        map_active_df = map_active_df(),
        map_var = map_var(),
        map_colour = input$map_colour,
        opacity = input$opacity,
        map_line_width = input$map_line_width,
        map_line_colour = input$map_line_colour,
        waiter = map_waiter
      )
    }
  })
  
  # update opacity
  observeEvent(input$opacity, {
    req(map_active_df())
    
    if (data_file$map_drawn == 1) {
      if ("sf" %in% class(map_active_df()) &
          is.atomic(map_active_df()[[map_var()]]) &
          nrow(map_active_df()) > 0) {
        add_layers_leaflet_no_zoom(
          map_object = "web_map",
          map_active_df = map_active_df(),
          map_var = map_var(),
          map_colour = input$map_colour,
          opacity = input$opacity,
          map_line_width = input$map_line_width,
          map_line_colour = input$map_line_colour,
          waiter = map_waiter
        )
      }
    }
  })
  
  # update colour
  observeEvent(input$map_colour, {
    req(map_active_df())
    
    if (data_file$map_drawn == 1) {
      if ("sf" %in% class(map_active_df()) &
          is.atomic(map_active_df()[[map_var()]]) &
          nrow(map_active_df()) > 0) {
        add_layers_leaflet_no_zoom(
          map_object = "web_map",
          map_active_df = map_active_df(),
          map_var = map_var(),
          map_colour = input$map_colour,
          opacity = input$opacity,
          map_line_width = input$map_line_width,
          map_line_colour = input$map_line_colour,
          waiter = map_waiter
        )
      }
    }
  })
  
  # add popup labels
  observe({
    leaflet::leafletProxy("web_map") %>% clearPopups()
    
    # capture click events
    # event_shape captures a user click on a shape object
    # event_marker captures a user click on a marker object
    event_shape <- input$web_map_shape_click
    event_marker <- input$web_map_marker_click
    
    # if a user has not clicked on a marker or object leave event as null if a
    # user has clicked on a shape or marker update event and pass it into
    # fct_add_popups to create popup for clicked object
    event <- NULL
    
    if (!is.null(event_shape)) {
      event <- event_shape
    }
    
    if (!is.null(event_marker)) {
      event <- event_marker
    }
    
    if (is.null(event)) {
      return()
    }
    
    isolate({
      req(label_vars())
      
      content <-
        add_popups(
          in_df = map_active_df,
          layer_id = event$id,
          label_vars = label_vars
        )
      print(content)
      
      leaflet::leafletProxy("web_map") %>%
        leaflet::addPopups(event$lng, event$lat, content, layerId = event$id)
    })
  })
  
  # add legend on top of leaflet object
  observe({
    req(map_active_df())
    
    if ("sf" %in% class(map_active_df()) &
        is.atomic(map_active_df()[[map_var()]]) &
        nrow(map_active_df()) > 0) {
      # make map active layer epsg 4326
      # make this an if statement
      map_df <- map_active_df() %>%
        sf::st_transform(4326)
      
      bbox <- sf::st_bbox(map_df) %>%
        as.vector()
      
      if (class(map_df[[map_var()]]) != "numeric" &
          class(map_df[[map_var()]]) != "integer") {
        pal <- leaflet::colorFactor(input$map_colour, map_df[[map_var()]])
      } else {
        pal <- leaflet::colorNumeric(input$map_colour, map_df[[map_var()]])
      }
      
      if (input$legend == TRUE) {
        leaflet::leafletProxy("web_map") %>%
          leaflet::clearControls() %>%
          leaflet::addLegend(
            pal = pal,
            values = map_df[[map_var()]],
            position = "topright",
            title = input$map_legend_title
          )
      } else {
        leaflet::leafletProxy("web_map") %>%
          leaflet::clearControls()
      }
    }
  })
  
  
  # Charts ------------------------------------------------------------------
  
  resize_waiter <- waiter::Waiter$new(
    html = resize_screen,
    color = "rgba(44,62,80,.6)"
  )
  
  # Map options
  # update select input for mapping layer
  observe({
    df <- data_file$data_file
    joined_df <- data_file$joined_df
    nm_jdf <- names(joined_df)
    choices <- unique(df$layer_disp_name_idx)
    choices <- c(choices, nm_jdf)
    updateSelectInput(session, "chart_active_layer", choices = choices)
  })
  
  # active df - use this df for rendering chart
  chart_active_df <- reactive({
    req(input$chart_active_layer)
    
    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))
    
    if (any(jdf == input$chart_active_layer)) {
      chart_active_df <-
        isolate(data_file$joined_df[[input$chart_active_layer]])
    } else {
      chart_active_df <- read_tables(df, input$chart_active_layer)
    }
    
    head(chart_active_df)
    chart_active_df
  })
  
  # histogram variable selection
  hist_choices <- reactive({
    req(chart_active_df())
    
    tmp_df <- chart_active_df() %>%
      select_if(is.numeric)
    choices <- names(tmp_df)
    print(choices)
    
    choices
  })
  
  hist_x_axis_vars <-
    mod_single_input_server(id = "hist_x_axis_var", s_df = hist_choices)
  
  # scatter plot variable selection
  scatter_choices <- reactive({
    req(chart_active_df())
    
    tmp_df <- chart_active_df() %>%
      select_if(is.numeric)
    choices <- names(tmp_df)
    print(choices)
    
    choices
  })
  
  scatter_x_axis_vars <-
    mod_single_input_server(id = "scatter_x_axis_var", s_df = scatter_choices)
  
  scatter_y_axis_vars <-
    mod_single_input_server(id = "scatter_y_axis_var", s_df = scatter_choices)
  
  # bar plot variables
  col_grouping_var <-
    mod_single_input_server(id = "col_grouping_var", s_df = chart_active_df)
  
  # filter out selected grouping variables in list of variables which can be summarised
  col_active_df <- reactive({
    req(chart_active_df(), col_grouping_var())
    
    tmp_df <- chart_active_df() %>%
      select_if(is.numeric)
    choices <- names(tmp_df)
    s_intersect <- intersect(choices, col_grouping_var())
    choices <- choices[!choices %in% s_intersect]
    
    choices
  })
  
  col_summarising_var <-
    mod_single_input_server(id = "col_summarising_var", s_df = col_active_df)
  
  # perform group by and summarise operation for bar plot
  col_summarised_df <- reactive({
    req(chart_active_df())
    chart_data()
    
    col_summarising_var <- isolate(col_summarising_var())
    col_grouping_var <- isolate(col_grouping_var())
    
    if (input$bar_plot_type == "count_records") {
      col_summarising_var <- NULL
    }
    
    col_group_df <-
      group_by_summarise(chart_active_df(), col_grouping_var, col_summarising_var)
    
    col_group_df
  })
  
  # make chart take reactive dependency on action button
  chart_data <- eventReactive(input$create_chart, {
    print("draw chart")
  })
  
  output$chart <- renderPlot(
    {
      chart_data()
      
      lab_font_size <- isolate(input$lab_font)
      axis_font_size <- isolate(input$axis_font)
      x_lab <- isolate(input$x_axis_label)
      y_lab <- isolate(input$y_axis_label)
      chart_type <- isolate(input$plotType)
      bar_plot_type <- isolate(input$bar_plot_type)
      
      if (chart_type == "histogram") {
        binwidth <- isolate(input$binwidth)
        hist_x_var <- isolate(hist_x_axis_vars())
        
        chart <-
          ggplot2::ggplot(isolate(chart_active_df()), ggplot2::aes(.data[[hist_x_var]])) +
          ggplot2::geom_histogram(
            binwidth = binwidth,
            color = "#2c3e50",
            fill = "#2c3e50"
          ) +
          ggplot2::xlab(x_lab) +
          ggplot2::ylab(y_lab) +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = NA, colour = NA),
            panel.background = ggplot2::element_rect(fill = NA, colour = "#2c3e50"),
            axis.text.x = ggplot2::element_text(
              angle = -45,
              vjust = 1,
              hjust = 0,
              size = axis_font_size
            ),
            axis.text.y = ggplot2::element_text(size = axis_font_size),
            axis.title.x = ggplot2::element_text(size = lab_font_size),
            axis.title.y = ggplot2::element_text(size = lab_font_size)
          )
      } else if (chart_type == "scatter") {
        scatter_x_var <- isolate(scatter_x_axis_vars())
        scatter_y_var <- isolate(scatter_y_axis_vars())
        point <- isolate(input$scatter_point_size)
        
        chart <-
          ggplot2::ggplot(
            isolate(chart_active_df()),
            ggplot2::aes(.data[[scatter_x_var]], .data[[scatter_y_var]])
          ) +
          ggplot2::geom_point(color = "#2c3e50", size = point) +
          ggplot2::xlab(x_lab) +
          ggplot2::ylab(y_lab) +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = NA, colour = NA),
            panel.background = ggplot2::element_rect(fill = NA, colour = "#2c3e50"),
            axis.text.x = ggplot2::element_text(
              angle = -45,
              vjust = 1,
              hjust = 0,
              size = axis_font_size
            ),
            axis.text.y = ggplot2::element_text(size = axis_font_size),
            axis.title.x = ggplot2::element_text(size = lab_font_size),
            axis.title.y = ggplot2::element_text(size = lab_font_size)
          )
      } else if (chart_type == "bar plot") {
        bar_x_var <- isolate(col_summarised_df()[, 1])
        
        if (bar_plot_type == "count_records") {
          bar_y_var <- isolate(col_summarised_df()[, 2])
        } else if (bar_plot_type == "sum_values") {
          bar_y_var <- isolate(col_summarised_df()[, 3])
        } else if (bar_plot_type == "mean") {
          bar_y_var <- isolate(col_summarised_df()[, 2])
        }
        
        col_chart_df <- data.frame(bar_x_var, bar_y_var)
        chart <-
          ggplot2::ggplot(
            col_chart_df,
            ggplot2::aes(col_chart_df[, 1], col_chart_df[, 2])
          ) +
          ggplot2::geom_col(color = "#2c3e50", fill = "#2c3e50") +
          ggplot2::xlab(x_lab) +
          ggplot2::ylab(y_lab) +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = NA, colour = NA),
            panel.background = ggplot2::element_rect(fill = NA, colour = "#2c3e50"),
            axis.text.x = ggplot2::element_text(
              angle = -45,
              vjust = 1,
              hjust = 0,
              size = axis_font_size
            ),
            axis.text.y = ggplot2::element_text(size = axis_font_size),
            axis.title.x = ggplot2::element_text(size = lab_font_size),
            axis.title.y = ggplot2::element_text(size = lab_font_size)
          )
      }
      
      chart
    },
    height = function() {
      input$chart_height
    },
    bg = "transparent"
  )
  
}
