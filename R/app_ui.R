#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import leaflet
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    navbarPage(
      theme = shinythemes::shinytheme("flatly"),
      "",
      collapsible = TRUE,
      id = "navbar",
      
      tabPanel(
        "Home",
        tags$style(HTML(
          "body {
          background-image: url('www/farm-bg-fade-2.jpg');
    
          background-position: center center;
          
          background-repeat: no-repeat;
      
          background-attachment: fixed;
          
          background-size: cover;
    
          background-color: #464646;
          -webkit-background-size: cover;
          -moz-background-size: cover;
          -o-background-size: cover;
          
          }  
        
        
        h1, h2, h3, h4 {
          text-align: center; 
          margin-left: auto; 
          margin-right: auto;
          }
          "
        )),
        
        fluidRow(
          style = "min-height: 25%; min-height: 25vh;"
        ),
        
        fluidRow(
          tags$h1("map.landscape"),
          tags$br(),
          tags$h4("Quickly explore geospatial data collected using QField"),
          tags$br(),
          div(
            class = "align-middle text-center",
            actionButton("enter", htmltools::HTML("enter &#8674;")),
          ),
          style = "min-height: 25%; min-height: 25vh;"
        )
      ),
      
      tabPanel(
        "Data",
        waiter::use_waiter(),
        
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          h4("Sync Completed Forms"),
          
          actionButton("sync_forms", "Sync forms"),
          
          hr(style = "border-color: #2c3e50 !important;"),
          
          h4("Upload Data"),
          
          mod_get_layers_ui(id = "qfield_data", label = "Select .gpkg or .zip file(s)", multiple = TRUE, accept = c(".gpkg", ".zip", ".csv")),
          
          selectInput("active_layer", "Select active layer", choices = NULL),
          
          hr(style = "border-color: #2c3e50 !important;"),
          
          h4("Table Analysis"),
          
          selectInput(
            "analysis", "Select analysis",
            c("Summary Tables", "Combine Tables", "Combine Spatial Layers", "Filter Rows", "Add Column")
          ),
          
          conditionalPanel(
            condition = "input.analysis == 'Summary Tables'",
            
            h4("Summary Tables"),
            
            mod_multiple_input_ui(id = "grouping_var", label = "Grouping variable(s)"),
            
            mod_multiple_input_ui(id = "summarising_var", label = "Summarising variable(s)")
          ),
          
          conditionalPanel(
            condition = "input.analysis == 'Combine Tables'",
            
            h4("Combine Tables"),
            
            selectInput("table_left", label = "Select left table in join", choices = NULL),
            
            selectInput("table_right", label = "Select right table in join", choices = NULL),
            
            mod_multiple_input_ui(id = "joining_p_key_left", label = "Select primary key(s) - left table"),
            
            mod_multiple_input_ui(id = "joining_f_key_right", label = "Select foreign key(s) - right table"),
            
            radioButtons("key_join_type", "Join Type:",
                         c(
                           "column - inner" = "col_inner",
                           "column - left" = "col_left"
                         ),
                         selected = NULL
            ),
            
            textInput("join_tbl_name", "Table name", value = "", placeholder = "enter table name for output"),
            
            actionButton("table_join_button", "Join")
          ),
          
          conditionalPanel(
            shinyFeedback::useShinyFeedback(),
            condition = "input.analysis == 'Combine Spatial Layers'",
            
            h4("Combine Spatial Layers"),
            
            selectInput("spatial_table_left", label = "Select left table in join", choices = NULL),
            
            selectInput("spatial_table_right", label = "Select right table in join", choices = NULL),
            
            radioButtons("spatial_join_type", "Join Type:",
                         c(
                           "spatial - inner" = "spatial_inner",
                           "spatial - left" = "spatial_left"
                         ),
                         selected = NULL
            ),
            
            textInput("spjoin_tbl_name", "Table name", value = "", placeholder = "enter table name for output"),
            
            actionButton("spatial_join_button", "Join")
          ),
          
          conditionalPanel(
            shinyFeedback::useShinyFeedback(),
            condition = "input.analysis == 'Filter Rows'",
            
            h4("Filter Rows"),
            
            selectInput("table_filter", label = "Select table to filter", choices = NULL),
            
            actionButton("filter", "Filter Options"),
          ),
          
          conditionalPanel(
            shinyFeedback::useShinyFeedback(),
            condition = "input.analysis == 'Add Column'",
            
            h4("Add New Column"),
            
            selectInput("table_mutate", label = "Select table to add new column", choices = NULL),
            
            actionButton("add_column", "Add Column Options"),
          ),
        ),
        
        
        
        # show data tables
        mainPanel(tabsetPanel(
          type = "tabs",
          
          tabPanel(
            "Data: Raw",
            
            br(),
            
            downloadButton("download_data_raw", "Download Data"),
            
            hr(),
            
            div(style = "overflow-x:scroll; overflow-y:scroll", mod_render_dt_ui(id = "data_raw"))
          ),
          
          tabPanel(
            "Data: Summary",
            
            br(),
            
            downloadButton("download_data_summarised", "Download Summarised Data"),
            
            hr(),
            
            div(style = "overflow-x:scroll; overflow-y:scroll", mod_render_dt_ui(id = "data_summary"))
          )
        ))
      ),
      
      tabPanel(
        "Map",
        tags$style(
          type = "text/css",
          "#web_map {height: calc(100vh - 60px) !important; position: fixed; top: 60px; left: 0; right: 0; bottom: 0; padding: 0; overflow: hidden;}",
          "body {
            margin: 0;
            padding: 0;
          }",
          ".leaflet-popup-content-wrapper {background-color: #ecf0f1}",
          # fill map to height of container;  https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height/36471739#36471739
          ".leaflet-map-pane { z-index: auto; }",
          ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar, .js-irs-0 .irs-from, .js-irs-0 .irs-to, .irs-grid-pol {background: #2c3e50;}",
          ".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             }"
        ),
        
        shinyFeedback::useShinyFeedback(),
        leafletOutput("web_map"),
        
        absolutePanel(
          top = "73px",
          left = "55px",
          width = "250px",
          height = "80%",
          wellPanel(
            style = "padding: 5px !important; border-color: #2c3e50 !important;",
            checkboxInput("map_controls", "Map controls", value = FALSE, width = NULL),
            conditionalPanel(
              condition = "input.map_controls == true",
              
              actionButton("create_map", "draw map"),
              
              selectInput("map_active_layer", "Select active layer", choices = NULL),
              
              mod_single_input_ui(id = "map_var", label = "Select variable"),
              
              selectInput("map_colour", "Fill colour palette", choices = colour_mappings),
              
              sliderInput("opacity", "Opacity:",
                          min = 0, max = 1,
                          value = 0.8, step = 0.1
              ),
              # numericInput("opacity", "Opacity", 0.8, min = 0, max = 1, step = 0.1),
              
              numericInput("map_line_width", "Line width", 0.5, min = 0, max = 2),
              
              selectInput("map_line_colour", "Select line colour", choices = line_colours),
              
              helpText("Check box to display legend."),
              
              checkboxInput("legend", label = "Legend", value = FALSE),
              
              textInput("map_legend_title", "Legend title:", value = ""),
              
              mod_multiple_input_ui(id = "label_vars", label = "Popup labels")
            )
          )
        )
      ),
      
      tabPanel(
        "Charts",
        tags$style(
          type = "text/css",
          ".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar, .js-irs-1 .irs-from, .js-irs-1 .irs-to, .irs-grid-pol {background: #2c3e50;}"
        ),
        waiter::use_waiter(),
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          h4("Charts"),
          
          tags$hr(),
          
          selectInput("chart_active_layer", "Select active layer", choices = NULL),
          
          selectInput(
            "plotType", "Chart type",
            c("histogram", "scatter", "bar plot")
          ),
          
          sliderInput("chart_height", "Chart Height:",
                      min = 100, max = 1500,
                      value = 400, step = 100
          ),
          
          textInput("x_axis_label", "X-axis label", ""),
          
          textInput("y_axis_label", "Y-axis label", ""),
          
          numericInput("lab_font", "Axis label - text size",
                       min = 10, max = 36,
                       value = 14, step = 1
          ),
          
          numericInput("axis_font", "Axis value - text size",
                       min = 6, max = 35,
                       value = 10, step = 1
          ),
          
          conditionalPanel(
            condition = "input.plotType == 'histogram'",
            
            mod_single_input_ui(id = "hist_x_axis_var", label = "X-axis variable"),
            
            numericInput("binwidth", "Histogram bin width", 100)
          ),
          
          conditionalPanel(
            condition = "input.plotType == 'scatter'",
            
            mod_single_input_ui(id = "scatter_x_axis_var", label = "X-axis variable"),
            
            mod_single_input_ui(id = "scatter_y_axis_var", label = "Y-axis variable"),
            
            numericInput("scatter_point_size", "Point size",
                         min = 1, max = 20,
                         value = 3, step = 0.5
            )
          ),
          
          conditionalPanel(
            condition = "input.plotType == 'bar plot'",
            
            mod_single_input_ui(id = "col_grouping_var", label = "Grouping variable"),
            
            mod_single_input_ui(id = "col_summarising_var", label = "Summary variable"),
            
            radioButtons(
              "bar_plot_type", "Bar plot type:",
              c(
                "count" = "count_records",
                "sum" = "sum_values",
                "mean" = "mean"
              )
            )
          ),
          
          actionButton("create_chart", "draw chart"),
        ),
        
        mainPanel(
          plotOutput("chart", height = "auto")
        )
      ),
      
      tabPanel(
        "Documentation",
        fixedPage(
          tabsetPanel(
            tabPanel(
              "Sync Forms",
              tags$div(
                style = "padding-top: 10px;
                                padding-right: 10px;
                                padding-bottom: 10px;
                                padding-left: 10px;",
                
                tags$h2("Sync Forms", style = "text-align:left;"),
                
                tags$br(),
                
                tags$p("Use the following workflow to sync data collected using forms on multiple mobile devices, or collected on multiple dates, using QField."),
                
                tags$br(),
                
                tags$img(
                  src = "www/sync-forms.png",
                  width = "70%",
                  height = "auto",
                  style = "display: block;
                           margin-left: auto; 
                           margin-right: auto;
                           padding: 5px;
                           border: thin silver solid;"
                ),
                
                tags$br(),
                
                tags$h4("Template database", style = "text-align:left;"),
                
                tags$p("Select a template database (GeoPackage file - .gpkg) which will be used to sync newly collected data with. 
                       The template database contains the structure (schema) of the database that data collected in the field, using QField, will be combined with. 
                       For example, if the project you are using to collect data in QField has crop type, plot boundary, or farm management 
                       tables then the template should have corresponding tables where the data will be synced to."),
                
                tags$div(HTML("<p>Click the <em>Browse</em> button under <em>Template of central database</em>.</p>")),
                
                tags$br(),
                
                tags$img(
                  src = "www/select-template.png",
                  width = "50%",
                  height = "auto",
                  style = "display: block;
                           margin-left: auto; 
                           margin-right: auto;
                           padding: 5px;
                           border: thin silver solid;"
                ),
                
                tags$br(),
                
                tags$p("Typically, the template database will either be a database with no records (i.e. before you have collected data in the field and synced it) or a database with all previously collected data synced. 
                       You will then sync the data you have collected using QField with the data stored in the template and update it."),
                
                tags$br(),
                
                tags$h4("Forms database", style = "text-align:left;"),
                
                tags$p(
                  "forms databases are GeoPackage files (.gpkg) that store data collected using the QField mobile GIS. 
                  You can select one or more forms to sync with the template database. When you sync data collected using QField (forms) with the template, 
                  the app looks for matching tables or layers in each form and the template. 
                  If there is a match, the app inserts the data from the form into the corresponding table in template and removes any duplicate records."
                ),
                
                tags$div(HTML("<p>Click the <em>Browse</em> button under <em>Completed forms.</em></p>")),
                
                tags$br(),
                
                tags$img(
                  src = "www/select-forms.png",
                  width = "50%",
                  height = "auto",
                  style = "display: block;
                           margin-left: auto; 
                           margin-right: auto;
                           padding: 5px;
                           border: thin silver solid;"
                ),
                
                tags$br(),
                
                tags$div(HTML("<p>Clicking the <em>Download</em> button will download a date- and time-stamped GeoPackage storing the synced data.</p>")),
                
                tags$br(),
                
                tags$img(
                  src = "www/download-forms.png",
                  width = "50%",
                  height = "auto",
                  style = "display: block;
                           margin-left: auto; 
                           margin-right: auto;
                           padding: 5px;
                           border: thin silver solid;"
                ),
                
                tags$br(),
                
                tags$div(HTML("<p>Clicking the <em>go to app</em> button will take you into the dashboard where you can explore the synced data.</p>")),
                
                tags$br(),
                
                tags$img(
                  src = "www/go-to-app.png",
                  width = "50%",
                  height = "auto",
                  style = "display: block;
                           margin-left: auto; 
                           margin-right: auto;
                           padding: 5px;
                           border: thin silver solid;"
                ),
                
                hr(style = "border-color: #2c3e50 !important;")
              )
            ),
            tabPanel(
              "Summary Tables"
            ),
            tabPanel(
              "Combine Tables"
            ),
            tabPanel(
              "Combine Spatial Layers",
              tags$div(
                style = "padding-top: 10px;
                                padding-right: 10px;
                                padding-bottom: 10px;
                                padding-left: 10px;",
                
                tags$h2("Combine Spatial Layers", style = "text-align:left;"),
                
                tags$br(),
                
                tags$div(HTML("<em>Combine spatial layers</em> provides tools for performing spatial joins. 
                       A spatial join allows you to combine attributes from two spatial layers based upon the feature’s relationship in space.")),
                
                tags$br(),
                
                tags$p("A spatial join could be useful in the following scenario: 
                        you have been mapping farms using QField and have a spatial layer storing the outline of fields and attributes about each field 
                        (e.g. farmer name, fertiliser use etc.). 
                        You might want to combine your map of field outlines with another spatial layer 
                       (e.g. tax allotment, village boundaries, census boundaries). 
                       This would involve assigning attributes in your village boundary layer 
                       (e.g. village name, village id) to fields whose outline overlaps with the village extent."),
                
                tags$br(),
                
                tags$p("In this dashboard features are joined based on the largest intersection (largest overlap) between two features. 
                       If 30% of a field overlapped with Village A and 70% of a field overlapped with Village B, 
                       the field would be assigned the attributes of Village B. Another way to think of this is: 
                       the field is getting the columns from the table associated with village boundaries with the values coming 
                       from the row in village boundaries table associated with the village with the largest overlap with the field."),
                
                tags$br(),
                
                tags$p("The following diagram illustrates the process of performing a spatial join using the field-to-village example."),
                
                tags$br(),
                
                tags$img(
                  src = "www/spatial-join.png",
                  width = "70%",
                  height = "auto",
                  style = "display: block;
                           margin-left: auto; 
                           margin-right: auto;
                           padding: 5px;
                           border: thin silver solid;"
                ),
                
                tags$br(),
                
                tags$div(HTML("Use the <em>Table analysis</em> tools to select the <em>Combine Spatial Layers</em> option.")),
                
                tags$br(),
                
                tags$img(
                  src = "www/select-analysis-spatial-intersection.png",
                  width = "50%",
                  height = "auto",
                  style = "display: block;
                           margin-left: auto; 
                           margin-right: auto;
                           padding: 5px;
                           border: thin silver solid;"
                ),
                
                tags$br(),
                
                tags$div(HTML("Then select the two tables you want to join in the <em>Select left table in join</em> and <em>Select right table join</em> dropdown lists. 
                              The columns from the right table will be appended to the right of the left table.")),
                
                tags$br(),
                
                tags$img(
                  src = "www/select-tables-in-join.png",
                  width = "50%",
                  height = "auto",
                  style = "display: block;
                           margin-left: auto; 
                           margin-right: auto;
                           padding: 5px;
                           border: thin silver solid;"
                ),
                
                tags$br(),
                
                tags$div(HTML("There are two types of join. A <em>spatial – inner</em> join will keep only feature in the left table where there is an overlap with a feature in the right table. 
                              A <em>spatial – left</em> join will keep all features in the left table and return an NA in columns from the right table where there is no overlap.")),
                
                tags$div(HTML("Click the <em>Join</em> button to join the two tables. The result of the Join will appear as a table that can be selected under <em>Select active layer</em> called joined table")),
                
                tags$br(),
                
                tags$img(
                  src = "www/spatial-join-button.png",
                  width = "50%",
                  height = "auto",
                  style = "display: block;
                           margin-left: auto; 
                           margin-right: auto;
                           padding: 5px;
                           border: thin silver solid;"
                ),
                
                tags$br(),
                
                tags$div(HTML("<b>When is this useful?</b> Let’s keep going with the farm mapping example. 
                              You might want to compute the area of different crop types grown in each village. 
                              If you join your spatial layer of fields (with crop type attributes) to a village boundary layer (with a village name attribute), 
                              you can use the table that is returned from the join to compute the area of each crop in each village.")),
                
                tags$br(),
                
                tags$div(HTML("<b>Where to get spatial layers from?</b> 
                              Using the Browse button under <em>Upload data to explore</em> you can add multiple spatial layers to the app that you can use in spatial joins.")),
                
                tags$br(),
                
                tags$img(
                  src = "www/upload-data.png",
                  width = "50%",
                  height = "auto",
                  style = "display: block;
                           margin-left: auto; 
                           margin-right: auto;
                           padding: 5px;
                           border: thin silver solid;"
                ),
                
                hr(style = "border-color: #2c3e50 !important;")
              ) 
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "maplandscape"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

