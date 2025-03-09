
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                       LIBRARY CHECK AND IMPORT
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Library list
libraries_to_install <- c(
  "rstudioapi",
  "shiny",
  "shinyjs",
  "shinythemes",
  "vegan",
  "factoextra",
  "cluster",
  "scales",
  "dplyr",
  "ggplot2",
  "qgraph",
  "corrplot",
  "plotly",
  "ggcorrplot",
  "reshape2",
  "shinycssloaders",
  "magrittr"
)
# Check and install libraries
check_and_install_library <- function(library_name) {
  if (!require(library_name, character.only = TRUE)) {
    install.packages(library_name, dependencies = TRUE)
    library(library_name, character.only = TRUE)
  } else {
    cat(paste("Library", library_name, "is already installed.\n"))
  }
}
# Run the function
lapply(libraries_to_install, check_and_install_library)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                    UI
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("GEMMA(Geo-EnvironMental Multivariate Analysis) toolbox"),
  fileInput("file", "Select a tidy dataset"),
  navbarPage("WORKFLOW",
             ## DATASET PANEL===================================================
             tabPanel("DATASET", 
                      tabsetPanel(
                        # Data matrix A: Response variablessub-panel____________
                        tabPanel("Data matrix A: Response variables", 
                                 fluidRow(
                                   column(2, div(
                                     style = "max-height: 60vh; overflow-y: scroll;", 
                                     checkboxGroupInput("show_columns_molecules", "", 
                                                        choices = NULL, 
                                                        selected = character(0)))), 
                                   column(10, div(
                                     style = "max-height: 60vh; overflow-y: scroll;", 
                                     dataTableOutput("file_table_molecules")))
                                 )#end fluidrow--
                        ),#end tabpanel--
                        
                        # PREDICTORS sub-panel__________________________________
                        tabPanel("Data Matrix B: Explanatory variables", 
                                 fluidRow(
                                   column(2, div(
                                     style = "max-height: 60vh; overflow-y: scroll;", 
                                     checkboxGroupInput("show_columns_proxies", "", 
                                                        choices = NULL, 
                                                        selected = character(0)))), 
                                   column(10, div(
                                     style = "max-height: 60vh; overflow-y: scroll;",
                                     dataTableOutput("file_table_proxies")))
                                 )#end fluidrow--
                        ),#end tabpanel--
                        
                        # COLLINEARITY ANALYSIS sub-panel_______________________
                        tabPanel("Colinearity Analysis",
                                 # SETUP
                                 fluidRow(
                                   column(width = 12, align = "center", 
                                          actionButton("start_coll_analysis", "START Collinearity Analysis"))),
                                 # TEXT HEADER
                                 fluidRow(
                                   column(width = 6, "ENVIRONMENTAL VARIABLES"),
                                   column(width = 6, "PREDICTORS")),
                                 
                                 # PLOTS
                                 fluidRow(
                                   column(width = 6, #"ENVIRONMENTAL VARIABLES", 
                                          fluidRow(width = 6, 
                                                   plotOutput("cmat_mole_plt", height="90vh")), 
                                          fluidRow(width = 6, 
                                                   plotOutput("cmat_mole_plt_pval", height="90vh")),),
                                   column(width = 6, #"PREDICTORS",
                                          fluidRow(width = 6, 
                                                   plotOutput("cmat_prox_plt", height="90vh")),
                                          fluidRow(width = 6, 
                                                   plotOutput("cmat_prox_plt_pval", height="90vh"))))
                        ),#end tabpanel--
                        
                        # TIME PLOT sub-panel___________________________________
                        tabPanel("Time plots",
                                 # MULTIPLOT
                                 fluidRow(
                                   column(width=2,
                                          # uiOutput("raw_time_sel"), # TIME variable drop-down
                                          uiOutput("raw_select")), # Raw data checklist
                                   column(width=10, 
                                          plotOutput("raw_all_vsTime", height="30vh"))),
                                 
                                 # RAW1 sv RAW2 sv TIME
                                 fluidRow(
                                   column(width = 2, 
                                          uiOutput("raw1_raw2_select")), # Raw data checklist
                                   column(width = 10,
                                          fluidRow(
                                            column(width = 12, 
                                                   plotOutput("raw1_raw2_vsTime", height="30vh"))),
                                          fluidRow(
                                            column(width = 12, 
                                                   plotlyOutput("raw1_raw2_vsTime_3D", height="70vh"))))
                                   )#end fluidRow--
                                 )#End tabPanel--
                      ),#end tabsetPanel--
             ),#end DATASE tabPanel--
             
             ## CLUSTER ANALYSIS PANEL =========================================
             tabPanel("CLUSTER ANALYSIS",
                      # SETUP___________________________________________________
                      column(width = 2,
                             # K NUMBER
                             selectInput("sbox_ca_method", "CA Method", 
                                         choices = list("Hierarchical" = 1, 
                                                        "K-Means" = 2, 
                                                        "HKmeans" = 3), 
                                         selected = 1),
                             print("GAP SETUP"),
                             textInput("gap_nstart", "Centroids", 5), 
                             textInput("gap_b", "Bootstraps", 5), 
                             actionButton("start_cluster_number", "Cluster number"),
                             # CLA
                             uiOutput("cla_setup")),
                      
                      # PLOTS___________________________________________________
                      column(width = 10, 
                             # wss, gap, silhouette plots
                             fluidRow(column(width = 4, plotOutput("wss_plot", height="20vh")),
                                      column(width = 4, plotOutput("gap_plot", height="20vh")),
                                      column(width = 4, plotOutput("sil_plot", height="20vh"))),
                             # clusters + silhuette plots
                             fluidRow(column(width = 6, plotOutput("ca_main_plot", height="50vh")),
                                      column(width = 6, plotOutput("ca_sil_plot", height="50vh"))))
                      ),#End tabpanel--
             
          
             ## PCA PANEL=======================================================
             tabPanel("PCA",
                      tabsetPanel(
                        # MAIN sub_panel________________________________________
                        tabPanel("Main",
                                 # PCA Setup
                                 column(width = 2,
                                        uiOutput("col_selector_pca"),
                                        column(width = 12, 
                                               actionButton("start_pca", "START PCA")),
                                        uiOutput("pca_setup2")),
                                 # SCREE PLOT & MONTECARLO PLOT
                                 column(width = 10,
                                        fluidRow(
                                          column(width = 6, plotOutput("pca_scree_plot", height="65vh")),
                                          column(width = 6, plotOutput("pca_mc_pvals", height="65vh"))))
                        ),#End tabpanel--
                        
                        # BIPLOTS sub-panel_____________________________________
                        tabPanel("Biplots",
                                 # Setup
                                 column(width = 2, uiOutput("pca_setup3")),
                                 # PLOTS
                                 column(width = 10,
                                        fluidRow(column(width = 12, plotOutput("pca_biplot_1", height="50vh"))),
                                        fluidRow(column(width = 12, plotOutput("pca_biplot_3", height="50vh"))),
                                        fluidRow(column(width = 12, plotOutput("pca_biplot_2", height="50vh"))),
                                        fluidRow(column(width = 12, plotlyOutput("pca_plot_3d", height="100vh"))))
                                 ),#End tabpanel--
                        
                        # TIMEPLOT sub-banel____________________________________
                        tabPanel("Time plots",
                                 # MULTIPLOT
                                 fluidRow(
                                   column(width=2,
                                          # uiOutput("pcs_time_sel"), # TIME variable drop-down
                                          uiOutput("pcs_select")), # Select PCs list
                                   column(width=10, 
                                          plotOutput("pcs_all_vsTime", height="30vh"))),
                                 
                                 # PC vs PC vs TIME
                                 fluidRow(
                                   column(width = 2, uiOutput("PcPc_select")),
                                   column(width = 10, 
                                          fluidRow(column(width = 12, plotOutput("PcPc_vsTime", height="30vh"))),
                                          fluidRow(column(width = 12, plotlyOutput("PcPc_vsTime_3D", height="70vh")))))
                                 )#End tabpanel--
                        )#End tabset--
                      ),#End PCA tabpanel--
             
             ## DCA PANEL ============================================================
             tabPanel("DCA",
                      tabsetPanel(
                        # MAIN sub-panel________________________________________
                        tabPanel("Main",
                                 # DCA Setup
                                 column(width = 2,
                                        uiOutput("col_selector_dca"),
                                        column(width = 12, actionButton("start_dca", "START DCA")),
                                        uiOutput("dca_setup2")),
                                 # SCREE PLOT & MONTECARLO PLOT
                                 column(width = 10,
                                        fluidRow(
                                          column(width = 6, plotOutput("dca_scree_plot", height="65vh")),
                                          column(width = 6, plotOutput("dca_mc_pvals", height="65vh"))))
                        ),#End sub-panel--
                        
                        # BIPLOTS sub-panel_____________________________________
                        tabPanel("Biplots",
                                 # DCA BIPLOTS Setup
                                 column(width = 2, uiOutput("dca_setup3")),
                                 # DCA BIPLOTS PLOTS
                                 column(width = 10,
                                        # Lower plot objects
                                        fluidRow(column(width = 12, plotOutput("dca_biplot_1", height="50vh"))),
                                        fluidRow(column(width = 12, plotOutput("dca_biplot_2", height="50vh"))),
                                        fluidRow(column(width = 12, plotOutput("dca_biplot_3", height="50vh"))),
                                        fluidRow(column(width = 12, plotlyOutput("dca_plot_3d", height="100vh"))))
                        ), #End tabpanel--
                        
                        # TIMEPLOT sub-panel____________________________________
                        tabPanel("Time plots",
                                 # MULTIPLOT
                                 fluidRow(
                                   column(width=2,
                                          # uiOutput("dcs_time_sel"), #TIME variable drop-down
                                          uiOutput("dcs_select")), # Select DCs list
                                   column(width=10, plotOutput("Dcs_all_vsTime", height="30vh"))),
                                 
                                 # DC1 sv DC2 vs TIME
                                 fluidRow(
                                   column(width = 2, uiOutput("DcDc_select")),
                                   column(width = 10, 
                                          fluidRow(column(width = 12, plotOutput("DcDc_vsTime", height="30vh"))),
                                          fluidRow(column(width = 12, plotlyOutput("DcDc_vsTime_3d", height="70vh")))))
                                 ) #End tabpanel--
                        ) #End tabsetpanel--
                      ), #END DCA tabpanel---
             
             ## RDA PANEL ============================================================
             tabPanel("RDA",
                      tabsetPanel(
                        # MAIN sub-panel________________________________________
                        tabPanel("Main",
                                 # RDA Setup
                                 column(width = 2,
                                        uiOutput("col_selector_rda"),
                                        textInput("rda_mc_munber", "Iterations", 1000),
                                        actionButton("btn_rda_test", "RUN TEST"),
                                        uiOutput("rda_show_test"),
                                        actionButton("btn_rda_show", "SHOW TEST ->")),
                                 # RDA TABS & PLOTS
                                 column(width = 10,
                                        # UPPER TABS
                                        fluidRow(column(6, "GLOBAL MODEL", 
                                                        div(style = "height: 50vh; overflow-y: scroll;",
                                                            verbatimTextOutput("rda_global_text"))),
                                                 column(6, "RDA_AXIS", 
                                                        div(style = "height: 50vh; overflow-y: scroll;", 
                                                            verbatimTextOutput("rda_axis_text")))),
                                        # LOWER TABS
                                        fluidRow(column(6, "Predictor_TERMS",
                                                        div(style = "height: 50vh; overflow-y: scroll;", 
                                                            verbatimTextOutput("rda_terms_text"))),
                                                 column(6, "MULTICOLLINEARITY",
                                                        div(style = "max-height: 50vh; overflow-y: scroll;", 
                                                            verbatimTextOutput("rda_mcol_text")))),
                                        # PLOTS
                                        fluidRow(column(width = 12, 
                                                        plotOutput("rda_screeplot", height="50vh"),
                                                        plotOutput("rda_heatmap", height="50vh"))))
                                 ),#End tabPanel--
                        
                        # BIPLOTS sub-panel_____________________________________
                        tabPanel("Biplots",
                                 # RDA Setup
                                 column(width = 2,
                                        uiOutput("rda_setup2"),
                                        actionButton("btn_rda_biplots", "PLOTS")),
                                 
                                 # RDA BIPLOTS
                                 column(width = 10,
                                        fluidRow(column(width = 12,
                                                        plotOutput("rda_biplot_1", height="60vh"),
                                                        tags$style(type = "text/css", "#rda_biplot_1 {margin-bottom: 2vh;}"),
                                                        plotOutput("rda_biplot_3", height="60vh"))),
                                          fluidRow(column(width = 12,
                                                          plotOutput("rda_biplot_2", height="60vh"),
                                                          tags$style(type = "text/css", "#rda_biplot_2 {margin-bottom: 2vh;}"),
                                                          plotlyOutput("rda_plot_3d", height="100vh"))))
                        ),#End tabPanel--
                        
                        # TIMEPLOT sub-panel____________________________________
                        tabPanel("Time plots",
                                 # MULTIPLOT
                                 fluidRow(
                                   column(width=2,
                                          # uiOutput("rds_time_sel"), #TIME variable drop-down
                                          uiOutput("rds_select")), #Select rds
                                   column(width=10, 
                                          plotOutput("rds_all_vsTime", height="30vh"))),
                                 
                                 # RD1 vs RD2 vs TIME
                                 fluidRow(
                                   column(width = 2,
                                          uiOutput("RdRd_select")),
                                   column(width = 10,
                                          fluidRow(
                                            column(width = 12,
                                                   plotOutput("RdRd_vsTime", height="30vh"))),
                                          fluidRow(
                                            column(width = 12, 
                                                   plotlyOutput("RdRd_vsTime_3D", height="70vh"))))))
                      )#End tabsetPanel--
             )#End RDA tabpanel--
  )# end navbarPage-----
)# end fluidPage----

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                               Server
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
server <- function(input, output, session) {
  ## DEFINE GLOBAL VARIABLES____________________________________________________
  ca_colormap <- reactiveVal(c("red", "purple", "orange", "brown", "green", "blue", "pink", 
                               "brown", "gray", "lightblue", "darkred", "darkorange", "yellow", 
                               "darkgreen", "darkblue", "gold", "black", "beige", 
                               "darkgray", "magenta"))
  clust_num <-reactiveVal(NULL) # Optimal cluster number
  ca_out <- reactiveVal(NULL) # Cluster Analysis output
  pca_data <- reactiveValues(env_pca = NULL)
  dca_data <- reactiveValues(env_dca = NULL)
  rda_data <- reactiveValues(rda = NULL)
  rda_outputs <- reactiveValues(list = list(), counter = 0)
  
  ##############################################################################
  ##                          PANEL: DATASET
  ##############################################################################
  ## Read dataframe =============================================================
  file_data <- reactive({
    req(input$file)
    
    # Determine file extension
    file_ext <- tools::file_ext(input$file$name)
    
    # Conditional reading based on file type
    if (file_ext == "csv") {
      env.r <- read.csv(input$file$datapath, row.names = 1)
    } else if (file_ext == "txt") {
      env.r <- read.delim(input$file$datapath, row.names = 1)
    } else if (file_ext == "xlsx") {
      library(readxl)
      env.r <- read_excel(input$file$datapath)
      env.r <- as.data.frame(env.r) # Convert to data frame if read as tibble
      rownames(env.r) <- env.r[, 1] # Assuming first column is row names
      env.r <- env.r[, -1]          # Remove the first column after setting row names
    } else {
      stop("Unsupported file type. Please upload a .csv, .txt, or .xlsx file.")
    }
    
    # NA remove
    env <- na.omit(env.r)
    
    # Identify and print the removed row names
    removed_row_names <- setdiff(rownames(env.r), rownames(env))
    cat("Rows removed for N.A.:", paste(removed_row_names, collapse = ", "), "\n")
    
    ## RETURN
    env
  })
  ## UPDATE GENERAL checkboxes==================================================
  observe({
    data <- file_data()
    # Update Molecules Checkbox Group
    selected_columns_molecules <- input$show_columns_molecules
    valid_columns_molecules <- names(data)
    selected_columns_molecules <- intersect(selected_columns_molecules, valid_columns_molecules)
    updateCheckboxGroupInput(session, "show_columns_molecules",
                             choices = valid_columns_molecules,
                             selected = selected_columns_molecules)
    # Update Proxies Checkbox Group
    selected_columns_proxies <- input$show_columns_proxies
    valid_columns_proxies <- names(data)
    selected_columns_proxies <- intersect(selected_columns_proxies, valid_columns_proxies)
    updateCheckboxGroupInput(session, "show_columns_proxies",
                             choices = valid_columns_proxies,
                             selected = selected_columns_proxies)
  })
  ## SELECT MOLECULES===========================================================
  output$file_table_molecules <- renderDataTable({
    data <- file_data()
    selected_columns <- input$show_columns_molecules
    # Check if selected columns are valid
    valid_columns <- names(data)
    selected_columns <- intersect(selected_columns, valid_columns)
    if (length(selected_columns) == 0) {
      # If no valid columns are selected, default to all columns
      selected_columns <- valid_columns
    }
    # Add row names as a new column
    data$Id <- row.names(data)
    # Reorder the columns to place RowNames at the beginning
    data <- data[, c("Id", selected_columns), drop = FALSE]
    return(data)
  })
  ## SELECT PROXIES=============================================================
  output$file_table_proxies <- renderDataTable({
    data <- file_data()
    selected_columns <- input$show_columns_proxies
    # Check if selected columns are valid
    valid_columns <- names(data)
    selected_columns <- intersect(selected_columns, valid_columns)
    if (length(selected_columns) == 0) {
      # If no valid columns are selected, default to all columns
      selected_columns <- valid_columns
    }
    # Add row names as a new column
    data$Id <- row.names(data)
    # Reorder the columns to place RowNames at the beginning
    data <- data[, c("Id", selected_columns), drop = FALSE]
    return(data)
  })
  ## COLLINEARITY ANALYSIS======================================================
  observeEvent(input$start_coll_analysis, {
    # Get molecules and proxies_________________________________________________
    selected_molecules <- subset(file_data(), select=input$show_columns_molecules)
    selected_proxies <- subset(file_data(), select=input$show_columns_proxies)
    
    ## ENVIRONMENTAL VARIABLES__________________________________________________
    if(length(selected_molecules)>1){
      ## Correlation matrix
      cmat_mole <- cor(selected_molecules)
      ## Create plot
      plot_cmat_mole <- ggcorrplot(cmat_mole, lab = TRUE, type = "lower", insig = "blank") + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        ggtitle("Correlation Matrix - Environmental variables")
      ## Update plot
      output$cmat_mole_plt <- renderPlot({plot_cmat_mole})
      ## Export plot
      subdirectory <- "plots"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      ggsave(file.path(subdirectory, "plot_mole.pdf"), 
             plot_cmat_mole, width = unit(20, "cm"), 
             height = unit(20, "cm"), device = "pdf")
      ## Export CSV
      subdirectory <- "tabs"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      write.csv(cmat_mole, file.path(subdirectory, "cmat_mole.csv"), row.names = TRUE)
    }else{
      ## Update plot with empty plot
      output$cmat_mole_plt <- renderPlot({})
    }
    
    #ENVIROEMNTAL VARIABLE only p val <0.05
    if(length(selected_molecules)>1){
      ## Compute correlation matrices
      cmat_mole <- cor(selected_molecules)
      ## Compute the p matrix
      p.mat_mole <- cor_pmat(cmat_mole)
      ## Create plot
      plot_cmat_mole_pval <- ggcorrplot(
        cmat_mole,
        p.mat = p.mat_mole,
        lab = TRUE, type = "lower", insig = "blank") + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        ggtitle("Correlation Matrix - Environmental variables only p.val<0.05")
      ## Update plot
      output$cmat_mole_plt_pval <- renderPlot({plot_cmat_mole_pval})
      ## Export plot
      subdirectory <- "plots"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      ggsave(file.path(subdirectory, "plot_mole_pval.pdf"), 
             plot_cmat_mole_pval, width = unit(20, "cm"), 
             height = unit(20, "cm"), device = "pdf")
      ## Export CSV
      subdirectory <- "tabs"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      write.csv(p.mat_mole, file.path(subdirectory, "p.mat_mole.csv"), row.names = TRUE)
    }else{
      ## Update plot with empty plot
      output$cmat_mole_plt_pval <- renderPlot({})
    }
    ## PREDICTORS_______________________________________________________________
    if(length(selected_proxies)>1){
      # Compute correlation matrices
      cmat_prox <- cor(selected_proxies)
      ## CREATE PLOTS
      plot_cmat_prox <- ggcorrplot(
        cmat_prox,
        lab = TRUE, type = "lower", insig = "blank")+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        ggtitle("Correlation Matrix - Predictors")
      ## UPDATE PLOT
      output$cmat_prox_plt <- renderPlot({plot_cmat_prox})
      ## EXPORT PLOT
      subdirectory <- "plots"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      ggsave(file.path(subdirectory, "plot_prox.pdf"),
             plot_cmat_prox, width = unit(20, "cm"),
             height = unit(20, "cm"), device = "pdf")
      ## EXPORT CSV
      subdirectory <- "tabs"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      write.csv(cmat_prox, file.path(subdirectory, "cmat_prox.csv"), row.names = TRUE)
      }else{
        ## Update plot with empty plot
        output$cmat_prox_plt <- renderPlot({})
        }
    ## PREDICTORS ONLY p val<0.05
    if(length(selected_proxies)>1){
      # Correlation matrix
      cmat_prox <- cor(selected_proxies)
      # p matrix
      p.mat_prox <- cor_pmat(cmat_prox)
      # CREATE PLOTS
      plot_cmat_prox_pval <- ggcorrplot(
        cmat_prox,
        p.mat = p.mat_prox,
        lab = TRUE, type = "lower", insig = "blank")+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        ggtitle("Correlation Matrix - Predictors only p.val<0.05")
      # UPDATE PLOT
      output$cmat_prox_plt_pval <- renderPlot({plot_cmat_prox_pval})
      # EXPORT PLOT
      subdirectory <- "plots"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      ggsave(file.path(subdirectory, "plot_prox_pval.pdf"),
             plot_cmat_prox_pval, width = unit(20, "cm"),
             height = unit(20, "cm"), device = "pdf")
      # EXPORT CSV
      subdirectory <- "tabs"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      write.csv(p.mat_prox, file.path(subdirectory, "p.mat_prox.csv"), row.names = TRUE)
      }else{
        # Update plot in app with empty plot
        output$cmat_prox_plt_pval <- renderPlot({})
        }
    })#End Collinearity--
  ## Checkboxlist for FULL TIMEPLOT=============================================
  output$raw_select <- renderUI({
    column(width=12,
           ## Chose time column
           selectInput("raw_time_var", "Select time variable",
                       choices = names(file_data()),
                       selected = 1),
           ## select raw data to plot (full plot)
           div(style = "max-height: 20vh; overflow-y: scroll;",
               checkboxGroupInput("raw_plt_select","MULTILINES",
                                  choices = colnames(file_data()),
                                  selected = character(0))),
           ## plot selected
           actionButton("btn_raw_timeplots", "PLOT"))
  })
  ## Checkboxlist for 2 TIMEPLOT================================================
  output$raw1_raw2_select <- renderUI({
    column(width = 12,
           ## select raw1_raw2 to plot
           div(style = "max-height: 20vh; overflow-y: scroll;",
               checkboxGroupInput("raw1_raw2_plt_select","RAW-RAW 3D PLOT",
                                  choices = colnames(file_data()),
                                  selected = character(0))),
           ## plot selected
           actionButton("btn_raw1_raw2_timeplots", "PLOT"))
    })
  ## RAWvsTIME MULTILINE_PLOT===================================================
  observeEvent(input$btn_raw_timeplots, {
    ## Selected PCs
    retained_raw <- input$raw_plt_select
    ## Time variable
    time <- file_data()[,input$raw_time_var]
    ## Subset
    raw_df <- file_data()[,retained_raw]
    ## Cluster check
    if (is.null(ca_out())){
      # print("Warning: Cluster analysis was not performed")
      if(length(retained_raw)>1){
        row_names <- rownames(raw_df)
        ca_clust_vector <- rep(1, length(row_names))
      }else{
        ca_clust_vector <- rep(1, length(raw_df))
        }
      cmap_temp <- ca_colormap()
      ca_colors <- cmap_temp[1]
      
    }else{
      # print("CA was performed!")
      ## Create temp subset (object type 'closure' is not subsriptable)
      ca_vector <- ca_out()
      cmap_temp <- ca_colormap()
      ## Update vectors
      ca_clust_vector <- ca_vector
      ca_colors <- cmap_temp[1:max(ca_clust_vector)]
    }
    ## Create plot object
    raw_multiline_plot <- ggplot()
    # Loop through each PC and create a plot
    c <- 1
    for (i in retained_raw) {
      if(length(retained_raw) == 1){
        N <- raw_df
      }else{
        N <- raw_df[,i]
      }
      raw_multiline_plot <- raw_multiline_plot +
        geom_point(data = data.frame(time = time, N = N, cluster = ca_clust_vector),
                   aes(x = time,
                       y = N,
                       color = factor(cluster),
                       fill = factor(cluster)),
                   size = 4, shape = 21, stroke = 0.25, show.legend = TRUE) +
        geom_line(data = data.frame(time = time, N = N),
                  aes(x = time, y = N),
                  color = factor(as.character(c)),
                  show.legend = FALSE) +
        geom_text(data = data.frame(time = min(time), N = N[length(N)], lable = i),
                  aes(x = time, y = N, label = lable),
                  color = factor(as.character(c)),
                  hjust = 0, vjust = 0) +
        xlab(input$raw_time_var) + ylab("") +
        scale_color_manual(name = "Clusters", values = ca_colors) +
        scale_fill_manual(name = "Clusters", values = ca_colors) +
        xlim(min(time), max(time)) +
        theme_minimal()
      # Update color counter
      c <- c + 1
    }
    ## PLOT
    output$raw_all_vsTime <- renderPlot({raw_multiline_plot})
    ## EXPORT
    subdirectory <- "plots"
    if (!dir.exists(subdirectory)) dir.create(subdirectory)
    ggsave(file.path(subdirectory, "raw_multiline_plot.pdf"), raw_multiline_plot, 
           width = unit(20, "cm"), height = unit(5, "cm"), device = "pdf")
    
    
  })#End RAWvsTIME MULTILINE---
  ## RAWvsTIME raw1_raw2 & 3D PLOT==============================================
  observeEvent(input$btn_raw1_raw2_timeplots, {
    ## Selected PCs
    raw_vs_time <- input$raw1_raw2_plt_select
    ## Time variable
    time <- file_data()[,input$raw_time_var]
    ## Subset
    raw_df <- file_data()[,raw_vs_time] ##ex PCs
    ## Cluster check
    if (is.null(ca_out())){
      # print("Warning: Cluster analysis was not performed")
      row_names <- rownames(raw_df)
      ca_clust_vector <- rep(1, length(row_names))
      camp_res <- ca_colormap()
      ca_colors <- camp_res[1]
    }else{
      # print("CA was performed!")
      ## Create temp subset
      ca_vector <- ca_out()
      cmap_temp <- ca_colormap()
      ## Update vectors
      ca_clust_vector <- ca_vector
      ca_colors <- cmap_temp[1:max(ca_clust_vector)]
    }
    ## COMBINED PLOT____________________________________________________________
    if(length(raw_vs_time)>1){
      plot_list <- list()
      c <- 1
      for (i in raw_vs_time) {
        N <- raw_df[, i]
        # Create individual plots 
        plot <- ggplot() +
          geom_point(data = data.frame(time = time, N = N, cluster = ca_clust_vector),
                     aes(x = time, y = N, color = factor(cluster), fill = factor(cluster)),
                     size = 4, shape = 21, stroke = 0.25) +
          geom_line(data = data.frame(time = time, N = N), aes(x = time, y = N), color = "black") +
          scale_y_continuous(limits = c(min(N), max(N))) +
          scale_color_manual(name = "Clusters", values = ca_colors) +
          scale_fill_manual(name = "Clusters", values = ca_colors) + 
          xlab(input$raw_time_var) + ylab(i) +
          ggtitle("") +
          theme_minimal()
        ## Update plot list
        plot_list[[c]] <- plot
        ## Update counter
        c <- c + 1
      }
      # Save the combined plots in a single graph
      raw_combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 1)
      ## UPDATE combined plot
      output$raw1_raw2_vsTime <- renderPlot({raw_combined_plot})
      ## EXPORT
      subdirectory <- "plots"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      ggsave(file.path(subdirectory, "raw_combined_plot.pdf"), raw_combined_plot, 
             width = unit(20, "cm"), height = unit(5, "cm"), device = "pdf")
      
      ## 3D PLOT________________________________________________________________
      samples_df <- data.frame(
        x = raw_df[, raw_vs_time[1]],
        y = raw_df[, raw_vs_time[2]],
        z = time,
        label = rownames(raw_df),
        stringsAsFactors = FALSE)
      ## PLOT
      raw_time_3d <- plot_ly()
      raw_time_3d <- raw_time_3d %>%
        add_trace(## SCATTERPLOT + LABLES
          data = samples_df, type = "scatter3d", mode = "markers+text",
          x = ~x, y = ~y, z = ~z, 
          color = ~ca_clust_vector, colors = ca_colors, 
          marker = list(size = 8, opacity = 0.8, line = list(width = 1, color = "DarkSlateGrey")),  
          text = ~label, 
          textfont = list(size = 12, color = "black"), 
          name = "Sample data", 
          showlegend = TRUE) %>%
        add_trace(## LINE TRACE
          data = samples_df,
          type = "scatter3d", 
          mode = "lines",
          x = ~x, y = ~y, z = ~z, 
          line = list(color = "black", width = 2),
          name = "Sample data", 
          showlegend = TRUE) %>%
        layout(## LAYOUT
          scene = list(
            xaxis = list(title = raw_vs_time[1]),
            yaxis = list(title = raw_vs_time[2]),
            zaxis = list(title = input$raw_time_var)),
          margin = list(l = 0, r = 0, b = 0, t = 0),
          showlegend = TRUE,
          legend = list(x = 0, y = 1))
      ## UPDATE PLOT OBJECT
      output$raw1_raw2_vsTime_3D <- renderPlotly({raw_time_3d})
      
    }#end if lengh_min
  })#End PcPc & 3D---
  
  ##############################################################################
  ##                          PANEL: CLUSTER ANALYSIS
  ##############################################################################
  ## CLUSTER NUMBER ============================================================
  observeEvent(input$start_cluster_number, {
    # Run cluster_number.R______________________________________________________
    selected_molecules <- subset(file_data(), select=input$show_columns_molecules)
    # If at least one variable was chosen_______________________________________
    if(length(selected_molecules)>=1){
      clust_num_out <- local({
        selected_molecules <- selected_molecules
        fun_method <- as.numeric(input$sbox_ca_method)
        gap_nstart <- as.numeric(input$gap_nstart)
        gap_b <- as.numeric(input$gap_b)
        source("scripts/CLA_num.R", local = TRUE)$value
      })
      # Plot results____________________________________________________________
      if (is.list(clust_num_out)) {
        ## Update plots
        output$wss_plot <- renderPlot({plot(clust_num_out[[1]])})
        output$gap_plot <- renderPlot({clust_num_out[[2]]})
        output$sil_plot <- renderPlot({clust_num_out[[3]]})
        ## Export plots
        subdirectory <- "plots"
        if (!dir.exists(subdirectory)) dir.create(subdirectory)
        ggsave(file.path(subdirectory, "wss_plot.pdf"), 
               plot(clust_num_out[[1]]), width = unit(20, "cm"), height = unit(4, "cm"), device = "pdf")
        ggsave(file.path(subdirectory, "gap_plot.pdf"), 
               clust_num_out[[2]], width = unit(20, "cm"), height = unit(4, "cm"), device = "pdf")
        ggsave(file.path(subdirectory, "sil_plot.pdf"), 
               clust_num_out[[3]], width = unit(20, "cm"), height = unit(4, "cm"), device = "pdf")
        
      } else {
        output$wss_plot <- renderText("Not enough cluster plots to display.")
        output$gap_plot <- renderText("Not enough cluster plots to display.")
        output$hie_plot <- renderText("Not enough cluster plots to display.")
      }
      # Save cluster number list on reactive variable_____________________________
      clust_num(clust_num_out[[4]])
      
      # Create SETUP GUI__________________________________________________________
      output$cla_setup <- renderUI({
        column(width = 12,
               radioButtons("radio_clust_num", "Number of clusters", 
                            choices = list("WSS" = 1, 
                                           "GAP" = 2, 
                                           "SIL" = 3, 
                                           "Manual" = 4), selected = 1),
               conditionalPanel(condition = "input.radio_clust_num == 4", 
                                numericInput("nimp_clust_num", "", value = 2)),
        actionButton("start_cluster_analysis", "Start CLA"))
      })
    }
  })##END CLUSTER NUMBER---
  ## Cluster number check (must be 2 or more)===================================
  observeEvent(input$nimp_clust_num, {
    if (input$nimp_clust_num < 2) {
      updateNumericInput(session, "nimp_clust_num", value = 2)}})
  ## CLUSTER ANALISYS===========================================================
  observeEvent(input$start_cluster_analysis, {
    selected_molecules <- subset(file_data(), select=input$show_columns_molecules)
    ## Run CA.R_________________________________________________________________
    ca_out_value <- local({
      selected_molecules <- selected_molecules
      k_selector <- input$radio_clust_num
      ca_cmap <- ca_colormap
      k_values <- if (input$radio_clust_num == 4) input$nimp_clust_num else clust_num
      sbox_ca_method <- as.numeric(input$sbox_ca_method)
      source("scripts/CLA.R", local = TRUE)$value
    })
    ## Plot results_____________________________________________________________
    if (is.list(ca_out_value)) {
      ## Update plots
      output$ca_main_plot <- renderPlot({plot(ca_out_value[[1]])})
      output$ca_sil_plot <- renderPlot({ca_out_value[[2]]})
      ca_out(ca_out_value[[3]])
      ## Export plots
      subdirectory <- "plots"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      ggsave(file.path(subdirectory, "cla_main.pdf"), plot(ca_out_value[[1]]), 
             width = unit(20, "cm"), height = unit(20, "cm"), device = "pdf")
      ggsave(file.path(subdirectory, "cla_silhuette.pdf"), ca_out_value[[2]], 
             width = unit(20, "cm"), height = unit(5, "cm"), device = "pdf")
      ## Export CSV
      subdirectory <- "tabs"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      write.csv(ca_out_value[[3]], file.path(subdirectory, "cla_tab.csv"), row.names = TRUE)
    } else {
      output$ca_main_plot <- renderText("Not enough cluster plots to display.")
      output$ca_sil_plot <- renderText("Not enough cluster plots to display.")
    }
  })## END CLUSTER ANALISYS---
  
  ##############################################################################
  ##                          PANEL: PCA
  ##############################################################################
  ## Checkbox update ===========================================================
  observe({
    data <- file_data()
    # Update PCA Data
    selected_columns_pca <- input$show_columns_molecules
    valid_columns_pca <- names(data)
    selected_columns_pca <- intersect(selected_columns_pca, valid_columns_pca)
    pca_env <- subset(data, select = selected_columns_pca)
    # Update PCA Checkbox Group
    output$col_selector_pca <- renderUI({
      div(
        style = "max-height: 200px; overflow-y: scroll;",
        checkboxGroupInput("pca_columns",
                           "Select",
                           choices = names(pca_env),
                           selected = names(pca_env))
      )
    })
  })##End PCA checkbox update---
  ## RUN PCA script=============================================================
  observeEvent(input$start_pca, {
    pca_env <- subset(file_data(), select=input$pca_columns)
    # If at least 1 variable was selected
    if(length(pca_env)>=1){
      pca_out <- local({
        ## Run script_____________________________________________________________
        # pca_env <- subset(file_data(), select=input$pca_columns)
        source("scripts/PCA.R", local = TRUE)$value
      })
      ## PLOT Screeplot PCA_______________________________________________________
      if (is.list(pca_out)) {
        ## Update PCA screeplot
        output$pca_scree_plot <- renderPlot({pca_out[[1]]})
        ## Export PCA screeplot
        subdirectory <- "plots"
        if (!dir.exists(subdirectory)) dir.create(subdirectory)
        ggsave(file.path(subdirectory, "pca_screeplot.pdf"), pca_out[[1]], 
               width = unit(20, "cm"),height = unit(5, "cm"), device = "pdf")
        }
      ## Montecarlo setup_________________________________________________________
      output$pca_setup2 <- renderUI({
        column(width=12,
               selectInput("pca_retain", "PCs to retain", 
                           choices = seq(1, length(colnames(pca_out[[2]])))),
               numericInput("pca_per_num", "Permutations", value = 1000),
               actionButton("btn_pca_mctest", "START MONTECARLO")
        )})# end setup MC
    }#End if check--
  })#End PCA---
  ## MONTECARLO PCA=============================================================
  observeEvent(input$btn_pca_mctest, {
    pca_env <- subset(file_data(), select=input$pca_columns)
    # If at least 1 variable was selected
    if(length(pca_env)>=1){
      pca_mc_out <- local({
        ## Run script_____________________________________________________________
        # pca_env <- subset(file_data(), select=input$pca_columns)
        pcs_retain <- as.numeric(input$pca_retain)
        perm_num <- input$pca_per_num
        source("scripts/PCA_MC.R", local = TRUE)$value
      })
      ## PLOT_____________________________________________________________________
      if (is.list(pca_mc_out)) {
        ## Update plots
        output$pca_mc_pvals <- renderPlot({pca_mc_out[[1]]})
        ## Export PCA Montecarlo
        subdirectory <- "plots"
        if (!dir.exists(subdirectory)) dir.create(subdirectory)
        ggsave(file.path(subdirectory, "pca_montecarlo.pdf"), pca_mc_out[[1]], 
               width = unit(20, "cm"),height = unit(5, "cm"), device = "pdf")
      }
      
      ## EXPORT PCA SUMMARY in txt________________________________________________
      subdirectory <- "tabs"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      # Capture summary output
      summary_output <- capture.output(print(summary(pca_mc_out[[2]])), print(pca_mc_out[[2]]$rotation))
      # Write summary to a text file
      writeLines(summary_output, file.path(subdirectory, "pca_summary.txt"))
      
      ## Checkboxlist for BIPLOTS_________________________________________________
      env_pca <- pca_mc_out[[2]]
      pc_to_retain <- colnames(env_pca$x)
      pc_to_retain <- pc_to_retain[1:input$pca_retain]
      output$pca_setup3 <- renderUI({
        column(width=12,
               div(style = "max-height: 20vh; overflow-y: scroll;",
                   checkboxGroupInput("pca_plot_selection","PLOT selection",
                                      choices = pc_to_retain, selected = character(0))),
               textInput("pcs_mfactor", "EV moltiplicator factor", 1),
               actionButton("btn_pca_biplots", "BIPLOTS"))})
      
      ## Checkboxlist for FULL TIMEPLOT___________________________________________
      output$pcs_select <- renderUI({
        column(width=12,
               ## Chose time column
               selectInput("pcs_time_var", "Select time variable",
                           choices = names(file_data()), selected = 1),
               ## select PCs to plot (full plot)
               div(style = "max-height: 20vh; overflow-y: scroll;",
                   checkboxGroupInput("pcs_plt_select","PLOT selection",
                                      choices = colnames(env_pca$x), selected = character(0))),
               ## BTN plot selected
               actionButton("btn_pcs_timeplots", "TIMEPLOT"))
        })
      ## Checkboxlist for 2 TIMEPLOT______________________________________________
      output$PcPc_select <- renderUI({
        column(width = 12,
          ## select PC-PC to plot
          div(style = "max-height: 20vh; overflow-y: scroll;",
              checkboxGroupInput("PcPc_plt_select","PLOT selection",
                                 choices = colnames(env_pca$x), selected = character(0))),
          ## btn plot selected
          actionButton("btn_PcPc_timeplots", "PC-PC vs Time PLOT"))
      })
      # Return PCA of PCs retain__________________________________________________
      pca_data$env_pca <- pca_mc_out[[2]]
    }
  })#End MC test---
  ## PLOT PCA BIPLOTS ==========================================================
  observeEvent(input$btn_pca_biplots, {
    ## If CA was performed update pca_env dataframe______________________________
    cluster_flag <- FALSE
    if (is.null(ca_out())){
      # print("Warning: Cluster analysis was not performed")
      cluster_flag <- FALSE
    }else{
      cluster_flag <- TRUE
      }
    pca_biplots <- local({
      ## Run script
      pcs_m <- input$pcs_mfactor
      ca_flag <- cluster_flag
      ca_vector <- ca_out()
      df_pca <- pca_data$env_pca
      retained_selected <- input$pca_plot_selection
      if(length(retained_selected) > 1){source("scripts/PCA_BIPLOTS.R", local = TRUE)$value}
      })
    ## PLOTS____________________________________________________________________
    if(is.list(pca_biplots)){
      if (length(pca_biplots) == 1) {
        ## Update PCA biplots
        output$pca_biplot_1 <- renderPlot({pca_biplots[[1]]})
        output$pca_biplot_2 <- renderPlot({})
        output$pca_biplot_3 <- renderPlot({})
        output$pca_plot_3d <- renderPlotly({})
        ## Export PCA biplot
        subdirectory <- "plots"
        if (!dir.exists(subdirectory)) dir.create(subdirectory)
        ggsave(file.path(subdirectory, "biplot_PC1vsPC2.pdf"), pca_biplots[[1]], 
               width = unit(20, "cm"),height = unit(20, "cm"), device = "pdf")
        
      }else if(length(pca_biplots) > 1){
        ## Update PCA biplots
        output$pca_biplot_1 <- renderPlot({pca_biplots[[1]]})
        output$pca_biplot_2 <- renderPlot({pca_biplots[[2]]})
        output$pca_biplot_3 <- renderPlot({pca_biplots[[3]]})
        output$pca_plot_3d <- renderPlotly({pca_biplots[[4]]})
        ## Export PCA biplot
        subdirectory <- "plots"
        if (!dir.exists(subdirectory)) dir.create(subdirectory)
        ggsave(file.path(subdirectory, "biplot_PCA_1.pdf"), pca_biplots[[1]], 
               width = unit(20, "cm"),height = unit(20, "cm"), device = "pdf")
        ggsave(file.path(subdirectory, "biplot_PCA_2.pdf"), pca_biplots[[2]], 
               width = unit(20, "cm"),height = unit(20, "cm"), device = "pdf")
        ggsave(file.path(subdirectory, "biplot_PCA_3.pdf"), pca_biplots[[3]], 
               width = unit(20, "cm"),height = unit(20, "cm"), device = "pdf")
        
      }
    }
  })#End PCA BIPLOTS---
  ## PCAvsTIME MULTILINE_PLOT===================================================
  observeEvent(input$btn_pcs_timeplots, {
    ## Selected PCs
    retained_pcs <- input$pcs_plt_select
    ## Time variable
    time <- file_data()[,input$pcs_time_var]
    ## Subset
    PCs <- pca_data$env_pca$x[,retained_pcs]
    ## Cluster check____________________________________________________________
    if (is.null(ca_out())){
      # print("Warning: Cluster analysis was not performed")
      if(length(retained_pcs)>1){
        row_names <- rownames(PCs)
        ca_clust_vector <- rep(1, length(row_names))
      }else{
        ca_clust_vector <- rep(1, length(PCs))
      }
      camp_res <- ca_colormap()
      ca_colors <- camp_res[1]
    }else{
      # print("CA was performed!")
      ## Create temp subset
      ca_vector <- ca_out()
      cmap_temp <- ca_colormap()
      ## Update vectors
      ca_clust_vector <- ca_vector
      ca_colors <- cmap_temp[1:max(ca_clust_vector)]
    }
    ## Loop through each PC and create a plot___________________________________
    pcs_multiline_plot <- ggplot() + labs(title = "PCs vs time")
    c <- 1
    for (i in retained_pcs) {
      if(length(retained_pcs) == 1){
        PC <- PCs
      }else{
        PC <- PCs[,i]
      }
      pcs_multiline_plot <- pcs_multiline_plot +
        geom_point(data = data.frame(time = time, PC = PC, cluster = ca_clust_vector),
                   aes(x = time, y = PC, color = factor(cluster), fill = factor(cluster)),
                   size = 4, shape = 21, stroke = 0.25) +
        geom_line(data = data.frame(time = time, PC = PC), 
                  aes(x = time, y = PC), color = as.character(c)) +
        # geom_text(data = data.frame(time = min(time), PC = PC[length(PC)], lable = i),
        #           aes(x = time, y = PC, label = lable),
        #           color = factor(as.character(c)),
        #           hjust = 0, vjust = 0) +
        scale_color_manual(name = "Clusters", values = ca_colors) + 
        scale_fill_manual(name = "Clusters", values = ca_colors) +
        xlab(input$pcs_time_var) + ylab("") +
        xlim(min(time), max(time)) +
        theme_minimal()
        
      # Update color counter
      c <- c + 1
    }
    ## UPDATE PLOT______________________________________________________________
    output$pcs_all_vsTime <- renderPlot({pcs_multiline_plot})
    
    ## Export plot______________________________________________________________
    subdirectory <- "plots"
    if (!dir.exists(subdirectory)) dir.create(subdirectory)
    ggsave(file.path(subdirectory, "pca_multiline_plot.pdf"), pcs_multiline_plot, 
           width = unit(20, "cm"),height = unit(5, "cm"), device = "pdf")
    
  })#End PCAvsTIME MULTILINE---
  ## PCAvsTIME PC-PC & 3D PLOT==================================================
  observeEvent(input$btn_PcPc_timeplots, {
    ## PCs to retain
    pcs_vs_time <- input$PcPc_plt_select
    ## Time variable
    time <- file_data()[,input$pcs_time_var]
    ## Subset
    PCs <- pca_data$env_pca$x[,pcs_vs_time]
    ## Cluster check____________________________________________________________
    if (is.null(ca_out())){
      # print("Warning: Cluster analysis was not performed")
      row_names <- rownames(PCs)
      ca_clust_vector <- rep(1, length(row_names))
      camp_res <- ca_colormap()
      ca_colors <- camp_res[1]
    }else{
      # print("CA was performed!")
      ## Create temp subset (object type 'closure' is not subsriptable)
      ca_vector <- ca_out()
      cmap_temp <- ca_colormap()
      ## Update vectors
      ca_clust_vector <- ca_vector
      ca_colors <- cmap_temp[1:max(ca_clust_vector)]
    }
    ## COMBINED PLOT____________________________________________________________
    if(length(pcs_vs_time) > 1){
      plot_list <- list()
      c <- 1
      for (i in pcs_vs_time) {
        PC <- PCs[, i]
        # Create individual plots for each PCA dimension against time
        plot <- ggplot() +
          geom_point(data = data.frame(time = time, PC = PC, cluster = ca_clust_vector),
                     aes(x = time, y = PC, color = factor(cluster), fill = factor(cluster)),
                     size = 4, shape = 21, stroke = 0.25) +
          geom_line(data = data.frame(time = time, PC = PC), aes(x = time, y = PC), color = "black") +
          scale_y_continuous(limits = c(min(PC), max(PC))) +
          xlab(input$pcs_time_var) + ylab(i) +
          ggtitle("") +
          scale_color_manual(name = "Clusters", values = ca_colors) + 
          scale_fill_manual(name = "Clusters", values = ca_colors) +
          theme_minimal()
        ## Update plot list
        plot_list[[c]] <- plot
        ## Update counter
        c <- c + 1
      }
      # Save the combined plots in a single graph
      pcs_combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 1)
      
      # UPDATE combined plot____________________________________________________
      output$PcPc_vsTime <- renderPlot({pcs_combined_plot})
      
      ## Export combined plot___________________________________________________
      subdirectory <- "plots"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      ggsave(file.path(subdirectory, "pca_combined_plot.pdf"), pcs_combined_plot, 
             width = unit(20, "cm"), height = unit(5, "cm"), device = "pdf")
      
      ##PCA  3D BIPLOT________________________________________________________________
      samples_df <- data.frame(
        x = PCs[, pcs_vs_time[1]],
        y = PCs[, pcs_vs_time[2]],
        z =  time,
        #label = rownames(PCs),
        stringsAsFactors = FALSE
      )
      ## PLOT
      pcs_time_3d <- plot_ly()
      pcs_time_3d <- pcs_time_3d %>%
        add_trace(#4# SCATTERPLOT + LABLES
          data = samples_df, type = "scatter3d", mode = "markers+text",
          x = ~x, y = ~y, z = ~z, 
          color = ~ca_clust_vector, colors = ca_colors, 
          marker = list(size = 8, opacity = 0.8, line = list(width = 1, color = "DarkSlateGrey")),  
          #text = ~label,
          textfont = list(size = 12, color = "black"),
         # name = "Sample data",
          showlegend = TRUE) %>%
        add_trace(## LINE TRACE
          data = samples_df,
          type = "scatter3d", 
          mode = "lines",
          x = ~x, y = ~y, z = ~z, 
          line = list(color = "black", width = 2),
          name = "Sample data", showlegend = TRUE) %>%
        layout(## LAYOUT
          scene = list(
            xaxis = list(title = pcs_vs_time[1]),
            yaxis = list(title = pcs_vs_time[2]),
            zaxis = list(title = input$pcs_time_var)
          ),
          margin = list(l = 0, r = 0, b = 0, t = 0),
          showlegend = TRUE,
          legend = list(x = 0, y = 1)
        )
      ## UPDATE PLOT OBJECT
      output$PcPc_vsTime_3D <- renderPlotly({pcs_time_3d})
      }#End COMBINED PLOT---
    })#End PcPc & 3D---
  
  ##############################################################################
  ##                          PANEL: DCA
  ##############################################################################
  ## Checkbox update ===========================================================
  observe({
    data <- file_data()
    # Update DCA Data
    selected_columns_dca <- input$show_columns_molecules
    valid_columns_dca <- names(data)
    selected_columns_dca <- intersect(selected_columns_dca, valid_columns_dca)
    DCA_env <- subset(data, select = selected_columns_dca)
    # Update DCA Checkbox Group
    output$col_selector_dca <- renderUI({
      div(
        style = "max-height: 200px; overflow-y: scroll;",
        checkboxGroupInput(
          "dca_columns",
          "Select",
          choices = names(DCA_env),
          selected = names(DCA_env)
        )
      )
    })
  })##END DCA checkbox update---
  ## RUN DCA script ============================================================
  observeEvent(input$start_dca, {
    # If at least 1 variable was selected
    DCA_env <- subset(file_data(), select = input$dca_columns)
    if(length(DCA_env)>=1){
      dca_out <- local({
        source("scripts/DCA.R", local = TRUE)$value
      })
      if (is.list(dca_out)) {
        ## Update dca screeplot
        output$dca_scree_plot <- renderPlot({dca_out[[1]]})
        ## Export dca screeplot
        subdirectory <- "plots"
        if (!dir.exists(subdirectory)) dir.create(subdirectory)
        ggsave(file.path(subdirectory, "dca_screeplot.pdf"), dca_out[[1]], 
               width = unit(20, "cm"), height = unit(5, "cm"), device = "pdf")
      }
      output$dca_setup2 <- renderUI({
        column(
          width = 12,
          selectInput("dca_to_retain", "DCAs to retain", 
                      choices = seq(1, length(colnames(dca_out[[2]])))),
          numericInput("dca_per_num", "Permutations", value = 1000),
          actionButton("btn_dca_mctest", "START MONTECARLO")
        )})
      }
  })##END DCA script---
  ## MONTECARLO DCA ============================================================
  observeEvent(input$btn_dca_mctest, {
    DCA_env <- subset(file_data(), select = input$dca_columns)
    if(length(DCA_env)>=1){
      dca_mc_out <- local({
        DCAs_retain <- as.numeric(input$dca_to_retain)
        dca_per_num <- input$dca_per_num
        dca_pdf_type <- as.numeric(input$dca_pdf_type)
        source("scripts/DCA_MC.R", local = TRUE)$value
      })
      ## Update plots_____________________________________________________________
      if (is.list(dca_mc_out)) {
        ## Update dca montecarlo plot
        output$dca_mc_pvals <- renderPlot({dca_mc_out[[1]]})
        ## Export dca montecarlo
        subdirectory <- "plots"
        if (!dir.exists(subdirectory)) dir.create(subdirectory)
        ggsave(file.path(subdirectory, "dca_montecarlo.pdf"), dca_mc_out[[1]], 
               width = unit(20, "cm"), height = unit(5, "cm"), device = "pdf")
        ## EXPORT DCA SUMMARY in txt______________________________________________
        subdirectory <- "tabs"
        if (!dir.exists(subdirectory)) dir.create(subdirectory)
        summary_output <- capture.output(print(summary(dca_mc_out[[3]])))
        writeLines(summary_output, file.path(subdirectory, "dca_summary.txt"))
        
      }
      ## Checkboxlist for BIPLOTS_________________________________________________
      env_dca <- dca_mc_out[[2]]
      dc_to_retain <- colnames(env_dca)[1:input$dca_to_retain]
      output$dca_setup3 <- renderUI({
        column(width = 12,
               div(style = "max-height: 20vh; overflow-y: scroll;",
                   checkboxGroupInput("dca_plot_selection", "PLOT selection",
                                      choices = dc_to_retain,
                                      selected = character(0))),
               textInput("dcs_mfactor", "EV multiplier factor", 1),
               actionButton("btn_dca_biplots", "BIPLOTS"))
      })
      ## Checkboxlist for FULL TIMEPLOT___________________________________________
      output$dcs_select <- renderUI({
        column(width=12,
               ## Chose time column
               selectInput("dcs_time_var", "Select time variable",
                           choices = names(file_data()),
                           selected = 1),
               ## select PCs to plot (full plot)
               div(style = "max-height: 20vh; overflow-y: scroll;",
                   checkboxGroupInput("dcs_plt_select","PLOT selection",
                                      choices = colnames(env_dca),
                                      selected = character(0))),
               ## plot selected
               actionButton("btn_dcs_timeplots", "TIMEPLOT"))
      })
      ## Checkboxlist for 2 TIMEPLOT______________________________________________
      output$DcDc_select <- renderUI({
        column(width = 12,
               ## select DC-DC to plot
               div(style = "max-height: 20vh; overflow-y: scroll;",
                   checkboxGroupInput("DcDc_plt_select","PLOT selection",
                                      choices = colnames(env_dca),
                                      selected = character(0))),
               ## plot selected
               actionButton("btn_DcDc_timeplots", "DC-DC vs Time PLOT"))
      })
      ## Return DCA of DCs retain_________________________________________________
      dca_data$env_dca <- dca_mc_out[[2]]
    }#End if check---
  })##END MONTECARLO DCA---
  ## DCA BIPLOTS ===============================================================
  observeEvent(input$btn_dca_biplots, {
    # If CA was performed update pca_env dataframe
    cluster_flag <- FALSE
    if (is.null(ca_out())){
      # print("Warning: Cluster analysis was not performed")
      cluster_flag <- FALSE
    }else{
      cluster_flag <- TRUE
      }
    dca_biplots <- local({
      ## Run script_____________________________________________________________
      dcs_m <- input$dcs_mfactor
      ca_flag <- cluster_flag
      ca_vector <- ca_out()
      DCA_env <- subset(file_data(), select = input$dca_columns)
      retained_selected <- input$dca_plot_selection
      if (length(retained_selected) > 1) {
        source("scripts/DCA_BIPLOTS.R", local = TRUE)$value
      }
    })
    ## Update PLOTS_____________________________________________________________
    if (is.list(dca_biplots)) {
      if (length(dca_biplots) == 1) {
        ## Update dca biplots
        output$dca_biplot_1 <- renderPlot({dca_biplots[[1]]})
        output$dca_biplot_2 <- renderPlot({})
        output$dca_biplot_3 <- renderPlot({})
        output$dca_plot_3d <- renderPlotly({})
        ## Export dca biplots
        subdirectory <- "plots"
        if (!dir.exists(subdirectory)) dir.create(subdirectory)
        ggsave(file.path(subdirectory, "biplot_DC1vsDC2.pdf"), dca_biplots[[1]], 
               width = unit(20, "cm"),height = unit(20, "cm"), device = "pdf")
        
      } else if (length(dca_biplots) > 1) {
        output$dca_biplot_1 <- renderPlot({dca_biplots[[1]]})
        output$dca_biplot_2 <- renderPlot({dca_biplots[[2]]})
        output$dca_biplot_3 <- renderPlot({dca_biplots[[3]]})
        output$dca_plot_3d <- renderPlotly({dca_biplots[[4]]})
        ## Export dca biplots
        subdirectory <- "plots"
        if (!dir.exists(subdirectory)) dir.create(subdirectory)
        ggsave(file.path(subdirectory, "biplot_DCA_1.pdf"), dca_biplots[[1]], 
               width = unit(20, "cm"),height = unit(20, "cm"), device = "pdf")
        ggsave(file.path(subdirectory, "biplot_DCA_2.pdf"), dca_biplots[[2]], 
               width = unit(20, "cm"),height = unit(20, "cm"), device = "pdf")
        ggsave(file.path(subdirectory, "biplot_DCA_3.pdf"), dca_biplots[[3]], 
               width = unit(20, "cm"),height = unit(20, "cm"), device = "pdf")
      }
    }
  })  # End DCA BIPLOTS---
  ## DCAvsTIME MULTILINE_PLOT===================================================
  observeEvent(input$btn_dcs_timeplots, {
    ## Selected dcs
    retained_dcs <- input$dcs_plt_select
    ## Time variable
    time <- file_data()[, input$dcs_time_var]
    ## Subset
    dcs <- dca_data$env_dca[, retained_dcs]
    ## Cluster check____________________________________________________________
    if (is.null(ca_out())){
      # print("Warning: Cluster analysis was not performed")
      if(length(retained_dcs)>1){
        row_names <- rownames(dcs)
        ca_clust_vector <- rep(1, length(row_names))
      }else{
        ca_clust_vector <- rep(1, length(dcs))
      }
      camp_res <- ca_colormap()
      ca_colors <- camp_res[1]
    }else{
      # print("CA was performed!")
      ## Create temp subset
      ca_vector <- ca_out()
      cmap_temp <- ca_colormap()
      ## Update vectors
      ca_clust_vector <- ca_vector
      ca_colors <- cmap_temp[1:max(ca_clust_vector)]
    }
    
    ## Loop through each DC and create a plot___________________________________
    dca_multiline_plot <- ggplot() + labs(title = "dcs vs time (DCA)")
    c <- 1
    for (i in retained_dcs) {
      if (length(retained_dcs) == 1) {
        DC <- dcs
      } else {
        DC <- dcs[, i]
      }
      dca_multiline_plot <- dca_multiline_plot +
        geom_point(data = data.frame(time = time, DC = DC, cluster = ca_clust_vector),
                   aes(x = time, y = DC, color = factor(cluster), fill = factor(cluster)),
                   size = 4, shape = 21, stroke = 0.25) +
        geom_line(data = data.frame(time = time, DC = DC), 
                  aes(x = time, y = DC), color = as.character(c)) +
        # geom_text(data = data.frame(time = min(time), DC = DC[length(DC)], lable = i),
        #           aes(x = time, y = DC, label = lable),
        #           color = factor(as.character(c)),
        #           hjust = 0, vjust = 0) +
        scale_color_manual(name = "Clusters", values = ca_colors) +
        scale_fill_manual(name = "Clusters", values = ca_colors) +
        xlab(input$dcs_time_var) + ylab("") +
        xlim(min(time), max(time)) +
        theme_minimal()
      # Update color counter
      c <- c + 1
    }
    ## Update PLOT
    output$Dcs_all_vsTime <- renderPlot({dca_multiline_plot})
    ## Export plot
    subdirectory <- "plots"
    if (!dir.exists(subdirectory)) dir.create(subdirectory)
    ggsave(file.path(subdirectory, "dca_multiline_plot.pdf"), dca_multiline_plot, 
           width = unit(20, "cm"),height = unit(5, "cm"), device = "pdf")
    
  })  # End DCAvsTIME MULTILINE---
  ## DCAvsTIME DC-DC & 3D PLOT__________________________________________________
  observeEvent(input$btn_DcDc_timeplots, {
    ## dcs to retain
    dcs_vs_time <- input$DcDc_plt_select
    ## Time variable
    time <- file_data()[, input$dcs_time_var]
    ## Subset
    dcs <- dca_data$env_dca[, dcs_vs_time]
    ## Cluster check____________________________________________________________
    if (!is.null(ca_out())) {
      ca_vector <- ca_out()
      cmap_temp <- ca_colormap()
      ca_clust_vector <- ca_vector
      ca_colors <- cmap_temp[1:max(ca_clust_vector)]
    } else {
      # print("Warning: DCA was not performed")
      row_names <- rownames(dcs)
      ca_clust_vector <- rep(1, length(row_names))
      cmap_res <- ca_colormap()
      ca_colors <- cmap_res[1]
    }
    ## COMBINED PLOT____________________________________________________________
    if (length(dcs_vs_time) > 1) {
      ## Create a list to store individual plots
      plot_list <- list()
      ## Loop through each DC and create a plot
      c <- 1
      for (i in dcs_vs_time) {
        DC <- dcs[, i]
        ## Create individual plots for each DCA dimension against time
        plot <- ggplot() +
          geom_point(data = data.frame(time = time, DC = DC, cluster = ca_clust_vector),
                     aes(x = time, y = DC, color = factor(cluster), fill = factor(cluster)),
                     size = 4, shape = 21, stroke = 0.25) +
          geom_line(data = data.frame(time = time, DC = DC), aes(x = time, y = DC), color = "black") +
          scale_y_continuous(limits = c(min(DC), max(DC))) +
          #xlab(input$dcs_time_var) + ylab(i) +
          ggtitle("") +
          scale_color_manual(name = "Clusters", values = ca_colors) +
          scale_fill_manual(name = "Clusters", values = ca_colors) +
          theme_minimal()
        ## Update plot list
        plot_list[[c]] <- plot
        ## Update counter
        c <- c + 1
      }
      ## Save the combined plots in a single graph
      dca_combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 1)
      ## Print or save the combined plot
      output$DcDc_vsTime <- renderPlot({dca_combined_plot})
      ## Export plot
      subdirectory <- "plots"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      ggsave(file.path(subdirectory, "dca_combined_plot.pdf"), dca_combined_plot, 
             width = unit(20, "cm"),height = unit(5, "cm"), device = "pdf")
      
      ## 3D DCA PLOT________________________________________________________________
      samples_df <- data.frame(
        x = dcs[, dcs_vs_time[1]],
        y = dcs[, dcs_vs_time[2]],
        z =  time,
        label = rownames(dcs),
        stringsAsFactors = FALSE
      )
      ## PLOT
      dca_time_3d <- plot_ly()
      dca_time_3d <- dca_time_3d %>%
        add_trace(## SCATTERPLOT + LABLES
          data = samples_df, type = "scatter3d", mode = "markers+text",
          x = ~x, y = ~y, z = ~z,
          color = ~ca_clust_vector, colors = ca_colors,
          marker = list(size = 8, opacity = 0.8, line = list(width = 1, color = "DarkSlateGrey")),
          #text = ~label,
          #textfont = list(size = 12, color = "black"),
          #name = "Sample data",
          showlegend = TRUE) %>%
        add_trace(## LINE TRACE
          data = samples_df,
          type = "scatter3d",
          mode = "lines",
          x = ~x, y = ~y, z = ~z,
          line = list(color = "black", width = 2),
          name = "Sample data", showlegend = TRUE) %>%
        layout(## LAYOUT
          scene = list(
            xaxis = list(title = dcs_vs_time[1]),
            yaxis = list(title = dcs_vs_time[2]),
            zaxis = list(title = input$dcs_time_var)
          ),
          margin = list(l = 0, r = 0, b = 0, t = 0),
          showlegend = TRUE,
          legend = list(x = 0, y = 1)
        )
      ## UPDATE PLOT OBJECT
      output$DcDc_vsTime_3d <- renderPlotly({dca_time_3d})
    }## END COMBINED PLOT
  })##End DcaDca & 3D---
  # End DCA section---
  
  ##############################################################################
  ##                          PANEL: RDA
  ##############################################################################
  ## Checkbox update ===========================================================
  observe({
    data <- file_data()
    # Update RDA Data
    selected_columns_rda <- input$show_columns_proxies
    valid_columns_rda <- names(data)
    selected_columns_rda <- intersect(selected_columns_rda, valid_columns_rda)
    rda_env <- subset(data, select = selected_columns_rda)
    # Update RDA Checkbox Group
    output$col_selector_rda <- renderUI({
      div(
        style = "max-height: 200px; overflow-y: scroll;",
        checkboxGroupInput(
          "rda_columns",
          "Select",
          choices = names(rda_env),
          selected = names(rda_env)
        )
      )
    })
  })##END RDA checkbox update---
  ## RUN RDA ===================================================================
  observeEvent(input$btn_rda_test, {
    # Create env_subset with both specified columns
    env_subset <- subset(file_data(), select = c(input$show_columns_molecules, input$rda_columns))
    # Scale and center variables
    env_stand <- decostand(env_subset, method = "standardize")
    # predictor and community subsets
    rda_community_env <- subset(env_stand, select=input$show_columns_molecules)
    rda_predictor_env <- subset(env_stand, select=input$rda_columns) 
    # if at least one variable was selected from both the subsets
    if(and(length(rda_community_env)>=1, length(rda_predictor_env)>=1)){
      # Run RDA function__________________________________________________________
      purmutation_number <- as.numeric(input$rda_mc_munber)
      rda_out <- local({
        # rda_perm_num <- 1000 ##
        rda_perm_num <- purmutation_number
        source("scripts/RDA_TEST.R", local = TRUE)$value
      })
      ## Store the RDA output (in list)___________________________________________
      rda_outputs$counter <- rda_outputs$counter + 1 
      rda_outputs$list[[paste("test", rda_outputs$counter)]] <- rda_out
      ## Update the "rda_show_test" UI element ___________________________________
      output$rda_show_test <- renderUI({
        div(style = "height: 200px; overflow-y: scroll;",
            radioButtons("rda_show_test",
                         "Test to show",
                         choices = names(rda_outputs$list),
                         selected = character(0)))
        })
      }
    })#End RDA---
  ## RDA SHOW TEST==============================================================
  observeEvent(input$btn_rda_show, {
    ## Get the selected test label from the radio button
    selected_test <- input$rda_show_test
    if (!is.null(selected_test)) { # Check if a test is selected
      ## PRINT TABLES___________________________________________________________
      rda_out <- rda_outputs$list[[selected_test]]
      out_1 <- rda_out[1]
      out_2 <- rda_out[2]
      out_3 <- rda_out[3]
      out_4 <- rda_out[4]
      ## GLOBAL MODEL___________________________________________________________                                                            
      output$rda_global_text <- renderText({
        r1 <- capture.output(cat(out_1[[1]][[1]]))
        r2 <- capture.output(out_1[[1]][[2]])
        r3 <- capture.output(cat(out_1[[1]][[3]]))
        formatted_list <- c(r1, r2, r3)                                   
        formatted_list <- paste(formatted_list, collapse = "\n")
        return(formatted_list)
      })
      ## AXIS SIGNIFICANCE______________________________________________________
      output$rda_axis_text <- renderText({
        formatted_list <- capture.output(print(out_2[[1]]))
        formatted_list <- paste(formatted_list, collapse = "\n")
        return(formatted_list)
      })
      ## THERMS SIGNIFICANCE____________________________________________________
      output$rda_terms_text <- renderText({
        formatted_list <- capture.output(print(out_3[[1]]))
        formatted_list <- paste(formatted_list, collapse = "\n")
        return(formatted_list)
      })
      ## MULTICOLLINEARITY______________________________________________________
      output$rda_mcol_text <- renderText({
        formatted_list <- capture.output(print(out_4[[1]]))
        formatted_list <- paste(formatted_list, collapse = "\n")
        return(formatted_list)
      })
      
      # ## Export CSV__________________________________________________________________
      # subdirectory <- "tabs"
      # if (!dir.exists(subdirectory)) dir.create(subdirectory)
      # write.csv(rda_out[1], file.path(subdirectory, "rda_global_model.csv"), row.names = TRUE)
      # write.csv(rda_out[2], file.path(subdirectory, "rda_axis_significance.csv"), row.names = TRUE)
      # write.csv(rda_out[3], file.path(subdirectory, "rda_therms_significance.csv"), row.names = TRUE)
      # write.csv(rda_out[4], file.path(subdirectory, "rda_multicollinearity.csv"), row.names = TRUE)
      
      ## Checkboxlist for BIPLOTS__________________________________________________
      if ("Residual" %in% rownames(out_2[[1]])) {
        choices_list <- rownames(out_2[[1]][rownames(out_2[[1]]) != "Residual", ])
      }
      ## Update chekboxlist
      output$rda_setup2 <- renderUI({
        column(width=12,
               div(style = "max-height: 200px; overflow-y: scroll;",
                   checkboxGroupInput("rda_plot_selection","PLOT selection",
                                      choices = choices_list,
                                      selected = character(0))),
               textInput("rds_mfactor", "EV moltiplicator factor", 1)
               )})
      
      ## PERFORM RDA____________________________________________________________
      ## Create env_subset with both specified columns
      env_subset <- subset(file_data(), select = c(input$show_columns_molecules, input$rda_columns))
      ## Scale and center variables
      env_stand <- decostand(env_subset, method = "standardize")
      ## Subsets
      rda_community_env <- subset(env_stand, select=input$show_columns_molecules)
      rda_predictor_env <- subset(env_stand, select=input$rda_columns) 
      ## Perform RDA
      myrda <- rda(rda_community_env ~ ., data = rda_predictor_env)
      ## Save model
      rda_data$rda <- myrda
      
      ## EXPORT RDA SUMMARY in txt______________________________________________
      subdirectory <- "tabs"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      # Capture summary output
      summary_output <- capture.output(print(summary(myrda)))
      # Write summary to a text file
      writeLines(summary_output, file.path(subdirectory, "rda_summary.txt"))
      
      ## SCREEPLOT______________________________________________________________
      # RDAs Proportion of variance explained (PVE)
      percent_explained <- as.matrix(round((summary(myrda)$concont$importance[2,]*100), 2))
      percent_explained <- t(percent_explained)
      
      # Elbow with d" inflection method
      diff1_percent <- diff(percent_explained)
      diff2_percent <- diff(diff1_percent)
      elbow_point <- which(diff2_percent < 0)[1]
      
      # Create df for ggplot
      plot_data <- data.frame(
        PrincipalComponent = 1:length(percent_explained),
        ProportionExplained = percent_explained[1,]
      )
      # Create plot
      screeplot <- ggplot(plot_data, 
                          aes(x = reorder(rownames(plot_data),-ProportionExplained), 
                              y = ProportionExplained)) +
        geom_bar(stat = "identity", fill = "blue") +
        labs(title = "RDA SCREEPLOT",
             x = "RDA compontents",
             y = "Proportion of Variance Explained")
      theme_minimal()
      
      ## HEATMAP________________________________________________________________
      biplot_matr <- as.matrix(myrda$CCA$biplot)
      ## Convert the matrix to a data frame using melt()
      biplot_df <- melt(biplot_matr)
      ## Plot the heatmap with cell values as labels using ggplot2
      heatmap <- ggplot(biplot_df, aes(x = Var2, y = Var1, fill = value, label = round(value, 2))) +
        geom_tile() +
        geom_text(size = 3) +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red",midpoint = 0, name = "Scores") +
        labs(title = "Heatmap RDA", x = "RDAs", y = "PREDICTORS") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      ## UPDATE PLOT
      output$rda_screeplot <- renderPlot({screeplot})
      output$rda_heatmap <- renderPlot({heatmap})
      
      ## Export plots___________________________________________________________
      subdirectory <- "plots"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      ggsave(file.path(subdirectory, "rda_screeplot.pdf"), screeplot, 
             width = unit(20, "cm"),height = unit(5, "cm"), device = "pdf")
      ggsave(file.path(subdirectory, "rda_heatmap.pdf"), heatmap, 
             width = unit(20, "cm"),height = unit(20, "cm"), device = "pdf")
      
      ## Checkboxlist for FULL TIMEPLOT__________________________________________
      env_rds <- myrda$CCA$wa
      output$rds_select <- renderUI({
        column(width=12,
               ## Chose time column
               selectInput("rds_time_var", "Select time variable",
                           choices = names(file_data()),
                           selected = 1),
               ## select rds to plot (full plot)
               div(style = "max-height: 20vh; overflow-y: scroll;",
                   checkboxGroupInput("rds_plt_select","PLOT selection",
                                      choices = colnames(env_rds), 
                                      selected = character(0))),
               ## plot selected
               actionButton("btn_rds_timeplots", "TIMEPLOT"))})
      # Checkboxlist for 2 TIMEPLOT_______________________________________________
      output$RdRd_select <- renderUI({
        column(width = 12,
               ## select PC-PC to plot
               div(style = "max-height: 20vh; overflow-y: scroll;",
                   checkboxGroupInput("RdRd_plt_select","PLOT selection",
                                      choices = colnames(env_rds),
                                      selected = character(0))),
               ## plot selected
               actionButton("btn_RdRd_timeplots", "RD-RD vs Time PLOT"))})
    }#End if_check---
  })#End SHOW TEST---
  ## RDA BIPLOTS ===============================================================
  observeEvent(input$btn_rda_biplots, {
    ## CLUSTER CHECK____________________________________________________________
    cluster_flag <- FALSE
    if (is.null(ca_out())){
      # print("Warning: Cluster analysis was not performed")
      cluster_flag <- FALSE
    }else{
      cluster_flag <- TRUE
    }
    ## Run script_______________________________________________________________
    env_subset <- subset(file_data(), select = c(input$show_columns_molecules, input$rda_columns))
    env_stand <- decostand(env_subset, method = "standardize")
    ## Run RDA function
    rda_plots <- local({
      rda_community_env <- subset(env_stand, select=input$show_columns_molecules)
      rda_predictor_env <- subset(env_stand, select=input$rda_columns) 
      rds_m <- input$rds_mfactor
      ca_flag <- cluster_flag
      ca_vector <- ca_out()
      retained_selected <- input$rda_plot_selection
      if(length(retained_selected) > 1){source("scripts/RDA_PLOTS.R", local = TRUE)$value}
    })
    ## PLOT_____________________________________________________________________
    if (length(rda_plots)==3) {
      ## Update biplots
      output$rda_biplot_1 <- renderPlot({rda_plots[[3]]})
      output$rda_biplot_2 <- renderPlot({})
      output$rda_biplot_3 <- renderPlot({})
      output$rda_plot_3d <- renderPlotly({})
      ## Export biplplots
      subdirectory <- "plots"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      ggsave(file.path(subdirectory, "biplot_RDA1vsRDA2.pdf"), rda_plots[[3]], 
             width = unit(20, "cm"),height = unit(20, "cm"), device = "pdf")
    }else if(length(rda_plots) > 3){
      ## Update biplots
      output$rda_biplot_1 <- renderPlot({rda_plots[[3]]})
      output$rda_biplot_2 <- renderPlot({rda_plots[[4]]})
      output$rda_biplot_3 <- renderPlot({rda_plots[[5]]})
      output$rda_plot_3d <- renderPlotly({rda_plots[[6]]})
      ## Export biplplots
      subdirectory <- "plots"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      ggsave(file.path(subdirectory, "biplot_RDA_1.pdf"), rda_plots[[3]], 
             width = unit(20, "cm"),height = unit(20, "cm"), device = "pdf")
      ggsave(file.path(subdirectory, "biplot_RDA_2.pdf"), rda_plots[[4]], 
             width = unit(20, "cm"),height = unit(20, "cm"), device = "pdf")
      ggsave(file.path(subdirectory, "biplot_RDA_3.pdf"), rda_plots[[5]], 
             width = unit(20, "cm"),height = unit(20, "cm"), device = "pdf")
    }
  })#End PCA BIPLOTS---
  ## RDAvsTIME MULTILINE_PLOT ==================================================
  observeEvent(input$btn_rds_timeplots, {
    ## Selected rds
    retained_rds <- input$rds_plt_select
    ## Time variable
    time <- file_data()[,input$rds_time_var]
    ## Subset
    rda_df <- rda_data$rda
    rds <- rda_df$CCA$wa[,retained_rds]
    ## Cluster check____________________________________________________________
    if (is.null(ca_out())){
      # print("Warning: Cluster analysis was not performed")
      if(length(retained_rds)>1){
        row_names <- rownames(rds)
        ca_clust_vector <- rep(1, length(row_names))
      }else{
        ca_clust_vector <- rep(1, length(rds))
      }
      camp_res <- ca_colormap()
      ca_colors <- camp_res[1]
    }else{
      print("CA was performed!")
      ## Create temp subset
      ca_vector <- ca_out()
      cmap_temp <- ca_colormap()
      ## Update vectors
      ca_clust_vector <- ca_vector
      ca_colors <- cmap_temp[1:max(ca_clust_vector)]
    }
    ## Create plot object_______________________________________________________
    rds_multiline_plot <- ggplot() + labs(title = "RDs vs time")
    c <- 1
    for (i in retained_rds) {
      if(length(retained_rds) == 1){
        RD <- rds
      }else{
        RD <- rds[,i]
      }
      rds_multiline_plot <- rds_multiline_plot +
        geom_point(data = data.frame(time = time, RD = RD, cluster = ca_clust_vector),
                   aes(x = time, y = RD, color = factor(cluster), fill = factor(cluster)),
                   size = 4, shape = 21, stroke = 0.25) +
        geom_line(data = data.frame(time = time, RD = RD), 
                  aes(x = time, y = RD), color = as.character(c)) +
        geom_text(data = data.frame(time = min(time), RD = RD[length(RD)], lable = i),
                  aes(x = time, y = RD, label = lable),
                  color = factor(as.character(c)),
                  hjust = 0, vjust = 0) +
        scale_color_manual(name = "Clusters", values = ca_colors) +
        scale_fill_manual(name = "Clusters", values = ca_colors) +
        xlab(input$rds_time_var) + ylab("") +
        xlim(min(time), max(time)) +
        theme_minimal()
      # Update color counter
      c <- c + 1
    }
    ## Update PLOT______________________________________________________________
    output$rds_all_vsTime <- renderPlot({rds_multiline_plot})
    
    ## Export plot______________________________________________________________
    subdirectory <- "plots"
    if (!dir.exists(subdirectory)) dir.create(subdirectory)
    ggsave(file.path(subdirectory, "rda_multiline_plot.pdf"), rds_multiline_plot, 
           width = unit(20, "cm"),height = unit(5, "cm"), device = "pdf")
    
  })#End PCAvsTIME MULTILINE---
  ## RDAvsTIME RD-RD & 3D PLOT__________________________________________________
  observeEvent(input$btn_RdRd_timeplots, {
    ## RDs to retain
    rds_vs_time <- input$RdRd_plt_select
    ## Time variable
    time <- file_data()[,input$rds_time_var]
    ## Subset
    rda_df <- rda_data$rda
    rds <- rda_df$CCA$wa[,rds_vs_time]
    ## Cluster check____________________________________________________________
    if (is.null(ca_out())){
      # print("Warning: Cluster analysis was not performed")
      row_names <- rownames(rds)
      ca_clust_vector <- rep(1, length(row_names))
      camp_res <- ca_colormap()
      ca_colors <- camp_res[1]
    }else{
      # print("CA was performed!")
      ## Create temp subset
      ca_vector <- ca_out()
      cmap_temp <- ca_colormap()
      ## Update vectors
      ca_clust_vector <- ca_vector
      ca_colors <- cmap_temp[1:max(ca_clust_vector)]
    }
    ## COMBINED PLOT____________________________________________________________
    if(length(rds_vs_time) > 1){
      # Loop through each RD and create a plot
      plot_list <- list()
      c <- 1
      for (i in rds_vs_time) {
        rd <- rds[, i]
        ## Create individual plots for each RDA
        plot <- ggplot() +
          geom_point(data = data.frame(time = time, rd = rd, cluster = ca_clust_vector),
                     aes(x = time, y = rd, color = factor(cluster), fill = factor(cluster)),
                     size = 4, shape = 21, stroke = 0.25) +
          geom_line(data = data.frame(time = time, rd = rd), 
                    aes(x = time, y = rd), color = "black") +
          
          scale_y_continuous(limits = c(min(rd), max(rd))) +
          scale_color_manual(name = "Clusters", values = ca_colors) + 
          scale_fill_manual(name = "Clusters", values = ca_colors) +
          xlab(input$rds_time_var) + ylab(i) +
          theme_minimal()
        ## Update plot list
        plot_list[[c]] <- plot
        ## Update counter
        c <- c + 1
      }
      ## Save the combined plots in a single graph
      rds_combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 1)
      ## Print the combined plot
      output$RdRd_vsTime <- renderPlot({rds_combined_plot})
      ## Export plot
      subdirectory <- "plots"
      if (!dir.exists(subdirectory)) dir.create(subdirectory)
      ggsave(file.path(subdirectory, "rda_combined_plot.pdf"), rds_combined_plot, 
             width = unit(20, "cm"),height = unit(5, "cm"), device = "pdf")
      
      ## 3D PLOT________________________________________________________________
      samples_df <- data.frame(
        x = rds[, rds_vs_time[1]],
        y = rds[, rds_vs_time[2]],
        z = time,
        label = rownames(rds),
        stringsAsFactors = FALSE
      )
      ## PLOT
      rds_time_3d <- plot_ly()
      rds_time_3d <- rds_time_3d %>%
        add_trace(## SCATTERPLOT + LABLES
          data = samples_df, type = "scatter3d", mode = "markers+text",
          x = ~x, y = ~y, z = ~z,
          color = ~ca_clust_vector, colors = ca_colors,
          marker = list(size = 8, opacity = 0.8, line = list(width = 1, color = "DarkSlateGrey")),
          text = ~label,
          textfont = list(size = 12, color = "black"),
          name = "Sample data",
          showlegend = TRUE) %>%
        add_trace(## LINE TRACE
          data = samples_df,
          type = "scatter3d",
          mode = "lines",
          x = ~x, y = ~y, z = ~z,
          line = list(color = "black", width = 2),
          name = "Sample data", 
          showlegend = TRUE, 
          showscale=FALSE) %>%
        layout(## LAYOUT
          scene = list(
            xaxis = list(title = rds_vs_time[1]),
            yaxis = list(title = rds_vs_time[2]),
            zaxis = list(title = "time")
          ),
          margin = list(l = 0, r = 0, b = 0, t = 0),
          showlegend = TRUE,
          legend = list(x = 0, y = 1)
        )
      ## UPDATE PLOT OBJECT
      output$RdRd_vsTime_3D <- renderPlotly({rds_time_3d})
    }
  })#End PcPc & 3D---

  ##############################################################################
  ##                    INTEGRITY CHECKER
  ##############################################################################
  ## RAW DATA ==================================================================
  ## Max 2 selections for "raw1_raw2_plt_select" checkbox_______________________
  observe({
    if(length(input$raw1_raw2_plt_select) > 2){
      updateCheckboxGroupInput(session, "raw1_raw2_plt_select",
                               selected= tail(input$raw1_raw2_plt_select, 2))}
    })#End observe---
  
  ## PCA =======================================================================
  ## Max 3 selections for "pca_plot_selection" checkbox_________________________
  observe({
    if(length(input$pca_plot_selection) > 3){
      updateCheckboxGroupInput(session, "pca_plot_selection", 
                               selected= tail(input$pca_plot_selection, 3))}
    })#End observe---
  ## Max 2 selections for "PcPc_plt_select" checkbox____________________________
  observe({
    if(length(input$PcPc_plt_select) > 2){
      updateCheckboxGroupInput(session, "PcPc_plt_select", 
                               selected= tail(input$PcPc_plt_select, 2))}
    })#End observe---
  
  ## RDA =======================================================================
  ## Max 3 selections for "rda_plot_selection" checkbox_________________________
  observe({
    if(length(input$rda_plot_selection) > 3){
      updateCheckboxGroupInput(session, "rda_plot_selection", 
                               selected= tail(input$rda_plot_selection, 3))}
    })#End observe---
  ## Max 2 selections for "RdRd_plt_select" checkbox____________________________
  observe({
    if(length(input$RdRd_plt_select) > 2){
      updateCheckboxGroupInput(session, "RdRd_plt_select", 
                               selected= tail(input$RdRd_plt_select, 2))}
    })#End observe---
  
  ## DCA =======================================================================
  ## Max 3 selections for "dca_plot_selection" checkbox_________________________
  observe({
    if(length(input$dca_plot_selection) > 3){
      updateCheckboxGroupInput(session, "dca_plot_selection", 
                               selected= tail(input$dca_plot_selection, 3))}
    })#End observe---
  ## Max 2 selections for "DcDc_plt_select" checkbox____________________________
  observe({
    if(length(input$DcDc_plt_select) > 2){
      updateCheckboxGroupInput(session, "DcDc_plt_select", 
                               selected= tail(input$DcDc_plt_select, 2))}
    })#End observe---
}## END SERVER ######

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##                                APP
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
shinyApp(ui, server)

