#' Launch a data explorer app for WinCruz surveys
#'
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' @param cohort The cohort whose data you would like to map, provided as a number indicating which slot in `cruz$cohorts` should be referenced.
#'
#' @return A `Shiny` app for interactive data exploration.
#' This app allows you to filter the `cruz` object according to various fields,
#' then explore summary tables of effort, sightings, and detection distances.
#' There are also tabs for reviewing the data in tabular form and exploring an interactive `leaflet` map
#' of survey tracks, geostrata, and sightings.
#'
#' @export
#'
cruz_explorer <- function(cruz,
                          cohort=1){

  if(FALSE){ # for debugging only -- not run! ==================================
    data("cnp_150km_1986_2020") # bring in example data
    cruz <- cnp_150km_1986_2020 # rename
    cohort = 1
    cruz_explorer(cruz)
  } # end debugging area =======================================================

  # Filter to correct cohort
  ani <- cruz$cohorts[[cohort]]

  # Prepare base datasets
  segments <- ani$segments
  sits <- ani$sightings
  das <- ani$das

  ##############################################################################
  ##############################################################################
  ##############################################################################
  # UI

  ui <- shiny::shinyUI(
    # Invisible formatting specs
    shiny::tagList( #needed for shinyjs
      shinyjs::useShinyjs(),  # Include shinyjs
      rintrojs::introjsUI(),   # Required to enable introjs scripts
      shiny::navbarPage(id = "intabset", #needed for landing page
                        title = 'LTabundR data explorer',
                        windowTitle = "LTabundR", #title for browser tab
                        theme = shinythemes::shinytheme("cerulean"), #Theme of the app (blue navbar)
                        collapsible = TRUE #tab panels collapse into menu in small screens
      ),

      # Top panel with filter options
      shiny::sidebarPanel(width=12,
                          shiny::fluidRow(shiny::column(3,
                                                        shiny::uiOutput('years'),
                                                        shiny::uiOutput('cruises')),
                                          shiny::column(3,
                                                        shiny::uiOutput('regions_include'),
                                                        shiny::uiOutput('regions_exclude')),
                                          shiny::column(3,
                                                        shiny::uiOutput('species'),
                                                        shiny::uiOutput('included')),
                                          shiny::column(3,
                                                        shiny::uiOutput('efftype'),
                                                        shiny::uiOutput('oneffort')))
                          ),

      ##############################################################################
      ##############################################################################
      # Main panel with tabs, plots, and maps
      shiny::mainPanel(width = 12, style="margin-left:4%; margin-right:4%",
                       shiny::tabsetPanel(
                         shiny::tabPanel(shiny::h4('Data'),
                                         shiny::br(),
                                         shiny::tabsetPanel(
                                           shiny::tabPanel(shiny::h5('DAS'),
                                                           shiny::br(),
                                                           shiny::fluidRow(shiny::column(12,DT::dataTableOutput('das')))),
                                           shiny::tabPanel(shiny::h5('Effort segments'),
                                                           shiny::br(),
                                                           shiny::fluidRow(shiny::column(12,DT::dataTableOutput('segments')))),
                                           shiny::tabPanel(shiny::h5('Sightings'),
                                                           shiny::br(),
                                                           shiny::fluidRow(shiny::column(12,DT::dataTableOutput('sits'))))
                                         )
                         ),
                         shiny::tabPanel(shiny::h4('Effort summaries'),
                                         shiny::br(),
                                         shiny::tabsetPanel(
                                           shiny::tabPanel(shiny::h5('Tables'),
                                                           shiny::br(),
                                                           shiny::fluidRow(shiny::column(3,
                                                                                         shiny::h5('Total effort'),
                                                                                         DT::dataTableOutput('effsumm_total')),
                                                                           shiny::column(1),
                                                                           shiny::column(3,
                                                                                         shiny::h5('Effort per effort type'),
                                                                                         DT::dataTableOutput('effsumm_efftype')),
                                                                           shiny::column(1),
                                                                           shiny::column(3,
                                                                                         shiny::h5('Effort per geostratum'),
                                                                                         DT::dataTableOutput('effsumm_stratum'))),
                                                           shiny::hr(),
                                                           shiny::fluidRow(shiny::column(4,
                                                                                         shiny::h5('Effort per year'),
                                                                                         DT::dataTableOutput('effsumm_years')),
                                                                           shiny::column(1),
                                                                           shiny::column(5,
                                                                                         shiny::h5('Effort per cruise'),
                                                                                         DT::dataTableOutput('effsumm_cruises'))),
                                                           shiny::hr()
                                           ),
                                           shiny::tabPanel(shiny::h5('Diagnostic Plots'),
                                                           shiny::br(),
                                                           shiny::fluidRow(shiny::column(5,shiny::plotOutput('plot_seg_use')),
                                                                           shiny::column(5,shiny::plotOutput('plot_seg_oth'))),
                                                           shiny::fluidRow(shiny::column(5,shiny::plotOutput('plot_annual_eff')),
                                                                           shiny::column(5,shiny::plotOutput('plot_bft'))),
                                                           shiny::br(),
                                                           shiny::br()
                                           )
                                         )
                         ),
                         shiny::tabPanel(shiny::h4('Sighting summaries'),
                                         shiny::br(),
                                         shiny::tabsetPanel(
                                           shiny::tabPanel(shiny::h5('Tables'),
                                                           shiny::br(),
                                                           shiny::fluidRow(shiny::column(2,
                                                                                         shiny::h5('Total sightings'),
                                                                                         DT::dataTableOutput('total_sightings'))),
                                                           shiny::hr(),
                                                           shiny::fluidRow(shiny::column(8,
                                                                                         shiny::h5('Species totals'),
                                                                                         DT::dataTableOutput('species_totals'))),
                                                           shiny::hr(),
                                                           shiny::fluidRow(shiny::column(6,
                                                                                         shiny::h5('School size metrics'),
                                                                                         DT::dataTableOutput('school_size'))),
                                                           shiny::hr(),
                                                           shiny::fluidRow(shiny::column(6,
                                                                                         shiny::h5('By year'),
                                                                                         DT::dataTableOutput('spp_yearly'))),
                                                           shiny::hr(),
                                                           shiny::fluidRow(shiny::column(6,
                                                                                         shiny::h5('By geostratum'),
                                                                                         DT::dataTableOutput('spp_stratum')))
                                                           ),
                                           shiny::tabPanel(shiny::h5('Detection distances'),
                                                           shiny::br(),
                                                           shiny::fluidRow(shiny::column(4,
                                                                                         shiny::h5('Detection distances:'),
                                                                                         shiny::sliderInput('distops',shiny::h6('Define range to review:'),
                                                                                                            min=0, max=15, post='km',value=c(0,5.5), step=.5, width='100%'),
                                                                                         shiny::sliderInput('distint',shiny::h6('Specify interval:'),
                                                                                                            min=0, max=1, post='km',value=.5, step=.05, width='100%'),
                                                                                         shiny::br(),
                                                                                         DT::dataTableOutput('distances')),
                                                                           shiny::column(1),
                                                                           shiny::column(5,
                                                                                         shiny::h5('Truncation distance options:'),
                                                                                         DT::dataTableOutput('td'),
                                                                                         shiny::br(), shiny::br(),
                                                                                         shiny::plotOutput('plot_distances'))),
                                                           shiny::br()
                                           ),
                                           shiny::tabPanel(shiny::h5('Diagnostic Plots'),
                                                           shiny::br(),
                                                           shiny::fluidRow(shiny::column(5,shiny::plotOutput('plot_annual')),
                                                                           shiny::column(5,shiny::plotOutput('plot_annual_animals'))),
                                                           shiny::fluidRow(shiny::column(5,shiny::plotOutput('plot_radial')),
                                                                           shiny::column(5,shiny::plotOutput('plot_bearing'))),
                                                           shiny::br(),
                                                           shiny::br()
                                           )
                                         )
                         ),
                         shiny::tabPanel(shiny::h4('Map'),
                                         shiny::br(),
                                         shiny::fluidRow(shiny::column(2,
                                                                       shiny::h5('Map settings'),
                                                                       shiny::checkboxInput('map_eez','Show EEZ boundaries?',value=TRUE,width='100%'),
                                                                       shiny::checkboxInput('map_strata','Show geostrata?',value=TRUE,width='100%'),
                                                                       shiny::checkboxInput('map_effort','Show survey tracks?',value=FALSE,width='100%'),
                                                                       shiny::checkboxInput('map_sits','Show sightings?',value=TRUE,width='100%'),
                                                                       shiny::checkboxInput('map_colors','Color-code sightings?',value=FALSE,width='100%'),
                                                                       shiny::helpText('(This is best with < 9 species)'),
                                                                       shiny::br(),
                                                                       shiny::sliderInput('map_sit_radius', 'Sighting dot size', min=0.2, max=5, value=1.25, step=.2, width='100%'),
                                                                       shiny::br(),
                                                                       shiny::selectInput('map_effort_coarseness','Track coarseness',choices=c(1,2,5,10,15,20),selected=15,width='100%'),
                                                                       shiny::helpText('High coarseness = faster render'),
                                                                       shiny::br(),
                                                                       shiny::sliderInput('map_effort_stroke', 'Track width', min=0.2, max=5, value=1, step=.2, width='100%'),
                                                                       shiny::br()),
                                                         shiny::column(9, leaflet::leafletOutput('map', width='100%', height=700))),
                                         shiny::br()
                         )
                       ),
                       shiny::br()

                       ##############################################################################
                       ##############################################################################
      ),
      shiny::div(style = "margin-bottom: 30px;"), # this adds breathing space between content and footer
      shiny::tags$footer(shiny::column(6, "LTabundR package"),
                         style = "
   position:fixed;
   text-align:center;
   left: 0;
   bottom:0;
   width:100%;
   z-index:1000;
   height:30px; /* Height of the footer */
   color: white;
   padding: 10px;
   background-color: #1995dc"
      )
    ) # end of tag list
  )

  ##############################################################################
  ##############################################################################
  ##############################################################################
  # Server

  server <- function(input, output, session) {

    # Reactive values ==========================================================
    rv <- reactiveValues()
    rv$das <- das
    rv$segments <- segments
    rv$sits <- sits
    rv$cruzi <- NULL
    rv$effsumm <- NULL
    rv$sitsumm <- NULL
    rv$sppsumm <- NULL
    rv$td <- NULL # truncation distances

    # Update data according to filters =========================================
    # This observe chunk runs every time a filter option is changed.
    observe({
      input_status <- c(input$years, input$cruises, input$regions_include, input$regions_exclude, input$species, input$included, input$efftype, input$oneffort)
      if(all(!is.null(input_status))){

        print(input$distops)
        print(input$distint)

        # Establish raw datasets
        dasi <- das
        segi <- segments
        siti <- sits

        # years ________________________________________________________________

        if(! all(c(length(input$years) == 1, input$years == 'All'))){
          inputi <- input$years
          inputi <- inputi[inputi != 'All']
          if(nrow(dasi) > 0){ dasi <- dasi %>% dplyr::filter(year %in% inputi) }
          if(nrow(segi) > 0){ segi <- segi %>% dplyr::filter(year %in% inputi) }
          if(nrow(siti) > 0){ siti <- siti %>% dplyr::filter(year %in% inputi) }
        }
        message('\nAfter filtering by year ...')
        message('DAS = ',nrow(dasi),' rows')
        message('segments = ',nrow(segi),' rows')
        message('sightings = ',nrow(siti),' rows')

        # cruises ______________________________________________________________

        if(! all(c(length(input$cruises) == 1, input$cruises == 'All'))){
          inputi <- input$cruises
          inputi <- inputi[inputi != 'All']
          if(nrow(dasi) > 0){ dasi <- dasi %>% dplyr::filter(Cruise %in% inputi) }
          if(nrow(segi) > 0){ segi <- segi %>% dplyr::filter(Cruise %in% inputi) }
          if(nrow(siti) > 0){ siti <- siti %>% dplyr::filter(Cruise %in% inputi) }
        }
        message('\nAfter filtering by cruise ...')
        message('DAS = ',nrow(dasi),' rows')
        message('segments = ',nrow(segi),' rows')
        message('sightings = ',nrow(siti),' rows')


        # regions_include ______________________________________________________

        print(input$regions_include)

        if(! all(c(length(input$regions_include) == 1,
                   input$regions_include == 'All'))){
          print('inside if')

          inputi <- input$regions_include
          inputi <- inputi[inputi != 'All']
          #inputi <- 'WHICEAS'
          (region_cols <- paste0('stratum_',inputi))
          print(region_cols)

          if(nrow(dasi)>0){
            (region_match <- which(gsub('-','.',names(dasi)) %in% region_cols))
            (filter_decision <- apply(dasi %>% dplyr::select(region_match),1,any))
            dasi <- dasi[filter_decision,]
          }
          if(nrow(segi)>0){
            segi <- segi %>% dplyr::filter(gsub('-','.',stratum) %in% inputi)
          }
          if(nrow(siti)>0){
            (region_match <- which(gsub('-','.',names(siti)) %in% region_cols))
            (filter_decision <- apply(siti %>% dplyr::select(region_match),1,any))
            siti <- siti[filter_decision,]
          }
        }
        message('\nAfter filtering by region_include ...')
        message('DAS = ',nrow(dasi),' rows')
        message('segments = ',nrow(segi),' rows')
        message('sightings = ',nrow(siti),' rows')

        # regions_exclude ______________________________________________________

        if(! all(c(length(input$regions_exclude) == 1, input$regions_exclude == 'None'))){
          inputi <- input$regions_exclude
          inputi <- inputi[inputi != 'None']
          #inputi <- 'WHICEAS'
          (region_cols <- paste0('stratum_',inputi))

          if(nrow(dasi)>0){
            (region_match <- which(gsub('-','.',names(dasi)) %in% region_cols))
            (filter_decision <- apply(dasi %>% dplyr::select(region_match),1,any))
            dasi <- dasi[! filter_decision,]
          }
          if(nrow(segi)>0){
            segi <- segi %>% dplyr::filter(! gsub('-','.',stratum) %in% inputi)
          }
          if(nrow(siti)>0){
            (region_match <- which(gsub('-','.',names(siti)) %in% region_cols))
            (filter_decision <- apply(siti %>% dplyr::select(region_match),1,any))
            siti <- siti[! filter_decision,]
          }
        }
        message('\nAfter filtering by region_exclude ...')
        message('DAS = ',nrow(dasi),' rows')
        message('segments = ',nrow(segi),' rows')
        message('sightings = ',nrow(siti),' rows')

        # species ______________________________________________________________

        if(! all(c(length(input$species) == 1, input$species == 'All'))){
          inputi <- input$species
          print(inputi)
          inputi <- inputi[inputi != 'All']
          siti$sp <- siti$species
          if(nrow(siti) > 0){ siti <- siti %>% dplyr::filter(sp %in% inputi) }
        }
        message('\nAfter filtering by species ...')
        message('DAS = ',nrow(dasi),' rows')
        message('segments = ',nrow(segi),' rows')
        message('sightings = ',nrow(siti),' rows')

        # included _____________________________________________________________

        if(! all(c(length(input$included) == 1, input$included == 'All'))){
          inputi <- input$included
          if(inputi == 'Analysis only'){inputi <- TRUE}
          if(inputi == 'Other only'){inputi <- FALSE}
          print(inputi)
          if(nrow(dasi) > 0){ dasi <- dasi %>% dplyr::filter(use %in% inputi) }
          if(nrow(segi) > 0){ segi <- segi %>% dplyr::filter(use %in% inputi) }
          if(nrow(siti) > 0){ siti <- siti %>% dplyr::filter(included %in% inputi) }
        }
        message('\nAfter filtering by analysis inclusion ...')
        message('DAS = ',nrow(dasi),' rows')
        message('segments = ',nrow(segi),' rows')
        message('sightings = ',nrow(siti),' rows')

        # efftype ______________________________________________________________

        if(! all(c(length(input$efftype) == 1, input$efftype == 'All'))){
          inputi <- input$efftype
          inputi <- inputi[inputi != 'All']
          if(nrow(dasi) > 0){ dasi <- dasi %>% dplyr::filter(EffType %in% inputi) }
          if(nrow(segi) > 0){ segi <- segi %>% dplyr::filter(EffType %in% inputi) }
          if(nrow(siti) > 0){ siti <- siti %>% dplyr::filter(EffType %in% inputi) }
        }
        message('\nAfter filtering by EffType ...')
        message('DAS = ',nrow(dasi),' rows')
        message('segments = ',nrow(segi),' rows')
        message('sightings = ',nrow(siti),' rows')

        # oneffort _____________________________________________________________

        if(! all(c(length(input$oneffort) == 1, input$oneffort == 'All'))){
          inputi <- input$oneffort
          if(inputi[1] == 'All'){input <- c(TRUE, FALSE)}
          if(inputi[1] == 'On Effort only'){inputi <- TRUE}
          if(inputi[1] == 'Off Effort only'){inputi <- FALSE}
          if(nrow(dasi) > 0){ dasi <- dasi %>% dplyr::filter(OnEffort %in% inputi) }
          if(nrow(segi) > 0){ segi <- segi %>% dplyr::filter(OnEffort %in% inputi) }
          if(nrow(siti) > 0){ siti <- siti %>% dplyr::filter(OnEffort %in% inputi) }
        }
        message('\nAfter filtering by OnEffort ...')
        message('DAS = ',nrow(dasi),' rows')
        message('segments = ',nrow(segi),' rows')
        message('sightings = ',nrow(siti),' rows')

        # Update working datasets
        rv$das <- dasi
        rv$segments <- segi
        rv$sits <- siti

        # Produce derivative datasets ==========================================

        cruzi <- cruz # fabricate a cruz object
        cruzi$cohorts[[1]]$segments <- segi
        cruzi$cohorts[[1]]$sightings <- siti
        cruzi$cohorts[[1]]$das <- dasi
        rv$cruzi <- cruzi

        rv$effsumm <- NULL
        if(nrow(segi)>0){
          rv$effsumm <- summarize_effort(cruzi)
        }
        rv$sitsumm <- NULL
        if(nrow(siti)>0){
          rv$sitsumm <- summarize_sightings(cruzi)
        }
        rv$sppsumm <- NULL
        if(nrow(siti)>0){
          spp <- input$species
          spp <- spp[spp != 'All']
          if(length(spp)==0){spp <- unique(siti$species)}
          rv$sppsumm <- summarize_species(spp,
                                          cruzi,
                                          distance_range = as.numeric(input$distops),
                                          distance_interval = as.numeric(input$distint))
          print(rv$sppsumm)
        }

        # Prep truncation distances
        if(nrow(siti)>0){
          dfi <- siti
          (q100 <- quantile(dfi$PerpDistKm,1,na.rm=TRUE))
          (q975 <- quantile(dfi$PerpDistKm,.975,na.rm=TRUE))
          (q95 <- quantile(dfi$PerpDistKm,.95,na.rm=TRUE))
          (q925 <- quantile(dfi$PerpDistKm,.925,na.rm=TRUE))
          (q90 <- quantile(dfi$PerpDistKm,.90,na.rm=TRUE))
          (q875 <- quantile(dfi$PerpDistKm,.875,na.rm=TRUE))
          (q85 <- quantile(dfi$PerpDistKm,.85,na.rm=TRUE))
          tds <- data.frame(Truncation_option = paste0(c(0,2.5,5,7.5,10, 12.5, 15),'%'),
                            km = c(q100,q975,q95,q925,q90, q875, q85) %>% round(2)) %>%
                            dplyr::mutate(n = sapply(km,function(x){length(which(dfi$PerpDistKm <= x))}))
          rv$td <- tds
        }
      } # end of if inputs are all not NULL (start up)
    })

    # Dynamic UI filters  ======================================================

    output$years <- shiny::renderUI({
      choices <- c('All', sort(unique(das$year)))
      shiny::selectInput('years', h5('Filter year(s)?'),choices=choices,selected=choices[1], multiple=TRUE, width='100%')
    })

    output$cruises <- shiny::renderUI({
      choices <- c('All', sort(unique(das$Cruise)))
      shiny::selectInput('cruises', h5('Filter cruise(s)?'),choices=choices,selected=choices[1], multiple=TRUE, width='100%')
    })

    output$regions_include <- shiny::renderUI({
      choices <- c('All')
      if(!is.null(cruz$strata)){
        choices <- c(choices,
                   gsub('-','.',cruz$strata$stratum[rev(order(cruz$strata$area))]))
      }
      shiny::selectInput('regions_include', h5('Filter to region(s)?'),choices=choices,selected=choices[1], multiple=TRUE, width='100%')
    })

    output$regions_exclude <- shiny::renderUI({
      choices <- c('None')
      if(!is.null(cruz$strata)){
        choices <- c(choices,
                     gsub('-','.',cruz$strata$stratum[rev(order(cruz$strata$area))]))
      }
      shiny::selectInput('regions_exclude', h5('Exclude region(s)?'),choices=choices,selected=choices[1], multiple=TRUE, width='100%')
    })

    output$species <- shiny::renderUI({
      choices <- c('All', sort(unique(sits$species)))
      shiny::selectInput('species', h5('Filter species?'),choices=choices,selected=choices[1], multiple=TRUE, width='100%')
    })

    output$included <- shiny::renderUI({
      choices <- c('All','Analysis only', 'Other only')
      shiny::radioButtons('included', h5('Filter by analysis inclusion?'),choices=choices,selected=choices[2], inline=TRUE, width='100%')
    })

    output$efftype <- shiny::renderUI({
      choices <- c('All', 'S','F','N')
      shiny::selectInput('efftype', h5('Filter EffType?'),choices=choices,selected=choices[1], multiple=TRUE, width='100%')
    })

    output$oneffort <- shiny::renderUI({
      choices <- c('All','On Effort only','Off Effort only')
      shiny::radioButtons('oneffort', h5('Filter OnEffort?'),choices=choices,selected=choices[1], inline=TRUE, width='100%')
    })

    # DataTables ===============================================================

    # Datasets
    output$das <- DT::renderDataTable(rv$das, rownames=FALSE)
    output$segments <- DT::renderDataTable(rv$segments, rownames=FALSE)
    output$sits <- DT::renderDataTable(rv$sits, rownames=FALSE)

    # Effort summaries
    output$effsumm_total <- DT::renderDataTable(rv$effsumm$total,
                                                options=list(dom='tip'),
                                                rownames=FALSE)

    output$effsumm_years <- DT::renderDataTable(rv$effsumm$total_by_year %>% dplyr::arrange(dplyr::desc(year)),
                                                rownames=FALSE)

    output$effsumm_cruises <- DT::renderDataTable(rv$effsumm$total_by_cruise,
                                                  rownames=FALSE)

    output$effsumm_efftype <- DT::renderDataTable(rv$effsumm$total_by_effort %>%
                                                    dplyr::group_by(use) %>%
                                                    dplyr::summarize(km=sum(km), days=sum(days)),
                                                  options=list(dom='tip'),
                                                  rownames=FALSE)

    output$effsumm_stratum <- DT::renderDataTable(rv$effsumm$total_by_stratum %>%
                                                    dplyr::group_by(stratum) %>%
                                                    dplyr::summarize(km=sum(km), days=sum(days)) %>%
                                                    dplyr::arrange(dplyr::desc(km)),
                                                  rownames=FALSE)

    # Sighting summaries
    output$total_sightings <- DT::renderDataTable(data.frame(n = rv$sppsumm$n_total),
                                                  options=list(dom='tip'),
                                                  rownames=FALSE)

    output$species_totals <- DT::renderDataTable(rv$sitsumm$simple_totals[,1:5],
                                                 rownames=FALSE)

    output$school_size <- DT::renderDataTable(rv$sppsumm$school_size %>%
                                                dplyr::arrange(dplyr::desc(ss_mean)) %>%
                                                dplyr::select(species,n,ss_mean:ss_cv) %>%
                                                dplyr::mutate(ss_mean=round(ss_mean,2),
                                                              ss_sd=round(ss_sd,2),
                                                              ss_se=round(ss_se,2),
                                                              ss_cv=round(ss_cv,2)),
                                              rownames=FALSE)

    output$spp_yearly <- DT::renderDataTable(rv$sppsumm$yearly_total %>% dplyr::arrange(dplyr::desc(year))%>%
                                               dplyr::mutate(ss_mean=round(ss_mean,2),
                                                             ss_sd=round(ss_sd,2),
                                                             ss_se=round(ss_se,2),
                                                             ss_cv=round(ss_cv,2)),
                                             rownames=FALSE)

    output$spp_stratum <- DT::renderDataTable(rv$sppsumm$regional_total %>%
                                                dplyr::arrange(dplyr::desc(n))%>%
                                                dplyr::mutate(ss_mean=round(ss_mean,2),
                                                              ss_sd=round(ss_sd,2),
                                                              ss_se=round(ss_se,2),
                                                              ss_cv=round(ss_cv,2)),
                                              rownames=FALSE)

    output$distances <- DT::renderDataTable(rv$sppsumm$detection_distances %>%
        dplyr::arrange(desc(km)) %>%
        dplyr::mutate(percent_beyond = paste0(round(percent_beyond,2),'%')),
      rownames=FALSE)

    output$td <- DT::renderDataTable(rv$td,
                                    rownames=FALSE,
                                    options=list(dom='tip'))

    # Plots ====================================================================

    output$plot_seg_use <- renderPlot({
      dfi <- rv$segments
      #dfi <- segments
      ggplot2::ggplot(dfi %>% dplyr::filter(use==TRUE),
                      ggplot2::aes(x=dist)) +
        ggplot2::geom_histogram(fill='cadetblue4', alpha=.7) +
        ggplot2::xlab('Segment length (km)') +
        ggplot2::labs(title='Segments to use in analysis')
    })

    output$plot_seg_oth <- renderPlot({
      dfi <- rv$segments
      #dfi <- segments
      ggplot2::ggplot(dfi %>% filter(use==FALSE),
                      ggplot2::aes(x=dist)) +
        ggplot2::geom_histogram(fill='darkblue', alpha=.5) +
        ggplot2::xlab('Segment length (km)') +
        ggplot2::labs(title='Segments *not* used in analysis')
    })

    output$plot_annual_eff <- renderPlot({
      dfi <- rv$segments
      #dfi <- segments
      ggplot2::ggplot(dfi %>%
               dplyr::group_by(year) %>%
               dplyr::summarize(km=sum(dist,na.rm=TRUE)),
               ggplot2::aes(x=year,y=km)) +
        ggplot2::geom_col(fill='firebrick',alpha=.5) +
        ggplot2::xlab('Year') +
        ggplot2::ylab('Distance surveyed (km)') +
        ggplot2::labs(title='Annual distance surveyed')
    })

    output$plot_bft <- renderPlot({
      dfi <- rv$das
      #dfi <- das
      ggplot2::ggplot(dfi,
                      ggplot2::aes(x=Bft)) +
        ggplot2::geom_histogram(fill='darkorange', alpha=.5, breaks=0:7) +
        ggplot2::scale_x_continuous(breaks=0:7) +
        ggplot2::xlab('Beaufort sea state') +
        ggplot2::labs(title='Beaufort sea state')
    })


    output$plot_distances <- renderPlot({
      dfi <- rv$sits
      tds <- rv$td

      if(FALSE){ # debugging area
        dfi <- siti
        (q100 <- quantile(dfi$PerpDistKm,1,na.rm=TRUE))
        (q975 <- quantile(dfi$PerpDistKm,.975,na.rm=TRUE))
        (q95 <- quantile(dfi$PerpDistKm,.95,na.rm=TRUE))
        (q925 <- quantile(dfi$PerpDistKm,.925,na.rm=TRUE))
        (q90 <- quantile(dfi$PerpDistKm,.90,na.rm=TRUE))
        tds <- data.frame(id=1:5,
                          Truncation_option=paste0(c(0,2.5,5,7.5,10),'%'),
                          km=c(q100,q975,q95,q925,q90))
      }

      p <- NULL
      if(length(dfi$PerpDistKm[is.finite(dfi$PerpDistKm)])>0){

        p <-
          ggplot2::ggplot(dfi,
                          ggplot2::aes(x=PerpDistKm)) +
          ggplot2::geom_histogram(binwidth=.5,alpha=.6, fill='steelblue') +
          ggplot2::scale_x_continuous(breaks=seq(0,ceiling(max(dfi$PerpDistKm, na.rm=TRUE)),by=1),
                             minor_breaks=seq(0,ceiling(max(dfi$PerpDistKm, na.rm=TRUE)),by=.25)) +
          ggplot2::ylab('Sightings') +
          ggplot2::xlab('Trackline distance (km)') +
          ggplot2::theme_light() +
          ggplot2::geom_vline(xintercept=tds$km,
                     col='firebrick') +
          ggplot2::annotate('text',
                   x=tds$km,
                   y=Inf,
                   label=tds$Truncation_option,
                   hjust=1, vjust=2,
                   size=3.5, color='firebrick', alpha=.8)
        ggplot2::scale_fill_manual(name='Truncation option',
                          labels=c("0%","2.5%","5%","7.5%","10%"))
      }

      p
    })

    output$plot_perpdist <- shiny::renderPlot({
      siti <- rv$sits
      ggplot2::ggplot(siti,
                      ggplot2::aes(x=PerpDistKm)) +
        ggplot2::xlab('Trackline distance (km)') +
        ggplot2::geom_histogram(binwidth=.5,fill='steelblue',alpha=.5) +
        ggplot2::labs(title='Perpendicular distance to sightings')
    })

    output$plot_radial <- shiny::renderPlot({
      siti <- rv$sits
      ggplot2::ggplot(siti,
                      ggplot2::aes(x=DistNm * 1.852)) +
        ggplot2::xlab('Radial distance (km)') +
        ggplot2::geom_histogram(binwidth=.5,fill='forestgreen',alpha=.5) +
        ggplot2::labs(title='Radial distance to sightings')
    })

    output$plot_bearing <- shiny::renderPlot({
      siti <- rv$sits
      ggplot2::ggplot(siti,
                      ggplot2::aes(x=Bearing)) +
        ggplot2::xlab('Angle from Bow') +
        ggplot2::geom_histogram(binwidth=5,fill='darkorchid4',alpha=.5) +
        ggplot2::labs(title='Bearing to sighting from bow')
    })

    output$plot_annual <- shiny::renderPlot({
      siti <- rv$sits
      ggplot2::ggplot(siti %>%
               dplyr::group_by(year) %>%
               dplyr::summarize(n=dplyr::n()),
               ggplot2::aes(x=year,y=n)) +
        ggplot2::geom_col(fill='firebrick',alpha=.5) +
        ggplot2::xlab('Year') +
        ggplot2::ylab('Sightings (n)') +
        ggplot2::labs(title='Annual sightings')
    })

    output$plot_annual_animals <- shiny::renderPlot({
      siti <- rv$sits
      ggplot2::ggplot(siti %>%
               dplyr::group_by(year) %>%
               dplyr::summarize(n=sum(best,na.rm=TRUE)),
               ggplot2::aes(x=year,y=n)) +
        ggplot2::geom_col(fill='darkblue',alpha=.5) +
        ggplot2::xlab('Year') +
        ggplot2::ylab('Animals (sum of school sizes)') +
        ggplot2::labs(title='Annual individuals observed')
    })


    # Maps  ====================================================================

    output$map <- leaflet::renderLeaflet({
      sit_code <- ifelse(input$map_colors,'color code','firebrick')

      map_cruz(rv$cruzi,
               cohort=1,
               eez_show=input$map_eez,
               eez_color = 'black',
               eez_weight = 0.6,
               eez_opacity = 0.5,
               strata_show=input$map_strata,
               strata_color = 'forestgreen',
               strata_weight = 1,
               strata_opacity = 0.5,
               effort_show=input$map_effort,
               effort_resolution=input$map_effort_coarseness,
               effort_color = 'darkblue',
               effort_weight = input$map_effort_stroke,
               effort_opacity = 0.85,
               sightings_show=input$map_sits,
               sightings_color = sit_code,
               sightings_radius = input$map_sit_radius,
               sightings_opacity = 0.6,
               verbose=TRUE)
    })

    # Save output  =============================================================

}

  ##############################################################################
  ##############################################################################
  ##############################################################################
  # Run app
  shiny::shinyApp(ui = ui, server = server)


}
