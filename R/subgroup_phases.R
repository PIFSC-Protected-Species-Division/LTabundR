#' Review & stage edits to subgroup phase assignments
#'
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' @param cohort The cohort whose subgroup phases you would like to review and stage edits for, provided as either the cohort name (e.g., `"all"` or `"pseudorca"`) or a number indicating which slot in `cruz$cohorts` should be referenced.
#'
#' @return A `Shiny` app that allows you to make manually stage revisions to the phase assigned to each
#' subgroup (this is in reference to the protocol developed for false killer whales by NOAA PIFSC). Those
#' phases were assigned automatically during the `process_surveys()` routine; this is a way for you
#' to review those assignments and prepare a set of revisions. When you close the app,
#' those revisions are returned as a `list()`, which you can then save to file and/or
#' pass to `LTabundR::subgroup_edit()`, which will implement the revisions by modifying the data in your `cruz` object.
#' This function, `subgroup_phases()`, does not edit the `cruz` object in any form.
#'
#' @export
#' @import dplyr
#' @import shiny
#' @import shinydashboard
#' @import DT


subgroup_phases <- function(cruz,
                            cohort){

  if(FALSE){ #=== objects for debugging ========================================

    data("cnp_150km_1986_2020")
    cruz <- cnp_150km_1986_2020
    cohort = 1

    edits <- subgroup_phases(cruz, 'all')
    #save(edits, file="/Users/ekezell/repos/LTAvignette/data/phase_edits.rds")

  } #=== end debugging code ====================================================

  # Stage return object
  edits_return <- NULL

  # Filter to correct cohort and analysis
  cohorti <- cruz$cohorts[[cohort]] ; names(cohorti)
  ani <- cohorti ; names(ani)
  sg <- ani$subgroups ; names(sg)

  if(nrow(sg$events)>0 & nrow(sg$subgroups)>0 & nrow(sg$sightings)>0){

    sg$events <-
      sg$events %>%
      mutate(row = 1:n()) %>%
      select(row, phase,
             #sitid, sgid,
             SightNo, SubGrp, Event,
             Cruise, DateTime, OnEffort, EffType, Bft,
             #ObsL, ObsR, ObsInd, Obs_Sight,
             ObsStd, Obs,
             Species, Line, GSBest, GSH, GSL, Angle, stratum)

    #===========================================================================

    doshiny <- function(sg){

    app = shinyApp(
      ui = shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(title = "Stage edits to subgroup phase assignments",
                                        titleWidth = 12,
                                        disable = TRUE),
        shinydashboard::dashboardSidebar(width = 0),
        shinydashboard::dashboardBody(
          fluidRow(column(6,
                          helpText(h4('Instructions:')),
                          helpText('(1) Review the `DAS` events and select the row(s) whose `Phase` column needs to be revised.'),
                          helpText('(2) Use the inputs below the table to specify how to change the `Phase` column, then click `Store`.'),
                          helpText('(3) Your staged edits will be listed in the second table. Stage as many edits as you wish.'),
                          helpText('(4) To cancel a staged edit, select it and click `Remove`.'),
                          helpText('(5) When you are done staging edits, click `Save all edits & exit`')),
                   column(6, br(), br(),
                          uiOutput('ending'))),
          br(),
          fluidRow(
            shinydashboard::tabBox(title = 'Select rows to edit', width='12', side='right',
                                   tabPanel('Raw Events',
                                            DT::dataTableOutput("tblevents")))),
          br(),
          fluidRow(column(4,uiOutput('plan'),
                          selectInput('new_phase',
                                      label = 'Coerce the Phase for these rows to:',
                                      choices = 1:2,
                                      selected = 1,
                                      width='100%'),
                          uiOutput('store_this_edit')),
                   column(1),
                   column(4,
                          shinydashboard::tabBox(title = 'Review list of staged edits', width='12', side='right',
                                                 tabPanel('Staged edits',
                                                          DT::dataTableOutput("tbledits")))),
                   column(3, uiOutput('remove_edits')))
        )
      ),

      ############################################################################
      ############################################################################

      server= function(input, output, session) {
        rv <- reactiveValues()
        rv$edits <- data.frame()
        rv$this_edit <- NULL
        rv$is <- NULL
        rv$edit_is <- NULL


        # Render UIs =============================================================

        output$tblevents = DT::renderDT(
          DT::datatable(
            sg$events,
            rownames = FALSE,
            extensions = c('Buttons', 'FixedHeader', 'FixedColumns','Scroller'),
            #style='bootstrap',
            #class = "display nowrap",
            class = "display",
            filter = list(position = 'bottom', clear = FALSE),
            fillContainer = TRUE,
            options = list(
              scroller = TRUE,
              scrollY = 175,
              dom = 'Bfrtip',
              fixedHeader = TRUE,
              fixedColumns = list(leftColumns = 2),
              buttons = list(list(extend = 'colvis', columns = 0:(ncol(sg$events)-1)), #c(0,1,2,3,4)),
                             list(extend= "colvisGroup",text="Show All",show=":hidden") # show all button
              ))) %>%
            DT::formatStyle(columns = 0:(ncol(sg$events)-1), fontSize = '75%')
        )

        output$tbledits = DT::renderDT(
          if(nrow(rv$edits)>0){
            DT::datatable(
              rv$edits,
              rownames = FALSE,
              extensions = c('FixedHeader','Scroller'),
              class = "display",
              fillContainer = TRUE,
              options = list(
                scroller = TRUE,
                scrollY = 200,
                dom = 't',
                fixedHeader = TRUE
              )) %>%
              DT::formatStyle(columns = 0:(ncol(sg$events)-1), fontSize = '75%')
          }else{
            NULL
          }
        )

        # Stage edit =============================================================

        observeEvent(input$tblevents_rows_selected, {
          if(length(input$tblevents_rows_selected) > 0){
            rv$is <- input$tblevents_rows_selected
          }else{
            rv$is <- NULL
          }
          #print(rv$is)
        })

        observeEvent(input$tbledits_rows_selected, {
          if(length(input$tbledits_rows_selected) > 0){
            rv$edit_is <- input$tbledits_rows_selected
          }else{
            rv$edit_is <- NULL
          }
          #print(rv$edit_is)
        })

        output$plan <- renderUI({
          if(!is.null(rv$is) && length(rv$is)>0){
            helpText(HTML(paste('This edit will be applied to these rows:<br/>',
                                paste(rv$is, collapse=', '))))
          }
        })

        output$store_this_edit <- renderUI({
          if(!is.null(rv$is) && length(rv$is)>0 && !is.null(input$new_phase)){
            actionButton(inputId = 'store_this_edit',
                         label = 'Add to list of edits!',
                         width = '100%')
          }
        })

        output$remove_edits <- renderUI({
          if(!is.null(rv$edit_is) && length(rv$edit_is)>0){
            actionButton(inputId = 'delete_this_edit',
                         label = 'Delete selected edits!',
                         width = '100%')
          }
        })

        proxy_edits = DT::dataTableProxy('tbledits')
        proxy = DT::dataTableProxy('tblevents')

        observeEvent(input$store_this_edit, {
          #print('adding to edits!')
          #print(rv$is)
          #print(input$new_phase)
          dfi <- data.frame(row = rv$is, new_phase = input$new_phase)
          #print(dfi)
          rv$edits <- rbind(rv$edits, dfi)
          #print(rv$edits)
          proxy %>% DT::selectRows(selected = NULL)
          rv$is <- NULL
          showNotification("This change has been added to your log of staged edits!")
        })

        observeEvent(input$delete_this_edit, {
          #print('deleting this edit(s)!')
          #print(rv$edit_is)
          edits <- rv$edits
          edits <- edits[-rv$edit_is,]
          if(nrow(edits)==0){
            edits <- data.frame()
          }
          rv$edits <- edits
          #print(rv$edits)
          proxy_edits %>% DT::selectRows(selected = NULL)
          rv$edit_is <- NULL
        })

        # Saving results =========================================================

        output$ending <- renderUI({
          if(!is.null(rv$edits)){
            if(nrow(rv$edits)>0){
              actionButton('ending',h4('Save all edits & exit'),
                           width='100%')
            }
          }
        })

        observeEvent(input$ending, {
          stopApp(rv$edits)
        })
      }
    )
    runApp(app)
  } # end of doshiny function

    # ============================================================================
    # Handle app output

    #if(exists('edits')){rm(edits)}
    edits <- doshiny(sg)
    edits

    # ============================================================================
    # Next step:
    # how to pass this to the user in a productive way?

    if(nrow(edits)>0){
      sg_return <-
        sg$events %>%
        select(row, phase, SubGrp, Event, Cruise, Species, Line) %>%
        rename(old_phase = phase)

      edits_return <-
        left_join(sg_return, edits, by='row') %>%
        mutate(new_phase = as.numeric(new_phase)) %>%
        mutate(cohort = cohort) %>%
        filter(!is.na(new_phase)) %>%
        select(-row) %>%
        select(cohort, Cruise, Species, Line, SubGrp, Event, old_phase, new_phase)

      edits_return
    }

  }else{
    message('\n**** No subgroup data found in this cohort! Exiting now. ****')
  }
  return(edits_return)
}

#subgroup_phases(cruz, cohort='pseudorca')

