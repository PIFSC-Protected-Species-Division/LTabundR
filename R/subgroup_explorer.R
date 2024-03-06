#' Simple  explorer for subgroups data
#'
#' @param cruz Your `cruz` object (produced from `LTabundR::process_surveys()`).
#' @param cohort The cohort whose data you would like to review, provided as the number or name of the slot in `cruz$cohorts` to be referenced.
#'
#' @return A `Shiny` app to explore subgroup event data for a cohort within a `cruz` object.
#' @export
#' @import shiny
#' @import shinydashboard
#' @import dplyr
#'
subgroup_explorer <- function(cruz,
                              cohort='pseudorca'){
  # will let you explore the subgroup data in an easy,
  # interactive way with a Shiny app. (No data modifications or edit staging,
  # just simple exploration with features
  # for data filtering, hiding columns, etc.)

  if(FALSE){ #==================================================================
    data("cnp_150km_1986_2020")
    cruz <- cnp_150km_1986_2020
    cohort = 'pseudorca'
    #cruz_explorer(cruz, cohort='pseudorca')
  }  #==========================================================================

  # Filter to correct cohort and analysis
  cohorti <- cruz$cohorts[[cohort]] ; names(cohorti)
  ani <- cohorti ; names(ani)
  sg <- ani$subgroups ; names(sg)

  if(nrow(sg$events)>0 & nrow(sg$subgroups)>0 & nrow(sg$sightings)>0){

    if(! 'population' %in% names(sg$events)){
      sg$events$population <- NA
    }
    if(! 'pop_prob' %in% names(sg$events)){
      sg$events$pop_prob <- NA
    }

    sg$events <-
      sg$events %>%
      select(Cruise,
             sgid,
             phase,
             stratum,
             population,
             pop_prob,
             #SightNo, SubGrp,
             Event,
             DateTime, OnEffort, EffType, Bft,
             #ObsL, ObsR, ObsInd, Obs_Sight,
             ObsStd, Obs,
             Species, Line, GSBest,
             GSH, GSL,
             Angle)


    #===========================================================================

    app = shinyApp(
      ui = shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(title = "Explore raw subgroup event data",
                                        titleWidth = 12,
                                        disable = TRUE),
        shinydashboard::dashboardSidebar(width = 0),
        shinydashboard::dashboardBody(
          h3('Explore raw subgroup event data'),
          div(style = 'overflow-x: scroll', DT::dataTableOutput('tblevents')),
          br()
        )
      ),

      ############################################################################
      ############################################################################

      server= function(input, output, session) {

        # Render UIs =============================================================

        output$tblevents = DT::renderDT(
          DT::datatable(
            sg$events,
            rownames = FALSE,
            extensions = c('Buttons', 'FixedHeader', 'FixedColumns','Scroller'),
            class = "display",
            filter = list(position = 'bottom', clear = FALSE),
            options = list(
              buttons = list(list(extend = 'colvis', columns = 0:(ncol(sg$events)-1)), #c(0,1,2,3,4)),
                             list(extend= "colvisGroup",text="Show All",show=":hidden") # show all button
              ))) %>%
            DT::formatStyle(columns = 0:(ncol(sg$events)-1), fontSize = '80%')
        )
      }
    )
    runApp(app)
  }
}
