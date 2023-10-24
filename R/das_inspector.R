#'
#' Use a `Shiny` app to explore a `DAS` file and build up a list of edits. Those edits
#' will be saved as a list of instructions, which can subsequently be provided as an argument to
#' `LTabundR::process_surveys()`, which will apply your list of edits before processing the data.
#' This framework allows you to record details of required edits reproducibly without ever
#' modifying the original dataset or producing modified versions of the original.
#'
#' @param das_file Path to your `DAS` file.
#'
#' @return A `Shiny` app is launched, where the user explores and builds up a list of staged edits.
#' Once the user closes that app, this function returns that list of edits. We recommend saving that return
#' in an object for easy call back later. To understand the structure of the list of edits, see `LTabundR::das_editor()`.
#'
#' @export
#' @import shiny
#' @import shinyjs
#' @import dplyr
#'
das_inspector <- function(das_file){

  if(FALSE){
    das_file <- 'data-raw/data/HICEASwinter2020.das'
    das_file <- '../test_code/eric/CNP/CenPac1986-2020_Final_alb.das'

    # Try it
    dasi <- das_inspector(das_file)
  }

    # ============================================================================
    # Shiny app
    doshiny <- function(das_file){
      das <- das_readtext(das_file)

      app = shinyApp(
        ui = fluidPage(
          shinyjs::useShinyjs(),
          titlePanel("Prepare a list of DAS edits"),
          fluidRow(
            column(9, uiOutput('index')),
            column(3, uiOutput('save'),
                      helpText('Once you propose an edit below, you will be able to save it to a "log" of staged edits.'))),
          fluidRow(
            column(2,
                   textInput('rows', h5('Vector of rows to edit:'), value = '', width='100%'),
                   helpText('e.g.:  c(1,4,9:12)'),
                   helpText('which(substr(das$das,4,4)=="B")'),
                   helpText('which(substr(das$das, 6, 18) == "110321 091817")')),

          column(2, textInput('chars', h5('Range of characters to edit:'), value = '', width='100%'),
                 helpText('e.g. 10:15'),
                 helpText('1:nchar(das$das)')),
            column(8,
                   fluidRow(
                     column(3, radioButtons('type', h5('Type of edit'), choices = c('Verbatim replacement',
                                                                                    'Function replacement',
                                                                                    'Move',
                                                                                    'Copy & paste', 'Insertion',
                                                                                    'Deletion'), selected = 'Verbatim replacement')),
                     column(9,textAreaInput('edit', 'Specify your edit:', value='', width='100%', height='100px'))),
                  )),
          hr(),
          fluidRow(
            column(6,
                   h5('Selected rows:'),
                   verbatimTextOutput('selected_rows'),
                   tags$head(tags$style("#selected_rows{font-size:12px; overflow-y:scroll; overflow-x:scroll; max-height: 200px; max-width: 500px; background: ghostwhite;}"))),
            column(6,
                   h5('Selected data:'),
                   verbatimTextOutput('selected'),
                   tags$head(tags$style("#selected{font-size:12px; overflow-y:scroll; overflow-x:scroll; max-height: 200px; max-width: 500px; background: ghostwhite;}")))),
          fluidRow(
            column(6,
                 h5('Staged edit log:'),
                 verbatimTextOutput('proposed'),
                 tags$head(tags$style("#proposed{font-size:12px; overflow-y:scroll; overflow-x:scroll; max-height: 100px; max-width: 500px; background: ghostwhite;}"))),
            column(6,
                   h5('Preview of edited rows:'),
                   verbatimTextOutput('preview'),
                   tags$head(tags$style("#preview{font-size:12px; overflow-y:scroll; overflow-x:scroll; max-height: 200px; max-width: 500px; background: ghostwhite;}")))),
          br(),
          hr(),
          fluidRow(
            column(12,
                   h4('Raw data explorer:'),
                   verbatimTextOutput('raw'),
                   tags$head(tags$style("#raw{font-size:12px; overflow-y:scroll; overflow-x:scroll; max-height: 500px; max-width: 2200px; background: ghostwhite;}")))),
          hr(),
          fluidRow(column(6, actionButton("ending",h5("Finished? Close app here."), width = '100%')),
                   column(6, helpText('When you close the app, your list of edits will be returned in R.'))),
          br(),
          fluidRow(
            column(12,
                   h5('Details'),
                   helpText('With "Verbatim replacement", your edit will be applied verbatim to each row you have specified.'),
                   helpText('With "Function replacement", your edit will be interpreted as R code for a function with a single input argument that represents the characters in a single row. This function will be applied to each row specified.'),
                   helpText('With "Move", the row you select will be moved just *below* the row you specify in the "Edit" textbox.'),
                   helpText('With "Copy & paste", the row you select will be copied then pasted *below* the row you specify in the "Edit" textbox. That pasted row will take the timestamp of the row immediately above it.'),
                   helpText('With "Insertion", your edit will be inserted as a full line *below* the row you specify.'),
                   helpText('With "Deletion", the row(s) you specify will be deleted.'))),
          br(),br()
        ),

        server= function(input, output, session) {
          rv <- reactiveValues()
          rv$select_ok <- FALSE
          rv$edit_ok <- FALSE
          rv$selected <- data.frame()
          rv$proposed <- data.frame()
          rv$this_edit <- list()
          rv$edits <- list()

          # Status toggles =====================================================
          observe({
            oks <- input$rows != ''
            if(oks){
              if(input$type %in% c('Verbatim replacement', 'Function replacement')){
                oks <- c(oks, input$chars != '')
              }
            }
            rv$select_ok <- all(oks)
          })

          observe({
            oks <- c(input$edit != '')
            if(input$type %in% c('Delete', 'Move')){
              oks <- TRUE
            }
            if(!is.null(rv$select_ok)){
              oks <- c(oks, rv$select_ok)
            }else{
              okds <- c(oks, FALSE)
            }
            rv$edit_ok <- all(oks)
          })

          output$save <- renderUI({
            ok <- rv$edit_ok
            if(ok){
              actionButton('save',h3('Save this edit'), width = '100%')
            }else{
              helpText('No edits suggested yet.')
            }
          })

          # Slider UIs =========================================================

          output$index <- renderUI({
            sliderInput('index', h5('Page through DAS file'), min=1, max=nrow(das), step=10, value=1, width='100%')
          })
          output$view <- renderUI({
            sliderInput('view', h5('Rows to show per page:'), min=1, max=min(c(1000, nrow(das))), step=1, value=500, width='100%')
          })

          # Text outputs =======================================================

          output$raw <- renderPrint({
            dass <-
              das %>%
              mutate(row = 1:n()) %>%
              mutate(das = stringr::str_pad(das, side='right', width=max(nchar(das)), pad=' ')) %>%
              select(row, das)
            print(dass[input$index:nrow(dass),],
                  row.names=F)
          }, width=3000)

          output$selected <- renderPrint({
            toprint <- 'Row(s) / column(s) not selected yet.'
            if(rv$select_ok){
              rows <- eval(parse(text=input$rows))
              chars <- eval(parse(text=input$chars))
              toprint <- das
              toprint <-
                toprint %>% mutate(das = substr(das, min(chars), max(chars)))
              toprint <- toprint[rows,]
            }
            print(toprint, row.names=F)
          })

          output$selected_rows <- renderPrint({
            toprint <- 'Row(s) not selected yet.'
            if(input$rows != ''){
              rows <- eval(parse(text=input$rows))
              toprint <- paste(unique(rows),collapse=', ')
            }
            print(toprint, row.names=F)
          })

          output$proposed <- renderPrint({
            toprint <- ''
            if(rv$edit_ok){
              rows <- eval(parse(text=input$rows))
              chars <- eval(parse(text=input$chars))
              editi <- input$edit
              if(input$type == 'Verbatim replacement'){typi <- 'text'}
              if(input$type == 'Function replacement'){typi <- 'function'}
              if(input$type == 'Move'){typi <- 'move'}
              if(input$type == 'Copy & paste'){typi <- 'copy'}
              if(input$type == 'Deletion'){typi <- 'delete'}
              if(input$type == 'Insertion'){typi <- 'insert'}
              editlist <- list(das_file = das_file,
                               type = typi,
                               rows = rows,
                               chars = chars,
                               edit = editi)
              toprint <- editlist
              rv$this_edit <- editlist
            }
            print(toprint, row.names=F)
          })

          output$preview <- renderPrint({
            toprint <- ''
            if(rv$edit_ok){
              if(!is.null(rv$this_edit)){
                dasedit <- das_editor(list(rv$this_edit))
                toprint <- dasedit$log[[1]]$result
              }
            }
            print(toprint, row.names=F)
          })

          # Saving results =====================================================

          observeEvent(input$save,{
            rv$edits[[length(rv$edits) + 1]] <- rv$this_edit
            rv$this_edit <- NULL
            shinyjs::reset("edit")
            showNotification("This edit has been added to your log of staged edits!")
          })

          observeEvent(input$ending, {
            stopApp(rv$edits)
          })
        }
      )

      runApp(app)

    } # end shiny app

    # ============================================================================
    # Handle app output

    #if(exists('edits')){rm(edits)}
    edits <- doshiny(das_file)
    edits
    return(edits)
}
