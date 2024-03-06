#' Stage edits to subgroups (e.g., phase and population assignments)
#'
#' @param cohort The cohort whose data will be edited,
#' provided as the number or name of the slot in `cruz$cohorts` to be referenced.
#' @param sgid A character vector of subgroup IDs whose data will be edited.
#' @param exclude A Boolean vector, with length of either 1 or the same as `sgid`,
#' indicating which `sgid`'s to erase: `TRUE` means exclude, `FALSE` means keep.
#' @param phase A numeric vector, with length of either 1 or the same as `sgid`,
#' indicating the new phase to assign the `sgid`'s.
#' If you wish to remove a phase assignment for a `sgid`, use `NA`.
#' @param population A character vector, with length of either 1 or the same as `sgid`,
#' indicating the new population to assign the `sgid`'s. If you want a `sgid` to be eligible for multiple
#' populations, separate the population names with a semicolon.
#' Both this input and `pop_prob` needs to not be `NULL` in order for this input to be recognized.
#' @param pop_prob A numeric vector, with length of either 1 or the same as `sgid`
#' and values ranging between 0 and 1,
#' indicating the probability of each population assignmend for the `sgid`'s.
#' If you want a `sgid` to be eligible for multiple
#' populations, separate the probabilities by a semicolon.
#' Both this input and `population` needs to not be `NULL` in order for this input to be recognized.
#'
#' @return A `list` of staged edits that will be accepted by the `edits` input in `process_subgroups()`.
#' @export
#' @import dplyr
#'
subgroup_edits <- function(cohort,
                           sgid,
                           exclude = NULL,
                           phase = NULL,
                           population = NULL,
                           pop_prob = NULL){
  # will let you stage edits in a reproducible way,
  # similar to the DAS editing functions.
  # This is the function you would use to manually change phase assignment,
  # population assignment, or population probabilities

  if(FALSE){ #==================================================================
    data("cnp_150km_1986_2020")
    cruz <- cnp_150km_1986_2020
    cohort = 'pseudorca'
    cruise = 1108
    sgid = '20111026-004-A'
    exclude = TRUE
    phase = NULL
    population = NULL
    pop_prob = NULL
  }  #==========================================================================

  edits <- list()
  (mr <- data.frame(cohort, sgid))

  # Exclusions =================================================================
  if(!is.null(exclude)){
      (mri <-
        mr %>%
        mutate(edit = 'exclude',
               exclude = exclude) %>%
        select(edit, cohort, sgid, exclude))
      edits[[length(edits) + 1]] <- mri
  }

  # phase ======================================================================
  if(!is.null(phase)){
    mri <-
      mr %>%
      mutate(edit = 'phase',
             phase = phase) %>%
      select(edit, cohort, sgid, phase)
    edits[[length(edits) + 1]] <- mri
  }

  # population =================================================================
  if(!is.null(population) & !is.null(pop_prob)){
    mri <-
      mr %>%
      mutate(edit = 'population',
             population = population,
             pop_prob = pop_prob) %>%
      select(edit, cohort, sgid, population, pop_prob)
    edits[[length(edits) + 1]] <- mri
  }

  return(edits)
}
