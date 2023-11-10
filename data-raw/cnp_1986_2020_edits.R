# Dataset of CNP edits

das_file <- '../test_code/eric/CNP/CenPac1986-2020_Final_alb.das'
das_file = "/Users/ekezell/Desktop/projects/noaa ltabundr/CenPac1986-2020_Final_alb.das"

#### Cruise 1607 sighting 55 {-}

#This sighting, at sequence ID `032` below, currently triggers errors
#in `swfscDAS` due to a manually entered `R` event a few lines above that does
#not have the `P` (observer positions) event that typically follows it.
#Without tht `P` entry, the observer positions of the sighting are unknown.

edit_1607_55 <-
  list(das_file = das_file,
       type = 'copy',
       rows = 128111,
       chars = NULL,
       edit = 128118)

#### Cruise 1607 sighting 68 {-}

#This sighting faces a similar issue: a rogue `R` event without the
#follow-up `P` event.
#This case is also missing the follow-up `V` event (viewing conditions).
# To fix this we will stage a similar edit, this time copying and pasting
# two rows (`P` and `V` events) below the rogue `R` event:

edit_1607_68 <-
  list(das_file = das_file,
       type = 'copy',
       rows = c(129982, 129983 , 129985),
       chars = NULL,
       edit = 129987)


#### Cruise 1621 sighting 245 {-}

# This is another case of a rogue `R` event, again missing both the
# requisite `P` and the `V` post-`R` events.

edit_1621_245 <-
  list(das_file = das_file,
       type = 'copy',
       rows = 271932:271933,
       chars = NULL,
       edit = 271937)

#### Timestamp issues with Cruise 1004 {-}
# An expedited (and safer) approximation of this edit would be
# to simply adjust the timezone by the GMT offset for Guam (UTC + 10 hours).

edit_1004_gmt10 <-
  list(das_file = das_file,
       type = 'function',
       rows = 433327:437665,
       chars = 6:39,
       edit = 'function(x){das_time(x, tz_adjust = 10)$dt}')

# Combine and save edits

edits <- list(edit_1607_55,
              edit_1607_68,
              edit_1621_245,
              edit_1004_gmt10)

cnp_1986_2020_edits <- edits
usethis::use_data(cnp_1986_2020_edits, overwrite = TRUE)
