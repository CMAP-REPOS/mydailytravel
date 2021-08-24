# This file provides the recoding scheme for modes, trip purpose, income levels,
# trip chains, and race and ethnicity for both My Daily Travel and Travel
# Tracker. It is run as part of the call to 'data_cleaning.R'. Any modifications
# made to this scheme will propagate to all analyses that rely on
# 'data_cleaning.R'.

#################################################
#                                               #
#                 Modes                         #
#                                               #
#################################################

# These vectors include the re-coding of mode codes into text descriptions.
recode_mode_detailed_mdt <-
  c(
    "101" = "walk",
    "102" = "personal bike",
    "103" = "bike share",
    "104" = "bike share",
    "201" = "motorcycle",
    "202" = "personal auto (driver)",
    "203" = "personal auto (passenger)",
    "301" = "carpool",
    "401" = "school bus",
    "500" = "rail and bus",
    "501" = "bus",
    "502" = "paratransit",
    "503" = "paratransit",
    "504" = "paratransit",
    "505" = "train",
    "506" = "local transit",
    "509" = "transit",
    "601" = "private shuttle",
    "701" = "taxi",
    "702" = "private limo",
    "703" = "private car",
    "704" = "rideshare",
    "705" = "shared rideshare",
    "801" = "airplane",
    "997" = "other",
    "-9"  = "missing",
    "-1" = "beginning")

recode_mode_detailed_tt <-
  c(
    "1"  = "walk",
    "2"  = "bike",
    "3"  = "personal auto (driver)",
    "4"  = "personal auto (passenger)",
    "5"  = "CTA bus",
    "6"  = "CTA train",
    "7"  = "Pace",
    "8"  = "Metra",
    "9"  = "private shuttle",
    "10" = "paratransit",
    "11" = "school bus",
    "12" = "taxi",
    "15" = "transit (many)",
    "97" = "other"
    # # Note the following codes are excluded because they did not appear in the
    # # data file (for CMAP trips)
    # ,"14" = "local transit",
    # "98" = "missing",
    # "99" = "missing"
    )

# These lists allow the text-based modes to be recoded into larger categories
recode_mode_buckets_mdt <-
  list(
    walk = c("walk"),
    bike = c("personal bike", "bike share"),
    transit = c("rail and bus", "bus", "train", "local transit", "transit"),
    driver = c("motorcycle", "personal auto (driver)"),
    passenger = c("personal auto (passenger)", "carpool"),
    schoolbus = "school bus",
    other = c("paratransit", "private shuttle",
              "taxi", "private limo", "private car", "rideshare",
              "shared rideshare", "airplane", "other"),
    missing = c("missing"),
    beginning = c("beginning"))

recode_mode_buckets_tt <-
  list(
    walk = "walk",
    bike = "bike",
    transit = c("CTA bus", "CTA train", "Pace", "Metra",
                # # Local transit is excluded because it was for non-CMAP areas
                # "local transit",
                "transit (many)"),
    driver = "personal auto (driver)",
    passenger = "personal auto (passenger)",
    schoolbus = "school bus",
    other = c("private shuttle", "paratransit",
              "taxi", "other")
    # # Similarly, "missing" does not appear in CMAP trips.
    # , missing = "missing"
    )


#################################################
#                                               #
#                 Purposes                      #
#                                               #
#################################################

# These vectors include the re-coding of purpose codes into text descriptions.

recode_tpurp_detailed_mdt <-
  c(
    "1"	= "Typical home activities",
    "2"	= "Worked at home (paid)",
    "3" = "Worked at fixed work location",
    "4"	= "Worked at non-fixed work location",
    "5"	= "Work related (off-site meeting)",
    "6"	= "Attended school or daycare / studied",
    "7"	= "Volunteered",
    "8" = "Shopped (non-routine like for appliances, cars, home furnishings)",
    "9"	= "Shopped (routine like grocery, clothing)",
    "10" = "Drive-thru errands (ATM, dry cleaning, pharmacy, etc.)",
    "11" = "Serviced a vehicle (purchased gas, regular maintenance)",
    "12" = "Health care visit for self",
    "13" = "Health care visit for someone else",
    "14" = "Visited a person staying at the hospital",
    "15" = "Non-shopping errands (banking, post office, government, etc.)",
    "16" = "Drive thru / take-out dining",
    "17" = "Ate / dined out",
    "18" = "Socialized with friends",
    "19" = "Socialized with relatives",
    "20" = "Attended a community event",
    "21" = "Attended a religious event",
    "22" = "Exercised outdoors",
    "23" = "Went to the gym",
    "24" = "Other recreation",
    "25" = "Attended a major special event",
    "26" = "Drop off / Pick up passenger(s) / child(ren)",
    "27" = "Accompanied someone else",
    "28" = "Changed travel mode / transferred",
    "97" = "Something else",
    "-7" = "Missing",
    "-8" = "Missing",
    "-9" = "Missing")

recode_tpurp_detailed_tt <-
  c(
    "1"	= "Working at home (for pay)",
    "2"	= "All other home activities",
    "3" = "Work/Job",
    "4"	= "All other activities at work",
    "5"	= "Attending class",
    "6"	= "All other activities at school",
    "7"	= "Change type of transportation/transfer",
    "8" = "Dropped off passenger from car",
    "9"	= "Picked up passenger",
    "10" = "Other - transportation",
    "11" = "Work/Business related",
    "12" = "Service private vehicle",
    "13" = "Routine shopping",
    "14" = "Shopping for major purpose",
    "15" = "Household errands",
    "16" = "Personal business",
    "17" = "Eat meal outside of home",
    "18" = "Health care",
    "19" = "Civic/religious activities",
    "20" = "Recreation/entertainment",
    "21" = "Visit friends/relatives",
    "24" = "Loop trip"
    # ,"97" = "Other"
    )

# These lists allow text-based purposes to be recoded into larger categories
recode_tpurp_buckets_mdt <-
  list(
    home = "Typical home activities",

    work = c("Worked at home (paid)",
             "Worked at fixed work location",
             "Worked at non-fixed work location",
             "Work related (off-site meeting)"),

    school = "Attended school or daycare / studied",

    "shopping/errands" =
      c("Shopped (non-routine like for appliances, cars, home furnishings)",
        "Shopped (routine like grocery, clothing)",
        "Drive-thru errands (ATM, dry cleaning, pharmacy, etc.)",
        "Non-shopping errands (banking, post office, government, etc.)",
        "Serviced a vehicle (purchased gas, regular maintenance)"),

    health = c("Health care visit for self",
               "Health care visit for someone else",
               "Visited a person staying at the hospital"),

    dining = c("Drive thru / take-out dining",
               "Ate / dined out"),

    community = c("Socialized with friends",
                  "Socialized with relatives",
                  "Attended a community event",
                  "Attended a religious event"),

    "recreation/fitness" = c("Other recreation",
                             "Exercised outdoors",
                             "Went to the gym"),

    transport = c("Drop off / Pick up passenger(s) / child(ren)",
                  "Accompanied someone else"),

    transfer = "Changed travel mode / transferred",

    other = c("Something else",
              "Attended a major special event",
              "Volunteered"),

    missing = "Missing")

recode_tpurp_buckets_tt <-
  list(
    home = "All other home activities",

    work = c("Working at home (for pay)",
             "Work/Job",
             "All other activities at work",
             "Work/Business related"),

    school = c("Attending class",
               "All other activities at school"),

    "shopping/errands" =
      c("Routine shopping",
        "Shopping for major purpose",
        "Personal business",
        "Service private vehicle",
        "Household errands"),

    health = "Health care",

    dining = "Eat meal outside of home",

    community = c("Civic/religious activities",
                  "Visit friends/relatives"),

    "recreation/fitness" = "Recreation/entertainment",

    transport = c("Dropped off passenger from car",
                  "Picked up passenger",
                  "Other - transportation"),

    transfer = "Change type of transportation/transfer",

    other = c(
      # "Other",
              "Loop trip"))

mode_c_levels <- c("driver","passenger","transit","walk",
                     "schoolbus","bike","other","missing")
tpurp_c_levels <- c("work","home","school","shopping/errands",
                   "community","dining","health","recreation/fitness",
                   "transport","transfer","other","missing")

#################################################
#                                               #
#                 Income                        #
#                                               #
#################################################

# These vectors include the re-coding of income codes into text descriptions.

recode_income_detailed_tt <-
  c("1" = "less than $20,000",
    "2" = "$20,000 to $34,999",
    "3" = "$35,000 to $49,999",
    "4" = "$50,000 to $59,999",
    "5" = "$60,000 to $74,999",
    "6" = "$75,000 to $99,999",
    "7" = "$100,000 or more",
    "9" = "refused")

recode_income_detailed_mdt <-
  c("1" = "less than $15,000",
    "2" = "$15,000 to $24,999",
    "3" = "$25,000 to $29,999",
    "4" = "$30,000 to $34,999",
    "5" = "$35,000 to $49,999",
    "6" = "$50,000 to $59,999",
    "7" = "$60,000 to $74,999",
    "8" = "$75,000 to $99,999",
    "9" = "$100,000 to $149,999",
    "10" = "$150,000 or more",
    "-9" = "not ascertained",
    "-8" = "don't know",
    "-7" = "prefer not to answer")

# These lists allow income codes to be recategorized into larger groups. Note
# that dollars are in nominal amounts, meaning that direct comparisons between
# surveys are not comparing the same real dollar values.
recode_income_buckets_tt <-
  list(low = c("less than $20,000", "$20,000 to $34,999"),
       `middle-low` = c("$35,000 to $49,999", "$50,000 to $59,999"),
       `middle-high` = c("$60,000 to $74,999", "$75,000 to $99,999"),
       high = "$100,000 or more",
       missing = "refused")

recode_income_buckets_mdt <-
  list(low = c("less than $15,000", "$15,000 to $24,999", "$25,000 to $29,999","$30,000 to $34,999"),
       `middle-low` = c("$35,000 to $49,999", "$50,000 to $59,999"),
       `middle-high` = c("$60,000 to $74,999", "$75,000 to $99,999"),
       high = c("$100,000 to $149,999", "$150,000 or more"),
       missing = c("not ascertained", "don't know", "prefer not to answer"))


#################################################
#                                               #
#                 Geographies                   #
#                                               #
#################################################

# These vectors are used to filter counties in the CMAP area

cmap_seven_counties <- c(31,43,89,93,97,111,197)
cmap_nine_counties <- c(cmap_seven_counties,37,63)
cmap_state_seven_counties <- c("17031","17043","17089","17093","17097","17111","17197")
cmap_state_nine_counties <- c(cmap_state_seven_counties,"17037","17063")
