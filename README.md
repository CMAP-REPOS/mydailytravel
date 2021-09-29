# My Daily Travel Analysis
 
This repo includes the code supporting an upcoming series of policy briefs by the Chicago Metropolitan Agency for Planning (CMAP). The series reviews the findings from CMAP's most recent travel survey, [My Daily Travel](https://www.cmap.illinois.gov/data/transportation/travel-survey).

## Data sources

The primary data sources for these policy briefs are the trip diaries and survey results from:
- [My Daily Travel](https://datahub.cmap.illinois.gov/dataset/mydailytravel-2018-2019-public) (MDT), CMAP's 2019 travel survey. This dataset is automatically extracted by the scripts but can also be accessed at the posted link.
- [Travel Tracker](https://datahub.cmap.illinois.gov/dataset/traveltracker0708/resource/092af96e-9c7a-4182-a1e1-ecff588a9de0?inner_span=True) (TT), CMAP's prior travel survey (conducted in 2008). If you are interested in replicating or building on these analyses, **this dataset must be downloaded and placed into the appropriate working directory before running the scripts.**

The posted datasets are supplemented by three files that provide additional detail to enable geographic and trip purpose breakdowns. These are available in the [sources](/source) folder.

The analyses also leverage several outside data sources. For example, the analysis of bike-share uses publicly available data from Divvy on system ridership. That data is automatically downloaded when executing these scripts, but can also be accessed [here](https://www.divvybikes.com/system-data). Similarly, the analysis of TNCs refers to the City of Chicago's Transportation Network Provider data, which was extracted from the city's data portal, [here](https://data.cityofchicago.org/Transportation/Transportation-Network-Providers-Trips/m6dm-c72p/data).

## Data notes

Each of the scripts includes comments explaining which records were included or excluded for analysis. While this does vary by analysis (for example, comparisons of My Daily Travel and Travel Tracker require additional exclusions), some records are handled uniformly across analyses:
- Trips are only included if they start and/or end in the Illinois counties of Cook, DeKalb, DuPage, Grundy, Kane, Kendall, Lake, McHenry, or Will. Trips that start and end outside of those counties are excluded.
- Trips that are 100 miles are longer are excluded.
- Trips by travelers younger than 5 years old are excluded. This is primarily applicable to Travel Tracker, since My Daily Travel only asked for the travel behavior of household members 5 years old or older.
- For My Daily Travel data, some single trips were split across multiple records. These records were merged using the `placeGroup` field, which is further explained in the My Daily Travel documentation.
- Both surveys includes a limited number of trips coded as walking trips that appear to be significant outliers and/or have an inaccurate reported mode, based on a combination of time and distance traveled (e.g., longer than 25 miles, or walking 10 miles in 15 minutes). Walking trips longer than 25 miles or that were longer than 10 miles and traveled faster than 5 minutes per mile are coded as improbable walks and are excluded from mode share analyses.

The My Daily Travel and Travel Tracker surveys are both representative of travel in the CMAP region for their respective travel periods. However, there were methodological differences between the two, which limits some possible comparisons. Where applicable, these limitations and caveats are noted in comments.

## R scripts

This repo includes several helper scripts that import, recode, and clean the data:
- [data_cleaning.R](/R/data_cleaning.R): This script imports and cleans data for both MDT and TT.
- [recoding.R](/R/recoding.R): This script includes lists of trip purposes, modes, household income buckets, and geographies. These lists are leveraged by the data cleaning script to recode data into larger categories for analysis.
- [helper_fns.R](/R/helper_fns.R): This script includes three helper functions that are used across the repo.
- [mdt_dates.R](/R/mdt_dates.R): This script includes date intervals used to filter non-household travel survey data for comparability with MDT (e.g., analyses of Divvy).

There are also individuals scripts that support various analyses and graphics in the policy briefs:
- [average_resident.R](/R/average_resident.R): This script calculates average travel behavior for MDT and TT. It includes general figures and breakdowns by demographic characteristics.
- [divvy.R](/R/divvy.R): This script produces a trips-in-motion graph for Divvy ridership during the MDT survey period.
- [mode_analysis.R](/R/tpurps_of_modes.R): This script analyzes the trip purpose patterns of specific modes.
- [mode_share.R](/R/mode_share.R): This script calculates the mode share in the CMAP region by home location and demographic characteristics.
- [racial_disparities.R](/R/racial_disparities.R): This script analyzes racial disparities in travel times in the region.
- [school_trips.R](/R/school_trips.R): This script analyzes the dynamics of school trips in the MDT and TT surveys, with a specific exploration of racial disparities in trip times for K-8 students.
- [tnc.R](/R/tnc.R): This script analyzes MDT's data on TNC ridership, including from both the trip diary and survey components.
- [tpurp_analysis.R](/R/modes_of_tpurps.R): This script analyzes the modes used for specific trip purposes, including health care, dining, and socializing.
- [trips_in_motion.R](/R/trips_in_motion.R): This script generates trips-in-motion charts for MDT.
- [wfh.R](/R/wfh.R): This script analyzes MDT and TT data on telecommuting and working from home behavior.
