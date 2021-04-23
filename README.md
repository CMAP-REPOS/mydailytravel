# My Daily Travel Analysis
 
This repo includes the code supporting an upcoming series of Policy Briefs by the Chicago Metropolitan Agency for Planning (CMAP). The series reviews the findings from CMAP's most recent travel survey, [My Daily Travel](https://www.cmap.illinois.gov/data/transportation/travel-survey).

## Data sources

The primary data sources for these Policy Briefs are the trip diaries and survey results from:
- [My Daily Travel](https://datahub.cmap.illinois.gov/dataset/mydailytravel-2018-2019-public), CMAP's 2019 travel survey. This dataset is automatically extracted by the scripts but can also be accessed at the posted link.
- [Travel Tracker](https://datahub.cmap.illinois.gov/dataset/traveltracker0708/resource/092af96e-9c7a-4182-a1e1-ecff588a9de0?inner_span=True), CMAP's prior travel survey (conducted in 2008). If you are interested in replicating or building on these analyses, **this dataset must be downloaded and placed into the appropriate working directory before running the scripts.**

The posted datasets are supplemented by three files that provide additional detail to enable geographic and trip purpose breakdowns. These are available in the [sources](/source) folder.

The analysis of bike-share also leverages publicly available data from Divvy on system ridership. That data is automatically downloaded when executing these scripts, but can also be accessed [here](https://www.divvybikes.com/system-data).

## Data notes

Each of the scripts includes comments explaining which records were included or excluded for analysis. While this does vary by analysis (for example, comparisons of My Daily Travel and Travel Tracker require additional exclusions), some records are handled uniformly across analyses:
- Trips are only included if they start and/or end in the Illinois counties of Cook, DeKalb, DuPage, Grundy, Kane, Kendall, Lake, McHenry, or Will. Trips that start and end outside of those counties are excluded.
- Trips that are 100 miles are longer are excluded.
- Trips by travelers younger than 5 years old are excluded. This is primarily applicable to Travel Tracker, since My Daily Travel only asked for the travel behavior of household members 5 years old or older.
- For My Daily Travel data, some single trips were split across multiple records. These records were merged using the `placeGroup` field, which is further explained in the My Daily Travel documentation.

The My Daily Travel and Travel Tracker surveys are both representative of travel in the CMAP region for their respective travel periods. However, there were methodological differences between the two, which limits some possible comparisons. Where applicable, these limitations and caveats are noted in comments.

## R scripts

This repo includes several helper scripts that import, recode, and clean the data:
- [data_cleaning.R](/R/data_cleaning.R): 
- [recoding.R](/R/recoding.R):
- [helper_fns.R](/R/helper_fns.R):

There are also individuals scripts that support various analyses and graphics in the Policy Briefs:
- [average_resident.R](/R/average_resident.R):
- [divvy.R](/R/divvy.R):
- [mode_share.R](/R/mode_share.R):
- [modes_of_tpurps.R](/R/modes_of_tpurps.R):
- [racial_disparities.R](/R/racial_disparities.R):
- [school_trips.R](/R/school_trips.R):
- [tnc.R](/R/tnc.R):
- [tpurps_of_modes.R](/R/tpurps_of_modes.R):
- [trips_in_motion.R](/R/trips_in_motion.R):
- [wfh.R](/R/wfh.R):
