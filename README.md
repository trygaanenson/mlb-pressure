# Do Pitchers Become More Predictable Under Pressure?

This project sets out to answer this question by using pitch-by-pitch MLB data to see if pitchers follow trends when they are under pressure. Clean, minimal R pipeline to reproduce the core tables/results found at [www.trygaanenson.com](https://www.trygaanenson.com/baseball)

## How to run
1. Put the raw files in `data/raw/` using the tree below.
2. In R:
```r
 install.packages(c("tidyverse","lubridate","readxl","tidymodels","plm","janitor","broom","svglite"))
 source("run_all.R")
```
3. Outputs:
- Derived tables in outputs/derived/
- Figures in outputs/figures/


4. Data Tree:
+ data/raw/
  + pitches.csv
  + atbats.csv
  + games.csv
  + player_names.csv
  + free_agents/
    - 2015FA.xlsx
    - 2016FA.xlsx
    - 2017FA.xlsx
    - 2018FA.xlsx
  + standings/
    - 2015-2018 MLB Standings Data.xlsx

5. Notes
- pressure_situation follows late-inning/RISP/score diff/standings/free-agency rules
- Ages and other optional enrichments are omitted to avoid external calls
