# Airbnb Final Project
This project explores a cross-sectional comparison of Airbnb and the hotel industry in four US cities (Chicago, Los Angeles, New York, and San Francisco), as well as Airbnb pricing and accommodation patterns for each city. 


## Folders
* __raw_data:__
This folder contains the raw data files for each city that was scraped through the Airbnb site
* __clean_data:__
This folder contains two folders: ***cities_cleaning*** and ***hotels_cleaning***. The ***cities_cleaning*** folder contains ***cleaning_script.Rmd*** that produces the ***updated_cleaned_four_cities.csv*** file, and inside the ***hotels_cleaning*** file, the ***final_hotels.R*** produces the ***cleaned_four_hotels.csv*** file. These csv’s are used for analysis.   
* __eda:__
This folder contains exploratory data analysis. 
* __functions:__ 
This folder contains scripts of functions that were written for analysis.
* __presentation:__ This folder contains the pdf of slides that were used to present the findings in the analysis.
* __paper:__ This folder contains the final report of the analysis in pdf and Rmd files. 


## Packages
* dplyr
* ggplot2
* ggthemes
* readr
* scales
* stringr
* tidyr


## Instructions to Produce the Final Paper
Open RStudio and knit the paper
