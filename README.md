# Project2

This repo details the Shiny application, statis data analysis

Repo Structure:

..\\Root

ConvertCSVtoRDS.R (Helper File for converting CSV to RDS)

EDA.qmd (Notebook containing data analysis and plots for static data set)

-------\\app

--------------- app.R (R Shiny application code)

--------------- \\rseconnect

----------------\\data (folder where data resides)

----------------\\www

----------------------- football.png (Image of a football that is used in the app)

## **Important**

As the data set used for analysis in this report was larger than GitHub's allowable file size and the data set was too large to load quickly enough for the web-hosted R-Shiny application to load without throwing an error, the user must follow the instructions to generate the data for this report:

1.  Download the data set from [this link](https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016/data?select=NFL+Play+by+Play+2009-2016+%28v3%29.csv)
2.  Rename the file "nfl.csv" and place it in the project folder "app/data"
3.  Run the file "ConvertCSVtoRDS.R" to convert the large .csv file to a smaller .rds file, which will be loaded more efficiently when the app loads.
4.  Deploy Shiny application and only upload the .rds file
5.  Remove the .csv and .rds data files from the folder, as both are larger than the allowable file size in the Repo
