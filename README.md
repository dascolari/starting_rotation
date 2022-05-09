Welcome to Predipitch, our Github repository for predicting pitches in Major League Baseball. This is completed as a project for ECO395M, Data Mining and Machine Learning. 

In this repository, we develop three models for predicting the next pitch in a baseball at-bat using random forests with different in-game features. A write-up explains our data, the methodology, the results, and some interesting conclusions about the models. We also create a Shiny App to allow the user to enter a game scenario and pitcher to see the likelihood of predicting a given pitch. 

## Reproducibility and Load Order

The following scripts allow the writeup to be reproduced:
- dugout.R (similar to a include.R) loads the necessary libraries and establishes the file path
- import.R* converts the enormous dataset into a more manageable subset of MLB pitchers
- pitchers.R* pre-processes the data so that it is easily fed into the predipitch script
- predi_pitch.R* takes the subset of pitchers and creates a predictive model for each one (**NOTE:** This will take upwards of 30 minutes to run, so we highly recommend not running it)
- performance.R creates a table that shows the performance of the models
- kershaw_sequence.R looks at one pitcher and shows sequencing trends throughout the game in different scenarios
- situation.R allows the manual entry of at-bat scenarios (balls, strikes, etc.) to create a prediction
- predipitch.Rmd creates the final write-up with visualizations

* Due to the size of the datasets at hand, running these scripts can take a *long* time. However, they output .RDs which can referenced directly from the Github so you may skip these scripts and run only the other scripts of interest. Just note, dugout.R should be run in either case. 

## A note about data:

Due to the size of datasets in this folder, we gitignore all .csv files. You can find some of the data in archive.zip. However, download pitches.zip from one of the below links to get pitches data

- data is from here: http://inalitic.com/datasets/mlb%20pitch%20data.html
- pitchers data dropbox: https://www.dropbox.com/s/9gyz3ujwx7jsh5j/pitches.zip?dl=0
- data is built from here: https://www.kaggle.com/datasets/pschale/mlb-pitch-data-20152018
- detailed info on the data: https://docs.google.com/document/d/1ztD20pt5K0HUi2EcJHT4SYdOZw9YPYhtLUmi8BpInuA/edit?pref=2&pli=1#heading=h.mnao9thv84r1
- hitting here: https://www.baseball-reference.com/leagues/majors/2015-standard-batting.shtml?sr&utm_source=direct&utm_medium=Share&utm_campaign=ShareTool#players_standard_batting



