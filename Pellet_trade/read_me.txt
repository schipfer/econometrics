The research for this paper is performed in the statistical programming environment R. 
I recommend to install R and R-Studio for executing the code which will re-produce all tables and figures from the manuscript.

(1) Trade data is derived from Eurostat and prepared as a panel data set and attached as .rds (R-data) file.
(2) Wood pellet prices are derived from various sources as explained in the manuscript. 
Prices for France and Austria are publically available, thus included in the data set prices.rds already.
Prices for Germany and Italy will have to be requested from DEPV and AIEL respectively and added to the prices.rds data set yourself.
(3) Then add your working directory in the top of the PelletEUtrade.R code and run all.

#############################################################################################
# Filename: PelletEUtrade.R
#
# Author: Fabian Schipfer (FS)
# Created: 04-Febr-2017
#
# Version: 1.0
#
# Changed on: 23-Apr-2017 by (FS)
#         preparation for submission in Energy Economics: documentation updated; 
# Run on: RStudio Version 0.98.1049 with R version 3.1.1 (2014-07-10)
#############################################################################################
#
# All calculations, graphics and tables presented in manuscript
#
#
# Data files required:   trade.rds, prices.rds
# Subfunctions:          none
# R-files required:      none
# other files:           none
# Problems:              none
#############################################################################################


