# Simulated Data and Custom Scripts
This repository contains data for performing simulations with SPLATCHE3 and the custom R scripts used in the following manuscript:  Quilodrán CS, Tsoupas A and M Currat. In review. The spatial signature of introgression after a biological invasion with hybridization.

There are three main folders: 
-	Settings: the setting files of SPLATCHE3 used in all explored scenarios. The example folder for each scenario (01_newset) contains the setting file and setting folder needed to run SPLATCHE3. This example folder contains a single simulation of the interbreeding rate (MigrRate_P1_to_P2 and MigrRate_P2_to_P1) that generates an output of 10,000 simulated loci. The folder also contains a public version of SPLATCHE3 for Linux. The software version for other platforms (Mac OS X and Windows) can be obtained from “http://www.splatche.com/splatche3".
-	Results: all results obtained in the different scenarios. The three simulations in a square world are presented for the invasive (NC files) and local organisms (NCbis). The neanderthal scenario presents the simulated proportion of introgression in modern humans sampled in France and China.  
-	Rcustom: custom R functions and files used for plotting the results. 

There are also four R scripts for plotting all main figures in the manuscript. 

### Acknowledgments 
This study was financed by grants from the Swiss National Science Foundation n° 31003A_182577 to MC and P400PB_183930 to CSQ. All computations were performed using the High-Performance Computing (HPC) cluster at baobab.unige.ch.
