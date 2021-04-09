# SSRP-McGuirk-Burke-2020-JPE
 Reproduction code package for McGuirk and Burke (2020)
"The Economic Origins of Conflict in Africa" published in The Journal of Political Economy

Datasets for reproduction have been deposited at the Open Science Framework: https://osf.io/uscmx/

The SSRP reproduction attempt associated with the creation of this repository can be found here: https://www.socialsciencereproduction.org/reproductions/f788f62c-cf96-4f81-897f-d0dd412d30be/index

## Original README file [below] 
*From the replication package provided by the authors, with the published article*

This file describes the replication package for “The Economic Origins of Conflict in Africa”, to be published in The Journal of Political Economy

Content
The replication package consists of four subfolders.

code_rep: 
This folder contains four *.do files necessary to reproduce all of the tables 
(i) replication_main.do
This code produces the main tables (excluding table 6) without Conley SEs.

(ii) replication_appendix.do
This code produces all appendix tables (excluding Afrobarometer tables) without Conley SEs.

(iii) replication_afrobarometer.do
This code produces all Afrobarometer tables in the paper

(iv) replication_conleySE.do
This code produces tables with Conley SEs only (note: it takes a long time to run)

(v) replication_figures.do
This code produces selected figures (please contact us for information on how to produce any of the remaining figures)

This folder also contains code to replicate Figures 4-6 using R. 

Ado files: 
This folder contains the *.ado files needed to run the code in the code_rep folder.

input_rep: 
This contains the four datasets required to run the code:
(i) 1deg_data.dta
(ii) afro_data.dta
(iii) cell_data.dta
(iv) country_data.dta
Plus a csv file necessary to generate figure 7

output_rep:
This folder contains one *.tex file and two subfolders
(i) replication_output.tex contains the latex code required to produce a pdf of the replication output
(ii) tables is an empty folder to which output tables will be written 
(ii) plots is an empty folder to which output figures will be written 

How to
To reproduce any of the results in the paper or online appendix:
1.) Open the relevant do file
2.) Set the path to your local path. 
3.) Run the .do file. 
