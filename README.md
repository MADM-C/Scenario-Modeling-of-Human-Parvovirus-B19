# Human Parvovirus B19 (B19) Burden of Disease and Scenario Modeling Toolset

## Table of Contents
- [Project Description](#project-description)
- [Intended Use Cases and Limitations](#intended-use-cases-and-limitations)
- [Installation and Run Guide](#installation-and-run-guide)
- [Organization Information](#organization-information)
- [Additional Information](#additional-information)
- [License](#license)

## Project Description
Human parvovirus B19 (B19) is a common infection that typically causes mild symptoms but can cause severe fetal complications among pregnant persons. There are limited data to inform public health responses because B19 is not a reportable disease. The most recent uptick in B19 cases was observed in 2024 across Europe and the U.S., triggering a CDC Health Alert Network notification in August 2024. This increase was supported by electronic health record (EHR) data from Minnesota, which found 19 B19-associated pregnancy complications within 60 days of a B19 diagnosis in a 10-month period in 2024, compared to only 28 complications in the prior 60-month period from 2019-2023.  
  
To support public health responses for current and future B19 outbreaks, we developed 3 tools in partnership with the Minnesota Department of Health (MDH) and the Minnesota Electronic Health Records Consortium (MNEHRC). These tools are meant to help provide public health practitioners with estimates of burden of disease and the impact of varying public health interventions on fetal outcomes.

- The Burden of Disease tool is a spreadsheet-based model that estimates the total number of B19-related severe fetal outcomes based on user-defined inputs.
- The Increased Detection Scenariom Modeling (IDS) tool uses a decision tree model in R to estimate the impact of improving detection of B19 as a scenario in averting B19-related fetal deaths and improving B19 transfusions.
- The Hypothetical Vaccination and Screening Scenario Modeling (HyVSS) tool uses a decision tree model in R to estimate the impact of a hypothetical vaccination and screening in different scenarios on averting B19-related fetal deaths and improving B19 transfusions.

## Intended Use Cases and Limitations

## Installation and Run Guide

## Organization Information

## Additional Information

## License



## About

- Welcome to the Midwest Analytics and Disease Modeling Center's ([MADMC](https://www.sph.umn.edu/research/centers/midwest-analytics-and-disease-modeling/)) Human Parvovirus B19 (B19) Toolset.
- We have developed 3 tools in partnership with the Minnesota Department of Health (MDH) and the Minnesota Electronic Health Records Consortium (MNEHRC).
- These tools are meant to help provide public health practitioners with estimates of burden of disease and the impact of varying public health interventions on fetal outcomes.

## Human Parvovirus B19 (B19)

- B19 is respiratory illness that usually manifests as a mild disease.
- However, in pregnant individuals development of B19 infection can spread to the fetus and cause severe fetal complications. 
- There was a recent uptick in B19 cases noted by the [CDC](https://www.cdc.gov/han/2024/han00514.html) and [MDH](https://www.health.state.mn.us/communities/ep/han/2024/aug16parvo.pdf).


## Tool 1 - Burden of Disease Spreadsheet

- The Burden of Disease tool is a spreadsheet-based model that estimates the total number of B19-related severe fetal outcomes based on user-defined inputs.
- This spreadsheet based tool can be found in the "Burden-of-Illness-Tool" folder.
- Open the folder and select the "MADMC B19 Pregnancy Outcome Estimate Tool.xlsx" and open in Microsoft Excel. 
- Detailed instructions and information about the tool and user options are included within the file itself within each sheet. 


## Tool 2 - Increased Detection Scenario Modeling (IDS)

- This IDS tool uses a decision tree model in R to estimate the impact of improving detection of B19 as a scenario in averting B19-related fetal deaths and improving B19 transfusions. 
- The IDS tool can be found in the "R" folder, select the "05_detection_model_analysis.Rmd" in RStudio and run.
- The .Rmd will pull in the parameter values from the "1_params_functions.R" file and the functions modeling the decision tree from the "02_detection_model_functions.R" file.
- Results and figures will populate under each code chunk for the .Rmd. Figures may also print as a .png into the "results" and "IDS Plots" folders. 


## Tool 3 - Hypothetical Vaccination and Screening Scenario Modeling (HyVSS)

- The HyVSS tool uses a decision tree model in R to estimate the impact of a hypothetical vaccination and screening in different scenarios on averting B19-related fetal deaths and improving B19 transfusions.
- The HyVSS tool can be found in the "R" folder, select the "05_screen_vax_model_analysis.Rmd" in RStudio and run.
- The .Rmd will pull in the parameter values from the "01_params_functions.R" file and the functions modeling the decision tree from the "02_screen_vax_model_functions.R" file.
- The validation functions for the HyVSS tool will be pulled from the "04_screen_vax_validation.R" file.
- Results and figures will populate under each code chunk for the .Rmd. Figures may also print as a .png into the "Results" and "HyVSS Plots" folders.


## Contact
- Please reach out with any questions or concerns to us directly at madmc@umn.edu








