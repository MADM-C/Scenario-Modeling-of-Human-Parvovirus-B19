# Human Parvovirus B19 (B19) Burden of Disease and Scenario Modeling Toolset


## Table of Contents
- [Project Description](#project-description)
- [Intended Use Cases and Limitations](#intended-use-cases-and-limitations)
- [Installation and Run Guide](#installation-and-run-guide)
- [Organization Information](#organization-information)
- [Additional Information](#additional-information)
- [License](#license)


## Project Description
Human parvovirus B19 (B19) is a common infection that typically causes mild symptoms but can cause severe fetal complications among pregnant persons. There are limited data to inform public health responses because B19 is not a reportable disease. The most recent uptick in B19 cases was observed in 2024 across Europe and the U.S., triggering a [CDC](https://www.cdc.gov/han/2024/han00514.html) Health Alert Network notification in August 2024 and an [MDH](https://www.health.state.mn.us/communities/ep/han/2024/aug16parvo.pdf) Advisory. This increase was supported by electronic health record (EHR) data from Minnesota, which found 19 B19-associated pregnancy complications within 60 days of a B19 diagnosis in a 10-month period in 2024, compared to only 28 complications in the prior 60-month period from 2019-2023.  
  
To support public health responses for current and future B19 outbreaks, we developed 3 tools in partnership with the Minnesota Department of Health (MDH) and the Minnesota Electronic Health Records Consortium (MNEHRC). These tools are meant to help provide public health practitioners with estimates of burden of disease and the impact of varying public health interventions on fetal outcomes.

- The Burden of Disease tool is a spreadsheet-based model that estimates the total number of B19-related severe fetal outcomes based on user-defined inputs.
- The Increased Detection Scenariom Modeling (IDS) tool uses a decision tree model in R to estimate the impact of improving detection of B19 as a scenario in averting B19-related fetal deaths and improving B19 transfusions.
- The Hypothetical Vaccination and Screening Scenario Modeling (HyVSS) tool uses a decision tree model in R to estimate the impact of a hypothetical vaccination and screening in different scenarios on averting B19-related fetal deaths and improving B19 transfusions.


## Intended Use Cases and Limitations
As B19 is not a reportable disease and limited data are available to inform public health responses, this project sought to fill knowledge gaps by estimating burden of disease, and providing scenario models to help assess the benefits of various interventions or policy changes on minmizing B19-related complications in pregnancy.   

Our project and models did have some limitations. Validating model results was challenging, due to the lack of published data on B19. The available literature which was used to inform model paramaters was also often from studies conducted over twenty years prior, or from other countries that are not representative of the U.S. population, as a result our models relied heavily on assumptions and clinical expertise from subject matter experts. We did conduct sensitivity analyses on these parameters to help understand the parameters with the greatest impact on model results. Finally, we made simplyfing assumptions like maternal infection occuring only between 14-20 weeks gestation (WG) were intervenable and maternal infections <13 WG were excluded from out estimates on stillbirth. 

## Installation and Run Guide
Tool 1 - Burden of Disease Spreadsheet
- This spreadsheet based tool can be found in the "Burden-of-Illness-Tool" folder.
- Open the folder and select the "MADMC B19 Pregnancy Outcome Estimate Tool.xlsx" and open in Microsoft Excel. 
- Detailed instructions and information about the tool and user options are included within the file itself within each sheet.   

Tool 2 - Increased Detection Scenario Modeling (IDS)
- The IDS tool can be found in the "R" folder, select the "05_detection_model_analysis.Rmd" in RStudio and run.
- The .Rmd will pull in the parameter values from the "1_params_functions.R" file and the functions modeling the decision tree from the "02_detection_model_functions.R" file.
- Results and figures will populate under each code chunk for the .Rmd. Figures may also print as a .png into the "results" and "IDS Plots" folders.

Tool 3 - Hypothetical Vaccination and Screening Scenario Modeling (HyVSS)
- The HyVSS tool can be found in the "R" folder, select the "05_screen_vax_model_analysis.Rmd" in RStudio and run.
- The .Rmd will pull in the parameter values from the "01_params_functions.R" file and the functions modeling the decision tree from the "02_screen_vax_model_functions.R" file.
- The validation functions for the HyVSS tool will be pulled from the "04_screen_vax_validation.R" file.
- Results and figures will populate under each code chunk for the .Rmd. Figures may also print as a .png into the "Results" and "HyVSS Plots" folders.


## Organization Information
This project was developed by the Midwest Analytics and Disease Modeling Center ([MADMC](https://www.sph.umn.edu/research/centers/midwest-analytics-and-disease-modeling/)) a CDC INSIGHT NET Center.  
  
The main contributing authors are as follows: Ritesh Sivakumar, Matthew Kaufmann, Margo Wheatley, Xiao Zang, Lindsey Erickson, Elizabeth Dufort, Sarah Lim, Ruth Lynfield, Stephen Contag, Kristin Sweet, and Eva Enns.  

Please contact us at madmc@umn.edu. 


## Additional Information
There are currently 2 papers under review for the modeling work conducted in this project. MADMC has also presented multiple posters and presentations about our B19 work at various conferences and seminars. 


## License
Copyright (c) 2022 Consortium of Infectious Disease Modeling Hubs

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
