# Socioeconomic, demographic and environmental factors inform intervention prioritization in urban Nigeria 

<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#Getting-Started-With-The-Descriptive-Modeling-Framework">Getting Started With The Descriptive Modeling Framework</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#Data-extraction-and-cleaning">Data extraction and cleaning</a></li>
        <li><a href="#Descriptive-analysis">Descriptive analysis</a></li>
        <li><a href="#GLM-Modeling-analysis">GLM Modeling analysis</a></li>
      </ul>
    </li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project
Nigeria is one of three countries projected to have the largest absolute increase in the size of its urban population and this could intensify malaria transmission in cities. Accelerated urban population growth is out-pacing the availability of affordable housing and basic services and resulting in living conditions that foster vector breeding and heterogeneous malaria transmission. Understanding community determinants of malaria transmission in urban areas informs the targeting of interventions to population at greatest risk. This repository contains data and analysis scripts for examining factors associated with malaria in urban areas described in the associated manuscript entitled "Socioeconomic, demographic and environmental factors inform intervention prioritization in urban Nigeria". 

### Built With
All data is extracted and analyzed using [R](https://www.r-project.org/)
<!-- GETTING STARTED -->
## Getting Started With The Descriptive Modeling Framework
### Prerequisites
Install [R](https://www.r-project.org/) then [RStudio](https://www.rstudio.com/) an integrated development environment for R and Python, with a console, syntax-highlighting editor that supports direct code execution, and tools for plotting, history, debugging and workspace management. 

<!-- Data extraction and cleaning -->
### Data extraction and cleaning
To replicate the findings, we extract data from various sources, namely, Demographic Health surveys for the years 2010, 2015, 2018, and various rasters. Note that this proccess takes considerable amount of time. However you can skip the step and procced to Descriptive analysis step and run a pre extracted and cleaned CSV.

1. [00_data_extraction](https://github.com/numalariamodeling/urban-malaria-dhs-publication-2021/tree/main/00_data_extraction). Scripts in this folder support extraction of study data and computation of data summaries for survey clusters. Scripts in [00_era5_temperature_precipitation_download_raster_generation](https://github.com/numalariamodeling/urban-malaria-dhs-publication-2021/tree/main/00_data_extraction/00_era5_temperature_precipitation_download_raster_generation) folder are used to download and transform era5 netcdf temperature and rainfall data into a usable format. The [01_data_extractor](https://github.com/numalariamodeling/urban-malaria-dhs-publication-2021/blob/main/00_data_extraction/01_data_extractor.R) script extracts malaria test positivity data and data for covariates and stores it several CSV files. All custom functions used are also provided within this folder. 

2. [01_file_cleaner.R](https://github.com/numalariamodeling/urban-malaria-dhs-publication-2021/blob/main/01_file_cleaner.R) This script cleans and merges the various extracted scripts into two CSVs. 

<!-- Descriptive analysis -->
### Descriptive analysis

3. [02_descriptive statistics.R](https://github.com/numalariamodeling/urban-malaria-dhs-publication-2021/blob/main/02_descriptive%20statistics.R) This script loads the cleaned data and conducts descriptive analysis for both the main manuscript and the suplement publication. All custom functions used are provided [here](https://github.com/numalariamodeling/urban-malaria-dhs-publication-2021/blob/main/other_functions/descriptive_analysis_functions.R)


<!-- GLM Modeling analysis -->
### GLM Modeling analysis

4. [03_glm_modelling.R](https://github.com/numalariamodeling/urban-malaria-dhs-publication-2021/blob/main/03_glm_modelling.R) This script loads pre-cleaned [study data](https://github.com/numalariamodeling/urban-malaria-dhs-publication-2021/tree/main/data) and conducts GLMM modeling analysis. Custom functions used are provided [here](https://github.com/numalariamodeling/urban-malaria-dhs-publication-2021/blob/main/other_functions/multivariate_functions.R)

<!-- CONTACT -->
## Contact
For inquiries, contact Ifeoma Ozodiegwu, [ Research Assistant Professor, Northwestern University (NU).](https://www.feinberg.northwestern.edu/faculty-profiles/az/profile.html?xid=52373) Email -[ifeoma.ozodiegwu@northwestern.edu](ifeoma.ozodiegwu@northwestern.edu) or Chilo Chiziba, [Research Assistant @ NU Malaria Modeling Team](https://www.numalariamodeling.org/team.html)

<!-- ACKNOWLEDGMENTS -->
## Acknowledgments
- [Jaline Gerardin - Principal Investigator @ NU Malaria Modeling Team](https://www.feinberg.northwestern.edu/faculty-profiles/az/profile.html?xid=44305)
- [Ousmane Diallo @ NU Malaria Modeling Team](https://www.numalariamodeling.org/team.html)
- [Amelia Bertozzi-Villa @ Institute for Disease Modeling](https://www.idmod.org/user/146)
- [Dan Weiss @ Faculty of Health Sciences, Curtin University](https://staffportal.curtin.edu.au/staff/profile/view/dan-weiss-0ef1c9d2/)
- [Laina Mercer @ PATH](https://www.path.org/)
- [All NU Malaria Modeling Team](https://www.numalariamodeling.org/team.html)


## DOI: <a href="https://zenodo.org/badge/latestdoi/389660281"><img src="https://zenodo.org/badge/389660281.svg" alt="DOI"></a>

