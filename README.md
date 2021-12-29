# Socioeconomic and environmental factors explain malaria transmission and inform intervention prioritization in urban Nigeria  

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
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project
Nigeria is one of three countries projected to have the largest absolute increase in the size of its urban population and this could intensify malaria transmission in cities. Accelerated urban population growth is out-pacing the availability of affordable housing and basic services and resulting in living conditions that foster vector breeding and heterogeneous malaria transmission. Understanding community determinants of malaria transmission in urban areas informs the targeting of interventions to population at greatest risk. This repository contains data and analysis scripts for examining factors associated with malaria in urban areas described in the associated manuscript entitled "Socioeconomic and environmental factors explain malaria transmission and inform intervention prioritization in urban Nigeria. 

### Built With
All data is extracted and analyzed using [R](https://www.r-project.org/)
<!-- GETTING STARTED -->
## Getting Started With The Descriptive Modeling Framework
### Prerequisites
Install [R](https://www.r-project.org/) then [RStudio](https://www.rstudio.com/) an integrated development environment for R and Python, with a console, syntax-highlighting editor that supports direct code execution, and tools for plotting, history, debugging and workspace management. 

<!-- USAGE EXAMPLES -->
### Data extraction and cleaning
To replicate the findings, we extract data from various sources, namely, Demographic Health surveys for the years 2010, 2015, 2018, and various rasters. Note that this proccess takes considerable amount of time. However you can skip the step and procced to step on and run a pre extracted and cleaned CSV.

1. [01_data_extractor.R](https://github.com/numalariamodeling/urban-malaria-dhs-publication-2021/blob/main/00_data_extraction/01_data_extractor.R) This script extracts and stores the extracted data into several CSV files. 

2. [01_file_cleaner.R](https://github.com/numalariamodeling/urban-malaria-dhs-publication-2021/blob/main/01_file_cleaner.R) This script cleans and merges the variaous extracted scripts into two CSVs. 

### Descriptive analysis
3. [02_descriptive statistics.R](https://github.com/numalariamodeling/urban-malaria-dhs-publication-2021/blob/main/02_descriptive%20statistics.R) This script loads the cleaned data and conducts descriptive analysis for both the main manuscript and the suplement publication. This script is also associated with a script containing all functions used. 

<!-- ROADMAP -->
## Roadmap



<!-- CONTRIBUTING -->
## Contributing

<!-- LICENSE -->
## License

<!-- CONTACT -->
## Contact
For all other inquiries, contact Ifeoma Ozodiegwu, [ Research Assistant Professor, Northwestern University (NU).](https://www.feinberg.northwestern.edu/faculty-profiles/az/profile.html?xid=52373) Email -[ifeoma.ozodiegwu@northwestern.edu](ifeoma.ozodiegwu@northwestern.edu)

<!-- ACKNOWLEDGMENTS -->
## Acknowledgments
