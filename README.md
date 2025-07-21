# Analysis of aspergillosis and histoplasmosis inpatient encounters

This repository contains the analysis code and data processing scripts for the manuscript titled *A Comparative Analysis Estimating Aspergillosis and Histoplasmosis Encounters among Inpatients in the United States Using Multi-Regional Electronic Health Record and National Discharge Data, 2014-2020*. The study compares inpatient encounters for aspergillosis and histoplasmosis between the Oracle EHR Real World Data (OERWD) and the Healthcare Cost and Utilization Project National Inpatient Sample (HCUP NIS).

## Project Structure

```
├── data/                       # Raw and processed data files (not included in repo)
├── scripts/                    # Data processing and analysis scripts
│   ├── 01_data_cleaning.R      # Data cleaning and preprocessing including applying and calculating weights
│   ├── 02_analysis.R           # Statistical analysis
│   ├── 03_figures.R            # Generation of figures
└── README.md                   # Project description and instructions
```

## Requirements

- Required packages:
```
library(broom)  
library(car)  
library(cowplot)  
library(data.table)  
library(dplyr)  
library(ggplot2)  
library(Kendall)  
library(magrittr)  
library(purrr)  
library(readr)  
library(stringr)  
library(survey)  
library(sf)  
library(tidyr)  
library(usmap)
```

## Data Access
The datasets used in this analysis (OERWD and HCUP NIS) are not included in this repository due to licensing restrictions. Access to these datasets can be requested through:

- **OERWD**: Oracle Real World Data (Contact Oracle for access)
- **HCUP NIS**: Healthcare Cost and Utilization Project ([HCUP NIS Data Access](https://www.hcup-us.ahrq.gov/nisoverview.jsp))

## Usage
To replicate the analysis:

1. Clone the repository:
```
git clone https://github.com/your-username/your-repo-name.git
cd your-repo-name
```

2. Run the scripts in sequence:
```
Rscript scripts/01_data_cleaning.R
Rscript scripts/02_analysis.R
Rscript scripts/03_figures.R
```
## Contact
For questions or collaborations, please contact *Britt LM Bustamante* at *blmb@berkeley.edu*.
