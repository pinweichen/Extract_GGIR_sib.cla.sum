<!-- ABOUT THE PROJECT -->
## About The Project
This repository contains a custom script that can extract the output of the GGIR adapted sleep algorithm without the sleep guider (GGIR part 4) implemented. The "GGIR_vs_PSG_sleep_epoch_extraction" script contained a function that converts the sleep periods determined by the sleep algorithm into 30-second (or your selected time window) epoch-by-epoch labels. You can use this result to compare with the PSG data. This same method was employed in our paper titled "Performance of an Automated Sleep Scoring Approach for Actigraphy Data in Children and Adolescents."

Please cite this paper when using this function.


<!-- GETTING STARTED -->
## How to use
### Prerequisites
packages: data.table and tidyverse 
data: Successful run of GGIR (version 2.10-0 to 3.2-8). It may work with future GGIR versions.

### Installation
No installation needed. The script contains two functions that you can use as helper functions in designing your sleep/wake comparison.

<!-- USAGE EXAMPLES -->
## Usage
1. Generate sleep algorithm results (without the sleep guider use) in comparing the gold standard sleep study results. 
2. Generate epoch-by-epoch results of sleep-wake to compare across different algorithms.


