# calibrating-building-simulation-review

This repository contains the research compendium for our paper:

> Adrian Chong, Yaonan Gu and Hongyuan Jia, (2021).
> *Calibrating building energy simulation models: A review of methods, inputs and outputs*.
> *In Review*. <https://doi.org/xxx/xxx>

The compendium includes all the data and code needed to reproduce the analysis, figures and text 
associated with the publication

## Citation

Please cite this compendium as:
```
@article{chong2021calibrating,
  title={Calibrating building energy simulation models: A review of methods, inputs and outputs},
  author={Chong, Adrian and Gu, Yaonan and Jia, Hongyuan},
  year={2021},
  note={In Review}
}
```

## Repository Structure
```
./
├── renv.lock                                     # File describing the state of R project library
├── R                                             # 
│   └── figures.R                                 # R script for the figures used in the paper 
│   └── tables.R                                  # R script for the tables used in the paper
├── data                                          # 
│   ├── data.csv                                  # Dataset summarizing papers reviewed
│   └── location_geocode.csv                      # Dataset with locations and their longitude and lattitude
├── paper                                         # 
│   └──figures                                    # Plots generated using figures.R
│   │   └── ...                                   #
│   ├── tex                                       # LaTeX source document 
│   └── bib                                       # Bibliographic information file
│   └── sty                                       # LaTeX style file
│   └── bst                                       # Bibliographic style files
└── calibrating-building-simulation-review.Rproj  # R project file for compendium
```
