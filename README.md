# calibrating-building-simulation-review

This repository contains the research compendium for our paper:

> Adrian Chong, Yaonan Gu and Hongyuan Jia, (2021).
> *Calibrating building energy simulation models: A review of the basics to guide future work*.
> Energy and Buildings, 111533. doi: <https://doi.org/10.1016/j.enbuild.2021.111533>


The compendium includes all the data and code needed to reproduce the analysis, figures and text 
associated with the publication

## Citation

Please cite this compendium as:
```
@article{chong2021calibrating,
  title={Calibrating building energy simulation models: A review of the basics to guide future work},
  author={Chong, Adrian and Gu, Yaonan and Jia, Hongyuan},
  journal={Energy and Buildings},
  pages={111533},
  year={2021},
  doi={https://doi.org/10.1016/j.enbuild.2021.111533},
  publisher={Elsevier}
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
│   └── figures                                   # Plots generated using figures.R
│   │   └── ...                                   #
│   ├── tex                                       # LaTeX source document 
│   └── bib                                       # Bibliographic information file
│   └── sty                                       # LaTeX style file
│   └── bst                                       # Bibliographic style files
└── calibrating-building-simulation-review.Rproj  # R project file for compendium
```

## Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)
