# CS333 Final Project — NY State School Report Card 2024-25

Quarto analysis of the New York State School Report Card (SRC2025) exploring:

1. How does money affect pupil performance?
2. How have test scores trended over recent years, and how does that differ between affluent and poorer schools, math vs. reading?
3. In what ways are disadvantaged schools disadvantaged versus their peers?

## Files

- `test_env.qmd` — the Quarto source
- `Test_env.Rproj` — RStudio project file
- `test_env.html` — rendered report
- `absent_by_group.html` — supporting figure

## Data

The raw data is not checked in. To reproduce the analysis, place these CSVs in a `data/` folder at the project root:

```
data/
├── ACC_EM_Growth.csv
├── Annual_EM_ELA.csv
├── Annual_EM_MATH.csv
├── Chronic_Absenteeism.csv
├── Expenditures_per_Pupil.csv
├── Inexperienced_Teachers.csv
└── Out_of_Cert_Teachers.csv
```

These come from the NYSED SRC2025 release (the `SRC2025_Group3.accdb` Access database, exported to CSV).

## Reproduce

```r
# in R
install.packages(c("tidyverse", "ggrepel"))
# then render in RStudio, or:
quarto::quarto_render("test_env.qmd")
```
