# Introduction

This repository contains R code used to correct data generated in the Stable Isotope Ecosystems Laboratory of UC Merced. The `.Rmd` files are tailored to the specific isodat export `.csv` files used by SIELO, but they could be modified to work with other files.

# Instructions

1.  Export your data as a `.csv` using the correct isodat export template

    -   EA: `CN_all.wke`

    -   TCEA: $\delta^{18}O$ Only: `CO export.wke`

    -   TCEA: $\delta^{18}O$ & $\delta^2H$: `H2 - CO export.wke`

    -   Gasbench: `carbonate.wke`

2.  Copy your exported data to the appropriate folder in `~/Box Sync/Data Repository/`.

3.  Open the appropriate data reduction `.Rmd`

    -   EA: `EA_tidy_reduction.Rmd`

    -   TCEA: $\delta^{18}O$ Only: `TCEA_tidy_reduction.Rmd`

    -   TCEA: $\delta^{18}O$ & $\delta^2H$: `TCEA_tidy_hydrogen.Rmd`

    -   Gasbench: `GB_tidy_reduction.Rmd`

4.  Update the file path to the raw data file you want to process. This is under the `## File Path` header in all the data reduction `.Rmd` templates. You should only have to change the last part of the file path that contains the file name.

    -   line 80 in `EA_tidy_reduction.Rmd`

    -   line 79 in `TCEA_tidy_reduction.Rmd`

    -   line 65 in `GB_tidy_reduction.Rmd`

5.  ***Caution talk to Robin before doing this***: If necessary change standards used and reduction parameters. Most analyses will not require any changes.

6.  Click `knit` in RStudio

7.  Inspect The report for problems.

8.  Once you are satisfied with you results *copy* the resulting `.html` file into the appropriate data repository folder. The `.html` file will apear in the `data_reduction` folder on the desktop. It must be copied to `~/Box Sync/Data Repository/INSTRUMENT/corrected/YEAR/RUNID` where `INSTRUMENT`, `YEAR`, and `RUNID` reflect what you are currently working on. The script should automatically create a folder that will contain the `_corrected.csv` , `_raw.csv` , and `_code.Rmd` files. The report file should be saved into this folder following the same naming convention of `runID_report.html`
