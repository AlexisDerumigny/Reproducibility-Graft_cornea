# Reproducibility-Graft_cornea


Structure of the code:

- `preparing_dataframe.Rmd` is a child `Rmarkdown` document that loads the data,
  merges it and does various cleaning and renaming tasks.
  
- `multiple-PCA.Rmd` is the `Rmarkdown` document that produces the Principal Component Analysis.
  
- `multiple-PCA-section.Rmd` is a child `Rmarkdown` document of `multiple-PCA.Rmd`.
  It is used for printing and displaying the various graphical outputs of the PCA.
  
- `linear-regressions.Rmd` is an `Rmarkdown` document that fit and displays
  a few linear regressions to predict the visual acuity (VA).

- `univariate-analysis.Rmd` is an `Rmarkdown` document that produces summary statistics
  of the dataset and displays them in a table.

