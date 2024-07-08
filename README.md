# Purpose

This R project contains all data and R scripts used to produce the analyses, figures, and tables in "Does migration constrain glucocorticoid phenotypes? Testing corticosterone levels during breeding in migratory versus resident birds." The project asks whether migratory status predicts baseline and stress-induced corticosterone levels in birds. The main dataset used in this project, HormoneBase, is publicly available at https://hormonebase.org/database/. For the analyses in this project, we used the subset of the HormoneBase dataset that contained measurements of corticosterone in birds. We performed research on each bird population in the dataset to determine whether it was migratory, resident, or unknown.

What follows are descriptions of the contents of each folder in this R project such that the reader can use these files to reproduce the analyses for this manuscript.

Authors: Jennifer J. Uehling, Emma Regnier, and Maren N. Vitousek

# Folders

## 0_data

Contains the two data files used for the analyses for this project.

**finalTree2018.tre**: tree file from previous HormoneBase papers (ex: Vitousek et al. 2019, AmNat, "Macroevolutionary patterning in glucocorticoids suggests different selective pressures shape baseline and stress-induced levels"). For each MCMCglmm, this tree is pruned and then used as a random effect to control for phylogeny.

**HormoneBase_v1_Migration_Data_Jenny_2023.csv**: csv file that includes all rows in HormoneBase with bird corticosterone measurements, and information about whether each bird population measured is migratory, resident, or unknown. To create this file, we did the following (this list of steps also appears in the R code **MigCortHB.R**):

  - Downloaded HormoneBase dataset from https://hormonebase.org/database/.
  - Deleted all records not from birds.
  - Removed all records without corticosterone measurements.
  - Researched each bird species (and subspecies and population, where relevant) to determine whether it is Migratory or Resident. For populations where this was unclear, we classified them as "Unknown."

Descriptions of the variables associated with each HormoneBase entry can be found at: https://hormonebase.org/database/. We added four additional columns to the HormoneBase dataset during our research about each population's migratory status. They are as follows:

- *Migratory_Status*: indicates whether the population is Migratory, Resident, or Unknown.
- *Mixed_Status_Species*: indicates whether this species is known to have a mixture of migratory statuses among different populations/individuals (Yes, No, or Unknown). This variable was not ultimately used in any analyses.
- *Notes_Migration*: contains notes about migratory status and how classification decisions were made.
- *Additional_Sources_Migration*: contains URLs of any additional sources that were used to determine each population's migratory status, beyond the specific paper where the corticosterone measurements were reported and Birds of the World.

## 1_r_code

Contains the script **MigCortHB.R** used to produce all analyses in this paper, including the contents of the tables and figure.

## 2_figures

Contains the single figure in this paper, including a PDF version (**Sampling_locations.pdf**) and a tiff version (**Sampling_locations.tiff**).

## 3_other_output

Contains output produced by the script **MigCortHB.R** other than figures. Each file is described below:

**Mig_bird_tree_lat.nex**: the tree pruned just for the model examining the relationship between migratory status and breeding latitude. This tree is created as part of the script **MigCortHB.R**.

**Mig_bird_tree_baseline.nex**: the tree pruned just for the model examining the relationship between migratory status and baseline corticosterone. This tree is created as part of the script **MigCortHB.R**.

**Mig_bird_tree_stress.nex**: the tree pruned just for the model examining the relationship between migratory status and stress-induced corticosterone. This tree is created as part of the script **MigCortHB.R**.

**Pop_ID_list_for_checking.xlsx**: an Excel spreadsheet used to manually examine the latitude values for each population and make decisions about whether populations should be classified together. See **MigCortHB.R** for more details about this process.

**Pop_ID_list.csv**: a csv file that was converted into the Excel spreadsheet (**Pop_ID_list_for_checking.xlsx**) used to manually examine the latitude values for each population and make decisions about whether populations should be classified together. See **MigCortHB.R** for more details about this process.

## 4_other_files

Contains one additional file that was used to make decisions about whether to group populations together:

**Copy of HormoneBase.Master.Apr26_2018.xlsx**: Master spreadsheet used to make calls about whether populations should be classified together. See **MigCortHB.R** for more details about this process.
