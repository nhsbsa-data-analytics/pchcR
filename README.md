# pchcR

This code is published as part of the NHSBSA Official Statistics team's commitment to open code and transparency in how we produce our publications.

# Introduction

This package contains the code used to be able to tansform data collected by the NHS Business Services Authority for use in the Prescribing Costs in Hospitals and the Community (PCHC) publication series. This package includes a `R` folder containing several files with functions specific to this publication. 

Some function, such as `create_fact.R()`, cannot be run in it's entirety by external users. However it should provide information on how the Official Statistics team extract the data from the NHSBSA data warehouse, analyse the data, and produce the outputs released on the NHSBSA website as part of this publication.

The PCHC reproducible analytical pipeline (RAP) is owned and maintained by the Official Statistics team, within a seperate repository.

This package is a work in progress and may be replaced as part of updates and improvements for each new release of the PCHC publication. The functions in the `functions` folder do not contain unit testing, although we will investigate adding this in future.

## Getting started

You can clone the repository containing the RAP through [GitHub](https://github.com/) using the following steps.

In RStudio, click on "New project", then click "Version Control" and select the "Git" option.

Click "Clone Git Repository" then enter the URL of the PCHC GitHub repository (https://github.com/nhsbsa-data-analytics/pchc-pipeline.git). You can click "Browse" to control where you want the cloned repository to be saved in your computer.

You will also need to create a [PAT key](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens).

You can view the [source code for the PCHC RAP](https://github.com/nhsbsa-data-analytics/pchcR) on GitHub.


# Contributing

Contributions are not currently being accepted for this package. If this changes, a contributing guide will be made available.

# Contact Information

If you wish to contact our team to make suggestions, or have ideas you would like to share, please contact us by email via: statistics@nhsbsa.nhs.uk

# License

The `pchcR` package, including associated documentation, is released under the MIT license. Details can be found in the `LICENSE` file.
