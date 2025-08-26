# casoils: Soil Health Reporting Tools for California

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/msimmond/ca-soils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/msimmond/ca-soils/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

Introducing {casoils}: an R package for California soil health data visualization and reporting needs. {casoils} provides an enhanced version of the {soils} package with dynamic grouping capabilities for treatment comparisons and automated report generation.

{casoils} is based on and extends the {soils} package, which was developed by the Washington State Department of Agriculture and Washington State University as part of the Washington Soil Health Initiative. This California-specific adaptation provides enhanced functionality while preserving the core methodology of the original package.

## Key Features

- **Dynamic Grouping Support**: New `set_dynamic_scales()` function for flexible treatment comparisons
- **California-Specific Adaptations**: Tailored for UCANR and California Farm Demonstration Network
- **Enhanced Visualization**: Improved plotting capabilities with dynamic symbol and color assignment
- **Automated Reporting**: Streamlined report generation for soil health assessments

## Requirements

The report template uses [Quarto](https://quarto.org/docs/get-started/), which is the next-generation version of R Markdown.

We assume you're using [RStudio v2022.07](https://dailies.rstudio.com/version/2022.07.2+576.pro12/) or later for editing and previewing Quarto documents. We **strongly recommend** you use the [latest release of RStudio](https://posit.co/download/rstudio-desktop/) for support of all Quarto features.

To render Microsoft Word (MS Word) documents, you must have MS Word installed and activated.

## Installation

Install from GitHub:

``` r
# Install from GitHub
remotes::install_github("msimmond/ca-soils")
```

Or using {pak}:

``` r
# Install pak if not already installed
# install.packages("pak")
pak::pkg_install("msimmond/ca-soils")
```

Load the package:

``` r
library(casoils)
```

## Usage

{casoils} was developed to work 'out of the box' so you can immediately install and render an example report. The package includes all the functionality of the original {soils} package plus enhanced dynamic grouping capabilities.

## Acknowledgement and Citation

This report was generated using the {casoils} R package.

{casoils} was developed for UC Agriculture and Natural Resources (UCANR) as part of the California Farm Demonstration Network (CFDN).

{casoils} is based on and extends the {soils} package, which was developed by the Washington State Department of Agriculture (WSDA) and Washington State University (WSU) as part of the Washington Soil Health Initiative.

**To cite {casoils} in publications, please use:**

Simmonds M. 2025. casoils: California Soil Health Reporting Tools. UC Agriculture and Natural Resources / California Farm Demonstration Network. https://github.com/msimmond/ca-soils

**To cite {soils} in publications, please use:**

Ryan JN, McIlquham M, Sarpong KA, Michel LM, Potter TS, Griffin LaHue D, Gelardi DL. 2024. Visualize and Report Soil Health Data with {soils}. Washington Soil Health Initiative. https://github.com/WA-Department-of-Agriculture/soils

## Credits

- {soils} adapts from RStudio Project Templates, {ratlas}, {quartotemplate}, and {golem}.
- Text and figures in {soils} were adapted from WSU Extension publication #FS378E Soil Health in Washington Vineyards.
- Indicators and template adaptations in {casoils} were developed in collaboration with UCANR's California Farm Demonstration Network (CFDN).

## License

This package is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Contributing

This package is developed for UCANR's California Farm Demonstration Network. For questions or contributions, please contact the maintainer.
