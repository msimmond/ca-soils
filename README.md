# casoils: Soil Health Reporting Tools for California

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/msimmond/ca-soils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/msimmond/ca-soils/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> Adapted from the [`soils`](https://github.com/WA-Department-of-Agriculture/soils) package developed by the Washington State Department of Agriculture and Washington State University as part of the Washington Soil Health Initiative.  
> `casoils` is a lightly modified version for California by **Maegen Simmonds** for the **California Farm Demonstration Network (CFDN)**, with support from **UC Agriculture and Natural Resources (UCANR)**.

## Overview

`casoils` is an R package for visualizing and reporting soil health data tailored to California’s unique agricultural context. It extends the [`soils`](https://github.com/WA-Department-of-Agriculture/soils) package developed by the Washington State Department of Agriculture and Washington State University, originally part of the **Washington Soil Health Initiative**.

**Note:** `casoils` involves only minor modifications to the original `soils` package — primarily to adjust default options, grouping behavior, and template content to suit California-specific datasets and workflows.

The California-specific `casoils` package enhances that core functionality with:

- Flexible grouping for treatment comparisons  
- Customizable reporting tools  
- Enhanced plotting and visualization options  
- Integration with Quarto templates  

## Requirements

The report template uses [Quarto](https://quarto.org/docs/get-started/), the next-generation version of R Markdown.

We recommend using [RStudio v2022.07](https://dailies.rstudio.com/version/2022.07.2+576.pro12/) or later for editing and previewing Quarto documents. For full feature support, install the [latest release of RStudio](https://posit.co/download/rstudio-desktop/).

> To render Microsoft Word (`.docx`) reports, MS Word must be installed locally.

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

`casoils` is designed to work out of the box. After installation, you can immediately render an example report using built-in data and templates.

## Acknowledgement and Citation

This report was generated using the `casoils` R package.

`casoils` was developed for UC Agriculture and Natural Resources (UCANR) as part of the California Farm Demonstration Network (CFDN).

`casoils` is based on and extends the `soils` package, which was developed by the Washington State Department of Agriculture (WSDA) and Washington State University (WSU) as part of the Washington Soil Health Initiative.

This adaptation includes only minor edits to support California-specific workflows and reporting needs.

**To cite `casoils` in publications, please use:**

Simmonds M. 2025. casoils: California Soil Health Reporting Tools. UC Agriculture and Natural Resources / California Farm Demonstration Network. https://github.com/msimmond/ca-soils

**To cite `soils` in publications, please use:**

Ryan JN, McIlquham M, Sarpong KA, Michel LM, Potter TS, Griffin LaHue D, Gelardi DL. 2024. Visualize and Report Soil Health Data with {soils}. Washington Soil Health Initiative. https://github.com/WA-Department-of-Agriculture/soils

## Credits

- `soils` adapts from RStudio Project Templates, `ratlas`, `quartotemplate`, and `golem`.
- Text and figures in `soils` were adapted from WSU Extension publication #FS378E Soil Health in Washington Vineyards.
- `casoils` involved light modifications to support California-specific indicators, grouping behavior, and reporting templates in collaboration with UCANR’s California Farm Demonstration Network (CFDN).

## License

This package is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Contributing

This package is developed for UCANR's California Farm Demonstration Network. For questions or contributions, please contact the maintainer.
