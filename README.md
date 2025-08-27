# Soil Health Reports Shiny App

A modular Shiny application for generating soil health reports using the `soils` package and Quarto.

## Features

- **Two modes**: Single farm upload or full dataset analysis
- **On-farm comparisons**: Distinguish between different fields on a farm
- **Modular design**: Clean separation of concerns with Shiny modules
- **Progress tracking**: Real-time progress indicators during report generation
- **Caching**: Memoized report generation for performance
- **Multiple outputs**: HTML and PDF download options

## Prerequisites

### Required Software

1. **R** (version 4.0 or higher)
2. **Quarto CLI** - Install from https://quarto.org/
3. **RStudio** (recommended for development)

### Required R Packages

The app will automatically install most dependencies, but you may need:

```r
install.packages(c("renv", "remotes", "devtools"))
```

## Setup

### 1. Clone and Navigate

```bash
cd ca-soil-health-reports
```

### 2. Initialize R Environment

```r
# Initialize renv for reproducible dependencies
renv::init()

# Restore packages (if renv.lock exists)
renv::restore()
```

### 3. Verify Quarto Installation

```r
# Check if Quarto is available
Sys.which("quarto")
```

If this returns an empty string, install Quarto from https://quarto.org/

### 4. Run the App

```r
# In RStudio: Open app.R and click "Run App"
# Or from R console:
shiny::runApp()
```

## Usage

### Single Mode (Default)

1. **Upload Data**: Choose "Upload CSV file" and select your soil health data CSV
2. **Select Producer**: Choose the producer from the dropdown
3. **Select Year**: Choose the year for analysis
4. **Select Field** (optional): Choose a specific field for detailed analysis
5. **Generate Report**: Click "Generate Report" to create the analysis

### Server Mode

1. **Select Dataset**: Choose "Use server dataset" and select from available datasets
2. **Configure Options**: Set producer, year, and field as above
3. **Generate Report**: Click "Generate Report"

### Report Options

- **Include regional comparisons**: Compare to background dataset
- **Include field maps**: Generate interactive field maps (if coordinates available)

## Data Requirements

Your CSV file should contain these columns:

### Required Columns
- `producer_id` (or `Farm.Name`)
- `year`
- `field_id` (or `Treatment.ID`)

### Optional Columns
- `sample_id` (will be auto-generated if missing)
- `latitude`, `longitude` (for maps)
- Measurement columns (defined in data dictionary)

### Example Data Structure

```csv
producer_id,year,field_id,latitude,longitude,organic_matter,ph,ec
"Example Farm",2024,"Field 1",40.0,-120.0,2.1,6.8,0.5
"Example Farm",2024,"Field 2",40.0,-120.0,1.9,7.1,0.4
```

## Configuration

The app uses a configuration-based approach for flexibility and maintainability.

### Filter Configuration

Edit `config/filter-config.csv` to customize which columns can be used for filtering:

```csv
column_name,filter_label,filter_type,required
site_type,Site Type,dropdown,FALSE
crop,Crop,dropdown,FALSE
texture,Texture,dropdown,TRUE
```

- **column_name**: The actual column name in your data
- **filter_label**: Display name for the filter dropdown
- **filter_type**: Type of filter (currently only "dropdown" supported)
- **required**: Whether this filter is required (TRUE/FALSE)

To add a new filter, simply add a row to this CSV file. No R code changes needed!

### App Configuration

Edit `config/config.yml` to customize:

- **Mode**: `single` or `full`
- **Paths**: Data files, templates, output directory
- **Visual settings**: Colors, fonts
- **Required columns**: Validation requirements

## Development

### Project Structure

```
ca-soil-health-reports/
├── app.R                 # Main Shiny app
├── global.R             # Global setup and dependencies
├── config/
│   ├── filter-config.csv    # Filter configuration
│   ├── required-fields.csv  # Data validation rules
│   ├── measurement_groups.csv # Measurement group definitions
│   ├── grouping_config.csv  # Grouping variable options
│   └── config.yml           # App configuration
── data/
│   └── template.xlsx        # Excel template
├── quarto/
│   └── template.qmd     # Report template
├── R/
│   ├── logic/           # Core business logic
│   ├── modules/         # Shiny modules
│   └── utils/           # Utility functions
├── outputs/             # Generated reports
└── www/                 # Static assets
```

### Adding New Features

1. **New Filters**: Add rows to `config/filter-config.csv` (no code changes needed!)
2. **New Validation Rules**: Update `config/required-fields.csv`
3. **New Measurement Groups**: Update `config/measurement_groups.csv`
4. **New Grouping Options**: Update `config/grouping_config.csv`
3. **New Modules**: Create in `R/modules/`
4. **New Logic**: Add to `R/logic/`
5. **New Templates**: Add to `quarto/`

### Testing

```r
# Test individual functions
source("R/logic/config.R")
cfg <- load_config("config/config.yml")
cfg <- resolve_paths(cfg)

# Test report generation
source("R/logic/wrapper.R")
generate_soil_health_report(
  data_path = "data/example_data.csv",
  producer_id = "Example Farm",
  year = 2024,
  config = cfg,
  out_dir = "outputs"
)
```

## Troubleshooting

### Common Issues

1. **"Quarto CLI is required"**
   - Install Quarto from https://quarto.org/
   - Ensure `quarto` is in your PATH

2. **"Missing required columns"**
   - Check your CSV file structure
   - Ensure column names match requirements
   - See "Data Requirements" section

3. **"Failed to load local 'soils'"**
   - The app will fall back to GitHub installation
   - For development, clone the [soils package](https://github.com/msimmond/soils) to a local directory

4. **Report generation fails**
   - Check data file format
   - Verify producer/year combinations exist
   - Check console for detailed error messages

### Performance Tips

- Use server mode for large datasets
- Reports are cached - regenerate only when needed
- Close unused browser tabs to free memory

## Contributing

1. Create a feature branch
2. Make changes
3. Test thoroughly
4. Submit pull request

## License

TBD
