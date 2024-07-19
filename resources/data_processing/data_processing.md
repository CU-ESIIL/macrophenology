# Data Processing Documentation

## Overview
Brief description of the data processing objectives and scope. Reminder to adhere to data ownership and usage guidelines.

## ACCESS proposal

### Abstract
The Macrophenology working group, supported by the Environmental Science Innovation and Inclusion Lab (ESIIL) at the Universit of Colorado Boulder, intends to explore the interaction between phenological shifts and species range shifts in response to climate change, using multiple sources of data (remote sensing, herbarium, and field observations). This work will include expanding the "ecological community data design pattern" (ecocomDP) data model and its associated tools to interact with plant trait and phenological databases to create ecocomDP+ derived datasets. Data integration and co-production will be most successful if we can bring together subject matter experts (SMEs) on key aspects of this synthesis endeavor, including scientists, Tribal community members and scholars, data providers, environmental data scientists, and cyberinfrastructure SMEs. 

Derived datasets will use the extended ecocomDP+ standard and will be stored in a shared Jetstream2 Volume using columnar file formats (e.g., parquet) to facilitate the use of cloud-native tools. Workflow development will be collaborative and reproducible using open-source software and tools (e.g., R, Python, GitHub). Initial development will use an ACCESS Explore allocation for the Jetstream2 Cloud using 200k SUs (Jetstream2 vCPU hours). Compute resources will be used for (1) compiling the ecocomDP+ datasets to feed into the analysis, (2) calculating shifts in species ranges and phenologies, and (3) conducting analyses linking range shifts and phenology to other species traits and climate change. Remotely sensed datasets, especially time-series data, are characterized by their voluminous nature, often referred to as "big data." Processing and analyzing these extensive datasets requires substantial storage capacities and high-performance computing nodes. The workflow will initially be developed for a subset of target taxa and then scaled up.

### PIs (need biosketches)
 - Eric Sokol
 - Sydne Record

### Associated grants
 - ESIIL
 - NEON
 - others?

 
## Data Sources
List and describe data sources used, including links to cloud-optimized sources. Highlight permissions and compliance with data ownership guidelines.

## CyVerse Discovery Environment
Instructions for setting up and using the CyVerse Discovery Environment for data processing. Tips for cloud-based data access and processing.

## Data Processing Steps

### Using GDAL VSI
Guidance on using GDAL VSI (Virtual System Interface) for data access and processing. Example commands or scripts:
```bash
gdal_translate /vsicurl/http://example.com/data.tif output.tif
```

## Cloud-Optimized Data
Advantages of using cloud-optimized data formats and processing data without downloading. Instructions for such processes.

## Data Storage

Information on storing processed data, with guidelines for choosing between the repository and CyVerse Data Store.

## Best Practices

Recommendations for efficient and responsible data processing in the cloud. Tips to ensure data integrity and reproducibility.

## Challenges and Troubleshooting

Common challenges in data processing and potential solutions. Resources for troubleshooting in the CyVerse Discovery Environment.

## Conclusions

Summary of the data processing phase and its outcomes. Reflect on the methods used.

## References

Citations of tools, data sources, and other references used in the data processing phase.
