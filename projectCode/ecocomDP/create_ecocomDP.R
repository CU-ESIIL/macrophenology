# -----------------------------------------------------------------------------
# This function converts source dataset "edi.2129" (archived in the EDI
# Data Repository) to ecocomDP dataset "edi.2130" (also archived in EDI)
# 
# Arguments:
#
# path        Where the ecocomDP tables will be written
# source_id   Identifier of the source dataset
# derived_id  Identifier of the derived dataset
# url         The URL by which the derived tables and metadata can be accessed 
#             by a data repository. This argument is used when automating the 
#             repository publication step, but not used when manually 
#             publishing.
#
# Value:
#
# tables      (.csv) ecocomDP tables
# metadata    (.xml) EML metadata for tables
# 
# Details:
#             This function facilitates automated updates to the derived 
#             "edi.2130" whenever new data are added to the source 
#             "edi.2129". The framework executing this maintenance 
#             routine is hosted on a remote server and jumps into action 
#             whenever an update notification is received for 
#             "edi.2129". The maintenance routine parses the 
#             notification to get the arguments to create_ecocomDP() below.
# -----------------------------------------------------------------------------

# Libraries used by this function

library(xml2)
library(magrittr)
library(readr)
library(lubridate)
library(tidyr)
library(dplyr)
library(taxonomyCleanr) # remotes::install_github("EDIorg/taxonomyCleanr")
library(EDIutils)

# Install the development version of ecocomDP with the traits extension
# remotes::install_github("EDIorg/ecocomDP", ref = "158-traits-extension")
library(ecocomDP)

create_ecocomDP <- function(path,
                            source_id, 
                            derived_id, 
                            url = NULL) {
  
  # Additional Parameters -----------------------------------------------------
  
  edi_environment <- "staging" # will be "production" when published
  entity_name <- "phenology_subset_ecocomDP" # name of table w/o extension
  
  # Read source dataset -------------------------------------------------------
  
  # Read the EML and data
  eml <- EDIutils::read_metadata(source_id, env=edi_environment)
  entities <- EDIutils::read_data_entity_names(source_id, env=edi_environment)
  entity_id <- entities[entities$entityName == "phenology_subset_ecocomDP", ][["entityId"]]
  raw <- EDIutils::read_data_entity(source_id, entity_id, edi_environment)
  data <- readr::read_csv(raw, na = c("NA", "-9999"))
  
  # Iterate over the EML dataTable attributes and extract the units into a 
  # dataframe where the columns are unit_[attributeName], with values being the 
  # units. This will be useful in the ecocomDP table creation step
  dataTable <- xml2::xml_find_all(eml, paste0(".//dataTable[.//objectName='", paste0(entity_name, ".csv"), "']"))
  attrs_w_units <- xml2::xml_find_all(dataTable, ".//attribute[.//standardUnit|.//customUnit]")
  unit_cols <- paste0("unit_", xml2::xml_text(xml2::xml_find_all(attrs_w_units, ".//attributeName")))
  unit_vals <- xml2::xml_text(xml2::xml_find_all(attrs_w_units, ".//standardUnit|.//customUnit"))
  units_df <- as.data.frame(t(unit_vals))
  colnames(units_df) <- unit_cols
  
  # Merge units with data
  data <- cbind(data, units_df[rep(1, nrow(data)), ])
  
  # Ensure date and time data are in POSIXct format
  data$update_datetime <- ymd_hms(data$update_datetime)
  data$observation_date <- ymd(data$observation_date)
  
  # Add columns for the observation table -------------------------------------
  
  names(data)[names(data) == "observation_date"] <- "datetime"
  
  # Create event_id: Assuming that each sampling event is uniquely defined by a 
  # combination of month and year
  data$event_id <- data %>% group_by(
    month = floor_date(data$datetime, "month"),
    year = year(data$datetime)) %>% group_indices()
  data <- data %>% arrange(event_id)
  
  # Assume each record is an occurrence measurement
  data$variable_name <- "occurrence"
  data$value <- 1L
  data$unit <- "number"
  
  # There is an observation_id in the L0 table but it contains NA values, which
  # is not allowed in ecocomDP. Therefore renaming this L0 column to 
  # observation_identifier, and creating an observation_id for ecocomDP below.
  names(data)[names(data) == "observation_id"] <- "observation_identifier"
  data$observation_id <- 1:nrow(data)
  
  # Add columns for the location table ----------------------------------------
  
  # A location_id is required but site_id in the data has NA's. Building a 
  # location_id by assuming unique combinations of latitude and longitude
  
  data$location_id <- data %>% group_by(latitude, longitude) %>% group_indices()
  
  # Add columns for the taxon table -------------------------------------------
  
  names(data)[names(data) == "species_id"] <- "taxon_id"
  names(data)[names(data) == "species"] <- "taxon_name"
  
  
  # While not required, resolving taxonomic entities to an authority system 
  # improves the discoverability and interoperability of the ecocomDP dataset. 
  # We can resolve taxa by sending names through taxonomyCleanr for direct 
  # matches against the Integrated Taxonomic Information System 
  # (ITIS; https://www.itis.gov/).
  
  taxa_resolved <- taxonomyCleanr::resolve_sci_taxa(
    x = unique(data$taxon_name),
    data.sources = 3)
  
  taxa_resolved <- taxa_resolved %>%
    select(taxa, rank, authority, authority_id) %>%
    rename(taxon_rank = rank,
           taxon_name = taxa,
           authority_system = authority,
           authority_taxon_id = authority_id)
  
  data <- left_join(data, taxa_resolved, by = "taxon_name")
  
  # Add columns for the dataset_summary table ---------------------------------
  
  dates <- data$datetime %>% stats::na.omit() %>% sort()
  
  # Use the calc_*() helper functions for consistency
  
  data$package_id <- derived_id
  data$original_package_id <- source_id
  data$length_of_survey_years <- ecocomDP::calc_length_of_survey_years(dates)
  data$number_of_years_sampled <- ecocomDP::calc_number_of_years_sampled(dates)
  data$std_dev_interval_betw_years <- 
    ecocomDP::calc_std_dev_interval_betw_years(dates)
  data$max_num_taxa <- length(unique(data$taxon_name))
  
  west <- min(data$longitude, na.rm = TRUE)
  east <- max(data$longitude, na.rm = TRUE)
  south <- min(data$latitude, na.rm = TRUE)
  north <- max(data$latitude, na.rm = TRUE)
  data$geo_extent_bounding_box_m2 <- 
    ecocomDP::calc_geo_extent_bounding_box_m2(west, east, north, south)
  
  data$dataset_level_bio_organization <- "individual"
  data$observation_finest_level <- "individual"
  # data$number_of_variables ... getting this information from the 
  # variable_mapping table, created below
  
  # Odds and ends -------------------------------------------------------------
  
  # The hard work is done! The data table contains all the source data and 
  # more! We can now use the "create" functions to parse this table into the 
  # ecocomDP tables.
  
  # Parse data into ecocomDP tables -------------------------------------------
  
  # Each ecocomDP table has an associated "create" function. Begin with the 
  # core required tables.
  
  observation <- ecocomDP::create_observation(
    L0_flat = data, 
    observation_id = "observation_id", 
    event_id = "event_id", 
    package_id = "package_id",
    location_id = "location_id", 
    datetime = "datetime", 
    taxon_id = "taxon_id", 
    variable_name = "variable_name",
    value = "value",
    unit = "unit")
  
  location <- ecocomDP::create_location(
    L0_flat = data, 
    location_id = "location_id", 
    location_name = c("site_id"), # Just to create table. Removed below.
    latitude = "latitude", 
    longitude = "longitude")
  location$location_name <- NULL
  
  taxon <- ecocomDP::create_taxon(
    L0_flat = data, 
    taxon_id = "taxon_id", 
    taxon_rank = "taxon_rank", 
    taxon_name = "taxon_name", 
    authority_system = "authority_system", 
    authority_taxon_id = "authority_taxon_id")
  
  # Create the ancillary ecocomDP tables. These are optional, but should be 
  # included if possible.
  
  observation_ancillary <- ecocomDP::create_observation_ancillary(
    L0_flat = data,
    observation_id = "observation_id", 
    variable_name = c(
      "observation_identifier",
      "update_datetime",
      "phenophase_id",
      "phenophase_description",
      "day_of_year",
      "phenophase_status",
      "data_name",
      "year_rect",
      "DomainID",
      "record_id",
      "references",
      "windowStart",
      "windowEnd",
      "windowMid",
      "quantPeak",
      "quantLow",
      "quantHigh",
      "duration",
      "tag_yr"
      ),
    unit = c(
      "unit_phenophase_id",
      "unit_day_of_year",
      "unit_year_rect",
      "unit_windowStart",
      "unit_windowEnd",
      "unit_windowMid",
      "unit_quantPeak",
      "unit_quantLow",
      "unit_quantHigh",
      "unit_duration"
      )
    )
  
  location_ancillary <- ecocomDP::create_location_ancillary(
    L0_flat = data,
    location_id = "location_id",
    variable_name = c("state", "site_id"),
    unit = c("unit_site_id"))
  
  taxon_ancillary <- ecocomDP::create_taxon_ancillary(
    L0_flat = data,
    taxon_id = "taxon_id",
    variable_name = c(
      "individual_id",
      "common_name",
      "kingdom"),
    unit = c("unit_individual_id"))
  
  # Create the variable_mapping table. This is optional but highly recommended
  # as it provides unambiguous definitions to variables and facilitates
  # integration with other ecocomDP datasets.

  variable_mapping <- ecocomDP::create_variable_mapping(
    observation = observation,
    observation_ancillary = observation_ancillary,
    location_ancillary = location_ancillary,
    taxon_ancillary = taxon_ancillary)

  # i <- variable_mapping$variable_name == 'abundance'
  # variable_mapping$mapped_system[i] <- 'Darwin Core'
  # variable_mapping$mapped_id[i] <- 'http://rs.tdwg.org/dwc/terms/individualCount'
  # variable_mapping$mapped_label[i] <- 'individualCount'
  # 
  # i <- variable_mapping$variable_name == 'treatment'
  # variable_mapping$mapped_system[i] <- 'The Ecosystem Ontology'
  # variable_mapping$mapped_id[i] <- 'http://purl.dataone.org/odo/ECSO_00000506'
  # variable_mapping$mapped_label[i] <- 'Manipulative experiment'
  # 
  # i <- variable_mapping$variable_name == 'trap.type'
  # variable_mapping$mapped_system[i] <- 'The Ecosystem Ontology'
  # variable_mapping$mapped_id[i] <- 'http://purl.dataone.org/odo/ECSO_00001591'
  # variable_mapping$mapped_label[i] <- 'type of trap'
  # 
  # i <- variable_mapping$variable_name == 'hl'
  # variable_mapping$mapped_system[i] <- 'Darwin Core'
  # variable_mapping$mapped_id[i] <- 'http://rs.tdwg.org/dwc/terms/measurementType'
  # variable_mapping$mapped_label[i] <- 'measurementType'
  # 
  # i <- variable_mapping$variable_name == 'rel'
  # variable_mapping$mapped_system[i] <- 'Darwin Core'
  # variable_mapping$mapped_id[i] <- 'http://rs.tdwg.org/dwc/terms/measurementType'
  # variable_mapping$mapped_label[i] <- 'measurementType'
  # 
  # i <- variable_mapping$variable_name == 'rll'
  # variable_mapping$mapped_system[i] <- 'Darwin Core'
  # variable_mapping$mapped_id[i] <- 'http://rs.tdwg.org/dwc/terms/measurementType'
  # variable_mapping$mapped_label[i] <- 'measurementType'
  # 
  # i <- variable_mapping$variable_name == 'colony.size'
  # variable_mapping$mapped_system[i] <- 'The Ecosystem Ontology'
  # variable_mapping$mapped_id[i] <- 'http://purl.dataone.org/odo/ECSO_00000311'
  # variable_mapping$mapped_label[i] <- 'Population'
  # 
  # i <- variable_mapping$variable_name == 'feeding.preference'
  # variable_mapping$mapped_system[i] <- 'Darwin Core'
  # variable_mapping$mapped_id[i] <- 'http://rs.tdwg.org/dwc/terms/behavior'
  # variable_mapping$mapped_label[i] <- 'behavior'
  # 
  # i <- variable_mapping$variable_name == 'primary.habitat'
  # variable_mapping$mapped_system[i] <- 'The Ecosystem Ontology'
  # variable_mapping$mapped_id[i] <- 'http://purl.dataone.org/odo/ECSO_00002736'
  # variable_mapping$mapped_label[i] <- 'type of habitat'
  # 
  # i <- variable_mapping$variable_name == 'secondary.habitat'
  # variable_mapping$mapped_system[i] <- 'The Ecosystem Ontology'
  # variable_mapping$mapped_id[i] <- 'http://purl.dataone.org/odo/ECSO_00002736'
  # variable_mapping$mapped_label[i] <- 'type of habitat'
  # 
  # i <- variable_mapping$variable_name == 'seed.disperser'
  # variable_mapping$mapped_system[i] <- 'Darwin Core'
  # variable_mapping$mapped_id[i] <- 'http://rs.tdwg.org/dwc/terms/behavior'
  # variable_mapping$mapped_label[i] <- 'behavior'
  # 
  # i <- variable_mapping$variable_name == 'slavemaker.sp'
  # variable_mapping$mapped_system[i] <- 'Darwin Core'
  # variable_mapping$mapped_id[i] <- 'http://rs.tdwg.org/dwc/terms/behavior'
  # variable_mapping$mapped_label[i] <- 'behavior'
  # 
  # i <- variable_mapping$variable_name == 'behavior'
  # variable_mapping$mapped_system[i] <- 'Darwin Core'
  # variable_mapping$mapped_id[i] <- 'http://rs.tdwg.org/dwc/terms/behavior'
  # variable_mapping$mapped_label[i] <- 'behavior'
  # 
  # i <- variable_mapping$variable_name == 'biogeographic.affinity'
  # variable_mapping$mapped_system[i] <- 'The Ecosystem Ontology'
  # variable_mapping$mapped_id[i] <- 'http://purl.dataone.org/odo/ECSO_00002736'
  # variable_mapping$mapped_label[i] <- 'type of habitat'
  # 
  # i <- variable_mapping$variable_name == 'source'
  # variable_mapping$mapped_system[i] <- 'Darwin Core'
  # variable_mapping$mapped_id[i] <- 'http://purl.org/dc/terms/references'
  # variable_mapping$mapped_label[i] <- 'references'
  
  # Dataset summary table
  data$number_of_variables <- nrow(variable_mapping)
  dataset_summary <- ecocomDP::create_dataset_summary(
    L0_flat = data, 
    package_id = "package_id", 
    original_package_id = "original_package_id", 
    length_of_survey_years = "length_of_survey_years",
    number_of_years_sampled = "number_of_years_sampled", 
    std_dev_interval_betw_years = "std_dev_interval_betw_years", 
    max_num_taxa = "max_num_taxa", 
    geo_extent_bounding_box_m2 = "geo_extent_bounding_box_m2",
    dataset_level_bio_organization = "dataset_level_bio_organization",
    observation_finest_level = "observation_finest_level",
    number_of_variables = "number_of_variables"
    )
  
  # Write tables to file
  
  ecocomDP::write_tables(
    path = path, 
    observation = observation, 
    location = location,
    taxon = taxon,
    dataset_summary = dataset_summary, 
    observation_ancillary = observation_ancillary,
    location_ancillary = location_ancillary, 
    taxon_ancillary = taxon_ancillary, 
    variable_mapping = variable_mapping)
  
  # Validate tables -----------------------------------------------------------
  
  # Validation checks ensure the derived set of tables comply with the ecocomDP
  # model. Any issues at this point 
  # should be addressed in the lines of code above, the tables rewritten, and 
  # another round of validation, to be certain the fix worked.
  
  issues <- ecocomDP::validate_data(path = path)
  
  # Create metadata -----------------------------------------------------------
  
  # Before publishing the derived ecocomDP dataset, we need to describe it. The 
  # create_eml() function does this all for us. It knows the structure of the 
  # ecocomDP model and applies standardized table descriptions and mixes in 
  # important elements of the source dataset metadata for purposes of 
  # communication and provenance tracking.
  
  # Convert "dataset level keywords" listed in the source to "dataset level 
  # annotations" in the derived. The predicate "is about" is used, which 
  # results in an annotation that reads "This dataset is about 'species 
  # abundance'", "This dataset is about an ecological 'Community'", etc. All 
  # source datasets involving a human induced manipulative experiment, not a 
  # natural disturbance/experiment, should include the "Manipulative 
  # experiment" annotation below to enable searching on this term.
  
  dataset_annotations <- c(
    `phenology` =
      "http://vocabs.lter-europe.net/EnvThes/21647",
    Community =
      "http://purl.dataone.org/odo/ECSO_00000310")
  
  # Add contact information for the author of this script and dataset
  
  additional_contact <- data.frame(
    givenName = 'Colin',
    surName = 'Smith',
    organizationName = 'Environmental Data Initiative',
    electronicMailAddress = 'ecocomdp@gmail.com',
    stringsAsFactors = FALSE)
  
  # Create EML metadata
  
  # Point the create_eml function at the right EDI environment
  assign("config.environment", edi_environment, envir = .GlobalEnv)
  
  eml <- ecocomDP::create_eml(
    path = path,
    source_id = source_id,
    derived_id = derived_id,
    is_about = dataset_annotations,
    script = "create_ecocomDP.R",
    script_description = 
      "A function for converting knb-lter-hrf.118 to ecocomDP",
    contact = additional_contact,
    user_id = 'ecocomdp',
    user_domain = 'EDI',
    basis_of_record = "HumanObservation")
  
}


create_ecocomDP(
  path = ".",
  source_id = "edi.2129.1",
  derived_id = "edi.2130.1"
)




