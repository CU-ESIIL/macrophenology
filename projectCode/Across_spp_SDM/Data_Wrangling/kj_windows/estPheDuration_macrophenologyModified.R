estimatePheTransByTag <- function(inputDataList = NULL,
                                  inputStatus = NULL,
                                  inputTags = NULL) {

  # Assign working data frame
  if(exists("inputDataList") ==TRUE & is.list(inputDataList)) {

    obs <- inputDataList$phe_statusintensity
    tags <- inputDataList$phe_perindividual

  } else {
    obs <- inputStatus
    tags <- inputTags
  }


  # Print data range for input data set
  print(paste("Observation date range:", min(obs$date), "to", max(obs$date)))

  # Format transition output dataframe
  step_one <- obs %>%

    # extract year from date
    dplyr::mutate(year = substr(.data$date, 1,4)) %>%
    dplyr::group_by(.data$individualID,
                    .data$phenophaseName) %>%

    # remove uninformative phenophaseStatuses
    dplyr::filter(.data$phenophaseStatus != "uncertain") %>%

    # get status, doy previous observation, create transition type for current obs
    dplyr::mutate(statusLag = dplyr::lag(.data$phenophaseStatus),
                  dateIntervalStart = dplyr::lag(date),
                  doyIntervalStart = dplyr::lag(.data$dayOfYear),
                  transitionType = paste0(.data$statusLag, "-", .data$phenophaseStatus))

  # verify input data set contains transitions
  if(any(step_one$transitionType %in% c('no-yes', 'yes-no')) == FALSE) {
    stop("Input dataset does not contain any phenophase transitions")
  }


  ##  Remove first observation with no preceding observation & steps with no transition
  step_two <- step_one %>%
    # dplyr::filter(!is.na(.data$statusLag),
    #               .data$phenophaseStatus != .data$statusLag) %>%

    # Calculate values for each time step
    dplyr::mutate(dateTransition = as.Date(.data$dateIntervalStart + (.data$date - .data$dateIntervalStart) / 2),
                  doyTransition = lubridate::yday(.data$dateTransition),
                  precisionDays = as.numeric(difftime(.data$date, .data$dateIntervalStart, units = "days"))/2,
                  samplingInterval = as.numeric(difftime(.data$date, .data$dateIntervalStart, units = "days"))) %>%

    dplyr::group_by(.data$year,
                    .data$individualID,
                    .data$phenophaseName) %>%

    # Count number of onsets (per year, individual, phenophase)
    dplyr::mutate(nthTransition = cumsum(.data$statusLag == "no" & .data$phenophaseStatus == "yes")) %>%

    # Clean up outputs
    dplyr::select("year",
                  "siteID",
                  "individualID",
                  "phenophaseName",
                  "transitionType",
                  "nthTransition",
                  "date",
                  "dateIntervalStart",
                  "dayOfYear",
                  "doyIntervalStart",
                  "samplingInterval",
                  "dateTransition",
                  "doyTransition",
                  "precisionDays") %>%

    dplyr::arrange(.data$year,
                   .data$phenophaseName,
                   .data$individualID)


  # Rename transition type to onset/offset
  step_two$transitionType <- ifelse(step_two$transitionType == 'no-yes',
                                    'onset',
                                    ifelse(step_two$transitionType == 'yes-no',
                                           'end',
                                           step_two$transitionType))

  # Prep tags df
  out <- tags %>%
    dplyr::select("individualID",
                  "taxonID",
                  "scientificName",
                  "growthForm") %>%

    # Join with Obs
    dplyr::right_join(step_two,
                      by = "individualID") %>%

    # Reorder fields
    dplyr::select("year",
                  "siteID",
                  "individualID",
                  "taxonID",
                  "scientificName",
                  "phenophaseName",
                  "transitionType",
                  "nthTransition",
                  "dateIntervalStart",
                  "doyIntervalStart",
                  "dateIntervalEnd" = "date",
                  "doyIntervalEnd" = "dayOfYear",
                  "dateTransition",
                  "doyTransition",
                  "samplingInterval",
                  "precisionDays")

  return(out)
}
