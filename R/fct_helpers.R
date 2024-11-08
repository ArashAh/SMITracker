
#' Dependencies
#' @name dependencies
#' @title Overall dependencies
#' @description
#'  All the dependencies needed for the functions to run are listed below and
#'  are imported at once in this block:
#' magrittr, dplyr, tidyr, viridis, broom, data.table, DT, shiny, shinyWidgets,
#' shinyFiles, shinyscreenshot, stringr, scales, zoo, jsonlite, caret,
#' shinythemes, ggplot2, Rcpp, RcppRoll.
#'
#' @importFrom magrittr %<>% set_colnames
#' @import dplyr
#' @import tidyr
#' @import broom
#' @import shiny
#' @import shinyWidgets
#' @import shinyFiles
#' @importFrom shinyscreenshot screenshotButton
#' @import stringr
#' @import scales
#' @import zoo
#' @importFrom jsonlite fromJSON
#' @import caret
#' @import shinythemes
#' @import ggplot2
#' @import Rcpp
#' @import RcppRoll
NULL


##### Read json files ######

#' @title Read Json files, collect tabular data
#'
#' @description A function that reads in the data from json files within a
#'  directory and collects all the existing data inside a tabular data frame.
#'
#' @param dataPath A directory containing the json files
#' @param nEnd Number of characters to be included in the informative files name
#'
#' @return A structured tabular data set by combining all the existing data

readJsonFiles <- function(dataPath, nEnd){

  list.of.files <- list.files(path = dataPath)

  nEnd1 <- nchar(list.of.files[1]) - nEnd

  collect.data <- NULL
  collected.data <- NULL

  for (i in list.of.files) {
    json.file <- paste(dataPath, i, sep = "")

    raw.data <- fromJSON(json.file)

    raw.data %<>%
      mutate(data.set.name = substr(i, start = 1, stop = nchar(i) - nEnd1)) %>%
      select(data.set.name, frame,`x [nm]`, `y [nm]`,`intensity [photon]`,
             `bkgstd [photon]`, `uncertainty [nm]`,`sigma [nm]`) %>%
      set_colnames(c("data.set.name","frame.number", "X", "Y", "intensity",
                     "background", "uncertainty", "sigma"))
    collect.data <- bind_rows(collect.data, raw.data)
  }

  return(collect.data)
}


##### Transform data #####

#' @title Transform data using metadata
#'
#' @description A function that ingrates the result of the signal localization
#' analysis with information on experimental conditions provided as metadata
#' in the files name.
#'
#' @param InputData A data frame which is an output of the readJsonFiles()
#' function; this is a tabular data frame containing information on all the
#' localized signals.
#'
#' @param nameList A vector of characters representing the name of the
#' variables provided in the files name.
#'
#' @param seprator The symbol that is used to separate different parts of the
#' file names.
#'
#' @param numerName A vector of charterers representing variables that contain
#' numerical values.
#'
#' @return A data frame containing all data and metadata


TransformData <- function(InputData, nameList, separator, numerName){

  Output <- InputData %>%
    arrange(data.set.name, frame.number) %>%
    mutate(n = data.set.name) %>%
    separate(n, nameList, sep = as.character(separator), fill = "right")

  for(i in numerName) {

    Output[,i] <- as.numeric(str_extract(Output[,i], "[0-9,.]+"))

  }

  Output <-  Output[,!grepl("remove",names(Output))]

  return(Output)
}


##### Make it Matrix #####

#' @title Create a matrix representation of the data frame
#'
#' @description A function that creates a matrix out of the provided data frame,
#' the row index is frame number and columns are number of simultaneously
#' localized signal and values are coordinates of the detected signal.
#'
#' @param Rawdata Output of tranformData() function that is filtered for
#' each data set name.
#'
#' @return A matrix representation of the data frame where each row is indexed
#' as unique frame number and detected signals are spread in columns with x-y
#' coordinates.

MakeMatrix <- function(RawData){

  input.data <- RawData

  row = max(RawData$frame.number) # max number of frame

  col <- input.data %>%
    group_by(frame.number) %>%
    summarise(nSignals = n()) %>%
    select(nSignals) %>%
    max()                       # max number of detected signal per frame

  DataMatx <- matrix(0,row,col)
  DataMaty <- matrix(0,row,col)
  DataMatf <- matrix(0,row,1)
  k = 0
  p = 0

  for (i in 1:row)
  {
    DataMatf[i,1] <- i

    l <- length(input.data$frame.number[input.data$frame.number == i])
    if (l == 0)
    {
      DataMatx[i,] <- 0
      DataMaty[i,] <- 0
      k <- k + 1
    }
    else
    {
      for (j in 1:l)
      {
        DataMatx[i,j] <- input.data$X[i - k + p + j - 1]
        DataMaty[i,j] <- input.data$Y[i - k + p + j - 1]

      }
      p <- p + l - 1
    }
  }
  DataMat <- data.frame(DataMatx, DataMaty)
  for (i in 1:col)
  {
    names(DataMat)[i] <- paste("x", i, sep = "")
    names(DataMat)[col + i] <- paste("y", i, sep = "")
  }
  return(DataMat)
}




##### Find trajectories #####

#' @title Finds trajectories of moving proteins
#'
#' @description A function that finds/traces the individual binding and scanning
#' events on the substrate. It rearranges the matrix representation of the
#' data so that continuous trajectories are set in columns of the matrix.
#'
#' @param FilteredDataSet The data in the matrix representation where where unique
#' frames numbers are set as frame numbers and each signal on individual
#' columns.
#'
#' @param dxmax The maximum displacement of the proteins along the substrate.
#'
#' @param dymax The maximum displacement of the protein across the substrate
#' before it is considered dissociated.
#'
#' @return A matrix representation of the data where detected trajectories are
#' arranged in separate column and concatenated along the frame numbers.

FindTrajectory <- function(FilteredDataSet, dxmax, dymax)
{
  dxmin = 0.0001
  info <- 0
  row <- nrow(FilteredDataSet)
  col <- ncol(FilteredDataSet)/2
  # detection loop
  for (i in 1:(row - 1))
  {
    for (j in 1:(col))
    {
      if (FilteredDataSet[i,j] != 0)
      {
        if (j != col)
        {
          if (sqrt((FilteredDataSet[i,j])^2 +
                   (FilteredDataSet[i,(j + col)])^2) -
              sqrt((FilteredDataSet[i,(j + 1)])^2 +
                   (FilteredDataSet[i,(j + col + 1)])^2) < 200 &
              abs(FilteredDataSet[i,j] - FilteredDataSet[i,(j + 1)]) < 200)
            # if two signal has been detected within one PSF in one frame,
            #consider the mean of the two
          {
            FilteredDataSet[i,j] <- (FilteredDataSet[i,j] +
                                       FilteredDataSet[i,(j + 1)]) / 2
            FilteredDataSet[i,j + col] <- (FilteredDataSet[i,j + col] +
                                           FilteredDataSet[i,(j + col + 1)])/2
            FilteredDataSet[i,j + 1] <- 0
            FilteredDataSet[i,j + col + 1] <- 0
          }
        }
        deltax <- abs(FilteredDataSet[i,j] - FilteredDataSet[i + 1, 1:col])
        deltay <- abs(FilteredDataSet[i,(j + col)] -
                       FilteredDataSet[i + 1, (col + 1):(2 * col)])
        min <- which.min(deltax)
        # guarantees that we follow the closest signal in the next frame

        if (deltax[min] < dxmax & deltax[min] > dxmin & deltay[min] < dymax)
        {
          if (i != 1)
          {
            if (FilteredDataSet[i - 1,j] != 0)
            {# this is for separation of two consecutive trajectories
              if (abs(FilteredDataSet[i,j] - FilteredDataSet[i - 1,j]) > dxmax |
                  abs(FilteredDataSet[i,j] - FilteredDataSet[i - 1,j]) < dxmin |
                  abs(FilteredDataSet[i,(j + col)] -
                      FilteredDataSet[i - 1,(j + col)]) > dymax)
              { # basically, if this is the first point
                #then make the previous point zero, make sure it is
                #not following another trajectory


                FilteredDataSet[i - 1, j] <- 0
                FilteredDataSet[i - 1,(j + col)] <- 0
              }
            }
          }
          if (abs(FilteredDataSet[i + 1,min] - FilteredDataSet[i,j]) ==
              min(abs(FilteredDataSet[i, 1:col] - FilteredDataSet[i + 1, min])))
          { # in case there are two signal within the dxmax and dymax,
            #this help to choose the closest signal to the signal in
            #the last frame
            ax <- FilteredDataSet[i + 1, j]
            FilteredDataSet[i + 1,j] <- FilteredDataSet[i + 1, min]
            FilteredDataSet[i + 1, min] <- ax
            ay <- FilteredDataSet[i + 1, (j + col)]
            FilteredDataSet[i + 1,(j + col)] <- FilteredDataSet[i + 1, (min + col)]
            FilteredDataSet[i + 1, (min + col)] <- ay
          }
          else
          {
            # print("it didn't move")
            # print(i)
            # print(j)
          }
        }
        else if ( i != 1)
        {
          if (FilteredDataSet[i - 1, j] != 0)
          {
            if (abs(FilteredDataSet[i,j] - FilteredDataSet[i - 1, j]) < dxmax &
                abs(FilteredDataSet[i,j] - FilteredDataSet[i - 1,j]) > dxmin &
                abs(FilteredDataSet[i,(j + col)] -
                    FilteredDataSet[i - 1,(j + col)]) < dymax)
            { # this is how the last point of the trajectory is checked and
              #preserved
              # print("last point or single point detected")
              # print(i)
              # print(j)
            }
            else
            {
              FilteredDataSet[i,j] <- 0
              FilteredDataSet[i,(j + col)] <- 0
            }
          }
          else
          {
            FilteredDataSet[i,j] <- 0
            FilteredDataSet[i,(j + col)] <- 0
          }
        }
        else
        {
          FilteredDataSet[i,j] <- 0
          FilteredDataSet[i,(j + col)] <- 0
        }
      }
      else
      {
        FilteredDataSet[i,j] <- 0
        FilteredDataSet[i,(j + col)] <- 0
      }
    }
  }
  return(FilteredDataSet)
}


##### Replace zero with NA #####

#' MakeZeroNA
#'
#' @title Replace zero values with NA
#'
#' @description A function that replaces zero values in the data set with NA.
#'
#' @param InputDataSet A matrix representation of the data set.
#'
#' @return A matrix representation of the data set where zero values are replaced
#' by Na.

MakeZeroNA <- function(InputDataSet)
{
  InputDataSet[InputDataSet == 0] <- NA
  return(InputDataSet)
}



#####  Make long form #####

#' MakeLongForm
#'
#' @title Rearrange from the matrix form to tidy data
#'
#' @description A function that transforms the data from matrix format to
#' four-column tidy data.
#'
#' @param InputDataSet A matrix representation of the data set.
#'
#' @return A four-column tidy representation of data with frame number and
#' x-y coordinates of trajectories along with their registered track.

MakeLongForm <- function(InputMatrix) {
  LongForm <- tibble(frame.number = rep(1:(nrow(InputMatrix)), (ncol(InputMatrix)/2)),
                         X = InputMatrix[, 1:(ncol(InputMatrix)/2)] %>%
                           unlist() %>%
                           as.vector(),
                         Y = InputMatrix[, (ncol(InputMatrix) / 2 + 1) :
                                           ncol(InputMatrix)] %>%
                           unlist() %>%
                           as.vector(),
                         track.register = rep(LETTERS[1:(ncol(InputMatrix)/2)],
                                              each = nrow(InputMatrix)))
  return(LongForm)

}




##### Correct blinking #####

#' CorrectBlinking
#'
#' @title Correct for discontinues trajectories
#'
#' @description A function that correct for fluorescent dye blinking effect by
#' connecting segments of trajectories that are apart only within the defined
#' threshold.
#'
#' @param InputData Long format of the data set including frame numbers and
#' x-y coordinates.
#'
#' @param x.distance The maximum distance along the substrate that is allowed
#' for connecting points in trajectories.
#'
#' @param y.distance The maximum distance across the substrate that is allowed
#' for connecting points in trajectories.
#'
#' @return A long format the data with frame number and x-y coordinates of
#' trajectories where blinking is corrected.

CorrectBlinking <- function(InputData, x.distance, y.distance){
  output <- InputData %>%
    mutate(pick = ifelse(X == 0 & lead(X, n = 1) != 0 &
                           lag(X, n = 1) != 0, 1, 0)) %>%
    mutate(setx = ifelse(pick == 1 & abs(lead(X, n = 1) -
                                           lag(X, n = 1)) < x.distance &
                           abs(lead(Y, n = 1) - lag(Y, n = 1)) < y.distance,
                         (lead(X, n = 1) + lag(X, n = 1))/2, 0),
           sety = ifelse(pick == 1 & abs(lead(X, n = 1) -
                                          lag(X, n = 1)) < x.distance &
                          abs(lead(Y, n = 1) - lag(Y, n = 1)) < y.distance,
                        (lead(Y, n = 1) + lag(Y, n = 1))/2, 0)) %>%
    mutate(X = ifelse(setx == 0 , X, setx),
           Y = ifelse(sety == 0 , Y, sety),
           blinked = ifelse(setx != 0 & sety != 0, "Yes", "No")) %>%
    select(-c(pick, setx, sety))
}
##### Arrange #####

#' ArrangeData
#'
#' @title Arrange the detected trajectories
#'
#' @description A function that arranges all the detected trajectories according
#' to the frame number and gives trajectories unique IDs and  joins all the
#' attributes and metadata of the detected signal with the data at this point.
#'
#' @param RawDataSet The output of the spatially filtered data set, this will
#' be used to join the additional attributes of each signal to the detected
#' trajectories.
#'
#' @param DetectedTrajectories A long format of the output of the
#' FindTrajectory() function.
#'
#' @return A tidy data frame of all detected trajectories along with all the
#' signal localization attributes and metadata in separate columns.

ArrangeData <- function(RawDataSet, DetectedTrajectories){

  output <- DetectedTrajectories %>%
    mutate(see = ifelse(is.na(X), 1,0), see2 = cumsum(see)) %>%
    mutate(trajectory.id = ifelse(see == 1, 0,see2)) %>%
    filter(!is.na(X)) %>%
    select(-c(see, see2))

  new.id <- output %>%
    group_by(trajectory.id) %>%
    group_indices()

  output$trajectory.id <- new.id

  output <- left_join(output, RawDataSet, by = join_by(frame.number, X, Y))


  output$data.set.name[is.na(output$data.set.name)] <- output$data.set.name[1]
  output$sub.data.set.name[is.na(output$sub.data.set.name)] <- output$sub.data.set.name[1]
  output$protein[is.na(output$protein)] <- output$protein[1]
  output$frame.interval[is.na(output$frame.interval)] <- output$frame.interval[1]
  output$analysis.id[is.na(output$analysis.id)] <- output$analysis.id[1]
  if ("expr.cond1" %in% colnames(output)) {
    output$expr.cond1[is.na(output$expr.cond1)] <- output$expr.cond1[1]
  }
  if ("expr.cond2" %in% colnames(output)) {
    output$expr.cond2[is.na(output$expr.cond2)] <- output$expr.cond2[1]
  }
  if ("expr.cond3" %in% colnames(output)) {
    output$expr.cond3[is.na(output$expr.cond3)] <- output$expr.cond3[1]
  }
  if ("expr.cond4" %in% colnames(output)) {
    output$expr.cond4[is.na(output$expr.cond4)] <- output$expr.cond4[1]
  }
  if ("expr.cond5" %in% colnames(output)) {
    output$expr.cond5[is.na(output$expr.cond5)] <- output$expr.cond5[1]
  }

  output$signal.type[is.na(output$intensity)] <- output$signal.type[1]
  output$intensity[is.na(output$intensity)] <- 0

  return(output)
}



##### Detect and exclude noise #####

#' @title Detect and arrange trajectories
#'
#' @description A parent function for finding, arranging and collecting
#' trajectories across all existing data sets. The data #' for each data set
#' is run through a series other functions sequentially and the end result is
#' collected in tidy data frame.
#'
#'
#' @param filteredDataSet The output of the spatially filtered data.
#'
#' @param dxmax The maximum displacement of the proteins along the substrate,
#' this parameter is passed to the FindTrajectory() function.
#'
#' @param dymax The maximum displacement of the protein across the substrate
#' before it is considered dissociated. this parameter is passed to the
#' FindTrajectory() function.
#'
#' @return A tidy data frame of all detected trajectories along with all the
#' signal localization attributes and metadata in separate columns across all
#' data sets.

detectTrajectories <- function(filteredDataSet, dxMax , dyMax ){

  collect.arranged.data  <- NULL
  j = 0
  for (i in filteredDataSet %>%
      "$"(sub.data.set.name) %>%
      unique()) {
    j = j + 1
    #  Separate the data sets into raw ans filtered
    print(paste("Processing: ",
                round(100 * j / (4 * length(filteredDataSet %>%
                                                          "$"(data.set.name) %>%
                                                          unique()))) ,
                " %",
                sep = ""))

    # report <- paste("Processing: ", j, " / ", 4* length(filteredDataSet %>%
    #                                                      "$"(data.set.name) %>%
    #                                                      unique()) , sep="")

    detected.trajectories <- filteredDataSet %>%
      filter(sub.data.set.name == i) %>%
      MakeMatrix() %>%
      FindTrajectory(dxmax = dxMax, dymax = dyMax) %>%
      MakeLongForm() %>%
      CorrectBlinking(x.distance = 2*dxMax, y.distance = dyMax) %>%
      MakeZeroNA()

    arranged.data <- ArrangeData(RawDataSet = filteredDataSet %>%
                                   filter(sub.data.set.name == i),
                                 DetectedTrajectories = detected.trajectories)

    collect.arranged.data <- bind_rows(collect.arranged.data, arranged.data)


  }
  return(collect.arranged.data)
}




##### Detect and exclude noise from stuck proteins  #####

#' @title Detect and arrange surface-associated signals
#'
#' @description A parent function for finding, arranging and collecting
#' the surface-associated signals across all existing data sets. The data
#' for each data set is run through a series other functions sequentially and
#' the end result is collected in tidy data frame.
#'
#' @param filteredDataSet The output of the spatially filtered data.
#'
#' @param dxmax The maximum displacement of the signal along the x-direction,
#' before it is considered dissociated.this parameter is passed to the
#' FindTrajectory() function.
#'
#' @param dymax The maximum displacement of the signal along the y-direction,
#' before it is considered dissociated. this parameter is passed to the
#' FindTrajectory() function.
#'
#' @return A tidy data frame of all detected surface-associated signal along
#' with all the signal localization attributes and metadata in separate columns
#' across all data sets.

detectStuck <- function(filteredDataSet, dxMax , dyMax ){

  collect.arranged.data <- NULL

  j = length(filteredDataSet %>%
              "$"(sub.data.set.name) %>%
              unique())/3

  for (i in filteredDataSet %>%
      "$"(sub.data.set.name) %>%
      unique()) {

    j = j + 1

    print(paste("Processing: ",
                round(100*j / (length(filteredDataSet %>%
                                       "$"(sub.data.set.name) %>%
                                       unique())*4/3)) ,
                " %", sep = ""))

    detected.trajectories <- filteredDataSet %>%
      filter(sub.data.set.name == i) %>%
      MakeMatrix() %>%
      FindTrajectory(dxmax = dxMax, dymax = dyMax) %>%
      MakeLongForm() %>%
      CorrectBlinking(x.distance = dxMax, y.distance = dyMax) %>%
      MakeZeroNA()

    arranged.data <- ArrangeData(RawDataSet = filteredDataSet %>%
                                   filter(sub.data.set.name == i),
                                 DetectedTrajectories = detected.trajectories)

    collect.arranged.data <- bind_rows(collect.arranged.data, arranged.data)


  }
  return(collect.arranged.data)
}

###### Add duration filter #####

#' @title Add trajectory duration filter
#'
#' @description A function that calculates the duration of trajectories and
#' labels them based on the given parameter.
#'
#' @param data.set The output of detectTrajectories() function.
#'
#' @param minFrameDuration Minimum number of recorded frames for the signal to
#' be considered trajectory.
#'
#' @return The input data set with a added column where a binary label defines
#' whether a trajectory passes the duration filer or not.


AddDurationFilter <- function(data.set, minFrameDuration){

  data.set %<>%
    group_by(sub.data.set.name, trajectory.id) %>%
    mutate(duration = max(frame.number) - min(frame.number)) %>%
    mutate(duration.filter = ifelse(duration >= minFrameDuration,
                                    TRUE, FALSE)) %>%
    select(-duration) %>%
    ungroup()

  return(data.set)

}

##### Add visual inspection filter #####

#' @title Add visual inspection indicator
#'
#' @description A function that picks longest ranging trajectories to send for
#' visusal inspection
#'
#' @param data.set The output of detectTrajectories() function.
#'
#' @param TrajsToInspect Number of trajectories to inspect for each data set
#'
#' @return The input data set with a added column where a binary label defines
#' whether a trajectory is selected for visual inspection or not.


AddVisualInspection <- function(data.set, TrajsToInspect){

  visual.inpsection.filter <- data.set %>%
    filter(signal.type != "stuck", duration.filter) %>%
    group_by(sub.data.set.name, trajectory.id) %>%
    summarise(x.range = max(X) - min(X)) %>%
    group_by(sub.data.set.name) %>%
    arrange(desc(x.range), .by_group = TRUE) %>%
    top_n(n = TrajsToInspect, wt = x.range) %>%
    mutate(visual.inspection = TRUE) %>%
    select(-x.range) %>% ungroup()

  data.set <- left_join(data.set, visual.inpsection.filter,
                        by = join_by(trajectory.id, sub.data.set.name)) %>%
    mutate(visual.inspection = ifelse(is.na(visual.inspection),
                                      FALSE, visual.inspection))

}






##### Make trajectory unique id #####

#' @title Make trajectory unique ID
#'
#' @description A function that labels trajectories with an unique ID and
#' arranges the rows based on data set name, trajectory unique ID and
#' frame number.
#'
#' @param data.set The output of detectTrajectories() function.
#'
#' @return The input data set with a added column where each trajectories is
#' given a unique ID.

MakeTrajectoryUniqeID <- function(data.set)
{
  data.set %<>%
    arrange(data.set.name, sub.data.set.name,frame.number) %>%
    unite(trajectory.unique.id, sub.data.set.name,
          trajectory.id, remove = FALSE) %>%
    mutate(trajectory.unique.id = as.factor(trajectory.unique.id)) %>%
    mutate(trajectory.unique.id = as.numeric(trajectory.unique.id)) %>%
    arrange(data.set.name, sub.data.set.name,trajectory.unique.id, frame.number)
}

##### Make trajectory address #####

#' @title Make trajectory address
#'
#' @description A function that extracts the temporal address of trajectories
#' that are selected for visual inspection.
#'
#' @param Input The output of detectTrajectories() function on which
#' AddDurationFilter() and AddVisualInspection() functions are applied.
#'
#' @return A three-column data frame that contains file name, starting frame
#' number and ending frame number for each trajectory.

MakeTrajectoryAddress <- function(Input){
  output <- Input %>%
    filter(visual.inspection , duration.filter ) %>%
    group_by(trajectory.unique.id) %>%
    summarise(file.name = data.set.name %>% unique(),
              start.frame = min(frame.number),
              end.frame = max(frame.number))
}



##### Rotation matrix #####

#' @title Apply rotation matrix
#'
#' @description A function that applies a rotation matrix to each pair of
#' x and y coordinates.
#'
#' @param xPos The X coordinate of the detected signal.
#'
#' @param yPos The Y coordinate of the detected signal.
#'
#' @param estimate The slope by which the rotation matrix is applied.
#'
#' @return a vector containing the X and Y coordinates of the signal after
#' rotation.


RotatePoints <- function(xPos, yPos, estimate) {
  coordinates <- matrix(c(xPos, yPos), ncol = 1)

  mat <- matrix(c(cos(atan(estimate)), sin(atan(estimate)),
                  -sin(atan(estimate)), cos(atan(estimate))), ncol = 2)
  (mat %*% coordinates) %>% as.vector()
}

##### Add rotated coordinate #####

#' @title Extract the rotation slope and apply rotation matrix
#'
#' @description A parent function for extracting the rotation slope and applying
#' the rotation matrix for each data set.
#'
#' @param data.set The output of detectTrajectories() function on which
#' AddDurationFilter() and AddVisualInspection() functions are applied.
#'
#' @return The input data set where rotated coordinates are added for each data
#' point.

AddRotatedCoord <- function(data.set){
  fit.on.trajectory <- data.set %>%
    filter(duration.filter, visual.inspection, signal.type != "stuck",
           !is.na(sub.data.set.name)) %>%
    group_by(sub.data.set.name) %>%
    do(tidy(lm(Y ~ X, data = .))) %>%
    mutate(fit.slope = estimate[term == "X"],
           fit.intercept = estimate[term == "(Intercept)"]) %>%
    filter(term == "X") %>%
    select(sub.data.set.name, fit.slope, fit.intercept) %>%
    ungroup()


  data.set %<>%
    filter(!is.na(sub.data.set.name)) %>%
    left_join(.,fit.on.trajectory, by = join_by(sub.data.set.name)) %>%
    rowwise() %>%
    mutate(r.X = RotatePoints(X, Y, -fit.slope)[1],
           r.Y = RotatePoints(X, Y, -fit.slope)[2]) %>%
    ungroup()


  anchor.point <- data.set %>%
    filter(duration.filter, visual.inspection, signal.type != "stuck" ,
           !is.na(sub.data.set.name)) %>%
    group_by(sub.data.set.name) %>%
    summarise(x.translate = mean(X) - mean(r.X),
              y.translate = mean(Y) - mean(r.Y))

  data.set %<>% filter(!is.na(sub.data.set.name)) %>%
    left_join(.,anchor.point, by = join_by(sub.data.set.name)) %>%
    mutate(r.X = r.X + x.translate, r.Y = r.Y + y.translate)

  return(data.set)
}

##### Add stuck filter #####

#' @title Add surface-associated label
#'
#' @description A function that based on data from surface-associated signals
#' creates a filtering system to label rest of the data.
#'
#' @param data.set The output of detectTrajectories() function on which
#' AddDurationFilter() and AddVisualInspection() functions are applied.
#'
#' @param guage An integer that defines how tightly the surface-associated
#' filter is applied.
#'
#' @return The input data set with another column added that defines whether
#' a signal is surface associated or not.


AddStuckFilter <- function(data.set, gauge){
  stuck.filters <- data.set %>%
    filter(duration.filter, signal.type == "stuck") %>%
    group_by(data.set.name, trajectory.unique.id) %>%
    summarise(sd.x = sd(X), sd.y = sd(Y),
              range.x = max(X) - min(X), range.y = max(Y) - min(Y)) %>%
    ungroup() %>%
    group_by(data.set.name) %>%
    summarise(sd.x.stuck.filter = mean(sd.x) + if_else(is.na(gauge*sd(sd.x)),
                                                       (gauge/10)*mean(sd.x),
                                                       gauge*sd(sd.x)),
              sd.y.stuck.filter = mean(sd.y) + if_else(is.na(gauge*sd(sd.y)),
                                                       (gauge/10)*mean(sd.y),
                                                       gauge*sd(sd.y)),
              range.x.stuck.filter = mean(range.x) +
                if_else(is.na(gauge*sd(range.x)),
                        (gauge/10)*mean(range.x),
                        gauge*sd(range.x)),
              range.y.stuck.filter = mean(range.y) +
                if_else(is.na(gauge*sd(range.y)),
                        (gauge/10)*mean(range.y),
                        gauge*sd(range.y))) %>%
    ungroup()


  data.set %<>%
    group_by(trajectory.unique.id) %>%
    mutate(sd.x = sd(r.X), sd.y = sd(r.Y),
           range.x = max(r.X) - min(r.X),
           range.y = max(r.Y) - min(r.Y)) %>%
    left_join(., stuck.filters, by = join_by(data.set.name)) %>%
    mutate(stuck.filter = ifelse(sd.x > sd.x.stuck.filter |
                                   sd.y > sd.y.stuck.filter,
                                 TRUE, FALSE)) %>%
    mutate(stuck.filter = ifelse(range.x > range.x.stuck.filter |
                                   range.y > range.y.stuck.filter,
                                 stuck.filter, FALSE)) %>%
    mutate(stuck.filter = ifelse(is.na(sd.x.stuck.filter),
                                  TRUE, stuck.filter)) %>%
    ungroup()

  return(data.set)
}


##### Add on DNA filter #####


#' @title Add substrate associated label
#'
#' @description A function that based on data from visually-inspected
#' trajectories creates a filtering system to label rest of the data.
#'
#' @param data.set The output of detectTrajectories() function on which
#' AddDurationFilter() and AddVisualInspection() functions are applied.
#'
#' @param guage An integer that defines how tightly the substrate-associated
#' filter is applied.
#'
#' @return The input data set with another column added that defines whether
#' a signal is substrate-associated or not.


AddFlankFilter <- function(data.set, gauge){

  on.dna.filters <- data.set %>%
    filter(duration.filter, visual.inspection, signal.type != "stuck") %>%
    group_by(sub.data.set.name, signal.type, trajectory.unique.id) %>%
    summarise(range.y = max(r.Y) - min(r.Y),
              sd.y = sd(r.Y), mid.y = mean(r.Y)) %>%
    ungroup() %>%
    group_by(sub.data.set.name) %>%
    summarise(range.y.flank.filter = mean(range.y) +
                if_else(is.na(gauge*sd(range.y)),
                        (gauge/10)*mean(range.y),
                        gauge*sd(range.y)),
              sd.y.flank.filter = mean(sd.y) +
                if_else(is.na(gauge*sd(sd.y)),
                        (gauge/10)*mean(sd.y),
                        gauge*sd(sd.y)),
              y.offset.filter = mean(mid.y)) %>%
    ungroup()

  data.set %<>%
    group_by(trajectory.unique.id) %>%
    mutate(range.y = max(r.Y) - min(r.Y), offset = mean(r.Y)) %>%
    left_join(., on.dna.filters, by = join_by(sub.data.set.name)) %>%
    mutate(on.dna.filter = ifelse(sd.y < sd.y.flank.filter &
                                    range.y < range.y.flank.filter &
                                    abs(offset - y.offset.filter) <
                                    range.y.flank.filter/2 , TRUE, FALSE)) %>%
    ungroup()

  data.set$on.dna.filter[is.na(data.set$on.dna.filter)] <- FALSE

  return(data.set)
}



##### Add intensity filter #####

#' @title Add intensity range label
#'
#' @description A function that based on data from visually-inspected
#' trajectories creates an intensity filtering system that defines whether the
#' rest of the data is within the acceptable range or not.
#'
#' @param data.set The output of detectTrajectories() function on which
#' AddDurationFilter() and AddVisualInspection() functions are applied.
#'
#' @param guage An integer that defines how tightly the intensity range
#' filter is applied.
#'
#' @return The input data set with another column added that defines whether
#' the intensity of a signal is within the acceptable range or not.

AddIntensityFilter <- function(data.set, gauge){
  intensity.filters <- data.set %>%
    filter(duration.filter, visual.inspection) %>%
    group_by(data.set.name) %>%
    summarise(intensity.limit = mean(intensity) + gauge*sd(intensity)) %>%
    ungroup()

  data.set %<>% group_by(trajectory.unique.id) %>%
    mutate(mean.intensity = mean(intensity)) %>%
    left_join(., intensity.filters, by = join_by(data.set.name)) %>%
    mutate(intensity.filter =
             ifelse(mean.intensity < intensity.limit, TRUE, FALSE)) %>%
    ungroup()
  return(data.set)
}


##### Add overall filter #####

#' @title Define the type of the detected signal
#'
#' @description A function based on the information from different filters
#' labels that type of the detected signal.
#'
#' @param data.set The output of detectTrajectories() function on which
#' AddDurationFilter(), AddVisualInspection(), AddStuckFilter(),
#' AddFlankFilter(), and AddIntensityFilter() functions are applied.
#'
#' @return The input data set with another column added that defines the type
#' of the detected signal based on information on existing filters.

ArrangeFilters <- function(data.set){

  data.set %<>% mutate(detected.data = ifelse(duration.filter &
                                                stuck.filter &
                                                on.dna.filter &
                                                intensity.filter,
                                              "Noise-excluded data",
                                              "detected"),
                       detected.data = ifelse(!duration.filter,
                                               "Short-lived noise",
                                               detected.data),
                       detected.data = ifelse(!intensity.filter &
                                                duration.filter,
                                               "High/low intensity noise",
                                              detected.data),
                       detected.data = ifelse(!stuck.filter &
                                                intensity.filter &
                                                duration.filter,
                                               "Surface-bound emitters",
                                              detected.data),
                       detected.data = ifelse(!on.dna.filter &
                                                stuck.filter &
                                                intensity.filter &
                                                duration.filter,
                                               "Non-linear movements",
                                              detected.data))


}



##### Add binding lifetime #####

#' @title Add scanning characterization parameters
#'
#' @description A function that calculates substrate binding lifetime,
#' scanning speed, scanning coverage and accumulative scanning range.
#'
#' @param data.set The output of the noise exclusion process.
#'
#' @return The input data set with several columns added for each parameter.


AddBindingLifetime <- function(data.set, protein.filter,
                               experimental.condition) {
  data.set %>%
    filter(protein %in% protein.filter,
           detected.data == "Noise excluded") %>%
    group_by(trajectory.unique.id) %>%
    mutate(step = lead(r.X, n = 1) - r.X) %>%
    group_by(protein, trajectory.unique.id,
             across(all_of(experimental.condition))) %>%
    summarise(binding.lifetime = mean((max(frame.number) -
                                         min(frame.number)) *
                                        unique(frame.interval)/1000),
              scanning.coverage = mean((max(r.X) - min(r.X))/0.34),
              bases.scanned = mean(max(cumsum(abs(step)), na.rm = T)/0.34),
              stepping.speed = mean(mean(abs(step), na.rm = T) /
                                      frame.interval))

}



