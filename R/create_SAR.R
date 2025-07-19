#' Create a species-area relationship (SAR) plot
#'
#' Use segmented regression to create a species-area relationship (SAR) plot.
#' The X axis represents log(island area) and the Y axis represents log(number
#' of species)
#'
#' If the user would prefer to create their own plot of the
#' `ssarp::create_SAR()` output, the `aggDF` element of the returned list
#' includes the raw points from the plot created here. They can be accessed
#' as demonstrated in the Examples section.
#' @param occurrences The dataframe output by `ssarp::find_areas()` (or if
#' using  a custom dataframe, ensure that it has the following columns:
#' specificEpithet, areas)
#' @param npsi The maximum number of breakpoints to estimate for model
#' selection.  Default: 1
#' @param visualize (boolean) Whether the plot should be displayed when the
#' function is called. Default: FALSE
#' @return A list of 4 including: the summary output, the regression
#' object, the aggregated dataframe used to create the plot, and the AIC scores
#' used in model selection
#' @examples
#' # The GBIF key for the Anolis genus is 8782549
#' # Read in example dataset filtered from:
#' #  dat <- rgbif::occ_search(taxonKey = 8782549,
#' #                           hasCoordinate = TRUE,
#' #                           limit = 10000)
#' dat <- read.csv(system.file("extdata",
#'                             "ssarp_Example_Dat.csv",
#'                             package = "ssarp"))
#' land <- find_land(occurrences = dat)
#' areas <- find_areas(occs = land)
#'
#' seg <- create_SAR(occurrences = areas,
#'                   npsi = 1,
#'                   visualize = FALSE)
#' plot(seg)
#' summary <- seg$summary
#' model_object <- seg$segObj
#' points <- seg$aggDF
#' @export

create_SAR <- function(occurrences, npsi = 1, visualize = FALSE) {
  # Checkmate input validation
  checkmate::assertDataFrame(occurrences)
  checkmate::assertNumeric(npsi)
  checkmate::testSubset(c("specificEpithet", "areas"), names(occurrences))
  checkmate::assertLogical(visualize)
  # Ensure columns are correct type
  checkmate::assertCharacter(occurrences$specificEpithet)
  checkmate::assertNumeric(occurrences$areas)

  #   formula Species ~ Area means to group scientific names by area
  #   function(x) length(unique(x)) tells it to give me the number of unique
  #     species for each unique island area
  agg <- stats::aggregate(
    data = occurrences,
    specificEpithet ~ areas,
    function(x) length(unique(x))
  )

  # Segmented package prefers tidy dataframes, so make one for it
  dat <- data.frame(x = log(agg$areas), y = log(agg$specificEpithet))

  # Run a linear model on the data to use in creating
  #  segmented/breakpoint regression
  linear <- stats::lm(y ~ x, data = dat)

  # Record x and y min/max for plot limits
  x_max <- max(dat$x)
  x_min <- min(dat$x)
  y_max <- max(dat$y)
  y_min <- min(dat$y)

  # First, create all requested models and compare their AIC scores to determine
  #  the best-fit
  # Empty list to populate with AIC scores
  aic_scores <- list()
  # Linear model is already created above
  aic_scores[1] <- stats::AIC(linear)
  # Name the list element in case the user wants to examine the AIC scores
  names(aic_scores)[1] <- "Linear"

  # Create a segmented model for each level of npsi specified by the user
  # This only makes sense if the user does not specify that they want zero
  #  breakpoints
  if (npsi != 0) {
    for (i in seq(npsi)) {
      seg <- segmented::segmented(
        linear,
        seg.Z = ~x,
        npsi = i,
        control = segmented::seg.control(display = FALSE, quant = TRUE)
      )
      # Since the first index is always linear, use i+1
      aic_scores[i + 1] <- stats::AIC(seg)
      # Name the list element
      names(aic_scores)[i + 1] <- paste0("BP", i)
    }
  }

  # Find the smallest AIC score (the best-fit model)
  min_score <- which.min(aic_scores)

  # If the min_score is index 1, then the best-fit model is linear
  if (min_score == 1) {
    # Plot linear regression (if the user wants)
    if (visualize) {
      plot(
        dat,
        xlim = c(x_min, (x_max + 0.5)),
        ylim = c(y_min, (y_max + 0.5)),
        ylab = "Log Number of Species",
        xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
        main = "Species-Area Relationship",
        pch = 16
      )
      graphics::abline(linear)
    }

    summary_line <- summary(linear)

    result <- list(
      "summary" = summary_line,
      "linObj" = linear,
      "aggDF" = dat,
      "AICscores" = aic_scores
    )

    class(result) <- "SAR"

    return(result)
  } else if (min_score == 2) {
    # Linear is the object we're segmenting, seg.Z is the continuous variable,
    #  npsi is the number of breakpoints to estimate,
    #  control is the bootstrap parameters
    #  (display = FALSE stops it from printing each iteration)
    # If min_score is 2, the best-fit model has one breakpoint
    seg <- segmented::segmented(
      linear,
      seg.Z = ~x,
      npsi = 1,
      control = segmented::seg.control(display = FALSE)
    )

    # Plot the breakpoint regression line (if user wants)
    if (visualize) {
      plot(
        seg,
        rug = FALSE,
        xlim = c(x_min, (x_max + 0.5)),
        ylim = c(y_min, (y_max + 0.5)),
        ylab = "Log Number of Species",
        xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
        main = "Species-Area Relationship"
      )
      # Add the points
      graphics::points(dat$x, dat$y, pch = 19)
    }

    # Save the summary as an object to add to the result list
    summary_seg <- summary(seg)

    result <- list(
      "summary" = summary_seg,
      "segObj" = seg,
      "aggDF" = dat,
      "AICscores" = aic_scores
    )

    class(result) <- "SAR"

    return(result)
  } else {
    # If min_score > 2, then the best-fit model has multiple breakpoints
    # If npsi is > 1, the seg object plots differently, so it needs a separate
    #  else statement
    # The actual number of segments is the index of the minimum AIC minus 1
    #  (because the first index is always linear)
    n_psi <- min_score - 1
    seg <- segmented::segmented(
      linear,
      seg.Z = ~x,
      npsi = n_psi,
      control = segmented::seg.control(display = FALSE)
    )

    # Plot defaults to multiple outputs when npsi > 1, so my npsi = 1 plot
    #  doesn't apply
    if (visualize) {
      plot(seg)
    }

    # Save the summary as an object to add to the result list
    summary_seg <- summary(seg)

    result <- list(
      "summary" = summary_seg,
      "segObj" = seg,
      "aggDF" = dat,
      "AICscores" = aic_scores
    )

    class(result) <- "SAR"
    return(result)
  }
}
