ExtractFleetQuantity <- function(OM,
                                 quantity = c('Selectivity', 'Retention', 'DiscardMortality'),
                                 by       = c('length', 'age'),
                                 years    = NULL,
                                 fleets   = NULL) {

  quantity <- match.arg(quantity)
  by       <- match.arg(by)

  accessor <- switch(quantity,
                     Selectivity      = Selectivity,
                     Retention        = Retention,
                     DiscardMortality = DiscardMortality
  )

  mean_fn <- switch(by,
                    length = MeanAtLength,
                    age    = MeanAtAge
  )

  label <- switch(quantity,
                  Selectivity      = 'Selectivity',
                  Retention        = 'Retention',
                  DiscardMortality = 'Discard Mortality'
  )

  if (is.null(years)) years <- max(Years(OM, 'H'))

  subset_args <- list(Year = years)
  if (!is.null(fleets)) subset_args$Fleets <- fleets

  arr <- purrr::map(accessor(OM@Fleet), \(stocklist) {
    purrr::map(stocklist, mean_fn) |> List2Array()
  }) |>
    List2Array('Stock', pos = 2) |>
    DropDimension('Area')

  do.call(Subset, c(list(arr), subset_args)) |>
    ReduceDims(IncYear = TRUE) |>
    Array2DF() |>
    dplyr::mutate(Variable = label,
                  Year     = as.factor(Year))
}


shift_selectivity_left <- function(MeanAtLength, classes, delta) {
  dnames <- dimnames(MeanAtLength)

  # Identify peak as the first index where the mean curve (over sims/years/areas)
  # reaches its maximum - use first peak to handle flat-topped curves
  mean_curve <- apply(MeanAtLength, 'Class', mean)
  peak_ind   <- which.max(mean_curve)

  asc_ind     <- seq_len(peak_ind - 1)
  asc_classes <- classes[asc_ind]
  shifted_asc <- asc_classes * delta

  out <- apply(MeanAtLength, c('Sim', 'Year', 'Area'), \(v) {
    new_asc <- approx(shifted_asc, v[asc_ind],
                      xout = asc_classes, rule = 2)$y
    c(new_asc, v[seq(peak_ind, length(v))])
  }) |> aperm(c(2, 1, 3, 4))

  dimnames(out) <- dnames
  out
}


