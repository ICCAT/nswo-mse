# Misc. Functions ----

#' Fixed TAC for the initial projection years
#'
#' @param Initial_MP_Yr The calendar year when the MP will first be implemented
#' @param Interval The management interval where the TAC is updated
#' @param Data An object of class `Data`
#'
#' @return TRUE if the TAC should remain unchanged and FALSE otherwise
#' @export
#'
SameTAC <- function(Initial_MP_Yr, Interval, Data) {
  # Are CMP implemented yet?
  if (max(Data@Year) <(Initial_MP_Yr-1)) return(TRUE)

  # Is it an TAC update year?
  Imp_Years <- seq(Initial_MP_Yr, by=Interval, length.out=30)
  if (!(max(Data@Year)+1) %in% Imp_Years) return(TRUE)

  FALSE
}

#' @export
#' @rdname SameTAC
FixedTAC <- function(Rec, Data) {

  df <- Catchdf

  if ((max(Data@Year)+1) %in% df$Year) {
    ind <- match(max(Data@Year)+1, df$Year)
    Rec@TAC <- df$Catch[ind]
  }
  Rec
}

#' @export
#' @rdname SameTAC
MaxChange <- function(TAC, LastTAC, mc) {
  deltaTAC <- TAC/ LastTAC
  if (any(is.na(mc)))
    return(TAC)

  if (length(mc)==2) {
    # different max increase/decrease
    if (deltaTAC < (1 - mc[1])) deltaTAC <- 1 - mc[1]
    if (deltaTAC > (1 + mc[2])) deltaTAC <- 1 + mc[2]
  } else {
    # same max increase/decrease
    if (deltaTAC < (1 - mc)) deltaTAC <- 1 - mc
    if (deltaTAC > (1 + mc)) deltaTAC <- 1 + mc
  }
  LastTAC * deltaTAC
}

#' @export
#' @rdname SameTAC
MaxChange2 <- function(TAC, LastTAC, mc, Brel) {
  deltaTAC <- TAC/ LastTAC
  if (any(is.na(mc)))
    return(TAC)

  if(Brel > 1){ # if Bcurrent > BMSY
    if (length(mc)==2) {
      # different max increase/decrease
      if (deltaTAC < (1 - mc[1])) deltaTAC <- 1 - mc[1]
      if (deltaTAC > (1 + mc[2])) deltaTAC <- 1 + mc[2]
    } else {
      # same max increase/decrease
      if (deltaTAC < (1 - mc)) deltaTAC <- 1 - mc
      if (deltaTAC > (1 + mc)) deltaTAC <- 1 + mc
    }
  }else{ # no constraint on downward adjustment if Bcurrent < BMSY
    if (length(mc)==2) {
      # different max increase/decrease
      if (deltaTAC > (1 + mc[2])) deltaTAC <- 1 + mc[2]
    } else {
      # same max increase/decrease
      if (deltaTAC > (1 + mc)) deltaTAC <- 1 + mc
    }

  }
  LastTAC * deltaTAC
}




