% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MPs_code.R
\name{SameTAC}
\alias{SameTAC}
\alias{FixedTAC}
\alias{MaxChange}
\alias{MaxChange2}
\title{Fixed TAC for the initial projection years}
\usage{
SameTAC(Initial_MP_Yr, Interval, Data)

FixedTAC(Rec, Data)

MaxChange(TAC, LastTAC, mc)

MaxChange2(TAC, LastTAC, mc, Brel)
}
\arguments{
\item{Initial_MP_Yr}{The calendar year when the MP will first be implemented}

\item{Interval}{The management interval where the TAC is updated}

\item{Data}{An object of class `Data`}
}
\value{
TRUE if the TAC should remain unchanged and FALSE otherwise
}
\description{
Fixed TAC for the initial projection years
}
