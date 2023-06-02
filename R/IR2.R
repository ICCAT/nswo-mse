#' @describeIn IR1 Same as IR1, but mean index values are calculated over last 2 years and the previous 2 years before that
IR2 <- IR1
formals(IR2)$yrs <- c(2,2)



