#' @describeIn IR1 Same as IR1, but mean index values are calculated over last 2 years and the previous 2 years before that
IR2 <- IR1
formals(IR2)$yrs <- c(2,2)




# ---- Tuned CMPs ----
#' @describeIn IR2 Tuned to PGK_6_10 = 0.6 across Reference OMs.
#' @export
IR2_a <- IR2
formals(IR2_a)$tunepar <- 1.00732609275053
class(IR2_a) <- "MP"


#' @describeIn IR2 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
IR2_b <- IR2
formals(IR2_b)$tunepar <- 1.07142857142857
class(IR2_b) <- "MP"


#' @describeIn IR2 Tuned to PGK_6_10 = 0.51 across Reference OMs.
#' @export
IR2_e <- IR2
formals(IR2_e)$tunepar <- 1.05251409774436
class(IR2_e) <- "MP"


#' @describeIn IR2 Tuned to LRP = 0.05 across Reference OMs.
#' @export
IR2_h <- IR2
formals(IR2_h)$tunepar <- 1.05535714285714
class(IR2_h) <- "MP"


