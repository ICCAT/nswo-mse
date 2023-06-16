CE3 <-  CE
formals(CE3)$mc <- 0.2

# ---- Tuned CMPs ----
#' @describeIn CE3 Tuned to PGK_6_10 = 0.6 across Reference OMs.
#' @export
CE3_a <- CE3
formals(CE3_a)$tunepar <- 1.00408777969019
class(CE3_a) <- "MP"


#' @describeIn CE3 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
CE3_b <- CE3
formals(CE3_b)$tunepar <- 1.06319133913862
class(CE3_b) <- "MP"


#' @describeIn CE3 Tuned to PGK_6_10 = 0.51 across Reference OMs.
#' @export
CE3_e <- CE3
formals(CE3_e)$tunepar <- 1.04765490533563
class(CE3_e) <- "MP"


#' @describeIn CE3 Tuned to PGK_6_10 = 0.7 across Reference OMs.
#' @export
CE3_f <- CE3
formals(CE3_f)$tunepar <- 0.953728661275831
class(CE3_f) <- "MP"


#' @describeIn CE3 Tuned to LRP = 0.05 across Reference OMs.
#' @export
CE3_h <- CE3
formals(CE3_h)$tunepar <- 1.04464285714286
class(CE3_h) <- "MP"


