CE2 <-  CE
formals(CE2)$mc <- 0.1

# ---- Tuned CMPs ----
#' @describeIn CE2 Tuned to PGK_6_10 = 0.6 across Reference OMs.
#' @export
CE2_a <- CE2
formals(CE2_a)$tunepar <- 1.00880099368471
class(CE2_a) <- "MP"


#' @describeIn CE2 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
CE2_b <- CE2
formals(CE2_b)$tunepar <- 1.02531570767842
class(CE2_b) <- "MP"


#' @describeIn CE2 Tuned to PGK_6_10 = 0.51 across Reference OMs.
#' @export
CE2_e <- CE2
formals(CE2_e)$tunepar <- 1.06380087495309
class(CE2_e) <- "MP"


#' @describeIn CE2 Tuned to PGK_6_10 = 0.7 across Reference OMs.
#' @export
CE2_f <- CE2
formals(CE2_f)$tunepar <- 0.944228639577477
class(CE2_f) <- "MP"


#' @describeIn CE2 Tuned to LRP = 0.05 across Reference OMs.
#' @export
CE2_h <- CE2
formals(CE2_h)$tunepar <- 1.03818332829961
class(CE2_h) <- "MP"


