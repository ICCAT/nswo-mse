CE4 <-  CE
formals(CE4)$mc <- 0.3

# ---- Tuned CMPs ----
#' @describeIn CE4 Tuned to PGK_6_10 = 0.6 across Reference OMs.
#' @export
CE4_a <- CE4
formals(CE4_a)$tunepar <- 1.00805782376253
class(CE4_a) <- "MP"


#' @describeIn CE4 Tuned to PGK_6_10 = 0.51 across Reference OMs.
#' @export
CE4_e <- CE4
formals(CE4_e)$tunepar <- 1.05208252677765
class(CE4_e) <- "MP"


#' @describeIn CE4 Tuned to PGK_6_10 = 0.7 across Reference OMs.
#' @export
CE4_f <- CE4
formals(CE4_f)$tunepar <- 0.959430389334949
class(CE4_f) <- "MP"


#' @describeIn CE4 Tuned to LRP = 0.05 across Reference OMs.
#' @export
CE4_h <- CE4
formals(CE4_h)$tunepar <- 1.07142857142857
class(CE4_h) <- "MP"


