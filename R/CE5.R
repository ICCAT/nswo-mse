CE5<-  CE
formals(CE5)$mc <- NA

# ---- Tuned CMPs ----
#' @describeIn CE5 Tuned to PGK_6_10 = 0.6 across Reference OMs.
#' @export
CE5_a <- CE5
formals(CE5_a)$tunepar <- 1.00271002710027
class(CE5_a) <- "MP"


#' @describeIn CE5 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
CE5_b <- CE5
formals(CE5_b)$tunepar <- 1.06299098050626
class(CE5_b) <- "MP"


#' @describeIn CE5 Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
CE5_c <- CE5
formals(CE5_c)$tunepar <- 1.00798872180451
class(CE5_c) <- "MP"


#' @describeIn CE5 Tuned to PGK_6_10 = 0.51 across Reference OMs.
#' @export
CE5_e <- CE5
formals(CE5_e)$tunepar <- 1.04190863337205
class(CE5_e) <- "MP"


#' @describeIn CE5 Tuned to PGK_6_10 = 0.7 across Reference OMs.
#' @export
CE5_f <- CE5
formals(CE5_f)$tunepar <- 0.957783113245298
class(CE5_f) <- "MP"


#' @describeIn CE5 Tuned to LRP = 0.05 across Reference OMs.
#' @export
CE5_h <- CE5
formals(CE5_h)$tunepar <- 1.00892857142857
class(CE5_h) <- "MP"


