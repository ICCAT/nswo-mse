#' @describeIn IR1 Same as IR1, but mean index values are calculated over last 2 years and the previous 2 years before that
IR2 <- IR1
formals(IR2)$yrs <- c(2,2)


# ---- Tuned CMPs ----
#' @describeIn IR2 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
IR2_a <- IR2
formals(IR2_a)$tunepar <- 0.876
class(IR2_a) <- "MP"


#' @describeIn IR2 Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
IR2_b <- IR2
formals(IR2_b)$tunepar <- 0.958
class(IR2_b) <- "MP"


#' @describeIn IR2 Tuned to PGK_30 = 0.6 across Reference OMs.
#' @export
IR2_c <- IR2
formals(IR2_c)$tunepar <- 0.977
class(IR2_c) <- "MP"


