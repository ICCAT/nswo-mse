CEcr5 <-  CEcr
formals(CEcr5)$mc <- NA

# ---- Tuned CMPs ----
#' @describeIn CEcr5 Tuned to PGK_6_10 = 0.6 across Reference OMs.
#' @export
CEcr5_a <- CEcr5
formals(CEcr5_a)$tunepar <- 1.01011378002528
class(CEcr5_a) <- "MP"


#' @describeIn CEcr5 Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
CEcr5_c <- CEcr5
formals(CEcr5_c)$tunepar <- 0.984070796460177
class(CEcr5_c) <- "MP"


#' @describeIn CEcr5 Tuned to PGK_6_10 = 0.51 across Reference OMs.
#' @export
CEcr5_e <- CEcr5
formals(CEcr5_e)$tunepar <- 1.05198033243113
class(CEcr5_e) <- "MP"


#' @describeIn CEcr5 Tuned to PGK_6_10 = 0.7 across Reference OMs.
#' @export
CEcr5_f <- CEcr5
formals(CEcr5_f)$tunepar <- 0.961703820490167
class(CEcr5_f) <- "MP"


#' @describeIn CEcr5 Tuned to LRP = 0.1 across Reference OMs.
#' @export
CEcr5_g <- CEcr5
formals(CEcr5_g)$tunepar <- 1.04687104930468
class(CEcr5_g) <- "MP"


#' @describeIn CEcr5 Tuned to LRP = 0.05 across Reference OMs.
#' @export
CEcr5_h <- CEcr5
formals(CEcr5_h)$tunepar <- 0.979650532779483
class(CEcr5_h) <- "MP"


