CEcr4 <-  CEcr
formals(CEcr4)$mc <- 0.3

# ---- Tuned CMPs ----
#' @describeIn CEcr4 Tuned to PGK_6_10 = 0.6 across Reference OMs.
#' @export
CEcr4_a <- CEcr4
formals(CEcr4_a)$tunepar <- 1.00876661978816
class(CEcr4_a) <- "MP"


#' @describeIn CEcr4 Tuned to PGK_6_10 = 0.51 across Reference OMs.
#' @export
CEcr4_e <- CEcr4
formals(CEcr4_e)$tunepar <- 1.0531935739917
class(CEcr4_e) <- "MP"


#' @describeIn CEcr4 Tuned to PGK_6_10 = 0.7 across Reference OMs.
#' @export
CEcr4_f <- CEcr4
formals(CEcr4_f)$tunepar <- 0.96054607437874
class(CEcr4_f) <- "MP"


#' @describeIn CEcr4 Tuned to LRP = 0.05 across Reference OMs.
#' @export
CEcr4_h <- CEcr4
formals(CEcr4_h)$tunepar <- 1.0689777137849
class(CEcr4_h) <- "MP"


