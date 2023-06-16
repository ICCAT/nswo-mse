CEcr3 <-  CEcr
formals(CEcr3)$mc <- 0.2

# ---- Tuned CMPs ----
#' @describeIn CEcr3 Tuned to PGK_6_10 = 0.6 across Reference OMs.
#' @export
CEcr3_a <- CEcr3
formals(CEcr3_a)$tunepar <- 1.00477637863656
class(CEcr3_a) <- "MP"


#' @describeIn CEcr3 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
CEcr3_b <- CEcr3
formals(CEcr3_b)$tunepar <- 1.07117925704313
class(CEcr3_b) <- "MP"


#' @describeIn CEcr3 Tuned to PGK_6_10 = 0.51 across Reference OMs.
#' @export
CEcr3_e <- CEcr3
formals(CEcr3_e)$tunepar <- 1.04874077290491
class(CEcr3_e) <- "MP"


#' @describeIn CEcr3 Tuned to PGK_6_10 = 0.7 across Reference OMs.
#' @export
CEcr3_f <- CEcr3
formals(CEcr3_f)$tunepar <- 0.954113924050633
class(CEcr3_f) <- "MP"


#' @describeIn CEcr3 Tuned to LRP = 0.05 across Reference OMs.
#' @export
CEcr3_h <- CEcr3
formals(CEcr3_h)$tunepar <- 1.04251700680272
class(CEcr3_h) <- "MP"


