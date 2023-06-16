CEcr2 <-  CEcr
formals(CEcr2)$mc <- 0.1

# ---- Tuned CMPs ----
#' @describeIn CEcr2 Tuned to PGK_6_10 = 0.6 across Reference OMs.
#' @export
CEcr2_a <- CEcr2
formals(CEcr2_a)$tunepar <- 1.00894160018988
class(CEcr2_a) <- "MP"


#' @describeIn CEcr2 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
CEcr2_b <- CEcr2
formals(CEcr2_b)$tunepar <- 1.02558261648829
class(CEcr2_b) <- "MP"


#' @describeIn CEcr2 Tuned to PGK_6_10 = 0.51 across Reference OMs.
#' @export
CEcr2_e <- CEcr2
formals(CEcr2_e)$tunepar <- 1.06486728549688
class(CEcr2_e) <- "MP"


#' @describeIn CEcr2 Tuned to PGK_6_10 = 0.7 across Reference OMs.
#' @export
CEcr2_f <- CEcr2
formals(CEcr2_f)$tunepar <- 0.944267543480142
class(CEcr2_f) <- "MP"


#' @describeIn CEcr2 Tuned to LRP = 0.05 across Reference OMs.
#' @export
CEcr2_h <- CEcr2
formals(CEcr2_h)$tunepar <- 1.0386545431821
class(CEcr2_h) <- "MP"


