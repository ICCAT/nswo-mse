#' Tune a Candidate Management Procedure with 1 Tuning Parameter
#'
#' @param multiHist An object of class `multiList` or a list of `multiList` objects
#' @param init_vals Initial values for the tuning parameter. Numeric values length 3
#' @param MP A function of class `MP`
#' @param tunefunc A function that takes an object of class `MMSE` and returns a numeric value
#' @param tunetarg The target value for `tunefunc` output
#' @param step Step size for new proposed tuning values
#' @param maxit Maximum number of iterations
#' @param tol Stopping tolerance
#' @param rnd Numeric value for rounding displayed numbers
#' @param silent Logical. Hide the output while running closed-loop simulations?
#' @export
TuneMP_1_Par <- function(multiHist=NULL,
                         init_vals=NULL,
                         MP=NULL,
                         tunefunc=NULL,
                         tunetarg=1,
                         step=0.8,
                         maxit=10,
                         tol=0.01,
                         rnd=3,
                         silent=TRUE){

  # ---- Checks ----
  if (inherits(multiHist, 'list') & !inherits(multiHist, 'multiHist'))
    if (!all(unlist(lapply(multiHist, inherits, 'multiHist'))))
      stop('`multiHist` must be a list of objects of class `multiHist')

  if (length(init_vals)!=3)
    stop('`init_vals` must be numeric values of length 3')

  if (any(diff(init_vals)<=0))
    stop('`init_vals` must be in ascending order')

  if (!inherits(MP, 'MP'))
    stop('`MP` must be a function of class `MP`')

  if (!inherits(tunefunc, 'function'))
    stop('`tunefunc` must be a function that takes an object of class `MMSE`')

  if  (inherits(multiHist, 'list') & !inherits(multiHist, 'multiHist')) {
    n.OM <- length(multiHist)
  } else {
    if (!inherits(multiHist, 'multiHist'))
      stop('`multiHist` must be class `multiHist')
    n.OM <- 1
  }

  MSEtool:::message_info('Tuning over', n.OM, 'OM(s)')

  # ---- Tuning MPs ----
  MP1 <- function(x,Data,tunepar,...) MP(x,Data,tunepar=par1, ...)
  MP2 <- function(x,Data,tunepar,...) MP(x,Data,tunepar=par2, ...)
  MP3 <- function(x,Data,tunepar,...) MP(x,Data,tunepar=par3, ...)
  class(MP1) <- class(MP2) <- class(MP3) <- "MP"

  assign("MP1", MP1, envir=globalenv())
  assign("MP2", MP2, envir=globalenv())
  assign("MP3", MP3, envir=globalenv())

  # ---- Tuning Values ----
  vals_array <- array(NA, c(maxit,3))
  vals_array[1,] <- init_vals
  i_vals <- rep(NA, maxit)
  i_vals[1] <- mean(init_vals)

  i <- 0
  dif <- 1E10
  dfList <- list()
  while(i<=maxit & dif>tol){
    i <- i+1
    MSEtool:::message('Iteration', i, 'of', maxit)
    assign("par1", vals_array[i,1], envir=globalenv())
    assign("par2", vals_array[i,2], envir=globalenv())
    assign("par3", vals_array[i,3], envir=globalenv())

    # Projections
    if (n.OM==1) {
      MMSE <- ProjectMOM(multiHist, MPs=c("MP1","MP2","MP3"),silent=silent,
                         parallel = FALSE, checkMPs=FALSE)
      df <- data.frame(i=i,
                       OM=1,
                       TunePar=vals_array[i,],
                       TuneVal=tunefunc(MMSE)
      )
    } else {
      MMSEList <- list()
      dflist <- list()
      for (om in 1:n.OM) {
        MSEtool:::message('Projecting OM', om, 'of', n.OM)
        MMSEList[[om]] <- ProjectMOM(multiHist[[om]], MPs=c("MP1","MP2","MP3"),
                                     silent=silent,parallel = FALSE, checkMPs=FALSE)
        dflist[[om]] <- data.frame(i=i,
                                   OM=om,
                                   TunePar=vals_array[i,],
                                   TuneVal=tunefunc(MMSEList[[om]])
        )
      }
      df <- do.call('rbind', dflist)
      MMSE <- combine_MMSE(MMSEList, 'name')
    }

    tune_vals <- tunefunc(MMSE)
    i_vals[i+1] <- approx(tune_vals,vals_array[i,],xout=tunetarg)$y[[1]]


    plot(vals_array[i,],tune_vals,pch=19,xlab="Tuning Parameter",ylab="Target Variable")
    lines(vals_array[i,],tune_vals,col="grey")
    abline(h=tunetarg, lty=2)
    abline(v=i_vals[i+0:1],col=c("grey","red"))

    rng <- abs(vals_array[i,1]-vals_array[i,2])*0.5*step
    trial <- c(i_vals[i+1]-rng, i_vals[i+1]+rng)
    trial[trial<min(vals_array[i,])]<-min(vals_array[i,])
    trial[trial>max(vals_array[i,])]<-max(vals_array[i,])
    vals_array[i+1,]<-c(trial[1],mean(trial),trial[2])
    dif <- abs(i_vals[i+1]-i_vals[i])
    abline(v=trial,col='blue',lty=2)
    legend('topleft',legend=paste0('Diff = ', round(dif,5)),text.col='red',bty='n')

    MSEtool:::message_info(paste0('Tuning Parameters: ', paste(round(vals_array[i,],rnd), collapse=" ")))
    MSEtool:::message_info(paste0('Tuning Target: ', paste(round(tune_vals,rnd), collapse=" ")))
    MSEtool:::message_info('Proposed Tuning Parameter:',round(i_vals[i+1],rnd))
    MSEtool:::message_info('Difference from Previous', round(dif,rnd), paste0('(tolerance: ', tol, ')') )
    MSEtool:::message_info(paste0('New Tuning Parameters: ', paste(round(vals_array[i+1,],rnd), collapse=" ")))

    dfList[[i]] <- df

  }
  df <- do.call('rbind', dfList)
  df <- df %>% round(3)

  out <- list()
  tune_val <- i_vals[!is.na(i_vals)]
  out$tune_val <- round(tune_val[length(tune_val)], rnd)
  out$i_vals <- i_vals
  out$df <- df
  out$tunetarg <- tunetarg
  out$tunefunc <- tunefunc
  out$MP <- MP
  out
}
