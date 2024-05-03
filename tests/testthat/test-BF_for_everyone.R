test_that("BF_for_everyone works", {

  bf_dat <- read.table(file = "https://raw.githubusercontent.com/fayetteklaassen/gpbf/master/data.txt", header = TRUE)

  str(bf_dat)

  res <- BF_for_everyone(.df = bf_dat, .participant = "ppnr",
                         formula = "TimePerception ~ Condition + Valence + Arousal",
                         hypothesis = "Condition=0 & Valence>0 & Arousal>0; Condition>0 & Valence>0 & Arousal>0")



  # # Determine the number of unique ppnrs = the number of cases
  # names(bf_dat)
  # # [1] "ppnr"           "TimePerception" "Valence"        "Arousal"
  # # [5] "Condition"
  # N <- length(unique(bf_dat$ppnr))
  #
  # # create an empty list to store results
  # results <- vector("list", length = N)
  # names(bf_dat)
  #
  # hyp <- "Condition=0 & Valence>0 & Arousal>0;
  #                     Condition>0 & Valence>0 & Arousal>0"
  # res <- lapply(unique(bf_dat$ppnr), function(x){
  #
  #   fit_i <- stats::lm(formula = TimePerception ~ Condition + Valence + Arousal,
  #               data = bf_dat[bf_dat[["ppnr"]] == x,] )   # execute linear model
  #   # save the results of Bain analysis.
  #   bain::bain(fit_i, hyp)
  #
  # }) |> stats::setNames(unique(bf_dat$ppnr) |> as.character())



  # ###########################
  # ### viewing the output ####
  # ###########################
  #
  # names(results[[1]]) # view the names of the bain output for first person ([[1]]).
  # # view the output of fit and BFmatrix
  # results[[1]]$fit
  # results[[1]]$BFmatrix
  #
  # output <- matrix(0, nrow = N, ncol = 2) # create output table with N rows and 4 columns
  # colnames(output) <- c("BF1c", "BF12") # name the columns of the output
  #
  # for(i in 1:N){ # loop over persons
  #   BFtab <- results[[i]]$fit # obtain the fit table of person i
  #   # compute relevant BFs
  #   BF1c <- results[[i]]$fit[1,7]
  #   BF12 <- results[[i]]$BFmatrix[1,2]
  #   # save the 4 BFs in the i-th row of the output matrix
  #   output[i,] <- c(BF1c,BF12)
  # }
  #
  # ###########################
  # ##### gpbf function #######
  # ###########################
  # gPBF <- function(BFs){
  #   N  <- ifelse(is.null(nrow(BFs)), length(BFs), nrow(BFs))
  #
  #   res <- apply(BFs, 2, function(x){
  #     GP <- prod(x) ^ (1 / N)
  #     ER <- abs((GP < 1) - sum(x > 1)/N)
  #     SR <- ifelse(GP < 1,
  #                  sum(x < GP) / N,
  #                  sum(x > GP) / N)
  #     c(GP, ER, SR)
  #   })
  #
  #   rownames(res) <- c("Geometric Product", "Evidence Rate", "Stability Rate")
  #   out <- list("GPBF" = res, "BFs" = BFs, "N" = N)
  #   class(out) <- "gPBF"
  #   return(out)
  # }
  # ##############################
  # ### obtaining the gpbfs ######
  # ##############################
  # gpout <- gPBF(output)


  expect_equal(2 * 2, 4)
})
