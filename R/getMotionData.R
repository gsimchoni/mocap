#' Get XYZ motion data from a pair of ASF/AMC files
#'
#' This function allows you to...
#' @param asf ASF object read with \code{readASF()}
#' @param amc AMC object read with \code{readAMC()}
#' @param verbose desc
#' @export
#' @examples
#' asfFilePath <- system.file("extdata", "lambada.asf", package = "mocap")
#' asf <- readASF(asfFilePath)
#' amcFilePath <- system.file("extdata", "lambada.amc", package = "mocap")
#' amc <- readAMC(amcFilePath, asf)
#' xyz <- getMotionData(asf, amc)
getMotionData <- function(asf, amc, verbose = FALSE) {
  xyz <- rep(list(matrix(0, nrow = amc$nFrames, ncol = 3)), nrow(amc$skeleton))
  names(xyz) <- amc$skeleton$bone
  bonesVector <- split(amc$skeleton[, c("x", "y", "z")],
                       seq(nrow(amc$skeleton[, c("x", "y", "z")])))
  names(bonesVector) <- amc$skeleton$bone
  xyz <- traverseSkeleton("root", asf, amc, xyz, 1, amc$nFrames,
                          bonesVector, verbose)
  return(xyz)
}