#' Get XYZ motion data from a pair of ASF/AMC objects
#'
#' This function will turn your \code{asf} and \code{amc} objects into a list of
#' 3D motion matrices, one for each bone.
#' 
#' @param asf ASF object read with \code{readASF()}
#' @param amc AMC object read with \code{readAMC()}
#' @param verbose a boolean indicating whether or not to print no. of frames read,
#' defaults to TRUE
#' 
#' @return
#' \item{xyz}{a named list of 3D motion data matrices, one for each bone, see Details.}
#' 
#' @details 
#' This function accepts a pair of \code{asf} and \code{amc} objects, for each
#' frame traverses the skeleton in a Depth First Search form to return the \code{xyz}
#' list. Each element of this list represents the 3D (global) coordinates of
#' motion for each bone. Thus each element of the list is of dimensions nFrames x 3
#' 
#' @references
#' A blog post describing the package with more examples: \url{http://giorasimchoni.com/}
#' 
#' The CMU Graphics Lab Motion Capture Database: \url{http://mocap.cs.cmu.edu/}
#' 
#' @examples
#' asfFilePath <- system.file("extdata", "lambada.asf", package = "mocap")
#' asf <- readASF(asfFilePath)
#' amcFilePath <- system.file("extdata", "lambada.amc", package = "mocap")
#' amc <- readAMC(amcFilePath, asf)
#' xyz <- getMotionData(asf, amc)
#' 
#' @export
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