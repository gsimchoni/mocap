#' Read and Parse a AMC file
#'
#' This function reads in and parses a AMC Motion Capture file and returns
#' several useful objects
#' @param amcFilePath path to a .asf file
#' @param asf ASF object read with \code{readASF()}
#' @param in2cm a boolean indicating whether to convert inches to centimeters, defaults to TRUE
#' 
#' @return a list containing:
#' \item{D}{a matrix of dimensions bones x frames containing raw (or converted
#' to cm) AMC values}
#' \item{degrees}{a named list containing for each bone the nFrames x 3 matrix
#' with AMC Euler Angles}
#' \item{nFrames}{no. of frames found in the AMC file}
#' \item{skeleton}{a data frame containing the details for each bone, e.g. DOF,
#' length, direction, angle}
#' 
#' @note 
#' Only ASF/AMC files from the CMU Graphics Lab Motion Capture Database have been tested.
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
#' 
#' @export
readAMC <- function(amcFilePath, asf, in2cm = TRUE) {
  res <- amc2Matrix(amcFilePath)
  
  D <- res$D
  D[1:3, ] <- D[1:3, ] * ifelse(in2cm, 2.54, 1) / asf$len
  
  bones <- c(res$bones, setdiff(asf$skeleton$bone, res$bones))
  
  skeleton <- asf$skeleton[match(bones, asf$skeleton$bone),]
  
  degrees <- rep(list(matrix(0, nrow = ncol(D), ncol = 3)), length(bones))
  names(degrees) <- bones
  for (frame in 1:ncol(D)) {
    counter <- 4
    for (b in 1:length(bones)) {
      for (colNum in 1:3) {
        if (skeleton[b, 8 + colNum] == 1) {
          degrees[[bones[b]]][frame, colNum] <- D[counter, frame]
          counter <- counter + 1
        }
      }
    }
  }
  return(list(D = D, degrees = degrees,
              nFrames = ncol(D),
              skeleton = skeleton))
}