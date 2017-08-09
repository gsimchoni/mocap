#' Make a movie out of motion data
#'
#' Use the \code{animation} and \code{scatterplot3d} packages to output a gif
#' or a mp4 movie out of the motion data extracted from the AFS/AMC files. For
#' gifs need to install ImageMagick, for mp4s need ffmpeg, see the \code{animation}
#' package for details. Note this function will create a new gif/mp4 file in your
#' working directory.
#' @param asf ASF object read with \code{readASF()}
#' @param amc AMC object read with \code{readAMC()}
#' @param xyz the \code{xyz} list object obtained with \code{getMotionData()}
#' @param movieType type of video, "gif" or "mp4", defaults to "gif"
#' @param framesPerSecond no. of frames per second, defaults to 120
#' @param skipNFrames size of frames step to skip, defaults to 1, i.e. no skipping.
#' This parameter is useful when there are many frames and the gif can fail simply
#' because the command is too long, e.g. in the "lambada" example, we input 4 here
#' @param fileName name for the output file, defaults to "motion"
#' @param imgName the \code{animation}'s \code{img.name} parameter, defaults to
#' "motion"
#' @param rotateMotion a boolean indicating whether to rotate the skeleton in
#' motion, defaults to FALSE
#' @param viewAngle the \code{scatterplot3d}'s \code{angle} parameter, defaults to 40
#' @param nSkeletons number of skeletons to animate from this motion, defaults to 1,
#' ignored if \code{twoSkeletons} is TRUE
#' @param sdExtraSkeleton standard deviation of the position of other skeletons,
#' in case nSkeletons > 1, defaults to 80 (centimeters)
#' @param twoSkeletons a boolean indicating whether a pair of skeletons are
#' entered into the function, defaults to TRUE. If FALSE one must specify \code{amc2}
#' and \code{xyz2}
#' @param amc2 AMC object of the 2nd skeleton, if \code{twoSkeletons} is TRUE
#' @param xyz2 \code{xyz} list object of the 2nd skeleton, if \code{twoSkeletons}
#' is TRUE
#' @param ... additional arguments to the \code{scatterplot3d} function
#' 
#' @references
#' A blog post describing the package with more examples: \url{http://giorasimchoni.com/2017/08/08/2017-08-08-lambada-the-mocap-package/}
#' 
#' The CMU Graphics Lab Motion Capture Database: \url{http://mocap.cs.cmu.edu/}
#' 
#' @examples
#' asfFilePath <- system.file("extdata", "lambada.asf", package = "mocap")
#' asf <- readASF(asfFilePath)
#' amcFilePath <- system.file("extdata", "lambada.amc", package = "mocap")
#' amc <- readAMC(amcFilePath, asf)
#' xyz <- getMotionData(asf, amc)
#' makeMotionMovie(asf, amc, xyz, skipNFrames = 4)
#' 
#' @export
makeMotionMovie <- function(asf, amc, xyz, movieType = "gif",
                            framesPerSecond = 120, skipNFrames = 1,
                            fileName = "motion",
                            imgName = "motion",
                            rotateMotion = FALSE,
                            viewAngle = 40,
                            nSkeletons = 1,
                            sdExtraSkeleton = 80,
                            twoSkeletons = FALSE,
                            amc2 = NULL,
                            xyz2 = NULL, ...) {
  if (!requireNamespace("animation", quietly = TRUE)) {
    stop("The animation package needed for this function to work.
         Please install it.",
         call. = FALSE)
  }
  if (twoSkeletons) {
    if (any(is.null(amc2), is.null(xyz2))) {
      stop("twoSkeletons = TRUE yet one of the 2nd skeleton parameters is NULL.")
    }
    xyzList <- list(xyz, xyz2)
    nFrames <- min(amc$nFrames, amc2$nFrames)
    if (amc$nFrames != amc2$nFrames) {
      warning("twoSkeletons mode, no. of frames in amc and amc2 objects is not
              equal, taking the minimum.")
    }
  } else if (nSkeletons > 1) {
    addX <- rnorm(nSkeletons, mean = 0, sd = sdExtraSkeleton)
    addZ <- rnorm(nSkeletons, mean = 0, sd = sdExtraSkeleton)
    xyzList <- list()
    for (i in 1:nSkeletons) {
      xyzList[[i]] <- lapply(xyz, function(x) {
        as.matrix(cbind(x[, 1] + addX[i], x[, 2], x[, 3] + addZ[i]))
      })
    }
    nFrames <- amc$nFrames
  } else {
    xyzList <- list(xyz)
    nFrames <- amc$nFrames
  }
  
  limX <- c(min(unlist(lapply(xyzList, function(x) lapply(x, function(y) min(y[, 1]))))),
            max(unlist(lapply(xyzList, function(x) lapply(x, function(y) max(y[, 1]))))))
  limY <- c(min(unlist(lapply(xyzList, function(x) lapply(x, function(y) min(y[, 2]))))),
            max(unlist(lapply(xyzList, function(x) lapply(x, function(y) max(y[, 2]))))))
  limZ <- c(min(unlist(lapply(xyzList, function(x) lapply(x, function(y) min(y[, 3]))))),
            max(unlist(lapply(xyzList, function(x) lapply(x, function(y) max(y[, 3]))))))
  
  lims <- list(limX = limX, limY = limY, limZ = limZ)
  
  animation::ani.options(interval = skipNFrames / framesPerSecond)
  
  rotateAngle <- 0
  
  fileName <- paste0(fileName, ".", movieType)
  
  if (movieType == "gif") {
    animation::saveGIF({
      for (frame in seq(1, nFrames, skipNFrames)) {
        if (rotateMotion) rotateAngle <- rotateAngle + 1
        plotMotionSingleFrame(xyzList, asf$childs, frame, lims,
                              rotateAngle, viewAngle, ...)
      }
    }, movie.name = fileName, img.name = imgName)
  } else if (movieType == "mp4") {
    animation::saveVideo({
      for (frame in seq(1, nFrames, skipNFrames)) {
        if (rotateMotion) rotateAngle <- rotateAngle + 1
        plotMotionSingleFrame(xyzList, asf$childs, frame, lims,
                              rotateAngle, viewAngle, ...)
      }
    }, video.name = fileName)
  } else {
    stop("movie type not supported") 
  }
}
