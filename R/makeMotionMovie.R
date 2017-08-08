#' Make a movie out of motion data
#'
#' Use the \code{animation} and \code{scatterplot3d} packages to output a gif
#' or a mp4 movie out of the motion data extracted from the AFS/AMC files. For
#' gifs need to install ImageMagick, for mp4s need ffmpeg, see the \code{animation}
#' package for details.
#' @param asf ASF object read with \code{readASF()}
#' @param amc AMC object read with \code{readAMC()}
#' @param xyz the \code{xyz} list object obtained with \code{getMotionData()}
#' @param type type of video, "gif" or "mp4", defaults to "gif"
#' @param framesPerSecond no. of frames per second, defaults to 120
#' @param skipNFrames size of frames step to skip, defaults to 1, i.e. no skipping
#' @param fileName name for the output file, defaults to "motion"
#' @param imgName the \code{animation}'s \code{img.name} parameter, defaults to
#' "motion"
#' @param rotateMotion a boolean indicating whether to rotate the skeleton in
#' motion, defaults to FALSE
#' @param viewAngle the \code{scatterplot3d}'s \code{angle} parameter, defaults to 40
#' @param ... additional arguments to the \code{scatterplot3d} function
#' 
#' @references
#' \url{http://giorasimchoni.com/}
#' \url{http://mocap.cs.cmu.edu/}
#' 
#' @examples
#' asfFilePath <- system.file("extdata", "lambada.asf", package = "mocap")
#' asf <- readASF(asfFilePath)
#' amcFilePath <- system.file("extdata", "lambada.amc", package = "mocap")
#' amc <- readAMC(amcFilePath, asf)
#' xyz <- getXYZLocationData(asf, amc)
#' makeMotionMovie(asf, amc, xyz, skipNFrames = 4)
#' 
#' @export
makeMotionMovie <- function(asf, amc, xyz, type = "gif",
                            framesPerSecond = 120, skipNFrames = 1,
                            fileName = paste0("motion.", type),
                            imgName = "motion",
                            rotateMotion = FALSE,
                            viewAngle = 40, ...) {
  if (!requireNamespace("animation", quietly = TRUE)) {
    stop("The animation package needed for this function to work.
         Please install it.",
         call. = FALSE)
  }
  
  limX <- c(min(unlist(lapply(xyz, function(x) min(x[, 1])))),
            max(unlist(lapply(xyz, function(x) max(x[, 1])))))
  limY <- c(min(unlist(lapply(xyz, function(x) min(x[, 2])))),
            max(unlist(lapply(xyz, function(x) max(x[, 2])))))
  limZ <- c(min(unlist(lapply(xyz, function(x) min(x[, 3])))),
            max(unlist(lapply(xyz, function(x) max(x[, 3])))))
  
  lims <- list(limX = limX, limY = limY, limZ = limZ)
  
  animation::ani.options(interval = skipNFrames / framesPerSecond)
  
  rotateAngle <- 0
  
  if (type == "gif") {
    animation::saveGIF({
      for (frame in seq(1, amc$nFrames, skipNFrames)) {
        plotMotionSingleFrame(xyz, asf$childs, frame, lims)
      }
    }, movie.name = fileName, img.name = imgName)
  } else if (type == "mp4") {
    animation::saveVideo({
      for (frame in seq(1, amc$nFrames, skipNFrames)) {
        if (rotateMotion) rotateAngle <- rotateAngle + 1
        plotMotionSingleFrame(xyz, asf$childs, frame, lims,
                              rotateAngle, viewAngle, ...)
      }
    }, video.name = fileName)
  } else {
    stop("movie type not supported") 
  }
}
