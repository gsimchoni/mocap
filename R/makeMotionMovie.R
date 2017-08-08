#' Make a movie out of motion data
#'
#' This function allows you to...
#' @param asf desc
#' @param amc desc
#' @param xyz desc
#' @param type desc
#' @param framesPerSecond desc
#' @param skipNFrames desc
#' @param fileName desc
#' @param imgName desc
#' @param rotateMotion desc
#' @param viewAngle desc
#' @param ... desc
#' @export
#' @examples
#' asfFilePath <- system.file("extdata", "lambada.asf", package = "mocap")
#' asf <- readASF(asfFilePath)
#' amcFilePath <- system.file("extdata", "lambada.amc", package = "mocap")
#' amc <- readAMC(amcFilePath, asf)
#' xyz <- getXYZLocationData(asf, amc)
#' makeMotionMovie(asf, amc, xyz, skipNFrames = 4)
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
