#' Plot a single frame of 3D motion data
#'
#' Plot a single 3D frame of the \code{xyz} data obtained with \code{getMotionData()}.
#' The plot used is a \code{scatterplot3d} plot.
#' @param xyzList a list of \code{xyz} objects such as obtained with \code{getMotionData()}
#' Each \code{xyz} object depicts the motion of a single skeleton
#' @param childs the \code{childs} list of a \code{asf} object obtained with \code{readASF()}
#' @param frame a frame number (currently unvalidated!)
#' @param lims a list of limits for the x, y, z axes.
#' Format: list(limX = c(min, max), limY = c(min, max), limZ = c(min, max))
#' @param rotateAngle angle in degrees to rotate the entire skeleton around the
#' Y (upwards) axis, defaults to 0
#' @param viewAngle the \code{scatterplot3d}'s \code{angle} parameter, defaults to 40
#' @param mainTitle the \code{scatterplot3d}'s \code{main} title parameter,
#' defaults o frame number
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
#' limX <- c(min(unlist(lapply(xyz, function(x) min(x[, 1])))),
#'   max(unlist(lapply(xyz, function(x) max(x[, 1])))))
#' limY <- c(min(unlist(lapply(xyz, function(x) min(x[, 2])))),
#'   max(unlist(lapply(xyz, function(x) max(x[, 2])))))
#' limZ <- c(min(unlist(lapply(xyz, function(x) min(x[, 3])))),
#'   max(unlist(lapply(xyz, function(x) max(x[, 3])))))
#' lims <- list(limX = limX, limY = limY, limZ = limZ)
#' plotMotionSingleFrame(list(xyz), asf$childs, 1, lims)
#' 
#' @export
plotMotionSingleFrame <- function(xyzList, childs, frame, lims,
                                  rotateAngle = 0,
                                  viewAngle = 40,
                                  mainTitle = frame, ...) {
  for (i in 1:length(xyzList)) {
    xyz <- xyzList[[i]]
    pos <- as.data.frame(t(sapply(xyz, function(x) x[frame, ]))[, c(1, 3, 2)])
    colnames(pos) <- c("x", "z", "y")
    pos[, 2] <- -pos[, 2]
    if (rotateAngle != 0) {
      pos <- as.data.frame(as.matrix(pos) %*%
                             rotation_matrix(deg2rad(rotateAngle), "z"))
      # TODO Avoid these magic numbers
      lims <- list(limX = c(-200, 200), limY = c(-200, 200), limZ = c(-200, 200))
    }
    if (i == 1) {
      s3d <- scatterplot3d::scatterplot3d(pos,
                                          xlim = lims$limX, ylim = lims$limZ,
                                          zlim = lims$limY,
                                          box = FALSE, main = mainTitle, type = "n",
                                          angle = viewAngle, ...)
    } else {
      s3d$points3d(pos, xlim = lims$limX, ylim = lims$limZ, type = "n")
    }
    
    for (parent in names(childs)) {
      for (child in childs[[parent]]) {
        pParent <- s3d$xyz.convert(pos[rownames(pos) == parent, ])
        pChild <- s3d$xyz.convert(pos[rownames(pos) == child, ])
        segments(pParent$x, pParent$y, pChild$x, pChild$y, lwd = 2, col = 2)
      }
    }
  }
}