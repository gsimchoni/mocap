#' Plot a single frame of XYZ location
#'
#' This function allows you to...
#' @param xyz desc
#' @param childs desc
#' @param frame desc
#' @param lims desc
#' @param rotateAngle desc
#' @param viewAngle desc
#' @param mainTitle desc
#' @param ... desc
#' @export
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
#' plotMotionSingleFrame(xyz, amc$childs, 1)
plotMotionSingleFrame <- function(xyz, childs, frame, lims,
                                  rotateAngle = 0,
                                  viewAngle = 40,
                                  mainTitle = frame, ...) {
  pos <- as.data.frame(t(sapply(xyz, function(x) x[frame, ]))[, c(1, 3, 2)])
  colnames(pos) <- c("x", "z", "y")
  pos[, 2] <- -pos[, 2]
  if (rotateAngle != 0) {
    pos <- as.data.frame(as.matrix(pos) %*%
                           rotation_matrix(deg2rad(rotateAngle), "z"))
    # TODO Avoid these magic numbers
    lims <- list(limX = c(-200, 200), limY = c(-200, 200), limZ = c(-200, 200))
  }
  s3d <- scatterplot3d::scatterplot3d(pos,
                                      xlim = lims$limX, ylim = lims$limZ,
                                      zlim = lims$limY,
                                      box = FALSE, main = mainTitle,
                                      angle = viewAngle, ...)
  for (parent in names(childs)) {
    for (child in childs[[parent]]) {
      pParent <- s3d$xyz.convert(pos[rownames(pos) == parent, ])
      pChild <- s3d$xyz.convert(pos[rownames(pos) == child, ])
      segments(pParent$x, pParent$y, pChild$x, pChild$y, lwd = 2, col = 2)
    }
  }
}