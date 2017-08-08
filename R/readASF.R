#' Read and Parse a AFS file
#'
#' This function reads in and parses a AFS Motion Capture file and returns
#' several useful objects
#' @param asfFilePath path to a .asf file
#' @param in2cm a boolean indicating whether to convert inches to centimeters,
#' defaults to TRUE
#' 
#' @return a list containing:
#' \item{skeleton}{a data frame containing the details for each bone, e.g. DOF,
#' length, direction, angle - DO NOT USE, use the one returned from \code{readAMC}}
#' \item{childs}{a named list containing a vector of "children" bones for each
#' "parent" bone}
#' \item{CMatList}{a named list with the C and Cinv angle-axis matrices for each bone}
#' \item{len}{the length parameter in the ASF file, scaling factor all lengths,
#' in CMU files always 0.45}
#' \item{angleType}{the angle parameter in the ASF file, "deg" or "rad",
#' in CMU files always "deg"}
#' 
#' @note 
#' Only ASF/AMC files from the CMU Graphics Lab Motion Capture Database have been tested.
#' 
#' @references
#' \url{http://giorasimchoni.com/}
#' \url{http://mocap.cs.cmu.edu/}
#' 
#' @examples
#' asfFilePath <- system.file("extdata", "lambada.asf", package = "mocap")
#' asf <- readASF(asfFilePath)
#' 
#' @export
readASF <- function(asfFilePath, in2cm = TRUE) {
  lines <- readLines(asfFilePath)
  
  skeleton <- data.frame(bone = character(0),
                         dirx = numeric(0), diry = numeric(0), dirz = numeric(0),
                         length = numeric(0),
                         angx = numeric(0), angy = numeric(0), angz = numeric(0),
                         dofx = numeric(0), dofy = numeric(0), dofz = numeric(0),
                         stringsAsFactors = FALSE)
  
  # read in length
  idx <- 1
  while (all(strsplit(lines[idx], "\\s+")[[1]] != "length")) idx <- idx + 1
  lengthLoc <- which(strsplit(lines[idx], "\\s+")[[1]] == "length") + 1
  len <- as.numeric(strsplit(lines[idx], "\\s+")[[1]][lengthLoc])
  
  # read in angle type
  idx <- idx + 1
  while (all(strsplit(lines[idx], "\\s+")[[1]] != "angle")) idx <- idx + 1
  angleTypeLoc <- which(strsplit(lines[idx], "\\s+")[[1]] == "angle") + 1
  angleType <- strsplit(lines[idx], "\\s+")[[1]][angleTypeLoc]
  
  # read in root
  idx <- idx + 1
  while (all(strsplit(lines[idx], "\\s+")[[1]] != "position")) idx <- idx + 1
  positionLoc <- which(strsplit(lines[idx], "\\s+")[[1]] == "position") + 1
  rootPosition <- as.numeric(
    strsplit(lines[idx], "\\s+")[[1]][positionLoc:(positionLoc + 2)])
  
  idx <- idx + 1
  while (all(strsplit(lines[idx], "\\s+")[[1]] != "orientation")) idx <- idx + 1
  orientLoc <- which(strsplit(lines[idx], "\\s+")[[1]] == "orientation") + 1
  rootOrientation <- as.numeric(
    strsplit(lines[idx], "\\s+")[[1]][orientLoc:(orientLoc + 2)])
  
  skeleton[nrow(skeleton) + 1, ] <- c("root",
                                      rootPosition[1], rootPosition[2],
                                      rootPosition[3],
                                      0,
                                      rootOrientation[1], rootOrientation[2],
                                      rootOrientation[3],
                                      1, 1, 1)
  
  # read in bones
  while (lines[idx] != ":hierarchy") {
    resBone <- readBone(idx, lines)
    idx <- resBone$idx + 1
    skeleton[nrow(skeleton) + 1, ] <- resBone$bone
  }
  
  # read in hierarchy
  childs <- rep(list(NULL), nrow(skeleton))
  names(childs) <- skeleton$bone
  skeleton$child <- 1:nrow(skeleton)
  skeleton$parent <- rep(0, nrow(skeleton))
  idx <- idx + 2
  while(trimws(lines[idx]) != "end") {
    parentChildren <- strsplit(trimws(lines[idx]), " ")[[1]]
    bone <- parentChildren[1]
    for (child in 2:length(parentChildren)) {
      childs[[bone]] <- c(childs[[bone]], parentChildren[child])
      skeleton[skeleton$bone == parentChildren[child], "parent"] <-
        skeleton[skeleton$bone == bone, "child"]
    }
    idx <- idx + 1
  }
  
  # make sure skeleton valus are numeric
  skeleton[, 2:ncol(skeleton)] <-
    apply(skeleton[, 2:ncol(skeleton)], 2, as.numeric)
  
  # get bones final vector: length x direction
  skeleton$x <- skeleton$length * skeleton$dirx * ifelse(in2cm, 2.54, 1) / len
  skeleton$y <- skeleton$length * skeleton$diry * ifelse(in2cm, 2.54, 1) / len
  skeleton$z <- skeleton$length * skeleton$dirz * ifelse(in2cm, 2.54, 1) / len
  
  # get C and CInv matrices
  CMatList <- apply(skeleton[, c("angx", "angy", "angz")], 1,
                    function(x) getCAxisAnglrMatrices(x, angleType))
  names(CMatList) <- skeleton$bone
  
  return(list(skeleton = skeleton,
              childs = childs,
              CMatList = CMatList,
              len = len,
              angleType = angleType))
}