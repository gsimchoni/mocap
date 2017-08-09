# function to read a single bone
readBone <- function(idx, lines) {
  idxBegin <- idx
  while (all(strsplit(lines[idxBegin], "\\s+")[[1]] != "begin")) {
    idxBegin <- idxBegin + 1
  }
  idxEnd <- idxBegin
  while (all(strsplit(lines[idxEnd], "\\s+")[[1]] != "end")) {
    idxEnd <- idxEnd + 1
  }
  boneString <- paste(lines[idxBegin:idxEnd], collapse = " ")
  boneName <- strsplit(
    regmatches(boneString,
               regexpr("name [a-z]+", boneString)), "\\s+")[[1]][2]
  boneDirections <- as.numeric(
    strsplit(regmatches(boneString,
                        regexpr("direction (-?[0-9.]+e?[-+]?[0-9]*[ ]?){3}",
                                boneString)), "\\s+")[[1]][2:4])
  boneLength <- as.numeric(
    strsplit(regmatches(boneString,
                        regexpr("length [0-9\\.]+",
                                boneString)), "\\s+")[[1]][2])
  boneAngles <- as.numeric(
    strsplit(regmatches(boneString,
                        regexpr("axis (-?[0-9.]+e?[-+]?[0-9]*[ ]?){3}",
                                boneString)), "\\s+")[[1]][2:4])
  boneDOFX <- ifelse(grepl("rx", boneString), 1, 0)
  boneDOFY <- ifelse(grepl("ry", boneString), 1, 0)
  boneDOFZ <- ifelse(grepl("rz", boneString), 1, 0)
  
  bone <- list(boneName,
               boneDirections[1], boneDirections[2], boneDirections[3],
               boneLength,
               boneAngles[1], boneAngles[2], boneAngles[3],
               boneDOFX, boneDOFY, boneDOFZ)
  return(list(idx = idxEnd, bone = bone))
}

# function to convert amc file to matrix
# TODO Avoid these magic numbers, they probably only apply to CMU files
# CREDIT: [Jernej Barbic](http://www-bcf.usc.edu/~jbarbic/)
# http://graphics.cs.cmu.edu/software/amc_to_matrix.m
amc2Matrix <- function(fname, verbose = FALSE) {
  lines <- readLines(fname)
  
  # read-in header
  idx <- 2
  while (lines[idx - 1] != ":DEGREES") idx <- idx + 1
  
  D <- numeric()
  dims <- c(6, 3, 3, 3, 3, 3, 3, 2, 3, 1, 1, 2, 1, 2, 2, 3, 1, 1, 2, 1, 2, 3,
            1, 2, 1, 3, 1, 2, 1)
  locations <- c(1, 7, 10, 13, 16, 19, 22, 25, 27, 30, 31, 32, 34, 35, 37, 39,
                 42, 43, 44, 46, 47, 49, 52, 53, 55, 56, 59, 60, 62)
  segments <- c('root', 'lowerback', 'upperback', 'thorax', 'lowerneck',
                'upperneck', 'head', 'rclavicle', 'rhumerus', 'rradius',
                'rwrist', 'rhand', 'rfingers', 'rthumb', 'lclavicle',
                'lhumerus', 'lradius', 'lwrist', 'lhand', 'lfingers', 'lthumb',
                'rfemur', 'rtibia', 'rfoot', 'rtoes', 'lfemur', 'ltibia',
                'lfoot', 'ltoes')
  
  getSegemtnID <- function(idx) strsplit(lines[idx], " ")[[1]][1]
  
  # read-in data
  # labels can be in any order
  frame <- 1
  while (idx <= length(lines)) {
    if (verbose & frame %% 100 == 0) cat("Reading frame: ", frame, "\n")
    
    row <- rep(0, 62)
    
    # read angle label
    idx <- idx + 1
    frameSegments <- sapply(idx:(idx + 28), getSegemtnID)
    index <- match(segments, frameSegments)
    
    # where to put the data
    location <- locations[index]
    len <- dims[index]
    
    for (i in 1:29) {
      row[location[i]:(location[i] + len[i] - 1)] <-
        as.numeric(strsplit(lines[idx], " ")[[1]][2:(len[i] + 1)])
      idx <- idx + 1
    }
    
    D <- cbind(D, row)
    
    frame <- frame + 1
  }
  if (verbose) cat("Total number of frames read: ", frame - 1, "\n")
  return(list(D = D, bones = segments))
}

# function to convert degrees to radians
deg2rad <- function(deg) deg * pi / 180

# function to get the position and initial root matrix
getRootMatrix <- function(frame, D, angleType, CMatList) {
  root_pos <- D[1:3, frame]
  root_angle <- D[4:6, frame]
  root_matrix <- getLRotationMatrix("root",
                                    root_angle[1], root_angle[2], root_angle[3],
                                    angleType, CMatList)
  return(list(root_pos = root_pos, root_matrix = root_matrix))
}

# function to get a rotation matrix around a single axis
rotation_matrix <- function(angle, axis) {
  if (axis == "x") {
    matrix(c(1, 0, 0,
             0, cos(angle), -sin(angle),
             0, sin(angle), cos(angle)),
           nrow = 3, byrow = TRUE)
  } else if (axis == "y") {
    matrix(c(cos(angle), 0, sin(angle),
             0, 1, 0,
             -sin(angle), 0, cos(angle)),
           nrow = 3, byrow = TRUE)
  } else if (axis == "z") {
    matrix(c(cos(angle), -sin(angle), 0,
             sin(angle), cos(angle), 0,
             0, 0, 1),
           nrow = 3, byrow = TRUE)
  } else {
    stop("unknown axis parameter, only x, y, z allowed.")
  }
}

# function to get L rotation matrix
getLRotationMatrix <- function(bone, ax, ay, az, angleType, CMatList) {
  ax <- ifelse(angleType == "deg", deg2rad(ax), ax)
  ay <- ifelse(angleType == "deg", deg2rad(ay), ay)
  az <- ifelse(angleType == "deg", deg2rad(az), az)
  
  Mx <- t(rotation_matrix(ax, "x"))
  My <- t(rotation_matrix(ay, "y"))
  Mz <- t(rotation_matrix(az, "z"))
  
  M <- Mx %*% My %*% Mz
  L <- CMatList[[bone]]$Cinv %*% M %*% CMatList[[bone]]$C
  
  return(L)
}

# function to get initial C and Cinv Angle-Axis matrices
getCAxisAnglrMatrices <- function(C_values, angleType) {
  ax <- ifelse(angleType == "deg", deg2rad(C_values[1]), C_values[1])
  ay <- ifelse(angleType == "deg", deg2rad(C_values[2]), C_values[2])
  az <- ifelse(angleType == "deg", deg2rad(C_values[3]), C_values[3])
  
  Cx <- t(rotation_matrix(ax, "x"))
  Cy <- t(rotation_matrix(ay, "y"))
  Cz <- t(rotation_matrix(az, "z"))
  
  C <- Cx %*% Cy %*% Cz
  Cinv <- solve(C)
  
  return(list(C = C, Cinv = Cinv))
}

# function to get L rotation matrix at a frame
getBoneMatrix <- function(bone, frame, degrees, angleType, CMatList) {
  bone_rotation <- degrees[[bone]][frame, ]
  L <- getLRotationMatrix(bone,
                          bone_rotation[1], bone_rotation[2], bone_rotation[3],
                          angleType, CMatList)
  return(L)
}

# traverse a skeleton calling DFS for every frame
traverseSkeleton <- function(bone, asf, amc, xyz, nStartFrame, nEndFrame,
                             bonesVector, verbose = FALSE) {
  stack <- list()
  for (frame in nStartFrame:nEndFrame) {
    if (verbose & frame %% 100 == 0) cat("frame: ", frame, "\n")
    xyz <- DFS("root", asf, amc, xyz, bonesVector, frame, stack = stack)
  }
  return(xyz)
}

# Depth First Search with a stack at a frame
DFS <- function(bone, asf, amc, xyz, bonesVector, frame, parent = NULL,
                stack = NULL) {
  if (bone == "root") {
    root_mat_res <- getRootMatrix(frame, amc$D, asf$angleType, asf$CMatList)
    pos <- root_mat_res$root_pos
    mat <- root_mat_res$root_matrix
    xyz[[bone]][frame, ] <- pos
    stack <- c(stack, list(mat))
  } else {
    stack <- c(stack, list(
      getBoneMatrix(bone, frame, amc$degrees, asf$angleType, asf$CMatList) %*%
        stack[[length(stack)]]))
    xyz[[bone]][frame, ] <- xyz[[parent]][frame, ] +
      as.numeric(bonesVector[[bone]]) %*%
      stack[[length(stack)]]
  }
  
  for (b in asf$childs[[bone]]) {
    xyz <- DFS(b, asf, amc, xyz, bonesVector, frame, parent = bone,
               stack = stack)
  }
  stack <- stack[-length(stack)]
  return(xyz)
}