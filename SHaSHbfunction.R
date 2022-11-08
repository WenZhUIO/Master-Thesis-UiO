## Spuriously High Scores person fit indices SHa and SHb  ##

##############sanity check###############

# Sanity.dma(): Sanity check - Data matrix adequacy ----
Sanity.dma <- function(matrix, N, I)
{
  if (!is.numeric(matrix) | (sum(matrix == 0 | matrix == 1, na.rm=TRUE) != (N*I - sum(is.na(matrix)))))
  {
    stop('The data matrix is not numeric with 0/1 entries only. Aborted.')
  }
}

# Sanity.dma.poly(): Sanity check - Data matrix adequacy (polytomous) ----
Sanity.dma.poly <- function(matrix, N, I, M)
{
  if (!is.numeric(matrix) | 
      (sum(matrix %in% (0:M), na.rm=TRUE) != (N*I - sum(is.na(matrix)))) | 
      sum(c(min(matrix, na.rm=TRUE), max(matrix, na.rm=TRUE)) != c(0, M), na.rm=TRUE))
  {
    stop('The data matrix is not numeric with entries {0, 1, ..., Ncat-1} only. Aborted.')
  }
}

# sanity.prv(): Sanity check - Perfect response vectors ----
Sanity.prv <- function(matrix, N, I)
{
  NC         <- rowSums(matrix, na.rm = TRUE)
  NC.withNAs <- NC + rowSums(is.na(matrix))
  all.0s     <- vector(mode = "numeric", length = 0)
  all.1s     <- vector(mode = "numeric", length = 0)
  perfect.vectors <- NULL
  if (min(NC)==0 | max(NC.withNAs)==I)
  {
    perfect.vectors <- noquote("Not all item response vectors were included in the analysis (all-0s and/or all-1s patterns removed).")
    all.0s  <- which(NC == 0)
    all.1s  <- which((NC > 0) & (NC.withNAs == I))
    NC      <- NC[-c(all.0s, all.1s)]
  } else
  {
    perfect.vectors <- noquote("All item response vectors were included in the analysis.")
  }
  # Data matrix without perfect vectors:
  if (length(c(all.0s, all.1s)) == 0)
  {
    matrix.red <- matrix
  } else
  {
    matrix.red <- matrix[(1:N)[-c(all.0s, all.1s)],]
  }
  return(list(perfect.vectors=perfect.vectors, all.0s=all.0s, all.1s=all.1s, NC=NC, matrix.red=matrix.red))
}

# Sanity.IPa(): Sanity check - IP matrix adequacy ----
Sanity.IPa <- function(ip, I)
{
  if (!is.numeric(ip) | dim(ip)[1] != I | dim(ip)[2] != 3)
  {
    stop('The item parameters matrix "IP" is not numeric with dimension I x 3 
           (I = number of items; columns = discrimination, difficulty, pseudo-guessing). 
           Aborted.')
  }
}

# Sanity.IPa.poly(): Sanity check - IP matrix adequacy (polytomous) ----
Sanity.IPa.poly <- function(IP, I, Ncat)
{
  if (!is.numeric(IP) | dim(IP)[1] != I | dim(IP)[2] != Ncat)
  {
    stop('The item parameters matrix "IP" is not numeric with dimension I x Ncat 
           (I = number of items; 
            first (Ncat-1) columns = thresholds [GRM] or difficulties [PCM, GPCM]; 
            column Ncat-th = slopes). 
           Aborted.')
  }
}

# Sanity.ABa(): Sanity check - Ability matrix adequacy ----
Sanity.ABa <- function(Ability, N)
{
  if (!is.numeric(Ability) | length(Ability) != N)
  {
    stop('The person parameters vector "Ability" is not numeric with length N 
           (N = number of respondents). 
           Aborted.')
  }
}

# Sanity.IPm(): Sanity check - IP model ----
Sanity.IPm <- function(model)
{
  if (!(model %in% c("1PL", "2PL", "3PL")))
  {
    stop('Parameter "model" can only be "1PL", "2PL", or "3PL". Aborted.')
  }
}

# Sanity.IPm.poly(): Sanity check - IP model (polytomous) ----
Sanity.IPm.poly <- function(model)
{
  if (!(model %in% c("PCM", "GPCM", "GRM")))
  {
    stop('Parameter "model" can only be "PCM", "GPCM", or "GRM". Aborted.')
  }
}

# Sanity.Abm(): Sanity check - Ability method ----
Sanity.Abm <- function(method)
{
  if (!(method %in% c("ML", "BM", "WL")))
  {
    stop('Parameter "method" can only be "ML", "BM", or "WL". Aborted.')
  }
}

# Sanity.Abm.poly(): Sanity check - Ability method (polytomous) ----
Sanity.Abm.poly <- function(method)
{
  if (!(method %in% c("EB", "EAP", "MI")))
  {
    stop('Parameter "method" can only be "EB", "EAP", or "MI". Aborted.')
  }
}

# Sanity.cls(): Sanity check - Class PerFit ----
Sanity.cls <- function(x)
{
  if (class(x) != "PerFit")
  {
    stop('Object "x" is not of class PerFit. Aborted.')
  }
}

# Sanity.clsPO(): Sanity check - Class PerFit.object ----
Sanity.clsPO <- function(x)
{
  if (class(x) != "PerFit.cutoff")
  {
    stop('Object "cutoff.obj" is not of class PerFit.cutoff. Aborted.')
  }
}


#############Model imputation############

# Parametric model imputation
# Item scores are generated from Bernoulli distributions, with probabilities estimated by means of parametric IRT models
#     (1PLM, 2PLM, 3PLM):
PModel.imputation <- function(matrix, save.matImp, ip, model, ability, method, mu, sigma)
{
  N <- dim(matrix)[1]; I <- dim(matrix)[2]
  
  A   <- ip[, 1]; B <- ip[, 2]; C <- ip[, 3]
  P   <- do.call(cbind, lapply(1:I, function (x) {C[x] + (1 - C[x]) / (1 + exp(-A[x] * (ability - B[x])))}))
  # 
  matrix.imp  <- matrix
  position.NA <- which(is.na(matrix) == 1, arr.ind = TRUE)
  P.NA        <- P[position.NA]
  matrix.imp[position.NA] <- rbinom(length(P.NA), 1, P.NA)
  # 
  if (save.matImp == TRUE)
  {
    write.matrix(matrix.imp, file="Datamatrix_imputted.txt", sep=" ")
  }
  return(list(matrix.imp, ip, ability, 1))
}

# Nonparametric model imputation (dichotomous)
# Similar to the hotdeck imputation, but item scores are generated from Bernoulli distributions, 
#    with probabilities defined by donors with similar total score than the recipient (based on all items except the NAs):
NPModel.imputation <- function(matrix, save.matImp, ip, ability)
{
  N <- dim(matrix)[1]; I <- dim(matrix)[2]
  matrix.imp   <- matrix
  position.NA  <- is.na(matrix)
  recipients   <- which(rowSums(position.NA) > 0)
  N.recipients <- length(recipients)
  donors       <- (1:N)[-recipients]
  N.donors     <- length(donors)
  # 
  vect.NC      <- rowSums(matrix, na.rm = TRUE)
  # 
  for (i in 1:N.recipients)
  {
    rcp       <- recipients[i]
    rcp.noNA  <- (1:I)[!position.NA[rcp, ]]
    rcp.NC    <- vect.NC[rcp]
    donors.NC <- rowSums(matrix[donors, rcp.noNA])
    mar       <- 0
    ctrl      <- 0
    while (ctrl == 0)
    {
      closest.donors <- (abs(donors.NC - rcp.NC) <= mar)
      if (sum(closest.donors) > 0)
      {
        ctrl <- 1
      } else
      {
        mar <- mar+1
      }
    }
    matrix.imp[rcp, position.NA[rcp, ]] <- rbinom(sum(position.NA[rcp, ]), 1, 
                                                  colMeans(matrix[donors[closest.donors], position.NA[rcp, ], drop = FALSE]))
  }
  # 
  if (save.matImp == TRUE)
  {
    write.matrix(matrix.imp, file="Datamatrix_imputted.txt", sep=" ")
  }
  return(list(matrix.imp, ip, ability, 1))
}


##############missing values###############
MissingValues <- function(matrix, NAs, Save.MatImp, IP, ParModel, Ability, Method, mu, sigma)
{
  N <- dim(matrix)[1]; I <- dim(matrix)[2]
  if (sum(is.na(matrix)) > 0)
  {
    lst <- switch(
      NAs,
      Pairwise = list(matrix, IP, Ability, 1),
      Hotdeck = HD.imputation(matrix, Save.MatImp, IP, Ability),
      NPModel = NPModel.imputation(matrix, Save.MatImp, IP, Ability),
      PModel  = {
        # Sanity check - IP model:
        Sanity.IPm(ParModel)
        # Sanity check - Ability method:
        Sanity.Abm(Method)
        # 
        PModel.imputation(matrix, Save.MatImp, 
                          IP, ParModel, Ability, Method, mu, sigma)
      }
    )
  } else
  {
    if (Save.MatImp == TRUE)
    {
      write.matrix(matrix, file="Datamatrix_original.txt", sep=" ")
    }
    lst <- list(matrix, IP, Ability, 0)
  }
  return(lst)
}

# MissingValues.poly(): Dealing with missing values (polytomous) ----
MissingValues.poly <- function(matrix, Ncat, NAs, Save.MatImp, IP, ParModel, Ability, Method)
{
  N <- dim(matrix)[1]; I <- dim(matrix)[2]
  if (sum(is.na(matrix)) > 0)
  {
    lst <- switch(
      NAs,
      Pairwise = list(matrix, IP, Ability, 1),
      Hotdeck = HD.imputation(matrix, Save.MatImp, IP, Ability),
      NPModel = NPModel.imputation.poly(matrix, Ncat, Save.MatImp, IP, Ability),
      PModel  = {
        PModel.imputation.poly(matrix, Ncat, Save.MatImp,IP, ParModel, Ability, Method)
      }
    )
  } else
  {
    if (Save.MatImp == TRUE)
    {
      write.matrix(matrix, file="Datamatrix_original.txt", sep=" ")
    }
    lst <- list(matrix, IP, Ability, 0)
  }
  lst
}


################export results#############
# export.res.P(): Export results (parametric) ----
export.res.P <- function(matrix, N, res, PFStatistic, part.res, Ncat, NAs, 
                         IRT.PModel, IP, Ability.PModel, Ability, IP.NA, Ability.NA, NAs.imp)
{
  res <- list(PFscores  = res.process(matrix, N, res), PFStatistic = PFStatistic, 
              PerfVects = part.res$perfect.vectors, ID.all0s = part.res$all0s, ID.all1s = part.res$all.1s, 
              Matrix = matrix, Ncat=Ncat, 
              IRT.PModel = if(IP.NA) {IRT.PModel} else {NULL}, 
              IP = IP, 
              Ability.PModel = if(Ability.NA) {Ability.PModel} else {NULL}, 
              Ability = Ability, 
              NAs.method = if(NAs.imp) {NAs} else {NULL})
  class(res) <- "PerFit"
  return(res)
}
##############response process############
# res.process(): Process the PFS scores from vector to data frame ----
res.process <- function(matrix, N, res)
{
  res <- data.frame(PFscores = round(res, 4))
  if (is.null(row.names(matrix)))
  {
    row.names(res) <- rep(paste0("Resp.", 1:N))
  } else
  {
    row.names(res) <- row.names(matrix)
  }
  return(res)
}

#########   functions for indices #########
####################SHa(1/2)####################################
SHa12 <- function(matrix, 
                  NA.method="Pairwise", Save.MatImp=FALSE, 
                  IP=NULL, IRT.PModel="2PL", Ability=NULL, Ability.PModel="ML", mu=0, sigma=1)
{
  matrix      <- as.matrix(matrix)
  N           <- dim(matrix)[1]; I <- dim(matrix)[2]
  IP.NA       <- is.null(IP); Ability.NA  <- is.null(Ability)
  # Sanity check - Data matrix adequacy:
  Sanity.dma(matrix, N, I)
  # Dealing with missing values:
  res.NA <- MissingValues(matrix, NA.method, Save.MatImp, IP, IRT.PModel, Ability, Ability.PModel, mu, sigma)
  matrix <- res.NA[[1]]
  # Perfect response vectors allowed.
  # Compute PFS: 
  A   <- IP[, 1]; B <- IP[, 2]; C <- IP[, 3]
  P   <- do.call(cbind, lapply(1:I, function (x) {C[x] + (1 - C[x]) / (1 + exp(-A[x] * (Ability - B[x])))}))
  Q   <- 1-P
  d1P <- do.call(cbind, lapply(1:I, function (x){
    (1 - C[x]) * A[x] * exp(A[x] * (Ability - B[x])) / (1 + exp(A[x] * (Ability - B[x])))^2}))
  d2P <- do.call(cbind, lapply(1:I, function (x){
    (1 - C[x]) * (A[x]^2) * exp(A[x] * (Ability - B[x])) * (1 - exp(A[x] * (Ability - B[x]))) / (1 + exp(A[x] * (Ability - B[x])))^3}))
  ri  <- d1P / (P * Q)
  r0  <- switch(Ability.PModel,
                ML = 0,
                BM = (mu - Ability) / (sigma^2),
                WL = rowSums((d1P * d2P) / (P * Q)) / (2 * rowSums((d1P^2) / (P * Q))))
  wi       <- (1-2 * P)/sqrt(P)   ###change the weight
  Wn       <- rowSums((matrix - P)*wi, na.rm = TRUE)
  sigma2n  <- rowSums((wi^2) * P * Q) / I
  cn       <- rowSums(d1P * wi) / rowSums(d1P * ri)
  wi.tilde <- wi - matrix(rep(cn, I), nrow = N) * ri
  tau2n    <- rowSums((wi.tilde^2) * P * Q) / I
  EWn      <- -cn * r0
  VWn      <- I * tau2n
  res      <- as.vector(round((Wn - EWn) / sqrt(VWn), 4))
  # Export results:
  export.res.P(matrix, N, res, "SHa(1/2)", vector("list", 5) , Ncat=2, NA.method, 
               IRT.PModel, res.NA[[2]], Ability.PModel, res.NA[[3]], IP.NA, Ability.NA, res.NA[[4]])
}



###################SHb(3)#####################
SHb3 <- function(matrix, 
                 NA.method="Pairwise", Save.MatImp=FALSE, 
                 IP=NULL, IRT.PModel="2PL", Ability=NULL, Ability.PModel="ML", mu=0, sigma=1)
{
  matrix      <- as.matrix(matrix)
  N           <- dim(matrix)[1]; I <- dim(matrix)[2]
  IP.NA       <- is.null(IP); Ability.NA  <- is.null(Ability)
  # Sanity check - Data matrix adequacy:
  Sanity.dma(matrix, N, I)
  # Dealing with missing values:
  res.NA <- MissingValues(matrix, NA.method, Save.MatImp, IP, IRT.PModel, Ability, Ability.PModel, mu, sigma)
  matrix <- res.NA[[1]]
  # Perfect response vectors allowed.
  # Compute PFS: 
  A   <- IP[, 1]; B <- IP[, 2]; C <- IP[, 3]
  P   <- do.call(cbind, lapply(1:I, function (x) {C[x] + (1 - C[x]) / (1 + exp(-A[x] * (Ability - B[x])))}))
  Q   <- 1-P
  d1P <- do.call(cbind, lapply(1:I, function (x){
    (1 - C[x]) * A[x] * exp(A[x] * (Ability - B[x])) / (1 + exp(A[x] * (Ability - B[x])))^2}))
  d2P <- do.call(cbind, lapply(1:I, function (x){
    (1 - C[x]) * (A[x]^2) * exp(A[x] * (Ability - B[x])) * (1 - exp(A[x] * (Ability - B[x]))) / (1 + exp(A[x] * (Ability - B[x])))^3}))
  ri  <- d1P / (P * Q)
  r0  <- switch(Ability.PModel,
                ML = 0,
                BM = (mu - Ability) / (sigma^2),
                WL = rowSums((d1P * d2P) / (P * Q)) / (2 * rowSums((d1P^2) / (P * Q))))
  wi       <- (1-2* P)*exp(-(3*P)^2)   ###change the weight
  Wn       <- rowSums((matrix - P)*wi, na.rm = TRUE)
  sigma2n  <- rowSums((wi^2) * P * Q) / I
  cn       <- rowSums(d1P * wi) / rowSums(d1P * ri)
  wi.tilde <- wi - matrix(rep(cn, I), nrow = N) * ri
  tau2n    <- rowSums((wi.tilde^2) * P * Q) / I
  EWn      <- -cn * r0
  VWn      <- I * tau2n
  res      <- as.vector(round((Wn - EWn) / sqrt(VWn), 4))
  # Export results:
  export.res.P(matrix, N, res, "SHa(1/2)", vector("list", 5) , Ncat=2, NA.method, 
               IRT.PModel, res.NA[[2]], Ability.PModel, res.NA[[3]], IP.NA, Ability.NA, res.NA[[4]])
}


#############   END   #############

