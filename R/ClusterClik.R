
#' ClusterClik
#'
#' @param matrice une matrice ou un data.Frame
#' @param R une valeur entre 0 et 1 definissant un seuil de correlation
#' @param N une valeur entiere du nombre minimum d'objets dans un cluster
#' @param dist_method voir 'method' de la fonction dist.binary package ade4
#'
#' @return un vecteur contenant pour chaque objet la valeur de son cluster
#' @export
#'
#' @useDynLib SeriClustR, .registration = TRUE
#' @importFrom ade4 dist.binary
ClusterClik <- function(matrice, R = 0.8, N = NULL, dist_method = 9){

  DRmat <- as.matrix(ade4::dist.binary(matrice, method = dist_method))
  Rmat <- 1 - DRmat^2
  ##correlation moyen
  VphiClikClus <- ClusterClikcpp(Rmat, R)

  ## suppression des clusters de taille < N
  VphiClikClus2 <- VphiClikClus
  if(!is.null(N)){
    for (C in 1 : max(VphiClikClus))   {
      TailleCl <- length(which(VphiClikClus==C))
      if (TailleCl < N)   {
        VphiClikClus2[which(VphiClikClus2==C)] <- NA
      }
    }
  }

  VphiClikClus2
}

