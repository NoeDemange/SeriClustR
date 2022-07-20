#' PhiCluster
#'
#' @param matrice une matrice ou un data.Frame
#' @param R une valeur entre -1 et 1 definissant un seuil de correlation
#' @param N une valeur entiere du nombre minimum d'objets dans un cluster
#'
#' @return un vecteur contenant pour chaque objet la valeur de son cluster
#' @export
#'
#' @useDynLib SeriClustR, .registration = TRUE
PhiCluster <- function(matrice, R = 0, N=NULL){#R = coefficient de correlation
  k <- 1
  Vclus<-vector()
  Vclus <- c(k)

  L <- PhiClustercpp(matrice,R,k)
  k <- L$k
  Vclus <- append(Vclus,L$Vclus)

  T <- table(matrice[nrow(matrice)-1,], matrice[nrow(matrice),])
  Tab2 = T/sum(T)
  a = Tab2[1, 1]
  b = Tab2[1, 2]
  c = Tab2[2, 1]
  d = Tab2[2, 2]
  Phi = (a - (a + b) * (a + c))/sqrt((a + b) * (c + d) * (a + c) * (b + d))
  if (Phi < R)   {
    k <- k+1
  }
  Vclus <- append(Vclus, k)

  ##suppression cluster <N
  if(!is.null(N)){
    for (C in 1 : max(Vclus))   {
      TailleCl <- length(which(Vclus==C))
      if (TailleCl < N)   {
        Vclus[which(Vclus==C)] <- NA
      }
    }
  }

  Vclus
}
