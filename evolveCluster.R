################################################################################
################################################################################
######################ALGORITMO EVOLVECLUSTER###################################
################################################################################
################################################################################
######################FUNCIONES IMPORTANTES#####################################
cargaDatos <- function(distancia, stroke, round, gender, heat, splitdistance){
  # 1º Cargo los datos. 
  load( "DATOS/2024/WORLDS2024BUDAPEST25mINTERFAZ.RData")
  prueba <- worlds2024Budapest25m[
    worlds2024Budapest25m$distance == distancia & 
      worlds2024Budapest25m$stroke == stroke & 
      worlds2024Budapest25m$round == round & 
      worlds2024Budapest25m$gender == gender & 
      worlds2024Budapest25m$heat == heat &
      worlds2024Budapest25m$splitdistance %% splitdistance == 0, 
  ]
  prueba$cluster <- NA
  prueba$cumswimtime<- as.numeric(prueba$cumswimtime)
  prueba <- prueba %>% arrange(splitdistance, cumswimtime)
  prueba <- prueba %>%
    group_by(splitdistance) %>%
    mutate(diferenciaLider = cumswimtime[1]- cumswimtime) %>%
    ungroup()  # Deshacer el agrupamiento después de calcular la diferencia
  
  prueba$diferenciaLider <- abs(prueba$diferenciaLider)
  return(prueba)
}

calcularGap <- function(x){
  diferencias <- x[-1] - x[-length(x)]
  return(diferencias)
}

clustering_manual <- function(observaciones, centroides_iniciales, max_iter = 100, tol = 1e-6) {
  x <- as.numeric(observaciones)
  centroides <- as.numeric(centroides_iniciales)
  K <- length(centroides)
  n <- length(x)
  asignaciones <- rep(NA_integer_, n)
  
  for (iter in 1:max_iter) {
    # Paso 1: Asignar cada punto al centroide más cercano
    distancias <- sapply(centroides, function(c) abs(x - c))
    nuevas_asignaciones <- apply(distancias, 1, which.min)
    
    # Paso 2: Calcular nuevos centroides
    nuevos_centroides <- sapply(1:K, function(k) {
      puntos_k <- x[nuevas_asignaciones == k]
      if (length(puntos_k) == 0) {
        NA  # cluster vacío
      } else {
        mean(puntos_k)
      }
    })
    
    # Parar si las asignaciones no cambian y los centroides convergen
    if (all(nuevas_asignaciones == asignaciones, na.rm = TRUE) &&
        all(abs(nuevos_centroides - centroides) < tol, na.rm = TRUE)) {
      break
    }
    
    # Actualizar
    asignaciones <- nuevas_asignaciones
    centroides <- nuevos_centroides
  }
  
  # Eliminar clusters vacíos
  clusters_validos <- which(!is.na(centroides))
  centroides_finales <- centroides[clusters_validos]
  asignaciones_filtradas <- match(asignaciones, clusters_validos)
  
  # Reordenar los clusters por el valor del centroide
  orden <- order(centroides_finales)
  centroides_finales <- centroides_finales[orden]
  asignaciones_reordenadas <- match(asignaciones_filtradas, orden)
  
  return(list(
    clusters = asignaciones_reordenadas,
    centroides = centroides_finales
  ))
}

refinarCluster <- function(observaciones, centroides, clustersAsignados, umbral_division){
  #INTERPRETACIÓN EN CASO DE QUE SÓLO SE PUEDA PARTIR EN 2 UN CLUSTER.
  num_centroides <- length(centroides)
  clusterEvaluado <- 1
  while (clusterEvaluado <= num_centroides){
    #Guardo en una variable todas las observaciones de dicho cluster:
    x <- which(clustersAsignados == clusterEvaluado)
    if (length(x) == 1){
      #No se puede separar. Paso al siguiente cluster para evaluarlo.
      clusterEvaluado <- clusterEvaluado+1
    }else{
      #Calculo la diferencia entre observaciones:
      elementosCluster <- observaciones[x]
      gap_entre_observaciones <- calcularGap(elementosCluster)
      #Me quedo con el índice de la máxima diferencia:
      indice <- which.max(gap_entre_observaciones)
      if (gap_entre_observaciones[indice] >= umbral_division){
        # Separo el cluster
        cluster1 <- elementosCluster[1:indice]
        cluster2 <- elementosCluster[(indice+1):length(elementosCluster)]
        # Calculo los nuevos centroides
        centroide1 <- mean(cluster1)
        centroide2 <- mean(cluster2)
        # Vuelco los nuevos centroides:
        centroides[clusterEvaluado]<-centroide1
        centroides <- append(centroides, centroide2, after = clusterEvaluado)
        # A los clusters siguientes al evaluado, les sumo 1:
        clustersAsignados[clustersAsignados> clusterEvaluado] <- clustersAsignados[clustersAsignados > clusterEvaluado] + 1
        # Ahora asigno los nuevos clusters:
        indicesAsignados <- which(clustersAsignados == clusterEvaluado)
        # Encuentra el índice del corte dentro del vector total.
        indiceSeparacion <- indicesAsignados[indice]
        # Sumar 1 a todos los elementos tras el corte:
        clustersAsignados[indicesAsignados[indicesAsignados > indiceSeparacion]] <- clustersAsignados[indicesAsignados[indicesAsignados > indiceSeparacion]] + 1
        # Actualizo los datos: 
        num_centroides <- num_centroides + 1
        clusterEvaluado <- clusterEvaluado + 2
        
      }else{
        #En este caso no hay motivos para dividir. 
        clusterEvaluado <- clusterEvaluado + 1
      }
      
    }
    
    
  }
  return(list(
    clusters = clustersAsignados,
    centroides = centroides
  ))
  
}


################################################################################
##############################ALGORITMO#########################################
################################################################################
evolveCluster <- function (distancia, stroke, round, gender, heat, split, umbral_division, k_inicial){
  #1º Cargo los datos en el df correspondiente.
  prueba <- cargaDatos(distancia, stroke, round, gender, heat, split)
  
  
  #2º Creo el dataframe de resultados. 
  resultados <- prueba[0,]
  
  
  #3º Realizo el primer cluster:
  df <- prueba %>% filter(splitdistance == split)
  cluster_inicial <- kmeans(df$diferenciaLider, centers = k_inicial, nstart = 25)
  
  
  #4º Compruebo si está ordenado:
  if (is.unsorted(cluster_inicial$centers[,1])){
    #Los ordeno de menor a mayor:
    orden <- order(cluster_inicial$centers[,1])
    #Los guardo en el correspondiente df:
    centroides <- cluster_inicial$centers[orden]
    # Reasignar etiquetas de cluster según el orden de los centros.
    df$cluster <- match(cluster_inicial$cluster, orden)
    
  }else{
    #Guardo los centroides:
    centroides <- cluster_inicial$centers[,1]
    #Asigno los clusters al parcial:
    df$cluster<-cluster_inicial$cluster
  }
  resultados <- rbind(df, resultados)
  
  
  #4º Inicio el EvolveCluster.
  inicio <- split + split
  for (i in seq(inicio, distancia, by = split)) {
    # Creo el dataframe necesario:
    df <- prueba %>% filter(splitdistance == i)
    # Realizo el cluster usando los centroides previos:
    clusterParcial <- clustering_manual(df$diferenciaLider, centroides)
    #Refino:
    clusterParcial <- refinarCluster(df$diferenciaLider, clusterParcial$centroides, clusterParcial$clusters, umbral_division)
    #Guardo los resultados y inicializo los centroides del siguiente cluster:
    df$cluster <- clusterParcial$clusters
    centroides <- clusterParcial$centroides
    resultados <- rbind(df, resultados)
    
  }
  return(resultados)
  
}

evolveCluster2 <- function (distancia, stroke, round, gender, heat, split, umbral_division, k_inicial){
  #1º Cargo los datos en el df correspondiente.
  prueba <- cargaDatos(distancia, stroke, round, gender, heat, split)
  
  
  #2º Creo el dataframe de resultados. 
  resultados <- prueba[0,]
  
  
  #3º Realizo el primer cluster:
  df <- prueba %>% filter(splitdistance == split)
  cluster_inicial <- kmeans(df$diferenciaLider, centers = k_inicial, nstart = 25)
  
  
  #4º Compruebo si está ordenado:
  if (is.unsorted(cluster_inicial$centers[,1])){
    #Los ordeno de menor a mayor:
    orden <- order(cluster_inicial$centers[,1])
    #Los guardo en el correspondiente df:
    centroides <- cluster_inicial$centers[orden]
    # Reasignar etiquetas de cluster según el orden de los centros.
    df$cluster <- match(cluster_inicial$cluster, orden)
    
  }else{
    #Guardo los centroides:
    centroides <- cluster_inicial$centers[,1]
    #Asigno los clusters al parcial:
    df$cluster<-cluster_inicial$cluster
  }
  resultados <- rbind(df, resultados)
  
  
  #4º Inicio el EvolveCluster.
  inicio <- split + split
  for (i in seq(inicio, distancia, by = split)) {
    # Creo el dataframe necesario:
    df <- prueba %>% filter(splitdistance == i)
    # Realizo el cluster usando los centroides previos:
    clusterParcial <- clustering_manual(df$diferenciaLider, centroides)
    #Refino:
    paso <- i/split
    clusterParcial <- refinarCluster(df$diferenciaLider, clusterParcial$centroides, clusterParcial$clusters, umbral_division[paso])
    #Guardo los resultados y inicializo los centroides del siguiente cluster:
    df$cluster <- clusterParcial$clusters
    centroides <- clusterParcial$centroides
    resultados <- rbind(df, resultados)
    
  }
  return(resultados)
  
}
