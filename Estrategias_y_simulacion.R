# Probabilidades reales de ganar en las maquinas
prob_reales <- c(0.3, 0.55, 0.45)
names(prob_reales) <- c("A", "B", "C")

# Estrategia al azar
al_azar <- function(maquina, ganancia) {
  # Elige una maquina al azar
  sample(names(prob_reales), 1)
}


# Greedy con tasa observada
gcto <- function(maquina, ganancia) {
  
  # Las tazas son 0 en la primera iteracion
  tasa <- numeric(3)
  
  
  # Cuando una maquina no se usa ni una vez, la tasa va a seguir siendo 0
  # Cuando se juegue al menos una vez su tasa será el numero de veces que gano en la maquina
  # dividido la cantidad de veces que jugo con la maquina
  if (sum(maquina == "A") > 0) {
    tasa[1] <- sum(ganancia[maquina == "A"])/sum(maquina == "A")
  }
  if (sum(maquina == "B") > 0) {
    tasa[2] <- sum(ganancia[maquina == "B"])/sum(maquina == "B")
  }
  if (sum(maquina == "C") > 0) {
    tasa[3] <- sum(ganancia[maquina == "C"])/sum(maquina == "C")
  }
  
  # Se asigna el nombre de la maquina para cada tasa
  names(tasa) <- c("A","B","C")
  
  # Verifica cual es la tasa mas alta y la elige, si hay una sola elige la maquina a la cual le
  # pertenezca esa tasa, si hay varias maquinas con la misma tasa elige una al azar entre las 
  # que tengan la mayor tasa
  if (sum(tasa == max(tasa)) == 1) {
    maq <- names(tasa[which.max(tasa)])
  } else {
    maq <- sample(names(tasa[tasa == max(tasa)]), size = 1)
  }
  
  return(maq)
}


# Greedy con probabilidad a posteriori

gcpp <- function(maquina, ganancia) {
  
  # Argumentos de las distribuciones de los parámetros
  a1 <- 2; b1 <- 2; c1 <- 2; a2 <- 2; b2 <- 2; c2 <- 2
  
  if (sum(maquina == "A") > 0) {
    a1 <- 2 + sum(ganancia[maquina == "A"])
    a2 <- 2 + abs(sum(ganancia[maquina == "A"] -1))
  }
  if (sum(maquina == "B") > 0) {
    b1 <- 2 + sum(ganancia[maquina == "B"])
    b2 <- 2 + abs(sum(ganancia[maquina == "B"] -1))
  }
  if (sum(maquina == "C") > 0) {
    c1 <- 2 + sum(ganancia[maquina == "C"])
    c2 <- 2 + abs(sum(ganancia[maquina == "C"] -1))
  }
  
  prob <- c(a1/(a1+a2), b1/(b1+b2), c1/(c1+c2))
  names(prob) <- c("A","B","C")
  
  if (sum(prob == max(prob)) == 1) {
    maq <- names(prob[which.max(prob)])
  } else {
    maq <- sample(names(prob[prob == max(prob)]), size = 1)
  }
  
  return(maq)
  
  
  
}


# Lista con las estrategias
estrategias <- list(al_azar = al_azar, gcto = gcto, gcpp = gcpp)


# Simulacion de mil corridas de los 366 dias
simulacion <- function(metodo, n) {
  
  # Creamos los vectores donde guardaremos las ganancias y maquinas de los 366 dias para cada repeticion
  # Creamos tambien una futura lista llamada sim que va a guardar las ganancias y las maquinas usadas en las repeticiones
  ganancias <- NULL
  maquinas <- NULL
  sim <- NULL
  
  # Hacemos n repeticiones
  for (rep in 1:n) {
    
    #Creamos los vectores que guardaran las ganancias y las maquinas usadas en cada dia
    ganancia <- numeric(length = 366)
    maquina <- NULL
    for (dia in 1:366) {
      
      # Para cada dia la maquina sera elegida con alguno de las estrategias diseñadas
      maquina_dia <- estrategias[[metodo]](maquina, ganancia)
      
      # una vez elegida la maquina simulamos con una bernoulli con la probabilidad de la maquina
      # y guardamos el resultado
      ganancia[dia] <- rbinom(1, size = 1, prob = prob_reales[maquina_dia])
      
      # Guardamos ademas la maquina utilizada
      maquina[dia] <- maquina_dia
    }
    
    # Guardamos en matrices las ganancias y las maquinas de los 366 dias de las diferentes repeticiones
    ganancias <- cbind(ganancias, ganancia)
    maquinas <- cbind(maquinas, maquina)
    
  }  
  
  #Transformamos las matrices en tibbles, y les asignamos nombres a las columnas segun el repeticion
  ganancias <- as_tibble(ganancias)
  maquinas <- as_tibble(maquinas)
  
  colnames(ganancias) <- paste0("Rep_",1:1000)
  colnames(maquinas) <- paste0("Rep_",1:1000)
  
  
  # Añadimos los tibbles a la lista
  sim[["Ganancias"]] <- ganancias
  sim[["Maquinas"]] <- maquinas
  
  # Devolvemos la lista
  sim 
  
}