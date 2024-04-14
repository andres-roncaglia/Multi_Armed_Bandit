# Probabilidades reales de ganar en las maquinas -------------
prob_reales <- c(0.3, 0.55, 0.45)
names(prob_reales) <- c("A", "B", "C")

# Estrategia al azar -------------
al_azar <- function() {
  # Elige una maquina al azar
  sample(names(prob_reales), 1)
}

 
# Greedy con tasa observada -------------
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


# Greedy con probabilidad a posteriori -------------

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



# e-Greedy con tasa observada ----------------

e_greedy <- function(maquina, ganancia, param) {
  # Las tazas son 0 en la primera iteracion
  tasa <- numeric(3)
  
  # Cuando una maquina no se usa ni una vez, la tasa va a seguir siendo 0
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
  # Se selecciona entre explorar o explotar
  aleatorio <- runif(1)
  
  probs <- c(0,0,0)
  if (aleatorio <= param) {return(sample(names(tasa),size = 1))}else{
    for (i in 1:3) {
      if((tasa == max(tasa))[i]){probs[i] <- 1/sum(tasa == max(tasa))}
    }
    return(sample(names(tasa),size = 1,prob = probs))
  }
}

# Softmax -------------

softmax <- function(maquina, ganancia, param) {
  
  # Las tasas son 0 en la primera iteracion
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
  
  # Calculo las probabilidades en base a las tasas
  # Como tasa es un vector puedo operar como muestro abajo
  probs <- exp(tasa/param)/sum(exp(tasa/param))
  
  # Directamente pongo el Sample en el return para eficiencia
  
  return(sample(names(tasa),size = 1,prob = probs))
}

  # El valor de temperatura mientras más chico es, más se casa con la maquina que mas gano.
  # Si el valor de temperatura es grande, le da menos peso a la tasa ganadora de la maquina.

  # Preguntar por el "intervalo de credibilidad Bayesiano"!!!!

# Upper-Bound -------------

UB <- function(maquina, ganancia, param) {
  
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
  
  # Limite superior de 95% credibilidad (Preguntar el miercoles 10/04)
  ls <- c(qbeta(1 - param/2, a1, a2), qbeta(1 - param/2, b1, b2), qbeta(1 - param/2, c1, c2))
  names(ls) <- c("A","B","C")
  
  # Para elegir el limite superior mayor
  if (sum(ls == max(ls)) == 1) {
    maq <- names(which.max(ls))
    
  } else if (sum(ls == max(ls)) == 2) {
    maq <- sample(names(ls)[ls == max(ls)], size = 1)
    
  } else {
    maq <- sample(names(ls), size = 1)
    
  }
  
  return(maq)
}

# Thompson -------------

thompson <- function(maquina, ganancia) {
  
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
  
  
  # Tomo una muestra de cada theta
  muestras <- c(rbeta(1, a1, a2),rbeta(1, b1, b2), rbeta(1, c1, c2))
  # Se podria agregar un parámetro para modificar el tamaño de las muestras y asi ponderar mas a los que tienen
  # parametros mayores.
  
  names(muestras) <- c("A","B","C")
  # Devuelvo la más grande
  
  return(names(muestras[which.max(muestras)]))
  
}




# Lista con las estrategias
estrategias <- list(al_azar = al_azar, 
                    gcto = gcto, 
                    gcpp = gcpp, 
                    e_greedy = e_greedy, 
                    softmax = softmax,
                    upper_bound = UB,
                    thompson = thompson)


# n simulaciones de los 366 dias, jugando 1 vez por día
simulacion <- function(metodo, n = 1, param = 0.2) {
  
  if(!(metodo %in% names(estrategias))){
    stop("El método introducido no está especificado")
  }
  if(n<1 || (n != round(n))){
    stop("La cantidad de simulaciones debe ser un número natural")
  }
  if(metodo == "softmax" && param <= 0){
    stop("Para el método softmax el parámetro de temperatura debe ser mayor que 0")
  }
  if(metodo == "e_greedy" && (param < 0 || param > 1)){
    stop("Para el método e-greedy el parámetro epsilon debe estar entre 0 y 1")
  }
  
  # Creamos los vectores donde guardaremos las ganancias y maquinas de los 366 dias para cada repeticion
  # Creamos tambien una futura lista llamada sim que va a guardar las ganancias y las maquinas usadas en las repeticiones
  ganancias <- NULL
  maquinas <- NULL
  sim <- NULL
  
  #Función en base a la estrategia elegida
  estrat <- estrategias[[metodo]]
  
  # Hacemos n repeticiones
  for (rep in 1:n) {
      
    #Creamos los vectores que guardaran las ganancias y las maquinas usadas en cada dia
    ganancia <- numeric(length = 366)
    maquina <- NULL
    for (dia in 1:366) {
      
      # Para cada dia la maquina sera elegida con alguno de las estrategias diseñadas
      # maquina_dia <- estrategias[[metodo]](maquina, ganancia, param)
      
      # Intento de limpiar el codigo
      argumentos <- switch (metodo,
                            "al_azar" = list(),
                            "gcto" = list(maquina, ganancia),
                            "gcpp" = list(maquina, ganancia),
                            "e_greedy" = list(maquina, ganancia, param),
                            "softmax" = list(maquina, ganancia, param),
                            "thompson" = list(maquina, ganancia),
                            "upper_bound" = list(maquina, ganancia, param)
                            )
      
      maquina_dia <- do.call(estrat, argumentos)
      
      
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




salidas <- function(sim) {
  
  df <- data.frame(ganancias = sim$Ganancias,
                   
                   maquinas = sim$Maquinas) |>
    
    rename(Ganancias = Rep_1, Maquinas = Rep_1.1)
  
  medidas <- df |>
    
    group_by(Maquinas) |>
    
    summarise(Tiradas = n(),
              
              Exitos = sum(Ganancias))
  
  if(!("A"%in%medidas$Maquinas)){
    
    medidas <- rbind(
      
      tibble(Maquinas = "A", Tiradas = 0, Exitos = 0),
      
      medidas
      
    )
    
  }
  
  if(!("C"%in%medidas$Maquinas)){
    
    medidas <- rbind(
      
      medidas,
      
      tibble(Maquinas = "C", Tiradas = 0, Exitos = 0)
      
    )
    
  }
  
  if(!("B"%in%medidas$Maquinas)){
    
    medidas <- rbind(
      
      medidas[1,],
      
      tibble(Maquinas = "B", Tiradas = 0, Exitos = 0),
      
      medidas[2,]
      
    )
    
  }
  
  # Parametros de las distribuciones a posteriori y distribuciones a posterior
  
  posterior <- NULL
  
  for(i in 1:3) {
    
    a <- 2 + medidas$Exitos[i]
    
    b <- 2 + medidas$Tiradas[i] - medidas$Exitos[i]
    
    posterior <- c(posterior, dbeta(thetas, a, b))
    
  }
  
  df_posteriors <- data.frame(x = thetas,
                              
                              posterior,
                              
                              Maquina = rep(c("A","B","C"), each = 1000))
  
  # Grafico posteriors
  
  grafico_posteriors <- df_posteriors |> ggplot()+
    
    geom_line(aes(x = x, y = posterior, color = Maquina))+
    
    geom_area(aes(x = x, y = posterior, fill = Maquina),
              
              alpha = 0.2, position = "identity")+
    
    labs(x = expression(theta),
         
         y = expression("p("~ theta ~"|  y )"))
  
  # Grafico ganancias acumuladas
  
  ganancias_acumuladas <- NULL
  
  for (i in seq(df$Ganancias)) {
    
    ganancias_acumuladas[i] <- sum(df$Ganancias[1:i])
    
  }
  
  grafico_ganancias_acumuladas <- tibble(
    ganancias_acumuladas, Dia = 1:length(ganancias_acumuladas)) |>
    
    ggplot() +
    
    geom_line(aes(x = Dia, y = ganancias_acumuladas),
              
              linewidth = 1, color = "#1d35ab") +
    
    scale_y_continuous(breaks = ceiling(seq(0,max(ganancias_acumuladas),length.out = 10)))+
    
    geom_vline(xintercept = 0) +
    
    geom_hline(yintercept = 0) +
    
    labs(y = "Unidades monetarias acumuladas")
  
  # Barplot jugadas cada maquina
  
  tabla_barplot <- tibble(
    
    Maquina = rep(c("A", "B", "C"), times = 2),
    
    Resultado = factor(rep(c("Exito", "Fracaso"), each = 3), levels = c("Fracaso", "Exito")),
    
    Cantidad = c(medidas$Exitos, medidas$Tiradas - medidas$Exitos)
    
  )
  
  grafico_barplot <- tabla_barplot |>
    
    ggplot(aes(fill=Resultado, y=Cantidad, x=Maquina)) +
    
    geom_bar(position="stack", stat="identity", color = "black", width = 0.8) +
    
    scale_fill_manual(values = c("Exito" = "palegreen3", "Fracaso" = "tomato2")) +
    
    geom_text(aes(label = ifelse(Cantidad > max(Cantidad)*0.15, Cantidad, NA)), position = "stack", vjust = 1.5, show.legend = F, size = 3.4)
  
  
  return(list(
    ganancias_acumuladas = ganancias_acumuladas,
    tabla_barplot = tabla_barplot,
    grafico_ganancias_acumuladas = grafico_ganancias_acumuladas,
    grafico_barplot = grafico_barplot,
    grafico_posteriors = grafico_posteriors
  ))
  
}


