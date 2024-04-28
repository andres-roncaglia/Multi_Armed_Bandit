# Multi Armed Bandit

Consiste en una serie de aplicaciones en el famoso problema del [bandido multibrazo](https://es.wikipedia.org/wiki/Bandido_multibrazo) donde se investiga mediante simulaciones el efecto en las ganancias, jugando en máquinas tragamonedas, que tienen distintas técnicas de selección. 

El principal enfoque es estudiar bajo el enfoque de la estadística bayesiana el comportamiento de las técnicas utilizadas.

Algunas consideraciones a tener en cuenta son:

- Cada "día" se juega con una sola máquina, y se estudian las ganancias al cabo de un año de 366 días.

- Hay 3 máquinas posibles con las cuales jugar, cada una con distinta probabilidad de ganar.

- Jugar no cuesta dinero, y ganar con una máquina resulta en la ganancia de una unidad monetaria.

- La máquina con la cual jugar se decide en base a la técnica de selección. Las técnicas estudiadas serán:
  
  - Completamente al azar
  
  - *Greedy* con tasa observada
  
  - *Greedy* con probabilidad a posteriori
  
  - $\epsilon$-*Greedy* con tasa observada
  
  - *Softmax*
  
  - *Upper-Bound*
  
  - *Thompson*

Todos los análisis se hicieron conforme a responder las consignas indicadas en el primer trabajo práctico de la materia [Estadística Bayesiana](https://www.fcecon.unr.edu.ar/carreras/grado/licenciatura-en-estadistica/materia/estadistica-bayesiana) de la carrera [Licenciatura en Estadística](https://www.fcecon.unr.edu.ar/carreras/grado/licenciatura-en-estadistica) de la [Universidad Nacional de Rosario](https://unr.edu.ar/).