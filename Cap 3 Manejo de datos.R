#cargar paquetes
library(tidyverse)
library(paqueteadp)
library(skimr)
#cargar datos
data(aprob_presidencial)
#ver que se haya cargado la data
ls()
#ver resumen de los datos
aprob_presidencial
#misma vaina, diferente
glimpse(aprob_presidencial)
View(aprob_presidencial)
skim(aprob_presidencial)
# ver conteo de variable pres_sexo, y ordenar de mayor a menor según n
count(aprob_presidencial, pres_sexo, sort = T) 
# seleccionar una o varias variables. Por si solo inútil, pero lo incluíremos más adelante en códigos grandes.
select(aprob_presidencial, pais)
select(aprob_presidencial, pais, anio, trimestre, presidente, aprob_neta)
select(aprob_presidencial, pais:aprob_neta)
select(aprob_presidencial, 1:5)
select(aprob_presidencial, presidente, everything()) 
select(aprob_presidencial, starts_with("wdi_")) 
#ojo, estos resumenes me presentan datos numericos con diferente notación según el espacio, el cual es determinado por la amplitud en caractéres de la variable.

#cambiar nombre de pib por pib_ppp_c2011, pero no se guarda:
rename(aprob_presidencial, pib_ppp_c2011 = pib)
#hacer el mismo cambio, pero guardándolo:
aprob_presidencial <- aprob_presidencial %>% #esto traduce: reemplaze a por a en el cual renombre x a x2
  + rename(pib_ppp_c2011 = pib)
#chequeamos llamándo la base, o haciendo un summary:
aprob_presidencial

#renombrar varias cosas a la vez:
rename(aprob_presidencial, 
       pib_ppp_c2011 = pib, 
       porc_desempleo = desempleo, 
       porc_crecimiento_pib = crec_pib)
rename(aprob_presidencial, pib_ppp_c2011 = pib, porc_desempleo = desempleo, porc_crecimiento_pib = crec_pib)
#espacios son irrelevantes, ambos logran lo mismo)

#filtrar observaciones
filter(aprob_presidencial, pais == "Chile")
filter(aprob_presidencial, aprob_neta > 0)
filter(aprob_presidencial, pais == "Argentina" | pais == "Chile" | pais == "Uruguay")
filter(aprob_presidencial, pais %in% c("Argentina", "Chile", "Uruguay")) 
filter(aprob_presidencial, corr_ejec > mean(corr_ejec)) # todas las observaciones en las que la corrupción ejecutiva es mayor a la del promedio de toda la base

#operadores lógicos
# == 	es igual a
# != 	es distinto a
# > 	es mayor a
# < 	es menor a
# >= 	es mayor o igual a
# <= 	es menor o igual a
# & 	intersección
# | 	unión
# %in% 	está contenido en

arrange(aprob_presidencial, corr_ejec)
arrange(aprob_presidencial, -corr_ejec) #agregar "-" previo al nombre de variable para orden inverso
arrange(aprob_presidencial, desc(presidente)) #también se puede con desc(), y para caracteres el orden será alfabético
arrange(aprob_presidencial, pres_sexo, -aprob_neta) #ordenar en base a primera variable, empates en base a segunda

mutate(aprob_presidencial, poblacion_mill = poblacion / 1000000) #crear nuevas variables, mutate(data, nueva variable = aritmética con otras variables o valores)

aprob_presidencial <- aprob_presidencial %>%
  mutate(poblacion_tamanio = ifelse(poblacion<=1000000 , "pequeño", "grande")) #también se puede así, aunque es más complejo

mutate(aprob_presidencial, pib_log = log(pib)) #pib en versión logarítmica

mutate(aprob_presidencial, pib_pc = pib / poblacion) #pib per capica

mutate(aprob_presidencial,  #multiples transformaciones a la vez
       poblacion_mill = poblacion / 1000000,
       pib_pc   = pib / poblacion)

summarize(aprob_presidencial, prom_desemp   = mean(desempleo), prom_crec     = mean(crec_pib), prom_aprob    = mean(aprob_neta))
#resúmenes con medidas de tendencia central, para una o varias observaciones

#en general vamos a estar creando versiones modificadas de la base de datos, y trabajar con estas en vez de la original según nuestras necesidades
aprob_presidencial_por_pais <- group_by(aprob_presidencial, pais) #en este caso agruparemos por pais

#y ahora podemos sacar medidas para cada grupo, o en este caso pais
summarize(aprob_presidencial_por_pais, prom_desemp   = mean(desempleo), prom_crec_pib = mean(crec_pib), prom_aprob    = mean(aprob_neta))

aprob_presidencial_por_pais_anio <- group_by(aprob_presidencial, pais, anio) #misma vuelta, con más criterios, y por ende más observaciones al momento de sacar las medidas

aprob_presidencial_por_pais_anio %>% #para desagruparlas... Util si estamos trabajando solo con una y transformándola en la medida que lo hacemos
  ungroup() # nota cómo ya no hay "groups" en el resumen de los datos

aprob_presidencial_con_pib_pc <- mutate(aprob_presidencial, pib_pc = pib / poblacion)
filter(aprob_presidencial_con_pib_pc, pib_pc > mean(pib_pc)) #estas dos lineas separadas se pueden correr en una sola, así:

aprob_presidencial %>%  #aquí se utiliza el pipe u operador %>%, que es como decirle a R "esta historia continua en la siguiente línea"
  mutate(pib_pc = pib / poblacion) %>% # %>% se puede teclear manualmente , o con CTRL+SHIFT+M
  filter(pib_pc > mean(pib_pc))

aprob_presidencial %>% #combo muy común, que combina group_by() y summarize()
  group_by(pais) %>% 
  summarize(prom_desemp   = mean(desempleo), prom_crec     = mean(crec_pib), prom_aprob    = mean(aprob_neta))

aprob_presidencial %>% #nueva variable que me transforma pres_sexo, variable de caractéres, a d_pres_mujer, variable numérica binaria
  mutate(d_pres_mujer = if_else(condition = pres_sexo == "female",
                                true      = 1,
                                false     = 0)) %>% 
  select(pais:presidente, pres_sexo, d_pres_mujer) # esta última linea es para que no nos muestre todos los valores, y poder revisar que quedó bien hecha la nueva variable


aprob_presidencial %>% #aqui estamos creando una nueva variable que nos diga cuando hay o no crisis económica, en base a si el cecimiento del pib y el desempleo cumplen ciertos requerimientos
  # no explicitamos los argumentos para hacer el código más sucinto, osea, por defecto se expresa Verdadero=1 y Falso=0
  mutate(d_crisis_ec = if_else(crec_pib < 0 | desempleo > 20, 1, 0)) %>% 
  # lo siguiente es solo para mostrar más claramente los resultados:
  select(pais:trimestre, crec_pib, desempleo, d_crisis_ec) %>% 
  filter(pais == "Argentina" & anio %in% c(2001, 2013))

unique(aprob_presidencial$pais) #muestra los valores que asume una variable en específico, en este caso pais
names(aprob_presidencial) #muestra las variables o columnas de la base de datos

#ahora, mientras que ifelse() solo nos permite clasificar en verdadero y falso, case_when() nos dá más libertad
aprob_presidencial %>% #este código me crea una variable que asigna subcategoría de regíon según el pais
  mutate(grupo_pais = case_when(
    pais %in% c("Argentina", "Chile", "Uruguay") ~ "Cono Sur", 
    pais %in% c("Costa Rica", "El Salvador", "Guatemala", "Honduras",
                "Nicaragua", "Panama") ~ "Centroamérica",
    TRUE ~ "Resto de AL")) %>%  #TRUE es igual a EN TODO EL RESTO DE CASOS
  # para ver mejor los resultados, achicaremos la base:
  filter(anio == 2000 & trimestre == 1) %>% 
  select(pais, grupo_pais)


#Ejercicio D
aprob_presidencial %>% 
  mutate(d_pres_mujer = case_when(pres_sexo %in% c("female") ~ "1", TRUE ~ "0")) %>% 
  select(pais:presidente, pres_sexo, d_pres_mujer)

aprob_presidencial %>% 
  mutate(d_pres_mujer = if_else(condition = pres_sexo == "female", true = 1, false = 0)) %>% 
  select(pais:presidente, pres_sexo, d_pres_mujer) #diferencia en el resultado, los valores resultantes son tipo character en vez de dbl(numérico)

#Pivoteo de bases
aprob_presidencial_por_anio_pais <- aprob_presidencial %>% 
  group_by(pais, anio) %>% 
  summarize(aprob_neta = mean(aprob_neta)) %>% 
  ungroup()

aprob_presidencial_por_anio_pais

library(paqueteadp)
data(aprob_presidencial_anual_wide)

aprob_presidencial_anual_wide
#este tipo de formato no-tidy, tipo wide, es útil para visualizar bases de datos con una sola variable.

#lo podemos convertir a formato tidy o "Pivotearlo" de la siguiente manera:
aprob_presidencial_anual_wide %>% 
  pivot_longer(cols = -pais, names_to = "anio", values_to = "aprob_neta")

#y viceversa:
aprob_presidencial_por_anio_pais %>% 
  pivot_wider(names_from = "anio", values_from = "aprob_neta")

#round and round, se cancela:
aprob_presidencial_anual_wide %>%
  pivot_longer(cols = -pais, names_to = "anio", values_to = "aprob_neta") %>% 
  pivot_wider(names_from = "anio", values_from = "aprob_neta")

#pendiente por registrar últimos dos ejemplos.


print("para el ejercicio de github, estoy agregando esto")