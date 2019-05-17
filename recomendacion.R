#https://github.com/anoop93/Netflix_movie_recommendation_system
rm(list=ls())
setwd("~")
gc()
setwd("C:/DataScience08012019/ejercicioCarlosRecomendacion/")
library(ggplot2)
library(dplyr)
library(stringr)

load('Data/dat_muestra_nflix.Rdata')
dim(dat.muestra)

pelis.nombres <- read.csv('Data/movies_title_fix.csv', stringsAsFactors = FALSE, header=FALSE)
names(pelis.nombres) <- c('peli_id','release','nombre')
head(pelis.nombres)

medias.peliculas <- dat.muestra %>% 
  group_by(peli_id) %>% 
  summarise(media_peli = mean(calif), num_calif_peli = length(calif))
#Dentro del summarise se crean las variables sobre la media de la calificación y el número de veces que
#se han calificado. Esto servirá para el ENPECYT
medias.p.2 <- left_join(medias.peliculas, pelis.nombres)

arrange(medias.p.2, desc(media_peli)) %>% data.frame %>% head(10)
ggplot(medias.p.2, aes(x=num_calif_peli, y=media_peli)) + geom_point()
mean(dat.muestra$calif)
#Se van a llamar las películas que han sido vistas al menos 500 veces, para evitar lo de 
#Blood and Black Lace que solo fue vista 4 veces
arrange(filter(medias.p.2, num_calif_peli > 500), desc(media_peli)) %>%
  data.frame %>% head(10) %>% select(peli_id, media_peli, nombre)

set.seed(28882)
valida_usuarios <- sample(unique(dat.muestra$usuario_id), 20000 )
valida_pelis <- sample(unique(dat.muestra$peli_id), 2000 )
dat.2 <- dat.muestra %>%
  mutate(valida_usu = usuario_id %in% valida_usuarios) %>%
  mutate(valida_peli = peli_id %in% valida_pelis)
dat.entrena <- filter(dat.2, !valida_usu | !valida_peli)
dat.valida <- filter(dat.2, valida_usu & valida_peli)
nrow(dat.entrena) + nrow(dat.valida)

medias.pred <- dat.entrena %>%
  group_by(peli_id) %>%
  summarise(media.pred = mean(calif))
dat.valida.2 <- left_join(dat.valida, medias.pred)
table(is.na(dat.valida.2$media.pred))
#Error cuadrático medio
sqrt(mean((dat.valida.2$calif - dat.valida.2$media.pred)^2))

entrena.usu <- sample(unique(dat.entrena$usuario_id), 50)
muestra.graf <- filter(dat.entrena, usuario_id %in% entrena.usu)
muestra.res <- muestra.graf %>% group_by(usuario_id) %>%
  summarise(media.calif = mean(calif), sd.calif = sd(calif))
muestra.res$usuario_id <- reorder(factor(muestra.res$usuario_id), muestra.res$media.calif)
ggplot(muestra.res, aes(x=factor(usuario_id), y=media.calif,
                                              ymin=media.calif-sd.calif, ymax=media.calif+sd.calif)) + geom_linerange() + geom_point()
set.seed(128)
n <- 50
niveles <- data.frame(persona=1:n, nivel=rnorm(n,2))
x <- rnorm(n)
gustos <- data.frame(persona=1:n, gusto_1=x+rnorm(n),
                     gusto_2=-x+rnorm(n))
head(gustos,3)
cor(gustos[,2:3])
medicion_1 <- niveles$nivel + gustos$gusto_1+rnorm(n,0.3)
medicion_2 <- niveles$nivel + gustos$gusto_2+rnorm(n,0.3)
mediciones <- data.frame(persona=1:n, medicion_1, medicion_2)
cor(mediciones[,2:3])
mat.cons <- matrix(NA, 5, 6)
mat.cons[1,1] <- 5;mat.cons[1,2] <- 5;mat.cons[1,3] <- 5;mat.cons[1,4] <- 2
mat.cons[2,1] <- 3;mat.cons[2,3] <- 4;
mat.cons[3,4] <- 5; mat.cons[3,5] <- 4
mat.cons[4,1] <- 1;mat.cons[4,3] <- 2;mat.cons[4,5] <- 5;mat.cons[4,6] <- 4;
mat.cons[5,1] <- 4; mat.cons[5,2] <- 5; mat.cons[5,6] <- 2
rownames(mat.cons) <- c('a','b','c','d','e')
colnames(mat.cons) <- c('SWars1','SWars4','SWars5','HPotter1','HPotter2','Twilight')
mat.cons
apply(mat.cons,1, mean, na.rm=TRUE)
#Parece matriz de términos documentos.
mat.c <- mat.cons - apply(mat.cons,1, mean, na.rm=TRUE)
mat.c

dcos <- function(x,y){
  #x <- x-mean(x, na.rm = T)
  #y <- y-mean(y, na.rm = T)
  sum(x*y, na.rm = T)/(sqrt(sum(x^2, na.rm = T))*sqrt(sum(y^2, na.rm = T)))
}
mat.c[,1]
#Comparación entre dos películas para ver su similitud
dcos(mat.c[,1], mat.c[,2])
dcos(mat.c[,4], mat.c[,5])

dat.entrena.c <- dat.entrena %>%
  group_by(usuario_id) %>%
  mutate(calif.c = calif - mean(calif))

## calculamos un id secuencial.
dat.entrena.c$id_seq <- as.numeric(factor(dat.entrena.c$usuario_id))

filter(pelis.nombres, str_detect(nombre,'Gremlins'))
filter(pelis.nombres, str_detect(nombre,'Captain'))
dat.1 <- filter(dat.entrena.c, peli_id==2897)
dat.2 <- filter(dat.entrena.c, peli_id==6482)
dat.3 <- filter(dat.entrena.c, peli_id==2660)
comunes <- inner_join(dat.1[, c('usuario_id','calif.c')], dat.2[, c('usuario_id','calif.c')] %>% rename(calif.c.2=calif.c))
comunes.2 <- inner_join(dat.1[, c('usuario_id','calif.c')], dat.3[, c('usuario_id','calif.c')] %>% rename(calif.c.2=calif.c))
#Se grafica cómo se calificaron dos películas y se ve si hay tendencia o no.
ggplot(comunes, aes(x=calif.c, y=calif.c.2)) + geom_point() + geom_smooth()

set.seed(28882)
valida_usuarios <- sample(unique(dat.muestra$usuario_id), 20000)
valida_pelis <- sample(unique(dat.muestra$peli_id), 2000)
dat.2 <- dat.muestra %>%
  mutate(valida_usu = usuario_id %in% valida_usuarios) %>%
  mutate(valida_peli = peli_id %in% valida_pelis)

arrange(filter(medias.p.2, num_calif_peli > 500), desc(media_peli)) %>%
  data.frame %>% head(10) %>% select(peli_id, media_peli, nombre)

dat.entrena <- filter(dat.2, !valida_usu | !valida_peli)
dat.valida <- filter(dat.2, valida_usu & valida_peli)
nrow(dat.entrena) + nrow(dat.valida)

ggplot(comunes.2, aes(x=calif.c, y=calif.c.2)) + geom_point() + geom_smooth()
dcos(comunes$calif.c, comunes$calif.c.2)
dcos(comunes.2$calif.c, comunes.2$calif.c.2)

dat.entrena.2 <- dat.entrena.c %>% ungroup() %>% select(peli_id, id_seq, calif.c)

ejemplos <- function(pelicula){
  mi_peli <- filter(dat.entrena.2, peli_id==pelicula) %>%
    rename(peli_id_1=peli_id, calif.c.1 = calif.c)
  datos.comp <- left_join(dat.entrena.2, mi_peli)
  # calcular similitudes
  out.sum <- datos.comp %>%
    group_by(peli_id) %>%
    summarise(dist = dcos(calif.c, calif.c.1)) %>%
    data.frame() %>%
    left_join(medias.p.2)
  out.sum %>% arrange(desc(dist))  %>% select(nombre, dist, num_calif_peli)
}
ejemplos(1234) %>% head(20)
