require(Rfacebook)
require(plyr)
library(tidyverse)
library(knitr)
library(wordcloud)
library(RColorBrewer)

token <- "EAACEdEose0cBAFeSbdHQPw0vFgnLnYhOaflryB0vuZBOho0MUoTBbnc6gt3g1ZCZAc9ClzYuzGd6eE4oAOSHIHEgQZAtZBUmnh7S5UK6uc6LZCjkQZAZA9wKDV6i4ZCVR7Ehdp8TZClnnrFvzDhg8Eh2fZAuHZAsr5dgsBRAZBvVDozrKGIRDiOxm0oSuP5AFn7s65wgZD"
#Petro
pet<-getUsers('me',token,private_info = T)

myFriends<-getFriends(token)
myFriendsInfo<-getUser(myFriends$id, token=token, private_info=T)

fbPage<-getPage(page='facebook',token)
grupos<-searchGroup('Ayacucho',token)
pages<-searchPages('Ayacucho',token,n=200)
search<-searchFacebook('ayacucho',token,n=200,since = NULL,until = NULL)
## Searching 100 public posts that mention "facebook" from yesterday
posts <- searchFacebook( string="ayacucho", token=token, n=100,
                         since = "yesterday 00:00", until = "yesterday 23:59" )


####################

pagePetro<-getPage("GustavoPetroUrrego", token, n = 500, private_info=T)
petro<-subset(pagePetro,!is.na(pagePetro$message))
#Fajardo
pageFajardo<-getPage("SergioFajardoV", token, n = 500)
fajardo<-subset(pageFajardo,!is.na(pageFajardo$message))
#Duque
pageDuque<-getPage("ivanduquemarquez", token, n = 500)
duque<-subset(pageDuque,!is.na(pageDuque$message))
#Vargas lleras
pageVargas<-getPage("GermanVargasLleras", token, n = 500)
vargas<-subset(pageVargas,!is.na(pageVargas$message))

#Uno todos
texts <- bind_rows(fajardo, duque, petro, vargas)
#Número de posts por usuario
texts %>% group_by(from_name) %>% summarise(numero_posts = n()) 

colnames(texts)


# Selección de variables
texts <- texts %>% select(from_name, created_time, id, message)

# Se renombran las variables con nombres más prácticos
texts <- texts %>% rename(autor = from_name, fecha = created_time,
                          texto = message, tweet_id = id)
head(texts)


texts$texto <- gsub("[[:cntrl:]]", " ", texts$texto)
texts$texto  = sub("([[:space:]])","",texts$texto)
texts$texto  = sub("([[:digit:]])","",texts$texto )
texts$texto  = sub("([[:punct:]])","",texts$texto )
texts$texto = gsub("http\\w+", "", texts$texto)
#Limpiar el texto
limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}

# Se aplica la función de limpieza y tokenización a cada tweet
texts <- texts %>% mutate(texto_tokenizado = map(.x = texto,
                                                 .f = limpiar_tokenizar))
texts %>% select(texto_tokenizado) %>% head()

texts %>% slice(1) %>% select(texto_tokenizado) %>% pull()

tweets_tidy <- texts %>% select(-texto) %>% unnest()
tweets_tidy <- tweets_tidy %>% rename(token = texto_tokenizado)
head(tweets_tidy) 

tweets<-texts

ggplot(tweets, aes(x = as.Date(fecha), fill = autor)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "5 month") +
  labs(x = "fecha de publicación", y = "número de tweets") +
  facet_wrap(~ autor, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


#Total de palabras
tweets_mes_anyo <- tweets %>% mutate(mes_anyo = format(fecha, "%Y-%m"))
tweets_mes_anyo %>% group_by(autor, mes_anyo) %>% summarise(n = n()) %>%
  ggplot(aes(x = mes_anyo, y = n, color = autor)) +
  geom_line(aes(group = autor)) +
  labs(title = "Número de tweets publicados", x = "fecha de publicación",
       y = "número de tweets") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 6),
        legend.position = "bottom")

#Total de palabras utilizadas por cada usuario
tweets_tidy %>% group_by(autor) %>% summarise(n = n()) 
#dfTwidy<-tweets_tidy %>% group_by(autor) %>% summarise(n = n()) 
tweets_tidy %>%  ggplot(aes(x = autor)) + geom_bar() + theme_minimal()  + ggtitle("Total de palabras utilizadas por cada usuario")

#Palabras distintas utilizadas por cada usuario
tweets_tidy %>% select(autor, token) %>% distinct() %>%  group_by(autor) %>%
  summarise(palabras_distintas = n()) 

tweets_tidy %>% select(autor, token) %>% distinct() %>%
  ggplot(aes(x = autor)) + geom_bar() + theme_bw()+ ggtitle("Palabras distintas usadas por usuarios")      

#Longitud media de los tweets por usuario
tweets_tidy %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>% group_by(autor) %>% summarise(media_longitud = mean(longitud),
                                                                                                          sd_longitud = sd(longitud))
tweets_tidy %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%                      group_by(autor) %>%
  summarise(media_longitud = mean(longitud),
            sd_longitud = sd(longitud)) %>%
  ggplot(aes(x = autor, y = media_longitud)) +
  geom_col() +
  geom_errorbar(aes(ymin = media_longitud - sd_longitud,
                    ymax = media_longitud + sd_longitud))  + theme_bw()

#Palabras más utilizadas por usuario
tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>% print(n=30)


#stopwords
lista_stopwords <- c('mi', 'yo', 'el', 'su', 'nostros', 'ustedes', 'ellos',
                     'tu','aquí', 'acá', 'allá', 'de', 'la', 'en','que',
                     'como', 'cómo', 'cuando', 'cúando', 'si', 'no', 'las', 'sus',
                     'mis', 'para', 'por', 'qué', 'vamos', 'voy', 'fui',
                     'muy', 'mucho', 'eso', 'esto', 'aquello', 'entre', 'o', 'y', 'otro',
                     'otra', 'tiene', 'tienen', 'también', 'poco','los','del','se','es',
                     'un','al','con','sin','una','uno','unos','ser','cuyo','hoy','este',
                     'esté','está','esta','me','nos','menos','más','los','desde','hasta',
                     'cotra','puede','hemos','todos','son','eran','seran','fueron',
                     'nuestra','tenemos','hacer','hacia','lo','quienes','sobre','ciudad',
                     'años','meses','mañana','semana','hoy','ayer','tarde','contra','día',
                     'gracias','siempre','nosotros','nuestro','estamos','seguimos','nosotros',
                     'necesitamos','fué','ni','han','fue','le','dia','és','hay','estar','tomar',
                     'están','ver','cual','llevar','cada','dar')

# Se filtran las stopwords
tweets_tidy <- tweets_tidy %>% filter(!(token %in% lista_stopwords))

#Representación gráfica de las frecuencias
tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~autor,scales = "free", ncol = 1, drop = TRUE)



#Nube de palabras
pal2 <- brewer.pal(8,"Set2")
# 
# pal2 <- brewer.pal(8,"Set2")
# wordcloud(names(words), words, scale=c(9,.1),min.freq=3,
#           max.words=Inf, random.order=F, rot.per=.3, colors=pal2)

wordcloud_custom <- function(grupo, df){
  print(grupo)
  wordcloud(words = df$token, scale=c(3,0.2),freq = df$frecuencia,
            max.words = 100, random.order = FALSE, rot.per = 0.35,
            colors = pal2)
}

df_grouped <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
  group_by(autor) %>% mutate(frecuencia = n / n()) %>%
  arrange(autor, desc(frecuencia)) %>% nest() 

walk2(.x = df_grouped$autor, .y = df_grouped$data, .f = wordcloud_custom)
