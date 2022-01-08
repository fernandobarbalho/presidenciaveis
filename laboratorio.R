library(rtweet)
library(purrr)
library(dplyr)
library(ggplot2)
library(readr)


#Carrega os dados com os twittes que citam os presidenciáveis

load("~/Github/presidenciaveis/tweets_presidenciaveis.RData")


#Função que gera um dataframe com as hashtags associadas aos twittes onde aparecem os candidatos
analise_hashtags<- function(df_tweets, n=NULL){
  
  library(purrr)
  
  hashtags<- df_tweets$hashtags
  
  
  keywords<-
    map_dfr(1:length(hashtags), function(inc){
      
      print(inc)
      if ( !is.na(hashtags[[inc]]) ){
        print(hashtags[[inc]])
        map_dfr(1:NROW(hashtags[[inc]]), function(inc_int){
          print(hashtags[[inc]][inc_int])
          tibble(hashtag= hashtags[[inc]][inc_int])
          
        })
      }
    })
  
  
  analise_keywords<-
    keywords %>%
    group_by(hashtag) %>%
    summarise(
      quantidade = n()
    )
  
  if (!is.null(n)){
    
    analise_keywords %>%
      slice_max(quantidade, n=n)
    
    
  }

}


analise_bolsonaro<- analise_hashtags(bolsonaro, n=10)
analise_lula<- analise_hashtags(lula, n=10)
analise_moro<- analise_hashtags(moro, n=10)
analise_ciro<- analise_hashtags(ciro, n=10)
analise_doria<- analise_hashtags(doria, n=10) 


#associação de mensagens positivas, negativas e neutras aos candidatos a uma amostra das mensagens

associa_tipo_mensagem <- function(df_twittes, n, tags_positivas, tags_negativas, usuarios_noticias, tipo=1, nome_arquivo=NULL, delim=","){
  

  df_trabalho<-
    df_twittes %>%
    mutate(tipo_mensagem = case_when(
      hashtags %in% tags_positivas ~ "positivo",
      hashtags %in% tags_negativas ~ "negativo",
      screen_name %in% usuarios_noticias ~ "noticia"
    )) %>%
    select(status_id, screen_name, text,tipo_mensagem, hashtags)
  
  if (tipo == 1){
    
    df_trabalho<-
    df_trabalho %>%
      slice_sample(n=n)
    
    #seleciona uma amostra das mensagens e sugere as primeiras definições do tipo de mesnagem a partir das hashtags e usuários
    
    
  } else{
    
    #seleciona uma amostra das mensagens e sugere as primeiras definições do tipo de mesnagem a partir das hashtags e usuários
    df_trabalho<-
      df_trabalho %>%
      filter(!is.na(tipo_mensagem)) %>%
      slice_sample(n=n)
  }
  
  
  if (!is.null(nome_arquivo)){
    df_trabalho%>%
      readr::write_delim(nome_arquivo, delim = delim,  )
  }
 
  df_trabalho

}

imprensa<- c("folha", "estadao", "opovo", "@JornalOGlobo")



############# Faz o primeiro treino

library(readr)
library(tidytext)
library(stringr)
library(dplyr)

set.seed(1972)

msg_lula_treino<-
  associa_tipo_mensagem(df_twittes =  lula, 
                        n= 180,
                        tags_positivas = analise_lula$hashtag[c(1,2,4,5,6,8,9)],
                        tags_negativas = analise_lula$hashtag[c(3,10)],
                        usuarios_noticias = imprensa,
                        nome_arquivo = "msg_lula_treino.csv"
  )


set.seed(1972)
msg_bolsonaro_treino<-
  associa_tipo_mensagem(df_twittes =  bolsonaro, 
                        n= 180,
                        tags_positivas = analise_bolsonaro$hashtag[c(2,3,8,9,10)], #corrigir
                        tags_negativas = analise_bolsonaro$hashtag[c(1,7)], #corrigir
                        usuarios_noticias = imprensa,
                        nome_arquivo = "msg_bolsonaro_treino.csv"
  )


############# Faz o segndo treino

set.seed(1972)

msg_lula_treino<-
  associa_tipo_mensagem(df_twittes =  lula, 
                        n= 1200,
                        tags_positivas = analise_lula$hashtag[c(2,4,5,9)],
                        tags_negativas = analise_lula$hashtag[c(3,10)],
                        usuarios_noticias = imprensa,
                        tipo =2
  )


set.seed(1972)
msg_bolsonaro_treino<-
  associa_tipo_mensagem(df_twittes =  bolsonaro, 
                        n= 180,
                        tags_positivas = analise_bolsonaro$hashtag[c(2,3,8,9,10)], #corrigir
                        tags_negativas = analise_bolsonaro$hashtag[c(1,7)], #corrigir
                        usuarios_noticias = imprensa,
                        tipo =2
  )

msg_lula_treino_tratado <- msg_lula_treino



tidy_tweet<- 
  msg_lula_treino_tratado %>%
  inner_join(lula) %>%
  select(status_id, screen_name, tipo_mensagem, text) %>%
  unnest_tweets(word,text, strip_url = TRUE)%>%
  dplyr::filter(!str_detect(word,"#"),
                !str_detect(word,"@"),
                !str_detect(word, "[^[:graph:][:space:]]"))

palavras_unicas_usuario<- 
  tidy_tweet %>%
  group_by(screen_name, word) %>%
  summarise(
    quantidade = n()
  ) %>%
  filter(quantidade == 1) %>%
  select(screen_name, word) %>%
  ungroup()

tidy_tweet<-   
 tidy_tweet %>%
   inner_join(palavras_unicas_usuario)
  
library(stopwords) 

library(tibble)

stopword <- as_tibble(stopwords::stopwords("pt")) 
stopword <- rename(stopword, word=value)
tb <- anti_join(tidy_tweet, stopword, by = 'word')


###########Term frequency (tf)

word_count <- count(tb, word, sort = TRUE)

tipo_count <-  tb %>% 
  count(tipo_mensagem, word, sort = TRUE)
  

########## Term frequency and inverse document frequency (tf-idf)

library(forcats)

plot_tb <- tb %>%
  count(tipo_mensagem, word, sort = TRUE) %>%
  bind_tf_idf(word, tipo_mensagem, n) %>%
  mutate(word = fct_reorder(word, tf_idf)) %>%
  mutate(tipo_mensagem = factor(tipo_mensagem, 
                         levels = c("positivo",
                                    "negativo",
                                    "neutro")))


plot_tb %>% 
  group_by(tipo_mensagem) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = tipo_mensagem)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~tipo_mensagem, ncol = 3, scales = "free") +
  coord_flip() +
  theme_classic(base_size = 12) +
  labs(fill= "Tipo Mensagem", 
       title="Term frequency and inverse document frequency (tf-idf)", 
       subtitle="Top 15 words by tipo mensage",
       x= NULL, 
       y= "tf-idf") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_fill_brewer()  


###########Tokenizing by n-gram

positivo_bigrams <- msg_lula_treino_tratado %>%
  filter(tipo_mensagem == "positivo") %>% 
  #unnest_tweets(word,text, strip_url = TRUE, n=2)
  unnest_tokens(bigram, text, token = "ngrams", n = 2)


positivo_bigrams_count <- positivo_bigrams %>% 
  count(bigram, sort = TRUE)

library(tidyr)

# seperate words
bigrams_separated <- positivo_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# filter stop words and NA
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stopword$word) %>%
  filter(!word2 %in% stopword$word) %>% 
  filter(!is.na(word1))

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

### Agora sim o ML. Preparação das base

library(caret)
library(tidyr)

pml_train_trabalho<-
tb %>%
  group_by(status_id, tipo_mensagem, word) %>%
  summarise(
    quantidade = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = word, values_from = quantidade)


pml_train_trabalho[is.na(pml_train_trabalho)]<-0  

library(janitor)

pml_train_model <- pml_train_trabalho[,-1]

pml_train_model<- janitor::clean_names(pml_train_model)


########### decistion tree para todos os elementos

# Create model with ramdom parameters
control_dt <- trainControl(method="cv")
seed <- 1972
set.seed(seed)


dt_model <- train(tipo_mensagem~., data=pml_train_model, method="rpart",  trControl=control_dt)

library(rattle)
fancyRpartPlot(dt_model$finalModel)





###########rf
set.seed(1972)
inTrain<- createDataPartition(y= pml_train_model$tipo_mensagem, p=0.7, list = FALSE)
pml_train_model<- pml_train_model[inTrain,]



# Create model with ramdom parameters
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
seed <- 1972
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(pml_train_model))
tunegrid <- expand.grid(.mtry=mtry)



rf_random <- train(tipo_mensagem~., data=pml_train_model, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, tuneLength=15)
print(rf_default)


rf_random


# Create model with grid search paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
seed <- 1972
metric <- "Accuracy"
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:15))






rf_gridsearch <- train(tipo_mensagem~., data=pml_train_model, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)


rf_gridsearch

predict(rf_gridsearch, pml_train_model[-inTrain,] )

pred<- predict(rf_random, pml_train_model[-inTrain,] )

confusionMatrix(factor(pml_train_model$tipo_mensagem[-inTrain]), pred)

importance<- rf_random[["finalModel"]][["importance"]]


####################### Aplicação do modelo para toda a base

#Modelo random deu melhor resultado na matriz de confusão

#aplicando para o restante das 

textos_base_total<-
lula %>%
  anti_join(msg_lula_treino) %>%
  arrange(status_id) %>%
  select(status_id,text)%>%
  mutate(text= stringr::str_replace_all(text, "@","user="),
         text= stringr::str_remove_all(text, "[^[:graph:][:space:]]"))%>%
  unnest_tweets(word,text, strip_url = TRUE)%>%
  dplyr::filter(!str_detect(word,"#"))

textos_decisao<-   
  palavras_unicas_usuario %>%
  distinct(word) %>%
  left_join(textos_base_total)


tabela_trabalho<-
  textos_decisao %>%
  group_by(status_id, word) %>%
  summarise(
    quantidade = n()
  ) %>%
  ungroup() %>%
  arrange(status_id) %>%
  pivot_wider(names_from = word, values_from = quantidade)


status_id<- tabela_trabalho$status_id

tabela_trabalho<- tabela_trabalho[,-1] 
tabela_trabalho[is.na(tabela_trabalho)]<-0  

tabela_trabalho<- janitor::clean_names(tabela_trabalho)

pred_trabalho<- predict(rf_random, tabela_trabalho )

resultado<- tibble(status_id= status_id,pred_trabalho)


resultado_join<-
resultado %>%
  inner_join(
    lula%>%
      select(status_id,
             user_id,
             screen_name,
             text)) 

set.seed(1972)
resultado_join %>%
  slice_sample(n=200) %>%
  readr::write_csv("resultado_sample.csv")

