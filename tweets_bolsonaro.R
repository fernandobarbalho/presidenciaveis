set.seed(1972)
msg_bolsonaro_treino<-
  associa_tipo_mensagem(df_twittes =  bolsonaro, 
                        n= 3300,
                        tags_positivas = analise_bolsonaro$hashtag[c(2,3,8,9,10)], #corrigir
                        tags_negativas = analise_bolsonaro$hashtag[c(1,7)], #corrigir
                        usuarios_noticias = imprensa,
                        tipo =2
  )






tidy_tweet<- 
  msg_bolsonaro_treino %>%
  inner_join(bolsonaro) %>%
  select(status_id, screen_name, tipo_mensagem, text) %>%
  unnest_tweets(word,text, strip_url = TRUE)%>%
  dplyr::filter(!str_detect(word,"#"),
                !str_detect(word,"@"),
                !str_detect(word, "[^[:graph:][:space:]]"))

tidy_tweet$word<- abjutils::rm_accent(tidy_tweet$word)

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


colnames(pml_train_model) <- make.names(colnames(pml_train_model))
#pml_train_model<- janitor::clean_names(pml_train_model)

pml_train_model<- pml_train_model[,substr(names(pml_train_model),1,2)!="X."]


########### decision tree para todos os elementos

# Create model with ramdom parameters
control_dt <- trainControl(method="cv")
seed <- 1972
set.seed(seed)


dt_model <- train(tipo_mensagem~., data=pml_train_model, method="rpart",  trControl=control_dt)

library(rattle)
fancyRpartPlot(dt_model$finalModel)


fab<-tibble(nomes=names(pml_train_model)) 




