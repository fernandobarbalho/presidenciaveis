#Para capturar apenas as principais hashtags que citam bolsonaro

analise_bolsonaro<-
  analise_hashtags(bolsonaro,10)

#Para processar os tweets das hashtagas positivas e negativas

tweets_processados_bolsonaro<-
  associa_tipo_mensagem(bolsonaro, 
                        n= 3300,
                        tags_positivas = analise_bolsonaro$hashtag[c(2,3,8,9,10)], 
                        tags_negativas = analise_bolsonaro$hashtag[c(1,7)],
                        #usuarios_noticias = imprensa,
                        tipo =2) %>%
  processa_tweets()

#Para fazer tf_idf (caminho completo)

tf_idf_bolsonaro<- 
  associa_tipo_mensagem(bolsonaro, 
                        n= 3300,
                        tags_positivas = analise_bolsonaro$hashtag[c(2,3,8,9,10)], 
                        tags_negativas = analise_bolsonaro$hashtag[c(1,7)],
                        #usuarios_noticias = imprensa,
                        tipo =2) %>%
  processa_tweets() %>%
  tabela_tf_idf()

#Para mostrar o gráfico tf_idf (caminho curto)

tf_idf_bolsonaro$graph


#para gerar a base do modelo ml (caminho completo)

modelo_ml_bolsonaro<-
  associa_tipo_mensagem(bolsonaro, 
                        n= 3300,
                        tags_positivas = analise_bolsonaro$hashtag[c(2,3,8,9,10)], 
                        tags_negativas = analise_bolsonaro$hashtag[c(1,7)],
                        #usuarios_noticias = imprensa,
                        tipo =2) %>%
  processa_tweets()%>%
  prepara_base_ml() 

#para gerar o modelo ml (caminho curto)

dt_bolsonaro<-
modelo_ml_bolsonaro %>%
  cria_arvore_decisao()

#Para gerar a figura da dt (caminho curto)
grafico_arvore_decisao(dt_bolsonaro)
