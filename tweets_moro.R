#Para capturar apenas as principais hashtags que citam moro

analise_moro<-
  analise_hashtags(moro,10)


#Para fazer tf_idf (caminho completo)

tf_idf_moro<- 
  associa_tipo_mensagem(moro, 
                        n= 3300,
                        tags_positivas = analise_moro$hashtag[c(3,5,8,13)], 
                        tags_negativas = analise_moro$hashtag[c(1,6,7)],
                        #usuarios_noticias = imprensa,
                        tipo =2) %>%
  processa_tweets() %>%
  tabela_tf_idf()

#Para mostrar o gráfico tf_idf (caminho curto)

tf_idf_moro$graph


#para gerar a base do modelo ml (caminho completo)

modelo_ml_moro<-
  associa_tipo_mensagem(moro, 
                        n= 3300,
                        tags_positivas = analise_moro$hashtag[c(3,5,8,13)], 
                        tags_negativas = analise_moro$hashtag[c(1,6,7)],
                        #usuarios_noticias = imprensa,
                        tipo =2) %>%
  processa_tweets() %>%
  prepara_base_ml() 

#para gerar o modelo ml (caminho curto)

dt_moro<-
modelo_ml_moro %>%
  cria_arvore_decisao()

#Para gerar a figura da dt (caminho curto)
grafico_arvore_decisao(dt_moro)
