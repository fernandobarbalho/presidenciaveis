#Para capturar apenas as principais hashtags que citam lula

analise_lula<-
  analise_hashtags(lula,10)


#Para fazer tf_idf (caminho completo)

tf_idf_lula<- 
  associa_tipo_mensagem(lula, 
                        n= 3300,
                        tags_positivas = analise_lula$hashtag[c(2,4,6,9)], 
                        tags_negativas = analise_lula$hashtag[c(3,10)],
                        #usuarios_noticias = imprensa,
                        tipo =2) %>%
  processa_tweets() %>%
  tabela_tf_idf()

#Para mostrar o gráfico tf_idf (caminho curto)

tf_idf_lula$graph


#para gerar a base do modelo ml (caminho completo)

modelo_ml_lula<-
  associa_tipo_mensagem(lula, 
                        n= 3300,
                        tags_positivas = analise_lula$hashtag[c(2,4,6,9)], 
                        tags_negativas = analise_lula$hashtag[c(3,10)],
                        #usuarios_noticias = imprensa,
                        tipo =2) %>%
  processa_tweets() %>%
  prepara_base_ml() 

#para gerar o modelo ml (caminho curto)

dt_lula<-
modelo_ml_lula %>%
  cria_arvore_decisao()

#Para gerar a figura da dt (caminho curto)
grafico_arvore_decisao(dt_lula)
