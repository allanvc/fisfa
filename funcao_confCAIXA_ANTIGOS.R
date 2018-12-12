funcao_confCAIXA_ANTIGOS<-function(teste2_bancos.loc, mesbox_string, anobox_string, ...){ #soh recebe o arquivo do banco como parametro
  #v0.8 2016.01.26 -- acrescentamos os parametros mes e ano
  
  #v0.8 2016.01.26 -- criamos uma pasta para salvar os resultados das analises do dia e por banco
  dir1<- paste(getwd(), "analises", sep='/') # juntando a pasta analises jah criada
  dir2 <- paste(dir1, paste("CAIXA ANTIGOS", mesbox_string, anobox_string, Sys.Date(), sep=' - '), sep='/')
  dir3<- paste(dir2, "/", sep='') # nao consegui acrescentar a contrabarra no final utilizando uma unica linha de codigo
  # eh importante acrescentar essa contrabarra ao final para que possamos indicar a pasta corretamente ao gravar o .csv
  dir.create(path=dir3)
  
  
  teste2_bancos.loc<-gtkFileChooserGetFilename(filename2)
  
  #teste2_bancos.loc<-file.choose()
  teste_bancos.dt<-fread(teste2_bancos.loc, dec=',')#, header = TRUE, fill = TRUE, sep=';', dec=',', stringsAsFactors = FALSE)
  #recebe os mesmos parAmetros de read.table
  # e tem tb a opcao de data.table show.progress
  ###PERFEITO!!! JAH FUNCIONA SOH COM ESSAS ALTERACOES!!!
  teste_bancos.df<-as.data.frame(teste_bancos.dt, stringsAsFactors = FALSE, dec=',')
  print("Dimensao Arquivo Banco (LINHASxCOLUNAS):" )
  print(dim(teste_bancos.df))
  #str(teste_bancos.df)
  #teste_bancos.df[,6]<-as.numeric(teste_bancos.df[,6])
  
  #v0.8 2016.01.25
  #pegando somente as colunas que interessam:
  teste_bancos.df<-teste_bancos.df[,c(5,7,8,12,13,14,15)] # nao precisamos importar o nome, pois o arquivo jah eh bem gde
  #str(teste_bancos.df)
  
  #padronizando os nomes das colunas:
  teste5.df<-teste_bancos.df
  colnames(teste5.df)<-c("cpf","sem_ref","ano_ref", "total_div", "dias_atraso", "fase_SIAPI", "total_aplic")
  
  teste5.2.df<-teste5.df
  #str(teste5.2.df)  
  
  #CALCULO
  
  #v0.8 2016.01.25
  #primeiro compensa separar pelos que são legado e os que nao sao
  # depois separaremos por ano e fase
  
  
  #####1)LEGADO/ ANTIGOS sao todos ateh 2S/2009:
  #CAIXA NAO DIVIDIA EM FASES O LEGADO
  #.. COBRAVA UMA ÚNICA TAXA DE TODOS
  
  #BASE DOS > 360
  #entao tudo que tiver ano menor que 2009 eh legado
  legado1.df<-teste5.2.df[( teste5.2.df$ano_ref <= 2009 & is.na(teste5.2.df$ano_ref)==FALSE ), ]
  obs_legado <- nrow(legado1.df) # temos que exportar tb a informacao sobre as linhas
  #ok. ficamos soh com legado
  #OBS: muitas entradas do arquivo da caixa vem com sem_ref e ano_ref = NA ---> por isso temos que fazer o filtro
  #... com false
  
  #BASE DOS < 360
  legado1_contra.df<-legado1.df[legado1.df$dias_atraso <= 360, ]
  obs_legado_contra <- nrow(legado1_contra.df) # temos que exportar tb a informacao sobre as linhas
  
    # SUB COMPARACAO DO LEGADO 
  
    ### 1.1) CONTRATOS ATEH 1S/2016 (1.5%a.a)
  
    # > 360  
    legado1.1.df<-legado1.df[(legado1.df$ano_ref < 2006) | (legado1.df$ano_ref == 2006 & legado1.df$sem_ref == 1), ]
    # ok! lembrando que nesses casos soh utilizamos um operador & ou |
    obs_legado1.1 <- nrow(legado1.1.df) # temos que exportar tb a informacao sobre as linhas
    
    # < 360  
    legado1.1_contra.df<-legado1_contra.df[(legado1_contra.df$ano_ref < 2006) | (legado1_contra.df$ano_ref == 2006 & legado1_contra.df$sem_ref == 1), ]
    # ok! lembrando que nesses casos soh utilizamos um operador & ou |
    obs_legado1.1_contra <- nrow(legado1.1_contra.df) # temos que exportar tb a informacao sobre as linhas
    
    
    ##CALCULO dos 1.1:
    # considera-se devedor a partir do 61º dia (paragrafo 2º - Portaria 505 de 16abril2010 )
    
    # > 360:
    SDT1.1 = sum(legado1.1.df[,"total_div"])
    VRM1.1=( SDT1.1*(1.5/1200) )  
    
    
    # < 360
    
    SDT1.1_contra = sum(legado1.1_contra.df[,"total_div"])
    VRM1.1_contra=( SDT1.1_contra*(1.5/1200) )
    
    
    ###!!!!!! FLAVIO INFORMOU QUE A FORMULA NAO SE APLICA AO LEGADO!!!
    ### O LEGADO EH SOMENTE O SALDO TOTAL DA DIVIDA * TAXA
    ### FLAVIO INFORMOU QUE OS ACIMA DE 360 DIAS TB TEM DE SAIR DO LEGADO !!!! EH A BRIGA CONSTANTE ENTRE FNDE E CAIXA.
    ### vamos entao no arquivo de saida colocar dados 'com > 360' e 'sem > 360'
    
    #SDT1.1*(1.5/1200)
    #OK! DEU BEM PROXIMO!
    
    #SDT1.2*(2.6/1200)
    #OK! DEU BEM PROXIMO!
    
    ### 1.2) CONTRATOS APOS 1S/2016 (2.6%a.a)
    
    # >360:
    legado1.2.df<-legado1.df[!( (legado1.df$ano_ref < 2006) | (legado1.df$ano_ref == 2006 & legado1.df$sem_ref == 1) ), ]
    # COLOCAMOS O EXCLAMACAO NESTE CASO PARA PEGAR OS DIFERENTES
    #ok! a soma de observacoes do legado1.1 e 1.2 bate com o legado total
    # ok! lembrando que nesses casos soh utilizamos um operador & ou |
    obs_legado1.2 <- nrow(legado1.2.df) # temos que exportar tb a informacao sobre as linhas
    ##Calculo dos 1.2:
    # considera-se devedor a partir do 61º dia (paragrafo 2º - Portaria 505 de 16abril2010 )
    
    
    # < 360:
    legado1.2_contra.df<-legado1_contra.df[!( (legado1_contra.df$ano_ref < 2006) | (legado1_contra.df$ano_ref == 2006 & legado1_contra.df$sem_ref == 1) ), ]
    # COLOCAMOS O EXCLAMACAO NESTE CASO PARA PEGAR OS DIFERENTES
    #ok! a soma de observacoes do legado1.1 e 1.2 bate com o legado total
    # ok! lembrando que nesses casos soh utilizamos um operador & ou |
    obs_legado1.2_contra <- nrow(legado1.2_contra.df) # temos que exportar tb a informacao sobre as linhas
    
    
    ################## NAO CONSIDERAR ESTE TRECHO ABAIXO:#####################
    #SDI1.2 <- sum(legado1.2.df[(legado1.2.df[,"dias_atraso"] > 60) , "total_div"])
    #SDT1.2 = sum(legado1.2.df[,"total_aplic"])
    #VA1.2 = sum(legado1.2.df[ ,"total_div"])
    #VRM1.2=( SDT1.2*( 1-(SDI1.2/VA1.2) )* (2.6/1200) )  
    ####################################################################
    
    # CALculo (2.6%a.a)
      # > 360:
    SDT1.2 = sum(legado1.2.df[,"total_div"])
    VRM1.2=( SDT1.2*(2.6/1200) )  
    
    
      # < 360
    
    SDT1.2_contra = sum(legado1.2_contra.df[,"total_div"])
    VRM1.2_contra=( SDT1.2_contra*(2.6/1200) )
    
    
    
    # TOTALIZACAO:
      # > 360:
    VRMT_antigos<-VRM1.1+VRM1.2 # dá uns 300 mil de glosa
    
      # > 360:
    VRMT_antigos_contra<-VRM1.1_contra+VRM1.2_contra # dá uns 300 mil de glosa  
    
  
  
  
  resultadoprevia.df<-data.frame("Quant._Contratos-INCLUI acima 360"=c(obs_legado1.1, obs_legado1.2, obs_legado),
                                 "Quant._Contratos-RETIRA acima 360"=c(obs_legado1.1_contra, obs_legado1.2_contra, obs_legado_contra),
                                 "Valor_Total_Taxa-INCLUI acima 360"=c(VRM1.1, VRM1.2, VRMT_antigos),
                                 "Valor_Total_Taxa-RETIRA acima 360"=c(VRM1.1_contra, VRM1.2_contra, VRMT_antigos_contra) )
  
  row.names(resultadoprevia.df) <- c('antigos ate 30/06/2006', 'antigos entre 01/07/2006 e 14/01/2010', 'SUBTOTAL ANTIGOS')
  
  write.csv2(resultadoprevia.df, file=paste(dir3, Sys.Date(),"- PREVIA TAXA - CAIXA ANTIGOS.csv", sep=''), row.names = TRUE)
  # DIFERENTE DO BB, NA CAIXA A OPCAO ROW.NAMES TEM QUE SER TRUE
  
}
