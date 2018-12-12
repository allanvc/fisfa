funcao_confCAIXA_NOVOS<-function(teste2_bancos.loc, mesbox, anobox, ...){ #soh recebe o arquivo do banco como parametro
  
  #v0.8 2016.01.26 -- criamos uma pasta para salvar os resultados das analises do dia e por banco
  dir1<- paste(getwd(), "analises", sep='/') # juntando a pasta analises jah criada
  dir2 <- paste(dir1, paste("CAIXA NOVOS", mesbox, anobox, Sys.Date(), sep=' - '), sep='/')
  dir3<- paste(dir2, "/", sep='') # nao consegui acrescentar a contrabarra no final utilizando uma unica linha de codigo
  # eh importante acrescentar essa contrabarra ao final para que possamos indicar a pasta corretamente ao gravar o .csv
  dir.create(path=dir3)
  
  
  teste2_bancos.loc<-gtkFileChooserGetFilename(filename2)
  #library(data.table)
  #teste2_bancos.loc<-file.choose()
  teste_bancos.dt<-fread(teste2_bancos.loc, dec=',')#, header = TRUE, fill = TRUE, sep=';', dec=',', stringsAsFactors = FALSE)
  #recebe os mesmos parAmetros de read.table
  # e tem tb a opcao de data.table show.progress
  ###PERFEITO!!! JAH FUNCIONA SOH COM ESSAS ALTERACOES!!!
  teste_bancos.df<-as.data.frame(teste_bancos.dt, stringsAsFactors = FALSE, dec=',')
  
  # v0.9.6.5 -- cleaning
    rm(teste_bancos.dt)
  
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
  
  
  
  #####2)CONTRATOS NOVOS todos APOS 2S/2009:
  #obs: NAO HA DIVISAO ENTRE UTILIZACAO E CARENCIA
  novos.df<-teste5.2.df[!( teste5.2.df$ano_ref <= 2009 & is.na(teste5.2.df$ano_ref)==FALSE ), ]
  # coloquei o ! para trazer todos os que nao sao legado (a regra eh a mesma do legado soh que com exclamacao):
  
  # FICANDO SOH COM OS ABAIXO DE 360 DIAS PARA O GERAL -- IGUAL NO BANCO DO BRASIL:
  novos2.df<-subset( novos.df, dias_atraso<=360 )
  # aparecem pouco menos de 100 contratos a mais do que mostra a CAIXA
  
  obs_novos2 <- nrow(novos2.df) # temos que exportar tb a informacao sobre as linhas  
  
    # SUB COMPARACAO DOS NOVOS 
    
    ### 2.1) CONTRATOS NOVOS EM UTILIZACAO (2%a.a)  
    # 10.05.2016 - v0.9_1 - acrescentado fase 0035 como sendo fase de amortizacao
    novos2.1.df<-subset( novos2.df, !(fase_SIAPI=='0032' | fase_SIAPI=='0033' | fase_SIAPI=='0035')  )
    # eh soh usar exclamacao para negar os da outra fase
    # o numero estah proximo
    obs_novos2.1 <- nrow(novos2.1.df)
    
    ## CÁLCULO:
    #VRM2.1 (FASES UTILIZACAO E CARENCIA):
    SDI2.1 = sum(novos2.1.df[(novos2.1.df[,"dias_atraso"] > 60) , "total_div"])
    SDT2.1 = sum(novos2.1.df[,"total_div"])
    VA2.1 = sum(novos2.1.df[ ,"total_aplic"])
    VRM2.1 =( SDT2.1*( 1-(SDI2.1/VA2.1) )* (1.5/1200) )
    # ok! deu bastante proximo!!! um pouco a mais que na CAIXA
    
    ### 2.2) CONTRATOS NOVOS EM AMORTIZACAO (1.5%a.a)  
    
    ## v.0.9.6.5 -- 2018.12.11 - problema de back compatibility do R (data.table) com leitura
    #... da coluna fase_SIAPI -- data.table passou a ler como numeric
    # (sugestao Giva)
    if (as.numeric(as.character(R.Version()$minor)) >= 5) {
      novos2.2.df <-
        subset(novos2.df,
               (fase_SIAPI == 0032 | fase_SIAPI == 0033 | fase_SIAPI == 0035))
      # eh soh usar exclamacao para negar os da outra fase
      # o numero estah proximo
      
    } else {
      novos2.2.df <-
        subset(novos2.df,
               (
                 fase_SIAPI == '0032' | fase_SIAPI == '0033' | fase_SIAPI == '0035'
               ))
      # eh soh usar exclamacao para negar os da outra fase
      # o numero estah proximo
      
    }
    # eh soh usar exclamacao para negar os da outra fase
    # o numero estah proximo
    obs_novos2.2 <- nrow(novos2.2.df)
    
    #VRM2 (FASE DE AMORTIZACAO):
    SDI2.2 = sum(novos2.2.df[(novos2.2.df[,"dias_atraso"] > 60) , "total_div"])
    SDT2.2 = sum(novos2.2.df[,"total_div"])
    VA2.2 = sum(novos2.2.df[ ,"total_aplic"])
    VRM2.2 =( SDT2.2*( 1-(SDI2.2/VA2.2) )* (2/1200) )
    # tb deu bem proximo o valor -- um pouco a mais que na CAIXA
    
    
    VRMT_novos = VRM2.1 + VRM2.2
    
  
  # a forma de apresentacao segue o formato das faturas, por isso a da CAIXA serah um pouco diferente...
  #... da do Banco do Brasil
  
  
  resultadoprevia.df<-data.frame( "Quant._Contratos"=c(obs_novos2.1, obs_novos2.2, obs_novos2),
                                 "Valor_Total_Taxa"=c(VRM2.1, VRM2.2, VRMT_novos),
                                "_valor_alternativo_" = c(obs_novos2.1*25, obs_novos2.2*35, (obs_novos2.1*25) + (obs_novos2.2*35) ))
  
  row.names(resultadoprevia.df) <- c('novos - utiliz. e carencia', 'novos - amortizacao', 'SUBTOTAL NOVOS')
  
  write.csv2(resultadoprevia.df, file=paste(dir3, Sys.Date(),"- PREVIA TAXA - CAIXA NOVOS.csv", sep=''), row.names = TRUE)
  # DIFERENTE DO BB, NA CAIXA A OPCAO ROW.NAMES TEM QUE SER TRUE
  
}
