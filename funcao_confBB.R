funcao_confBB<-function(teste2_bancos.loc, mesbox_string, anobox_string, ...){ #soh recebe o arquivo do banco como parametro
  
  #v0.8 2016.01.26 -- criamos uma pasta para salvar os resultados das analises do dia e por banco
  dir1<- paste(getwd(), "analises", sep='/') # juntando a pasta analises jah criada
  dir2 <- paste(dir1, paste("BB", mesbox_string, anobox_string, Sys.Date(), sep=' - '), sep='/')
  dir3<- paste(dir2, "/", sep='') # nao consegui acrescentar a contrabarra no final utilizando uma unica linha de codigo
  # eh importante acrescentar essa contrabarra ao final para que possamos indicar a pasta corretamente ao gravar o .csv
  dir.create(path=dir3)
  

    
  teste2_bancos.loc<-gtkFileChooserGetFilename(filename2)
  #teste2_bancos.loc<-file.choose()
  teste_bancos.dt<-fread(teste2_bancos.loc, dec=',')#, header = TRUE, fill = TRUE, sep=';', dec=',', stringsAsFactors = FALSE)
  #recebe os mesmos parAmetros de read.table
  # e tem tb a opcaoo de data.table show.progress
  ###PERFEITO!!! JAH FUNCIONA SOH COM ESSAS ALTERAcoES!!!
  teste_bancos.df<-as.data.frame(teste_bancos.dt, stringsAsFactors = FALSE, dec=',')
  print("Dimensao Arquivo Banco (LINHASxCOLUNAS):" )
  print(dim(teste_bancos.df))
  #str(teste_bancos.df)
  #teste_bancos.df[,6]<-as.numeric(teste_bancos.df[,6])
  
  #pegando somente as colunas que interessam:
  teste_bancos.df<-teste_bancos.df[,c(1,2,12,13,14,16,19)]
  #str(teste_bancos.df)
  
  #padronizando os nomes das colunas:
  teste5.df<-teste_bancos.df
  colnames(teste5.df)<-c("nome", "cpf", "saldo_AI", "vl_lib", "fase", "dias_atraso", "saldo_inad")
  
  teste5.2.df<-teste5.df
    #CALCULO
  #separando por fases:
  teste6.df<-subset(teste5.2.df, fase<3) # utilizaCAo e carencia
  teste7.df<-subset(teste5.2.df, fase==3) #amortizacao
  #ficando somente com os abaixo ou igual a 360 dias de atraso (retirando os acima de 360)
  #OBS:poderia colocar junto com o subset de cima
  teste6.df<-subset(teste6.df, dias_atraso<=360)   
  teste7.df<-subset(teste7.df, dias_atraso<=360)
  
  # dez_2017
  # nro_obs:
  n_util <- nrow(teste6.df)
  n_amort <- nrow(teste7.df)
  
  #VRM1 (FASES UTILIZACAO E CARENCIA):
  SDI1 <- sum(teste6.df[(teste6.df[,"dias_atraso"] > 60) , "saldo_inad"])
  SDT1 = sum(teste6.df[,"saldo_AI"])
  VA1 = sum(teste6.df[ ,"vl_lib"])
  VRM1=( SDT1*( 1-(SDI1/VA1) )* (1.5/1200) )
  #VRM2 (FASE DE AMORTIZACAO):
  SDI2 <- sum(teste7.df[(teste7.df[,"dias_atraso"] > 60) , "saldo_inad"])
  SDT2 = sum(teste7.df[ ,"saldo_AI"])
  VA2 = sum(teste7.df[,"vl_lib"])
  VRM2=(SDT2*( 1-(SDI2/VA2) )* (2/1200) )
  VRMT = VRM1 + VRM2
  resultadoprevia.df<-data.frame("SDT1"=SDT1, "SDI1"=SDI1, "VA1"=VA1, "VRM1"=VRM1,
                                 "SDI2"=SDI2, "SDT2"=SDT2, "VA2"=VA2, "VRM2"=VRM2, "VRMT"=VRMT,
                                "qte_contratos_utiliz" = n_util, "qte_contratos_amortiz" = n_amort,
                               "_valor_alternativo_utiliz_" = n_util*25, "_valor_alternativo_amort_" = n_amort*35,
                               "_valor_alternativo_total_" = (n_util*25) +  (n_amort*35))
  
  
  write.csv2(resultadoprevia.df, file=paste(dir3, Sys.Date()," - PREVIA TAXA - BB.csv", sep=''), row.names = FALSE)
  
}
