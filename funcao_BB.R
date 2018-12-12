funcao_BB<-function(dti.loc, teste2_bancos.loc, mesbox_string, anobox_string, ...){
  # versão operacional estável antes da mudança de dez2017 (forma alternativa de cálculo - utiliz - 25R$; amortiz - 35R$).
  
  #v0.8 2016.01.26 -- criamos uma pasta para salvar os resultados das analises do dia e por banco
  dir1<- paste(getwd(), "analises", sep='/') # juntando a pasta analises jah criada
  dir2 <- paste(dir1, paste("BB", mesbox_string, anobox_string, Sys.Date(), sep=' - '), sep='/')
  dir3<- paste(dir2, "/", sep='') # nao consegui acrescentar a contrabarra no final utilizando uma unica linha de codigo
  # eh importante acrescentar essa contrabarra ao final para que possamos indicar a pasta corretamente ao gravar o .csv
  dir.create(path=dir3)
  
  
  dti.loc<- gtkFileChooserGetFilename(filename1)
  teste2_bancos.loc<-gtkFileChooserGetFilename(filename2)
  #perfeito! ...resolvi olhando o Help: ??gtkFileChooserGet
  #teste2_bancos.loc<-file.choose()
  #extr_bancos.df<-read.table(teste2_bancos.loc, header=FALSE, sep=";", dec=",", stringsAsFactors=FALSE, nrows=5) #extraindo poucoas linhas sao para pegar os 'modes' das variaveis
  #head.classes<-sapply(extr_bancos.df, mode) #extraindo as classes/modes do banco de dados de teste extr_bancos.df
  #head.classes[c(4,6)]<-"factor" #transformando as colunas de CNPJ das mantenedoras em fator para nao dar erro
  #head.classes[head.classes %in% "character"]<-"factor"  #transformando todas a svariaveis do tipo character em fator para nao dar erro
  #lendo base do BB
  #teste_bancos.df<-read.table(teste2_bancos.loc, header = TRUE, fill = TRUE, sep=';', dec=',', stringsAsFactors = FALSE)
  
  ##v0.7 - 10.01.2016
  #library(data.table)
  teste_bancos.dt<-fread(teste2_bancos.loc, dec=',')#, header = TRUE, fill = TRUE, sep=';', dec=',', stringsAsFactors = FALSE)
  #recebe os mesmos parametros de read.table
  # e tem tb a opcao de data.table show.progress
  ###PERFEITO!!! JAH FUNCIONA SOH COM ESSAS ALTERACOES!!!
  teste_bancos.df<-as.data.frame(teste_bancos.dt, dec=',') # POR DEFAULT O FREAD JAH EH COM STRINGSASFACTORS = FALSE
  #str(teste_bancos.df)
  #teste_bancos.df[,"CPF"] <- as.character(teste_bancos.df[,"CPF"])
  #ATENCAO: PARA O CPF, SE OS DOIS NAO ESTIVEREM EM NUMERIC, NAO FUNCIONA
  
  print("Dimensao Arquivo Banco (LINHASxCOLUNAS):" )
  print(dim(teste_bancos.df))
  #por padrao jah vem header = FALSE
  #str(teste_bancos.df)
  #lendo base da DTI
  #extr_dti.df<-read.table(dti.loc, header=TRUE, sep=";", dec=".", stringsAsFactors=FALSE, nrows=5) #extraindo poucoas linhas sao para pegar os 'modes' das variaveis
  # o da dti vem com cabeCalho -- temos de eliminar antes
  #jah esta com decimal de ponto
  #head.classes.dti<-sapply(extr_dti.df, mode) #extraindo as classes/modes do banco de dados de teste extr_bancos.df
  #head.classes.dti[head.classes.dti %in% "character"]<-"factor"  #transformando todas a svariaveis do tipo character em fator para nAOo dar erro
  # o ff nao aceita classe de character, sao como fator
  #dti.loc<-file.choose()
  #teste_dti.df<-read.table(dti.loc, header = TRUE, fill = TRUE, stringsAsFactors = FALSE, sep =';', dec='.')
  
  #v0.7 - 10.01.2016:
  #fazendo a barra mexer entre a leitura dos arquivos:
  #for(i in 1:50)
  #{
  #  gtkProgressBarPulse(bar)
  #  Sys.sleep(0.05)
  # com o Sys.sleep ela vai andando de um lado para o outro devagarinho
  #}
  
  # v0.9.6.4.1 - 2017 set 07
  ## necessidade de fill= TRUE nas novas versões de fread
  ## sol em: https://stackoverflow.com/questions/35518401/fread-eof-instead-of-separator/41242287#41242287
  
  #teste_dti.dt<-fread(dti.loc, dec='.') 
  
  teste_dti.dt<- tryCatch({
    fread(dti.loc, dec='.') 
    },
    error = function(e){
      fread(dti.loc, dec='.', fill=TRUE)     
    }
  )
  
  #rm(teste_dti.dt)
  
  #teste_dti.dt<-fread(dti.loc, dec='.', fill=TRUE)#, header = TRUE, fill = TRUE, sep=';', dec=',', stringsAsFactors = FALSE)
  # atencao, o decimal de dti eh ponto
  #recebe os mesmos parametros de read.table
  # e tem tb a opcao de data.table show.progress
  ###PERFEITO!!! JAH FUNCIONA SOH COM ESSAS ALTERACOES!!!
  teste_dti.df<-as.data.frame(teste_dti.dt, dec='.')
  #str(teste_dti.df)
  
  print("Dimensao Arquivo DTI (LINHASxCOLUNAS):" )
  print(dim(teste_dti.df))
  
  #v0.9.1: 05.03.2016
  # atribuindo/forcando os nomes das colunas ao df da DTI antes
  #... pq a DTI, as vezes, nao traz nomes das colunas para BB
  #.. eh uma forma de anteciparmos um erro que aconteceria na versao de fevereiro
  colnames(teste_dti.df)<-c("co_banco", "nu_cpf", "no_usuario", "nu_ano", "nu_mes",
                            "vl_liberado", "co_fase", "co_grupo_preliminar")
  
  
  #v0.9: 15.02.2016
  tryCatch({
    teste_dti.df[,"nu_cpf"]<-as.numeric(teste_dti.df[,"nu_cpf"])},
    error = function(e){
      teste_dti.df[,2]<-as.numeric(teste_dti.df[,2])    
    }
  )
  #str(teste_dti.df)
  # jah estah automaticamente trazendo como integer64 (numeric)
  
  
  #v0.7:
  #transformando as colunas sensiveis da DTI em numeric (primeira eh necessaria e demais sao precaucao):
  #teste_dti.df[,"nu_cpf"]<-as.numeric(teste_dti.df[,"nu_cpf"])
  
  teste_dti.df[,"vl_liberado"]<-as.numeric(teste_dti.df[,"vl_liberado"])
  
  teste_dti.df[,"co_fase"]<-as.numeric(teste_dti.df[,"co_fase"])
  
  teste_dti.df[,"co_grupo_preliminar"]<-as.numeric(teste_dti.df[,"co_grupo_preliminar"])
  
  
  
  ####v0.9.2: 10.05.2016 - retirando todos os cpf's repetidos da DTI e salvando num arquivo...
  # ... para que o fiscal informe à DTI
  #nrow(teste_dti.df[duplicated(teste_dti.df),])
  rep_cpf_dti.df<-teste_dti.df[duplicated(teste_dti.df[,"nu_cpf"]),]
  print("CPF's repetidos DTI:")
  print(nrow(rep_cpf_dti.df))
  # criando arquivo/ relatorio:
  write.csv2(rep_cpf_dti.df, file=paste(dir3, Sys.Date()," - CPFs reptidos DTI - BB.csv",
                                        sep=''), row.names = FALSE)
  # eliminando do arquivo da DTI (ficando só com os únicos)
  teste_dti1.2.df<-teste_dti.df[!duplicated(teste_dti.df[,'nu_cpf']) ,]
  #ok! retirou os 98
  
  
  #v0.7 - 10.01.2016:
  #fazendo a barra mexer entre a leitura dos arquivos:
  #for(i in 1:50)
  #{
  # gtkProgressBarPulse(bar)
  #  Sys.sleep(0.05)
  #  # com o Sys.sleep ela vai andando de um lado para o outro devagarinho
  #}
  
  
  #pegando somente as colunas que interessam:
  teste_bancos.df<-teste_bancos.df[,c(1,2,12,13,14,16,19)]
  #str(teste_bancos.df)
  
  teste_dti1.2.df<-teste_dti1.2.df[,c(2,3,6,7,8)]
  #str(teste_dti.df)
  
  ### comparacaoo 1 - CPF's - extraindo data.frame da primeira diferenCa
  #### só assim podemos usar %in% -- NAO DAH PARA USAR PARA SUBSTITUIR !!!
  
  verif_1<-teste_bancos.df[!teste_bancos.df[,2]%in%teste_dti1.2.df[,1], c(1,2)]
  colnames(verif_1)<-c("nome", "cpf")
  #exportando primeira verificacao -- lembrando que vai mandar para o working directory
  write.csv2(verif_1, file=paste(dir3, Sys.Date()," - divergencia CPF - BB.csv", sep=''), row.names = FALSE)
  #OBS: tiramos o 'sep=";" do write.csv para ele nao trazer mensagens de warning
  
  #retirando da base do banco os CPF's que constavam do BB, mas nao constavam da base da DTI (ou seja comparando com verif_1:
  
  teste5.df<-teste_bancos.df[!teste_bancos.df[,2] %in% verif_1[,2],]
  
  ###comaparacao 2 - fases:
  # anti_join com o pacote dplyr para ffbase2 --- jah sem as linhas dos cpfs que nao constavam
  
  #padronizando os nomes das colunas:
  colnames(teste5.df)<-c("nome", "cpf", "saldo_AI", "vl_lib", "fase", "dias_atraso", "saldo_inad")
  colnames(teste_dti1.2.df)<-c("cpf", "nome", "vl_lib", "fase", "gp_prelim")
  
  
  
  #v0.9.2 - 10.05.2016: bug - erro de algoritmo - precisavamos tirar os cpfs para
  # ... que nao fossem comparados no banco tb
  # criando indice com os cpfs em fase 4 ou 0 na dti
  ind.fase4_e_0.dti<-teste_dti1.2.df[teste_dti1.2.df$fase == 4 | teste_dti1.2.df$fase == 0, "cpf"]
  # length(ind.fase4_e_0.dti)
  
  # soh precisamos retirar das divergencias ao final de cada comparacao (para fase e vl lib)
  
  
  
  #NOVO v0_3:
  ####NA VERIF 2, VAMOS ACRESCENTAR A V13 TB, QUE EH O VALOR LIBERADO
  ####MAS ANTES TEREMOS DE DEFINIR COM CERTEZA QUAIS SAO AS COLUNAS EXATAS DA DTI
  ### lembrar que os calculos FINAIS SEMPRE TEM DE SER FEITOS COM OS QUE SOBRARAM NO ARQUIVO DO BANCO
  #### TEREMOS QUE FAZER A ALTERACAO TB DA QUESTAO DOS PRELIMINARES
  verif_2<-anti_join(teste5.df[,c("nome","cpf","fase","vl_lib")], teste_dti1.2.df, by=c("fase", "cpf")) # vamos trazer tudo para depois poder juntar no consolidado
  
  #mostrando a diferenca entre fase banco(x) e fase dti(y):
  # o segredo estava em usar left join ao inves do %in%
  verif_2.2<-verif_2
  verif_2.2<-left_join(verif_2, teste_dti1.2.df, by = ("cpf")) # no help da left join explica (todas linhas e colunas de x com todas as colunas de y)
  verif_2.2<-verif_2.2[,c(1,2,3,7,8)]
  #ajustando os nomes para apresentacao:
  colnames(verif_2.2)<-c("nome", "cpf", "fase_banco", "fase_dti", "gp_prelim")
  
  
  #v 0.9.2
  # retirar da comparação divergencia aqueles com fase 4 e 0 na DTI
  #... utilizando o indice lá de cima
  verif_2.2<-verif_2.2[!verif_2.2[,1] %in% ind.fase4_e_0.dti, ]
  
  
  verif_2.3<-verif_2.2[,-5]
  #aqui tanto faz se os preliminares sao 0 ou 1 pq isso soh influencia no valor liberado
  #para 2016 depois vamos trabalhar sem essa coluna na analise
  #exportando primeira verificacao -- lembrando que vai mandar para o working directory
  #OBS: !!!! mais pra frente, a ideia eh exportar com os nomes das variaveis
  write.csv2(verif_2.3, file=paste(dir3, Sys.Date()," - divergencia fase(geral) - BB.csv", sep=''), row.names = FALSE)
  ####
  
  ##novo v0_6 - verificacao qualificada da fase (c/ Impacto)
  #criando nova coluna:
  v2_qualif.df<-verif_2.3
  v2_qualif.df[,"media_fases"]<- with(v2_qualif.df, (fase_banco + fase_dti)/2)
  #todos os que tiverem media das fases != 1.5 devem constar como divergencia de fases
  # depois verificar os casos para fases 4 e 3 - se tambem devem sair
  # nos casos de fase 4 e 3 , a media eh 3.5
  # no futuro, para casos em que a fase da DTI estiver mais avancada, podemos ao inves de somar, subtrair um pelo outro
  #.. e dividir e ver quais sao negativos
  
  v2_qualif1.df<-v2_qualif.df
  v2_qualif1.df<-v2_qualif.df[v2_qualif.df$media_fases!=1.5,]
  #ok! o resultado sao os que devem sair
  
  
  ### v.0.7.1 2016.01.22
  # eliminar os que apresentam fase - 4 ou 7 no banco (solicitacao do Paulo) -- nao devem ser comparados:
  v2_qualif1.df<-v2_qualif1.df[v2_qualif1.df$fase_banco!=4 & v2_qualif1.df$fase_banco!=7,]
  # aqui entra so um & comercial -- com dois & comercial nao funciona nesse caso
  
  # retirando a coluna media das fases para apresentacao:
  v2_qualif1.df<-v2_qualif1.df[,-5]
  
  
  
  ## (!) v0.9.3 - 11.06.2017:
  #teste5.1.df<-teste5.df[!teste5.df[,"cpf"] %in% v2_qualif1.df[,"cpf"],] # eliminando as linhas com fases inconsistentes do arquivo do BB
  # o que antes eliminávamos, agora será pago o incontroverso, ou seja, a fase da DTI:
  
  # fazendo uns testes antes:
  #teste <- data.frame(sort(v2_qualif1.df[,"nome"]))
  #teste5.df[teste5.df[,"cpf"] %in% v2_qualif1.df[,"cpf"],"nome"] <- v2_qualif1.df[,4]
  # ok! o numero de linhas e os nomes batem, então está indexando direito com o %in%
  
  # onde houve divergências, recebe os valores da DTI
  # precisamos dos índices nessa nova operação:
  
  #help(sort)
  #v2_qualif2.df <- v2_qualif1.df[order(v2_qualif1.df[,"nome"]),]
  # precisamos ordenar antes
  
  teste5.1.df<-teste5.df
  # teste5.1.df[teste5.1.df[,"cpf"] %in% v2_qualif2.df[,"cpf"],5] <- 
  #   v2_qualif2.df[rownames(v2_qualif2.df),4]
  # teste5.1.df[teste5.1.df[,"cpf"] %in% v2_qualif2.df[,"cpf"],"fase"] <-
  #   v2_qualif2.df[,4]
  # só funciona ordenando. O mais acima tb funciona
  
  
  #mais robusto:
  teste5.1.df <- left_join(teste5.1.df, v2_qualif1.df[, c("cpf","fase_dti")], by="cpf",  sort=FALSE)
  
  teste5.1.df[,"fase"] <- ifelse(!is.na(teste5.1.df[,"fase_dti"]), teste5.1.df[,"fase_dti"], teste5.1.df[,"fase"] )
  
  
  
  
  #ok!
  
  #exportando a verificacao qualificada de fase(com impacto):
  write.csv2(v2_qualif1.df, file=paste(dir3, Sys.Date()," - divergencia fase(com impacto) - BB.csv", sep=''), row.names = FALSE)
  
  ## TESTANDO VALOR LIBERADO:
  ## EH MELHOR SOH EXCLUIR OS QUE FICAM ACIMA DO DA DTI, POIS IMPACTAM EM UMA TAXA MAIOR
  ##O BANCO QUE SE VIRE SE NAO ESTAH CALCULANDO DIREITO OS OUTROS
  verif_3<-anti_join(teste5.1.df[,c("nome", "cpf", "fase", "vl_lib")], teste_dti1.2.df, by=c("cpf", "vl_lib"))
  verif_3.2<-verif_3
  verif_3.2<-left_join(verif_3, teste_dti1.2.df, by = ("cpf")) #para trazer as demais colunas
  verif_3.3<-verif_3.2[,c(1,2,4,6,8)]
  colnames(verif_3.3)<-c("nome", "cpf", "vl_lib_banco", "vl_lib_dti", "gp_prelim")
  
  #ficando soh com os preliminares ==0 ### pode ser que tenha um grupo preliminar =2 alem de 1, por isso queremos soh zero
  verif_3.4<-verif_3.3
  #with(verif_3.3, gp_prelim ==1)
  verif_3.4<-verif_3.4[verif_3.4$gp_prelim == 0,] # boa tecnica para deletar, havia me esquecido
  #em:http://stackoverflow.com/questions/8005154/conditionally-remove-dataframe-rows-with-r
  
  #v 0.9.2
  # retirar da comparação divergencia aqueles com fase 4 e 0 na DTI
  #... utilizando o indice lá de cima
  verif_3.4<-verif_3.4[!verif_3.4[,1] %in% ind.fase4_e_0.dti, ]
  
  
  
  #RETIRANDO OS DO BANCO QUE ESTAO ACIMA DA DTI:
  verif_3.5<-verif_3.4
  verif_3.5<-verif_3.5[verif_3.5$vl_lib_banco > verif_3.5$vl_lib_dti,]
  #PODERIAMOS AINDA TIRAR SOMENTE OS QUE ESSES VALORES ULTRAPASSAM POR EXEMPLO 100 REAIS
  
  #NOVO v0_6:
  # retirando as diferencas acima de R$ 1 (um real) tambem:
  verif_3.6_qualif<-verif_3.5
  verif_3.6_qualif[,"dif_vl_lib"]<-with(verif_3.6_qualif, vl_lib_banco - vl_lib_dti)
  verif_3.6_qualif<-verif_3.6_qualif[verif_3.6_qualif$dif_vl_lib > 1,]
  
  
  verif_3.7_qualif<-verif_3.6_qualif[,-c(5,6)] #limpando para exportacao - tirando gp_prelim e dif_vl_lib
  
  #OBS: !!!! mais pra frente, a ideia eh exportar com os nomes das variaveis
  # aqui vamos exportar a verificacao qualificada (pelos vl_lib acima de 1R$) mesmo
  write.csv2(verif_3.7_qualif, file=paste(dir3, Sys.Date()," - divergencia valor liberado(com impacto) - BB.csv", sep=''), row.names = FALSE)
  
  
  ## (!) v0.9.3 - 11.06.2017:
  # o que antes eliminávamos, agora será pago o incontroverso, ou seja, a fase da DTI:
  #teste5.2.df<-teste5.1.df[!teste5.1.df[,"cpf"] %in% verif_3.7_qualif[,"cpf"],] # eliminando as linhas com fases inconsistentes do arquivo do BB
  
  
  #help(sort)
  #verif_3.7_qualif2 <- verif_3.7_qualif[order(verif_3.7_qualif[,"nome"]),]
  # precisamos ordenar antes
  
  teste5.2.df<-teste5.1.df
  
  # v0.9.6.4.1 2017 set 09
  # teste5.2.df$novo_vl_lib <- NA
  # teste5.2.df[ rownames(teste5.2.df) %in% rownames(verif_3.7_qualif), "vl_lib"]  <- verif_3.7_qualif2[,4]
  # # só funciona com rownames(). !!! Dá bug sem !!
  # 
  # nrow(verif_3.7_qualif)
  
  # melhor usar o left join do dplyr !!
  # verif_3.7_qualif2 <- verif_3.7_qualif
  # colnames(verif_3.7_qualif2)[4] <- c("vl_lib")
  
  #mais robusto:
  teste5.2.df <- left_join(teste5.2.df, verif_3.7_qualif[, c(2,4)], by="cpf",  sort=FALSE)
  
  teste5.2.df[,"vl_lib"] <- ifelse(!is.na(teste5.2.df[,"vl_lib_dti"]), teste5.2.df[,"vl_lib_dti"], teste5.2.df[,"vl_lib"] )
  
  
  
  ###EXPORTANDO A PLANILHA PENDENCIAS CONSOLIDADA:
  pendencias <-left_join(rbind(verif_2, verif_3), teste_dti1.2.df, by = "cpf") 
  # esse pendencias pega inclusive os valores em que DTI > banco para vl. liberado --- soh para controle do fiscal
  
  
  pendencias2<-pendencias
  colnames(pendencias2)<-c("nome_banco", "cpf", "fase_banco", "vl_lib_banco", "nome_dti", "vl_lib_dti", "fase_dti", "gp_prelim")
  #exportando
  write.csv2(pendencias2, file=paste(dir3, Sys.Date()," - pendencias gerais(vl_lib e fase) - BB.csv", sep=''), row.names = FALSE)
  
  
  #CALCULO
  #separando por fases:
  teste6.df<-subset(teste5.2.df, fase<3) # utilizacao e carencia
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
  resultadosteste.df<-data.frame("SDT1"=SDT1, "SDI1"=SDI1, "VA1"=VA1, "VRM1"=VRM1,
                                 "SDI2"=SDI2, "SDT2"=SDT2, "VA2"=VA2, "VRM2"=VRM2, "VRMT"=VRMT,
                                "qte_contratos_utiliz" = n_util, "qte_contratos_amortiz" = n_amort,
                                 "_valor_alternativo_utiliz_" = n_util*25, "_valor_alternativo_amort_" = n_amort*35,
                                "_valor_alternativo_total_" = (n_util*25) +  (n_amort*35) )
  write.csv2(resultadosteste.df, file=paste(dir3, Sys.Date()," - RESULTADO FINAL TAXA - BB.csv", sep=''), row.names = FALSE)
  
  #ARQUIVO DE LOG:
  #necessidade de colocar "/n", para que ele pule linha no write.table
  nome_data_arq <- paste("********Arquivo Log de Fiscalizacao de Fatura********", Sys.Date(), "\n", sep=' ')
  arq_banco <- paste("Arquivo Banco analisado:", teste2_bancos.loc, "\n", sep=' ')
  arq_DTI<- paste("Arquivo DTI analisado:", dti.loc,"\n", sep=' ')
  qte_total_pendencias_impacto<- paste("Quantidade Total Pendencias com Impacto na TAXA:", nrow(verif_1)+nrow(v2_qualif1.df)+nrow(verif_3.7_qualif),"\n", sep=' ')
  qte_div_cpf<- paste("Quantidade divergencias em CPF:", nrow(verif_1),"\n", sep=' ')
  qte_div_fase<-paste("Quantidade divergencias em FASE (c/ Impacto):", nrow(v2_qualif1.df),"\n", sep=' ')
  qte_div_vl_lib<-paste("Quantidade divergencias em VALOR LIBERADO (c/ Impacto):", nrow(verif_3.7_qualif),"\n", sep=' ')
  
  versao_fisc <- paste("Fiscaliza Fatura FIES/FNDE v0.9 - Desenvolvido por: Allan Vieira de C. Quadros","\n", sep=' ')
  versao_os_R <- paste(R.Version()[['version.string']],"-",Sys.info()['sysname'],Sys.info()['release'],"\n", sep=' ')
  
  contato<- paste("http://www.fnde.gov.br", "e-mail: allan.quadros@fnde.gov.br", sep ='  ') # 2 espacos
  
  log_df<-list(nome_data_arq, arq_banco, arq_DTI, qte_total_pendencias_impacto, qte_div_cpf,
               qte_div_fase, qte_div_vl_lib, versao_fisc,versao_os_R, contato)
  
  
  write.table(log_df, file=paste(dir3, Sys.Date()," - log FISCAL - BB.txt", sep=''), col.names = FALSE, row.names = FALSE)
}
# versão operacional estável antes da mudança de dez2017 (forma alternativa de cálculo - utiliz - 25R$; amortiz - 35R$).
