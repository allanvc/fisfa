funcao_CAIXA_NOVOS<-function(dti.loc, teste2_bancos.loc, mesbox_string, anobox_string, ...){
  # versão operacional estável antes da mudança de dez2017 (forma alternativa de cálculo - utiliz - 25R$; amortiz - 35R$).
  
  #v0.8 2016.01.26 -- criamos uma pasta para salvar os resultados das analises do dia e por banco
  dir1<- paste(getwd(), "analises", sep='/') # juntando a pasta analises jah criada
  dir2 <- paste(dir1, paste("CAIXA NOVOS", mesbox_string, anobox_string, Sys.Date(), sep=' - '), sep='/')
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
  #library(bit64)
  #library(dplyr)
  teste_bancos.dt<-fread(teste2_bancos.loc, dec=',')#, header = TRUE, fill = TRUE, sep=';', dec=',', stringsAsFactors = FALSE)
  #recebe os mesmos parametros de read.table
  # e tem tb a opcao de data.table show.progress
  ###PERFEITO!!! JAH FUNCIONA SOH COM ESSAS ALTERACOES!!!
  teste_bancos.df<-as.data.frame(teste_bancos.dt, dec=',') # POR DEFAULT O FREAD JAH EH COM STRINGSASFACTORS = FALSE
  #str(teste_bancos.dt)
  #teste_bancos.df[,"CPF"] <- as.character(teste_bancos.df[,"CPF"])
  #ATENCAO: PARA O CPF, SE OS DOIS NAO ESTIVEREM EM NUMERIC, NAO FUNCIONA
  
  # v0.9.6.5 -- cleaning
    rm(teste_bancos.dt)
  
  
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
  
  
  
  ########## v0.9: 19.02.2016
  ##### ATENCAO!!!! O ARQUIVO DA DTI NA CAIXA ESTAH VINDO COMO "," E NAO "." NO
  ##VL.LIBERADO. ... ENTAO VAMOS IMPLEMENTAR A SUBVIRG E SUBPONTO COM AQUELE GREP
  ### (FUNCAO DO APP DE COMPARAR PLAILHAS) PARA FORTALECER A ANALISE E DIMINUIR CHANCE DE ERROS
  
  ### por enquanto soh subvirg e subponto bastarah, pois nenhum data.frame vem com padrao
  ###. 111,3383.00 ou 1.123.234,88
  ### soh vem no padrao neutro: 37373.00 ou 383839,00
  
  ## na verdade, soh precisariamos da subvirg no caso da dti-caixa
  
  subvirg <- function (v){
    gsub(',','.',v)
    return(v)
  }
  
  subponto <- function (v){
    gsub('\\.','',v)
    return(v)
  }
  
  
  ############ PARECE QUE NAO ESTAH NAO, MAS EH MELHOR GARANTIR
  
  #library(bit64)
  #dti.loc<-file.choose()
  ## v0.9.6.4.1 - 2017 set 07
  ## necessidade de fill= TRUE nas novas versões de fread (por conta de espaços em branco antes de entradas
  ## sol em: https://stackoverflow.com/questions/35518401/fread-eof-instead-of-separator/41242287#41242287
  
   
  #teste_dti.dt<-fread(dti.loc, dec='.') 
  
  teste_dti.dt<- tryCatch({
    fread(dti.loc, dec='.') 
    },
    error = function(e){
      fread(dti.loc, dec='.', fill=TRUE)     
    }
  ) 
  
  # atencao, o decimal de dti eh ponto
  #recebe os mesmos parametros de read.table
  # e tem tb a opcao de data.table show.progress
  ###PERFEITO!!! JAH FUNCIONA SOH COM ESSAS ALTERACOES!!!
  teste_dti.df<-as.data.frame(teste_dti.dt, dec=',')
  #str(teste_dti.df)
  
  print("Dimensao Arquivo DTI (LINHASxCOLUNAS):" )
  print(dim(teste_dti.df))
  
  
  #v0.9: 15.02.2016
  # atribuindo os nomes das colunas ao df da DTI antes
  #... pq a DTI nao trouxe nomes das colunas para CAIXA
  colnames(teste_dti.df)<-c("co_banco", "nu_cpf", "no_usuario", "nu_ano", "nu_mes",
                            "vl_liberado", "co_fase", "co_grupo_preliminar")
  #str(teste_dti.df)
  # algumas colunas nem vamos usar, mas eh uma forma de diminuir chances
  #... de erros, caso a DTI nao envie
  
  #v0.7:
  #transformando as colunas sensiveis da DTI em numeric (primeira eh necessaria e demais sao precaucao):
  
  #v0.9: 15.02.2016
  tryCatch({
    teste_dti.df[,"nu_cpf"]<-as.numeric(teste_dti.df[,"nu_cpf"])},
    error = function(e){
      teste_dti.df[,2]<-as.numeric(teste_dti.df[,2])    
    }
  )
  #str(teste_dti.df)
  # jah estah automaticamente trazendo como integer64 (numeric)
  
  
  ####v0.9: 19.02.2016
  # RETIRANDO POSSIVEIS VIRGULAS DO DECIMAL:
  #primeiro- forcar como character:
  teste_dti.df[,"vl_liberado"]<-as.character(teste_dti.df[,"vl_liberado"])
  
  #segundo- aplicar subvirg:
  teste_dti.df[,"vl_liberado"]<-subvirg(teste_dti.df[,"vl_liberado"])
  
  #### agora estamos prontos para fazer as.numeric:
  
  
  
  
  teste_dti.df[,"vl_liberado"]<-as.numeric(teste_dti.df[,"vl_liberado"])
  #str(teste_dti.df)
  teste_dti.df[,"co_fase"]<-as.numeric(teste_dti.df[,"co_fase"])
  
  teste_dti.df[,"co_grupo_preliminar"]<-as.numeric(teste_dti.df[,"co_grupo_preliminar"])
  
  
  ####v0.9.2: 10.05.2016 - retirando todos os cpf's repetidos da DTI e salvando num arquivo...
  # ... para que o fiscal informe ??? DTI
  #nrow(teste_dti.df[duplicated(teste_dti.df),])
  rep_cpf_dti.df<-teste_dti.df[duplicated(teste_dti.df[,"nu_cpf"]),]
  print("CPF's repetidos DTI:")
  print(nrow(rep_cpf_dti.df))
  
  # criando arquivo/ relatorio:
  write.csv2(rep_cpf_dti.df, file=paste(dir3, Sys.Date()," - CPFs reptidos DTI - CAIXA NOVOS.csv", 
                                        sep=''), row.names = FALSE)
  
  # eliminando do arquivo da DTI (ficando s??? com os ???nicos)
  teste_dti1.2.df<-teste_dti.df[!duplicated(teste_dti.df[,'nu_cpf']) ,]
  #ok! retirou os 98
  
  
  
  #teste_dti1.2.df<-unique(teste_dti.df[,"nu_cpf"])
  
  
  ##########################!!!!!!!!!!!!!!!!!!!!!
  # V0.8 2016.01.26
  ### REPLICAR A LEITURA DOS BANCOS PARA A DTI ENQUANTO NAO TEMOS O ARQUIVO:
  ##################################
  
  
  #v0.7 - 10.01.2016:
  #fazendo a barra mexer entre a leitura dos arquivos:
  #for(i in 1:50)
  #{
  # gtkProgressBarPulse(bar)
  #  Sys.sleep(0.05)
  #  # com o Sys.sleep ela vai andando de um lado para o outro devagarinho
  #}
  
  
  #pegando somente as colunas que interessam:
  #pegando somente as colunas que interessam:
  teste_bancos2.df<-teste_bancos.df[,c(5,6,7,8,12,13,14,15)] # NAO PRECISAMOS MAIS DA QTE SEMESTRES
  #str(teste_bancos.df)
  
  teste_dti1.2.df<-teste_dti1.2.df[,c(2,3,6,7,8)]
  #str(teste_dti.df)
  
  
  
  
  #v0.8 2016.01.25
  
  # AQUI EH MELHOR TRAZERMOS O NOME TB (coluna 6)
  # TB TEMOS DE TRAZER A QTE DE SEMESTRE FINANCIADOS PARA TRABALHAR NO LEGADO
  #str(teste_bancos.df)
  
  
  
  
  #padronizando os nomes das colunas:
  teste5.df<-teste_bancos2.df
  colnames(teste5.df)<-c("cpf","nome","sem_ref","ano_ref", "total_div", "dias_atraso", "fase_SIAPI", "total_aplic")
  
  teste5.2.df<-teste5.df
  #str(teste5.2.df)  
  
  ###1) SEPARACAO DA BASE BANCO
  
  ####CONTRATOS NOVOS todos APOS 2S/2009:
  #obs: NAO HA DIVISAO ENTRE UTILIZACAO E CARENCIA
  novos.df<-teste5.2.df[!( teste5.2.df$ano_ref <= 2009 & is.na(teste5.2.df$ano_ref)==FALSE ), ]
  # coloquei o ! para trazer todos os que nao sao legado (a regra eh a mesma do legado soh que com exclamacao):
  
  # retirando as colunas de ano_ref e sem_ref que soh servem para dizer quem eh contrato novo e quem eh legado para ...
  # ... aumentar a velocidade de processamento
  novos2.df<-novos.df[,c(-3,-4)]
  
  #### FICANDO SOH COM OS <= DE 360 DIAS PARA O GERAL -- IGUAL NO BANCO DO BRASIL:
  novos_geral.df<-subset( novos2.df, dias_atraso<=360 )
  
  obs_novos_geral <- nrow(novos_geral.df) # temos que exportar tb a informacao sobre as linhas  
  
  #*******########## até aqui ok!
  #str(novos_geral.df)
  
  
  ###2) CRUZAMENTO CPF
  
  # o cpf da caixa jah vem como numeric entao estah OK para comparar
  ### comparacaoo 1 - CPF's - extraindo data.frame da primeira diferenCa
  novos_div_cpf<-novos_geral.df[!novos_geral.df[,1]%in%teste_dti1.2.df[,1], c(1,2)] # vai trazer soh as colunas cpf e nome
  # na caixa o cpf eh a primeira coluna
  colnames(novos_div_cpf)<-c("cpf", "nome")
  
  #exportando primeira verificacao -- lembrando que vai mandar para o working directory
  write.csv2(novos_div_cpf, file=paste(dir3, Sys.Date()," - divergencia CPF - CAIXA NOVOS.csv", sep=''), row.names = FALSE)
  #OBS: tiramos o 'sep=";" do write.csv para ele nao trazer mensagens de warning
  
  #retirando da base do banco os CPF's que constavam do BB, mas nao constavam da base da DTI (ou seja comparando com verif_1:
  novos_geral2.df<-novos_geral.df[!novos_geral.df[,1] %in% novos_div_cpf[,1],] # lembrar que na CAIXA os CPF's estao na primeira coluna
  
  #1260204-259
  #*******########## até aqui ok!
  
  ###3) CRUZAMENTO FASES
  
  ### AQUI, LEMBRAR QUE A TAXA TRATA AS FASES DE MANEIRA DIFERENTE.
  ### entao a ideia eh a seguinte:
  ### os que sao fase 4 na DTI jah sao excluidos automaticamente (sao eliminados antes)
  ### transformar todos da DTI que sao fase 1 e 2 em "U" (utilizacao) e os que sao fase 3 em "A" (amortizacao)
  ### para o arquivo da CAIXA, tudo que for 0032 e 0033 vira "A", o resto vira "U"
  
  # v0.9.6.5.0 (28dez18)
  # acertando fase siapi banco -- problema back compatibility data.table v1.11
  novos_geral2.df$fase_SIAPI <- as.character(as.numeric(as.character(novos_geral2.df$fase_SIAPI)))
  
  novos_geral2.df$fase_SIAPI <- str_pad(novos_geral2.df$fase_SIAPI, 4, pad= "0", "left")
  
  #padronizando os nomes das colunas:
  colnames(novos_geral2.df)[c(5,6)]<-c("fase","vl_lib") # na CAIXA, soh precisamos alterar a fase e o total aplic por vl_lib
  # estamos seguindo a padronizacao de nomes prevista  nos contratos (na formula)
  colnames(teste_dti1.2.df)<-c("cpf", "nome", "vl_lib", "fase", "gp_prelim")
  #OBS: lembrando que gp_prelim deve sair ao longo do ano de 2016
  
  #aplicando "U" e "A".
  
    
  # aplicando no data.frame do BANCO:
  novos_geral2.1.df<-novos_geral2.df
  # 10.05.2016 - v0.9.2 - acrescentado fase_SIAPI 0035 como sendo tb amortizacao
  novos_geral2.1.df$fase <- ifelse (novos_geral2.df$fase == "0032" | 
                                    novos_geral2.df$fase == "0033" |
                                    novos_geral2.df$fase == "0035", "A", "U")
  
  
  # aplicando no data.frame da DTI:
  teste_dti2.df<-teste_dti1.2.df
  
  #   nrow(teste_dti2.df[duplicated(teste_dti2.df),])
  #   58 linhas exatamente duplicadas
  #   nrow(teste_dti2.df[duplicated(teste_dti2.df[,1]),])
  #   98 cpf's duplicados
  
  # no da dti, antes temos que eliminar os fase == 4
  # 10.05.2016 - v0.9.2 - retiramos da analise os que est???o em contrata??????o na DTI ( fase == 0)
  
  # BUG constatdo na v 0.9.3 -- não estava indexando corretamente nessa formula de cima
  # assim nao funciona:
  #teste_dti2.df<-teste_dti2.df[(teste_dti2.df$fase != 4 | teste_dti2.df$fase != 0), ]
  
  teste_dti2.df<-teste_dti2.df[!(teste_dti2.df$fase == 4 | teste_dti2.df$fase == 0), ]
  
  
  # ou:
  #teste_dti2.df<-teste_dti2.df[teste_dti2.df$fase != 4,]
  #teste_dti2.df<-teste_dti2.df[teste_dti2.df$fase != 0,]
  
  # teste_dti2.df<-teste_dti2.df[which(teste_dti2.df$fase != 4 || teste_dti2.df$fase != 0), ]
  # length( which(teste_dti2.df$fase == 4 | teste_dti2.df$fase == 0) )
  # esses, ao serem retirados do df, nao ser???o nem analisados? NAO!
  
  # teste_dti2.df[1438498,]
  # aplicando:
  teste_dti2.df$fase <- ifelse(teste_dti2.df$fase == 3, "A", "U")
  
  
  
  #v0.9.2 - 10.05.2016: bug - erro de algoritmo - precisavamos tirar os cpfs para
  # ... que nao fossem comparados no banco tb
  # aplicando no anterior a fase transformada
  ind.fase4_e_0.dti<-teste_dti1.2.df[teste_dti1.2.df$fase == 4 | teste_dti1.2.df$fase == 0, "cpf"]
  # length(ind.fase4_e_0.dti)
  
  # soh precisamos retirar das divergencias ao final de cada comparacao (para fase e vl lib)
  
  
  ###v0.9: bug 15.02.2016
  # precisamos aplicar isso tb ao data.frame inicial da DTI, pois senao nao conseguiremos cruza-lo
  #... para trazer num mesmo dataframe os do banco e da dti
  
  #teste_dti.df2<-teste_dti.df
  #teste_dti.df2$fase <- ifelse(teste_dti.df2$fase == 3, "A", "U")
  
  ##NAO PRECISA!! ESTAH TRAZENDO MAIS DO QUE A REAL DIVERGENCIA PQ OS CPF ESTAO REPETIDOS
  ### QUANDO TIRAMOS DO BANCO, SOH HA UM UNICO REGISTRO PARA AQUELE CPF... POR ISSO
  ##. A DIF NA QUANTIDADE
  
  # cruzando fase e cpf
  novos_div_fase.df<-anti_join(novos_geral2.1.df[,c("cpf","nome","fase","vl_lib")], 
                               teste_dti2.df, by=c("fase", "cpf")) 
  # vamos trazer tudo para depois poder juntar no consolidado
  #mostrando a diferenca entre fase banco e fase dti:
  
  # o segredo estava em usar left join ao inves do %in%
  #novos_div_fase2.df<-novos_div_fase.df
  novos_div_fase2.df<-left_join(novos_div_fase.df, teste_dti2.df, by = ("cpf")) 
  # no help da left join explica (todas linhas e colunas de x com todas as colunas de y)
  # tirando os repetidos l??? no come???o est??? ficando OK!
  
  #v 0.9.2
  # retirar da compara??????o divergencia aqueles com fase 4 e 0 na DTI
  #... utilizando o indice l??? de cima
  novos_div_fase2.df<-novos_div_fase2.df[!novos_div_fase2.df[,1] %in% ind.fase4_e_0.dti, ]
  
  
  
  ##############################################  
  #   # na volta estah trazendo mais cpfs no novos_div_fase2 do que em novos_div_fase.df
  #   # ISSO pq provavelmente ha alguns cpfs repetidos na DTI:
  #   
  #   #dim(teste_dti2.df[duplicated(teste_dti2.df$cpf),])
  #   
  #   # HA BASTANTES REPETIDOS (98)
  #   #teste_dti2.df[teste_dti2.df$nome == 'TAMARA SOARES ROSA',]
  #   #teste_dti.df[teste_dti.df$nome == 'TAMARA SOARES ROSA',]
  #   
  #   #novos_div_fase.df[!novos_div_fase.df$cpf %in% novos_div_fase2.df$cpf,]
  #   
  #   novos_div_fase2.df[duplicated(novos_div_fase2.df[,1]),]
  #   # mostra os que est???o repetidos
  #   
  #   novos_div_fase2.df[duplicated(novos_div_fase2.df),]
  #   
  #   novos_div_fase.df[duplicated(novos_div_fase.df),]
  
  #v0.9 -- acima consertei de teste_dti.df pata teste_dti2.df para trazer fase da dti como letras 
  
  
  ################################################################ ATENCAO
  ### !!!!! verificar se eh com esses numeros de colunas abaixo que ficaremos QDO RECEBERMOS O ARQUIVO DA DTI
  novos_div_fase2.df<-novos_div_fase2.df[,c(1,2,3,7,8)]
  
  #ajustando os nomes para apresentacao:
  colnames(novos_div_fase2.df)<-c("cpf" ,"nome", "fase_banco", "fase_dti", "gp_prelim")
  novos_div_fase3.df<-novos_div_fase2.df[,-5] # retirando a coluna de preliminares para apresentacao
  #aqui tanto faz se os preliminares sao 0 ou 1 pq isso soh influencia no valor liberado
  #para 2016 depois vamos trabalhar sem essa coluna na analise
  
  #exportando primeira verificacao -- lembrando que vai mandar para o working directory
  #OBS: !!!! mais pra frente, a ideia eh exportar com os nomes das variaveis
  write.csv2(novos_div_fase3.df, file=paste(dir3, Sys.Date()," - divergencia fase(impacto) - CAIXA NOVOS.csv", sep=''), row.names = FALSE)
  ####
  
  ### OBS: NA CAIXA, SOH VERIFICACAO QUALIFICADA DA FASE (IMPACTO) - NAO TEMOS VERIFICACAO SEM IMPACTO
  
  ## (!) v0.9.3 - 11.06.2017:
  
  # retirando os com fase divergente da base do banco
  # novos_geral3.df<-novos_geral2.1.df[!novos_geral2.1.df[,1] %in% novos_div_fase2.df[,1],] # lembrar que na CAIXA os CPF's estao na primeira coluna
  # pegamos de novos_div_fase2 e nao da novos_div_fase3, pq no valor liberado precisaremos da coluna preliminares, para eliminar os que sao preliminares
  
  # o que antes eliminavamos, agora sera pago o incontroverso, ou seja, a fase da DTI:
  
  #help(sort)
  # novos_div_fase3.df2 <- novos_div_fase3.df[order(novos_div_fase3.df[,"nome"]),]
  # novos_div_fase3.df2 <- novos_div_fase3.df[order(novos_div_fase3.df[,"nome"]),]
  # precisamos ordenar antes
  
  # (!) BUG encontrado durante alteração na v0.9.3: tirando os que na DTI estão com fase "A", pq vamos pagar menos:
  
  #novos_div_fase3.df3<-novos_div_fase3.df2
  novos_div_fase3.df2<-novos_div_fase3.df
  novos_div_fase3.df2<-novos_div_fase3.df2[novos_div_fase3.df2$fase_dti == "U",]
  
  
  novos_geral3.df<-novos_geral2.1.df
  
  
  novos_geral3.df <- left_join(novos_geral3.df, novos_div_fase3.df2[,c(1,4)], by="cpf")
  
  novos_geral3.df$fase <- ifelse(!is.na(novos_geral3.df$fase_dti), novos_geral3.df$fase_dti,
                                 novos_geral3.df$fase)
  
  # bem rápido tb, porém mais confiável que o outro jeito
  # que não entendi pq às vezes funciona e outras vezes não
  # não estava funcionando do outro jeito
  
  # removendo coluna do join:
  novos_geral3.df <- novos_geral3.df[,-7]
  
  
  # novos_geral3.df[novos_geral3.df[,"cpf"] %in% novos_div_fase3.df3[,"cpf"], "fase"] <- 
  #   novos_div_fase3.df3[,4]
  # novos_geral3.df[novos_geral3.df$cpf == 3321857537,]
  # indexação do R funciona perfeitamente:: mais uma vez, só depois de ordenado
  
  
  
  ###4) CRUZAMENTO VALOR LIBERADO
  
  ## TESTANDO VALOR LIBERADO:
  #library(dplyr)
  ## EH MELHOR SOH EXCLUIR OS QUE FICAM ACIMA DO DA DTI, POIS IMPACTAM EM UMA TAXA MAIOR
  ##O BANCO QUE SE VIRE SE NAO ESTAH CALCULANDO DIREITO OS OUTROS
  novos_div_va1.df<-anti_join(novos_geral3.df[,c("cpf","nome", "fase", "vl_lib")], teste_dti2.df, by=c("cpf", "vl_lib"))
  novos_div_va2.df<-novos_div_va1.df
  
  #help(left_join)
  novos_div_va3.df<-left_join(novos_div_va2.df, teste_dti2.df, by = ("cpf")) #para trazer as demais colunas para apresentacao comparada
  #OBS!!!! traz traz a mais do que a o va2 --- nao era para acontecer
  # tirando os repetidos da dti lá em cima, resolveuu o problema
  
  ######################### VERIFICAR SE SAO ESSAS COLUNAS MESMO QUE PRECISAREMOS:
  
  novos_div_va3.df<-novos_div_va3.df[,c(1,2,4,6,8)]
  
  colnames(novos_div_va3.df)<-c("cpf","nome", "vl_lib_banco", "vl_lib_dti", "gp_prelim")
  
  #ficando soh com os preliminares ==0 ### pode ser que tenha um grupo preliminar =2 alem de 1, por isso queremos soh zero
  novos_div_va4.df<-novos_div_va3.df
  #with(verif_3.3, gp_prelim ==1)
  novos_div_va4.df<-novos_div_va4.df[novos_div_va4.df$gp_prelim == 0,] # boa tecnica para deletar, havia me esquecido
  #em:http://stackoverflow.com/questions/8005154/conditionally-remove-dataframe-rows-with-r
  
  ################### v0.9.2 - 10.05.2016
  # retirando da analise os que estao em contrata??????o na DTI:
  #v 0.9.2
  # retirar da compara??????o divergencia aqueles com fase 4 e 0 na DTI
  #... utilizando o indice l??? de cima
  novos_div_va4.df<-novos_div_va4.df[!(novos_div_va4.df[,1] %in% ind.fase4_e_0.dti), ]
  # ao retirarmos, eles simplesmente n???o ser???o cruzados
  
  ###TESTES:
  #   em_contratacao_DTI.ind<-c('WESLEY GOULART')
  #   em_contratacao_DTI.ind<-data.frame(nome=c('WESLEY GOULART'), stringsAsFactors = FALSE)
  #   str(em_contratacao_DTI.ind)
  #   
  #   novos_div_va3.df[novos_div_va3.df$nome == 'WESLEY GOULART',]
  #   
  #   novos_div_va3.df[novos_div_va3.df[,2]%in% em_contratacao_DTI.ind, ] # boa tecnica para deletar, havia me esquecido
  
  
  #RETIRANDO OS DO BANCO QUE ESTAO ACIMA DA DTI:
  novos_div_va5.df<-novos_div_va4.df
  novos_div_va5.df<-novos_div_va5.df[novos_div_va5.df$vl_lib_banco > novos_div_va5.df$vl_lib_dti,]
  #PODERIAMOS AINDA TIRAR SOMENTE OS QUE ESSES VALORES ULTRAPASSAM POR EXEMPLO 100 REAIS
  
  #NOVO v0_6:
  # retirando as diferencas acima de R$ 1 (um real) tambem:
  novos_div_va6.df<-novos_div_va5.df
  novos_div_va6.df[,"dif_vl_lib"]<-with(novos_div_va6.df, vl_lib_banco - vl_lib_dti)
  novos_div_va6.df<-novos_div_va6.df[novos_div_va6.df$dif_vl_lib > 1,]
  
  
  novos_div_va7.df<-novos_div_va6.df[,-c(5,6)] #limpando para exportacao - tirando gp_prelim e dif_vl_lib
  
  
  # aqui vamos exportar a verificacao qualificada (pelos vl_lib acima de 1R$) mesmo
  write.csv2(novos_div_va7.df, file=paste(dir3, Sys.Date()," - divergencia valor liberado(impacto) - CAIXA NOVOS.csv", sep=''), row.names = FALSE)
  
  
  # retirando as divergencias de valor liberado do banco  
  
  ## (!) v0.9.3 - 11.06.2017:
  #novos_geral4.df<-novos_geral3.df[!novos_geral3.df[,1] %in% novos_div_va7.df[,1],] # lembrar que na CAIXA os CPF's estao na primeira coluna
  
  # o que antes elimin???vamos, agora ser??? pago o incontroverso, ou seja, a fase da DTI:
  
  #help(sort)
  #novos_div_va7.df2 <- novos_div_va7.df[order(novos_div_va7.df[,"nome"]),]
  # precisamos ordenar antes
  
  novos_geral4.df<-novos_geral3.df
  
  
  novos_geral4.df <- left_join(novos_geral4.df, novos_div_va7.df[,c(1,4)], by ="cpf")
  
  novos_geral4.df$vl_lib <- ifelse(!is.na(novos_geral4.df$vl_lib_dti), novos_geral4.df$vl_lib_dti,
                                   novos_geral4.df$vl_lib)
  # bem rápido e mais confiável
  # não estava funcionando do outro jeito
  # novos_geral4.df[novos_geral4.df[,"cpf"] %in% novos_div_va7.df2[,"cpf"], "vl_lib"] <- 
  #   novos_div_va7.df2[,4]
  # # soh funciona ordenando 
  # # novos_geral4.df[novos_geral3.df$cpf == 10761281657,]
  
  
  # eliminando a coluna extra resultante do anti_join:
  novos_geral4.df <- novos_geral4.df[,-7]
  
  # ok! o numero de linhas tem que bater com a previa do banco - CPF que nao constam na DTI
  
  
  
  # PRONTO, AGORA A BASE DO BANCO ESTAH TOTALMENTE LIMPA PARA SE EFETUAR OS CALCULOS !!!
  obs_novos_geral_limpo<-nrow(novos_geral4.df)
  # serah usado na planilha dos resultados
  
  ### 5) EXPORTANDO A PLANILHA PENDENCIAS CONSOLIDADA:
  ## LEMBRANDO QUE ESSAS PENDENCIAS REFERENCE-SE SOMENTE A FASE E VALOR LIBERADO -- CPF EH UMA UNICA PLANILHA
  
  ###v0.9: antes ajustando as colunas iguais para a divergencia de fase e vl.lib para fazermos o rbind
  # NAO PRECISA : EH SOH PEGAR OS PRIMEIROS RESULTADOS DO ANTI JOIN DE FASE E VL LIB...
  #... IGUAL AO DO BB
  
  pendencias <-left_join(rbind(novos_div_fase.df, novos_div_va1.df), teste_dti2.df, by = "cpf") # o left join pega todas as linhas do x e todas as colunas do x e y
  # esse pendencias pega inclusive os valores em que DTI > banco para vl. liberado --- soh para controle do fiscal
  
  
  pendencias2<-pendencias
  ######################### verificar se vai funcionar trazer essas colunas !!!!!!!!!!!!!!!!
  colnames(pendencias2)<-c("cpf", "nome_banco", "fase_banco", "vl_lib_banco", "nome_dti", "vl_lib_dti", "fase_dti", "gp_prelim")
  #exportando
  write.csv2(pendencias2, file=paste(dir3, Sys.Date()," - pendencias gerais(vl_lib e fase) - CAIXA NOVOS.csv", sep=''), row.names = FALSE)
  
  
  
  ### 6) CALCULOS:
  # ANTES DE EFETUAR OS CALCULOS, EH NECESSARIO DIVIDIR AS FASES:
  
  ### 2.1) CONTRATOS NOVOS EM UTILIZACAO (1.5%a.a)  
  
  novos2.1.df<-subset( novos_geral4.df, fase=='U' ) # NAO CONFUNDIR COM NOVOS_GERAL2.1.DF
  # OU eh soh usar exclamacao para negar os da outra fase
  obs_novos2.1 <- nrow(novos2.1.df)
  # ok! o esperado é que o numero de contratos em utilizacao aumente com
  #... relação à prévia, pois as divergencias receberão a U como fase
  
  ## CALCULO:
  #VRM2.1 (FASES UTILIZACAO E CARENCIA):
  SDI2.1 = sum(novos2.1.df[(novos2.1.df[,"dias_atraso"] > 60) , "total_div"])
  SDT2.1 = sum(novos2.1.df[,"total_div"])
  VA2.1 = sum(novos2.1.df[ ,"vl_lib"]) # A CAIXA USA O NOME total_aplicacao nessa coluna
  VRM2.1 =( SDT2.1*( 1-(SDI2.1/VA2.1) )* (1.5/1200) )
  
  
  ### 2.2) CONTRATOS NOVOS EM AMORTIZACAO (2%a.a)  
  
  novos2.2.df<-subset( novos_geral4.df, fase == 'A'  )
  #OU eh soh usar exclamacao para negar os da outra fase
  obs_novos2.2 <- nrow(novos2.2.df)
  # ok! o esperado é que o numero de contratos em amortização DIMINUA com
  #... relação à prévia, pois as divergencias receberão a U como fase
  
  
  #VRM2.2 (FASE DE AMORTIZACAO):
  SDI2.2 = sum(novos2.2.df[(novos2.2.df[,"dias_atraso"] > 60) , "total_div"])
  SDT2.2 = sum(novos2.2.df[,"total_div"])
  VA2.2 = sum(novos2.2.df[ ,"vl_lib"])
  VRM2.2 =( SDT2.2*( 1-(SDI2.2/VA2.2) )* (2/1200) )
  # tb deu bem proximo o valor -- um pouco a mais que na CAIXA
  
  
  VRMT_novos = VRM2.1 + VRM2.2
  
  ####### PARECE ESTAR, OK! EH PARA FUNCIONAR, DESDE QUE NAO HAJA CPF'S REPETIDOS
  
  #   ##### v0.9.2 - 10.05.2016
  #   SDI_total=SDI2.1+SDI2.2
  #   SDT_total=SDT2.1+SDT2.2
  #   VA_total=VA2.1+VA2.2
  
  
  ### 7) PLANILHA DE RESULTADOS
  
  resultadosteste.df<-data.frame("Quant._Contratos"=c(obs_novos2.1, obs_novos2.2, obs_novos_geral_limpo),
                                 "Valor_Total_Taxa"=c(VRM2.1, VRM2.2, VRMT_novos),
                                 "_valor_alternativo_" = c(obs_novos2.1*25, obs_novos2.2*35, (obs_novos2.1*25) + (obs_novos2.2*35) ) )
  
  row.names(resultadosteste.df) <- c('novos - utiliz. e carencia', 'novos - amortizacao', 'SUBTOTAL NOVOS')
  
  write.csv2(resultadosteste.df, file=paste(dir3, Sys.Date()," - RESULTADO FINAL TAXA - CAIXA NOVOS.csv", sep=''), row.names = TRUE)
  
  
  ### 8) ARQUIVO DE LOG:
  
  #necessidade de colocar "/n", para que ele pule linha no write.table
  nome_data_arq <- paste("********Arquivo Log de Fiscalizacao de Fatura - CAIXA NOVOS********", Sys.Date(), "\n", sep=' ')
  arq_banco <- paste("Arquivo Banco analisado:", teste2_bancos.loc, "\n", sep=' ')
  arq_DTI<- paste("Arquivo DTI analisado:", dti.loc,"\n", sep=' ')
  qte_total_pendencias_impacto<- paste("Quantidade Total Pendencias com Impacto na TAXA:", nrow(novos_div_cpf)+nrow(novos_div_fase3.df)+nrow(novos_div_va7.df),"\n", sep=' ')
  qte_div_cpf<- paste("Quantidade divergencias em CPF:", nrow(novos_div_cpf),"\n", sep=' ')
  qte_div_fase<-paste("Quantidade divergencias em FASE (c/ Impacto):", nrow(novos_div_fase3.df),"\n", sep=' ')
  qte_div_vl_lib<-paste("Quantidade divergencias em VALOR LIBERADO (c/ Impacto):", nrow(novos_div_va7.df),"\n", sep=' ')
  
  versao_fisc <- paste("Fiscaliza Fatura FIES/FNDE v0.9 - Desenvolvido por: Allan Vieira de C. Quadros","\n", sep=' ')
  versao_os_R <- paste(R.Version()[['version.string']],"-",Sys.info()['sysname'],Sys.info()['release'],"\n", sep=' ')
  
  contato<- paste("http://www.fnde.gov.br", "e-mail: allan.quadros@fnde.gov.br", sep ='  ') # 2 espacos
  
  log_df<-list(nome_data_arq, arq_banco, arq_DTI, qte_total_pendencias_impacto, qte_div_cpf,
               qte_div_fase, qte_div_vl_lib, versao_fisc,versao_os_R, contato)
  
  
  write.table(log_df, file=paste(dir3, Sys.Date()," - log FISCAL - CAIXA NOVOS.txt", sep=''), col.names = FALSE, row.names = FALSE)
}
# versão operacional estável antes da mudança de dez2017 (forma alternativa de cálculo - utiliz - 25R$; amortiz - 35R$).
