funcao_CAIXA_ANTIGOS<-function(teste2_bancos.loc, mesbox, mesbox_string, anobox_string, ...){
  # nao precisamos do arquivo da dti no LEGADO
  
  # #v0.8 2016.01.26
  #jah criando uma string com a data atual com a qual trabalharemos na avaliacao das fases:
  # vamos jah salavar no formato americano?
  string_data_fatura <- paste('1', (mesbox+1), anobox_string, sep='/')
  # string_data_fatura<-"01/01/2016"
  # lembrar de sempre acrescentar um da posicao do mes dada pelo combo
  #transformando em data:
  string_data_fatura<-as.Date(string_data_fatura, "%d/%m/%Y")
  # perfeito!!! AGORA EH SOH SUBSTITUIR OS SYS.DATE(), por essa string de data !!!!
  
  #v0.8 2016.01.26 -- criamos uma pasta para salvar os resultados das analises do dia e por banco
  dir1<- paste(getwd(), "analises", sep='/') # juntando a pasta analises jah criada
  dir2 <- paste(dir1, paste("CAIXA ANTIGOS", mesbox_string, anobox_string, Sys.Date(), sep=' - '), sep='/')
  dir3<- paste(dir2, "/", sep='') # nao consegui acrescentar a contrabarra no final utilizando uma unica linha de codigo
  # eh importante acrescentar essa contrabarra ao final para que possamos indicar a pasta corretamente ao gravar o .csv
  dir.create(path=dir3)
  #library(data.table)
  #library(dplyr)
  
  #dti.loc<- gtkFileChooserGetFilename(filename1)
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
  
  
  
  #v0.8 2016.01.25
  #pegando somente as colunas que interessam:
  teste_bancos2.df<-teste_bancos.df[,c(5,6,7,8,11,12,13,14,15)]
  # AQUI EH MELHOR TRAZERMOS O NOME TB (coluna 6)
  # TB TEMOS DE TRAZER A QTE DE SEMESTRE FINANCIADOS PARA TRABALHAR NO LEGADO
  #str(teste_bancos.df)
  
  #padronizando os nomes das colunas:
  teste5.df<-teste_bancos2.df
  colnames(teste5.df)<-c("cpf","nome","sem_ref","ano_ref", "qte_sem_fin","total_div", "dias_atraso", "fase_SIAPI", "total_aplic")
  
  teste5.2.df<-teste5.df
  #str(teste5.2.df)  
  
  #CALCULO
  
  #v0.8 2016.01.25
  #primeiro compensa separar pelos que são legado e os que nao sao
  # depois separaremos por ano e fase
  
  
  #####1)LEGADO/ ANTIGOS sao todos ateh 2S/2009:
  #CAIXA NAO DIVIDIA EM FASES O LEGADO
  #.. COBRAVA UMA ÚNICA TAXA DE TODOS
  
  #entao tudo que tiver ano menor ou igual que 2009 eh legado
  legado_geral.df<-teste5.2.df[( teste5.2.df$ano_ref <= 2009 & is.na(teste5.2.df$ano_ref)==FALSE ), ]
  
  #ok. ficamos soh com legado
  #OBS: muitas entradas do arquivo da caixa vem com sem_ref e ano_ref = NA ---> por isso temos que fazer o filtro
  #... com false
  
  # A ESTRATEGIA AQUI TERAH DE SER DIFERENTE -- PRIMEIRO SEPARAREMOS PELAS FASES/ DATAS
  # .. DEPOIS RETIRAMOS OS ACIMA DE 360 DIAS
  
  ##############
  ########### PREPARANDO O ARQUIVO GERAL/ CRIANDO COLUNAS NECESSARIAS DE DATAS ANTES DE DIVIDIR O LEGADO EM 1.1 E 1.2
  ##############
  
  
  ################ VERIFICACAO B) DIVERGÊNCIA DE FASE:
  
  ##0) ALTERACAO INFORMADA PELO FLAVIO: CAIXA RENEGOCIOU COM ALGUNS ALUNOS E ISSO ESTENDEU O PRAZO PARA QUE ESTES ALUNOS
  #... DEVEDORES PAGASSEM. MAS A PRINCÍPIO, A BASE CEF NAO APRESENTA ESSA DATA ATUALIZADA
  # POR ISSO VAMOS TIRAR DA COMPARACAO/PENTE FINO OS COM FASE 0034 (RENEGOCIADOS) E 0035 (DESCONTO)
  #### MAS DECIDI COM O PAULO QUE O PROBLEMA EH DA CAIXA, POIS ELA DEVERIA TER ATUALIZADO.
  ####... A PROPRIA PLANILHA DE DIVERGENCIAS DE FASE A SER GERADA VAI CONTEMPLAR ESSES ALUNOS E A CAIXA QUE DEVERA VER O QUE 
  ####... ESTAH OCORRENDO... ENTAO NAO ALTERAMOS
  
  
  #1) criando uma coluna extra para termos a data:
  #fase_teorica.fun
  
  ## caso 1.1)
  legado_geral1.df<- legado_geral.df
  #str(div_fase_legado1.1.df)
  #criando uma coluna com uma data base:
  
  #help(ifelse)
  
  #ifelse( div_fase_legado1.1.df$sem_ref == 1 , div_fase_legado1.1.df$data_base <- as.Date( paste("1/1/", div_fase_legado1.1.df$ano_ref, sep=''), "%d/%m/%Y" ), div_fase_legado1.1.df$data_base <- as.Date( paste("1/7/", div_fase_legado1.1.df$ano_ref, sep=''), "%d/%m/%Y" ) ) 
  
  #ifelse( div_fase_legado1.1.df$sem_ref == 1 , div_fase_legado1.1.df$data_base <- 1, div_fase_legado1.1.df$data_base <- 7 ) 
  
  # ele muda todos, mas nao aplica o filtro 
  
  
  ### PORRA!!!! ATENCAO! LEMBRAR QUE NUNCA DEVEMOS ATRIBUIR DENTRO DO IFELSE !!!!!!!!
  legado_geral1.df$data_base<- ifelse( legado_geral1.df$sem_ref == 1 , as.Date( paste("1/1/", legado_geral1.df$ano_ref, sep=''), "%d/%m/%Y" ), as.Date( paste("1/7/", legado_geral1.df$ano_ref, sep=''), "%d/%m/%Y" ) ) 
  # OK!
  
  #str(div_fase_legado1.1.df)
  #library(zoo)
  legado_geral1.df$data_base<-as.Date(legado_geral1.df$data_base) # essa pode transformar em data soh antes
  # SE NAO USAR O ZOO , NAO FUNCIONA PQ A AS.DATE NO BASE-R NAO LIDA DIRETO
  # SOLUCAO EM: https://stat.ethz.ch/pipermail/r-help/2010-March/233159.html
  
  # com o zoo funciona o as.Dtae direto no numeric.
  # ver:
  #library(zoo) # LEMBRAR DE CARREGAR AO INICIAR O PROGRAMA
  
  
  
  #2) CRIANDO NOVA COLUNA COM DIFERENCA DA DATA_BASE PARA DATA ATUAL DO SISTEMA
  
  #div_fase_legado1.1.df$dif_data <- Sys.Date() - div_fase_legado1.1.df$data_base
  # cria a diferenca em dias creio eu
  # isso mesmo
  #div_fase_legado1.1.df$dif_data<-((div_fase_legado1.1.df$dif_data/365)*12)/6
  # isso seria equivalente ao metodo de baixo:
  
  
  ## criando funcao que calcula diferenca entre datas em meses:
  # solucao em: http://stackoverflow.com/questions/1995933/number-of-months-between-two-dates
  elapsed_months <- function(end_date, start_date) {
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    12 * (ed$year - sd$year) + (ed$mon - sd$mon)
  }
  
  ##
  legado_geral1.df$dif_data_mes<-elapsed_months(string_data_fatura, legado_geral1.df$data_base)#/6
  #/6 caso desejemos calcular em semestres
  
  
  ### 3) AGORA ENTRAM AS CONDICOES PARA OS 3 CASOS QUE TEMOS:
  
  # CONFORME E-MAIL FLAVIO DE 27 JAN 2016
  
  ##OBS: PARA O CASO 1.1, SOH APLICAREMOS A LETRA A --- PODE APLICAR TODOS -- O IFELSE VAI FUNCIONAR E DEPOIS CORTAMOS
  # em meses, mas nas contas eh preciso deixar em dias (*30):
  #... se multiplicar por mais de 30, em alguns casos ele acaba ultrapassando o semestre
  
  #1) MAX UTILIZ
  
  #legado_geral1.df$data_max_utiliz <- legado_geral1.df$data_base + ((legado_geral1.df$qte_sem_fin)*6)*30
  ######IDEIA:
  #depois vamos criar uma coluna de fase_teorica dependendo da comparacao entre data atual todas as datas maximas
  #... a data do tipo de fase que for maior que a data atual serah a data teorica
  #... se nenhuma for maior que a data atual, entao esse contrato tem que ser excluido
  #... !!! ATENCAO: vamos usar uma compensacao para o arredondamento dos dias somando mais 30 dias para a comparacao com a data atual
  
  legado_geral1.df$data_max_utiliz <- ifelse (legado_geral1.df$ano_ref < 2007, legado_geral1.df$data_base + ((legado_geral1.df$qte_sem_fin)*6)*30, 
                                              ifelse(legado_geral1.df$ano_ref < 2009 | (legado_geral1.df$ano_ref == 2009 & legado_geral1.df$sem_ref == 1) , 
                                                     legado_geral1.df$data_base + ((legado_geral1.df$qte_sem_fin)*6)*30 + 6*30,
                                                     legado_geral1.df$data_base + ((legado_geral1.df$qte_sem_fin)*6)*30 + 18*30 ) )
  # SOH FUNCIONOU COLACANDO UM UNICO | E UM UNICO &
  # COM DOIS EM CADA OPERADOR NAO FUNCIONA - #########!!!!!!!!!PQ?
  # EXPLICACAO EM:
  #http://stackoverflow.com/questions/1379415/whats-the-difference-between-and-in-matlab
  #A & B (A and B are evaluated)
  #A && B (B is only evaluated if A is true)
  
  legado_geral1.df$data_max_utiliz<-as.Date(legado_geral1.df$data_max_utiliz)
  # TROUXE EM DIAS E AI PASSAMOS PARA DATA
  
  
  ##### FLAVIO DISSE QUE EH MELHOR FAZER O GUARDA CHUVA PARA UMA UNICA FASE DE AMORTIZACAO, QUE ABARCOU QUASE TODOS OS ALUNOS...
  #... EM LEI DE 2010
  
  #1) MAX AMORTIZACAO TOTAL
  
  #
  #str(legado_geral1.df)
  #legado_geral1.df[,"fase_SIAPI"]=='0032'
  # fase_SIAPI estah como caractere
  
  legado_geral1.df$data_max_amortiz_TOTAL <- ifelse (legado_geral1.df$ano_ref < 2007, legado_geral1.df$data_max_utiliz +  (((legado_geral1.df$qte_sem_fin)*6)*30*3) + 12*30, 
                                                     ifelse(legado_geral1.df$ano_ref < 2009 | (legado_geral1.df$ano_ref == 2009 & legado_geral1.df$sem_ref == 1), 
                                                            legado_geral1.df$data_max_utiliz + 6*30 + (((legado_geral1.df$qte_sem_fin)*6)*30*3) + 12*30 ,
                                                            legado_geral1.df$data_max_utiliz + 18*30 + (((legado_geral1.df$qte_sem_fin)*6)*30*3) + 12*30 ) )
  
  # PRA NAO TER PROBLEMA, SOH TRANSFORMAR EM DATAS DEPOIS (senao dah erro ao multiplicar as datas):
  #Error in Ops.Date((legado_geral1.df$data_base + ((legado_geral1.df$qte_sem_fin) *  : 
  #* not defined for "Date" objects
  # ante sfuncionava OK
  ########!!! ESTA FUNCIONANDO MESMO TRANSFORMANDO A UTILIZACAO EM DATA ANTES AGORA !!!!
  
  #legado_geral1.df$data_base<-as.Date(legado_geral1.df$data_base)
  #legado_geral1.df$data_max_utiliz<-as.Date(legado_geral1.df$data_max_utiliz)
  legado_geral1.df$data_max_amortiz_TOTAL<-as.Date(legado_geral1.df$data_max_amortiz_TOTAL)
  
  
  ### DEPOIS EH CRIAR A COLUNA DE FASE TEORICA:
  ## PARA O CASO 1.1) TODOS TEM QUE ESTAR OBRIGATORIAMENTE EM UMA DAS FASES DE AMORTIZACAO
  
  legado_geral1.df$fase_teorica <- ifelse ( (legado_geral1.df$data_max_utiliz+30) >= string_data_fatura , "U" ,
                                                 ifelse ( (legado_geral1.df$data_max_amortiz_TOTAL+30) >= string_data_fatura, "A" ,
                                                          'DIV') )
  #OK1 FUNCIONOU!
  # RACIOCINIO: SOMAMOS MAIS 30 DIAS A DATA TEORICA PARA DAR UMA 'LAMBUJA' PARA O BANCO DEVIDO A ARREDONDAMENTO NO CALCULO COM DATAS...
  #... EM ALGUNS JAH ESTAH ACIMA DO QUE DEVERIA INCLUSIVE
  #... O ' DIV' INDICA QUE HA DIVERGENCIA, QUE O CONTRATO NAO DEVERIA ESTAR NA BASE
  

  ## AGORA TUDO QUE FOR DIV NESSA  COLUNA DEVERÁ SAIR DO ARQUIVO E SER APRESENTADO COMO INCONSISTENCIA
  
  
  
  #### 1ª VERIFICACAO: RETIRAR FASES 'DIV'
  
  #extraindo:
  legado_div_fase.df<-legado_geral1.df[legado_geral1.df$fase_teorica=='DIV',]
  
  # exportando:
  write.csv2(legado_div_fase.df, file=paste(dir3, Sys.Date()," - divergencia fase (impacto) - CAIXA ANTIGOS.csv", sep=''), row.names = FALSE)
  
  # retirando da planilha base do legado para continuar comparacao:
  legado_geral2.df<- legado_geral.df[!legado_geral.df[,"cpf"] %in% legado_div_fase.df[,"cpf"], ] # precisamos indicar qual a coluna comparar, senao ele pega todas e nao vai ter resultado 
  #...pq um tem mais colunas que o outro
  #ok! nao precisamos trazer as colunas com datas aqui!
  
  
  #### 2ª VERIFICACAO: RETIRAR OS ACIMA DE 360 DIAS
  
  #extraindo:
  legado_div_360.df<-legado_geral2.df[legado_geral2.df$dias_atraso > 360, ]
  
  # exportando:
  write.csv2(legado_div_360.df, file=paste(dir3, Sys.Date()," - divergencia 360 (impacto) - CAIXA ANTIGOS.csv", sep=''), row.names = FALSE)
  
  # retirando da planilha base do legado para realizar os calculos:
  legado_geral3.df<- legado_geral2.df[!legado_geral2.df[,"cpf"] %in% legado_div_360.df[,"cpf"], ] # precisamos indicar qual a coluna comparar, senao ele pega todas e nao vai ter resultado 
  # tiramos da ultima base que eh o legado2 (sem as divergencias de fase jah)
  
  #### NUMEROS DE CONTRATOS VALIDOS:
  
  obs_legado_geral <- nrow(legado_geral3.df)
  
  
  ### 3ª PLANILHA PENDENCIAS CONSOLIDADAS DO LEGADO:
  #extraindo:
  #pendencias <-legado_geral1.df[legado_geral1.df$fase_teorica=='DIV' & legado_geral1.df$dias_atraso > 360 ,]
  # eh soh tirar os dois de uma vez
  
  #v0.9.1 da funcao 17.03.2016
  # estah trazendo numero errado (menor que a soma das obs de 360 e de fase), pois tem que ser usado o perador 'OU' e nao 'E'
  pendencias <-legado_geral1.df[legado_geral1.df$fase_teorica=='DIV' | legado_geral1.df$dias_atraso > 360 ,]
  #ok! funcionou!
  
  #exportando:
  write.csv2(pendencias, file=paste(dir3, Sys.Date()," - pendencias (impacto fase+360) - CAIXA ANTIGOS.csv", sep=''), row.names = FALSE)
  #v0.9.1 da funcao 17.03.2016
  # por algum motivo, se colocar o termo gerais, ele nao abre o excel, sendo necessario
  #... renomea-lo para funcionar
  #... parece que ficam muitas palavras soltas
  
  
  #### CALCULOS DA TAXA:
  
  # SUB DIVISAO DO LEGADO 1.5%a.a vs 2.6%a.a.
  
    ### 1.1) CONTRATOS ATEH 1S/2006 (1.5%a.a)
    legado1.1.df<-legado_geral3.df[(legado_geral3.df$ano_ref < 2006) | (legado_geral3.df$ano_ref == 2006 & legado_geral3.df$sem_ref == 1), ]
    # ok! lembrando que nesses casos soh utilizamos um operador & ou |
    
    #nro contratos validos no grupo:
    obs_legado1.1 <- nrow(legado1.1.df)
    
    ### 1.2) CONTRATOS APOS 1S/2006 (2.6%a.a)
    legado1.2.df<-legado_geral3.df[!( (legado_geral3.df$ano_ref < 2006) | (legado_geral3.df$ano_ref == 2006 & legado_geral3.df$sem_ref == 1) ), ]
    # COLOCAMOS O EXCLAMACAO NESTE CASO PARA PEGAR OS DIFERENTES
    
    #nro contratos validos no grupo:
    obs_legado1.2 <- nrow(legado1.2.df)
  
  
  ##CALCULO:
  ###1.1)
  # considera-se devedor a partir do 61º dia (paragrafo 2º - Portaria 505 de 16abril2010 )
    SDT1.1 = sum(legado1.1.df[,"total_div"])
    VRM1.1=( SDT1.1*(1.5/1200) )
  
  
  ### 1.2) CONTRATOS APOS 1S/2016 (2.6%a.a)
  
    SDT1.2 = sum(legado1.2.df[,"total_div"])
    VRM1.2=( SDT1.2*(2.6/1200) )  
  
  ## TOTAL:
  VRMT_antigos<-VRM1.1+VRM1.2
  
  
  
  resultadosteste.df<-data.frame("Quant._Contratos"=c(obs_legado1.1, obs_legado1.2, obs_legado_geral),
                                 "Valor_Total_Taxa"=c(VRM1.1, VRM1.2, VRMT_antigos) )
  
  row.names(resultadosteste.df) <- c('antigos ate 30/06/2006', 'antigos entre 01/07/2006 e 14/01/2010', 'SUBTOTAL ANTIGOS')
  
  
  write.csv2(resultadosteste.df, file=paste(dir3, Sys.Date()," - resultado final TAXA - CAIXA ANTIGOS.csv", sep=''), row.names = TRUE)
  #nao esquecer de row.names = TRUE para a CAIXA
    
  #ARQUIVO DE LOG: (DIFERENTE PARA CAIXA ANTIGOS)
  #necessidade de colocar "/n", para que ele pule linha no write.table
  nome_data_arq <- paste("********Arquivo Log de Fiscalizacao de Fatura - CAIXA ANTIGOS********", Sys.Date(), "\n", sep=' ')
  arq_banco <- paste("Arquivo Banco analisado:", teste2_bancos.loc, "\n", sep=' ')
  #arq_DTI<- paste("Arquivo DTI analisado:", dti.loc,"\n", sep=' ')
  qte_total_pendencias_impacto<- paste("Quantidade Total Pendencias com Impacto na TAXA:", obs_legado_geral,"\n", sep=' ')
  qte_div_fase<- paste("Quantidade divergencias em FASE (c/ Impacto):", nrow(legado_div_fase.df),"\n", sep=' ')
  qte_div_360<-paste("Quantidade divergencias em DIAS ATRASO (>360) (c/ Impacto):", nrow(legado_div_360.df),"\n", sep=' ')
  
  versao_fisc <- paste("Fiscaliza Contrato FIES/FNDE v0.9 - Desenvolvido por: Allan Vieira de C. Quadros","\n", sep=' ')
  versao_os_R <- paste(R.Version()[['version.string']],"-",Sys.info()['sysname'],Sys.info()['release'],"\n", sep=' ')
  
  contato<- paste("http://www.fnde.gov.br", "e-mail: allan.quadros@fnde.gov.br", sep ='  ') # 2 espacos
  
  log_df<-list(nome_data_arq, arq_banco, qte_total_pendencias_impacto, qte_div_fase,
               qte_div_360, versao_fisc,versao_os_R, contato)
  
  
  write.table(log_df, file=paste(dir3, Sys.Date()," - log FISCAL - CAIXA ANTIGOS.txt", sep=''), col.names = FALSE, row.names = FALSE)
}
