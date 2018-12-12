funcao_fiscal<-function(){
  # alteração na GUI em jun.17:
  #(!) alteração na GUI em jun.17:
  # gdkPixBuf (para alterar icone da janela), na nova versão do RGtk2, deixou de ser passado como lista e não precisamos mais indexxar por [[1]]
  
  
  #v0.8 26.01.2016-- nao precisa de qualquer parametro -- pegamos direto as variaveis da GUI que estarao no environment
  #pq esta funcao em especifico estah vinculada a um botao.
  #... as outras que chamaremos a partir dela, por sua vez, ai sim devem ter os parametros declarados e passados
  
  # Gtk Spin para mostrar execucao
  
  janela2 <- gtkWindow()
  janela2["title"]<-"Resultados"
  
  
  ### v0.7 10.01.2016
  # impedindo o usuArio de aumentar e minimizar a janela
  # dica em: http://stackoverflow.com/questions/19559970/r-fixing-my-gwidget-window-size
  gtkWindowSetResizable(janela2, FALSE)
  
  ### v0.7 10.01.2016
  #acrescentando o logo do FNDE na janela:
  #image <- gdkPixbuf(filename = paste(getwd(),"icons/fnde.gif", sep='/'))
  #image <- gdkPixbuf(filename = paste(getwd(),"icons/fnde.gif", sep='/'))
  janela2$set(icon = image)
  
  
  frame2 <-gtkFrameNew("Processando ...")
  #adicionando a nossa window de nome janela
  janela2$add(frame2)
  #adicionando um container vertical no frame:
  #nos deixarAH adicionar varios widgets
  vbox2 <- gtkVBoxNew(spacing = 10)
  vbox2$setBorderWidth(2)
  frame2$add(vbox2)
  
  #ADICIONANDO O spin
  #adicionando um container para o spin ficar menorzinho
  hbox3 <- gtkHBoxNew(FALSE, 8)
  hbox3$setBorderWidth(2)
  vbox2$packStart(hbox3, FALSE, FALSE, 50)
  
  
  spin <-gtkSpinner()
  hbox3$add(spin)
  #vbox2$add(spin) 
 
  #??gtkSpinner
  spin$start()
  
  ######ATENcaoO!!!v0_6 2015.01.07: O SPIN DAVA UM PROBLEMA NO LINUX, DEVIDO A UMA HIERARQUIA DE EXECUCAO ENTRE AS FUNCOES PESADAS
  #... E A INTERFACE GTK, ALGO RELACIONADO A ASSINCRONIA DE FUNCOES (MULTITHREAD):
  #http://stackoverflow.com/questions/16680430/using-gtk-spinner-with-a-long-time-function
  #http://stackoverflow.com/questions/29539894/glib-critical-error-gthread-and-gtk-spinner-not-showing-up
  ###SOLUCAO EM:
  #http://stackoverflow.com/questions/3820402/gtkprogressbar-in-rgtk2
  #PEGUEI A IDEIA GERAL QUE ERA PARA PROGRESSBAR E ACRESCENTEI A SPIN$START
  #aTENcaoO2:!!! FUNCIONA QDO RODAMOS DIRETO NO R, MAS Nao PELA LINHA DE COMANDO
  #while(gtkEventsPending()){
  #  spin$start()
  #  gtkMainIteration()
  #} 
    
  #gtkMainIterationDo(FALSE)
  
  
  
  #gtkIdleAdd(0,spin.fun)
  #??gtkInit
  #??gtkIdle
  #gtkInit()
  #teste2_dti.loc<-paste(getwd(),gtkFileChooserGetFile(filename1),sep="/")
  #teste2_bancos.loc<-paste(getwd(),gtkFileChooserGetFile(filename2),sep="/")
  
  
  ###NOVO NA v.03 (pegando valor do radio button do banco):
  bancobox <- gtkComboBoxGetActive(combo)
  #FUNCIONOU!!!!
  # o gtkComboBoxGetActive pega soh o numero de ordem da lista
  
  ### v0.8 -- 26.01.2016 -- pegando os valores das entradas de mes e ano
  #??gtkComboBoxGet
  # o gtkComboBoxGetActiveText, diferente do getActive, jah pega a string selecionada
  # NAO FUNCIONA -- TERIAMOS QUE TER CRIADO O COMBO BOX VIA gtkComboBoxNewText para usar essa funcao
  # entao vou fazer um 'truque' usando indexacao de vetores:
  #... vou pegar o vetor de meses e indexar pelo valor retornado do combo box dos meses
  
  vetor.meses<-c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ")
  
  mesbox<-gtkComboBoxGetActive(combo.mes)
  anobox<-gtkComboBoxGetActive(combo.ano)
  
  # entao serah o mes +1:
  #mesbox<-0
  mesbox_string <- vetor.meses[mesbox+1]
  # eh essa string que eu tenho que passar para as demais funcoes
  
  ### tenho que fazer isso tb o ano!!:
  range_ano.fun<- function(){
    library(zoo) # caso nao tenha carregado
    x<-year(Sys.Date())
    y <- seq(x-5, x)
    return(y)
  }
  
  # a funcao year nao eh do pacote zoo!! eh do data.table!!!
  vetor.ano <- c(as.character(range_ano.fun()) )
  
  #anobox<-5
  anobox_string<-vetor.ano[anobox+1]
  
  # MAS PARA A CAIXA TENHO QUE PASSAR TB o numero para entrar na data!!!
  ## ENTAO PARA A CAIXA_antigos (SEM SER CONF), PASSO TUDO MENOS O ANOBOX (NAO INTERESSA POSICAO DO ANO) E PARA O BB E OS DEMAIS PASSO SOH AS STRINGS
    
    
  ##v0_6 - pegando o status do check button
  #... o checkbutton eh um tipo de togglebutton- por isso usamos gtkToggleButtonGetActive
  conf.button <- gtkToggleButtonGetActive(check.previa)
  
  #caso conferir BB
  if (conf.button == TRUE && bancobox == 0){
    funcao_confBB(teste2_bancos.loc, mesbox_string, anobox_string)
    
    #x<-"TRUE"
    #write.table(x, file=paste(Sys.Date(),"testeTRUE.txt", sep='-'), row.names = FALSE)
  #caso conferir CAIXA ANTIGOS
  } else if (conf.button == TRUE && bancobox == 1){
    funcao_confCAIXA_ANTIGOS(teste2_bancos.loc, mesbox_string, anobox_string)
    
    #x<-"FALSE"
    #write.table(x, file=paste(Sys.Date(),"testeFALSE.txt", sep='-'), row.names = FALSE)
  
  # caso conferir CAIXA NOVOS
  } else if (conf.button == TRUE && bancobox == 2){
    funcao_confCAIXA_NOVOS(teste2_bancos.loc, mesbox_string, anobox_string)
    
    #caso conf.button=FALSE
  } else {
    if(bancobox == 0){
      funcao_BB(dti.loc, teste2_bancos.loc, mesbox_string, anobox_string) 
    } else if (bancobox == 1) {
      funcao_CAIXA_ANTIGOS(teste2_bancos.loc, mesbox, mesbox_string, anobox_string ) # nao precisamos mais de dti.loc aqui
    } else {
      funcao_CAIXA_NOVOS(dti.loc, teste2_bancos.loc, mesbox_string, anobox_string)
    }
    #ATENCAO: DENTRO DE FUNCOES CHAMAMOS AS OUTRAS COM OS PARAMETROS. EH SOH NO RGTK GSIGNAL QUE NAO CHAMAMOS 
    
  }
  

  # parando spin:
  #spin$stop()
  spin$destroy()
  
  #novo container GUI com resultados -- acrescido a janela do spin
  frame2$destroy()
  frame3 <-gtkFrameNew("Resultado da comparação")
  #adicionando a nossa window de nome janela
  janela2$add(frame3)
  
  vbox3 <- gtkVBoxNew(spacing = 10)
  vbox3$setBorderWidth(2)
  frame3$add(vbox3)
  hbox4 <- gtkHBoxNew(FALSE, 8)
  hbox4$setBorderWidth(2)
  vbox3$packStart(hbox4, FALSE, FALSE, 50)
  
  label4 = gtkLabel("Os arquivos foram gerados com êxito.")
  hbox4$packStart(label4)
  
  # Add button
  the.buttons2 = gtkHButtonBoxNew()
  the.buttons2$setBorderWidth(5)
  vbox3$add(the.buttons2)
  the.buttons2$setLayout("spread")
  the.buttons2$setSpacing(40)
  buttonCancel2 = gtkButtonNewFromStock("gtk-close")
  gSignalConnect(buttonCancel2, "clicked", janela2$destroy)
  the.buttons2$packStart(buttonCancel2,fill=F)
  
  ### NAO ESTah FUNCIONADO O DATA.FRAME -- ENQUANTO ISSO SERA MELHOR EXPORTAR O ARQUIVO EM excel MESMO
  #e soh avisar que acabou a analise
  
  #(list(SDI1, SDT1, VA1, SDI2, SDT2, VA2, VRM1, VRM2, VRMT))

}
