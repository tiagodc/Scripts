# rm(list = ls()[ls() != 'data_analise'] )

# ler tabela contendo os p90
p90 = read.table('p90.csv', header = T, sep = ';')

# concatenar as coordenadas criando um fator (cada pixel = 1 classe)
p90$coord = paste('x', p90$POINT_X, 'y', p90$POINT_Y, sep = '')

# isolar apenas eucaliptos com idade de 6 anos
df = data_analise[ data_analise$e6 == 0 ,]

# expressão regular: rearranjar as classes de pixels eliminando 0s depois da vírgula
# para que df e p90 tenham exatamente os mesmos nomes de classe, para que o merge seja feito corretamente
df$coord = sub('(x[:0-9:]+\\.[:1-9:])0*(y[:0-9:]+\\.[:1-9:])0*', '\\1\\2', df$coord)
df = merge(df, p90)

# classes de distância: 1=50m, 2=40m, 3=30m, 4=20m, 5=10m (soma dos binários)
bClass = apply(df[,10:14], 1, sum)

{
# exportar figura
# png('thais.png', width = 24, height = 8, units = 'cm', res = 300)

#parâmetros gráficos 
par(mfrow = c(5,3), oma=c(2,3,5,2), mar=c(2,5,2,2))

  # iterar sobre todas as distâncias consideradas
for(distancia in 1:5*10){
# distancia = 20 # metros

  # calcular qual classe de distância corresponde à iteração 
  classDist = 1+(50 - distancia)/10

  # atribui código do vizinho mias próximo
vizinho = with(df, ifelse(agua >= distancia, 'a', 
                          ifelse(pas <= distancia & nat <= distancia, 'np', 
                                 ifelse(pas <= distancia, 'p',
                                        ifelse(nat <= distancia, 'n', NA)))))

# looping interno: iterar sobre pixels com vizinhança de água, pasto e nativas
for(i in c('a', 'n', 'p')){
  # p90 dos pixels de borda para a categoria iterada (a, n ou p)
  denBorder = df[ bClass >= classDist & vizinho == i & !is.na(vizinho) & df$GRID_CODE > 15 ,'GRID_CODE']
  
  # p90 dos pixels de interior
  denInner  = df[ bClass == 0 & !is.na(vizinho) & df$GRID_CODE > 15 ,'GRID_CODE']

  # histogramas
  bHist = hist(denBorder, plot = F, breaks = seq(0,50,1))
  iHist = hist(denInner, plot = F, breaks = seq(0,50,1))
  
  # labels de linha e coluna
  yLab = ifelse(i == 'a', paste(distancia, 'm'), '')
  xMain = ifelse(distancia > 100, '', ifelse(i == 'a', 'Water', ifelse(i == 'p', 'Pasture', 'Natural')))
  
  # gráficos de densidade de borda e interior combinados
  # plot dos histogramas
  plot(iHist, xlim = c(15,50), ylim=c(0,.4) , col=rgb(0,0,128/255), 
       freq=F, main=xMain, ylab = yLab, xlab='', cex.lab=1.6, cex.main=1.6)
  plot(bHist, col=rgb(1,0,0,.3), freq=F, add=T)
  
  # eixo x da distribuição normal a ser calculada (quantis dos p90)
  xNorm = seq(15, 50, .1)
  
  # valores de densidade (distribuição normal) correspondentes aos quantis xNorm
  # para borda e interior, respectivamente
  bNorm = dnorm(xNorm, mean(denBorder), sd(denBorder))  
  iNorm = dnorm(xNorm, mean(denInner), sd(denInner))
  
  # plot das curvas normais de borda e interior
  lines(xNorm, iNorm, col = rgb(0,0,210/255), lwd=2)
  lines(xNorm, bNorm, col = 'red', lwd=2)
  
  # cálculo do intervalo de confiança para a borda
  cfBorder = confint(lm(denBorder~1), level = .95)
  cfText = paste('CI:',round(mean(denBorder), 4), '±', round(diff(cfBorder[1,]/2),4), 'm')
  
  # número e proporção de observações na borda para a distância e categoria da iteração atual 
  n = length(denBorder)
  perc = round(100*n/nrow(df), 2)
  
  
  # testes estatísticos
  # t = t.test(denBorder, denInner, paired = F)
  # u = wilcox.test(denBorder, denInner, paired = F, exact = T, correct = T, conf.level = .95)

  # plot do texto sobre os gráficos
  text(35, .3, cfText, pos = 4, xpd=T)
  # text(35, .23, paste(n, 'pixels'), pos=4, xpd=T)
  # text(35, .16,  paste(perc, '%'), pos=4, xpd=T)
  
  # legenda
  if(distancia == 10 & i=='a'){
    legend('topleft', legend = c('Inner', 'Border'), lty = 1, lwd=2, col=c('blue', 'red'))
  }
}
}
# finalizar exportação de figura 
# dev.off()
}

# intervalo de confiança para o interior
cfInner = confint(lm(denInner~1), level = .95)
cfText = paste('CI:',round(mean(denInner), 4), '±', round(diff(cfInner[1,]/2),4), 'm')
n = length(denInner)
perc = round(100*n/nrow(df), 4)
