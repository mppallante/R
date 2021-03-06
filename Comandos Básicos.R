##### CONVERSÃO DE FORMATO DE CARACTERES
Base$Coluna <- iconv(Base$Coluna, "latin1", "UTF-8")

##### SOBRESCREVER VALORES NULOS
Base$Coluna <- replace_na(Base$Coluna, 'NULO')

##### DELETAR LINHAS COM VALORES NULOS
Base <- drop_na(Base)

##### MUDAR TIPO DA COLUNA
Base$Coluna <- as.factor(Base$Coluna) #Fator
            <- as.character(Base$Coluna) #Texto
            <- as.numeric(Base$Coluna) #Numerico
            <- as_datetime(Base$Coluna) #DataTime
            
##### DATA & HORA - LUBRIDATE
second() # extrai os segundos.
minute() # extrai os minutos.
hour()   # extrai a hora.
wday()   # extrai o dia da semana.
mday()   # extrai o dia do mês.
month()  # extrai o mês.
year()   # extrai o ano.

##### DATA & HORA - CALCULO DE DIFERENÇA ENTRE DIAS
difftime(Base$DATA_FINAL, 
         Base$DATA_INICIO, 
         units = 'days')
         
 ##### SPLIT DE UMA COLUNA
 colsplit(Base$Coluna, 
          ';',  # separador
          names = tex)
          
##### FILTRO - SUBSET
subset[Base, EXPRESSÃO LOGICA]
                #  X>10 ...
                  
##### FILTRO PELA TABELA
Base[FILTRO LOGICO, EXPRESSÃO MATEMATICA, .(COLUNA1, COLUNA2, COLUNAn...)]

##### ESTATISTICA
## MEDIA
mean(dado)
## MEDIANA
median(dado)
## VARIANCIA
var(dado)
## DESVIO PADRÃO
sd(dado)
## QUARTIS 
quantile(dado)
## DESCRIÇÃO DOS DADOS
summary(dados)

##### DISTRIBUIÇÃO BINOMIAL
# X = Sucesso
# SIZE = nº de Experimentos
# PROP = Probabilidade de acertos (0 - 1)
dbinom(x, size, prob) 
