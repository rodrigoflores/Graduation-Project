# Nome: Rodrigo Luiz Marques Flores (flores@ime.usp.br)
# #USP: 5127470

#Escreva um script em R que faça o seguinte:
#- lê o arquivo iris_dataset.txt OK
#- separa os itens de cada classe em dois subconjuntos: um para estimar a função de densidade condicional e outro para teste (você pode variar o tamanho desses subconjuntos e ver o que acontece). OK
#- suponha que a função densidade condicional dos objetos de cada classe é uma normal. Faça a estimação paramétrica. OK
#- usando o classificador de Bayes (com a função densidade estimada), classifique os items de cada conjunto de teste.OK
#- Imprima o resultado da classificação (especifique quantos de cada classe foram classificados corretamente e erroneamente; quando a classificação estiver errada, especifique como foram classificados também). Você pode imprimir simplemente a matriz de confusão. OK

library(mvtnorm)

estimacaoParametrica <- function(input,adjustSample)
{
	n <- length(adjustSample)
	matrizDados = NULL
	for (i in 1:(length(input) - 1))
	{
		col <- input[[i]]
		dataAdjusted <- col[adjustSample]
		dataAdjusted.mean <- mean(dataAdjusted)
		dataAdjusted.sd <-   sd(dataAdjusted)
		normal <- rnorm(n,dataAdjusted.mean,dataAdjusted.sd)
		ajuste <- dnorm(dataAdjusted,dataAdjusted.mean,dataAdjusted.sd)
	}
	ajuste
}

mediasCovariancas <- function(input, adjustSample)
{
	n <- length(adjustSample)
	matrizDados = NULL
	means = NULL
	for (i in 1:(length(input) - 1))
	{
		
		col <- input[[i]]
		dataAdjusted <- col[adjustSample]
		matrizDados = cbind(matrizDados,dataAdjusted)
		means = c(means,mean(matrizDados[,i]))
	}
	#print(means)
	cov = cov(matrizDados)			
	#print(cov)
	#print(dmvnorm(matrizDados,means,cov))
	list(means=means, covariance=cov)
}

input <- read.table("iris_dataset.txt",header = T, sep='\t');


#Pegando o nome das espécies
species <- input[[5]]
#Selecionando apenas os elementos distintos
distinct_names <- unique(species)

#Para cada um destes elementos...

testData = NULL

for (i in distinct_names)
{
	# Selecionando os que são iguais aos elementos na tabela de dados
	selectedIndex <- which(distinct_names[i] == species,arr.ind = T)
	#Imprimindo o nome da espécie
	#message("Especie : ", appendLF = FALSE)
	message(distinct_names[i])	
	#Preciso selecionar alguns para teste
	
	#Escolhendo alguns aleatoriamente
	#sampleSize <- 1	
	sampleSize <- 20
	#Sorteando indices para escolher o grupo de estimação e o resto fica para o teste	
	adjustSampleIndexes <- sort(sample(c(1:length(selectedIndex)),sampleSize))
	
	adjustSample <- selectedIndex[adjustSampleIndexes]
	testSample   <- selectedIndex[-adjustSampleIndexes]
	testData <- c(testData, testSample)	
	
	#Fazer a estimação paramétrica	
	estimacaoParametrica(input,adjustSample)	
	#Montando a matriz
	x <- mediasCovariancas(input, adjustSample)
	if( distinct_names[i] == 'setosa')
	{
		covariancia.setosa <- x$covariance
		media.setosa <- x$means
	}
	else if (distinct_names[i] == 'versicolor')
	{	
		covariancia.versicolor <- x$covariance
		media.versicolor <- x$means
	}
	else
	{
		covariancia.virginica <- x$covariance
		media.virginica <- x$means
	}
}

acertos <- 0
acertos.setosa <- 0
acertos.virginica <- 0
acertos.versicolor <- 0
erros <- 0
erros.setosa <- 0
erros.virginica <- 0
erros.versicolor <- 0

names <- input[[5]];
for (i in sort(testData))
{
	message("Classificacao conhecida:  ",appendLF = FALSE)
	conhecida <- names[i]	
	message(conhecida)
	message("Valores de Densidades Multivariadas :")
	x <- NULL
	for (j in 1:(length(input) - 1))
	{
		col <- input[[j]]
		x <- c(x,col[i])
	}
	setosa <- dmvnorm(x,media.setosa,covariancia.setosa)
	message("Densidade para a Normal da Setosa     : ", appendLF = FALSE)
	message(setosa)
	versicolor <- dmvnorm(x,media.versicolor,covariancia.versicolor)
	message("Densidade para a Normal da Versicolor : ", appendLF = FALSE)
	message(versicolor)
	virginica <- dmvnorm(x,media.virginica,covariancia.virginica)
	message("Densidade para a Normal da Virginica  : ", appendLF = FALSE)
	message(virginica)
	message("Decisao ", appendLF = FALSE)
	if(setosa > versicolor & setosa > virginica)
	{
		decisao = "setosa"
		message(decisao)
		if(conhecida == decisao)
		{
			acertos <- acertos + 1
			acertos.setosa <- acertos.setosa + 1
			message("Correta!")
		}
		else
		{
			erros <- erros + 1
			erros.setosa <- erros.setosa + 1
			message("Equivocada!")
			message("Classificacao correta :", appendLF = FALSE)
			message(decisao)
	
		}
	}
	if(versicolor > setosa & versicolor > virginica)
	{
		decisao = "versicolor"
		if(conhecida == decisao)
		{
			acertos <- acertos + 1
			acertos.versicolor <- acertos.versicolor + 1
			message("Correta!")
		}
		else
		{
			erros <- erros + 1
			erros.versicolor <- erros.versicolor + 1
			message("Equivocada!")
			message("Classificacao correta :", appendLF = FALSE)
			message(decisao)
		}
	
	}
	if(virginica > setosa & virginica > versicolor)
	{
		decisao = "virginica"
		if(conhecida == decisao)
		{
			acertos <- acertos + 1
			acertos.virginica <- acertos.virginica + 1
			message("Correta!")
		}
		else
		{
			erros <- erros + 1
			erros.virginica <- erros.virginica + 1
			message("Equivocada!")
			message("Classificacao correta :", appendLF = FALSE)
			message(decisao)
	
		}

	}
	message("")
	message("************************************")
	message("")
}

message("Corretas para Setosa : ", appendLF = FALSE)
message(acertos.setosa)
message("Corretas para Versicolor : ", appendLF = FALSE)
message(acertos.versicolor)
message("Corretas para Virginica : ", appendLF = FALSE)
message(acertos.virginica)

message("")
message("************************************")
message("")

message("Equivocadas para Setosa : ", appendLF = FALSE)
message(erros.setosa)
message("Equivocadas para Versicolor : ", appendLF = FALSE)
message(erros.versicolor)
message("Equivocadas para Virginica : ", appendLF = FALSE)
message(erros.virginica)

message("")
message("************************************")
message("")

message("Corretas : ", appendLF = FALSE)
message(acertos)
message("Erradas : ", appendLF = FALSE)
message(erros)



message("Porcentagem de Acerto : ", appendLF = FALSE)
message(100*acertos/(erros + acertos))

