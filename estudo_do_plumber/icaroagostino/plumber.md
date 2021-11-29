---
author: Ícaro
cover: img/plumber.jpg
date: "2020-04-14"
description: 'Nesse post aprendemos a criar APIs utilizando o pacote plumber, e como se cominar com APIs utilizando a biblioteca httr.'
title: 'Rodando modelos [R] na nuvem - parte 1: Criando APIs'
---

# **Parte 1: Criando API's com o pacote plumber**

Essa sequência de tutoriais vai ser dividida em 3 partes, aqui vamos aprender a construir API's com o pacote `plumber`. **Na parte 2** vamos entender como colocar uma API dentro de um contêiner utilizando `docker`. E na **parte final** vamos abordar como colocar sua aplicação dockerizada para rodar na nuvem utilizando a Amazon Web Service ([AWS](https://aws.amazon.com/)).

![](fig1.jpg#center)

Nesse post vamos aprender como transformar um modelo estatístico/ML em uma API utilizando `R` com o pacote `plumber`. Esse é um passo intermediário entre o desenvolvimento local de um modelo e a fase de produção, quando colocamos o modelo para rodar em um sistema real, ficando disponível para receber requisições de predições. Tudo que aprendermos aqui serve para outros tipos de aplicações, no futuro vou fazer um post para ensinar a rodar uma aplicação `shiny` na nuvem.

Para esse exemplo vamos utilizar um modelo simples de regressão, com dados do próprio `R` base, a ideia aqui não é se aprofundar na modelagem, mas sim mostrar como colocar um modelo já treinado para funcionar no mundo real. Se você se interessa por previsão, tem dois posts aqui no blog sobre o assunto utilizando modelos [ARIMA](https://icaroagostino.github.io/post/arima) e de [Redes Neurais](https://icaroagostino.github.io/post/ann). Se você ainda é iniciante em `R` e tiver dificuldades para executar o tutorial, no final dessa página tem uma indicação de por onde começar com os passos básicos de `R`.

Todos os `scripts` desse tutorial estão hospedados no meu Github, clique [aqui](https://github.com/icaroagostino/tutoriaisBlog/tree/master/plumber/) para acessar.

*Vamos começar...*

![](https://media.giphy.com/media/Lm63QU87HvgVEuTV63/giphy.gif#center)

## Ajustando um modelo simples de regressão

Vamos agora ajustar um modelo linear de regressão, nessa parte do tutorial não vou entrar em detalhes, pois o foco será as etapas seguintes. Vamos utilizar o dataset `airquality` disponível no `R` base. Usando `head()` podemos ver o início do `dataset`:

```visual-basic
head(airquality)

##
##   Ozone Solar.R Wind Temp Month Day
## 1    41     190  7.4   67     5   1
## 2    36     118  8.0   72     5   2
## 3    12     149 12.6   74     5   3
## 4    18     313 11.5   62     5   4
## 5    NA      NA 14.3   56     5   5
## 6    28      NA 14.9   66     5   6
##
```

Esse dataset tem 6 variáveis:

  - __Ozone__: Ozônio médio em partes por bilhão
  - __Solar.R__: Radiação solar (frequência 4000--7700)
  - __Wind__: Velocidade média do vento em milhas por hora
  - __Temp__: Temperatura máxima diária em graus Fahrenheit
  - __Month__: Mês da medição (1-12)
  - __Day__: Dia da medição (1-31)

Vamos estão construir nosso modelo, faremos a predição da variável `Ozone` em função das variáveis `Wind`, `Temp` e `Month`. Para isso vamos utilizar a função `lm()` que ajusta modelos de regressão linear, precisamos passar a formula e o conjunto de dados para a função.

```visual-basic
modelo <- lm(Ozone ~ Wind + Temp + Month, data = airquality)

summary(modelo) # Resumo do modelo

## 
## Call:
## lm(formula = Ozone ~ Wind + Temp + Month, data = airquality)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -43.685 -14.413  -2.425  10.851 102.522 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -64.2504    23.3283  -2.754  0.00687 ** 
## Wind         -3.0382     0.6510  -4.667 8.53e-06 ***
## Temp          2.0690     0.2647   7.816 3.22e-12 ***
## Month        -3.4414     1.4946  -2.303  0.02315 *  
## ---
## Signif. codes:  
## 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
```

O nosso modelo ajustado ficou com todos os parâmetros significativos (α = 0.05). Agora vamos desenvolver uma API simples baseado no modelo ajustado. __Mas antes, o que é uma API?__

>Se você já sabe o que é e como funciona uma API pode pular para a próxima seção [Criando uma API](#criando-uma-api).

## Interfaces de Programação de Aplicações - APIs

API significa __Interface de Programação de Aplicações__ (do inglês _Application Programming Interface_), é um conjunto de rotinas e padrões estabelecidos por um software para a utilização das suas funcionalidades por aplicativos que não pretendem envolver-se em detalhes da implementação do software, mas apenas usar seus serviços.

Aqui vamos abordar APIs que se comunicam pelo protocolo `HTTP` (Protocolo de Transferência de Hipertexto), trata-se de um padrão de comunicação utilizado por aplicações Web. Juntamente com a arquitetura `REST` (Transferência Representacional de Estado), em que diversas aplicações estão disponíveis para serem acessadas via API.
O RedHat fez uma postagem bastante completa sobre o funcionamento de APIs e um pouco da história, se você quiser se aprofundar pode clicar [aqui](https://www.redhat.com/pt-br/topics/api/what-are-application-programming-interfaces).

![](fig2.png#center)

[Photo by RedHat](https://www.redhat.com/pt-br/topics/api/what-are-application-programming-interfaces)

Através de APIs é possível, por exemplo, calcular a distância entre dois lugares utilizando o [__Open Route Service__](https://openrouteservice.org/), que é um serviço gratuito que fornece aplicações para geolocalização e dados de transporte similar ao Google Maps. O [__Spotify__](https://developer.spotify.com/documentation/web-api/) (sim o serviço de música por _streamer_) fornece uma API gratuita para coletar metadados de músicas, artistas, álbuns e etc.

![](fig3.png#center)

[Photo by Spotify](https://developer.spotify.com/documentation/web-api/)

Outra API que foi desenvolvida recentemente é a [__covid19api__](https://covid19api.com/) que permite acessar dados atualizados de casos Covid-19 no mundo inteiro, os dados são obtidos de forma dinâmica utilizando como fonte dados da [Johns Hopkins CSSE](https://github.com/CSSEGISandData/COVID-19).

Basicamente precisamos enviar uma requisição para o URL onde esse serviços estão hospedados e vamos receber um conjunto de dados de retorno, existem centenas de API disponíveis na internet, para os mais diversos e variados tipos de serviços. Na parte final desse tutorial vamos aprender como se comunicar com APIs, usaremos um dos exemplos acima e claro nossa própria API também.

## Criando uma API

Para criar uma API em `R` vamos utilizar o pacote [`Plumber`](https://www.rplumber.io/) desenvolvido pela equipe do [Rstudio](rstudio.com/).

![](fig4.jpg#center)

[Photo by Plumber](https://www.rplumber.io/docs/)

Dessa forma a IDE possuí suporte nativo para criação do APIs com `plumber`. Para criar uma nova API clique em __Novo arquivo__ e depois em __plumber API__. Após dar um nome e escolher um diretório, um exemplo pronto será aberto. Agora vamos tentar entender o padrão que devemos utilizar para desenvolver uma API em `plumber`.

![](fig5.jpg#center)

Para configurar a API devemos utilizar a notação `#*` combinados com alguns termos:

  - `@apiTitle` para dar o título da aplicação
  - `@param` para nomear parâmetros
  - `@get` e/ou `@post` para definir o tipo de comunicação
  
__GET__ e __POST__ são comandos padrões para APIs que utilizam a arquitetura `REST`, ambos são utilizados para comunicação, a maior diferença entre eles é que com o `post` é possível especificar melhor os parâmetros, sendo mais indicados para aplicação com múltiplos parâmetros. Além disso é possível antes dos parâmetros definir um `Endpoint` que basicamente é uma das 'saídas' da API, pois uma mesma API pode prover diferentes serviços com diferentes saídas.

Por padrão o `plumber` transforma funções em `R` em APIs, dessa forma precisaremos escrever nossa função que ao receber os dados de entrada realizará a predição de `Ozonio`. Podemos apagar o exemplo pronto que veio na criação do arquivo e vamos colocar nosso modelo em API.

```visual-basic
library(plumber)

modelo <- lm(Ozone ~ Wind + Temp + Month, data = airquality)

#* @apiTitle Exemplo de API [modelo linear]

#* Retorna a distancia percorrida em função da velocidade
#* @param Wind Velocidade do vento (mph)
#* @param Temp Temperautura (degrees F)
#* @param Month Mês (1-12)
#* @post /modelo

function(Wind, Temp, Month) {
  
  Wind  <- as.double(Wind)  # converte para numero
  Temp  <- as.double(Temp)  # converte para numero
  Month <- as.double(Month) # converte para numero
  
  resultado <- predict(modelo, list(Wind = Wind,
                                    Temp = Temp,
                                    Month = Month))
  
  return(resultado)
  
}
```

Aqui vão mais alguns detalhes, é possível na inicialização da API rodar códigos de `R` normalmente, então logo após carregar a biblioteca `plumber` vamos ajustar o modelo. No nosso caso isso não é um problema, pois o ajuste do modelo linear é bem rápido, mas se você estiver usando um modelo mais pesado, como uma Rede Neural Profunda, você pode salvar seu modelo em objeto `.Rdata` e apenas carregar ele na inicialização da API, sem necessidade de ajuste a cada inicialização.

Outro detalhe é que por padrão o `plumber` recebe os dados como texto, para isso aplicamos uma conversão para número no inicio da função. Na sequencia usamos a função `predict` para calcular o resultado e retornamos o valor final. Agora que nossa API está pronta basta clicar em `Run API`, que agora fica no lugar do `run` tradicional no `script`.

![](fig6.png#center)

Perceba que abriu uma janela parecida com a imagem acima, nossa API está rodando em `localhost`, ocupando uma porta do nosso computador. A interface que você está vendo se chama [`Swagger`](https://swagger.io/), serve como padrão para construção de APIs, permitindo que possamos interagir graficamente com a aplicação.

Clicando em `post` vai abrir a descrição dos parâmetros de entrada. Clicando em `Try it out` podemos manualmente testar API, colocando os valores:

  - `Month = 6`
  - `Temp = 80`
  - `Wind = 20`
  
  Clicando em `execute` podemos ver no `Response body` o resultado **19.8577**.

![](fig7.png#center)

Perceba que o `swagger` também nos fornece o código em `curl` para se comunicar com a API. Se você já tem algum conhecimento de linha de comando, pode copiar esse código e colar no `Prompt`. O valor retornado será o mesmo. Localmente nossa API já está funcionando, estamos utilizando nosso modelo para receber requisições do tipo `post` processar os dados de entrada e retornar uma predição :D

Agora vamos aprender como usar o próprio `R` para se comunicar com a API que acabamos de criar e com outras APIs externas. Você pode parar sua API fechando a janela. Salve sua API como um arquivo `.R` normal, dentro de uma pasta do seu computador separada, sugiro usar o nome `modelo.R` para que possa acompanhar o restante do tutorial sem problemas.

## Se comunicando com API's

Podemos abrir outra seção do `Rstudio` e utilizar a biblioteca [`httr`](https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html) para se comunicar com APIs que utilizam arquitetura `REST`. Antes de realizar as requisições vamos criar um novo `script` para chamar nossa API de forma mais fácil e endereçada. Crie um novo arquivo da seguinte forma:

```visual-basic
library(plumber)

API <- plumb('modelo.R')

API$run(host = '0.0.0.0',
        port = 8080,
        swagger = T)
```

Antes de rodar o `script` acima não esqueça de definir a pasta que você criou e que está com seu modelo salvo. Para isso vá em `Session -> Set working diretory -> Choose diretory` ou use o atalho `Crtl + shift + H`. Agora rodando o `script` o `swagger` vai abrir novamente, com a diferença que a porta `8080` será fixa como definida no `script`.

Agora abra outra seção do `Rstudio` (não apenas um novo `script`, mas um novo "programa") para criarmos um `script` que se comunicará com a nossa API. Vamos utilizar a função `POST` da biblioteca `httr`, da seguinte forma:

```visual-basic
library(httr)
library(magrittr) # operador pipe

POST(url = 'http://127.0.0.1:8080/modelo',
     encode = 'json',
     body = list(Wind = 20,
                 Temp = 70,
                 Month = 8)
     ) %>% 
  content %>%
  unlist
```

A função `POST` executa requisições do tipo `post`, precisamos passar o endereço onde a API está no argumento `url`, o argumento `encode` que no caso do `plumber` é `.json`, e os parâmetros da API pelo argumento `body`. Nesse último devemos criar uma lista que contém os parâmetros com os nome corretos e os respectivos valores.

Utilizando o operador pipe `%>%` podemos passar o resultado da requisição para a função `content` também da biblioteca `httr`, essa função extrai apenas o conteúdo do resultado, por padrão outros metadados vêm juntos, depois você pode rodar esse `script` sem a função `content` para analisar as demais informações que retornam. Por fim utilizamos a função `unlist` para extrair o conteúdo final que por padrão vem em forma de lista. Rodando esse código o resultado será esse:

```visual-basic
##
## [1] 19.8577
##
```

Agora estamos com o modelo rodando localmente na porta `8080` e utilizando também o `R` para se comunicar com nosso modelo, nas próximas partes do nosso tutorial vamos aprender a colocar nosso modelo para rodar na nuvem, isso vai permitir que o modelo fique público podendo ser acessado por qualquer um que tenha internet. Salve esse `script` na mesma pasta, usaremos ele nas próximas partes.

Como bônus desse tutorial vamos nos comunicar com uma das APIs que falamos na seção anterior utilizando a biblioteca `httr`. Por facilidade vamos utilizar a API __covid19api__, já fiz outro tutorial de como se comunicar com essa API [aqui](icaroagostino.github.io/post/rmd-cov19/). A covid19api não precisa de nenhum tipo de cadastro para acessar.

Aqui vamos puxar os dados de casos confirmados do Brasil, utilizando a função `GET`:

```visual-basic
library(httr)
library(jsonlite)
library(magrittr)

# Consultando dados com GET
cov_br_conf <- GET(url = "https://api.covid19api.com/country/brazil/status/confirmed",
                   encode = 'json') %>%
  content %>%
  toJSON %>%
  fromJSON

# Visualizando as últimas linhas
tail(cov_br_conf[,6:9])

##
##       Lat    Lon Cases    Status
## 77 -14.24 -51.93 14034 confirmed
## 78 -14.24 -51.93 16170 confirmed
## 79 -14.24 -51.93 18092 confirmed
## 80 -14.24 -51.93 19638 confirmed
## 81 -14.24 -51.93 20727 confirmed
## 82 -14.24 -51.93 20962 confirmed
##
```

Como podemos ver, obtemos todo o histórico de dados do Brasil, e visualizamos com a função `tail` os dados dos últimos dias. A possibilidade de utilizar esse tipo de comunicação abre diversas portas, como falei anteriormente existem centenas de APIs disponíveis que fornecem dados em tempo real que podem ser integradas a sua aplicação, e muitas delas funcionam de forma gratuita.

As APIs do [__Spotify__](https://developer.spotify.com/documentation/web-api/) e do [__Open Route Service__](https://openrouteservice.org/dev/#/api-docs) precisam de chaves de acesso, você deve criar um cadastro que dará acesso a uma chave pessoal gratuitamente. Encorajo você a tentar aprender a mexer nessas outras APIs, clicando nos nomes delas nesse parágrafo. Você irá para as documentações, basta entender como fazer a requisição e escrever o código em `R` utilizando `httr`.

O modelo que criamos nesse tutorial, e depois convertemos para API é bem simples e inicial, crie modelos mais complexos, assim como APIs mais complexas com múltiplas saídas e tipos de requisições.

## Chegamos ao fim

A continuação desse tutorial, onde abordaremos o `Docker` está disponível clicando [aqui](https://icaroagostino.github.io/post/docker/). Você pode me seguir nas minhas redes (nos botões a seguir), se quiser interagir comigo o twitter é a plataforma que mais uso para se comunicar com a comunidade de `R`.

{{< mysm >}}

Os dados utilizados nesse exemplo são públicos, para mais detalhes baixem os scripts [neste](https://github.com/icaroagostino/tutoriaisBlog/tree/master/plumber/) repositório.

> `R` é uma linguagem de programação open source. Todos os recursos, bibliotecas, dados e implementações são gratuitos e desenvolvidos pela comunidade.
>
> Se você tem interesse de aprender a instalar o R e os passos básicos para iniciar nesse mundo sugiro o tutorial básico desenvolvido pelo pessoal do [curso-r](https://www.curso-r.com/): http://material.curso-r.com/instalacao/

---

Até o próximo post, bons estudos em `R` :D

Você pode compartilhar esse post nas redes sociais utilizando os botões no fim dessa página.

![](https://media.giphy.com/media/13HBDT4QSTpveU/giphy.gif#center)