---
author: Ícaro
cover: img/aws.png
date: "2020-05-05"
description: 'Nesse post aprendemos como colocar nossas aplicações para rodar na AWS.'
title: 'Rodando modelos [R] na nuvem - parte 3: AWS'
---

# **Parte 3: Rodando aplicações na AWS**

Esse tutorial é a terceira parte da série __Rodando modelos [R] na nuvem__, na primeira parte aprendemos como criar APIs com o pacote `plumber` (se você ainda não leu clique [aqui](https://icaroagostino.github.io/post/plumber)). Na segunda parte aprendemos a colocar nossa aplicação em containers utilizando `Docker` (se você ainda não leu clique [aqui](https://icaroagostino.github.io/post/docker)). E nessa parte final vamos subir esse container para a cloud da Amazon ([AWS](https://aws.amazon.com/)).

![](fig1.jpg#center)

O foco aqui será subir container contendo aplicações em `R` para rodar na cloud da Amazon. Vamos entender como configurar um servidor AWS e subir a aplicação para rodar no servidor. Como falei na [parte 1](https://icaroagostino.github.io/post/plumber/) e na [parte 2](https://icaroagostino.github.io/post/docker/), esse é um passo intermediário entre o desenvolvimento local de um modelo e a fase de produção, quando colocamos o modelo para rodar em um sistema real, ficando disponível para receber requisições de predições.

Se você ainda é iniciante em `R` e tiver dificuldades para executar o tutorial, no final dessa página tem uma indicação de por onde começar com os passos básicos de `R`.

*Vamos começar...*

![](https://media1.tenor.com/images/96c25e8638b6c7eab5df0b58c9717117/tenor.gif#center)

## Rodando modelos localmente e em ambientes Cloud

Hoje o que chamamos de cloud se trata de servidores físicos que podem ser alugados de empresas que proveem tais serviços, dessa forma é possível rodar aplicações sem ter um servidor físico localmente, alugando uma máquina cloud para sua aplicação. Hoje temos três grandes empresas que fornecem esse tipo de serviço: a microsoft com a [Azure](https://azure.microsoft.com/pt-br/), a [Google Cloud](https://cloud.google.com/) e [Amazon Web Service](https://aws.amazon.com/pt/).

A grande diferença entre rodar um modelo ou aplicação na sua máquina local e rodar em um servidor cloud é a possibilidade de expor a aplicação online, possibilitando integrar com outros serviços e escalar sua aplicação. O objetivo aqui é servir como um passo introdutório para aplicações em cloud, servindo como primeiros passos nesse mundo.

## Amazon Web Service

Nesse tutorial vamos utilizar a [Amazon Web Service](https://aws.amazon.com/pt/) como opção, as outras clouds também funcionam muito bem, a escolha aqui é pela simples comodidade de já ter mais conhecimento com esse serviço. Você precisará criar uma conta na AWS gratuitamente, a máquina que vamos usar para nossa aplicação também será gratuita, você não terá nenhum custo para executar esse tutorial =)

Faça o seu cadastro no site da AWS: https://aws.amazon.com/pt/

Obs.: provavelmente será pedido para cadastrar um cartão de crédito, mas não se preocupe, vamos utilizar apenas serviços gratuitos.

Agora vamos criar nosso servidor utilizando o serviço EC2, para isso após criar a sua conta e logar no sistema você estará em um página como essa:

![](fig1.png#center)

No canto superior direito você verá a região que seu serviço será criado, selecione o local mais próximo de onde o serviço será consumido. Nessa caso a AWS tem servidores no Brasil, na região de São Paulo. Clique em em EC2 conforme destacado no print acima.

Após isso você verá seu painel EC2, procure a opção __Launch instance__:

![](fig2.png#center)

Você será direcionado para uma página com diversos tipos de serviços disponíveis, vamos selecionar o __Ubuntu Sever 18.04 LTS (HVM) SSD Volume Type__. Note que essa opção tem uma `Tag` escrita _free tier eligible_, o que significa que podemos utilizar opções de teste gratuitos para esse serviço.

![](fig3.png#center)

Após selecionar essa opção você verá as várias configurações possíveis para o servidor em termos de hardware, perceba que é possível com algum custo consumir serviços de máquinas extremamente poderosas. No nosso caso a opção gratuita nos fornece apenas 1GiB de memória, apesar de pouca é o suficiente para rodar nossa aplicação, lembrando que servidores não possuem interface gráfica, o que torna mais eficiente o uso dos recursos.

![](fig4.png#center)

Após marcar a opção de teste gratuito clique em __6. Configure Security Group__. Aqui vamos liberar o acesso ao servidor com algumas configurações de segurança, note que esse post não aborda a parte de segurança em servidores, vamos tratar essa máquina virtual como um ambiente de desenvolvimento negligenciando questões de segurança, de forma alguma coloque dados ou arquivos pessoais nesse servidor. Para acessar a nossa aplicação configure as portas da seguinte forma:

![](fig5.png#center)

Aqui estamos dando acesso `SSH`, `HTTP` e `HTTPS` para qualquer IP que conheça o endereço do nosso servidor e que tenha a chave de acesso (note que a opção _Anywhere_ foi selecionada para source, não definindo um IP especifico de acesso). Após isso clique em __7. Review__ e depois em `Launch` no canto inferior direito. Uma janela para criação de chaves surgirá:

![](fig6.png#center)

Aqui escolha __Create a new key pair__, depois dê um nome a sua chave e clique em `Download Key Pair`. Um arquivo de chave será baixado, é importante que você não perca esse arquivo, não haverá outra forma de acessar sua máquina sem a chave. Depois clique em __Launch__ e o servidor será criado, na sequencia você será direcionado para um página informando que tudo deu certo, clique em __View instances__ e você estará no painel de gerenciamento de suas instancias.

![](fig7.png#center)

Você talvez precise esperar alguns segundos ou minutos até tudo estar pronto, você saberá que está tudo OK quando o indicador de __running__ estiver verde. Aqui podemos nomear nossa instancia conforme indicado no print acima.

![](fig8.png#center)

Clicando com o botão direto na instância você verá as principais opções, em `Image State` você terá os principais comandos, como reiniciar, parar e terminar aquele servidor. Clicando em connect você verá as instruções para se conectar a sua máquina =)

![](fig9.png#center)

Aqui vamos precisar copiar esse comando destacado para nos conectar a nossa máquina criada. Agora podemos abrir o terminal, no meu caso o windows, mas o precedimento será semelhante em outros sistemas. Após abrir o terminal você deve ir até a pasta que salvou sua chave, no meu caso eu salvei em dentro de uma pasta nomeada AWS nos meus documentos `Documents/AWS`, para ir para esse local eu usei o comando `cd Documents/AWS`, aqui você deverá ir para o local especifico do seu computador, para ver se você está na pasta correta pode usar o comando `dir` no windows ou `ls` no linux, se tudo tiver certo você verá o arquivo da chave no diretório.

![](fig10.png#center)

Agora podemos colar aquele comando que copiamos lá da página da AWS e dar enter (para colar texto copiado no terminal windows use o botão direto do mouse, no Ubuntu use Ctrl+Shift+V), como é a primeira vez que usamos essa chave será pedido uma confirmação, escreva sim ou yes dependendo da sua configuração, se tudo ocorrer certo vamos nos conectar ao nosso servidor:

![](fig11.png#center)

Pronto, nosso servidor já está configurado e estamos acessando ele remotamente, note que em verde agora aparece o usuário `Ubuntu` com o IP do servidor. Agora vamos instalar o `Docker` e baixar nossa aplicação, colocar para rodar e testar o funcionamento.

## Subindo containers and Run!

Para instalar o `Docker` nessa máquina vamos utilizar os mesmos passos da parte anterior desse tutorial, utilizando a sequência de comandos abaixo:

```visual-basic
sudo apt update
sudo apt install docker.io
```

Após cada comando acima você verá várias arquivos sendo baixados e instalados, fique atento pois possivelmente você precisará confirma alguma instalação com `Y`. Após todo o processo terminar vamos baixar o container que criamos na [Parte 2](https://icaroagostino.github.io/post/docker/) desse tutorial. Aqui se você não tiver criado seu próprio container e subido para o `Docker Hub` pode usar o meu, ele ficará disponível por algum tempo lá. Para isso use o comando abaixo, ou ajuste o seu nome de usuário e do container caso for utilizar seu próprio container:

```visual-basic
sudo docker pull icaroagostino/modelo-linear
```

A imagem será baixada contendo tudo necessário para nossa aplicação, incluindo o `R` base, os pacotes e a própria aplicação. Lembrando que a grande vantagem de utilizar o `Docker` é que nesse caso não precisamos instalar o `R`, configurar pacotes e nada disso, já está tudo pronto dentro do container. Após baixar, utilize o comando abaixo para rodar:

```visual-basic
sudo docker run -d -p 8080:8080 icaroagostino/modelo-linear
```

Um código do container será gerado se tudo der certo. Agora voltando a página da AWS podemos copiar o link para o DNS público do nosso servidor.

![](fig12.png#center)

Cole o link no seu navegar e adicione no final `:8080/__swagger__/` (lembrando que 8080 é a porta que escolhemos quando criamos a API). Se tudo deu certo nossa API está no ar =). Você pode testar manualmente na interface do `Swagger` e verá que tudo está funcionando:

![](fig13.png#center)

Se você quiser deixar rodando basta digitar `exit` no terminal que você desconectará do servidor. Para finalizar essa seção vamos fazer uma requisição de predição ao nosso servidor utilizar o `Rstudio`. Para isso basta utilizar o comando que já aprendemos na Parte 1, trocando apenas o endereço que antes era `localhost` para o link do servidor. Na seção seção do `R` use o seguinte comando (não esqueça de trocar o link para o seu servidor):

```visual-basic
library(httr)
library(magrittr) # operador pipe

POST(url = 'http://ec2-18-231-171-23.sa-east-1.compute.amazonaws.com:8080/modelo',
     encode = 'json',
     body = list(Wind = 20,
                 Temp = 70,
                 Month = 8)
) %>% 
  content %>%
  unlist
```

Você deve receber esse resultado

```visual-basic
##
## [1] -7.7152
##
```

Agora sua aplicação está finalmente rodando na nuvem, sendo acessível de qualquer lugar que tenha acesso a internet.

Aqui vai um pequeno _disclaimer_ sobre segurança, novamente é importante lembrar que esse servidor não está seguro para produção, é a penas uma máquina de desenvolvimento, trate esse tutorial como um primeiro passo. Possivelmente uma aplicação de predição não ficaria acessível dessa forma ao usurário final, sendo mais provável ser consumida por outro serviço que usaria esse resultado.

Outro cuidado que pode gerar custos é iniciar mais de uma instancia simultaneamente, isso consumirá o dobro de horas e passará do limite gratuito mensal, então nunca inicie duas instancias simultaneamente, termine uma antes de iniciar a próxima, a menos que esteja ciente dos custos.

Uma única instancia do tipo _free tier_ ligada ininterruptamente nunca passara do limite de horas gratuitas por mês. Outra limitação é que essa opção gratuita é valida por um ano, então não esqueça uma máquina ligada para sempre, após um ano você começará a ser cobrado =)

## Gerenciando containers

De volta ao Terminal, acessando novamente o nosso servidor podemos usar um conjunto de comandos simples para gerenciar nossos containers.

Para ver a lista de containers que estão rodando use:

```visual-basic
sudo docker ps
```

_Resultado:_

```visual-basic
CONTAINER ID  IMAGE         COMMAND                 CREATED
6d2ce98af45a  modelo-linear "R -e source('API.R')"  11 seconds ago

STATUS        PORTS                   NAMES
Up 7 seconds  0.0.0.0:8080->8080/tcp  peaceful_driscoll
```

Para parar esse container use (aqui temos que colocar do ID do container):

```visual-basic
sudo docker stop 6d2ce98af45a
```

Para ver a lista de imagens disponíveis:

```visual-basic
sudo docker images
```

_Resultado:_

```visual-basic
REPOSITORY                  TAG     IMAGE ID      CREATED       SIZE
icaroagostino/modelo-linear latest  ce4703adbb16  2 hours ago   901MB
```

Para remover essa imagem use:

```visual-basic
sudo docker rmi -f icaroagostino/modelo-linear
```

Para baixar a mesma imagem use novamente:

```visual-basic
sudo docker pull icaroagostino/modelo-linear
```

Para iniciar um container baseado nessa imagem, utilize novamente:

```visual-basic
sudo docker run -d -p 8080:8080 icaroagostino/modelo-linear
```

Para aprender a mexer mais no docker sugiro sempre ir na documentação oficial do projeto, que é bastante clara e objetiva: https://docs.docker.com/

## Chegamos ao fim

Finalmente chegamos ao fim dessa sequência de tutoriais que abordou o fluxo completo para criação de APIs, containers e cloud, muita gente interagiu comigo nas semanas que fui postando as partes e dando _feedbacks_ positivos =). Então se esse tutorial está sendo útil para você e foi possível executar tudo, ou mesmo se teve algum problema me marque em um tweet, meu user é `@icaroagostino`. Você pode me seguir nas minhas redes (nos botões a seguir), se quiser interagir comigo o twitter é a plataforma que mais uso para se comunicar com a comunidade de `R`.

{{< mysm >}}

Os dados utilizados nesse exemplo são públicos, para mais detalhes baixem os scripts [neste](https://github.com/icaroagostino/tutoriaisBlog/tree/master/docker/) repositório.

> `R` é uma linguagem de programação open source. Todos os recursos, bibliotecas, dados e implementações são gratuitos e desenvolvidos pela comunidade.
>
> Se você tem interesse de aprender a instalar o R e os passos básicos para iniciar nesse mundo sugiro o tutorial básico desenvolvido pelo pessoal do curso-r: http://material.curso-r.com/instalacao/

---

Até o próximo post, bons estudos em `R` :D

Você pode compartilhar esse post nas redes sociais utilizando os botões no fim dessa página.

![](https://media1.tenor.com/images/cb27704982766b4f02691ea975d9a259/tenor.gif#center)