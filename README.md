# Ferramentas_do_Docker

 Ferramentas_do_Docker
 
### https://aboland.ie/Docker.html  
### https://jsta.github.io/r-docker-tutorial/
### https://environments.rstudio.com/docker   

    Docker 101 for Data Scientists
    Layers in a Container
        Base Operating System
        System Dependencies
        R
        R Packages
        Code
        Data
    Example Registries
        Rocker Project
        R-Hub
        RStudio Images

### https://colinfay.me/docker-r-reproducibility/  
### https://hub.docker.com/r/rstudio/r-base  
### https://github.com/rstudio/r-docker   


# Como instalar o Docker?
Se o seu sistema operacional for linux, Ubuntu, por exemplo, você deve seguir os seguintes passos:  

1. Abra o terminal com o atalho Ctrl + Alt + T. Baixe as últimas atualizações do sistema.  
 $ sudo apt update && sudo apt upgrade

2. Instale utilizando o repositório do Ubuntu 18.04
 $ sudo apt install docker.io 

3. Inicie o Docker
 $ sudo systemctl start docker 

4. Entretanto, garanta que ele seja iniciado após a reinicialização
$ sudo systemctl enable docker 

5. Caso queira verificar a versão instalada
$ docker -v 


