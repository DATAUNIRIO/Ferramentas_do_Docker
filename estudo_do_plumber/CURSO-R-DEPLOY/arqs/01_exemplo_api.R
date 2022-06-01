library(plumber)

#* Escreve uma mensagem
#* @param msg A mensagem para escrever
#* @get /echo
function(msg = "") {
  
  paste0("A mensagem é: '", msg, "'")
}

#* Retorna a soma de dois números
#* @param a O primeiro número
#* @param b O segundo número
#* @post /sum
function(a, b){
  as.numeric(a) + as.numeric(b)
}


#* Retorna a soma de dois números
#* @serializer png
#* @post /plot
function(){
  plot(1:10)
}


#* Retorna o csv
#* @serializer csv
#* @post /base
function() {
  mtcars
}

#* Retorna o csv
#* @post /dados
function(senha) {
  if (senha!='s10e26a21'){
    stop("você tem que inserir a senha")
  }
  mtcars
}


