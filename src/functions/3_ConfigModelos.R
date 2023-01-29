#############################################################################
# titulo        : Configuracion modelos y calculo de importancia de las covariables;
# proposito     : Configurar modelos por DEFECTO o llamados por CONFIG;
# autor(es)     : Victoria Daniela Camacho Ochoa;
# creacion      : v1 Bogota, Colombia - Diciembre 2022;
# entrada       : N/A;
# salida        : Funciones;
# observaciones : Agregar a la funcion de importancia de las variables modelos que no 
#                 tienen esa opcion en la libreria caret;
##############################################################################

dict <- new.env(hash = TRUE)
Add <- function(key, val) dict[[key]] <- val

modelos.config.manual <- function(){
  modelos.lista <- c('J48', 'C5.0', 'ranger', 'svmLinear','multinom',
                    'xgbTree', 'gbm_h2o', 'glmnet', 'mlp', 'svmRadial','cubist')
  
  modelos.dict = mapply(Add, modelos.lista, modelos.lista)

  #opcion defecto: todos con un mismo tamano para busqueda de mejores hiperparametros
  tuneLenght_size <- rep(20, length(modelos.lista))
  tuneLenght = mapply(Add, modelos.lista, tuneLenght_size)
  
  ##opcion alternativa: tamano para busqueda de mejores hiperparametros por modelo
  # tuneLenght <- c('J48'=5, 'C5.0'=5, 'multinom','ranger'=20, 'svmLinear'=5, 'xgbTree'=20, 'gbm_h2o'=3,
  #                'glmnet'=5,'mlp'=5, 'svmRadial'=20)
  
  conflist = list(modelos.dict, tuneLenght)
  names(conflist) = c('modelos.dict','tuneLenght')

  return (conflist)
  
}

modelos.variables.importancia <- function(modelo,nombre){
  if (nombre == 'ranger'){
    
    finalModel <- modelo$finalModel

    #IMPORTANCIA VARIABLES
    imp <-as.vector(finalModel$variable.importance)
    variable <- names(finalModel$variable.importance)
    r <-data.frame(variable=variable,importance=imp)

    p <- ggplot(r, aes(x=reorder(variable,importance), y=importance,fill=importance))+ 
      geom_bar(stat="identity", position="dodge",fill = "darkgrey")+ coord_flip()+
      ylab("Importancia") +
      xlab("Covariable")+
      guides(fill=F) +
      theme_bw() +
      theme(text=element_text(size=18))
    
  } else{
    p <- NULL
  } 
  
  return (p)
}