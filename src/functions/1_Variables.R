#############################################################################
# titulo        : Variables objetivo;
# proposito     : Mapear las variables objetivo para modelado;
# autor(es)     : Victoria Daniela Camacho Ochoa;
# creacion      : v1 Bogota, Colombia - Diciembre 2022;
# entrada       : config.txt;
# salida        : listado de variables objetivo para su modelado en la herramienta;
# observaciones : ninguna;
##############################################################################

VariablesObjectivo = function()
{
  # Cargar archivo configuracion
  conf.args <- LoadConfig(conf.file)

  # Cargar componentes relacionados con este script
  project.folder <- conf.args[['proyecto.carpeta']]
  
  project.vars.categoricas <- conf.args[['vars.categoricas']]
  project.vars.categoricas = unlist(strsplit(project.vars.categoricas,';'))
  
  project.covars.vector <- conf.args[['covariables.vector']]
  project.covars.vector <- unlist(strsplit(project.covars.vector,';'))

  carpeta.entrada <- paste0(project.folder,'/datos/salida/matriz')
  datos.entrada <- read.csv(paste0(carpeta.entrada,'/MatrixDatos.csv'),sep=',')
  
  project.vars.categoricas <- paste(project.vars.categoricas, collapse='|')
  project.vars.continuas <- paste(project.vars.continuas, collapse='|')

  columnas.matriz <- names(datos.entrada[which(grepl(project.vars.categoricas,colnames(datos.entrada)) |  grepl(project.vars.continuas,colnames(datos.entrada)))])
  
  return(columnas.matriz)
}