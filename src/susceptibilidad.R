#############################################################################
# titulo        : Herramienta para modelar la susceptibilidad del terreno
#                 a movimientos en masa, via aprendizaje automatizado;
# proyecto      : Tesis de grado, Master en Visualizacion y Big Data, Universidad
#                 Internacional de la Rioja;
# autor(es)     : Victoria Daniela Camacho Ochoa
# actualizacion : v1 Bogota, Colombia - Diciembre 2022;
# observaciones : favor consultar al correo victoria.kiki1987@gmail.com;
# modificado de : Coca et al. (2021)
##############################################################################
###
# ------------------------------------------------------- #
# Limpiar espacio de trabajo y configurar opciones
# ------------------------------------------------------- #
rm(list = ls()); options(scipen = 999, warn = -1)

# ------------------------------------------------------- #
# Definir espacio y archivo de configuracion de trabajo
# ------------------------------------------------------- #
### Directorio de los codigos R  (copiar la ruta completa donde estan los codigos 
### o carpeta src)
r.dir = 'C:/thesis_codes/src'

### Directorio del proyecto (copiar la ruta completa donde se aloja el proyecto 
### o carpeta proyecto)
# Indicar la ruta al proyecto
proyecto.dir = 'C:/thesis_codes/project'
# Cargar el archivo de configuracion
conf.file = paste0(proyecto.dir,'/config/conf.txt')

########################################################
####PARTE 2 - Generacion de los Datos                ###
########################################################

#### cargar modulo ###
source(paste0(r.dir,'/modules/1_Datos.R'))

########################################################
#####PARTE 3 - Seleccion variables (RFE y Boruta) ######
########################################################

#### cargar modulo ###
source(paste0(r.dir,'/modules/2_SelVariables.R'))

########################################################
#####PARTE 4a - Modelado: Exploracion de los datos    ##
########################################################
#### cargar modulo ###
source(paste0(r.dir,'/modules/3a_ModExploratorio.R'))

########################################################
#####PARTE 4b - Modelado: Ejecucion de los modelos    ##
########################################################
#### cargar modulo ###
source(paste0(r.dir,'/modules/3b_ModEjecutar.R'))

###########################################################
#####PARTE 4c - Modelado: Identificacion del mejor modelo #
###########################################################

#### cargar modulo ###
source(paste0(r.dir,'/modules/3c_ModMejor.R'))

######################################################################
#####PARTE 4d - Modelado: Evaluacion/Incertidumbre del mejor modelo ##
######################################################################

#### cargar modulo ###
source(paste0(r.dir,'/modules/3d_ModEvaluacion-Incertidumbre.R'))

###########################################################
#####PARTE 4e - Modelado: Uso del modelo (prediccion)    ##
#### cargar modulo ###
source(paste0(r.dir,'/modules/3e_ModUso.R'))

#### fin de la modelacion ###