# Instrucciones de scripts para el análisis de ENVE 2014 en R

*Patricio R. Estévez Soto*  

## Versión 0.1

Estas instrucciones detallan brevemente el procedimiento necesario para correr exitosamente los scripts diseñados para el análisis de la ENVE 2014 en R.

La ejecución consta principalmente de dos partes, la **Instalación de paquetes requeridos**, y la **Ejecución del script**.

La instalación de paquetes solo se requiere realizar una vez. La ejecución del script puede realizarse las veces que sean necesarias si se encuentran errores.

## Instalación de paquetes requeridos

Los paquetes requeridos por este proyecto son los siguientes:

- **devtools**: Permite la instalación de paquetes especializados desde Github
- **victim**: Paquete desarrollado por el autor para facilitar este análisis
- **foreign**: Importa archivos .dbf a R
- **ggplot2**: Genera gráficos más atractivos que R básico
- **Cairo**: Permite guardar los gráficos como archivos png
- **lme4**: Genera modelos estadísticos multinivel
- **texreg**: Genera tablas complejas en varios formatos
- **R2admb**: Requerido por `glmmADMB`
- **coda**: Requerido por `glmmADMB`
- **glmmADMB**: Algoritmo utilizado para los modelos estadísticos multinivel (fuera de CRAN, requiere instalación desde código fuente).
- **classInt**: Genera intervalos para quantiles
- **dplyr**: permite más flexibilidad en la manipulación de datos
- **reshape2**: permite más flexibilidad en la manipulación de datos
- **lmtest**: Realiza pruebas de hipótesis al comparar modelos
- **car**: Realiza pruebas de hipótesis al comparar modelos
- **pscl**: Genera modelos zero-inflated

Algunos de estos paquetes pueden ya estar instalados en nuestra computadora.

**Para verificar qué paquetes no se encuentran ya instalados, ejecute el siguiente comando:**

```r
source("package_check")
```

Puede instalar los paquetes que hagan falta manualmente con los siguientes comandos:

```r
install.packages("devtools")
install.packages("foreign")
install.packages("ggplot2")
install.packages("Cairo")
install.packages("lme4")
install.packages("texreg")
install.packages("R2admb")
install.packages("classInt")
install.packages("reshape2")
install.packages("lmtest")
install.packages("car")
install.packages("pscl")
```

Para instalar el paquete `victim` es necesario usar el siguiente comando (requiere tener `devtools` instalado).

```r
devtools::install_github("prestevez/victim")
```

El paquete **glmmADMB** no se encuentra en los repositorios CRAN y debe de instalarse desde código fuente (para ello se requiere Rtools en un ambiente Windows; vea las instrucciones abajo). El paquete **glmmADMB** posiblemente requiera de la [**versión más nueva de R disponible en CRAN**](https://cran.r-project.org). De ser posible, asegúrese de contar con la versión más actualizada de R. De lo contrario, la instalación puede fallar.

Para instalar **glmmADMB**, pruebe con el siguiente código:

```r
install.packages("glmmADMB",
    repos=c("http://glmmadmb.r-forge.r-project.org/repos",
            getOption("repos")),
    type="source")
```

Es posible que deba instalar manualmente los paquetes **coda** y **R2admb**, pues son dependencias requeridas. Si la instalación de **glmmADMB** falla, intente instalar las dependencias primero.

Si lo prefiere, puede realizar una instalación automática de los paquetes requeridos usando el siguiente comando:

```r
source("package_installer.R")
```


### Instalación de Rtools
**La siguientes instrucciones están pensadas para un sistema Windows**.

Si la instalación reporta **tERROR: compilation failed for package "glmmADMB"**, o similar, será necesario instalar Rtools en la computadora.

Rtools permite la instalación de paquetes en R desde el código fuente (*source*). Para instalar Rtools navegamos a la página de [Rtools](http://cran.r-project.org/bin/windows/Rtools/), y descargamos el archivo ejecutable acorde a nuestra versión de R. Corremos el instalador seleccionando la opción default: "Package authoring installation".

Tras la instalación reinicie R y vuelva a correr el comando de instalación desde código fuente.

```r
install.packages("glmmADMB",
    repos=c("http://glmmadmb.r-forge.r-project.org/repos",
            getOption("repos")),
    type="source")
```

Finalmente, el script de análisis carga los paquetes, por lo que no es estrictamente necesario cargarlos de antemano.

## Ejecución del script

El proyecto está diseñado para requerir la menor intervención posible para su ejecución. Una vez puesto en marcha, el script generará los reportes y outputs relevantes en una carpeta para su posterior revisión y envío.

Además de la instalación de los paquetes necesarios, descrita en la sección anterior, la única intervención necesaria es colocar los archivos .dbf que contienen los datos de la ENVE en el folder del proyecto y asignar el *working directory* en R al folder del proyecto.

### Archivos .dbf ENVE 2014

El análisis utiliza ambas tablas de la ENVE, tanto del cuestionario principal como del módulo de delitos. Para poder ejecutar el script, es necesario copiar los archivos respectivos (**"enve2014cuest_ciega_2014.dbf"** y **"enve2014delitos_ciega_2014.dbf"**) al folder de este proyecto.

### Establecer working directory en R

Establezca el *working directory* en R para que apunte al folder del proyecto.

Por ejemplo:
```r
getwd() # Devuelve cuál es el working directory actual.

setwd("C:/R/ENVE_EstevezSoto") # Establece el working directory en el folder del proyecto
```

Sólo se requiere que el *path* al folder del proyecto sea especificado.

### Ejecutar el script

El proyecto está contenido en un archivo [R Markdown](http://rmarkdown.rstudio.com) que permite elaborar reportes automáticos de análisis realizados en R. Cuando el script termina, guarda los resultados en una archivo de texto con formato markdown (extensión `.md`) que puede ser abierto en cualquier editor de texto en cualquier computadora (Notepad, etc.).

Para correr el análisis, ejecute el siguiente comando en la consola de R:

```r
knitr::knit("Revision_analysis_2017.Rmd")
```

La ejecución es tardada, pero depende del equipo en el que se corra; es normal que parezca que no hace nada.

## Resultados

El script automáticamente realizará los análisis requeridos y guardará los resultados en un archivo llamado **Revision_analysis_2017.md** en la carpeta del proyecto. Asimismo, el programa guardará en una subcarpeta llamada `figure/` las gráficas elaboradas en el análisis.

## Licencia

Creative Commons Attribution 4.0 License (CC-BY) 2017 Patricio Rodrigo Estévez Soto

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons Licence" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.
