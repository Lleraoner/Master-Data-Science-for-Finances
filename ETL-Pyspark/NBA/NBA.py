
# coding: utf-8

# # Examen ETL: SPARK 02/02

# Se podrá utilizar toda la información que se encuentra en el campus. 
# 
# El fichero de datos sobre el que se trabajará es el de partidosLigaNBA.txt.
# 
# A cada una de las preguntas hay que responder explicando brevemente que se pretende hacer antes de lanzar el código.
# 
# Al documento lo llamareís con vuestro nombre y apellido. Debeís enviarlo a mi correo de CUNEF antes del final del examen.
# 
# El lenguaje para trabajar con Spark podrá ser python o R indistintamente.

# ## Primera pregunta: Describe brevemente que diferencia el persists, cache y collect en spark. Explica brevemente casos en los que es interesante su aplicación

# Persist <- te permite almacenar los datos tanto en la memoria
# Cache <- te permite guardar los datos en el caché
# Y collect es una acción de spark que lo que hace es sacar los resultados, equivale a take

# ## Segunda pregunta: Explica brevemente los pasos a seguir para realizar la carga de un conjunto de datos (pasos que se siguieron en la práctica con datos de logs)

# Primero lo que tenemos que realizar es la carga de las librerías pertinentes y necesarias para hacer la prácica, luego los 
# contextos necesarios que necesitemos. Más tarde leemos los datos con la ruta, los introducimos en nuestro contexto con sc().

# ## Tercera Pregunta: Índica un tipo de problema que puede empeorar los datos. (pe. Que no exista un representante del CDO en todas las áreas de negocio), pon algún ejemplo específico (pe. Datos duplicados) y cómo lo tratarías con técnicas de data cleaning.

# Por ejemplo que existan fechas y no estén en tipo fecha, que haya datos faltantes, o que haya filas o columnas que no contengan información
# Y lo único que nos hagan sea distorsionarnos el análisis, que haya variables string que deberían de ser numéricas, que las variables no estén escaladas para la realización de modelos...

# ## Cuarta tarea: Inicializar spark context y cargar los datos desde el fichero.

# In[5]:

import datetime as dt
import re
from datetime import *
import os
import pandas as pd
from time import time

from pyspark import SparkContext
from pyspark.sql import *


# In[2]:

sc = SparkContext()


# In[6]:

sqlCont = SQLContext(sc)


# In[8]:

datos = './partidosLigaNBA.csv'
raw_data = sc.textFile(datos)


# In[9]:

raw_data.take(5)


# In[10]:

data_clean = raw_data.map(lambda x:x.split(':'))
data_clean.take(5)


# In[11]:

header = data_clean.first()


# In[12]:

data_clean = raw_data.map(lambda x:x.split(':')).filter(lambda x:'Playoffs' not in x[0]).filter(lambda x: x != header)
data_clean.take(5)


# ## Quinta tarea: Media de la diferencia de puntos por año

# In[54]:

from pyspark.sql import *
from pyspark.sql.functions import *
from pyspark.sql.types import *
import datetime as dt


# In[85]:

data_frame = data_clean.map(lambda x:Row(fecha = dt.datetime.strptime((x[0]),'%a, %b %d, %Y').strftime('%m/%d/%Y'),
                                         hora = (x[1] +' : '+ x[2]),
                                         equipo_residente = x[5],
                                         equipo_visitante = x[3],
                                         puntos_visitante=int(x[4]),
                                         puntos_local=int(x[6])))



# In[102]:

df = sqlCont.createDataFrame(data_frame)
df.registerTempTable('interactions')


# In[103]:

df.show(5)


# In[104]:

df = df.withColumn('abs', abs(df.puntos_local - df.puntos_visitante)).show(2)


# Por RDD

# In[13]:

media_rdd = data_clean.map(lambda x: (x[0][len(x)-11:18], __builtin__.abs(int(x[4]) - int(x[6]))))

miTupla = (0, 0)
media_rdd.aggregateByKey(miTupla, lambda a,b: (a[0] + b,    a[1] + 1),
                                 lambda a,b: (a[0] + b[0], a[1] + b[1]))\
                                .mapValues(lambda x: x[0]/x[1])\
                                .sortBy(lambda x: x[0], False)\
                                .collect()


# ## Sexta tarea: ¿Han judado todos los equipos el mismo número de partidos? ¿ Si es qué no a que puede deberse?

# In[16]:

from operator import add


# In[19]:

equipos_visiantes = data_clean.map(lambda y: (y[3], 1))
equipos_locales = data_clean.map(lambda y: (y[5], 1))

agrupados_locales = equipos_locales.reduceByKey(add).sortBy(lambda x: x[1], False).collect()
agrupados_visitantes = equipos_visiantes.reduceByKey(add).sortBy(lambda x: x[1], False).collect()


# In[18]:

agrupados_locales


# In[20]:

agrupados_visitantes


# Como puede verse no han jugado los mismos numeros de partidos de local y visitante

# ## Séptima pregunta: ¿Cuantos partidos ha ganado en Enero Clevelant?

# In[24]:

cleve_vis = data_clean.filter(lambda x:x[3] == 'Cleveland Cavaliers').filter(lambda x:'Jan' in x[0]).filter(lambda x: int(x[4])>int(x[6]))
cleve_vis.count()


# In[28]:

cleve_local = data_clean.filter(lambda x:x[5] == 'Cleveland Cavaliers').filter(lambda x:'Jan' in x[0]).filter(lambda x: int(x[4])<int(x[6]))
cleve_local.count()


# In[29]:

# Sumar para tener el ejercicio completo, como podemos observar tenemos 41 + 42

cleve_local.count() + cleve_vis.count()


# ## Octava pregunta: ¿Los Warrios son mejores fuera de casa o en casa?

# In[206]:

victorias_warrior_local = data_clean.filter(lambda x:x[3] == 'Golden State Warriors').filter(lambda x: int(x[4])>int(x[6])).map(lambda x: (x[3],1)).reduceByKey(add).sortByKey().take(1)


# In[207]:

victorias_Warrior_visitante = data_clean.filter(lambda x:x[5] == 'Golden State Warriors').filter(lambda x:int(x[6])>int(x[4])).map(lambda x : (x[5],1)).reduceByKey(add).sortByKey().take(1)


# In[208]:

victorias_warrior_local + victorias_Warrior_visitante


# Como podemos observar, las victorias de Warriors fuera de casa son mayores que en casa, 308 > 215

# ## Novena pregunta: Equipo que ha quedado primerio en victorias más temporadas. (si es que hay alguno que más)

# ##  Décima pregunta: Escribe la expresión regular correcta que sólo macheen los teléfonos y el correo del siguiente texto.

# Si eres cliente y necesitas información sobre tus posiciones, productos o realizar operaciones: Desde España. Desde el extranjero. Banca telefónica en castellano. Bandera castellano. 902 13 23 13. Banca telefónica en catalán. Bandera catalana. 902 88 30 08. Banca telefónica en inglés. Bandera inglesa. 902 88 88 35. O por correo electrónico a atencioncliente@bankinter.com
