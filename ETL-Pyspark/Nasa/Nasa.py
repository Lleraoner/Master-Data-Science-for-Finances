
# coding: utf-8

# ## PRÁCTICA 1

# Cargamos nuestro entorno

# In[1]:

from pyspark import SparkContext
sc = SparkContext()


# In[2]:

# DEFINIMOS PRIMERAMENTE NUESTRAS VARIABLES
#0 ESLA URL 
# 1 NO ES NADA 
#2 NO ES NADA 
#3 ES LA FECHA QUE HABRÁ QUE CONVERTIR, 
#4 NO LO SE 
#5 PROTOCOLO DE RESPUESTA: GET Y POST 
#6 URL DEL ARCHIVO
#7 Código de respuesta
#8 Http status
#9 Tamaño


# In[3]:

import urllib
f = urllib.urlretrieve("https://www.dropbox.com/s/73wr8xb5s6fdj7g/apache.access.log.PROJECT?dl=1", "apache.access.log.PROJECT")


# In[2]:

#Cargamos y definimos nuestro data
data_file = "./apache.access.log.PROJECT"
raw_data = sc.textFile(data_file)


# Ya hemos cargado nuestro data, hemos intentado cargarlo con la ruta completa, pero el docker notiene acceso a 
# Todas nuestras carpetas Con el small, pero como no podemos cargar un archivo de mas de 25mb lo cargamos de dropbx

# In[3]:

##Contamos el numero de datos para explorar los datos
raw_data.count() 


# In[6]:

##Realizamos la exploración de los datos
raw_data.take(5)


# In[7]:

##Ahora definimos nuestro archivo limpio, que seguira siendo un rdd

data_clean = raw_data.map(lambda x: x.replace(' -', ' 0')).map(lambda x: x.split(" "))

data_clean.take(1)


# EJERCICIO 1

# In[8]:

##Calculamos el máximo 
data_clean.map(lambda x: int(x[-1])).max()


# In[9]:

##Calculamos el mínimo
data_clean.map(lambda x: int(x[-1])).min()


# In[10]:

##Calculamos la media

data_clean.map(lambda x: int(x[-1])).mean()


# NUMERO DE PETICIONES POR CADA CODIGO DE RESPUESTA

# In[11]:

##Ahora el ejercicio 2###############
# para el mismo se nos genera una tupla, todos los fallos con el número establecido

data_clean.map(lambda y: (int(y[-2]), 1)) 


# In[12]:

##Importamos las librerias necesarias.
from operator import add


# In[13]:

##Y ahora aplizamos el mapreduce, y que nos lo ordene de manera ascendente en función de su key que e
cod_response =data_clean.map(lambda y: (int(y[-2]), 1)).reduceByKey(add).sortByKey(ascending= True)


# In[14]:

cod_response.take(5)


# EJERCICIO 3
# - Mostrar 20 hosts que han sido visitados mas de 10 veces
# - Definimos la vaiable sin grupar bajo el critero de mapeo de la primera columna

# In[15]:

##Para el EJERCICIO 3 vamos a aplicar un filter, mostrar los 20 hosts, visitados mas de 10 veces


# In[16]:


hosts = data_clean.map(lambda y: (y[0], 1))
#ahora realizamos la reducción, y ordenación, y por último un filtro sobre la 2 columna que es la de conteo, ya que el filter
#Se aplica a el nuevo rdd creado por el map.
agrupation = hosts.reduceByKey(add).sortByKey().filter(lambda x: x[-1] > 10)



# In[17]:

##Ahora realizamos la agrupación de 20 host con mas de 10 visitas.
agrupation.take(20)


# Ejercicio 4
# - Encuentra los 10 endpoints maximos

# In[18]:


endpoint = data_clean.map(lambda y: (y[6], 1))


# In[19]:

##Aplicamos el reduce, luego lo ordenamos en el 2 valor de la tupla que es el contador, que nos lo ponga en orden descendiente, y que nos saque los 10 primeros.

top10endpoints = endpoint.reduceByKey(add).sortBy(lambda x: x[1], ascending = False).take(10)
top10endpoints


# Ejercicio 5
# - Top 10 de endpoints visitados sin el código de respuesta 200

# In[20]:

endpointno200 = data_clean.map(lambda y: (y[6], int(y[-2])))


# In[21]:


no_200 = endpointno200.filter(lambda x: x[1] != 200).take(10)
no_200


# EJERCICIO 6 
# - Este es el número de hosts distintos

# In[22]:

data_clean.map(lambda y: (y[0])).distinct().count()


# In[23]:

##Carga del nuevo contexto SQL
from pyspark.sql import Row, SQLContext
from pyspark.sql.functions import *
sqlContext = SQLContext(sc)


# EJERCICIO 7
# - Calcular el numero de host unicos cada dia

# In[24]:

row_data = data_clean.map(lambda p: Row(host = p[0], UserId = int(p[1]), empty = p[2],
    fechas = p[3],get_post = p[4],endpoint = p[6],version = p[7],status_Code = int(p[-2]),size = p[-1]))

interactions_df = sqlContext.createDataFrame(row_data)

newdata_clean = interactions_df.withColumn('fechas', regexp_replace('fechas', '\[', ''))

newdata_clean2 = newdata_clean.withColumn('fechas', regexp_replace('fechas', 'Aug', '08'))

newdata_clean3 = newdata_clean2.select(from_unixtime(unix_timestamp('fechas', 'dd/MM/yyyy:hh:mm:ss')).alias('fechas'), 'host')

newdata_clean4 = newdata_clean3.na.fill(0)

#Muestra los host distintos de cada día del mes de Agosto
newdata_clean4.groupby(dayofmonth("fechas").alias("Dia del mes")).agg(countDistinct('host').alias('Host distintos')).sort('Dia del mes')        .show()


# Ejercicio 8
# - Peticiones diarias por host

# In[25]:

##### EJERCICIO 8 #####
# Calcular la media de peticiones diarias por host

parsed_df2 = newdata_clean4.withColumn("month", month(col("fechas"))).                                   withColumn("DayOfmonth", dayofmonth(col("fechas")))
parsed_df2.createOrReplaceTempView('parsed_df2')
sqlContext.sql("SELECT DATE(fechas) Date, COUNT(host)/COUNT(DISTINCT host)                AS Peticiones_diarias_host FROM parsed_df2 GROUP BY  DATE(fechas) ORDER BY DATE(fechas) ASC").show(n = 10)


# EJERCICIO 9
# - Ahora 40 endpoints distintos que generen codigos de respuesta 404 

# In[26]:

code400 = endpointno200.filter(lambda x: x[1] == 404).distinct().take(40)
code400


# EJERCICIO 10

# In[27]:

only404 = data_clean.filter(lambda x: int(x[-2]) == 404)


# In[28]:

only404.map(lambda y: (y[6], 1)).reduceByKey(add).sortBy(lambda x: x[1], ascending = False).take(25)


# EJERCICIO 11
# - Top 5 días que generaron código de respuesta 404

# In[29]:

newdata_clean5 = newdata_clean2.select(from_unixtime(unix_timestamp('fechas', 'dd/MM/yyyy:hh:mm:ss')).alias('fechas'), 'status_Code')

newdata_clean5.filter(newdata_clean5['status_Code'] == 404).groupBy(dayofmonth('fechas')).count().sort(col("count").desc()).show(n = 5)

