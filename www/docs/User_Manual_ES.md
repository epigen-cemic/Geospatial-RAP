# Manual de Usuario

**Geospatial RAP Tool**  
Mapas de prevalencia y análisis de conglomerados espaciales  
Revisión de documentación: junio de 2026  
Idioma: Español

---

# 1. Propósito y alcance

La herramienta Geospatial RAP agrega casos y población para un país, nivel administrativo, año y ventana de semanas epidemiológicas. Produce mapas de prevalencia, un resumen de Moran I Global y clasificaciones locales LISA.

> **Todas las unidades se analizan juntas.** La aplicación independiente no exige seleccionar y analizar cada comuna, departamento u otra ubicación por separado. Al elegir un nivel, todas las unidades disponibles de ese nivel se procesan en la misma ejecución.

> **Interpretación.** Las estadísticas espaciales describen patrones del conjunto seleccionado. No demuestran causalidad ni reemplazan la investigación epidemiológica local.


# 2. Datos de entrada y requisitos espaciales

Cargue uno o más CSV compatibles con API-POP, delimitados por coma o punto y coma. La población es obligatoria porque la prevalencia se calcula como casos/población por el multiplicador configurado, actualmente 100.000.

| Columna interna | Requerida | Significado | Alias o ejemplos |
| --- | --- | --- | --- |
| year | Sí | Año epidemiológico. | year, anio, año, epiyear |
| week | Sí | Semana epidemiológica. | week, semana, epiweek, se |
| country | Recomendado | País o unidad nacional. | Argentina o England |
| level1 | Según el nivel | Primer nivel administrativo. | province, region, Level1 |
| level2 | Para mapas de segundo nivel | Segundo nivel administrativo. | department, district, LAD, Level2 |
| level3 / level4 | Para mapas detallados | Niveles administrativos más detallados. | fraction, MSOA, Level3 |
| n_cases | Sí | Casos observados o recuento de eventos. | n_cases, cases, case_count, count, n |
| population | Sí | Denominador para calcular prevalencia. | population, pop, denominator |

También se necesita un GeoPackage configurado para el país activo. Cada nivel se vincula en config.json con una capa, un identificador espacial y una columna de nombre.

> **Coincidencia de identificadores.** Los códigos del CSV deben coincidir con los ID o etiquetas del mapa después de normalización y relleno con ceros. Los ceros iniciales son significativos.


# 3. Configurar y ejecutar

1. Cargue uno o más CSV.
2. Use Manage files para activar, desactivar, eliminar o limpiar archivos.
3. Seleccione Country cuando haya varios.
4. Elija Geographic Level.
5. Elija Single Week o Week Range.
6. Seleccione Year y la semana o rango.
7. Seleccione Run Analysis.

| Control | Comportamiento |
| --- | --- |
| Manage files | Combina todos los archivos activos. |
| Geographic Level | Selecciona el nivel y la capa de GeoPackage configurada. |
| Single Week | Usa una sola semana. |
| Week Range | Agrega casos en el rango inclusivo. |
| Year | Limita a un año epidemiológico. |
| Run Analysis | Prepara, valida, carga geometría, une registros, calcula LISA y actualiza resultados. |

En un rango se suman los casos. La población se trata como denominador estático: se deduplican las unidades detalladas antes de agregar al nivel seleccionado, evitando sumar la misma población cada semana.


# 4. Revisar los resultados

## Banner de Moran I Global

Muestra Moran I Global y su p-valor. Un valor positivo significativo sugiere que áreas vecinas tienen prevalencias similares; cerca de cero indica poco patrón global; un valor negativo sugiere diferencias entre vecinas.

## Prevalence Map

El mapa coroplético muestra la prevalencia configurada, normalmente casos por 100.000. La leyenda permanece visible y los popups incluyen área, ID, prevalencia, categoría LISA y p-valor cuando existen.

## LISA Cluster Map

| Categoría | Significado |
| --- | --- |
| High-High | Área alta rodeada de áreas altas; patrón de punto caliente. |
| Low-Low | Área baja rodeada de áreas bajas. |
| High-Low | Área alta rodeada de áreas bajas; valor atípico. |
| Low-High | Área baja rodeada de áreas altas; valor atípico inverso. |
| Not Significant | El patrón local no alcanzó p <= 0,05. |

## Results Table

Contiene identificadores, nombres, casos, población, semanas, prevalencia, categoría LISA y p-valor. Search by permite filtrar una columna.

## Prepared Data

Muestra el conjunto agregado previo a la geometría y el registro de preparación: filtro temporal, agregación de casos, deduplicación de población, cálculo de prevalencia y cantidad de unidades.

> **Visualización.** Help permite cambiar el tamaño de fuente y los decimales mostrados. Los CSV conservan precisión completa.


# 5. Interpretación responsable

- Moran Global resume todo el mapa y no identifica por sí solo las áreas de un conglomerado.
- LISA clasifica patrones locales; deben considerarse pruebas múltiples y datos escasos.
- Un conglomerado puede reflejar intensidad de vigilancia, estructura poblacional, acceso o prácticas de notificación.
- Compare ventanas temporales justificadas y no seleccione una ventana solo porque produce el patrón deseado.

Antes de actuar, revise numerador, denominador, coincidencia de identificadores, semanas elegidas y relevancia epidemiológica de la vecindad espacial.


# 6. Descargas

| Descarga | Contenido |
| --- | --- |
| Download Results CSV | Resultados de todas las unidades coincidentes. |
| Download Prepared Data CSV | Datos agregados usados para unión y prevalencia. |
| Exportación de mapas, cuando está habilitada | Imagen del mapa; depende de paquetes de captura instalados. |

Filtros y decimales en pantalla no modifican los valores descargados.


# 7. Solución de problemas

| Problema | Qué revisar |
| --- | --- |
| Geographic data are not available | Falta raw_data/<file_name>.gpkg. |
| Error de capa o columna | Revise layer_name, spatial_id, name_col o label_col. |
| Spatial Join Failure | Los códigos no coinciden; revise ceros iniciales, nivel y longitud. |
| No data available | No hay datos para el año o semanas. |
| Sin banner Global Moran | Menos de cuatro unidades, valores ausentes, varianza cero o vecindad inválida. |
| LISA vacío | Las mismas condiciones pueden impedir el análisis local. |
| Error de población | Falta población, es cero/inválida o usa alias no reconocido. |

> **Información para escalar.** Incluya país, nivel, año, ventana, archivos activos, registro, mensaje pre-flight y una muestra no sensible de identificadores.


# 8. Glosario

| Término | Significado |
| --- | --- |
| Prevalencia por 100.000 | Casos/población por 100.000. |
| Moran I Global | Medida de autocorrelación espacial de todo el mapa. |
| Moran I Local / LISA | Estadística local para conglomerados y valores atípicos. |
| Pesos espaciales | Relaciones de vecindad estandarizadas por fila a partir de contigüidad. |
| GeoPackage | Base espacial con capas de polígonos. |
| Unión espacial | Coincidencia entre analysis_unit_id y los ID o etiquetas del mapa. |

