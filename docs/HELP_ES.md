# Geospatial-RAP – Manual de uso

## Qué hace esta app

Esta app utiliza el mismo CSV agregado de API-Pop que el CUSUM-RAP, pero en lugar de correr un algoritmo de alerta temprana temporal, realiza un análisis geoespacial simple.

La app:
- calcula prevalencia basada en consulta por 100.000 habitantes,
- muestra un mapa coroplético de prevalencia,
- calcula Local Moran's I (LISA),
- y muestra un mapa de clusters LISA.

## Parámetros del usuario

1. Subir uno o más archivos CSV de API-Pop.
2. Seleccionar el nivel geográfico:
   - Departamento
   - Fracción Censal
3. Seleccionar un año.
4. Elegir entre:
   - una sola semana epidemiológica, o
   - un rango de semanas.
5. Hacer clic en **Run Spatial Analysis**.

## Salidas

- Mapa de Prevalencia
- Mapa de Clusters LISA
- Tabla de Resultados (descargable como CSV)
