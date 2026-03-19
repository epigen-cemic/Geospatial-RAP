# Geospatial RAP Tool: Guía de Desarrollo y Administración

Esta guía está dirigida a desarrolladores, administradores de IT y mantenedores de la herramienta Geospatial RAP.
Los usuarios finales deben consultar el Manual de Usuario disponible dentro de la aplicación.

---

## 1. Estructura del Proyecto

El repositorio sigue una arquitectura modular estándar de Shiny.

* Directorio raíz: scripts principales (`app.R`, `global.R`, `config.json`)
* `R/`: lógica principal y módulos
* `data/`: capas espaciales optimizadas (`.rds`)
* `build_data/`: scripts de preprocesamiento
* `www/`: recursos de interfaz y documentación

---

## 2. Ejecución Local (Desarrollo)

1. Abrir el proyecto en RStudio
2. Abrir `app.R`
3. Ejecutar:

shiny::runApp()

---

## 3. Despliegue

### Opción A: shinyapps.io

* Instalar `rsconnect`
* Desplegar con `rsconnect::deployApp()`

### Opción B: Infraestructura propia

* Shiny Server (Linux)
* Contenerización con Docker

### Opción C: Aplicación de escritorio (Electron)

* Empaqueta R + la app en un ejecutable
* No requiere configuración del usuario

---

## 4. Configuración

### 4.1 Configuración geográfica (`config.json`)

Define:

* Niveles administrativos
* Mapeo entre columnas del CSV y capas espaciales
* Identificadores y etiquetas

Permite soportar múltiples países sin modificar la lógica del código.

---

### 4.2 Personalización de la interfaz (`config.css`)

Controla:

* Diseño y colores
* Apariencia del panel lateral
* Fondo del mapa

---

### 4.3 Datos espaciales offline

Las capas optimizadas (`.rds`) se almacenan en:

data/

Para generarlas:

build_data/optimize_spatial_data.R

---

## 5. Pipeline de Datos

La aplicación:

* Lee archivos CSV de API-POP
* Estandariza nombres de columnas
* Ajusta identificadores geográficos (padding)
* Agrega datos
* Realiza la unión espacial

---

## 6. Motor de Análisis Espacial

Incluye:

* Moran’s I Global
* Moran’s I Local (LISA)
* Manejo de datos dispersos con configuraciones seguras

---

## 7. Solución de Problemas

* Polígonos faltantes → verificar identificadores espaciales
* Mapa vacío → revisar configuración
* Bajo rendimiento → asegurar uso de `.rds`

---

## 8. Documentación

* Manual de usuario: `www/docs/Documentation.pdf`
* Versión en inglés: `HELP_EN.md`
