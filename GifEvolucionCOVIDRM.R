#library(chilemapas)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(gganimate)
library(readr)
library(lubridate)
library(stringr)
library(forcats)

## --
## Input: Mapa 
## --

# Vector de comunas del "Gran Santiago"
comunas_considerar <- c("Huechuraba", "Quilicura", "Pudahuel", "Maipú",
                        "Padre Hurtado", "Cerrillos", "San Bernardo",
                        "La Pintana", "Pirque", "San José de Maipo",
                        "Puente Alto", "La Florida", "Peñalolén",
                        "Las Condes", "Lo Barnechea", "Vitacura",
                        "Conchalí", "Recoleta", "Independencia", "Quinta Normal",
                        "Renca", "Cerro Navia", "Lo Prado", "Estación Central",
                        "Santiago", "Pedro Aguirre Cerda", "Lo Espejo",
                        "El Bosque", "La Cisterna", "San Ramón", "La Granja",
                        "San Miguel", "San Joaquín", "Macul", "La Reina",
                        "Ñuñoa", "Providencia")

# Vector de localidades a no considerar
urbano_noconsiderar <- c("EL PRINCIPAL", "SAN ALFONSO", "SAN JOSÉ DE MAIPO",
                         "CIUDAD DEL VALLE", "VILLA CAMPO ALEGRE", "EL CAMBUCHO",
                         "LO AGUIRRE", "LO HERRERA", "NOVICIADO ALTO", "EL MAITÉN")

# Cargar shape de zonas urbanas (sacar zonas "discontinuas")
RM_urb_shp <- st_read("shapes/Zonas_urbanas_2017_Chile.shp", quiet = TRUE) %>% 
  filter(NOM_COMUNA %in% str_to_upper(comunas_considerar),
         !URBANO %in% urbano_noconsiderar)

# Ver Shape "RM_urb_shp"
ggplot() +
  geom_sf(data = RM_urb_shp) +
  theme_void()

# Unir los shapes de localidad a nivel de comuna (resulta un shape con las divisiones por comuna)
RM_urb_shp_comb <- RM_urb_shp %>% 
  distinct(COMUNA, NOM_COMUNA, geometry) %>% 
  group_by(COMUNA) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  ungroup()

# Ver Shape "RM_urb_shp_comb"
ggplot() +
  geom_sf(data = RM_urb_shp_comb) +
  theme_void()

## --
## Input: Datos etapa
## --

# Cargar datos desde github del ministerio de ciencias
positividad <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto65/PositividadPorComuna.csv")

# Dejar solo datos para región metropolitana/zonas urbanas
# pasar datos de "ancho" a "largo"
# transformar "clase" de algunas variables
positividad_rm <- positividad %>% 
  filter(`Codigo region` == 13) %>% 
  pivot_longer(6:(ncol(positividad)), names_to = "fecha", values_to = "positividad") %>% 
  mutate(codigo_comuna = as.character(`Codigo comuna`),
         fecha = as.Date(fecha),
         positividad_class = case_when(
           positividad <= 5 ~ "<5%",
           positividad > 5 & positividad <= 10 ~ "5-10%",
           positividad > 10 & positividad <= 15 ~ "10-15%",
           positividad > 15 & positividad <= 20 ~ "15-20%",
           positividad > 20 ~ ">20%"
         ),
         positividad_class = factor(positividad_class, levels = c("<5%", "5-10%", "10-15%", "15-20%", ">20%"))) %>% 
  group_by(fecha) %>% 
  mutate(positividad_promedio = mean(positividad, na.rm = TRUE)) %>% 
  ungroup()

## --
## Unir información 
## --

# Anexar a mapa (shapefile) información del paso a paso
RM_urb_shp_positividad <- left_join(RM_urb_shp_comb, positividad_rm, by = c("COMUNA" = "codigo_comuna"))

# Probar con un día ("hoy") que esté todo OK
cols = c("<5%" = "#ffffb2", 
         "5-10%" = "#fecc5c", 
         "10-15%" = "#fd8d3c", 
         "15-20%" = "#f03b20", 
         ">20%" = "#bd0026")

RM_urb_shp_positividad %>% 
  filter(fecha == last(fecha)) %>% 
  ggplot() +
  #geom_sf(aes(fill = positividad_class), col = NA) +
  #scale_fill_manual(values = cols) +  
  geom_sf(aes(fill = positividad), col = NA) +
  geom_sf_text(aes(label = Comuna), size = 2.5) +
  scale_fill_gradient(low = "#ffffb2", high = "#bd0026") +
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  labs(title = "Fecha: XXXX-XX-XX")

## --
## Animación
## --

# Crear animación
animacion_positividad <- RM_urb_shp_positividad %>% 
  ggplot() +
  #geom_sf(aes(fill = positividad_class), col = NA) +
  #scale_fill_manual(values = cols) +  
  geom_sf(aes(fill = positividad), col = NA) +
  geom_sf_text(aes(label = Comuna), size = 2.5) +
  scale_fill_gradient(low = "#ffffb2", high = "#bd0026") +
  theme_void() +
  transition_time(fecha) +
  theme(plot.title = element_text(size = 16, face = "bold"))  +
  labs(title = 'Fecha: {frame_time}')

# Mostrar animación
animate(animacion_positividad, fps = 5, renderer = gifski_renderer(loop = FALSE))

# Guardar animación como GIF
anim_save("GIFPositividad.gif")

