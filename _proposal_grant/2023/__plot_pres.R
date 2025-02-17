# Load necessary library
library(ggplot2)
install.packages("showtext")
library(ggplot2)
library(showtext)

# Configurar la fuente Oswald desde Google Fonts
font_add_google("Oswald", "oswald")
showtext_auto() # Activar el uso de showtext
# basic ambulatory (n=4.360), intensive
# ambulatory for the general population (GP) (n=4.998), GP residential (n=2.178), women-
#   only (WO) intensive ambulatory (n=745), and WO residential (n=1.036).
# Create the data frame
data <- data.frame(
  TreatmentSetting = rep(c("Pob.General,\nBásico\n(n=4.360)",
                           "Pob.General,\nAmbulatorio intensivo\n(n=4.998)",
                           "Pob.General,\nResidencial\n(n=2.178)",
                           "Esp. mujeres,\nAmbulatorio intensivo\n(n=745)",
                           "Esp. mujeres,\nResidencial\n(n=1.036)"), each = 3),
  Model = rep(c("RR", "IIW RR lag=0", "IIW RR lag=1"), times = 5),
  RR = c(1.03, 1.02, 1.02,
         1.04, 1.04, 1.04,
         0.97, 0.97, 0.95,
         0.99, 0.99, 0.99,
         1.14, 1.15, 1.13),
  LowerCI = c(1.00, 0.99, 1.00,
              1.01, 1.01, 1.01,
              0.92, 0.92, 0.90,
              0.93, 0.92, 0.92,
              1.06, 1.06, 1.04),
  UpperCI = c(1.05, 1.05, 1.05,
              1.07, 1.08, 1.07,
              1.02, 1.02, 1.01,
              1.05, 1.07, 1.06,
              1.23, 1.25, 1.22)
)
# Define the custom colors
custom_colors <- c("#052049", "#606c38", "#463d96")
names(custom_colors) <- c("RR", "IIW RR lag=0", "IIW RR lag=1")

fct=1.2
showtext_opts(dpi = 900)

# Create the plot
ggplot(data, aes(x = RR, y = TreatmentSetting, color = Model)) +
  geom_point(position = position_dodge(width = .5), size = 5) +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI),
                 height = .5,
                 linewidth= 2,
                 position = position_dodge(width = .5)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray", linewidth=2) +
  scale_color_manual(values = custom_colors) +
  scale_x_log10() +
  labs(title = "Asociación entre PSU al ingreso y Abandono de tratamiento",
       x = "Riesgo Relativo (RR)",
       y = "Tipo de Tratamiento",
       color = "Tipo de Modelo") +
  theme_minimal() +
  coord_flip() +
  theme(
    text = element_text(family = "oswald"),
    legend.position="bottom",
    panel.grid.major = element_blank(),  # Quitar líneas de fondo principales
    panel.grid.minor = element_blank(),  # Quitar líneas de fondo menores
    plot.title = element_text(size = 24*fct, face = "bold"),      # Tamaño del título
    axis.title.x = element_text(size = 22*fct, margin = margin(t = 12)),                  # Tamaño del título del eje X
    axis.title.y = element_text(size = 22*fct),                  # Tamaño del título del eje Y
    axis.text.x = element_text(size = 20*fct, color= "black"),                   # Tamaño del texto del eje X
    axis.text.y = element_text(size = 20*fct),                   # Tamaño del texto del eje Y
    legend.title = element_text(size = 20*fct),                  # Tamaño del título de la leyenda
    legend.text = element_text(size = 18*fct)                    # Tamaño del texto de la leyenda
  )
ggsave("esto.png", height=8,width=13, dpi=900)