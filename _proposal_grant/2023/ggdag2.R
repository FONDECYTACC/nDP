
if(!require(ggdag)){install.packages("ggdag")}
if(!require(dagitty)){install.packages("dagitty")}
if(!require(tidyverse)){install.packages("tidyverse")}

# second DAG ---------------------------------------------------------------
library(dagitty)
dag_proposal_grant_2324dd <- dagitty('dag {
  bb="-4.294,-5.126,3.655,5.227"
  Admission month 1 [pos="-1.453,-3.671"]
  Admission month 2 [pos="0.073,-3.738"]
  Admission month 3 [pos="1.758,-3.749"]
  Baseline observed covariates [pos="-2.032,-1.970"]
  Dropout month 1 [pos="-1.532,0.134"]
  Dropout month 2 [pos="0.096,0.056"]
  Dropout month 3 [pos="1.780,0.000"]
  Obs. Dropout month 1 [pos="-0.752,-2.026"]
  Obs. Dropout month 2 [pos="0.994,-2.070"]
  Obs. Dropout month 3 [pos="2.852,-2.115"]
  Outcome Random Effects [pos="-3.626,1.522"]
  Random Effects for Admission [pos="-3.575,-4.745"]
  Shared Random Effects [pos="-3.138,-1.970"]
  Admission month 1 -> "Admission month 2"
  Admission month 1 -> "Admission month 3" [pos="-0.522,-5.092"]
  Admission month 1 -> "Obs. Dropout month 1"
  Admission month 2 -> "Admission month 3"
  Admission month 2 -> "Obs. Dropout month 2"
  Admission month 3 -> "Obs. Dropout month 3"
  Baseline observed covariates -> "Admission month 1"
  Baseline observed covariates -> "Admission month 2"
  Baseline observed covariates -> "Admission month 3"
  Baseline observed covariates -> "Dropout month 1"
  Baseline observed covariates -> "Dropout month 2"
  Baseline observed covariates -> "Dropout month 3"
  Dropout month 1 -> "Dropout month 2"
  Dropout month 1 -> "Dropout month 3" [pos="0.062,2.205"]
  Dropout month 1 -> "Obs. Dropout month 1"
  Dropout month 2 -> "Dropout month 3"
  Dropout month 2 -> "Obs. Dropout month 2"
  Dropout month 3 -> "Obs. Dropout month 3"
  Obs. Dropout month 1 -> "Admission month 2"
  Obs. Dropout month 1 -> "Admission month 3"
  Obs. Dropout month 2 -> "Admission month 3"
  Outcome Random Effects -> "Dropout month 1"
  Outcome Random Effects -> "Dropout month 2"
  Outcome Random Effects -> "Dropout month 3"
  Random Effects for Admission -> "Admission month 1"
  Random Effects for Admission -> "Admission month 2"
  Random Effects for Admission -> "Admission month 3"
}')


tidy_dag_proposal_grant_2324dd <- tidy_dagitty(dag_proposal_grant_2324dd) %>% 
  dplyr::mutate(label=dplyr::case_when(grepl("A0",as.character(name))~"A0",
                                       T~as.character(name))) %>% 
  dplyr::mutate(label2=dplyr::case_when(grepl("at admission",name)~"adj",
                                        grepl("until admission",name)~"adj",
                                        grepl("LM",name)~"white",
                                        T~"black")) %>% 
  #Allows us to modify transparency of arrows as a function of whether or not a variable is adjusted:
  dplyr::mutate(adjusted=
                  dplyr::case_when(
                    grepl("^U",name)~"unadjusted",
                    grepl("^L",name)~"adjusted",
                    grepl("^A",name)~"adjusted",
                    grepl("t+1",name)~"unadjusted",
                    grepl("^Y",name)~"adjusted",
                    name=="Yt\\+1"~"unadjusted",T~"unadjusted"),
                arrow = ifelse(adjusted == "adjusted", 0.15, 0.85)) %>% 
  dplyr::mutate(adjusted=factor(ifelse(name=="Yt+1","unadjusted",adjusted))) %>% 
  dplyr::mutate(label= gsub("\\[", "\\\n\\[",label))

edge_function <- ggdag:::edge_type_switch("link_arc")


#ggdag:::if_not_tidy_daggity(tidy_dag_proposal_grant_2324) %>% ggdag:::node_status() %>% data.frame() %>% View()

library(ggplot2)
dag34dd_plot<-
  ggdag:::if_not_tidy_daggity(tidy_dag_proposal_grant_2324dd) %>% ggdag:::node_status() %>% 
  ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, 
                               yend = yend, color = status, shape=factor(adjusted)))+ 
  #edge_function()+
  scale_adjusted()+ ggdag:::breaks(c("exposure", "outcome","latent"))+
  theme_dag()+
  # geom_dag_edges(aes(#Adjusts transparency of arrows:
  #   edge_alpha = arrow), edge_width = 0.5) +
  geom_dag_edges()+
  #geom_dag_edges_arc(curvature = c(rep(0,240)))+ #14 y 16 de 24
  ggdag:::geom_dag_point(size = 16)+
  ggdag:::geom_dag_label_repel(ggplot2::aes_string(label = "label", 
                                                   fill = "status"), size = 4, col = "white", 
                               show.legend = FALSE)+
  
  scale_shape_manual(values = c(15, 16), name="Ajustado", labels=c("Sí", "No"))+ 
  scale_fill_manual(values = c("gray70", "gray35","gray30"), name="Status",na.value="black", labels=c("Exposure", "Outcome"), limits = c('exposure', 'outcome'))+  
  scale_color_manual(values = c("gray75", "gray35","gray30"), name="Status",na.value="black", labels=c("Exposure", "Outcome"), limits = c('exposure', 'outcome'))+#E6E6E6
  guides(linetype="none", edge_alpha="none", shape="none")+
  guides(color=guide_legend(override.aes = list(arrow = NULL)))+#,guide_colourbar(order = 1)
  theme(plot.caption = element_text(hjust = 0))+
  theme(legend.position = "bottom", aspect.ratio=4/10, text = element_text(size = 15))+
  scale_y_reverse(expand = expand_scale(c(0.1, 0.1)))+
  labs(caption="Source. Prepared by the authors")

dag34dd_plot

invisible("Figura C) , all the backdoors of the month of assessment (admission ")
invisible("to treatment), may pass through observed outcomes, history of evolution or baseline covariates")
invisible("If you assume that is a proportional intensity model holds, we ")
invisible("can construct evidence that variables predict outcomes")