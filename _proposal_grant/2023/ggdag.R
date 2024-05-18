
if(!require(ggdag)){install.packages("ggdag")}
if(!require(dagitty)){install.packages("dagitty")}


# first DAG ---------------------------------------------------------------


#out.height=450, 
dag_proposal_grant_2324b <- dagitty('dag {
bb="-10.134,-8.255,11.229,7.944"
A0 [exposure,pos="-8.085,2.617"]
A1 [exposure,pos="-3.675,2.617"]
A2 [exposure,pos="0.879,2.617"]
At [exposure,pos="5.060,2.617"]
L0 [adjusted,pos="-8.085,-1.524"]
L1 [adjusted,pos="-3.675,-1.524"]
L2 [adjusted,pos="0.879,-1.524"]
Lt [adjusted,pos="5.060,-1.524"]
U0 [latent,pos="-8.085,-5.809"]
U1 [latent,pos="-3.675,-5.809"]
U2 [latent,pos="0.879,-5.809"]
Ut [latent,pos="5.060,-5.809"]
Y1 [outcome,pos="-4.817,5.462"]
Y2 [outcome,pos="-0.217,5.462"]
Yt [outcome,pos="3.884,5.462"]
"Yt+1" [outcome,pos="7.720,5.462"]
A0 -> "Yt+1"
A0 -> A1
A0 -> L1
A0 -> Y1
A0 -> Y2
A0 -> Yt
A1 -> "Yt+1"
A1 -> A2
A1 -> L2
A1 -> Y2
A1 -> Yt
A2 -> "Yt+1"
A2 -> At
A2 -> Lt
A2 -> Yt
At -> "Yt+1"
L0 -> A0
L0 -> L1
L0 -> Y1
L1 -> A1
L1 -> L2
L1 -> Y2 [pos="-1.771,1.648"]
L2 -> A2
L2 -> Lt
L2 -> Yt [pos="2.804,1.673"]
Lt -> "Yt+1" [pos="7.130,1.164"]
Lt -> At
U0 -> L0
U0 -> U1
U0 -> Y1
U1 -> L1
U1 -> U2
U1 -> Y1
U2 -> L2
U2 -> Ut
U2 -> Y2
Ut -> "Yt+1" [pos="-1.212,-1.112"]
Ut -> Lt
Ut -> Yt
Y1 -> A1
Y1 -> L1
Y2 -> A2
Y2 -> L2
Yt -> At
Yt -> Lt
}
')

#Sea, A= tratamientos; Y= Mortalidad; L0=Confusores a la base; {L1,L2, ... , Lt}= 
#confusores tiempo-dependientes; U= Confusores no-observados; 
#v= Modificador (resultado del tratamiento)


#https://go-bayes.github.io/psych-447/posts/11_1/
#https://rpubs.com/leahmoubadder/726250

tidy_dag_proposal_grant_2324 <- tidy_dagitty(dag_proposal_grant_2324b) %>% 
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
dag34_plot<-
  ggdag:::if_not_tidy_daggity(tidy_dag_proposal_grant_2324) %>% ggdag:::node_status() %>% 
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

dag34_plot

ggsave("dag.pdf", width=10, height=5, dpi=500)
ggsave("dag.jpg", width=10, height=5, dpi=500)



