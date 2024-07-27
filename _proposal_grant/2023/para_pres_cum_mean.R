load("E:/Mi unidad/Alvacast/SISTRAT 2022 (github)/_proposal_grant/2023/an_grant_23_24_4.RData")
library(tidyverse)


data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(num_tr = row_number(), tot_tr= max(num_tr))%>% 
  summarise(n=max(num_tr)) %>% 
  pull(n) %>% 
  {
    print(round(mean(.),1))
    print(round(sd(.),1))
  }

#la diferencia en meses
data_mine_miss_restr_proc2$time_diff<-ifelse(is.na(data_mine_miss_restr_proc2$time-data_mine_miss_restr_proc2$lag_time), 0, data_mine_miss_restr_proc2$time-data_mine_miss_restr_proc2$lag_time)

data_mine_miss_restr_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(cumulative_prom = cummean(time_diff))%>% #, na.rm = TRUE
  dplyr::ungroup() %>% 
  dplyr::group_by(policonsumo2) %>% 
  summarise(p25= quantile(cumulative_prom, .25, na.rm=T),
            mdn= quantile(cumulative_prom, .5, na.rm=T),
            p75= quantile(cumulative_prom, .75, na.rm=T), 
            mean= mean(cumulative_prom, na.rm=T),
            sd= sd(cumulative_prom, na.rm=T))
#  policonsumo2   p25   mdn   p75  mean    sd
#          <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1            0     0  5.16  13.2  8.34 10.1 
# 2            1     0  3.23  11.8  7.20  9.56

data_hist <- data_mine_miss_restr_proc2 %>%
  dplyr::group_by(hash_key) %>%
  dplyr::mutate(cumulative_prom = cummean(time_diff)) %>%
  dplyr::ungroup()

# Crear el histograma con facetas por policonsumo2
ggplot(data_hist, aes(x = cumulative_prom)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~ policonsumo2) +
  labs(title = "Histograma de Cumulative Prom por Policonsumo2",
       x = "Cumulative Prom",
       y = "Frecuencia")

nrow(data_mine_miss_proc2)#90075
nrow(data_mine_miss_restr_proc2)#30988

invisible("AHora con los que tienen 1 tratamiento también")

data_mine_miss_proc2$time_diff<-ifelse(is.na(data_mine_miss_proc2$time-data_mine_miss_proc2$lag_time), 0, data_mine_miss_proc2$time-data_mine_miss_proc2$lag_time)

data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(cumulative_prom = cummean(time_diff))%>% #, na.rm = TRUE
  dplyr::ungroup() %>% 
  dplyr::group_by(policonsumo2) %>% 
  summarise(p25= quantile(cumulative_prom, .25, na.rm=T),
            mdn= quantile(cumulative_prom, .5, na.rm=T),
            p75= quantile(cumulative_prom, .75, na.rm=T), 
            mean= mean(cumulative_prom, na.rm=T),
            sd= sd(cumulative_prom, na.rm=T))
#   policonsumo2   p25   mdn   p75  mean    sd
#          <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1            0     0     0     0  2.32  6.49
# 2            1     0     0     0  2.64  6.74
# 
# 
#
# 
# 1. IRRs -----------------------------------------------------------------

data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(num_tr = row_number(), tot_tr= max(num_tr))%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(tot_tr==1) %>% 
  dplyr::group_by(policonsumo2) %>% 
  summarise(prev_noncomp= sum(tr_outcome==1)/n(), n=n()) %>% 
  dplyr::mutate(prev=n/sum(n))
#   policonsumo2 prev_noncomp     n  prev
#          <dbl>        <dbl> <int> <dbl>
# 1            0        0.635 16459 0.279
# 2            1        0.744 42628 0.721

#when examining patients with multiple treatment episodes, xx reported PSU in at least one treatment episode
data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(num_tr = row_number(), tot_tr= max(num_tr))%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(tot_tr>1) %>% 
  dplyr::group_by(hash_key) %>% 
  summarise(psu= sum(policonsumo2==1)) %>% 
  dplyr::ungroup() %>% 
  dplyr::summarise(prev_psu=sum(psu>0)/n())
  

#Specifically, 71% of patients with only one treatment did not complete it, 
##whereas 79%, 81%, and 85% of the treatment episodes of patients with two, 
###three, or four treatments and more, respectively, had a non-completion status. 
data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(num_tr = row_number(), tot_tr= max(num_tr))%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(num_tr==1) %>% 
  dplyr::group_by(tr_outcome) %>% 
  summarise(n=n()) %>% 
  dplyr::mutate(prev=n/sum(n))


data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(num_tr = row_number(), tot_tr= max(num_tr))%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(num_tr==2) %>% 
  dplyr::group_by(tr_outcome) %>% 
  summarise(n=n()) %>% 
  dplyr::mutate(prev=n/sum(n))


data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(num_tr = row_number(), tot_tr= max(num_tr))%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(num_tr==1) %>% 
  dplyr::group_by(policonsumo2) %>% 
  summarise(prev_noncomp= sum(tr_outcome==1)/n(), n=n()) %>% 
  dplyr::mutate(prev=n/sum(n))
#   policonsumo2 prev_noncomp     n
#          <dbl>        <dbl> <int>
# 1            0        0.652 18842
# 2            1        0.753 53562
# 

data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(num_tr = row_number())%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(num_tr==2) %>% 
  dplyr::group_by(policonsumo2) %>% 
  summarise(prev_noncomp= sum(tr_outcome==1)/n(), n=n()) %>% 
  dplyr::mutate(prev=n/sum(n))
#   policonsumo2 prev_noncomp     n
#          <dbl>        <dbl> <int>
# 1            0        0.676  3059
# 2            1        0.754 10258

data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(num_tr = row_number())%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(num_tr==3) %>% 
  dplyr::group_by(policonsumo2) %>% 
  summarise(prev_noncomp= sum(tr_outcome==1)/n(), n=n()) %>% 
  dplyr::mutate(prev=n/sum(n))
#   policonsumo2 prev_noncomp     n
#          <dbl>        <dbl> <int>
# 1            0        0.714   682
# 2            1        0.746  2514

data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(num_tr = row_number())%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(num_tr==4) %>% 
  dplyr::group_by(policonsumo2) %>% 
  summarise(prev_noncomp= sum(tr_outcome==1)/n(), n=n()) %>% 
  dplyr::mutate(prev=n/sum(n))
#   policonsumo2 prev_noncomp     n
#          <dbl>        <dbl> <int>
# 1            0        0.695   174
# 2            1        0.792   659

data1 <- data.frame(
  num_tr= rep(1:4, each=2),
  policonsumo2 = c(0, 1, 0, 1, 0, 1, 0, 1),
  prev_noncomp = c(0.652, 0.753, 0.676, 0.754, 0.714, 0.746, 0.695, 0.792),
  n = c(18842, 53562, 3059, 10258, 682, 2514, 174, 659)
) %>% 
  dplyr::group_by(num_tr) %>% 
  dplyr::mutate(tot=sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc= n/tot)
#   num_tr policonsumo2 prev_noncomp     n   tot  perc
#    <int>        <dbl>        <dbl> <dbl> <dbl> <dbl>
# 1      1            0        0.652 18842 72404 0.260
# 2      1            1        0.753 53562 72404 0.740
# 3      2            0        0.676  3059 13317 0.230
# 4      2            1        0.754 10258 13317 0.770
# 5      3            0        0.714   682  3196 0.213
# 6      3            1        0.746  2514  3196 0.787
# 7      4            0        0.695   174   833 0.209
# 8      4            1        0.792   659   833 0.791

invisible("El porcentaje de los que tienen policonsumo aumenta, pero no pasa lo mismo con no-completar si no desde el 4to tto. Lo que sí pasa")
invisible("es que los que no completan crecen en los que no tienen policonsumo")

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
invisible("Y si vemos policonsumo acumulado??")
data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(sum_tr_outcomes=sum(tr_outcome==1,na.rm=T),  sum_policonsumo2=sum(policonsumo2==1,na.rm=T)) %>% 
  dplyr::mutate(num_tr = row_number())%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(num_tr==1) %>% 
  dplyr::group_by(policonsumo2) %>% 
  summarise(prev_noncomp= sum(sum_tr_outcomes>0)/n(), n=n())
#   policonsumo2 prev_noncomp     n
#          <dbl>        <dbl> <int>
# 1            0        0.668 18842
# 2            1        0.780 53562
data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(sum_tr_outcomes=sum(tr_outcome==1,na.rm=T),  sum_policonsumo2=sum(policonsumo2==1,na.rm=T)) %>% 
  dplyr::mutate(num_tr = row_number())%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(num_tr==2) %>% 
  dplyr::group_by(policonsumo2) %>% 
  summarise(prev_noncomp= sum(sum_tr_outcomes>0)/n(), n=n())
#   policonsumo2 prev_noncomp     n
#          <dbl>        <dbl> <int>
# 1            0        0.880  3059
# 2            1        0.929 10258

data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(sum_tr_outcomes=sum(tr_outcome==1,na.rm=T),  sum_policonsumo2=sum(policonsumo2==1,na.rm=T)) %>% 
  dplyr::mutate(num_tr = row_number())%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(num_tr==3) %>% 
  dplyr::group_by(policonsumo2) %>% 
  summarise(prev_noncomp= sum(sum_tr_outcomes>0)/n(), n=n())
#   policonsumo2 prev_noncomp     n
#          <dbl>        <dbl> <int>
# 1            0        0.957   682
# 2            1        0.973  2514

data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(sum_tr_outcomes=sum(tr_outcome==1,na.rm=T),  sum_policonsumo2=sum(policonsumo2==1,na.rm=T)) %>% 
  dplyr::mutate(num_tr = row_number())%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(num_tr==4) %>% 
  dplyr::group_by(policonsumo2) %>% 
  summarise(prev_noncomp= sum(sum_tr_outcomes>0)/n(), n=n())
#   policonsumo2 prev_noncomp     n
#          <dbl>        <dbl> <int>
# 1            0        0.994   174
# 2            1        0.988   659
# 
data2 <- data.frame(
  num_tr= rep(1:4, each=2),
  policonsumo2 = c(0, 1, 0, 1, 0, 1, 0, 1),
  prev_noncomp = c(0.668, 0.780, 0.880, 0.929, 0.957, 0.973, 0.994, 0.988),
  n = c(18842, 53562, 3059, 10258, 682, 2514, 174, 659)
)%>% 
  dplyr::group_by(num_tr) %>% 
  dplyr::mutate(tot=sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc= n/tot)
#   num_tr policonsumo2 prev_noncomp     n   tot  perc
#    <int>        <dbl>        <dbl> <dbl> <dbl> <dbl>
# 1      1            0        0.668 18842 72404 0.260
# 2      1            1        0.78  53562 72404 0.740
# 3      2            0        0.88   3059 13317 0.230
# 4      2            1        0.929 10258 13317 0.770
# 5      3            0        0.957   682  3196 0.213
# 6      3            1        0.973  2514  3196 0.787
# 7      4            0        0.994   174   833 0.209
# 8      4            1        0.988   659   833 0.791

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
invisible("non-completion acumuladas")
data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(sum_tr_outcomes=sum(tr_outcome==1,na.rm=T),  sum_policonsumo2=sum(policonsumo2==1,na.rm=T)) %>% 
  dplyr::mutate(num_tr = row_number())%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(num_tr==1) %>% 
  dplyr::group_by(sum_policonsumo2>0) %>% 
  summarise(prev_noncomp= sum(tr_outcome==1)/n(), n=n())
#   `sum_policonsumo2 > 0` prev_noncomp     n
#   <lgl>                         <dbl> <int>
# 1 FALSE                         0.642 17730
# 2 TRUE                          0.754 54674

data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(sum_tr_outcomes=sum(tr_outcome==1,na.rm=T),  sum_policonsumo2=sum(policonsumo2==1,na.rm=T)) %>% 
  dplyr::mutate(num_tr = row_number())%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(num_tr==2) %>% 
  dplyr::group_by(sum_policonsumo2>0) %>% 
  summarise(prev_noncomp= sum(tr_outcome==1)/n(), n=n())
#   `sum_policonsumo2 > 0` prev_noncomp     n
#   <lgl>                         <dbl> <int>
# 1 FALSE                         0.639  1271
# 2 TRUE                          0.747 12046

data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(sum_tr_outcomes=sum(tr_outcome==1,na.rm=T),  sum_policonsumo2=sum(policonsumo2==1,na.rm=T)) %>% 
  dplyr::mutate(num_tr = row_number())%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(num_tr==3) %>% 
  dplyr::group_by(sum_policonsumo2>0) %>% 
  summarise(prev_noncomp= sum(tr_outcome==1)/n(), n=n())
#   `sum_policonsumo2 > 0` prev_noncomp     n
#   <lgl>                         <dbl> <int>
# 1 FALSE                         0.717   138
# 2 TRUE                          0.740  3058

data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(sum_tr_outcomes=sum(tr_outcome==1,na.rm=T),  sum_policonsumo2=sum(policonsumo2==1,na.rm=T)) %>% 
  dplyr::mutate(num_tr = row_number())%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(num_tr==4) %>% 
  dplyr::group_by(sum_policonsumo2>0) %>% 
  summarise(prev_noncomp= sum(tr_outcome==1)/n(), n=n())
#   `sum_policonsumo2 > 0` prev_noncomp     n
#   <lgl>                         <dbl> <int>
# 1 FALSE                         0.667    12
# 2 TRUE                          0.773   821
# 
data3 <- data.frame(num_tr= rep(1:4, each=2),
  sum_policonsumo2_greater_0 = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
  prev_noncomp = c(0.642, 0.754, 0.639, 0.747, 0.717, 0.740, 0.667, 0.773),
  n = c(17730, 54674, 1271, 12046, 138, 3058, 12, 821)
)%>% 
  dplyr::group_by(num_tr) %>% 
  dplyr::mutate(tot=sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc= n/tot)
#   num_tr sum_policonsumo2_greater_0 prev_noncomp     n   tot   perc
#    <int> <lgl>                             <dbl> <dbl> <dbl>  <dbl>
# 1      1 FALSE                             0.642 17730 72404 0.245 
# 2      1 TRUE                              0.754 54674 72404 0.755 
# 3      2 FALSE                             0.639  1271 13317 0.0954
# 4      2 TRUE                              0.747 12046 13317 0.905 
# 5      3 FALSE                             0.717   138  3196 0.0432
# 6      3 TRUE                              0.74   3058  3196 0.957 
# 7      4 FALSE                             0.667    12   833 0.0144
# 8      4 TRUE                              0.773   821   833 0.986 

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
invisible("ambas acumuladas")
data_mine_miss_proc2 %>% 
dplyr::group_by(hash_key)  %>%
  dplyr::mutate(sum_tr_outcomes=sum(tr_outcome==1,na.rm=T),  sum_policonsumo2=sum(policonsumo2==1,na.rm=T)) %>% 
  dplyr::mutate(num_tr = row_number())%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(num_tr==1) %>% 
  dplyr::group_by(sum_policonsumo2>0) %>% 
  summarise(prev_noncomp= sum(sum_tr_outcomes>0)/n(), n=n())
#   `sum_policonsumo2 > 0` prev_noncomp     n
#   <lgl>                         <dbl> <int>
# 1 FALSE                         0.651 17730
# 2 TRUE                          0.783 54674
# 
data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(sum_tr_outcomes=sum(tr_outcome==1,na.rm=T),  sum_policonsumo2=sum(policonsumo2==1,na.rm=T)) %>% 
  dplyr::mutate(num_tr = row_number())%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(num_tr==2) %>% 
  dplyr::group_by(sum_policonsumo2>0) %>% 
  summarise(prev_noncomp= sum(sum_tr_outcomes>0)/n(), n=n())
#   `sum_policonsumo2 > 0` prev_noncomp     n
#   <lgl>                         <dbl> <int>
# 1 FALSE                         0.865  1271
# 2 TRUE                          0.923 12046

data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(sum_tr_outcomes=sum(tr_outcome==1,na.rm=T),  sum_policonsumo2=sum(policonsumo2==1,na.rm=T)) %>% 
  dplyr::mutate(num_tr = row_number())%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(num_tr==3) %>% 
  dplyr::group_by(sum_policonsumo2>0) %>% 
  summarise(prev_noncomp= sum(sum_tr_outcomes>0)/n(), n=n())
#   `sum_policonsumo2 > 0` prev_noncomp     n
#   <lgl>                         <dbl> <int>
# 1 FALSE                         0.957   138
# 2 TRUE                          0.971  3058
data_mine_miss_proc2 %>% 
  dplyr::group_by(hash_key)  %>%
  dplyr::mutate(sum_tr_outcomes=sum(tr_outcome==1,na.rm=T),  sum_policonsumo2=sum(policonsumo2==1,na.rm=T)) %>% 
  dplyr::mutate(num_tr = row_number())%>% 
  dplyr::ungroup() %>% 
  dplyr::filter(num_tr==4) %>% 
  dplyr::group_by(sum_policonsumo2>0) %>% 
  summarise(prev_noncomp= sum(sum_tr_outcomes>0)/n(), n=n())
#   `sum_policonsumo2 > 0` prev_noncomp     n
#   <lgl>                         <dbl> <int>
# 1 FALSE                         1        12
# 2 TRUE                          0.989   821
# 
data4 <- data.frame(num_tr= rep(1:4, each=2),
  sum_policonsumo2_greater_0 = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
  prev_noncomp = c(0.651, 0.783, 0.865, 0.923, 0.957, 0.971, 1.000, 0.989),
  n = c(17730, 54674, 1271, 12046, 138, 3058, 12, 821)
)%>% 
  dplyr::group_by(num_tr) %>% 
  dplyr::mutate(tot=sum(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(perc= n/tot)
#   num_tr sum_policonsumo2_greater_0 prev_noncomp     n   tot   perc
#    <int> <lgl>                             <dbl> <dbl> <dbl>  <dbl>
# 1      1 FALSE                             0.651 17730 72404 0.245 
# 2      1 TRUE                              0.783 54674 72404 0.755 
# 3      2 FALSE                             0.865  1271 13317 0.0954
# 4      2 TRUE                              0.923 12046 13317 0.905 
# 5      3 FALSE                             0.957   138  3196 0.0432
# 6      3 TRUE                              0.971  3058  3196 0.957 
# 7      4 FALSE                             1        12   833 0.0144
# 8      4 TRUE                              0.989   821   833 0.986 
# 
# 3. IRRs -----------------------------------------------------------------


broom::tidy(glm( sum_tr_outcomes>0 ~ policonsumo2 + offset(log(cens_time)), family=poisson, data =data_mine_miss_restr_proc2 %>% 
                  dplyr::group_by(hash_key) %>% 
                  dplyr::mutate(sum_tr_outcomes=sum(tr_outcome==1,na.rm=T)) %>% 
                  dplyr::filter(is_first_occurrence==1) %>% 
#   term         estimate std.error statistic      p.value conf.low conf.high
#   <chr>           <dbl>     <dbl>     <dbl>        <dbl>    <dbl>     <dbl>
# 1 (Intercept)    0.0132    0.0216   -200.   0              0.0126    0.0138
# 2 policonsumo2   0.876     0.0238     -5.56 0.0000000266   0.836     0.918 

broom::tidy(glm(tr_outcome ~ policonsumo2 + offset(log(cens_time)), family=poisson, data = data_mine_miss_restr_proc2 %>% 
                  dplyr::group_by(hash_key) %>% 
                  dplyr::mutate(sum_tr_outcomes=sum(tr_outcome==1,na.rm=T)) %>% 
                  dplyr::filter(is_first_occurrence==1) %>% 
                  dplyr::ungroup()), exponentiate=T, conf.int=T)
#   term         estimate std.error statistic      p.value conf.low conf.high
#   <chr>           <dbl>     <dbl>     <dbl>        <dbl>    <dbl>     <dbl>
# 1 (Intercept)    0.0113    0.0234   -192.   0              0.0108    0.0119
# 2 policonsumo2   0.871     0.0257     -5.35 0.0000000893   0.829     0.917 

broom::tidy(glm(sum_tr_outcomes>0 ~ policonsumo2 + offset(log(cens_time)), family=poisson, data = data_mine_miss_restr_proc2 %>% 
                  dplyr::group_by(hash_key) %>% 
                  dplyr::mutate(sum_tr_outcomes=sum(tr_outcome==1,na.rm=T),
                                sum_policonsumo2=sum(policonsumo2==1,na.rm=T)) %>% 
                  dplyr::filter(is_first_occurrence==1) %>% 
                  dplyr::ungroup() %>% dplyr::mutate(policonsumo2=ifelse(sum_policonsumo2>0,1,0))), exponentiate=T, conf.int=T)
#   term         estimate std.error statistic      p.value conf.low conf.high
#   <chr>           <dbl>     <dbl>     <dbl>        <dbl>    <dbl>     <dbl>
# 1 (Intercept)    0.0139    0.0302   -142.   0              0.0131    0.0148
# 2 policonsumo2   0.836     0.0316     -5.68 0.0000000135   0.786     0.890 

broom::tidy(glm(tr_outcome==1 ~ policonsumo2 + offset(log(cens_time)), family=poisson, data = data_mine_miss_restr_proc2 %>% 
                  dplyr::group_by(hash_key) %>% 
                  dplyr::mutate(sum_tr_outcomes=sum(tr_outcome==1,na.rm=T),
                                sum_policonsumo2=sum(policonsumo2==1,na.rm=T)) %>% 
                  dplyr::filter(is_first_occurrence==1) %>% 
                  dplyr::ungroup() %>% dplyr::mutate(policonsumo2=ifelse(sum_policonsumo2>0,1,0))), exponentiate=T, conf.int=T)
#   term         estimate std.error statistic     p.value conf.low conf.high
#   <chr>           <dbl>     <dbl>     <dbl>       <dbl>    <dbl>     <dbl>
# 1 (Intercept)    0.0119    0.0327   -136.   0             0.0111    0.0126
# 2 policonsumo2   0.839     0.0343     -5.12 0.000000309   0.785     0.898 
# 
# 
# 
# 4. ver interaccion -----------------------------------------------------------------

library(emmeans);library(geepack)


data_mine_miss_restr_proc2exp2<-
  data_mine_miss_restr_proc2 %>% 
  left_join(Base_fiscalia_v15f_grant_23_24[,c("hash_key","n_off_acq","n_off_vio","n_off_sud","n_off_oth", "n_prev_off")], by="hash_key") %>%
  tidylog::left_join(Base_fiscalia_v15f_grant_23_24[,c("hash_key","fech_ing_num","otras_sus1_mod","otras_sus2_mod","otras_sus3_mod")], by=c("hash_key"="hash_key","fech_ing_num"="fech_ing_num")) %>% 
  {if (nrow(.) > nrow(data_mine_miss_restr_proc2)) {message("left join added rows"); .} else .} %>% 
  dplyr::mutate(n_prev_off_bin= ifelse(n_prev_off>0,1,0)) %>% 
  dplyr::mutate(
    susinidumrec_coc = ifelse(sus_ini_mod_mvv == "Cocaine hydrochloride", 1, 0),
    susinidumrec_pbc = ifelse(sus_ini_mod_mvv == "Cocaine paste", 1, 0),
    susinidumrec_mar = ifelse(sus_ini_mod_mvv == "Marijuana", 1, 0),
    susinidumrec_otr = ifelse(sus_ini_mod_mvv == "Other", 1, 0), #sus_principal_mod
    susprindumrec_coc = ifelse(sus_principal_mod == "Cocaine hydrochloride", 1, 0),
    susprindumrec_pbc = ifelse(sus_principal_mod == "Cocaine paste", 1, 0),
    susprindumrec_mar = ifelse(sus_principal_mod == "Marijuana", 1, 0),
    susprindumrec_otr = ifelse(sus_principal_mod == "Other", 1, 0) # "susprindum_coc", "susprindum_pbc", "susprindum_mar", "susprindum_oh
  ) %>% 
  rowwise() %>%
  dplyr::mutate(oh_otras_sus = if_else("Alcohol" %in% c(otras_sus1_mod, otras_sus2_mod, otras_sus3_mod),1,0)) %>%
  dplyr::mutate(policonsumo2_rec= dplyr::case_when(policonsumo2==1 & oh_otras_sus==1~"2.both",policonsumo2==1 & oh_otras_sus==0~"1.only PSU",T~"0.no PSU")) %>% 
  ungroup()


nocorr_nowgt_int_list<-list()
nocorr_nowgt_int_probs_list<-list()
nocor_nowgt_int_tab <-list()
for (i in seq_along(plan_names)) {
  # Subset the data for the current plan type
  current_data <- subset(data_mine_miss_restr_proc2exp2, tipo_de_plan_2_mod == plan_names[i])
  
  # Fit the GEE model for the current subset
  model <- geeglm(tr_outcome ~ policonsumo2_rec +
                   comp_bpsc_y3_severe+
                   edad_al_ing_1 + 
                   ano_nac_corr + 
                   susinidumrec_otr +
                   susinidumrec_coc +
                   susinidumrec_pbc +
                   susinidumrec_mar +
                   psycom_dum_study +
                   psycom_dum_with +
                   freq_cons_dum_5day +
                   cond_oc_dum_2inact +
                   cond_oc_dum_3unemp +
                   susprindumrec_coc +
                   susprindumrec_pbc +
                   susprindumrec_mar +
                   susprindumrec_otr, #origen_ingreso_mod fis_comorbidity_icd_10 dg_trs_cons_sus_or
                 id = id, 
                 data = current_data,
                 family = poisson, 
                 corstr = "independence")

  # Assign the model to the list with a name based on the plan name
  model_name <- gsub(" ", "_", plan_names[i])
  model_name <- gsub("[^[:alnum:]_]", "", model_name)  # Clean up non-alphanumeric characters
  
  nocor_nowgt_int_tab[[paste0("tab_", model_name)]] <- table(current_data$policonsumo2_rec, current_data$tr_outcome) %>% data.frame()
  nocorr_nowgt_int_list[[paste("model", model_name, sep = "_")]] <- model
  emmeans_response <- emmeans(model, ~policonsumo2_rec,  rg.limit=2e5, type = "response")
  nocorr_nowgt_int_probs_list[[paste0("emmeans_response_int_", model_name)]] <- emmeans_response
  assign(paste0("emmeans_response_int_", model_name), emmeans_response, envir = .GlobalEnv)
}

nocor_nowgt_int_tab[[1]] %>% dplyr::mutate(Var0=ifelse(grepl("no PSU",Var1),1,0)) %>% dplyr::group_by(Var0, Var2) %>% dplyr::summarise(n= sum(Freq))
nocor_nowgt_int_tab[[2]] %>% dplyr::mutate(Var0=ifelse(grepl("no PSU",Var1),1,0)) %>% dplyr::group_by(Var0, Var2) %>% dplyr::summarise(n= sum(Freq))
nocor_nowgt_int_tab[[3]] %>% dplyr::mutate(Var0=ifelse(grepl("no PSU",Var1),1,0)) %>% dplyr::group_by(Var0, Var2) %>% dplyr::summarise(n= sum(Freq))
nocor_nowgt_int_tab[[4]] %>% dplyr::mutate(Var0=ifelse(grepl("no PSU",Var1),1,0)) %>% dplyr::group_by(Var0, Var2) %>% dplyr::summarise(n= sum(Freq))
nocor_nowgt_int_tab[[5]] %>% dplyr::mutate(Var0=ifelse(grepl("no PSU",Var1),1,0)) %>% dplyr::group_by(Var0, Var2) %>% dplyr::summarise(n= sum(Freq))
# > nocor_nowgt_int_tab[1]
# $tab_basic_ambulatory
# Var1 Var2 Freq
# 1   0.no PSU    0  629
# 2 1.only PSU    0 1210
# 3     2.both    0  286
# 4   0.no PSU    1 1827
# 5 1.only PSU    1 4450
# 6     2.both    1 1591
# 
# > nocor_nowgt_int_tab[2]
# $tab_GP_intensive_ambulatory
# Var1 Var2 Freq
# 1   0.no PSU    0  624
# 2 1.only PSU    0 1615
# 3     2.both    0  401
# 4   0.no PSU    1 1573
# 5 1.only PSU    1 5226
# 6     2.both    1 2058
# 
# > nocor_nowgt_int_tab[3]
# $tab_GP_residential
# Var1 Var2 Freq
# 1   0.no PSU    0  261
# 2 1.only PSU    0  905
# 3     2.both    0  400
# 4   0.no PSU    1  572
# 5 1.only PSU    1 2360
# 6     2.both    1  706
# 
# > nocor_nowgt_int_tab[4]
# $tab_WO_intensive_ambulatory
# Var1 Var2 Freq
# 1   0.no PSU    0  110
# 2 1.only PSU    0  253
# 3     2.both    0   61
# 4   0.no PSU    1  292
# 5 1.only PSU    1  726
# 6     2.both    1  318
# 
# > nocor_nowgt_int_tab[5]
# $tab_WO_residential
# Var1 Var2 Freq
# 1   0.no PSU    0  172
# 2 1.only PSU    0  413
# 3     2.both    0  130
# 4   0.no PSU    1  289
# 5 1.only PSU    1 1147
# 6     2.both    1  383
# 
# 
# 

# 
 inviisble("no hay tan pocos casos en un estrato, salvo en WO intensive ambulatory")

#[1] "basic ambulatory"        "GP intensive ambulatory" "GP residential"          "WO intensive ambulatory" "WO residential"          

invisible("En el basic ambulatory [no era en el main], GP intensive-amb [sí era] y GP residential [no era], el sig es el ambos (consumo alcohol)")
  
broom::tidy(nocorr_nowgt_int_list[[1]], exponentiate=T, conf.int=T)[1:3,] 
#    term                         estimate std.error statistic     p.value conf.low conf.high
#   <chr>                           <dbl>     <dbl>     <dbl>       <dbl>    <dbl>     <dbl>
# 1 (Intercept)                0.00000870    4.83       5.81  0.0159      6.68e-10     0.113
# 2 policonsumo2_rec1.only PSU 1.01          0.0143     0.488 0.485       9.82e- 1     1.04 
# 3 policonsumo2_rec2.both     1.08          0.0161    24.8   0.000000622 1.05e+ 0     1.12 

broom::tidy(nocorr_nowgt_int_list[[2]], exponentiate=T, conf.int=T)[1:3,]
#   term                         estimate std.error statistic       p.value conf.low conf.high
#   <chr>                           <dbl>     <dbl>     <dbl>         <dbl>    <dbl>     <dbl>
# 1 (Intercept)                0.00000655    4.81        6.15 0.0132        5.23e-10    0.0822
# 2 policonsumo2_rec1.only PSU 1.02          0.0153      1.98 0.160         9.92e- 1    1.05  
# 3 policonsumo2_rec2.both     1.10          0.0162     35.1  0.00000000312 1.07e+ 0    1.14  

invisible("Aunque GP residential es protector el policonsumo en both")
broom::tidy(nocorr_nowgt_int_list[[3]], exponentiate=T, conf.int=T)[1:3,]
#   term                       estimate std.error statistic  p.value     conf.low    conf.high
#   <chr>                         <dbl>     <dbl>     <dbl>    <dbl>        <dbl>        <dbl>
# 1 (Intercept)                   1.14     9.06    0.000199 0.989    0.0000000221 58484383.   
# 2 policonsumo2_rec1.only PSU    0.989    0.0275  0.175    0.676    0.937               1.04 
# 3 policonsumo2_rec2.both        0.885    0.0332 13.5      0.000243 0.830               0.945


invisible("En WO intensive ambulatory, no tira ninguno")

broom::tidy(nocorr_nowgt_int_list[[4]], exponentiate=T, conf.int=T)[1:3,]
# # A tibble: 3 x 7
#   term                       estimate std.error statistic p.value conf.low  conf.high
#   <chr>                         <dbl>     <dbl>     <dbl>   <dbl>    <dbl>      <dbl>
# 1 (Intercept)                0.000318   12.0        0.452  0.501  2.02e-14 4991312.  
# 2 policonsumo2_rec1.only PSU 0.956       0.0357     1.62   0.203  8.91e- 1       1.02
# 3 policonsumo2_rec2.both     1.07        0.0376     3.13   0.0767 9.93e- 1       1.15
# 
invisible("En WO residential, ambos mueven la aguja a mayor riesgo")
# 
broom::tidy(nocorr_nowgt_int_list[[5]], exponentiate=T, conf.int=T)[1:3,]
#   term                        estimate std.error statistic  p.value  conf.low conf.high
#   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>     <dbl>     <dbl>
# 1 (Intercept)                249351.     11.6         1.15 0.284    0.0000331   1.88e15
# 2 policonsumo2_rec1.only PSU      1.14    0.0388     11.3  0.000775 1.06        1.23e 0
# 3 policonsumo2_rec2.both          1.14    0.0439      8.83 0.00297  1.05        1.24e 0