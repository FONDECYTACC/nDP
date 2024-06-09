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
