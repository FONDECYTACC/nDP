rbind(c("TOTAL",score_classifier("Base_fiscalia_v13c_dic_2022_3", "tot_off_top_bin", "tot_bin")),
      c("Acquisitive",score_classifier("Base_fiscalia_v13c_dic_2022_3", "acq_top_bin", "acq_bin")),
      c("SUD-rel",score_classifier("Base_fiscalia_v13c_dic_2022_3", "sud_top_bin", "sud_bin")),
      c("Violent",score_classifier("Base_fiscalia_v13c_dic_2022_3", "vio_top_bin", "vio_bin")),
      c("Other",score_classifier("Base_fiscalia_v13c_dic_2022_3", "oth_top_bin", "oth_bin"))) %>% 
  knitr::kable("html",caption= "Table 1. Properties") %>% kableExtra::kable_classic()