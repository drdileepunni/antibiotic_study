f <-file.choose()
abx_df_first_wave <- read.csv(f)
vent_abx_cov_frst_wave <- filter(abx_mar_sep_20, ((abx_mar_sep_20$vent==1) & (abx_mar_sep_20$abx_for_or==1) & (abx_mar_sep_20$covid_bin==1)))
non_vent_abx_frst_wave <- filter(abx_mar_sep_20, ((abx_mar_sep_20$vent==0) & (abx_mar_sep_20$abx_for_or==1) & (abx_mar_sep_20$covid_bin==1)))

abx_cov_first_wave <- filter(abx_mar_sep_20, (abx_mar_sep_20$abx_for_or==1) & (abx_mar_sep_20$covid_bin==1))
abx_cov_sec_wave <-filter(abx_mar_sep_21, (abx_mar_sep_21$abx_for_or==1) & (abx_mar_sep_21$covid_bin==1))

vent_abx_cov_sec_wave <- filter(abx_mar_sep_21, ((abx_mar_sep_21$vent==1) & (abx_mar_sep_21$abx_for_or==1) & (abx_mar_sep_21$covid_bin==1)))
non_vent_abx_cov_sec_wave <- filter(abx_mar_sep_21, ((abx_mar_sep_21$vent==0) & (abx_mar_sep_21$abx_for_or==1) & (abx_mar_sep_21$covid_bin==1)))

vent_cov_sec_wave <- filter(abx_mar_sep_21, (abx_mar_sep_21$vent==1) & (abx_mar_sep_21$covid_bin==1))
non_vent_cov_sec_wave <- filter(abx_mar_sep_21, (abx_mar_sep_21$vent==0) & (abx_mar_sep_21$covid_bin==1))


barplot(all_adm_df$date_of_admission)
