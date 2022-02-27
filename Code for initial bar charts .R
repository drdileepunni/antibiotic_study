f<- file.choose()
abx_df_cleaned <- read.csv(f)

library(ggplot2)
library(ggplot2)
library(plotly)
library(tableone)

library(ggpubr)
library(stringr)

library(tidyr)

abx_df_cleaned$date_for_plot <- format(as.Date(abx_df_cleaned$date_of_admission), "%b %y")
abx_df_cleaned$date_for_plot <- factor(abx_df_cleaned$date_for_plot, levels = c("Mar 20","Apr 20", "May 20","Jun 20","Jul 20","Aug 20","Sep 20","Oct 20","Nov 20","Dec 20","Jan 21","Feb 21","Mar 21","Apr-21", "May 21","Jun 21","Jul 21","Aug 21","Sep 21","Oct 21","Nov 21","Dec 21"))
abx_df_cleaned <- abx_df_cleaned[complete.cases(abx_df_cleaned$date_for_plot),]

abx_df_cleaned$vent <-ifelse(abx_df_cleaned$vented_hours > 0, 1, 0)
##### GETTING DATASETS NEW #####
covid_df <- filter(abx_df_cleaned, abx_df_cleaned$covid %in% c("suspected","positive"))
covid_suspected_df <-filter(abx_df_cleaned, abx_df_cleaned$covid %in% c("suspected"))
covid_positive_df <- filter(abx_df_cleaned, abx_df_cleaned$covid %in% c("positive"))
covid_vented_df <- filter(covid_df, covid_df$vent %in% 1)
covid_nonvented_df <- filter(covid_df, covid_df$vent %in% 0 )
covid_positive_minus_remdesivir_df <- filter(covid_positive_df, !(covid_positive_df$name %in% c("Remdesivir")))
covid_suspected_minus_remdesivir_df <- filter(covid_suspected_df, !(covid_suspected_df$name %in% c("Remdesivir")))


##DEFINING A FUNCTION FOR THE PLOTS
print_plot <- function(df, title_name, y_limit, fill) {
  
  my_plot <- ggplot(df, aes(x=df$date_for_plot, fill=fill)) +
    geom_bar(stat="count", colour = "black")+
    theme(legend.title=element_blank())+
    ggtitle(title_name) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
    theme(axis.title.x = element_blank())+
    ylim(0,y_limit)+
    theme(panel.grid.major = element_line(colour = "white"))+
    theme(panel.grid.minor = element_line(colour = "white"))+
    theme(panel.background = element_rect(fill = "white", colour = "white"))+
    theme(panel.border=element_rect(colour = "gray90", fill=NA, size=1))+
    theme(panel.border = element_blank(),
          axis.line.x = element_line(color="white", size = 0.5),
          axis.line.y = element_line(color="gray5", size = 0.5))
  print(my_plot)
  
  return(my_plot)
  
}




# # GETTING INDIVIDUAL PLOTS
bar_covid_full <- print_plot(covid_df, "COVID positive/suspect", 1500, fill=covid_df$name)
bar_abx_full <- print_plot(abx_df_cleaned, "All antibiotic orders", 2500, fill=abx_df_cleaned$name)
bar_covid_positive <- print_plot(covid_positive_df, "COVID positive", 1000, fill=covid_positive_df$name)
bar_covid_suspected <- print_plot(covid_suspected_df, "COVID suspects", 600, fill=covid_suspected_df$name)
bar_covid_positive_minus_remdesivir <- print_plot(covid_positive_minus_remdesivir_df, 
                                                  "COVID positive excluding Remdesivir orders", 600, 
                                                  fill=covid_positive_minus_remdesivir_df$name)
bar_covid_suspected_minus_remdesivir <- print_plot(covid_suspected_minus_remdesivir_df, 
                                                   "COVID suspects excluding Remdesivir", 600, 
                                                   fill=covid_suspected_minus_remdesivir_df$name)
bar_covid_vented <- print_plot(covid_vented_df, "COVID vented", 600, 
                               fill=covid_vented_df$name)
bar_covid_non_vented <- print_plot(covid_nonvented_df, "COVID non vented", 600, 
                                   fill=covid_nonvented_df$name)
##bar_full_admissions <- print_plot(all_admissions, "All admissions", 1500, fill='admission_count')

# # ARRANGING PLOTS TO A SINGLE FIGURE
w <- ggarrange(bar_covid_positive, bar_covid_suspected, bar_covid_positive_minus_remdesivir, bar_covid_suspected_minus_remdesivir, ncol = 2, nrow = 2,common.legend = TRUE, legend="bottom")
annotate_figure(w, top = text_grob("Antibiotic orders", color = "red", face = "bold", size = 14))

x<-ggarrange(bar_covid_full,bar_abx_full,bar_covid_vented,bar_covid_non_vented, ncol = 2, nrow = 2,common.legend = TRUE, legend="bottom")
annotate_figure(x, top = text_grob("Antibiotic orders", color = "red", face = "bold", size = 14))


