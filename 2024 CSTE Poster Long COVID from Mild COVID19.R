library(tidyverse)
library(readr)
library(survey)
library(jtools)
library(MetBrewer)


hps23_raw <- plyr::rbind.fill(
                read_csv("C:/Users/subli/Documents/Projects/HPS/HPS_Week57_PUF_CSV/pulse2023_puf_57.csv"),
                read_csv("C:/Users/subli/Documents/Projects/HPS/HPS_Week58_PUF_CSV/pulse2023_puf_58.csv"),
                read_csv("C:/Users/subli/Documents/Projects/HPS/HPS_Week59_PUF_CSV/pulse2023_puf_59.csv"),
                read_csv("C:/Users/subli/Documents/Projects/HPS/HPS_Week60_PUF_CSV/pulse2023_puf_60.csv"),
                read_csv("C:/Users/subli/Documents/Projects/HPS/HPS_Week61_PUF_CSV/pulse2023_puf_61.csv"),
                read_csv("C:/Users/subli/Documents/Projects/HPS/HPS_Week62_PUF_CSV/pulse2023_puf_62.csv"),
                read_csv("C:/Users/subli/Documents/Projects/HPS/HPS_Week63_PUF_CSV/pulse2023_puf_63.csv")
                )

hps24_raw <- plyr::rbind.fill(
                read_csv("C:/Users/subli/Documents/Projects/HPS/HPS_Phase4Cycle01_PUF_CSV/hps_04_00_01_puf.csv"),
                read_csv("C:/Users/subli/Documents/Projects/HPS/HPS_Phase4Cycle02_PUF_CSV/hps_04_00_02_puf.csv"),
                read_csv("C:/Users/subli/Documents/Projects/HPS/HPS_Phase4Cycle03_PUF_CSV/hps_04_00_03_puf.csv")
                
            ) %>% mutate(WEEK = case_when(CYCLE == 1 ~ 64,
                                          CYCLE == 2 ~ 65,
                                          CYCLE == 3 ~ 66,
                                          CYCLE == 4 ~ 67))


hps <- rbind(
    hps23_raw %>% select(SCRAM, PWEIGHT, HWEIGHT,REGION, EST_MSA,EST_ST, WEEK, TBIRTH_YEAR, 
                         HADCOVIDRV, WHENCOVIDRV1,WHENCOVIDRV2,WHENCOVIDRV3, SYMPTOMS, LONGCOVID, SYMPTMNOW,SYMPTMIMPCT
         ),
    
    hps24_raw %>% select(SCRAM, PWEIGHT, HWEIGHT,REGION, EST_MSA,EST_ST, WEEK, TBIRTH_YEAR, 
                         HADCOVIDRV, WHENCOVIDRV1,WHENCOVIDRV2,WHENCOVIDRV3, SYMPTOMS, LONGCOVID, SYMPTMNOW,SYMPTMIMPCT
         )) %>%

  
    mutate(
        pweight10 = PWEIGHT / 10,
      
          state = as.factor(case_when(
          EST_ST == '01' ~ 'Alabama', EST_ST == '02' ~ 'Alaska', EST_ST == '04' ~ 'Arizona', 
          EST_ST == '05' ~ 'Arkansas', EST_ST == '06' ~ 'California', EST_ST == '08' ~ 'Colorado', 
          EST_ST == '09' ~ 'Connecticut', EST_ST == '10' ~ 'Delaware', EST_ST == '11' ~ 'District of Columbia',
          EST_ST == '12' ~ 'Florida', EST_ST == '13' ~ 'Georgia', EST_ST == '15' ~ 'Hawaii',
          EST_ST == '16' ~ 'Idaho', EST_ST == '17' ~ 'Illinois', EST_ST == '18' ~ 'Indiana', 
          EST_ST == '19' ~ 'Iowa',EST_ST == '20' ~ 'Kansas', EST_ST == '21' ~ 'Kentucky', 
          EST_ST == '22' ~ 'Louisiana', EST_ST == '23' ~ 'Maine', EST_ST == '24' ~ 'Maryland', 
          EST_ST == '25' ~ 'Massachusetts', EST_ST == '26' ~ 'Michigan', EST_ST == '27' ~ 'Minnesota', 
          EST_ST == '28' ~ 'Mississippi', EST_ST == '29' ~ 'Missouri', EST_ST == '30' ~ 'Montana',
          EST_ST == '31' ~ 'Nebraska', EST_ST == '32' ~ 'Nevada', EST_ST == '33' ~ 'New Hampshire', 
          EST_ST == '34' ~ 'New Jersey', EST_ST == '35' ~ 'New Mexico', EST_ST == '36' ~ 'New York', 
          EST_ST == '37' ~ 'North Carolina', EST_ST == '38' ~ 'North Dakota', EST_ST == '39' ~ 'Ohio', 
          EST_ST == '40' ~ 'Oklahoma', EST_ST == '41' ~ 'Oregon', EST_ST == '42' ~ 'Pennsylvania', 
          EST_ST == '44' ~ 'Rhode Island', EST_ST == '45' ~ 'South Carolina', EST_ST == '46' ~ 'South Dakota',
          EST_ST == '47' ~ 'Tennessee', EST_ST == '48' ~ 'Texas', EST_ST == '49' ~ 'Utah',
          EST_ST == '50' ~ 'Vermont', EST_ST == '51' ~ 'Virginia', EST_ST == '53' ~ 'Washington',
          EST_ST == '54' ~ 'West Virginia', EST_ST == '55' ~ 'Wisconsin', EST_ST == '56' ~ 'Wyoming',
          TRUE ~ as.character(EST_ST))),
         
          metro = as.factor(case_when(
          EST_MSA == '35620' ~ "New York-Newark-Jersey City",
          EST_MSA == '31080' ~ "Los Angeles-Long Beach-Anaheim",
          EST_MSA == '16980' ~ "Chicago-Naperville-Elgin",
          EST_MSA == '19100' ~ "Dallas-Fort Worth-Arlington",
          EST_MSA == '26420' ~ "Houston-The Woodlands-Sugar Land",
          EST_MSA == '47900' ~ "Washington-Arlington-Alexandria",
          EST_MSA == '33100' ~ "Miami-Fort Lauderdale-Pompano Beach",
          EST_MSA == '37980' ~ "Philadelphia-Camden-Wilmington",
          EST_MSA == '12060' ~ "Atlanta-Sandy Springs-Alpharetta",
          EST_MSA == '38060' ~ "Phoenix-Mesa-Chandler",
          EST_MSA == '14460' ~ "Boston-Cambridge-Newton",
          EST_MSA == '41860' ~ "San Francisco-Oakland-Berkeley",
          EST_MSA == '40140' ~ "Riverside-San Bernardino-Ontario",
          EST_MSA == '19820' ~ "Detroit-Warren-Dearborn",
          EST_MSA == '42660' ~ "Seattle-Tacoma-Bellevue")),
         

    
      longcovid_ever = factor(
      case_when(
        LONGCOVID == 1 ~ 1,
        LONGCOVID == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1),
      labels = c("No Long COVID", "Long COVID")),
    
    longcovid_current = factor(
      case_when(
        LONGCOVID == 2 ~ NA_real_,
        LONGCOVID == 1 & SYMPTMNOW == 1 ~ 1,
        LONGCOVID == 1 & SYMPTMNOW == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1),
      labels = c("Not Current", "Current")),
    
      
      symptoms_recode = factor(case_when(SYMPTOMS %in% c(1,2,3,4) ~ SYMPTOMS,
                                         SYMPTOMS %in% c(-88,-99) ~ NA_real_),
                                levels = c(1,2,3,4),
                                labels = c("None","Mild","Moderate","Severe")),
      
      symptoms_recode2 = factor(case_when(SYMPTOMS %in% c(3,4) ~ SYMPTOMS-1,
                                            SYMPTOMS %in% c(1,2) ~ 1,
                                     SYMPTOMS %in% c(-88,-99) ~ NA_real_),
                                levels = c(1,2,3),
                                labels = c("Mild","Moderate","Severe")),
    
    covidstatus = factor(case_when(HADCOVIDRV == 2 ~ 0,
                                   HADCOVIDRV == 1 & WHENCOVIDRV1 == 1 & WHENCOVIDRV2 != 1 & WHENCOVIDRV3 != 1 ~ 1,    
                                   LONGCOVID == 2 ~ 2,
                                   LONGCOVID == 1 & SYMPTMNOW == 2 ~ 3,
                                   LONGCOVID == 1 & SYMPTMNOW == 1 ~ 4),
                         levels = c(0,1,2,3,4),
                         labels = c("Never COVID", "Recent COVID", "No Long COVID", "Past Long COVID", "Current Long COVID")
                         ),
    
    lc_mild = case_when(
        LONGCOVID == 1 & SYMPTOMS %in% c(1,2) ~ 1,
        LONGCOVID == 1 & SYMPTOMS %in% c(3,4) ~ 0,
        TRUE ~ NA_real_),
    
    lc_n = case_when(
        LONGCOVID == 1 ~ 1,
        LONGCOVID == 2 ~ 0,
        TRUE ~ NA_real_),
    
    impact = factor(case_when(
        SYMPTMIMPCT %in% c(1:3) ~ SYMPTMIMPCT,
        TRUE ~ NA_real_
    ),
                    levels = c(1,2,3), 
                    labels = c("Yes, a lot","Yes, a little","No, not at all"))
    
    ) %>%
  rename_all(tolower)


### Create Survey object from combined dataframe
hps_svy <-svydesign(id = ~scram, weights = ~pweight10, data = hps)

# Sankey Numbers
svytable(~longcovid_ever+symptoms_recode2,design=hps_svy)
svytable(~longcovid_current,design=hps_svy)
svytable(~impact,design=hps_svy)

# Percentage of Long COVID Cases Mild, Moderate, or Severe

ggplot(data = svytable(~symptoms_recode2,design=hps_svy %>% 
              subset(longcovid==1)) %>% 
              prop.table() %>% as.data.frame(), 
       
       aes(x=Freq*100, 
           y = symptoms_recode2,
           label=paste0(round(Freq*100, digits = 1) %>% format(big.mark = ","),"%"),
           fill = symptoms_recode2
           )    
       )  + 
    geom_col() + 
    scale_fill_manual(values = met.brewer("Austria",3)) + 
    theme_nice() + theme(legend.position = "none") + geom_label(color="white", fontface="bold", size =5) +
    xlim(0,50) +
    xlab("% Of Long COVID Cases") + 
    ylab("COVID-19 Severity")

ggsave(filename = "C:/Users/subli/Dropbox/Projects/COVID/CSTE 2024 Poster/Fig1.tiff",
  plot = last_plot(),
  width = 10,
  height = 3,
  units = "in",
  dpi = 400
)


ggplot(data = svytable(~impact,design = subset(hps_svy,longcovid_current == "Current" & lc_mild == 1)) %>% 
         prop.table() %>% 
         as.data.frame(), 
       
       aes(y=Freq*100, 
           x = impact,
           label=paste0(round(Freq*100, digits = 1) %>% format(big.mark = ","),"%"),
           fill = impact
           )    
       )  + 
    geom_col() + 
    scale_fill_manual(values = met.brewer("Austria",3,override.order = TRUE)) + 
    theme_nice() + theme(legend.position = "none") + geom_label(color="white", fontface="bold", size =5) +
    #ylim(0,60) +
    ylab("% Of Long COVID Cases") + 
    xlab("Symptom Impact")

ggsave(filename = "C:/Users/subli/Dropbox/Projects/COVID/CSTE 2024 Poster/Fig2.tiff",
  plot = last_plot(),
  width = 10,
  height = 3,
  units = "in",
  dpi = 400
)

############# State-level Analysis ##################################

# Total number of Long COVID cases from Mild COVID-19 in each state
lc_st_n <- svyby(~lc_mild, ~state, FUN = svytotal, vartype = "ci", design=subset(hps_svy, is.na(lc_mild) == FALSE)) %>%
  rename(lc_cases = lc_mild, ci_l_cases = ci_l, ci_u_cases = ci_u)

# Percentage of Long COVID cases that resulted from mild infections in each state

## Get average and CI bounds
svyciprop(~lc_mild, design=hps_svy, na.rm = TRUE)

##                 2.5% 97.5%
##  lc_mild 0.230 0.225 0.236

lc_st <- svyby(~lc_mild,~state, design=hps_svy,FUN = svyciprop, vartype = "ci") %>% 
                mutate_if(is.numeric, ~ . * 100) %>% 
                        mutate(quintile = ntile(lc_mild,5),
                       tertile = ntile(lc_mild,3),
                       p211 = factor(case_when(ci_u > 23.0 & ci_l < 23.0 ~ 2, 
                                               ci_u < 23.0 ~ 1,
                                               ci_l > 23.0 ~ 3,
                                               TRUE ~ NA_real_),
                                     levels= c(1,2,3),
                                     labels = c("Below Average",
                                                "95% CI Contains Average",
                                                "Above Average") )) %>%
    left_join(lc_st_n)


g_lc_st <-  lc_st %>%
            mutate(st = fct_reorder(state, lc_cases )) %>%
            ggplot(aes(x=lc_mild,y=st, color=p211)) + 
                geom_point(size=2) + 
                geom_errorbarh(aes(xmin=ci_l,xmax=ci_u, height = .5)) +
                geom_vline(xintercept = c(23.0),linetype="dashed") + 
                geom_vline(xintercept = c(42.5)) + 
                scale_color_manual(values =c("red","darkgrey","blue"), name = NULL)+
                xlim(0,50)+
                scale_x_continuous(breaks = seq(0, 40, by = 5)) + # Set x-axis breaks every 10 from 0 to 40
                geom_text(aes(x = 50, label = scales::comma(round(lc_cases)), hjust = 1),color = "black",size =3) + # Round the text and adjust the position
                theme_nice() + theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                xlab("% Of Long COVID Cases Resulting From Mild COVID-19") + ylab(NULL) +
                coord_cartesian(xlim = c(0, 50)) + 
                annotate("text", x = 50, y = -0.5, label = "# of Cases", hjust = 1, vjust = 1) # Adjusting y coordinate to appear below x-axis
  
                
g_lc_st



ggsave(filename = "C:/Users/subli/Dropbox/Projects/COVID/CSTE 2024 Poster/States.tiff",
  plot = last_plot(),
  width = 7,
  height = 10,
  units = "in",
  dpi = 400
)


