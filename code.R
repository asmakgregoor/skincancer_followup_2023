
################################################################################
################################################################################
##Script Time to reconsider skin cancer related follow-up visits 
## DD: 23-07-07 ##
## Author: AM Smak Gregoor ##
################################################################################
################################################################################

################################################################################
################################################################################
#### Step 0: Load necessary packages and functions #############################
################################################################################
################################################################################

required_packages <- c("cowplot", "data.table", "dplyr", "ggplot2",
                       "ggsci", "svglite", "scales")

for(package in required_packages) {
  if(!require(package, character.only = TRUE)) install.packages(package)
  library(package, character.only = TRUE)
}

################################################################################
################################################################################
#### Step 1: Overview of all follow-up schedules ###############################
################################################################################
################################################################################

##Follow-up cSCCs
#source #1: https://nvdv.nl/patienten/richtlijnen-en-onderzoek/richtlijnen/richtlijn-pcc
#source #2: https://richtlijnendatabase.nl/richtlijn/plaveiselcelcarcinoom_pcc_van_de_huid/follow_up.html
#Follow-up schedule low risk cutaneous squamous cell carcinoma (from stage 0-I)
# 1 x per 6 months in the first two years
# 1 x per year during the third, fourth and fifth year
 
# Follow-up schedule high risk cutaneous squamous cell carcinoma (from stage III):
# 1 x per 3 months during the first year
# 1 x per 4 months during the second year
# 1 x per 6 months during the third year
# 1 x per year during the fourth and fifth year

##Follow-up Melanoma: https://richtlijnendatabase.nl/gerelateerde_documenten/f/12160/Oncoline%20-%20Melanoom.pdf
# For patients with stage IA melanoma
# 1 consult in total (in first year)
# For patients from stage IB melanoma onward:
# 1st year: once per three months;
# 2nd year: once per six months;
# 3rd until 5th year: once per year

## Follow-up BCCs
# source #1: https://nvdv.nl/patienten/richtlijnen-en-onderzoek/richtlijnen/richtlijn-bcc
# source #2: https://richtlijnendatabase.nl/richtlijn/basaalcelcarcinoom/follow-up_bcc.html
# 50% of BCCs is high risk
# High risk is 1 consult per year for 5 years
# Low risk is only 1 consult (in first year)

################################################################################
################################################################################
#### Step 2: Create dataframe of skin cancer incidences ########################
################################################################################
################################################################################

####################################
#### Observed trend 1993 - 2021 ####
####################################

## Source data: NKR-cijfers iKNL
## URL: https://iknl.nl/nkr-cijfers 
# Last downloaded 11-01-23 # 
# BCC data provided on request#

df1 <- data.frame(year = seq(from = 1989, to = 2021))
df1$cscc_inc <- c(2098, 2152, 2165, 2376, 2377, 2710, 2697, 3001, 
                  2936, 2895, 3230, 3238, 3473, 3518, 3742, 4206,
                  4734, 5176, 5736, 6218, 6782, 7858, 8853, 8961, 
                  9203, 9316, 8874, 8520, 11216, 13280, 13924, 13794, 
                  14884)
df1$mm_inc <- c(1719, 1563, 1614, 1726, 1651, 2031, 1968, 2122, 2340, 
                2274, 2453, 2527, 2860, 2846, 3072, 3294, 3611, 
                3614, 3834, 4212, 4443, 4770, 5203, 5356, 5627, 
                5737, 6009, 6649, 6211, 6731, 7072, 6565, 7530)

# Data on incidence of BCCs are available from 2001 onwards
# Data on BCCs can be provided on request. Request should be directed to iKNL
# URL: https://iknl.nl/forms/dataapplication 
df1$bcc_inc <- c(rep(NA, 12), 
                 rep(0, 21))

##############################################
#### Predicted trend from 2022 until 2040 ####
##############################################

# source: https://iknl.nl/getmedia/fddd879f-e15f-4193-b151-647b98167921/IKNL_huidkanker-in-NL_rapport_NKR.pdf
# based on IKNL data, p.32, we expect an increase per year of:
# cscc: 899 per year
# mm: 328 per year
# bcc: 1051 per year

df2 <- data.frame(year = seq(from = 2022, to = 2040))
df2$cscc_inc <- seq(from = tail(df1$cscc_inc, 1), by = 899,  length.out = nrow(df2))
df2$mm_inc   <- seq(from = tail(df1$mm_inc, 1),   by = 328,  length.out = nrow(df2))
df2$bcc_inc  <- seq(from = tail(df1$bcc_inc, 1),  by = 1051, length.out = nrow(df2))

df <- rbind(df1, df2)

#############################
#### Incidence per stage ####
#############################

#### cSCC ####
# 64% stage 1, 9% stage 2, 24% stage 3, 4% stage 4#
# Source #1: https://www.kanker.nl/kankersoorten/plaveiselcelcarcinoom/algemeen/overlevingscijfers-van-plaveiselcelcarcinoom
# Source #2: https://iknl.nl/getmedia/fddd879f-e15f-4193-b151-647b98167921/IKNL_huidkanker-in-NL_rapport_NKR.pdf
df$cscc_stage1 <- df$cscc_inc * 0.64
df$cscc_stage2 <- df$cscc_inc * 0.09
df$cscc_stage3 <- df$cscc_inc * 0.24
df$cscc_stage4 <- df$cscc_inc * 0.04

#### Melanoma ####
# 57.5% stage 1a, 15.2 stage 1b, 15.7% stage 2, 8.4% stage 3, 3.1% stage 4#
# Source #1: https://www.kanker.nl/kankersoorten/melanoom/algemeen/overlevingscijfers-van-melanoom
# Source #2: https://iknl.nl/getmedia/fddd879f-e15f-4193-b151-647b98167921/IKNL_huidkanker-in-NL_rapport_NKR.pdf
df$mm_stage1a <- df$mm_inc * 0.575
df$mm_stage1b <- df$mm_inc * 0.152
df$mm_stage2 <-  df$mm_inc * 0.157
df$mm_stage3 <-  df$mm_inc * 0.084
df$mm_stage4 <-  df$mm_inc * 0.031

#### BCC ####
# 50% high risk, 50% low risk
# Source:
df$bcc_lr <- df$bcc_inc * 0.5
df$bcc_hr <- df$bcc_inc * 0.5

################################################################################
################################################################################
#### Step 3: Determine number of follow-up consultations based on guidelines ###
#### for follow-up per skin cancer per stage ###################################
################################################################################
################################################################################

# As we use five years of follow-up, repeat each row 5 times to calculate the 
# burden per preceeding year
df$freq <- 5
df_expanded <- as.data.table(df[rep(row.names(df), df$freq), 
                                -which(names(df) == "freq")])
df_expanded[, rn := rowid(year)]

##############
#### cSCC ####
##############

# Define follow-up frequencies cSCC per year
df_expanded[, cscc_stage1_2 := cscc_stage1 + cscc_stage2]
df_expanded[, cscc_stage3_4 := cscc_stage3 + cscc_stage4]
df_expanded[, fu_cscc_stage1_2 := case_when(rn %in% 1:2 ~ 2, 
                                            rn %in% 3:5 ~ 1)]

df_expanded[, fu_cscc_stage3_4 := case_when(rn == 1 ~ 4,
                                            rn == 2 ~ 3,
                                            rn == 3 ~ 2,
                                            rn %in% 4:5 ~ 1)]

# Define total amount of cSCC follow-up consultations
df_expanded[, csccsinFU := cscc_stage1_2 * fu_cscc_stage1_2 + cscc_stage3_4 * fu_cscc_stage3_4]
df_expanded[, csccsinFU_yr := year + rn - 1]

# Sum per year is number of cSCCs in FU in that year
df_fucscc <- df_expanded[, c("csccsinFU_yr", "cscc_inc", "csccsinFU")]
df_fucscc$csccsinFU_yr <- sort(df_fucscc$csccsinFU_yr)

df_fucsccfinal <- df_fucscc %>%
  dplyr::group_by(csccsinFU_yr) %>%
  dplyr::summarize(csccs_final = sum(csccsinFU)) %>%
  dplyr::ungroup()

names(df_fucsccfinal) <- c("year", "count") # = number of FU consultations
df_fucsccfinal$type <- "cscc"

##################
#### Melanoma ####
##################

# Define follow-up frequencies melanoma per year
df_expanded[, mm_stage1b_4 := mm_stage1b + mm_stage2 + mm_stage3 + mm_stage4]
df_expanded[, fu_mm_stage1a := case_when(rn == 1 ~ 1, 
                                         rn %in% 2:5 ~ 0)]
df_expanded[, fu_mm_stage1b_4 := case_when(rn == 1 ~ 4, 
                                           rn == 2 ~ 2,
                                           rn %in% 3:5 ~ 1)]

# Define total amount of mm follow-up consultations
df_expanded[, mmsinFU := mm_stage1a * fu_mm_stage1a + mm_stage1b_4 * fu_mm_stage1b_4]
df_expanded[, mmsinFU_yr := year + rn - 1]

# Sum per year is number of mms in FU in that year
df_fumm <- df_expanded[, c("mmsinFU_yr", "mm_inc", "mmsinFU")]
df_fucscc$mmsinFU_yr <- sort(df_fumm$mmsinFU_yr)

df_fummfinal <- df_fumm %>%
  dplyr::group_by(mmsinFU_yr) %>%
  dplyr::summarize(mms_final = sum(mmsinFU)) %>%
  dplyr::ungroup()

names(df_fummfinal) <- c("year", "count") # = number of FU consultations
df_fummfinal$type <- "mm"

#############
#### BCC ####
#############

# Define follow-up frequencies BCC per year
df_expanded[, fu_bcc_lr := case_when(rn == 1 ~ 1,
                                     rn %in% 2:5 ~ 0)]
df_expanded[, fu_bcc_hr := case_when(rn %in% 1:5 ~ 1)]

# Define total amount of BCC follow-up consultations
df_expanded[, bccsinFU := bcc_lr * fu_bcc_lr + bcc_hr * fu_bcc_hr]
df_expanded[, bccsinFU_yr := year + rn - 1]

# Sum per year is number of BCC in FU in that year
df_fubcc <- df_expanded[, c("bccsinFU_yr", "bcc_inc", "bccsinFU")]
df_fubcc$bccsinFU_yr <- sort(df_fubcc$bccsinFU_yr)

df_fubccfinal <- df_fubcc %>%
  dplyr::group_by(bccsinFU_yr) %>%
  dplyr::summarize(bccs_final = sum(bccsinFU)) %>%
  dplyr::ungroup() 

names(df_fubccfinal) <- c("year", "count") # = number of FU consultations
df_fubccfinal$type <- "bcc"

################################################################################
################################################################################
#### Step 4: Prepare data for plotting #########################################
################################################################################
################################################################################

base_levels <- c("Incidence ",
                 "Predicted incidence ",
                 "Number of follow-up consultations",
                 "Predicted number of follow-up consultations")

cscc_levels  <- paste0(base_levels, rep(c("cSCC", ""), each = 2))
mm_levels    <- paste0(base_levels, rep(c("melanoma", ""), each = 2))
bcc_levels   <- paste0(base_levels, rep(c("BCC", ""), each = 2))
total_levels <- paste0(base_levels, rep(c("skin cancer", ""), each = 2))

##############
#### cSCC ####
##############

df_cscc <- df[, c("year", "cscc_inc")]
names(df_cscc) <- c("year", "count") # = cscc_inc
df_cscc$type <- "inc_cscc"

graph_cscc <- rbind(df_fucsccfinal, df_cscc)

graph_cscc <- graph_cscc %>% 
  mutate(color = case_when(
    type == "inc_cscc" & year <= 2021 ~ cscc_levels[1],
    type == "inc_cscc" & year >  2021 ~ cscc_levels[2],
    type == "cscc"     & year <= 2021 ~ cscc_levels[3],
    type == "cscc"     & year >  2021 ~ cscc_levels[4]
    )
  )

graph_cscc$color <- factor(graph_cscc$color, levels = cscc_levels)

##################
#### Melanoma ####
##################

df_mm <- df[, c("year", "mm_inc")]
names(df_mm) <- c("year", "count") # = mm_inc
df_mm$type <- "inc_mm"

graph_mm <- rbind(df_fummfinal, df_mm)

graph_mm <- graph_mm %>% 
  mutate(color = case_when(
    type == "inc_mm" & year <= 2021 ~ mm_levels[1],
    type == "inc_mm" & year >  2021 ~ mm_levels[2],
    type == "mm"     & year <= 2021 ~ mm_levels[3],
    type == "mm"     & year >  2021 ~ mm_levels[4]
    )
  )

graph_mm$color <- factor(graph_mm$color, mm_levels)

##############
#### BCC #####
##############

df_bcc <- df[, c("year", "bcc_inc")]
names(df_bcc) <- c("year", "count") # = bcc_inc
df_bcc$type <- "inc_bcc"

graph_bcc <- rbind(df_fubccfinal, df_bcc)

graph_bcc <- graph_bcc %>% 
  mutate(color = case_when(
    type == "inc_bcc" & year <= 2021 ~ bcc_levels[1],
    type == "inc_bcc" & year >  2021 ~ bcc_levels[2],
    type == "bcc"     & year <= 2021 ~ bcc_levels[3],
    type == "bcc"     & year >  2021 ~ bcc_levels[4]
    )
  )

graph_bcc$color <- factor(graph_bcc$color, bcc_levels)

####################
#### All types #####
####################

graph_bcc$type <-  gsub("bcc",  "tumor", graph_bcc$type)
graph_mm$type  <-  gsub("mm",   "tumor", graph_mm$type)
graph_cscc$type <- gsub("cscc", "tumor", graph_cscc$type)

total <- merge(graph_bcc, graph_mm,   by = c("year", "type"))
total <- merge(total,     graph_cscc, by = c("year", "type"))
total$countfinal <- total$count + total$count.x + total$count.y # everything with NA is now removed as well

total <- total %>% 
  mutate(colorfinal = case_when(
    type == "inc_tumor" & year <= 2021 ~ total_levels[1],
    type == "inc_tumor" & year >  2021 ~ total_levels[2],
    type == "tumor"     & year <= 2021 ~ total_levels[3],
    type == "tumor"     & year >  2021 ~ total_levels[4]
    )
  )

total$colorfinal <- factor(total$colorfinal, total_levels)

#########################################################################################
#########################################################################################
#### Step 5: Plot each type of skin cancer separately and make a multi-facetted plot ####
#########################################################################################
#########################################################################################

# Values
plot_cols <- c("#3399FF", "#99CCFF", "#4C9900", "#66CC00")
y_label <- "Frequency (n)"
x_label <- "Years"


## With range of uncertainty ##
##BCC##
uncertainty <- 0.25
graph_bcc$upperbound <- graph_bcc$count * (1 + uncertainty)
graph_bcc$lowerbound <- graph_bcc$count * (1 - uncertainty)

plots_bcc <- ggplot(subset(graph_bcc, year > 2005), aes(x = year, y = count, group = as.factor(type), color = as.factor(color))) +
  geom_line(size = 1.5) +
  geom_point(size = 0.85) +
  geom_ribbon(data = (subset(graph_bcc, year > 2005 & (color=="Number of follow-up consultations"|(color=="Predicted number of follow-up consultations"&year==2022)))), 
              aes(x = year, ymin = lowerbound, ymax = upperbound), 
              fill = "#4C9900", alpha = 0.1, inherit.aes = FALSE) +
  geom_ribbon(data = (subset(graph_bcc, (color=="Predicted number of follow-up consultations"))), 
              aes(x = year, ymin = lowerbound, ymax = upperbound), 
              fill = "#66CC00", alpha = 0.1, inherit.aes = FALSE) +
  scale_x_continuous(limits = c(1993, 2040), breaks = c(NA, seq(2000, 2040, 10)), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 650000), labels = label_number(big.mark = ","), expand = c(0, 0)) +
  theme_cowplot() +
  ggtitle("Incidence and related follow-up consultations for BCC")+
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = y_label, x = x_label) +
  scale_color_manual(values = plot_cols) +
  guides(fill = "none")

#cSCC #
graph_cscc$upperbound <- graph_cscc$count * (1 + uncertainty)
graph_cscc$lowerbound <- graph_cscc$count * (1 - uncertainty)


plots_cscc <- ggplot(graph_cscc, aes(x = year, y = count, group = as.factor(type), color = as.factor(color))) +
  geom_line(size = 1.5) +
  geom_point(size = 0.85) +
  geom_ribbon(data = (subset(graph_cscc, (color=="Number of follow-up consultations"|(color=="Predicted number of follow-up consultations"&year==2022)))), 
              aes(x = year, ymin = lowerbound, ymax = upperbound), 
              fill = "#4C9900", alpha = 0.1, inherit.aes = FALSE) +
  geom_ribbon(data = (subset(graph_cscc, (color=="Predicted number of follow-up consultations"))), aes(x = year, ymin = lowerbound, ymax = upperbound), 
              fill = "#66CC00", alpha = 0.1, inherit.aes = FALSE) +
  scale_x_continuous(limits = c(1993, 2040), breaks = c(NA, seq(2000, 2040, 10)), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 650000), labels = label_number(big.mark = ","), expand = c(0, 0)) +
  theme_cowplot() +
  ggtitle("Incidence and related follow-up consultations for cSCC")+
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = y_label, x = x_label) +
  scale_color_manual(values = plot_cols) +
  guides(fill = "none")


#Melanoma #
graph_mm$upperbound <- graph_mm$count * (1 + uncertainty)
graph_mm$lowerbound <- graph_mm$count * (1 - uncertainty)


plots_mm <- ggplot(graph_mm, aes(x = year, y = count, group = as.factor(type), color = as.factor(color))) +
  geom_line(size = 1.5) +
  geom_point(size = 0.85) +
  geom_ribbon(data = (subset(graph_mm, (color=="Number of follow-up consultations"|(color=="Predicted number of follow-up consultations"&year==2022)))), 
              aes(x = year, ymin = lowerbound, ymax = upperbound), 
              fill = "#4C9900", alpha = 0.1, inherit.aes = FALSE) +
  geom_ribbon(data = (subset(graph_mm, (color=="Predicted number of follow-up consultations"))), aes(x = year, ymin = lowerbound, ymax = upperbound), 
              fill = "#66CC00", alpha = 0.1, inherit.aes = FALSE) +
  scale_x_continuous(limits = c(1993, 2040), breaks = c(NA, seq(2000, 2040, 10)), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 650000), labels = label_number(big.mark = ","), expand = c(0, 0)) +
  theme_cowplot() +
  ggtitle("Incidence and related follow-up consultations for Melanoma")+
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = y_label, x = x_label) +
  scale_color_manual(values = plot_cols) +
  guides(fill = "none")

# Total #
total$upperbound <- total$countfinal * (1 + uncertainty)
total$lowerbound <- total$countfinal * (1 - uncertainty)


plots_total <- ggplot(subset(total, year > 2005), aes(x = year, y = countfinal, group = as.factor(type), color = as.factor(colorfinal))) +
  geom_line(size = 1.5) +
  geom_point(size = 0.85) +
  geom_ribbon(data = (subset(total, year > 2005 & (colorfinal=="Number of follow-up consultations"|(colorfinal=="Predicted number of follow-up consultations"&year==2022)))), 
              aes(x = year, ymin = lowerbound, ymax = upperbound), 
              fill = "#4C9900", alpha = 0.1, inherit.aes = FALSE) +
  geom_ribbon(data = (subset(total, (colorfinal=="Predicted number of follow-up consultations"))), aes(x = year, ymin = lowerbound, ymax = upperbound), 
              fill = "#66CC00", alpha = 0.1, inherit.aes = FALSE) +
  scale_x_continuous(limits = c(1993, 2040), breaks = c(NA, seq(2000, 2040, 10)), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 650000), labels = label_number(big.mark = ","), expand = c(0, 0)) +
  theme_cowplot() +
  ggtitle("All types combined")+
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = y_label, x = x_label) +
  scale_color_manual(values = plot_cols) +
  guides(fill = "none")

annotate_label <- "For illustrative purposes only \nFor BCC data see data request information"
plots_bcc <- plots_bcc +  annotate("text", 
                                     label = annotate_label, 
                                     size = 6, x = 2015, y = 250000, color="red")
plots_total <- plots_total +  annotate("text", 
                                         label = annotate_label, 
                                         size = 6, x = 2015, y = 250000, color="red")

# Combine into final figure
plots <- list(plots_bcc, plots_cscc, plots_mm, plots_total)
fig1 <- plot_grid(plotlist = plots, 
                  labels = c("(a)", "(b)", "(c)", "(d)"))
# ggsave("figure_skincancer_followup_illustrative.png", dpi = 600, height = 10, width = 20, bg = "white")


