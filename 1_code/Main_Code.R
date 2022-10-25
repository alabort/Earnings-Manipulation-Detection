##############################################################################
############################## ALAN BORTOLOTTI ###############################
##############################      Paper      ###############################
##############################################################################

rm(list=ls())

# Import data downloaded from COMPUSTAT and ...
AB_Company_Data <- read.csv("US.csv")

##############################################################################
#############################   INTRODUCTION   ###############################
##############################################################################
# Evidence of earnings manipulation

# Calculate Earnings per share (Net income/Number of shares ordinary)
AB_Company_Data$eps <- AB_Company_Data$ni/AB_Company_Data$csho

# Attach data
attach(AB_Company_Data)
detach("package:dplyr", unload = TRUE)
library(dplyr)

# Calculate change in EPS (if error on deltaeps reload package dplyr)
change_eps <- AB_Company_Data %>% 
  group_by(gvkey) %>% 
  mutate(eps_lag1 = dplyr::lag(eps, k=1)) %>%
  mutate(deltaeps = eps - eps_lag1) %>% 
  filter(deltaeps >= -0.1 & deltaeps <= 0.1)

library(ggplot2)
# Plot the histogram 
ggplot(change_eps)+geom_histogram(aes(deltaeps), bins = 50)+
  labs(title = "Change of Earnings Per Share" , 
       x = 'EPS change', y='Observations') + 
  geom_vline(xintercept=0, col='green', linetype=1)+
  theme_classic()

# Calculate change in ROA (if error on ch_roa reload package dplyr)
# ROA calculated as Net income over Total assets
ROA <- AB_Company_Data  %>% 
  mutate(at_lag = dplyr::lag(at, k = 1)) %>% 
  mutate(roa = ni / at_lag) %>% 
  filter(roa >= -0.1 & roa <= 0.1) %>% 
  mutate(roa_lag = dplyr::lag(roa, k=1),ch_roa = roa - roa_lag) %>% 
  filter(ch_roa >-0.1 & ch_roa <=0.1)

# Plot the histogram 
ggplot(subset(ROA, ch_roa != 'NA')) + 
  geom_histogram(aes(ch_roa), bins = 50)+
  labs(title =  "Change in Return on Assets", x = "ROA change",
       y= "Observations") + 
  geom_vline(xintercept=0, col='green', linetype=1)+
  theme_classic()

detach(AB_Company_Data)

##############################################################################
#############################   DATA CLEANING  ###############################
##############################################################################

# Sort data according to gvkey to be able to create lags
AB_Company_Data <- AB_Company_Data %>% arrange(gvkey)


# Lag of variable to compute Beneish ratios
AB_Company_Data$sga <- AB_Company_Data$sale-AB_Company_Data$ib-
  AB_Company_Data$cogs-AB_Company_Data$dp
AB_Company_Data$gvkey_lag <- lag(AB_Company_Data$gvkey)
AB_Company_Data$act_lag <- lag(AB_Company_Data$act)
AB_Company_Data$at_lag <- lag(AB_Company_Data$at)
AB_Company_Data$che_lag <- lag(AB_Company_Data$che)
AB_Company_Data$cogs_lag <- lag(AB_Company_Data$cogs)
AB_Company_Data$dlc_lag <- lag(AB_Company_Data$dlc)
AB_Company_Data$dltt_lag <- lag(AB_Company_Data$dltt)
AB_Company_Data$dp_lag <- lag(AB_Company_Data$dp)
AB_Company_Data$lct_lag <- lag(AB_Company_Data$lct)
AB_Company_Data$ppegt_lag <- lag(AB_Company_Data$ppegt)
AB_Company_Data$rect_lag <- lag(AB_Company_Data$rect)
AB_Company_Data$sale_lag <- lag(AB_Company_Data$sale)
AB_Company_Data$txp_lag <- lag(AB_Company_Data$txp)
AB_Company_Data$sga_lag <- lag(AB_Company_Data$sga)

# The function added "NA" in each first-year (t) ratio available for that 
# company if for the computation of that ratio data (t-1) was required and 
# previous year data was not available.  
# This function adjusts values and takes a considerable amount of time

numrow <- nrow(AB_Company_Data)-1

for(i in 1:numrow) {
  if(is.na(AB_Company_Data$gvkey_lag[i])) {
    print("Initial value")
  } else if(AB_Company_Data$gvkey[i] != AB_Company_Data$gvkey_lag[i]) {
    AB_Company_Data$act_lag[i+1] <- NA
    AB_Company_Data$at_lag[i+1] <- NA
    AB_Company_Data$che_lag[i+1] <- NA
    AB_Company_Data$cogs_lag[i+1] <- NA
    AB_Company_Data$dlc_lag[i+1] <- NA
    AB_Company_Data$dltt_lag[i+1] <- NA
    AB_Company_Data$dp_lag[i+1] <- NA
    AB_Company_Data$lct_lag[i+1] <- NA
    AB_Company_Data$ppegt_lag[i+1] <- NA
    AB_Company_Data$rect_lag[i+1] <- NA
    AB_Company_Data$sale_lag[i+1] <- NA
    AB_Company_Data$txp_lag[i+1] <- NA
    AB_Company_Data$sga_lag[i+1] <- NA
  }
}

# Move the data to another database
AB_Cleaned_Data <- AB_Company_Data

# Unfortunately, the data contains NAs and other errors such as negative
# values for revenues, account receivables and total assets.
# The approach followed was to remove observations with negative 
# total assets, accounts receivables and sales. Moreover, observations
# with NA in total assets, accounts receivables, sales were dropped
# because calculating ratios is impossible in case of NA as denominator or
# numerator.

attach(AB_Cleaned_Data)
AB_Cleaned_Data <- AB_Cleaned_Data[!is.na(AB_Cleaned_Data$at),] 
AB_Cleaned_Data <- AB_Cleaned_Data[!is.na(AB_Cleaned_Data$sale),] 
AB_Cleaned_Data <- AB_Cleaned_Data[!is.na(AB_Cleaned_Data$rect),] 
AB_Cleaned_Data <- AB_Cleaned_Data[!is.na(AB_Cleaned_Data$at_lag),] 
AB_Cleaned_Data <- AB_Cleaned_Data[!is.na(AB_Cleaned_Data$sale_lag),] 
AB_Cleaned_Data <- AB_Cleaned_Data[!is.na(AB_Cleaned_Data$rect_lag),] 
AB_Cleaned_Data <- subset(AB_Cleaned_Data, at>0)
AB_Cleaned_Data <- subset(AB_Cleaned_Data, sale>0)
AB_Cleaned_Data <- subset(AB_Cleaned_Data, rect>0)
AB_Cleaned_Data <- subset(AB_Cleaned_Data, at_lag>0)
AB_Cleaned_Data <- subset(AB_Cleaned_Data, sale_lag>0)
AB_Cleaned_Data <- subset(AB_Cleaned_Data, rect_lag>0)


# Computation of indexes of Beneish model
detach(AB_Cleaned_Data)
attach(AB_Cleaned_Data)
AB_Cleaned_Data$dsri <- (rect/sale)/(rect_lag/sale_lag)
AB_Cleaned_Data$gmi <- (sale-cogs)/(sale_lag-cogs_lag)
AB_Cleaned_Data$aqi <- ((1-act+ppegt)/at)/((1-act_lag+ppegt_lag)/at_lag)
AB_Cleaned_Data$sgi <- sale/sale_lag
AB_Cleaned_Data$depi <- (dp_lag/ppegt_lag)/(dp/ppegt)
AB_Cleaned_Data$sgai <- (sga/sale)/(sga_lag/sale_lag)
AB_Cleaned_Data$lvgi <- ((dltt+lct)/at)/((dltt_lag+lct_lag)/at_lag)
AB_Cleaned_Data$tata <- ((act-act_lag)-(che-che_lag)-((lct-lct_lag)-
                        (dlc-dlc_lag)-(txp-txp_lag))-dp)/at


# Database - eliminate rows with NA and INF
Ratios <- AB_Cleaned_Data[,c(1:3,9,39:43,45:47,68:75)]
Ratios <- Ratios[!is.na(rowSums(Ratios[,5:19])),] 
Ratios <- Ratios[is.finite(rowSums(Ratios[,5:19])),]

# A cutoff value has been chosen because some ratios were excessively 
# high probabbly due to errors in the data
cutoff <- 20
Ratios <- subset(Ratios, gmi > -cutoff & gmi <cutoff)
Ratios <- subset(Ratios, aqi >-cutoff & aqi<cutoff)
Ratios <- subset(Ratios, sgi >-cutoff & sgi <cutoff)
Ratios <- subset(Ratios, depi >-cutoff & depi <cutoff)
Ratios <- subset(Ratios, sgai >-cutoff & sgai <cutoff)
Ratios <- subset(Ratios, lvgi >-cutoff & lvgi <cutoff)
Ratios <- subset(Ratios, tata >-cutoff & tata <cutoff)
Ratios <- subset(Ratios, dsri >-cutoff & dsri <cutoff)

##############################################################################
############################# DESCRIPTIVE STATISTICS #########################
##############################################################################

# Fraudulent companies 
Miss <- subset(Ratios, misstate == 1)
Company_misstated <- unique(Miss$gvkey)
length(Company_misstated)
# [1] 311

# Total companies 
NoMiss <- subset(Ratios, misstate == 0)
Company_tot <- unique(Ratios$gvkey)
length(Company_tot)
#[1] 13872

# Non-fraudulent companies
13872-311

# Misstatements observation length
length(Miss$misstate)
#[1] 759

# Non-Misstatements observation length
length(NoMiss$misstate)
#[1] 100973

# Percentage of manipulators
perc_manipulators <- length(Company_misstated)/length(Company_tot)
perc_manipulators
# [1] 0.02241926

# Figure in appendix: Number of AAER per year
Year_Manipulators <- Miss %>% count(fyear)

ggplot(Year_Manipulators, aes(x=fyear, y=n)) +
  geom_bar(stat="identity")+
  labs(x = 'Fiscal year', y='Misstatements')+
  geom_text(aes(label=n), vjust=-1, color="black", size=5)+
  theme_classic()


# Number of manipulator divided by industry and number of manipulators
# observations divided by industry
Miss_sector <- Miss
Miss_sector <- Miss_sector[!duplicated(Miss_sector$gvkey), ]
Sector <- Miss_sector %>% count(sich)

Sector <- Miss
Sector <- Sector %>% count(sich)

Industry <- data.frame(count = rep(0,16))
for (x in 1:163){
  if (Sector$sich[x] >=100 & Sector$sich[x] <=999 ){
    Industry$count[1] <- Industry$count[1] + Sector$n[x]
  } else if (Sector$sich[x] >=1000 & Sector$sich[x] <=1299 ){
    Industry$count[2] <- Industry$count[2] + Sector$n[x]
  } else if (Sector$sich[x] >=1400 & Sector$sich[x] <=1999 ){
    Industry$count[2] <- Industry$count[2] + Sector$n[x]
  } else if (Sector$sich[x] >=2000 & Sector$sich[x] <=2141){
    Industry$count[3] <- Industry$count[3] + Sector$n[x]
  } else if (Sector$sich[x] >=2200 & Sector$sich[x] <=2399){
    Industry$count[4] <- Industry$count[4] + Sector$n[x]
  } else if (Sector$sich[x] >=2400 & Sector$sich[x] <=2796){
    Industry$count[5] <- Industry$count[5] + Sector$n[x]
  } else if (Sector$sich[x] >=2800 & Sector$sich[x] <=2824){
    Industry$count[6] <- Industry$count[6] + Sector$n[x]
  } else if (Sector$sich[x] >=2840 & Sector$sich[x] <=2899){
    Industry$count[6] <- Industry$count[6] + Sector$n[x]
  } else if (Sector$sich[x] >=1300 & Sector$sich[x] <=1399){
    Industry$count[7] <- Industry$count[7] + Sector$n[x]
  } else if (Sector$sich[x] >=2900 & Sector$sich[x] <=2999){
    Industry$count[7] <- Industry$count[7] + Sector$n[x]
  } else if (Sector$sich[x] >=3000 & Sector$sich[x] <=3569){
    Industry$count[8] <- Industry$count[8] + Sector$n[x]
  } else if (Sector$sich[x] >=3580 & Sector$sich[x] <=3669){
    Industry$count[8] <- Industry$count[8] + Sector$n[x]
  } else if (Sector$sich[x] >=3680 & Sector$sich[x] <=3999){
    Industry$count[8] <- Industry$count[8] + Sector$n[x]
  } else if (Sector$sich[x] >=3570 & Sector$sich[x] <=3579){
    Industry$count[9] <- Industry$count[9] + Sector$n[x]
  } else if (Sector$sich[x] >=3670 & Sector$sich[x] <=3679){
    Industry$count[9] <- Industry$count[9] + Sector$n[x]
  } else if (Sector$sich[x] >=7370 & Sector$sich[x] <=7379){
    Industry$count[9] <- Industry$count[9] + Sector$n[x]
  } else if (Sector$sich[x] >=4000 & Sector$sich[x] <=4899){
    Industry$count[10] <- Industry$count[10] + Sector$n[x]
  } else if (Sector$sich[x] >=4900 & Sector$sich[x] <=4999){
    Industry$count[11] <- Industry$count[11] + Sector$n[x]
  }  else if (Sector$sich[x] >=5000 & Sector$sich[x] <=5999){
    Industry$count[12] <- Industry$count[12] + Sector$n[x]
  } else if (Sector$sich[x] >=7000 & Sector$sich[x] <=7369){
    Industry$count[13] <- Industry$count[13] + Sector$n[x]
  } else if (Sector$sich[x] >=7380 & Sector$sich[x] <=9999){
    Industry$count[13] <- Industry$count[13] + Sector$n[x]
  }  else if (Sector$sich[x] >=6000 & Sector$sich[x] <=6999){
    Industry$count[14] <- Industry$count[14] + Sector$n[x]
  }  else if (Sector$sich[x] >=2830 & Sector$sich[x] <=2836){
    Industry$count[15] <- Industry$count[15] + Sector$n[x]
  }  else if (Sector$sich[x] >=3829 & Sector$sich[x] <=3851){
    Industry$count[15] <- Industry$count[15] + Sector$n[x]
  }  else {
    Industry$count[16] <- Industry$count[16] + Sector$n[x]
  }
}

# Each row number of the database indutry corresponds to the following sectors:
# 1) Agriculture (0100-0999)
# 2) Mining and Construction (1000-1299, 1400-1999)
# 3) Food and Tobacco (2000-2141)
# 4) Textile and Apparel (2200-2399)
# 5) Lumber, Furniture, and Printing (2400-2796)
# 6) Chemicals (2800-2824, 2840-2899)
# 7) Refining and Extractive (1300-1399, 2900-2999)
# 8) Durable Manufacturers (3000-3569, 3580-3669, 3680-3999)
# 9) Computers (3570-3579, 3670-3679, 7370-7379) 
# 10) Transportation (4000-4899)
# 11) Utilities (4900-4999)
# 12) Retail (5000-5999)
# 13) Services (7000-7369, 7380-9999)
# 14)Banks and Insurance (6000-6999)
# 15) Pharmaceuticals (2830-2836, 3829-3851)
Industry


# Print of summary statistics
library(stargazer)
stargazer(Miss, type ="text", title = "Average and other measures", 
          median = TRUE)
# 
# Average and other measures
# =====================================================================================
#   Statistic    N     Mean     St. Dev.    Min   Pctl(25)   Median   Pctl(75)     Max   
# -------------------------------------------------------------------------------------
#   fyear       759 2,002.017    5.016     1,990    1,999     2,002     2,005     2,014  
# gvkey       759 45,567.690 55,643.520  1,009   8,324.5   25,130    62,515    284,041 
# sich        753 4,902.600  1,992.770  100.000 3,576.000 3,851.000 7,340.000 9,997.000
# misstate    759   1.000      0.000       1        1         1         1         1    
# ch_rsst     759   0.070      0.230    -0.967   -0.020     0.040     0.130     1.381  
# dch_rec     759   0.029      0.082    -0.305   -0.005     0.016     0.051     0.383  
# dch_inv     759   0.016      0.057    -0.222   -0.001     0.002     0.025     0.268  
# soft_assets 759   0.641      0.215     0.012    0.508     0.691     0.808     0.982  
# ch_cs       759   0.273      0.532    -1.191    0.011     0.138     0.378     3.692  
# ch_roa      759   -0.013     0.161    -1.056   -0.048    -0.005     0.025     0.735  
# issue       759   0.975      0.156       0        1         1         1         1    
# bm          759   0.524      0.606    -7.034    0.222     0.397     0.698     4.671  
# dsri        759   1.080      0.647     0.073    0.864     0.998     1.138    11.724  
# gmi         759   1.263      1.017    -6.892    0.928     1.113     1.368    12.604  
# aqi         759   0.918      1.636    -19.596   0.749     0.991     1.171    19.231  
# sgi         759   1.258      0.784     0.004    0.995     1.122     1.354    16.891  
# depi        759   1.138      0.872     0.087    0.890     1.014     1.136    12.946  
# sgai        759   1.169      1.105    -6.950    0.901     1.000     1.153    14.646  
# lvgi        759   1.081      0.541     0.019    0.890     1.002     1.139     7.116  
# tata        759   -0.074     0.623    -10.921  -0.063    -0.027     0.013     2.596  
# -------------------------------------------------------------------------------------
stargazer(NoMiss, type ="text", title = "Average and other measures", 
          median = TRUE)

# Average and other measures
# =========================================================================================
#   Statistic      N       Mean     St. Dev.    Min   Pctl(25)   Median   Pctl(75)     Max   
# -----------------------------------------------------------------------------------------
#   fyear       100,973 2,002.437    6.629     1,990    1,997     2,002     2,008     2,014  
# gvkey       100,973 45,893.540 55,326.810  1,004    8,984    20,904    63,826    297,209 
# sich        93,914  4,411.180  1,941.453  100.000 3,065.000 3,825.000 5,621.000 9,997.000
# misstate    100,973   0.000      0.000       0        0         0         0         0    
# ch_rsst     100,973   0.015      0.291    -2.072   -0.051     0.022     0.095     1.504  
# dch_rec     100,973   0.011      0.075    -0.305   -0.012     0.006     0.033     0.383  
# dch_inv     100,973   0.006      0.054    -0.222   -0.004     0.000     0.016     0.268  
# soft_assets 100,973   0.532      0.249     0.005    0.337     0.557     0.736     0.994  
# ch_cs       100,973   0.184      0.914    -6.484   -0.047     0.072     0.231     9.600  
# ch_roa      100,973   -0.005     0.274    -1.672   -0.048    -0.001     0.036     1.622  
# issue       100,973   0.883      0.321       0        1         1         1         1    
# bm          100,973   0.523      1.254    -7.034    0.244     0.487     0.839     4.937  
# dsri        100,973   1.152      1.107    0.0005    0.840     0.992     1.159    19.995  
# gmi         100,973   1.143      1.483    -19.989   0.887     1.066     1.261    19.997  
# aqi         100,973   0.928      1.872    -19.968   0.732     0.987     1.169    19.969  
# sgi         100,973   1.235      1.122    0.0001    0.946     1.072     1.237    19.905  
# depi        100,973   1.123      0.892     0.000    0.891     1.010     1.140    19.815  
# sgai        100,973   1.083      1.602    -19.980   0.838     0.991     1.134    19.988  
# lvgi        100,973   1.142      0.829     0.000    0.892     1.004     1.165    19.902  
# tata        100,973   -0.100     0.852    -19.901  -0.093    -0.041     0.001    19.812  
# -----------------------------------------------------------------------------------------

# Correlation plot to see how correlated is the data
Correlation <- round(cor(Ratios[,4:19]),2)

library(corrplot)
Colors_plot <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", 
                                  "#77AADD", "#4477AA"))
corrplot(Correlation, method="color", col=Colors_plot(200),  
         type="upper", order = "hclust",
         addCoef.col = "black",
         tl.col="black", tl.srt=45,
         diag=TRUE
)


##############################################################################
############################# MODEL CREATION #################################
##############################################################################
detach(AB_Cleaned_Data)
attach(Ratios)
library(AER)

# Beneish model
Formula_BEN <- misstate ~ dsri + gmi + aqi + sgi + depi + sgai + lvgi + tata
Reg_BEN <- glm(formula = Formula_BEN, data = Ratios, family = binomial(link = "logit"))
coeftest(Reg_BEN, vcov=vcovHC(Reg_BEN, type ="HC1"))

# z test of coefficients:
#   
#   Estimate Std. Error  z value  Pr(>|z|)    
#   (Intercept) -4.7760741  0.0988037 -48.3390 < 2.2e-16 ***
#   dsri        -0.0868664  0.0383682  -2.2640 0.0235728 *  
#   gmi          0.0513935  0.0154557   3.3252 0.0008835 ***
#   aqi         -0.0031158  0.0175621  -0.1774 0.8591813    
#   sgi         -0.0065375  0.0231037  -0.2830 0.7772043    
#   depi         0.0268527  0.0337283   0.7961 0.4259478    
#   sgai         0.0389570  0.0144709   2.6921 0.0071004 ** 
#   lvgi        -0.1264274  0.0575609  -2.1964 0.0280626 *  
#   tata         0.0325215  0.0398338   0.8164 0.4142552    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Dechow model
Formula_DEC <- misstate ~ ch_rsst + dch_rec + dch_inv + soft_assets + 
  ch_cs + ch_roa + issue
Reg_DEC <- glm(formula = Formula_DEC, data = Ratios, family = binomial(link = "logit"))
coeftest(Reg_DEC, vcov=vcovHC(Reg_DEC, type ="HC1"))

# z test of coefficients:
#   
#   Estimate Std. Error  z value  Pr(>|z|)    
#   (Intercept) -7.533002   0.250402 -30.0836 < 2.2e-16 ***
#   ch_rsst      0.568672   0.100098   5.6812 1.338e-08 ***
#   dch_rec      1.121850   0.436406   2.5707  0.010151 *  
#   dch_inv      0.746787   0.564848   1.3221  0.186134    
#   soft_assets  1.867253   0.159732  11.6899 < 2.2e-16 ***
#   ch_cs        0.052497   0.018129   2.8957  0.003783 ** 
#   ch_roa      -0.348739   0.084367  -4.1336 3.571e-05 ***
#   issue        1.568711   0.232651   6.7428 1.554e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# To check the robustness of results (Beneish)
Formula_BEN2 <- misstate ~ gmi +  sgi + depi + sgai + lvgi + tata
Reg_BEN2 <- glm(formula = Formula_BEN2, data = Ratios, family = binomial(link = "logit"))
coeftest(Reg_BEN2, vcov=vcovHC(Reg_BEN2, type ="HC1"))


# z test of coefficients:
#   
#   Estimate Std. Error  z value  Pr(>|z|)    
#   (Intercept) -4.8664481  0.0861456 -56.4910 < 2.2e-16 ***
#   gmi          0.0510627  0.0151491   3.3707 0.0007498 ***
#   sgi         -0.0040234  0.0225063  -0.1788 0.8581201    
#   depi         0.0221540  0.0339441   0.6527 0.5139748    
#   sgai         0.0352336  0.0141994   2.4813 0.0130887 *  
#   lvgi        -0.1288263  0.0569515  -2.2620 0.0236952 *  
#   tata         0.0275811  0.0376982   0.7316 0.4643942    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# To check the robustness of results Dechow model
Formula_DEC2 <- misstate ~  dch_rec + dch_inv + soft_assets + 
  ch_cs + issue
Reg_DEC2 <- glm(formula = Formula_DEC2, data = Ratios, family = binomial(link = "logit"))
coeftest(Reg_DEC2, vcov=vcovHC(Reg_DEC2, type ="HC1"))

# z test of coefficients:
#   
#   Estimate Std. Error  z value  Pr(>|z|)    
#   (Intercept) -7.533653   0.250623 -30.0597 < 2.2e-16 ***
#   dch_rec      1.411581   0.428452   3.2946 0.0009856 ***
#   dch_inv      1.091471   0.558849   1.9531 0.0508114 .  
#   soft_assets  1.851373   0.160445  11.5390 < 2.2e-16 ***
#   ch_cs        0.061071   0.018423   3.3150 0.0009166 ***
#   issue        1.596291   0.232479   6.8664 6.585e-12 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# Full and model selected with backward and forward selection
Formula_logit_full <- misstate ~ gmi +aqi + depi + sgai + 
  lvgi + tata +  ch_rsst + dch_rec +   dch_inv + soft_assets + ch_cs +
  ch_roa + issue  +bm
Formula_logit_empty <- misstate~1

Reg_empty <- glm(formula = Formula_logit_empty, data = Ratios,
                 family = binomial(link = "logit"))
Reg_full <- glm(formula = Formula_logit_full, data = Ratios,
                family = binomial(link = "logit"))

#Forward Selection (8 models)
FW_results <- step(Reg_empty, scope=list(lower= Reg_empty, upper=Reg_full), 
                                        direction="forward")
# Followed order of addition: 1) soft_assets, 2) issue, 3) ch_rsst, 4) dch_rec, 5) ch_roa,
# 6) lvgi, 7) ch_cs, 8) sgai

#Backward Selection (6 models)
BW_results <- step(Reg_full, scope=list(lower=Reg_empty, upper=Reg_full),
                   direction="backward")

# Results Forward
coeftest(FW_results, vcov=vcovHC(FW_results, type ="HC1"))
# z test of coefficients:
#   
#   Estimate Std. Error  z value  Pr(>|z|)    
#   (Intercept) -7.471375   0.255450 -29.2479 < 2.2e-16 ***
#   soft_assets  1.890148   0.159364  11.8606 < 2.2e-16 ***
#   issue        1.575260   0.232618   6.7719 1.271e-11 ***
#   ch_rsst      0.515435   0.103359   4.9868 6.138e-07 ***
#   dch_rec      1.380566   0.423060   3.2633 0.0011013 ** 
#   ch_roa      -0.343770   0.083388  -4.1225 3.747e-05 ***
#   lvgi        -0.111232   0.060388  -1.8420 0.0654824 .  
#   ch_cs        0.062589   0.017674   3.5413 0.0003981 ***
#   sgai         0.039749   0.016823   2.3628 0.0181378 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

coeftest(BW_results, vcov=vcovHC(BW_results, type ="HC1"))
# z test of coefficients:
#   
#   Estimate Std. Error  z value  Pr(>|z|)    
#  (Intercept) -7.471375   0.255450 -29.2479 < 2.2e-16 ***
#   sgai         0.039749   0.016823   2.3628 0.0181378 *  
#   lvgi        -0.111232   0.060388  -1.8420 0.0654824 .  
#   ch_rsst      0.515435   0.103359   4.9868 6.138e-07 ***
#   dch_rec      1.380566   0.423060   3.2633 0.0011013 ** 
#   soft_assets  1.890148   0.159364  11.8606 < 2.2e-16 ***
#   ch_cs        0.062589   0.017674   3.5413 0.0003981 ***
#   ch_roa      -0.343770   0.083388  -4.1225 3.747e-05 ***
#   issue        1.575260   0.232618   6.7719 1.271e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# The selected models are the same

COV_BEN <-vcovHC(Reg_BEN, type ="HC1")
COV_DEC<-vcovHC(Reg_DEC, type ="HC1")
COV_full<-vcovHC(Reg_full, type ="HC1")
COV_FW<-vcovHC(FW_results, type ="HC1")
COV_BW<-vcovHC(BW_results, type ="HC1")

ROBSE_BEN <- sqrt(diag(COV_BEN))
ROBSE_DEC <- sqrt(diag(COV_DEC))
ROBSE_full <- sqrt(diag(COV_full))
ROBSE_FW <- sqrt(diag(COV_FW))
ROBSE_BW <-sqrt(diag(COV_BW))

# Print results as a table
stargazer(Reg_BEN,Reg_DEC,Reg_full,FW_results, BW_results, 
          se = list(ROBSE_BEN,ROBSE_DEC,ROBSE_full,ROBSE_FW,ROBSE_BW),
          type="text", title="Regression results")


# Calculation of R squared
PSR2_BEN <- 1-(logLik(Reg_BEN)/logLik(Reg_empty))
PSR2_BEN
# 'log Lik.' 0.001949296 (df=9)
PSR2_DEC <- 1-(logLik(Reg_DEC)/logLik(Reg_empty))
PSR2_DEC
# 'log Lik.' 0.03163513 (df=8)
PSR2_full <- 1-(logLik(Reg_full)/logLik(Reg_empty))
PSR2_full 
# 'log Lik.' 0.03253806 (df=15)
PSR2_FW <- 1-(logLik(FW_results)/logLik(Reg_empty))
PSR2_FW
# 'log Lik.' 0.03213762 (df=9)

# Hosmer Lemeshow test
library(ResourceSelection)
HL_BEN <- hoslem.test(Reg_BEN$y, fitted(Reg_BEN), g=10)
HL_DEC <- hoslem.test(Reg_DEC$y, fitted(Reg_DEC), g=10)
HL_full <- hoslem.test(Reg_full$y, fitted(Reg_full), g=10)
HL_FW <- hoslem.test(FW_results$y, fitted(FW_results), g=10)

HL_BEN
# Hosmer and Lemeshow goodness of fit (GOF) test
# 
# data:  Reg_BEN$y, fitted(Reg_BEN)
# X-squared = 21.033, df = 8, p-value = 0.007059

HL_DEC
# Hosmer and Lemeshow goodness of fit (GOF) test
# 
# data:  Reg_DEC$y, fitted(Reg_DEC)
# X-squared = 14.049, df = 8, p-value = 0.08049

HL_full
# Hosmer and Lemeshow goodness of fit (GOF) test
# 
# data:  Reg_full$y, fitted(Reg_full)
# X-squared = 13.819, df = 8, p-value = 0.08661

HL_FW
# Hosmer and Lemeshow goodness of fit (GOF) test
# 
# data:  FW_results$y, fitted(FW_results)
# X-squared = 7.3783, df = 8, p-value = 0.4964

# Calculation of the LR (or deviance) test statistic
LR <- FW_results$deviance - Reg_full$deviance 
pchisq(LR, FW_results$df.residual-Reg_full$df.residual, lower.tail = FALSE)
# or also with
anova(FW_results, Reg_full, test="LRT")
# Analysis of Deviance Table
# 
# Model 1: misstate ~ soft_assets + issue + ch_rsst + dch_rec + ch_roa + 
#   lvgi + ch_cs + sgai
# Model 2: misstate ~ gmi + aqi + depi + sgai + lvgi + tata + ch_rsst + 
#   dch_rec + dch_inv + soft_assets + ch_cs + ch_roa + issue + 
#   bm
# Resid. Df Resid. Dev Df Deviance Pr(>Chi)
# 1    101723     8660.1                     
# 2    101717     8656.5  6    3.583   0.7329

# Removal of Group Variables 
# Regression 1) 
linearHypothesis(Reg_BEN, c("dsri=0", "aqi=0", "depi=0", "tata=0"), 
                 vcov=vcovHC(Reg_BEN,type="HC1"))
linearHypothesis(Reg_BEN, c("sgi=0", "sgai=0", "gmi=0"), 
                 vcov=vcovHC(Reg_BEN,type="HC1"))
linearHypothesis(Reg_BEN, c("lvgi=0"), 
                 vcov=vcovHC(Reg_BEN,type="HC1"))

# Regression 2) 
linearHypothesis(Reg_DEC, c("ch_rsst=0", "dch_rec=0", "dch_inv=0", "soft_assets=0"), 
                 vcov=vcovHC(Reg_DEC,type="HC1"))
linearHypothesis(Reg_DEC, c("ch_cs=0", "ch_roa=0"), 
                 vcov=vcovHC(Reg_DEC,type="HC1"))
linearHypothesis(Reg_DEC, c("issue=0"), 
                 vcov=vcovHC(Reg_DEC,type="HC1"))

# Regression 3) 
linearHypothesis(Reg_full, c("ch_rsst=0", "dch_rec=0", "dch_inv=0", "soft_assets=0", 
                            "depi=0", "aqi=0", "tata=0"), 
                 vcov=vcovHC(Reg_full,type="HC1"))
linearHypothesis(Reg_full, c("ch_cs=0", "ch_roa=0", "sgai=0", "gmi=0"), 
                 vcov=vcovHC(Reg_full,type="HC1"))
linearHypothesis(Reg_full, c("issue=0", "lvgi=0", "bm=0"), 
                 vcov=vcovHC(Reg_full,type="HC1"))


# Regression 4) 
linearHypothesis(FW_results, c("ch_rsst=0", "dch_rec=0", "soft_assets=0"), 
                 vcov=vcovHC(FW_results,type="HC1"))
linearHypothesis(FW_results, c("ch_cs=0", "ch_roa=0", "sgai=0"), 
                 vcov=vcovHC(FW_results,type="HC1"))
linearHypothesis(FW_results, c("issue=0", "lvgi=0"), 
                 vcov=vcovHC(FW_results,type="HC1"))

################################### END ########################################