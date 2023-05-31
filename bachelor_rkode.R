# Laster ned pakker
library(tidyverse)
library(ggplot2)
library(dplyr)
library(rjstat)
library(httr)
library(ggrepel) 
library(readxl)
library(vtable)
library(car)
library(knitr)
suppressPackageStartupMessages(library(sjPlot))
suppressPackageStartupMessages(library(sjmisc))
suppressPackageStartupMessages(library(sjlabelled))

setwd("~/Desktop/Livskvalitetsundersøkelsen 2020 (NSD)")

navdf_2020_feb <- 
  read_excel("202012_HL060_Helt_ledige_Fylke_og_kommune. Tidsserie maaned.xlsx",
             sheet = 3,
             na = "*")
colnames(navdf_2020_feb) <- navdf_2020_feb[6, ]
navdf_2020_feb <- navdf_2020_feb[,-6:-15]
navdf_2020_feb <- navdf_2020_feb[,-3:-4]

navdf_2020_feb <- navdf_2020_feb[-1:-7, ]
navdf_2020_feb <- navdf_2020_feb[-12:-13, ]

colnames(navdf_2020_feb)[1] <- "fylkenr"
colnames(navdf_2020_feb)[2] <- "fylke"
colnames(navdf_2020_feb)[3] <- "ledighetsraten"

navdf_2020_feb$fylkenr <- as.numeric(navdf_2020_feb$fylkenr)
navdf_2020_feb$ledighetsraten <- as.numeric(navdf_2020_feb$ledighetsraten)

NSD2020 <- read_csv("NSD2935.csv")

NSD <- NSD2020 %>% 
  select(A1a, saminnt, Kjonn, sysselsatt, alder, Fylke, H91) %>% 
  filter(A1a >= 0, A1a <= 10) %>% 
  filter(alder <= 67) %>% 
  mutate(saminnt = saminnt/1000) %>% 
  rename(fylkenr = Fylke)

NSD$fylkenr <- as.numeric(NSD$fylkenr)
OLS_test <- merge(NSD, navdf_2020_feb, by=c("fylkenr"),all.x=TRUE)
OLS_test <- OLS_test %>% 
  rename(arbeidsledig = sysselsatt, livstilfredshet = A1a, kjonn = Kjonn) %>% 
  mutate(ledighetslevel = ledighetsraten) %>% 
  mutate(ledighetsfac = ledighetsraten) %>% rename(betrakter = H91)

OLS_test$betrakter[OLS_test$betrakter == 0] <- 1 
OLS_test$betrakter[OLS_test$betrakter == 1] <- 1 
OLS_test$betrakter[is.na(OLS_test$betrakter)] <- 1 
OLS_test$betrakter[OLS_test$betrakter == 2] <- 2 
OLS_test$betrakter[OLS_test$betrakter == 3] <- 3  
OLS_test$betrakter[OLS_test$betrakter == 4] <- 3 
OLS_test$betrakter[OLS_test$betrakter == 5] <- 3 
OLS_test$betrakter[OLS_test$betrakter == 6] <- 3  
OLS_test$betrakter[OLS_test$betrakter == 7] <- 3 
OLS_test$betrakter[OLS_test$betrakter == 8] <- 3
OLS_test$betrakter[OLS_test$betrakter == 9] <- 1  

OLS_test <- OLS_test %>% filter(betrakter <= 3) %>% na.omit() %>% 
  mutate(arbeidsstatus = betrakter)
OLS_test$arbeidsstatus[OLS_test$arbeidsstatus == 1] <- "Sysselsatt"
OLS_test$arbeidsstatus[OLS_test$arbeidsstatus == 2] <- "Arbeidsledig"
OLS_test$arbeidsstatus[OLS_test$arbeidsstatus == 3] <- "Ikke Sysselsatt"
OLS_test$betrakter <- as.factor(OLS_test$betrakter) 

OLS_test$ledighetsfac[OLS_test$ledighetsfac == 2] <- 2 
OLS_test$ledighetsfac[OLS_test$ledighetsfac == 2.1] <- 2
OLS_test$ledighetsfac[OLS_test$ledighetsfac == 1.9] <- 2
OLS_test$ledighetsfac[OLS_test$ledighetsfac == 2.5] <- 3 
OLS_test$ledighetsfac[OLS_test$ledighetsfac == 2.8] <- 3
OLS_test$ledighetsfac[OLS_test$ledighetsfac == 2.3] <- 1
OLS_test$ledighetsfac[OLS_test$ledighetsfac == 2.2] <- 1
# Ledighet i nivå (høy-lav-snitt)
OLS_test$ledighetslevel[OLS_test$ledighetslevel == 2] <- "Lav Arbeidsledighet" 
OLS_test$ledighetslevel[OLS_test$ledighetslevel == 2.1] <- "Lav Arbeidsledighet"
OLS_test$ledighetslevel[OLS_test$ledighetslevel == 1.9] <- "Lav Arbeidsledighet"
OLS_test$ledighetslevel[OLS_test$ledighetslevel == 2.5] <- "Hoy Arbeidsledighet" 
OLS_test$ledighetslevel[OLS_test$ledighetslevel == 2.8] <- "Hoy Arbeidsledighet"
OLS_test$ledighetslevel[OLS_test$ledighetslevel== 2.3] <- 
  "Gjennomsnittlig Arbeidsledighet"
OLS_test$ledighetslevel[OLS_test$ledighetslevel == 2.2] <- 
  "Gjennomsnittlig Arbeidsledighet"

OLS_test$ledighetsfac <- as.factor(OLS_test$ledighetsfac)
#Figur1 livstilfredshet og arbeidsledig
OLS_test %>%  select(ledighetsraten, livstilfredshet, arbeidsledig, fylkenr) %>%
  mutate(arbeidsledig = as.character(arbeidsledig)) %>%
  pivot_wider(names_from = arbeidsledig, values_from = livstilfredshet,
              values_fn = mean) %>% 
  rename(Sysselsatt = `1`, Arbeidsledig = `2`) %>% 
  pivot_longer(cols = 3:4) %>% rename(Arbeidsstatus=name) %>% 
  ggplot(aes(x = ledighetsraten, y = value, col = Arbeidsstatus)) + 
  geom_line() + 
  xlab("Arbeidsledighet i %") + 
  ylab("Gjennomsnittlig Livstilfredshet") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'))+
  theme_classic()

# Figur 2 livstilfredshetgap i ulike fylkene
OLS_test %>% select(livstilfredshet, ledighetsraten, arbeidsledig,
                    fylkenr, fylke) %>% group_by(fylke) %>% 
  pivot_wider(names_from = arbeidsledig, values_from = livstilfredshet, 
              values_fn = mean) %>% 
  mutate(lykkegap=`1`-`2`) %>% 
  ggplot(aes(x = ledighetsraten, y = lykkegap)) +
  geom_point() + 
  xlab("Regional Arbeidsledighet i %") + 
  ylab("Differanse i gjennomsnittlig livstilfredshet") +
  geom_text(aes(label = fylke),position = position_dodge(width = 0.6),
            vjust = -0.8, size = 3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white')) +
  theme_classic()

OLS_test$kjonn[OLS_test$kjonn == 1] <- 0 
OLS_test$kjonn[OLS_test$kjonn == 2] <- 1 
OLS_test$arbeidsledig[OLS_test$arbeidsledig == 1] <- 0 
OLS_test$arbeidsledig[OLS_test$arbeidsledig == 2] <- 1 

#Tabell med deskriptiv statistikk
df_tabell <- subset(OLS_test, select = c("livstilfredshet", 
                                         "arbeidsledig",
                                         "ledighetslevel", 
                                         "saminnt",
                                         "kjonn",
                                         "alder"))
labs <- c("Livstilfredshet", 
          "Arbeidsstatus: Arbeidsledig", 
          "Regional Arbeidsledighet:",
          "Samlet inntekt i 1000kr", 
          "Kjonn:Kvinner",
          "Alder")
st(df_tabell, labels=labs, col.width = 0.6,factor.numeric = TRUE,
   summ = list(
     c("notNA(x)","mean(x)","sd(x)","min(x)","max(x)"), 
     c("notNA(x)","mean(x)")
   ),
   summ.names = list(
     c("N","Gjennomsnitt","std.avvik","Min","Max")
   ))


#Regresjon
modell1 <- 
  lm(livstilfredshet ~ arbeidsledig,
     data = OLS_test)
summary(modell1)

modell2 <- 
  lm(livstilfredshet ~ arbeidsledig + ledighetsfac,
     data = OLS_test)
summary(modell2)

modell3 <- 
  lm(livstilfredshet ~ arbeidsledig + ledighetsfac + 
       arbeidsledig:ledighetsfac,
     data = OLS_test)
summary(modell3)

modell4 <- 
  lm(livstilfredshet ~ arbeidsledig + ledighetsfac + 
       arbeidsledig:ledighetsfac + saminnt + kjonn + alder,
     data = OLS_test)
summary(modell4)

#setter sammen
labss <- c("Konstantledd",
           "Arbeidsledig", 
           "Lav Arbeidsledighet", 
           "Høy Arbeidsledighet",
           "Arbeidsledig:Lav Arbeidsledighet",
           "Arbeidsledig:Høy Arbeidsledighet", 
           "Samlet inntekt i 1000kr",
           "Kvinner",
           "Alder")
labsz <- c("Modell 1 ", "Modell 2 ", "Modell 3 ", "Modell 4 ")

tab_model(modell1,
          modell2,
          modell3,
          modell4,
          digits = 4,
          show.ci = F, 
          show.se = TRUE, 
          p.style = "stars", 
          pred.labels = labss,
          string.pred = "Prediktorer",
          string.est = "Estimater",
          string.se = "std.avvik",
          dv.labels = labsz,
          collapse.se = TRUE)

# Robusthetsanalyse

# Deskriptiv 
df_tabell2 <- subset(OLS_test, select = c("livstilfredshet", 
                                         "arbeidsstatus",
                                         "ledighetslevel", 
                                         "saminnt",
                                         "kjonn",
                                         "alder"))
labs2 <- c("Livstilfredshet", 
          "Arbeidsstatus:", 
          "Regional Arbeidsledighet:",
          "Samlet inntekt i 1000kr", 
          "Kjonn:Kvinner",
          "Alder")
st(df_tabell2, labels=labs2, col.width = 0.6,factor.numeric = TRUE,
   summ = list(
     c("notNA(x)","mean(x)","sd(x)","min(x)","max(x)"), 
     c("notNA(x)","mean(x)")
   ),
   summ.names = list(
     c("N","Gjennomsnitt","std.avvik","Min","Max")
   ))

# regresjon
modell5 <- 
  lm(livstilfredshet ~ betrakter,
     data = OLS_test)
summary(modell5)

modell6 <- 
  lm(livstilfredshet ~ betrakter + ledighetsfac,
     data = OLS_test)
summary(modell6)

modell7 <- 
  lm(livstilfredshet ~ betrakter + ledighetsfac + 
       betrakter:ledighetsfac,
     data = OLS_test)
summary(modell7)

modell8 <- 
  lm(livstilfredshet ~ betrakter + ledighetsfac + 
       betrakter:ledighetsfac + saminnt + kjonn + alder,
     data = OLS_test)
summary(modell8)

#setter sammen
labss4 <- c("Konstantledd",
           "Arbeidsledig",
           "Ikke sysselsatt",
           "Lav Arbeidsledighet", 
           "Høy Arbeidsledighet",
           "Arbeidsledig:Lav Arbeidsledighet",
           "Ikke sysselsatt:Lav Arbeidsledighet",
           "Arbeidsledig:Lav Arbeidsledighet",
           "Ikke sysselsatt:Høy Arbeidsledighet",
           "Samlet inntekt i 1000kr",
           "Kvinner",
           "Alder")
labsz4 <- c("Modell 5 ", "Modell 6 ", "Modell 7 ", "Modell 8 ")

tab_model(modell5,
          modell6,
          modell7,
          modell8,
          digits = 4,
          show.ci = F, 
          show.se = TRUE, 
          p.style = "stars", 
          pred.labels = labss4,
          string.pred = "Prediktorer",
          string.est = "Estimater",
          string.se = "std.avvik",
          dv.labels = labsz4,
          collapse.se = TRUE)
