
library(knitr)
library(rvest)
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)

#TABELA 1 - območje: [1,10] x [1,10], delitev območja do 100 s korakom 10, do 950 tock v množici S 

stolpci1 <- c("ID", "Stevilo_vseh_tock_v_množici_S", "Delez_izbranih_tock", "Uspešnost_izracunane_ploscine_1._metode", "Uspešnost_izracunanega_obsega_1._metode", "Relativna_napaka_ploscine_2._metode", "Relativna_napaka_obsega_2._metode")
tabela1 <- read_table("files/rezultati_primerjave1000_tock_na_obmocju_10_10.tsv", col_names = stolpci1, skip = 1,
                    locale=locale(encoding = "Windows-1250"))


# izbira podatkov, kjer je uspešnost več kot 90%

tabela_uspesnosti_vec_kot_90 <- tabela1 %>% select(-c(ID, Relativna_napaka_obsega_2._metode, Relativna_napaka_ploscine_2._metode, Uspešnost_izracunanega_obsega_1._metode))%>% 
  filter(Uspešnost_izracunane_ploscine_1._metode >= 90) %>%
  group_by(Stevilo_vseh_tock_v_množici_S)%>% summarise(Delez_tock = mean(Delez_izbranih_tock)) %>%
  rename( Stevilo_vseh_tock = Stevilo_vseh_tock_v_množici_S )


graf_povprecij_2 <- ggplot(data=tabela_uspesnosti_vec_kot_90, aes(x=Stevilo_vseh_tock, y=Delez_tock)) + 
  geom_point(color=rgb(0.8,0.4,0.1,0.7)) + 
  geom_line(color=rgb(0.8,0.4,0.1,0.7)) + 
  xlab('Število vseh točk v množici S') + 
  ylab('Povprečno število deleža izbranih točk') +
  ggtitle('Primerjava povprečnega deleža izbranih točk \n za 90% natančnost metode') +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))

# izbira podatkov, kjer je uspešnost več kot 99%

tabela_uspesnosti_vec_kot_99 <- tabela1 %>% select(-c(ID, Relativna_napaka_obsega_2._metode, Relativna_napaka_ploscine_2._metode, Uspešnost_izracunanega_obsega_1._metode))%>% 
  filter(Uspešnost_izracunane_ploscine_1._metode >= 99) %>%
  group_by(Stevilo_vseh_tock_v_množici_S)%>% summarise(Delez_tock = mean(Delez_izbranih_tock)) %>%
  rename( Stevilo_vseh_tock = Stevilo_vseh_tock_v_množici_S )


#izbira podatkov, kjer je uspešnost več kot 99,9%

tabela_uspesnosti_vec_kot_99.9 <- tabela1 %>% select(-c(ID, Relativna_napaka_obsega_2._metode, Relativna_napaka_ploscine_2._metode, Uspešnost_izracunanega_obsega_1._metode))%>% 
  filter(Uspešnost_izracunane_ploscine_1._metode >= 99.9) %>%
  group_by(Stevilo_vseh_tock_v_množici_S)%>% summarise(Delez_tock = mean(Delez_izbranih_tock)) %>%
  rename( Stevilo_vseh_tock = Stevilo_vseh_tock_v_množici_S )
  
skupna_tabela_1 <- inner_join(tabela_uspesnosti_vec_kot_90,tabela_uspesnosti_vec_kot_99,by="Stevilo_vseh_tock")
skupna_tabela <- inner_join(skupna_tabela_1, tabela_uspesnosti_vec_kot_99.9, by="Stevilo_vseh_tock")%>% 
pivot_longer(-c(Stevilo_vseh_tock), names_to = "Povprečni_delež_točk", values_to = "Vrednosti") 
skupna_tabela$Povprečni_delež_točk <- sub("Delez_tock.y", "Povprečni delež točk v vzorcu za 99% natančnost", skupna_tabela$Povprečni_delež_točk)
skupna_tabela$Povprečni_delež_točk <- sub("Delez_tock.x", "Povprečni delež točk v vzorcu za 90% natančnost", skupna_tabela$Povprečni_delež_točk)
skupna_tabela$Povprečni_delež_točk <- sub("Delez_tock", "Povprečni delež točk v vzorcu za 99,9% natančnost", skupna_tabela$Povprečni_delež_točk)

#graf za število točk v vzorcu za različne natančnosti

graf_različnih_deležev <- skupna_tabela %>% ggplot(aes(x=Stevilo_vseh_tock, y=Vrednosti, palette="Pastel1", col=Povprečni_delež_točk)) + 
  geom_point()+
  geom_line() +
  xlab('Število vseh točk v množici S') + 
  ylab('Povprečni delež točk v vzorcu') +
  ggtitle('Primerjava povprečnega deleža izbranih točk \n za 90%, 99% in 99,9% natančnost 1. metode \n pri aproksimaciji ploščine konveksne lupine') +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))

# napake kroga

tabela_napak_kroga <- tabela1 %>% select(c(Stevilo_vseh_tock_v_množici_S, Relativna_napaka_ploscine_2._metode, Relativna_napaka_obsega_2._metode))

povprecna_napaka <- mean(tabela_napak_kroga$Relativna_napaka_ploscine_2._metode)
povprecna_napaka_obseg <- mean(tabela_napak_kroga$Relativna_napaka_obsega_2._metode)


#primerjava povprečne napake pri 1 in 2. metodi pri istem številu točk

#Napaka 1. metode

napake_za_1._metodo <- tabela1 %>% select(Stevilo_vseh_tock_v_množici_S, Delez_izbranih_tock, Uspešnost_izracunane_ploscine_1._metode, Uspešnost_izracunanega_obsega_1._metode) %>%
                                  mutate(Delez_izbranih_tock = round(Delez_izbranih_tock,0)) %>%
                                  mutate(Napaka_1._metode_pri_aproksimaciji_ploščine = 100 - Uspešnost_izracunane_ploscine_1._metode) %>%
                                  select(-Uspešnost_izracunane_ploscine_1._metode)%>%
                                  mutate(Napaka_1._metode_pri_aproksimaciji_obsega = 100 - Uspešnost_izracunanega_obsega_1._metode) %>%
                                  select(-c(Uspešnost_izracunanega_obsega_1._metode, Delez_izbranih_tock)) %>%
                                  pivot_longer(c(-Stevilo_vseh_tock_v_množici_S ),names_to = "Napaka_aproksimacije", values_to = "Vrednosti") %>%
                                  group_by(Stevilo_vseh_tock_v_množici_S, Napaka_aproksimacije) %>% summarize(Napake= mean(Vrednosti))
napake_za_1._metodo$Napaka_aproksimacije <- sub("Napaka_1._metode_pri_aproksimaciji_ploščine", "Napaka 1. metode pri aproksimaciji ploščine", napake_za_1._metodo$Napaka_aproksimacije )
napake_za_1._metodo$Napaka_aproksimacije <- sub("Napaka_1._metode_pri_aproksimaciji_obsega", "Napaka 1. metode pri aproksimaciji obsega", napake_za_1._metodo$Napaka_aproksimacije ) 


graf_napak_1.metode <- napake_za_1._metodo %>% ggplot(aes(x=Stevilo_vseh_tock_v_množici_S, y=Napake, palette="Pastel1", col=Napaka_aproksimacije)) + 
  geom_line() + 
  geom_point()+
  xlab('Število vseh točk v množici S') + 
  ylab('Povprečna napaka aproksimacije') +
  ggtitle('Povprečne napake aproksimacije 1. metode pri različnih velikostih vzorca') +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))

#Napake 2. metode

napake_za_2._metodo <- tabela_napak_kroga %>% 
  mutate(Napaka_2._metode_pri_aprokismaciji_ploščine = Relativna_napaka_ploscine_2._metode) %>%
    mutate(Napaka_2._metode_pri_aprokismaciji_obsega = Relativna_napaka_obsega_2._metode) %>%
    select(-c(Relativna_napaka_obsega_2._metode, Relativna_napaka_ploscine_2._metode)) %>%
  pivot_longer(c(-Stevilo_vseh_tock_v_množici_S ),names_to = "Napaka_aproksimacije_2._metode", values_to = "Vrednosti_2._metode") %>%
  group_by(Stevilo_vseh_tock_v_množici_S, Napaka_aproksimacije_2._metode) %>% summarize(Napake_2._metode= mean(Vrednosti_2._metode))

napake_za_2._metodo$Napaka_aproksimacije_2._metode <- sub("Napaka_2._metode_pri_aprokismaciji_ploščine", "Napaka 2. metode pri aproksimaciji ploščine", napake_za_2._metodo$Napaka_aproksimacije_2._metode)
napake_za_2._metodo$Napaka_aproksimacije_2._metode <- sub("Napaka_2._metode_pri_aprokismaciji_obsega", "Napaka 2. metode pri aproksimaciji obsega", napake_za_2._metodo$Napaka_aproksimacije_2._metode) 


graf_napak_2.metode <- napake_za_2._metodo %>% ggplot(aes(x=Stevilo_vseh_tock_v_množici_S, y=Napake_2._metode, palette="Pastel1", col=Napaka_aproksimacije_2._metode)) + 
  geom_line() + 
  geom_point()+
  xlab('Število vseh točk v množici S') + 
  ylab('Povprečna napaka aproksimacije') +
  ggtitle('Povprečne napake aproksimacije 2. metode pri različnih velikostih vzorca') +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))

