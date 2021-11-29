
library(knitr)
library(rvest)
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)

#TABELA 1 - območje: [1,10] x [1,10], delitev območja do 100 s korakom 10, do 950 tock v množici S 

stolpci1 <- c("ID", "Stevilo_vseh_tock_v_množici_S", "Delez_izbranih_tock", "Uspesnost_izracunane_ploscine_1._metode", "Uspesnost_izracunanega_obsega_1._metode", "Relativna_napaka_ploscine_2._metode", "Relativna_napaka_obsega_2._metode", "m")
tabela1 <- read_table("files/rezultati_primerjave1001_tock_na_obmocju_10_10_pri999_iteracijah_in_101_delitvah_obmocja.tsv", col_names = stolpci1, skip = 1,
                    locale=locale(encoding = "Windows-1250"))


# izbira podatkov, kjer je uspešnost več kot 90%

tabela2 <- tabela1 %>% select(-c(ID, Relativna_napaka_obsega_2._metode, Relativna_napaka_ploscine_2._metode, Uspesnost_izracunanega_obsega_1._metode, m))


# USPEŠNOSTI ZA PLOŠČINO

tabela_uspesnosti_vec_kot_90 <- tabela1 %>% select(-c(ID, Relativna_napaka_obsega_2._metode, Relativna_napaka_ploscine_2._metode, Uspesnost_izracunanega_obsega_1._metode, m))%>% 
  filter(Uspesnost_izracunane_ploscine_1._metode >= 90) %>%
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

tabela_uspesnosti_vec_kot_99 <- tabela1 %>% select(-c(ID, Relativna_napaka_obsega_2._metode, Relativna_napaka_ploscine_2._metode, Uspesnost_izracunanega_obsega_1._metode, m))%>% 
  filter(Uspesnost_izracunane_ploscine_1._metode >= 99) %>%
  group_by(Stevilo_vseh_tock_v_množici_S)%>% summarise(Delez_tock = mean(Delez_izbranih_tock)) %>%
  rename( Stevilo_vseh_tock = Stevilo_vseh_tock_v_množici_S )


#izbira podatkov, kjer je uspešnost več kot 99,9%

tabela_uspesnosti_vec_kot_99.9 <- tabela1 %>% select(-c(ID, Relativna_napaka_obsega_2._metode, Relativna_napaka_ploscine_2._metode, Uspesnost_izracunanega_obsega_1._metode, m))%>% 
  filter(Uspesnost_izracunane_ploscine_1._metode >= 99.9) %>%
  group_by(Stevilo_vseh_tock_v_množici_S)%>% summarise(Delez_tock = mean(Delez_izbranih_tock)) %>%
  rename( Stevilo_vseh_tock = Stevilo_vseh_tock_v_množici_S )
  
skupna_tabela_1 <- inner_join(tabela_uspesnosti_vec_kot_90,tabela_uspesnosti_vec_kot_99,by="Stevilo_vseh_tock")
skupna_tabela <- inner_join(skupna_tabela_1, tabela_uspesnosti_vec_kot_99.9, by="Stevilo_vseh_tock")%>% 
pivot_longer(-c(Stevilo_vseh_tock), names_to = "Povprečni_delež_točk", values_to = "Vrednosti") 
skupna_tabela$Povprečni_delež_točk <- sub("Delez_tock.y", "Povprečni delež točk v vzorcu za 99% natančnost", skupna_tabela$Povprečni_delež_točk)
skupna_tabela$Povprečni_delež_točk <- sub("Delez_tock.x", "Povprečni delež točk v vzorcu za 90% natančnost", skupna_tabela$Povprečni_delež_točk)
skupna_tabela$Povprečni_delež_točk <- sub("Delez_tock", "Povprečni delež točk v vzorcu za 99,9% natančnost", skupna_tabela$Povprečni_delež_točk)

#graf za število točk v vzorcu za različne natančnosti

graf_razlicnih_delezev <- skupna_tabela %>% ggplot(aes(x=Stevilo_vseh_tock, y=Vrednosti, palette="Pastel1", col=Povprečni_delež_točk)) + 
  geom_point()+
  geom_line() +
  scale_x_continuous(breaks = 50*0:1000) +
  scale_y_continuous(breaks = 1*0:100) +
  xlab('Število vseh točk v množici S') + 
  ylab('Povprečni delež točk v vzorcu') +
  ggtitle('Primerjava povprečnega deleža izbranih točk \n za 90%, 99% in 99,9% natančnost 1. metode \n pri aproksimaciji ploščine konveksne lupine') +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))

#USPEŠNOSTI ZA OBSEG

tabela_uspesnosti_obseg_90 <- tabela1 %>% select(-c(ID, Relativna_napaka_obsega_2._metode, Relativna_napaka_ploscine_2._metode, Uspesnost_izracunane_ploscine_1._metode, m))%>% 
  filter(Uspesnost_izracunanega_obsega_1._metode >= 90) %>%
  group_by(Stevilo_vseh_tock_v_množici_S)%>% summarise(Delez_tock = mean(Delez_izbranih_tock)) %>%
  rename( Stevilo_vseh_tock = Stevilo_vseh_tock_v_množici_S )

tabela_uspesnosti_obseg_99 <- tabela1 %>% select(-c(ID, Relativna_napaka_obsega_2._metode, Relativna_napaka_ploscine_2._metode, Uspesnost_izracunane_ploscine_1._metode, m))%>% 
  filter(Uspesnost_izracunanega_obsega_1._metode >= 99) %>%
  group_by(Stevilo_vseh_tock_v_množici_S)%>% summarise(Delez_tock = mean(Delez_izbranih_tock)) %>%
  rename( Stevilo_vseh_tock = Stevilo_vseh_tock_v_množici_S )

tabela_uspesnosti_obseg_99.9 <- tabela1 %>% select(-c(ID, Relativna_napaka_obsega_2._metode, Relativna_napaka_ploscine_2._metode, Uspesnost_izracunane_ploscine_1._metode, m))%>% 
  filter(Uspesnost_izracunanega_obsega_1._metode >= 99.9) %>%
  group_by(Stevilo_vseh_tock_v_množici_S)%>% summarise(Delez_tock = mean(Delez_izbranih_tock)) %>%
  rename( Stevilo_vseh_tock = Stevilo_vseh_tock_v_množici_S )

skupna_tabela_1_obseg <- inner_join(tabela_uspesnosti_obseg_90,tabela_uspesnosti_obseg_99,by="Stevilo_vseh_tock")
skupna_tabela_obseg <- inner_join(skupna_tabela_1_obseg , tabela_uspesnosti_obseg_99.9, by="Stevilo_vseh_tock")%>% 
  pivot_longer(-c(Stevilo_vseh_tock), names_to = "Povprečni_delež_točk", values_to = "Vrednosti") 
skupna_tabela_obseg$Povprečni_delež_točk <- sub("Delez_tock.y", "Povprečni delež točk v vzorcu za 99% natančnost", skupna_tabela_obseg$Povprečni_delež_točk)
skupna_tabela_obseg$Povprečni_delež_točk <- sub("Delez_tock.x", "Povprečni delež točk v vzorcu za 90% natančnost", skupna_tabela_obseg$Povprečni_delež_točk)
skupna_tabela_obseg$Povprečni_delež_točk <- sub("Delez_tock", "Povprečni delež točk v vzorcu za 99,9% natančnost", skupna_tabela_obseg$Povprečni_delež_točk)

#graf za število točk v vzorcu za različne natančnosti - OBSEG

graf_razlicnih_delezev_obseg <- skupna_tabela_obseg %>% ggplot(aes(x=Stevilo_vseh_tock, y=Vrednosti, palette="Pastel1", col=Povprečni_delež_točk)) + 
  geom_point()+
  geom_line() +
  xlab('Število vseh točk v množici S') + 
  ylab('Povprečni delež točk v vzorcu') +
  scale_x_continuous(breaks = 50*0:1000) +
  scale_y_continuous(breaks = 1*0:100) +
  ggtitle('Primerjava povprečnega deleža izbranih točk \n za 90%, 99% in 99,9% natančnost 1. metode \n pri aproksimaciji obsega konveksne lupine') +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))
# napake kroga

tabela_napak_kroga <- tabela1 %>% select(c(Stevilo_vseh_tock_v_množici_S, Relativna_napaka_ploscine_2._metode, Relativna_napaka_obsega_2._metode))

povprecna_napaka <- mean(tabela_napak_kroga$Relativna_napaka_ploscine_2._metode)
povprecna_napaka_obseg <- mean(tabela_napak_kroga$Relativna_napaka_obsega_2._metode)


#primerjava povprečne napake pri 1 in 2. metodi pri istem številu točk

#Napaka 1. metode

napake_za_1._metodo <- tabela1 %>% select(Stevilo_vseh_tock_v_množici_S, Delez_izbranih_tock, Uspesnost_izracunane_ploscine_1._metode, Uspesnost_izracunanega_obsega_1._metode) %>%
                                  mutate(Delez_izbranih_tock = round(Delez_izbranih_tock,0)) %>%
                                  mutate(Napaka_1._metode_pri_aproksimaciji_ploščine = 100 - Uspesnost_izracunane_ploscine_1._metode) %>%
                                  select(-Uspesnost_izracunane_ploscine_1._metode)%>%
                                  mutate(Napaka_1._metode_pri_aproksimaciji_obsega = 100 - Uspesnost_izracunanega_obsega_1._metode) %>%
                                  select(-c(Uspesnost_izracunanega_obsega_1._metode, Delez_izbranih_tock)) %>%
                                  pivot_longer(c(-Stevilo_vseh_tock_v_množici_S ),names_to = "Napaka_aproksimacije", values_to = "Vrednosti") %>%
                                  group_by(Stevilo_vseh_tock_v_množici_S, Napaka_aproksimacije) %>% summarize(Napake= mean(Vrednosti))
napake_za_1._metodo$Napaka_aproksimacije <- sub("Napaka_1._metode_pri_aproksimaciji_ploščine", "Napaka 1. metode pri aproksimaciji ploščine", napake_za_1._metodo$Napaka_aproksimacije )
napake_za_1._metodo$Napaka_aproksimacije <- sub("Napaka_1._metode_pri_aproksimaciji_obsega", "Napaka 1. metode pri aproksimaciji obsega", napake_za_1._metodo$Napaka_aproksimacije ) 


graf_napak_1.metode <- napake_za_1._metodo %>% ggplot(aes(x=Stevilo_vseh_tock_v_množici_S, y=Napake, palette="Pastel1", col=Napaka_aproksimacije)) + 
  geom_line() + 
  geom_point()+
  xlab('Število vseh točk v množici S') + 
  ylab('Povprečna napaka aproksimacije') +
  scale_x_continuous(breaks = 50*0:1000) +
  scale_y_continuous(breaks = 0.5*0:20) +
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
  scale_x_continuous(breaks = 50*0:1000) +
  scale_y_continuous(breaks = 5*0:75) +
  ggtitle('Povprečne napake aproksimacije 2. metode pri različnih velikostih vzorca') +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))

#USPESNOST 2. METODE - OBSEG

tabela_uspesnosti_vec_kot_90_krog <- tabela1 %>% select(-c(ID, Relativna_napaka_ploscine_2._metode, Uspesnost_izracunanega_obsega_1._metode, Uspesnost_izracunane_ploscine_1._metode, m ))%>% 
  filter(Relativna_napaka_obsega_2._metode <= 0.1) %>%
  group_by(Stevilo_vseh_tock_v_množici_S)%>% summarise(Delez_tock = mean(Delez_izbranih_tock)) %>%
  rename( Stevilo_vseh_tock = Stevilo_vseh_tock_v_množici_S )

#PLOŠČINA

stolpci3 <- c("ID", "Stevilo_vseh_tock_v_množici_S", "Relativna_napaka_ploscine_2._metode", "Relativna_napaka_obsega_2._metode")
tabela3 <- read_table("files/rezultati_za_krog_51_tock_na_obmocju_10_10.tsv", col_names = stolpci3, skip=1,
                      locale=locale(encoding = "Windows-1250"))

PL_krog <- tabela3 %>% select(-c(ID, Relativna_napaka_obsega_2._metode)) %>%
                                group_by(Stevilo_vseh_tock_v_množici_S) %>% summarise(povprecna_napaka_pl_krog = mean(Relativna_napaka_ploscine_2._metode))
  
O_krog <- tabela3 %>% select(-c(ID, Relativna_napaka_ploscine_2._metode )) %>%
  group_by(Stevilo_vseh_tock_v_množici_S) %>% summarise(povprecna_napaka_o_krog = mean(Relativna_napaka_obsega_2._metode))

krog_povp_napake <- inner_join(PL_krog, O_krog, by = "Stevilo_vseh_tock_v_množici_S")

krog_p_n <- krog_povp_napake %>% pivot_longer(-c(Stevilo_vseh_tock_v_množici_S), names_to = "Napaka", values_to = "Vrednosti_napak")

krog_p_n$Napaka <- sub("povprecna_napaka_o_krog", "Povprecna napaka 2. metode pri aproksimaciji obsega", krog_p_n$Napaka)
krog_p_n$Napaka <- sub("povprecna_napaka_pl_krog", "Povprecna napaka 2. metode pri aproksimaciji ploščine",krog_p_n$Napaka )

graf_napak_za_krog <- krog_p_n %>% ggplot(aes(x=Stevilo_vseh_tock_v_množici_S, y=Vrednosti_napak , palette="Pastel1", col=Napaka)) + 
  geom_line() + 
  geom_point()+
  xlab('Število vseh točk v množici S') + 
  ylab('Povprečna napaka aproksimacije') +
  scale_x_continuous(breaks = 50*0:1000) +
  ggtitle('Povprečne napake aproksimacije 2. metode pri različnih velikostih vzorca') +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))


### primerjava uspešnosti 2. metode v odvisnosti m

tabela_3 <- tabela1 %>% select(Stevilo_vseh_tock_v_množici_S, Delez_izbranih_tock, Uspesnost_izracunane_ploscine_1._metode, m) %>%
  group_by(Stevilo_vseh_tock_v_množici_S, m) %>% summarise(Napake = mean(Uspesnost_izracunane_ploscine_1._metode))
tabela_3$m <- as.character(tabela_3$m)
tabela_3 <- tabela_3 %>% rename("Število_delitev" = m)



graf_m <- tabela_3 %>% ggplot(aes(x=Stevilo_vseh_tock_v_množici_S, y=Napake, palette="Dark2", color=Število_delitev )) + 
  geom_point()+
  scale_color_discrete(breaks=c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100")) + 
  xlab('Število vseh točk v množici S') + 
  ylab('Povprečna uspešnost aproksimacije') +
  scale_x_continuous(breaks = 50*0:1000) +
  scale_y_continuous(breaks = 5*0:75) +
  ggtitle('Povprečne uspešnost aproksimacije ploščine 2. metode pri različnih močeh \n množice S  in pri različnih delitvah območja') +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))


#2. metoda - izboljšana 

stolpci5_100_tock <- c("ID", "Stevilo_vseh_tock_v_mnozici_S",  "Relativna_napaka_ploscine_2._metode", "Relativna_napaka_obsega_2._metode", "k")
tabela5 <- read_table("files/rezultati_za_boljsi_krog_501_tock_na_obmocju_10_10_pri_vzorcu_50.tsv", col_names = stolpci5_100_tock, skip = 1,
                      locale=locale(encoding = "Windows-1250"))
tabela5_povprecja <- tabela5 %>% select(c(Stevilo_vseh_tock_v_mnozici_S,  Relativna_napaka_ploscine_2._metode, k)) %>%group_by(Stevilo_vseh_tock_v_mnozici_S,k) %>% summarise(Napake = mean(Relativna_napaka_ploscine_2._metode))
tabela5_povprecja$k <-  as.character(tabela5_povprecja$k)  
tabela5_povprecja <- tabela5_povprecja %>% rename("Stevilo_tock_v_podmnozici" = k)

graf_5 <- tabela5_povprecja %>% ggplot(aes(x=Stevilo_tock_v_podmnozici, y=Napake, palette="Dark2", color=Stevilo_tock_v_podmnozici )) + 
  geom_point()+
  facet_wrap(.~Stevilo_vseh_tock_v_mnozici_S, ncol=3)  + 
  xlab('Število točk v podmnožici = vzorcu') +
  scale_color_discrete(breaks=c("3", "5", "7", "9", "11", "13", "15", "17", "19", "21", "23", "25", "27", "29", "31", "33", "35", "37", "39", "41","43", "45", "47", "49")) +
  ylab('Povprečna uspešnost aproksimacije') +
  scale_y_continuous(breaks = 5*0:75) +
  scale_x_discrete(limits = c("3", "5", "7", "9", "11", "13", "15", "17", "19", "21", "23", "25", "27", "29", "31", "33", "35", "37", "39", "41","43", "45", "47", "49")) +
  ggtitle('Povprečne uspešnost aproksimacije ploščine druge različice 2. metode pri različnih \n  močeh množice S in pri različnih močeh podmnožice') +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))

tabela5 <- tabela5 %>%  filter(Relativna_napaka_ploscine_2._metode <= 0.1)
tabela5$k <-  as.character(tabela5$k) 
tabela5$Stevilo_vseh_tock_v_mnozici_S <-  as.character(tabela5$Stevilo_vseh_tock_v_mnozici_S)
tabela5 <- tabela5 %>% rename("Stevilo_tock_v_podmnozici" = k)

  
graf_5_boljsi  <- tabela5 %>% ggplot(aes(x=Stevilo_vseh_tock_v_mnozici_S, y=Relativna_napaka_ploscine_2._metode, palette="Pastel1", fill=Stevilo_tock_v_podmnozici)) + 
  geom_bar(position="dodge", stat = "identity" ) +
  xlab('Število vseh točk v množici S') +
  ylab('Relativna napaka aproksimacije') +
  #scale_y_continuous(breaks = 0.02*0:0.08) +
  scale_fill_discrete(breaks=c("3", "5", "7", "9", "11", "13", "15", "17", "19", "21", "23", "25", "27", "29", "31", "33", "35", "37", "39", "41","43", "45", "47", "49")) +
  scale_x_discrete(limits = c("51", "151", "201", "301","351", "401", "451")) +
  ggtitle("Napake druge različice 2. metode, ki so manjše od 0.1") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))

#------------
stolpci6_1000_tock <- c("ID", "Stevilo_vseh_tock_v_mnozici_S",  "Relativna_napaka_ploscine_2._metode", "Relativna_napaka_obsega_2._metode", "k")
tabela1000 <- read_table("files/rezultati_za_boljsi_krog_1001_tock_na_obmocju_10_10_pri_vzorcu_501.tsv", col_names = stolpci6_1000_tock, skip = 1,
                      locale=locale(encoding = "Windows-1250"))


tabela1000 <- tabela1000 %>%  filter(Relativna_napaka_ploscine_2._metode <= 0.1)  
tabela1000$k <-  as.character(tabela1000$k) 
tabela1000 <- tabela1000 %>% rename("Stevilo_tock_v_podmnozici" = k)
tabela1000$Stevilo_vseh_tock_v_mnozici_S <-  as.character(tabela1000$Stevilo_vseh_tock_v_mnozici_S)

graf_6_boljsi  <- tabela1000 %>% ggplot(aes(x=Stevilo_vseh_tock_v_mnozici_S, y=Relativna_napaka_ploscine_2._metode, palette="Pastel1", fill=Stevilo_tock_v_podmnozici)) + 
  geom_bar(position="dodge", stat = "identity" ) +
  xlab('Število vseh točk v množici S') +
  ylab('Relativna napaka aproksimacije') +
  #scale_y_continuous(breaks = 0.02*0:0.08) +
  scale_fill_discrete(breaks=c("3", "5", "7", "9", "11", "13", "15", "17", "19", "21", "23","25", "27", "29", "31", "33", "35", "37", "39", "41","43", "45", "47", "49")) +
  scale_x_discrete(limits = c("502", "552", "602", "652","702", "752", "802", "852", "902", "952")) +
  ggtitle("Napake druge različice 2. metode, ki so manjše od 0.1") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))

##-------------------------------------------------------------------

stolpci7 <- c("ID", "Stevilo_vseh_tock_v_množici_S", "Delez_izbranih_tock", "Uspesnost_izracunane_ploscine_1._metode", "Uspesnost_izracunanega_obsega_1._metode", "Relativna_napaka_ploscine_2._metode", "Relativna_napaka_obsega_2._metode", "m", "k")
tabela7 <- read_table("files/rezultati_primerjave_krog_boljse501_tock_na_obmocju_10_10_pri99_iteracijah_in_51_delitvah_obmocja_s50.tsv", col_names = stolpci7, skip = 1,
                      locale=locale(encoding = "Windows-1250"))

napake_za_1._m <- tabela7 %>% select(Stevilo_vseh_tock_v_množici_S, Delez_izbranih_tock, Uspesnost_izracunane_ploscine_1._metode, Uspesnost_izracunanega_obsega_1._metode) %>%
  mutate(Delez_izbranih_tock = round(Delez_izbranih_tock,0)) %>%
  mutate(Napaka_1._metode_pri_aproksimaciji_ploščine = 100 - Uspesnost_izracunane_ploscine_1._metode) %>%
  select(-Uspesnost_izracunane_ploscine_1._metode)%>%
  mutate(Napaka_1._metode_pri_aproksimaciji_obsega = 100 - Uspesnost_izracunanega_obsega_1._metode) %>%
  select(-c(Uspesnost_izracunanega_obsega_1._metode, Delez_izbranih_tock)) %>%
  pivot_longer(c(-Stevilo_vseh_tock_v_množici_S ),names_to = "Napaka_aproksimacije", values_to = "Vrednosti") %>%
  group_by(Stevilo_vseh_tock_v_množici_S, Napaka_aproksimacije) %>% summarize(Napake= mean(Vrednosti))
napake_za_1._m$Napaka_aproksimacije <- sub("Napaka_1._metode_pri_aproksimaciji_ploščine", "Napaka 1. metode pri aproksimaciji ploščine", napake_za_1._m$Napaka_aproksimacije )
napake_za_1._m$Napaka_aproksimacije <- sub("Napaka_1._metode_pri_aproksimaciji_obsega", "Napaka 1. metode pri aproksimaciji obsega", napake_za_1._m$Napaka_aproksimacije ) 

graf_napak_1.m <- napake_za_1._m %>% ggplot(aes(x=Stevilo_vseh_tock_v_množici_S, y=Napake, palette="Pastel1", col=Napaka_aproksimacije)) + 
  geom_line() + 
  geom_point()+
  xlab('Število vseh točk v množici S') + 
  ylab('Povprečna napaka aproksimacije') +
  scale_x_continuous(breaks = 50*0:1000) +
  scale_y_continuous(breaks = 0.5*0:20) +
  ggtitle('Povprečne napake aproksimacije 1. metode pri različnih velikostih vzorca') +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))
#--

tabela_napak_k <- tabela7 %>% select(c(Stevilo_vseh_tock_v_množici_S, Relativna_napaka_ploscine_2._metode, Relativna_napaka_obsega_2._metode, k))

napake_za_2._m <- tabela_napak_k %>% 
  mutate(Napaka_2._metode_pri_aprokismaciji_ploščine = Relativna_napaka_ploscine_2._metode) %>%
  mutate(Napaka_2._metode_pri_aprokismaciji_obsega = Relativna_napaka_obsega_2._metode) %>%
  select(-c(Relativna_napaka_obsega_2._metode, Relativna_napaka_ploscine_2._metode)) %>%
  pivot_longer(c(-Stevilo_vseh_tock_v_množici_S, -k),names_to = "Napaka_aproksimacije_2._metode", values_to = "Vrednosti_2._metode") %>%
  group_by(Stevilo_vseh_tock_v_množici_S, Napaka_aproksimacije_2._metode) %>% summarize(Napake_2._metode= mean(Vrednosti_2._metode))

napake_za_2._m$Napaka_aproksimacije_2._metode <- sub("Napaka_2._metode_pri_aprokismaciji_ploščine", "Napaka 2. metode pri aproksimaciji ploščine", napake_za_2._m$Napaka_aproksimacije_2._metode)
napake_za_2._m$Napaka_aproksimacije_2._metode <- sub("Napaka_2._metode_pri_aprokismaciji_obsega", "Napaka 2. metode pri aproksimaciji obsega", napake_za_2._m$Napaka_aproksimacije_2._metode) 

graf_napak_2.m <- napake_za_2._m%>% ggplot(aes(x=Stevilo_vseh_tock_v_množici_S, y=Napake_2._metode, palette="Pastel1", col=Napaka_aproksimacije_2._metode)) + 
  geom_line() + 
  geom_point()+
  xlab('Število vseh točk v množici S') + 
  ylab('Povprečna napaka aproksimacije') +
  scale_x_continuous(breaks = 50*0:1000) +
  scale_y_continuous(breaks = 5*0:75) +
  ggtitle('Povprečne napake aproksimacije 2. metode pri različnih velikostih vzorca') +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))
