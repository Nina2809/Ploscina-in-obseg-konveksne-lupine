
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

tabela_uspesnosti_vec_kot_90 <- tabela1 %>% select(-ID)%>% 
                                filter(Uspešnost_izracunane_ploscine_1._metode >= 90)

povprecje_deleza_tock <- mean(tabela_uspesnosti_vec_kot_90$Delez_izbranih_tock)

# vzorec 50 tock:

vzorec_50_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 50) 
                                            
povprecno_stevilo_deleza_tock_v_vzorcu_50_tock <- mean(vzorec_50_tock$Delez_izbranih_tock)

# vzorec 100 tock:

vzorec_100_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 100) 

povprecno_stevilo_deleza_tock_v_vzorcu_100_tock <- mean(vzorec_100_tock$Delez_izbranih_tock)

# vzorec 150 tock:

vzorec_150_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 150) 

povprecno_stevilo_deleza_tock_v_vzorcu_150_tock <- mean(vzorec_150_tock$Delez_izbranih_tock)

# vzorec 200 tock:

vzorec_200_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 200) 

povprecno_stevilo_deleza_tock_v_vzorcu_200_tock <- mean(vzorec_200_tock$Delez_izbranih_tock)

# vzorec 250 tock:

vzorec_250_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 250) 

povprecno_stevilo_deleza_tock_v_vzorcu_250_tock <- mean(vzorec_250_tock$Delez_izbranih_tock)

# vzorec 300 tock:

vzorec_300_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 300) 

povprecno_stevilo_deleza_tock_v_vzorcu_300_tock <- mean(vzorec_300_tock$Delez_izbranih_tock)

# vzorec 350 tock:
vzorec_350_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 350) 

povprecno_stevilo_deleza_tock_v_vzorcu_350_tock <- mean(vzorec_350_tock$Delez_izbranih_tock)

# vzorec 400 tock:
vzorec_400_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 400) 

povprecno_stevilo_deleza_tock_v_vzorcu_400_tock <- mean(vzorec_400_tock$Delez_izbranih_tock)

# vzorec 450 tock:
vzorec_450_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 450) 

povprecno_stevilo_deleza_tock_v_vzorcu_450_tock <- mean(vzorec_450_tock$Delez_izbranih_tock)

# vzorec 500 tock:
vzorec_500_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 500) 

povprecno_stevilo_deleza_tock_v_vzorcu_500_tock <- mean(vzorec_500_tock$Delez_izbranih_tock)

# vzorec 550 tock:
vzorec_550_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 550) 

povprecno_stevilo_deleza_tock_v_vzorcu_550_tock <- mean(vzorec_550_tock$Delez_izbranih_tock)

# vzorec 600 tock:

vzorec_600_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 600) 

povprecno_stevilo_deleza_tock_v_vzorcu_600_tock <- mean(vzorec_600_tock$Delez_izbranih_tock)

# vzorec 650 tock:

vzorec_650_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 650) 

povprecno_stevilo_deleza_tock_v_vzorcu_650_tock <- mean(vzorec_650_tock$Delez_izbranih_tock)

# vzorec 700 tock:

vzorec_700_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 700) 

povprecno_stevilo_deleza_tock_v_vzorcu_700_tock <- mean(vzorec_700_tock$Delez_izbranih_tock)

# vzorec 750 tock:

vzorec_750_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 750) 

povprecno_stevilo_deleza_tock_v_vzorcu_750_tock <- mean(vzorec_750_tock$Delez_izbranih_tock)

# vzorec 800 tock:

vzorec_800_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 800) 

povprecno_stevilo_deleza_tock_v_vzorcu_800_tock <- mean(vzorec_800_tock$Delez_izbranih_tock)

# vzorec 850 tock:

vzorec_850_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 850) 

povprecno_stevilo_deleza_tock_v_vzorcu_850_tock <- mean(vzorec_850_tock$Delez_izbranih_tock)

# vzorec 900 tock:
vzorec_900_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 900) 

povprecno_stevilo_deleza_tock_v_vzorcu_900_tock <- mean(vzorec_900_tock$Delez_izbranih_tock)

# vzorec 950 tock:
vzorec_950_tock <- tabela_uspesnosti_vec_kot_90 %>% filter(Stevilo_vseh_tock_v_množici_S == 950) 

povprecno_stevilo_deleza_tock_v_vzorcu_950_tock <- mean(vzorec_950_tock$Delez_izbranih_tock)


povprecja <- data.frame(Stevilo_vseh_tock = c(50,100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600,650, 700, 750, 800, 850, 900, 950), 
                        Povprecno_stevilo_deleza_tock = c(povprecno_stevilo_deleza_tock_v_vzorcu_50_tock, povprecno_stevilo_deleza_tock_v_vzorcu_100_tock, povprecno_stevilo_deleza_tock_v_vzorcu_150_tock,
povprecno_stevilo_deleza_tock_v_vzorcu_200_tock, povprecno_stevilo_deleza_tock_v_vzorcu_250_tock, povprecno_stevilo_deleza_tock_v_vzorcu_300_tock, povprecno_stevilo_deleza_tock_v_vzorcu_350_tock, povprecno_stevilo_deleza_tock_v_vzorcu_400_tock,
povprecno_stevilo_deleza_tock_v_vzorcu_450_tock, povprecno_stevilo_deleza_tock_v_vzorcu_500_tock, povprecno_stevilo_deleza_tock_v_vzorcu_550_tock, povprecno_stevilo_deleza_tock_v_vzorcu_600_tock, povprecno_stevilo_deleza_tock_v_vzorcu_650_tock, povprecno_stevilo_deleza_tock_v_vzorcu_700_tock, povprecno_stevilo_deleza_tock_v_vzorcu_750_tock,
povprecno_stevilo_deleza_tock_v_vzorcu_800_tock, povprecno_stevilo_deleza_tock_v_vzorcu_850_tock, povprecno_stevilo_deleza_tock_v_vzorcu_900_tock, povprecno_stevilo_deleza_tock_v_vzorcu_950_tock))

graf_povprecij <- ggplot(data=povprecja, aes(x=Stevilo_vseh_tock, y=Povprecno_stevilo_deleza_tock)) + 
  geom_point(color=rgb(0.8,0.4,0.1,0.7)) + 
  geom_line(color=rgb(0.8,0.4,0.1,0.7)) + 
  xlab('Število vseh točk v množici S') + 
  ylab('Povprečno število deleža izbranih točk') +
  ggtitle('Primerjava povprečnega deleža izbranih točk \n za uspešnost več kot 90%, pri različnih močeh množice S') +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))

# izbira podatkov, kjer je uspešnost več kot 99%

tabela_uspesnosti_vec_kot_99 <- tabela1 %>% select(-ID)%>% 
  filter(Uspešnost_izracunane_ploscine_1._metode >= 99)

povprecje_deleza_tock_pri_99_uspešnosti <- mean(tabela_uspesnosti_vec_kot_99$Delez_izbranih_tock)

# vzorec 50 tock:

vzorec_50_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 50) 

povprecno_stevilo_deleza_tock_v_vzorcu_50_tock_2 <- mean(vzorec_50_tock$Delez_izbranih_tock)

# vzorec 100 tock:

vzorec_100_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 100) 

povprecno_stevilo_deleza_tock_v_vzorcu_100_tock_2 <- mean(vzorec_100_tock$Delez_izbranih_tock)

# vzorec 150 tock:

vzorec_150_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 150) 

povprecno_stevilo_deleza_tock_v_vzorcu_150_tock_2 <- mean(vzorec_150_tock$Delez_izbranih_tock)

# vzorec 200 tock:

vzorec_200_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 200) 

povprecno_stevilo_deleza_tock_v_vzorcu_200_tock_2 <- mean(vzorec_200_tock$Delez_izbranih_tock)

# vzorec 250 tock:

vzorec_250_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 250) 

povprecno_stevilo_deleza_tock_v_vzorcu_250_tock_2 <- mean(vzorec_250_tock$Delez_izbranih_tock)

# vzorec 300 tock:

vzorec_300_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 300) 

povprecno_stevilo_deleza_tock_v_vzorcu_300_tock_2 <- mean(vzorec_300_tock$Delez_izbranih_tock)

# vzorec 350 tock:
vzorec_350_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 350) 

povprecno_stevilo_deleza_tock_v_vzorcu_350_tock_2 <- mean(vzorec_350_tock$Delez_izbranih_tock)

# vzorec 400 tock:
vzorec_400_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 400) 

povprecno_stevilo_deleza_tock_v_vzorcu_400_tock_2 <- mean(vzorec_400_tock$Delez_izbranih_tock)
# vzorec 450 tock:
vzorec_450_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 450) 

povprecno_stevilo_deleza_tock_v_vzorcu_450_tock_2 <- mean(vzorec_450_tock$Delez_izbranih_tock)

# vzorec 500 tock:
vzorec_500_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 500) 

povprecno_stevilo_deleza_tock_v_vzorcu_500_tock_2 <- mean(vzorec_500_tock$Delez_izbranih_tock)

# vzorec 550 tock:
vzorec_550_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 550) 

povprecno_stevilo_deleza_tock_v_vzorcu_550_tock_2 <- mean(vzorec_550_tock$Delez_izbranih_tock)

# vzorec 600 tock:

vzorec_600_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 600) 

povprecno_stevilo_deleza_tock_v_vzorcu_600_tock_2 <- mean(vzorec_600_tock$Delez_izbranih_tock)

# vzorec 650 tock:

vzorec_650_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 650) 

povprecno_stevilo_deleza_tock_v_vzorcu_650_tock_2 <- mean(vzorec_650_tock$Delez_izbranih_tock)

# vzorec 700 tock:

vzorec_700_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 700) 

povprecno_stevilo_deleza_tock_v_vzorcu_700_tock_2 <- mean(vzorec_700_tock$Delez_izbranih_tock)

# vzorec 750 tock:

vzorec_750_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 750) 

povprecno_stevilo_deleza_tock_v_vzorcu_750_tock_2 <- mean(vzorec_750_tock$Delez_izbranih_tock)

# vzorec 800 tock:

vzorec_800_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 800) 

povprecno_stevilo_deleza_tock_v_vzorcu_800_tock_2 <- mean(vzorec_800_tock$Delez_izbranih_tock)

# vzorec 850 tock:

vzorec_850_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 850) 

povprecno_stevilo_deleza_tock_v_vzorcu_850_tock_2 <- mean(vzorec_850_tock$Delez_izbranih_tock)

# vzorec 900 tock:
vzorec_900_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 900) 

povprecno_stevilo_deleza_tock_v_vzorcu_900_tock_2 <- mean(vzorec_900_tock$Delez_izbranih_tock)

# vzorec 950 tock:
vzorec_950_tock <- tabela_uspesnosti_vec_kot_99 %>% filter(Stevilo_vseh_tock_v_množici_S == 950) 

povprecno_stevilo_deleza_tock_v_vzorcu_950_tock_2 <- mean(vzorec_950_tock$Delez_izbranih_tock)


povprecja_2 <- data.frame(Stevilo_vseh_tock = c(50,100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600,650, 700, 750, 800, 850, 900, 950), 
                        Povprecno_stevilo_deleza_tock_2 = c(povprecno_stevilo_deleza_tock_v_vzorcu_50_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_100_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_150_tock_2,
                                                          povprecno_stevilo_deleza_tock_v_vzorcu_200_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_250_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_300_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_350_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_400_tock_2,povprecno_stevilo_deleza_tock_v_vzorcu_450_tock_2,
                                                          povprecno_stevilo_deleza_tock_v_vzorcu_500_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_550_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_600_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_650_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_700_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_750_tock_2,
                                                          povprecno_stevilo_deleza_tock_v_vzorcu_800_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_850_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_900_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_950_tock_2))

graf_povprecij_2 <- ggplot(data=povprecja_2, aes(x=Stevilo_vseh_tock, y=Povprecno_stevilo_deleza_tock_2)) + 
  geom_point(color=rgb(0.8,0.4,0.1,0.7)) + 
  geom_line(color=rgb(0.8,0.4,0.1,0.7)) + 
  xlab('Število vseh točk v množici S') + 
  ylab('Povprečno število deleža izbranih točk') +
  ggtitle('Primerjava povprečnega deleža izbranih točk \n za 99% natančnost metode') +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))

skupna_tabela <- inner_join(povprecja, povprecja_2, by="Stevilo_vseh_tock") %>% pivot_longer(-c(Stevilo_vseh_tock), names_to = "Povprečni_delež_točk", values_to = "Vrednosti")
skupna_tabela$Povprečni_delež_točk <- sub("Povprecno_stevilo_deleza_tock_2", "Povprečni delež točk v vzorcu za 99% natančnost", skupna_tabela$Povprečni_delež_točk)
skupna_tabela$Povprečni_delež_točk <- sub("Povprecno_stevilo_deleza_tock", "Povprečni delež točk v vzorcu za 90% natančnost", skupna_tabela$Povprečni_delež_točk)

#graf za število točk v vzorcu za različne natančnosti

graf_različnih_deležev <- skupna_tabela %>% ggplot(aes(x=Stevilo_vseh_tock, y=Vrednosti, palette="Pastel1", col=Povprečni_delež_točk)) + 
  geom_point()+
  geom_line() +
  xlab('Število vseh točk v množici S') + 
  ylab('Povprečni delež točk v vzorcu') +
  ggtitle('Primerjava povprečnega deleža izbranih točk \n za 90% in 99% natančnost metode') +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))

# napake kroga

tabela_napak_kroga <- tabela1 %>% select(c(Stevilo_vseh_tock_v_množici_S, Relativna_napaka_ploscine_2._metode, Relativna_napaka_obsega_2._metode))

povprecna_napaka <- mean(tabela_napak_kroga$Relativna_napaka_ploscine_2._metode)
povprecna_napaka_obseg <- mean(tabela_napak_kroga$Relativna_napaka_obsega_2._metode)

#povprecne napake, glede na velikost vzorca

vzorec_50_krog <- tabela_napak_kroga %>% filter(Stevilo_vseh_tock_v_množici_S == 50)
povprecna_napaka_vzorca_50 <- mean(vzorec_50_krog$Relativna_napaka_ploscine_2._metode)

vzorec_100_krog <- tabela_napak_kroga %>% filter(Stevilo_vseh_tock_v_množici_S == 100)
povprecna_napaka_vzorca_100 <- mean(vzorec_100_krog$Relativna_napaka_ploscine_2._metode)

vzorec_150_krog <- tabela_napak_kroga %>% filter(Stevilo_vseh_tock_v_množici_S == 150)
povprecna_napaka_vzorca_150 <- mean(vzorec_150_krog$Relativna_napaka_ploscine_2._metode)

vzorec_200_krog <- tabela_napak_kroga %>% filter(Stevilo_vseh_tock_v_množici_S == 200)
povprecna_napaka_vzorca_200 <- mean(vzorec_200_krog$Relativna_napaka_ploscine_2._metode)

vzorec_250_krog <- tabela_napak_kroga %>% filter(Stevilo_vseh_tock_v_množici_S == 250)
povprecna_napaka_vzorca_250 <- mean(vzorec_250_krog$Relativna_napaka_ploscine_2._metode)

vzorec_300_krog <- tabela_napak_kroga %>% filter(Stevilo_vseh_tock_v_množici_S == 300)
povprecna_napaka_vzorca_300 <- mean(vzorec_300_krog$Relativna_napaka_ploscine_2._metode)

vzorec_350_krog <- tabela_napak_kroga %>% filter(Stevilo_vseh_tock_v_množici_S == 350)
povprecna_napaka_vzorca_350 <- mean(vzorec_350_krog$Relativna_napaka_ploscine_2._metode)

vzorec_400_krog <- tabela_napak_kroga %>% filter(Stevilo_vseh_tock_v_množici_S == 400)
povprecna_napaka_vzorca_400 <- mean(vzorec_400_krog$Relativna_napaka_ploscine_2._metode)

vzorec_450_krog <- tabela_napak_kroga %>% filter(Stevilo_vseh_tock_v_množici_S == 450)
povprecna_napaka_vzorca_450 <- mean(vzorec_450_krog$Relativna_napaka_ploscine_2._metode)

vzorec_500_krog <- tabela_napak_kroga %>% filter(Stevilo_vseh_tock_v_množici_S == 500)
povprecna_napaka_vzorca_500 <- mean(vzorec_500_krog$Relativna_napaka_ploscine_2._metode)

vzorec_550_krog <- tabela_napak_kroga %>% filter(Stevilo_vseh_tock_v_množici_S == 550)
povprecna_napaka_vzorca_550 <- mean(vzorec_550_krog$Relativna_napaka_ploscine_2._metode)

vzorec_600_krog <- tabela_napak_kroga %>% filter(Stevilo_vseh_tock_v_množici_S == 600)
povprecna_napaka_vzorca_600 <- mean(vzorec_600_krog$Relativna_napaka_ploscine_2._metode)

vzorec_650_krog <- tabela_napak_kroga %>% filter(Stevilo_vseh_tock_v_množici_S == 650)
povprecna_napaka_vzorca_650 <- mean(vzorec_650_krog$Relativna_napaka_ploscine_2._metode)

vzorec_700_krog <- tabela_napak_kroga %>% filter(Stevilo_vseh_tock_v_množici_S == 700)
povprecna_napaka_vzorca_700 <- mean(vzorec_700_krog$Relativna_napaka_ploscine_2._metode)

napake_kroga_glede_na_velikost_vzorca <- data.frame(Stevilo_vseh_tock = c("50","100", "150", "200", "250", "300", "350", "400","450", "500", "550", "600","650", "700"), Povprecna_napaka_ploscine = c(povprecna_napaka_vzorca_50,
 povprecna_napaka_vzorca_100, povprecna_napaka_vzorca_150, povprecna_napaka_vzorca_200, povprecna_napaka_vzorca_250, povprecna_napaka_vzorca_300, povprecna_napaka_vzorca_350, povprecna_napaka_vzorca_400, povprecna_napaka_vzorca_450, povprecna_napaka_vzorca_500,
 povprecna_napaka_vzorca_550, povprecna_napaka_vzorca_600, povprecna_napaka_vzorca_650, povprecna_napaka_vzorca_700))                           

graf_napak_ploščine <- ggplot(data=napake_kroga_glede_na_velikost_vzorca, aes(x=Stevilo_vseh_tock, y=Povprecna_napaka_ploscine)) + 
  geom_point(color=rgb(0.8,0.4,0.1,0.7)) + 
  geom_line(color=rgb(0.8,0.4,0.1,0.7)) + 
  geom_path() +
  xlab('Število vseh točk v množici S') + 
  ylab('Povprečna napaka aproksimirane ploščine') +
  ggtitle('Povprečne napake pri različnih velikostih vzorca') +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))

#primerjava povprečne napake pri 1 in 2. metodi pri istem številu točk

#Napaka 1. metode

napake_za_1._metodo <- tabela1 %>% select(Stevilo_vseh_tock_v_množici_S, Delez_izbranih_tock, Uspešnost_izracunane_ploscine_1._metode, Uspešnost_izracunanega_obsega_1._metode) %>%
                                  mutate(Delez_izbranih_tock = round(Delez_izbranih_tock,0)) %>%
                                  mutate(Napaka_1._metode_pri_aproksimaciji_ploščine = round(100 - Uspešnost_izracunane_ploscine_1._metode,2)) %>%
                                  select(-Uspešnost_izracunane_ploscine_1._metode)%>%
                                  mutate(Napaka_1._metode_pri_aproksimaciji_obsega = round(100 - Uspešnost_izracunanega_obsega_1._metode,2)) %>%
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
  mutate(Napaka_2._metode_pri_aprokismaciji_ploščine = round(Relativna_napaka_ploscine_2._metode, 2 )) %>%
    mutate(Napaka_2._metode_pri_aprokismaciji_obsega = round(Relativna_napaka_obsega_2._metode, 2 )) %>%
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

