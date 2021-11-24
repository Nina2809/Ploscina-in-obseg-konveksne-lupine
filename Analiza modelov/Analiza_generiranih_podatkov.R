
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


povprecja <- data.frame(Stevilo_vseh_tock = c("50","100", "150", "200", "250", "300", "350", "400","500", "550", "600","650", "700", "750", "800", "850", "900", "950"), 
                        Povprecno_stevilo_deleza_tock = c(povprecno_stevilo_deleza_tock_v_vzorcu_50_tock, povprecno_stevilo_deleza_tock_v_vzorcu_100_tock, povprecno_stevilo_deleza_tock_v_vzorcu_150_tock,
povprecno_stevilo_deleza_tock_v_vzorcu_200_tock, povprecno_stevilo_deleza_tock_v_vzorcu_250_tock, povprecno_stevilo_deleza_tock_v_vzorcu_300_tock, povprecno_stevilo_deleza_tock_v_vzorcu_350_tock, povprecno_stevilo_deleza_tock_v_vzorcu_400_tock,
povprecno_stevilo_deleza_tock_v_vzorcu_500_tock, povprecno_stevilo_deleza_tock_v_vzorcu_550_tock, povprecno_stevilo_deleza_tock_v_vzorcu_600_tock, povprecno_stevilo_deleza_tock_v_vzorcu_650_tock, povprecno_stevilo_deleza_tock_v_vzorcu_700_tock, povprecno_stevilo_deleza_tock_v_vzorcu_750_tock,
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


povprecja_2 <- data.frame(Stevilo_vseh_tock = c("50","100", "150", "200", "250", "300", "350", "400","500", "550", "600","650", "700", "750", "800", "850", "900", "950"), 
                        Povprecno_stevilo_deleza_tock_2 = c(povprecno_stevilo_deleza_tock_v_vzorcu_50_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_100_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_150_tock_2,
                                                          povprecno_stevilo_deleza_tock_v_vzorcu_200_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_250_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_300_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_350_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_400_tock_2,
                                                          povprecno_stevilo_deleza_tock_v_vzorcu_500_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_550_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_600_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_650_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_700_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_750_tock_2,
                                                          povprecno_stevilo_deleza_tock_v_vzorcu_800_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_850_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_900_tock_2, povprecno_stevilo_deleza_tock_v_vzorcu_950_tock_2))

graf_povprecij_2 <- ggplot(data=povprecja_2, aes(x=Stevilo_vseh_tock, y=Povprecno_stevilo_deleza_tock_2)) + 
  geom_point(color=rgb(0.8,0.4,0.1,0.7)) + 
  geom_line(color=rgb(0.8,0.4,0.1,0.7)) + 
  xlab('Število vseh točk v množici S') + 
  ylab('Povprečno število deleža izbranih točk') +
  ggtitle('Primerjava povprečnega deleža izbranih točk \n za uspešnost več kot 90%, pri različnih močeh množice S') +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5))

skupna_tabela <- inner_join(povprecja, povprecja_2, by="Stevilo_vseh_tock")




  