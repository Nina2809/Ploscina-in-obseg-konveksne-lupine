

# ISKANJE KONVEKSNE LUPINE MNOŽICE P Z n točkami

import random
import math
from numpy import mat
import pandas as pd
import csv
from tqdm import tqdm

class Tocka:
    def __init__(self, x, y):
        self.x = x
        self.y = y
 
def Najbolj_leva(tocke):

    '''
    funckija, ki v primeru, da imamo več najnižjih točk poišče najbolj levo od vseh
    '''
    minn = 0
    for i in range(1,len(tocke)):
        if tocke[i].x < tocke[minn].x:
            minn = i
        elif tocke[i].x == tocke[minn].x:
            if tocke[i].y < tocke[minn].y:
                minn = i
    return minn
 
def orientacija(p, q, r):
    '''
    Iskanje orientacije urejenega trojčka (p, q, r).
    Funkcija vrne naslednje vrednosti
    0 --> p, q in r so kolinearni
    1 --> orientacija je v smeri urinega kazalca
    2 --> orientacija je v nasprotni smeri urinega kazalca
    '''
    vrednost = (q.y - p.y) * (r.x - q.x) - \
          (q.x - p.x) * (r.y - q.y)
 
    if vrednost == 0:
        return 0
    elif vrednost > 0:
        return 1
    else:
        return 2

# ISKANJE KONVEKSNE LUPINE 

def konveksna_lupina(tocke, n):
     
    if n < 3:
        return
    izbrana = Najbolj_leva(tocke)
    lupina = []

    #Nastavimo točko p na najbolj levo in q na 0

    p = izbrana
    q = 0

    while(True):
         
        lupina.append(p)
 
        '''
        Poiščite točko 'q', ki je takšna, da je orientacija(p, q,
        x) za vse točke 'x' v smeri urinega kazalca. Ideja
        je slediti zadnjemu obisku, ki je bil največkrat v smeri urinega kazalca.
        v kolikor je katera koli točka 'i' bolj proti smeri urinega kazalca
        kot q, potem posodobi q.
        '''
        q = (p + 1) % n
 
        for i in range(n):
             
            if(orientacija(tocke[p],
                           tocke[i], tocke[q]) == 2):
                q = i
        '''
        Sedaj je q najbolj v smeri urinega kazalca glede na p
        Za naslednjo iteracijo nastavite p kot q, tako da se q doda k
        rezultatu 'lupina' --> to bomo ponavljali dokler ne pridemo do začetne točke
        '''
        p = q

        if(p == izbrana):
            break
    
    seznam_tock = []

    for vsako_tocko in lupina:

        seznam_tock.append([tocke[vsako_tocko].x, tocke[vsako_tocko].y])
    
    return(seznam_tock)
 
# Vir : https://www.geeksforgeeks.org/convex-hull-set-1-jarviss-algorithm-or-wrapping/

#Uporaba funkcije konveksna_lupina na množici točk, ki ima poljubno moč in točke
# Pri izbiri točk se omejiva na interval [a,b]

def nakljucna_mnozica(qty,a,b):
    rangeX = (0, a)
    rangeY = (0, b)
    randPoints = []
    i = 0
    while i<qty:
        x = round(random.uniform(*rangeX),4)
        y = round(random.uniform(*rangeY),4)
        randPoints.append(Tocka(x,y))
        i += 1

    return(randPoints)

#mnozica = nakljucna_mnozica(3)

#kon_lup = konveksna_lupina(mnozica, len(mnozica))


#### PLOŠČINA

def ploscina(seznam_tock):
    
    ploscina = 0

    for i in range(0, len(seznam_tock)):
        if i == len(seznam_tock)-1:
            ploscina += seznam_tock[i][0]*seznam_tock[0][1] - seznam_tock[0][0]*seznam_tock[i][1]
        else:
            ploscina += seznam_tock[i][0]*seznam_tock[i+1][1] - seznam_tock[i+1][0]*seznam_tock[i][1]
    
    return(0.5*abs(ploscina))

#### OBSEG

def obseg(seznam_tock):

    obseg = 0    

    for i in range(0,len(seznam_tock)):
        if i == len(seznam_tock)-1:
            obseg += math.sqrt((seznam_tock[i][0]-seznam_tock[0][0])**2 + (seznam_tock[i][1]-seznam_tock[0][1])**2)
        else:
            obseg += math.sqrt((seznam_tock[i][0]-seznam_tock[i+1][0])**2 + (seznam_tock[i][1]-seznam_tock[i+1][1])**2)
    
    return(obseg)

# 1. METODA ZA APROKSIMACIJO PLOŠČINE IN OBSEGA

def razdeli_pravokotnik(a,b,m):
    seznam_pravokotnikov = []
    dolzina = a/m
    visina = b/m
    for i in range(0,m):
        for j in range(0,m):
            A = Tocka(j*dolzina, i*visina)
            B = Tocka((j+1)*dolzina, i*visina)
            C = Tocka((j+1)*dolzina, (i+1)*visina)
            D = Tocka(j*dolzina, (i+1)*visina)
            seznam_pravokotnikov.append([[A.x,A.y],[B.x,B.y],[C.x,C.y],[D.x,D.y]])
    
    return(seznam_pravokotnikov)

def razdeli_pravokotnik2(a,b,m):
    seznam_pravokotnikov = []
    dolzina = a/m
    visina = b/m
    for i in range(0,m):
        for j in range(0,m):
            x1 = i*dolzina
            x2 = (i+1)*dolzina
            y1 = j*visina
            y2 = (j+1)*visina
            seznam_pravokotnikov.append([x1,x2,y1,y2])
#    print(seznam_pravokotnikov)
    return(seznam_pravokotnikov)   

#razdeli_pravokotnik2(4,4,4)

def izberi_tocke(a,b,m,mnozica):
    pravokotniki = razdeli_pravokotnik2(a,b,m)
    izbrane = []
    for pravokotnik in pravokotniki:
        vsebovane = []
        for tocka in mnozica:
            if pravokotnik[0] <= tocka.x <= pravokotnik[1] and pravokotnik[2] <= tocka.y <= pravokotnik[3]:
                vsebovane.append(tocka)
            else:
                pass
        if len(vsebovane) == 0:
            pass
        else:
            u = len(vsebovane)
            izbrane.append(vsebovane[random.randint(0,u-1)])
    return(izbrane)

mnozica = nakljucna_mnozica(20,4,4)
izberi_tocke(4,4,4,mnozica)

def primerjava(a,b,m,st_vseh):
    mnozica = nakljucna_mnozica(st_vseh,a,b)
    kon_lup_eksaktna = konveksna_lupina(mnozica, len(mnozica))

    izbrane = izberi_tocke(a,b,m,mnozica)
    kon_lup_simulirana = konveksna_lupina(izbrane, len(izbrane))

    ploscina_eksaktna = ploscina(kon_lup_eksaktna)
    ploscina_simulirana = ploscina(kon_lup_simulirana)
    napaka_ploscina = abs(ploscina_eksaktna-ploscina_simulirana)
    relativna_napaka_ploscine = 100 - (abs(ploscina_simulirana-ploscina_eksaktna)/ ploscina_eksaktna)*100

    obseg_eksakten = obseg(kon_lup_eksaktna)
    obseg_simuliran = obseg(kon_lup_simulirana)
    napaka_obseg = abs(obseg_eksakten-obseg_simuliran)
    relativna_napaka_obseg = 100 - (abs(obseg_simuliran - obseg_eksakten)/obseg_eksakten)*100

    delez_izbranih_tock = (len(izbrane)/len(mnozica))*100


    rezultati = {'st_vseh': [], 'delez_izbranih_tock': [], 'relativna_napaka_ploscine': [], 'relativna_napaka_obseg' : [] }
    rezultati['st_vseh'] += [st_vseh]
    rezultati['delez_izbranih_tock'] += [delez_izbranih_tock]
    rezultati['relativna_napaka_ploscine'] += [relativna_napaka_ploscine]
    rezultati['relativna_napaka_obseg'] += [relativna_napaka_obseg]

    data = pd.DataFrame(rezultati)
    

    return(data)


primerjava(10,10,10,1000)

primerjava(5,5,10,400)

def generiraj_primere(a,b,m, st_vseh):
    # a in b sta največji vrednosti na x in y osi
    # m je število delitev intervala od 0 do a in od 0 do b
    # najvecje st_vseh elementov v množici S
    
    koncni_rezultati = [] 
    for r in tqdm(range(50, st_vseh, 10)):
        for n in range(2, m, 10):
            for j in range(0,10):
                koncni_rezultati += [primerjava(a,b,n,r)]
    zadnji_rezultati = pd.concat(koncni_rezultati, axis=0, ignore_index= True)
    zadnji_rezultati.index.name = 'ID'
    zadnji_rezultati.to_csv(f'files/rezultati_{st_vseh}_tock_pri_{m}_delitvah_na_obmocju_{a}_{b}.tsv')   


#2. METODA:

def tezisce_vzorca(mnozica):
    vsota_x = 0
    vsota_y = 0
    for i in mnozica:
        vsota_x += i.x
        vsota_y += i.y
    povprecje_x = vsota_x/len(mnozica)
    povprecje_y = vsota_y/len(mnozica)
    return([povprecje_x, povprecje_y])

def polmer(mnozica, tezisce_vzorca):
    razdalja = 0
    for i in mnozica:
        razdalja_tocke = math.sqrt((i.x - tezisce_vzorca[0])**2 + (i.y - tezisce_vzorca[1])**2)
        if razdalja_tocke >= razdalja:
            razdalja = razdalja_tocke
        else:
            pass
    return(razdalja)

def ploscina_kroga(polmer):
    ploscina = math.pi * polmer**2
    return(ploscina)

def obseg_kroga(polmer):
    obseg = 2 * math.pi * polmer
    return(obseg)

def primerjava_s_krogom(a,b,st_vseh):
    mnozica = nakljucna_mnozica(st_vseh,a,b)
    kon_lup_eksaktna = konveksna_lupina(mnozica, len(mnozica))
    ploscina_eksaktna = ploscina(kon_lup_eksaktna)
    obseg_eksakten = obseg(kon_lup_eksaktna)

    t = tezisce_vzorca(mnozica)
    r = polmer(mnozica, t)
    ploscina_simulirana_krog = ploscina_kroga(r)
    
    relativna_napaka_ploscine_krog = 100 - (abs(ploscina_simulirana_krog-ploscina_eksaktna)/ ploscina_eksaktna)*100

    
    obseg_simuliran_krog = obseg_kroga(r)
    relativna_napaka_obseg_krog = 100 - (abs(obseg_simuliran_krog - obseg_eksakten)/obseg_eksakten)*100

    
    rezultati = {'st_vseh': [],'relativna_napaka_ploscine_krog': [], 'relativna_napaka_obseg_krog' : [] }
    rezultati['st_vseh'] += [st_vseh]
    rezultati['relativna_napaka_ploscine_krog'] += [relativna_napaka_ploscine_krog]
    rezultati['relativna_napaka_obseg_krog'] += [relativna_napaka_obseg_krog]

    data = pd.DataFrame(rezultati)
    

    return(data)

def generiraj_primere_za_krog(a,b,st_vseh):
    # a in b sta največji vrednosti na x in y osi
    # najvecje st_vseh elementov v množici S
    
    koncni_rezultati_za_krog = [] 
    for r in tqdm(range(50, st_vseh, 10)):
        for j in range(0,10):
            koncni_rezultati_za_krog += [primerjava_s_krogom(a,b,r)]
    zadnji_rezultati = pd.concat(koncni_rezultati_za_krog, axis=0, ignore_index= True)
    zadnji_rezultati.index.name = 'ID'
    zadnji_rezultati.to_csv(f'files/rezultati_za_krog_{st_vseh}_tock_na_obmocju_{a}_{b}.tsv')   



def primerjava_prve_in_druge_metode(a,b,m,st_vseh):

    mnozica = nakljucna_mnozica(st_vseh,a,b)

    kon_lup_eksaktna = konveksna_lupina(mnozica, len(mnozica))
    ploscina_eksaktna = ploscina(kon_lup_eksaktna)
    obseg_eksakten = obseg(kon_lup_eksaktna)

    # rezultati metode 1

    izbrane = izberi_tocke(a,b,m,mnozica)
    kon_lup_simulirana = konveksna_lupina(izbrane, len(izbrane))
    ploscina_simulirana_1 = ploscina(kon_lup_simulirana)
    obseg_simuliran_1 = obseg(kon_lup_simulirana)

    # napake 1. metode

    napaka_ploscina_1 = abs(ploscina_eksaktna-ploscina_simulirana_1)
    relativna_napaka_ploscine_1 = (abs(ploscina_simulirana_1 -ploscina_eksaktna)/ ploscina_eksaktna)*100

    napaka_obseg_1 = abs(obseg_eksakten-obseg_simuliran_1)
    relativna_napaka_obseg_1 = (abs(obseg_simuliran_1 - obseg_eksakten)/obseg_eksakten)*100

    delez_izbranih_tock = (len(izbrane)/len(mnozica))*100

    # podatki za metodo 2

    t = tezisce_vzorca(mnozica)
    r = polmer(mnozica, t)

    ploscina_simulirana_krog = ploscina_kroga(r)
    relativna_napaka_ploscine_krog = (abs(ploscina_simulirana_krog-ploscina_eksaktna)/ ploscina_eksaktna)*100
     
    obseg_simuliran_krog = obseg_kroga(r)
    relativna_napaka_obseg_krog = (abs(obseg_simuliran_krog - obseg_eksakten)/obseg_eksakten)*100


    rezultati = {'st_vseh': [], 'delez_izbranih_tock': [], 'relativna_napaka_ploscine_1': [], 'relativna_napaka_obseg_1' : [], 'relativna_napaka_ploscine_krog': [], 'relativna_napaka_obseg_krog' : [] }
    rezultati['st_vseh'] += [st_vseh]
    rezultati['delez_izbranih_tock'] += [delez_izbranih_tock]
    rezultati['relativna_napaka_ploscine_1'] += [relativna_napaka_ploscine_1]
    rezultati['relativna_napaka_obseg_1'] += [relativna_napaka_obseg_1]
    rezultati['relativna_napaka_ploscine_krog'] += [relativna_napaka_ploscine_krog]
    rezultati['relativna_napaka_obseg_krog'] += [relativna_napaka_obseg_krog]

    data = pd.DataFrame(rezultati)
    print(data)
    return(data)

def generiraj_primere_za_primerjavo(a,b,m,st_vseh):
    koncni_rezultati_za_primerjavo = [] 
    for r in tqdm(range(50, st_vseh, 10)):
        for n in range(2, m, 10):
            for j in range(0,10):
                koncni_rezultati_za_primerjavo += [primerjava_prve_in_druge_metode(a,b,n,r)]
    zadnji_rezultati = pd.concat(koncni_rezultati_za_primerjavo, axis=0, ignore_index= True)
    zadnji_rezultati.index.name = 'ID'
    zadnji_rezultati.to_csv(f'files/rezultati_primerjave{st_vseh}_tock_na_obmocju_{a}_{b}.tsv')
     
if __name__ == '__main__':
    print('delam, delam, delam, delam kot zamorc')

    #a = generiraj_primere(10,10,10,100)
    c = generiraj_primere_za_primerjavo(10,10,4,70)      