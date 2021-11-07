# ISKANJE KONVEKSNE LUPINE MNOŽICE P Z n točkami

import random
import math

#razred, ki definira točko

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
 
def konveksna_lupina(tocke, n):
     
    if n < 3:
        return
 
    izbrana = Najbolj_leva(tocke)
 
    lupina = []
     
    '''
    Konveksno lupino gradi tako, da začne na najbolj levo točko in se premika v smer urinega kazalca, dokler ne doseže
    začetne točke.

    '''

    #Nastavimo točko p na najbolj levo in q na 0

    p = izbrana
    q = 0

    while(True):
         
        lupina.append(p)
 
        '''
        Poiščite točko 'q', ki je takšna, da je orientacija(p, q,
        x) za vse točke 'x' v smeri urinega kazalca. Ideja
        je slediti zadnjemu obisku, ki je bil največkrat v smeri urinega kazalca.
        Če je katera koli točka 'i' bolj proti smeri urinega kazalca
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
    print(seznam_tock)
    return(seznam_tock)
 
# Vir : https://www.geeksforgeeks.org/convex-hull-set-1-jarviss-algorithm-or-wrapping/

#Uporaba funkcije konveksna_lupina na množici točk, ki ima poljubno moč in točke
# Pri izbiri točk se omejiva na interval [a,b]


def nakljucna_mnozica(qty):
    rangeX = (-10, 10)
    rangeY = (-10, 10)
    randPoints = []
    i = 0
    while i<qty:
        x = random.randrange(*rangeX)
        y = random.randrange(*rangeY)
        randPoints.append(Tocka(x,y))
        i += 1

    return(randPoints)

mnozica = nakljucna_mnozica(3)

kon_lup = konveksna_lupina(mnozica, len(mnozica))


#### PLOŠČINA

def ploscina(seznam_tock):
    
    ploscina = 0

    for i in range(0, len(seznam_tock)):
        if i == len(seznam_tock)-1:
            ploscina += seznam_tock[i][0]*seznam_tock[0][1] - seznam_tock[0][0]*seznam_tock[i][1]
        else:
            ploscina += seznam_tock[i][0]*seznam_tock[i+1][1] - seznam_tock[i+1][0]*seznam_tock[i][1]
    print(0.5*abs(ploscina))
    return(ploscina)



#### OBSEG

def obseg(seznam_tock):

    obseg = 0    

    for i in range(0,len(seznam_tock)):
        if i == len(seznam_tock)-1:
            obseg += math.sqrt((seznam_tock[i][0]-seznam_tock[0][0])**2 + (seznam_tock[i][1]-seznam_tock[0][1])**2)
        else:
            obseg += math.sqrt((seznam_tock[i][0]-seznam_tock[i+1][0])**2 + (seznam_tock[i][1]-seznam_tock[i+1][1])**2)
    print(obseg)
    return(obseg)

