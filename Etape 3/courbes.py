import csv
import matplotlib.pyplot as plt
import numpy as np
file = "D:/MPET/TIPE/nvl_bdd/resultats_bb_1.csv" # A MODIFIER
file2 = "D:/MPET/TIPE/resultats_bb_1.csv" # A MODIFIER
tps_naif = []
colo_naif = []
tps_welsh_powel = []
colo_welsh_powel = []
tps_dsatur = []
colo_dsatur = []
tps_dsatur_file = []
colo_dsatur_file = []
tps_backtracking = []
colo_backtracking = []
taille_g = []
aretes_g = []

l_cat = [ (10,2 ) , ( 10,4 ) , ( 10,6 ) , ( 10,8 ) , (
    12,2 ) , ( 12,4 ) , ( 12,6 ) , ( 12,8 ) , (
    15,2 ) , ( 15,4 ) , ( 15,6 ) , ( 15,8 ) , ( 15,10 ) , ( 15,12 ) , (
    17,2 ) , ( 17,4 ) , ( 17,6 ) , ( 17,8 ) , ( 17,10 ) , ( 17,12 ) , ( 17,14 ) , (
    20,2 ) , ( 20,4 ) , ( 20,6 ) , ( 20,8 ) , ( 20,10 ) , ( 20,12 ) , ( 20,14 )]





def liste_categorie(s,a):
    c = rech_ind((s,a))
    l = [c + i for i in range(100)]
    return l

def moyenne(couple, atraiter):
    l = liste_categorie(s,a)
    s = 0
    for i in l:
        s = s + atraiter[i]
    return (s/(len(l)))

with open(file2, 'r', encoding='utf-8') as csvfile:
    reader = csv.reader(csvfile,delimiter=',')
    for row in reader:
        if row[0] != 'Taille':
#            tps_naif.append(float(row[0]))
#            colo_naif.append(int(row[1]))
#            tps_welsh_powel.append(float(row[2]))
#            colo_welsh_powel.append(int(row[3]))
#            tps_dsatur.append(float(row[4]))
#            colo_dsatur.append(int(row[5]))
#            tps_dsatur_file.append(float(row[6]))
#            colo_dsatur_file.append(int(row[7]))
            tps_backtracking.append(float(row[3]))
            colo_backtracking.append(int(row[4]))
#            taille_g.append(int(row[3]))
#            aretes_g.append(int(row[4]))



with open(file, 'r', encoding='utf-8') as csvfile2:
    reader2 = csv.reader(csvfile2,delimiter=',')
    for row in reader2:
        if row[0] != 'Taille':
            taille_g.append(int(row[0]))
            aretes_g.append(int(row[1]))
            tps_naif.append(float(row[3]))
            colo_naif.append(int(row[4]))
            tps_welsh_powel.append(float(row[5]))
            colo_welsh_powel.append(int(row[6]))
            tps_dsatur.append(float(row[7]))
            colo_dsatur.append(int(row[8]))
            tps_dsatur_file.append(float(row[9]))

            colo_dsatur_file.append(int(row[10]))
#            tps_backtracking.append(float(row[0]))
#            colo_backtracking.append(int(row[1]))




l_cat = [  (15,10) , (20,10) , (25,10) , (30,10) , (35,10) , (40,10) , (45,10) , (50,10) , (55,10) , (60,10) , (65,10) , (70,10) , (75,10) , (80,10) , (85,10) , (90,10) , (95,10) , (100,10) ]






dico_d = {}
for i in range(len(l_cat)):
    t,d = l_cat[i]
    if t <= 100 : #fin du tableau excell à prendre en compte, à changer à la main
        if not d in dico_d :
            dico_d[d] = [l_cat[i]]
        else :
            dico_d[d].append(l_cat[i])


dico_t = {}
for i in range(len(l_cat)):
    t,d = l_cat[i]
    if t <= 100 : #fin du tableau excell à prendre en compte, à changer à la main
        if not t in dico_t :
            dico_t[t] = [l_cat[i]]
        else :
            dico_t[t].append(l_cat[i])





def nom(fonc):
    if fonc == 'n' :
        return 'First-Fit'
    if fonc == 'wp' :
        return 'Welsh-Powell'
    if fonc == 'd' :
        return 'DSATUR'
    if fonc == 'df' :
        return 'DSATUR file'
    if fonc == 'b' :
        return 'Branch-and-bound'



def t(fonc):
    if fonc == 'n' :
        return tps_naif,colo_naif
    if fonc == 'wp' :
        return tps_welsh_powel,colo_welsh_powel
    if fonc == 'd' :
        return tps_dsatur,colo_dsatur
    if fonc == 'df' :
        return tps_dsatur_file,colo_dsatur_file
    if fonc == 'b' :
        return tps_backtracking,colo_backtracking

def deb_fin(c):
    i = rech_ind(c)
    d = i
    return d,d+1


def rech_ind(c):
    for i in range(len(l_cat)):
        if l_cat[i] == c :
            return i

def deb_fin_ok(c):
    i = rech_ind(c)
    d = i
    return d,d+1


def nb_opti(c,f1,f2,f3,f4,f5):
    tps1,colo1 = t(f1)
    tps2,colo2 = t(f2)
    tps3,colo3 = t(f3)
    tps4,colo4 = t(f4)
    tps5,colo5 = t(f5)
    n1 = 0
    n2 = 0
    n3 = 0
    n4 = 0
    d,f = deb_fin(c)
    for i in range(d,f):
        if colo1[i] == colo5[i]:
            n1 +=1
        if colo2[i] == colo5[i]:
            n2 +=1
        if colo3[i] == colo5[i]:
            n3 +=1
        if colo4[i] == colo5[i]:
            n4 +=1

    return n1,n2,n3,n4


def moy_nc(c,fonc):
    tps,colo = t(fonc)
    moy = 0
    d,f = deb_fin_ok(c)
    for i in range(d,f):
        moy = moy + colo[i]

    return moy


def moy_nc_t(c,fonc):
    tps,colo = t(fonc)
    moy = 0
    d,f = deb_fin_ok(c)
    for i in range(d,f):

        moy = moy + tps[i]

    return moy


def func(x, a, b, c):
    return a * np.exp(-b * np.log(x)) + c




##TEMPS à DENSITE fixée
def comp_tps_d(dens,f1,f2,f3,f4,f5):
    plt.close()
    t1 = []
    t2 = []
    t3 = []
    t4 = []
    t4 = []
    t5 = []
    lc = dico_d[dens]
    tm = []
    tailles = []

    for c in lc :
        t,d = c
        m1 = moy_nc_t(c,f1)
        m2 = moy_nc_t(c,f2)
        m3 = moy_nc_t(c,f3)
        m4 = moy_nc_t(c,f4)
        m5 = moy_nc_t(c,f5)
        t1.append(m1)
        t2.append(m2)
        t3.append(m3)
        t4.append(m4)
        t5.append(m5)
        tm.append(m1+m2+m3+m4+m5)

        tailles.append(t)
    tm2 = np.array(tm)/4
    a,b,c = np.polyfit(tailles,tm2,2)
    hyp = []
    for i in range(len(tailles)):
        hyp.append(a*(tailles[i]**2) + b*tailles[i] + c)
    plt.figure()

    plt.plot(tailles,t1,ls = '-', marker ='', color = 'blue', label = nom(f1))
    plt.plot(tailles,t2,ls = '-', marker ='', color = 'green', label = nom(f2))
    plt.plot(tailles,t3,ls = '-', marker ='', color = 'red', label = nom(f3))
    plt.plot(tailles,t4,ls = '-', marker ='', color = 'purple', label = nom(f4))


    plt.xlabel('Taille de graphe, degré moyen = '+ str(dens), fontsize = 30)
    plt.ylabel("Temps d'exécution", fontsize = 30)
    plt.xticks(fontsize = 30)
    plt.yticks(fontsize = 30)
    plt.legend(loc=2, prop={'size': 30})
    plt.show()








## TEMPS à TAILLE fixée
def comp_tps_t(ta,f1,f2,f3,f4,f5):
    plt.close()
    t1 = []
    t2 = []
    t3 = []
    t4 = []
    t5 = []
    lc = dico_t[ta]

    tailles = []

    for c in lc :
        t,d = c
        m1 = moy_nc_t(c,f1)
        m2 = moy_nc_t(c,f2)
        m3 = moy_nc_t(c,f3)
        m4 = moy_nc_t(c,f4)
        m5 = moy_nc_t(c,f5)
        t1.append(m1)
        t2.append(m2)
        t3.append(m3)
        t4.append(m4)
        t5.append(m5)

        tailles.append(d)

    plt.figure()
    plt.plot(tailles,t1,ls = '-', marker ='', color = 'blue', label = nom(f1))
    plt.plot(tailles,t2,ls = '-', marker ='', color = 'green', label = nom(f2))
    plt.plot(tailles,t3,ls = '-', marker ='', color = 'red', label = nom(f3))
    plt.plot(tailles,t4,ls = '-', marker ='', color = 'purple', label = nom(f4))

    plt.xlabel('Degré moyen, taille = '+ str(ta), fontsize = 30)
    plt.ylabel("Temps d'exécution", fontsize = 30)
    plt.xticks(fontsize = 30)
    plt.yticks(fontsize = 30)
    plt.legend(loc=2, prop={'size': 30})
    plt.show()








## NB COLO à DENSITE fixée
def comp_nc_d(dens,f1,f2,f3,f4,f5):
    plt.close()
    nc1 = []
    nc2 = []
    nc3 = []
    nc4 = []
    nc5 = []
    nc1sur3 = []
    nc2sur3 = []
    nc3sur3 = []
    nc4sur3 = []
    lc = dico_d[dens]

    tailles = []

    for c in lc :
        t,d = c
        m1 = moy_nc(c,f1)
        m2 = moy_nc(c,f2)
        m3 = moy_nc(c,f3)
        m4 = moy_nc(c,f4)
        m5 = moy_nc(c,f5)
        nc1.append(m1)
        nc2.append(m2)
        nc3.append(m3)
        nc4.append(m4)
        nc5.append(m5)
        nc1sur3.append(m1/m5)
        nc2sur3.append(m2/m5)
        nc4sur3.append(m4/m5)
        nc3sur3.append(m3/m5)
        tailles.append(t)

    plt.figure()
    plt.plot(tailles,nc1,ls = '-', marker ='', color = 'blue', label = nom(f1))
    plt.plot(tailles,nc2,ls = '-', marker ='', color = 'green', label = nom(f2))
    plt.plot(tailles,nc3,ls = '-', marker ='', color = 'red', label = nom(f3))
    plt.plot(tailles,nc4,ls = '-', marker ='', color = 'purple', label = nom(f4))
    plt.plot(tailles,nc5,ls = '-', marker ='', color = 'gold', label = nom(f5))
    plt.xlabel('Taille de graphe, degré moyen = '+ str(dens), fontsize = 30)
    plt.ylabel('Coloration moyenne', fontsize = 30)
    plt.xticks(fontsize = 30)
    plt.yticks(fontsize = 30)
    plt.legend(loc=2, prop={'size': 30})
    plt.show()


    plt.figure()
    plt.plot(tailles,[1]*len(tailles), color = 'lightgrey')
    plt.plot(tailles,nc1sur3,ls = '-', marker ='', color = 'blue', label = nom(f1) + ' / ' + nom(f5))
    plt.plot(tailles,nc2sur3,ls = '-', marker ='', color = 'green', label = nom(f2) + ' / ' + nom(f5))
    plt.plot(tailles,nc4sur3,ls = '-', marker ='', color = 'purple', label = nom(f4) + ' / ' + nom(f5))
    plt.plot(tailles,nc3sur3,ls = '-', marker ='', color = 'red', label = nom(f3) + ' / ' + nom(f5))
    plt.ylim([0.95, 1.45])
    plt.xlabel('Taille de graphe, degré moyen = '+ str(dens), fontsize = 30 )
    plt.ylabel('Nb coul / Nb coul ' + nom(f5), fontsize = 30)
    plt.xticks(fontsize = 30)
    plt.yticks(fontsize = 30)
    plt.legend(loc=2, prop={'size': 30})

    plt.show()







## NB COLO à TAILLE fixée
def comp_nc_t(ta,f1,f2,f3,f4,f5):
    plt.close()
    nc1 = []
    nc2 = []
    nc3 = []
    nc4 = []
    nc5 = []
    nc1sur3 = []
    nc2sur3 = []
    nc4sur3 = []
    nc3sur3 = []
    lc = dico_t[ta]

    tailles = []

    for c in lc :
        t,d = c
        m1 = moy_nc(c,f1)
        m2 = moy_nc(c,f2)
        m3 = moy_nc(c,f3)
        m4 = moy_nc(c,f4)
        m5 = moy_nc(c,f5)
        nc1.append(m1)
        nc2.append(m2)
        nc3.append(m3)
        nc4.append(m4)
        nc5.append(m5)
        nc1sur3.append(m1/m5)
        nc2sur3.append(m2/m5)
        nc4sur3.append(m4/m5)
        nc3sur3.append(m3/m5)
        tailles.append(d)

    plt.figure()
    plt.plot(tailles,nc1,ls = '-', marker ='', color = 'blue', label = nom(f1))
    plt.plot(tailles,nc2,ls = '-', marker ='', color = 'green', label = nom(f2))
    plt.plot(tailles,nc3,ls = '-', marker ='', color = 'red', label = nom(f3))
    plt.plot(tailles,nc4,ls = '-', marker ='', color = 'purple', label = nom(f4))
    plt.plot(tailles,nc5,ls = '-', marker ='', color = 'gold', label = nom(f5))
    plt.xlabel('Degré moyen, taille = '+ str(ta), fontsize = 30 )
    plt.ylabel('Coloration moyenne', fontsize = 30 )
    plt.xticks(fontsize = 30)
    plt.yticks(fontsize = 30)
    plt.legend(loc=2, prop={'size': 30})

    plt.show()




    plt.figure()
    plt.plot(tailles,[1]*len(tailles), color = 'lightgrey')
    plt.plot(tailles,nc1sur3,ls = '-', marker ='', color = 'blue', label = nom(f1) + ' / ' + nom(f5))
    plt.plot(tailles,nc2sur3,ls = '-', marker ='', color = 'green', label = nom(f2) + ' / ' + nom(f5))
    plt.plot(tailles,nc4sur3,ls = '-', marker ='', color = 'purple', label = nom(f4) + ' / ' + nom(f5))
    plt.plot(tailles,nc3sur3,ls = '-', marker ='', color = 'red', label = nom(f3) + ' / ' + nom(f5))
    plt.ylim([0.95, 1.45])
    plt.xlabel('Degré moyen, taille = '+ str(ta), fontsize = 30 )
    plt.ylabel('Nb coul / Nb coul ' + nom(f5) , fontsize = 30)
    plt.xticks(fontsize = 30)
    plt.yticks(fontsize = 30)
    plt.legend(loc=2, prop={'size': 30})

    plt.show()





## RATIO D'OPTIMALITE à TAILLE fixée
def ratio_opti_nc_t(ta,f1,f2,f3,f4,f5):
    plt.close()
    nc1 = []
    nc2 = []
    nc3 = []
    nc4 = []


    lc = dico_t[ta]

    tailles = []

    for c in lc :
        t,d = c
        m1,m2,m3,m4 = nb_opti(c,f1,f2,f3,f4,f5)
        nc1.append(m1)
        nc2.append(m2)
        nc3.append(m3)
        nc4.append(m4)


        tailles.append(d)

    plt.figure()
    plt.plot(tailles,nc1,ls = '-', marker ='', color = 'blue', label = nom(f1))
    plt.plot(tailles,nc2,ls = '-', marker ='', color = 'green', label = nom(f2))
    plt.plot(tailles,nc3,ls = '-', marker ='', color = 'red', label = nom(f3))
    plt.plot(tailles,nc4,ls = '-', marker ='', color = 'purple', label = nom(f4))
    plt.plot(tailles,[100]*len(tailles),ls = '-', marker ='', color = 'gold', label = nom(f5))
    plt.ylim([10, 102])
    plt.xlabel('Degré moyen, taille = '+ str(ta), fontsize = 30 )
    plt.ylabel('Pourcentage de renvoi optimal (%)', fontsize = 30 )
    plt.xticks(fontsize = 30)
    plt.yticks(fontsize = 30)
    plt.legend(loc=2, prop={'size': 30})

    plt.show()






## RATIO D'OPTIMALITE à DENSITE fixée
def ratio_opti_nc_d(dens,f1,f2,f3,f4,f5):
    plt.close()
    nc1 = []
    nc2 = []
    nc3 = []
    nc4 = []


    lc = dico_d[dens]

    tailles = []

    for c in lc :
        t,d = c
        m1,m2,m3,m4 = nb_opti(c,f1,f2,f3,f4,f5)
        nc1.append(m1)
        nc2.append(m2)
        nc3.append(m3)
        nc4.append(m4)

        tailles.append(t)

    plt.figure()
    plt.plot(tailles,nc1,ls = '-', marker ='', color = 'blue', label = nom(f1))
    plt.plot(tailles,nc2,ls = '-', marker ='', color = 'green', label = nom(f2))
    plt.plot(tailles,nc3,ls = '-', marker ='', color = 'red', label = nom(f3))
    plt.plot(tailles,nc4,ls = '-', marker ='', color = 'purple', label = nom(f4))
    plt.plot(tailles,[100]*len(tailles),ls = '-', marker ='', color = 'gold', label = nom(f5))
    plt.ylim([10, 102])
    plt.xlabel('Taille de graphe, degré moyen = '+ str(dens), fontsize = 30 )
    plt.ylabel('Pourcentage de renvoi optimal (%)', fontsize = 30 )
    plt.xticks(fontsize = 30)
    plt.yticks(fontsize = 30)
    plt.legend(loc=2, prop={'size': 30})

    plt.show()









##TEMPS à DENSITE fixée
# def comp_tps_d(dens,f1,f2,f3,f4):

## TEMPS à TAILLE fixée
# def comp_tps_t(ta,f1,f2,f3,f4):

## NB COLO à DENSITE fixée
# def comp_nc_d(dens,f1,f2,f3,f4,f5):

## NB COLO à TAILLE fixée
# def comp_nc_t(ta,f1,f2,f3,f4,f5):

## RATIO D'OPTIMALITE à TAILLE fixée
# def ratio_opti_nc_t(ta,f1,f2,f3,f4,f5):

#Pour les tailles des légendes, chez moi je mets : prop={'size': 30} et fontsize = 30, mais au lycée je mets prop={'size': 30} et fontsize = 30 (à changer donc selon l'ordi)

# toujours écrire 'n','wp','d','df' (+ ,'b' si pas temps) dans les paramètres

#Exemple 1  ta
# comp_nc_t(20,'n','wp','d','df','b')

#Exemple 2 dens
# comp_nc_d(6,'n','wp','d','df','b')

# ratio_opti_nc_t(20,'n','wp','d','df','b')