# cviko 6 BREAKPOINTOVE TRIDENI

## ukol 1 - Ur�en� indexu, kde za��n� neset��d�n� oblast permuta�n�ho vektoru
NajdiSetridene <- function(Vec){
  # Vypise index prvni cislice, ktera neni serazena
  # Vec <- c(0, 1, 2, 3, 6, 7, 4, 5, 8) # vstup
  # vystup: 5
 
  Vec <- as.integer(Vec)
  vzor <- c(0:(length(Vec)-1))
  for (i in 1 : length(Vec)){
    if (identical(Vec[i],vzor[i])){
      
    }else {
        return(i)
    }
  }
return(i)
}
  
  
## ukol 2 - Indikace vzestupn�ch/sestupn�ch ��st� vektoru
Vzestupne <- function(Vec){
  # vytvorime indikacni vektor, 1 jsou vzestupne, 0 jsou sestupne
  # Vec <- c(0, 4, 5, 3, 2, 1, 6, 7, 8)     # vstup
  # vystup: 1 1 1 0 0 0 1 1 1
  
  Vec <- as.integer(Vec)
  ind <- as.integer(numeric(length(Vec)))   # indikacni vektor numeric(); rep(0,length(Vec))
  ind[c(1,length(Vec))] <- 1    # nastaveni 1. a posledni hodnoty na 1
  for (i in 1:(length(Vec)-1)){
    if((Vec[i]+1) == Vec[i+1]){
      ind[c(i,i+1)] <- 1
    }
  }
  return(ind)
}
  
  
## ukol 3 - Breakpointov� t��d�n�
BreakPointSort <- function(Vec){
  # funkce setridi vektor na zaklade breakpointove metody
  # Vec <- c(5, 1, 4, 3, 7, 8, 9, 2, 6)   #vstup
  # vystup: 1 2 3 4 5 6 7 8 9
  
  # K permuta�n�mu vektoru se p�idaj� okrajov� hodnoty. V cyklu se opakuj� kroky:
  # indikace vzestupu/sestupu, nalezen� nejmen�� hodnoty ze sestupn�ch, nalezen� za��tku neset��d�n�
  # oblasti, reverze mezi za��tkem neset��d�n� oblasti a nejmen�� hodnotou ze sestupn�ch. Cyklus kon��,
  # kdy� je permuta�n� vektor set��d�n�. Pozor na kolizn� situace.
  # Funkce k �e�en�: match(), which(), print(), identical(), as.integer(), break
  
  vec_perm <- as.integer(c(0, Vec, max(Vec)+1))     # vektor permutaci, doplneni o okrajove hodnoty
  index <- NajdiSetridene(vec_perm)                 # pokolik to mame setridene
  krok <- 0                                        # promenna k pocitani kroku pro serazeni
  
  while (index < length(vec_perm)){
    vec_ind <- Vzestupne(vec_perm)                  # vektor indikaci vzestupne 1, sestupne 0
    # dopneni kolize - vektor neni setrideny, ale vsechny casti jsou vzestupne
    if (sum(vec_ind) == length(vec_perm)){
      # otoceni vzestupne casti od serazeni po predposledni prvek
      vec_perm <- c(vec_perm[1:(index-1)], rev(vec_perm[index:(length(vec_perm)-1)]), vec_perm[length(vec_perm)])
    }else{
      min_sestupny <- min(vec_perm[vec_ind == 0])           # vybere nejmensi sestupny prvek
      min_sestupny_idx <- which(vec_perm == min_sestupny)   # index nejmensiho prvku
      # otoceni sestupne casti od serazeni po min prvek
      vec_perm <- c(vec_perm[1:index-1], rev(vec_perm[index:min_sestupny_idx]), vec_perm[(min_sestupny_idx+1):length(vec_perm)])
    }
    index <- NajdiSetridene(vec_perm)               # pokolik to mame setridene
    krok <- krok + 1                               # kolik kroku potrebujem k serazeni
  }
  vec <- vec_perm[2:(length(vec_perm)-1)]           # odstraneni kotvicich prvku 
  print(c('Minimalni pocet kroku:', krok))
  return(vec)
}
 
  
  