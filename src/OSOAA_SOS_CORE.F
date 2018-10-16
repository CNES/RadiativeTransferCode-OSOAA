C******************************************************************************
C* FICHIER: OSOAA_SOS_CORE.F
C*
C*------------------------------------------------------------------------ 
C* OSOAA Licence
C*   Copyright (c) 2015, Université Pierre et Marie Curie - Paris 6
C*                       and Centre National d'Etudes Spatiales (CNES)
C*   Copyright (c) 2016, Université Pierre et Marie Curie - Paris 6
C*                       and Communications & Systemes (CS SI)
C* 
C*   This program is free software: you can redistribute it and/or modify
C*   it under the terms of the GNU General Public License as published by
C*   the Free Software Foundation, either version 2 of the License, or
C*   (at your option) any later version.
C* 
C*   This program is distributed in the hope that it will be useful,
C*   but WITHOUT ANY WARRANTY; without even the implied warranty of
C*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
C*   See the GNU General Public License for more details.
C* 
C*   You should have received a copy of the GNU General Public License
C*   along with this program.  If not, see <http://www.gnu.org/licenses/>.
C*------------------------------------------------------------------------ 
C* 
C*
C* PROJET: Ordres Successifs Océan Atmosphère  - Avancé
C*         Ocean Successive Orders with Atmosphere - Advanced
C*
C* ROLE: Routines composant le coeur du code des OS. 
C*       Résolution de l'Equation de Transfert Radiatif
C*       pour une décomposition en séries de Fourier. 
C*
C*
C* AUTEURS: 
C*   Codes initiaux: 
C*      - Laboratoire d'Optique Atmosphérique (LOA, Villeneuve d'Ascq) 
C*          Jean-Luc Deuzé, Maurice Herman, Richard Santer
C*          ==> code des Ordres Successifs de Diffusion 
C*            (version atmosphérique et glitter).
C*      - Laboration d'Océanographie de Villefranche sur Mer (LOV)
C*          Malik Chami
C*          ==> code OSOA (version mer plate).
C*      - Communication & Systèmes (CS, Toulouse) pour le compte du CNES
C*          Bruno Lafrance
C*          ==> code SOS (version industrialisée du code des OS du LOA).
C*
C*   Code OSOAA : 
C*      - Laboration d'Océanographie de Villefranche sur Mer (LOV)
C*          Malik Chami
C*          ==> Spécification de besoin.
C*      - Communication & Systemes (CS, Toulouse)
C*          Bruno Lafrance 
C*          ==> Reprise du codage et développements complémentaires
C*          ==> Spécification de l'interface air/mer agitée, implémentation.
C*
C*
C* ROUTINES COUVERTES PAR LE FICHIER : 
C*   - OSOAA_SOS_CORE
C*   - OSOAA_NOYAUX
C*   - OSOAA_FSOURCE_ATM_ORDRE1
C*   - OSOAA_FSOURCE_ATM_ORDREIG
C*   - OSOAA_FSOURCE_SEA_ORDRE1_MERPLATE
C*   - OSOAA_FSOURCE_SEA_ORDREIG
C*   - OSOAA_INTEGR_EPOPT_ATM_UP
C*   - OSOAA_INTEGR_EPOPT_ATM_DOWN
C*   - OSOAA_INTEGR_EPOPT_SEA_UP
C*   - OSOAA_INTEGR_EPOPT_SEA_DOWN
C*   - OSOAA_PARAM_CONV
C*   - OSOAA_ARRET_DIFFUS
C*   - OSOAA_AJOUT_QUEUE
C*   - OSOAA_ARRET_FOURIER
C*   - OSOAA_INTERF_MERPLATE
C*   - OSOAA_FSOURCE_DIFF_FRESNEL1
C*
C*
C* MOD:VERSION:1.0: 02/04/2015: Initial version of the OSOAA code
C* MOD:VERSION:1.1: 15/06/2015: All comments for log files are now in english
C* MOD:VERSION:1.2: 27/04/2016:  
C*     - New output file FIC_FLUX : Profile of downward and upward 
C*                                  fluxes from TOA to the sea bottom 
C*                                  normalised to the solar irradiance at TOA     
C*     - New arguments : 
C*          - FIC_FLUX           
C*          - PROFIL_Z(0:NT_TOT) the profile of altitudes in the atmosphere (m) 
C*                               or the sea depth (m) with respect of each level                  
C* MOD:VERSION:1.3: 18/07/2018:  
C*     - Correction of anomalies for the calculation of the upward direct flux
C*       in the atmosphere provided in the Flux.txt file : 
C*       Both the case of a rough sea and the case of a flat sea were impacted by
C*       an error of calculation of the atmospheric transmission from the surface
C*       to any level in the atmosphere.
C*       Moreover, the normalisation by the solar irradiance (PI) was forgotten.
C*   
C*     - The ratio Eu/Ed is provided in the Flux.txt file (column n°9).
C*   
C*     - Flux given in the trace file are normalized by the solar irradiance (PI)
C*
C******************************************************************************

C----------------------------------------------------------------------------
C Définition des constantes  
C---------------------------------------------------------------------------- 
C Constantes utilisées : 
C
C    CTE_LENFIC2 : Longueur des noms de fichiers avec arborescence.
C    CTE_OS_NBMU_MAX  : Nombre maximal d'angles de Gauss positifs 
C                       pour les calculs de luminance
C    CTE_NT_ATM : Nombre de couches du profil atmosphérique
C    CTE_NT_SEA : Nombre de couches du profil océanique
C    CTE_MDF_AIR : Facteur de dépolarisation des molécules dans l'atmosphère 
C    CTE_MDF_SEA : Facteur de dépolarisation des molécules dans la mer   
C
C    CTE_PH_SEUIL_CV_SG :  Seuil de convergence en série géométrique.
C    CTE_PH_SEUIL_SUMDIF : Seuil d'arrêt des diffusions multiples.
C    CTE_PH_SEUIL_SF :     Seuil pour l'arrêt de la décomposition en séries de Fourier.
C
C    CTE_OS_NB_MAX :  Valeur maximale pour l'ordre limite du développement en 
C                     polynômes de Legendre des fonctions de phase des particules
C                     et pour le développement en séries de Fourier des matrices
C                     d'interface et des champs de luminance.
C
C
C Constantes spécifiques :
C    INCTE_IDLOG_SOS : ID du ficher Trace de la routine
C    INCTE_PI : Valeur de PI (calculé par la machine)
C
C    EXPERT_MODE_FORCED_FATM_NULL : Constant devoted to expert analyses 
C         ==> Force the atmospheric source function to be null if TRUE
C    EXPERT_MODE_FORCED_FSEA_NULL : Constant devoted to expert analyses 
C         ==> Force the sea source function to be null if TRUE
C----------------------------------------------------------------------------
#include "OSOAA.h"
#define INCTE_IDLOG_SOS 99
#define INCTE_PI DACOS(-1.D+00)

#define EXPERT_MODE_FORCED_FATM_NULL .FALSE.
#define EXPERT_MODE_FORCED_FSEA_NULL .FALSE.



C==============================================================================
C PROCEDURE: OSOAA_SOS_CORE
C ==========
C      Programme de simulation de transfert radiatif 
C      réalisant les étapes de traitement suivantes 
C      pour chaque ordre de la décomposition en série de Fourier:
C
C      1) Pour l'ordre IG = 1 de diffusion :
C         ----------------------------------
C        - Calcul des fonctions sources de diffusion primaire dans l'atmosphère 
C          et dans la mer.
C        - Etape 1 : Réflexion au fond de mer du faisceau solaire direct transmis.
C        - Etape 2 : Calcul de la luminance marine montante d'ordre 1 du fond à la surface.
C        - Etape 3 : Calcul de la luminance montante transmise au passage mer -> air.
C        - Etape 4 : Calcul de la réflexion lambertienne du faisceau solaire direct 
C                    incident au niveau de la surface marine.
C        - Etape 5 : Calcul de la luminance montante de la surface au TOA.
C        - Etape 6 : Calcul de la luminance atmosphérique descendante du TOA à la surface.
C        - Etape 7 : Calcul de la luminance descendante transmise au passage air -> mer.
C        - Etape 8 : Calcul de la luminance descendante au niveau de la surface par 
C                    réflexion (mer -> mer) de la luminance marine montante d'ordre 1.
C        - Etape 9 : Calcul de la luminance descendante du TOA au fond de mer.
C        - Etape 10: Calcul de la luminance montante au niveau de la surface par réflexion
C                    (air -> air) de la luminance atmosphérique descendante d'ordre 1.
C
C      2) Pour l'ordre IG > 1 de diffusion :
C         ----------------------------------
C        - Calcul des fonctions sources de diffusion d'ordre IG dans l'atmosphère 
C          et dans la mer à partir du champ d'ordre IG-1.
C        - Etape 1 : Réflexion au fond de mer du champ de luminance d'ordre IG-1.
C        - Etape 2 : Calcul de la luminance marine montante d'ordre 1 du fond à la surface.
C        - Etape 3 : Calcul de la luminance montante transmise au passage mer -> air.
C        - Etape 4 : Calcul de la réflexion lambertienne du champ de luminance atmosphérique
C                    incident au niveau de la surface marine.
C        - Etape 5 : Calcul de la luminance montante de la surface au TOA.
C        - Etape 6 : Calcul de la luminance atmosphérique descendante du TOA à la surface.
C        - Etape 7 : Calcul de la luminance descendante transmise au passage air -> mer.
C        - Etape 8 : Calcul de la luminance descendante au niveau de la surface par 
C                    réflexion (mer -> mer) de la luminance marine montante d'ordre IG.
C        - Etape 9 : Calcul de la luminance descendante du TOA au fond de mer.
C        - Etape 10: Calcul de la luminance montante au niveau de la surface par réflexion
C                    (air -> air) de la luminance atmosphérique descendante d'ordre IG.
C
C
C      Le programme distingue au niveau de l'interface air / mer le cas d'une mer plate
C      du cas d'une mer agitée .
C
C      Le programme réalise des tests de convergence des calculs en série géométrique
C      pour les diffusions successives et en séries de Fourier pour les ordres de
C      décomposition.
C
C      Le programme calcule des flux au TOA, au niveau de la surface 
C      (juste au-dessus : niveau 0+ et juste en-dessous : niveau 0-)
C
C      Les paramètres de Stokes I, Q et U sont simulées.
C
C
C
C Description des paramètres 			 
C ------------------------------
C
C     N0  (I4)        (E) : Numéro de mu de Gauss pour l'angle zénithal solaire dans l'air.
C                          (angle complémentaire : mus = -rmu(N0)) 
C
C     M0  (I4)        (E) : Numéro de mu de Gauss pour l'angle zénithal solaire dans l'eau.
C      			    (transmission mer plate).
C			    (angle complémentaire : musw = -rmu(M0))
C
C     NBMU (I4)       (E) : Nombre d'angles (positifs) effectivement utiles.
C
C     RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  
C                     (E) : Tableau des cosinus des angles utilisés.
C
C     GA(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  
C                     (E) : Tableau des poids des angles utilisés.
C
C     OS_NB (I4)      (E) : Ordre max des développements des fonctions de phase 
C		            en Polynômes de Legendre à utiliser.
C
C     IMAT_SURF  (I4) (E) : Indice précisant s'il y a usage des fichiers 
C                           SURFACE  d'interface air / mer agitée 
C			    (1 si c'est le cas, 0 sinon).
C
C     FICRAA (CHAR*CTE_LENFIC2) (E) : Nom du fichier Matrice de réflexion air / air.
C     FICTAW (CHAR*CTE_LENFIC2) (E) : Nom du fichier Matrice de transmission air / eau.
C     FICRWW (CHAR*CTE_LENFIC2) (E) : Nom du fichier Matrice de réflexion eau / eau.
C     FICTWA (CHAR*CTE_LENFIC2) (E) : Nom du fichier Matrice de transmission eau / air.
C
C     SEA_IND (double)    	(E) : Indice de réfraction eau / air à la longueur 
C				      d'onde de simulation des luminances.
C
C     RHO_SURF  (double)    	(E) : Albédo de surface à la longueur d'onde
C				      de simulation des luminances.
C
C     RHO_BOT  (double)    	(E) : Albédo de fond de mer à la longueur d'onde
C				      de simulation des luminances.
C
C     ATM_TAUEXT_TR(0:CTE_NT_ATM)  (double)  
C                     (E) : Profil d'épaisseur optique d'extinction 
C			    de l'atmosphère ajusté à la troncature. 
C
C     ATM_PCAER_TR(0:CTE_NT_ATM)  (double)  
C                     (E) : Proportion d'aérosols pour la diffusion dans chaque couche 
C			    atmosphérique, ajustée à la troncature.
C
C     ATM_PCRAY_TR(0:CTE_NT_ATM)  (double)  
C                     (E) : Proportion de molécules pour la diffusion dans chaque couche 
C			    atmosphérique, ajustée à la troncature.
C
C     SEA_TAUEXT_TR(0:CTE_NT_SEA)  (double)  
C                     (E) : Profil d'épaisseur optique d'extinction
C			    de la mer, ajusté aux troncatures.
C
C     SEA_PCRAY_TR(0:CTE_NT_SEA)  (double)  
C                     (E) : Proportion de molécules pour la diffusion dans chaque couche
C			    marine, ajustée aux troncatures.
C
C     SEA_PC_CHL_TR(0:CTE_NT_SEA)  (double)  
C                     (E) : Proportion de phytoplancton pour la diffusion dans chaque couche
C			    marine, ajustée aux troncatures.
C
C     SEA_PC_MLP_TR(0:CTE_NT_SEA)  (double)  
C                     (E) : Proportion de sédiments pour la diffusion dans chaque couche
C			    marine, ajustée aux troncatures.
C
C     PROFIL_Z(0:NT_TOT)  (double)  
C                     (E) : Profil global d'altitudes et de profondeurs (m).
C  
C     ALPHA_AER(0:CTE_OS_NB_MAX)  (double)  
C                     (E) : Paramètres alpha(k) de la décomposition de la matrice de phase
C                           des aérosols.
C
C     BETA_AER(0:CTE_OS_NB_MAX)  (double)  
C                     (E) : Paramètres beta11(k) de la décomposition de la matrice de phase
C                           des aérosols.
C
C     GAMMA_AER(0:CTE_OS_NB_MAX)  (double)  
C                     (E) : Paramètres gamma12(k) de la décomposition de la matrice de phase
C                           des aérosols.
C
C     ZETA_AER(0:CTE_OS_NB_MAX)  (double)  
C                     (E) : Paramètres zeta(k) de la décomposition de la matrice de phase
C                           des aérosols.
C
C     ALPHA_CHL(0:CTE_OS_NB_MAX)  (double)  
C                     (E) : Paramètres alpha(k) de la décomposition de la matrice de phase
C                           du phytoplancton.
C
C     BETA_CHL(0:CTE_OS_NB_MAX)  (double)  
C                     (E) : Paramètres beta11(k) de la décomposition de la matrice de phase
C                           du phytoplancton.
C
C     GAMMA_CHL(0:CTE_OS_NB_MAX)  (double)  
C                     (E) : Paramètres gamma12(k) de la décomposition de la matrice de phase
C                           du phytoplancton.
C
C     ZETA_CHL 0:CTE_OS_NB_MAX)  (double)  
C                     (E) : Paramètres zeta(k) de la décomposition de la matrice de phase
C                           du phytoplancton.
C
C     ALPHA_MLP(0:CTE_OS_NB_MAX)  (double)  
C                     (E) : Paramètres alpha(k) de la décomposition de la matrice de phase
C                           des sédiments.
C
C     BETA_MLP(0:CTE_OS_NB_MAX)  (double)  
C                     (E) : Paramètres beta11(k) de la décomposition de la matrice de phase
C                           des sédiments.
C
C     GAMMA_MLP(0:CTE_OS_NB_MAX)  (double)  
C                     (E) : Paramètres gamma12(k) de la décomposition de la matrice de phase
C                           des sédiments.
C
C     ZETA_MLP 0:CTE_OS_NB_MAX)  (double)  
C                     (E) : Paramètres zeta(k) de la décomposition de la matrice de phase
C                           des sédiments.
C    
C     IGMAX  (I4)     (E) : Ordre maximal d'interaction.
C     IBORM  (I4)     (E) : Ordre maximal du développement en séries de Fourier
C
C     FIC_FLUX (CHAR*CTE_LENFIC2)    
C                     (S) : Nom du fichier de profil du flux montant descendant,
C			    normalisé par l'éclairement solaire au TOA (chemin complet).
C
C     FICSOS_LOG (CHAR*CTE_LENFIC2)    
C                     (S) : Nom du fichier de log (chemin complet)
C
C     FICSOS_RES_BIN (CHAR*CTE_LENFIC2)
C                     (S) : Nom du fichier résultat binaire des calculs SOS.
C   		            (chemin complet)
C
C     IER (I4)        (S) : indice d'erreur : 0 si pas d'erreur, -1 sinon.
C   
C
C
C Résultats fournis par la procédure	 
C ----------------------------------
C 
C  - FICSOS_RES_BIN : Sortie avancée dédié aux utilisateurs expert
C      Fichier binaire du champ de luminance simulé (I,Q,U) pour chaque niveau du profil 
C      atmosphérique et marin, par ordre de la décomposition en série de Fourier.
C   
C  - Fichier Trace optionnel.
C
C
C Description des fichiers utilisés	 
C ---------------------------------
C
C    Fichiers d'interface air / mer agitée:
C      - réflexion Air / Air    : FICRAA
C      - transmission Air / Mer : FICTAW
C      - réflexion Mer / Mer    : FICRWW
C      - transmission Mer / Air : FICTWA
C
C       Chacun des ces fichiers est un fichier binaire non formaté
C       qui contient la matrice d'interface décomposée en série de Fourier.
C        --> Pour chaque ordre IS de la décomposition en série de Fourier (IS = 0 à OS_NB),
C            le fichier contient l'enregistrement des éléments Pij de la matrice,
C            limité aux angles utiles : 
C              P11(LUM_NBMU,LUM_NBMU) (réel),
C              P12(LUM_NBMU,LUM_NBMU) (réel),
C              P13(LUM_NBMU,LUM_NBMU) (réel),
C              P21(LUM_NBMU,LUM_NBMU) (réel),
C              P22(LUM_NBMU,LUM_NBMU) (réel),
C              P23(LUM_NBMU,LUM_NBMU) (réel),
C              P31(LUM_NBMU,LUM_NBMU) (réel),
C              P32(LUM_NBMU,LUM_NBMU) (réel),
C              P33(LUM_NBMU,LUM_NBMU) (réel)
C
C
C    Fichier résultat FICSOS_RES_BIN
C      ==> Fichier binaire non formaté du champ de luminance simulé (I,Q,U)
C
C		format : Pour chaque ordre IS de la décomposition en série de Fourier
C                        (IS = 0 à IS_MAX variable selon la simulation),      
C                        enregistrement d'un bloc pour les paramètres de Stokes I, Q et U,
C                        sur tout le profil (du TOA au fond de mer : P=0,NT_TOT)
C				((I3(I,K),P=0,NT_TOT),K=-NBMU,NBMU),
C    		   	        ((Q3(I,K),P=0,NT_TOT),K=-NBMU,NBMU),
C			        ((U3(I,K),P=0,NT_TOT),K=-NBMU,NBMU)
C
C    		--> Les tableaux ont une taille limitée angles utiles : -NBMU:NBMU   
C
C
C    Fichier résultat FIC_FLUX
C      ==> Fichier Ascii formaté du profil de flux montant descendant du TOA au fond marin
C          normalisé par l'éclairement solaire au TOA
C
C		format : 5 lignes d'entete
C                        9 colonnes pour chaque niveau du profil (0 à NT_TOT)
C                           --> Level, Z in meters, Direct_Downward_Flux, Diffuse_Downward_Flux, 
C                               Total_Downward_Flux, Direct_Upward_Flux, Diffuse_Upward_Flux, 
C                               Total_Upward_Flux, (Total_Upward_Flux/Total_Downward_Flux)
C				FORMAT(2X,I4,1X,F12.5,2X,7(2X,E12.6))
C 
C
C Common utilisé:
C --------------
C  Aucun
C
C
C Cas d'erreur :  
C ------------
C     - Erreur d'écriture dans le fichier Trace
C     - Erreur d'ouverture ou de lecture des fichiers de matrice d'interface
C     - Erreur d'ouverture ou d'écriture du fichier résultat.
C     - Erreur dans programme appelé (OSOAA_INTERF_MERPLATE)
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant
C
C
C  Sous programmes utilisés: 
C --------------------------
C  Ce programme fait appel aux routines:
C      - OSOAA_NOYAUX
C      - OSOAA_FSOURCE_ATM_ORDRE1
C      - OSOAA_FSOURCE_SEA_ORDREIG
C      - OSOAA_INTERF_MERPLATE
C      - OSOAA_FSOURCE_SEA_ORDRE1_MERPLATE
C      - OSOAA_INTEGR_EPOPT_SEA_UP
C      - OSOAA_INTEGR_EPOPT_ATM_UP
C      - OSOAA_FSOURCE_DIFF_FRESNEL1
C      - OSOAA_INTEGR_EPOPT_ATM_DOWN
C      - OSOAA_INTEGR_EPOPT_SEA_DOWN
C      - OSOAA_FSOURCE_ATM_ORDREIG
C      - OSOAA_PARAM_CONV
C      - OSOAA_AJOUT_QUEUE
C      - OSOAA_ARRET_DIFFUS
C      - OSOAA_ARRET_FOURIER
C
C==============================================================================

      SUBROUTINE OSOAA_SOS_CORE(N0,M0,NBMU,RMU,GA,OS_NB,
     &			        IMAT_SURF,
     &                          FICRAA,FICTAW,FICRWW,FICTWA,
     &				SEA_IND,RHO_SURF,RHO_BOT,
     &				ATM_TAUEXT_TR,ATM_PCAER_TR,ATM_PCRAY_TR,
     &				SEA_TAUEXT_TR,SEA_PCRAY_TR,
     &				SEA_PC_CHL_TR,SEA_PC_MLP_TR,
     &   			ALPHA_AER,BETA_AER,GAMMA_AER,ZETA_AER, 
     &	 		   	ALPHA_CHL,BETA_CHL,GAMMA_CHL,ZETA_CHL,
     &	 		   	ALPHA_MLP,BETA_MLP,GAMMA_MLP,ZETA_MLP,
     &				IGMAX,IBORM,
     &                          PROFIL_Z, FIC_FLUX,
     &				FICSOS_LOG,FICSOS_RES_BIN,IER)


      IMPLICIT NONE


C Constantes SOS
C -----------------
      INTEGER*2 IDLOG			! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_SOS)

      INTEGER*2 IDFICOS			! Numéro fichier résultat binaire OS    
      PARAMETER(IDFICOS=10)




      INTEGER*2 IDRAA		! Numéro identifiant du fichier SURFACE RAA
      PARAMETER(IDRAA=21)

      INTEGER*2 IDTAW		! Numéro identifiant du fichier SURFACE TAW
      PARAMETER(IDTAW=22)
      
      INTEGER*2 IDRWW		! Numéro identifiant du fichier SURFACE RWW
      PARAMETER(IDRWW=23)

      INTEGER*2 IDTWA		! Numéro identifiant du fichier SURFACE TWA
      PARAMETER(IDTWA=24)


      INTEGER*4 NT_TOT		! Nombre total de couches (atmosphère + mer)      
      PARAMETER(NT_TOT=CTE_NT_ATM+CTE_NT_SEA+1)
      

      LOGICAL TRACE_DEBUG	! Option pour des traces en mode d'expertise
      PARAMETER(TRACE_DEBUG=.FALSE.)
      

C* Définition des variables   
C*------------------------------------------------------
      DOUBLE PRECISION RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
                                ! (E) Cosinus des angles de Gauss (et utilisateur).
      DOUBLE PRECISION GA(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
                                ! (E) Poids des angles de Gauss (et utilisateur).

      DOUBLE PRECISION Z	! Valeur ponctuelle du tableau des poids de Gauss.

      DOUBLE PRECISION TAB      ! Valeur du cosinus de l'angle zénithal 
                                ! solaire (mus) dans l'atmosphère.

      DOUBLE PRECISION TABW     ! Valeur du cosinus de l'angle zénithal 
                                ! solaire (mus) transmis dans la mer (plate).

      DOUBLE PRECISION SIGN	! Valeur 1 ou -1.
      DOUBLE PRECISION COEF	! Coefficient 1 ou 2.


      DOUBLE PRECISION SEA_IND  ! (E) Indice de réfraction de la mer 
      				! pour la longueur d'onde de simulation de la luminance.


	!Profils atmosphériques ajustés à la troncature aérosols
      DOUBLE PRECISION ATM_TAUEXT_TR(0:CTE_NT_ATM) 
      				! (E) Profil d'épaisseur optique d'extinction 
				! de l'atmosphère ajusté à la troncature. 


      DOUBLE PRECISION ATM_PCRAY_TR(0:CTE_NT_ATM)  
      				! (E)  Proportion de molécules pour la diffusion 
      				! dans chaque couche atmosphérique, ajustée à la troncature.
      DOUBLE PRECISION ATM_PCAER_TR(0:CTE_NT_ATM)  
      				! (E)  Proportion d'aérosols pour la diffusion 
				! dans chaque couche atmosphérique, ajustée à la troncature.

						
	!Profils marins ajustés à la troncature des fonctions de phase 
	!des sédiments et du phytoplancton      
      DOUBLE PRECISION SEA_TAUEXT_TR(0:CTE_NT_SEA) 
      				! (E) Profil d'épaisseur optique d'extinction de la mer, 
      				! ajusté aux troncatures.     

      DOUBLE PRECISION SEA_PCRAY_TR(0:CTE_NT_SEA)  
      				! (E)  Proportion de molécules pour la diffusion 
      				! dans chaque couche marine, ajustée aux troncatures.
      DOUBLE PRECISION SEA_PC_CHL_TR(0:CTE_NT_SEA) 
      				! (E)  Proportion de phytoplancton pour la diffusion 
      				! dans chaque couche marine, ajustée aux troncatures.
      DOUBLE PRECISION SEA_PC_MLP_TR(0:CTE_NT_SEA)
      				! (E)  Proportion de sédiments pour la diffusion 
      				! dans chaque couche marine, ajustée aux troncatures.


      DOUBLE PRECISION CH_ATM(0:CTE_NT_ATM)	
      				! Atténuation du faisceau solaire direct / 4  
                                ! dans l'atmosphère pour la direction de propagation 
				! du faisceau solaire. 

      DOUBLE PRECISION ATM_TAUEXT_TR_TOT	! Epaisseur optique totale de l'atmosphère
      						! ajustée à la troncature

      DOUBLE PRECISION TRANS_ATM_DIRECT_TR	! Transmission du faisceau solaire direct 
      						! dans l'atmosphère, ajustée à la troncature
      DOUBLE PRECISION TRANS     		! Transmission 
      
      DOUBLE PRECISION CH_SEA(CTE_OS_NBMU_MAX,0:CTE_NT_SEA)	
      				! Atténuation du faisceau solaire direct / 4 dans la mer
				! en fonction de la profondeur et de la direction 
				! de propagation  
				! ==> transmission de la surface au niveau I dans la mer.

      DOUBLE PRECISION CH_SEA_MUS(0:CTE_NT_SEA)	
      				! Atténuation du faisceau solaire direct / 4 dans la mer 
                                ! en fonction de la profondeur pour la direction 
				! de propagation du faisceau solaire transmis.
				! ==> transmission de la surface au niveau I dans la mer.

      DOUBLE PRECISION PROFIL_Z(0:NT_TOT) ! Profil global d'altitudes et de profondeurs (m).
						
      DOUBLE PRECISION TAUSEA		! Valeur d'épaisseur optique de la mer.

      DOUBLE PRECISION CH_ATM_TOT	! Valeur d'atténuation totale de l'atmosphère 
      					! pour la direction d'incidence solaire.
      DOUBLE PRECISION CH_SEA_I		! Valeur ponctuelle d'atténuation dans la mer.

      DOUBLE PRECISION ATTDIR		! Atténuation dans l'atmosphère du TOA au sol 
      					! pour la direction d'incidence solaire.
						
      DOUBLE PRECISION ATTVIS		! Atténuation dans l'atmosphère du sol à un niveau I
      					! donné pour une direction de propagation donnée.
      
      DOUBLE PRECISION DTAU   		! Différence d'épaisseur optique entre niveaux.
      
	!Coefficients de la matrice de phase de la diffusion moléculaire :         	
      DOUBLE PRECISION BETA0_MOL_AIR	! Coefficients du développement de la matrice 
      DOUBLE PRECISION BETA2_MOL_AIR	! de phase moléculaire en fonctions de Legendre
      DOUBLE PRECISION GAMMA2_MOL_AIR	! dans l'air
      DOUBLE PRECISION ALPHA2_MOL_AIR	

      DOUBLE PRECISION BETA0_MOL_SEA	! Coefficients du développement de la matrice 
      DOUBLE PRECISION BETA2_MOL_SEA	! de phase moléculaire en fonctions de Legendre
      DOUBLE PRECISION GAMMA2_MOL_SEA	! dans l'eau
      DOUBLE PRECISION ALPHA2_MOL_SEA	
      
      DOUBLE PRECISION AAA	! Facteur correctif de la dépolarisation des molécules.  

      DOUBLE PRECISION VAL	! Valeur intermédiaire de calcul
      
      
C---- Eléments Pij des matrices d'interface
	!Matrice RAA
      REAL*4 RAA11(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 RAA12(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 RAA13(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 RAA21(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 RAA22(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 RAA23(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 RAA31(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 RAA32(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 RAA33(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)

	!Matrice TAW
      REAL*4 TAW11(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 TAW12(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 TAW13(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 TAW21(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 TAW22(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 TAW23(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 TAW31(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 TAW32(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 TAW33(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)     
      
	!Matrice RWW
      REAL*4 RWW11(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 RWW12(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 RWW13(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 RWW21(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 RWW22(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 RWW23(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 RWW31(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 RWW32(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 RWW33(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      
	!Matrice TWA
      REAL*4 TWA11(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 TWA12(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 TWA13(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 TWA21(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 TWA22(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
     &          
      REAL*4 TWA23(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 TWA31(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 TWA32(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL*4 TWA33(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)             


	!Paramètres des propriétés radiatives des aérosols
      DOUBLE PRECISION ALPHA_AER(0:CTE_OS_NB_MAX)	!  (E) Paramètres alpha(k).
      DOUBLE PRECISION BETA_AER(0:CTE_OS_NB_MAX)	!  (E) Paramètres beta11(k).
      DOUBLE PRECISION GAMMA_AER(0:CTE_OS_NB_MAX)	!  (E) Paramètres gamma12(k).
      DOUBLE PRECISION ZETA_AER(0:CTE_OS_NB_MAX)	!  (E) Paramètres zeta(k).

	!Paramètres des propriétés radiatives du phytoplancton
      DOUBLE PRECISION ALPHA_CHL(0:CTE_OS_NB_MAX)	!  (E) Paramètres alpha(k).
      DOUBLE PRECISION BETA_CHL(0:CTE_OS_NB_MAX)	!  (E) Paramètres beta11(k).
      DOUBLE PRECISION GAMMA_CHL(0:CTE_OS_NB_MAX)	!  (E) Paramètres gamma12(k).
      DOUBLE PRECISION ZETA_CHL(0:CTE_OS_NB_MAX)	!  (E) Paramètres zeta(k).

	!Paramètres des propriétés radiatives des sédiments
      DOUBLE PRECISION ALPHA_MLP(0:CTE_OS_NB_MAX)	!  (E) Paramètres alpha(k).
      DOUBLE PRECISION BETA_MLP(0:CTE_OS_NB_MAX)	!  (E) Paramètres beta11(k).
      DOUBLE PRECISION GAMMA_MLP(0:CTE_OS_NB_MAX)	!  (E) Paramètres gamma12(k).
      DOUBLE PRECISION ZETA_MLP(0:CTE_OS_NB_MAX)	!  (E) Paramètres zeta(k).




	!Fonctions de Legendre et fonctions dérivées pour le calcul
	! de matrice de phase.    
      DOUBLE PRECISION XPL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      		! Fonction de Legendre PSL à l'ordre L=2 et S=IS
      		! en fonction de l'angle mu.     		
      DOUBLE PRECISION XRL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      		! Fonction RSL à l'ordre L=2 et S=IS en fonction de mu.
      DOUBLE PRECISION XTL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      		! Fonction TSL à l'ordre L=2 et S=IS en fonction de mu.
 
	! Eléments Pij de la matrice de phase des aérosols pour un ordre
	! IS de la décomposition en séries de Fourier.
	! Soit mup la direction d'incidence (-1 < mup < 1)
	! et mu celle de diffusion (-1 < mu < 1),
	! la diffusion est exprimée par Pij(mu,mup).	
      DOUBLE PRECISION 
     &    BP_AER(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! Elément P11.
      DOUBLE PRECISION 
     &    GR_AER(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! P12 ou P21.
      DOUBLE PRECISION 
     &    GT_AER(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! -P13 ou -P31.
      DOUBLE PRECISION 
     &    ARR_AER(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! P22.
      DOUBLE PRECISION 
     &    ART_AER(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! -P23 ou -P32.
      DOUBLE PRECISION 
     &    ATT_AER(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! P33.


	! Eléments Pij de la matrice de phase du phytoplancton pour un ordre
	! IS de la décomposition en séries de Fourier.
	! Soit mup la direction d'incidence (-1 < mup < 1)
	! et mu celle de diffusion (-1 < mu < 1),
	! la diffusion est exprimée par Pij(mu,mup).	
      DOUBLE PRECISION 
     &    BP_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! Elément P11.
      DOUBLE PRECISION 
     &    GR_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! P12 ou P21.
      DOUBLE PRECISION 
     &    GT_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! -P13 ou -P31.
      DOUBLE PRECISION 
     &    ARR_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! P22.
      DOUBLE PRECISION 
     &    ART_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! -P23 ou -P32.
      DOUBLE PRECISION 
     &    ATT_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! P33.



	! Eléments Pij de la matrice de phase des sédiments pour un ordre
	! IS de la décomposition en séries de Fourier.
	! Soit mup la direction d'incidence (-1 < mup < 1)
	! et mu celle de diffusion (-1 < mu < 1),
	! la diffusion est exprimée par Pij(mu,mup).	
      DOUBLE PRECISION 
     &    BP_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! Elément P11.
      DOUBLE PRECISION 
     &    GR_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! P12 ou P21.
      DOUBLE PRECISION 
     &    GT_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! -P13 ou -P31.
      DOUBLE PRECISION 
     &    ARR_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! P22.
      DOUBLE PRECISION 
     &    ART_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! -P23 ou -P32.
      DOUBLE PRECISION 
     &    ATT_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! P33.





C* Variables associées à l'intégration spatiale et verticale : 
      	! Champ de luminance d'ordre IG-1 ou IG en fonction du niveau du profil
	! et de la direction.       	
      DOUBLE PRECISION I1_ATM(0:CTE_NT_ATM,
     &			      -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ I.
      DOUBLE PRECISION Q1_ATM(0:CTE_NT_ATM,
     &			      -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ Q.
      DOUBLE PRECISION U1_ATM(0:CTE_NT_ATM,
     &			      -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ U.
	  	
      DOUBLE PRECISION I1_SEA(0:CTE_NT_SEA,
     &			      -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ I.
      DOUBLE PRECISION Q1_SEA(0:CTE_NT_SEA,
     &			      -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ Q.
      DOUBLE PRECISION U1_SEA(0:CTE_NT_SEA,
     &			      -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ U.


      DOUBLE PRECISION COEF_INTSPA  ! Coefficient de pondération des 
      				    ! intégrations spatiales
 
 
      	! Composante du champ (I, Q, U) diffus réfléchi sur mer plate      
      DOUBLE PRECISION I1_ATM_FRESNEL(0:CTE_NT_ATM,
     &			      -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ I.
      DOUBLE PRECISION Q1_ATM_FRESNEL(0:CTE_NT_ATM,
     &			      -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ Q.
      DOUBLE PRECISION U1_ATM_FRESNEL(0:CTE_NT_ATM,
     &			      -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ U.
     

      	! Composante du champ (I, Q, U) atmosphérique d'ordre 1,
	! due à la réflexion du faisceau solaire direct en surface.  
      DOUBLE PRECISION RII(0:CTE_NT_ATM,
     &			   -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ I.
      DOUBLE PRECISION RQQ(0:CTE_NT_ATM,
     &			   -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ Q.
      DOUBLE PRECISION RUU(0:CTE_NT_ATM,
     &			   -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ U.
     
                
	! Vecteur fonction source en fonction du niveau du profil
	! et de la direction.
      DOUBLE PRECISION I2_ATM(0:CTE_NT_ATM,
     &                        -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour I.
      DOUBLE PRECISION Q2_ATM(0:CTE_NT_ATM,
     &                        -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour Q.
      DOUBLE PRECISION U2_ATM(0:CTE_NT_ATM,
     &                        -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour U.

      DOUBLE PRECISION I2_SEA(0:CTE_NT_SEA,
     &                        -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour I.
      DOUBLE PRECISION Q2_SEA(0:CTE_NT_SEA,
     &                        -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour Q.
      DOUBLE PRECISION U2_SEA(0:CTE_NT_SEA,
     &                        -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour U.

	! Vecteur fonction source pour diffusion du champ réfléchi sur mer plate
      DOUBLE PRECISION I2_ATM_FRESNEL(0:CTE_NT_ATM,
     &                        -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour I.
      DOUBLE PRECISION Q2_ATM_FRESNEL(0:CTE_NT_ATM,
     &                        -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour Q.
      DOUBLE PRECISION U2_ATM_FRESNEL(0:CTE_NT_ATM,
     &                        -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour U.


      DOUBLE PRECISION II2 	! Valeur intermédiaire pour le calcul de I2
      DOUBLE PRECISION QQ2 	! Valeur intermédiaire pour le calcul de Q2 
      DOUBLE PRECISION UU2 	! Valeur intermédiaire pour le calcul de U2 

      DOUBLE PRECISION XI1	! Valeur de I du champ montant d'ordre IG-1.
      DOUBLE PRECISION XQ1	! Valeur de Q du champ montant d'ordre IG-1.
      DOUBLE PRECISION XU1	! Valeur de U du champ montant d'ordre IG-1.

      DOUBLE PRECISION I1_REFL_L1ATM_SURF	! Valeur de I,Q et U du champ montant 
      DOUBLE PRECISION Q1_REFL_L1ATM_SURF	! à la surface de la réflexion
      DOUBLE PRECISION U1_REFL_L1ATM_SURF	! de la lumière atmosphérique 
					
						
C* Variables associées aux tests de sorties de boucles :

      DOUBLE PRECISION Z1      		! Variable seuil pour les tests d'arrêt.
      
      DOUBLE PRECISION Z1_TOA_FOND	! Variable seuil pour les tests d'arrêt.
      					! Test sur luminance émergeant au TOA
					! et descendante au fond de mer

      DOUBLE PRECISION Z1_SURF		! Variable seuil pour les tests d'arrêt.
      					! Test sur luminance en surface
					! montante et descendante 
					
      DOUBLE PRECISION Z1_SSURF		! Variable seuil pour les tests d'arrêt.
      					! Test sur luminance sous la surface
					! montante et descendante 

C*  - Boucle sur l'ordre de la décomposition en séries de Fourier :
	! Tableaux de sommation des ordres S de la décomposition
        ! en séries de Fourier, en fonction de l'angle mu.
      DOUBLE PRECISION I4(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      DOUBLE PRECISION Q4(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      DOUBLE PRECISION U4(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)

        ! Tableaux de sommation des ordres S de la décomposition
        ! en séries de Fourier, avec permutation du signe des ordres S,
	! en fonction de l'angle mu.
      DOUBLE PRECISION I5(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      DOUBLE PRECISION Q5(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      DOUBLE PRECISION U5(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      
      										
C*  - Boucle sur les diffusions :
	!    Fonctions pour le développement en séries géométriques,
	!    champ émergeant au TOA (mu > 0) et descendant au sol (mu < 0).
	!    Les fonctions sont établies pour un ordre S du développement
	!    en séries de Fourier, en fonction de l'angle mu et du niveau du profil.	
      DOUBLE PRECISION I3(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)		
                                ! Paramètre de Stokes I pour le cumul des
      				! ordres de diffusion précédant IG en cours.
      DOUBLE PRECISION Q3(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
      				! Paramètre de Stokes Q.
      DOUBLE PRECISION U3(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
      				! Paramètre de Stokes U.
      

	! Fonctions pour le test de convergence en séries géométrique.
	! Les fonctions sont établies pour un ordre S du développement
	! en séries de Fourier, en fonction de l'angle mu.	
	
	   ! --> champ émergeant au TOA (mu > 0) 
	   !     et descendant au fond de mer (mu < 0)
	   ! --> Cumul des ordres de diffusion
      DOUBLE PRECISION I3_TOA_FOND(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      DOUBLE PRECISION Q3_TOA_FOND(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      DOUBLE PRECISION U3_TOA_FOND(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)

	   ! --> champ au niveau de la surface : 
	   !     montant (mu > 0) et descendant (mu < 0)
	   ! --> Cumul des ordres de diffusion	
      DOUBLE PRECISION I3_SURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      DOUBLE PRECISION Q3_SURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      DOUBLE PRECISION U3_SURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)

	   ! --> champ sous la surface
	   !     montant (mu > 0) et descendant (mu < 0)
	   ! --> Cumul des ordres de diffusion	
      DOUBLE PRECISION I3_SSURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      DOUBLE PRECISION Q3_SSURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      DOUBLE PRECISION U3_SSURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
    

      DOUBLE PRECISION A1_TOA_FOND(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
       				! Paramètre de Stokes I pour l'ordre IG-2 
				! du champ émergeant au TOA (mu>0),
				! ou descendant au niveau du fond (mu<0). 
      DOUBLE PRECISION B1_TOA_FOND(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes Q pour l'ordre IG-2.
      DOUBLE PRECISION C1_TOA_FOND(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes U pour l'ordre IG-2.

      DOUBLE PRECISION D1_TOA_FOND(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes I pour l'ordre IG-1.	
      DOUBLE PRECISION E1_TOA_FOND(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes Q pour l'ordre IG-1.
      DOUBLE PRECISION F1_TOA_FOND(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      			  	! Paramètre de Stokes U pour l'ordre IG-1.

      DOUBLE PRECISION G1_TOA_FOND(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes I pour l'ordre IG.	
      DOUBLE PRECISION H1_TOA_FOND(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes Q pour l'ordre IG.
      DOUBLE PRECISION P1_TOA_FOND(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      			  	! Paramètre de Stokes U pour l'ordre IG.
      


      DOUBLE PRECISION A1_SURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
       				! Paramètre de Stokes I pour l'ordre IG-2 
				! du champ en surface,
				! montant (mu>0) ou descendant (mu<0). 
      DOUBLE PRECISION B1_SURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes Q pour l'ordre IG-2.
      DOUBLE PRECISION C1_SURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes U pour l'ordre IG-2.

      DOUBLE PRECISION D1_SURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes I pour l'ordre IG-1.	
      DOUBLE PRECISION E1_SURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes Q pour l'ordre IG-1.
      DOUBLE PRECISION F1_SURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      			  	! Paramètre de Stokes U pour l'ordre IG-1.

      DOUBLE PRECISION G1_SURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes I pour l'ordre IG.	
      DOUBLE PRECISION H1_SURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes Q pour l'ordre IG.
      DOUBLE PRECISION P1_SURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      			  	! Paramètre de Stokes U pour l'ordre IG.



      DOUBLE PRECISION A1_SSURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
       				! Paramètre de Stokes I pour l'ordre IG-2 
				! du champ sous la surface,
				! montant (mu>0) ou descendant (mu<0). 
      DOUBLE PRECISION B1_SSURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes Q pour l'ordre IG-2.
      DOUBLE PRECISION C1_SSURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes U pour l'ordre IG-2.

      DOUBLE PRECISION D1_SSURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes I pour l'ordre IG-1.	
      DOUBLE PRECISION E1_SSURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes Q pour l'ordre IG-1.
      DOUBLE PRECISION F1_SSURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      			  	! Paramètre de Stokes U pour l'ordre IG-1.

      DOUBLE PRECISION G1_SSURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes I pour l'ordre IG.	
      DOUBLE PRECISION H1_SSURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes Q pour l'ordre IG.
      DOUBLE PRECISION P1_SSURF(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      			  	! Paramètre de Stokes U pour l'ordre IG.
					     
      DOUBLE PRECISION ISUNDIR_DOWN_SSURF_MP !Valeur de EI ECLAIREMENT solaire 
      					     !direct (divisé par PI) transmis
      					     !sous la surface pour mer plate
      DOUBLE PRECISION QSUNDIR_DOWN_SSURF_MP !Valeur de EQ éclairement solaire 
      					     !direct (divisé par PI) transmis
      					     !sous la surface pour mer plate
      

      DOUBLE PRECISION ISUNDIR_DOWN_SSURF_VAG_K !Variable intermédiaire de calcul de
      					        !EI éclairement solaire direct transmis
      					        !sous la surface vers une direction 
					        !donnée K pour une mer agitée.
      DOUBLE PRECISION QSUNDIR_DOWN_SSURF_VAG_K !Variable intermédiaire de calcul de
      					        !EQ éclairement solaire direct transmis 
      					        !sous la surface vers une direction 
					        !donnée K pour une mer agitée.
      DOUBLE PRECISION USUNDIR_DOWN_SSURF_VAG_K !Variable intermédiaire de calcul de
      					        !EU éclairement solaire direct transmis 
      					        !sous la surface vers une direction 
					        !donnée K pour une mer agitée.
					      
      DOUBLE PRECISION D1(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes I pour l'ordre IG-1,
				! sur tout le profil.	
      DOUBLE PRECISION E1(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes Q pour l'ordre IG-1.
      DOUBLE PRECISION F1(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      			  	! Paramètre de Stokes U pour l'ordre IG-1.

      DOUBLE PRECISION G1(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes I pour l'ordre IG.	
      DOUBLE PRECISION H1(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes Q pour l'ordre IG.
      DOUBLE PRECISION P1(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      			  	! Paramètre de Stokes U pour l'ordre IG.  
	

C----

      DOUBLE PRECISION RHO_SURF	! (E)  Albédo de la surface (composante lambertienne)
      DOUBLE PRECISION RHO_BOT	! (E)  Albédo du fond marin (composante lambertienne)     
         

      DOUBLE PRECISION IINC(1:CTE_OS_NBMU_MAX)	! Valeur du champ I incident à la surface 
      						! au niveau 0- ou 0+ (pour ordres IG et IS).
						! Champ montant ou descendant selon le cas.
      DOUBLE PRECISION QINC(1:CTE_OS_NBMU_MAX)	! Idem pour le champ Q.
      DOUBLE PRECISION UINC(1:CTE_OS_NBMU_MAX)	! Idem pour le champ U.

      DOUBLE PRECISION IDEV(1:CTE_OS_NBMU_MAX)	! Valeur du champ I dévié à la surface
      						! au niveau 0- ou 0+ (pour ordres IG et IS).
						! Champ montant ou descendant selon le cas.
      DOUBLE PRECISION QDEV(1:CTE_OS_NBMU_MAX)	! Idem pour le champ Q.
      DOUBLE PRECISION UDEV(1:CTE_OS_NBMU_MAX)	! Idem pour le champ U.
      
C----
      DOUBLE PRECISION FLUX_UP_SURF_DIFF	! Flux montant diffus en surface (niveau 0+)
      DOUBLE PRECISION FLUX_UP_SURF_DIR		! Flux montant direct au niveau 0+
      DOUBLE PRECISION FLUX_UP_SURF_TOT		! Flux montant total (diffus + direct)
      DOUBLE PRECISION FLUX_DOWN_SURF_DIFF	! Flux descendant diffus 
      						! au niveau de la surface (0+)
      DOUBLE PRECISION FLUX_DOWN_SURF_DIR	! Flux descendant direct au niveau 0+
      DOUBLE PRECISION FLUX_DOWN_SURF_TOT	! Flux descendant total (diffus + direct)
      						! au niveau de la surface (0+) 
      DOUBLE PRECISION FLUX_TOT_SURF_DIFF	! Flux diffus total (up+down) au niveau 0+

      DOUBLE PRECISION FLUX_UP_TOA_DIFF		! Flux montant diffus au niveau TOA
      DOUBLE PRECISION FLUX_UP_TOA_DIR		! Flux montant direct au niveau TOA
      DOUBLE PRECISION FLUX_UP_TOA_TOT		! Flux montant total (diffus + direct)
      DOUBLE PRECISION FLUX_DOWN_TOA_DIR	! Flux descendant direct au niveau TOA

	   
      DOUBLE PRECISION FLUX_UP_SSURF		! Flux montant sous la surface (0-).
      DOUBLE PRECISION FLUX_DOWN_SSURF_DIFF	! Flux descendant diffus sous la surface
      						! (niveau 0-).	    
      DOUBLE PRECISION FLUX_DOWN_SSURF_DIR	! Flux descendant direct au niveau 0-
      DOUBLE PRECISION FLUX_DOWN_SSURF_DIR_MP	! Flux descendant direct au niveau 0- 
      						! pour mer plate.
      DOUBLE PRECISION FLUX_DOWN_SSURF_TOT	! Flux descendant total (diffus + direct)
      						! au niveau de la surface (0-). 
      DOUBLE PRECISION FLUX_TOT_SSURF_DIFF	! Flux diffus total (up+down) au niveau 0-
	    
      DOUBLE PRECISION FLUX_DOWN_FOND_DIR       ! Flux descendant direct transmis 
      						! au fond de mer.
    
      DOUBLE PRECISION FLUX_SPHE_UP_SSURF		! Flux sphérique montant
      							! sous la surface (niveau 0-)
      DOUBLE PRECISION FLUX_SPHE_DOWN_SSURF_DIFF	! Flux sphérique descendant diffus
      							! sous la surface (niveau 0-)
      DOUBLE PRECISION FLUX_SPHE_DOWN_SSURF_DIR		! Flux sphérique descendant diret
      							! sous la surface (niveau 0-)
  
      DOUBLE PRECISION FLUX_SPHE_TOT_SSURF_DIFF		! Flux sphérique diffus total 
      							! (up+down) au niveau 0-

	! Paramètres pour le calcul de la réflexion du flux solaire direct en surface
      DOUBLE PRECISION IND2			! Carré de l'indice de réfraction air/mer
      DOUBLE PRECISION Xsun				
      DOUBLE PRECISION RLsun   			! Coefficient de réflexion parallèle.
      DOUBLE PRECISION RRsun   			! Coefficient de réflexion perpendiculaire.
      DOUBLE PRECISION F11sun  			! Elément de la matrice de Fresnel en 
      			        		! réflexion avec pour incidence la direction
			       			! d'éclairement solaire.
		
	! Flux descendant, normalisé par rapport à l'éclairement incident au TOA Es 
	! (avec Es = PI).
      DOUBLE PRECISION PROFIL_FLUX_DOWN_DIFF(0:NT_TOT)  ! Profil du flux descendant diffus, 
      							! normalisé par Es
      DOUBLE PRECISION PROFIL_FLUX_DOWN_DIR(0:NT_TOT)   ! Profil du flux descendant direct,  
      							! normalisé par Es
      DOUBLE PRECISION PROFIL_FLUX_DOWN_TOT(0:NT_TOT)   ! Profil du flux descendant total 
      							! (direct+diffus), normalisé par Es
      DOUBLE PRECISION PROFIL_FLUX_UP_DIFF(0:NT_TOT)    ! Profil du flux montant diffus, 
      							! normalisé par Es
      DOUBLE PRECISION PROFIL_FLUX_UP_DIR(0:NT_TOT)     ! Profil du flux montant direct,  
      							! normalisé par Es
      DOUBLE PRECISION PROFIL_FLUX_UP_TOT(0:NT_TOT)     ! Profil du flux montant total 
      							! (direct+diffus), normalisé par Es
      

	    
      DOUBLE PRECISION FLUX_DOWN_SURF_IGM1	! Flux d'ordre IG-1 descendant diffus 
      						! au niveau de la surface (0+)  
						! (Flux / PI précisément)	

      DOUBLE PRECISION FLUX_DOWN_FOND_IGM1	! Flux d'ordre IG-1 descendant diffus 
      						! au fond de mer (Flux / PI précisément)

      DOUBLE PRECISION I_REFL_BOT 		!Valeur de l'intensité de l'éclairement 
      						!solaire réfléchi au fond de mer
      DOUBLE PRECISION I_REFL_SURF 		!Valeur de l'intensité de l'éclairement 
      						!solaire réfléchi en surface


      INTEGER*4 J       ! Indice des tableaux d'angles 
      INTEGER*4 K	! Indice des tableaux de matrice de phase  
      INTEGER*4 I	! Niveau des profils
      INTEGER*4 ISEA    ! Indice du profil marin
      
      INTEGER*4 IBORM	! Ordre maximal du développement en séries de Fourier
      			! en cours de traitement :
			!   _ 2 pour une atmosphère & mer purement moléculaire,
			!   _ OS_NB en présence d'aérosols ou d'hydrosols.
 				
  
      INTEGER*4 IMAT_SURF !  (E) Option de BRDF / BPDF de surface :
      			  !   0 : mer plate.
			  !   1 : mer agitée.
			  
      INTEGER*4 IER       ! (S) code d'erreur =0 si pas d'erreur, =-1 sinon

     
      INTEGER*4 NBMU      !  (E) Nombre d'angles effectifs des simulations      


      INTEGER*4 OS_NB 	  ! (E) Ordre des développements des fonctions de phase 
			  ! en Polynômes de Legendre.      
				
      INTEGER*4 N0	  ! (E) Numéro de mu de Gauss pour l'angle
      			  ! zénithal solaire dans l'air.
			  ! (angle complémentaire : mus = -rmu(N0))

      INTEGER*4 M0	  ! (E) Numéro de mu de Gauss pour l'angle
      			  ! zénithal solaire dans l'eau (transmission mer plate).
			  ! (angle complémentaire : musw = -rmu(M0))
			  
      INTEGER*4 JK	  ! Numéro de stockage de mus dans l'air dans le tableau RMU
      			  ! (JK=-N0 ==> Correspond au mus vrai < 0).

      INTEGER*4 JKW	  ! Numéro de stockage de mus dans l'eau dans le tableau RMU
      			  ! (JKW=-M0 ==> Correspond au musw vrai < 0).
			

      INTEGER*4 IS	! Ordre en cours du développement en séries 
                        ! de Fourier associé à la matrice de phase
			   
      INTEGER*4 IG	! Ordre d'interaction en cours (diffusion + réflexion).
     
      INTEGER*4 IGMAX	! (E)  Ordre maximal de diffusion.
      
  							      
C---- Nom des fichiers utilisés (chemin complet)
      CHARACTER*CTE_LENFIC2 FICRAA 	! (E)  Fichier Matrice de réflexion air / air.
      CHARACTER*CTE_LENFIC2 FICTAW 	! (E)  Fichier Matrice de transmission air / eau.
      CHARACTER*CTE_LENFIC2 FICRWW 	! (E)  Fichier Matrice de réflexion eau / eau.
      CHARACTER*CTE_LENFIC2 FICTWA 	! (E)  Fichier Matrice de transmission eau / air.  

      CHARACTER*CTE_LENFIC2 FIC_FLUX  	! (S) Fichier des profils de flux
      
      CHARACTER*CTE_LENFIC2 FICSOS_LOG  ! (S) Fichier trace 
						
      CHARACTER*CTE_LENFIC2 FICSOS_RES_BIN  	! (S) Fichier résultat binaire
      						!     en sortie des calculs SOS	


C---- Variables logiques
      LOGICAL TRACE     ! = vrai si écriture dans le fichier trace    
      


      
C* Initialisation du code d'erreur
C------------------------------------------------------
      IER=0

C* Positionnement du boolean TRACE du fichier Trace 
C------------------------------------------------------
      IF (FICSOS_LOG.EQ.'NO_LOG_FILE') THEN
	 TRACE=.FALSE.
      ELSE
         TRACE=.TRUE.
      ENDIF


C* Information dans le fichier Trace d'un éventuel usage 
C* du code en Mode expert spécifique
C------------------------------------------------------
      IF (EXPERT_MODE_FORCED_FATM_NULL.AND.TRACE) THEN 
      	 !annulation de la diffusion atmosphérique
         WRITE (IDLOG,*,ERR=1010) " "
         WRITE (IDLOG,*,ERR=1010) 
     &	 "  ********************************************** ",
     &	 "  WARNING : ATMOS SOURCE FUNCTION FORCED TO NULL ",
     &	 "  ********************************************** "
         WRITE (IDLOG,*,ERR=1010) " "
      ENDIF
     
      IF (EXPERT_MODE_FORCED_FSEA_NULL.AND.TRACE) THEN 
      	!annulation de la diffusion marine
         WRITE (IDLOG,*,ERR=1010) " "
         WRITE (IDLOG,*,ERR=1010) 
     &	 "  ******************************************** ",
     &	 "  WARNING : SEA SOURCE FUNCTION FORCED TO NULL ",
     &	 "  ******************************************** "
         WRITE (IDLOG,*,ERR=1010) " "
      ENDIF
      
      
C* Prise en compte du facteur de dépolarisation pour les paramètres
C* de la fonction de phase moléculaire atmosphérique et maritime.
C---------------------------------------------------------------------

      !Molécules atmosphériques
      AAA=2.*(1-CTE_MDF_AIR)/(2+CTE_MDF_AIR)

      BETA0_MOL_AIR=1.
      BETA2_MOL_AIR=0.5*AAA
      GAMMA2_MOL_AIR=-AAA*DSQRT(1.5D+00)
      ALPHA2_MOL_AIR=3.*AAA

      IF (TRACE) THEN
        WRITE (IDLOG,*,ERR=1010) "MOLECULAR PHASE MATRIX "
	 WRITE (IDLOG,*,ERR=1010) "**********************"
	 WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) " In the air : "
	 WRITE (IDLOG,*,ERR=1010) 
     &	 "   - Depolarization factor: ", CTE_MDF_AIR 
	 WRITE (IDLOG,*,ERR=1010)  
     &	 "   - Coef F for the depolarization correction : ", AAA   
        WRITE (IDLOG,*,ERR=1010)"   - BETA0 : ", BETA0_MOL_AIR
	 WRITE (IDLOG,*,ERR=1010)"   - BETA2 : ", BETA2_MOL_AIR
	 WRITE (IDLOG,*,ERR=1010)"   - GAMMA2 : ", GAMMA2_MOL_AIR
	 WRITE (IDLOG,*,ERR=1010)"   - ALPHA2 : ", ALPHA2_MOL_AIR 
	 WRITE (IDLOG,*,ERR=1010) " "	
      ENDIF !Fichier Trace
 
      
      !Molécules océaniques
      AAA=2.*(1-CTE_MDF_SEA)/(2+CTE_MDF_SEA)
      
      BETA0_MOL_SEA=1.
      BETA2_MOL_SEA=0.5*AAA
      GAMMA2_MOL_SEA=-AAA*DSQRT(1.5D+00)
      ALPHA2_MOL_SEA=3.*AAA

      IF (TRACE) THEN  
	 WRITE (IDLOG,*,ERR=1010) " In the sea : "
	 WRITE (IDLOG,*,ERR=1010) 
     &	 "   - Depolarization factor : ", CTE_MDF_SEA 
	 WRITE (IDLOG,*,ERR=1010)  
     &	 "   - Coef F for the depolarization correction : ", AAA   
        WRITE (IDLOG,*,ERR=1010)"   - BETA0 : ", BETA0_MOL_SEA
	 WRITE (IDLOG,*,ERR=1010)"   - BETA2 : ", BETA2_MOL_SEA
	 WRITE (IDLOG,*,ERR=1010)"   - GAMMA2 : ", GAMMA2_MOL_SEA
	 WRITE (IDLOG,*,ERR=1010)"   - ALPHA2 : ", ALPHA2_MOL_SEA 
	 WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) " "	
      ENDIF !Fichier Trace
      
            
C* Ouverture des fichiers de matrice d'interface
C---------------------------------------------------------------------

      IF (IMAT_SURF.EQ.1) THEN

         ! Ouverture du fichier RAA
         ! ------------------------ 
         OPEN(UNIT=IDRAA,FILE=FICRAA,FORM='UNFORMATTED',
     &        STATUS='OLD',ERR=1050)      
      
         ! Ouverture du fichier TAW
         ! ------------------------
         OPEN(UNIT=IDTAW,FILE=FICTAW,FORM='UNFORMATTED',
     &        STATUS='OLD',ERR=1052)       

         ! Ouverture du fichier RWW
         ! ------------------------
         OPEN(UNIT=IDRWW,FILE=FICRWW,FORM='UNFORMATTED',
     &        STATUS='OLD',ERR=1054)      
      
         !Ouverture du fichier TWA
         ! ----------------------------------   
         OPEN(UNIT=IDTWA,FILE=FICTWA,FORM='UNFORMATTED',
     &        STATUS='OLD',ERR=1056)      
              
      ENDIF  !Fin cas avec matrices de surface
      


C* Ouverture du fichier résultat des OS
C  Si ce fichier existe déjà, il est écrasé par le nouveau fichier
C---------------------------------------------------------------------
      OPEN(UNIT=IDFICOS,FILE=FICSOS_RES_BIN,FORM='UNFORMATTED',
     &     ERR=1060)
      
      IF (TRACE) THEN  
	 WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) " Result binary file SOS : ",
     &	  			  FICSOS_RES_BIN
	 WRITE (IDLOG,*,ERR=1010) " "	
      ENDIF !Fichier Trace      

C* Définition de mus (cosinus de l'angle zénithal solaire)
C* Stockage de mus pour l'indice 0 du tableau des mu de GAUSS (RMU).
C----------------------------------------------------------
      !mus dans l'air (<0)
      TAB=-RMU(N0)
      JK=-N0
      
      !mus dans l'eau
      TABW = -DSQRT(1.- (1.-TAB*TAB)/(SEA_IND*SEA_IND))     
      JKW=-M0
	
      	
      IF (TRACE) THEN  
	 WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) 
     &	    " Solar incidence angle in the air :",
     &      DACOS(TAB)*180.D+00/INCTE_PI,
     &      DACOS(-TAB)*180.D+00/INCTE_PI
	 WRITE (IDLOG,*,ERR=1010) 
     &	    " Cosine of the solar incidence angle in the air :",TAB
        WRITE (IDLOG,*,ERR=1010) 
     &	    " Cosine of the solar incidence angle in the sea :",TABW
        WRITE (IDLOG,*,ERR=1010) 
     &	    " Solar incidence angle in the sea water :",
     &      DACOS(TABW)*180.D+00/INCTE_PI,
     &      DACOS(-TABW)*180.D+00/INCTE_PI
	 WRITE (IDLOG,*,ERR=1010) " "	
      ENDIF !Fichier Trace      


C* Stockage de l'ep opt totale de l'atmosphère (ajustée à la troncature)
C----------------------------------------------------------
      ATM_TAUEXT_TR_TOT = ATM_TAUEXT_TR(CTE_NT_ATM)   
  
C* Atténuation du faisceau solaire direct dans l'atmosphère (divisée par 4).
C----------------------------------------------------------
      DO I=0,CTE_NT_ATM
         CH_ATM(I)=DEXP(ATM_TAUEXT_TR(I)/TAB)/4.
      ENDDO


C* Atténuation du faisceau solaire direct dans la mer (divisée par 4).
C----------------------------------------------------  
      DO I=0,CTE_NT_SEA
      
         TAUSEA=SEA_TAUEXT_TR(I)
	 
	 !Direction transmission pour mer plate : 
	 !==> transmission surface -> niveau I dans la mer
	 CH_SEA_MUS(I) = DEXP(-TAUSEA/(-TABW))/4.
	 
	 !Transmission pour toutes les directions :
	 !==> transmission surface -> niveau I dans la mer
         DO J=1,NBMU
            CH_SEA(J,I) = DEXP(-TAUSEA/RMU(J))/4.
	 ENDDO
	 
      ENDDO
      
      IF (TRACE) THEN  
	 WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) 
     &	    " Attenuation /4 toward the solar incidence direction :"
        WRITE (IDLOG,*,ERR=1010) 
     &	    "           level      Opt_thick    Attenuation"
	 DO I=0,CTE_NT_ATM
	    WRITE (IDLOG,30,ERR=1010) '    Atm : ', 
     &	                             I, ATM_TAUEXT_TR(I), 4*CH_ATM(I)
	 ENDDO
	 CH_ATM_TOT=CH_ATM(CTE_NT_ATM)
        WRITE (IDLOG,*,ERR=1010) "           level     Opt_thick"//
     &   "     Attenuation   Tot_attenuation"
	 DO I=0,CTE_NT_SEA
	    WRITE (IDLOG,31,ERR=1010) '    Sea : ', 
     &	                  	  I, SEA_TAUEXT_TR(I), 4*CH_SEA_MUS(I),
     &				  CH_SEA_MUS(I)*CH_ATM_TOT*16
	 ENDDO	 
	 WRITE (IDLOG,*,ERR=1010) " "	
      ENDIF !Fichier Trace  
      
       	
C* Initialisation des tableaux pour le test d'arrêt de la décomposition
C* en séries de Fourier.
C-----------------------------------------------------------------------
      DO 22 J=-NBMU,NBMU
         Q4(J)=0.
         U4(J)=0.
         I4(J)=0.

         Q5(J)=0.
         U5(J)=0.
         I5(J)=0.
 22   CONTINUE
 
 
C--------------------------------------------------------------
C
C       ******  DECOMPOSITION EN SERIE DE FOURIER   ******
C
C--------------------------------------------------------------

      IF (TRACE) THEN
         WRITE (IDLOG,*,ERR=1010) " " 
	  WRITE (IDLOG,*,ERR=1010) "FOURIER SERIES EXPANSION"
	  WRITE (IDLOG,*,ERR=1010) "************************"
	  WRITE (IDLOG,*,ERR=1010) " " 
      ENDIF !Fichier Trace  

      SIGN=-1.
	
C* Boucle sur les ordres de la décomposition en séries de Fourier
C------------------------------------------------------
      DO 24 IS=0,IBORM
      		
         IG=1
	  	 
         SIGN=-SIGN
 
C* Fichier trace 
C------------------------------------------------------  
         IF (TRACE) THEN
            WRITE(IDLOG,*,ERR=1010) 
     &       ' Fourier series expansion order: IS = ', IS
            WRITE(IDLOG,*,ERR=1010) 
     &       ' -----------------------------------------------'
            WRITE(IDLOG,*,ERR=1010) ' '	   
         ENDIF

C* Suppression du coefficient moléculaire BETA0 pour le calcul
C* de la matrice de phase de diffusion moléculaire pour les
C* ordres IS > 0.
C--------------------------------------------------------------
         IF(IS.GT.0) THEN
	    BETA0_MOL_AIR=0.
	    BETA0_MOL_SEA=0.
         ENDIF		

C* Initialisation des tableaux donnant les paramètres I, Q, U 
C* pour l'ordre IS de la décomposition en séries de Fourier.
C* (sommation du cumul des diffusions)
C-------------------------------------------------------------
         DO J=-NBMU,NBMU
	    DO I=0,NT_TOT
               I3(I,J)=0.
               Q3(I,J)=0.
               U3(I,J)=0.
	    ENDDO  ! Fin boucle sur niveau I
         ENDDO ! Fin boucle sur angle J
   

   
C* Lecture des éléments des matrices d'interface pour l'ordre IS en cours.
C------------------------------------------------------------------------
         IF (IMAT_SURF.EQ.1) THEN
C*      Lecture des fichiers qui ont été enregistrés avec des 
C*      bornes de tableaux limitées au dimensionnement utile -NBMU:NBMU         
	     READ(IDRAA,ERR=1051),
     &	       ((RAA11(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((RAA12(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((RAA13(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((RAA21(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((RAA22(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((RAA23(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((RAA31(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((RAA32(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((RAA33(I,J),I=1,NBMU),J=1,NBMU)  

	     READ(IDTAW,ERR=1053),
     &	       ((TAW11(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((TAW12(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((TAW13(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((TAW21(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((TAW22(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((TAW23(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((TAW31(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((TAW32(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((TAW33(I,J),I=1,NBMU),J=1,NBMU)  
     
	     READ(IDRWW,ERR=1055),
     &	       ((RWW11(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((RWW12(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((RWW13(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((RWW21(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((RWW22(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((RWW23(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((RWW31(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((RWW32(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((RWW33(I,J),I=1,NBMU),J=1,NBMU)  
     
	     READ(IDTWA,ERR=1057),
     &	       ((TWA11(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((TWA12(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((TWA13(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((TWA21(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((TWA22(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((TWA23(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((TWA31(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((TWA32(I,J),I=1,NBMU),J=1,NBMU),
     &	       ((TWA33(I,J),I=1,NBMU),J=1,NBMU)                  
	 ENDIF  !IMAT_SURF=1
	   
   
   
   
C* Appel de la routine de calcul des noyaux de diffusion pour 
C* l'ordre IS de la décomposition en séries de Fourier.
C* Calcul pour les aérosols, le phytoplancton et les sédiments
C--------------------------------------------------------------

C* ----> Noyaux de diffusion des aérosols à l'ordre IS
         CALL OSOAA_NOYAUX(IS,NBMU,RMU,OS_NB,
     &	 		   ALPHA_AER,BETA_AER,GAMMA_AER,ZETA_AER,
     &                     XPL,XRL,XTL,
     &                     BP_AER,GR_AER,GT_AER,
     & 			   ARR_AER,ART_AER,ATT_AER)

C* ----> Noyaux de diffusion du phytoplancton à l'ordre IS
         CALL OSOAA_NOYAUX(IS,NBMU,RMU,OS_NB,
     &	 		   ALPHA_CHL,BETA_CHL,GAMMA_CHL,ZETA_CHL,
     &                     XPL,XRL,XTL,
     &                     BP_CHL,GR_CHL,GT_CHL,
     & 			   ARR_CHL,ART_CHL,ATT_CHL)
     
C* ----> Noyaux de diffusion des sédiments à l'ordre IS
         CALL OSOAA_NOYAUX(IS,NBMU,RMU,OS_NB,
     &	 		   ALPHA_MLP,BETA_MLP,GAMMA_MLP,ZETA_MLP,
     &                     XPL,XRL,XTL,
     &                     BP_MLP,GR_MLP,GT_MLP,
     & 			   ARR_MLP,ART_MLP,ATT_MLP)
     
        
  	

C--------------------------------------------------------------
C
C   ******  CALCULS DE DIFFUSION ET REFLEXION PRIMAIRE   ******
C
C--------------------------------------------------------------
    
         IF (TRACE) THEN
            WRITE(IDLOG,*,ERR=1010) 
     &       '     Scattering order : IG = ',IG
            WRITE(IDLOG,*,ERR=1010) 
     &       '     ------------------------------'
            WRITE(IDLOG,*,ERR=1010) ' '	   
         ENDIF
	
	 
	 
C* Calcul de la fonction source de diffusion primaire pour l'atmosphère : 
C* ====> I2_ATM, Q2_ATM, U2_ATM
C-----------------------------------------------------------------------
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) 
     &	       " Calculation of the atmospheric source function "
	 ENDIF
	 
         CALL OSOAA_FSOURCE_ATM_ORDRE1(IS,NBMU,JK,
     &	 			           ATM_PCAER_TR,ATM_PCRAY_TR,
     &                                 BETA0_MOL_AIR,BETA2_MOL_AIR,
     &				           GAMMA2_MOL_AIR,
     &                                 XPL,XRL,XTL,
     &                                 BP_AER,GR_AER,GT_AER,CH_ATM,
     &                                 I2_ATM,Q2_ATM,U2_ATM)       

	 !******* Traces d'expertise ***********************************
         IF (TRACE_DEBUG) THEN
	    WRITE(IDLOG,*,ERR=1010) " "
	    WRITE(IDLOG,*,ERR=1010) 
     &	       "Atmos. source function order 1 : IS = ",IS
	    WRITE(IDLOG,*,ERR=1010) " "
	    DO I=0,CTE_NT_ATM
	       IF ((I.EQ.0).OR.(I.EQ.2).OR.(I.EQ.13).OR.(I.EQ.24)
     &	            .OR.(I.EQ.26)) THEN
	          WRITE(IDLOG,*,ERR=1010) "Atmos levl I : ",I
	          WRITE(IDLOG,*,ERR=1010) 
     &	       "  I     K      RMU          I2          Q2          U2"
	          DO K=-NBMU,NBMU,4
	             WRITE(IDLOG,402,ERR=1010) 
     &			I,K,RMU(K),I2_ATM(I,K),Q2_ATM(I,K),U2_ATM(I,K)
	          ENDDO
	       ENDIF  
	    ENDDO  
	    WRITE(IDLOG,*,ERR=1010) " "
	 ENDIF
	 !**************************************************************


	 		
C* Calcul de la fonction source de diffusion primaire pour l'océan : 
C* ====> I2_SEA, Q2_SEA, U2_SEA
C-------------------------------------------------------------------    
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) 
     &	    " Calculation ot the air / sea transmitted direct"//
     &     " solar radiance"
     	    WRITE(IDLOG,*,ERR=1010) 
     &	    " Calculation of the marine source function of order 1"
           WRITE (IDLOG,*,ERR=1010) " "
	 ENDIF

         !Initialisation à zéro du champ marin d'ordre 0 (toutes directions confondues)
	  DO I=0,CTE_NT_SEA
	    DO K=-NBMU,NBMU
	       I1_SEA(I,K)=0.	
	       Q1_SEA(I,K)=0.	
	       U1_SEA(I,K)=0.	
	    ENDDO
	 ENDDO

	  
         IF (IMAT_SURF.EQ.1) THEN
	 
	    !Cas mer agitée	 
	 
	    !On a un champs solaire primaire transmis sur une gamme de directions
	    !==> on peut utiliser la fonction OSOAA_FSOURCE_SEA_ORDREIG en 
	    !    renseignant comme champ d'ordre 0 le champ solaire direct transmis
	    !    et atténué pour chaque niveau du profil marin.
	    !==> Numériquement cela suppose avoir une description assez fine 
	    !    du découpage en angles pour le cas pic très pointus (faible vent).
	    
	    
	    
	    !Calcul du champ marin d'ordre 0 descendant au niveau de la surface (niveau 0-)
	    CH_ATM_TOT=4.*CH_ATM(CTE_NT_ATM)   !Atténuation TOA -> surface 
	    				       !pour la direction d'incidence solaire
	    
	    
            DO K=1,NBMU	   !K direction de transmission

	       		   !Direction d'incidence = solaire direct dans atmosphère = N0
				
	       		   !CH_ATM_TOT => Eclairement solaire incident en surface 
			   !(non polarisé, Es=PI, facteur multiplicatif inclus dans 
		  	   !l'expression de TAW)	
			   
			   !On applique la matrice de transmission à l'éclairement direct 
			   !incident pour avoir l'éclairement direct transmis
			   !On n'oublie pas de diviser par MU (voir spécifications)
			   !et de prendre en compte la transmission dans la mer
				
	       ISUNDIR_DOWN_SSURF_VAG_K = TAW11(N0,K) * CH_ATM_TOT 		
	       QSUNDIR_DOWN_SSURF_VAG_K = TAW21(N0,K) * CH_ATM_TOT 		
	       USUNDIR_DOWN_SSURF_VAG_K = TAW31(N0,K) * CH_ATM_TOT 		
	       		
      
	       DO I=0,CTE_NT_SEA
	          CH_SEA_I = 4.*CH_SEA(K,I)		!Atténuation au niveau I marin	    
	          I1_SEA(I,-K)=ISUNDIR_DOWN_SSURF_VAG_K*CH_SEA_I/RMU(K)
	          Q1_SEA(I,-K)=QSUNDIR_DOWN_SSURF_VAG_K*CH_SEA_I/RMU(K)
	          U1_SEA(I,-K)=USUNDIR_DOWN_SSURF_VAG_K*CH_SEA_I/RMU(K)
	       ENDDO	
    		    
	    ENDDO !Fin boucle sur K
	  
	  
	    !Calcul du flux direct transmis au niveau 0-
	    
 
	    IF (IS.EQ.0) THEN
	 
	       FLUX_DOWN_SSURF_DIR=0. 
	       FLUX_DOWN_FOND_DIR=0.
	       
	       DO J=1,NBMU
	       
	          VAL = GA(J)*I1_SEA(0,-J)*RMU(J)
	          FLUX_DOWN_SSURF_DIR = FLUX_DOWN_SSURF_DIR + VAL
	          FLUX_DOWN_FOND_DIR = FLUX_DOWN_FOND_DIR 
     &	   	    + VAL*4.*CH_SEA(J,CTE_NT_SEA)     
	       ENDDO

               FLUX_DOWN_SSURF_DIR = 
     &	   	       FLUX_DOWN_SSURF_DIR * 2.*INCTE_PI

               FLUX_DOWN_FOND_DIR = 
     &	   	        FLUX_DOWN_FOND_DIR * 2.*INCTE_PI
      	    
	    ENDIF ! Si IS=0
	    
	    
	    
	    
	    !******* Traces d'expertise ********************************
            IF (TRACE_DEBUG) THEN
	       WRITE(IDLOG,*,ERR=1010) " "
	       WRITE(IDLOG,*,ERR=1010) 
     &	       " Transm. solar radiance just below the surface: IS=",
     &         IS
              WRITE(IDLOG,*,ERR=1010) 
     &	       "   K    MU       ANGLE     I           Q           U"
              II2=0.
	       QQ2=0.			
	       DO K=-NBMU,-1
	          WRITE(IDLOG,401,ERR=1010) K,RMU(K),
     &		          -DACOS(RMU(-K))*180.D+00/INCTE_PI,
     &			   I1_SEA(0,K),Q1_SEA(0,K),U1_SEA(0,K)
	          II2 = II2 +GA(K)*I1_SEA(0,K)
		   QQ2 = QQ2 +GA(K)*Q1_SEA(0,K)
	       ENDDO
	       WRITE(IDLOG,*,ERR=1010) " "
	       WRITE(IDLOG,*,ERR=1010) "Integrated values : ",II2,QQ2
	    ENDIF
	    !***********************************************************
	 	    
	    !Application du champ marin d'ordre 0 pour le calcul de la fonction 
	    !source marine d'ordre 1	    
	    CALL OSOAA_FSOURCE_SEA_ORDREIG(IS,NBMU,
     &	 		   SEA_PCRAY_TR,SEA_PC_CHL_TR,SEA_PC_MLP_TR,
     &                     BETA0_MOL_SEA,BETA2_MOL_SEA,
     &			   GAMMA2_MOL_SEA,ALPHA2_MOL_SEA,
     &                     XPL,XRL,XTL,
     &			   I1_SEA,Q1_SEA,U1_SEA,
     &                     BP_CHL,GR_CHL,GT_CHL,
     & 			   ARR_CHL,ART_CHL,ATT_CHL,
     &                     BP_MLP,GR_MLP,GT_MLP,
     & 			   ARR_MLP,ART_MLP,ATT_MLP,
     &                     GA,I2_SEA,Q2_SEA,U2_SEA)

         	   
         ELSE 
	    !Cas mer plate   

	    !Calcul de l'éclairement solaire direct transmis au passage air / mer : 
	    !====> composantes I = ISUNDIR_DOWN_SSURF_MP et Q = QSUNDIR_DOWN_SSURF_MP
	    !sous la surface (niveau 0-) pour une mer plate


	    !Affectation des valeurs incidentes du champ descendant en surface
            DO K=1,NBMU
 	       IINC(K) = 0.
 	       QINC(K) = 0.
 	       UINC(K) = 0.
 	    ENDDO
 	    IINC(N0) = 4.*CH_ATM(CTE_NT_ATM) !Eclairement solaire incident en surface 
	 				     !divisé par PI
	    				     !(non polarisé, Es=PI : non apparent car 
					     !on simplifie directement l'expression 
					     !de la fonction source avec ES/PI=1)
					     !Mono-directionnel : indice N0 (mu0 > 0)
							
 	    CALL  OSOAA_INTERF_MERPLATE("TAW",SEA_IND,RMU,NBMU,
     & 				        IINC,QINC,UINC,IDEV,QDEV,UDEV,
     &                                  .FALSE.,IER)

            IF (IER.NE.0) GOTO 1200   ! cas d'erreur   
 
	    
            !Récupération du champ descendant transmis à partir du tableau du champ dans 
	    !l'eau au niveau de la surface 
	    !(pour direction transmise : indice M0 (muw0 > 0) ).
	    ! --> La routine OSOAA_INTERF_MERPLATE applique un calcul pour luminance.
	    !     Pour retrouver l'analogie en éclairement, il faut :
	    !       - supprimer l'application du facteur indice^2 (propre au calcul 
	    !         en luminance) pour revenir à la matrice Tflux.
	    !       - selon l'expression d'un Flux = Eclairement * cos(angle incident)
	    !         on passe à l'expression d'une matrice de transmission de 
	    !         l'éclairement en la multipliant par le rapport des cosinus
	    !         des angles solaires incident et transmis. 
	    !     NB: On ne multiplie pas l'expression par Es (Es=PI)
	    !         car Es=PI est pris en compte dans les routines de calcul des
	    !         fonctions sources qui utilisent les grandeurs 
	    !         ISUNDIR_DOWN_SSURF_MP et QSUNDIR_DOWN_SSURF_MP
	    !
	    !  ==> On obtient ISUNDIR_DOWN_SSURF_MP = R * exp(-Tau/cosThetas)
	    !      avec R = ECL_DIR_0- / ECL_DIR_0+ le rapport des éclairements
	    !      solaire direct transmis sous l'eau et incident en surface.
	    !
	    !      Comme ECL_DIR_0+ = PI * exp(-Tau/cosThetas) à la surface
	    !      On a donc aussi : ISUNDIR_DOWN_SSURF_MP = ECL_DIR_0- / PI 
	    ISUNDIR_DOWN_SSURF_MP= IDEV(M0)*(TAB/TABW)/(SEA_IND*SEA_IND)
	    QSUNDIR_DOWN_SSURF_MP= QDEV(M0)*(TAB/TABW)/(SEA_IND*SEA_IND)
	   
            IF (TRACE) THEN
	        WRITE(IDLOG,*,ERR=1010) 
     &          '        Att. of direct solar flux: TOA to surface'//
     &          ' (level 0+) :', 4*CH_ATM(CTE_NT_ATM)
	        WRITE(IDLOG,*,ERR=1010)
     &          '        Solar irradiance on surface (0+) / PI :',
     &                   IINC(N0)
               WRITE(IDLOG,*,ERR=1010) 
     &          '        Solar irradiance just below surface (0-) :'
               WRITE(IDLOG,*,ERR=1010) 
     &          '           Direction : ', 
     &                      DACOS(-TABW)*180.D+00/INCTE_PI
               WRITE(IDLOG,*,ERR=1010) 
     &          '           Parameter I / PI: ', ISUNDIR_DOWN_SSURF_MP   
               WRITE(IDLOG,*,ERR=1010) 
     &          '           Parameter Q / PI: ', QSUNDIR_DOWN_SSURF_MP
               WRITE(IDLOG,*,ERR=1010) ' '
               WRITE(IDLOG,*,ERR=1010) 
     &          '        Ratio of solar irradiance 0- / 0+ :'
               WRITE(IDLOG,*,ERR=1010) 
     &          '           Parameter I : ',
     &          ISUNDIR_DOWN_SSURF_MP/IINC(N0)
               WRITE(IDLOG,*,ERR=1010) 
     &          '           Parameter Q : ',
     &          QSUNDIR_DOWN_SSURF_MP/IINC(N0)
               WRITE(IDLOG,*,ERR=1010) ' '
	    ENDIF
     	    	

            IF(IS.EQ.0) THEN
    	        !  ==> On a ISUNDIR_DOWN_SSURF_MP = R * exp(-Tau/cosThetas)
	    	!      avec R = ECL_DIR_0- / ECL_DIR_0+ le rapport des éclairements
	    	!      solaire direct transmis sous l'eau et incident en surface.
	   	!
	   	!      Comme ECL_DIR_0+ = PI * exp(-Tau/cosThetas) à la surface
	   	!      On a donc aussi : ISUNDIR_DOWN_SSURF_MP = ECL_DIR_0- / PI 
	    	!
	    	!      Le flux sous la surface est donc :
	    	!      Flux = ECL_DIR_0- * cos(ThetasWater)
	    	!           = ISUNDIR_DOWN_SSURF_MP * PI * cos(ThetasWater) 	    
	       FLUX_DOWN_SSURF_DIR_MP = 
     &	           ISUNDIR_DOWN_SSURF_MP*INCTE_PI*(-TABW)     
     
     
                ! ==> Flux au fond par simple atténuation directe du flus sous la surface
		!     selon la direction d'incidence solaire dans l'eau
               FLUX_DOWN_FOND_DIR = 
     &	           FLUX_DOWN_SSURF_DIR_MP *4.*CH_SEA_MUS(CTE_NT_SEA) 	     
	        
	        ! Affection à la variable commune mer plate / mer agitée
		FLUX_DOWN_SSURF_DIR = FLUX_DOWN_SSURF_DIR_MP	  
	    ENDIF
     
     
	    !-- Calcul de la fonction source d'ordre 1 : 
	    !          - source directe = éclairement solaire transmis en composante I et Q
	    !			(ISUNDIR_DOWN_SSURF_MP,QSUNDIR_DOWN_SSURF_MP)
	    !          - atténuation dans la mer pour la direction transmise (CH_SEA_MUS)
            CALL OSOAA_FSOURCE_SEA_ORDRE1_MERPLATE(
     &	                   IS,JKW,NBMU,CH_SEA_MUS,
     &	 		   SEA_PCRAY_TR,SEA_PC_CHL_TR,SEA_PC_MLP_TR,
     &                     BETA0_MOL_SEA,BETA2_MOL_SEA,
     &			   GAMMA2_MOL_SEA,ALPHA2_MOL_SEA,
     &                     XPL,XRL,XTL,
     &		           ISUNDIR_DOWN_SSURF_MP,QSUNDIR_DOWN_SSURF_MP,
     &                     BP_CHL,GR_CHL,GT_CHL,ARR_CHL,ART_CHL,
     &			   BP_MLP,GR_MLP,GT_MLP,ARR_MLP,ART_MLP,
     &                     I2_SEA,Q2_SEA,U2_SEA)             
     
         ENDIF  !Fin test mer plate ou agitée       
     
    
         !******* Traces d'expertise ***********************************
         IF (TRACE_DEBUG) THEN
	    WRITE(IDLOG,*,ERR=1010) " "
	    WRITE(IDLOG,*,ERR=1010) 
     &	       "Marine source function of order 1 : IS = ",IS
	    WRITE(IDLOG,*,ERR=1010) " "
	    DO I=0,CTE_NT_SEA,20
	       WRITE(IDLOG,*,ERR=1010) "Level sea I : ",I
	       WRITE(IDLOG,*,ERR=1010) 
     &	       "  I     K      RMU          I2          Q2          U2"
	       DO K=-NBMU,NBMU,4
	          WRITE(IDLOG,402,ERR=1010) 
     &			I,K,RMU(K),I2_SEA(I,K),Q2_SEA(I,K),U2_SEA(I,K)
	       ENDDO
	    ENDDO  
	  ENDIF
	 !**************************************************************
	 


C*******************************************************************		
C******** Initialisation à zéro des champs marin et atmosphérique 
C         d'ordre IG =1 pour ordre S = IS en cours 
C        (toutes directions confondues)
C*******************************************************************	
         !
	  DO I=0,CTE_NT_SEA
	    DO K=-NBMU,NBMU
	       I1_SEA(I,K)=0.	
	       Q1_SEA(I,K)=0.	
	       U1_SEA(I,K)=0.
	    ENDDO
	 ENDDO
	 
        DO I=0,CTE_NT_ATM
	    DO K=-NBMU,NBMU
	       I1_ATM(I,K)=0.	
	       Q1_ATM(I,K)=0.	
	       U1_ATM(I,K)=0.	
	    ENDDO
	 ENDDO	 	 
	 	
C*******************************************************************		
C******** Etape 1 : Signal montant 
C                   Calcul de la luminance réfléchie au fond de mer
C		    ==> Réflexion lambertienne
C		    ==> Application du même calcul que la mer soit plate
C                       ou agitée en faisant l'hypothèse que le flux
C                       incident au fond de mer est le même dans les 2 cas.
C                       (possible car seul IS=0 compte 
C                        et car la surface est lambertienne)
C*******************************************************************	
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	    WRITE(IDLOG,*,ERR=1010)  
     &      " Order S =",IS," / Order IG = 1 - Step 1"
	    WRITE(IDLOG,*,ERR=1010) "             Sea bottom reflection"
	    WRITE(IDLOG,*,ERR=1010) "----------------------------------" 
	 ENDIF
	   
	 I_REFL_BOT = 0.  
	 IF (RHO_BOT.NE.0..AND.IS.EQ.0) THEN   
	    I_REFL_BOT = RHO_BOT*FLUX_DOWN_FOND_DIR/INCTE_PI
	 ENDIF
	
	 DO K=1,NBMU
	    I1_SEA(CTE_NT_SEA,K)=I_REFL_BOT
	 ENDDO  
	 					
	 IF (TRACE) THEN
	     WRITE(IDLOG,*,ERR=1010) 
     &	     'I reflected on bottom :',I_REFL_BOT
	     
	     IF (IS.EQ.0) THEN
	       WRITE(IDLOG,*,ERR=1010) '  - Bottom albedo :',RHO_BOT
	       WRITE(IDLOG,*,ERR=1010) '  - CosThetas_water :',TABW
	       
	       IF(IMAT_SURF.EQ.0) THEN !Cas mer plate	       
	          WRITE(IDLOG,*,ERR=1010) '  - Trans. irrad 0- / PI:',
     &            ISUNDIR_DOWN_SSURF_MP
	          WRITE(IDLOG,*,ERR=1010) '  - Attenuation into sea :',
     &		    	             4.*CH_SEA_MUS(CTE_NT_SEA)
	          WRITE(IDLOG,*,ERR=1010) 
     &	             '  - Solar direct irradiance / PI,'//
     &	             ' at sea bottom   - Flat sea :',  
     &	             ISUNDIR_DOWN_SSURF_MP*4.*CH_SEA_MUS(CTE_NT_SEA)
              ENDIF
	       
	       WRITE(IDLOG,*,ERR=1010) 
     &	          '  - Direct solar flux, transm. and att.'//
     &	          ' at sea bottom : ', FLUX_DOWN_FOND_DIR
               
	     ELSE
	       WRITE(IDLOG,*,ERR=1010)
     &	       'The incidente flux on bottom is null'
	     ENDIF	 
	 
	 ENDIF  
         
C*************************************************************************			
C******** Etape 2 : Signal montant 
C                   Calcul de la luminance remontante 
C                   du fond de mer à la surface
C		    --> Intégration du champ de luminance montant (mu > 0) 
C			du fond de mer à la surface
C*************************************************************************	
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) " "
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	    WRITE(IDLOG,*,ERR=1010) 
     &	    " Order S =",IS," / Order IG = 1 - Step 2"
	    WRITE(IDLOG,*,ERR=1010) "          Integr bottom -> surf"
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	 ENDIF
	 
	 CALL OSOAA_INTEGR_EPOPT_SEA_UP(NBMU,RMU,SEA_TAUEXT_TR,
     &				        I2_SEA,Q2_SEA,U2_SEA,
     &  	                        I1_SEA,Q1_SEA,U1_SEA)
     
         !******* Traces d'expertise ***********************************
         IF (TRACE_DEBUG) THEN
	    WRITE(IDLOG,*,ERR=1010) " "
	    WRITE(IDLOG,*,ERR=1010) 
     &	       "Luminance marine up ordre 1 - étape 2 : IS = ",IS
	    WRITE(IDLOG,*,ERR=1010) " "
	    DO I=0,CTE_NT_SEA,20
	       WRITE(IDLOG,*,ERR=1010) "Niveau sea I : ",I
	       WRITE(IDLOG,*,ERR=1010) 
     &	       "  I     K      RMU          I1          Q1          U1"
	       DO K=-NBMU,NBMU,4
	          WRITE(IDLOG,402,ERR=1010) 
     &			I,K,RMU(K),I1_SEA(I,K),Q1_SEA(I,K),U1_SEA(I,K)
	       ENDDO
	    ENDDO  
	 ENDIF
	 !**************************************************************
  
      
C*******************************************************************
C******** Etape 3 : Signal montant 
C                   Calcul de la luminance montante transmise 
C                   au passage mer -> air
C*******************************************************************

         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) " "
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	    WRITE(IDLOG,*,ERR=1010) 
     &	    " Order S =",IS," / Order IG = 1 - Step 3"
	    WRITE(IDLOG,*,ERR=1010) "             Transm. Sea -> air"
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	 ENDIF

	 
	 IF (IMAT_SURF.EQ.1) THEN	!************************
	    				!Etape 3 : Cas mer agitée
					!************************

            IF (TRACE) WRITE(IDLOG,*,ERR=1010) " Rough sea : "


            DO K=1,NBMU		!K direction de transmission
	    
               II2=0.
               QQ2=0.
               UU2=0.			       
	       DO J=1,NBMU	!J direction d'incidence montante
                  Z= GA(J)
                  XI1 = I1_SEA(0,J)
                  XQ1 = Q1_SEA(0,J)
                  XU1 = U1_SEA(0,J)
	  
                  II2 = II2 
     &	              +Z*(XI1*TWA11(J,K)+XQ1*TWA12(J,K)+XU1*TWA13(J,K))
                  QQ2 = QQ2 
     &	              +Z*(XI1*TWA21(J,K)+XQ1*TWA22(J,K)+XU1*TWA23(J,K))
                  UU2 = UU2 
     &	              +Z*(XI1*TWA31(J,K)+XQ1*TWA32(J,K)+XU1*TWA33(J,K))
	       ENDDO
	       
	       COEF_INTSPA = 2./RMU(K)
	       I1_ATM(CTE_NT_ATM,K) = II2*COEF_INTSPA
	       Q1_ATM(CTE_NT_ATM,K) = QQ2*COEF_INTSPA
	       U1_ATM(CTE_NT_ATM,K) = UU2*COEF_INTSPA
	    ENDDO

            IF (TRACE) THEN
	       WRITE(IDLOG,*,ERR=1010) " "
	       WRITE(IDLOG,*,ERR=1010) 
     &	     " Upward field 0- / Upward field transmitted 0+ :"
	       WRITE(IDLOG,*,ERR=1010)"  - Incident field by direction"
	       WRITE(IDLOG,*,ERR=1010) 
     &	       "  - Transmitted field = spatially integrated"
	       WRITE(IDLOG,*,ERR=1010) 
     &	       "     ==> Warning : a transmitted direction includes"//
     &	       " contributions from all the incidente directions "
               WRITE(IDLOG,*,ERR=1010) 
     &	       "         (spatial integration)"
      
	       DO K=1,NBMU
                  WRITE(IDLOG,500,ERR=1010)
     &             ' Ang inc :',DACOS(RMU(K))*180./INCTE_PI,
     &             ' I_INC ',I1_SEA(0,K),
     &		     ' Q_INC ',Q1_SEA(0,K),
     &		     ' U_INC ',U1_SEA(0,K),
     &             ' Ang trans :',DACOS(RMU(K))*180./INCTE_PI,
     &             ' I_TR ',I1_ATM(CTE_NT_ATM,K),
     &		     ' Q_TR ',Q1_ATM(CTE_NT_ATM,K),
     &		     ' U_TR ',U1_ATM(CTE_NT_ATM,K),
     &             ' I_UP_SURF ',I1_ATM(CTE_NT_ATM,K),
     &		     ' Q_UP_SURF ',Q1_ATM(CTE_NT_ATM,K),
     &		     ' U_UP_SURF ',U1_ATM(CTE_NT_ATM,K)
               ENDDO
	    ENDIF
	     	 	    	   
	   
         ELSE  !************************
	       !Etape 3 :  Cas mer plate 
	       !************************
	    
	    !Affectation des valeurs incidentes du champ montant sous la surface
            DO K=1,NBMU
	       IINC(K) = I1_SEA(0,K)
	       QINC(K) = Q1_SEA(0,K)
	       UINC(K) = U1_SEA(0,K)
	     ENDDO
	    
	     CALL  OSOAA_INTERF_MERPLATE("TWA",SEA_IND,RMU,NBMU,
     & 				     IINC,QINC,UINC,IDEV,QDEV,UDEV,
     &                                  TRACE,IER)

            IF (IER.NE.0) GOTO 1200   ! cas d'erreur   
	    
            !Affectation du champ montant transmis au tableau du champ 
	    !dans l'atmosphère au niveau de la surface
            DO K=1,NBMU
	       I1_ATM(CTE_NT_ATM,-K) = 0.
	       Q1_ATM(CTE_NT_ATM,-K) = 0.
	       U1_ATM(CTE_NT_ATM,-K) = 0.
	       
	       I1_ATM(CTE_NT_ATM,K) = IDEV(K)
	       Q1_ATM(CTE_NT_ATM,K) = QDEV(K)
	       U1_ATM(CTE_NT_ATM,K) = UDEV(K)
	     ENDDO     


            IF (TRACE) THEN
	       WRITE(IDLOG,*,ERR=1010) 
     &	     " Upward field 0- / Upward field transmitted 0+ :"
	       DO K=1,NBMU
                  WRITE(IDLOG,500,ERR=1010)
     &             ' Ang inc :',DACOS(RMU(K))*180./INCTE_PI,
     &             ' I_INC ',IINC(K),
     &		     ' Q_INC ',QINC(K),
     &		     ' U_INC ',UINC(K),
     &             ' Ang trans :',DACOS(RMU(K))*180./INCTE_PI,
     &             ' I_TR ',IDEV(K),
     &		     ' Q_TR ',QDEV(K),
     &		     ' U_TR ',UDEV(K),
     &             ' I_UP_SURF ',I1_ATM(CTE_NT_ATM,K),
     &		     ' Q_UP_SURF ',Q1_ATM(CTE_NT_ATM,K),
     &		     ' U_UP_SURF ',U1_ATM(CTE_NT_ATM,K)
               ENDDO
	     ENDIF
	    	    	    
         ENDIF  !Fin test mer plate ou agitée


        
	 
	 

C*******************************************************************	
C******** Etape 4 : Signal montant 
C                   Réflexion lambertienne de l'écume
C*******************************************************************
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	    WRITE(IDLOG,*,ERR=1010)  
     &	    " Order S =",IS," / Order IG = 1 - Step 4"
	    WRITE(IDLOG,*,ERR=1010) "             Foam reflection     "
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	 ENDIF
	 
         IF ((RHO_SURF.NE.0.).AND.(IS.EQ.0)) THEN
	    I_REFL_SURF = 
     &	       - RHO_SURF*TAB*DEXP(ATM_TAUEXT_TR_TOT/TAB)
	    DO K=1,NBMU
	       I1_ATM(CTE_NT_ATM,K) =
     &             I1_ATM(CTE_NT_ATM,K) + I_REFL_SURF
	    ENDDO
	 ENDIF ! Fin test sur RHO_SURF et IS

	 IF (TRACE) THEN
	 
	     IF (IS.EQ.0) THEN
	       WRITE(IDLOG,*,ERR=1010)
     &	       'I reflected on surface :',I_REFL_SURF
	       WRITE(IDLOG,*,ERR=1010)'  - Surface albedo:',RHO_SURF
	       WRITE(IDLOG,*,ERR=1010)'  - CosThetas_air :',TAB
	       WRITE(IDLOG,*,ERR=1010) 
     &	       '  - Solar direct irradiance att. at the sea surface :',
     &			DEXP(ATM_TAUEXT_TR_TOT/TAB)
	       WRITE(IDLOG,*,ERR=1010) " "
	     ELSE
	       WRITE(IDLOG,*,ERR=1010)
     &	          'The incidente flux on surface is null'
	     ENDIF
	 ENDIF    
	 
	 
C*************************************************************************		    
C******** Etape 5 : Signal montant 
C                   Calcul de la luminance remontante de la surface au TOA
C
C                   - Intégration sur la colonne atmosphérique du signal
C                     émergeant de la surface au TOA (signal émergeant =
C                     solaire réfléchi au fond de mer, diffusion primaire 
C                     marine et écume de surface)
C
C                   - Ajout de la réflexion du faisceau solaire direct
C		      (cas mer agitée)
C
C                   - Ajout de la composante de diffusion du rayonnement
C                     solaire direct réfléchi en surface selon la loi 
C                     de Fresnel (cas mer plate)
C*************************************************************************	
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) " "
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	    WRITE(IDLOG,*,ERR=1010)  
     &	    " Order S =",IS," / Order IG = 1 - Step 5"
	    WRITE(IDLOG,*,ERR=1010) "             Integr surf -> TOA"
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	    WRITE(IDLOG,*,ERR=1010) " "
	 ENDIF

	 !Intégration de la surface au TOA 
	 !Mer agitée : sans glitter du faisceau solaire direct.
	 !Mer plate  : sans diffusion de la réflexion
	 !             de Fresnel du faisceau solaire direct.
	 CALL OSOAA_INTEGR_EPOPT_ATM_UP(NBMU,RMU,ATM_TAUEXT_TR,
     &				        I2_ATM,Q2_ATM,U2_ATM,
     &  	                        I1_ATM,Q1_ATM,U1_ATM)


	 !Initialisation des termes de stockage de la composante de 
	 !réflexion du faisceau solaire direct
         DO K=1,NBMU       
	     RII(I,K) = 0.		
	     RQQ(I,K) = 0.		
	     RUU(I,K) = 0.		  
	 ENDDO

	 
	 IF (IMAT_SURF.EQ.1) THEN	!************************
	    				!Etape 5 : Cas mer agitée
					!************************

            ! Ajout de la réflexion du faisceau solaire direct
	    ! réfléchi sur les vagues, sur le profil atmosphérique.
	    
	    !Réflexion du faisceau solaire direct sur les vagues, en surface
            TRANS_ATM_DIRECT_TR = DEXP(ATM_TAUEXT_TR_TOT/TAB)
	    
	    DO I = 0,CTE_NT_ATM
	   
	       DTAU = ATM_TAUEXT_TR_TOT - ATM_TAUEXT_TR(I)
	       
	       DO K=1,NBMU
	       
	          TRANS = TRANS_ATM_DIRECT_TR * DEXP(-DTAU/RMU(K)) 
	          COEF_INTSPA = 1./(4.*RMU(K))
		  
		  !Stockage de la composante de réflexion du faisceau solaire direct
		  RII(I,K) = RAA11(N0,K)*TRANS*COEF_INTSPA		
		  RQQ(I,K) = RAA21(N0,K)*TRANS*COEF_INTSPA		
		  RUU(I,K) = RAA31(N0,K)*TRANS*COEF_INTSPA		
		  
		  !Ajout au champ de luminance atmosphérique
		  I1_ATM(I,K) = I1_ATM(I,K) + RII(I,K)
	       	  Q1_ATM(I,K) = Q1_ATM(I,K) + RQQ(I,K)
	          U1_ATM(I,K) = U1_ATM(I,K) + RUU(I,K)	
		  
	      ENDDO
	    
	    ENDDO ! Boucle sur le niveau I
	    

	   
         ELSE  !************************
	       !Etape 5 :  Cas mer plate 
	       !************************

	    ! Ajout au profil de luminance de la composante de
	    ! diffusion du faisceau solaire direct réfléchi
		
	    ! Calcul de la fonction source pour la diffusion de L1fresnel 	 
	    CALL OSOAA_FSOURCE_DIFF_FRESNEL1(
     &	   	          -TAB,JK,SEA_IND,ATM_PCAER_TR,ATM_PCRAY_TR,
     &                    BETA0_MOL_AIR,BETA2_MOL_AIR,
     &                    GAMMA2_MOL_AIR,ALPHA2_MOL_AIR,
     &                    BP_AER,GR_AER,GT_AER,ARR_AER,ART_AER,
     &                    XPL,XRL,XTL,IS,NBMU,ATM_TAUEXT_TR,
     &		          I2_ATM_FRESNEL,Q2_ATM_FRESNEL,U2_ATM_FRESNEL)


            ! Intégration sur le profil atmosphérique. 
            ! Calcul du champ de luminance pour la diffusion primaire de L1fresnel.	 
            DO K=1,NBMU
               I1_ATM_FRESNEL(CTE_NT_ATM,K)=0.	!Initialisation en surface.
               Q1_ATM_FRESNEL(CTE_NT_ATM,K)=0.
               U1_ATM_FRESNEL(CTE_NT_ATM,K)=0. 
   	    ENDDO

	    CALL OSOAA_INTEGR_EPOPT_ATM_UP(NBMU,RMU,ATM_TAUEXT_TR,
     &				           I2_ATM_FRESNEL,
     &				           Q2_ATM_FRESNEL,
     &				   	   U2_ATM_FRESNEL,
     &  	                           I1_ATM_FRESNEL,
     &				           Q1_ATM_FRESNEL,
     &                                     U1_ATM_FRESNEL)

            ! Ajout au champ de luminance atmosphérique
            DO I=0,CTE_NT_ATM
               DO K=-NBMU,NBMU
                     I1_ATM(I,K)=I1_ATM(I,K)+I1_ATM_FRESNEL(I,K)
                     Q1_ATM(I,K)=Q1_ATM(I,K)+Q1_ATM_FRESNEL(I,K)
                     U1_ATM(I,K)=U1_ATM(I,K)+U1_ATM_FRESNEL(I,K)
   	       ENDDO	     
 	    ENDDO
	    	                	    
         ENDIF  !Fin test mer plate ou agitée  		

		

     
         !******* Traces d'expertise ***********************************
         IF (TRACE_DEBUG) THEN
	    WRITE(IDLOG,*,ERR=1010) " "
	    WRITE(IDLOG,*,ERR=1010) " "
	    WRITE(IDLOG,*,ERR=1010) 
     &	       "Luminance atmos up ordre 1 - étape 5 : IS = ",IS
	    WRITE(IDLOG,*,ERR=1010) " "
	    DO I=0,CTE_NT_ATM
	       IF ((I.EQ.0).OR.(I.EQ.2).OR.(I.EQ.13).OR.(I.EQ.24)
     &	            .OR.(I.EQ.26)) THEN
	          WRITE(IDLOG,*,ERR=1010) "Niveau atmos I : ",I
	          WRITE(IDLOG,*,ERR=1010) 
     &	       "  I     K      RMU          I1          Q1          U1"
	          DO K=-NBMU,NBMU,4
	             WRITE(IDLOG,402,ERR=1010) 
     &			I,K,RMU(K),I1_ATM(I,K),Q1_ATM(I,K),U1_ATM(I,K)
	          ENDDO
	       ENDIF
	    ENDDO  
	 ENDIF
	 !**************************************************************     
     
     	 
C*************************************************************************		    
C******** Etape 6 : Signal descendant 
C                   Calcul de la luminance atmosphérique descendante
C		    du TOA à la surface
C*************************************************************************		     	 
	 IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	    WRITE(IDLOG,*,ERR=1010)  
     &	    " Order S =",IS," / Order IG = 1 - Step 6"
	    WRITE(IDLOG,*,ERR=1010) "             Integr TOA -> surf "
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	 ENDIF

	 CALL OSOAA_INTEGR_EPOPT_ATM_DOWN(NBMU,RMU,ATM_TAUEXT_TR,
     &				             I2_ATM,Q2_ATM,U2_ATM,
     &  	                           I1_ATM,Q1_ATM,U1_ATM)		
  		
         !******* Traces d'expertise ***********************************
         IF (TRACE_DEBUG) THEN
	    WRITE(IDLOG,*,ERR=1010) " "
	    WRITE(IDLOG,*,ERR=1010) 
     &	       "Luminance atmos up & down ordre 1 - étape 6 : IS = ",IS
	    WRITE(IDLOG,*,ERR=1010) " "
	    DO I=0,CTE_NT_ATM
	       IF ((I.EQ.0).OR.(I.EQ.2).OR.(I.EQ.13).OR.(I.EQ.24)
     &	            .OR.(I.EQ.26)) THEN
	          WRITE(IDLOG,*,ERR=1010) "Niveau atmos I : ",I
	          WRITE(IDLOG,*,ERR=1010) 
     &	       "  I     K      RMU          I1          Q1          U1"
	          DO K=-NBMU,NBMU,4
	             WRITE(IDLOG,402,ERR=1010) 
     &		       I,K,RMU(K),I1_ATM(I,K),Q1_ATM(I,K),U1_ATM(I,K)
	          ENDDO
	       ENDIF
	    ENDDO  
	 ENDIF
	 !**************************************************************



*************************************************************************
C******** Etape 7 : Signal descendant 
C                   Calcul de la luminance descendante transmise 
C                   au passage air -> mer
C*************************************************************************
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	    WRITE(IDLOG,*,ERR=1010)  
     &	    " Order S =",IS," / Order IG = 1 - Step 7"
	    WRITE(IDLOG,*,ERR=1010) "             Transm. Air -> sea"
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	 ENDIF


	 IF (IMAT_SURF.EQ.1) THEN	!************************
	    				!Etape 7 : Cas mer agitée
					!************************

            IF (TRACE) WRITE(IDLOG,*,ERR=1010) " Rough sea : "


            DO K=1,NBMU		!K direction de transmission
	       
	       !Champ montant déjà initialisé : pas redéfini ici

               II2=0.
               QQ2=0.
               UU2=0.		       
	       		       
	       DO J=1,NBMU	!J direction d'incidence descendante
                  Z= GA(J)
                  XI1 = I1_ATM(CTE_NT_ATM,-J) 		
                  XQ1 = Q1_ATM(CTE_NT_ATM,-J) 
                  XU1 = U1_ATM(CTE_NT_ATM,-J)
	  
                  II2 = II2 
     &	              +Z*(XI1*TAW11(J,K)+XQ1*TAW12(J,K)+XU1*TAW13(J,K))
                  QQ2 = QQ2 
     &	              +Z*(XI1*TAW21(J,K)+XQ1*TAW22(J,K)+XU1*TAW23(J,K))
                  UU2 = UU2 
     &	              +Z*(XI1*TAW31(J,K)+XQ1*TAW32(J,K)+XU1*TAW33(J,K))
		  
	       ENDDO !Fin boucle sur J

               COEF_INTSPA = 2./RMU(K)  
	       I1_SEA(0,-K)= II2*COEF_INTSPA
	       Q1_SEA(0,-K)= QQ2*COEF_INTSPA
	       U1_SEA(0,-K)= UU2*COEF_INTSPA 		      
	       
	    ENDDO !Fin boucle sur K

            IF (TRACE) THEN
	       WRITE(IDLOG,*,ERR=1010) " "
	       WRITE(IDLOG,*,ERR=1010) 
     &	      " Downward radiance O+ / Transmitted radiance 0- :"
	       DO K=1,NBMU
                  WRITE(IDLOG,500,ERR=1010)
     &              ' Ang inc :',DACOS(RMU(-K))*180./INCTE_PI,
     &              ' I_INC ',I1_ATM(CTE_NT_ATM,-K),
     &		      ' Q_INC ',Q1_ATM(CTE_NT_ATM,-K),
     &		      ' U_INC ',U1_ATM(CTE_NT_ATM,-K),
     &              ' Ang trans :',DACOS(RMU(-K))*180./INCTE_PI,
     &              ' I_TR ',I1_SEA(0,-K),
     &		      ' Q_TR ',Q1_SEA(0,-K),
     &		      ' U_TR ',U1_SEA(0,-K),
     &              ' I_D_SSURF ',I1_SEA(0,-K),
     &		      ' Q_D_SSURF ',Q1_SEA(0,-K),
     &		      ' U_D_SSURF ',U1_SEA(0,-K)
               ENDDO
	    ENDIF	        	   
	   
         ELSE  !************************
	       !Etape 7 :  Cas mer plate 
	       !************************
	    
	    !Affectation des valeurs incidentes du champ descendant en surface
            DO K=1,NBMU
	       IINC(K) = I1_ATM(CTE_NT_ATM,-K)
	       QINC(K) = Q1_ATM(CTE_NT_ATM,-K)
	       UINC(K) = U1_ATM(CTE_NT_ATM,-K)	     
	    ENDDO
            
	    
	    CALL  OSOAA_INTERF_MERPLATE("TAW",SEA_IND,RMU,NBMU,
     & 				     IINC,QINC,UINC,IDEV,QDEV,UDEV,
     &                                  TRACE,IER)

            IF (IER.NE.0) GOTO 1200   ! cas d'erreur   
	    
            !Affectation du champ descendant transmis au tableau du champ dans 
	    !l'eau au niveau de la surface 
	    !(pas d'initialisation du champ montant car il est déjà calculé).
            DO K=1,NBMU       
	       I1_SEA(0,-K) = IDEV(K)
	       Q1_SEA(0,-K) = QDEV(K)
	       U1_SEA(0,-K) = UDEV(K)
	    ENDDO     

            IF (TRACE) THEN
	       WRITE(IDLOG,*,ERR=1010) 
     &	      " Downward radiance O+ / Transmitted radiance 0- :"
	       DO K=1,NBMU
                  WRITE(IDLOG,500,ERR=1010)
     &              ' Ang inc :',DACOS(RMU(-K))*180./INCTE_PI,
     &              ' I_INC ',IINC(K),
     &		      ' Q_INC ',QINC(K),
     &		      ' U_INC ',UINC(K),
     &              ' Ang trans :',DACOS(RMU(-K))*180./INCTE_PI,
     &              ' I_TR ',IDEV(K),
     &		      ' Q_TR ',QDEV(K),
     &		      ' U_TR ',UDEV(K),
     &              ' I_D_SSURF ',I1_SEA(0,-K),
     &		      ' Q_D_SSURF ',Q1_SEA(0,-K),
     &		      ' U_D_SSURF ',U1_SEA(0,-K)
               ENDDO
	    ENDIF	    
	 
         ENDIF  !Fin test mer plate ou agitée       

		
C*************************************************************************		  
C******** Etape 8 : Signal descendant 
C                   Réflexion mer -> mer du signal marin montant 
C                   de même ordre (calculé à l'étape 2)
C*************************************************************************	
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	    WRITE(IDLOG,*,ERR=1010)  
     &	    " Order S =",IS," / Order IG = 1 - Step 8"
	    WRITE(IDLOG,*,ERR=1010) "             Reflex. Sea -> sea"
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	 ENDIF

	 IF (IMAT_SURF.EQ.1) THEN	!************************
	    				!Etape 8 : Cas mer agitée
					!************************
	    IF (TRACE) THEN
	       WRITE(IDLOG,*,ERR=1010) " Rough sea : "
	       
	       DO K=1,NBMU		!K direction de réflexion
                 IINC(K) = I1_SEA(0,K)	!Stockage pour le fichier Trace
	          QINC(K) = Q1_SEA(0,K)
	          UINC(K) = U1_SEA(0,K)		  
	       ENDDO
            ENDIF !Fin Trace

            DO K=1,NBMU		!K direction de réflexion
	       
               II2=0.
               QQ2=0.
               UU2=0.			       
	       DO J=1,NBMU	!J direction d'incidence montante
                  Z= GA(J)
                  XI1 = I1_SEA(0,J) 		
                  XQ1 = Q1_SEA(0,J)
                  XU1 = U1_SEA(0,J)

                  II2 = II2 
     &	              +Z*(XI1*RWW11(J,K)+XQ1*RWW12(J,K)+XU1*RWW13(J,K))
                  QQ2 = QQ2 
     &	              +Z*(XI1*RWW21(J,K)+XQ1*RWW22(J,K)+XU1*RWW23(J,K))
                  UU2 = UU2 
     &	              +Z*(XI1*RWW31(J,K)+XQ1*RWW32(J,K)+XU1*RWW33(J,K))
	       ENDDO
	       
	       COEF_INTSPA = 1./(2.*RMU(K))	 
	       IDEV(K)=II2*COEF_INTSPA 
	       QDEV(K)=QQ2*COEF_INTSPA
	       UDEV(K)=UU2*COEF_INTSPA
	             
	       I1_SEA(0,-K)= I1_SEA(0,-K) + IDEV(K)     ! Affectation au champ 
	       Q1_SEA(0,-K)= Q1_SEA(0,-K) + QDEV(K)     ! descendant
	       U1_SEA(0,-K)= U1_SEA(0,-K) + UDEV(K)
	    ENDDO
	 		   
	   
         ELSE !************************
	       !Etape 8 :  Cas mer plate 
	       !************************

	    !Affectation des valeurs incidentes du champ montant sous la surface
            DO K=1,NBMU
	       IINC(K) = I1_SEA(0,K)
	       QINC(K) = Q1_SEA(0,K)
	       UINC(K) = U1_SEA(0,K)
	     ENDDO
	    
	     CALL  OSOAA_INTERF_MERPLATE("RWW",SEA_IND,RMU,NBMU,
     & 				     IINC,QINC,UINC,IDEV,QDEV,UDEV,
     &                                  TRACE,IER)

            IF (IER.NE.0) GOTO 1200   ! cas d'erreur   
	    
	    
            !Ajout du champ montant réfléchi vers le fond au tableau du champ dans l'eau
	    !sous la surface.
            DO K=1,NBMU       
	       I1_SEA(0,-K) = I1_SEA(0,-K) + IDEV(K)	!Affectation au champ descendant
	       Q1_SEA(0,-K) = Q1_SEA(0,-K) + QDEV(K)
	       U1_SEA(0,-K) = U1_SEA(0,-K) + UDEV(K)
	     ENDDO     



         ENDIF  !Fin test mer plate ou agitée       
	 		


         IF (TRACE) THEN
	     WRITE(IDLOG,*,ERR=1010) 
     &	     " Upward radiance 0- / Downward radiance 0- :"
	     DO K=1,NBMU
                WRITE(IDLOG,500,ERR=1010)
     &             ' Ang inc :',DACOS(RMU(K))*180./INCTE_PI,
     &             ' I_INC ',IINC(K),
     &		     ' Q_INC ',QINC(K),
     &		     ' U_INC ',UINC(K),
     &             ' Ang trans :',DACOS(RMU(-K))*180./INCTE_PI,
     &             ' I_RF ',IDEV(K),
     &		     ' Q_RF ',QDEV(K),
     &		     ' U_RF ',UDEV(K),
     &             ' I_D_SSURF ',I1_SEA(0,-K),
     &		     ' Q_D_SSURF ',Q1_SEA(0,-K),
     &		     ' U_D_SSURF ',U1_SEA(0,-K)
             ENDDO
	 ENDIF	    

	 	 
C*************************************************************************				
C******** Etape 9 : Signal descendant 
C                   Calcul de la luminance descendante 
C                   de la surface au fond de mer 
C*************************************************************************	
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	    WRITE(IDLOG,*,ERR=1010)  
     &	    " Order S =",IS," / Order IG = 1 - Step 9"
	    WRITE(IDLOG,*,ERR=1010) "          Integr Surf -> bottom"
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	 ENDIF

	 CALL OSOAA_INTEGR_EPOPT_SEA_DOWN(NBMU,RMU,SEA_TAUEXT_TR,
     &				          I2_SEA,Q2_SEA,U2_SEA,
     &  	                          I1_SEA,Q1_SEA,U1_SEA)		
  
  
         !******* Traces d'expertise ***********************************
         IF (TRACE_DEBUG) THEN
	    WRITE(IDLOG,*,ERR=1010) " "
	    WRITE(IDLOG,*,ERR=1010) 
     &	       "Luminance sea up & down ordre 1 - étape 9 : IS = ",IS
	    WRITE(IDLOG,*,ERR=1010) " "
	    DO I=0,CTE_NT_SEA,20
	       WRITE(IDLOG,*,ERR=1010) "Niveau sea I : ",I
	       WRITE(IDLOG,*,ERR=1010) 
     &	       "  I     K      RMU          I1          Q1          U1"
	       DO K=-NBMU,NBMU,4
	          WRITE(IDLOG,402,ERR=1010) 
     &		       I,K,RMU(K),I1_SEA(I,K),Q1_SEA(I,K),U1_SEA(I,K)
	       ENDDO
	    ENDDO  
	 ENDIF
	 !**************************************************************  

		
		 
C*************************************************************************			
C******** Etape 10 : Signal montant 
C                    Ajout au signal montant de la luminance atmosphérique
C                    descendante réfléchie sur la surface (air -> air)
C
C		     --> Affectation au profil atmosphérique 
C			 pour le signal montant
C*************************************************************************
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	    WRITE(IDLOG,*,ERR=1010)  
     &	    " Order S =",IS," / Order IG = 1 - Step 10"
	    WRITE(IDLOG,*,ERR=1010) "             Reflex. Air -> air"
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	 ENDIF
	 	
	 IF (IMAT_SURF.EQ.1) THEN	!*************************
	    				!Etape 10 : Cas mer agitée
					!*************************
	    IF (TRACE) THEN
	       WRITE(IDLOG,*,ERR=1010) " Rough sea : "
	       WRITE(IDLOG,*,ERR=1010) 
     & 	          "  Reflexion air / air on surface :"
              WRITE(IDLOG,*,ERR=1010) 
     &	          " Downward radiance 0+ / Upward radiance 0+ :"
            ENDIF	     	    
	    
            DO K=1,NBMU		!K direction de réflexion montante

               II2=0.
               QQ2=0.
               UU2=0.			       
	       DO J=1,NBMU	!J direction d'incidence descendante
                  Z= GA(J)
                  XI1 = I1_ATM(CTE_NT_ATM,-J) 		
                  XQ1 = Q1_ATM(CTE_NT_ATM,-J)
                  XU1 = U1_ATM(CTE_NT_ATM,-J)

                  II2 = II2 
     &	              +Z*(XI1*RAA11(J,K)+XQ1*RAA12(J,K)+XU1*RAA13(J,K))
                  QQ2 = QQ2 
     &	              +Z*(XI1*RAA21(J,K)+XQ1*RAA22(J,K)+XU1*RAA23(J,K))
                  UU2 = UU2 
     &	              +Z*(XI1*RAA31(J,K)+XQ1*RAA32(J,K)+XU1*RAA33(J,K)) 
	       ENDDO
	       
	       COEF_INTSPA = 1./(2.*RMU(K))	       
	       I1_REFL_L1ATM_SURF= II2*COEF_INTSPA	 ! Affectation au champ montant
	       Q1_REFL_L1ATM_SURF= QQ2*COEF_INTSPA	 ! en surface	 	  
	       U1_REFL_L1ATM_SURF= UU2*COEF_INTSPA		 

	    
	       DO I=CTE_NT_ATM,0,-1	!Ajout au champ montant pour chaque niveau
	       
	          DTAU = ATM_TAUEXT_TR(I)-ATM_TAUEXT_TR_TOT
	          ATTVIS = DEXP(DTAU/RMU(K))

	          I1_ATM(I,K) = I1_ATM(I,K) + I1_REFL_L1ATM_SURF*ATTVIS
		  Q1_ATM(I,K) = Q1_ATM(I,K) + Q1_REFL_L1ATM_SURF*ATTVIS
		  U1_ATM(I,K) = U1_ATM(I,K) + U1_REFL_L1ATM_SURF*ATTVIS
	       
	       ENDDO !Fin boucle sur profil atmosphérique

               IF (TRACE) THEN
                   WRITE(IDLOG,511,ERR=1010)
     &              ' Ang inc :',-DACOS(RMU(-K))*180./INCTE_PI,
     &              ' Ang refl  :',DACOS(RMU(K))*180./INCTE_PI,
     &              ' I_RF ',I1_REFL_L1ATM_SURF,
     &		      ' Q_RF ',Q1_REFL_L1ATM_SURF,
     &		      ' U_RF ',U1_REFL_L1ATM_SURF,
     &              ' I_UP_SURF ',I1_ATM(CTE_NT_ATM,K),
     &		      ' Q_UP_SURF ',Q1_ATM(CTE_NT_ATM,K),
     &		      ' U_UP_SURF ',U1_ATM(CTE_NT_ATM,K)
	       ENDIF	     	 
	                        
	    ENDDO !Fin boucle sur direction propagation montante
	    
	    	    
         ELSE  !*************************
	       !Etape 10 :  Cas mer plate 
	       !*************************

	    !Affectation des valeurs incidentes de l'éclairement atmosphérique primaire 
	    !en surface
            DO K=1,NBMU
	       IINC(K) = I1_ATM(CTE_NT_ATM,-K)
	       QINC(K) = Q1_ATM(CTE_NT_ATM,-K)
	       UINC(K) = U1_ATM(CTE_NT_ATM,-K)
	    ENDDO         
	    
	    CALL  OSOAA_INTERF_MERPLATE("RAA",SEA_IND,RMU,NBMU,
     & 				        IINC,QINC,UINC,IDEV,QDEV,UDEV,
     &                                  TRACE,IER)

            IF (IER.NE.0) GOTO 1200   ! cas d'erreur   

	    IF (TRACE) THEN
	        WRITE(IDLOG,*,ERR=1010) " "
               WRITE(IDLOG,*,ERR=1010) 
     &	         " Downward radiance 0+ / Upward radiance 0+ :"
            ENDIF	     	    	    
	    	    
            !Affectation du champ montant réfléchi au tableau du champ dans l'air
	    !(prise en compte de l'atténuation de la surface au niveau du profil).
            DO K=1,NBMU		!Direction de réflexion montante
	    
	       DO I=CTE_NT_ATM,0,-1
	       
	          DTAU = ATM_TAUEXT_TR(I)-ATM_TAUEXT_TR_TOT
	          ATTVIS = DEXP(DTAU/RMU(K))
		  
		  I1_ATM(I,K) = I1_ATM(I,K) + IDEV(K)*ATTVIS 	
		  Q1_ATM(I,K) = Q1_ATM(I,K) + QDEV(K)*ATTVIS 	
		  U1_ATM(I,K) = U1_ATM(I,K) + UDEV(K)*ATTVIS 	
	       
	       ENDDO !Fin boucle sur profil atmosphérique
                 
	    ENDDO !Fin boucle sur direction propagation montante

            IF (TRACE) THEN
	       DO K=1,NBMU	
                   WRITE(IDLOG,500,ERR=1010)
     &              ' Ang inc :',-DACOS(RMU(-K))*180./INCTE_PI,
     &              ' I_INC ',IINC(K),
     &		      ' Q_INC ',QINC(K),
     &		      ' U_INC ',UINC(K),
     &              ' Ang refl  :',DACOS(RMU(K))*180./INCTE_PI,
     &              ' I_RF ',IDEV(K),
     &		      ' Q_RF ',QDEV(K),
     &		      ' U_RF ',UDEV(K),
     &              ' I_UP_SURF ',I1_ATM(CTE_NT_ATM,K),
     &		      ' Q_UP_SURF ',Q1_ATM(CTE_NT_ATM,K),
     &		      ' U_UP_SURF ',U1_ATM(CTE_NT_ATM,K)
	       ENDDO
	    ENDIF	     	 
	       
	       
         ENDIF  !Fin test mer plate ou agitée       


         !******* Traces d'expertise ***********************************
         IF (TRACE_DEBUG) THEN
	    WRITE(IDLOG,*,ERR=1010) " "
	    WRITE(IDLOG,*,ERR=1010) 
     &	      "Luminance atmos up & down ordre 1 - étape 10 : IS = ",IS
	    WRITE(IDLOG,*,ERR=1010) " "
	    DO I=0,CTE_NT_ATM
	       IF ((I.EQ.0).OR.(I.EQ.2).OR.(I.EQ.13).OR.(I.EQ.24)
     &	            .OR.(I.EQ.26)) THEN
	          WRITE(IDLOG,*,ERR=1010) "Niveau atmos I : ",I
	          WRITE(IDLOG,*,ERR=1010) 
     &	       "  I     K      RMU          I1          Q1          U1"
	          DO K=-NBMU,NBMU,4
	             WRITE(IDLOG,402,ERR=1010) 
     &		       I,K,RMU(K),I1_ATM(I,K),Q1_ATM(I,K),U1_ATM(I,K)
	          ENDDO
	       ENDIF
	    ENDDO  
	 ENDIF
	 !**************************************************************

	    	  	 
C------------------------------------------------------------
C* Stockage des résultats de calcul de l'interaction d'ordre 1 
C-------------------------------------------------------------

C* Stockage du champ d'ordre IG = 1.
C-----------------------------------

	 DO K=-NBMU,NBMU
	    DO I=0,CTE_NT_ATM
	        I3(I,K)=I1_ATM(I,K)
	        Q3(I,K)=Q1_ATM(I,K)
                U3(I,K)=U1_ATM(I,K)
		
		D1(I,K)=I1_ATM(I,K)	!initialisation IG-1
	        E1(I,K)=Q1_ATM(I,K)
                F1(I,K)=U1_ATM(I,K)   
	    ENDDO 
	    
	    DO ISEA=0,CTE_NT_SEA
	        I=ISEA+CTE_NT_ATM+1		
	        I3(I,K)=I1_SEA(ISEA,K)
	        Q3(I,K)=Q1_SEA(ISEA,K)
                U3(I,K)=U1_SEA(ISEA,K) 
		
		D1(I,K)=I1_SEA(ISEA,K)	!initialisation IG-1
	        E1(I,K)=Q1_SEA(ISEA,K)
                F1(I,K)=U1_SEA(ISEA,K)   
	    ENDDO       
	 ENDDO

C* Stockage du champ d'ordre IG = 1 : 
C* ---------------------------------
C*      montant TOA et descendant au fond     => I3_TOA_FOND, Q3_TOA_FOND, U3_TOA_FOND
C*      montant et descendant à la surface    => I3_SURF,     Q3_SURF,     U3_SURF
C*      montant et descendant sous la surface => I3_SSURF,    Q3_SSURF,    U3_SSURF
C-------------------------------------------------------------------------------------
         DO K=-NBMU,NBMU
	 
	    IF (K<0) THEN 
	    
	       !Champ descendant au fond
	       I3_TOA_FOND(K)=I1_SEA(CTE_NT_SEA,K)
	       Q3_TOA_FOND(K)=Q1_SEA(CTE_NT_SEA,K)
               U3_TOA_FOND(K)=U1_SEA(CTE_NT_SEA,K)
	       
	       D1_TOA_FOND(K)=I1_SEA(CTE_NT_SEA,K)	!initialisation IG-1
	       E1_TOA_FOND(K)=Q1_SEA(CTE_NT_SEA,K)
               F1_TOA_FOND(K)=U1_SEA(CTE_NT_SEA,K)
	       
	       !Champ descendant au niveau de la surface
	       I3_SURF(K)=I1_ATM(CTE_NT_ATM,K)
	       Q3_SURF(K)=Q1_ATM(CTE_NT_ATM,K)
               U3_SURF(K)=U1_ATM(CTE_NT_ATM,K)
	       
	       !Champ descendant sous la surface
	       I3_SSURF(K)=I1_SEA(0,K)
	       Q3_SSURF(K)=Q1_SEA(0,K)
               U3_SSURF(K)=U1_SEA(0,K)    
	       
	    ELSE
	     
	       !Champ émergeant au TOA
	       I3_TOA_FOND(K)=I1_ATM(0,K)
	       Q3_TOA_FOND(K)=Q1_ATM(0,K)
               U3_TOA_FOND(K)=U1_ATM(0,K)
	       
	       D1_TOA_FOND(K)=I1_ATM(0,K)
	       E1_TOA_FOND(K)=Q1_ATM(0,K)
               F1_TOA_FOND(K)=U1_ATM(0,K)
	       
	       !Champ montant au niveau de la surface
	       I3_SURF(K)=I1_ATM(CTE_NT_ATM,K)
	       Q3_SURF(K)=Q1_ATM(CTE_NT_ATM,K)
               U3_SURF(K)=U1_ATM(CTE_NT_ATM,K)
	       
	       D1_SURF(K)=I1_ATM(CTE_NT_ATM,K)
	       E1_SURF(K)=Q1_ATM(CTE_NT_ATM,K)
               F1_SURF(K)=U1_ATM(CTE_NT_ATM,K)
	       
	       !Champ montant sous la surface
	       I3_SSURF(K)=I1_SEA(0,K)
	       Q3_SSURF(K)=Q1_SEA(0,K)
               U3_SSURF(K)=U1_SEA(0,K)    
	       
	       D1_SSURF(K)=I1_SEA(0,K)
	       E1_SSURF(K)=Q1_SEA(0,K)
               F1_SSURF(K)=U1_SEA(0,K)    
	       
	    ENDIF
	    
         ENDDO !Fin boucle sur les directions de propagation


												
C--------------------------------------------------------------
C
C   ******  BOUCLE SUR LES INTERACTIONS SUCCESSIVES   ******
C
C--------------------------------------------------------------



  503    IG=IG+1
  
C* Test d'arrêt des calculs si l'ordre maximal de diffusion est atteint
C-----------------------------------------------------------------------
         IF (IG.GT.IGMAX) GOTO 505

         IF (TRACE) THEN
            WRITE(IDLOG,*,ERR=1010) 
     &       '     Scattering order : IG = ',IG
            WRITE(IDLOG,*,ERR=1010) 
     &       '     ------------------------------'
            WRITE(IDLOG,*,ERR=1010) ' '	   
         ENDIF


C-------------------------------------------------------------------
C* Calcul du flux diffus d'ordre IG-1 à la surface et au fond de mer 
C* (Flux / PI précisément)
C-------------------------------------------------------------------
         IF (IS.EQ.0) THEN
	 
	    FLUX_DOWN_SURF_IGM1=0.
	    FLUX_DOWN_FOND_IGM1=0.
	 
	    DO J=1,NBMU
	       FLUX_DOWN_SURF_IGM1 = FLUX_DOWN_SURF_IGM1 
     &	   		           + GA(J)*I1_ATM(CTE_NT_ATM,-J)*RMU(J)
     
     	       FLUX_DOWN_FOND_IGM1 = FLUX_DOWN_FOND_IGM1 
     &			           + GA(J)*I1_SEA(CTE_NT_SEA,-J)*RMU(J)
	    ENDDO

            FLUX_DOWN_SURF_IGM1 = 2.*FLUX_DOWN_SURF_IGM1
	    FLUX_DOWN_FOND_IGM1 = 2.*FLUX_DOWN_FOND_IGM1

            IF (TRACE) THEN
               WRITE(IDLOG,*,ERR=1010) 'Downward flux order IG-1 :'
               WRITE(IDLOG,*,ERR=1010) '   - Flux on surface / PI :',
     &               	     FLUX_DOWN_SURF_IGM1 
               WRITE(IDLOG,*,ERR=1010) 
     &            '   - Flux on sea bottom / PI :',FLUX_DOWN_FOND_IGM1
	    ENDIF	    

	 ENDIF ! Si IS=0
	 
	 
C* Calcul de la fonction source de diffusion d'ordre IG pour l'atmosphère : 
C* ====> I2_ATM, Q2_ATM, U2_ATM
C--------------------------------------------------------------------------
         CALL OSOAA_FSOURCE_ATM_ORDREIG(IS,NBMU,
     &	 		   ATM_PCAER_TR,ATM_PCRAY_TR,
     &                     BETA0_MOL_AIR,BETA2_MOL_AIR,
     &			   GAMMA2_MOL_AIR,ALPHA2_MOL_AIR,
     &                     XPL,XRL,XTL,
     &                     I1_ATM,Q1_ATM,U1_ATM,
     &                     BP_AER,GR_AER,GT_AER,
     &                     ARR_AER,ART_AER,ATT_AER,
     &                     GA,I2_ATM,Q2_ATM,U2_ATM)       


	 !******* Traces d'expertise ***********************************
         IF (TRACE_DEBUG) THEN
	    WRITE(IDLOG,*,ERR=1010) " "
	    WRITE(IDLOG,*,ERR=1010) 
     &	       "Fonction source atmos ordre IG : ",IG," : IS = ",IS
	    WRITE(IDLOG,*,ERR=1010) " "
	    DO I=0,CTE_NT_ATM
	       IF ((I.EQ.0).OR.(I.EQ.2).OR.(I.EQ.13).OR.(I.EQ.24)
     &	            .OR.(I.EQ.26)) THEN
	          WRITE(IDLOG,*,ERR=1010) "Niveau atmos I : ",I
	          WRITE(IDLOG,*,ERR=1010) 
     &	       "  I     K      RMU          I2          Q2          U2"
	          DO K=-NBMU,NBMU,4
	             WRITE(IDLOG,402,ERR=1010) 
     &			I,K,RMU(K),I2_ATM(I,K),Q2_ATM(I,K),U2_ATM(I,K)
	          ENDDO
	       ENDIF  
	    ENDDO  
	    WRITE(IDLOG,*,ERR=1010) " "
	 ENDIF
	 !**************************************************************
	 
C* Calcul de la fonction source de diffusion d'ordre IG pour l'océan : 
C* ====> I2_SEA, Q2_SEA, U2_SEA
C---------------------------------------------------------------------    	
         CALL OSOAA_FSOURCE_SEA_ORDREIG(IS,NBMU,
     &	 		   SEA_PCRAY_TR,SEA_PC_CHL_TR,SEA_PC_MLP_TR,
     &                     BETA0_MOL_SEA,BETA2_MOL_SEA,
     &			   GAMMA2_MOL_SEA,ALPHA2_MOL_SEA,
     &                     XPL,XRL,XTL,
     &			   I1_SEA,Q1_SEA,U1_SEA,
     &                     BP_CHL,GR_CHL,GT_CHL,
     &                     ARR_CHL,ART_CHL,ATT_CHL,
     &                     BP_MLP,GR_MLP,GT_MLP,
     &                     ARR_MLP,ART_MLP,ATT_MLP,
     &                     GA,I2_SEA,Q2_SEA,U2_SEA) 
     
     	
         !******* Traces d'expertise ***********************************
         IF (TRACE_DEBUG) THEN
	    WRITE(IDLOG,*,ERR=1010) " "
	    WRITE(IDLOG,*,ERR=1010) 
     &	       "Fonction source marine ordre IG : ",IG," : IS = ",IS
	    WRITE(IDLOG,*,ERR=1010) " "
	    DO I=0,CTE_NT_SEA,20
	       WRITE(IDLOG,*,ERR=1010) "Niveau sea I : ",I
	       WRITE(IDLOG,*,ERR=1010) 
     &	       "  I     K      RMU          I2          Q2          U2"
	       DO K=-NBMU,NBMU,4
	          WRITE(IDLOG,402,ERR=1010) 
     &			I,K,RMU(K),I2_SEA(I,K),Q2_SEA(I,K),U2_SEA(I,K)
	       ENDDO
	    ENDDO  
	 ENDIF
	 !**************************************************************	

	
	
C*******************************************************************		
C******** Initialisation à zéro des champs marin et atmosphériques 
C         d'ordre IG  pour ordre S = IS en cours 
C        (toutes directions confondues)
C*******************************************************************	
         !
	  DO I=0,CTE_NT_SEA
	    DO K=-NBMU,NBMU
	       I1_SEA(I,K)=0.	
	       Q1_SEA(I,K)=0.	
	       U1_SEA(I,K)=0.	
	    ENDDO
	 ENDDO
	 
        DO I=0,CTE_NT_ATM
	    DO K=-NBMU,NBMU
	       I1_ATM(I,K)=0.	
	       Q1_ATM(I,K)=0.	
	       U1_ATM(I,K)=0.	
	    ENDDO
	 ENDDO	 	 
	 
	 
C*******************************************************************		
C******** Etape 1 : Signal montant 
C                   Calcul de la luminance réfléchie au fond de mer
C		    ==> Réflexion lambertienne
C
C                  NB: FLUX_DOWN_FOND_IGM1 est le flux d'ordre n-1 
C                      au fond divisé par PI.
C                              -------------
C*******************************************************************	
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	    WRITE(IDLOG,*,ERR=1010)  
     &	    " Order S =",IS," / Order IG =",IG," - Step 1"
	    WRITE(IDLOG,*,ERR=1010) "          Sea bottom reflection "
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	 ENDIF

         I_REFL_BOT = 0.
	 IF (RHO_BOT.NE.0..AND.IS.EQ.0) THEN
	    I_REFL_BOT = RHO_BOT*FLUX_DOWN_FOND_IGM1		
	 ENDIF	    		
	         
	 DO K=1,NBMU
	    I1_SEA(CTE_NT_SEA,K)=I_REFL_BOT
	    Q1_SEA(CTE_NT_SEA,K)=0.
	    U1_SEA(CTE_NT_SEA,K)=0.
	 ENDDO  
	 					
	 IF (TRACE) THEN
	     WRITE(IDLOG,*,ERR=1010) 
     &	     "I reflected on bottom : ",I_REFL_BOT
	 ENDIF    
	   

C*******************************************************************
C******** Etape 2 : Signal montant 
C                   Calcul de la luminance remontante 
C                   du fond de mer à la surface
C*******************************************************************	
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------" 
	    WRITE(IDLOG,*,ERR=1010)  
     &	    " Order S =",IS," / Order IG =",IG," - Step 2"
	    WRITE(IDLOG,*,ERR=1010) "          Integr bottom -> surf"
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	 ENDIF
	 
	 CALL OSOAA_INTEGR_EPOPT_SEA_UP(NBMU,RMU,SEA_TAUEXT_TR,
     &				        I2_SEA,Q2_SEA,U2_SEA,
     &  	                        I1_SEA,Q1_SEA,U1_SEA)


C*******************************************************************	
C******** Etape 3 : Signal montant 
C                   Calcul de la luminance montante transmise 
C                   au passage mer -> air
C*******************************************************************	

         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------" 
	    WRITE(IDLOG,*,ERR=1010)  
     &	    " Order S =",IS," / Order IG =",IG," - Step 3"
	    WRITE(IDLOG,*,ERR=1010) "             Transm. Sea -> air"
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------" 	 
	 ENDIF
	 

C NB: traitements identiques à l'étape 3 ordre 1.
C     à l'initialisation à zéro près de I1_ATM, Q1_ATM et U1_ATM.

	 
	 IF (IMAT_SURF.EQ.1) THEN	!************************
	    				!Etape 3 : Cas mer agitée	 
					!************************	 

            IF (TRACE) WRITE(IDLOG,*,ERR=1010) " Rough sea : "

            DO K=1,NBMU		!K direction de transmission
	    
	       !Champ descendant initialisé nul
	       I1_ATM(CTE_NT_ATM,-K) = 0.
	       Q1_ATM(CTE_NT_ATM,-K) = 0.
	       U1_ATM(CTE_NT_ATM,-K) = 0.

               II2=0.
               QQ2=0.
               UU2=0.			       
	       DO J=1,NBMU	!J direction d'incidence montante
                  Z= GA(J)
                  XI1 = I1_SEA(0,J)
                  XQ1 = Q1_SEA(0,J)
                  XU1 = U1_SEA(0,J)

                  II2 = II2 
     &	              +Z*(XI1*TWA11(J,K)+XQ1*TWA12(J,K)+XU1*TWA13(J,K))
                  QQ2 = QQ2 
     &	              +Z*(XI1*TWA21(J,K)+XQ1*TWA22(J,K)+XU1*TWA23(J,K))
                  UU2 = UU2 
     &	              +Z*(XI1*TWA31(J,K)+XQ1*TWA32(J,K)+XU1*TWA33(J,K))
	       ENDDO
	       
	       COEF_INTSPA = 2./RMU(K)	       
	       I1_ATM(CTE_NT_ATM,K) = II2*COEF_INTSPA
	       Q1_ATM(CTE_NT_ATM,K) = QQ2*COEF_INTSPA
	       U1_ATM(CTE_NT_ATM,K) = UU2*COEF_INTSPA
	    ENDDO

            IF (TRACE) THEN
	       WRITE(IDLOG,*,ERR=1010) 
     &	     " Upward field 0- / Upward field transmitted 0+ :"
	       DO K=1,NBMU
                  WRITE(IDLOG,500,ERR=1010)
     &               ' Ang inc :',DACOS(RMU(K))*180./INCTE_PI,
     &               ' I_INC ',I1_SEA(0,K),
     &		       ' Q_INC ',Q1_SEA(0,K),
     &		       ' U_INC ',U1_SEA(0,K),
     &               ' Ang trans :',DACOS(RMU(K))*180./INCTE_PI,
     &               ' I_TR ',I1_ATM(CTE_NT_ATM,K),
     &		       ' Q_TR ',Q1_ATM(CTE_NT_ATM,K),
     &		       ' U_TR ',U1_ATM(CTE_NT_ATM,K),
     &               ' I_UP_SURF ',I1_ATM(CTE_NT_ATM,K),
     &		       ' Q_UP_SURF ',Q1_ATM(CTE_NT_ATM,K),
     &		       ' U_UP_SURF ',U1_ATM(CTE_NT_ATM,K)
               ENDDO
	    ENDIF	    	    	   
	   
         ELSE  !************************
	       !Etape 3 :  Cas mer plate 
	       !************************
	    
	    !Affectation des valeurs incidentes du champ montant sous la surface
            DO K=1,NBMU
	       IINC(K) = I1_SEA(0,K)
	       QINC(K) = Q1_SEA(0,K)
	       UINC(K) = U1_SEA(0,K)
	     ENDDO
	    
	     CALL  OSOAA_INTERF_MERPLATE("TWA",SEA_IND,RMU,NBMU,
     & 				     IINC,QINC,UINC,IDEV,QDEV,UDEV,
     &                                  TRACE,IER)

            IF (IER.NE.0) GOTO 1200   ! cas d'erreur   
	    
            !Affectation du champ montant transmis au tableau du champ dans 
	    !l'atmosphère au niveau de la surface
            DO K=1,NBMU
	       I1_ATM(CTE_NT_ATM,-K) = 0.
	       Q1_ATM(CTE_NT_ATM,-K) = 0.
	       U1_ATM(CTE_NT_ATM,-K) = 0.
	       
	       I1_ATM(CTE_NT_ATM,K) = IDEV(K)
	       Q1_ATM(CTE_NT_ATM,K) = QDEV(K)
	       U1_ATM(CTE_NT_ATM,K) = UDEV(K)
	     ENDDO     


            IF (TRACE) THEN
	       WRITE(IDLOG,*,ERR=1010) 
     &	     " Upward field 0- / Upward field transmitted 0+ :"
	       DO K=1,NBMU
                  WRITE(IDLOG,500,ERR=1010)
     &               ' Ang inc :',DACOS(RMU(K))*180./INCTE_PI,
     &               ' I_INC ',IINC(K),
     &		       ' Q_INC ',QINC(K),
     &		       ' U_INC ',UINC(K),
     &               ' Ang trans :',DACOS(RMU(K))*180./INCTE_PI,
     &               ' I_TR ',IDEV(K),
     &		       ' Q_TR ',QDEV(K),
     &		       ' U_TR ',UDEV(K),
     &               ' I_UP_SURF ',I1_ATM(CTE_NT_ATM,K),
     &		       ' Q_UP_SURF ',Q1_ATM(CTE_NT_ATM,K),
     &		       ' U_UP_SURF ',U1_ATM(CTE_NT_ATM,K)
               ENDDO
	    ENDIF	    
	    	    
         ENDIF  !Fin test mer plate ou agitée      
	 
  

	 	 
C*******************************************************************
C******** Etape 4 : Signal montant 
C                   Réflexion lambertienne de l'écume
C*******************************************************************	
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	    WRITE(IDLOG,*,ERR=1010)  
     &	    " Order S =",IS," / Order IG =",IG," - Step 4"
	    WRITE(IDLOG,*,ERR=1010) "             Foam reflection"
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	    
	 ENDIF

         IF ((RHO_SURF.NE.0.).AND.(IS.EQ.0)) THEN
	    I_REFL_SURF = RHO_SURF*FLUX_DOWN_SURF_IGM1
	    DO K=1,NBMU
	       I1_ATM(CTE_NT_ATM,K) = I1_ATM(CTE_NT_ATM,K) + I_REFL_SURF
	    ENDDO
	 ENDIF 

	 IF (TRACE) THEN
	     WRITE(IDLOG,*,ERR=1010) 
     &	     'I reflected on surface :',I_REFL_SURF
	     WRITE(IDLOG,*,ERR=1010) '  - Surface albedo :',RHO_SURF
	     WRITE(IDLOG,*,ERR=1010) '  - Incidente flux / PI :',
     &       FLUX_DOWN_SURF_IGM1
	     WRITE(IDLOG,*,ERR=1010) " "
	 ENDIF    	

C*************************************************************************	 
C******** Etape 5 : Signal montant 
C                   Calcul de la luminance remontante de la surface au TOA
C*************************************************************************
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------" 
	    WRITE(IDLOG,*,ERR=1010)  
     &	    " Order S =",IS," / Order IG =",IG," - Step 5"
	    WRITE(IDLOG,*,ERR=1010) "             Integr surf -> TOA"
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"   
	 ENDIF

	 CALL OSOAA_INTEGR_EPOPT_ATM_UP(NBMU,RMU,ATM_TAUEXT_TR,
     &				          I2_ATM,Q2_ATM,U2_ATM,
     &  	                        I1_ATM,Q1_ATM,U1_ATM)
 

     
         !******* Traces d'expertise ***********************************
         IF (TRACE_DEBUG) THEN
	    WRITE(IDLOG,*,ERR=1010) " "
	    WRITE(IDLOG,*,ERR=1010)
     &	       "Luminance atmos up ordre IG - étape 5 : IS = ",IS
	    WRITE(IDLOG,*,ERR=1010) " "
	    DO I=0,CTE_NT_ATM
	       IF ((I.EQ.0).OR.(I.EQ.2).OR.(I.EQ.13).OR.(I.EQ.24)
     &	            .OR.(I.EQ.26)) THEN
	          WRITE(IDLOG,*,ERR=1010) "Niveau atmos I : ",I
	          WRITE(IDLOG,*,ERR=1010) 
     &	       "  I     K      RMU          I1          Q1          U1"
	          DO K=-NBMU,NBMU,4
	             WRITE(IDLOG,402,ERR=1010) 
     &			I,K,RMU(K),I1_ATM(I,K),Q1_ATM(I,K),U1_ATM(I,K)
	          ENDDO
	       ENDIF
	    ENDDO  
	 ENDIF
	 !**************************************************************     
	 
	 
C*************************************************************************		     		
C******** Etape 6 : Signal descendant 
C                   Calcul de la luminance atmosphérique descendante
C		    du TOA à la surface
C*************************************************************************		     
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------" 
	    WRITE(IDLOG,*,ERR=1010)  
     &	    " Order S =",IS," / Order IG =",IG," - Step 6"
	    WRITE(IDLOG,*,ERR=1010) "             Integr TOA -> surf"
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"       
	 ENDIF
	 
	 !Initialisation du champ descendant au TOA
	 DO K=-NBMU,-1
	    I1_ATM(0,K)=0.
            Q1_ATM(0,K)=0.
            U1_ATM(0,K)=0.
	 ENDDO

	 CALL OSOAA_INTEGR_EPOPT_ATM_DOWN(NBMU,RMU,ATM_TAUEXT_TR,
     &				          I2_ATM,Q2_ATM,U2_ATM,
     &  	                          I1_ATM,Q1_ATM,U1_ATM)		
  

         !******* Traces d'expertise ***********************************
         IF (TRACE_DEBUG) THEN
	    WRITE(IDLOG,*,ERR=1010) " "
	    WRITE(IDLOG,*,ERR=1010) 
     &	      "Luminance atmos up & down ordre IG - étape 6 : IS = ",IS
	    WRITE(IDLOG,*,ERR=1010) " "
	    DO I=0,CTE_NT_ATM
	       IF ((I.EQ.0).OR.(I.EQ.2).OR.(I.EQ.13).OR.(I.EQ.24)
     &	            .OR.(I.EQ.26)) THEN
	          WRITE(IDLOG,*,ERR=1010) "Niveau atmos I : ",I
	          WRITE(IDLOG,*,ERR=1010) 
     &	       "  I     K      RMU          I1          Q1          U1"
	          DO K=-NBMU,NBMU,4
	             WRITE(IDLOG,402,ERR=1010) 
     &		       I,K,RMU(K),I1_ATM(I,K),Q1_ATM(I,K),U1_ATM(I,K)
	          ENDDO
	       ENDIF
	    ENDDO  
	 ENDIF
	 !**************************************************************     
	 
	 
C*************************************************************************	     		
C******** Etape 7 : Signal descendant 
C                   Calcul de la luminance descendante transmise 
C                   au passage air -> mer
C*************************************************************************			
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------" 
	    WRITE(IDLOG,*,ERR=1010)  
     &	    " Order S =",IS," / Order IG =",IG," - Step 7"
	    WRITE(IDLOG,*,ERR=1010) "             Transm. Air -> sea"
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"     
	 ENDIF

C NB: traitements identiques à l'étape 7 ordre 1.


	 IF (IMAT_SURF.EQ.1) THEN	!************************
	    				!Etape 7 : Cas mer agitée	 
					!************************	 

            IF (TRACE) WRITE(IDLOG,*,ERR=1010) " Rough sea : "

            DO K=1,NBMU		!K direction de transmission
	       
	       !Champ montant déjà initialisé : pas redéfini ici

               II2=0.
               QQ2=0.
               UU2=0.	
	       		    		       
	       DO J=1,NBMU	!J direction d'incidence descendante
                  Z= GA(J)
                  XI1 = I1_ATM(CTE_NT_ATM,-J) 		
                  XQ1 = Q1_ATM(CTE_NT_ATM,-J) 
                  XU1 = U1_ATM(CTE_NT_ATM,-J)

                  II2 = II2 
     &	              +Z*(XI1*TAW11(J,K)+XQ1*TAW12(J,K)+XU1*TAW13(J,K))
                  QQ2 = QQ2 
     &	              +Z*(XI1*TAW21(J,K)+XQ1*TAW22(J,K)+XU1*TAW23(J,K))
                  UU2 = UU2 
     &	              +Z*(XI1*TAW31(J,K)+XQ1*TAW32(J,K)+XU1*TAW33(J,K)) 

	       ENDDO             
	      
	       COEF_INTSPA = 2./RMU(K)	 
	       I1_SEA(0,-K)= II2*COEF_INTSPA
	       Q1_SEA(0,-K)= QQ2*COEF_INTSPA
	       U1_SEA(0,-K)= UU2*COEF_INTSPA	       		
      
	    ENDDO
    	   
            IF (TRACE) THEN
	       WRITE(IDLOG,*,ERR=1010) " "
	       WRITE(IDLOG,*,ERR=1010) 
     &	      " Downward radiance O+ / Transmitted radiance 0- :"
	       DO K=1,NBMU
                  WRITE(IDLOG,500,ERR=1010)
     &              ' Ang inc :',DACOS(RMU(-K))*180./INCTE_PI,
     &              ' I_INC ',I1_ATM(CTE_NT_ATM,-K),
     &		      ' Q_INC ',Q1_ATM(CTE_NT_ATM,-K),
     &		      ' U_INC ',U1_ATM(CTE_NT_ATM,-K),
     &              ' Ang trans :',DACOS(RMU(-K))*180./INCTE_PI,
     &              ' I_TR ',I1_SEA(0,-K),
     &		      ' Q_TR ',Q1_SEA(0,-K),
     &		      ' U_TR ',U1_SEA(0,-K),
     &              ' I_D_SSURF ',I1_SEA(0,-K),
     &		      ' Q_D_SSURF ',Q1_SEA(0,-K),
     &		      ' U_D_SSURF ',U1_SEA(0,-K)
               ENDDO
	    ENDIF	        	   
	    	   
         ELSE  !************************
	       !Etape 7 :  Cas mer plate 
	       !************************
	    
	    !Affectation des valeurs incidentes du champ descendant en surface
            DO K=1,NBMU
	       IINC(K) = I1_ATM(CTE_NT_ATM,-K)
	       QINC(K) = Q1_ATM(CTE_NT_ATM,-K)
	       UINC(K) = U1_ATM(CTE_NT_ATM,-K)
	    ENDDO
            
	    
	    CALL  OSOAA_INTERF_MERPLATE("TAW",SEA_IND,RMU,NBMU,
     & 				        IINC,QINC,UINC,IDEV,QDEV,UDEV,
     &                                  TRACE,IER)

            IF (IER.NE.0) GOTO 1200   ! cas d'erreur   
	    
            !Affectation du champ descendant transmis au tableau du champ dans l'eau
	    !au niveau de la surface 
	    !(pas d'initialisation du champ montant car il est déjà calculé).
            DO K=1,NBMU       
	       I1_SEA(0,-K) = IDEV(K)
	       Q1_SEA(0,-K) = QDEV(K)
	       U1_SEA(0,-K) = UDEV(K)
	    ENDDO     

            IF (TRACE) THEN
	       WRITE(IDLOG,*,ERR=1010) " "
	       WRITE(IDLOG,*,ERR=1010) 
     &	      " Downward radiance O+ / Transmitted radiance 0- :"
	       DO K=1,NBMU
                  WRITE(IDLOG,500,ERR=1010)
     &              ' Ang inc :',DACOS(RMU(-K))*180./INCTE_PI,
     &              ' I_INC ',IINC(K),
     &		      ' Q_INC ',QINC(K),
     &		      ' U_INC ',UINC(K),
     &              ' Ang trans :',DACOS(RMU(-K))*180./INCTE_PI,
     &              ' I_TR ',IDEV(K),
     &		      ' Q_TR ',QDEV(K),
     &		      ' U_TR ',UDEV(K),
     &              ' I_D_SSURF ',I1_SEA(0,-K),
     &		      ' Q_D_SSURF ',Q1_SEA(0,-K),
     &		      ' U_D_SSURF ',U1_SEA(0,-K)
               ENDDO
	    ENDIF	    
	
         ENDIF  !Fin test mer plate ou agitée       


	 
		
C*************************************************************************		  
C******** Etape 8 : Signal descendant 
C                   Réflexion mer -> mer du signal marin montant 
C                   de même ordre (calculé à l'étape 2)
C*************************************************************************	
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	    WRITE(IDLOG,*,ERR=1010)  
     &	    " Order S =",IS," / Order IG =",IG," - Step 8"
	    WRITE(IDLOG,*,ERR=1010) "             Reflex. Sea -> sea"
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	 ENDIF
         
C NB: traitements identiques à l'étape 8 ordre 1.

		
	 IF (IMAT_SURF.EQ.1) THEN	!************************
	    				!Etape 8 : Cas mer agitée	 
					!************************	 
            IF (TRACE) THEN
	       WRITE(IDLOG,*,ERR=1010) " Rough sea : "
	       
	       DO K=1,NBMU		!K direction de réflexion
                 IINC(K) = I1_SEA(0,K)	!Stockage pour le fichier Trace
	          QINC(K) = Q1_SEA(0,K)
	          UINC(K) = U1_SEA(0,K)		  
	       ENDDO
            ENDIF !Fin Trace
	    
            DO K=1,NBMU		!K direction de réflexion

               II2=0.
               QQ2=0.
               UU2=0.			       
	       DO J=1,NBMU	!J direction d'incidence montante
                  Z= GA(J)
                  XI1 = I1_SEA(0,J) 		
                  XQ1 = Q1_SEA(0,J)
                  XU1 = U1_SEA(0,J)

                  II2 = II2 
     &	              +Z*(XI1*RWW11(J,K)+XQ1*RWW12(J,K)+XU1*RWW13(J,K))
                  QQ2 = QQ2 
     &	              +Z*(XI1*RWW21(J,K)+XQ1*RWW22(J,K)+XU1*RWW23(J,K))
                  UU2 = UU2 
     &	              +Z*(XI1*RWW31(J,K)+XQ1*RWW32(J,K)+XU1*RWW33(J,K))
	       ENDDO

	       COEF_INTSPA = 1./(2.*RMU(K))	 
	       IDEV(K)=II2*COEF_INTSPA 
	       QDEV(K)=QQ2*COEF_INTSPA
	       UDEV(K)=UU2*COEF_INTSPA
	             
	       I1_SEA(0,-K)= I1_SEA(0,-K) + IDEV(K)	! Affectation au champ 
	       Q1_SEA(0,-K)= Q1_SEA(0,-K) + QDEV(K)     ! descendant
	       U1_SEA(0,-K)= U1_SEA(0,-K) + UDEV(K)
	       
	    ENDDO
		   
	   
         ELSE !************************
	       !Etape 8 :  Cas mer plate 
	       !************************

	    !Affectation des valeurs incidentes du champ montant sous la surface
            DO K=1,NBMU
	       IINC(K) = I1_SEA(0,K)
	       QINC(K) = Q1_SEA(0,K)
	       UINC(K) = U1_SEA(0,K)
	     ENDDO
	    
	     CALL  OSOAA_INTERF_MERPLATE("RWW",SEA_IND,RMU,NBMU,
     & 				     IINC,QINC,UINC,IDEV,QDEV,UDEV,
     &                                  TRACE,IER)

            IF (IER.NE.0) GOTO 1200   ! cas d'erreur   
	    
	    
            !Ajout du champ montant réfléchi vers le fond au tableau du champ dans l'eau
	    !sous la surface.
            DO K=1,NBMU       
	       I1_SEA(0,-K) = I1_SEA(0,-K) + IDEV(K)	!Affectation au champ descendant
	       Q1_SEA(0,-K) = Q1_SEA(0,-K) + QDEV(K)
	       U1_SEA(0,-K) = U1_SEA(0,-K) + UDEV(K)
	     ENDDO     



         ENDIF  !Fin test mer plate ou agitée       
	 		
	
         IF (TRACE) THEN
	     WRITE(IDLOG,*,ERR=1010) 
     &	     " Upward radiance 0- / Downward radiance 0- :"
	     DO K=1,NBMU
                WRITE(IDLOG,500,ERR=1010)
     &             ' Ang inc :',DACOS(RMU(K))*180./INCTE_PI,
     &             ' I_INC ',IINC(K),
     &		     ' Q_INC ',QINC(K),
     &		     ' U_INC ',UINC(K),
     &             ' Ang trans :',DACOS(RMU(-K))*180./INCTE_PI,
     &             ' I_RF ',IDEV(K),
     &		     ' Q_RF ',QDEV(K),
     &		     ' U_RF ',UDEV(K),
     &             ' I_D_SSURF ',I1_SEA(0,-K),
     &		     ' Q_D_SSURF ',Q1_SEA(0,-K),
     &		     ' U_D_SSURF ',U1_SEA(0,-K)
             ENDDO
	 ENDIF	    		
		
		
C*************************************************************************				
C******** Etape 9 : Signal descendant 
C                   Calcul de la luminance descendante 
C                   de la surface au fond de mer 
C*************************************************************************	
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	    WRITE(IDLOG,*,ERR=1010)  
     &	    " Order S =",IS," / Order IG =",IG," - Step 9"
	    WRITE(IDLOG,*,ERR=1010) "          Integr Surf -> bottom"
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"
	 ENDIF
	 
	 !Initialisation du champ descendant au TOA
	 DO K=-NBMU,-1
	    I1_ATM(0,K)=0.
            Q1_ATM(0,K)=0.
            U1_ATM(0,K)=0.
	 ENDDO

	 CALL OSOAA_INTEGR_EPOPT_SEA_DOWN(NBMU,RMU,SEA_TAUEXT_TR,
     &				          I2_SEA,Q2_SEA,U2_SEA,
     &  	                          I1_SEA,Q1_SEA,U1_SEA)		
  
		
         !******* Traces d'expertise ***********************************
         IF (TRACE_DEBUG) THEN
	    WRITE(IDLOG,*,ERR=1010) " "
	    WRITE(IDLOG,*,ERR=1010) 
     &	       "Luminance sea up & down ordre IG - étape 9 : IS = ",IS
	    WRITE(IDLOG,*,ERR=1010) " "
	    DO I=0,CTE_NT_SEA,20
	       WRITE(IDLOG,*,ERR=1010) "Niveau sea I : ",I
	       WRITE(IDLOG,*,ERR=1010) 
     &	       "  I     K      RMU          I1          Q1          U1"
	       DO K=-NBMU,NBMU,4
	          WRITE(IDLOG,402,ERR=1010) 
     &		       I,K,RMU(K),I1_SEA(I,K),Q1_SEA(I,K),U1_SEA(I,K)
	       ENDDO
	    ENDDO  
	 ENDIF
	 !**************************************************************  



C*************************************************************************			
C******** Etape 10 : Signal montant 
C                    Ajout au signal montant de la luminance atmosphérique
C                    descendante réfléchie sur la surface (air -> air)
C
C		     --> Affectation au profil atmosphérique 
C			 pour le signal montant
C*************************************************************************
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------" 
	    WRITE(IDLOG,*,ERR=1010)  
     &	    " Order S =",IS," / Order IG =",IG," - Step 10"
	    WRITE(IDLOG,*,ERR=1010) "             Reflex. Air -> air"
	    WRITE(IDLOG,*,ERR=1010) "-------------------------------"     
	 ENDIF
	
C NB: traitements identiques à l'étape 10 ordre 1.

	 	
	 IF (IMAT_SURF.EQ.1) THEN	!*************************
	    				!Etape 10 : Cas mer agitée	 
					!*************************	 
	     IF (TRACE) THEN
	       WRITE(IDLOG,*,ERR=1010) " Rough sea : "
	       WRITE(IDLOG,*,ERR=1010) 
     & 	            "  Reflexion air / air on surface :"
            ENDIF	     	 
	    
            DO K=1,NBMU		!K direction de réflexion montante

               II2=0.
               QQ2=0.
               UU2=0.			       
	       DO J=1,NBMU	!J direction d'incidence descendante
                  Z= GA(J)
                  XI1 = I1_ATM(CTE_NT_ATM,-J) 		
                  XQ1 = Q1_ATM(CTE_NT_ATM,-J)
                  XU1 = U1_ATM(CTE_NT_ATM,-J)

                  II2 = II2 
     &	              +Z*(XI1*RAA11(J,K)+XQ1*RAA12(J,K)+XU1*RAA13(J,K))
                  QQ2 = QQ2 
     &	              +Z*(XI1*RAA21(J,K)+XQ1*RAA22(J,K)+XU1*RAA23(J,K))
                  UU2 = UU2 
     &	              +Z*(XI1*RAA31(J,K)+XQ1*RAA32(J,K)+XU1*RAA33(J,K))
	       ENDDO
	       
	       COEF_INTSPA = 1./(2.*RMU(K))	       
	       I1_REFL_L1ATM_SURF= II2*COEF_INTSPA	! Affectation au champ montant 
	       Q1_REFL_L1ATM_SURF= QQ2*COEF_INTSPA	! en surface
	       U1_REFL_L1ATM_SURF= UU2*COEF_INTSPA

	    
	       DO I=CTE_NT_ATM,0,-1	!Ajout au champ montant pour chaque niveau
	       
	          DTAU = ATM_TAUEXT_TR(I)-ATM_TAUEXT_TR_TOT
	          ATTVIS = DEXP(DTAU/RMU(K))
		  
		   I1_ATM(I,K) = I1_ATM(I,K) + I1_REFL_L1ATM_SURF*ATTVIS
		   Q1_ATM(I,K) = Q1_ATM(I,K) + Q1_REFL_L1ATM_SURF*ATTVIS
		   U1_ATM(I,K) = U1_ATM(I,K) + U1_REFL_L1ATM_SURF*ATTVIS
	       
	       ENDDO !Fin boucle sur profil atmosphérique

               IF (TRACE) THEN
                   WRITE(IDLOG,511,ERR=1010)
     &              ' Ang inc :',-DACOS(RMU(-K))*180./INCTE_PI,
     &              ' Ang refl:',DACOS(RMU(K))*180./INCTE_PI,
     &              ' I_RF ',I1_REFL_L1ATM_SURF,
     &		      ' Q_RF ',Q1_REFL_L1ATM_SURF,
     &		      ' U_RF ',U1_REFL_L1ATM_SURF,
     &              ' I_UP_SURF ',I1_ATM(CTE_NT_ATM,K),
     &		      ' Q_UP_SURF ',Q1_ATM(CTE_NT_ATM,K),
     &		      ' U_UP_SURF ',U1_ATM(CTE_NT_ATM,K)
	       ENDIF	     	 
	                        
	    ENDDO !Fin boucle sur direction propagation montante
	    
	    	    
         ELSE  !*************************
	       !Etape 10 :  Cas mer plate 
	       !*************************
	    
	    !Affectation des valeurs incidentes de l'éclairement atmosphérique primaire 
	    !en surface
            DO K=1,NBMU
	       IINC(K) = I1_ATM(CTE_NT_ATM,-K)
	       QINC(K) = Q1_ATM(CTE_NT_ATM,-K)
	       UINC(K) = U1_ATM(CTE_NT_ATM,-K)
	     ENDDO         
	    
	     CALL  OSOAA_INTERF_MERPLATE("RAA",SEA_IND,RMU,NBMU,
     & 				     IINC,QINC,UINC,IDEV,QDEV,UDEV,
     &                                  TRACE,IER)

            IF (IER.NE.0) GOTO 1200   ! cas d'erreur   
	    
            !Affectation du champ montant réfléchi au tableau du champ dans l'air
	    !(prise en compte de l'atténuation de la surface au niveau du profil).
            DO K=1,NBMU		!Direction de réflexion montante
	    
	       DO I=CTE_NT_ATM,0,-1
	       
	          DTAU = ATM_TAUEXT_TR(I)-ATM_TAUEXT_TR_TOT
	          ATTVIS = DEXP(DTAU/RMU(K))
		  
		  I1_ATM(I,K) = I1_ATM(I,K) + IDEV(K)*ATTVIS	
		  Q1_ATM(I,K) = Q1_ATM(I,K) + QDEV(K)*ATTVIS	
		  U1_ATM(I,K) = U1_ATM(I,K) + UDEV(K)*ATTVIS	
	       
	       ENDDO !Fin boucle sur profil atmosphérique
                 
	     ENDDO !Fin boucle sur direction propagation montante


            IF (TRACE) THEN
	       WRITE(IDLOG,*,ERR=1010) 
     &	       " Downward radiance 0+ / Upward radiance 0+ :"
	       DO K=1,NBMU		
                   WRITE(IDLOG,500,ERR=1010)
     &              ' Ang inc :',-DACOS(RMU(-K))*180./INCTE_PI,
     &              ' I_INC ',IINC(K),
     &		    ' Q_INC ',QINC(K),
     &		    ' U_INC ',UINC(K),
     &              ' Ang refl  :',DACOS(RMU(K))*180./INCTE_PI,
     &              ' I_RF ',IDEV(K),
     &		    ' Q_RF ',QDEV(K),
     &		    ' U_RF ',UDEV(K),
     &              ' I_UP_SURF ',I1_ATM(CTE_NT_ATM,K),
     &		    ' Q_UP_SURF ',Q1_ATM(CTE_NT_ATM,K),
     &		    ' U_UP_SURF ',U1_ATM(CTE_NT_ATM,K)
	       ENDDO
	    ENDIF	     	 
	       
	       
         ENDIF  !Fin test mer plate ou agitée       


         !******* Traces d'expertise ***********************************
         IF (TRACE_DEBUG) THEN
	    WRITE(IDLOG,*,ERR=1010) " "
	    WRITE(IDLOG,*,ERR=1010) 
     &	     "Luminance atmos up & down ordre IG - étape 10 : IS = ",IS
	    WRITE(IDLOG,*,ERR=1010) " "
	    DO I=0,CTE_NT_ATM
	       IF ((I.EQ.0).OR.(I.EQ.2).OR.(I.EQ.13).OR.(I.EQ.24)
     &	            .OR.(I.EQ.26)) THEN
	          WRITE(IDLOG,*,ERR=1010) "Niveau atmos I : ",I
	          WRITE(IDLOG,*,ERR=1010) 
     &	       "  I     K      RMU          I1          Q1          U1"
	          DO K=-NBMU,NBMU,4
	             WRITE(IDLOG,402,ERR=1010) 
     &		       I,K,RMU(K),I1_ATM(I,K),Q1_ATM(I,K),U1_ATM(I,K)
	          ENDDO
	       ENDIF
	    ENDDO  
	 ENDIF
	 !**************************************************************  




	    	  	 
C--------------------------------------------------------------
C* Stockage des résultats de calcul de l'interaction d'ordre IG 
C---------------------------------------------------------------


C* Stockage du champ d'ordre IG en cours : 
C----------------------------------------

	 DO K=-NBMU,NBMU
	    DO I=0,CTE_NT_ATM
	        G1(I,K)=I1_ATM(I,K)
	        H1(I,K)=Q1_ATM(I,K)
                P1(I,K)=U1_ATM(I,K)
	    ENDDO 
	    
	    DO ISEA=0,CTE_NT_SEA
	        I=ISEA+CTE_NT_ATM+1		
	        G1(I,K)=I1_SEA(ISEA,K)
	        H1(I,K)=Q1_SEA(ISEA,K)
                P1(I,K)=U1_SEA(ISEA,K) 
	    ENDDO       
	 ENDDO
	 
C* --------------------------------------
C* Stockage des champs d'ordre IG
C*      montant TOA et descendant au fond     => G1_TOA_FOND, H1_TOA_FOND, P1_TOA_FOND
C*      montant et descendant à la surface    => G1_SURF,     H1_SURF,     P1_SURF
C*      montant et descendant sous la surface => G1_SSURF,    H1_SSURF,    P1_SSURF
C-------------------------------------------------------------------------------------
         DO K=-NBMU,NBMU
	 
	    IF (K<0) THEN 
	    
	       !Champ descendant au fond
	       G1_TOA_FOND(K)=I1_SEA(CTE_NT_SEA,K)		!Paramètre I(ig)	
	       H1_TOA_FOND(K)=Q1_SEA(CTE_NT_SEA,K)		!Paramètre Q(ig)	
               P1_TOA_FOND(K)=U1_SEA(CTE_NT_SEA,K)		!Paramètre U(ig)	
	       
	       !Champ descendant au niveau de la surface
	       G1_SURF(K)=I1_ATM(CTE_NT_ATM,K)
	       H1_SURF(K)=Q1_ATM(CTE_NT_ATM,K)
               P1_SURF(K)=U1_ATM(CTE_NT_ATM,K)
	       
	       !Champ descendant sous la surface
	       G1_SSURF(K)=I1_SEA(0,K)
	       H1_SSURF(K)=Q1_SEA(0,K)
               P1_SSURF(K)=U1_SEA(0,K)    
	       
	    ELSE
	     
	       !Champ émergeant au TOA
	       G1_TOA_FOND(K)=I1_ATM(0,K)
	       H1_TOA_FOND(K)=Q1_ATM(0,K)
               P1_TOA_FOND(K)=U1_ATM(0,K)
	       
	       !Champ montant au niveau de la surface  
	       G1_SURF(K)=I1_ATM(CTE_NT_ATM,K)		
	       H1_SURF(K)=Q1_ATM(CTE_NT_ATM,K)
               P1_SURF(K)=U1_ATM(CTE_NT_ATM,K)
	       
	       !Champ montant sous la surface
	       G1_SSURF(K)=I1_SEA(0,K)			
	       H1_SSURF(K)=Q1_SEA(0,K)
               P1_SSURF(K)=U1_SEA(0,K)    
	       
	    ENDIF
	    
         ENDDO !Fin boucle sur les directions de propagation



 

C*  Test de convergence en série géométrique et stockage de l'ordre IG-2
C-------------------------------------------------------------------------
         IF (IG.EQ.2) GO TO 506
      
         CALL OSOAA_PARAM_CONV(NBMU,
     & 			       A1_TOA_FOND,B1_TOA_FOND,C1_TOA_FOND,
     &	 		       D1_TOA_FOND,E1_TOA_FOND,F1_TOA_FOND,
     &			       G1_TOA_FOND,H1_TOA_FOND,P1_TOA_FOND,
     &                         I3_TOA_FOND,Q3_TOA_FOND,U3_TOA_FOND,
     &			       Z1_TOA_FOND)

         CALL OSOAA_PARAM_CONV(NBMU,A1_SURF,B1_SURF,C1_SURF,
     &	 		       D1_SURF,E1_SURF,F1_SURF,
     &			       G1_SURF,H1_SURF,P1_SURF,
     &                         I3_SURF,Q3_SURF,U3_SURF,
     &			       Z1_SURF)
     
         CALL OSOAA_PARAM_CONV(NBMU,A1_SSURF,B1_SSURF,C1_SSURF,
     &	 		       D1_SSURF,E1_SSURF,F1_SSURF,
     &			       G1_SSURF,H1_SSURF,P1_SSURF,
     &                         I3_SSURF,Q3_SSURF,U3_SSURF,
     &			       Z1_SSURF)
     

         Z1=DMAX1(Z1_TOA_FOND,Z1_SURF,Z1_SSURF) 
	 
	          
C* Si on excède le seuil CTE_PH_SEUIL_CV_SG pour au moins un angle mu, 
C* on n'est pas en condition de convergence. On passe à l'ordre suivant 
C* de diffusion.
         IF (Z1.GT.CTE_PH_SEUIL_CV_SG) GO TO 506


C* Si on est en régime de convergence, on ajoute la queue de la
C* série géométrique à la somme des diffusions précédentes.
C* ---> Applique à toutes les directions et tous les niveaux
C-------------------------------------------------------
	 CALL OSOAA_AJOUT_QUEUE(NBMU,D1,E1,F1,G1,H1,P1,
     &                          I3,Q3,U3)

C* Fichier trace 
C------------------------------------------------------  
         IF (TRACE) THEN
            WRITE(IDLOG,*,ERR=1010) 
     &       '    Convergence in geometric series from IG = ',
     &       IG
            WRITE(IDLOG,*,ERR=1010) ' '	   
         ENDIF

C* On quitte la boucle sur les diffusions.
C* Passage au test sur l'ordre de la décomposition en séries de Fourier.
C---------------------------------------------------------------------
         GO TO 505


C* On reste dans la boucle sur les diffusions.
C* Stockage de l'ordre IG-1 dans A1, B1 et C1.
C* Stockage de l'ordre IG dans D1, E1 et F1.
C* pour le test au passage de l'ordre suivant IG+1.
C-------------------------------------------------------
  506    DO 86 K=-NBMU,NBMU
            A1_TOA_FOND(K)=D1_TOA_FOND(K)
            B1_TOA_FOND(K)=E1_TOA_FOND(K)
            C1_TOA_FOND(K)=F1_TOA_FOND(K)
            D1_TOA_FOND(K)=G1_TOA_FOND(K)
            E1_TOA_FOND(K)=H1_TOA_FOND(K)
            F1_TOA_FOND(K)=P1_TOA_FOND(K)
	    
	    A1_SURF(K)=D1_SURF(K)
            B1_SURF(K)=E1_SURF(K)
            C1_SURF(K)=F1_SURF(K)
            D1_SURF(K)=G1_SURF(K)
            E1_SURF(K)=H1_SURF(K)
            F1_SURF(K)=P1_SURF(K)
	    
	    A1_SSURF(K)=D1_SSURF(K)
            B1_SSURF(K)=E1_SSURF(K)
            C1_SSURF(K)=F1_SSURF(K)
            D1_SSURF(K)=G1_SSURF(K)
            E1_SSURF(K)=H1_SSURF(K)
            F1_SSURF(K)=P1_SSURF(K)
   86    CONTINUE

         !Stockage ordre IG dans D1,E1 et F1 sur tout le profil
	 DO K=-NBMU,NBMU
	    DO I=0,NT_TOT
	        D1(I,K)=G1(I,K)
	        E1(I,K)=H1(I,K)
                F1(I,K)=P1(I,K)
	    ENDDO       
	 ENDDO   
	    
C* Calcul de la somme des diffusions
C------------------------------------
17       DO 400 J=1,NBMU
            I3_TOA_FOND(J)=I3_TOA_FOND(J)+I1_ATM(0,J)
            Q3_TOA_FOND(J)=Q3_TOA_FOND(J)+Q1_ATM(0,J)
            U3_TOA_FOND(J)=U3_TOA_FOND(J)+U1_ATM(0,J)
            I3_TOA_FOND(-J)=I3_TOA_FOND(-J)+I1_SEA(CTE_NT_SEA,-J)
            Q3_TOA_FOND(-J)=Q3_TOA_FOND(-J)+Q1_SEA(CTE_NT_SEA,-J)
            U3_TOA_FOND(-J)=U3_TOA_FOND(-J)+U1_SEA(CTE_NT_SEA,-J)
	    
	    I3_SURF(J)=I3_SURF(J)+I1_ATM(CTE_NT_ATM,J)
            Q3_SURF(J)=Q3_SURF(J)+Q1_ATM(CTE_NT_ATM,J)
            U3_SURF(J)=U3_SURF(J)+U1_ATM(CTE_NT_ATM,J)
            I3_SURF(-J)=I3_SURF(-J)+I1_ATM(CTE_NT_ATM,-J)
            Q3_SURF(-J)=Q3_SURF(-J)+Q1_ATM(CTE_NT_ATM,-J)
            U3_SURF(-J)=U3_SURF(-J)+U1_ATM(CTE_NT_ATM,-J)
	    
	    I3_SSURF(J)=I3_SSURF(J)+I1_SEA(0,J)
            Q3_SSURF(J)=Q3_SSURF(J)+Q1_SEA(0,J)
            U3_SSURF(J)=U3_SSURF(J)+U1_SEA(0,J)
            I3_SSURF(-J)=I3_SSURF(-J)+I1_SEA(0,-J)
            Q3_SSURF(-J)=Q3_SSURF(-J)+Q1_SEA(0,-J)
            U3_SSURF(-J)=U3_SSURF(-J)+U1_SEA(0,-J)
  400    CONTINUE
  
  
         !Somme des diffusions pour tableaux globaux tous niveaux
         DO K=-NBMU,NBMU
	    DO I=0,CTE_NT_ATM
	        I3(I,K)=I3(I,K)+I1_ATM(I,K)
	        Q3(I,K)=Q3(I,K)+Q1_ATM(I,K)
                U3(I,K)=U3(I,K)+U1_ATM(I,K)
	    ENDDO 
	    
	    DO ISEA=0,CTE_NT_SEA
	        I=ISEA+CTE_NT_ATM+1		
	        I3(I,K)=I3(I,K)+I1_SEA(ISEA,K)
	        Q3(I,K)=Q3(I,K)+Q1_SEA(ISEA,K)
                U3(I,K)=U3(I,K)+U1_SEA(ISEA,K) 
	    ENDDO       
	 ENDDO


C* Test d'arrêt sur les diffusions quand les termes suivant 
C* deviennent très petits
C*   (G1, H1, P1 ) = (I, Q, U)(ordre ig)
C*   (I3, Q3, U3 ) = (I, Q, U)(ordres cumulés jusqu'à ig inclu)
C--------------------------------------------------------------
         CALL OSOAA_ARRET_DIFFUS(NBMU,
     &				 G1_TOA_FOND,H1_TOA_FOND,P1_TOA_FOND,
     &                           I3_TOA_FOND,Q3_TOA_FOND,U3_TOA_FOND,
     &                           Z1_TOA_FOND)

         CALL OSOAA_ARRET_DIFFUS(NBMU,
     &				 G1_SURF,H1_SURF,P1_SURF,
     &                           I3_SURF,Q3_SURF,U3_SURF,
     &                           Z1_SURF)
     
         CALL OSOAA_ARRET_DIFFUS(NBMU,
     &				 G1_SSURF,H1_SSURF,P1_SSURF,
     &                           I3_SSURF,Q3_SSURF,U3_SSURF,
     &                           Z1_SSURF)
     
         Z1=DMAX1(Z1_TOA_FOND,Z1_SURF,Z1_SSURF) 
	     
         IF (Z1.GT.CTE_PH_SEUIL_SUMDIF) GOTO 508
      
C* Fichier trace 
C------------------------------------------------------  
         IF (TRACE) THEN
            WRITE(IDLOG,*,ERR=1010) 
     &        '    End of the scattering loop due to the test',
     &        '    comparing radiance of order IG to the accumulation',
     &        ' of previous orders'
            WRITE(IDLOG,*,ERR=1010) ' '	   
         ENDIF
	 
	 
         GOTO 505 !Sortie de boucle sur IG




C* Test d'arrêt sur le nombre maximal de diffusions.
C---------------------------------------------------
  508    IF(IG.LT.IGMAX) GOTO 503
  
C* Fichier trace 
C------------------------------------------------------  
         IF (TRACE) THEN
            WRITE(IDLOG,*,ERR=1010) 
     &       '    End of the scattering loop because of reaching',
     &       '    the maximal order : IGMAX = ',IGMAX
            WRITE(IDLOG,*,ERR=1010) ' '	   
         ENDIF
	 
  505    CONTINUE
   


C* Suppression du terme de réflexion direct RAA
C* pour la composante atmosphérique montante
C*  --> Il reste un champ diffus
C----------------------------------------------------
	 IF (IMAT_SURF.EQ.1) THEN ! Cas mer agitée
            DO K=1,NBMU
	       DO I=0,CTE_NT_ATM
	           I3(I,K) = I3(I,K) - RII(I,K)
	           Q3(I,K) = Q3(I,K) - RQQ(I,K)
                   U3(I,K) = U3(I,K) - RUU(I,K)
	       ENDDO 	       
	    ENDDO
	 ENDIF

C* Calcul des éclairements diffus, albédos sphériques 
C* et albédos plans
C----------------------------------------------------

         IF (IS.EQ.0) THEN

	    FLUX_UP_TOA_DIFF = 0.
	    	 
	    FLUX_UP_SURF_DIFF = 0.
	    FLUX_DOWN_SURF_DIFF = 0.
	    
	    FLUX_UP_SSURF = 0.
	    FLUX_DOWN_SSURF_DIFF = 0.
	    
	    FLUX_SPHE_UP_SSURF = 0.
	    FLUX_SPHE_DOWN_SSURF_DIFF = 0.	        

            CH_ATM_TOT=4.*CH_ATM(CTE_NT_ATM)   !Atténuation TOA -> surface 
	    				       !pour la direction d'incidence solaire

	       
	    !Calcul des flux et albédo diffus en surface (O+), sous la surface (0-)
	    DO J=1,NBMU

	       FLUX_UP_TOA_DIFF = FLUX_UP_TOA_DIFF 
     &	      		        + RMU(J)*GA(J)*I3(0,J)
     	    
	       FLUX_UP_SURF_DIFF = FLUX_UP_SURF_DIFF 
     &	      		         + RMU(J)*GA(J)*I3(CTE_NT_ATM,J)
     
	       FLUX_DOWN_SURF_DIFF = FLUX_DOWN_SURF_DIFF 
     &	      		           + RMU(J)*GA(J)*I3(CTE_NT_ATM,-J)

	       FLUX_UP_SSURF = FLUX_UP_SSURF 
     &	      		     + RMU(J)*GA(J)*I3(CTE_NT_ATM+1,J)
     
     
     	       !Purement diffus car la transmission du faisceau solaire
	       !direct n'est pas ajoutée au champ descendant dans la mer
	       FLUX_DOWN_SSURF_DIFF = FLUX_DOWN_SSURF_DIFF 
     &	      		              + RMU(J)*GA(J)*I3(CTE_NT_ATM+1,-J)

	       FLUX_SPHE_UP_SSURF = FLUX_SPHE_UP_SSURF
     &	      		          + GA(J)*I3(CTE_NT_ATM+1,J)

	       FLUX_SPHE_DOWN_SSURF_DIFF = FLUX_SPHE_DOWN_SSURF_DIFF
     &	      		                   + GA(J)*I3(CTE_NT_ATM+1,-J)
 
	    ENDDO
	 



	    !Au TOA
	    !---------------------------
	    !Flux descendant direct au TOA
	    !   Flux = Es*cosThetas avec Es = PI
	    FLUX_DOWN_TOA_DIR = (-TAB)*INCTE_PI	

	                   
	    !A la surface (niveau 0+)
	    !------------------------
	    !Flux descendant diffus à la surface (niveau 0+)
	    FLUX_DOWN_SURF_DIFF = 
     &         FLUX_DOWN_SURF_DIFF * 2.*INCTE_PI

	    !Flux descendant direct à la surface (niveau 0+) 
	    !   Flux = Es*cosThetas*exp(-TAU/cosThetas) avec Es = PI
	    FLUX_DOWN_SURF_DIR = CH_ATM_TOT*FLUX_DOWN_TOA_DIR	    
	    	    
	    !Flux descendant total à la surface (niveau 0+)
	    FLUX_DOWN_SURF_TOT = FLUX_DOWN_SURF_DIFF 
     &			       + FLUX_DOWN_SURF_DIR 

	    !Flux montant diffus à la surface (niveau 0+)
	    FLUX_UP_SURF_DIFF =
     &         FLUX_UP_SURF_DIFF * 2.*INCTE_PI

	    !Flux montant direct à la surface (niveau 0+) 
	    !   = Flux_direct réfléchi
	    IF (IMAT_SURF.EQ.1) THEN !Cas mer agitée
		FLUX_UP_SURF_DIR = 0.
		DO J=1,NBMU
		   FLUX_UP_SURF_DIR = FLUX_UP_SURF_DIR 
     &		   		    + RAA11(N0,J)*GA(J)
		ENDDO
		FLUX_UP_SURF_DIR = FLUX_UP_SURF_DIR*INCTE_PI*0.5
		FLUX_UP_SURF_DIR = FLUX_UP_SURF_DIR*CH_ATM_TOT
	    ELSE
		IND2 = SEA_IND*SEA_IND			
                Xsun = DSQRT(IND2-1.D+00+TAB*TAB)
                RLsun = (-IND2*TAB-Xsun)/(-IND2*TAB+Xsun)
                RRsun = (-TAB-Xsun)/(-TAB+Xsun)
	        F11sun = (RLsun*RLsun+RRsun*RRsun)/2.
      
	        FLUX_UP_SURF_DIR = 
     &		CH_ATM_TOT*(-TAB)*INCTE_PI*F11sun
	    ENDIF	    
	    
	    
	    !Flux montant total à la surface (niveau 0+)
	    FLUX_UP_SURF_TOT = FLUX_UP_SURF_DIFF 
     &			     + FLUX_UP_SURF_DIR 

	    !Flux total diffus à la surface (niveau 0+)
	    FLUX_TOT_SURF_DIFF = FLUX_UP_SURF_DIFF 
     &	                       + FLUX_DOWN_SURF_DIFF
	    


	    !Au TOA
	    !---------------------------
     	    !Flux montant diffus au TOA
	    FLUX_UP_TOA_DIFF = FLUX_UP_TOA_DIFF * 2.*INCTE_PI

	    !Flux montant direct au TOA 
	    IF (IMAT_SURF.EQ.1) THEN !Cas mer agitée
		FLUX_UP_TOA_DIR = 0.
		DO J=1,NBMU
		   FLUX_UP_TOA_DIR = FLUX_UP_TOA_DIR 
     &		   + RAA11(N0,J)*DEXP(-ATM_TAUEXT_TR_TOT/RMU(J))*GA(J)
		ENDDO
		FLUX_UP_TOA_DIR = FLUX_UP_TOA_DIR*INCTE_PI*0.5
		FLUX_UP_TOA_DIR = FLUX_UP_TOA_DIR*CH_ATM_TOT
	    ELSE      !Cas mer plate : Flux_direct réfléchi atténué par remontée sol -> TOA
	              !avec angle zénithal = celui d'incidence = angle zénithal solaire
	        FLUX_UP_TOA_DIR = 
     &		   FLUX_UP_SURF_DIR*DEXP(ATM_TAUEXT_TR_TOT/TAB)   
	    ENDIF
	     
	    
	    !Flux montant total au TOA
	    FLUX_UP_TOA_TOT = FLUX_UP_TOA_DIFF
     &			    + FLUX_UP_TOA_DIR 


     	    
	    !Sous la surface (niveau 0-)
	    !---------------------------
	    !Flux descendant diffus sous la surface (niveau 0-)
	    FLUX_DOWN_SSURF_DIFF =
     &         FLUX_DOWN_SSURF_DIFF * 2.*INCTE_PI
	    
	    !Flux descendant total sous la surface (niveau 0-)
	    ! FLUX_DOWN_SSURF_DIR a été calculé au début des traitements
	    FLUX_DOWN_SSURF_TOT = FLUX_DOWN_SSURF_DIFF 
     &			        + FLUX_DOWN_SSURF_DIR 

	    !Flux montant diffus sous la surface (niveau 0-)
	    FLUX_UP_SSURF =
     &         FLUX_UP_SSURF * 2.*INCTE_PI

	    !Flux total diffus sous la surface (niveau 0-)
	    FLUX_TOT_SSURF_DIFF = FLUX_UP_SSURF + FLUX_DOWN_SSURF_DIFF

	    !Flux sphérique descendant diffus sous la surface (niveau 0-)
	    FLUX_SPHE_DOWN_SSURF_DIFF = 
     &	       FLUX_SPHE_DOWN_SSURF_DIFF * 2.*INCTE_PI
	    	 
	    !Flux sphérique descendant direct sous la surface (niveau 0-)
	    !  ==> Intégration sur L(mu)*dmu pour le flux sphérique  
	    !      alors que pour un flux l'intégration est 
	    !      du type L(mu)*mu*dmu
	    !
	    !      Le flux sphérique sous la surface est donc :
	    !      Flux_sphérique = ECL_DIR_0- 
	    !           = ISUNDIR_DOWN_SSURF_MP * PI 
	    FLUX_SPHE_DOWN_SSURF_DIR = 
     &	       ISUNDIR_DOWN_SSURF_MP*INCTE_PI  

	    !Flux sphérique montant diffus sous la surface (niveau 0-)
	    FLUX_SPHE_UP_SSURF =
     &         FLUX_SPHE_UP_SSURF * 2.*INCTE_PI

	    !Flux sphérique total diffus sous la surface (niveau 0-)
	    FLUX_SPHE_TOT_SSURF_DIFF = FLUX_SPHE_UP_SSURF 
     &	   			     + FLUX_SPHE_DOWN_SSURF_DIFF

	     


            !Profil du flux descendant du TOA à la surface 
	    !(inclue toutes diffusions, y compris du signal réfléchi remontant)
            DO I=0,CTE_NT_ATM
	       	       
	       !Flux descendant et montant diffus, normalisé par rapport à l'éclairement TOA Es (Es=PI)
	       PROFIL_FLUX_DOWN_DIFF(I) = 0.
	       PROFIL_FLUX_UP_DIFF(I) = 0.
	       DO J=1,NBMU 
	          PROFIL_FLUX_DOWN_DIFF(I) = PROFIL_FLUX_DOWN_DIFF(I) 
     &	      	                           + RMU(J)*GA(J)*I3(I,-J) 	
	          PROFIL_FLUX_UP_DIFF(I) = PROFIL_FLUX_UP_DIFF(I) 
     &	      	                         + RMU(J)*GA(J)*I3(I,J) 	
	       ENDDO
	       !Multiplication par 2*PI pour avoir flux 
	       !et division par PI pour normalisation à Es TOA  
	       PROFIL_FLUX_DOWN_DIFF(I) = PROFIL_FLUX_DOWN_DIFF(I)* 2.
               PROFIL_FLUX_UP_DIFF(I) = PROFIL_FLUX_UP_DIFF(I)* 2.
     
	       !Flux descendant et montant directs pour ep opt TAU, normalisé par rapport 
	       !à l'éclairement TOA Es (Es=PI)  
	       !   Flux descendant = Es*cosThetas*exp(-TAU/cosThetas) avec Es = PI
	       PROFIL_FLUX_DOWN_DIR(I) = 4.*CH_ATM(I)*(-TAB)  
	       !   Flux montant = (Flux surface glitter direct)*exp[-(TAUatm-TAU)/cosThetas]

              IF (IMAT_SURF.EQ.1) THEN !Cas mer agitée		! MOD V1.3: 30/05/2018 : 
									! Distinction cas mer agitée / mer plate
		    DTAU =  ATM_TAUEXT_TR(I) - ATM_TAUEXT_TR_TOT

                  PROFIL_FLUX_UP_DIR(I) = 0.
		    DO J=1,NBMU
		       PROFIL_FLUX_UP_DIR(I) = PROFIL_FLUX_UP_DIR(I) 
     &		   	+ RAA11(N0,J)*DEXP(DTAU/RMU(J))*GA(J)		
		    ENDDO
		    PROFIL_FLUX_UP_DIR(I) = 
     &            PROFIL_FLUX_UP_DIR(I)*0.5*CH_ATM_TOT  ! Integration incluant la normalisation par Es=PI

              ELSE ! Cas mer plate					! MOD V1.3: 30/05/2018 : 
									! Correction des trajets optiques
									! et normalisation par Es=PI
		    PROFIL_FLUX_UP_DIR(I) = FLUX_UP_SURF_DIR * CH_ATM_TOT 
     &	                                 / (4.*CH_ATM(I)*INCTE_PI) 
              ENDIF	   
 	       
	       !Flux descendant et montant total : direct + diffus, , normalisé par rapport 
	       !à l'éclairement TOA Es (Es=PI)  
	       PROFIL_FLUX_DOWN_TOT(I) = PROFIL_FLUX_DOWN_DIR(I) 
     &	      	                       + PROFIL_FLUX_DOWN_DIFF(I) 
	       PROFIL_FLUX_UP_TOT(I) = PROFIL_FLUX_UP_DIR(I) 
     &	      	                     + PROFIL_FLUX_UP_DIFF(I)
	    ENDDO 


            !Profil du flux descendant et montant de sous la surface au fond de l'océan 
	    !(inclue toutes diffusions, y compris du signal réfléchi remontant)
            DO ISEA=0,CTE_NT_SEA
	    
	       I = ISEA + CTE_NT_ATM + 1
	       
	       !Flux descendant et montant diffus, normalisé par rapport à l'éclairement TOA Es (Es=PI)
	       PROFIL_FLUX_DOWN_DIFF(I) = 0.
	       PROFIL_FLUX_UP_DIFF(I) = 0.
	       DO J=1,NBMU 
	          PROFIL_FLUX_DOWN_DIFF(I) = PROFIL_FLUX_DOWN_DIFF(I) 
     &	      	                           + RMU(J)*GA(J)*I3(I,-J) 	
	          PROFIL_FLUX_UP_DIFF(I) = PROFIL_FLUX_UP_DIFF(I) 
     &	      	                         + RMU(J)*GA(J)*I3(I,J) 	
	       ENDDO	  
	       !Multiplication par 2*PI pour avoir flux 
	       !et division par PI pour normalisation à Es TOA    
	       PROFIL_FLUX_DOWN_DIFF(I) = PROFIL_FLUX_DOWN_DIFF(I)* 2
               PROFIL_FLUX_UP_DIFF(I) = PROFIL_FLUX_UP_DIFF(I)* 2.
	       
	       !Flux descendant et montant directs à la profondeur TAU_SEA, normalisé par rapport 
	       !à l'éclairement TOA Es (Es=PI)  
	       !   Flux = FluxTransmis*exp(-TAU_SEA/cos(ThetasWater)) / Es
	       PROFIL_FLUX_DOWN_DIR(I) 
     &	          = FLUX_DOWN_SSURF_DIR *4.*CH_SEA_MUS(ISEA) / 
     &            INCTE_PI
	       PROFIL_FLUX_UP_DIR(I) = 0. 	!La réflexion de fond est considérée 
						!comme un terme diffus car lambertienne
	       !Flux descendant et montant total : direct + diffus, normalisé par rapport 
	       !à l'éclairement TOA Es (Es=PI)  
	       PROFIL_FLUX_DOWN_TOT(I) = PROFIL_FLUX_DOWN_DIR(I) 
     &	      	                       + PROFIL_FLUX_DOWN_DIFF(I)
	       PROFIL_FLUX_UP_TOT(I) = PROFIL_FLUX_UP_DIFF(I)
	    ENDDO 
	     
	 ENDIF ! Fin IS=0 => Calcul des flux diffus et albédos


C* Calcul des fonctions de mu I4, Q4, U4, I5, Q5 et U5:	
C-------------------------------------------------------
         COEF=2.
         IF (IS.EQ.0) COEF=1.

         DO 67 J=-NBMU,NBMU
            IF (J.EQ.0) GOTO 67  
	    
	    I4(J)=I4(J)+COEF*I3_TOA_FOND(J)
            Q4(J)=Q4(J)+COEF*Q3_TOA_FOND(J)
            U4(J)=U4(J)+COEF*U3_TOA_FOND(J)

	    I5(J)=I5(J)+COEF*I3_TOA_FOND(J)*SIGN
            Q5(J)=Q5(J)+COEF*Q3_TOA_FOND(J)*SIGN
            U5(J)=U5(J)+COEF*U3_TOA_FOND(J)*SIGN
67       CONTINUE



C* Fichier trace 
C------------------------------------------------------  
         IF (TRACE) THEN
            WRITE(IDLOG,*,ERR=1010) '  '
	     WRITE(IDLOG,*,ERR=1010) 
     &      '    Scattering accumulation : IS = ',IS
	     WRITE(IDLOG,*,ERR=1010) '    ------------------------'
	     WRITE(IDLOG,*,ERR=1010) '  '
	     WRITE(IDLOG,*,ERR=1010)	    
     &      '  --> Upward radiance at TOA '//
     &      '& Downward radiance at sea bottom:'
            WRITE(IDLOG,*,ERR=1010) '  '	   	    
            DO K=1,NBMU
              WRITE(IDLOG,20,ERR=1010) RMU(K),
     &                             '  ID ',I3(NT_TOT,-K),
     &                             '  QD ',Q3(NT_TOT,-K),
     &                             '  UD ',U3(NT_TOT,-K),
     &                             '  IM ',I3(0,K),
     &                             '  QM ',Q3(0,K),
     &                             '  UM ',U3(0,K) 
 	     ENDDO
            WRITE(IDLOG,*,ERR=1010) ' '
	     WRITE(IDLOG,*,ERR=1010) ' '	 
	     WRITE(IDLOG,*,ERR=1010)	    
     &        '  --> Field above sea surface (level 0+) :'
            WRITE(IDLOG,*,ERR=1010) '  ' 
            DO K=1,NBMU
              WRITE(IDLOG,20,ERR=1010) RMU(K),
     &                             '  ID ',I3(CTE_NT_ATM,-K),
     &                             '  QD ',Q3(CTE_NT_ATM,-K),
     &                             '  UD ',U3(CTE_NT_ATM,-K),
     &                             '  IM ',I3(CTE_NT_ATM,K),
     &                             '  QM ',Q3(CTE_NT_ATM,K),
     &                             '  UM ',U3(CTE_NT_ATM,K)
 	     ENDDO
            WRITE(IDLOG,*,ERR=1010) ' '  	    	 
	     WRITE(IDLOG,*,ERR=1010) ' '	 
	     WRITE(IDLOG,*,ERR=1010)	    
     &        '  --> Field just below sea surface (level 0-) :'
            WRITE(IDLOG,*,ERR=1010) '  '	   	    
            DO K=1,NBMU
              WRITE(IDLOG,20,ERR=1010) RMU(K),
     &                             '  ID ',I3(CTE_NT_ATM+1,-K),
     &                             '  QD ',Q3(CTE_NT_ATM+1,-K),
     &                             '  UD ',U3(CTE_NT_ATM+1,-K),
     &                             '  IM ',I3(CTE_NT_ATM+1,K),
     &                             '  QM ',Q3(CTE_NT_ATM+1,K),
     &                             '  UM ',U3(CTE_NT_ATM+1,K)
 	     ENDDO
            WRITE(IDLOG,*,ERR=1010) ' '  	    	 
	     WRITE(IDLOG,*,ERR=1010) ' '	 
	     WRITE(IDLOG,*,ERR=1010)	    
     &        '  --> Upward radiance  '//
     &        '& Downward radiance at sea bottom:'

            WRITE(IDLOG,*,ERR=1010) '  '	   	    
            DO K=1,NBMU
              WRITE(IDLOG,20,ERR=1010) RMU(K),
     &                             '  ID ',I3(NT_TOT,-K),
     &                             '  QD ',Q3(NT_TOT,-K),
     &                             '  UD ',U3(NT_TOT,-K),
     &                             '  IM ',I3(NT_TOT,K),
     &                             '  QM ',Q3(NT_TOT,K),
     &                             '  UM ',U3(NT_TOT,K)
 	     ENDDO
            WRITE(IDLOG,*,ERR=1010) ' '
         ENDIF


C* Enregistrement des tableaux de luminances à l'ordre IS de 
C* la décomposition en séries de Fourier, sur tout le profil
C* Bornes d'enregistrement des tableaux limitées au dimensionnement 
C* utile en angles : -NBMU:NBMU
C--------------------------------------------------------------
         WRITE(IDFICOS,ERR=1061)((I3(I,K),I=0,NT_TOT),K=-NBMU,NBMU),
     & 			    ((Q3(I,K),I=0,NT_TOT),K=-NBMU,NBMU),
     &			           ((U3(I,K),I=0,NT_TOT),K=-NBMU,NBMU)
      


	
C* Test d'arrêt sur la décomposition en série fourier
C-------------------------------------------------------
         CALL OSOAA_ARRET_FOURIER(NBMU,I3_TOA_FOND,Q3_TOA_FOND,
     &	 			  U3_TOA_FOND,I4,Q4,U4,I5,Q5,U5,Z1)
 
C* Si au moins un terme est supérieur au seuil, on passe
C* à l'ordre IS suivant.
C-------------------------------------------------------
         IF (Z1.GT.CTE_PH_SEUIL_SF) GOTO 24

	 
C* Sortie de la boucle sur IS
C---------------------------
         GOTO 243

	
   
C* FIN DE BOUCLE SUR IS
C----------------------
   24 CONTINUE
	
  243 CONTINUE 



      IF (TRACE) THEN
        WRITE(IDLOG,*,ERR=1010) ' '
	 WRITE(IDLOG,*,ERR=1010) "--> End of the Fourier series"//
     & 				 " expansion for the order : ", IS
	 WRITE(IDLOG,*,ERR=1010) ' '
	 WRITE(IDLOG,*,ERR=1010) ' '
	 WRITE(IDLOG,*,ERR=1010) 
     &  'FLUX (normalized by TOA solar irradiance): '
	 WRITE(IDLOG,*,ERR=1010) 
     &  '***************************************** '
	 WRITE(IDLOG,*,ERR=1010) ' '
	 WRITE(IDLOG,*,ERR=1010) ' TOA level:'
	 WRITE(IDLOG,*,ERR=1010) ' ----------'
	 WRITE(IDLOG,*,ERR=1010) ' '
	 WRITE(IDLOG,*,ERR=1010) 
     &	    '   - Flux Up diffuse :',FLUX_UP_TOA_DIFF / INCTE_PI
	 WRITE(IDLOG,*,ERR=1010) 
     &	    '   - Flux Up direct  :',FLUX_UP_TOA_DIR / INCTE_PI
	 WRITE(IDLOG,*,ERR=1010) 
     &	    '   - Flux Up total (diffuse + direct) :',
     &     FLUX_UP_TOA_TOT / INCTE_PI
	 WRITE(IDLOG,*,ERR=1010) 
     &	    '   - Flux Down direct (incident solar flux):',
     &      FLUX_DOWN_TOA_DIR / INCTE_PI
        WRITE(IDLOG,*,ERR=1010) ' '
	 WRITE(IDLOG,*,ERR=1010) ' '
	 WRITE(IDLOG,*,ERR=1010) ' Level 0+ (surface):'
	 WRITE(IDLOG,*,ERR=1010) ' -------------------'
	 WRITE(IDLOG,*,ERR=1010) ' '
	 WRITE(IDLOG,*,ERR=1010) 
     &	    '   - Flux Up diffuse :',FLUX_UP_SURF_DIFF / INCTE_PI
	 WRITE(IDLOG,*,ERR=1010) 
     &	    '   - Flux Up direct  :',FLUX_UP_SURF_DIR / INCTE_PI
	 WRITE(IDLOG,*,ERR=1010) 
     &	    '   - Flux Up total (diffuse + direct) :',
     &     FLUX_UP_SURF_TOT / INCTE_PI
	 WRITE(IDLOG,*,ERR=1010) 
     &	    '   - Flux Down diffus :',
     &     FLUX_DOWN_SURF_DIFF / INCTE_PI
	 WRITE(IDLOG,*,ERR=1010) 
     &	    '   - Flux Down direct (incident solar flux):',
     &      FLUX_DOWN_SURF_DIR / INCTE_PI
	 WRITE(IDLOG,*,ERR=1010) 
     &	    '   - Flux Down total (diffus + direct) :',
     &      FLUX_DOWN_SURF_TOT / INCTE_PI
	 WRITE(IDLOG,*,ERR=1010) 
     &	    '   - Flux diffus total (up + down) :',
     &     FLUX_TOT_SURF_DIFF / INCTE_PI
        WRITE(IDLOG,*,ERR=1010) ' '
	 WRITE(IDLOG,*,ERR=1010) ' Level 0- (below surface):'
	 WRITE(IDLOG,*,ERR=1010) ' -------------------------'
	 WRITE(IDLOG,*,ERR=1010) ' '
	 WRITE(IDLOG,*,ERR=1010) '   - Flux Up :',
     &  FLUX_UP_SSURF / INCTE_PI
	 WRITE(IDLOG,*,ERR=1010) 
     &	    '   - Flux Down diffuse :',
     &     FLUX_DOWN_SSURF_DIFF / INCTE_PI
	 WRITE(IDLOG,*,ERR=1010) 
     &	    '   - Flux Down direct (transmitted incident solar flux):',
     &      FLUX_DOWN_SSURF_DIR / INCTE_PI
	 WRITE(IDLOG,*,ERR=1010) 
     &	    '   - Flux Down total (diffuse + direct) :',
     &      FLUX_DOWN_SSURF_TOT / INCTE_PI
	 WRITE(IDLOG,*,ERR=1010) 
     &	    '   - Flux diffus total (up + down) :', 
     &     FLUX_TOT_SSURF_DIFF / INCTE_PI
       
        WRITE(IDLOG,*,ERR=1010) ''
	 WRITE(IDLOG,*,ERR=1010) '   - Spherical Flux Up:', 
     &	    FLUX_SPHE_UP_SSURF / INCTE_PI
	 WRITE(IDLOG,*,ERR=1010) 
     &	    '   - Spherical Flux Down diffuse :',
     &      FLUX_SPHE_DOWN_SSURF_DIFF / INCTE_PI
	 WRITE(IDLOG,*,ERR=1010) 
     &	    '   - Spherical Flux Down direct :',
     &      FLUX_SPHE_DOWN_SSURF_DIR / INCTE_PI
	 WRITE(IDLOG,*,ERR=1010) 
     &	    '   - Spherical Flux total diffuse (up + down) :',
     &      FLUX_SPHE_TOT_SSURF_DIFF / INCTE_PI
	 WRITE(IDLOG,*,ERR=1010) ' ' 
	 WRITE(IDLOG,*,ERR=1010) ' '
	 WRITE(IDLOG,*,ERR=1010) ' Ratio Flux down total 0- /0+ :',
     &	        FLUX_DOWN_SSURF_TOT / FLUX_DOWN_SURF_TOT
        WRITE(IDLOG,*,ERR=1010) ' Ratio Flux down direct 0- /0+ :',
     &	        FLUX_DOWN_SSURF_DIR / FLUX_DOWN_SURF_DIR
        WRITE(IDLOG,*,ERR=1010) ' Ratio Flux down diffuse 0- /0+ :',
     &	        FLUX_DOWN_SSURF_DIFF / FLUX_DOWN_SURF_DIFF
	 WRITE(IDLOG,*,ERR=1010) ' '
	 WRITE(IDLOG,*,ERR=1010) ' '
	 
	 WRITE(IDLOG,*,ERR=1010) ' '
	 WRITE(IDLOG,*,ERR=1010) 
     &	 ' Profile of downward and upward flux'//
     &	 ' from TOA to the sea bottom '
        WRITE(IDLOG,*,ERR=1010) 
     &	 ' normalized by the solar irradiance at TOA :'
        WRITE(IDLOG,*,ERR=1010) 
     &	 ' -----------------------------------------------------------'
        WRITE(IDLOG,*,ERR=1010) 'Level    Z(m)         Direct_Down'//
     &	 '   Diffuse_Down  Total_Down '//
     &	 '   Direct_Up     Diffuse_Up    Total_Up   Total_Up/Total_Down'
	 
	 WRITE(IDLOG,*,ERR=1010) ' '
	 DO I=0,NT_TOT
	    WRITE(IDLOG,600,ERR=1010) 
     &           I,PROFIL_Z(I),
     &           PROFIL_FLUX_DOWN_DIR(I),
     &	          PROFIL_FLUX_DOWN_DIFF(I),
     &           PROFIL_FLUX_DOWN_TOT(I),
     &		   PROFIL_FLUX_UP_DIR(I),
     &	          PROFIL_FLUX_UP_DIFF(I),
     &           PROFIL_FLUX_UP_TOT(I),
     &           (PROFIL_FLUX_UP_TOT(I)/PROFIL_FLUX_DOWN_TOT(I))
	 ENDDO
	 
      ENDIF !fin Trace  

 
C* Enregistrement du profil de flux dans le fichier "Flux.txt"
C------------------------------------------------------------------
      OPEN(UNIT=11,FILE=FIC_FLUX,ERR=1300) 
      WRITE(11,*,ERR=1301) 
     &' Profile of downward and upward fluxes'//
     &' from TOA to the sea bottom '
      WRITE(11,*,ERR=1301) 
     &' normalised to the solar irradiance at TOA :'
      WRITE(11,*,ERR=1301) 
     &' ------------------------------------------------------'
      WRITE(11,*,ERR=1301) 'Level    Z(m)         Direct_Down'//
     &	 '   Diffuse_Down  Total_Down '//
     &	 '   Direct_Up     Diffuse_Up    Total_Up   Total_Up/Total_Down'
	 
      WRITE(11,*,ERR=1301) ' '
      DO I=0,NT_TOT
	 WRITE(11,600,ERR=1301) 
     &       I,PROFIL_Z(I),
     &       PROFIL_FLUX_DOWN_DIR(I),
     &	      PROFIL_FLUX_DOWN_DIFF(I),
     &       PROFIL_FLUX_DOWN_TOT(I),
     &       PROFIL_FLUX_UP_DIR(I),
     &	      PROFIL_FLUX_UP_DIFF(I),
     &       PROFIL_FLUX_UP_TOT(I),
     &       (PROFIL_FLUX_UP_TOT(I)/PROFIL_FLUX_DOWN_TOT(I))
      ENDDO
      CLOSE(11)
	     	 
	 
	 
C* Fermeture fichiers
C-------------------
      CLOSE(IDRAA)	! Fermeture du fichier SURFACE RAA
      CLOSE(IDTAW)	! Fermeture du fichier SURFACE TAW
      CLOSE(IDRWW)	! Fermeture du fichier SURFACE RWW
      CLOSE(IDTWA)	! Fermeture du fichier SURFACE TWA

      CLOSE(IDFICOS)    ! Fermeture du fichier binaire des OS
      
      
C* Fin nominale 
C-------------------
      GOTO 9999
            
C* Cas d'erreur et retour du status -1 au programme appelant 
C----------------------------------------------
 1010 WRITE(6,*) '  OSOAA_SOS_CORE ERROR_1010 on logfile writing'
      GOTO 9998

 1050 WRITE(6,*) 
     & '  OSOAA_SOS_CORE ERROR_1050 on SURFACE RAA file opening'
      GOTO 9998      

 1051 WRITE(6,*) 
     & '  OSOAA_SOS_CORE ERROR_1051 on SURFACE RAA file reading'
      GOTO 9998

 1052 WRITE(6,*) 
     & '  OSOAA_SOS_CORE ERROR_1052 on SURFACE TAW file opening'
      GOTO 9998      

 1053 WRITE(6,*) 
     & '  OSOAA_SOS_CORE ERROR_1053 on SURFACE TAW file reading'
      GOTO 9998
      
 1054 WRITE(6,*)  
     & '  OSOAA_SOS_CORE ERROR_1054 on SURFACE RWW file opening'
      GOTO 9998      

 1055 WRITE(6,*) 
     & '  OSOAA_SOS_CORE ERROR_1055 on SURFACE RWW file reading'
      GOTO 9998
      
 1056 WRITE(6,*) 
     & '  OSOAA_SOS_CORE ERROR_1056 on SURFACE TWA file opening'
      GOTO 9998      

 1057 WRITE(6,*) 
     & '  OSOAA_SOS_CORE ERROR_1057 on SURFACE TWA file reading'
      GOTO 9998  

 1060 WRITE(6,*) 
     & '  OSOAA_SOS_CORE ERROR_1060 on  binary SOS result file opening'
      GOTO 9998      

 1061 WRITE(6,*) 
     & '  OSOAA_SOS_CORE ERROR_1061 on  binary SOS result file writing'
      GOTO 9998   

 1200 WRITE(6,*) '  OSOAA_SOS_CORE ERROR_1200 '
      WRITE(6,*) '     on OSOAA_INTERF_MERPLATE subroutine'
      GOTO 9998   

 1300 WRITE(6,*) '  OSOAA_SOS_CORE ERROR_1300 '
      WRITE(6,*) '     Error while opening Flux.txt'
      GOTO 9998   

 1301 WRITE(6,*) '  OSOAA_SOS_CORE ERROR_1301 '
      WRITE(6,*) '     Error while writing Flux.txt'
      GOTO 9998   
             
9998  IER=-1             
9999  RETURN   


C* Format
C---------  

   20 FORMAT(F9.6,6(A5,E12.5))  
   30 FORMAT(A10,1X,I4,7X,F8.4,6X,F10.8)    	  
   31 FORMAT(A10,1X,I4,7X,F8.4,6X,F10.8,6X,F10.8) 
  401 FORMAT(I4,2X,F9.6,2X,F7.2,3(2X,E10.4))
  402 FORMAT(I4,2X,I4,2X,F10.6,2X,3(2X,E10.4))              
  500 FORMAT(A10,F9.3,3(A7,E9.3),1X,A12,F8.4,3(A6,E9.3),3(A11,E9.3))   
  511 FORMAT(A10,F9.3,1X,A12,F8.4,3(A6,E9.3),3(A11,E9.3))   
  600 FORMAT(2X,I4,1X,F12.5,2X,7(2X,E12.6))     
      END      !FIN DE LA PROCEDURE OSOAA_SOS_CORE
      
      
  
      
      
      
      
C*===========================================================================
C PROCEDURE: OSOAA_NOYAUX
C ==========
C      Cette procédure calcule les éléments de la matrice de phase P(mu,mup)
C      des particules (aérosols ou hydrosols) à l'ordre IS du développement 
C      en séries de Fourier, pour une matrice exprimée dans le repère lié 
C      au plan méridien.
C      Elle se base sur les coefficients alpha(k), beta(k), gamma(k) et zeta(k)
C      de la décomposition de la matrice de phase (d'intensité et de polarisation)
C      en polynômes de Legendre, pour une matrice exprimée dans le repère lié 
C      au plan de diffusion.
C      Les calculs utilisent les polynômes et fonctions de Legendre.
C
C      La matrice de phase des particules P(mu,mup) s'écrit :
C        BP(mu,mup)    GR(mu,mup)    -GT(mu,mup)
C        GR(mup,mu)    ARR(mu,mup)   -ART(mup,mu)
C       -GT(mup,mu)   -ART(mu,mup)    ATT(mu,mup)
C
C      On calcule BP(mu,mup), GR(mu,mup), GT(mu,mup), ARR(mu,mup),
C      ART(mu,mup) et ATT(mu,mup) pour -1 < mu < 1 et pour -1 < mup < 1.
C
C
C      Les fonctions complémentaires XPL, XRL et XTL, calculées pour un ordre L=2,
C      sont utiles pour calculer les éléments de la matrice de phase des molécules.
C
C
C
C Description des paramètres
C -------------------------- 
C
C     IS  (I4)  : (E) Ordre de la décomposition en séries de Fourier.
C     NBMU (I4) : (E) Nombre d'angles (positifs) effectivement utiles
C     RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  
C                 (E) : Tableau des cosinus des angles utilisés.
C     OS_NB (I4)  (E) : Ordre max des développements des fonctions de phase 
C		        en Polynômes de Legendre à utiliser, 
C                       et des développements en séries de Fourier des matrices
C                       d'interfaces et des champs de luminances.
C
C     ALPHA(0:CTE_OS_NB_MAX) (double)  (E)    Paramètres alpha(k).
C     BETA(0:CTE_OS_NB_MAX)  (double)  (E)    Paramètres beta(k).
C     GAMMA(0:CTE_OS_NB_MAX) (double)  (E)    Paramètres gamma(k).
C     ZETA(0:CTE_OS_NB_MAX)  (double)  (E)    Paramètres zeta(k).
C
C     XPL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)  (S) 
C      		        Fonction de Legendre PSL à l'ordre L=2
C   		        et S=IS en fonction de l'angle mu.
C     XRL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)  (S)
C      		        Fonction RSL à l'ordre L=2 et S=IS.
C     XTL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)  (S)
C      		        Fonction TSL à l'ordre L=2 et S=IS
C	        ==>  Fonctions de Legendre et fonctions dérivées pour le calcul
C	             de la matrice de phase moléculaire.
C
C	
C     BP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (S) 
C                                                        Elément P11.
C     GR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (S) 
C                                                        Elément P12 ou P21.
C     GT(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (S) 
C                                                        Elément -P13 ou -P31.
C     ARR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)  (S) 
C                                                        Elément P22.
C     ART(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)  (S)
C                                                        Elément -P23 ou -P32.
C     ATT(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)  (S) 
C                                                        Elément P33.
C        ==>  Eléments Pij de la matrice de phase des particules pour un ordre
C             IS de la décomposition en séries de Fourier.
C             Soit mup la direction d'incidence ( -1 < mup < 1)
C             et mu celle de diffusion ( -1 < mu < 1),
C             la diffusion est exprimée par Pij(mu,mup).
C
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     Aucun
C
C==============================================================================
      SUBROUTINE OSOAA_NOYAUX(IS,NBMU,RMU,OS_NB,ALPHA,BETA,GAMMA,ZETA,
     &                        XPL,XRL,XTL,BP,GR,GT,ARR,ART,ATT)
     
      IMPLICIT NONE


C* Définition des variables    
C*-----------------------------------------------------------------------

      DOUBLE PRECISION RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      				!  Tableau des mu de Gauss.

      DOUBLE PRECISION ALPHA(0:CTE_OS_NB_MAX)	!  Paramètres alpha(k).
      DOUBLE PRECISION BETA(0:CTE_OS_NB_MAX)	!  Paramètres beta(k).
      DOUBLE PRECISION GAMMA(0:CTE_OS_NB_MAX)	!  Paramètres gamma(k).
      DOUBLE PRECISION ZETA(0:CTE_OS_NB_MAX)	!  Paramètres zeta(k).

	! Fonctions de Legendre et fonctions dérivées pour le calcul
	! de la matrice de phase des molécules.
      DOUBLE PRECISION XPL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				!   Fonction de Legendre PSL à l'ordre L=2
      				!   et S=IS en fonction de l'angle mu.
      DOUBLE PRECISION XRL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				!   Fonction RSL à l'ordre L=2 et S=IS.
      DOUBLE PRECISION XTL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				!   Fonction TSL à l'ordre L=2 et S=IS
							
	! Eléments Pij de la matrice de phase des aérosols pour un ordre
	! IS de la décomposition en séries de Fourier.
	! Soit mup la direction d'incidence (-1 < mup < 1)
	! et mu celle de diffusion (-1 < mu < 1),
	! la diffusion est exprimée par Pij(mu,mup).	
      DOUBLE PRECISION 
     &    BP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &       -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! Elément P11.
      DOUBLE PRECISION 
     &    GR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &       -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! P12 ou P21.
      DOUBLE PRECISION 
     &    GT(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &       -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! -P13 ou -P31.
      DOUBLE PRECISION 
     &    ARR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &        -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! P22.
      DOUBLE PRECISION 
     &    ART(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &        -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! -P23 ou -P32.
      DOUBLE PRECISION 
     &    ATT(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &        -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! P33.



      DOUBLE PRECISION 
     &    PSL(-1:CTE_OS_NB_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
      				! Fonction de Legendre PSL pour l'ordre S=IS
      				! en fonction de l'ordre L et de l'angle mu.
      DOUBLE PRECISION 
     &    RSL(-1:CTE_OS_NB_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Fonction RSL.
      DOUBLE PRECISION 
     &    TSL(-1:CTE_OS_NB_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				  ! Fonction TSL.

      DOUBLE PRECISION C	! Valeur ponctuelle du mu de Gauss.

      DOUBLE PRECISION A,B,D,E,F     ! Coefficients pour la récurrence 
      				     ! sur PSL, RSL et TSL.

      DOUBLE PRECISION X	! Variable intermédiaire de calcul.
      DOUBLE PRECISION XX	! Variable intermédiaire de calcul.
      DOUBLE PRECISION YY	! Variable intermédiaire de calcul.

      DOUBLE PRECISION SBP	! Variable de sommation de L = IS à OS_NB pour BP.
      DOUBLE PRECISION SATT	! Variable de sommation de L = IS à OS_NB pour SATT.
      DOUBLE PRECISION SARR	! Variable de sommation de L = IS à OS_NB pour SARR.
      DOUBLE PRECISION SGR	! Variable de sommation de L = IS à OS_NB pour SGR.
      DOUBLE PRECISION SGT	! Variable de sommation de L = IS à OS_NB pour SGT.
      DOUBLE PRECISION SART	! Variable de sommation de L = IS à OS_NB pour SART.
      DOUBLE PRECISION R1		
      DOUBLE PRECISION R2	

      DOUBLE PRECISION RAC3	! Racine de 3.
      DOUBLE PRECISION X26	! 2*racine(6)

      INTEGER*4 OS_NB 	! Ordre max des développements des fonctions de phase 
			! en Polynômes de Legendre, des matrices d'interface et
			! des champs de luminance.

      INTEGER IS	! Ordre de la décomposition en séries de Fourier.

      INTEGER*4 NBMU    ! Nombre d'angles effectifs des simulations
      INTEGER*4 I
      INTEGER*4 K
      INTEGER*4 IG	! Variable à 1 ou -1.
      INTEGER*4 L	! Ordre L des développements
      INTEGER*4 LP	! Valeur L+1
      INTEGER*4 LM	! Valeur L-1
      INTEGER*4 J	! Indice du tableau de RMU.


C* Initialisation 
C----------------
      RAC3=DSQRT(3.D+00)
      X26=2.*DSQRT(6.D+00)

C-----------------------------------------------------
C* Initialisation des fonctions d'ordre IS pour les 
C* ordres L = 0, 1 et 2.
C-----------------------------------------------------

C* Initialisation pour l'ordre IS = 0
C-------------------------------------
      IF (IS.NE.0) GO TO 700
      
      DO 25 J=0,NBMU
         C=RMU(J)
         PSL(0,-J)=1.
         PSL(0,J)=1.
         PSL(1,J)=C
         PSL(1,-J)=-C
         X=(3.*C*C-1.)*0.5
         PSL(2,-J)=X
         PSL(2,J)=X
         RSL(1,J)=0.
         RSL(1,-J)=0.
         X=3.*(1.-C*C)/X26
         RSL(2,-J)=X
         RSL(2,J)=X
         TSL(1,J)=0.
         TSL(1,-J)=0.
         TSL(2,J)=0.
         TSL(2,-J)=0.
   25 CONTINUE
   
      PSL(1,0)=RMU(0)
      RSL(1,0)=0.
      
      GOTO 501

C* Initialisation pour l'ordre IS = 1
C-------------------------------------
  700 IF (IS.NE.1) GO TO 701
  
      DO 26 J=0,NBMU
         C=RMU(J)
         X=1.-C*C
         PSL(0,J)=0.
         PSL(0,-J)=0.
         PSL(1,-J)=DSQRT(X*0.5)
         PSL(1,J)=DSQRT(X*0.5)
         PSL(2,J)=C*PSL(1,J)*RAC3
         PSL(2,-J)=-PSL(2,J)
         RSL(1,-J)=0.
         RSL(1,J)=0.
         RSL(2,J)=-C*DSQRT(X)*0.5
         RSL(2,-J)=-RSL(2,J)
         TSL(1,-J)=0.
         TSL(1,J)=0.
         TSL(2,J)=-DSQRT(X)*0.5
         TSL(2,-J)=-DSQRT(X)*0.5
   26 CONTINUE
   
      PSL(2,0)=-PSL(2,0)
      RSL(2,0)=-RSL(2,0)
      RSL(1,0)=0.
      TSL(1,0)=0.
      
      GOTO 501

C* Initialisation pour l'ordre IS >= 2
C----------------------------------------
  701 A=1.
  
      DO 27 I=1,IS
        X=I
        A=A*DSQRT((I+IS)/X)*0.5
 27   CONTINUE
 
      B=A*DSQRT(IS/(IS+1.D+00))*DSQRT((IS-1.D+00)/(IS+2.)) 
      
      DO 28 J=0,NBMU
         C=RMU(J)
         XX=1.-C*C
         YY=IS*0.5-1.
         PSL(IS-1,J)=0.
         RSL(IS-1,J)=0.
         TSL(IS-1,J)=0.
         X=A*XX**(IS*0.5)
         PSL(IS,-J)=X
         PSL(IS,J)=X
         X=B*(1.+C*C)*XX**YY
         RSL(IS,-J)=X
         RSL(IS,J)=X
         X=2.*B*C*XX**YY
         TSL(IS,-J)=-X
         TSL(IS,J)=X
   28 CONTINUE


C* Calcul des fonctions PSL(L,mu), RSL(L,mu) et TSL(L,mu) 
C* par récurrence sur l'ordre L pour IS fixé.
C------------------------------------------------------
  501 K=2

      IF (IS.GT.2) K=IS
      
      IF (K.EQ.OS_NB) GOTO 502
      
      IG=-1
      IF (IS.EQ.1) IG=1

      DO 30 L=K,(OS_NB-1)
         LP=L+1
         LM=L-1

C* Coefficients pour la récurrence sur PSL
         A=(2*L+1.)/DSQRT((L+IS+1.D+00)*(L-IS+1.))
         B=DSQRT(DFLOAT((L+IS)*(L-IS)))/(2.*L+1.)

C* Coefficients pour la récurrence sur RSL et TSL
         D=(L+1.)*(2*L+1.)/DSQRT((L+3.D+00)*(L-1.)*(L+IS+1.)  
     &     *(L-IS+1.))
         E=DSQRT((L+2.D+00)*(L-2.)*(L+IS)*(L-IS))/(L*(2.*L+1.))
         F=2.*IS/(L*(L+1.))

C* Application de la récurrence pour chaque mu de Gauss
C------------------------------------------------------
         DO 31 J=0,NBMU
            C=RMU(J)
            X=A*(C*PSL(L,J)-B*PSL(LM,J))
            PSL(LP,J)=X
            X=D*(C*RSL(L,J)-F*TSL(L,J)-E*RSL(LM,J))
            RSL(LP,J)=X
            X=D*(C*TSL(L,J)-F*RSL(L,J)-E*TSL(LM,J))
            TSL(LP,J)=X
	    
            IF(J.EQ.0) GO TO 31
	    
            PSL(LP,-J)=IG*PSL(LP,J)
            RSL(LP,-J)=IG*RSL(LP,J)
            TSL(LP,-J)=-IG*TSL(LP,J)
   31    CONTINUE

         IG=-IG
   30 CONTINUE

  502 CONTINUE


C* Stockage de PSL(L=2,mu), RSL(L=2,mu) et TSL(L=2,mu)
C------------------------------------------------------
      DO 1005 J=-NBMU,NBMU
         XPL(J)=PSL(2,J)
         XRL(J)=RSL(2,J)
         XTL(J)=TSL(2,J)
 1005 CONTINUE
      
C--------------------------------------------------------
C* Calcul des éléments de matrice de phase des particules 
C* (aérosols ou hydrosols)
C--------------------------------------------------------

C* Boucles sur les directions d'incidence (K = mup, 
C* avec -1 < mup < 1) et d'émergence (J = mu > 0)
C------------------------------------------------------
      DO 32 J=-NBMU,NBMU
         DO 32 K=-NBMU,NBMU
            SBP=0.
            SATT=0.
            SARR=0.
            SGR=0.
            SGT=0.
            SART=0.
	    
            IF(IS.GT.OS_NB) GOTO 1

C* Boucle de sommation sur L = IS à OS_NB.
C-----------------------------------------------
           DO 33 L=IS,OS_NB
              R1=TSL(L,J)*TSL(L,K)
              R2=RSL(L,J)*RSL(L,K)
              SBP=SBP+BETA(L)*PSL(L,J)*PSL(L,K)
              SATT=SATT+ALPHA(L)*R1+ZETA(L)*R2
              SARR=SARR+ZETA(L)*R1+ALPHA(L)*R2
              SGR=SGR+GAMMA(L)*PSL(L,J)*RSL(L,K)
              SGT=SGT+GAMMA(L)*PSL(L,J)*TSL(L,K)
              SART=SART+ALPHA(L)*RSL(L,K)*TSL(L,J)+
     &             ZETA(L)*RSL(L,J)*TSL(L,K)
  33       CONTINUE
  
 1         CONTINUE

            BP(J,K)=SBP
            ATT(J,K)=SATT
            ARR(J,K)=SARR
            GR(J,K)=SGR
            GT(J,K)=SGT
            ART(J,K)=SART
   32 CONTINUE
   
      RETURN
      END  	!FIN DE LA PROCEDURE OSOAA_NOYAUX      
      
      
      
      
      

C*============================================================================
C PROCEDURE: OSOAA_FSOURCE_ATM_ORDRE1
C ==========
C      Cette procédure calcule le vecteur fonction source de la diffusion 
C      primaire atmosphérique, à partir du champ de rayonnement solaire incident 
C      et des propriétés de diffusion de l'atmosphère (fonction de phase).
C      
C      Calcul pour chaque niveau du profil et pour chaque direction de propagation.
C
C
C      NB: Si EXPERT_MODE_FORCED_FATM_NULL = .TRUE. la fonction source est posée nulle.
C          (constante introduite par #define)
C
C
C Description des paramètres
C -------------------------- 
C
C       IS   (I4)    (E)  Ordre en cours du développement en séries de Fourier.
C       NBMU (I4)    (E)  Nombre d'angles (positifs) effectivement utiles
C       JK   (I4)    (E)  Numéro de stockage de mus dans le tableau RMU
C                        (0 par défaut).
C       XDEL(0:CTE_NT_ATM)  (double)  (E)  Pourcentage d'aérosols par niveau.
C       YDEL(0:CTE_NT_ATM)  (double)  (E)  Pourcentage de molécules par niveau.
C
C       BETA0	  (double)  (E) 
C       BETA2     (double)  (E) 	.
C       GAMMA2	  (double)  (E) 	
C                   Coefficients du développement de la matrice de phase
C                   moléculaire en fonctions de Legendre.
C
C       XPL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) 	
C      		     Fonction de Legendre PSL à l'ordre L=2 et S=IS
C      		     en fonction de l'angle mu.     		
C       XRL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) 	
C     		     Fonction RSL à l'ordre L=2 et S=IS en fonction de mu.
C       XTL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) 	
C     		     Fonction TSL à l'ordre L=2 et S=IS en fonction de mu.
C	
C       BP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)
C                    Elément P11.
C       GR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)
C                    Elément P12 ou P21.
C       GT(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)
C                    Elément -P13 ou -P31.
C               Eléments Pij de la matrice de phase des aérosols pour un ordre
C               IS de la décomposition en séries de Fourier.
C               Soit mup la direction d'incidence (-1 < mup < 1)
C               et mu celle de diffusion (-1 < mu < 1),
C               la diffusion est exprimée par Pij(mu,mup).
C
C       CH(0:CTE_NT_ATM)  (double)  (E)  Atténuation du faisceau solaire direct/4.
C
C       I2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (S)	
C       Q2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (S)	
C       U2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (S)	
C	         Vecteur fonction source I,Q,U en fonction du niveau du profil
C	         et de la direction.
C 
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     Aucun
C
C==============================================================================
      SUBROUTINE OSOAA_FSOURCE_ATM_ORDRE1(IS,NBMU,JK,XDEL,YDEL,
     &                                    BETA0,BETA2,GAMMA2,
     &                                    XPL,XRL,XTL,
     &                                    BP,GR,GT,CH,I2,Q2,U2)
      
      IMPLICIT NONE
      

C* Définition des variables     
C*------------------------------------------------------------------

      DOUBLE PRECISION XDEL(0:CTE_NT_ATM)   !Pourcentage d'aérosols par niveau.
      DOUBLE PRECISION YDEL(0:CTE_NT_ATM)   !Pourcentage de molécules par niveau.

      DOUBLE PRECISION BETA0	! Coefficients du développement de la matrice 
      DOUBLE PRECISION BETA2	! de phase moléculaire en fonctions de Legendre.
      DOUBLE PRECISION GAMMA2

      DOUBLE PRECISION XPL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      			      ! Fonction de Legendre PSL à l'ordre L=2 et S=IS
      			      ! en fonction de l'angle mu.
      		
      DOUBLE PRECISION XRL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      			! Fonction RSL à l'ordre L=2 et S=IS en fonction de mu
      DOUBLE PRECISION XTL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      			! Fonction TSL à l'ordre L=2 et S=IS en fonction de mu

	! Eléments Pij de la matrice de phase des aérosols pour un ordre
	! IS de la décomposition en séries de Fourier.
	! Soit mup la direction d'incidence (-1 < mup < 1)
	! et mu celle de diffusion (-1 < mu < 1),
	! la diffusion est exprimée par Pij(mu,mup).	
      DOUBLE PRECISION 
     &    BP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &       -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! Elément P11.
      DOUBLE PRECISION 
     &    GR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &       -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! P12 ou P21.
      DOUBLE PRECISION 
     &    GT(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &       -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! -P13 ou -P31.
    

      DOUBLE PRECISION CH(0:CTE_NT_ATM)	! Atténuation du faisceau solaire 
                                        ! direct / 4.

	! Vecteur fonction source en fonction du niveau du profil
	! et de la direction.
      DOUBLE PRECISION 
     &       I2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour I.
      DOUBLE PRECISION 
     &       Q2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour Q.
      DOUBLE PRECISION
     &       U2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour U.

      DOUBLE PRECISION SPL   ! Valeur de la fonction de XPL pour mu = mus.
      DOUBLE PRECISION SA1   ! Elément P11 de la matrice de phase moléculaire.
      DOUBLE PRECISION SB1   ! Elément P21 de la matrice de phase moléculaire.
      DOUBLE PRECISION SC1   ! Elément P31 de la matrice de phase moléculaire.
      DOUBLE PRECISION SA2   ! Elément P11 de la matrice de phase des aérosols.
      DOUBLE PRECISION SB2   ! Elément P21 de la matrice de phase des aérosols.
      DOUBLE PRECISION SC2   ! Elément P31 de la matrice de phase des aérosols.
 
      DOUBLE PRECISION PCRAY	! Valeur du pourcentage de molécules.
      DOUBLE PRECISION PCAER	! Valeur du pourcentage d'aérosols.

      DOUBLE PRECISION ATTDIR	! Valeur ponctuelle du tableau CH.

          
      INTEGER*4 IS	! Ordre en cours du développement en séries de Fourier.
      INTEGER*4 NBMU    ! Nombre d'angles effectifs des simulations
      
      INTEGER JK	! Numéro de stockage de mus dans le tableau RMU
      			! (0 par défaut).

      INTEGER*4 J,K

      
C-------------------------------------------------------------      
C* Calcul des éléments P11 = SAi , P21 = SBi  et P31 = SCi
C* de la matrice de phase, selon l'ordre IS du développement 
C* en séries de Fourier (i = 1 : molécules, i = 2 : aérosols).
C* La direction d'incidence est la direction d'éclairement 
C* solaire (mus <=> JK = 0).
C-------------------------------------------------------------      


C* Boucle sur les directions de diffusion.
C------------------------------------------
      DO 100 J=-NBMU,NBMU

        IF (IS-2) 200,200,201
      
C* Calculs pour IS <= 2.
C-----------------------
 200    SPL=XPL(JK)
        SA1=BETA0+BETA2*XPL(J)*SPL
        SA2=BP(JK,J)
        SB1=GAMMA2*XRL(J)*SPL
        SB2=GR(JK,J)
        SC1=GAMMA2*XTL(J)*SPL
        SC2=GT(JK,J)

        GOTO 202

C* Calculs pour IS > 2. La matrice de phase des 
C* molécules est nulle pour IS > 2
C------------------------------------------------
 201    SA2=BP(JK,J)
        SA1=0.
        SB2=GR(JK,J)
        SB1=0.
        SC2=GT(JK,J)
        SC1=0.

202     CONTINUE

C------------------------------------------------------------------------
C* Calculs de la fonction source de diffusion primaire pour chaque niveau
C* du profil et chaque direction.
C------------------------------------------------------------------------
        DO 101 K=0,CTE_NT_ATM
           ATTDIR=CH(K)
           PCRAY=YDEL(K)
           PCAER=XDEL(K)
           I2(K,J)=ATTDIR*(SA2*PCAER+SA1*PCRAY)
           Q2(K,J)=ATTDIR*(SB2*PCAER+SB1*PCRAY)
           U2(K,J)=-ATTDIR*(SC2*PCAER+SC1*PCRAY)
  101    CONTINUE
  
  100 CONTINUE
  

      !Mode expert spécifique : annulation de la diffusion atmosphérique
      IF (EXPERT_MODE_FORCED_FATM_NULL) THEN 
         WRITE(6,*) 
     &	 " \n WARNING : ATMOS SOURCE FUNCTION FORCED TO NULL \n"
         DO J=1,NBMU
	    DO K=0,CTE_NT_ATM
	       I2(K,J)=0.
	       I2(K,-J)=0.
	       Q2(K,J)=0.
	       Q2(K,-J)=0.
	       U2(K,J)=0.
	       U2(K,-J)=0.
	    ENDDO
	 ENDDO
      ENDIF
      
      END  	!FIN DE LA PROCEDURE OSOAA_FSOURCE_ATM_ORDRE1




C*============================================================================
C PROCEDURE: OSOAA_FSOURCE_ATM_ORDREIG
C ==========
C      Cette procédure calcule le vecteur fonction source de la diffusion
C      atmosphérique d'ordre IG>1, à partir du champ de rayonnement d'ordre IG-1 
C      et des propriétés de diffusion de l'atmosphère (fonction de phase).
C
C      Elle réalise le calcul des éléments de la matrice de phase, 
C      selon l'ordre IS du développement en séries de Fourier.
C      On construit indirectement la matrice suivante P(mu,mup) :
C         BP(mu,mup)    GR(mu,mup)    -GT(mu,mup)
C         GR(mup,mu)    ARR(mu,mup)   -ART(mup,mu)
C        -GT(mup,mu)   -ART(mu,mup)    ATT(mu,mup)
C 
C      Les termes réellement calculés sont équivalents en suivant les règles
C      de permutation des indices. 
C	
C      Une intégration spatiale est appliquée sur les directions d'incidence.
C
C      Calcul pour chaque niveau du profil et pour chaque direction de propagation.
C
C
C      NB: Si EXPERT_MODE_FORCED_FATM_NULL = .TRUE. la fonction source est posée nulle.
C          (constante introduite par #define)
C
C
C Description des paramètres
C -------------------------- 
C   
C       IS   (I4)    (E)  Ordre en cours du développement en séries de Fourier.
C       NBMU (I4)    (E)  Nombre d'angles (positifs) effectivement utiles
C       XDEL(0:CTE_NT_ATM)  (double)  (E)  Pourcentage d'aérosols par niveau.
C       YDEL(0:CTE_NT_ATM)  (double)  (E)  Pourcentage de molécules par niveau.
C
C       BETA0	  (double)  (E) 
C       BETA2     (double)  (E) 	.
C       GAMMA2	  (double)  (E) 
C       ALPHA2    (double)  (E) 	
C                   Coefficients du développement de la matrice de phase
C                   moléculaire en fonctions de Legendre
C
C       XPL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) 	
C      		     Fonction de Legendre PSL à l'ordre L=2 et S=IS
C      		     en fonction de l'angle mu.     		
C       XRL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) 	
C     		     Fonction RSL à l'ordre L=2 et S=IS en fonction de mu.
C       XTL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) 	
C     		     Fonction TSL à l'ordre L=2 et S=IS en fonction de mu.
C
C      	
C       I1(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)    Champ I.
C       Q1(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)    Champ Q.
C       U1(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)    Champ U.
C         	     Champ de luminance d'ordre  IG en fonction du 
C                    niveau du profil et de la direction. 
C	
C       BP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) 
C                    Elément P11.
C       GR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) 
C                    Elément P12 ou P21.
C       GT(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)
C                    Elément -P13 ou -P31.
C       ARR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)  (E) 
C                    Elément P22.
C       ART(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)  (E) 
C                    Elément -P23 ou -P32.
C       ATT(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)  (E)
C                    Elément P33.
C               Eléments Pij de la matrice de phase des aérosols pour un ordre
C               IS de la décomposition en séries de Fourier.
C               Soit mup la direction d'incidence ( -1 < mup < 1)
C               et mu celle de diffusion ( -1 < mu < 1),
C               la diffusion est exprimée par Pij(mu,mup).
C
C       GA(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) Tableau des poids de Gauss.
C
C       I2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (S)	
C       Q2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (S)	
C       U2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (S)	
C	         Vecteur fonction source I,Q,U en fonction du niveau du profil
C	         et de la direction.
C
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     Aucun
C
C==============================================================================
      SUBROUTINE OSOAA_FSOURCE_ATM_ORDREIG(IS,NBMU,XDEL,YDEL,
     &                                      BETA0,BETA2,GAMMA2,ALPHA2,
     &                                      XPL,XRL,XTL,I1,Q1,U1,
     &                                      BP,GR,GT,ARR,ART,ATT,GA,
     &                                      I2,Q2,U2)
      
      IMPLICIT NONE
      

C* Définition des variables     
C*------------------------------------------------------------------

      DOUBLE PRECISION XDEL(0:CTE_NT_ATM)  ! Pourcentage d'aérosols par niveau.
      DOUBLE PRECISION YDEL(0:CTE_NT_ATM)  ! Pourcentage de molécules par niveau.

      DOUBLE PRECISION BETA0	! Coefficients du développement de la matrice 
      DOUBLE PRECISION BETA2	! de phase moléculaire en fonctions de Legendre.
      DOUBLE PRECISION GAMMA2	
      DOUBLE PRECISION ALPHA2	

      DOUBLE PRECISION XPL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      			! Fonction de Legendre PSL à l'ordre L=2 et S=IS
      			! en fonction de l'angle mu.     		
      DOUBLE PRECISION XRL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      			! Fonction RSL à l'ordre L=2 et S=IS en fonction de mu.
      DOUBLE PRECISION XTL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      			! Fonction TSL à l'ordre L=2 et S=IS en fonction de mu.


      	! Champ de luminance d'ordre IG-1 en fonction du niveau du profil
	! et de la direction.       	
      DOUBLE PRECISION 
     &       I1(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ I.
      DOUBLE PRECISION 
     &       Q1(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ Q.
      DOUBLE PRECISION 
     &       U1(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ U.

	! Vecteur fonction source en fonction du niveau du profil
	! et de la direction.
      DOUBLE PRECISION 
     &       I2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour I.
      DOUBLE PRECISION 
     &       Q2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour Q.
      DOUBLE PRECISION 
     &       U2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour U.


	! Eléments Pij de la matrice de phase des aérosols pour un ordre
	! IS de la décomposition en séries de Fourier.
	! Soit mup la direction d'incidence (-1 < mup < 1)
	! et mu celle de diffusion (-1 < mu < 1),
	! la diffusion est exprimée par Pij(mu,mup).	
      DOUBLE PRECISION 
     &    BP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &       -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! Elément P11.
      DOUBLE PRECISION 
     &    GR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &       -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! P12 ou P21.
      DOUBLE PRECISION 
     &    GT(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &       -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! -P13 ou -P31.
      DOUBLE PRECISION 
     &    ARR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &        -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! P22.
      DOUBLE PRECISION 
     &    ART(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &        -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! -P23 ou -P32.
      DOUBLE PRECISION 
     &    ATT(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &        -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! P33.


      DOUBLE PRECISION GA(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! Tableau des poids de Gauss

      DOUBLE PRECISION XPK	! Valeur de XPL pour les directions de 
      				! diffusion mu > 0 (indice K).	
      DOUBLE PRECISION XRK	! Valeur de XRL pour mu > 0.
      DOUBLE PRECISION XTK	! Valeur de XTL pour mu > 0.
      DOUBLE PRECISION YPK	! Valeur de XPL pour -mu.	
      DOUBLE PRECISION YRK	! Valeur de XRL pour -mu.
      DOUBLE PRECISION YTK	! Valeur de XTL pour -mu.	
      	
      DOUBLE PRECISION XPJ	! Valeur de XPL pour les directions incidentes
      				! mu = mup > 0 (indice J).
      DOUBLE PRECISION XRJ	! Valeur de XRL pour mu = mup > 0.
      DOUBLE PRECISION XTJ	! Valeur de XTL pour mu = mup > 0.
      DOUBLE PRECISION YPJ	! Valeur de XPL pour mu = -mup.	
      DOUBLE PRECISION YRJ	! Valeur de XRL pour mu = -mup.
      DOUBLE PRECISION YTJ	! Valeur de XTL pour mu = -mup.		

      DOUBLE PRECISION XI1	! Valeur de I du champ montant d'ordre IG-1.
      DOUBLE PRECISION XQ1	! Valeur de Q du champ montant d'ordre IG-1.
      DOUBLE PRECISION XU1	! Valeur de U du champ montant d'ordre IG-1.
      DOUBLE PRECISION XI2	! Valeur de I du champ descendant d'ordre IG-1.
      DOUBLE PRECISION XQ2	! Valeur de Q du champ descendant d'ordre IG-1.
      DOUBLE PRECISION XU2	! Valeur de U du champ descendant d'ordre IG-1.

      DOUBLE PRECISION II1	! Valeur intermédiaire pour le calcul de I2 (mu < 0).
      DOUBLE PRECISION QQ1	! Valeur intermédiaire pour le calcul de Q2 (mu < 0).
      DOUBLE PRECISION UU1	! Valeur intermédiaire pour le calcul de U2 (mu < 0).
      DOUBLE PRECISION II2 	! Valeur intermédiaire pour le calcul de I2 (mu > 0).
      DOUBLE PRECISION QQ2 	! Valeur intermédiaire pour le calcul de Q2 (mu > 0).
      DOUBLE PRECISION UU2 	! Valeur intermédiaire pour le calcul de U2 (mu > 0).

      DOUBLE PRECISION Z   	! Valeur ponctuelle du tableau des poids de Gauss.

      DOUBLE PRECISION PCRAY	! Valeur du pourcentage de molécules.
      DOUBLE PRECISION PCAER	! Valeur du pourcentage d'aérosols.
 
      DOUBLE PRECISION BPJK	! Valeur des éléments de la matrice de 
      DOUBLE PRECISION BPJMK    ! phase du mélange molécules + aérosols.
      DOUBLE PRECISION GTJMK
      DOUBLE PRECISION GTJK
      DOUBLE PRECISION GTKJ
      DOUBLE PRECISION GTKMJ
      DOUBLE PRECISION GRJK
      DOUBLE PRECISION GRKJ
      DOUBLE PRECISION GRKMJ
      DOUBLE PRECISION GRJMK
      DOUBLE PRECISION ARRJK
      DOUBLE PRECISION ARRJMK
      DOUBLE PRECISION ARTJK
      DOUBLE PRECISION ARTJMK
      DOUBLE PRECISION ARTKJ
      DOUBLE PRECISION ARTKMJ
      DOUBLE PRECISION ATTJK
      DOUBLE PRECISION ATTJMK

      INTEGER*4 IS	! Ordre en cours du développement en séries de Fourier.
      INTEGER*4 NBMU    ! Nombre d'angles effectifs des simulations

      INTEGER*4 I
      INTEGER*4 K
      INTEGER*4 J
      

C------------------------------------------------------
C* Calcul des éléments de la matrice de phase, selon 
C* l'ordre IS du développement en séries de Fourier.
C------------------------------------------------------

      IF (IS-2) 210,210,211
      
C--------------------------------
C* Calculs pour IS <= 2.
C-------------------------------

C* Boucle sur les directions de diffusion mu (mu > 0).
C------------------------------------------------------
 210  DO 455 K=1,NBMU

C* Valeur des fonctions PSL, RSL et TSL à l'ordre L=2 et S=IS
C* en fonction de l'angle mup et -mup
C* (pour le calcul de la matrice de phase moléculaire 
C*  par le programme appelant).
C------------------------------------------------------
         XPK=XPL(K)
         XRK=XRL(K)
         XTK=XTL(K)
         YPK=XPL(-K)
         YRK=XRL(-K)
         YTK=XTL(-K)

C* Boucle sur le profil.
C-----------------------
         DO 455 I=0,CTE_NT_ATM
            II1=0.
            II2=0.
            QQ1=0.
            QQ2=0.
            UU1=0.
            UU2=0.
            PCAER=XDEL(I)
            PCRAY=YDEL(I)

C* Boucle sur les directions d'incidence mup.
C------------------------------------------------------
            DO 477 J=1,NBMU

C* Calcul des éléments de la matrice de phase P(mu,mup) 
C* à l'ordre IS pour le mélange molécules + aérosols.
C------------------------------------------------------
               XPJ=XPL(J)
               XRJ=XRL(J)
               XTJ=XTL(J)
               YPJ=XPL(-J)
               YRJ=XRL(-J)
               YTJ=XTL(-J)

               BPJK=BP(J,K)*PCAER+PCRAY*(BETA0+BETA2*XPJ*XPK)
               BPJMK=BP(J,-K)*PCAER+PCRAY*(BETA0+BETA2*XPJ*YPK)
               GTJMK=GT(J,-K)*PCAER+PCRAY*(GAMMA2*XPJ*YTK)
               GTJK=GT(J,K)*PCAER+PCRAY*(GAMMA2*XPJ*XTK)
               GTKMJ=GT(K,-J)*PCAER+PCRAY*(GAMMA2*XPK*YTJ)
               GTKJ=GT(K,J)*PCAER+PCRAY*(GAMMA2*XPK*XTJ)
               GRJK=GR(J,K)*PCAER+PCRAY*(GAMMA2*XPJ*XRK)
               GRJMK=GR(J,-K)*PCAER+PCRAY*(GAMMA2*XPJ*YRK)
               GRKJ=GR(K,J)*PCAER+PCRAY*(GAMMA2*XPK*XRJ)
               GRKMJ=GR(K,-J)*PCAER+PCRAY*(GAMMA2*XPK*YRJ)
               ARRJK=ARR(J,K)*PCAER+PCRAY*(ALPHA2*XRJ*XRK)
               ARRJMK=ARR(J,-K)*PCAER+PCRAY*(ALPHA2*XRJ*YRK)
               ARTJK=ART(J,K)*PCAER+PCRAY*(ALPHA2*XTJ*XRK)
               ARTJMK=ART(J,-K)*PCAER+PCRAY*(ALPHA2*XTJ*YRK)
               ARTKJ=ART(K,J)*PCAER+PCRAY*(ALPHA2*XTK*XRJ)
               ARTKMJ=ART(K,-J)*PCAER+PCRAY*(ALPHA2*XTK*YRJ)
               ATTJMK=ATT(J,-K)*PCAER+PCRAY*(ALPHA2*XTJ*YTK)
               ATTJK=ATT(J,K)*PCAER+PCRAY*(ALPHA2*XTJ*XTK)

C* Calculs de la fonction source d'ordre IG, pour chaque 
C* niveau du profil et chaque direction.
C------------------------------------------------------
               Z=GA(J)

C* Récupération des valeurs de luminance d'ordre IG-1.
C------------------------------------------------------
               XI1=I1(I,J)
               XI2=I1(I,-J)
               XQ1=Q1(I,J)
               XQ2=Q1(I,-J)
               XU1=U1(I,J)
               XU2=U1(I,-J)

C* Intégration spatiale sur mup.
C---------------------------------
               II2=II2+Z*(XI1*BPJK+XI2*BPJMK+XQ1*GRKJ
     &             +XQ2*GRKMJ-XU1*GTKJ-XU2*GTKMJ)
               II1=II1+Z*(XI1*BPJMK+XI2*BPJK+XQ1*GRKMJ
     &             +XQ2*GRKJ+XU1*GTKMJ+XU2*GTKJ)
               QQ2=QQ2+Z*(XI1*GRJK+XI2*GRJMK+XQ1*ARRJK
     &             +XQ2*ARRJMK+XU2*ARTJMK-XU1*ARTJK)
               QQ1=QQ1+Z*(XI1*GRJMK+XI2*GRJK+XQ1*ARRJMK
     &             +XQ2*ARRJK-XU1*ARTJMK+XU2*ARTJK)
               UU2=UU2-Z*(XI1*GTJK-XI2*GTJMK+XQ1*ARTKJ
     &             +XQ2*ARTKMJ-XU1*ATTJK-XU2*ATTJMK)
               UU1=UU1-Z*(XI1*GTJMK-XI2*GTJK-XQ1*ARTKMJ
     &             -XQ2*ARTKJ-XU1*ATTJMK-XU2*ATTJK)
 477        CONTINUE

C* Stockage des résultats
C--------------------------
            I2(I,K)=II2 * 0.5
            I2(I,-K)=II1 * 0.5
            Q2(I,K)=QQ2 * 0.5
            Q2(I,-K)=QQ1 * 0.5
            U2(I,K)=UU2 * 0.5
            U2(I,-K)=UU1 * 0.5
 455  CONTINUE   ! Boucle sur I et K
 
      GOTO 212
      
      
C-----------------------------
C* Calculs pour IS > 2.
C-----------------------------

C* Boucle sur les directions de diffusion mu (mu > 0).
C------------------------------------------------------
 211  DO 45 K=1,NBMU

C* Boucle sur le profil.
C------------------------
         DO 45 I=0,CTE_NT_ATM
            II1=0.
            II2=0.
            QQ1=0.
            QQ2=0.
            UU1=0.
            UU2=0.
            PCAER=XDEL(I)

C* Boucle sur les directions d'incidence mup
C-------------------------------------------
            DO 47 J=1,NBMU

C* Calcul des éléments de la matrice de phase aérosols P(mu,mup)
C* à l'ordre IS, pondérée par la proportion en aérosols.
C------------------------------------------------------
               BPJK=BP(J,K)*PCAER
               BPJMK=BP(J,-K)*PCAER
               GTJMK=GT(J,-K)*PCAER
               GTJK=GT(J,K)*PCAER
               GTKMJ=GT(K,-J)*PCAER
               GTKJ=GT(K,J)*PCAER
               GRJK=GR(J,K)*PCAER
               GRJMK=GR(J,-K)*PCAER
               GRKJ=GR(K,J)*PCAER
               GRKMJ=GR(K,-J)*PCAER
               ARRJK=ARR(J,K)*PCAER
               ARRJMK=ARR(J,-K)*PCAER
               ARTJK=ART(J,K)*PCAER
               ARTJMK=ART(J,-K)*PCAER
               ARTKJ=ART(K,J)*PCAER
               ARTKMJ=ART(K,-J)*PCAER
               ATTJMK=ATT(J,-K)*PCAER
               ATTJK=ATT(J,K)*PCAER
	       
C* Calculs de la fonction source d'ordre IG, pour chaque 
C* niveau du profil et chaque direction.
C------------------------------------------------------
               Z=GA(J)

C* Récupération des valeurs de luminance d'ordre IG-1.
C------------------------------------------------------
               XI1=I1(I,J)
               XI2=I1(I,-J)
               XQ1=Q1(I,J)
               XQ2=Q1(I,-J)
               XU1=U1(I,J)
               XU2=U1(I,-J)

C* Intégration spatiale sur mup.
C-------------------------------
               II2=II2+Z*(XI1*BPJK+XI2*BPJMK+XQ1*GRKJ
     &             +XQ2*GRKMJ-XU1*GTKJ-XU2*GTKMJ)
               II1=II1+Z*(XI1*BPJMK+XI2*BPJK+XQ1*GRKMJ
     &             +XQ2*GRKJ+XU1*GTKMJ+XU2*GTKJ)
               QQ2=QQ2+Z*(XI1*GRJK+XI2*GRJMK+XQ1*ARRJK
     &             +XQ2*ARRJMK+XU2*ARTJMK-XU1*ARTJK)
               QQ1=QQ1+Z*(XI1*GRJMK+XI2*GRJK+XQ1*ARRJMK
     &             +XQ2*ARRJK-XU1*ARTJMK+XU2*ARTJK)
               UU2=UU2-Z*(XI1*GTJK-XI2*GTJMK+XQ1*ARTKJ
     &             +XQ2*ARTKMJ-XU1*ATTJK-XU2*ATTJMK)
               UU1=UU1-Z*(XI1*GTJMK-XI2*GTJK-XQ1*ARTKMJ
     &             -XQ2*ARTKJ-XU1*ATTJMK-XU2*ATTJK)
     
   47       CONTINUE


C* Stockage des résultats.
C--------------------------
            I2(I,K)=II2 * 0.5
            I2(I,-K)=II1 * 0.5
            Q2(I,K)=QQ2  * 0.5
            Q2(I,-K)=QQ1 * 0.5
            U2(I,K)=UU2  * 0.5
            U2(I,-K)=UU1 * 0.5
	    
   45 CONTINUE    ! Boucle sur I et K

  212 CONTINUE

      !Mode expert spécifique : annulation de la diffusion atmosphérique
      IF (EXPERT_MODE_FORCED_FATM_NULL) THEN 
         WRITE(6,*) 
     &	 " \n WARNING : ATMOS SOURCE FUNCTION FORCED TO NULL \n"
         DO K=1,NBMU
	    DO I=0,CTE_NT_ATM
	       I2(I,K)=0.
	       I2(I,-K)=0.
	       Q2(I,K)=0.
	       Q2(I,-K)=0.
	       U2(I,K)=0.
	       U2(I,-K)=0.
	    ENDDO
	 ENDDO
      ENDIF
      
      RETURN
      END  	!FIN DE LA PROCEDURE OSOAA_FSOURCE_ATM_ORDREIG







C*============================================================================
C PROCEDURE: OSOAA_FSOURCE_SEA_ORDRE1_MERPLATE
C ==========
C
C      Cette procédure calcule le vecteur fonction source de la diffusion
C      marine primaire, à partir du champ de rayonnement solaire incident transmis
C      et des propriétés de diffusion de la mer (fonction de phase).
C
C      Elle réalise le calcul des éléments de la matrice de phase, 
C      selon l'ordre IS du développement en séries de Fourier.
C      On construit indirectement la matrice suivante P(mu,mup) :
C         BP(mu,mup)    GR(mu,mup)    -GT(mu,mup)
C         GR(mup,mu)    ARR(mu,mup)   -ART(mup,mu)
C        -GT(mup,mu)   -ART(mu,mup)    ATT(mu,mup)
C 
C      Les termes réellement calculés sont équivalents en suivant les règles
C      de permutation des indices. 
C	
C      Une intégration spatiale est appliquée sur les directions d'incidence.
C
C      Calcul pour chaque niveau du profil et pour chaque direction de 
C      propagation.
C
C
C     NB :
C        Le tableau CH_SEA_MUS donne l'atténuation de la surface à la profondeur I.
C        Le couple (EI_TR_SSURF,EQ_TR_SSURF) est alors l'éclairement solaire transmis 
C        sous la surface (composantes relatives aux paramètres de Stokes I et Q).
C        Il s'agit de l'éclairement solaire transmis, calculé pour une mer plate.
C		
C	  ==> Conditions indispensables pour une expression correcte du champ solaire
C	      dans la mer.
C
C      NB: Si EXPERT_MODE_FORCED_FSEA_NULL = .TRUE. la fonction source est posée nulle.
C          (constante introduite par #define)
C
C
C Description des paramètres
C -------------------------- 
C   
C       IS   (I4)    (E)  Ordre en cours du développement en séries de Fourier.
C
C       JKW  (I4)    (E)  Indice de localisation dans le tableau des angles
C                         de la valeur du cosinus de l'angle zénithal solaire
C                         transmis dans la mer.
C                         (JKW=-M0 ==> Correspond au musw vrai < 0) 
C
C       NBMU (I4)    (E)  Nombre d'angles (positifs) effectivement utiles.
C
C       CH_SEA_MUS(0:CTE_NT_SEA)  (double)  
C                    (E)  Atténuation du faisceau solaire direct / 4 
C                         dans la mer en fonction de la profondeur pour la 
C                         direction de propagation du faisceau solaire transmis.
C
C       PC_RAY(0:CTE_NT_SEA)  (double)  (E)  Pourcentage de molécules par niveau.
C       PC_CHL(0:CTE_NT_SEA)  (double)  (E)  Pourcentage de phytoplancton par niveau.
C       PC_MLP(0:CTE_NT_SEA)  (double)  (E)  Pourcentage de sédiments par niveau.
C
C       BETA0	  (double)  (E) 
C       BETA2     (double)  (E) 	.
C       GAMMA2	  (double)  (E) 
C       ALPHA2    (double)  (E) 	
C                   Coefficients du développement de la matrice de phase
C                   moléculaire en fonctions de Legendre
C
C       XPL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) 	
C      		     Fonction de Legendre PSL à l'ordre L=2 et S=IS
C      		     en fonction de l'angle mu.     		
C       XRL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) 	
C     		     Fonction RSL à l'ordre L=2 et S=IS en fonction de mu.
C       XTL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) 	
C     		     Fonction TSL à l'ordre L=2 et S=IS en fonction de mu.
C
C       EI_TR_SSURF (double) (E)  Eclairement solaire direct transmis sous la surface
C				  --> Composante selon I
C
C       EQ_TR_SSURF (double) (E)  Eclairement solaire direct transmis sous la surface
C				  --> Composante selon Q
C
C       BP_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)
C                    (E)  Elément P11.
C       GR_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)
C                    (E)  Elément P12 ou P21.
C       GT_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)
C                    (E)  Elément -P13 ou -P31.
C       ARR_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)
C                    (E)  Elément P22.
C       ART_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)
C                    (E)  Elément -P23 ou -P32.
C       ATT_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)
C                    (E)  Elément P33.
C               Eléments Pij de la matrice de phase du phytoplancton pour un ordre
C               IS de la décomposition en séries de Fourier.
C               Soit mup la direction d'incidence ( -1 < mup < 1)
C               et mu celle de diffusion ( -1 < mu < 1),
C               la diffusion est exprimée par Pij(mu,mup).
C
C
C       BP_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)
C                    (E)  Elément P11.
C       GR_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)
C                    (E)  Elément P12 ou P21.
C       GT_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)
C                    (E)  Elément -P13 ou -P31.
C       ARR_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)
C                    (E)  Elément P22.
C       ART_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)
C                    (E)  Elément -P23 ou -P32.
C       ATT_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)
C                    (E)  Elément P33.
C               Eléments Pij de la matrice de phase des sédiments pour un ordre
C               IS de la décomposition en séries de Fourier.
C               Soit mup la direction d'incidence ( -1 < mup < 1)
C               et mu celle de diffusion ( -1 < mu < 1),
C               la diffusion est exprimée par Pij(mu,mup).
C
C       I2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (S)	
C       Q2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (S)	
C       U2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (S)	
C	         Vecteur fonction source I,Q,U en fonction du niveau du profil
C	         et de la direction.
C
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     Aucun
C
C==============================================================================
      SUBROUTINE OSOAA_FSOURCE_SEA_ORDRE1_MERPLATE
     &                         (IS,JKW,NBMU,CH_SEA_MUS,
     &                          PC_RAY,PC_CHL,PC_MLP,
     &                          BETA0,BETA2,GAMMA2,ALPHA2,
     &                          XPL,XRL,XTL,
     &				EI_TR_SSURF,EQ_TR_SSURF,
     &                          BP_CHL,GR_CHL,GT_CHL,
     &				ARR_CHL,ART_CHL,
     &                          BP_MLP,GR_MLP,GT_MLP,
     &				ARR_MLP,ART_MLP,
     &                          I2,Q2,U2)
      
      IMPLICIT NONE
      


C* Définition des variables     
C*------------------------------------------------------------------

      DOUBLE PRECISION CH_SEA_MUS(0:CTE_NT_SEA)	
                                ! Atténuation du faisceau solaire direct / 4
                                ! dans la mer en fonction de la profondeur pour la 
				! direction de propagation du faisceau solaire transmis.

      DOUBLE PRECISION PC_RAY(0:CTE_NT_SEA)   !Pourcentage de molécules  par niveau.
      DOUBLE PRECISION PC_CHL(0:CTE_NT_SEA)   !Pourcentage de phytoplancton par niveau.
      DOUBLE PRECISION PC_MLP(0:CTE_NT_SEA)   !Pourcentage de sédiments par niveau.
      
      DOUBLE PRECISION BETA0	! Coefficients du développement de la matrice 
      DOUBLE PRECISION BETA2	! de phase moléculaire en fonctions de Legendre.
      DOUBLE PRECISION GAMMA2	
      DOUBLE PRECISION ALPHA2	

      DOUBLE PRECISION XPL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      			      ! Fonction de Legendre PSL à l'ordre L=2 et S=IS
      			      ! en fonction de l'angle mu.     		
      DOUBLE PRECISION XRL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      			! Fonction RSL à l'ordre L=2 et S=IS en fonction de mu.
      DOUBLE PRECISION XTL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      			! Fonction TSL à l'ordre L=2 et S=IS en fonction de mu.


      	! Eclairement solaire direct transmis sous la surface (composante U nulle) 
      DOUBLE PRECISION EI_TR_SSURF	! Champ I.
      DOUBLE PRECISION EQ_TR_SSURF	! Champ Q.

	! Vecteur fonction source en fonction du niveau du profil
	! et de la direction.
      DOUBLE PRECISION 
     &       I2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour I.
      DOUBLE PRECISION 
     &       Q2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour Q.
      DOUBLE PRECISION 
     &       U2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour U.


	! Eléments Pij de la matrice de phase du phytoplancton pour un ordre
	! IS de la décomposition en séries de Fourier.
	! Soit mup la direction d'incidence (-1 < mup < 1)
	! et mu celle de diffusion (-1 < mu < 1),
	! la diffusion est exprimée par Pij(mu,mup).	
      DOUBLE PRECISION 
     &    BP_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! Elément P11.
      DOUBLE PRECISION 
     &    GR_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! P12 ou P21.
      DOUBLE PRECISION 
     &    GT_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! -P13 ou -P31.
      DOUBLE PRECISION 
     &    ARR_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! P22.
      DOUBLE PRECISION 
     &    ART_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! -P23 ou -P32.
      

	! Eléments Pij de la matrice de phase des sédiments pour un ordre
	! IS de la décomposition en séries de Fourier.
	! Soit mup la direction d'incidence (-1 < mup < 1)
	! et mu celle de diffusion (-1 < mu < 1),
	! la diffusion est exprimée par Pij(mu,mup).	
      DOUBLE PRECISION 
     &    BP_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! Elément P11.
      DOUBLE PRECISION 
     &    GR_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! P12 ou P21.
      DOUBLE PRECISION 
     &    GT_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! -P13 ou -P31.
      DOUBLE PRECISION 
     &    ARR_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! P22.
      DOUBLE PRECISION 
     &    ART_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! -P23 ou -P32.



      DOUBLE PRECISION XPK	! Valeur de XPL pour les directions de
      				! diffusion mu > 0 (indice K).	
      DOUBLE PRECISION XRK	! Valeur de XRL pour mu > 0.
      DOUBLE PRECISION XTK	! Valeur de XTL pour mu > 0.
      DOUBLE PRECISION YPK	! Valeur de XPL pour -mu.	
      DOUBLE PRECISION YRK	! Valeur de XRL pour -mu.
      DOUBLE PRECISION YTK	! Valeur de XTL pour -mu.	
      	
      DOUBLE PRECISION XPJ	! Valeur de XPL pour les directions incidentes
      				! mu = mup > 0 (indice J).
      DOUBLE PRECISION XRJ	! Valeur de XRL pour mu = mup > 0.
      DOUBLE PRECISION XTJ	! Valeur de XTL pour mu = mup > 0.
      DOUBLE PRECISION YPJ	! Valeur de XPL pour mu = -mup.	
      DOUBLE PRECISION YRJ	! Valeur de XRL pour mu = -mup.
      DOUBLE PRECISION YTJ	! Valeur de XTL pour mu = -mup.		

      DOUBLE PRECISION XI2	! Valeur de I du champ descendant d'ordre IG-1.
      DOUBLE PRECISION XQ2	! Valeur de Q du champ descendant d'ordre IG-1.

      DOUBLE PRECISION ATTDIR   ! Atténuation dans l'océan pour la direction
      				! d'incidence solaire transmise.

      DOUBLE PRECISION PCRAY	! Valeur du pourcentage de molécules.
      DOUBLE PRECISION PCCHL	! Valeur du pourcentage de phytoplancton.
      DOUBLE PRECISION PCMLP	! Valeur du pourcentage des sédiments.
 
      DOUBLE PRECISION BPJK	! Valeur des éléments de la matrice de 
      DOUBLE PRECISION BPJMK    ! phase du mélange hydrosols.
      DOUBLE PRECISION GTJMK
      DOUBLE PRECISION GTJK
      DOUBLE PRECISION GRJK
      DOUBLE PRECISION GRKJ
      DOUBLE PRECISION GRKMJ
      DOUBLE PRECISION GRJMK
      DOUBLE PRECISION ARRJK
      DOUBLE PRECISION ARRJMK
      DOUBLE PRECISION ARTKJ
      DOUBLE PRECISION ARTKMJ

      INTEGER*4 IS	! Ordre en cours du développement en séries de Fourier.
      INTEGER*4 NBMU    ! Nombre d'angles effectifs des simulations

      INTEGER*4 JKW	! Numéro de stockage de mus dans l'eau dans le tableau RMU
      			! (JKW=-M0 ==> Correspond au musw vrai < 0).

      INTEGER*4 I
      INTEGER*4 K
      INTEGER*4 J
            

C------------------------------------------------------
C* Calcul des éléments de la matrice de phase, selon 
C* l'ordre IS du développement en séries de Fourier.
C------------------------------------------------------

      IF (IS-2) 210,210,211
      
C--------------------------------
C* Calculs pour IS <= 2.
C-------------------------------

C* Boucle sur les directions de diffusion mu (mu > 0).
C------------------------------------------------------
 210  DO 455 K=1,NBMU

C* Valeur des fonctions PSL, RSL et TSL à l'ordre L=2 et S=IS
C* en fonction de l'angle mup et -mup
C* (pour le calcul de la matrice de phase moléculaire 
C*  par le programme appelant).
C------------------------------------------------------
         XPK=XPL(K)
         XRK=XRL(K)
         XTK=XTL(K)
         YPK=XPL(-K)
         YRK=XRL(-K)
         YTK=XTL(-K)

C* Boucle sur le profil.
C-----------------------
         DO 455 I=0,CTE_NT_SEA
            
            PCRAY=PC_RAY(I)
            PCCHL=PC_CHL(I)	   
            PCMLP=PC_MLP(I)
            ATTDIR=CH_SEA_MUS(I)

C* Direction d'incidence solaire : mup.
C------------------------------------------------------
            J=-JKW	!valeur M0

C* Calcul des éléments de la matrice de phase P(mu,mup) 
C* à l'ordre IS pour le mélange molécules + hydrosols.
C------------------------------------------------------
            XPJ=XPL(J)
            XRJ=XRL(J)
            XTJ=XTL(J)
            YPJ=XPL(-J)
            YRJ=XRL(-J)
            YTJ=XTL(-J)

            BPJK=BP_CHL(J,K)*PCCHL+BP_MLP(J,K)*PCMLP
     &  	   +PCRAY*(BETA0+BETA2*XPJ*XPK)
            BPJMK=BP_CHL(J,-K)*PCCHL+BP_MLP(J,-K)*PCMLP
     &  	   +PCRAY*(BETA0+BETA2*XPJ*YPK)
            GTJMK=GT_CHL(J,-K)*PCCHL+GT_MLP(J,-K)*PCMLP
     &  	   +PCRAY*(GAMMA2*XPJ*YTK)
            GTJK=GT_CHL(J,K)*PCCHL+GT_MLP(J,K)*PCMLP
     &  	   +PCRAY*(GAMMA2*XPJ*XTK)          
            GRJK=GR_CHL(J,K)*PCCHL+GR_MLP(J,K)*PCMLP
     &  	   +PCRAY*(GAMMA2*XPJ*XRK)
            GRJMK=GR_CHL(J,-K)*PCCHL+GR_MLP(J,-K)*PCMLP
     &  	   +PCRAY*(GAMMA2*XPJ*YRK)
            GRKJ=GR_CHL(K,J)*PCCHL+GR_MLP(K,J)*PCMLP
     &  	   +PCRAY*(GAMMA2*XPK*XRJ)
            GRKMJ=GR_CHL(K,-J)*PCCHL+GR_MLP(K,-J)*PCMLP
     &  	   +PCRAY*(GAMMA2*XPK*YRJ)
            ARRJK=ARR_CHL(J,K)*PCCHL+ARR_MLP(J,K)*PCMLP
     &  	   +PCRAY*(ALPHA2*XRJ*XRK)
            ARRJMK=ARR_CHL(J,-K)*PCCHL+ARR_MLP(J,-K)*PCMLP
     &  	   +PCRAY*(ALPHA2*XRJ*YRK)
            ARTKJ=ART_CHL(K,J)*PCCHL+ART_MLP(K,J)*PCMLP
     &  	   +PCRAY*(ALPHA2*XTK*XRJ)
            ARTKMJ=ART_CHL(K,-J)*PCCHL+ART_MLP(K,-J)*PCMLP
     &  	   +PCRAY*(ALPHA2*XTK*YRJ)
  



C* Calculs de la fonction source d'ordre IG, pour chaque 
C* niveau du profil et chaque direction.
C------------------------------------------------------
               
C* Récupération des valeurs de luminance d'ordre IG-1.
C------------------------------------------------------
            XI2 = EI_TR_SSURF * ATTDIR		
            XQ2 = EQ_TR_SSURF * ATTDIR

C* Application à la direction d'incidence solaire.
C-------------------------------------------------
            I2(I,K)  =XI2*BPJMK +XQ2*GRKMJ  
            I2(I,-K) =XI2*BPJK  +XQ2*GRKJ   
            Q2(I,K)  =XI2*GRJMK +XQ2*ARRJMK 
            Q2(I,-K) =XI2*GRJK  +XQ2*ARRJK  
            U2(I,K)  =XI2*GTJMK -XQ2*ARTKMJ 
            U2(I,-K) =XI2*GTJK  +XQ2*ARTKJ  

 455  CONTINUE   ! Boucle sur I et K
 
      GOTO 212
      
      
C-----------------------------
C* Calculs pour IS > 2.
C-----------------------------

C* Boucle sur les directions de diffusion mu (mu > 0).
C------------------------------------------------------
 211  DO 45 K=1,NBMU

C* Boucle sur le profil.
C------------------------
         DO 45 I=0,CTE_NT_SEA
            
            PCCHL=PC_CHL(I)	   
            PCMLP=PC_MLP(I)
	    ATTDIR=CH_SEA_MUS(I)

C* Direction d'incidence solaire : mup.
C------------------------------------------------------
            J=-JKW	 !valeur M0


C* Calcul des éléments de la matrice de phase aérosols P(mu,mup)
C* à l'ordre IS, pondérée par la proportion en hydrosols.
C------------------------------------------------------
            BPJK=BP_CHL(J,K)*PCCHL + BP_MLP(J,K)*PCMLP
            BPJMK=BP_CHL(J,-K)*PCCHL + BP_MLP(J,-K)*PCMLP
            GTJMK=GT_CHL(J,-K)*PCCHL + GT_MLP(J,-K)*PCMLP
            GTJK=GT_CHL(J,K)*PCCHL + GT_MLP(J,K)*PCMLP
            GRJK=GR_CHL(J,K)*PCCHL + GR_MLP(J,K)*PCMLP
            GRJMK=GR_CHL(J,-K)*PCCHL + GR_MLP(J,-K)*PCMLP
            GRKJ=GR_CHL(K,J)*PCCHL + GR_MLP(K,J)*PCMLP
            GRKMJ=GR_CHL(K,-J)*PCCHL + GR_MLP(K,-J)*PCMLP
            ARRJK=ARR_CHL(J,K)*PCCHL + ARR_MLP(J,K)*PCMLP
            ARRJMK=ARR_CHL(J,-K)*PCCHL + ARR_MLP(J,-K)*PCMLP
            ARTKJ=ART_CHL(K,J)*PCCHL + ART_MLP(K,J)*PCMLP
            ARTKMJ=ART_CHL(K,-J)*PCCHL + ART_MLP(K,-J)*PCMLP
            
	       
C* Récupération des valeurs de luminance d'ordre IG-1.
C------------------------------------------------------
            XI2 = EI_TR_SSURF * ATTDIR		
            XQ2 = EQ_TR_SSURF * ATTDIR

C* Application à la direction d'incidence solaire.
C-------------------------------------------------
            I2(I,K) = XI2*BPJMK +XQ2*GRKMJ  
            I2(I,-K)= XI2*BPJK  +XQ2*GRKJ   
            Q2(I,K) = XI2*GRJMK +XQ2*ARRJMK 
            Q2(I,-K)= XI2*GRJK  +XQ2*ARRJK 
            U2(I,K) = XI2*GTJMK -XQ2*ARTKMJ 
            U2(I,-K)= XI2*GTJK  +XQ2*ARTKJ  

   45 CONTINUE    ! Boucle sur I et K

  212 CONTINUE


      !Mode expert spécifique : annulation de la diffusion marine
      IF (EXPERT_MODE_FORCED_FSEA_NULL) THEN 
         WRITE(6,*) 
     &	 " \n WARNING : SEA SOURCE FUNCTION FORCED TO NULL \n"
         DO K=1,NBMU
	    DO I=0,CTE_NT_SEA
	       I2(I,K)=0.
	       I2(I,-K)=0.
	       Q2(I,K)=0.
	       Q2(I,-K)=0.
	       U2(I,K)=0.
	       U2(I,-K)=0.
	    ENDDO
	 ENDDO
      ENDIF
      	
      RETURN
      END  	!FIN DE LA PROCEDURE OSOAA_FSOURCE_SEA_ORDRE1_MERPLATE









C*============================================================================
C PROCEDURE: OSOAA_FSOURCE_SEA_ORDREIG
C ==========
C      Cette procédure calcule le vecteur fonction source de la diffusion
C      marine d'ordre IG>1, à partir du champ de rayonnement d'ordre IG-1 
C      et des propriétés de diffusion de la mer (fonction de phase).
C
C      Elle réalise le calcul des éléments de la matrice de phase, 
C      selon l'ordre IS du développement en séries de Fourier.
C      On construit indirectement la matrice suivante P(mu,mup) :
C         BP(mu,mup)    GR(mu,mup)    -GT(mu,mup)
C         GR(mup,mu)    ARR(mu,mup)   -ART(mup,mu)
C        -GT(mup,mu)   -ART(mu,mup)    ATT(mu,mup)
C 
C      Les termes réellement calculés sont équivalents en suivant les règles
C      de permutation des indices. 
C	
C      Une intégration spatiale est appliquée sur les directions d'incidence.
C
C      Calcul pour chaque niveau du profil et pour chaque direction de propagation.
C
C
C      NB: Si EXPERT_MODE_FORCED_FSEA_NULL = .TRUE. la fonction source est posée nulle.
C          (constante introduite par #define)
C
C 
C Description des paramètres
C -------------------------- 
C   
C       IS   (I4)    (E)  Ordre en cours du développement en séries de Fourier.
C       NBMU (I4)    (E)  Nombre d'angles (positifs) effectivement utiles
C
C       PC_RAY(0:CTE_NT_SEA)  (double)  (E)  Pourcentage de molécules par niveau.
C       PC_CHL(0:CTE_NT_SEA)  (double)  (E)  Pourcentage de phytoplancton par niveau.
C       PC_MLP(0:CTE_NT_SEA)  (double)  (E)  Pourcentage de sédiments par niveau.
C
C       BETA0	  (double)  (E) 
C       BETA2     (double)  (E) 	.
C       GAMMA2	  (double)  (E) 
C       ALPHA2    (double)  (E) 	
C                   Coefficients du développement de la matrice de phase
C                   moléculaire en fonctions de Legendre
C
C       XPL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) 	
C      		     Fonction de Legendre PSL à l'ordre L=2 et S=IS
C      		     en fonction de l'angle mu.     		
C       XRL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) 	
C     		     Fonction RSL à l'ordre L=2 et S=IS en fonction de mu.
C       XTL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) 	
C     		     Fonction TSL à l'ordre L=2 et S=IS en fonction de mu.
C      	
C       I1(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)    Champ I.
C       Q1(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)    Champ Q.
C       U1(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)    Champ U.
C         	     Champ de luminance d'ordre IG-1 en fonction du 
C                    niveau du profil et de la direction. 	
C
C       BP_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)
C                    (E)  Elément P11.
C       GR_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)
C                    (E)  Elément P12 ou P21.
C       GT_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)
C                    (E)  Elément -P13 ou -P31.
C       ARR_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)
C                    (E)  Elément P22.
C       ART_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)
C                    (E)  Elément -P23 ou -P32.
C       ATT_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)
C                    (E)  Elément P33.
C               Eléments Pij de la matrice de phase du phytoplancton pour un ordre
C               IS de la décomposition en séries de Fourier.
C               Soit mup la direction d'incidence ( -1 < mup < 1)
C               et mu celle de diffusion ( -1 < mu < 1),
C               la diffusion est exprimée par Pij(mu,mup).
C
C       BP_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)
C                    (E)  Elément P11.
C       GR_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)
C                    (E)  Elément P12 ou P21.
C       GT_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)
C                    (E)  Elément -P13 ou -P31.
C       ARR_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)
C                    (E)  Elément P22.
C       ART_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)
C                    (E)  Elément -P23 ou -P32.
C       ATT_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)
C                    (E)  Elément P33.
C               Eléments Pij de la matrice de phase des sédiments pour un ordre
C               IS de la décomposition en séries de Fourier.
C               Soit mup la direction d'incidence ( -1 < mup < 1)
C               et mu celle de diffusion ( -1 < mu < 1),
C               la diffusion est exprimée par Pij(mu,mup).
C
C       GA(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) Tableau des poids des angles.
C
C       I2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (S)	
C       Q2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (S)	
C       U2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (S)	
C	         Vecteur fonction source I,Q,U en fonction du niveau du profil
C	         et de la direction.
C
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     Aucun
C
C==============================================================================
      SUBROUTINE OSOAA_FSOURCE_SEA_ORDREIG(IS,NBMU,
     &                                     PC_RAY,PC_CHL,PC_MLP,
     &                                     BETA0,BETA2,GAMMA2,ALPHA2,
     &                                     XPL,XRL,XTL,I1,Q1,U1,
     &                                     BP_CHL,GR_CHL,GT_CHL,
     &					   ARR_CHL,ART_CHL,ATT_CHL,
     &                                     BP_MLP,GR_MLP,GT_MLP,
     &					   ARR_MLP,ART_MLP,ATT_MLP,GA,
     &                                     I2,Q2,U2)
      
      IMPLICIT NONE
      


C* Définition des variables     
C*------------------------------------------------------------------

      DOUBLE PRECISION PC_RAY(0:CTE_NT_SEA)   !Pourcentage de molécules  par niveau.
      DOUBLE PRECISION PC_CHL(0:CTE_NT_SEA)   !Pourcentage de phytoplancton par niveau.
      DOUBLE PRECISION PC_MLP(0:CTE_NT_SEA)   !Pourcentage de sédiments par niveau.
      
      DOUBLE PRECISION BETA0	! Coefficients du développement de la matrice 
      DOUBLE PRECISION BETA2	! de phase moléculaire en fonctions de Legendre.
      DOUBLE PRECISION GAMMA2	
      DOUBLE PRECISION ALPHA2	

      DOUBLE PRECISION XPL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      			      ! Fonction de Legendre PSL à l'ordre L=2 et S=IS
      			      ! en fonction de l'angle mu.     		
      DOUBLE PRECISION XRL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      			! Fonction RSL à l'ordre L=2 et S=IS en fonction de mu.
      DOUBLE PRECISION XTL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      			! Fonction TSL à l'ordre L=2 et S=IS en fonction de mu.


      	! Champ de luminance d'ordre IG-1 en fonction du niveau du profil
	! et de la direction.       	
      DOUBLE PRECISION 
     &       I1(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ I.
      DOUBLE PRECISION 
     &       Q1(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ Q.
      DOUBLE PRECISION 
     &       U1(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ U.

	! Vecteur fonction source en fonction du niveau du profil
	! et de la direction.
      DOUBLE PRECISION 
     &       I2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour I.
      DOUBLE PRECISION 
     &       Q2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour Q.
      DOUBLE PRECISION 
     &       U2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour U.


	! Eléments Pij de la matrice de phase du phytoplancton pour un ordre
	! IS de la décomposition en séries de Fourier.
	! Soit mup la direction d'incidence (-1 < mup < 1)
	! et mu celle de diffusion (-1 < mu < 1),
	! la diffusion est exprimée par Pij(mu,mup).	
      DOUBLE PRECISION 
     &    BP_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! Elément P11.
      DOUBLE PRECISION 
     &    GR_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! P12 ou P21.
      DOUBLE PRECISION 
     &    GT_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! -P13 ou -P31.
      DOUBLE PRECISION 
     &    ARR_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! P22.
      DOUBLE PRECISION 
     &    ART_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! -P23 ou -P32.
      DOUBLE PRECISION 
     &    ATT_CHL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! P33.    

	! Eléments Pij de la matrice de phase des sédiments pour un ordre
	! IS de la décomposition en séries de Fourier.
	! Soit mup la direction d'incidence (-1 < mup < 1)
	! et mu celle de diffusion (-1 < mu < 1),
	! la diffusion est exprimée par Pij(mu,mup).	
      DOUBLE PRECISION 
     &    BP_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! Elément P11.
      DOUBLE PRECISION 
     &    GR_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! P12 ou P21.
      DOUBLE PRECISION 
     &    GT_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &           -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! -P13 ou -P31.
      DOUBLE PRECISION 
     &    ARR_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! P22.
      DOUBLE PRECISION 
     &    ART_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! -P23 ou -P32.
      DOUBLE PRECISION 
     &    ATT_MLP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &            -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! P33.   
   



      DOUBLE PRECISION GA(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! Tableau des poids de Gauss

      DOUBLE PRECISION XPK	! Valeur de XPL pour les directions de
      				! diffusion mu > 0 (indice K).	
      DOUBLE PRECISION XRK	! Valeur de XRL pour mu > 0.
      DOUBLE PRECISION XTK	! Valeur de XTL pour mu > 0.
      DOUBLE PRECISION YPK	! Valeur de XPL pour -mu.	
      DOUBLE PRECISION YRK	! Valeur de XRL pour -mu.
      DOUBLE PRECISION YTK	! Valeur de XTL pour -mu.	
      	
      DOUBLE PRECISION XPJ	! Valeur de XPL pour les directions incidentes
      				! mu = mup > 0 (indice J).
      DOUBLE PRECISION XRJ	! Valeur de XRL pour mu = mup > 0.
      DOUBLE PRECISION XTJ	! Valeur de XTL pour mu = mup > 0.
      DOUBLE PRECISION YPJ	! Valeur de XPL pour mu = -mup.	
      DOUBLE PRECISION YRJ	! Valeur de XRL pour mu = -mup.
      DOUBLE PRECISION YTJ	! Valeur de XTL pour mu = -mup.		

      DOUBLE PRECISION XI1	! Valeur de I du champ montant d'ordre IG-1.
      DOUBLE PRECISION XQ1	! Valeur de Q du champ montant d'ordre IG-1.
      DOUBLE PRECISION XU1	! Valeur de U du champ montant d'ordre IG-1.
      DOUBLE PRECISION XI2	! Valeur de I du champ descendant d'ordre IG-1.
      DOUBLE PRECISION XQ2	! Valeur de Q du champ descendant d'ordre IG-1.
      DOUBLE PRECISION XU2	! Valeur de U du champ descendant d'ordre IG-1.

      DOUBLE PRECISION II1	! Valeur intermédiaire pour le calcul de I2 (mu < 0).
      DOUBLE PRECISION QQ1	! Valeur intermédiaire pour le calcul de Q2 (mu < 0).
      DOUBLE PRECISION UU1	! Valeur intermédiaire pour le calcul de U2 (mu < 0).
      DOUBLE PRECISION II2 	! Valeur intermédiaire pour le calcul de I2 (mu > 0).
      DOUBLE PRECISION QQ2 	! Valeur intermédiaire pour le calcul de Q2 (mu > 0).
      DOUBLE PRECISION UU2 	! Valeur intermédiaire pour le calcul de U2 (mu > 0).

      DOUBLE PRECISION Z        ! Valeur ponctuelle du tableau des poids de Gauss.

      DOUBLE PRECISION PCRAY	! Valeur du pourcentage de molécules.
      DOUBLE PRECISION PCCHL	! Valeur du pourcentage de phytoplancton.
      DOUBLE PRECISION PCMLP	! Valeur du pourcentage des sédiments.
 
      DOUBLE PRECISION BPJK	! Valeur des éléments de la matrice de 
      DOUBLE PRECISION BPJMK    ! phase du mélange molécules + aérosols.
      DOUBLE PRECISION GTJMK
      DOUBLE PRECISION GTJK
      DOUBLE PRECISION GTKJ
      DOUBLE PRECISION GTKMJ
      DOUBLE PRECISION GRJK
      DOUBLE PRECISION GRKJ
      DOUBLE PRECISION GRKMJ
      DOUBLE PRECISION GRJMK
      DOUBLE PRECISION ARRJK
      DOUBLE PRECISION ARRJMK
      DOUBLE PRECISION ARTJK
      DOUBLE PRECISION ARTJMK
      DOUBLE PRECISION ARTKJ
      DOUBLE PRECISION ARTKMJ
      DOUBLE PRECISION ATTJK
      DOUBLE PRECISION ATTJMK

      INTEGER*4 IS	! Ordre en cours du développement en séries de Fourier.
      INTEGER*4 NBMU    ! Nombre d'angles effectifs des simulations

      INTEGER*4 I
      INTEGER*4 K
      INTEGER*4 J


C------------------------------------------------------
C* Calcul des éléments de la matrice de phase, selon 
C* l'ordre IS du développement en séries de Fourier.
C------------------------------------------------------

      IF (IS-2) 210,210,211
      
C--------------------------------
C* Calculs pour IS <= 2.
C-------------------------------

C* Boucle sur les directions de diffusion mu (mu > 0).
C------------------------------------------------------
 210  DO 455 K=1,NBMU

C* Valeur des fonctions PSL, RSL et TSL à l'ordre L=2 et S=IS
C* en fonction de l'angle mup et -mup
C* (pour le calcul de la matrice de phase moléculaire 
C*  par le programme appelant).
C------------------------------------------------------
         XPK=XPL(K)
         XRK=XRL(K)
         XTK=XTL(K)
         YPK=XPL(-K)
         YRK=XRL(-K)
         YTK=XTL(-K)

C* Boucle sur le profil.
C-----------------------
         DO 455 I=0,CTE_NT_SEA
            II1=0.
            II2=0.
            QQ1=0.
            QQ2=0.
            UU1=0.
            UU2=0.
            PCRAY=PC_RAY(I)
            PCCHL=PC_CHL(I)	   
            PCMLP=PC_MLP(I)


C* Boucle sur les directions d'incidence mup.
C------------------------------------------------------
            DO 477 J=1,NBMU

C* Calcul des éléments de la matrice de phase P(mu,mup) 
C* à l'ordre IS pour le mélange molécules + hydrosols.
C------------------------------------------------------
               XPJ=XPL(J)
               XRJ=XRL(J)
               XTJ=XTL(J)
               YPJ=XPL(-J)
               YRJ=XRL(-J)
               YTJ=XTL(-J)

               BPJK=BP_CHL(J,K)*PCCHL+BP_MLP(J,K)*PCMLP
     &  	   +PCRAY*(BETA0+BETA2*XPJ*XPK)
               BPJMK=BP_CHL(J,-K)*PCCHL+BP_MLP(J,-K)*PCMLP
     &  	   +PCRAY*(BETA0+BETA2*XPJ*YPK)
               GTJMK=GT_CHL(J,-K)*PCCHL+GT_MLP(J,-K)*PCMLP
     &  	   +PCRAY*(GAMMA2*XPJ*YTK)
               GTJK=GT_CHL(J,K)*PCCHL+GT_MLP(J,K)*PCMLP
     &  	   +PCRAY*(GAMMA2*XPJ*XTK)
               GTKMJ=GT_CHL(K,-J)*PCCHL+GT_MLP(K,-J)*PCMLP
     &  	   +PCRAY*(GAMMA2*XPK*YTJ)
               GTKJ=GT_CHL(K,J)*PCCHL+GT_MLP(K,J)*PCMLP
     &  	   +PCRAY*(GAMMA2*XPK*XTJ)
               GRJK=GR_CHL(J,K)*PCCHL+GR_MLP(J,K)*PCMLP
     &  	   +PCRAY*(GAMMA2*XPJ*XRK)
               GRJMK=GR_CHL(J,-K)*PCCHL+GR_MLP(J,-K)*PCMLP
     &  	   +PCRAY*(GAMMA2*XPJ*YRK)
               GRKJ=GR_CHL(K,J)*PCCHL+GR_MLP(K,J)*PCMLP
     &  	   +PCRAY*(GAMMA2*XPK*XRJ)
               GRKMJ=GR_CHL(K,-J)*PCCHL+GR_MLP(K,-J)*PCMLP
     &  	   +PCRAY*(GAMMA2*XPK*YRJ)
               ARRJK=ARR_CHL(J,K)*PCCHL+ARR_MLP(J,K)*PCMLP
     &  	   +PCRAY*(ALPHA2*XRJ*XRK)
               ARRJMK=ARR_CHL(J,-K)*PCCHL+ARR_MLP(J,-K)*PCMLP
     &  	   +PCRAY*(ALPHA2*XRJ*YRK)
               ARTJK=ART_CHL(J,K)*PCCHL+ART_MLP(J,K)*PCMLP
     &  	   +PCRAY*(ALPHA2*XTJ*XRK)
               ARTJMK=ART_CHL(J,-K)*PCCHL+ART_MLP(J,-K)*PCMLP
     &  	   +PCRAY*(ALPHA2*XTJ*YRK)
               ARTKJ=ART_CHL(K,J)*PCCHL+ART_MLP(K,J)*PCMLP
     &  	   +PCRAY*(ALPHA2*XTK*XRJ)
               ARTKMJ=ART_CHL(K,-J)*PCCHL+ART_MLP(K,-J)*PCMLP
     &  	   +PCRAY*(ALPHA2*XTK*YRJ)
               ATTJMK=ATT_CHL(J,-K)*PCCHL+ATT_MLP(J,-K)*PCMLP
     &  	   +PCRAY*(ALPHA2*XTJ*YTK)
               ATTJK=ATT_CHL(J,K)*PCCHL+ATT_MLP(J,K)*PCMLP
     &  	   +PCRAY*(ALPHA2*XTJ*XTK)




C* Calculs de la fonction source d'ordre IG, pour chaque 
C* niveau du profil et chaque direction.
C------------------------------------------------------
               Z=GA(J)

C* Récupération des valeurs de luminance d'ordre IG-1.
C------------------------------------------------------
               XI1=I1(I,J)
               XI2=I1(I,-J)
               XQ1=Q1(I,J)
               XQ2=Q1(I,-J)
               XU1=U1(I,J)
               XU2=U1(I,-J)

C* Intégration spatiale sur mup.
C---------------------------------
               II2=II2+Z*(XI1*BPJK+XI2*BPJMK+XQ1*GRKJ
     &             +XQ2*GRKMJ-XU1*GTKJ-XU2*GTKMJ)
               II1=II1+Z*(XI1*BPJMK+XI2*BPJK+XQ1*GRKMJ
     &             +XQ2*GRKJ+XU1*GTKMJ+XU2*GTKJ)
               QQ2=QQ2+Z*(XI1*GRJK+XI2*GRJMK+XQ1*ARRJK
     &             +XQ2*ARRJMK+XU2*ARTJMK-XU1*ARTJK)
               QQ1=QQ1+Z*(XI1*GRJMK+XI2*GRJK+XQ1*ARRJMK
     &             +XQ2*ARRJK-XU1*ARTJMK+XU2*ARTJK)
               UU2=UU2-Z*(XI1*GTJK-XI2*GTJMK+XQ1*ARTKJ
     &             +XQ2*ARTKMJ-XU1*ATTJK-XU2*ATTJMK)
               UU1=UU1-Z*(XI1*GTJMK-XI2*GTJK-XQ1*ARTKMJ
     &             -XQ2*ARTKJ-XU1*ATTJMK-XU2*ATTJK)
 477        CONTINUE

C* Stockage des résultats
C--------------------------
            I2(I,K)=II2 * 0.5
            I2(I,-K)=II1 * 0.5
            Q2(I,K)=QQ2 * 0.5
            Q2(I,-K)=QQ1 * 0.5
            U2(I,K)=UU2 * 0.5
            U2(I,-K)=UU1 * 0.5
 455  CONTINUE   ! Boucle sur I et K
 
      GOTO 212
      
      
C-----------------------------
C* Calculs pour IS > 2.
C-----------------------------

C* Boucle sur les directions de diffusion mu (mu > 0).
C------------------------------------------------------
 211  DO 45 K=1,NBMU

C* Boucle sur le profil.
C------------------------
         DO 45 I=0,CTE_NT_SEA
            II1=0.
            II2=0.
            QQ1=0.
            QQ2=0.
            UU1=0.
            UU2=0.
            PCCHL=PC_CHL(I)	   
            PCMLP=PC_MLP(I)

C* Boucle sur les directions d'incidence mup
C-------------------------------------------
            DO 47 J=1,NBMU

C* Calcul des éléments de la matrice de phase hydrosols P(mu,mup)
C* à l'ordre IS, pondérée par la proportion en hydrosols.
C------------------------------------------------------
               BPJK=BP_CHL(J,K)*PCCHL + BP_MLP(J,K)*PCMLP
               BPJMK=BP_CHL(J,-K)*PCCHL + BP_MLP(J,-K)*PCMLP
               GTJMK=GT_CHL(J,-K)*PCCHL + GT_MLP(J,-K)*PCMLP
               GTJK=GT_CHL(J,K)*PCCHL + GT_MLP(J,K)*PCMLP
               GTKMJ=GT_CHL(K,-J)*PCCHL + GT_MLP(K,-J)*PCMLP
               GTKJ=GT_CHL(K,J)*PCCHL + GT_MLP(K,J)*PCMLP
               GRJK=GR_CHL(J,K)*PCCHL + GR_MLP(J,K)*PCMLP
               GRJMK=GR_CHL(J,-K)*PCCHL + GR_MLP(J,-K)*PCMLP
               GRKJ=GR_CHL(K,J)*PCCHL + GR_MLP(K,J)*PCMLP
               GRKMJ=GR_CHL(K,-J)*PCCHL + GR_MLP(K,-J)*PCMLP
               ARRJK=ARR_CHL(J,K)*PCCHL + ARR_MLP(J,K)*PCMLP
               ARRJMK=ARR_CHL(J,-K)*PCCHL + ARR_MLP(J,-K)*PCMLP
               ARTJK=ART_CHL(J,K)*PCCHL + ART_MLP(J,K)*PCMLP
               ARTJMK=ART_CHL(J,-K)*PCCHL + ART_MLP(J,-K)*PCMLP
               ARTKJ=ART_CHL(K,J)*PCCHL + ART_MLP(K,J)*PCMLP
               ARTKMJ=ART_CHL(K,-J)*PCCHL + ART_MLP(K,-J)*PCMLP
               ATTJMK=ATT_CHL(J,-K)*PCCHL + ATT_MLP(J,-K)*PCMLP
               ATTJK=ATT_CHL(J,K)*PCCHL + ATT_MLP(J,K)*PCMLP
	       
C* Calculs de la fonction source d'ordre IG, pour chaque 
C* niveau du profil et chaque direction.
C------------------------------------------------------
               Z=GA(J)

C* Récupération des valeurs de luminance d'ordre IG-1.
C------------------------------------------------------
               XI1=I1(I,J)
               XI2=I1(I,-J)
               XQ1=Q1(I,J)
               XQ2=Q1(I,-J)
               XU1=U1(I,J)
               XU2=U1(I,-J)

C* Intégration spatiale sur mup.
C-------------------------------
               II2=II2+Z*(XI1*BPJK+XI2*BPJMK+XQ1*GRKJ
     &             +XQ2*GRKMJ-XU1*GTKJ-XU2*GTKMJ)
               II1=II1+Z*(XI1*BPJMK+XI2*BPJK+XQ1*GRKMJ
     &             +XQ2*GRKJ+XU1*GTKMJ+XU2*GTKJ)
               QQ2=QQ2+Z*(XI1*GRJK+XI2*GRJMK+XQ1*ARRJK
     &             +XQ2*ARRJMK+XU2*ARTJMK-XU1*ARTJK)
               QQ1=QQ1+Z*(XI1*GRJMK+XI2*GRJK+XQ1*ARRJMK
     &             +XQ2*ARRJK-XU1*ARTJMK+XU2*ARTJK)
               UU2=UU2-Z*(XI1*GTJK-XI2*GTJMK+XQ1*ARTKJ
     &             +XQ2*ARTKMJ-XU1*ATTJK-XU2*ATTJMK)
               UU1=UU1-Z*(XI1*GTJMK-XI2*GTJK-XQ1*ARTKMJ
     &             -XQ2*ARTKJ-XU1*ATTJMK-XU2*ATTJK)
     
   47       CONTINUE


C* Stockage des résultats.
C--------------------------
            I2(I,K)=II2 * 0.5
            I2(I,-K)=II1 * 0.5
            Q2(I,K)=QQ2  * 0.5
            Q2(I,-K)=QQ1 * 0.5
            U2(I,K)=UU2  * 0.5
            U2(I,-K)=UU1 * 0.5
	    
   45 CONTINUE    ! Boucle sur I et K

  212 CONTINUE


      !Mode expert spécifique : annulation de la diffusion marine
      IF (EXPERT_MODE_FORCED_FSEA_NULL) THEN 
         WRITE(6,*) 
     &	 " \n WARNING : SEA SOURCE FUNCTION FORCED TO NULL \n"
         DO K=1,NBMU
	    DO I=0,CTE_NT_SEA
	       I2(I,K)=0.
	       I2(I,-K)=0.
	       Q2(I,K)=0.
	       Q2(I,-K)=0.
	       U2(I,K)=0.
	       U2(I,-K)=0.
	    ENDDO
	 ENDDO
      ENDIF
      
      RETURN
      END  	!FIN DE LA PROCEDURE OSOAA_FSOURCE_SEA_ORDREIG









C*===========================================================================
C PROCEDURE: OSOAA_INTEGR_EPOPT_ATM_UP
C ==========
C      Cette procédure réalise l'intégration sur l'épaisseur optique
C      du champ de rayonnement dans l'atmosphère:
C        - champ montant    : Intégration de la surface au TOA.
C
C      L'intégration est faite de proche en proche du niveau de départ
C      au niveau final.
C      Ceci implique dans la résolution de l'Equation de Transfert Radiatif
C      de faire des intégrations sur l'épaisseur optique entre deux niveaux
C      consécutifs. Ces intégrations font intervenir les fonctions sources
C      de diffusion (pour I, Q et U - en fonction du niveau d'épaisseur
C      optique et de la direction de propagation) pour les ordres en cours
C      de diffusion et de décomposition en séries de Fourier.
C      L'intégration entre deux niveaux est faite par linéarisation de la
C      fonction source sur la couche.
C
C      Les couches atmosphériques sont supposées homogènes.
C
C
C Description des paramètres
C -------------------------- 
C
C     NBMU (I4)    (E)  Nombre d'angles (positifs) effectivement utiles
C     RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double) (E) Tableau des mu de Gauss.
C     H(0:CTE_NT_ATM) (double) (E)  Epaisseur optique à chaque niveau.
C
C     I2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (E) 
C                                                  Fonction source pour I
C     Q2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (E) 
C                                                  Fonction source pour Q
C     U2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (E) 
C                                                  Fonction source pour U
C                          Vecteur fonction source en fonction du niveau du 
C                          profil et de la direction.
C    	
C     I1(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (S) Champ I.
C     Q1(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (S) Champ Q.
C     U1(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (S) Champ U.
C                          Champ de luminance  en fonction du niveau
C                          du profil et de la direction. 
C
C
C      Les conditions limites sont supposées affectées aux champs I1, Q1 et U1
C      au passage des arguments (par le programme appelant) :
C      		I1(CTE_NT_ATM,K>0) en surface  (resp. Q1 et U1)
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     Aucun
C
C==============================================================================
      SUBROUTINE OSOAA_INTEGR_EPOPT_ATM_UP(NBMU,RMU,H,I2,Q2,U2,
     &                                     I1,Q1,U1)
      
      IMPLICIT NONE
      

C* Définition des variables     
C*------------------------------------------------------------------

C* Directions de propagation :
      DOUBLE PRECISION RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      				!  Tableau des mu de Gauss.

C* Profil d'épaisseur optique :
      DOUBLE PRECISION H(0:CTE_NT_ATM)	!  Epaisseur optique pour chaque niveau.
     	
C* Vecteur fonction source en fonction du niveau du profil et de la direction.
      DOUBLE PRECISION 
     &       I2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      					!  Fonction source pour I.
      DOUBLE PRECISION 
     &       Q2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      					!  Fonction source pour Q.
      DOUBLE PRECISION 
     &       U2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      					!  Fonction source pour U.

C* Champ de luminance en fonction du niveau du profil et de la direction.       	
      DOUBLE PRECISION 
     &       I1(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	!  Champ I.
      DOUBLE PRECISION 
     &       Q1(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	!  Champ Q.
      DOUBLE PRECISION 
     &       U1(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	!  Champ U.

      DOUBLE PRECISION RMUK 	! Valeur K du tableau des mu de Gauss 

      DOUBLE PRECISION ZI1	! Valeur intermédiaire pour le calcul de I1
      				! à l'ordre IG.
      DOUBLE PRECISION ZQ1	! Valeur intermédiaire pour le calcul de Q1.
      DOUBLE PRECISION ZU1	! Valeur intermédiaire pour le calcul de U1.

      DOUBLE PRECISION DTAU   	! Epaisseur optique d'une couche.

      DOUBLE PRECISION ATT	! Atténuation entre deux niveaux consécutifs.

      DOUBLE PRECISION MATT	! 1 - att.

      DOUBLE PRECISION ATTDTAU	! att * dtau

      DOUBLE PRECISION A,B	! Coefficients d'interpolation linéaire

      INTEGER*4 I,JJ		! Indices de niveau.
      INTEGER*4 K		! Indice de direction.
      INTEGER*4 NBMU            ! Nombre d'angles effectifs des simulations
		
		
C------------------------------------------------------	
C* Intégration du champ de luminance montant (mu > 0) du sol au TOA.
C*
C* Intégration de proche en proche du sol au TOA par linéarisation
C* de la fonction source entre deux niveaux successifs.
C------------------------------------------------------
      DO 108 K=1,NBMU

         RMUK=RMU(K)

C* Initialisation par la valeur au sol.
         ZI1=I1(CTE_NT_ATM,K)
         ZQ1=Q1(CTE_NT_ATM,K)
         ZU1=U1(CTE_NT_ATM,K)
		
         DO 108 I=CTE_NT_ATM-1,0,-1
            JJ=I+1
            DTAU = H(JJ)-H(I)
            ATT = DEXP(-DTAU/RMUK)
            MATT = 1.D+00 - ATT
            ATTDTAU =  ATT * DTAU

            B = I2(I,K)
            A = (I2(JJ,K) - B) / DTAU
            ZI1 = ZI1*ATT + MATT*(A*RMUK+B) - A*ATTDTAU
            I1(I,K)=ZI1

            B = Q2(I,K)
            A = (Q2(JJ,K) - B) / DTAU
            ZQ1 = ZQ1*ATT + MATT*(A*RMUK+B) - A*ATTDTAU
            Q1(I,K)=ZQ1

            B = U2(I,K)
            A = (U2(JJ,K) - B) / DTAU
            ZU1 = ZU1*ATT + MATT*(A*RMUK+B) - A*ATTDTAU
            U1(I,K)=ZU1
108   CONTINUE        !Boucles sur I et K



      RETURN
      END  	!FIN DE LA PROCEDURE OSOAA_INTEGR_EPOPT_ATM_UP






C*===========================================================================
C PROCEDURE: OSOAA_INTEGR_EPOPT_ATM_DOWN
C ==========
C      Cette procédure réalise l'intégration sur l'épaisseur optique
C      du champ de rayonnement dans l'atmosphère:
C        - champ descendant : Intégration du TOA à la surface.
C
C      L'intégration est faite de proche en proche du niveau de départ
C      au niveau final.
C      Ceci implique dans la résolution de l'Equation de Transfert Radiatif
C      de faire des intégrations sur l'épaisseur optique entre deux niveaux
C      consécutifs. Ces intégrations font intervenir les fonctions sources
C      de diffusion (pour I, Q et U - en fonction du niveau d'épaisseur
C      optique et de la direction de propagation) pour les ordres en cours
C      de diffusion et de décomposition en séries de Fourier.
C      L'intégration entre deux niveaux est faite par linéarisation de la
C      fonction source sur la couche.
C
C      Les couches atmosphériques sont supposées homogènes.
C
C
C Description des paramètres
C -------------------------- 
C
C     NBMU (I4)    (E)  Nombre d'angles (positifs) effectivement utiles
C     RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double) (E) Tableau des mu de Gauss.
C     H(0:CTE_NT_ATM) (double) (E)  Epaisseur optique à chaque niveau.
C
C     I2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (E) 
C                                                  Fonction source pour I
C     Q2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (E) 
C                                                  Fonction source pour Q
C     U2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (E) 
C                                                  Fonction source pour U
C                          Vecteur fonction source en fonction du niveau du 
C                          profil et de la direction.
C    	
C     I1(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (S) Champ I.
C     Q1(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (S) Champ Q.
C     U1(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (S) Champ U.
C                          Champ de luminance  en fonction du niveau
C                          du profil et de la direction. 
C
C
C      Les conditions limites sont supposées affectées aux champs I1, Q1 et U1
C      au passage des arguments (par le programme appelant) :
C      		I1(0,K<0) = 0. au TOA          (resp. Q1 et U1)
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     Aucun
C
C==============================================================================
      SUBROUTINE OSOAA_INTEGR_EPOPT_ATM_DOWN(NBMU,RMU,H,I2,Q2,U2,
     &                                       I1,Q1,U1)
      
      IMPLICIT NONE
      

C* Définition des variables     
C*------------------------------------------------------------------

C* Directions de propagation :
      DOUBLE PRECISION RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      				!  Tableau des mu de Gauss.

C* Profil d'épaisseur optique :
      DOUBLE PRECISION H(0:CTE_NT_ATM)	!  Epaisseur optique pour chaque niveau.
     	
C* Vecteur fonction source en fonction du niveau du profil et de la direction.
      DOUBLE PRECISION 
     &       I2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      					!  Fonction source pour I.
      DOUBLE PRECISION 
     &       Q2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      					!  Fonction source pour Q.
      DOUBLE PRECISION 
     &       U2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      					!  Fonction source pour U.

C* Champ de luminance en fonction du niveau du profil et de la direction.       	
      DOUBLE PRECISION 
     &       I1(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	!  Champ I.
      DOUBLE PRECISION 
     &       Q1(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	!  Champ Q.
      DOUBLE PRECISION 
     &       U1(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	!  Champ U.

      DOUBLE PRECISION RMUK 	! Valeur K du tableau des mu de Gauss 

      DOUBLE PRECISION ZI1	! Valeur intermédiaire pour le calcul de I1
      				! à l'ordre IG.
      DOUBLE PRECISION ZQ1	! Valeur intermédiaire pour le calcul de Q1.
      DOUBLE PRECISION ZU1	! Valeur intermédiaire pour le calcul de U1.

      DOUBLE PRECISION DTAU   	! Epaisseur optique d'une couche.

      DOUBLE PRECISION ATT	! Atténuation entre deux niveaux consécutifs.

      DOUBLE PRECISION MATT	! 1 - att.

      DOUBLE PRECISION ATTDTAU	! att * dtau

      DOUBLE PRECISION A,B	! Coefficients d'interpolation linéaire

      INTEGER*4 I,JJ		! Indices de niveau.
      INTEGER*4 K		! Indice de direction.
      INTEGER*4 NBMU            ! Nombre d'angles effectifs des simulations
		


C------------------------------------------------------
C* Intégration du champ de luminance descendant (mu < 0) du TOA au sol
C*
C* Intégration de proche en proche du TOA au sol par linéarisation
C* de la fonction source entre deux niveaux successifs.
C* Initialisation par un champ diffus descendant nul au TOA.
C------------------------------------------------------
      DO 109 K=-NBMU,-1

         RMUK=RMU(K)

C* Initialisation par la valeur au TOA.
         ZI1=I1(0,K)
         ZQ1=Q1(0,K)
         ZU1=U1(0,K)

         DO 109 I=1,CTE_NT_ATM
            JJ=I-1
            DTAU = H(I)-H(JJ)
            ATT = DEXP(DTAU/RMUK)
            MATT = 1.D+00 - ATT
            ATTDTAU =  ATT * DTAU

            B = I2(I,K)
            A = (B - I2(JJ,K)) / DTAU
            ZI1 = ZI1*ATT + MATT*(A*RMUK+B) + A*ATTDTAU
            I1(I,K)=ZI1

            B = Q2(I,K)
            A = (B - Q2(JJ,K)) / DTAU
            ZQ1 = ZQ1*ATT + MATT*(A*RMUK+B) + A*ATTDTAU
            Q1(I,K)=ZQ1

            B = U2(I,K)
            A = (B- U2(JJ,K)) / DTAU
            ZU1 = ZU1*ATT + MATT*(A*RMUK+B) + A*ATTDTAU
            U1(I,K)=ZU1
  109 CONTINUE  !Boucles sur I et K

      RETURN
      END  	!FIN DE LA PROCEDURE OSOAA_INTEGR_EPOPT_ATM_DOWN





C*===========================================================================
C PROCEDURE: OSOAA_INTEGR_EPOPT_SEA_UP
C ==========
C      Cette procédure réalise l'intégration sur l'épaisseur optique
C      du champ de rayonnement dans la mer:
C        - champ montant    : Intégration du fond à la surface.
C
C      L'intégration est faite de proche en proche du niveau de départ
C      au niveau final.
C      Ceci implique dans la résolution de l'Equation de Transfert Radiatif
C      de faire des intégrations sur l'épaisseur optique entre deux niveaux
C      consécutifs. Ces intégrations font intervenir les fonctions sources
C      de diffusion (pour I, Q et U - en fonction du niveau d'épaisseur
C      optique et de la direction de propagation) pour les ordres en cours
C      de diffusion et de décomposition en séries de Fourier.
C      L'intégration entre deux niveaux est faite par linéarisation de la
C      fonction source sur la couche.
C
C      Les couches sont supposées homogènes.
C
C      Les conditions limites sont supposées affectées aux champs I1, Q1 et U1
C      au passage des arguments (par le programme appelant) :
C      		I1(CTE_NT_SEA,K>0) au fond  (resp. Q1 et U1)
C
C
C Description des paramètres
C -------------------------- 
C
C     NBMU (I4)    (E)  Nombre d'angles (positifs) effectivement utiles
C     RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double) (E) Tableau des mu de Gauss.
C     H(0:CTE_NT_SEA) (double) (E)  Epaisseur optique à chaque niveau.
C
C     I2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (E) 
C                                                  Fonction source pour I
C     Q2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (E) 
C                                                  Fonction source pour Q
C     U2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (E) 
C                                                  Fonction source pour U
C                          Vecteur fonction source en fonction du niveau du 
C                          profil et de la direction.
C    	
C     I1(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (S) Champ I.
C     Q1(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (S) Champ Q.
C     U1(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (S) Champ U.
C                          Champ de luminance  en fonction du niveau
C                          du profil et de la direction. 
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     Aucun
C
C==============================================================================
      SUBROUTINE OSOAA_INTEGR_EPOPT_SEA_UP(NBMU,RMU,H,I2,Q2,U2,
     &        				   I1,Q1,U1)
      
      IMPLICIT NONE
      

C* Définition des variables     
C*------------------------------------------------------------------

C* Directions de propagation :
      DOUBLE PRECISION RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      				!  Tableau des mu de Gauss.

C* Profil d'épaisseur optique :
      DOUBLE PRECISION H(0:CTE_NT_SEA)	!  Epaisseur optique pour chaque niveau.
     	
C* Vecteur fonction source en fonction du niveau du profil et de la direction.
      DOUBLE PRECISION 
     &       I2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      					!  Fonction source pour I.
      DOUBLE PRECISION 
     &       Q2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      					!  Fonction source pour Q.
      DOUBLE PRECISION 
     &       U2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      					!  Fonction source pour U.

C* Champ de luminance en fonction du niveau du profil et de la direction.       	
      DOUBLE PRECISION 
     &       I1(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	!  Champ I.
      DOUBLE PRECISION
     &       Q1(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	!  Champ Q.
      DOUBLE PRECISION 
     &       U1(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	!  Champ U.

      DOUBLE PRECISION RMUK 	! Valeur K du tableau des mu de Gauss 

      DOUBLE PRECISION ZI1	! Valeur intermédiaire pour le calcul de I1
      				! à l'ordre IG.
      DOUBLE PRECISION ZQ1	! Valeur intermédiaire pour le calcul de Q1.
      DOUBLE PRECISION ZU1	! Valeur intermédiaire pour le calcul de U1.

      DOUBLE PRECISION DTAU   	! Epaisseur optique d'une couche.

      DOUBLE PRECISION ATT	! Atténuation entre deux niveaux consécutifs.

      DOUBLE PRECISION MATT	! 1 - att.

      DOUBLE PRECISION ATTDTAU	! att * dtau

      DOUBLE PRECISION A,B	! Coefficients d'interpolation linéaire

      INTEGER*4 I,JJ		! Indices de niveau.
      INTEGER*4 K		! Indice de direction.
      INTEGER*4 NBMU            ! Nombre d'angles effectifs des simulations
		
C------------------------------------------------------	
C* Intégration du champ de luminance montant (mu > 0) du fond à la surface.
C*
C* Intégration de proche en proche du fond à la surface par linéarisation
C* de la fonction source entre deux niveaux successifs.
C------------------------------------------------------
      DO 108 K=1,NBMU

         RMUK=RMU(K)

C* Initialisation par la valeur au fond de mer.
         ZI1=I1(CTE_NT_SEA,K)
         ZQ1=Q1(CTE_NT_SEA,K)
         ZU1=U1(CTE_NT_SEA,K)
		
         DO 108 I=CTE_NT_SEA-1,0,-1
            JJ=I+1
            DTAU = H(JJ)-H(I)
            ATT = DEXP(-DTAU/RMUK)
            MATT = 1.D+00 - ATT
            ATTDTAU =  ATT * DTAU

            B = I2(I,K)
            A = (I2(JJ,K) - B) / DTAU
            ZI1 = ZI1*ATT + MATT*(A*RMUK+B) - A*ATTDTAU
            I1(I,K)=ZI1

            B = Q2(I,K)
            A = (Q2(JJ,K) - B) / DTAU
            ZQ1 = ZQ1*ATT + MATT*(A*RMUK+B) - A*ATTDTAU
            Q1(I,K)=ZQ1

            B = U2(I,K)
            A = (U2(JJ,K) - B) / DTAU
            ZU1 = ZU1*ATT + MATT*(A*RMUK+B) - A*ATTDTAU
            U1(I,K)=ZU1
108   CONTINUE        !Boucles sur I et K


      RETURN
      END  	!FIN DE LA PROCEDURE OSOAA_INTEGR_EPOPT_SEA_UP






C*===========================================================================
C PROCEDURE: OSOAA_INTEGR_EPOPT_SEA_DOWN
C ==========
C      Cette procédure réalise l'intégration sur l'épaisseur optique
C      du champ de rayonnement dans la mer:
C        - champ descendant : Intégration de la surface au fond.
C
C      L'intégration est faite de proche en proche du niveau de départ
C      au niveau final.
C      Ceci implique dans la résolution de l'Equation de Transfert Radiatif
C      de faire des intégrations sur l'épaisseur optique entre deux niveaux
C      consécutifs. Ces intégrations font intervenir les fonctions sources
C      de diffusion (pour I, Q et U - en fonction du niveau d'épaisseur
C      optique et de la direction de propagation) pour les ordres en cours
C      de diffusion et de décomposition en séries de Fourier.
C      L'intégration entre deux niveaux est faite par linéarisation de la
C      fonction source sur la couche.
C
C      Les couches sont supposées homogènes.
C
C      Les conditions limites sont supposées affectées aux champs I1, Q1 et U1
C      au passage des arguments (par le programme appelant) :
C      		I1(0,K<0) en surface	    (resp. Q1 et U1)
C
C Description des paramètres
C -------------------------- 
C
C     NBMU (I4)    (E)  Nombre d'angles (positifs) effectivement utiles
C     RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double) (E) Tableau des mu de Gauss.
C     H(0:CTE_NT_SEA) (double) (E)  Epaisseur optique à chaque niveau.
C
C     I2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (E) 
C                                                  Fonction source pour I
C     Q2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (E) 
C                                                  Fonction source pour Q
C     U2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (E) 
C                                                  Fonction source pour U
C                          Vecteur fonction source en fonction du niveau du 
C                          profil et de la direction.
C    	
C     I1(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (S) Champ I.
C     Q1(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (S) Champ Q.
C     U1(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (S) Champ U.
C                          Champ de luminance  en fonction du niveau
C                          du profil et de la direction. 
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     Aucun
C
C==============================================================================
      SUBROUTINE OSOAA_INTEGR_EPOPT_SEA_DOWN(NBMU,RMU,H,I2,Q2,U2,
     &                                       I1,Q1,U1)
      
      IMPLICIT NONE
      

C* Définition des variables     
C*------------------------------------------------------------------

C* Directions de propagation :
      DOUBLE PRECISION RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      				!  Tableau des mu de Gauss.

C* Profil d'épaisseur optique :
      DOUBLE PRECISION H(0:CTE_NT_SEA)	!  Epaisseur optique pour chaque niveau.
     	
C* Vecteur fonction source en fonction du niveau du profil et de la direction.
      DOUBLE PRECISION 
     &       I2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      					!  Fonction source pour I.
      DOUBLE PRECISION 
     &       Q2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      					!  Fonction source pour Q.
      DOUBLE PRECISION 
     &       U2(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      					!  Fonction source pour U.

C* Champ de luminance en fonction du niveau du profil et de la direction.       	
      DOUBLE PRECISION 
     &       I1(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	!  Champ I.
      DOUBLE PRECISION 
     &       Q1(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	!  Champ Q.
      DOUBLE PRECISION 
     &       U1(0:CTE_NT_SEA,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	!  Champ U.

      DOUBLE PRECISION RMUK 	! Valeur K du tableau des mu de Gauss 

      DOUBLE PRECISION ZI1	! Valeur intermédiaire pour le calcul de I1
      				! à l'ordre IG.
      DOUBLE PRECISION ZQ1	! Valeur intermédiaire pour le calcul de Q1.
      DOUBLE PRECISION ZU1	! Valeur intermédiaire pour le calcul de U1.

      DOUBLE PRECISION DTAU   	! Epaisseur optique d'une couche.

      DOUBLE PRECISION ATT	! Atténuation entre deux niveaux consécutifs.

      DOUBLE PRECISION MATT	! 1 - att.

      DOUBLE PRECISION ATTDTAU	! att * dtau

      DOUBLE PRECISION A,B	! Coefficients d'interpolation linéaire

      INTEGER*4 I,JJ		! Indices de niveau.
      INTEGER*4 K		! Indice de direction.
      INTEGER*4 NBMU            ! Nombre d'angles effectifs des simulations
		


C------------------------------------------------------
C* Intégration du champ de luminance descendant (mu < 0) de la surface au fond
C*
C* Intégration de proche en proche de la surface au fond par linéarisation
C* de la fonction source entre deux niveaux successifs.
C* Initialisation par le champ diffus descendant en surface (niveau 0-).
C------------------------------------------------------
      DO 109 K=-NBMU,-1

         RMUK=RMU(K)

C* Initialisation par la valeur en surface.
	 ZI1=I1(0,K)
         ZQ1=Q1(0,K)
         ZU1=U1(0,K)

         DO 109 I=1,CTE_NT_SEA
            JJ=I-1
            DTAU = H(I)-H(JJ)
            ATT = DEXP(DTAU/RMUK)
            MATT = 1.D+00 - ATT
            ATTDTAU =  ATT * DTAU

            B = I2(I,K)
            A = (B - I2(JJ,K)) / DTAU
            ZI1 = ZI1*ATT + MATT*(A*RMUK+B) + A*ATTDTAU
            I1(I,K)=ZI1

            B = Q2(I,K)
            A = (B - Q2(JJ,K)) / DTAU
            ZQ1 = ZQ1*ATT + MATT*(A*RMUK+B) + A*ATTDTAU
            Q1(I,K)=ZQ1

            B = U2(I,K)
            A = (B- U2(JJ,K)) / DTAU
            ZU1 = ZU1*ATT + MATT*(A*RMUK+B) + A*ATTDTAU
            U1(I,K)=ZU1
  109 CONTINUE  !Boucles sur I et K

      RETURN
      END  	!FIN DE LA PROCEDURE OSOAA_INTEGR_EPOPT_SEA_DOWN





       

C*===========================================================================
C PROCEDURE: OSOAA_PARAM_CONV
C ==========
C      Cette procédure calcule le paramètre de convergence pour le test
C      de convergence en série géométrique. 
C
C      Principe:
C      --------
C      On analyse la convergence en série géométrique des diffusions 
C      successives à partir des ordres IG-2, IG-1 et IG.
C      Le test est réalisé :
C      - soit, pour le champ de luminance émergeant au TOA et pour
C        le champ éclairant le fond. 
C      - soit, pour le champ de luminance montant et descendant 
C        au niveau de la surface.
C      - soit, pour le champ de luminance montant et descendant 
C        juste sous la surface.
C 
C      S'il y a convergence à ces limites, on admet
C      qu'il y a convergence dans toute l'atmosphère et l'océan.
C
C      Supposons avoir un comportement de série géométrique convergente à 
C      partir de l'ordre IG-2 de diffusion.
C      La raison de la série est estimée entre les termes IG-2 et IG-1, ainsi
C      qu'entre les termes IG-1 et IG :
C 		_ q  = D1 / A1  (rapport des ordres IG-1 / IG-2 pour I)
C		_ q' = G1 / D1  (rapport des ordres IG / IG-1 pour I)
C     On teste que q et q' sont voisins (condition pour une approximation de
C     série géométrique).
C     On teste en même temps le rapport de la somme des termes de la série
C     géométrique convergente à partir de l'ordre IG (ordre IG / (1-q) )
C     devant la somme des termes précédents
C     (de 1 à IG-1, stockée dans I3, Q3 ou U3).
C
C     La routine sort le paramètre de convergence Z1 qui est testé par le
C     programme appelant.
C
C
C Description des paramètres
C -------------------------- 
C
C    NBMU (I4)    (E)  Nombre d'angles (positifs) effectivement utiles
C
C    I3(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)		
C                         Paramètre de Stokes I pour le cumul des
C      		          ordres de diffusion précédant IG en cours.
C    Q3(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)  Paramètre de Stokes Q.
C    U3(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)  Paramètre de Stokes U.
C            			
C    A1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)
C       	          Paramètre de Stokes I pour l'ordre IG-2. 
C    B1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)
C      			  Paramètre de Stokes Q pour l'ordre IG-2.
C    C1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)
C      			  Paramètre de Stokes U pour l'ordre IG-2.
C
C    D1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)
C      			  Paramètre de Stokes I pour l'ordre IG-1.	
C    E1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)
C      			  Paramètre de Stokes Q pour l'ordre IG-1.
C    F1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)
C      			  Paramètre de Stokes U pour l'ordre IG-1.
C
C    G1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)
C      		          Paramètre de Stokes I pour l'ordre IG.	
C    H1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)
C      			  Paramètre de Stokes Q pour l'ordre IG.
C    P1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)
C      			  Paramètre de Stokes U pour l'ordre IG.
C
C    Z1	   (double)   (S)    Paramètre de convergence.
C
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     Aucun
C
C==============================================================================
      SUBROUTINE OSOAA_PARAM_CONV(NBMU,A1,B1,C1,D1,E1,F1,G1,H1,P1,
     &                            I3,Q3,U3,Z1)
      
      IMPLICIT NONE
      

C* Définition des variables     
C*------------------------------------------------------------------

      DOUBLE PRECISION Z1	! Paramètre de convergence.

	! Fonctions pour le test de convergence en séries géométriques,
	! champ émergeant au TOA (mu > 0) et descendant au sol (mu < 0).
	! Les fonctions sont établies pour un ordre S du développement
	! en séries de Fourier, en fonction de l'angle mu.		
      DOUBLE PRECISION I3(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)		
                                ! Paramètre de Stokes I pour le cumul des
      				! ordres de diffusion précédant IG en cours.
      DOUBLE PRECISION Q3(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
      				! Paramètre de Stokes Q.
      DOUBLE PRECISION U3(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
      				! Paramètre de Stokes U.
            			
      DOUBLE PRECISION A1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
       				! Paramètre de Stokes I pour l'ordre IG-2. 
      DOUBLE PRECISION B1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes Q pour l'ordre IG-2.
      DOUBLE PRECISION C1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes U pour l'ordre IG-2.

      DOUBLE PRECISION D1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes I pour l'ordre IG-1.
      DOUBLE PRECISION E1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes Q pour l'ordre IG-1.
      DOUBLE PRECISION F1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      			  	! Paramètre de Stokes U pour l'ordre IG-1.

      DOUBLE PRECISION G1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes I pour l'ordre IG.
      DOUBLE PRECISION H1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes Q pour l'ordre IG.
      DOUBLE PRECISION P1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      			  	! Paramètre de Stokes U pour l'ordre IG.

      DOUBLE PRECISION Y	! Variable pour les tests d'arrêt.

      INTEGER*4 K
      INTEGER*4 NBMU    ! Nombre d'angles effectifs des simulations


C* Détermination de la valeur maximale de Z1 pour tous les angles mu.
C----------------------------------------------------------------------------
      Z1=0.D+00

      DO 99 K=-NBMU,NBMU
         IF (K) 56,99,56
   56    IF (A1(K).EQ.0.D+00) GO TO 51
         IF (D1(K).EQ.0.D+00) GO TO 51
         IF (I3(K).EQ.0.D+00) GO TO 51
         Y=((G1(K)/D1(K)-D1(K)/A1(K))
     &       /((1-G1(K)/D1(K))**2)*(G1(K)/I3(K)))
         Z1=DMAX1(Z1,DABS(Y))
	 
   51    CONTINUE
   
         IF (B1(K).EQ.0.D+00) GO TO 53
         IF (E1(K).EQ.0.D+00) GO TO 53
         IF (Q3(K).EQ.0.D+00) GO TO 53
         Y=((H1(K)/E1(K)-E1(K)/B1(K))
     &       /((1-H1(K)/E1(K))**2)*(H1(K)/Q3(K)))
         Z1=DMAX1(Z1,DABS(Y))
	 
   53    CONTINUE
   
         IF (C1(K).EQ.0.D+00) GO TO 55
         IF (F1(K).EQ.0.D+00) GO TO 55
         IF (U3(K).EQ.0.D+00) GO TO 55
         Y=((P1(K)/F1(K)-F1(K)/C1(K))
     &       /((1-P1(K)/F1(K))**2)*(P1(K)/U3(K)))
         Z1=DMAX1(Z1,DABS(Y))
	 
   55    CONTINUE
   
   99 CONTINUE

      RETURN
      END  	!FIN DE LA PROCEDURE OSOAA_PARAM_CONV





C*============================================================================
C PROCEDURE: OSOAA_ARRET_DIFFUS
C ==========
C      Cette procédure prépare le test d'arrêt sur les diffusions quand les 
c      termes suivant deviennent très petits
C
C
C Description des paramètres
C -------------------------- 
C    
C    NBMU (I4) (E)  Nombre d'angles (positifs) effectivement utiles
C
C    I1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double)  (E)
C    Q1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double)  (E)
C    U1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double)  (E)
C      	 Champ de luminance d'ordre IG en fonction de la direction
C        (pour les niveaux présélectionnés)     	
C
C    I3(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)	
C            Paramètre de Stokes I pour le cumul des ordres de diffusion 
C            précédant IG en cours, en fonction de la direction
C            (pour les niveaux présélectionnés)
C    Q3(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double)  (E) Paramètre de Stokes Q.
C    U3(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double)  (E) Paramètre de Stokes U
C
C    Z1	   (double)   (S)    Variable seuil pour les tests d'arrêt.
C
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     Aucun
C
C==============================================================================
      SUBROUTINE OSOAA_ARRET_DIFFUS(NBMU,I1,Q1,U1,I3,Q3,U3,Z1)
      
      IMPLICIT NONE
      

C* Définition des variables     
C*------------------------------------------------------------------

      DOUBLE PRECISION Z1	! Variable seuil pour les tests d'arrêt.

      	! Champ de luminance d'ordre IG en fonction du niveau du profil
	! et de la direction.       	
      DOUBLE PRECISION I1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ I.
      DOUBLE PRECISION Q1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ Q.
      DOUBLE PRECISION U1(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	! Champ U.

	! Fonctions pour le test de convergence en séries géométrique,
	! champ montant (mu > 0) et descendant (mu < 0).
	! Les fonctions sont établies pour un ordre S du développement
	! en séries de Fourier, en fonction de l'angle mu.	
      DOUBLE PRECISION I3(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)		
                                ! Paramètre de Stokes I pour le cumul des
      				! ordres de diffusion précédant IG en cours.
      DOUBLE PRECISION Q3(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
      				! Paramètre de Stokes Q.
      DOUBLE PRECISION U3(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
      				! Paramètre de Stokes U.
 
      DOUBLE PRECISION Y	! Variable pour les tests d'arrêt.
     
      INTEGER*4 K 
      INTEGER*4 NBMU    ! Nombre d'angles effectifs des simulations


C--------------------------------------------------------------------------- 
C* On compare la valeur du terme d'ordre IG à la somme des termes précédents.
C* Détermination de la valeur maximale de leur rapport
C* pour le champ montant au TOA et descendant au sol.
C---------------------------------------------------------------------------
      Z1=0.D+00
      DO 88 K=-NBMU,NBMU
  
         IF (I3(K).EQ.0.D+00) GO TO 92
  
         Y=I1(K)/I3(K)
         Z1=DMAX1(Z1,DABS(Y))
	 
   92    IF (Q3(K).EQ.0.D+00) GO TO 94
   
         Y=Q1(K)/Q3(K)
         Z1=DMAX1(Z1,DABS(Y))
	 
   94    IF (U3(K).EQ.0.D+00) GO TO 88
   
         Y=U1(K)/U3(K)
         Z1=DMAX1(Z1,DABS(Y))
   88 CONTINUE
   
      RETURN
      END  	!FIN DE LA PROCEDURE OSOAA_ARRET_DIFFUS





C*========================================================================
C PROCEDURE: OSOAA_AJOUT_QUEUE
C ==========
C      Cette procédure réalise, si on est en régime de convergence, l'ajout 
C      de la queue de la série géométrique à la somme des diffusions
C      précédentes, ceci pour toutes les directions et tous les niveaux.
C
C
C Description des paramètres
C -------------------------- 
C
C    NBMU (I4) (E)  Nombre d'angles (positifs) effectivement utiles
C
C    D1(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) 
C                            Paramètre de Stokes I pour l'ordre IG-1.	
C    E1(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)
C      		             Paramètre de Stokes Q pour l'ordre IG-1.
C    F1(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)
C      			     Paramètre de Stokes U pour l'ordre IG-1.
C
C    G1(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)
C      			     Paramètre de Stokes I pour l'ordre IG.	
C    H1(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)
C      			     Paramètre de Stokes Q pour l'ordre IG.
C    P1(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)
C      			     Paramètre de Stokes U pour l'ordre IG.
C
C
C    I3(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E/S)	
C            Paramètre de Stokes I pour le cumul des ordres de diffusion 
C            précédant l'ordre IG en cours.
C    Q3(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double)  (E/S) Paramètre de Stokes Q.
C    U3(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double)  (E/S) Paramètre de Stokes U
C
C
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     Aucun
C
C==============================================================================
      SUBROUTINE OSOAA_AJOUT_QUEUE(NBMU,D1,E1,F1,G1,H1,P1,I3,Q3,U3)
      
      IMPLICIT NONE


C Constantes OSOAA_AJOUT_QUEUE
C ----------------------------  
      INTEGER*4 NT_TOT		! Nombre total de couches (atmosphère + mer)      
      PARAMETER(NT_TOT=CTE_NT_ATM+CTE_NT_SEA+1)		
   
   
   
C* Définition des variables     
C*------------------------------------------------------------------

	! Cumul précédent des ordres d'interaction.
	! Les fonctions sont établies pour un ordre S du développement
	! en séries de Fourier, en fonction de l'angle mu.	
      DOUBLE PRECISION I3(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)		
                                ! Paramètre de Stokes I pour le cumul des
      				! ordres de diffusion précédant IG en cours.
      DOUBLE PRECISION Q3(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
      				! Paramètre de Stokes Q.
      DOUBLE PRECISION U3(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
      				! Paramètre de Stokes U
				
      DOUBLE PRECISION D1(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes I pour l'ordre IG-1.
      DOUBLE PRECISION E1(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes Q pour l'ordre IG-1.
      DOUBLE PRECISION F1(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      			  	! Paramètre de Stokes U pour l'ordre IG-1.

      DOUBLE PRECISION G1(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes I pour l'ordre IG.
      DOUBLE PRECISION H1(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      				! Paramètre de Stokes Q pour l'ordre IG.
      DOUBLE PRECISION P1(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      			  	! Paramètre de Stokes U pour l'ordre IG.


      DOUBLE PRECISION I1QUEUE	
      		! Valeur estimée de la queue de série géométrique pour I.
      DOUBLE PRECISION Q1QUEUE
      		! Valeur estimée de la queue de série géométrique pour Q.
      DOUBLE PRECISION U1QUEUE
      		! Valeur estimée de la queue de série géométrique pour U.

      INTEGER*4 J
      INTEGER*4 K
      INTEGER*4 NBMU    ! Nombre d'angles effectifs des simulations


C* Pour le champ émergeant au TOA et descendant au sol
C------------------------------------------------------

      DO 600 K=0,NT_TOT
      
         DO 610 J=-NBMU,NBMU
      
            IF (J.EQ.0) GOTO 610
	
            IF (D1(K,J).EQ.0.) THEN
               I1QUEUE = 0.
            ELSE	
               I1QUEUE = G1(K,J) / ( 1-G1(K,J)/D1(K,J) )
            ENDIF
	 	
            IF (E1(K,J).EQ.0.) THEN
               Q1QUEUE = 0.
            ELSE	
               Q1QUEUE = H1(K,J) / ( 1-H1(K,J)/E1(K,J) )
            ENDIF	
	
            IF (F1(K,J).EQ.0.) THEN
               U1QUEUE = 0.
            ELSE	
               U1QUEUE = P1(K,J) / ( 1-P1(K,J)/F1(K,J) )
            ENDIF	

            I3(K,J)=I3(K,J)+I1QUEUE
            Q3(K,J)=Q3(K,J)+Q1QUEUE
            U3(K,J)=U3(K,J)+U1QUEUE
	
  610    CONTINUE
  600 CONTINUE
      
      RETURN
      END  	!FIN DE LA PROCEDURE OSOAA_AJOUT_QUEUE
      
      




C*============================================================================
C PROCEDURE: OSOAA_ARRET_FOURIER
C ==========
C      Cette procédure prépare le test d'arrêt sur la décomposition en 
c      séries de Fourier.
c
C
C Description des paramètres
C -------------------------- 
C
C    NBMU (I4) (E)  Nombre d'angles (positifs) effectivement utiles
C
C      mu > 0 ==> Champ émergeant au TOA
C      mu < 0 ==> Champ descendant au fond de mer
C
C    I4(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double)  (E) 	
C    Q4(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double)  (E) 
C    U4(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double)  (E) 
C        Tableaux de sommation des ordres S de la décomposition
C        en séries de Fourier, en fonction de l'angle mu.
C
C    I5(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double)  (E) 	
C    Q5(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double)  (E) 
C    U5(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double)  (E) 	
C        Tableaux de sommation des ordres S de la décomposition
C        en séries de Fourier, avec permutation du signe des ordres S,
C	 en fonction de l'angle mu.
C
C    I3(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E)	
C            Paramètre de Stokes I pour le cumul des ordres de diffusion 
C            précédant IG en cours.
C    Q3(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double)  (E) Paramètre de Stokes Q.
C    U3(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double)  (E) Paramètre de Stokes U
C
C    Z1	   (double)   (S)    Variable seuil pour les tests d'arrêt.
C
C
C   
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     Aucun
C
C==============================================================================
      SUBROUTINE OSOAA_ARRET_FOURIER(NBMU,I3,Q3,U3,I4,Q4,U4,I5,Q5,U5,Z1)
      
      IMPLICIT NONE
      

C* Définition des variables     
C*------------------------------------------------------------------

      DOUBLE PRECISION Z1	! Variable seuil pour les tests d'arrêt.

	! Tableaux de sommation des ordres S de la décomposition
        ! en séries de Fourier, en fonction de l'angle mu.
      DOUBLE PRECISION I4(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      DOUBLE PRECISION Q4(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      DOUBLE PRECISION U4(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)

        ! Tableaux de sommation des ordres S de la décomposition
        ! en séries de Fourier, avec permutation du signe des ordres S,
	! en fonction de l'angle mu.
      DOUBLE PRECISION I5(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      DOUBLE PRECISION Q5(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
      DOUBLE PRECISION U5(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	

	! Champ émergeant au TOA (mu > 0) et descendant au sol (mu < 0).
	! Les fonctions sont établies pour un ordre S du développement
	! en séries de Fourier, en fonction de l'angle mu.
      DOUBLE PRECISION I3(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)		
                                ! Paramètre de Stokes I pour le cumul des
      				! ordres de diffusion précédant IG en cours.
      DOUBLE PRECISION Q3(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
      				! Paramètre de Stokes Q.
      DOUBLE PRECISION U3(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
      				! Paramètre de Stokes U.
 

      DOUBLE PRECISION Y	! Variable pour les tests d'arrêt.
      
      INTEGER*4 J
      INTEGER*4 NBMU    ! Nombre d'angles effectifs des simulations


C------------------------------------------------------------------------- 
C* On teste l'arrêt de la décomposition en séries de Fourier par
C* comparaison de l'ordre IS (pour chaque direction mu) par rapport
C* à la somme des contributions précédentes.
C* On sort la valeur maximale Z1 de ce rapport pour le champ emergant 
C* au TOA et descendant au niveau du sol.
C* Si la contribution relative (en valeur absolue) est inférieure
C* au seuil SOS_PH_SEUIL_SF on arrête la décomposition en séries de Fourier
C* dans le programme appelant.
C----------------------------------------------------------------------------
      Z1=0.D+00
      DO 169 J=-NBMU,NBMU
         IF (J.EQ.0) GOTO 169

         IF (Q4(J).EQ.0.D+00) GO TO 71
	 
         Y=DABS(Q3(J)/Q4(J))
         Z1=DMAX1(Z1,Y)
	 
   71    IF (I4(J).EQ.0.D+00) GO TO 73
   
         Y=DABS(I3(J)/I4(J))
         Z1=DMAX1(Z1,Y)
	 
   73    IF (U4(J).EQ.0.D+00) GO TO 74
   
         Y=DABS(U3(J)/U4(J))
         Z1=DMAX1(Z1,Y)

   74    IF (Q5(J).EQ.0.D+00) GO TO 771
   
         Y=DABS(Q3(J)/Q5(J))
         Z1=DMAX1(Z1,Y)
	 
  771    IF (U5(J).EQ.0.D+00) GO TO 772
  
         Y=DABS(U3(J)/U5(J))
         Z1=DMAX1(Z1,Y)
	 
  772    IF (I5(J).EQ.0.D+00) GO TO 169
  
         Y=DABS(I3(J)/I5(J))
         Z1=DMAX1(Z1,Y)

  169 CONTINUE

      RETURN
      END  	!FIN DE LA PROCEDURE OSOAA_ARRET_FOURIER
      
      
      
      
      
      


C*============================================================================
C PROCEDURE: OSOAA_INTERF_MERPLATE
C ==========
C
C      Pour un champ de LUMINANCE incident, cette routine délivre
C      le champ de luminance réfléchi ou transmis par une mer plane.
C
C      Cas d'une réflexion : l'angle de réflexion correspond à l'angle d'incidence.
C
C      Cas d'une transmission : l'angle de transmission est calculé selon la loi
C                               de Snell-Descartes, puis le champ de luminance 
C                               est interpolé aux angles du fichier des angles 
C                               de propagation (angles de Gauss et complémentaires).
C
C
C
C      L'indice de réfraction relatif du dioptre, entre le milieu de 
C      réflexion (N1) et le milieu de transmission (N2), est N = N1/N2.
C			
C      La matrice de Fresnel, exprimée pour un repère lié au plan de réflexion
C      s'écrit pour une réflexion comme pour une transmission :
C                   | F11(i)  F12(i)    0    |
C         F(i)= C * | F12(i)  F11(i)    0    |
C                   |  0       0      F33(i) |
C
C       * Avec pour le cas d'une réflexion :
C                                ---------
C           _ C = 1
C           _ i : angle de réflexion par rapport à la normale au dioptre,
C           _ F11(i) = 0.5 * (RL*RL + RR*RR),
C           _ F12(i) = 0.5 * (RL*RL - RR*RR),
C           _ F33(i) = RL*RR,
C
C         On calcule RL et RR en fonction de N et i, par :
C           _ RL : le coefficient de réflexion parallèle
C
C               RL(i) = [ cos(i) - N*cos(t) ] / [ cos(i) + N*cos(t) ]
C
C           _ RR : le coefficient de réflexion perpendiculaire
C
C               RR(i) = [ N*cos(i) - cos(t) ] / [ N*cos(i) + cos(t) ]
C
C         avec : t l'angle de réfraction tel que N*sin(i) = sin(t)
C
C
C       * Avec pour le cas d'une transmission :
C                                ------------
C           _ C = [ cos(t) ] /  [ N**3 * cos(i) ] 
C           _ i : angle de réflexion par rapport à la normale au dioptre,
C           _ F11(i) = 0.5 * (TL*TL + TR*TR),
C           _ F12(i) = 0.5 * (TL*TL - TR*TR),
C           _ F33(i) = TL*TR, 
C       
C         On calcule TL et TR  par :
C           _ TL : le coefficient de transmission parallèle
C
C               TL(i) = N * [ 1 + RL(i) ]
C
C           _ TR : le coefficient de transmission perpendiculaire
C
C               TR(i) = 1 + RR(i)
C
C
C       * Dans le cas d'un dépassement de l'angle limite d'incidence 
C         (incidence donnant une transmission à 90°) 
C          ==> La transmission est nulle et la réflexion est totale
C
C              	  Cas d'une transmission	| 	Cas d'une réflexion
C   		      F11(i) = 0.0		|    	   F11(i) = 1.0
C                     F12(i) = 0.0		|          F12(i) = 0.0
C                     F33(i) = 0.0		|          F33(i) = 1.0
C
C
C
C
C	Pour le cas d'une transmission, la matrice F(i) est N*N*Tflux 
C      (avec Tflux la matrice de transmission en flux)
C
C       ==> Applicable pour le calcul de luminances transmises
C
C       ==> Mais non applicable directement pour le calcul de transmission
C           d'éclairement solaire.
C
C
C
C Description des paramètres
C -------------------------- 
C    
C       CINTER (CHAR*3) (E) : Chaine de 3 caractères précisant le type d'interface
C			      à simuler :
C   				une réflexion air -> air : "RAA"  
C			  	une réflexion mer -> mer : "RWW"
C				une transmission air -> mer : "TAW"
C				une transmission mer -> air : "TWA"
C
C       IND   (double) (E)  : Indice de réfraction de l'eau / air.
C
C	RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (DOUBLE) (E) : Cosinus des angles.
C	
C       NBMU (I4) (E) : Nombre d'angles (positifs) effectivement utiles
C
C       I_INC(1:CTE_OS_NBMU_MAX) (double) (E)  
C       Q_INC(1:CTE_OS_NBMU_MAX) (double) (E)
C       U_INC(1:CTE_OS_NBMU_MAX) (double) (E) :
C			Valeur des champs I, Q et U incidents 
C			- sous la surface : champ Up au niveau 0-
C 			- en surface : champ Down au niveau 0+
C
C      I_DEV_INTERPO(1:CTE_OS_NBMU_MAX) (double) (S) 
C      Q_DEV_INTERPO(1:CTE_OS_NBMU_MAX) (double) (S) 
C      U_DEV_INTERPO(1:CTE_OS_NBMU_MAX) (double) (S) 
C			Valeur des champs I, Q et U déviés, 
C                       les angles de transmission étant les angles du tableau RMU
C                       (i.e. du fichier des angles de propagation)
C			- en surface : champ Up au niveau 0+
C			- sous la surface : champ Down au niveau 0-
C
C       TRACE  (LOGICAL)  (E)  ! = vrai si écriture dans le fichier trace.
C       IER  (I4)   (S) : code d'erreur =0 si pas d'erreur, =-1 sinon
C
C   
C Common utilisé:
C --------------
C     Aucun
C
C
C Cas d'erreur :  
C ------------
C     - Erreur de définition du type d'interface
C     - Erreur d'écriture dans le fichier Trace
C     - Erreur dans le sous-programme appelé (SOS_INTERPO_SPLINT)
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant
C
C
C  Sous programmes utilisés: 
C --------------------------
C  Ce programme fait appel à la routine: SOS_INTERPO_SPLINT
C  
C
C==============================================================================
      SUBROUTINE OSOAA_INTERF_MERPLATE(CINTER,IND,RMU,NBMU,
     & 				I_INC,Q_INC,U_INC,
     &				       I_DEV_INTERPO,Q_DEV_INTERPO,
     &				       U_DEV_INTERPO, TRACE, IER)
      
      IMPLICIT NONE
      
C Constantes 
C -----------------            
      INTEGER*2 IDLOG	! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_SOS)
      
      
C* Définition des variables     
C*------------------------------------------------------------------
C* Directions de propagation :
      DOUBLE PRECISION RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      				!  (E) Tableau des mu de Gauss.

      DOUBLE PRECISION RMUT(1:CTE_OS_NBMU_MAX)	! Tableau des mu transmis.
				
      DOUBLE PRECISION IND	! (E) Indice de réfraction de l'eau / air

      DOUBLE PRECISION I_INC(1:CTE_OS_NBMU_MAX)	! (E) Valeur du champ I incident
      						!     sous l'eau : champ Up au niveau 0-
						!     en surface : champ Down au niveau 0+
      DOUBLE PRECISION Q_INC(1:CTE_OS_NBMU_MAX)	! (E) Valeur du champ Q incident
      DOUBLE PRECISION U_INC(1:CTE_OS_NBMU_MAX)	! (E) Valeur du champ U incident

      DOUBLE PRECISION I_DEV(1:CTE_OS_NBMU_MAX)	
      				! Valeur du champ I dévié (transmis ou réfléchi) 
      				! pour les angles correspondant aux angles de déviation.
				!   - en surface : champ Up au niveau 0+
      				!   - sous l'eau : champ Down au niveau 0-
      DOUBLE PRECISION Q_DEV(1:CTE_OS_NBMU_MAX)	! Valeur du champ Q dévié
      DOUBLE PRECISION U_DEV(1:CTE_OS_NBMU_MAX)	! Valeur du champ U dévié

      DOUBLE PRECISION I_DEV_INTERPO(1:CTE_OS_NBMU_MAX)	
      				! (S)  Valeur du champ I dévié pour les 
      				!      pour les angles correspondant aux angles 
				!      de déviation. 
				!      - en surface : champ Up au niveau 0+
      				!      - sous l'eau : champ Down au niveau 0-
      DOUBLE PRECISION Q_DEV_INTERPO(1:CTE_OS_NBMU_MAX)	! (S)  Valeur du champ Q dévié
      DOUBLE PRECISION U_DEV_INTERPO(1:CTE_OS_NBMU_MAX)	! (S)  Valeur du champ U dévié

      DOUBLE PRECISION COS_I	! Cosinus de l'angle d'incidence.
      DOUBLE PRECISION SIN2_I	! Sinus carré de l'angle d'incidence.
      DOUBLE PRECISION COS_T	! Cosinus de l'angle réfracté.
      DOUBLE PRECISION SIN2_T	! Sinus carré de l'angle réfracté.
      DOUBLE PRECISION N1SN2	! Rapport de l'indice N1 du milieu incident
      				! sur l'indice N2 du milieu de réfraction.

      DOUBLE PRECISION COS_TMAX ! Cosinus de l'angle maximal de transmission.
      
      DOUBLE PRECISION CL	! Coefficient de réflexion ou transmission parallèle.
      DOUBLE PRECISION CR	! Coefficient de réflexion ou transmission perpendiculaire.
      
      DOUBLE PRECISION COEFT	! Coefficient de pondération de la matrice de Fresnel
      				! en transmission.
 
      DOUBLE PRECISION F11(1:CTE_OS_NBMU_MAX)	
                                ! Elément R11 de la matrice de Fresnel en réflexion
      				! ou transmission en fonction de l'angle d'incidence.
      DOUBLE PRECISION F12(1:CTE_OS_NBMU_MAX)     
      				! Elément R12 de la matrice de Fresnel.
      DOUBLE PRECISION F33(1:CTE_OS_NBMU_MAX)    
      				! Elément R33 de la matrice de Fresnel


      INTEGER*4 IER       	! (S) code d'erreur =0 si pas d'erreur, =-1 sinon

      INTEGER*4 NBMU      	!  (E) Nombre d'angles effectifs des simulations      
      INTEGER*4 NBMUT     	!  Nombre d'angles pour lesquels il y a une transmission
      				!  pour une incidence selon les angles de Gauss
      
      INTEGER*4 NBMU_INTERPO    !  Nombre d'angles de propagation pour lesquels il y a 
      				!  une transmission non nulle à interpoler.
      
      INTEGER*4 J	  	! Indice d'angle
      
      CHARACTER*3 CINTER	! (E) Chaine de caractères précisant le cas d'interface 
      				!   une réflexion air -> air : "RAA"
				!   une réflexion mer -> mer : "RWW"
				!   une transmission air -> mer : "TAW"
				!   une transmission mer -> air : "TWA"

      LOGICAL TRACE        	! (E) = vrai si écriture dans le fichier trace




C* Initialisation
C----------------
      NBMUT=0     


C* Calcul du rapport d'indice du milieu incident sur le milieu de réfraction	
C---------------------------------------------------------------------------      
      IF ((CINTER.NE."RAA").AND.(CINTER.NE."TAW").AND.
     &    (CINTER.NE."RWW").AND.(CINTER.NE."TWA")) GOTO 900

      IF (CINTER.EQ."RAA") THEN	!Cas de la réflexion air --> air  
	  N1SN2 = 1.D+00/IND  !milieu incident d'indice 1, milieu de réfraction d'indice IND
      ENDIF
      
      IF (CINTER.EQ."TAW") THEN !Cas de la transmission air --> mer 
	  N1SN2 = 1.D+00/IND  !milieu incident d'indice 1, milieu de réfraction d'indice IND
      ENDIF
      
      IF (CINTER.EQ."RWW") THEN !Cas de la réflexion mer --> mer
	  N1SN2 = IND !milieu incident d'indice IND, milieu de réfraction d'indice 1
      ENDIF
      
      IF (CINTER.EQ."TWA") THEN !Cas de la transmission mer --> air   
	  N1SN2 = IND !milieu incident d'indice IND, milieu de réfraction d'indice 1
      ENDIF     
      
      
      IF (TRACE) THEN
      
         WRITE(IDLOG,*,ERR=910) "\n Flat sea :"
	 
         IF (CINTER.EQ."RAA") THEN
	    WRITE(IDLOG,*,ERR=910) " Reflection Air -> Air  "
	 ENDIF   

         IF (CINTER.EQ."RWW") THEN
	    WRITE(IDLOG,*,ERR=910) " Reflection Sea -> Sea  "
	 ENDIF   
	 
         IF (CINTER.EQ."TAW") THEN
	    WRITE(IDLOG,*,ERR=910) " Transmission Air -> Sea  "
	 ENDIF  

         IF (CINTER.EQ."TWA") THEN
	    WRITE(IDLOG,*,ERR=910) " Transmission Sea -> Air  "
	 ENDIF   	  	 
      ENDIF 
      
      
      
C* Calcul des éléments de la matrice de Fresnel	(réflexion ou transmission)
C--------------------------------------------------------------------------
      DO 104 J=1,NBMU

         !Cosinus de l'angle d'incidence	
	 COS_I=RMU(J)		
		 
	 SIN2_I= 1.D+00-COS_I*COS_I

C* Cosinus de l'angle de réfraction (angle / normale au dioptre) :
C-----------------------------------------------------------------
	 SIN2_T = N1SN2*N1SN2*SIN2_I
	 
	 IF (SIN2_T.LE.1.D+00) THEN
	 
             COS_T = DSQRT(1.D+00 - SIN2_T)
	     RMUT(J)=COS_T	!Stockage de mu transmis

C* Coefficients de réflexion parallèle et perpendiculaire
C--------------------------------------------------------
	     CL= (COS_I - N1SN2*COS_T) / (COS_I + N1SN2*COS_T)
	     CR= (N1SN2*COS_I - COS_T) / (N1SN2*COS_I + COS_T)

C* Coefficients de réflexion ou transmission parallèle et perpendiculaire
C------------------------------------------------------------------------	 
	     COEFT = 1.D+00
	     IF ((CINTER.EQ."TAW").OR.(CINTER.EQ."TWA")) THEN    
	        IF (COS_I.NE.0.) THEN
		   COEFT = COS_T  / (COS_I*N1SN2*N1SN2*N1SN2)
		ELSE
		   !Cas d'une incidence à 90 degrés ==> Pas de transmission
		   COEFT = 0.
		ENDIF   
	        CL= N1SN2 * (1.D+00+CL)
	        CR= 1.D+00 + CR
		
		NBMUT=NBMUT+1	!Incrémentation du nb de mu transmis (hors réflexion totale)
	     ENDIF 
	 	
C* Eléments de la matrice de Fresnel en réflexion ou transmission
C----------------------------------------------------------------
             F11(J)=.5*(CL*CL+CR*CR) * COEFT
             F12(J)=.5*(CL*CL-CR*CR) * COEFT
             F33(J)=CL*CR * COEFT

	 ELSE ! Gestion dépassement de l'angle limite

             RMUT(J)=0.	!Stockage de mu transmis
	     
	     IF ((CINTER.EQ."TAW").OR.(CINTER.EQ."TWA")) THEN    
	        F11(J)=0.D+00
                F12(J)=0.D+00
                F33(J)=0.D+00
	     ELSE
	        F11(J)=1.D+00
                F12(J)=0.D+00
                F33(J)=1.D+00
	     ENDIF
	     
	 ENDIF !Fin test transmission possible	


C* Passage de l'interface : champ pour les angles de transmissions
C*                          directement associés aux angles de Gauss incident	
C--------------------------------------------------------------
        I_DEV(J) = F11(J)*I_INC(J) + F12(J)*Q_INC(J)
	 Q_DEV(J) = F12(J)*I_INC(J) + F11(J)*Q_INC(J)
	 U_DEV(J) = F33(J)*U_INC(J) 
	 
	 
C* Fichier trace 
C-----------------
         IF (TRACE) THEN
	 
            WRITE(IDLOG,200,ERR=910) 
     &           ' Angle inc :',DACOS(COS_I)*180./INCTE_PI,
     &           ' Ang trans :',DACOS(RMUT(J))*180./INCTE_PI,
     &           ' F11 ',F11(J),' F12 ',F12(J),' F33 ',F33(J),
     &           ' I_I ',I_INC(J),' I_D ',I_DEV(J),
     &		   ' Q_I ',Q_INC(J),' Q_D ',Q_DEV(J),
     &		   ' U_I ',U_INC(J),' U_D ',U_DEV(J)

         ENDIF
	 
 104  CONTINUE
 
      IF (TRACE) THEN
         WRITE(IDLOG,*,ERR=910) " "
         WRITE(IDLOG,*,ERR=910) " "
      ENDIF

C* Interpolation du champ pour les angles de transmissions
C* directement associés aux angles de Gauss incident (=angles de propagation)
C--------------------------------------------------------------
      
      IF ((CINTER.EQ."TAW").OR.(CINTER.EQ."TWA")) THEN  
         !Cas d'une transmission : angles de transmission 
	 !                         à interpoler aux angles de Gauss
	 !                         ==> jusqu'à l'angle de transmission maximal
	
	
	 IF (N1SN2.GT.1.) THEN
	    !Cas transition mer -> air
	    NBMU_INTERPO = NBMU
	 ELSE
	    !Cas transition air -> mer
	    COS_TMAX = DSQRT(1. - N1SN2*N1SN2)
	    J=1
	    DO WHILE (RMU(J).GT.COS_TMAX)
	       J=J+1
	    ENDDO
	    NBMU_INTERPO=J
	 ENDIF
	  
	 
	 CALL SOS_INTERPO_SPLINT(NBMUT, RMUT, I_DEV(1),
     &	                         NBMU_INTERPO, RMU(1), I_DEV_INTERPO, 
     &                          IER)	
	 IF (IER.NE.0) GOTO 920   ! cas d'erreur  

	 CALL SOS_INTERPO_SPLINT(NBMUT, RMUT, Q_DEV(1),
     &	                         NBMU_INTERPO, RMU(1), Q_DEV_INTERPO, 
     &                          IER)	
	 IF (IER.NE.0) GOTO 920   ! cas d'erreur  
	 
	 CALL SOS_INTERPO_SPLINT(NBMUT, RMUT, U_DEV(1),
     &	                         NBMU_INTERPO, RMU(1), U_DEV_INTERPO, 
     &                          IER)	
	 IF (IER.NE.0) GOTO 920   ! cas d'erreur  
	 

         IF (N1SN2.LE.1.) THEN
	    !Cas transition air -> mer : 
	    !on force à zéro les valeurs de I, Q, U pour les directions 
	    !en dehors du cône de transmission (mal interpolées par la fonction Splint)
	    
	    DO J=NBMU_INTERPO,NBMU
	       I_DEV_INTERPO(J)=0.D+00
	       Q_DEV_INTERPO(J)=0.D+00
	       U_DEV_INTERPO(J)=0.D+00
	    ENDDO

	 ENDIF
	 	 
         IF (TRACE) THEN
	    WRITE(IDLOG,*,ERR=910) 'Nb transmitted angles :', NBMUT
	    WRITE(IDLOG,*,ERR=910) 'Nb Gauss angles for interpo :',
     &                 	           NBMU_INTERPO
	    
	    DO J=1,NBMU
	       WRITE(IDLOG,210,ERR=910) 
     &           ' Ang gauss :',DACOS(RMU(J))*180./INCTE_PI,
     &           ' I_AngGauss ',I_DEV_INTERPO(J),
     &           ' Q_AngGauss ',Q_DEV_INTERPO(J),
     &           ' U_AngGauss ',U_DEV_INTERPO(J)
	    ENDDO
         ENDIF
	 	 	  
       ELSE !Cas d'une réflexion : angles inchangés
       
         DO J=1,NBMU
	    I_DEV_INTERPO(J) = I_DEV(J)
    	    Q_DEV_INTERPO(J) = Q_DEV(J)
     	    U_DEV_INTERPO(J) = U_DEV(J)
	 ENDDO 
	 
	       
       ENDIF ! Fin test si transmission
       
                    
C* Fin nominale 
C-------------------
      IER=0
      GOTO 9999
      
C* Cas d'erreur : retour du status -1 au programme appelant
C------------------------------------------------------    
  900 WRITE(6,*) '  OSOAA_INTERF_MERPLATE : ERROR_900'
      WRITE(6,*) '     on mode interface definition'
      GOTO 9998
      
  910 WRITE(6,*) '  OSOAA_INTERF_MERPLATE : ERROR_910'
      WRITE(6,*) '     on logfile writing' 
      GOTO 9998

  920 WRITE(6,*) '  OSOAA_INTERF_MERPLATE : ERROR_920'
      WRITE(6,*) '     error while interpolating transmitted radiance'
      GOTO 9998          
   
 9998 IER=-1   
 9999 CONTINUE


   
C* Format
C---------   
  200 FORMAT(2(A11,F8.4),12(A6,E9.3))
  210 FORMAT(A11,F8.4,2X,3(A11,1X,E9.3,3X))



      RETURN
      END  	!OSOAA_INTERF_MERPLATE
      
      
      



C*===========================================================================
C PROCEDURE: OSOAA_FSOURCE_DIFF_FRESNEL1
C ==========
C      Cette procédure calcule la fonction source de la diffusion 
C      du rayonnement solaire remontant après une réflexion directe
C      sur mer plate, selon les lois de Fresnel (diffusion du champ
C      de réflexion d'ordre 1).
C
C
C      NB: Si EXPERT_MODE_FORCED_FATM_NULL = .TRUE. la fonction source est posée nulle.
C          (constante introduite par #define)
C
C
C Description des paramètres
C -------------------------- 
C
C      MUS (double)  (E)   Cosinus de l'angle zénithal solaire (MUS>0)
C
C      IND_SURF (double)  (E)   Indice de réfraction mer / air.
C
C      JK (integer)  (E)  Numéro de stockage de mus dans l'air dans le tableau RMU
C
C      XDEL(0:CTE_NT_ATM)  (double)  (E) Pourcentage d'aérosols par niveau.
C      YDEL(0:CTE_NT_ATM)  (double)  (E) Pourcentage de molécules par niveau. 
C
C      BETA0   (double)  (E)  
C      BETA2   (double)  (E) 
C      GAMMA2  (double)  (E)	
C      ALPHA2  (double)  (E)
C             Coefficients du développement de la matrice 
C             de phase moléculaire en fonctions de Legendre.
C
C      BP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) 
C                                                        Elément P11.
C      GR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) 
C                                                        Elément P12 ou P21.
C      GT(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  (E) 
C                                                        Elément -P13 ou -P31.
C      ARR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)  (E) 
C                                                        Elément P22.
C      ART(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)  (E)
C                                                        Elément -P23 ou -P32.
C             Eléments Pij de la matrice de phase des aérosols pour un ordre
C             IS de la décomposition en séries de Fourier.
C             Soit mup la direction d'incidence ( -1 < mup < 1)
C             et mu celle de diffusion ( -1 < mu < 1),
C             la diffusion est exprimée par Pij(mu,mup).	
C
C      XPL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)  (E) 
C      		        Fonction de Legendre PSL à l'ordre L=2
C   		        et S=IS en fonction de l'angle mu.
C      XRL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)  (E)
C      		        Fonction RSL à l'ordre L=2 et S=IS.
C      XTL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (double)  (E)
C      		        Fonction TSL à l'ordre L=2 et S=IS
C	      Fonctions de Legendre et fonctions dérivées pour le calcul
C	      de la matrice de phase.
C
C      IS (I4)   (E)  Ordre de la décomposition en séries de Fourier.
C
C      NBMU (I4) (E)  Nombre d'angles (positifs) effectivement utiles
C
C
C      H(0:CTE_NT_ATM) (double) (E)  Epaisseur optique à chaque niveau.
C
C      I2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (S) 
C                                                  Fonction source pour I
C      Q2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (S) 
C                                                  Fonction source pour Q 
C      U2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double) (S) 
C                                                  Fonction source pour U
C             Vecteur fonction source, en fonction du niveau du 
C             profil et de la direction, pour la diffusion de
C             la lumière obtenue par réflexion de Fresnel du faisceau 
C             solaire direct.
C                          
C			
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     Aucun
C
C==============================================================================
      SUBROUTINE OSOAA_FSOURCE_DIFF_FRESNEL1(MUS,JK,IND_SURF,XDEL,YDEL,
     &                                       BETA0,BETA2,GAMMA2,ALPHA2,
     &                                       BP,GR,GT,ARR,ART,
     &                                       XPL,XRL,XTL,IS,NBMU,
     &					     H,I2,Q2,U2)
      
      IMPLICIT NONE


C* Définition des variables    
C*-----------------------------------------------------------------------     
	! Vecteur fonction source en fonction du niveau du profil
	! et de la direction.
      DOUBLE PRECISION 
     &       I2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour I.
      DOUBLE PRECISION 
     &       Q2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour Q.
      DOUBLE PRECISION  
     &       U2(0:CTE_NT_ATM,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      						! Fonction source pour U.

      DOUBLE PRECISION IND_SURF	! (E) Indice de réfraction de l'eau / air
						
      DOUBLE PRECISION F11sun   ! Elément de la matrice de Fresnel
      			        ! avec pour incidence la direction
			        ! d'éclairement solaire.
      DOUBLE PRECISION F12sun   ! Elément de la matrice de Fresnel.

      DOUBLE PRECISION XDEL(0:CTE_NT_ATM)  !Pourcentage d'aérosols par niveau.
      DOUBLE PRECISION YDEL(0:CTE_NT_ATM)  !Pourcentage de molécules par niveau.

      DOUBLE PRECISION MUS               ! Cosinus de l'angle zénithal solaire (mus>0)
      DOUBLE PRECISION H(0:CTE_NT_ATM)	 !Epaisseur optique pour chaque niveau.
           
      DOUBLE PRECISION YR   ! Pourcentage de molécules au niveau K du profil.
      DOUBLE PRECISION YYR  ! Pourcentage de molécules au niveau K+1 du profil.
      DOUBLE PRECISION XP   ! Pourcentage d'aérosols au niveau K du profil.
      DOUBLE PRECISION XXP  ! Pourcentage d'aérosols au niveau K+1 du profil.      


      DOUBLE PRECISION XPL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      			! Fonction de Legendre PSL à l'ordre L=2 et S=IS
      			! en fonction de l'angle mu.     		
      DOUBLE PRECISION XRL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      			! Fonction RSL à l'ordre L=2 et S=IS en fonction de mu.
      DOUBLE PRECISION XTL(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      			! Fonction TSL à l'ordre L=2 et S=IS en fonction de mu.

      DOUBLE PRECISION SPL   ! Valeur de la fonction de XPL pour mu = mus
           	
      DOUBLE PRECISION BETA0	! Coefficients du développement de la matrice 
      DOUBLE PRECISION BETA2	! de phase moléculaire en fonctions de Legendre
      DOUBLE PRECISION GAMMA2	
      DOUBLE PRECISION ALPHA2	

	! Eléments Pij de la matrice de phase des aérosols pour un ordre
	! IS de la décomposition en séries de Fourier.
	! Soit mup la direction d'incidence (-1 < mup < 1)
	! et mu celle de diffusion (-1 < mu < 1),
	! la diffusion est exprimée par Pij(mu,mup).	
      DOUBLE PRECISION 
     &    BP(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &       -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! Elément P11.
      DOUBLE PRECISION 
     &    GR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &       -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! P12 ou P21.
      DOUBLE PRECISION 
     &    GT(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &       -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! -P13 ou -P31.
      DOUBLE PRECISION 
     &    ARR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &        -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! P22.
      DOUBLE PRECISION 
     &    ART(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX,
     &        -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! -P23 ou -P32.
     
           
      DOUBLE PRECISION BP0MJ		!Coeff BP(0,-J) ==> devenu BP(JK,-J) 
      DOUBLE PRECISION BP0J		!                   pour JK indice de table 
      DOUBLE PRECISION GRJ0		!		    de l'angle solaire
      DOUBLE PRECISION GR0J
      DOUBLE PRECISION GR0MJ
      DOUBLE PRECISION GRMJ0
      DOUBLE PRECISION GT0MJ
      DOUBLE PRECISION GT0J
      DOUBLE PRECISION ARR0MJ
      DOUBLE PRECISION ARR0J
      DOUBLE PRECISION ARTJ0
      DOUBLE PRECISION ARTMJ0
       
      
      DOUBLE PRECISION COEFNT
      DOUBLE PRECISION COEFK
      DOUBLE PRECISION COEFKP1 

      DOUBLE PRECISION MU2  ! Carré de mus.
      DOUBLE PRECISION IND2 ! Carré de IND_SURF.
      DOUBLE PRECISION X    
      DOUBLE PRECISION RL   ! Coefficient de réflexion parallèle.
      DOUBLE PRECISION RR   ! Coefficient de réflexion perpendiculaire.


      
      INTEGER IS	! Ordre de la décomposition en séries de Fourier.     
      INTEGER K		! Indice de niveau du profil.
      INTEGER J		! Indice d'angle d'incidence.
      INTEGER JK	! Indice d'angle d'incidence solaire.
      INTEGER*4 NBMU    ! Nombre d'angles effectifs des simulations
      
       
C* Traitements    
C*------------

C* Initialisation

      DO 10 J=-NBMU,NBMU
         DO 10 K=0,CTE_NT_ATM
	    I2(K,J)=0.
	    Q2(K,J)=0.
	    U2(K,J)=0.
10    CONTINUE      



C* Calcul des termes F11 et F12 de la réflexion de Fresnel pour l'incidence solaire
      IND2 = IND_SURF*IND_SURF
      X = DSQRT(IND2-1.D+00+MUS*MUS)
      RL = (IND2*MUS-X)/(IND2*MUS+X)
      RR = (MUS-X)/(MUS+X)
	 
      F11sun = (RL*RL+RR*RR)/2.
      F12sun = (RL*RL-RR*RR)/2.


      COEFNT  = DEXP(-2.*H(CTE_NT_ATM)/MUS) / 4.  

      SPL=XPL(JK)	!incidence solaire

      
      DO 1500 K= 0 , CTE_NT_ATM-1
      
         YR=YDEL(K)
         XP=XDEL(K)
         
         YYR=YDEL(K+1)
         XXP=XDEL(K+1)
	 
         DO 1502 J=1,NBMU

C* Eléments de matrice de phase pour le mélange molécules + aérosols
            IF (IS.LE.2) THEN	 
	    
                BP0MJ=BP(JK,-J)*XP+(BETA0+BETA2*XPL(-J)*SPL)*YR
                BP0J=BP(JK,J)*XXP+(BETA0+BETA2*XPL(J)*SPL)*YYR
            
                GRJ0=GR(J,JK)*XXP+YYR*XRL(JK)*XPL(J)*GAMMA2
	        GR0J=GR(JK,J)*XXP+YYR*XRL(J)*XPL(JK)*GAMMA2
                GR0MJ=GR(JK,-J)*XP+YR*XRL(-J)*SPL*GAMMA2
	        GRMJ0 =GR(-J,JK)*XP+YR*GAMMA2*XRL(JK)*XPL(-J)	
	    
                GT0MJ=GT(JK,-J)*XP+YR*GAMMA2*SPL*XTL(-J)
                GT0J=GT(JK,J)*XXP+YYR*GAMMA2*SPL*XTL(J)
	                
                ARR0MJ=ARR(JK,-J)*XP+ALPHA2*YR*XRL(JK)*XRL(-J)
                ARR0J=ARR(JK,J)*XXP+ALPHA2*YYR*XRL(JK)*XRL(J)
            
                ARTJ0=ART(J,JK)*XXP+YYR*ALPHA2*XTL(J)*XRL(JK)
                ARTMJ0=ART(-J,JK)*XP+YR*ALPHA2*XTL(-J)*XRL(JK)	
		
            ELSE
	    
                BP0MJ=BP(JK,-J)*XP
                BP0J=BP(JK,J)*XXP
            
                GRJ0=GR(J,JK)*XXP
	        GR0J=GR(JK,J)*XXP
                GR0MJ=GR(JK,-J)*XP
	        GRMJ0 =GR(-J,JK)*XP
	    
                GT0MJ=GT(JK,-J)*XP
                GT0J=GT(JK,J)*XXP
	                
                ARR0MJ=ARR(JK,-J)*XP
                ARR0J=ARR(JK,J)*XXP          
            
                ARTJ0=ART(J,JK)*XXP
                ARTMJ0=ART(-J,JK)*XP	  
		  
	    ENDIF
   
	     
	     
C* Fonction source pour le champ montant
            COEFK   = COEFNT * DEXP(H(K)/MUS)
	    
            I2(K,J)= COEFK * ( F11sun*BP0MJ + F12sun*GRMJ0 )		
            Q2(K,J)= COEFK * ( F11sun*GR0MJ + F12sun*ARR0MJ )
            U2(K,J)= COEFK * ( F11sun*GT0MJ + F12sun*ARTMJ0 )		

	
C* Fonction source pour le champ descendant
            COEFKP1 = COEFNT * DEXP(-H(K+1)/MUS) 
	    
            I2(K+1,-J)=  COEFKP1 * ( F11sun*BP0J + F12sun*GRJ0 )
            Q2(K+1,-J)=  COEFKP1 * ( F11sun*GR0J + F12sun*ARR0J )
            U2(K+1,-J)=  COEFKP1 * ( F11sun*GT0J + F12sun*ARTJ0 )
	     
 1502    CONTINUE
 1500 CONTINUE
      
  
      !Mode expert spécifique : annulation de la diffusion atmosphérique
      IF (EXPERT_MODE_FORCED_FATM_NULL) THEN 
         WRITE(6,*) 
     &	 " \n WARNING : ATMOS SOURCE FUNCTION FORCED TO NULL \n"
         DO J=1,NBMU
	    DO K=0,CTE_NT_ATM
	       I2(K,J)=0.
	       I2(K,-J)=0.
	       Q2(K,J)=0.
	       Q2(K,-J)=0.
	       U2(K,J)=0.
	       U2(K,-J)=0.
	    ENDDO
	 ENDDO
      ENDIF
      
      END      !FIN DE LA PROCEDURE OSOAA_FSOURCE_DIFF_FRESNEL1
       
       
       
       
            
