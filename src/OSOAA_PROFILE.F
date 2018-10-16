C******************************************************************************
C* FICHIER: OSOAA_PROFILE.F
C*
C*------------------------------------------------------------------------ 
C* OSOAA Licence
C*   Copyright (c) 2015, Université Pierre et Marie Curie - Paris 6
C*                       and Centre National d'Etudes Spatiales (CNES)
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
C* ROLE: Définition des profils d'épaisseurs optiques atmosphériques et maritimes
C*
C* AUTEURS: 
C*   Laboration d'Océanographie de Villefranche sur Mer (LOV) : Malik Chami
C*      - Code initial: code OSOA (version mer plate)
C*      - Compléments de spécification de besoin pour OSOAA 
C*
C*   Communication & Systemes (CS, Toulouse) : Xavier Lenot & Bruno Lafrance
C*   => Reprise du codage et développements complémentaires :
C*      - Rétro-ingénierie sur la gestion des profils marins et atmosphériques dans OSOA
C*      - Industrialisation du module
C*
C*
C* ROUTINES COUVERTES PAR LE FICHIER : 
C*   - OSOAA_PROFILE
C*   - OSOAA_ALTI_LAYER
C*   - OSOAA_SEA_EUPHOTIC_DEPTH
C*   - OSOAA_SEA_MOL_COEFFS
C*   - OSOAA_SEA_PHY_COEFFS
C*   - OSOAA_SEA_SED_COEFFS
C*
C*
C*
C* MOD:VERSION:1.0: 02/04/2015: Initial version of the OSOAA code
C* MOD:VERSION:1.1: 15/06/2015: All comments for log files are now in english
C* MOD:VERSION:1.2: 03/11/2016: 
C*      Improvement of the calculation for the number of layers NZ
C*      --> use of the NINT() function to avoid a potential truncation effect.
C* MOD:VERSION:1.3: 23/05/2018: 
C*      Routine OSOAA_SEA_PHY_COEFFS is changed in version VERSION:1.1                               
C*   
C******************************************************************************

C----------------------------------------------------------------------------
C Définition des constantes  
C---------------------------------------------------------------------------- 
C Constantes utilisées : 
C
C    CTE_LENDIR :  Longueur du nom d'un répertoire
C    CTE_NOT_DEFINED_VALUE_DBLE : Identifiant de valeur double non définie
C                                 par l'utilisateur.
C    CTE_LENFIC2 : Longueur des noms de fichiers avec arborescence.
C    CTE_STD_PRESSURE : pression atmosphérique standard
C    CTE_ALT_TOA : altitude retenue pour le sommet de l'atmosphère
C    CTE_NT_ATM : nombre de couches du profil atmosphérique
C    CTE_NT_SEA : nombre de couches du profil océanique
C    CTE_TRANS_OPT_THICKNESS : épaisseur optique de la couche de transition mer / atmosphère
C    CTE_NZ_MAX : nombre de couches maxi dans les profils
C    CTE_SEA_DEPTH_STEP : pas d'échantillonnage (en m) du profil maritime initial
C    CTE_FIC_EUPH_DEPTH : nom du fichier donnant la profondeur euphotique en fonction de la
C  		          concentration en chlorophylle
C    CTE_FIC_MOL_SPECTRAL_DATA : nom du fichier contenant les coeffs d'absorption 
C                                et diffusion des molécules d'eau
C    CTE_FIC_PHYTO_SPECTRAL_DATA : nom du fichier contenant les coeffs AP et EP 
C                                  du phytoplancton en fonction de la longueur d'onde.
C    CTE_NBWA_MAX : nombre maximal de données spectrales des fichiers 
C                    CTE_FIC_MOL_SPECTRAL_DATA et CTE_FIC_PHYTO_SPECTRAL_DATA
C    CTE_DEFAULT_SPECTRAL_YS : valeur par défaut du coefficient de variation spectrale
C                              de l'absorption de la substance jaune (m-1).  
C    CTE_DEFAULT_SPECTRAL_DET : valeur par défaut du coefficient de variation spectrale
C                               de l'absorption des détritus (m-1).  
C    CTE_SEA_T_LIMIT : valeur max de l'épaisseur optique de la colonne d'eau
C
C Constantes spécifiques :
C    INCTE_IDLOG_PROFILE : ID du ficher Trace de la routine
C----------------------------------------------------------------------------
#include "OSOAA.h"
#define INCTE_IDLOG_PROFILE 99




C==============================================================================
C PROCEDURE: OSOAA_PROFILE
C ==========
C
C      Programme de génération des profils atmosphériques et maritimes
C
C
C Description des paramètres 
C ------------------------------
C
C     WA_SIMU (DOUBLE)	(E) : Longueur d'onde de simulation des luminances (en microns)
C          
C     TA (DOUBLE)       (E) : Epaisseur optique des aerososl 
C			      (à la longueur d'onde de simulation)
C
C     HA (DOUBLE)       (E) : Echelle de hauteur du profil d'aérosols (km)
C
C     TR (DOUBLE)       (E) : Epaisseur optique rayleigh  
C			      (à la longueur d'onde de simulation)
C
C     HR (DOUBLE)       (E) : Echelle de hauteur du profil moléculaire (km)
C
C     PRESSION (DOUBLE) (E) : Pression atmosphérique au niveau de la mer (mbar).
C
C     PIZAER (DOUBLE) (E) : Albédo de simple diffusion des aérosols.
C     
C     SEA_DEPTH (DOUBLE) (E) : Profondeur maximale du profil marin (en m)
C
C     CHL (DOUBLE)       (E) : Concentration de la chlorophylle en surface (mg/m3)
C
C     IPHYTO_PROFIL (I2) (E) : Indice de type de profil de phytoplancton
C
C     CHL_GP_BG (DOUBLE) (E) : Constante de biomasse du profil gaussien de chlorophylle 
C                              (mg/m3)
C
C     CHL_GP_DEEP (DOUBLE)   (E) : Profondeur max du profil gaussien de chlorophylle (m)
C
C     CHL_GP_WIDTH  (DOUBLE) (E) : Largeur du pic du profil gaussien de chlorophylle (m)
C
C     FICPHYTO_USER (CHAR)   (E) : Nom du fichier utilisateur (chemin complet) donnant 
C                                  le profil de chlorophylle
C
C     SED_CSED (DOUBLE)      (E) : Concentration des sédiments en surface (mg/litre)
C
C     SED_MRWA	(DOUBLE)     (E) : Partie réelle de l'indice de réfraction moyen 
C				  des sédiments (à la longueur d'onde de simulation)
C
C     SED_VSED  (DOUBLE)     (E) : Volume moyen d'une particule de type "sédiment" (mic^3)
C
C     SED_KMAT2  (DOUBLE)    (E) : Section efficace d'une particule de type "sédiment" 
C				   (mic^2) (à la longueur d'onde de simulation)
C
C     SED_PIZ (DOUBLE)  (E) : Albédo de simple diffusion des sédiments 
C			     (à la longueur d'onde de simulation des luminances)
C
C     YS_A440  (DOUBLE) (E) : Coeff d'absorption de la substance jaune à 440 nm (m-1)
C
C     YS_SWA (DOUBLE)   (E) : Coefficient de conversion spectrale de l'absorption 
C			      de la substance jaune (m-1)
C
C     DET_A440 (DOUBLE) (E) : Coeff d'absorption des détritus à 440 nm (m-1)
C
C     DET_SWA (DOUBLE)  (E) : Coefficient de conversion spectrale de l'absorption 
C                             des détritus (m-1)
C
C     FICPROFIL_LOG (CHAR)      (E) : Nom du fichier de log (chemin complet)
C
C     ATM_FICPROFIL_RES (CHAR) (E) : Nom du fichier contenant le profil atmosphérique 
C                                    (chemin complet)
C
C     SEA_FICPROFIL_RES (CHAR) (E) : Nom du fichier contenant le profil maritime 
C                                    (chemin complet)
C
C     IER (I4)                 (S) : indice d'erreur : 0 si pas d'erreur, -1 sinon.
C   
C
C Résultats fournis par la procédure
C ----------------------------------
C 
C    Un fichier de profil atmosphérique.
C    Un fichier de profil maritime.
C    Un fichier Trace optionnel.
C
C
C Variable d'environnement
C ------------------------
C
C   OSOAA_ROOT qui définit l'arborescence d'accès au répertoire principal du code OSOAA
C   Cette variable d'environnement permet de localiser les fichiers sous $OSOAA_ROOT/fic
C
C
C Description des fichiers utilisés	
C ---------------------------------
C
C    Fichier utilisateur FICPHYTO_USER: 
C      ==>   contient le profil en chlorophylle fourni par l'utilisateur
C
C		format = 1 ligne par couche du profil (2 colonnes / ligne)
C		chaque ligne contient:
C			- l'altitude de la couche (en m)
C			- la concentration en chlorophylle (mg.m-3)			
C
C    Fichier trace : identifiant = IDLOG = INCTE_IDLOG_PROFILE 
C      ==>   contient des informations sur les étapes de traitements
C
C    Fichier source CTE_FIC_EUPH_DEPTH
C      ==>   contient la profondeur euphotique associée à une concentration 
C            de chlorophylle en surface
C
C		format = 1 ligne par concentration de chlorophylle (2 colonnes / ligne)
C		chaque ligne contient:
C			- la concentration de chlorophylle en surface (en mg.m-3)
C			- la profondeur euphotique (en m)
C
C    Fichier source CTE_FIC_PHYTO_SPECTRAL_DATA
C      ==>   contient les coefficients AP et EP pour le phytoplancton
C
C		format = 1 ligne par longueur d'onde (3 colonnes / ligne)
C		chaque ligne contient:
C			- la longueur d'onde (en nm)
C			- le coeff AP (en m2.mg-1)
C			- le coeff EP 
C
C    Fichier source CTE_FIC_MOL_SPECTRAL_DATA
C      ==>   contient les coefficients d'absorption et diffusion des molécules d'eau
C
C		format = 1 ligne par longueur d'onde (3 colonnes / ligne)
C		chaque ligne contient:
C			- la longueur d'onde (en nm)
C			- le coefficient d'absorption
C			- le coefficient de diffusion
C
C    Fichier résultat FIC_ATM_PROFILE: 
C      ==>   contient le profil atmosphérique en épaisseurs optiques
C
C		format = 10 lignes d'entête
C                        puis 1 ligne par couche du profil (5 colonnes)
C		chaque ligne contient:
C			- le numéro du niveau
C 			- l'altitude (en km)
C			- l'épaisseur optique totale d'extinction du niveau
C			- le taux de mélange des aérosols pour la diffusion
C			- le taux de mélange des molécules pour la diffusion
C
C    Fichier résultat FIC_SEA_PROFILE: 
C      ==>   contient le profil maritime en épaisseurs optiques
C
C		format = 11 lignes d'entête
C                        puis 1 ligne par couche du profil (6 colonnes)
C		chaque ligne contient:
C			- le numéro du niveau
C			- la profondeur du niveau (en m)
C			- l'épaisseur optique totale d'extinction du niveau
C			- le taux de mélange des molécules d'eau pour la diffusion
C			- le taux de mélange du phytoplancton pour la diffusion
C			- le taux de mélange des sédiments pour la diffusion
C
C Common utilisé:
C --------------
C  Aucun
C
C
C Cas d'erreur :
C ------------
C     - Erreur dans un sous-programme
C     - Erreur à l'ouverture, la lecture et l'écriture du fichier Trace 
C       et des fichiers E/S :  fichiers de données marines prédéfinies (CTE_FIC*)
C                              et fichiers résultats
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant
C
C
C  Sous programmes utilisés:
C --------------------------
C  Ce programme fait appel aux routines:
C      - OSOAA_ALTI_LAYER: calcule l'altitude de chaque couche du profil atmosphérique
C      - OSOAA_SEA_EUPHOTIC_DEPTH: calcule la profondeur euphotique à partir de la 
C                                  concentration en chlorophylle.
C      - OSOAA_SEA_MOL_COEFFS: calcule les coefficients d'absorption et diffusion 
C                              des molécules d'eau
C      - OSOAA_SEA_PHY_COEFFS: calcule le profil d'absorption et diffusion du phytoplancton
C      - OSOAA_SEA_SED_COEFFS: calcule les coeffs d'absorption et diffusion des sédiments
C
C==============================================================================

      SUBROUTINE OSOAA_PROFILE(WA_SIMU, TR, HR, PRESSION, TA, HA,    
     &                         PIZAER, SEA_DEPTH, CHL, IPHYTO_PROFIL,  
     &                         CHL_GP_BG, CHL_GP_DEEP, CHL_GP_WIDTH,    
     &			       FICPHYTO_USER, SED_CSED, SED_MRWA, 
     &			       SED_VSED, SED_KMAT2, SED_PIZ,
     &			       YS_A440, YS_SWA, DET_A440, DET_SWA,   
     &                         FICPROFIL_LOG, FICPROFIL_ATM_RES, 
     &			       FICPROFIL_SEA_RES, IER)

      IMPLICIT NONE
      
C Constantes PROFIL
C -----------------
            
      INTEGER*2 IDLOG			! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_PROFILE)
      	    
C* Définition des variables                         
C*-----------------------------------------------------------------

C-- Variables globales      
      DOUBLE PRECISION WA_SIMU	! Longueur d'onde de simulation des luminances (en microns)
      DOUBLE PRECISION WANM	! Longueur d'onde de simulation des luminances (en nm)

      DOUBLE PRECISION PC_DEN   ! Dénominateur pour calcul des proportions de diffusion
      INTEGER*4 I		! Niveau.
      INTEGER*4 J		! Niveau
      INTEGER*4 IFIN		! Indice de dernier caractère d'un répertoire
      
C---- Variables liées à la définition des profils atmosphérique et marin  
      
C*   Atmosphere :
      DOUBLE PRECISION TR    	! (E) Epaisseur optique rayleigh.
      DOUBLE PRECISION TA	! (E) Epaisseur optique des aérosols
      DOUBLE PRECISION PRESSION ! (E) Pression atmosphérique au niveau de la mer (mbar).
      DOUBLE PRECISION HR    	! (E) Echelle de hauteur du profil moléculaire (km).
      DOUBLE PRECISION HA    	! (E) Echelle de hauteur du profil d'aérosols (km).
      DOUBLE PRECISION PIZAER  	! (E) Albédo de simple diffusion des aérosols
      DOUBLE PRECISION PROF_ATM_Z(0:CTE_NT_ATM)	  ! Profil d'altitudes
      DOUBLE PRECISION PROF_ATM_T(0:CTE_NT_ATM)	  ! Profil d'épaisseur optique
      DOUBLE PRECISION PROF_TR(0:CTE_NT_ATM)	  ! Profil d'épaisseur optique moléculaire
      DOUBLE PRECISION PROF_TA(0:CTE_NT_ATM)	  ! Profil d'épaisseur optique des aérosols
      DOUBLE PRECISION PROF_PCRAY(0:CTE_NT_ATM) ! Proportion de molécules pour la diffusion 
      						! dans chaque couche atmosphérique
      DOUBLE PRECISION PROF_PCAER(0:CTE_NT_ATM) ! Proportion d'aérosols pour la diffusion 
      						! dans chaque couche atmosphérique
      DOUBLE PRECISION Z	! Altitude d'un niveau (en km)
      
      
C*   Mer
      DOUBLE PRECISION SEA_DEPTH    ! (E) Profondeur de la mer (m)  
      DOUBLE PRECISION EUPH_DEPTH   ! Profondeur euphotique 
      				    ! (calculée si pas de profondeur de la mer définie) (m)
      DOUBLE PRECISION CHL	    ! (E) Concentration de chlorophylle en surface (mg/m3)
      DOUBLE PRECISION CHL_GP_BG    ! (E) Constante de biomasse du profil gaussien 
      				    !     de chlorophylle (mg/m3)
      DOUBLE PRECISION CHL_GP_DEEP  ! (E) Profondeur maximale du profil gaussien 
      				    !     de chlorophylle (m)
      DOUBLE PRECISION CHL_GP_WIDTH ! (E) Largeur du pic du profil gaussien 
      				    !     de chlorophylle (m)
      DOUBLE PRECISION SED_CSED     ! (E) Concentration des sédiments en surface (mg/litre)
          
      DOUBLE PRECISION SED_MRWA	   ! Partie réelle de l'indice de réfraction moyen 
      				   ! des sédiments.
      				   ! (à la longueur d'onde de simulation des luminances)
      DOUBLE PRECISION SED_VSED    ! Volume moyen d'une particule de type "sédiment" (mic^3)
      DOUBLE PRECISION SED_KMAT2   ! Section efficace de diffusion d'une particule 
      				   ! de type "sédiment" (mic^2) 
				   ! (à la longueur d'onde de simulation des luminances)
      DOUBLE PRECISION SED_PIZ	   ! Albédo de simple diffusion des sédiments 
      				   ! (à la longueur d'onde de simulation des luminances).
      DOUBLE PRECISION YS_A440     ! (E)  Coeff d'absorption de la substance jaune 
      				   !      à 440 nm (m-1). 
      DOUBLE PRECISION YS_SWA      ! (E)  Coefficient de conversion spectrale de 
      				   !      l'absorption de la substance jaune. 
      DOUBLE PRECISION DET_A440    ! (E)  Coeff d'absorption des détritus à 440 nm (m-1). 
      DOUBLE PRECISION DET_SWA     ! (E)  Coefficient de conversion spectrale de 
                                   !      l'absorption des détritus. 
      
      INTEGER*2 IPHYTO_PROFIL   ! (E) Indice de type de profil de phytoplancton
      
      INTEGER*4 NZ                 ! Nombre de couches du profil océanique
      DOUBLE PRECISION MOL_ABS     ! Coefficient d'absorption moléculaire
      DOUBLE PRECISION MOL_DIF     ! Coefficient de diffusion moléculaire
      DOUBLE PRECISION YS_ABS      ! Coefficient d'absorption de la matière jaune
      DOUBLE PRECISION DET_ABS     ! Coefficient d'absorption des détritus
      DOUBLE PRECISION SED_ABS     ! Coefficient d'absorption des sédiments 
      				   ! (pour une profondeur donnée)
      DOUBLE PRECISION SED_DIF     ! Coefficient de diffusion des sédiments 
      				   ! (pour une profondeur donnée)
      DOUBLE PRECISION PROF_Z(0:CTE_NZ_MAX)	  ! Profondeurs du profil océanique
      DOUBLE PRECISION PROF_PHY_ABS(0:CTE_NZ_MAX) ! Profil des coeffs d'absorption 
      						  ! du phytoplancton
      DOUBLE PRECISION PROF_PHY_DIF(0:CTE_NZ_MAX) ! Profil des coeffs de diffusion 
      						  ! du phytoplancton
      
      DOUBLE PRECISION CEXT_TOT	    ! Coefficient d'extinction total 
      				    ! (pour une profondeur donnée)
				    
      DOUBLE PRECISION WATAB_MIN    ! Longueur d'onde min dans table d'info spectrale (nm)
      DOUBLE PRECISION WATAB_MAX    ! Longueur d'onde max dans table d'info spectrale (nm)
            
	! Profils en profondeurs du taux de mélange en diffusion :	
      DOUBLE PRECISION PROF_Z_MEL_SED(0:CTE_NZ_MAX)	  ! Cas des sédiments
      DOUBLE PRECISION PROF_Z_MEL_MOL(0:CTE_NZ_MAX)	  ! Cas des molécules d'eau
      DOUBLE PRECISION PROF_Z_MEL_PHY(0:CTE_NZ_MAX)	  ! Cas du phytoplancton
      
      DOUBLE PRECISION PROF_Z_T_SEA(0:CTE_NZ_MAX)   ! Profil en profondeurs de 
      						    ! l'épaisseur optique d'extinction
      
      DOUBLE PRECISION TT	                ! Variable temporaire pour interpo. linéaire
      DOUBLE PRECISION SEA_T_STEP               ! Pas d'échantillonnage du profil en ep. opt.
      DOUBLE PRECISION MAX_SEA_T		! Ep. opt. max de la colonne d'eau
      DOUBLE PRECISION PROF_SEA_T(0:CTE_NT_SEA)	! Profil en épaisseurs optiques
      	! Taux de mélange en diffusion :
      DOUBLE PRECISION MEL_SED	    ! Cas des sédiments (pour une couche du profil)
      DOUBLE PRECISION MEL_MOL	    ! Cas des molécules d'eau (pour une couche du profil)
      DOUBLE PRECISION MEL_PHY	    ! Cas du phytoplancton (pour une couche du profil)
      
C---- Les fichiers utilisés
      CHARACTER*CTE_LENFIC2 FICPROFIL_LOG       ! Nom du fichier trace 
      	        
      CHARACTER*CTE_LENFIC2 FICPHYTO_USER       ! Nom du fichier utilisateur donnant le
	     				        ! profil de chlorophylle (chemin complet)
					      
      CHARACTER*CTE_LENFIC2 FICPROFIL_ATM_RES   ! Nom du fichier résultat de profil 
      						! atmosphérique (chemin complet)
                                                  
      CHARACTER*CTE_LENFIC2 FICPROFIL_SEA_RES   ! Nom du fichier résultat de profil
      						! maritime (chemin complet)

      CHARACTER*CTE_LENDIR DIRFIC       	! Nom du répertoire contenant les 
      						! tables de données de profil 
            
      LOGICAL TRACE     ! = vrai si écriture dans le fichier trace
      
C---- Code d'erreur
      INTEGER*4 IER     ! Code d'erreur =0 si pas d'erreur, =-1 sinon
      
      
C* Initialisation                     
C*-----------------------------------------------------------------
      IER=0
	
     	
C* Ouverture du fichier Trace 
C------------------------------------------------------
      IF (FICPROFIL_LOG.EQ.'NO_LOG_FILE') THEN
	 TRACE=.FALSE.
      ELSE
         TRACE=.TRUE.
	 OPEN (IDLOG,FILE=FICPROFIL_LOG,ERR=2000)
      ENDIF


C* Récupération du nom du répertoire contenant les fichiers  
C* de paramètres de profil
C----------------------------------------------------------------------------
      CALL GETENV('OSOAA_ROOT',DIRFIC)
      IFIN=INDEX(DIRFIC,' ')
      IFIN=IFIN-1
      IF (IFIN.LE.0) GOTO 2050
      
      IF ((IFIN+4).GT.CTE_LENDIR) GOTO 2051
      
      DIRFIC(IFIN+1:IFIN+4)='/fic'
      IFIN=IFIN+4
      IF (IFIN.LE.0) IFIN=CTE_LENDIR
            
C======================================================
C* Création du profil atmosphérique  
C======================================================                     
      IF (TRACE) THEN
        WRITE (IDLOG,*,ERR=2010) 
     &	"GENERATION OF OPTICAL THICKNESS PROFILES"
	WRITE (IDLOG,*,ERR=2010) 
     &	 "****************************************"
	WRITE (IDLOG,*,ERR=2010) ""
        WRITE (IDLOG,*,ERR=2010) 
     &	"**** Calculation of the atmospheric profile"
	WRITE (IDLOG,*,ERR=2010) "Wavelength: ",WA_SIMU
      ENDIF
      
C* Calcul de l'épaisseur optique moléculaire à partir de la pression
C* Formule classique de Hansen & Travis
C* Cas où l'utilisateur a fourni la pression atmosphérique en entrée
C-------------------------------------------------------------------
      IF (PRESSION.NE.CTE_NOT_DEFINED_VALUE_DBLE) THEN
      
	    TR = (PRESSION/CTE_STD_PRESSURE) * 1.d-4 *       
     &     (84.35/WA_SIMU**4 - 1.225/WA_SIMU**5 + 1.4/WA_SIMU**6) 
	 
	 IF (TRACE) THEN
              WRITE (IDLOG,*,ERR=2010) "Molecular optical thickness"
     	      WRITE (IDLOG,*,ERR=2010) "deduced from the atmos pressure"
	 ENDIF
	   
      ENDIF
	
      IF (TRACE) THEN
         WRITE (IDLOG,*,ERR=2010) ""
         WRITE (IDLOG,*,ERR=2010) "*- Atmospheric parameters:"
	 WRITE (IDLOG,*,ERR=2010) "Nb layers: ", CTE_NT_ATM
	 WRITE (IDLOG,*,ERR=2010) "Pressure: ", PRESSION
	 WRITE (IDLOG,*,ERR=2010) "Molecular opt. thickness: ", TR
	 WRITE (IDLOG,*,ERR=2010) "Arosols  opt. thickness : ", TA
	 WRITE (IDLOG,*,ERR=2010) "Height scale for molecules: ", HR
	 WRITE (IDLOG,*,ERR=2010) "Height scale for aerosols : ", HA
	 WRITE (IDLOG,*,ERR=2010) 
     &	 "Single scattering albedo of aerosols: ", PIZAER  
	 WRITE (IDLOG,*,ERR=2010) ""
      ENDIF
      	
C* Initialisation des paramètres du profil
C  ----------------------------------------
       DO I=0,CTE_NT_ATM
	  PROF_ATM_T(I)=0.    ! épaisseur optique
	  PROF_ATM_Z(I)=0     ! altitude
	  PROF_TA(I)=0.	      ! épaisseur optique des aérosols
	  PROF_TR(I)=0.       ! épaisseur optique moléculaire
	  PROF_PCAER(I)=0.    ! proportion d'aérosols pour la diffusion
	  PROF_PCRAY(I)=0.    ! proportion de molécules pour la diffusion
       ENDDO
       
C*-----------------------------------------------------
C* CAS 1: Cas d'une couche sans diffusion moléculaire : 
C*        le profil est homogène.
C*-----------------------------------------------------
       IF (TR.LT.0.0001) THEN
       
          IF (TRACE) THEN
            WRITE (IDLOG,*,ERR=2010) "Case 1: no molecular scattering"
	    WRITE (IDLOG,*,ERR=2010) "Homogeneous profile"
          ENDIF
	  
	  !Init du profil en altitude
	  PROF_ATM_Z(0)=CTE_ALT_TOA
	  
          ! Profil sur les N couches de même épaisseur optique
          DO I=0,CTE_NT_ATM
	     
	     PROF_ATM_T(I)=I*TA/(CTE_NT_ATM)
             PROF_PCAER(I) = PIZAER
             PROF_PCRAY(I) = 0.000
	     
	     IF (I.NE.0) THEN
	     	PROF_ATM_Z(I)=-HA*DLOG(PROF_ATM_T(I)/TA)
	     ENDIF
	     
 	  ENDDO   
	  
       ENDIF

C*-----------------------------------------------------       
C* CAS 2: Cas d'une couche sans diffusion aérosol : 
C*        le profil est homogène
C*-----------------------------------------------------
       IF (TA.LT.0.0001) THEN

          IF (TRACE) THEN
            WRITE (IDLOG,*,ERR=2010) "Case 2: No aerosol scattering"
	    WRITE (IDLOG,*,ERR=2010) "Homogeneous profile "
          ENDIF
	  
	  !Init du profil en altitude
	  PROF_ATM_Z(0)=CTE_ALT_TOA
	         
       	  ! Profil sur les N couches de même épaisseur optique
          DO I=0,CTE_NT_ATM
	     
	     PROF_ATM_T(I)=I*TR/(CTE_NT_ATM)
             PROF_PCAER(I) = 0.000
             PROF_PCRAY(I) = 1.000
	     
	     IF (I.NE.0) THEN
	     	PROF_ATM_Z(I)=-HR*DLOG(PROF_ATM_T(I)/TR)
	     ENDIF
	     
 	  ENDDO   
	  
       ENDIF
              
C*---------------------------------------------------------------       
C* CAS 3: Cas d'une couche avec diffusion aérosol et moléculaire 
C*---------------------------------------------------------------
       IF ((TA.GE.0.0001).AND.(TR.GE.0.0001)) THEN
       
       IF (TRACE) THEN
         WRITE (IDLOG,*,ERR=2010) "Case 3 "
	 WRITE (IDLOG,*,ERR=2010) "Aerosols and molecular scattering "
	 WRITE (IDLOG,*,ERR=2010) "Layers of variable optical thickness"
       ENDIF
	        
C* Calcul de l'épaisseur optique et de la contribution à la 
C* diffusion des molécules et des aérosols du TOA au sol.
C----------------------------------------------------------------
	 !Init de l'altitude (altitude TOA)
	 Z = CTE_ALT_TOA
	 
	 !Calcul couche par couche
         DO  I=0,CTE_NT_ATM

            !Détermination de l'altitude pour l'épaisseur optique au niveau I.
	    !-----------------------------------------------------------------
            IF (I.NE.0) THEN
               CALL OSOAA_ALTI_LAYER(TA,HA,TR,HR,I,PROF_ATM_T(I-1),
     &	       		             PROF_PCRAY(I-1),Z)
	    ENDIF
	    
	    !Calcul des épaisseurs optique au niveau de la couche I.
	    !------------------------------------------------------
            PROF_TR(I)=TR * DEXP(-Z/HR)         ! épaisseur optique des aérosols
            PROF_TA(I)=TA * DEXP(-Z/HA)         ! épaisseur optique moléculaire
            PROF_ATM_T(I)=PROF_TR(I) + PROF_TA(I)   ! épaisseur optique totale
	    PROF_ATM_Z(I)=Z			! Altitude de la couche atmosphérique
	    
	    !Calcul des proportions moléculaires et aérosols pour la diffusion.
	    !-----------------------------------------------------------------
	    PC_DEN = PROF_TR(I)/HR + PROF_TA(I)/HA   ! dénominateur
	    PROF_PCRAY(I) = (PROF_TR(I)/HR) / PC_DEN             ! proportion moléculaire 
	    PROF_PCAER(I) =  PIZAER * (PROF_TA(I)/HA) / PC_DEN   ! proportion aérosols 
	    
         ENDDO
  	 
	   
       ENDIF

C* Création du fichier contenant le profil atmosphérique
C  -----------------------------------------------------
      OPEN(10,FILE=FICPROFIL_ATM_RES,ERR=2100)

      !Ecriture des informations de contenu du fichier      
      WRITE(10,80,ERR=2110)
      WRITE(10,81,ERR=2110)
      WRITE(10,82,ERR=2110)
      WRITE(10,83,ERR=2110)
      WRITE(10,84,ERR=2110)
      WRITE(10,85,ERR=2110)
      WRITE(10,86,ERR=2110)
      WRITE(10,87,ERR=2110)
      WRITE(10,88,ERR=2110)
      WRITE(10,89,ERR=2110)
        
      DO I=0,CTE_NT_ATM
         WRITE(10,100,err=2110)I,PROF_ATM_Z(I),PROF_ATM_T(I),
     &		       	       PROF_PCAER(I),PROF_PCRAY(I)
      ENDDO


C======================================================
C* Création du profil océanique
C======================================================   

      IF (TRACE) THEN
       WRITE (IDLOG,*,ERR=2010) ""
       WRITE (IDLOG,*,ERR=2010) ""
       WRITE (IDLOG,*,ERR=2010) "**** Calculation of the marine profile"
       WRITE (IDLOG,*,ERR=2010) "Wavelength: ",WA_SIMU
       WRITE (IDLOG,*,ERR=2010) "Sea depth: ",SEA_DEPTH
       WRITE (IDLOG,*,ERR=2010) ""       
       WRITE (IDLOG,*,ERR=2010) "Params for absorbing matters:"
       WRITE (IDLOG,*,ERR=2010) 
     &   "Coeff abs yellow substance (440nm): ",YS_A440
       WRITE (IDLOG,*,ERR=2010) 
     &  "Coeff spectral variation of yellow substance: ",YS_SWA
       WRITE (IDLOG,*,ERR=2010)  
     &   "Coeff abs detritus(440nm): ",DET_A440
       WRITE (IDLOG,*,ERR=2010)  
     &   "Coeff spectral variation of detritus: ",DET_SWA
       WRITE (IDLOG,*,ERR=2010) ""
       WRITE (IDLOG,*,ERR=2010) "Chlorophyll parameters:"
       WRITE (IDLOG,*,ERR=2010) "Surface concentration: ",CHL
       WRITE (IDLOG,*,ERR=2010) "Biomass constante: ",CHL_GP_BG
       WRITE (IDLOG,*,ERR=2010) "max depth: ",CHL_GP_DEEP
       WRITE (IDLOG,*,ERR=2010) 
     &   "Peak width of gaussian profile: ",CHL_GP_WIDTH
       WRITE (IDLOG,*,ERR=2010) "Profile type: ",IPHYTO_PROFIL
       WRITE (IDLOG,*,ERR=2010) ""       
       WRITE (IDLOG,*,ERR=2010) "Mineral-like particles parameters:"
       WRITE (IDLOG,*,ERR=2010) "Surface concentration: ",SED_CSED
       WRITE (IDLOG,*,ERR=2010) 
     & "refractive index (real part): ",SED_MRWA
       WRITE (IDLOG,*,ERR=2010) "Mean volume of a particle: ",SED_VSED
       WRITE (IDLOG,*,ERR=2010) "Scattering cross-section: ",SED_KMAT2
       WRITE (IDLOG,*,ERR=2010) "Single scattering albedo: ",SED_PIZ
       WRITE (IDLOG,*,ERR=2010) ""
       WRITE (IDLOG,*,ERR=2010) "Use of files :"     
       WRITE (IDLOG,*,ERR=2010) 
     &  "- Euphotic depth from Chlorophyll concentration:",
     &  CTE_FIC_EUPH_DEPTH
       WRITE (IDLOG,*,ERR=2010) 
     &  "- Absorption and scattering coefficients for water molecules:",
     &  CTE_FIC_MOL_SPECTRAL_DATA
       WRITE (IDLOG,*,ERR=2010) 
     &  "- Spectral data AP and EP for phytoplankton absorption:",
     &  CTE_FIC_PHYTO_SPECTRAL_DATA
       WRITE (IDLOG,*,ERR=2010) "under the directory: ", DIRFIC
       WRITE (IDLOG,*,ERR=2010) ""
      ENDIF ! Fin Trace
      
C*-----------------------------------------------------
C* Définition de la profondeur de mer
C*-----------------------------------------------------
      IF(SEA_DEPTH.EQ.CTE_NOT_DEFINED_VALUE_DBLE) THEN
      
        CALL OSOAA_SEA_EUPHOTIC_DEPTH(CHL,DIRFIC,EUPH_DEPTH,IER)
	IF (IER.EQ.-1) GOTO 3020
	SEA_DEPTH=EUPH_DEPTH
	
	!Sauvegarde dans la trace
	IF (TRACE) THEN
	   WRITE(IDLOG,*,ERR=2010) 
     &     "Zmax ajusted to the euphotic depth"
     	   WRITE (IDLOG,*,ERR=2010) "Sea depth: ",SEA_DEPTH
	ENDIF
      ENDIF
      
C*-----------------------------------------------------
C* Profil initial avec un pas de profondeur constant
C*-----------------------------------------------------

C* Détermination du nb de couches du profil
C  ----------------------------------------
      NZ=NINT(SEA_DEPTH/CTE_SEA_DEPTH_STEP)
      
C* On seuille le nombre de couches max
C  ----------------------------------------      
      IF (NZ.GT.CTE_NZ_MAX) THEN   
	  NZ=CTE_NZ_MAX  	  	  	  
      ENDIF
      
C* Détermination des profondeurs du profil océanique
C  --------------------------------------------------
      DO I=0,NZ
         PROF_Z(I)=I*CTE_SEA_DEPTH_STEP
      ENDDO
            
C* Calcul des coeffs de diffusion et d'absorption des molécules d'eau
C* Cette valeur est constante quelle que soit la profondeur
C  -------------------------------------------------------------------      
      CALL OSOAA_SEA_MOL_COEFFS(WA_SIMU,DIRFIC,MOL_ABS,MOL_DIF,IER)      
      IF (IER.EQ.-1) GOTO 3000

      IF (TRACE) THEN
         WRITE(IDLOG,*,ERR=2010) ""
	 WRITE(IDLOG,*,ERR=2010) 
     &	       "Radiative properties of sea molecules"
         WRITE(IDLOG,*,ERR=2010) "  - Wavelength (mic.) : ",WA_SIMU
	 WRITE(IDLOG,*,ERR=2010) "  - Absorption coef. : ",MOL_ABS
	 WRITE(IDLOG,*,ERR=2010) "  - Scattering coef. : ",MOL_DIF
         WRITE(IDLOG,*,ERR=2010) ""
      ENDIF ! Fin Trace       
      
C* Calcul des coeffs d'absorption des détritus et de la matière jaune
C* Absorption constante sur le profil. Ces matières sont purement
C* absorbandes: coefficients de diffusion nuls
C* attention, formules avec longueur d'onde en nm (WA_SIMU en microns)
C  -------------------------------------------------------------------    

      IF(YS_SWA.EQ.CTE_NOT_DEFINED_VALUE_DBLE) THEN     
         YS_SWA = CTE_DEFAULT_SPECTRAL_YS 
	 
	 IF (TRACE) WRITE(IDLOG,*,ERR=2010) 
     &     "Use of default value for YS spectral coef. : " , YS_SWA 	
     
      ENDIF
     
      IF(DET_SWA.EQ.CTE_NOT_DEFINED_VALUE_DBLE) THEN      
         DET_SWA = CTE_DEFAULT_SPECTRAL_DET 
	 
	 IF (TRACE) WRITE(IDLOG,*,ERR=2010) 
     &     "Use of default value for detritus spectral coef. : ",DET_SWA
	
      ENDIF
      
      
      YS_ABS=YS_A440*DEXP(-YS_SWA*(WA_SIMU*1000.-440.))
      DET_ABS=DET_A440*DEXP(-DET_SWA*(WA_SIMU*1000.-440.))
      
      IF (TRACE) THEN
         WRITE(IDLOG,*,ERR=2010) ""
	 WRITE(IDLOG,*,ERR=2010) "Calculated abs coef for YS : ",YS_ABS
	 WRITE(IDLOG,*,ERR=2010) 
     &	 "Calculated abs coef for detritus : ",DET_ABS
         WRITE(IDLOG,*,ERR=2010) ""
      ENDIF ! Fin Trace
      
C* Calcul du profil des coeffs de diffusion et d'absorption du phytoplancton
C  --------------------------------------------------------------------------
      CALL OSOAA_SEA_PHY_COEFFS(WA_SIMU,DIRFIC,NZ,PROF_Z,CHL,CHL_GP_BG,
     &				CHL_GP_DEEP,CHL_GP_WIDTH,
     &				IPHYTO_PROFIL,FICPHYTO_USER,
     &                          WATAB_MIN,WATAB_MAX,
     &				PROF_PHY_ABS,PROF_PHY_DIF,IER)
     
      IF (IER.EQ.-1) GOTO 3010
      
      IF (TRACE) THEN 
	 WRITE(IDLOG,*,ERR=2010) ""
	 WRITE(IDLOG,*,ERR=2010) 
     &	    "Phytoplankton scatering and absorption coeffs (z=0)"
	 WRITE(IDLOG,*,ERR=2010) 
     &	    "  - Absorption coef. : ",PROF_PHY_ABS(0)
	 WRITE(IDLOG,*,ERR=2010) 
     &	    "  - Scattering coef. : ",PROF_PHY_DIF(0)
         WRITE(IDLOG,*,ERR=2010) ""
	 
	 WANM=WA_SIMU*1000.
	 IF ((WANM.LT.WATAB_MIN).OR.(WANM.GT.WATAB_MAX)) THEN
            WRITE(IDLOG,*,ERR=2010) 
     &	       "     Warning : Out of the spectral range of tabulated"
	    WRITE(IDLOG,*,ERR=2010) 
     &	       "     data ==> Absorption is assumed to be null!"  
            WRITE(IDLOG,*,ERR=2010) 
     &	       "     --> Wavelength for simulation (nm) :", WANM
            WRITE(IDLOG,*,ERR=2010) 
     &	       "     --> Min tabulated wavelength (nm) :", WATAB_MIN
            WRITE(IDLOG,*,ERR=2010) 
     &	       "     --> Max tabulated wavelength (nm) :", WATAB_MAX
         ENDIF
	 
      ENDIF ! Fin Trace  
      
C* Init du profil océanique en profondeurs (coeffs à la surface Z=0)
C  -----------------------------------------------------------------

      ! Coeffs d'absorption et diffusion des sédiments
      ! En fonction de l'altitude
      CALL OSOAA_SEA_SED_COEFFS(PROF_Z(0),SED_CSED,
     &				SED_MRWA,SED_VSED,SED_KMAT2,   
     &				SED_PIZ,SED_ABS,SED_DIF,TRACE)
     
      ! Calcul du coefficient total d'extinction
      CEXT_TOT=MOL_ABS+MOL_DIF+SED_ABS+SED_DIF+YS_ABS+
     &	       DET_ABS+PROF_PHY_ABS(0)+PROF_PHY_DIF(0)
     
      ! Calcul du taux de mélange pour cette profondeur
      PROF_Z_MEL_MOL(0)=MOL_DIF/CEXT_TOT
      PROF_Z_MEL_SED(0)=SED_DIF/CEXT_TOT
      PROF_Z_MEL_PHY(0)=PROF_PHY_DIF(0)/CEXT_TOT
	 
      !Epaisseur optique nulle au niveau 0
      PROF_Z_T_SEA(0)=0.
     
	 
C* Calcul du profil océanique en profondeurs
C  -----------------------------------------
      
      DO I=1,NZ
	       
         ! Coeffs d'absorption et diffusion des sédiments
	 ! En fonction de l'altitude
	 CALL OSOAA_SEA_SED_COEFFS(PROF_Z(I),SED_CSED,
     &				   SED_MRWA,SED_VSED,SED_KMAT2,   
     &				   SED_PIZ,SED_ABS,SED_DIF,.FALSE.)
     	 
	 ! Calcul du coefficient total d'extinction
	 CEXT_TOT=MOL_ABS+MOL_DIF+SED_ABS+SED_DIF+YS_ABS+
     &	 	  DET_ABS+PROF_PHY_ABS(I)+PROF_PHY_DIF(I)
	 
	 ! Calcul du taux de mélange pour cette profondeur
	 PROF_Z_MEL_MOL(I)=MOL_DIF/CEXT_TOT
	 PROF_Z_MEL_SED(I)=SED_DIF/CEXT_TOT
	 PROF_Z_MEL_PHY(I)=PROF_PHY_DIF(I)/CEXT_TOT
	 
	 !Calcul de l'épaisseur optique d'extinction
	 PROF_Z_T_SEA(I)=PROF_Z_T_SEA(I-1)+CEXT_TOT*CTE_SEA_DEPTH_STEP
	     
      ENDDO !Fin boucle sur le profil en profondeurs

      IF (TRACE) THEN
       WRITE (IDLOG,*,ERR=2010) ""
       WRITE (IDLOG,*,ERR=2010) 
     &  "* Calculation of the profile versus the depth"
       WRITE (IDLOG,*,ERR=2010) "  Step (in m) : ",CTE_SEA_DEPTH_STEP
       WRITE (IDLOG,*,ERR=2010) ""
       WRITE (IDLOG,*,ERR=2010) "    I      Z      EP_OPT",
     &                          "          PCRAY     PCCHL     PCSED \n"
       DO I=0,NZ
         WRITE(IDLOG,200,err=2010)I,PROF_Z(I),PROF_Z_T_SEA(I),
     &		       	          PROF_Z_MEL_MOL(I),PROF_Z_MEL_PHY(I),
     &				  PROF_Z_MEL_SED(I)
       ENDDO
       WRITE (IDLOG,*,ERR=2010) ""
      ENDIF
      
C* Calcul de l'épaisseur optique max de la colonne d'eau
C  -----------------------------------------------------
      IF (PROF_Z_T_SEA(NZ).GT.CTE_SEA_T_LIMIT) THEN   
          ! On seuille la valeur max pour établir le profil détaillé
	  MAX_SEA_T=CTE_SEA_T_LIMIT  	
	  
	  IF (TRACE) THEN
	   WRITE(IDLOG,*,ERR=2010) "Warning: detailed profile ",
     &     "of optical thickness limited till ",CTE_SEA_T_LIMIT
	  ENDIF
	  
      ELSE     	
	  MAX_SEA_T=PROF_Z_T_SEA(NZ)	  	  
      ENDIF

C* Création du profil en épaisseur optique
C* Sauvegarde du profil dans un fichier
C  -----------------------------------------------------

      ! Pas en épaisseur optique
      SEA_T_STEP=(MAX_SEA_T-CTE_TRANS_OPT_THICKNESS)/(CTE_NT_SEA-1)

      IF (TRACE) THEN
       WRITE (IDLOG,*,ERR=2010) ""
       WRITE (IDLOG,*,ERR=2010) 
     &   "* Calculation of the profile versus the optical thickness"
       WRITE (IDLOG,*,ERR=2010) 
     &  "Max. opt. thick. of water column: ",MAX_SEA_T
       WRITE (IDLOG,*,ERR=2010) "Step of opt. thick.: ",SEA_T_STEP
       WRITE (IDLOG,*,ERR=2010) "Nb of layers: ",CTE_NT_SEA
       WRITE (IDLOG,*,ERR=2010) "Result file : ",FICPROFIL_SEA_RES
      ENDIF
      
      ! Ouverture du fichier
      OPEN(20,FILE=FICPROFIL_SEA_RES,ERR=3100)

      !Ecriture des informations de contenu du fichier      
      WRITE(20,189,ERR=2110)
      WRITE(20,190,ERR=2110)
      WRITE(20,191,ERR=2110)
      WRITE(20,192,ERR=2110)
      WRITE(20,193,ERR=2110)
      WRITE(20,194,ERR=2110)
      WRITE(20,195,ERR=2110)
      WRITE(20,196,ERR=2110)
      WRITE(20,197,ERR=2110)
      WRITE(20,198,ERR=2110)
      WRITE(20,199,ERR=2110)

            
      ! Initialisation
      PROF_SEA_T(0)=0
      
      ! Ecriture de la première ligne (Ep opt=0 et Z=0)
      WRITE(20,200,err=3110)0,0.,PROF_SEA_T(0),PROF_Z_MEL_MOL(0),
     &		       	    PROF_Z_MEL_PHY(0),PROF_Z_MEL_SED(0)
	    
      !Calcul du profil
      DO I=1,CTE_NT_SEA
      
      	     !Epaisseur optique de la couche océanique
	     PROF_SEA_T(I)=CTE_TRANS_OPT_THICKNESS+(I-1)*SEA_T_STEP
	     
	     !Recherche des niveaux du profil initial
	     !entourant ce niveau d'épaisseur optique
	     IF (PROF_SEA_T(I).GT.PROF_Z_T_SEA(NZ)) THEN
	        !Gestion dépassement par arrondi numérique
	        J=NZ
	     ELSE
	        !Cas nominal dans domaine du profil fin
	        J=1
	        DO WHILE(PROF_SEA_T(I).GT.PROF_Z_T_SEA(J))
		   J=J+1
      	        ENDDO
	     ENDIF
	     
	     !Calcul de coeffs pour l'interpolation linéaire
	     TT=(PROF_SEA_T(I)-PROF_Z_T_SEA(J-1))/
     & 	        (PROF_Z_T_SEA(J)-PROF_Z_T_SEA(J-1))
     
     	     !Calcul de la profondeur de la couche
	     Z=(1-TT)*PROF_Z(J-1)+TT*PROF_Z(J)
	     
	     !Calcul des taux de mélange en diffusion
	     MEL_MOL=(1-TT)*PROF_Z_MEL_MOL(J-1)+TT*PROF_Z_MEL_MOL(J)
	     MEL_SED=(1-TT)*PROF_Z_MEL_SED(J-1)+TT*PROF_Z_MEL_SED(J)
	     MEL_PHY=(1-TT)*PROF_Z_MEL_PHY(J-1)+TT*PROF_Z_MEL_PHY(J)
	
	     !Ecriture du profil dans un fichier
	     WRITE(20,200,err=3110)I,Z,PROF_SEA_T(I),MEL_MOL,
     &		       	       MEL_PHY,MEL_SED
     		
      ENDDO !Fin boucle sur le profil en épaisseur optique
	  
      
C* Fermeture fichiers
C-------------------
      CLOSE(IDLOG)	! Fermeture du fichier Trace
      CLOSE(10)		! Fermeture du fichier de profil atmosphérique
      CLOSE(20)		! Fermeture du fichier de profil océanique
      
C* Fin nominale 
C-------------------
      GOTO 9999
            
C* Cas d'erreur et retour du status -1 au programme appelant
C----------------------------------------------
      
 2000 WRITE(6,*) '  OSOAA_PROFILE : ERROR_2000 on logfile opening' 
      GOTO 9998             

 2010 WRITE(6,*) '  OSOAA_PROFILE : ERROR_2000 on logfile writing' 
      GOTO 9998

 2050 WRITE(6,*) '  OSOAA_PROFILE : ERROR_2050: '
      WRITE(6,*) '  => Error while getting OSOAA_ROOT variable'
      GOTO 9998

 2051 WRITE(6,*) '  OSOAA_PROFILE : ERROR_2051: '
      WRITE(6,*) '  => Length of $OSOAA_ROOT/fic is too long' 
      WRITE(6,*) '  => CTE_LENDIR has to be adjusted.' 
      GOTO 9998
            
 2100 WRITE(6,*) '  OSOAA_PROFILE : ERROR_2100 '
      WRITE(6,*) '      on atmospheric profile file opening'
      GOTO 9998 
 
 2110 WRITE(6,*) '  OSOAA_PROFILE : ERROR_2110 ' 
      WRITE(6,*) '      on atmospheric profile file writing'
      GOTO 9998 
                 
 3000 WRITE(6,*) '  OSOAA_PROFILE : ERROR_3000  '
      WRITE(6,*) '      on subroutine OSOAA_SEA_MOL_COEFFS'
      GOTO 9998

 3010 WRITE(6,*) '  OSOAA_PROFILE : ERROR_3010  '
      WRITE(6,*) '      on subroutine OSOAA_SEA_PHY_COEFFS'
      GOTO 9998

 3020 WRITE(6,*) '  OSOAA_PROFILE : ERROR_3020  '
      WRITE(6,*) '      on subroutine OSOAA_SEA_EUPH_DEPTH'
      GOTO 9998
                  
 3100 WRITE(6,*) '  OSOAA_PROFILE : ERROR_3100 '
      WRITE(6,*) '      on sea profile file opening'
      GOTO 9998 

 3110 WRITE(6,*) '  OSOAA_PROFILE : ERROR_2110 ' 
      WRITE(6,*) '      on sea profile file writing'
      GOTO 9998 
            
9998  IER=-1             
9999  RETURN   

C* Format
C--------- 
   80 FORMAT(19hATMOSPHERIC PROFILE)
   81 FORMAT(19h-------------------)
   82 FORMAT(20hColumns parameters :)
   83 FORMAT(46h  LEVEL   : Level I of the atmospheric profile)
   84 FORMAT(43h  ALT     : Altitude of the level I (in km))
   85 FORMAT(54h  TAU_EXT : Total extinction opt. thickness at level I)
   86 FORMAT(54h  AER_PC  : Mixing rate aer. for scattering in layer I)
   87 FORMAT(54h  RAY_PC  : Mixing rate mol. for scattering in layer I)
   88 FORMAT(55h-------------------------------------------------------)      
   89 FORMAT(44h  LEVEL  ALT(km)  TAU_EXT   AER_PC    RAY_PC)      
  100 FORMAT(2X,I4,F9.3,3(1X,F9.5))

  189 FORMAT(11hSEA PROFILE)
  190 FORMAT(11h-----------)
  191 FORMAT(20hColumns parameters :)
  192 FORMAT(38h  LEVEL   : Level I of the sea profile)
  193 FORMAT(44h  DEPTH   : Depth of the level I (in meters))
  194 FORMAT(54h  TAU_EXT : Total extinction opt. thickness at level I)
  195 FORMAT(54h  MOL_PC  : Mixing rate mol. for scattering in layer I)
  196 FORMAT(55h  PHY_PC  : Mixing rate phyto for scattering in layer I)
  197 FORMAT(53h  MLP_PC  : Mixing rate MLP for scattering in layer I)
  198 FORMAT(55h-------------------------------------------------------)      
  199 FORMAT(54h  LEVEL  DEPTH(m)  TAU_EXT      MOL_PC  PHY_PC  MLP_PC)       
  200 FORMAT(I5,1X,F9.3,1X,E14.7,1X,3(1X,F7.5))
 
 
      
      END      !FIN DE LA PROCEDURE OSOAA_PROFILE


C==============================================================================
C PROCEDURE: OSOAA_ALTI_LAYER
C ==========
C      Cette procédure estime l'altitude à attribuer au niveau I du profil
C      atmosphérique.
C      L'estimation dépend de l'épaisseur optique du niveau I-1 et de sa
C      contribution moléculaire.
C
C      A) La couche atmosphérique du sol au niveau I-1 est découpée en
C      sous couches de mêmes épaisseurs optiques. On estime ainsi une première
C      valeur de l'épaisseur optique du niveau I.
C
C      B) On évalue l'altitude correspondante à ce niveau I par une méthode de
C      dichotomie.
C
C      C) La proportion moléculaire est calculée pour le niveau I. Si elle
C      s'écarte de la proportion du niveau I-1 de plus de 75%, la variation
C      est trop importante. On réduit d'un facteur 2 le pas en épaisseur
C      optique entre les niveaux I-1 et I. On reprend l'estimation de
C      l'altitude correspondante au point B.
C
C Description des paramètres
C --------------------------
C   Molecules :
C      TR  (double)  (E)   Epaisseur optique rayleigh.
C      HR  (double)  (E)   Echelle de hauteur du profil moléculaire (km).
C
C   Aérosols :
C      TA  (double)  (E)   Epaisseur optique des aérosols.
C      HA  (double)  (E)   Echelle de hauteur du profil d'aérosols (km).
C
C   Informations sur le niveau I-1 :
C      I     (I4)           (E)  Numéro du niveau traite.
C      TI_M1  (double)      (E)  Epaisseur optique totale du niveau I-1.
C      PCRAY_M1    (double) (E)  Pourcentage moléculaire au niveau I-1.
C
C      ZX   (double) (S)  Altitude estimée pour le niveau I.
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
      SUBROUTINE OSOAA_ALTI_LAYER(TA,HA,TR,HR,I,TI_M1,PCRAY_M1,ZX)

      IMPLICIT NONE
 
C* Définition des variables       
C*-----------------------------------------

C*   Molecules :
      DOUBLE PRECISION TR	! Epaisseur optique rayleigh.
      DOUBLE PRECISION HR	! Echelle de hauteur du profil moléculaire.

C*   Aérosols :
      DOUBLE PRECISION TA	! Epaisseur optique des aérosols (non tronquée).
      DOUBLE PRECISION HA	! Echelle de hauteur du profil d'aérosols.

C*   Informations sur le niveau I-1 :
      DOUBLE PRECISION TI_M1	! Epaisseur optique totale du niveau I-1.
      DOUBLE PRECISION PCRAY_M1	! Pourcentage moléculaire au niveau I-1.

      DOUBLE PRECISION ZX	! Altitude estimée pour le niveau I.
      	
      DOUBLE PRECISION DT	! Pas en épaisseur optique.
      DOUBLE PRECISION TI	! Epaisseur optique estimée au niveau I.
      DOUBLE PRECISION ZMAX	! Altitude (km).
      DOUBLE PRECISION ZMIN
      DOUBLE PRECISION ZMOY	! Altitude moyenne.
      DOUBLE PRECISION TZMOY	! Epaisseur optique totale pour l'altitude Zmoy.
      DOUBLE PRECISION XD	! Ecart absolu.
      DOUBLE PRECISION DELTA
      DOUBLE PRECISION ECART

      INTEGER*4 I		 ! Numéro du niveau traite.


C-----------------------------------------------------------
C* A) Découpe de l'atmosphère entre la couche de transition
C* atmosphère / mer et le niveau I-1 en (NT-1)-(I-1) couches 
C* de mêmes épaisseurs optiques DT.
C*
C* Ep opt = TA + TR 
C-----------------------------------------------------------
      DT=2.*(TA+TR-TI_M1)/(CTE_NT_ATM-I+1)
      
  99  DT=DT/2.

C* Estimation de l'épaisseur optique du niveau I en fonction
C* de celle du niveau I-1 et de l'estimation du pas DT.
C------------------------------------------------------
      TI=TI_M1+DT
      
C------------------------------------------------------
C* B) Estimation de l'altitude correspondant à l'épaisseur 
C*    optique TI du niveau I-1.
C------------------------------------------------------

C*   Initialisation des altitudes .
C--------------------------------------
      ZMAX=CTE_ALT_TOA
      ZMIN=0.

C*   Initialisation
C---------------------------
  706 ZMOY=(ZMAX+ZMIN)/2.
  
C*   Epaisseur optique totale pour l'altitude Zmoy.
C----------------------------------------------------
      TZMOY=TA*DEXP(-ZMOY/HA)+TR*DEXP(-ZMOY/HR)
      
C*   Ecart absolu avec l'épaisseur optique estimée du niveau I.
C-------------------------------------------------------------
      XD=DABS(TI-TZMOY)
      
C*   Test de sortie si Zmoy correspond au niveau I.
C------------------------------------------------------
      IF(XD.LT..000001) GOTO 705
      
C*   Affinage de l'encadrement de l'altitude du niveau I.
C------------------------------------------------------
      IF(TI-TZMOY) 701,703,703
      
 701  ZMIN=ZMOY
      GOTO 706
      
 703  ZMAX=ZMOY
      GOTO 706
      
C*   Affectation du résultat
C------------------------------------------------------
 705  ZX=ZMOY
 
C------------------------------------------------------
C* C) Comparaison des proportions moléculaires entre 
C*    le niveau I-1 et celui en cours.
C------------------------------------------------------
      IF(TR.EQ.0.) GOTO 112
      
C*   Evaluation de la contribution moléculaire au niveau I 
C*   pour l'altitude ZX estimée.
C------------------------------------------------------
      DELTA=1./(1.+TA*HR/TR/HA*DEXP(ZX*(1./HR-1./HA)))
      
C*   Comparaison avec celle du niveau I-1. On réduit l'écart 
C*   entre le niveau I-1 et le niveau I si les contributions 
C*   moléculaires diffèrent de plus de 75%.
C-----------------------------------------------------------
      ECART=DABS((PCRAY_M1-DELTA)/PCRAY_M1)
      
      IF(ECART.GT.0.75) GOTO 99
      
 112  CONTINUE
 
      RETURN
      END	!FIN DE LA PROCEDURE OSOAA_ALTI_LAYER
      
    
    
C==============================================================================
C PROCEDURE: OSOAA_SEA_EUPHOTIC_DEPTH
C ==========
C      Cette procédure calcule la profondeur euphotique à partir de la
C      concentration de chlorophylle. Cette fonction utilise les données tabulées
C      de MOREL (1988)
C
C Description des paramètres
C --------------------------
C      CHL (double)  (E)   Concentration de la chlorophylle en surface (mg/m3)
C      DIRFIC (char*LENDIR)  (E)   Nom du répertoire contenant la table
C      EUPH_DEPTH  (double)  (S)   Profondeur euphotique (en m)
C      IER (int) (S) Code d'erreur
C 
C Description des fichiers utilisés
C ---------------------------------
C
C    Fichier source CTE_FIC_EUPH_DEPTH
C      ==>   contient la profondeur euphotique associée à une concentration 
C            de chlorophylle en surface
C
C		format = 1 ligne par concentration de chlorophylle (2 colonnes / ligne)
C		chaque ligne contient:
C			- la concentration de chlorophylle en surface (en mg.m-3)
C			- la profondeur euphotique (en m)
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     - La concentration de chlorophylle en entrée est en dehors des données tabulées
C     - Erreur à l'ouverture ou la lecture du fichier contenant les données tabulées
C
C==============================================================================
      SUBROUTINE OSOAA_SEA_EUPHOTIC_DEPTH(CHL,DIRFIC,EUPH_DEPTH,IER)

      IMPLICIT NONE
 
C* Définition des variables       
C*-----------------------------------------

C-- Variables globales      
      INTEGER*4  NROWS		! Nombre de lignes dans le fichier contenant la table
      INTEGER*4 I		! Indice de boucle
      DOUBLE PRECISION XX	! Valeur temporaire (pour interp linéaire)
            
C-- Chlorophylle
      DOUBLE PRECISION CHL	! (E) Concentration de la chlorophylle en surface (mg/m3)
      
C-- Données tabulées
      DOUBLE PRECISION TAB_CHL(1:CTE_NZ_MAX) ! Tableau de concentrations en chlorophylle 
      					     ! (en mg/m3)
      DOUBLE PRECISION TAB_Z(1:CTE_NZ_MAX)   ! Tableau de profondeurs (en m)

C-- Sortie
      DOUBLE PRECISION EUPH_DEPTH	! (S) Profondeur euphotique (en m)
      
C---- Pour le fichier
      CHARACTER*CTE_LENDIR DIRFIC       ! (E) Nom du répertoire contenant la table 
      CHARACTER*CTE_LENFIC2 FICDATA     ! Nom complet du fichier des données tabulées
      INTEGER*4 IFIN			! Indice du dernier caractère du répertoire
      
C---- Code d'erreur
      INTEGER*4 IER        		! Code d'erreur =0 si pas d'erreur, =-1 sinon
      
      
C* Initialisation                     
C*-----------------------------------------------------------------
      IER=0


C* Nom du fichier contenant les données tabulées
C*----------------------------------------------

      !Récupération du répertoire
      IFIN=INDEX(DIRFIC,' ')
      IFIN=IFIN-1
      IF (IFIN.LE.0) IFIN=CTE_LENDIR
                 
      !Nom complet du fichier
      FICDATA=DIRFIC(1:IFIN)//'/'//CTE_FIC_EUPH_DEPTH
      		
		
C* Calcul des coeffs par interpolation sur des données tabulées 
C*-------------------------------------------------------------

      !Lecture du fichier avec les coefficients
      OPEN(30,FILE=FICDATA,ERR=1000,STATUS='OLD')
      NROWS=0  
      DO I=1,CTE_NZ_MAX
         READ(30,*,err=1010,end=1020),TAB_CHL(I),TAB_Z(I)
	 NROWS=NROWS+1
      ENDDO
           
      !Test que la valeur de concentration en entrée est comprise dans la table
1020  IF (CHL.LT.TAB_CHL(1).
     &    OR.CHL.GT.TAB_CHL(NROWS)) GOTO 500
      	   
      !On recherche les concentrations de chlorophylle
      !entourant la donnée en entrée
      I=2
      DO WHILE(CHL.GT.TAB_CHL(I))
	I=I+1
      ENDDO
		
      !Calcul des coeffs à la longueur d'onde de simu par interpolation linéaire
      XX=(CHL-TAB_CHL(I-1))/(TAB_CHL(I)-TAB_CHL(I-1))
      EUPH_DEPTH=(1-XX)*TAB_Z(I-1)+XX*TAB_Z(I)

C* Fin nominale 
C-------------------
      CLOSE(30)
      GOTO 9999
            
C* Cas d'erreur et retour du status -1 au programme appelant  
C----------------------------------------------

 500  WRITE(6,*) '  OSOAA_SEA_EUPHOTIC_DEPTH : ERROR_500 '
      WRITE(6,*) '  concentration is outside bounds of table'
      GOTO 9998             

 1000 WRITE(6,*) '  OSOAA_SEA_EUPHOTIC_DEPTH : ERROR_1000 '
      WRITE(6,*) '  on Morel table file opening'
      GOTO 9998 
 
 1010 WRITE(6,*) '  OSOAA_SEA_EUPHOTIC_DEPTH : ERROR_1010 '
      WRITE(6,*) '  on Morel table file reading'
      GOTO 9998 
                  
9998  IER=-1                
9999  RETURN   

      
      END      !FIN DE LA PROCEDURE OSOAA_SEA_EUPHOTIC_DEPTH

        
	    
C==============================================================================
C PROCEDURE: OSOAA_SEA_MOL_COEFFS
C ==========
C      Cette procédure calcule par interpolation linéaire les coefficients
C      d'absorption et de diffusion des molécules d'eau.
C      Les coefficients sont obtenus à partir de données tabulées qui définissent
C      les coefficients en fonction de la longueur d'onde (de 200 nm a 900 nm)
C
C Description des paramètres
C --------------------------
C      WA  (double)  (E)   Longueur d'onde en microns.
C      DIRFIC (char*LENDIR) (E)  Nom du répertoire contenant la table
C      SIG_ABS  (double)  (S)   Coefficient d'absorption moléculaire
C      SIG_DIF  (double)  (S)   Coefficient de diffusion moléculaire.
C      IER (int) (S) Code d'erreur
C 
C Description des fichiers utilisés
C ---------------------------------
C
C    Fichier source CTE_FIC_MOL_SPECTRAL_DATA
C      ==>   contient les coefficients d'absorption et diffusion des molécules d'eau
C
C		format = 1 ligne par longueur d'onde (3 colonnes / ligne)
C		chaque ligne contient:
C			- la longueur d'onde (en nm)
C			- le coefficient d'absorption
C			- le coefficient de diffusion
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     - Longueur d'onde de simulation non comprise dans la table
C     - Erreur à l'ouverture ou la lecture du fichier contenant les données tabulées
C
C==============================================================================
      SUBROUTINE OSOAA_SEA_MOL_COEFFS(WA,DIRFIC,SIG_ABS,SIG_DIF,IER)

      IMPLICIT NONE
 
C* Définition des variables       
C*-----------------------------------------

C-- Variables globales      
      DOUBLE PRECISION WA	! (E) Longueur d'onde de simulation des luminances (microns)
      DOUBLE PRECISION WANM	! (E) Longueur d'onde de simulation des luminances (en nm)
      INTEGER*4  NROWS		! Nombre de lignes dans le fichier contenant la table
      INTEGER*4 I		! Indice de boucle
      DOUBLE PRECISION XX	! Valeur temporaire (pour interp linéaire)
            
C-- Molecules
      DOUBLE PRECISION SIG_ABS	! (S) Coefficient d'absorption moléculaire maritime
      DOUBLE PRECISION SIG_DIF	! (S) Coefficient de diffusion moléculaire maritime
      DOUBLE PRECISION TAB_WL(1:CTE_NBWA_MAX)	! Tableau de longueur d'onde (nm)
      DOUBLE PRECISION TAB_ABS(1:CTE_NBWA_MAX)	! Tableau de coeffs d'absorption
      DOUBLE PRECISION TAB_DIF(1:CTE_NBWA_MAX)	! Tableau de coeffs de diffusion

C---- Pour le fichier
      CHARACTER*CTE_LENDIR DIRFIC       ! (E) Nom du répertoire contenant la table 
      CHARACTER*CTE_LENFIC2 FICDATA     ! Nom complet du fichier des données tabulées
      INTEGER*4 IFIN			! Indice du dernier caractère du répertoire
      
C---- Code d'erreur
      INTEGER*4 IER        		! Code d'erreur =0 si pas d'erreur, =-1 sinon
      
      
C* Initialisation                     
C*-----------------------------------------------------------------
      IER=0
      
C* Nom du fichier contenant les données tabulées
C*----------------------------------------------

      !Récupération du répertoire
      IFIN=INDEX(DIRFIC,' ')
      IFIN=IFIN-1
      IF (IFIN.LE.0) IFIN=CTE_LENDIR
                 
      !Nom complet du fichier
      FICDATA=DIRFIC(1:IFIN)//'/'//CTE_FIC_MOL_SPECTRAL_DATA
      		
C* Lecture du fichier contenant les coefficients
C*----------------------------------------------

      !Lecture table + comptage des lignes
      OPEN(30,FILE=FICDATA,ERR=1000,STATUS='OLD')
      I=1  
       
   20 READ(30,*,ERR=1010,END=30) TAB_WL(I),TAB_ABS(I),TAB_DIF(I)	
         I=I+1
         IF (I.GT.CTE_NBWA_MAX) GOTO 1020         
      GOTO 20
   30 CLOSE(30)
      NROWS=I-1          	
	
C* Vérification que la longueur d'onde en entrée est incluse dans la table de coeffs     
      WANM=WA*1000.   ! Conversion en nanomètres  
      
      IF (WANM.LT.TAB_WL(1).
     &    OR.WANM.GT.TAB_WL(NROWS)) GOTO 500
            		
C* Calcul des coeffs par interpolation sur des données tabulées 
C*-------------------------------------------------------------
                
      !On recherche les longueurs d'onde de la table de coeffs
      !entourant la longueur d'onde de simulation
      I=2
      DO WHILE(WANM.GT.TAB_WL(I))
	I=I+1
      ENDDO
		
      !Calcul des coeffs à la longueur d'onde de simu par interpolation linéaire
      XX=(WANM-TAB_WL(I-1))/(TAB_WL(I)-TAB_WL(I-1))
      SIG_ABS=(1-XX)*TAB_ABS(I-1)+XX*TAB_ABS(I)
      SIG_DIF=(1-XX)*TAB_DIF(I-1)+XX*TAB_DIF(I)

C* Fin nominale 
C-------------------
      GOTO 9999
            
C* Cas d'erreur et retour du status -1 au programme appelant  
C----------------------------------------------

 500  WRITE(6,*) '  OSOAA_SEA_MOL_COEFFS : ERROR_500 '
      WRITE(6,*) '  wavelength is outside bounds of table coeffs :'
      WRITE(6,*)  FICDATA
      GOTO 9998             

 1000 WRITE(6,*) '  OSOAA_SEA_MOL_COEFFS : ERROR_1000 '
      WRITE(6,*) '  on molecular table coeffs file opening :'
      WRITE(6,*)  FICDATA
      GOTO 9998 
 
 1010 WRITE(6,*) '  OSOAA_SEA_MOL_COEFFS : ERROR_1010 '
      WRITE(6,*) '  on molecular table coeffs file reading :'
      WRITE(6,*)  FICDATA
      GOTO 9998 

 1020 WRITE(6,*) '  OSOAA_SEA_MOL_COEFFS : ERROR_1020 '
      WRITE(6,*) '  on molecular table coeffs file reading :' 
      WRITE(6,*)  FICDATA
      WRITE(6,*) "  --> Array size is too low :"
      WRITE(6,*) "      The number of data lines has to be < or = to"
      WRITE(6,*) "      CTE_NBWA_MAX =",CTE_NBWA_MAX
      GOTO 9998       
                               
9998  IER=-1                
9999  RETURN   

      
      END      !FIN DE LA PROCEDURE OSOAA_SEA_MOL_COEFFS
   
   
         
C==============================================================================
C PROCEDURE: OSOAA_SEA_PHY_COEFFS
C ==========
C      Cette procédure calcule le profil de coefficients d'absorption et de diffusion 
C      du phytoplancton. 
C      3 cas sont possibles:
C      - profil en concentration de chlorophylle constant,
C      - profil en concentration de chlorophylle de type gaussien,
C      - profil en concentration de chlorophylle fourni par l'utilisateur.
C      Le coefficient de diffusion est ensuite calculé pour chaque profondeur
C      en fonction de la concentration en chlorophylle. Le coefficient d'absorption
C      dépend aussi de coefficients AP et EP obtenus par interpolation sur des
C      données tabulées (en fonction de la longueur d'onde). Si la longueur d'onde
C      est en dehors du domaine couvert, l'absorption est posée nulle.
C
C      Les longueurs d'onde limites de la couverture spectrale sont des sorties de 
C      la routine pour permettre d'insérer dans le fichier Trace du code appelant
C      l'information que l'absorption est posée nulle si la longueur d'onde de 
C      simulation est hors domaine couvert par le fichier de données tabulées : 
C      WATAB_MIN et WATAB_MAX
C
C
C
C MOD:VERSION:1.1: 23/05/2018:
C     - Corrections pour l'usage du fichier de profil défini par l'utilisateur FICPHYTO_USER 
C         * Correction du premier indice lu dans le fichier FICPHYTO_USER :
C           1 au lieu de 0 pour être cohérent avec la taille des tables USER_Z(1:CTE_NZ_MAX) 
C           et USER_CHL(1:CTE_NZ_MAX).
C         * Nombre de lignes stockés dans NROWS_FICUSER au lieu de NROWS pour éviter un conflit
C           sur le contenu de cette variable avec le nombre de ligne du fichier FICDATA.
C           (contenant les coefficients AP, EP)
C         * Complément d'informations pour label 1500.
C
C     - Corrections pour la lecture du fichier des coefficients AP, EP de la chlorophylle FICDATA 
C         * Nombre de lignes stockés dans NROWS_FICDATA au lieu de NROWS pour éviter un conflit
C           sur le contenu de cette variable avec le nombre de ligne du fichier FICPHYTO_USER.
C
C
C
C Description des paramètres
C --------------------------
C      WA  (double)  (E)   Longueur d'onde en microns.
C      DIRFIC (char*LENDIR) (E)  Nom du répertoire contenant la table
C      Z   (double)  (S)   Profil en profondeur (en m)
C      NZ  (int)     (E)   Nombre de couches du profil
C      CHL (double)  (E)   Concentration de la chlorophylle en surface (mg/m3)
C      CHL_GP_BG    (double)  (E)   Constante de biomasse du profil gaussien 
C                                   de chlorophylle (mg/m3)
C      CHL_GP_DEEP  (double)  (E)   Profondeur max du profil gaussien de chlorophylle (m) 
C      CHL_GP_WIDTH (double)  (E)   Largeur du pic du profil gaussien de chlorophylle (m)
C      IPHYTO_PROFIL (int)    (E)   Indice de type de profil de phytoplancton
C      FICPHYTO_USER (char)   (E)   Nom du fichier utilisateur donnant le profil 
C                                   de chlorophylle (chemin complet)
C      WATAB_MIN 	      (S)   Longueur d'onde minimale des données spectrales 
C                                   tabulée (nanomètres)
C      WATAB_MAX 	      (S)   Longueur d'onde maximale des données spectrales 
C                                   tabulée (nanomètres)
C      SIG_ABS  (double*NZ)   (S)   Profil d'absorption du phytoplancton
C      SIG_DIF  (double*NZ)   (S)   Profil de diffusion du phytoplancton
C      IER      (int)         (S)   Code d'erreur
C 
C
C Description des fichiers utilisés
C ---------------------------------
C
C    Fichier source CTE_FIC_PHYTO_SPECTRAL_DATA
C      ==>   contient les coefficients AP et EP pour le phytoplancton
C
C		format = 1 ligne par longueur d'onde (3 colonnes / ligne)
C		chaque ligne contient:
C			- la longueur d'onde (en nm)
C			- le coeff AP (en m2.mg-1)
C			- le coeff EP 
C
C    Fichier utilisateur FICPHYTO_USER: 
C      ==>   contient le profil en chlorophylle fourni par l'utilisateur
C
C		format = 1 ligne par couche du profil (2 colonnes / ligne)
C		chaque ligne contient:
C			- l'altitude de la couche (en m)
C			- la concentration en chlorophylle (mg.m-3)			
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     - Longueur d'onde de simulation non comprise dans la table
C     - Erreur à l'ouverture ou la lecture du fichier contenant les données tabulées
C     - Profondeur non comprise dans le profil utilisateur
C     - Erreur à l'ouverture ou la lecture du fichier contenant le profil utilisateur
C     - Indice de type de profil non valide
C
C==============================================================================
      SUBROUTINE OSOAA_SEA_PHY_COEFFS(WA,DIRFIC,NZ,Z,CHL,CHL_GP_BG,
     &				      CHL_GP_DEEP,CHL_GP_WIDTH,
     &				      IPHYTO_PROFIL,FICPHYTO_USER,
     &				      WATAB_MIN,WATAB_MAX,
     &				      SIG_ABS,SIG_DIF,IER)

     
      IMPLICIT NONE
 
C* Définition des variables       
C*-----------------------------------------

C-- Variables globales      
      
      DOUBLE PRECISION WA    ! (E) Longueur d'onde de simulation des luminances (en microns)
      DOUBLE PRECISION WANM  ! (E) Longueur d'onde de simulation des luminances (en nm)
      INTEGER*4  NZ	        ! (E)  Nombre de couches dans le profil de profondeurs
      INTEGER*4  NROWS_FICUSER	 !  Nombre de lignes dans un fichier utilisateur de profil
                                    !  de concentration de chlorophylle
      INTEGER*4  NROWS_FICDATA	 !  Nombre de lignes dans un fichier de données
                                    !  spectrales d'absorption et diffusion de la chlorophylle
      DOUBLE PRECISION Z(0:CTE_NZ_MAX)	! (E)  Profil en profondeurs
      
      INTEGER*4 I	     ! Niveau du profil en entrée
      INTEGER*4 J	     ! Niveau du profil utilisateur
      DOUBLE PRECISION TMP   ! Valeur temporaire (contenu exp gaussienne)
      DOUBLE PRECISION XX    ! Valeur temporaire (pour interp linéaire)
      DOUBLE PRECISION ZZ    ! Valeur temporaire (pour interp linéaire)
          
C-- Chlorophylle

      DOUBLE PRECISION CHL	    ! (E) Concentration de la chlorophylle en surface 
      				    !     (mg/m3)
      DOUBLE PRECISION CHL_GP_BG    ! (E) Constante de biomasse du profil gaussien 
      				    !     de chlorophylle (mg/m3)
      DOUBLE PRECISION CHL_GP_DEEP  ! (E) Profondeur max. du profil gaussien de chloro. (m)
      DOUBLE PRECISION CHL_GP_WIDTH ! (E) Largeur du pic du profil gaussien de chloro. (m)
      INTEGER*2 IPHYTO_PROFIL       ! (E) Indice de type de profil de phytoplancton
      
      CHARACTER*CTE_LENFIC2 FICPHYTO_USER       ! (E) Nom du fichier de profil utilisateur 
      DOUBLE PRECISION USER_Z(1:CTE_NZ_MAX)	! Profondeurs du profil utilisateur (en m)
      DOUBLE PRECISION USER_CHL(1:CTE_NZ_MAX)	! Concentrations en chlorophylle 
      						! du profil utilisateur (mg/m3)
      
      DOUBLE PRECISION AP	! Coefficient AP à la longueur d'onde de simulation
      DOUBLE PRECISION EP	! Coefficient EP à la longueur d'onde de simulation
      DOUBLE PRECISION TAB_WL(1:CTE_NBWA_MAX)	! Tableau de longueur d'onde (nm)
      DOUBLE PRECISION TAB_AP(1:CTE_NBWA_MAX)	! Tableau de coeffs AP (m2/mg)
      DOUBLE PRECISION TAB_EP(1:CTE_NBWA_MAX)	! Tableau de coeffs EP
      DOUBLE PRECISION PROF_CHL(0:CTE_NZ_MAX)	! Profil de concentration en chlorophylle

      DOUBLE PRECISION WATAB_MIN 		! (S)  Longueur d'onde min dans table d'info 
        					!      spectrale (nm)
      DOUBLE PRECISION WATAB_MAX 	 	! (S)  Longueur d'onde max dans table d'info 
        					!      spectrale (nm)
      
      DOUBLE PRECISION SIG_ABS(0:CTE_NZ_MAX)	! (S)  Profil de coefficients d'absorption
      DOUBLE PRECISION SIG_DIF(0:CTE_NZ_MAX)	! (S)  Profil de coefficients de diffusion

C---- Pour le fichier
      CHARACTER*CTE_LENDIR DIRFIC       ! (E) Nom du répertoire contenant la table 
      CHARACTER*CTE_LENFIC2 FICDATA     ! Nom complet du fichier des données tabulées
      INTEGER*4 IFIN			! Indice du dernier caractère du répertoire
      
C---- Code d'erreur
      INTEGER*4 IER        		! Code d'erreur =0 si pas d'erreur, =-1 sinon
      
      
      
C* Initialisation                     
C*-----------------------------------------------------------------
      IER=0

C* Nom du fichier contenant les données tabulées
C*----------------------------------------------

      !Récupération du répertoire
      IFIN=INDEX(DIRFIC,' ')
      IFIN=IFIN-1
      IF (IFIN.LE.0) IFIN=CTE_LENDIR
                 
      !Nom complet du fichier
      FICDATA=DIRFIC(1:IFIN)//'/'//CTE_FIC_PHYTO_SPECTRAL_DATA

C* Lecture du fichier avec les coeffs AP et EP
C*----------------------------------------------

      !Lecture + recherche nombre de lignes
      OPEN(30,FILE=FICDATA,ERR=2000,STATUS='OLD')
      I=1  
      
   20 READ(30,*,ERR=2010,END=30) TAB_WL(I),TAB_AP(I),TAB_EP(I)
         I=I+1
         IF (I.GT.CTE_NBWA_MAX) GOTO 2020         
      GOTO 20
   30 CLOSE(30)
      NROWS_FICDATA=I-1       
           	
C* Profil de concentration en fonction de la profondeur       
C*-----------------------------------------
      IF (IPHYTO_PROFIL.EQ.1) THEN
      
        ! Profil homogène
      	DO I=0,NZ
         PROF_CHL(I)=CHL
        ENDDO
	
      ELSE IF (IPHYTO_PROFIL.EQ.2) THEN
      
      	! Profil gaussien
        DO I=0,NZ
	 TMP=-(Z(I)-CHL_GP_DEEP)**2/(2*CHL_GP_WIDTH**2)
         PROF_CHL(I)=CHL_GP_BG+CHL*DEXP(TMP)
        ENDDO 
	
      ELSE IF (IPHYTO_PROFIL.EQ.3) THEN
      
         !Lecture du fichier utilisateur
      	 OPEN(40,FILE=FICPHYTO_USER,ERR=1000,STATUS='OLD')  
	 NROWS_FICUSER=0
      	 DO I=1,CTE_NZ_MAX 	! MOD: 23/05/2018 : La première valeur de 
				! l'indice vaut 1 au lieu de 0
         	READ(40,*,err=1010,end=1020),USER_Z(I),USER_CHL(I)
		NROWS_FICUSER=NROWS_FICUSER+1
      	 ENDDO
	 CLOSE(40)
      
        
        !Vérification que le profil utilisateur contient les valeurs
	!min et max du profil en profondeurs en entrée
1020	IF (Z(0).LT.USER_Z(1).OR.Z(NZ).GT.USER_Z(NROWS_FICUSER)) THEN
           GOTO 1500
       ENDIF
	
      	!Calcul des concentrations sur les profondeurs fournies en entrée
	DO I=0,NZ

		!On recherche les couches du profil utilisateur
		!entourant la profondeur du profil
		J=2
		DO WHILE(Z(I).GT.USER_Z(J))
		   J=J+1
		ENDDO
		
		!Calcul par interpolation linéaire de la concentration en chlorophylle
		ZZ=(Z(I)-USER_Z(J-1))/(USER_Z(J)-USER_Z(J-1))
		PROF_CHL(I)=(1-ZZ)*USER_CHL(J-1)+ZZ*USER_CHL(J)
		
        ENDDO
	
      ELSE
      	 !Erreur type de profil invalide
         GOTO 1600
      
      ENDIF
   
   
      
C* Calcul des coefficients AP et EP en fonction de la longueur d'onde
C*-------------------------------------------------------------------

      ! Conversion de la longueur d'onde en nanomètres
      WANM=WA*1000.   
      
      ! Longueur d'onde min et max des données tabulées dans FICDATA
      WATAB_MIN = TAB_WL(1)
      WATAB_MAX = TAB_WL(NROWS_FICDATA)

      IF ((WANM.LT.WATAB_MIN).OR.(WANM.GT.WATAB_MAX)) THEN
         !Cas : longueur d'onde en entrée en dehors de la table de coeffs
	 ! ==> On suppose nulle l'absorption du phytoplancton
	 AP=0.
	 EP=1.
	 
	 WRITE(6,*) "Warning : Exceeding the spectral range in file of "
	 WRITE(6,*) "          phytoplankton absorption coef.:", FICDATA
        WRITE(6,*) "    - Radiance simulation wavelength (nm) :", WANM
	 WRITE(6,*) "    - File spectral range (nm):", WATAB_MIN,
     &	                                               WATAB_MAX
	 WRITE(6,*) " ==> Phytoplankton absorption is FORCED to be null"
            
      ELSE
         !Cas : longueur d'onde en entrée est incluse dans la table de coeffs
	    
         !On recherche les longueurs d'onde de la table de coeffs
         !entourant la longueur d'onde de simulation
         I=2
         DO WHILE(WANM.GT.TAB_WL(I))
	   I=I+1
         ENDDO
		
         !Calcul des coeffs à la longueur d'onde de simu par interpolation linéaire
         XX=(WANM-TAB_WL(I-1))/(TAB_WL(I)-TAB_WL(I-1))
         AP=(1-XX)*TAB_AP(I-1)+XX*TAB_AP(I)
         EP=(1-XX)*TAB_EP(I-1)+XX*TAB_EP(I)
	 
      ENDIF		

C* Calcul des profils de coeffs d'absorption et de diffusion
C*-------------------------------------------------------------------
      DO I=0,NZ
         SIG_DIF(I)=0.3*(550/WANM)*PROF_CHL(I)**0.62
         SIG_ABS(I)=AP*PROF_CHL(I)**EP
      ENDDO
      
      
C* Fin nominale 
C-------------------
      GOTO 9999
            
C* Cas d'erreur et retour du status -1 au programme appelant  
C----------------------------------------------

 500  WRITE(6,*) '  OSOAA_SEA_PHY_COEFFS : ERROR_500 '
      WRITE(6,*) '  wavelength is outside bounds of phyto table coeffs:'
      WRITE(6,*)  FICDATA
      GOTO 9998             

 1000 WRITE(6,*) '  OSOAA_SEA_PHY_COEFFS : ERROR_1000 '
      WRITE(6,*) '  on user phyto profile file opening'
      GOTO 9998 
 
 1010 WRITE(6,*) '  OSOAA_SEA_PHY_COEFFS : ERROR_1010 '
      WRITE(6,*) '  on user phyto profile file reading'
      GOTO 9998 
      
 1500 WRITE(6,*) '  OSOAA_SEA_PHY_COEFFS : ERROR_1500 '
      WRITE(6,*) '  depth is outside bounds of user profile depths:'
      WRITE(6,*) 
     &'  - User profile for phytoplancton is defined between depths:',
     &   USER_Z(1), USER_Z(NROWS_FICUSER)
      WRITE(6,*) 
     &'  - while the complete sea profile is defined between depths:',
     &   Z(0),Z(NZ)
      GOTO 9998             

 1600 WRITE(6,*) '  OSOAA_SEA_PHY_COEFFS : ERROR_1600 '
      WRITE(6,*) '  incorrect phytoplancton profile type'
      GOTO 9998  
      
 2000 WRITE(6,*) '  OSOAA_SEA_PHY_COEFFS : ERROR_2000 '
      WRITE(6,*) '  on phyto table coeffs file opening :'
      WRITE(6,*)  FICDATA
      GOTO 9998 
      
 2010 WRITE(6,*) '  OSOAA_SEA_PHY_COEFFS : ERROR_2010 '
      WRITE(6,*) '  on phyto table coeffs file reading :' 
      WRITE(6,*)  FICDATA
       
 2020 WRITE(6,*) '  OSOAA_SEA_PHY_COEFFS : ERROR_2010 '
      WRITE(6,*) '  on phyto table coeffs file reading :' 
      WRITE(6,*)  FICDATA
      WRITE(6,*) "  --> Array size is too low :"
      WRITE(6,*) "      The number of data lines has to be < or = to"
      WRITE(6,*) "      CTE_NBWA_MAX =",CTE_NBWA_MAX
      GOTO 9998 
    
9998  IER=-1                
9999  RETURN   

  
      END      !FIN DE LA PROCEDURE OSOAA_SEA_PHY_COEFFS
     
     
         
C==============================================================================
C PROCEDURE: OSOAA_SEA_SED_COEFFS
C ==========
C      Cette procédure calcule les coefficients d'absorption et de diffusion
C      des sédiments.
C
C Description des paramètres
C --------------------------
C  Variables globales:
C      Z   (double)  (E)   Profondeur en m
C
C  Sédiments:
C      CSED  (double) (E)   Concentration des sédiments en surface
C      MRWA  (double) (E)   Partie réelle de l'indice de réfraction moyen 
C                           des sédiments	 
C      VSED  (double) (E)   Volume moyen d'une particule de type "sédiment" (mic^3)
C      KMAT2 (double) (E)   Section efficace de diffusion d'une particule de type 
C                           "sédiment" (mic^2)
C			    (à la longueur d'onde de simulation des luminances)
C      PIZ   (double) (E)   Albédo de simple diffusion des sédiments
C			    (à la longueur d'onde de simulation des luminances)
C      SIG_ABS  (double)  (S)   Coefficient d'absorption des sédiments
C      SIG_DIF  (double)  (S)   Coefficient de diffusion des sédiments
C      TRACE    (logical) (E)   Indicateur de production de traces
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     Erreur d'écriture dans le fichier Trace
C
C==============================================================================
      SUBROUTINE OSOAA_SEA_SED_COEFFS(Z,CSED,MRWA,VSED,KMAT2,
     &				      PIZ,SIG_ABS,SIG_DIF,TRACE)

      IMPLICIT NONE

C-- Traces      
      INTEGER*2 IDLOG		! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_PROFILE)
       
C* Définition des variables       
C*-----------------------------------------

C-- Variables globales      
      DOUBLE PRECISION Z	! (E) Profondeur de la couche océanique (en m)
      LOGICAL TRACE		! (E) Indicateur de production de traces
      
C-- Sédiments
      DOUBLE PRECISION CSED     ! (E) Concentration des sédiments en surface (mg/litre)
      DOUBLE PRECISION MRWA	! (E) Partie réelle de l'indice de réfraction moyen 
      				!     des sédiments
      DOUBLE PRECISION VSED     ! (E) Volume moyen d'une particule de type "sédiment" 
      				!     (mic^3).
      DOUBLE PRECISION KMAT2    ! (E) Section efficace de diffusion d'une particule 
      				!     de type "sédiment" (mic^2)
      DOUBLE PRECISION PIZ	! (E) Albédo de simple diffusion des sédiments 
      DOUBLE PRECISION SIG_ABS	! (S) Coefficient d'absorption des sédiments
      DOUBLE PRECISION SIG_DIF	! (S) Coefficient de diffusion des sédiments
      DOUBLE PRECISION SIG_EXT	! (S) Coefficient d'extinction des sédiments
      DOUBLE PRECISION DENS	! Masse volumique des sédiments (en mg/litre)


      
C*   Calcul de la masse volumique de sédiments
C------------------------------------------------------
      DENS=1000*(8.036*MRWA-6.826)
      
C*   Calcul du coefficient de diffusion
C    La concentration est supposée constante sur le profil
C----------------------------------------------------------
C     Le facteur 10e3 est empirique. Il a été introduit
C     pour correspondre aux résultats de OSOA. 			
C   
      SIG_DIF=1.d+3*CSED*KMAT2/(DENS*VSED)
      
C*   Calcul du coefficient d'extinction
C    Calculé à partir de l'albédo de simple diffusion
C----------------------------------------------------------
      SIG_EXT=SIG_DIF/PIZ

C*   On en déduit le coefficient d'absorption
C------------------------------------------------------
      SIG_ABS=SIG_EXT-SIG_DIF
     
      IF (TRACE) THEN
          WRITE (IDLOG,*,ERR=700) ""
	  WRITE (IDLOG,*,ERR=700) "Calculation of MLP  coeff "
	  WRITE (IDLOG,*,ERR=700) "--------------------------"
	  WRITE (IDLOG,*,ERR=700) "Refractive index (real part): ",MRWA
	  WRITE (IDLOG,*,ERR=700) "Scattering cross-section: ",KMAT2
	  WRITE (IDLOG,*,ERR=700) "Single scattering albedo: ",PIZ
          WRITE (IDLOG,*,ERR=700) "Particle density: ",DENS
	  WRITE (IDLOG,*,ERR=700) "Particle concentration: ",CSED
	  WRITE (IDLOG,*,ERR=700) "Mean particle volume: ",VSED
	  WRITE (IDLOG,*,ERR=700) "10-3*CSED/(DENS*VSED) : ",
     &	  			   1.d-3*CSED/(DENS*VSED)
	  WRITE (IDLOG,*,ERR=700) "MLP scattering coef. : ",SIG_DIF
	  WRITE (IDLOG,*,ERR=700) "MLP extinction coef. : ",SIG_EXT
	  WRITE (IDLOG,*,ERR=700) "MLP absorption coef. : ",SIG_ABS
	  
	  WRITE (IDLOG,*,ERR=700) ""
      ENDIF !Fin Trace
      
      GOTO 800

C* Cas d'erreur
C--------------      
700   WRITE(6,*) '  OSOAA_SEA_SED_COEFFS : ERROR_700 on logfile writing'
      GOTO 800
         
800   RETURN
      END      !FIN DE LA PROCEDURE OSOAA_SEA_SED_COEFFS
