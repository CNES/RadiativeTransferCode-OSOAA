C******************************************************************************
C* FICHIER: OSOAA_AEROSOLS.F
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
C* ROLE: Calcul des coefficients des matrices de phase des aérosols
C*       et des sections efficaces de diffusion et d'extinction.
C*       ==> Génération des fichiers de matrice de phase.
C*
C* AUTEURS:  
C*
C*   Fondement algorithmique pour les calculs de MIE et le développement
C*   des fonctions de phase en fonctions de Legendre :
C*      - Laboratoire d'Optique Atmosphérique (LOA, Villeneuve d'Ascq) 
C*          Jean-Luc Deuzé, Maurice Herman, Richard Santer
C*        ==> code des Ordres Successifs de Diffusion 
C*            (version atmosphérique et glitter).
C*      - Communication & Systèmes (CS, Toulouse) pour le compte du CNES
C*          Bruno Lafrance
C*          ==> code SOS (version industrialisée du code des OS du LOA).
C*
C*   Compléments de spécification de besoin :
C*     Code initial ==> code OSOA (version mer plate). 
C*     Code OSOAA  
C         Laboration d'Océanographie de Villefranche sur Mer (LOV)
C*          Malik Chami      
C*
C*   Reprise du codage et développements complémentaires : 
C*      - Communication & Systemes (CS, Toulouse)
C*          Bruno Lafrance
C*
C*
C* ROUTINES COUVERTES PAR LE FICHIER : 
C*   - OSOAA_AEROSOLS
C*   - SOS_INIT_PARAMWMO
C*   - SOS_INIT_PARAMSF
C*
C*
C*
C* MOD:VERSION:1.0: 02/04/2015: Initial version of the OSOAA code
C* MOD:VERSION:1.1: 15/06/2015: 
C*         - All comments for log files are now in english
C*         - Changing the way of reading the formatted files (Angles, ...)
C*           to be compatible with the compiler gfortran.
C* MOD:VERSION:1.2: 25/03/2016: 
C*         - Removal of screen information when MIE file has already been calculated
C*         - Change of reading the string FICANGLES_USER in file FICANGLES
C*           (for gfortran compilation - linux system)                                      
C* MOD:VERSION:1.3: 03/11/2016:
C*     Changes to be compliant with a f77 compilation
C*         - CALL FLUSH() changed to CALL FLUSH(6)
C*         - Change a variable type to read the file FICANGLES
C*                                   
C*   
C******************************************************************************

C----------------------------------------------------------------------------
C Définition des constantes  
C---------------------------------------------------------------------------- 
C Constantes utilisées : 
C
C    CTE_LENDIR :  Longueur du nom d'un répertoire
C    CTE_LENFIC1 : Longueur des noms de fichiers sans arborescence.
C    CTE_LENFIC2 : Longueur des noms de fichiers avec arborescence.
C    CTE_MIE_NBMU_MAX : Nombre maximal d'angles de Gauss positifs 
C                       pour les calculs de MIE
C    CTE_OS_NB_MAX : Ordre maximal de la décomposition en séries de Fourier
C    et ordre limite de la décomposition en polynômes de Legendre des fonctions
C    de phase et de la luminance.
C    CTE_ALPHAMAX_WMO_DL : Valeur limite du paramètre de taille pour le calcul
C                          des fichiers du composant Dust-Like de la WMO.
C    CTE_ALPHAMAX_WMO_WS : Idem pour le composant Water Soluble.
C    CTE_ALPHAMAX_WMO_OC : Idem pour le composant Oceanic.
C    CTE_ALPHAMAX_WMO_SO : Idem pour le composant Soot.
C    CTE_ALPHAMAX_SF_SR : Idem pour le composant Small Rural de Shettle & Fenn.
C    CTE_ALPHAMAX_SF_SU : Idem pour le composant Small Urban de Shettle & Fenn.
C    CTE_COEF_NRMAX : Constante pour la détermination du paramètre de taille max.
C    CTE_NOT_DEFINED_VALUE_DBLE : Identifiant de valeur double non définie
C                                 par l'utilisateur.
C    CTE_MAXNB_ANG_EXT : Taille maximale des tableaux de fonctions de phase externes.
C    CTE_WAMIN : Longueur d'onde minimale applicable aux traitements.
C    CTE_JUNGE_SLOPE_COR : Correctif de la pente de loi de Junge dans le cas 
C                          ou l'utilisateur aurait saisi 3.0.
C
C    CTE_AER_MU1_TRONCA : cos de l'angle Theta1 pour l'estimation du segment de
C                         troncature de la fonction de phase des aérosols.
C    CTE_AER_MU2_TRONCA : cos de l'angle Theta2 pour l'estimation du segment de
C          
C    CTE_AER_DATAWMO : Nom du fichier contenant les informations de granulométrie
C                      des composants des modèles WMO.
C
C    CTE_AER_DATASF  : Nom du fichier contenant les informations de granulométrie
C                      en fonction de l'humidité relative des modèles Shettle & Fenn.
C
C    CTE_AER_SR_SF   : Nom du fichier contenant les informations d'indice de réfraction
C                      en fonction de la longueur d'onde et de l'humidité relative 
C                      pour le composant Small Rural de Shettle & Fenn.      
C    CTE_AER_LR_SF   : Idem pour le composant Large Rural de Shettle & Fenn.
C    CTE_AER_SU_SF   : Idem pour le composant Small Urban de Shettle & Fenn.
C    CTE_AER_LU_SF   : Idem pour le composant Large Urban de Shettle & Fenn.
C    CTE_AER_OM_SF   : Idem pour le composant Oceanic Model de Shettle & Fenn.   
C        
C Constantes spécifiques :
C    INCTE_IDLOG_PARTICULES : ID du ficher Trace de la routine
C    INCTE_PI : Valeur de PI (calculé par la machine)
C    LENLINE  : Longueur maximale d'une ligne de données lue dans un fichier
C----------------------------------------------------------------------------
#include "OSOAA.h"
#define INCTE_IDLOG_PARTICULES 99
#define INCTE_PI DACOS(-1.D+00)
#define LENLINE 1000


C==============================================================================
C PROCEDURE: OSOAA_AEROSOLS
C ==========
C
C      Programme de génération des fichiers de propriétés radiatives des aérosols
C      ==> Coefficients de diffusion et d'extinction, matrice de phase.
C
C
C Description des paramètres 
C ------------------------------
C
C     FICANGLES (CHAR*CTE_LENFIC2)  (E) :  Nom complet du fichier des angles utilisés 
C                                          pour les calculs de Mie
C                 			   (répertoire + nom fichier + extension)
C
C     WA (DOUBLE)  (E) :  Longueur d'onde sélectionnée pour les calculs des propriétés
C                         radiatives des aérosols (en microns).
C
C     AOT (DOUBLE) (E) :  Epaisseur optique des aérosols (non tronquée) 
C                         pour la longueur d'onde WA
C
C     WAREF (DOUBLE)  (E) :  Longueur d'onde de référence de l'ep opt des aérosols (microns)
C
C     AOT_REF (DOUBLE) (E) :  Epaisseur optique des aérosols (non tronquée) 
C                             pour la longueur d'onde WAREF
C
C     ITRONC (I4)  (E) :  Option de troncature  (1 pour la réaliser).
C                               
C     IMOD   (I2)  (E) :  Granulométrie mono-modal ou multi-modale.
C      		            0 : Modèles mono-mode
C			    1 : Modèles WMO
C			    2 : Modèles Shettle & Fenn
C		            3 : Modèles bimodaux de LND
C                           4 : Utilisation de fonctions de phase externes
C
C     RN_MMD (DOUBLE) (E) : Partie réelle de l'indice de réfraction pour modèle mono-modal
C                           à la longueur d'onde de simulation des luminances (WA)
C
C     IN_MMD (DOUBLE) (E) : Partie imaginaire de l'indice de réfraction pour modèle 
C                           mono-modal à la longueur d'onde de simulation des luminances (WA)
C
C     IGRANU_MMD (I4)  (E) :  Indice de type de granulométrie pour modèle mono-modal
C                          1 : LND (paramètres : rayon modal en microns et variance);  
C                          2 : Loi de Junge (paramètres : rayon minimal et maximal 
C                              		     en microns et pente). 
C
C     VARGRANU1_MMD  (DOUBLE) (E) :  1er paramètre de granulométrie pour modèle mono-modal
C     VARGRANU2_MMD  (DOUBLE) (E) :  2nd paramètre de granulométrie pour modèle mono-modal
C     VARGRANU3_MMD  (DOUBLE) (E) :  3eme paramètre de granulométrie pour modèle mono-modal	
C	 		If LND       : modal radius and standard deviation
C	  		If Junge law (cas 1) : min radius and slope
C	  		If Junge law (cas 2) : min radius, slope and max radius
C        
C     IMODELE_WMO (I4)  (E) :  Indice de choix du modèle WMO
C      		        1 : Modèle WMO Continental
C			2 : Modèle WMO Maritime
C			3 : Modèle WMO Urban
C			4 : Modèle WMO "user definition"
C
C     C_WMO_DL  (DOUBLE) (E) :  Pourcentage volumique C des particules DL (WMO).
C				pour le cas IMODELE_WMO = 4 (définition par l'utilisateur)
C
C     C_WMO_WS  (DOUBLE) (E) :  Pourcentage volumique C des particules WS (WMO).
C				pour le cas IMODELE_WMO = 4 (définition par l'utilisateur)
C
C     C_WMO_OC  (DOUBLE) (E) :  Pourcentage volumique C des particules OC (WMO).
C				pour le cas IMODELE_WMO = 4 (définition par l'utilisateur)
C
C     C_WMO_SO  (DOUBLE) (E) :  Pourcentage volumique C des particules SO (WMO).
C				pour le cas IMODELE_WMO = 4 (définition par l'utilisateur)
C        
C     IMODELE_SF (I4)  (E) :  Indice de choix du modèle Shettle & Fenn
C      		        1 : Modèle SF Tropospheric
C			2 : Modèle SF Urban
C			3 : Modèle SF Maritime
C			4 : Modèle SF Coastal
C
C     RH  (DOUBLE) (E) :   Humidité relative pour modèle Shettle & Fenn
C
C     MODE_PARAM_BILND (I4)  (E) : Indice de type de description du mélange bimodal LND. 
C      		        1 : Utilisation des concentrations volumiques de l'utilisateur
C			2 : Utilisation du rapport AOT_coarse / AOT_totale 
C
C     USER_CV_COARSE  (DOUBLE) (E) : Concentration volumique de l'utilisateur 
C 				     pour le mode "LND coarse".  
C				     (cas MODE_PARAM_BILND = 1)
C
C     USER_CV_FINE  (DOUBLE) (E) :   Concentration volumique de l'utilisateur 
C 				     pour le mode "LND fine".  
C				     (cas MODE_PARAM_BILND = 1) 
C
C     RTAUct_WAREF  (DOUBLE) (E) :   rapport AOT_coarse / AOT_tot pour la longueur 
C				     d'onde WAREF. 
C				     (cas MODE_PARAM_BILND = 1) 
C	 
C     BMD_CM_MRWA  (DOUBLE) (E) :   Partie réelle de l'indice de réfraction des 
C                                   particules du mode "LND coarse" pour WA de simulation
C	 
C     BMD_CM_MIWA  (DOUBLE) (E) :   Partie imaginaire de l'indice de réfraction des 
C                                   particules du mode "LND coarse" pour WA de simulation
C
C     BMD_CM_MRWAREF  (DOUBLE) (E) : Partie réelle de l'indice de réfraction des 
C                                    particules du mode "LND coarse" pour WAREF de référence
C	 
C     BMD_CM_MIWAREF  (DOUBLE) (E) : Partie imaginaire de l'indice de réfraction des 
C                                    particules du mode "LND coarse" pour WAREF de référence
C
C     BMD_CM_RMODAL  (DOUBLE) (E) : Rayon modal de la "LND coarse" (micron)
C
C     BMD_CM_VAR  (DOUBLE) (E)    : Variance de la "LND coarse"
C
C	 
C     BMD_FM_MRWA  (DOUBLE) (E) :   Partie réelle de l'indice de réfraction des 
C                                   particules du mode "LND fine" pour WA de simulation
C	 
C     BMD_FM_MIWA  (DOUBLE) (E) :   Partie imaginaire de l'indice de réfraction
C                                   des particules du mode "LND fine" pour WA de simulation
C
C     BMD_FM_MRWAREF  (DOUBLE) (E) : Partie réelle de l'indice de réfraction des 
C                                    particules du mode "LND fine" pour WAREF de référence
C	 
C     BMD_FM_MIWAREF  (DOUBLE) (E) : Partie imaginaire de l'indice de réfraction des 
C                                    particules du mode "LND fine" pour WAREF de référence
C
C     BMD_FM_RMODAL  (DOUBLE) (E) : Rayon modal de la "LND fine" (micron)
C
C     BMD_FM_VAR  (DOUBLE) (E)    : Variance de la "LND fine"
C 
C     FICEXTDATA  (CHAR*CTE_LENFIC2)  (E) : Nom du fichier de données externes de fonctions
C                    de phase et paramètres radiatifs (coefficients extinction et diffusion)
C                    (répertoire + nom fichier + extension)
C  
C     DIRMIE  (CHAR*CTE_LENDIR)	 (E) : Répertoire de stockage des fichiers MIE
C
C     FICMIE_LOG (CHAR*CTE_LENFIC2) (S) : Nom du fichier Trace des calculs de MIE
C
C     FICGRANU (CHAR*CTE_LENFIC2) (S) : Nom complet du fichier résultat GRANU
C                 			(répertoire + nom fichier + extension)
C	  
C     FICAER_LOG (CHAR*CTE_LENFIC2) (S) : Nom du fichier de log 
C
C     KMAT1  (DOUBLE)          (S) : Section efficace d'extinction (micron^2)  
C				     pour WA de simulation. Non tronquée.
C
C     PIZ (DOUBLE)             (S) : Albédo de simple diffusion. Non tronqué.
C
C     IER (I4)                 (S) : indice d'erreur : 0 si pas d'erreur, -1 sinon.
C  
C
C Résultat fourni par la procédure
C --------------------------------
C 
C    Un fichier de paramètres radiatifs (sections efficaces, etc)
C    et de coefficients de matrice de phase des aérosols.
C
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
C    Fichier FICANGLES:
C      ==>   contient les informations suivantes
C		NB_TOTAL_ANGLES :  nb d'angles total à utiliser
C               NB_GAUSS_ANGLES :  nb d'angles de Gauss
C               ANGLES_USERFILE :  Nom du fichier des angles utilisateur
C               INTERNAL_OS_NB :   Ordre des développements des fonctions de phase 
C				   en Polynômes de Legendre à utiliser.
C               INDEX COS_ANGLE WEIGHT : lignes donnant le numéro d'angle, son cosinus et 
C                                        son poids au format I4,1X,2D21.14
C 
C
C
C    Fichier utilisateur FICEXTDATA:
C       ==> contient les données externes (utilisateur) de fonctions de phase
C           et paramètres radiatifs (coefficients extinction et diffusion).
C           Données applicables à la longueur d'onde de simulation du champ de rayonnement.
C
C       Format : Fichier ascii formaté, créé par l'utilisateur.
C
C                1ere ligne : 
C                   Valeur : Coefficient d'extinction
C		    Format : EXTINCTION_COEF :  Valeur
C                             ==> Contient la valeur après un champ texte suivi de ":"
C                                 (contenu du texte quelconque avant le ":")
C                
C                2eme ligne : 
C                   Valeur : Coefficient de diffusion  
C                            (doit avoir la même unité que le coef d'extinction)
C		    Format : SCATTERING_COEF :  Valeur
C                             ==> Contient la valeur après un champ texte suivi de ":"
C                                 (contenu du texte quelconque avant le ":")
C              
C                3eme ligne : 
C                   Valeur : Nombre d'angles décrivant les fonctions de phase.
C                   Format : NB_LINES :  Valeur
C                             ==> Contient la valeur après un champ texte suivi de ":"
C                                 (contenu du texte quelconque avant le ":")
C               
C                4eme ligne : Ligne de commentaire 
C           
C                Lignes 5 à 5+nbAng-1
C                   Valeurs : ANGLE  F11  -F12/F11  F22/F1   F33/F11
C                               ANGLE 	 : angle en degrés.
C                               F11 	 : fonction de phase en intensité
C			        -F12/F11 : rapport des fonctions -F12 et F11
C  			        F22/F11  : rapport des fonctions F22 et F11
C			        F33/F11  : rapport des fonctions F33 et F11 
C                ----------------------   ===========================================
C                NB CONVENTION DE SIGNE : F12 est négative pour la diffusion Rayleigh.
C                                         = convention des OS
C                ----------------------   ===========================================
C		 Format  : Pas de format imposé
C
C           Pas de lecture des lignes suivantes.	
C  
C
C  
C    Fichier résultat FICGRANU:
C      ==>   contient les informations suivantes
C
C               2 lignes de commentaires
C
C               Bloc d'information:
C		  EXTINCTION CROSS SECTION (mic^2)     :  Section efficace d'extinction.
C		  SCATTERING CROSS SECTION (mic^2)     :  Section efficace de diffusion.
C		  ASYMMETRY FACTOR (no truncation)     :  Facteur d'asymétrie.
C		  MEAN PARTICULES ALTITUDE/DEPTH (m)   :  Niveau moyen des particules
C							  dans le profil.
C		  VOLUME OF A MEAN PARTICULE (mic^3)   :  Volume moyenne des particules.
C		  MEAN REFRACTIVE INDEX (real part)    :  Indice de réfraction moyen.
C		  TRUNCATION COEFFICIENT               :  Coefficient de troncature appliqué
C							  à la fonction de phase.
C		  SINGLE SCATTERING ALBEDO (truncation):  Albédo de simple diffusion.
C
C               3 lignes de commentaires (dont l'information sur l'ordre maximal OS_NB  
C                                         de décomposition de la fonction de phase en
C                                         polynômes de Legendre).
C
C               Paramètres de la matrice de phase, donnés sur 4 colonnes :
C			ALPHA(K)        BETA11(K)       GAMMA12(K)      ZETA(K)
C			==> lignes de K=0 à OS_NB
C
C  
C    Fichier trace global de la routine : FICAER_LOG
C      ==>   contient des informations sur les étapes de traitements
C
C    Fichier trace : FICMIE_LOG
C      ==>   contient des informations sur les calculs de Mie.
C
C
C
C Common utilisé:
C --------------
C  Aucun
C
C Cas d'erreur :
C ------------
C     - Erreur dans un sous-programme
C     - Erreur à l'ouverture, la lecture et l'écriture du fichier Trace
C       et des fichiers E/S :  fichier des angles, fichier de données
C                              utilisateur et fichiers résultats
C     - Paramètres non valables
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant
C
C
C  Sous programmes utilisés:
C --------------------------
C  Ce programme fait appel aux routines:
C   SOS_NOM_FICMIE
C   SOS_MIE
C   OSOAA_GRANU
C   SOS_DECOMPO_LEGENDRE
C   OSOAA_ENREG_GRANU
C   SOS_INIT_PARAMWMO
C   SOS_INIT_PARAMSF
C   SOS_INTERPO_SPLINT
C   SOS_INTERPOL
C
C==============================================================================

      SUBROUTINE OSOAA_AEROSOLS(FICANGLES, 
     &                          WA, AOT_WA, WAREF, AOT_WAREF,
     &                          ITRONC, IMOD,
     &                          RN_MMD, IN_MMD,
     &                          IGRANU_MMD, VARGRANU1_MMD, 
     &                          VARGRANU2_MMD, VARGRANU3_MMD,
     &				IMODELE_WMO,
     &				C_WMO_DL, C_WMO_WS, C_WMO_OC, C_WMO_SO,
     &			        IMODELE_SF, RH,
     &   			MODE_PARAM_BILND,
     &				USER_CV_COARSE, USER_CV_FINE, 
     &		  		RTAUct_WAREF, 
     &		  		BMD_CM_MRWA, BMD_CM_MIWA, 
     &		  		BMD_CM_MRWAREF, BMD_CM_MIWAREF,
     &		  		BMD_CM_RMODAL, BMD_CM_VAR,
     &		  		BMD_FM_MRWA, BMD_FM_MIWA, 
     &		  		BMD_FM_MRWAREF, BMD_FM_MIWAREF,
     &		  		BMD_FM_RMODAL, BMD_FM_VAR,
     &				FICEXTDATA,
     &				DIRMIE,FICMIE_LOG,
     &                          FICGRANU, FICAER_LOG, KMAT1, PIZ, IER)



	
C*-------------
C* Déclarations                        
C*-------------    

      IMPLICIT NONE

C Index de fichier
C -----------------
      INTEGER*2 IDLOG		! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_PARTICULES)

      INTEGER*2 IDANG		! Numéro identifiant du fichier des angles	    
      PARAMETER(IDANG=10)
           
      
C Constantes  AEROSOLS
C --------------------     
      INTEGER SOS_MODALITE	!Valeur max du paramètre modalité 
      PARAMETER (SOS_MODALITE=4)

      INTEGER SOS_NB_MODELE	!Nombre de modèles WMO ou Shettle & Fenn possibles.
      PARAMETER (SOS_NB_MODELE=4)

      INTEGER SOS_NB_WMO	!Nombre de types d'aérosols de la WMO utilisés:
      PARAMETER (SOS_NB_WMO=4)  !DL,WS,OC,SO.
      
      INTEGER SOS_NB_SF		!Nombre de types d'aérosols du modèle Shettle & Fenn 
      PARAMETER (SOS_NB_SF=5)   !utilisés:SR,LR,SU,LU,OM.
          
      INTEGER SOS_NB_AERO	!Initialisation avec le plus grand nombre de types 
      PARAMETER (SOS_NB_AERO=5)	!d'aérosols (cas Shettle & Fenn).
         
      DOUBLE PRECISION ALPHAMIN	      !Valeur minimale du paramètre de taille Alpha 
      PARAMETER (ALPHAMIN=0.0001D+00) !des calculs de MIE 
      			              !(doit être comprise entre 0.0001 et 10.0) 

      	    
C* Définition des variables                         
C*-----------------------------------------------------------------
      DOUBLE PRECISION XMU(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)  
                                              !Cosinus des angles de Gauss (et utilisateur).
      DOUBLE PRECISION XHR(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)  
                                              !Poids des angles de Gauss.
      DOUBLE PRECISION WA	! Longueur d'onde sélectionnée pour les calculs (en microns)
      DOUBLE PRECISION WAREF	! Longueur d'onde de référence (en microns).


      DOUBLE PRECISION AOT_WA	 ! Epaisseur optique des aérosols (non tronquée) pour WA.
      DOUBLE PRECISION AOT_WAREF ! Epaisseur optique des aérosols (non tronquée) pour WAREF.

 	!   Modèle mono-mode
	!------------------- 
 	! Indice de réfraction pour modèle mono-mode 
      DOUBLE PRECISION RN_MMD	! partie réelle pour WA
      DOUBLE PRECISION IN_MMD	! partie imaginaire pour WA
 	! Paramètres de granulométrie pour modèle mono-mode       
      DOUBLE PRECISION VARGRANU1_MMD    ! Paramètre 1 de la granulométrie.
      DOUBLE PRECISION VARGRANU2_MMD    ! Paramètre 2 de la granulométrie.
      DOUBLE PRECISION VARGRANU3_MMD    ! Paramètre 3 de la granulométrie.

 	!   Modèle WMO 
	!-------------
      DOUBLE PRECISION C(SOS_NB_WMO)	 !Proportion volumique des constituants WMO
      DOUBLE PRECISION C_WMO_DL	    	 ! --> Valeur utilisateur pour particule DL
      DOUBLE PRECISION C_WMO_WS	    	 ! --> Valeur utilisateur pour particule WS
      DOUBLE PRECISION C_WMO_OC	    	 ! --> Valeur utilisateur pour particule OC
      DOUBLE PRECISION C_WMO_SO	    	 ! --> Valeur utilisateur pour particule SO
      DOUBLE PRECISION V(SOS_NB_WMO)	 !Concentration volumique des constituants WMO
      DOUBLE PRECISION N(SOS_NB_WMO)	 ! C/V
      DOUBLE PRECISION NTOT		 ! Somme des coefficients de pondération 
                                         ! des composants aérosols.
      DOUBLE PRECISION ALPHAF_PARTICULE(4) ! Valeur Alpha Max pour chaque
      					   ! composant de la WMO (DL, WS, OC, SO)

 	!   Modèle Shettle & Fenn 
	!------------------------ 					   
      DOUBLE PRECISION Ni(SOS_NB_SF)	 ! Densité de particules (Shettle & Fenn)
      DOUBLE PRECISION RH                ! % d'humidité relative des modèles Shettle & Fenn 
      
      DOUBLE PRECISION MR(SOS_NB_AERO)   ! partie réelle de l'indice de réfraction
      					 ! pour chaque composant d'aérosols
      DOUBLE PRECISION MI(SOS_NB_AERO)   ! partie imaginaire de l'indice de réfraction.
      					 ! pour chaque composant d'aérosols.
     
      DOUBLE PRECISION VARGRANU1I(SOS_NB_AERO)   ! Paramètre 1 de la granulométrie
      						 ! pour chaque modèle de constituant.
      DOUBLE PRECISION VARGRANU2I(SOS_NB_AERO)   ! Paramètre 2 de la granulométrie
      						 ! pour chaque modèle de constituant. 
				    	   						 
	!   Modèle bimodal de LND
	!------------------------
      INTEGER*2 MODE_PARAM_BILND       	!Indice du choix de description du mélange de 2 LND.
      DOUBLE PRECISION CVI(2)		!Concentration volumique de la "LND coarse" (1) et 
      				      	!de la "LND fine" (2)pour WA
      DOUBLE PRECISION USER_CV_COARSE	!"LND coarse" : concentration volumique "user"
      DOUBLE PRECISION USER_CV_FINE	!"LND fine"   : concentration volumique "user"	
      DOUBLE PRECISION RTAUct_WAREF	!Rapport des épaisseurs opt. AOT_coarse / AOT_total
      					
      DOUBLE PRECISION RN_LND(2)	!Partie réelle de l'indice de réfraction de la 
      					!"LND coarse" (1) et de la "LND fine" (2) 
					!pour la longeur d'onde WA.		
      DOUBLE PRECISION BMD_CM_MRWA	! Valeur lue pour RN_LND(1) : coarse mode
      DOUBLE PRECISION BMD_FM_MRWA	! Valeur lue pour RN_LND(2) : fine mode    
       
      DOUBLE PRECISION IN_LND(2) 	!Partie imaginaire de l'indice de réfraction de la 
      					!"LND coarse" (1) et de la "LND fine" (2) 
					!pour la longeur d'onde WA.	
      DOUBLE PRECISION BMD_CM_MIWA	! Valeur lue pour IN_LND(1) : coarse mode
      DOUBLE PRECISION BMD_FM_MIWA	! Valeur lue pour IN_LND(2) : fine mode 
      
      DOUBLE PRECISION RN_LND_WAREF(2)	!Partie réelle de l'indice de réfraction de la 
      					!"LND coarse" (1) et de la "LND fine" (2) 
					!pour la longueur d'onde WAREF.
      DOUBLE PRECISION BMD_CM_MRWAREF	! Valeur lue pour RN_LND_WAREF(1) : coarse mode
      DOUBLE PRECISION BMD_CM_MIWAREF	! Valeur lue pour RN_LND_WAREF(2) : fine mode 
      
      DOUBLE PRECISION IN_LND_WAREF(2) 	!Partie imaginaire de l'indice de réfraction de la 
      					!"LND coarse" (1) et de la "LND fine" (2) 
					!pour la longueur d'onde WAREF.
      DOUBLE PRECISION BMD_FM_MRWAREF	! Valeur lue pour IN_LND_WAREF(1) : coarse mode
      DOUBLE PRECISION BMD_FM_MIWAREF	! Valeur lue pour IN_LND_WAREF(2) : fine mode      
      
      DOUBLE PRECISION RMODAL_LND(2)  	!Rayon modal des "LND coarse" (1) et "fine" (2).
      DOUBLE PRECISION BMD_CM_RMODAL	! Valeur lue pour RMODAL_LND(1) : coarse mode
      DOUBLE PRECISION BMD_FM_RMODAL	! Valeur lue pour RMODAL_LND(2) : fine mode  
      
      DOUBLE PRECISION VAR_LND(2)	! Variance des "LND coarse" (1) et "fine" (2).
      DOUBLE PRECISION BMD_CM_VAR	! Valeur lue pour VAR_LND(1) : coarse mode
      DOUBLE PRECISION BMD_FM_VAR	! Valeur lue pour VAR_LND(2) : fine mode 


      DOUBLE PRECISION KMAT1c		! Section efficace d'extinction (mic^2) pour le mode
      					! "coarse" à la longueur d'onde WAREF. Non tronqué.
      DOUBLE PRECISION KMAT1f		! Section efficace d'extinction (mic^2) pour le mode
      					! "fine" à la longueur d'onde WAREF. Non tronqué.
     
      DOUBLE PRECISION KMAT1I	! Section efficace d'extinction (micron^2)
      				! pour un constituant I. Non tronqué.
      DOUBLE PRECISION KMAT2I 	! Section efficace de diffusion (micron^2)
      				! pour un constituant I.  Non tronqué.    
      DOUBLE PRECISION VMOYI    ! Volume de la particule moyenne de la granulométrie
      				! d'un constituant I (micron^3).
					          
      DOUBLE PRECISION P11I(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)	
                       !Fonction de phase calculée à partir des fonctions de 
		       !la théorie de MIE pour le constituant I.
      DOUBLE PRECISION P12I(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)	
                       !Elément P12 de la matrice de phase pour le constituant I.
      DOUBLE PRECISION P33I(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)	
                       !Elément P33 de la matrice de phase pour le constituant I.

	!   Angles bornes pour la troncature
	!-----------------------------------
      DOUBLE PRECISION MU1_TRONCA ! cosinus des angles Theta1 et Theta2 pour la définition
      DOUBLE PRECISION MU2_TRONCA ! du segment tronquant la fonction de phase

	!   Données externes
	!------------------------
      DOUBLE PRECISION VAL	! Variable pour la lecture de valeurs inutilisées
      
      DOUBLE PRECISION ANG_EXT(CTE_MAXNB_ANG_EXT)     ! Table des angles des fonctions de
      						      ! phase externes.
      DOUBLE PRECISION MU_EXT(CTE_MAXNB_ANG_EXT)      ! Table des cosinus des angles.
      DOUBLE PRECISION F11_EXT(CTE_MAXNB_ANG_EXT)     ! Fct de phase P11 de données externes
      DOUBLE PRECISION F12_EXT(CTE_MAXNB_ANG_EXT)     ! Fct de phase P12 de données externes
      DOUBLE PRECISION F22_EXT(CTE_MAXNB_ANG_EXT)     ! Fct de phase P22 de données externes
      DOUBLE PRECISION F33_EXT(CTE_MAXNB_ANG_EXT)     ! Fct de phase P33 de données externes
      DOUBLE PRECISION F12sF11_EXT(CTE_MAXNB_ANG_EXT) ! Fonction P12/P11 de données externes
      DOUBLE PRECISION F22sF11_EXT(CTE_MAXNB_ANG_EXT) ! Fonction P22/P11 de données externes
      DOUBLE PRECISION F33sF11_EXT(CTE_MAXNB_ANG_EXT) ! Fonction P33/P11 de données externes 



	!   Paramètres généraux
	!------------------------
       
      DOUBLE PRECISION P11(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)	
                       !Fonction de phase du modèle simulé.
      DOUBLE PRECISION P12(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)	
                       !Elément P12 de la matrice de phase.
      DOUBLE PRECISION P22(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)	
                       !Elément P22 de la matrice de phase.
      DOUBLE PRECISION P33(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)	
                       !Elément P33 de la matrice de phase.
		       
      DOUBLE PRECISION TTT(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)	
                       !Copie de la fonction de phase (P11) avant troncature.
     
      DOUBLE PRECISION Z1	!Valeur de BETA11(0) avant normalisation.
      DOUBLE PRECISION TETA     !Angle de diffusion.
      
                  
      DOUBLE PRECISION ALP(0:CTE_OS_NB_MAX)	! Paramètres alpha(k).
      DOUBLE PRECISION BETA11(0:CTE_OS_NB_MAX)	! Paramètres beta11(k).
      DOUBLE PRECISION BETA22(0:CTE_OS_NB_MAX)	! Paramètres beta22(k).
      DOUBLE PRECISION GAMMA12(0:CTE_OS_NB_MAX)	! Paramètres gamma12(k).
      DOUBLE PRECISION ZETA(0:CTE_OS_NB_MAX)	! Paramètres zeta(k).
      DOUBLE PRECISION DELTA33(0:CTE_OS_NB_MAX)	! Paramètres delta33(k).
     
	    
      DOUBLE PRECISION X, RMAX  ! Paramètres pour le calcul de AlphaMax 
      				! selon la taille des particules.
      DOUBLE PRECISION ALPHAF   ! Valeur de alpha maximale des calculs de Mie.

      DOUBLE PRECISION SOMME_NR	! Nombre de particules 
      				! = intégration sur les rayons de NR * PR.  
      				    
      DOUBLE PRECISION KMAT1	! Section efficace d'extinction (micron^2). Non tronqué.
      DOUBLE PRECISION KMAT2 	! Section efficace de diffusion (micron^2). Non tronqué. 
      DOUBLE PRECISION A_TRONCA	! Coefficient de troncature.
      DOUBLE PRECISION PIZ	! Albédo de simple diffusion.
      DOUBLE PRECISION PIZTR	! Albédo de simple diffusion après troncature.
                     
      DOUBLE PRECISION RN_MOY	! Indice de réfraction moyen (partie réelle) 
      				! représentatif d'un mélange de particules  

      DOUBLE PRECISION V_MOY	! Volume d'une particule moyenne 
      				! d'un mélange de particules (micron^3).        

      DOUBLE PRECISION VMOY_MMD ! Volume de la particule moyenne 
      				! d'une granulométrie mono-mode (micron^3).
      
      
C---- Paramètres de type entier
      INTEGER*4 NBMU_GAUSS ! Nombre d'angles de Gauss utilisés 
      INTEGER*4 MIE_NBMU   ! Nombre d'angles effectifs des simulations
      INTEGER*4 OS_NB 	   ! Ordre des développements des fonctions de phase 
			   ! en Polynômes de Legendre.   

      INTEGER*4 J	   ! Indice de tableau 	
      INTEGER*4 I	   ! Indice de tableau
      INTEGER*4 K	   ! Indice de tableau		   

      INTEGER*4 ITRONC	   ! Option de troncature  (1 pour la réaliser).  
      INTEGER*4 IGRANU_MMD 	! Numéro du modèle de granulométrie mono-modale 
      				! pour le prog MAIN
      INTEGER*4 IGRANU_MMD_CALL ! Numéro du modèle de granulométrie mono-modale
      				! pour l'appel de OSOAA_GRANU
      INTEGER*4 LDIRMIE	   ! longueur de la chaine de caractères DIRMIE
      INTEGER*4 LFICMIE	   ! longueur de la chaine de caractères FICMIE  

      INTEGER*2 IMOD	    ! Granulométrie mono ou multi-modale.
      INTEGER*2 IMODELE_WMO ! Type du modèle WMO.
      INTEGER*2 IMODELE_SF  ! Type du modèle Shettle & Fenn.

      INTEGER*4 IFIN       ! nb de caractères significatifs d'une chaine  

      INTEGER*4 NB_FICEXT_ANGLES ! Nombre d'angles du fichier de fct de phase externe

      INTEGER*4 NVAL 	! Integer pour la lecture de fichier ANGLES
                         			    
C---- Code d'erreur
      INTEGER*4 IER     ! Code d'erreur =0 si pas d'erreur, =-1 sinon
      
                        
C---- Les fichiers utilisés
      CHARACTER*CTE_LENFIC2 FICANGLES   ! Chemin et nom du fichier des angles utilisés 
                                        ! pour les calculs de Mie      
 
      CHARACTER*CTE_LENFIC2 FICAER_LOG  ! Nom du fichier trace des calculs de phase aérosols
            
      CHARACTER*CTE_LENFIC2 FICANGLES_USER      ! Nom du fichier des angles "utilisateurs"

      CHARACTER*CTE_LENFIC2 FICGRANU		! Nom du fichier résultat GRANU  
            
      CHARACTER*CTE_LENDIR DIRFIC	        ! chemin d'accès aux fichiers des paramètres
      						! des modèles WMO et S&F
      CHARACTER*CTE_LENFIC2 FICDATAWMO          ! chemin et nom du fichier Data_WMO
      
      CHARACTER*15 PARTICULE(SOS_NB_AERO)       ! nom des différentes particules de la WMO
      					        ! ou des modèles S&F	

 
      CHARACTER*1000 CVAL	                ! Chaine de caractères pour la lecture 
      						! de valeurs inutilisées

      CHARACTER*LENLINE LIGNE_TEXTE	!Ligne de caractères pour décodage de paramètres
						
      CHARACTER*CTE_LENFIC2 FICEXTDATA  ! Chemin et nom du fichier de données externes de 
      					! fonctions de phase et paramètres radiatifs 
					! (coefficients d'extinction et de diffusion)  

      CHARACTER*CTE_LENDIR  DIRMIE              ! chemin d'accès au fichier MIE
      CHARACTER*CTE_LENFIC1 FICS		! nom du fichier MIE 
      CHARACTER*CTE_LENFIC2 FICMIE	        ! chemin et nom du fichier MIE
      CHARACTER*CTE_LENFIC2 FICMIE_LOG          ! nom du fichier TRACE de MIE 
 

C---- Expressions logiques
      LOGICAL TRACE     ! = vrai si écriture dans le fichier trace      
      LOGICAL EX	! Vérifie l'existence d'un fichier



	
C*-------------
C* Traitements
C*-------------
 
 
 
      
C* Initialisation                     
C*-----------------------------------------------------------------
      IER=0
	
      MU1_TRONCA = CTE_AER_MU1_TRONCA	
      MU2_TRONCA = CTE_AER_MU2_TRONCA	
       
C* Ouverture du fichier Trace 
C------------------------------------------------------
      IF (FICAER_LOG.EQ.'NO_LOG_FILE') THEN
	 TRACE=.FALSE.
      ELSE
         TRACE=.TRUE.
	 OPEN (IDLOG,FILE=FICAER_LOG,ERR=911)

         WRITE (IDLOG,*,ERR=921) ""
	 WRITE (IDLOG,*,ERR=921) ""
	 WRITE (IDLOG,*,ERR=921) "****************************"	 
	 WRITE (IDLOG,*,ERR=921) "OSOAA_AEROSOLS :  "
	 WRITE (IDLOG,*,ERR=921) "****************************"
	 WRITE (IDLOG,*,ERR=921) ""
	 WRITE (IDLOG,*,ERR=921) "Reference wavelength:", WAREF
	 WRITE (IDLOG,*,ERR=921) 
     &	 "Wavelength for radiance simulation:", WA
	 WRITE (IDLOG,*,ERR=921) ""
	 
      ENDIF
      
	 
C* Lecture des tables d'angles et de poids utiles aux simulations
C* --> Lecture de chaque ligne d'information et stockage en tant
C*     que chaine de caractères. Identification du séparateur (:)
C*     "texte : valeur utile" et extraction du champ suivant le 
C*     séparateur avec affectation au paramètre du type final
C*     souhaité (integer, float ou chaine de caractères)
C* -----------------------------------------------------------------
      OPEN(IDANG,FILE=FICANGLES,STATUS='OLD',ERR=930)

      READ(IDANG,'(a)',ERR=931) LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')
      READ(LIGNE_TEXTE(IFIN+1:LENLINE),*) MIE_NBMU
      
      READ(IDANG,'(a)',ERR=931)  LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')
      READ(LIGNE_TEXTE(IFIN+1:LENLINE),*) NBMU_GAUSS
      
      READ(IDANG,'(a)',ERR=931)  LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')  
      FICANGLES_USER=LIGNE_TEXTE(IFIN+1:LENLINE)
      IFIN=INDEX(FICANGLES,' ')
      FICANGLES_USER=FICANGLES_USER(1:IFIN)
      
      READ(IDANG,'(a)',ERR=931)  LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')
      READ(LIGNE_TEXTE(IFIN+1:LENLINE),*) OS_NB
      
      READ(IDANG,*,ERR=931) CVAL
      DO J=1,MIE_NBMU
	  READ(IDANG,*,ERR=931) NVAL, XMU(J), XHR(J)
      ENDDO 

      CLOSE(IDANG)
      
      DO J=1,MIE_NBMU
         XMU(-J) = -XMU(J)
	 XHR(-J) =  XHR(J)
      ENDDO 

        
      IF (TRACE) THEN
         WRITE (IDLOG,*,ERR=921) "USED ANGLES"
	 WRITE (IDLOG,*,ERR=921) "***********"
	 WRITE (IDLOG,*,ERR=921) " "
	 WRITE (IDLOG,*,ERR=921) "Total number of angles : ",MIE_NBMU
	 WRITE (IDLOG,*,ERR=921) "Number of Gauss angles : ",NBMU_GAUSS
	 WRITE (IDLOG,*,ERR=921) " "
         IF (FICANGLES_USER.NE.'NO_USER_ANGLES') THEN
	     WRITE(IDLOG,*,ERR=921)  
     &	       "A file of user's angles is used :", FICANGLES_USER
     	     WRITE(IDLOG,*,ERR=921) "Number of user's angles :",
     &         (MIE_NBMU-NBMU_GAUSS)
	 ELSE
	     WRITE(IDLOG,*,ERR=921)  
     &	       "No file of user's angles"
	 ENDIF
         WRITE (IDLOG,*,ERR=921) " " 
	 WRITE (IDLOG,*,ERR=921) "Max order NB : ", OS_NB
	 WRITE (IDLOG,*,ERR=921) " " 
	 WRITE (IDLOG,*,ERR=921) " " 
	 WRITE (IDLOG,*,err=921) "List of used angles :"
	 WRITE (IDLOG,*,err=921) "  Index, mu, angle (degrees), weigth"
	 DO 1000 J=-MIE_NBMU,MIE_NBMU
	    IF (J.EQ.0) GOTO 1000
	    IF (J.LT.0) THEN 
	        WRITE(IDLOG,222,ERR=921) J, XMU(J),
     &	                    -DACOS(-XMU(J))*180./INCTE_PI, XHR(J)
            ELSE
	        WRITE(IDLOG,222,ERR=921) J, XMU(J),
     &	                     DACOS(XMU(J))*180./INCTE_PI, XHR(J)
	    ENDIF	    
1000     CONTINUE
	 WRITE (IDLOG,*,ERR=921) " " 
	 WRITE (IDLOG,*,ERR=921) " "  	
      ENDIF !Fichier Trace


C* Récupération du nom du répertoire contenant les fichiers de paramètres WMO 
C* et Shettle & Fenn
C----------------------------------------------------------------------------
      CALL GETENV('OSOAA_ROOT',DIRFIC)
      IFIN=INDEX(DIRFIC,' ')
      IFIN=IFIN-1
      IF (IFIN.LE.0) GOTO 925
      
      IF ((IFIN+4).GT.CTE_LENDIR) GOTO 926
      
      DIRFIC(IFIN+1:IFIN+4)='/fic'
      IFIN=IFIN+4
      IF (IFIN.LE.0) IFIN=CTE_LENDIR

  

C* Initialisation des proportions de constituants des modèles WMO et 
C* Shettle & Fenn
C----------------------------------------------------------------------------------      
      DO I=1,SOS_NB_WMO
	  C(I)=0.
	  N(I)=0.
      ENDDO
      
       DO I=1,SOS_NB_SF
	  Ni(I)=0.
      ENDDO

      NTOT=0.
      
   
C* Initialisation des tableaux de paramètres de la matrice de phase
C--------------------------------------------------------------------
      DO 101 K=0,OS_NB
         BETA11(K)=0.D+00
	 BETA22(K)=0.D+00
         GAMMA12(K)=0.D+00
         DELTA33(K)=0.D+00
         ALP(K)=0.D+00
         ZETA(K)=0.D+00
  101 CONTINUE

C* Initialisation des tableaux de fonction de phase
C--------------------------------------------------
      DO 102 J=-MIE_NBMU,MIE_NBMU
         P11(J)=0.
         P12(J)=0.
         P22(J)=0.
	 P33(J)=0.
         TTT(J)=0.
  102 CONTINUE

C* Initialisation des sections efficaces d'extinction et de diffusion
C--------------------------------------------------------------------
      KMAT1=0.
      KMAT2=0.

C* Initialisation de l'indice de réfraction moyen du modèle
C--------------------------------------------------------------------
      RN_MOY=0. 
      
C* Initialisation du volume moyen des particules du modèle
C--------------------------------------------------------------------      
      V_MOY=0.      
            
C* Calcul des paramètres de la matrice de phase des aérosols
C* et de l'albédo de simple diffusion.
C------------------------------------------------------------
      IF (AOT_WA.EQ.0.D+00) THEN	!Pas de calcul sur la granulométrie
      
      	PIZ = 0.D+00
	PIZTR = 0.D+00
	A_TRONCA=0.
	GOTO 1111	!Enregistrement du fichier résultat GRANU
      
      ENDIF

      !Poursuite des traitements pour TA > 0
      !=====================================

C* Longueur du nom du répertoire des fichiers de Mie
C---------------------------------------------------
      LDIRMIE = INDEX(DIRMIE,' ')
      LDIRMIE = LDIRMIE - 1     
      IF((LDIRMIE.LE.0).OR.(LDIRMIE.GT.CTE_LENDIR)) GOTO 927
      
      WRITE(6,*) 'Aerosols --> Mie files repertory : ', 
     &            DIRMIE(1:LDIRMIE)      


C ===================
C* Calcul mono-modal
C ===================

      IF(IMOD.EQ.0) THEN

C* Contrôle la validité de l'argument IGRANU_MMD
C  ---------------------------------------------
	 IF ((IGRANU_MMD.LT.1).OR.(IGRANU_MMD.GT.2)) GOTO 1011	 

C* Contrôle le signe de l'argument IN
C  -----------------------------------------
	 IF (IN_MMD.GT.0.D+00) GOTO 1010
	 
C* Valeurs de RN et IN forcées au format F5.3 et F8.5
C  -----------------------------------------
	 RN_MMD = DNINT(RN_MMD*1000.D+00) / 1000.D+00
	 IN_MMD = -DNINT(-IN_MMD*100000.D+00) / 100000.D+00 	 

C* Contrôle de la valeur de pente de la loi de Junge
C  ------------------------------------------------
	 IF ( (IGRANU_MMD.EQ.2).AND.(VARGRANU2_MMD.EQ.3.D+00) ) THEN
     
	     VARGRANU2_MMD = VARGRANU2_MMD + CTE_JUNGE_SLOPE_COR
	     
	     IF (TRACE) THEN 
		 WRITE (IDLOG,*,ERR=921) " "
	         WRITE (IDLOG,*,ERR=921) "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                 WRITE (IDLOG,*,ERR=921) "WARNING : The defined value"
	         WRITE (IDLOG,*,ERR=921) "          for slope of the "
	         WRITE (IDLOG,*,ERR=921) "          Junge model is 3.0"
	         WRITE (IDLOG,*,ERR=921) "  ==> It is a singular value "
		 WRITE (IDLOG,*,ERR=921) 
     &		 "  ==> The value is changed to : ", VARGRANU2_MMD
	         WRITE (IDLOG,*,ERR=921) "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
	         WRITE (IDLOG,*,ERR=921) " "
                ENDIF !Trace
	 ENDIF
	 	 
C* Calcul de la limite du paramètre de taille
C* et contrôle de la plage de valeur des paramètres de taille 
C  ------------------------------------------------------- 
         IF (TRACE) THEN
            WRITE (IDLOG,*,ERR=921) "MODELE MONO-MODAL"
	    WRITE (IDLOG,*,ERR=921) "*****************"
	    WRITE (IDLOG,*,ERR=921) " "
	    WRITE (IDLOG,*,err=921) "Wavelength in microns : ",WA
	    WRITE (IDLOG,*,ERR=921) " " 
	    WRITE(IDLOG,*,ERR=921) 
     & 	    "Refractive index (real part) : ",RN_MMD
	    WRITE(IDLOG,*,ERR=921) 
     & 	    "Refractive index (imaginary part) : ",IN_MMD
         ENDIF
      	 
         IF (IGRANU_MMD.EQ.1) THEN	! Cas LND
	 
	     IGRANU_MMD_CALL = 1	! Indice LND pour routine OSOAA_GRANU
	     
	     X = VARGRANU2_MMD   
	     RMAX = VARGRANU1_MMD * DEXP(X*X) *
     &	            DEXP(X*DSQRT(-2.D+00*LOG(CTE_COEF_NRMAX)))
     
             IF (TRACE) THEN
                WRITE(IDLOG,*,ERR=921) "Size distribution : Log-normal"
	        WRITE(IDLOG,*,ERR=921) "  Modal radius (microns) : " , 
     &	        VARGRANU1_MMD 
	        WRITE(IDLOG,*,ERR=921) "  Standard deviation  : " , 
     &		VARGRANU2_MMD
                WRITE(IDLOG,*,ERR=921) 
     &	        "  Calculated Rmax limit (mic) : ", RMAX
             ENDIF !Trace
	     
	 ENDIF ! Fin cas IGRANU_MMD = 1
	
	 IF (IGRANU_MMD.EQ.2) THEN	! Cas loi Junge pour aérosols
	 
	     IGRANU_MMD_CALL = 3	! Indice Junge pour aérosols pour OSOAA_GRANU

	     RMAX = VARGRANU3_MMD
	     
             IF (TRACE) THEN	     
                WRITE(IDLOG,*,ERR=921) "Size distribution : Junge's law"
                WRITE(IDLOG,*,ERR=921) "  Minimal radius (microns) : ", 
     &	        VARGRANU1_MMD
                WRITE(IDLOG,*,ERR=921) "  Slope : " , VARGRANU2_MMD 
                WRITE(IDLOG,*,ERR=921) "  Maximal radius (microns) : ",
     &	        RMAX
             ENDIF !Trace
	     
	 ENDIF ! Fin cas IGRANU_MMD = 2
       

	 ALPHAF 
     &	 =REAL(100+100*DINT(2.*INCTE_PI*RMAX / (100.*CTE_WAMIN)))

         IF (TRACE) THEN  
	     WRITE(IDLOG,*,ERR=921) "  AlphaF limit : ", ALPHAF
	     WRITE (IDLOG,*,ERR=921) " "	
         ENDIF !Trace
	 
	 
	 IF ((ALPHAMIN.GT.ALPHAF).OR.(ALPHAF.GE.1D5)) GOTO 1009 
	 	 
      	 
C* Constitution du nom du fichier MIE
C  ----------------------------------
	 CALL SOS_NOM_FICMIE(NBMU_GAUSS,FICANGLES_USER,
     &                       RN_MMD,IN_MMD,ALPHAMIN,ALPHAF,FICS) 	     
	    
	 FICMIE = DIRMIE(1:LDIRMIE)//'/'//FICS	 
	 LFICMIE=INDEX(FICMIE,' ')-1
	 IF (TRACE) THEN 
	    WRITE (IDLOG,*,ERR=921) "MIE file : ", 
     &	    FICMIE(LDIRMIE+2:LFICMIE)	
	 ENDIF	
	 
	 	  
C* Si le fichier MIE existe déjà, le programme n'est pas lancé
C  -----------------------------------------------------------	 
         INQUIRE(FILE=FICMIE,EXIST=EX)
	 
         IF (EX) THEN  
	    
            IF (TRACE) THEN
	        WRITE (IDLOG,*,ERR=921) 
     &		'--> MIE file has already been calculated'	
		WRITE (IDLOG,*,err=921) ' '
	    ENDIF
	    
	 ELSE   
            !Affichage écran
	    WRITE(6,*) "MIE file : ", FICMIE(LDIRMIE+2:LFICMIE)
	    WRITE(6,*)'-- MIE file is being calculated'
	    CALL FLUSH(6)	!Force l'envoie du message à l'écran
	    	
            IF (TRACE) THEN
	        WRITE (IDLOG,*,ERR=921) '--> Generation of MIE file'
		WRITE (IDLOG,*,err=921) ' '
	    ENDIF 
	         
C* Calcul du fichier de MIE
C  ------------------------ 
            CALL SOS_MIE(MIE_NBMU,XMU,XHR,RN_MMD,IN_MMD,ALPHAMIN,ALPHAF,
     &	                 FICMIE,FICMIE_LOG,IER)
            IF (IER.NE.0) GOTO 993   ! cas d'erreur
	       
         ENDIF
	 
C* Calcul des sections efficaces d'extinction et de diffusion 
C  et des fonctions de phase I, Q, U
C  ----------------------------------------------------------          
         CALL OSOAA_GRANU(FICMIE,IGRANU_MMD_CALL,
     &	 		  VARGRANU1_MMD,VARGRANU2_MMD,VARGRANU3_MMD,
     &                    WA,MIE_NBMU,XMU,TRACE,
     &                    KMAT1,KMAT2,SOMME_NR,
     &                    VMOY_MMD,P11,P12,P33,IER)
         IF (IER.NE.0) GOTO 995   ! cas d'erreur 

C* Cas de particules sphériques : P22(J) = P11(J)
C  -----------------------------------------------------
         DO 1021 J=-MIE_NBMU,MIE_NBMU
            P22(J)=P11(J)
 1021 CONTINUE	 
  
C* Calcul des paramètres alpha, beta, gamma, delta, zeta
C  avec application ou non de la troncature
C  -----------------------------------------------------
	 CALL SOS_DECOMPO_LEGENDRE(ITRONC,MU1_TRONCA,MU2_TRONCA,
     &			 	   TRACE,MIE_NBMU,XMU,XHR,OS_NB,
     &				   P11,TTT,P12,P22,P33,A_TRONCA,Z1,
     &				   ALP,BETA11,BETA22,
     &                             GAMMA12,DELTA33,ZETA,IER)  
         IF (IER.NE.0) GOTO 996   ! cas d'erreur

C* Estimation de l'indice de réfraction moyen
C  et du volume moyen des particules (granulométrie unique)
C  -----------------------------------------------------
         RN_MOY = RN_MMD
	 V_MOY  = VMOY_MMD
      
      
      ENDIF	!Fin test sur Modalite IMOD=0 (Mono-modal)
      
      



C ====================================================
C* Calcul multi-modal avec les constituants de la WMO
C ====================================================
	    
      IF(IMOD.EQ.1) THEN
	     
C* Contrôle la validité du paramètre IMODELE
C  -----------------------------------------	    
	 IF ((IMODELE_WMO.LT.1).OR.
     &       (IMODELE_WMO.GT.SOS_NB_MODELE)) GOTO 1012	
	 
C* Initialisation
C  --------------
         ! Nom des différentes particules de la WMO
	 PARTICULE(1)='DUST LIKE'
         PARTICULE(2)='WATER SOLUBLE'
         PARTICULE(3)='OCEANIC'
         PARTICULE(4)='SOOT'

         ALPHAF_PARTICULE(1) = CTE_ALPHAMAX_WMO_DL
	 ALPHAF_PARTICULE(2) = CTE_ALPHAMAX_WMO_WS
	 ALPHAF_PARTICULE(3) = CTE_ALPHAMAX_WMO_OC
	 ALPHAF_PARTICULE(4) = CTE_ALPHAMAX_WMO_SO
	 
	 IGRANU_MMD_CALL =   1	! distribution Log Normal par défaut
	    
C* Initialisation des paramètres de la WMO
C  ---------------------------------------
	 FICDATAWMO=DIRFIC(1:IFIN)//'/'//CTE_AER_DATAWMO

	 CALL SOS_INIT_PARAMWMO(FICDATAWMO,WA,VARGRANU1I,
     &                          VARGRANU2I,MR,MI,V,IER)
         IF (IER.NE.0) GOTO 994   ! cas d'erreur 

C* Définition de la proportion volumique de chaque constituant
C  pour le modèle sélectionné
C  -----------------------------------------------------------	 
       	 IF(IMODELE_WMO.EQ.1) THEN	!Modèle Continental
	    C(1) = 0.70
	    C(2) = 0.29
	    C(4) = 0.01      
	    
       	 ELSE IF(IMODELE_WMO.EQ.2) THEN !Modèle Maritime
	    C(2) = 0.05
	    C(3) = 0.95
	 
         ELSE IF(IMODELE_WMO.EQ.3) THEN	!Modèle Urbain
	    C(1) = 0.17
	    C(2) = 0.61
	    C(4) = 0.22
	       
         ELSE IF(IMODELE_WMO.EQ.4) THEN	!Modèle défini par l'utilisateur
	 
C*          Contrôle de la récupération des proportions volumiques 
C*          définies par l'utilisateur
C           ------------------------------------------------------
	    IF ( (C_WMO_DL.EQ.CTE_NOT_DEFINED_VALUE_DBLE)
     &	          .OR.(C_WMO_WS.EQ.CTE_NOT_DEFINED_VALUE_DBLE)
     &	          .OR.(C_WMO_OC.EQ.CTE_NOT_DEFINED_VALUE_DBLE)
     &            .OR.(C_WMO_SO.EQ.CTE_NOT_DEFINED_VALUE_DBLE) )GOTO 825
	    
	    C(1) = C_WMO_DL    !Pourcentage volumique des particules DL
	    C(2) = C_WMO_WS    !Pourcentage volumique des particules WS
	    C(3) = C_WMO_OC    !Pourcentage volumique des particules OC
	    C(4) = C_WMO_SO    !Pourcentage volumique des particules SO	       	    

         ENDIF	!Fin définition des modèles
	    
         DO 103 I=1,SOS_NB_WMO	    
            N(I) = C(I)/V(I)
	    NTOT = NTOT + N(I)
  103    CONTINUE

      IF (TRACE) THEN
         WRITE (IDLOG,*,ERR=921) "WMO MODEL"
	 WRITE (IDLOG,*,ERR=921) "*********"
	 WRITE (IDLOG,*,ERR=921) " "
	 IF(IMODELE_WMO.EQ.1) 
     &	    WRITE(IDLOG,*,ERR=921) "CONTINENTAL Model"
	 IF(IMODELE_WMO.EQ.2) WRITE(IDLOG,*,ERR=921) "MARITIME Model"
	 IF(IMODELE_WMO.EQ.3) WRITE(IDLOG,*,ERR=921) "URBAN Model"
	 WRITE (IDLOG,*,ERR=921) " "
	 WRITE (IDLOG,*,ERR=921) "  Coefficients alphaK of the mixture:"
	 DO I=1,SOS_NB_WMO
	    WRITE (IDLOG,*,ERR=921) "    ",PARTICULE(I)," : ", N(I)/NTOT
         ENDDO
	 WRITE (IDLOG,*,ERR=921) " "
	 IF(IMODELE_WMO.EQ.4) THEN
	     WRITE (IDLOG,*,ERR=921) "USER Model "
	     DO I=1,4
	        WRITE (IDLOG,*,ERR=921) "    ",PARTICULE(I)," : ",
     &		                     100*C(I)," % volume"
             ENDDO
	 ENDIF 
	 WRITE (IDLOG,*,err=921) "Wavelength in microns : ",WA
	 WRITE (IDLOG,*,ERR=921) " " 
	 WRITE (IDLOG,*,ERR=921) " "  		
      ENDIF !Fichier Trace
      
      

C* Pour chaque type d'aérosols de la WMO
C  -------------------------------------	       
	 DO 105 I=1,SOS_NB_WMO
	    
	    IF(C(I).EQ.0.) GOTO 105

	    IF (TRACE) THEN
	       WRITE(IDLOG,*,err=921) PARTICULE(I)
	       WRITE(IDLOG,*,err=921)'============='
	       WRITE(IDLOG,*,err=921) ' '
	    ENDIF
	    		
C* Constitution du nom du fichier MIE
C  ----------------------------------
	    ALPHAF = ALPHAF_PARTICULE(I)

            CALL SOS_NOM_FICMIE(NBMU_GAUSS,FICANGLES_USER,
     &	                        MR(I),MI(I),ALPHAMIN,ALPHAF,FICS)       
	    FICMIE = DIRMIE(1:LDIRMIE)//'/'//FICS
            LFICMIE=INDEX(FICMIE,' ')-1
	    IF (TRACE) THEN 
	       WRITE(IDLOG,*,ERR=921) "MIE file : ",
     &	       FICMIE(LDIRMIE+2:LFICMIE)	
	    ENDIF		
	    
	    
C* Si le fichier MIE existe déjà, le programme n'est pas lancé
C  -----------------------------------------------------------		              
            INQUIRE(FILE=FICMIE,EXIST=EX)
  	    
	    IF (EX) THEN  
	       
               IF (TRACE) THEN
	          WRITE (IDLOG,*,ERR=921)
     &		  '--> MIE file has already been calculated'
		  WRITE (IDLOG,*,err=921) ' '
	       ENDIF
	  
	    ELSE   
	       !Affichage écran
	       WRITE(6,*) "MIE file : ", FICMIE(LDIRMIE+2:LFICMIE)
	       WRITE(6,*)'-- MIE file is being calculated'
	       CALL FLUSH(6)	!Force l'envoie du message à l'écran
		
               IF (TRACE) THEN
	          WRITE (IDLOG,*,ERR=921)'--> Generation of MIE file'
		  WRITE (IDLOG,*,err=921) ' '
	       ENDIF 
	       
C* Calcul du fichier de MIE
C  ------------------------ 
              CALL SOS_MIE(MIE_NBMU,XMU,XHR,MR(I),MI(I),ALPHAMIN,ALPHAF,
     &                      FICMIE,FICMIE_LOG,IER)
    	       IF (IER.NE.0) GOTO 993   ! cas d'erreur
         
            ENDIF
	 
C* Calcul des sections efficaces d'extinction et de diffusion 
C  et des fonctions de phase I, Q, U du constituant I
C  ---------------------------------------------------------- 	       
            CALL OSOAA_GRANU(FICMIE,IGRANU_MMD_CALL,VARGRANU1I(I),
     &                       VARGRANU2I(I),CTE_NOT_DEFINED_VALUE_DBLE,
     &                       WA,MIE_NBMU,XMU,TRACE,
     &			     KMAT1I,KMAT2I,SOMME_NR,VMOYI,
     &                       P11I,P12I,P33I,IER)
            IF (IER.NE.0) GOTO 995   ! cas d'erreur  
	    

C* Calcul des sections efficaces d'extinction et de diffusion du mélange
C  ---------------------------------------------------------------------	     
	    KMAT1 = KMAT1 + (N(I)/NTOT)*KMAT1I
	    KMAT2 = KMAT2 + (N(I)/NTOT)*KMAT2I
	    
	    DO 104 J=-MIE_NBMU,MIE_NBMU	     
	       P11(J) = P11(J) + (N(I)/NTOT)*P11I(J)*KMAT2I
	       P12(J) = P12(J) + (N(I)/NTOT)*P12I(J)*KMAT2I
	       P33(J) = P33(J) + (N(I)/NTOT)*P33I(J)*KMAT2I
  104	    CONTINUE  !Fin boucle sur les angles de gauss

C* Estimation de l'indice de réfraction moyen
C  et du volume moyen des particules 
C  ----------------------------------------------------- 
	 RN_MOY = RN_MOY + (N(I)/NTOT)*MR(I)
	 V_MOY  =  V_MOY + (N(I)/NTOT)*VMOYI
    
  105	 CONTINUE	 !Fin boucle sur les constituants de la WMO


C* Normalisation des fonctions de phase du mélange
C  -----------------------------------------------	    
	 DO 106 J=-MIE_NBMU,MIE_NBMU   
	    P11(J) = P11(J) / KMAT2
	    P12(J) = P12(J) / KMAT2
	    P33(J) = P33(J) / KMAT2
  106	 CONTINUE

C* Cas de particules sphériques : P22(J) = P11(J)
C  -----------------------------------------------------
         DO 1022 J=-MIE_NBMU,MIE_NBMU
            P22(J)=P11(J)
 1022    CONTINUE	 
 
C* Calcul des paramètres alpha, beta, gamma, delta, zeta
C  avec application ou non de la troncature
C  -----------------------------------------------------  
	 CALL SOS_DECOMPO_LEGENDRE(ITRONC,MU1_TRONCA,MU2_TRONCA,
     &			 	   TRACE,MIE_NBMU,XMU,XHR,OS_NB,
     &				   P11,TTT,P12,P22,P33,A_TRONCA,Z1,
     &				   ALP,BETA11,BETA22,
     &                             GAMMA12,DELTA33,ZETA,IER)  
         IF (IER.NE.0) GOTO 996   ! cas d'erreur
	 
      ENDIF	!Fin test sur Modalite IMOD=1 (WMO) 
  


C =====================================================================
C* Calcul bi-modal avec les constituants du modèle Shettle & Fenn (S&F)
C =====================================================================
      
      IF(IMOD.EQ.2) THEN

C* Contrôle la validité des paramètres IMODELE et RH
C  -------------------------------------------------		 
	 IF ((IMODELE_SF.LT.1).OR.(IMODELE_SF.GT.SOS_NB_MODELE))
     &      GOTO 1012  					        !Erreur choix modèle
	 IF ((RH.LT.0.D-00).OR.(RH.GT.99.D-00)) GOTO 1013	!Erreur sur % humidité

	 
C* Initialisation
C  --------------
         ! Nom des différentes particules du modèle S&F
	 PARTICULE(1)='SMALL_RURAL'
         PARTICULE(2)='LARGE RURAL'
         PARTICULE(3)='SMALL_URBAN'
         PARTICULE(4)='LARGE_URBAN'
	 PARTICULE(5)='OCEANIC_MODEL'

	 IGRANU_MMD_CALL =   1	! distribution Log Normal par défaut
	    
C* Initialisation des paramètres du modèle S&F
C  -------------------------------------------
	 CALL SOS_INIT_PARAMSF(DIRFIC,WA,RH,VARGRANU1I,
     &                         VARGRANU2I,MR,MI,IER)
         IF (IER.NE.0) GOTO 997   ! cas d'erreur 

C* Définition de la densité de chaque constituant pour le 
C  modèle sélectionné
C  -----------------------------------------------------------	 
       	 IF(IMODELE_SF.EQ.1) THEN	!Modèle Troposphérique
	    Ni(1) = 1.0     
	    
       	 ELSE IF(IMODELE_SF.EQ.2) THEN 	!Modèle Urbain
	    Ni(3) = 0.999875
	    Ni(4) = 0.000125
	 
         ELSE IF(IMODELE_SF.EQ.3) THEN	!Modèle Maritime
	    Ni(1) = 0.99
	    Ni(5) = 0.01
	       
         ELSE IF(IMODELE_SF.EQ.4) THEN	!Modèle cotier
	    Ni(1) = 0.995
	    Ni(5) = 0.005
	       
         ENDIF	!Fin définition des modèles S&F


      IF (TRACE) THEN
         WRITE (IDLOG,*,ERR=921) "SHETTLE & FENN MODEL"
	 WRITE (IDLOG,*,ERR=921) "********************"
	 WRITE (IDLOG,*,ERR=921) " "
	 IF(IMODELE_SF.EQ.1) WRITE (IDLOG,*,ERR=921) 
     &   "Modèle TROPOSPHERIQUE"
	 IF(IMODELE_SF.EQ.2) WRITE (IDLOG,*,ERR=921) "URBAN Model"
	 IF(IMODELE_SF.EQ.3) WRITE (IDLOG,*,ERR=921) "MARITIME  Model"
	 IF(IMODELE_SF.EQ.4) WRITE (IDLOG,*,ERR=921) "COASTAL Model"
	 WRITE (IDLOG,*,ERR=921) " "
	 WRITE (IDLOG,*,ERR=921) "  Coefficients alphaK of the mixture:"
	 DO I=1,SOS_NB_SF
	    WRITE (IDLOG,*,ERR=921) "    ",PARTICULE(I)," : ", Ni(I)
         ENDDO
         WRITE (IDLOG,*,ERR=921) " "
	 WRITE (IDLOG,*,err=921) 
     &   "Percentage of relative humidity : ",RH
	 WRITE (IDLOG,*,err=921) "Wavelength in microns : ",WA 
	 WRITE (IDLOG,*,ERR=921) " "   
	 WRITE (IDLOG,*,ERR=921) " "		
      ENDIF !Fichier Trace
      
C* Pour chaque type d'aérosols du modèle S&F
C  ----------------------------------------     
	 DO 109 I=1,SOS_NB_SF
	    
	    IF(Ni(I).EQ.0.) GOTO 109

	    IF (TRACE) THEN
	       WRITE(IDLOG,*,err=921) PARTICULE(I)
	       WRITE(IDLOG,*,err=921)'============='
	       WRITE(IDLOG,*,err=921) ' '
	    ENDIF
	    		
C* Constitution du nom du fichier MIE
C  ----------------------------------	   
            IF (I.EQ.1) ALPHAF = CTE_ALPHAMAX_SF_SR
	    IF (I.EQ.3) ALPHAF = CTE_ALPHAMAX_SF_SU
	    IF ((I.NE.1).AND.(I.NE.3)) THEN
	        X = VARGRANU2I(I)  
	        RMAX = VARGRANU1I(I) * DEXP(X*X) *
     &	               DEXP(X*DSQRT(-2.D+00*LOG(CTE_COEF_NRMAX)))
     	
	        ALPHAF =
     &	   	REAL(100+100*DINT(2.*INCTE_PI*RMAX/(100.*WA)))
		
		
		IF (TRACE) THEN
	           WRITE(IDLOG,*,err=921) 'RMAX (microns) :',RMAX
		   WRITE(IDLOG,*,err=921) 'Wavelength (microns) : ',WA
		   WRITE(IDLOG,*,err=921) 
     &		      'Maximal size parameter : ',ALPHAF
	       ENDIF
	   
	    ENDIF
	    
            CALL SOS_NOM_FICMIE(NBMU_GAUSS,FICANGLES_USER,
     &	                        MR(I),MI(I),ALPHAMIN,ALPHAF,FICS)
	    FICMIE = DIRMIE(1:LDIRMIE)//'/'//FICS
            LFICMIE=INDEX(FICMIE,' ')-1
	    IF (TRACE) THEN 
	       WRITE(IDLOG,*,ERR=921) "MIE file : ",
     &	       FICMIE(LDIRMIE+2:LFICMIE)	
	    ENDIF	

	    
C* Si le fichier MIE existe déjà, le programme n'est pas lancé
C  -----------------------------------------------------------		              
            INQUIRE(FILE=FICMIE,EXIST=EX)
  	    
	    IF (EX) THEN  
	       
               IF (TRACE) THEN
	          WRITE (IDLOG,*,ERR=921)
     &		  '--> MIE file has already been calculated'
		  WRITE (IDLOG,*,err=921) ' '
	       ENDIF
	  
	    ELSE   
	       !Affichage écran
 	       WRITE(6,*) "MIE file : ", FICMIE(LDIRMIE+2:LFICMIE)
	       WRITE(6,*)'-- MIE file is being calculated'
	       CALL FLUSH(6)	!Force l'envoie du message à l'écran
		
               IF (TRACE) THEN
	          WRITE (IDLOG,*,ERR=921) '--> Generation of MIE file'
		  WRITE (IDLOG,*,err=921) ' '
	       ENDIF 
      
C* Calcul du fichier de MIE
C  ------------------------ 
               CALL SOS_MIE(MIE_NBMU,XMU,XHR,
     &	                    MR(I),MI(I),ALPHAMIN,ALPHAF,
     &                      FICMIE,FICMIE_LOG,IER)
     	       IF (IER.NE.0) GOTO 993   ! cas d'erreur
         
            ENDIF
	 
C* Calcul des sections efficaces d'extinction et de diffusion 
C  et des fonctions de phase I, Q, U du constituant I
C  ----------------------------------------------------------        
            CALL OSOAA_GRANU(FICMIE,IGRANU_MMD_CALL,VARGRANU1I(I),
     &                       VARGRANU2I(I),CTE_NOT_DEFINED_VALUE_DBLE,
     &                       WA,MIE_NBMU,XMU,TRACE,
     &			     KMAT1I,KMAT2I,SOMME_NR,VMOYI,
     &                       P11I,P12I,P33I,IER)
            IF (IER.NE.0) GOTO 995   ! cas d'erreur  
	     
C* Calcul des sections efficaces d'extinction et de diffusion du mélange
C  ---------------------------------------------------------------------
	    KMAT1 = KMAT1 + Ni(I)*KMAT1I
	    KMAT2 = KMAT2 + Ni(I)*KMAT2I
	    
	    DO 108 J=-MIE_NBMU,MIE_NBMU	     
	       P11(J) = P11(J) + Ni(I)*P11I(J)*KMAT2I
	       P12(J) = P12(J) + Ni(I)*P12I(J)*KMAT2I
	       P33(J) = P33(J) + Ni(I)*P33I(J)*KMAT2I
  108	    CONTINUE  !Fin boucle sur les angles de gauss


C* Estimation de l'indice de réfraction moyen
C  et du volume moyen des particules 
C  ----------------------------------------------------- 
	    RN_MOY = RN_MOY + Ni(I)*MR(I)
	    V_MOY  =  V_MOY + Ni(I)*VMOYI
	     
  109	 CONTINUE	 !Fin boucle sur les constituants du modèle Shettle et Fenn
	    

C* Normalisation des fonctions de phase du mélange
C  -----------------------------------------------
	 DO 110 J=-MIE_NBMU,MIE_NBMU   
	    P11(J) = P11(J) / KMAT2
	    P12(J) = P12(J) / KMAT2
	    P33(J) = P33(J) / KMAT2
  110	 CONTINUE

C* Cas de particules sphériques : P22(J) = P11(J)
C  -----------------------------------------------------
      DO 1023 J=-MIE_NBMU,MIE_NBMU
         P22(J)=P11(J)
 1023 CONTINUE	 
 
C* Calcul des paramètres alpha, beta, gamma, delta, zeta
C  avec application ou non de la troncature
C  -----------------------------------------------------  
	 CALL SOS_DECOMPO_LEGENDRE(ITRONC,MU1_TRONCA,MU2_TRONCA,
     &			 	   TRACE,MIE_NBMU,XMU,XHR,OS_NB,
     &				   P11,TTT,P12,P22,P33,A_TRONCA,Z1,
     &				   ALP,BETA11,BETA22,
     &                             GAMMA12,DELTA33,ZETA,IER)  
         IF (IER.NE.0) GOTO 996   ! cas d'erreur	 

      	 
      ENDIF	!Fin test sur Modalite IMOD=2 (Shettle&Fenn) 





C =====================================================================
C* Calcul des propriétés radiatives d'une granulométrie LND bimodale
C =====================================================================
      
      IF(IMOD.EQ.3) THEN
	
C*    Contrôle de la récupération des paramètres spécifiques au calcul bimodal LND
C     ----------------------------------------------------------------------------
         IF ((MODE_PARAM_BILND.LT.1).OR.(MODE_PARAM_BILND.GT.2)) 
     &	     GOTO 1015
         	
       
C*       Récupération de l'information de la description du mélange
C        ----------------------------------------------------------      
C        Utilisation des concentrations volumiques de l'utilisateur
         IF (MODE_PARAM_BILND.EQ.1) THEN 
	     CVI(1) = USER_CV_COARSE !mode "coarse"
	     CVI(2) = USER_CV_FINE   !mode "fine"
	  
         ENDIF
	    
C*       Récupération de l'information d'indice et granulométrie des 2 LND
C        -----------------------------------------------------------------         	    
         RN_LND(1) = BMD_CM_MRWA  !mode "coarse"
         RN_LND(2) = BMD_FM_MRWA  !mode "fine"
      
         IN_LND(1) = BMD_CM_MIWA  !mode "coarse"
         IN_LND(2) = BMD_FM_MIWA  !mode "fine"
      
         RMODAL_LND(1) = BMD_CM_RMODAL  !mode "coarse"
         RMODAL_LND(2) = BMD_FM_RMODAL  !mode "fine"
      
         VAR_LND(1) = BMD_CM_VAR  !mode "coarse"
         VAR_LND(2) = BMD_FM_VAR  !mode "fine"
      
      
         IF (MODE_PARAM_BILND.EQ.2) THEN 
	    RN_LND_WAREF(1) = BMD_CM_MRWAREF !mode "coarse"
	    RN_LND_WAREF(2) = BMD_FM_MRWAREF !mode "fine"
	  
	    IN_LND_WAREF(1) = BMD_CM_MIWAREF !mode "coarse"
	    IN_LND_WAREF(2) = BMD_FM_MIWAREF !mode "fine"
         ENDIF
 
         

         DO 111 I=1,2
	 
C* Contrôle le signe de l'argument IN
C  -----------------------------------------
	    IF (IN_LND(I).GT.0.D+00) GOTO 1010	 

C* Valeurs de RN et IN forcées au format F5.3 et F8.5
C  -----------------------------------------
	    RN_LND(I) = DNINT(RN_LND(I)*1000.D+00) / 1000.D+00
	    IN_LND(I) = -DNINT(-IN_LND(I)*100000.D+00) / 100000.D+00 
	    
111      CONTINUE	 
	 



         IF (TRACE) THEN
            WRITE (IDLOG,*,ERR=921) "LOG-NORMAL BIMODAL MODEL"
	    WRITE (IDLOG,*,ERR=921) "************************"
	    WRITE (IDLOG,*,ERR=921) " "
	    WRITE (IDLOG,*,ERR=921) "Wavelength in microns : ", WA
	    WRITE (IDLOG,*,ERR=921) " "
	    WRITE(IDLOG,*,ERR=921) 
     &	    "Size distribution : log-normal coarse"
	    WRITE(IDLOG,*,ERR=921) 
     &	    "                    -----------------"
	    WRITE(IDLOG,*,ERR=921) 
     & 	    "   Refractive index (real part) : ",RN_LND(1)
	    WRITE(IDLOG,*,ERR=921) 
     & 	    "   Refractive index (imaginary part) : ",IN_LND(1)
	    WRITE(IDLOG,*,ERR=921) 
	    WRITE(IDLOG,*,ERR=921) "   Modal radius (microns) : " , 
     &	    RMODAL_LND(1) 
	    WRITE(IDLOG,*,ERR=921) 
     &	    "   Standard deviation : " , VAR_LND(1) 
	    WRITE (IDLOG,*,ERR=921) " "
	    WRITE(IDLOG,*,ERR=921) 
     &	    "Size distribution : log-normal fine"
	    WRITE(IDLOG,*,ERR=921)
     &	    "                    ---------------"
	    WRITE(IDLOG,*,ERR=921) 
     & 	    "   Refractive index (real part) : ",RN_LND(2)
	    WRITE(IDLOG,*,ERR=921) 
     & 	    "   Refractive index (imaginary part) : ",IN_LND(2)
	    WRITE(IDLOG,*,ERR=921) 
	    WRITE(IDLOG,*,ERR=921) "   Modal radius (microns) : " , 
     &	    RMODAL_LND(2) 
	    WRITE(IDLOG,*,ERR=921) 
     &	    "   Standard deviation : " , VAR_LND(2) 
	    WRITE(IDLOG,*,ERR=921) " "
	 
	    IF (MODE_PARAM_BILND.EQ.1) THEN 
	       WRITE(IDLOG,*,ERR=921) 
     &         "Proportion of components defined by", 
     &	       " volume concentrations :"	    
	       WRITE(IDLOG,*,ERR=921) 
     & 	       "   Volume concentration for LND coarse mode : ",CVI(1)
	       WRITE(IDLOG,*,ERR=921) 
     & 	       "   Volume concentration for LND fine mode   : ",CVI(2)
	       WRITE(IDLOG,*,ERR=921) " "
	    ENDIF 
	  
	    IF (MODE_PARAM_BILND.EQ.2) THEN 
	       WRITE(IDLOG,*,ERR=921) 
     &         "Proportion of components defined by the", 
     &	       "by the ratio AOT_coarse / AOT_totale :"
     	       WRITE(IDLOG,*,ERR=921) 
     &	       "   Reference wavelength (microns) : ",WAREF
     	       WRITE(IDLOG,*,ERR=921) 
     &	       "   --> Total aerosols extinction optical thickness : ",
     &         AOT_WAREF	    
	       WRITE(IDLOG,*,ERR=921) 
     &         "   --> ratio AOT coarse / totale : ",RTAUct_WAREF
               WRITE(IDLOG,*,ERR=921) " "
	       WRITE(IDLOG,*,ERR=921) 
     &	       "   --> Refractive index of the coarse mode "
	       WRITE(IDLOG,*,ERR=921) 
     & 	       "          - Real part : ",RN_LND_WAREF(1)
	       WRITE(IDLOG,*,ERR=921) 
     & 	       "          - Imaginary part : ",IN_LND_WAREF(1)
	       WRITE(IDLOG,*,ERR=921) 
     &	       "   --> Refractive index of the fine mode "
	       WRITE(IDLOG,*,ERR=921) 
     & 	       "          - Real part : ",RN_LND_WAREF(2)
	       WRITE(IDLOG,*,ERR=921) 
     & 	       "          - Imaginary part : ",IN_LND_WAREF(2)
	       WRITE(IDLOG,*,ERR=921) " "
	    ENDIF  	        	    
	 
         ENDIF !Fichier Trace	
      
      	 
C* Initialisation
C  --------------
 	 IGRANU_MMD_CALL =   1	! distribution Log Normal par défaut


C* Calcul des coefficients CVI pour une expression du
C  mélange en AOT_coarse / AOT_totale
C  --------------------------------------------------
         IF (MODE_PARAM_BILND.EQ.2) THEN
	    !Utilisation du rapport AOT_coarse / AOT_totale
 
	    IF (TRACE) THEN
	       WRITE (IDLOG,*,ERR=921) " "
	       WRITE (IDLOG,*,ERR=921) 
     &	       "Calculation of the mixture coefficients"
	       WRITE (IDLOG,*,ERR=921) 
     &	       "======================================="
               WRITE (IDLOG,*,ERR=921) "  "
	    ENDIF
	    
C* Pour chacun des deux modes (1 : coarse, 2 : fine)
C  -------------------------------------------------  
            DO 114 I=1,2  
		
C* Constitution du nom du fichier MIE (pour WAREF)
C  -----------------------------------------------	               
	       X = VAR_LND(I)  
	       RMAX = RMODAL_LND(I) * DEXP(X*X) *
     &	              DEXP(X*DSQRT(-2.D+00*LOG(CTE_COEF_NRMAX)))

               ALPHAF = REAL( 100 +
     &	       	 100*DINT(2.*INCTE_PI*RMAX / (100.*WAREF)) )  

               CALL SOS_NOM_FICMIE(NBMU_GAUSS,FICANGLES_USER,
     &	                           RN_LND_WAREF(I),IN_LND_WAREF(I),
     &	                           ALPHAMIN,ALPHAF,FICS)       
	       FICMIE = DIRMIE(1:LDIRMIE)//'/'//FICS
               LFICMIE=INDEX(FICMIE,' ')-1
	        
	       IF (TRACE) THEN
	          IF (I.EQ.1) THEN
	             WRITE(IDLOG,*,err=921) "   LND coarse mode"
		     WRITE(IDLOG,*,err=921) '   ==============='
	          ELSE !I=2 : fine mode
	             WRITE(IDLOG,*,err=921) "   LND fine mode" 
		     WRITE(IDLOG,*,err=921) '   =============' 
	          ENDIF	  
	          WRITE(IDLOG,*,err=921) ' '
	          WRITE(IDLOG,*,err=921) '      RMAX (microns) :',RMAX
	          WRITE(IDLOG,*,err=921) 
     &	          '      Wavelength (microns) : ',WAREF
	          WRITE(IDLOG,*,err=921) 
     &	          '      Max size parameter : ',ALPHAF	  
	          WRITE(IDLOG,*,err=921) ' ' 
		  WRITE(IDLOG,*,err=921) 
     &		  '      MIE file for WAREF : ',
     &            FICMIE(LDIRMIE+2:LFICMIE)
		  WRITE(IDLOG,*,err=921) ' '    
	       ENDIF

	    
C* Si le fichier MIE existe déjà, le programme n'est pas lancé
C  -----------------------------------------------------------           	              
               INQUIRE(FILE=FICMIE,EXIST=EX)
	    
	       IF (EX) THEN 
		  
                  IF (TRACE) THEN
	             WRITE (IDLOG,*,ERR=921)
     &	  	     '      --> MIE file has already been calculated'
		     WRITE (IDLOG,*,err=921) ' '
	          ENDIF
	  
	       ELSE   
	          !Affichage écran
                  WRITE(6,*)"MIE file : ", FICMIE(LDIRMIE+2:LFICMIE)
	          WRITE(6,*)'-- MIE file is being calculated'
		  CALL FLUSH(6)	!Force l'envoie du message à l'écran
	       
                  IF (TRACE) THEN
	             WRITE (IDLOG,*,ERR=921)
     &	  	     '      --> Generation of MIE file'
		     WRITE (IDLOG,*,err=921) ' '
	          ENDIF 
	       
	             
C* Calcul du fichier de MIE (pour WAREF)
C  ------------------------------------ 
                  CALL SOS_MIE(MIE_NBMU,XMU,XHR,
     &		               RN_LND_WAREF(I),IN_LND_WAREF(I),
     &                         ALPHAMIN,ALPHAF,
     &                         FICMIE,FICMIE_LOG,IER)
     	          IF (IER.NE.0) GOTO 993   ! cas d'erreur
         
               ENDIF
	 
C* Calcul des sections efficaces d'extinction et de diffusion 
C  et des fonctions de phase I, Q, U du constituant I pour WAREF
C  ------------------------------------------------------------- 	       
               CALL OSOAA_GRANU(FICMIE,IGRANU_MMD_CALL,RMODAL_LND(I),
     &                          VAR_LND(I),
     &                          CTE_NOT_DEFINED_VALUE_DBLE,
     &			        WAREF,MIE_NBMU,XMU,TRACE,
     &			        KMAT1I,KMAT2I,SOMME_NR,VMOY_MMD,
     &			        P11I,P12I,P33I,IER)
               IF (IER.NE.0) GOTO 995   ! cas d'erreur  

    	     
C* Stockage des coefficients d'extinction de chaque mode
C  ---------------------------------------------------------------
	       IF (I.EQ.1)  KMAT1c = KMAT1I	!mode coarse
	       IF (I.EQ.2)  KMAT1f = KMAT1I	!mode fine
	       
      	    	        
  114	    CONTINUE	 !Fin boucle sur les mode coarse et fine de la LND bimodale


C* Calcul des coefficients CVI de chaque mode
C  ---------------------------------------------------------------
            CVI(1)=   (RTAUct_WAREF * AOT_WAREF) / KMAT1c
	    CVI(2)=   ( (1.-RTAUct_WAREF)*AOT_WAREF) / KMAT1f

	    IF (TRACE) THEN
	       WRITE(IDLOG,*,err=921) 
     &	       '      Estimate : CV coarse mode (non-normalized):',
     &         CVI(1)
	       WRITE(IDLOG,*,err=921) 
     &	       '      Estimate : CV fine mode (non-normalized):',
     &         CVI(2)
	       WRITE(IDLOG,*,err=921) ' '  
	       WRITE(IDLOG,*,err=921) ' '   
	    ENDIF
			    
	 ENDIF !Fin test si utilisation du rapport AOT_coarse / AOT_tot

		
C* Normalisation à 1 de la somme des coefficients CVI de chaque mode
C  -----------------------------------------------------------------
        NTOT = CVI(1) + CVI(2)
	CVI(1) = CVI(1) / NTOT
        CVI(2) = CVI(2) / NTOT
	
        IF (TRACE) THEN
	    WRITE(IDLOG,*,err=921)'      CV coarse mode (normalized):', 
     &	     CVI(1)
	    WRITE(IDLOG,*,err=921)'      CV fine mode   (normalized):', 
     &	     CVI(2)	  
	    WRITE(IDLOG,*,err=921) ' '  
	    WRITE(IDLOG,*,err=921) ' '   
	ENDIF

	    
C* Pour chacun des deux modes (1 : coarse, 2 : fine)
C  ------------------------------------------------- 
	IF (TRACE) THEN
	    WRITE (IDLOG,*,ERR=921) 
     &	    "Calculation of the radiative properties of the mixture"
	    WRITE (IDLOG,*,ERR=921) 
     &	    "======================================================"
            WRITE (IDLOG,*,ERR=921) "  "
	 ENDIF
	    
	        
	 DO 112 I=1,2
	    	    
	    IF(CVI(I).EQ.0.) GOTO 112
		
C* Constitution du nom du fichier MIE (pour la longueur d'onde de simulation)
C  ----------------------------------	               
	    X = VAR_LND(I)  
	    RMAX = RMODAL_LND(I) * DEXP(X*X) *
     &	           DEXP(X*DSQRT(-2.D+00*LOG(CTE_COEF_NRMAX)))
     	
	    ALPHAF 
     &	    = REAL( 100+100*DINT(2.*INCTE_PI*RMAX / (100.*WA)) )  

            CALL SOS_NOM_FICMIE(NBMU_GAUSS,FICANGLES_USER,
     & 	                        RN_LND(I),IN_LND(I),
     &                          ALPHAMIN,ALPHAF,FICS)
	    FICMIE = DIRMIE(1:LDIRMIE)//'/'//FICS
            LFICMIE=INDEX(FICMIE,' ')-1  

	    IF (TRACE) THEN
	       IF (I.EQ.1) THEN
	          WRITE(IDLOG,*,err=921) "   1) LND coarse mode"
		  WRITE(IDLOG,*,err=921) '   =================='
	       ELSE !I=2 : fine mode
	          WRITE(IDLOG,*,err=921) "   2) LND fine mode" 
		  WRITE(IDLOG,*,err=921) '   ================' 
	       ENDIF	  
	       WRITE(IDLOG,*,err=921) ' '
	       WRITE(IDLOG,*,err=921) '      RMAX (microns) :',RMAX
	       WRITE(IDLOG,*,err=921) 
     &	       '      Wavelength (microns) : ',WA
	       WRITE(IDLOG,*,err=921) 
     &	       '      Max size parameter : ',ALPHAF	  
	       WRITE(IDLOG,*,err=921) ' ' 
	       WRITE(IDLOG,*,err=921) '      MIE file for WA : ',
     &		  FICMIE(LDIRMIE+2:LFICMIE)
	       WRITE(IDLOG,*,err=921) ' '    
	    ENDIF
	     
C* Si le fichier MIE existe déjà, le programme n'est pas lancé
C  -----------------------------------------------------------           	              
            INQUIRE(FILE=FICMIE,EXIST=EX)
	    
	    IF (EX) THEN  
	    	       
               IF (TRACE) THEN
	          WRITE (IDLOG,*,ERR=921)
     &        	  '      --> MIE file has already been calculated'
		  WRITE (IDLOG,*,err=921) ' '
	       ENDIF
	  
	    ELSE   
	       !Affichage écran
	       WRITE(6,*) "MIE file : ", FICMIE(LDIRMIE+2:LFICMIE)
	       WRITE(6,*)'-- MIE file is being calculated'
	       CALL FLUSH(6)	!Force l'envoie du message à l'écran
	       
               IF (TRACE) THEN
	          WRITE (IDLOG,*,ERR=921)
     &            '      --> Generation of MIE file'
		  WRITE (IDLOG,*,err=921) ' '
	       ENDIF  
	            
C* Calcul du fichier de MIE  (pour la longueur d'onde de simulation)
C  ------------------------ 
               CALL SOS_MIE(MIE_NBMU,XMU,XHR,
     &                      RN_LND(I),IN_LND(I),ALPHAMIN,ALPHAF,
     &                      FICMIE,FICMIE_LOG,IER)
     	       IF (IER.NE.0) GOTO 993   ! cas d'erreur
         
            ENDIF
	 
C* Calcul des sections efficaces d'extinction et de diffusion 
C  et des fonctions de phase I, Q, U du constituant I
C   (pour la longueur d'onde de simulation)
C  ---------------------------------------------------------- 
            CALL OSOAA_GRANU(FICMIE,IGRANU_MMD_CALL,RMODAL_LND(I),
     &                       VAR_LND(I),
     &                       CTE_NOT_DEFINED_VALUE_DBLE,
     &                       WA,MIE_NBMU,XMU,TRACE,
     &			     KMAT1I,KMAT2I,SOMME_NR,VMOYI,
     &                       P11I,P12I,P33I,IER)
            IF (IER.NE.0) GOTO 995   ! cas d'erreur  

		 
C* Calcul des coefficients d'extinction et de diffusion du mélange
C  ---------------------------------------------------------------
	    KMAT1 = KMAT1 + CVI(I)*KMAT1I
	    KMAT2 = KMAT2 + CVI(I)*KMAT2I
	    
	    DO 113 J=-MIE_NBMU,MIE_NBMU	     
	       P11(J) = P11(J) + CVI(I)*P11I(J)*KMAT2I
	       P12(J) = P12(J) + CVI(I)*P12I(J)*KMAT2I
	       P33(J) = P33(J) + CVI(I)*P33I(J)*KMAT2I	       
  113	    CONTINUE  !Fin boucle sur les angles de gauss


C* Estimation de l'indice de réfraction moyen
C  et du volume moyen des particules 
C  ----------------------------------------------------- 
	    RN_MOY = RN_MOY + CVI(I)*RN_LND(I)
	    V_MOY  =  V_MOY + CVI(I)*VMOYI
	        
  112	 CONTINUE	 !Fin boucle sur les mode coarse et fine de la LND bimodale
	    
	 		
		
C* Normalisation des fonctions de phase du mélange
C  -----------------------------------------------
	 DO 120 J=-MIE_NBMU,MIE_NBMU   
	    P11(J) = P11(J) / KMAT2
	    P12(J) = P12(J) / KMAT2
	    P33(J) = P33(J) / KMAT2
  120	 CONTINUE

C* Cas de particules sphériques : P22(J) = P11(J)
C  -----------------------------------------------------
      DO 1024 J=-MIE_NBMU,MIE_NBMU
         P22(J)=P11(J)
 1024 CONTINUE	 
 
C* Calcul des paramètres alpha, beta, gamma, delta, zeta
C  avec application ou non de la troncature
C  -----------------------------------------------------  
	 CALL SOS_DECOMPO_LEGENDRE(ITRONC,MU1_TRONCA,MU2_TRONCA,
     &			 	   TRACE,MIE_NBMU,XMU,XHR,OS_NB,
     &				   P11,TTT,P12,P22,P33,A_TRONCA,Z1,
     &				   ALP,BETA11,BETA22,
     &                             GAMMA12,DELTA33,ZETA,IER)     
         IF (IER.NE.0) GOTO 996   ! cas d'erreur	 

          	 
      ENDIF	!Fin test sur Modalite IMOD=3 (LND bimodale) 




C ==============================================================================
C* Données externes :
C* --> Fonctions de phase externes et coefficients de diffusion et d'extinction
C*
C*     NB : Développé sur la base des sorties de l'outil Oleg Dubovik (LOA)
C*          qui fournit les propriétés de diffusion primaire de particules
C*          sphéroides homogènes orientées de facon aléatoire.
C*          Un paramètre précisant le nombre d'angles décrivant les fonctions
C*          à toutefois dû être ajouté.
C*
C*          Bien faire attention au fait qu'on lit un rapport -F12 / F11.
C*          Importance du signe : F12 < 0 pour la diffusion rayleigh.

C ==============================================================================

      IF(IMOD.EQ.4) THEN

C* Lecture du fichier de données externes
C  -----------------------------------------------------------------
         OPEN(UNIT=20,FILE=FICEXTDATA,STATUS='OLD',ERR=940)
	 	 
	 READ(20,'(a)',ERR=941) LIGNE_TEXTE
         IFIN=INDEX(LIGNE_TEXTE,':')
         READ(LIGNE_TEXTE(IFIN+1:LENLINE),*) KMAT1
	 
         READ(20,'(a)',ERR=941) LIGNE_TEXTE
         IFIN=INDEX(LIGNE_TEXTE,':')
         READ(LIGNE_TEXTE(IFIN+1:LENLINE),*) KMAT2
	 
	 READ(20,'(a)',ERR=941) LIGNE_TEXTE
         IFIN=INDEX(LIGNE_TEXTE,':')
         READ(LIGNE_TEXTE(IFIN+1:LENLINE),*) NB_FICEXT_ANGLES

	 
	 !Contrôle du nombre d'angles par rapport à la taille des tableaux
	 IF (NB_FICEXT_ANGLES.GT.CTE_MAXNB_ANG_EXT) GOTO 950
	
	 READ(20,*,ERR=941) 

	 DO J=1,NB_FICEXT_ANGLES
		
            READ(20,*,ERR=941,END=942) 
     &	                 ANG_EXT(J), F11_EXT(J), F12sF11_EXT(J),
     &	                 F22sF11_EXT(J), F33sF11_EXT(J)
         ENDDO 
	
	 CLOSE(20)
	 
	 IF (TRACE) THEN
            WRITE (IDLOG,*,ERR=921) "EXTERNAL DATA (USER'S DATA)"
	    WRITE (IDLOG,*,ERR=921) "***************************"
	    WRITE (IDLOG,*,ERR=921) " "
	    WRITE (IDLOG,*,ERR=921)  "Filename : ",FICEXTDATA
	    WRITE (IDLOG,*,ERR=921) " "  		 
            WRITE (IDLOG,*,ERR=921) "Extinction coef : ", KMAT1
	    WRITE (IDLOG,*,ERR=921) "Scattering coef : ", KMAT2
	    WRITE (IDLOG,*,ERR=921) "Nb of angles : ", NB_FICEXT_ANGLES
	    WRITE (IDLOG,*,ERR=921) 
     &	    "  ANGLE     F11         -F12/F11      F22/F11     F33/F11"
	    DO J=1,NB_FICEXT_ANGLES
               WRITE (IDLOG,402,ERR=921) ANG_EXT(J), F11_EXT(J), 
     &	       			      F12sF11_EXT(J), F22sF11_EXT(J), 
     &	                              F33sF11_EXT(J)
            ENDDO 
	    WRITE (IDLOG,*,ERR=921) " "
         ENDIF !Fichier Trace
	 

C* Passage aux fonctions F12, F22 et F33 pour les mu des angles 
C* décrivant les fonctions externes
C  -----------------------------------------------------------------	 
	 DO J=1,NB_FICEXT_ANGLES
	    MU_EXT(J)  = DCOS(ANG_EXT(J)*INCTE_PI/180.D+00)
            F12_EXT(J) = -F12sF11_EXT(J)*F11_EXT(J)
	    F22_EXT(J) = F22sF11_EXT(J)*F11_EXT(J)
	    F33_EXT(J) = F33sF11_EXT(J)*F11_EXT(J)
         ENDDO 
	 
C* Passage aux fonctions F11, F12, F22 et F33 pour les mu de Gauss 
C  -----------------------------------------------------------------	

C*       P11 : L'appel de la routine se fait en passant XMU et P11
C*             qui poitent sur le premier élément utile
C*             (car le domaine couvert est plus grand que celui utile
C*             et qu'il y a un début de table à indice négatif)
C*       ---	
	 CALL SOS_INTERPO_SPLINT(NB_FICEXT_ANGLES, MU_EXT, F11_EXT,
     &	                         2*MIE_NBMU+1, XMU(-MIE_NBMU), 
     &                           P11(-MIE_NBMU), IER)	
	 IF (IER.NE.0) GOTO 945   ! cas d'erreur
	
C*       P12 :
C*       ---	
	 CALL SOS_INTERPO_SPLINT(NB_FICEXT_ANGLES, MU_EXT, F12_EXT,
     &	                         2*MIE_NBMU+1, XMU(-MIE_NBMU), 
     &                           P12(-MIE_NBMU), IER)	
	 IF (IER.NE.0) GOTO 945   ! cas d'erreur
	
C*       P22 :
C*       ---	
	 CALL SOS_INTERPO_SPLINT(NB_FICEXT_ANGLES, MU_EXT, F22_EXT,
     &	                         2*MIE_NBMU+1, XMU(-MIE_NBMU), 
     &                           P22(-MIE_NBMU), IER)	
	 IF (IER.NE.0) GOTO 945   ! cas d'erreur
		
C*       P33 :
C*       ---	
	 CALL SOS_INTERPO_SPLINT(NB_FICEXT_ANGLES, MU_EXT, F33_EXT,
     &	                         2*MIE_NBMU+1, XMU(-MIE_NBMU), 
     &                           P33(-MIE_NBMU), IER)		
	 IF (IER.NE.0) GOTO 945   ! cas d'erreur 
	
	 
	 IF (TRACE) THEN
	    WRITE (IDLOG,*,ERR=921) 
     &	    " Functions F11, F12, F22 and F33 for"
	    WRITE (IDLOG,*,ERR=921)  
     &	    " the original angles of the datafile"
	    WRITE (IDLOG,*,ERR=921)  
     &	    " ==================================="
	    WRITE (IDLOG,*,ERR=921) 
     &	    "  ANGLE     F11          F12          F22          F33"
	    DO J=1,NB_FICEXT_ANGLES
               WRITE (IDLOG,402,ERR=921) ANG_EXT(J), F11_EXT(J), 
     &	       			     F12_EXT(J), F22_EXT(J), F33_EXT(J)
            ENDDO 
	    WRITE (IDLOG,*,ERR=921) " " 
	    WRITE (IDLOG,*,ERR=921) " Functions  F11, F12, F22 and F33"
	    WRITE (IDLOG,*,ERR=921) " for the simulation angles"
	    WRITE (IDLOG,*,ERR=921) " ================================" 
	    DO J=-MIE_NBMU, MIE_NBMU
	       TETA=DACOS(XMU(J))*180./INCTE_PI
               WRITE(IDLOG,402,ERR=921) TETA,P11(J),P12(J),P22(J),
     &	                                P33(J)
            ENDDO 	  		
         ENDIF !Fichier Trace	 
	 
	 
C* Calcul des paramètres alpha, beta, gamma, delta, zeta
C  avec application ou non de la troncature
C  -----------------------------------------------------
	 CALL SOS_DECOMPO_LEGENDRE(ITRONC,MU1_TRONCA,MU2_TRONCA,
     &			 	   TRACE,MIE_NBMU,XMU,XHR,OS_NB,
     &				   P11,TTT,P12,P22,P33,A_TRONCA,Z1,
     &				   ALP,BETA11,BETA22,
     &                             GAMMA12,DELTA33,ZETA,IER)  
         IF (IER.NE.0) GOTO 996   ! cas d'erreur

C* Indice de réfraction moyen et du volume moyen des particules 
C  ==> Non défini pour les mesures externes
C  -----------------------------------------------------  
	 RN_MOY = CTE_NOT_DEFINED_VALUE_DBLE
	 V_MOY  = CTE_NOT_DEFINED_VALUE_DBLE
	 
      ENDIF	!Fin test sur Modalite IMOD=4 (données externes)
      
      
      
      
      
      
C ======================================
C* Calculs communs pour tous les modèles
C ======================================
	 
C* Calcul de l'albédo de simple diffusion
C-------------------------------------------
      PIZ=KMAT2/KMAT1

C* Troncature de l'albédo de simple diffusion des aérosols
C---------------------------------------------------------
      PIZTR = PIZ*(1.-A_TRONCA/2.)/(1.-PIZ*A_TRONCA/2.)      
    
C* Ecriture du fichier trace
C-------------------------------------------
      IF (TRACE) THEN
         WRITE(IDLOG,*,err=921) ' '
	 WRITE(IDLOG,*,err=921) ' '
	 WRITE(IDLOG,*,err=921) '=================================='
         WRITE(IDLOG,*,err=921) '* AEROSOL COMPUTATIONS: RESULTS  *'
	 WRITE(IDLOG,*,err=921) '=================================='
         WRITE(IDLOG,*,err=921) ' '
	 WRITE(IDLOG,*,err=921) ' '
	
	 IF(ITRONC.EQ.1) THEN
	      WRITE(IDLOG,*,err=921) 
     &	      '  Phase function truncation applied '
	 ELSE
	      WRITE(IDLOG,*,err=921) '  No phase function truncation'   
	 ENDIF
	 
         WRITE(IDLOG,*,err=921) ' '
            	 
	 IF (A_TRONCA.NE.0.D+00) THEN
	    WRITE(IDLOG,*,err=921) '  Truncation coefficient : ',
     &	    A_TRONCA
	    WRITE(IDLOG,*,err=921) 
     &	    '  Extinction cross-section (mic^2) - no trunca. applied:',
     &          KMAT1
	    WRITE(IDLOG,*,err=921) 
     &	    '  Scattering cross-section (mic^2) - no trunca. applied:',
     &          KMAT2
	    WRITE(IDLOG,*,err=921) 
     &	    '  Mean refractive index (real part) : ',RN_MOY
	    WRITE(IDLOG,*,err=921) 
     &	    '  Mean volume of a particle (micron^3) : ',V_MOY
	    WRITE(IDLOG,*,err=921) 
     &	    '  Single scattering albedo - adjusted to the truncation:',
     &      PIZTR 
	    WRITE(IDLOG,*,err=921) 
     &	    '  Single scattering albedo - no trunca. applied: ',
     &         PIZTR / (1.-A_TRONCA/2.+PIZTR*A_TRONCA/2.)      
	    WRITE(IDLOG,*,err=921)
     &	    '  Beta11(0) non-normalized - adjusted to the truncation: ',
     &      Z1  
            WRITE(IDLOG,*,err=921) 
     &	    '  Asymmetry factor - adjusted to the truncation: ',
     &      BETA11(1)/3.
            WRITE(IDLOG,*,err=921) 
     &	    '  Asymmetry factor - no trunca. applied: ',
     &         A_TRONCA/2.+(1.-A_TRONCA/2.)*BETA11(1)/3. 
     
            WRITE(IDLOG,*,err=921) ' '
	    WRITE(IDLOG,*,err=921)
     &        '  1st column : Scattering angle'
	    WRITE(IDLOG,*,err=921) 
     &	      '  2nd column : Truncated phase function P11'
	    WRITE(IDLOG,*,err=921)
     &        '  3rd column : Original phase function P11 '
	    WRITE(IDLOG,*,err=921) 
     &        '  4th column : Truncated polarized phase function '
            WRITE(IDLOG,*,err=921) '		     P12(mu) truncated'
	    WRITE(IDLOG,*,err=921)
     &        '  5th column : Original polarized phase function '
            WRITE(IDLOG,*,err=921) '		     P12(mu) '
	    WRITE(IDLOG,*,err=921)
     &        '  6th column : Truncated polarized phase function '
            WRITE(IDLOG,*,err=921) '                 P33(mu) truncated'
	    WRITE(IDLOG,*,err=921) 
     &        '  7th column : Original polarized phase function '
            WRITE(IDLOG,*,err=921) '                 P33(mu) '
	    WRITE(IDLOG,*,err=921) ' '	    
            DO 44 J=-MIE_NBMU,MIE_NBMU
	       TETA=DACOS(XMU(J))*180./INCTE_PI
	       WRITE(IDLOG,10,err=921) TETA,P11(J),TTT(J),
     &                              P12(J)*P11(J)/TTT(J),P12(J),
     &                              P33(J)*P11(J)/TTT(J),P33(J)
   44       CONTINUE    
	 ELSE
	    WRITE(IDLOG,*,err=921) 
     & 	      '  Single scattering albedo - no trunca. applied : ',
     &           PIZTR
	    WRITE(IDLOG,*,err=921) 
     & 	      '  Beta11(0) non-normalized - no trunca. applied : ',Z1
            WRITE(IDLOG,*,err=921) 
     &	      '  Asymmetry factor - no trunca. applied : ',BETA11(1)/3.
     	 ENDIF !Fin test si troncature   


    
         WRITE(IDLOG,*,err=921) ' '  
	 WRITE(IDLOG,35,err=921)'I','ALP(I)','BETA11(I)','GAMMA12(I)',
     &                       'DELTA33(I)','ZETA(I)'
         DO 45 K=0,OS_NB
	    WRITE(IDLOG,25,err=921) K,ALP(K),BETA11(K),GAMMA12(K),
     &	                            DELTA33(K),ZETA(K)
   45    CONTINUE
   	 WRITE(IDLOG,*,err=921) ' '
	 	   	   	   	   	   
      ENDIF   !Trace   

      
1111  CALL OSOAA_ENREG_GRANU(KMAT1, KMAT2, A_TRONCA, PIZTR,
     &                       CTE_NOT_DEFINED_VALUE_DBLE, V_MOY, RN_MOY,
     &                       OS_NB, ALP, BETA11, GAMMA12, ZETA,
     &                       FICGRANU,IER)

      IF (IER.NE.0) GOTO 998   ! cas d'erreur
   
      
C* Fermeture fichiers
C-------------------
C
C    NB : On ne ferme pas le fichier TRACE car il est enrichi pour les 
C         différents appels de la routine OSOAA_AEROSOLS (appel pour les 
C         différentes longueurs d'onde).
      
      
C* Fin nominale 
C-------------------
      GOTO 9999
            
C* Cas d'erreur et retour du status -1 au programme appelant  
C----------------------------------------------
  825 WRITE(6,*) '    OSOAA_AEROSOLS : ERROR_825 :'
      WRITE(6,*) '  --> User volumetric concentrations for WMO models'
      WRITE(6,*) '      have to be defined.'
      GOTO 9998  
      
 911  WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_911 on logfile opening' 
      GOTO 9998 
      
 921  WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_921 on logfile writing' 
      GOTO 9998
     
 925  WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_925: '
      WRITE(6,*) '  => Error while getting OSOAA_ROOT variable'
      GOTO 9998

 926  WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_926: '
      WRITE(6,*) '  => Length of $OSOAA_ROOT/fic is too long' 
      WRITE(6,*) '  => CTE_LENDIR has to be adjusted.' 
      GOTO 9998

 927  WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_927: '
      WRITE(6,*) '  => Error on DIRMIE : maybe too long'
      WRITE(6,*) '     (Name of the storage directory for MIE files)'
      WRITE(6,*) '  => Check if CTE_LENDIR has to be adjusted.' 
      GOTO 9998
                  
 930  WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_930: '
      WRITE(6,*) '  => Error on angles file opening'
      GOTO 9998  

 931  WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_931: '
      WRITE(6,*) '  => Error on angles file reading'
      GOTO 9998  

  940 WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_940:'
      WRITE(6,*) '  on external phase functions file opening'
      GOTO 9998
      
  950 WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_950:  '
      WRITE(6,*) '  The number of angles for the external phase'
      WRITE(6,*) '  functions description is higher than the defined '
      WRITE(6,*) '  size of internal tables'
      WRITE(6,*) '  --> Check the value of CTE_MAXNB_ANG_EXT '
      WRITE(6,*) '      in OSOAA.h compared to the value associated'
      WRITE(6,*) '      to NbAng in the external phase functions file'
      GOTO 9998

  941 WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_941:'
      WRITE(6,*) '  on external phase functions file reading'
      GOTO 9998
            
  942 WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_942:'
      WRITE(6,*) '  on external phaze functions file reading : '
      WRITE(6,*) '  End of file encountered'
      WRITE(6,*) '  --> Check if the value associated to NbAng is '
      WRITE(6,*) '      correct, in the external phase functions file'
      GOTO 9998 
          
  945 WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_945:'
      WRITE(6,*) '  while interpolating external phaze functions.'
      WRITE(6,*) '  --> Check that the abscissas of the phaze '
      WRITE(6,*) '      functions are correct : '
      WRITE(6,*) '  	   do not use twice same values'
      GOTO 9998            

 
  993 WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_993 on subroutine SOS_MIE'
      GOTO 9998
      
  994 WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_994 '
      WRITE(6,*) '                   on subroutine SOS_INIT_PARAMWMO'
      GOTO 9998
      
  995 WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_995 on sub. OSOAA_GRANU'
      GOTO 9998 
      
  996 WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_996 '
      WRITE(6,*) '    on subroutine SOS_DECOMPO_LEGENDRE'
      GOTO 9998
      
  997 WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_997 '
      WRITE(6,*) '  		     on subroutine SOS_INIT_PARAMSF'
      GOTO 9998 

  998 WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_998 '
      WRITE(6,*) '  		     on subroutine OSOAA_ENREG_GRANU'
      GOTO 9998 
      
 1009 WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_1009 '
      WRITE(6,*) '      on max value of parameter size ALPHAF '
      GOTO 9998 
       
 1010 WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_1010 '
      WRITE(6,*) '   --> imaginary part of refractive'
      WRITE(6,*) '       indexes have to be negative'
      GOTO 9998         
                    
 1011 WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_1011 '
      WRITE(6,*) '  ERROR on -AER.MMD.SDtype value  : '
      WRITE(6,*) '  --> out of available range for mono-modal model.'
      GOTO 9998 

 1012 WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_1012 '
      WRITE(6,*) '  ERROR on -AER.WMO.Model or -AER.SF.Model value  : '
      WRITE(6,*) '  --> out of available range for WMO '
      WRITE(6,*) '      or Shettle&Fenn model option.'
      GOTO 9998
      
 1013 WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_1013 '
      WRITE(6,*) '  ERROR on -AER.SF.RH : Relative Humidity (0 to 99%)'
      GOTO 9998           
         
 1015 WRITE(6,*) '  OSOAA_AEROSOLS : ERROR_1015 '
      WRITE(6,*) '  ERROR on -AER.BMD.VDdef : Option for volumetric '
      WRITE(6,*) '  concentration definition for bimodal LND (1 or 2)'
      GOTO 9998
                        
9998  IER=-1             
9999  RETURN   


C* Formats
C--------- 
 
   10 FORMAT(1X,F6.2,6(2X,E12.4))
   25 FORMAT(1X,I3,5(2X,E13.5))
   35 FORMAT(3X,A1,5X,A6,9X,A9,6X,A10,5X,A10,5X,A7)
   
  402 FORMAT(F7.2,4E14.5)
       
  222 FORMAT(I4,1X,D21.14,2X,F7.3,2X,D21.14,2X)
  
      END      !FIN DE LA PROCEDURE OSOAA_AEROSOLS
      
      
      
      
      
 
C==============================================================================
C PROCEDURE: SOS_INIT_PARAMWMO
C ==========
C
C      Cette procédure initialise l'ensemble des paramètres de chaque
C      constituant de la WMO pour la longueur d'onde de simulation :
C      	- le rayon modal
C	- la variance 
C	- concentration volumique
C	- partie réelle de l'indice de réfraction
C	- partie imaginaire
C
C      Conversion de la variance du fichier de données WMO (en log décimal)
C      en variance conforme à la définition des LND dans OSOAA: VAR_WMO * LOG(10)
C
C      Interpolation linéaire des indices pour la longueur d'onde d'entrée.
C
C      Force les indices résultats au format F5.3 pour la partie réelle
C      et F8.5 pour la partie imaginaire (valeur négative).
C
C
C Description des paramètres entrée / sortie
C ------------------------------------------
C	FICDATAWMO (CHARACTER*CTE_LENFIC2)	: (E) chemin et nom du fichier Data_WMO
C	WA (double)	: (E) longueur d'onde sélectionnée
C       VARGRANU1I(SOS_NB_WMO) (double) : (S) rayon modal de chaque constituant
C       VARGRANU2I(SOS_NB_WMO) (double) : (S) variance de chaque constituant
C	MR(SOS_NB_WMO) (double)	: (S) partie réelle de l'indice de réfraction
C       MI(SOS_NB_WMO) (double) : (S) partie imaginaire de l'indice de réfraction
C	V(SOS_NB_WMO) (double)	: (S) concentration volumique micron3/cm3
C	IER (I4)	: (S) code d'erreur = 0 si pas d'erreur, sinon =-1	
C
C 
C Description des fichiers utilisés
C ---------------------------------
C   -->Contenu du fichier des données de la WMO
C      Fichier Ascii, format d'un enregistrement :  et 
C      1ere ligne : 4(1X,F9.5)
C                   valeur du rayon modal (mic) 
C		    pour les constituants DL,WS,OC,SO
C      2eme ligne : 4(1X,F9.5)
C                   log décimal de la variance 
C		    pour les constituants DL,WS,OC,SO  
C      3eme ligne : 2(1X,F9.5,1X,E12.5)
C                   concentration volumique (mic**3/cm**3) 
C		    pour les constituants DL,WS,OC,SO        
C      4eme ligne à la fin : 9(1X,F9.5)
C                            longueur d'onde, partie réelle et imaginaire
C			     de l'indice de réfraction de chaque constituant.   
C
C
C Constantes utilisées :
C --------------------
C    CTE_LENFIC2 : Longueur des noms de fichiers avec arborescence.
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------    
C     -Ouverture d'un fichier
C       Erreur si le fichier des données de la WMO n'existe pas
C     -Lecture dans un fichier
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant
C
C
C  Sous programmes utilisés:
C --------------------------
C  Ce programme fait appel à la routine: SOS_INTERPOL
C
C==============================================================================
      SUBROUTINE SOS_INIT_PARAMWMO(FICDATAWMO,WA,VARGRANU1I,
     &				   VARGRANU2I,MR,MI,V,IER)

      IMPLICIT NONE
      
C Constantes AEROSOLS
C -------------------

      INTEGER SOS_NB_WMO	!Nombre de types d'aérosols de la WMO utilisés: DL,WS,OC,SO
      PARAMETER (SOS_NB_WMO=4)

      INTEGER SOS_NB_AERO	!Dimension des tableaux résultats 
      PARAMETER (SOS_NB_AERO=5)	!(4 composants WMO, 5 composants Shettle & Fenn)	
      
            
C* Définition des variables       
C*-----------------------------------------

      CHARACTER*CTE_LENFIC2 FICDATAWMO	! chemin et nom du fichier Data_WMO
      
      DOUBLE PRECISION WA	! Longueur d'onde en microns sélectionnée.	
      DOUBLE PRECISION WA1	! Longueur d'onde en microns lue dans le
      DOUBLE PRECISION WA2	! fichier Data_WMO.
      
      DOUBLE PRECISION VARGRANU1I(SOS_NB_AERO)   ! Paramètre 1 de la granulométrie
      						 ! pour chaque modèle de constituant.
      DOUBLE PRECISION VARGRANU2I(SOS_NB_AERO)   ! Paramètre 2 de la granulométrie
      						 ! pour chaque modèle de constituant.
      
      DOUBLE PRECISION MR(SOS_NB_AERO)  ! Partie réelle de l'indice de réfraction
      					! pour chaque modèle d'aérosols.
      DOUBLE PRECISION MI(SOS_NB_AERO)	! Partie imaginaire de l'indice de réfraction.
      					! pour chaque modèle d'aérosols.
      DOUBLE PRECISION MR1(SOS_NB_WMO)	!
      DOUBLE PRECISION MI1(SOS_NB_WMO)  ! utilisé pour le stockage de 
      DOUBLE PRECISION MR2(SOS_NB_WMO)  ! valeurs intermédiaires
      DOUBLE PRECISION MI2(SOS_NB_WMO)  !
      
      DOUBLE PRECISION V(SOS_NB_WMO)	!Concentration volumique des constituants
      
      INTEGER*4 I	   !Indice.       
      INTEGER*4 IER        ! code d'erreur =0 si pas d'erreur, =-1 sinon
      
C* Initialisation
C  --------------
       IER=0	 
       
       DO I=1,SOS_NB_WMO
          VARGRANU1I(I)=0.
          VARGRANU2I(I)=0.
	  MR(I)=0.
    	  MI(I)=0.
	  V(I)=0.
       ENDDO

C* Ouverture du fichier contenant les données de la WMO
C  ---------------------------------------------------	 
       OPEN(8,FILE=FICDATAWMO,STATUS='OLD',ERR=991)

C* Lecture du rayon modal
C  ---------------------- 	 
       READ(8,333,err=992) VARGRANU1I(1),VARGRANU1I(2),
     &                     VARGRANU1I(3),VARGRANU1I(4)
     	  
C* Lecture du log décimal de la variance
C  ------------------------------------- 	 
       READ(8,333,err=992) VARGRANU2I(1),VARGRANU2I(2),
     &                     VARGRANU2I(3),VARGRANU2I(4)      

C* Conversion en variance pour LND compatible avec la définition de OSOAA
C  ------------------------------------- 	 
       DO I=1,4
          VARGRANU2I(I) = VARGRANU2I(I) * DLOG(10.D+00)
       ENDDO
     
C* Lecture de la concentration volumique
C  ------------------------------------- 	 
       READ(8,444,err=992) V(1),V(2),V(3),V(4) 

C* Détermination des indices de réfraction par interpolation linéaire
C  ------------------------------------------------------------------

C* Initialisation avec les premières valeurs du fichier
C  ---------------------------------------------------- 
       READ(8,555,END=999,err=992)WA1,MR1(1),MI1(1),MR1(2),MI1(2),
     &                            MR1(3),MI1(3),MR1(4),MI1(4)

C* Pour chaque ligne du fichier, on recherche la longueur d'onde
C  juste supérieure à WA sélectionnée
C  --------------------------------------------------------------
  882  READ(8,555,END=999,err=992)WA2,MR2(1),MI2(1),MR2(2),MI2(2),
     &                            MR2(3),MI2(3),MR2(4),MI2(4)
     
       ! WA1<=WA<WA2, interpolation des indices	     
       IF((WA1.LE.WA).AND.(WA.LE.WA2)) THEN
       
	  DO I=1,SOS_NB_WMO	  
	     CALL SOS_INTERPOL(MR1(I),MR2(I),WA1,WA2,WA,MR(I))
	     CALL SOS_INTERPOL(MI1(I),MI2(I),WA1,WA2,WA,MI(I))
	     
C******** Conversion aux formats F5.3 et F8.3	   
	     MR(I) =  DNINT( MR(I)*1000.D+00) / 1000.D+00
	     MI(I) = -DNINT(-MI(I)*100000.D+00) / 100000.D+00	    
	  ENDDO
	  GOTO 881
	       
       ELSE
       	 
          WA1=WA2	! Ré-initialisation: ligne1 recoit ligne2
	  DO I=1,SOS_NB_WMO
	     MR1(I)=MR2(I)
	     MI1(I)=MI2(I)
     	  ENDDO
	  GOTO 882	! passage à la ligne suivante
	       
       ENDIF

  999  CONTINUE
  	    
  881  CONTINUE
       CLOSE(8)

C* Fin nominale 
C-------------------
       GOTO 9999
              
C* Cas d'erreur et retour du status -1 au programme appelant
C----------------------------------------------------------
  991  WRITE(6,*) 'Error while opening the WMO datafile'
       IER=-1
       GOTO 9999
  992  WRITE(6,*) 'Error while reading the WMO datafile'
       IER=-1
       GOTO 9999

C* Format
C---------   
  333  FORMAT(4(1X,F9.5))
  444  FORMAT(1X,F9.5,1X,E12.5,1X,F9.5,1X,E12.5)
  555  FORMAT(9(1X,F9.5))

       
 9999  CONTINUE
 
       RETURN
       END	!FIN DE LA PROCEDURE SOS_INIT_PARAMWMO



C==============================================================================
C PROCEDURE: SOS_INIT_PARAMSF
C ==========
C
C      Cette procédure initialise l'ensemble des paramètres de chaque
C      constituant des modèles de Shettle et Fenn pour la longueur d'onde 
C      et l'humidité relative sélectionnées :
C      	- le rayon modal
C	- la variance
C	- partie réelle de l'indice de réfraction
C	- partie imaginaire
C
C
C      Conversion de la variance du fichier de données S&F (en log décimal)
C      en variance conforme à la définition des LND dans OSOAA: VAR_SF * LOG(10)
C
C Description des paramètres
C --------------------------
C	DIRE (CHARACTER*CTE_LENDIR)  : (E) chemin d'accès au fichier de données
C	WA (double)	      : (E) longueur d'onde sélectionnée
C	RH (double)	      : (E) humidité relative
C       VARGRANU1I(SOS_NB_AERO) (double) : (S) rayon modal de chaque constituant
C       VARGRANU2I(SOS_NB_AERO) (double) : (S) variance de chaque constituant
C	MR(SOS_NB_AERO) (double) : (S) partie réelle de l'indice de réfraction
C       MI(SOS_NB_AERO) (double) : (S) partie imaginaire de l'indice de réfraction
C	IER (I4) : (S) code d'erreur = 0 si pas d'erreur, sinon =-1	
C
C 
C Description des fichiers utilisés
C ---------------------------------
C   -->Contenu du fichier des paramètres de granulométrie des composants
C      de Shettle et Fenn :
C      Fichier Ascii, format d'un enregistrement:
C      1ere ligne : 5(1X,F9.5)
C		    log décimal de la variance 
C		    pour les constituants SR,LR,SU,LU et OM    
C      2eme ligne à la fin : 1X,F5.2,5(1X,F9.5)
C			     % d'humidité relative, rayon modal
C			     de chaque constituant.   
C
C   -->Contenu des fichiers IRefrac_XX (avec XX = SR, LR ...)
C      Fichier Ascii, format d'un enregistrement 17(1X,F9.5)
C      pour chaque ligne: longeur d'onde, partie réelle et partie imaginaire 
C			  de l'indice de réfraction pour 8 humidités 	
C			  relatives : 0%, 50%, 70%, 80%, 90%, 95%, 98%, 99%.
C
C
C Constantes utilisées :
C --------------------
C    CTE_LENDIR : Longueur des noms de répertoires.
C    CTE_LENFIC2 : Longueur des noms de fichiers avec arborescence.
C    CTE_AER_DATASF  : Nom du fichier contenant rayon modal et log variance S&F.
C    CTE_AER_XX_SF   : Nom des fichiers contenant les indices de réfraction S&F
C		       pour chaque constituant XX = SR, LR, SU, LU, OM.
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------    
C     -Ouverture d'un fichier
C       Erreur si l'un des fichiers de données n'existe pas
C     -Lecture dans un fichier
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant
C
C  Sous programmes utilisés:
C --------------------------
C  Ce programme fait appel à la routine: SOS_INTERPOL
C
C==============================================================================
      SUBROUTINE SOS_INIT_PARAMSF(DIRE,WA,RH,VARGRANU1I,
     &                         	  VARGRANU2I,MR,MI,IER)

      IMPLICIT NONE
      
C Constantes AEROSOLS
C -------------------

      INTEGER SOS_NB_SF	        !Nombre des composants des modèles Shettle & Fenn
      PARAMETER (SOS_NB_SF=5)

      INTEGER SOS_NB_RH		!Nombre de valeurs d'humidité relative
      PARAMETER (SOS_NB_RH=8)
 
      INTEGER SOS_NB_AERO	!Dimension des tableaux résultats 
      PARAMETER (SOS_NB_AERO=5)	!(4 composants WMO, 5 composants Shettle & Fenn)	
      
            
C* Définition des variables       
C*-----------------------------------------

      CHARACTER*CTE_LENDIR DIRE	! Chemin d'accès aux fichiers S&F
      CHARACTER*CTE_LENFIC2 FICDATASF	! Nom du fichier Data_SF
      CHARACTER*CTE_LENFIC2 FIC_SR_SF 	! Fichier IRefrac_SR
      CHARACTER*CTE_LENFIC2 FIC_LR_SF 	! Fichier IRefrac_LR
      CHARACTER*CTE_LENFIC2 FIC_SU_SF 	! Fichier IRefrac_SU
      CHARACTER*CTE_LENFIC2 FIC_LU_SF 	! Fichier IRefrac_LU
      CHARACTER*CTE_LENFIC2 FIC_OM_SF 	! Fichier IRefrac_OM
      
      DOUBLE PRECISION WA	! Longueur d'onde en microns sélectionnée.	
      DOUBLE PRECISION WA1	! Longueur d'onde en microns lue dans les fichiers
      DOUBLE PRECISION WA2	
      
      DOUBLE PRECISION RH	! Humidité relative (%)
      DOUBLE PRECISION RH1	! Humidité relative lue dans les fichiers
      DOUBLE PRECISION RH2	!
      
      DOUBLE PRECISION VARGRANU1I(SOS_NB_AERO)  ! Paramètre 1 de la granulométrie
      						! pour chaque modèle de constituant.
      DOUBLE PRECISION VARGRANU2I(SOS_NB_AERO)  ! Paramètre 2 de la granulométrie
      						! pour chaque modèle de constituant.
      
      DOUBLE PRECISION MR(SOS_NB_AERO)  ! partie réelle de l'indice de réfraction
      					! pour chaque modèle d'aérosols.
      DOUBLE PRECISION MI(SOS_NB_AERO)	! partie imaginaire de l'indice de réfraction.
      					! pour chaque modèle d'aérosols.
      
      DOUBLE PRECISION MR_CPT1		!
      DOUBLE PRECISION MR_CPT2		! Valeurs intermédiaires
      DOUBLE PRECISION MI_CPT1		!
      DOUBLE PRECISION MI_CPT2		!
      
      DOUBLE PRECISION RM1(SOS_NB_SF)	!
      DOUBLE PRECISION RM2(SOS_NB_SF)	! utilisé pour le stockage de
      DOUBLE PRECISION MR1(SOS_NB_RH)   ! valeurs intermédiaires 
      DOUBLE PRECISION MR2(SOS_NB_RH)   ! 
      DOUBLE PRECISION MI1(SOS_NB_RH)   ! 
      DOUBLE PRECISION MI2(SOS_NB_RH)   !
	
      INTEGER*4 I,H	   ! Indice
      INTEGER*4 CPT	   ! Compteur 
      INTEGER*4 IFIN	   ! Indicateur de fin d'une chaine de caractères
      
      INTEGER*4 IER        ! code d'erreur =0 si pas d'erreur, =-1 sinon
      
C* Initialisation
C  --------------
       IER=0
       
       DO I=1,SOS_NB_SF
          VARGRANU1I(I)=0.
          VARGRANU2I(I)=0.	  
	  MR(I)=0.
    	  MI(I)=0. 
       ENDDO

       
       ! Initialisation des noms des différents fichiers contenant 
       ! les données du modèle S&F
       
       IFIN=INDEX(DIRE,' ')
       IFIN=IFIN-1
       IF (IFIN.LE.0) IFIN=CTE_LENDIR
      
       FICDATASF = DIRE(1:IFIN)//'/'//CTE_AER_DATASF
       FIC_SR_SF = DIRE(1:IFIN)//'/'//CTE_AER_SR_SF
       FIC_LR_SF = DIRE(1:IFIN)//'/'//CTE_AER_LR_SF
       FIC_SU_SF = DIRE(1:IFIN)//'/'//CTE_AER_SU_SF
       FIC_LU_SF = DIRE(1:IFIN)//'/'//CTE_AER_LU_SF
       FIC_OM_SF = DIRE(1:IFIN)//'/'//CTE_AER_OM_SF

C* Ouverture du fichier contenant les paramètres de granulométrie
C* des composants de S&F
C  --------------------------------------------------------------
       OPEN(9,FILE=FICDATASF,STATUS='OLD',ERR=991)
   	  
C* Lecture du log décimal de la variance
C  ------------------------------------- 	 
       READ(9,222,err=993) VARGRANU2I(1),VARGRANU2I(2),VARGRANU2I(3),
     &                     VARGRANU2I(4),VARGRANU2I(5) 

C* Conversion en variance pour LND compatible avec la définition de OSOAA
C  ------------------------------------- 	 
       DO I=1,5
          VARGRANU2I(I) = VARGRANU2I(I) * DLOG(10.D+00)
       ENDDO
                 
C* Lecture du rayon modal en fonction de l'humidité relative
C  --------------------------------------------------------- 	 
       READ(9,333,err=993) RH1,RM1(1),RM1(2),
     &                      RM1(3),RM1(4),RM1(5)
        
	CPT=1
	
	! Cas ou RH = 0%
	IF(RH1.EQ.RH) THEN
	
	   DO I=1,SOS_NB_SF
	      VARGRANU1I(I)=RM1(I)
	   ENDDO
	   GOTO 884
	   
	ENDIF

C* Recherche des valeurs de RH encadrant la valeur sélectionnée
C  ------------------------------------------------------------
  883   READ(9,333,err=993) RH2,RM2(1),RM2(2),
     &                      RM2(3),RM2(4),RM2(5)
     
        CPT=CPT+1	! Compteur indiquant la position de 
			! la valeur RH2 juste supérieure à RH
	
	! Cas ou RH1<RH<=RH2, interpolation des indices de réfraction
	IF((RH1.LT.RH).AND.(RH.LE.RH2)) THEN
	
	   DO I=1,SOS_NB_SF	  
	      CALL SOS_INTERPOL(RM1(I),RM2(I),RH1,RH2,RH,VARGRANU1I(I))
     	   ENDDO
	   GOTO 884
	       
        ELSE	! Ré-initialisation des valeurs
       
          RH1=RH2
	  DO I=1,SOS_NB_SF
	     RM1(I)=RM2(I)
     	  ENDDO
	  GOTO 883	! passage à la ligne suivante dans le fichier	       
	  
       ENDIF
       
  884  CONTINUE


C* Ouverture des fichiers contenant l'indice de réfraction pour 
C  chaque constituant des modèles S&F
C  -----------------------------------------------------------
       OPEN(11,FILE=FIC_SR_SF,STATUS='OLD',ERR=992)
       OPEN(12,FILE=FIC_LR_SF,STATUS='OLD',ERR=992)
       OPEN(13,FILE=FIC_SU_SF,STATUS='OLD',ERR=992)
       OPEN(14,FILE=FIC_LU_SF,STATUS='OLD',ERR=992)
       OPEN(15,FILE=FIC_OM_SF,STATUS='OLD',ERR=992)
  
       DO I=1,SOS_NB_SF

C* Détermination des indices de réfraction par interpolation linéaire
C  ------------------------------------------------------------------

C* Initialisation avec les premières valeurs du fichier
C  ----------------------------------------------------
          READ((I+10),555,END=999,err=993)WA1,MR1(1),MI1(1),MR1(2),
     &                             MI1(2),MR1(3),MI1(3),MR1(4),MI1(4),
     &				   MR1(5),MI1(5),MR1(6),MI1(6),MR1(7),
     &				   MI1(7),MR1(8),MI1(8)

C* Pour chaque ligne du fichier, on recherche la longueur d'onde
C  juste supérieure à celle sélectionnée
C  --------------------------------------------------------------
  885     READ((I+10),555,END=999,err=993)WA2,MR2(1),MI2(1),MR2(2),
     &                             MI2(2),MR2(3),MI2(3),MR2(4),MI2(4),
     &				   MR2(5),MI2(5),MR2(6),MI2(6),MR2(7),
     &				   MI2(7),MR2(8),MI2(8)
     
          ! Cas ou WA1<=WA<=WA2, interpolation des données
	  IF((WA1.LE.WA).AND.(WA.LE.WA2)) THEN
	  
	     ! Cas ou RH = 0%, interpolation de WA mais pas de RH
	     IF(CPT.EQ.1) THEN
	        CALL SOS_INTERPOL(MR1(CPT),MR2(CPT),WA1,WA2,WA,MR(I))
		CALL SOS_INTERPOL(MI1(CPT),MI2(CPT),WA1,WA2,WA,MI(I))
	     
	     ELSE
	      ! Interpolations pour la partie réelle de l'indice de réfraction	     
		! Interpolation de WA pour RH1(CPT-1) et RH2(CPT) encadrant RH
		CALL SOS_INTERPOL(MR1(CPT-1),MR2(CPT-1),
     &		                  WA1,WA2,WA,MR_CPT1)
	        CALL SOS_INTERPOL(MR1(CPT),MR2(CPT),WA1,WA2,WA,MR_CPT2)
		! Interpolation de RH
	        CALL SOS_INTERPOL(MR_CPT1,MR_CPT2,RH1,RH2,RH,MR(I))
	      
	      ! Interpolations pour la partie imaginaire
	        CALL SOS_INTERPOL(MI1(CPT-1),MI2(CPT-1),
     &		                  WA1,WA2,WA,MI_CPT1)
	        CALL SOS_INTERPOL(MI1(CPT),MI2(CPT),WA1,WA2,WA,MI_CPT2)
	        CALL SOS_INTERPOL(MI_CPT1,MI_CPT2,RH1,RH2,RH,MI(I))
	     ENDIF
		
          ELSE
       
             WA1=WA2	! Ré-initilisation 
	     DO H=1,SOS_NB_RH
	        MR1(H)=MR2(H)
	        MI1(H)=MI2(H)
     	     ENDDO
	     GOTO 885	! Passage à la ligne suivante du fichier
	       
          ENDIF
	  
C* Conversion aux formats F5.3 et F8.3	
	  MR(I) =  DNINT( MR(I)*1000.D+00) / 1000.D+00
	  MI(I) = -DNINT(-MI(I)*100000.D+00) / 100000.D+00
	  	    
       ENDDO
       
  999  CONTINUE
  	    
       CLOSE(9)
       CLOSE(11)
       CLOSE(12)
       CLOSE(13)
       CLOSE(14)
       CLOSE(15)

C
C* Fin nominale 
C-------------------
       GOTO 9999
              
C* Cas d'erreur et retour du status -1 au programme appelant
C----------------------------------------------------------
  991  WRITE(6,*) 
     &     'Error while opening the Shettle&Fenn component  datafile'
       IER=-1
       GOTO 9999
  992  WRITE(6,*) 
     &	  'Error while opening the S&F  refractive indexes datafile'
       IER=-1
       GOTO 9999     
  993  WRITE(6,*) 
     &    'Error while reading the Shettle&Fenn component  datafile'
       IER=-1
       GOTO 9999
  994  WRITE(6,*) 
     &	  'Error while reading the S&F  refractive indexes datafile'
       IER=-1
       GOTO 9999
       
C* Format
C---------   
  222  FORMAT(5(1X,F9.5))
  333  FORMAT(1X,F5.2,5(1X,F9.5))
  555  FORMAT(17(1X,F9.5))

       
 9999  CONTINUE
 
       RETURN
       END	!FIN DE LA PROCEDURE SOS_INIT_PARAMSF


