C******************************************************************************
C* FICHIER: OSOAA_HYDROSOLS.F
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
C* ROLE: Calcul des coefficients des matrices de phase des hydrosols
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
C*   - OSOAA_HYDROSOLS
C*   - OSOAA_INIT_MATR_PHAZE
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
C*         - Compute the integration of the phase function over all directions
C*           or only over backward directions.                                    
C* MOD:VERSION:1.3: 03/11/2016:
C*     Changes to be compliant with a f77 compilation
C*         - CALL FLUSH() changed to CALL FLUSH(6)
C*         - Change a variable type to read the file FICANGLES
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
C    CTE_COEF_NRMAX : Constante pour la détermination du paramètre de taille max.
C    CTE_NOT_DEFINED_VALUE_DBLE : Identifiant de valeur double non définie
C                                 par l'utilisateur.
C    CTE_WAMIN : Longueur d'onde minimale applicable aux traitements.
C    CTE_JUNGE_SLOPE_COR : Correctif de la pente de loi de Junge dans le cas 
C                          où l'utilisateur aurait saisi 3.0.
C    CTE_HYD_MU1_TRONCA : cos de l'angle Theta1 pour l'estimation du segment de
C                         troncature de la fonction de phase des hydrosols.
C    CTE_HYD_MU2_TRONCA : cos de l'angle Theta2 pour l'estimation du segment de
C                         troncature de la fonction de phase des hydrosols.
C
C Constantes spécifiques :
C    INCTE_IDLOG_PARTICULES : ID du ficher Trace de la routine
C    INCTE_PI : Valeur de PI (calculé par la machine)
C    LENLINE  : Longueur maximale d'une ligne de données lue dans un fichier
C    INCTE_NB_MODE : Nombre de modes des composants Hydrosols
C----------------------------------------------------------------------------
#include "OSOAA.h"
#define INCTE_IDLOG_PARTICULES 99
#define INCTE_PI DACOS(-1.D+00)
#define LENLINE 1000



C==============================================================================
C PROCEDURE: OSOAA_HYDROSOLS
C ==========
C
C      Programme de génération des fichiers de propriétés radiatives des hydrosols
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
C                         radiative des hydrosols (en microns).
C
C     IMOD (I4)    (E) :  Type de caractérisation des hydrosols
C			  1 : Use of models 
C			  2 : Use of measurements
C
C     CONC (DOUBLE)     (E) : Concentration de l'hydrosol en surface
C
C     JD_SLOPE (DOUBLE) (E) : Pente de la loi de Junge (mode principal)
C                             du modèle d'hydrosol
C
C     JD_RMIN (DOUBLE)  (E) : Rayon minimal (microns) de la distribution de Junge
C
C     JD_RMAX (DOUBLE)  (E) : Rayon maximal (microns) de la distribution de Junge
C
C     JD_MR   (DOUBLE)  (E) : Partie réelle de l'indice de réfraction 
C			      de la distribution de Junge
C
C     JD_MI   (DOUBLE)  (E) : Partie imaginaire de l'indice de réfraction 
C			      de la distribution de Junge
C
C     JD_RATE (DOUBLE)  (E) : Proportion de la distribution de Junge pour le 
C                             modèle d'hydrosol
C			      ==> proportion des particules du mode de la distribution
C			          de Junge par rapport au nombre total de particules 
C			          incluant tous les modes de l'hydrosol
C					
C     LND_SM_RADIUS (DOUBLE) (E) : Rayon modal (microns) de la LND (mode secondaire)
C                                  du modèle d'hydrosol
C
C     LND_SM_VAR (DOUBLE)    (E) : Variance de la LND (mode secondaire)
C
C     LND_SM_MR (DOUBLE)     (E) : Partie réelle de l'indice de réfraction 
C				   de la LND (mode secondaire)
C
C     LND_SM_MI (DOUBLE)     (E) : Partie imaginaire de l'indice de réfraction 
C				   de la LND (mode secondaire)
C
C     LND_SM_RATE (DOUBLE)   (E) : Proportion de la distribution LND (mode secondaire)
C			           ==> proportion des particules du mode LND secondaire
C			               par rapport au nombre total de particules 
C			               incluant tous les modes de l'hydrosol
C
C     LND_TM_RADIUS (DOUBLE) (E) : Rayon modal (microns) de la LND (mode tertiaire)
C                                  du modèle d'hydrosol
C
C     LND_TM_VAR (DOUBLE)    (E) : Variance de la LND (mode tertiaire)
C
C     LND_TM_MR (DOUBLE)     (E) : Partie réelle de l'indice de réfraction 
C				   de la LND (mode tertiaire)
C
C     LND_TM_MI (DOUBLE)     (E) : Partie imaginaire de l'indice de réfraction 
C				   de la LND (mode tertiaire)
C
C     LND_TM_RATE (DOUBLE)   (E) : Proportion de la distribution LND (mode tertiaire)
C			           ==> proportion des particules du mode LND tertiaire
C			               par rapport au nombre total de particules 
C			               incluant tous les modes de l'hydrosol
C
C     FICEXTDATA (CHAR*CTE_LENFIC2)  (E) : Nom du fichier de données externes de fonctions
C                                         de phase et paramètres radiatifs (coefficients 
C                                         extinction et diffusion)
C                   			 (répertoire + nom fichier + extension)
C
C     FICGRANU (CHAR*CTE_LENFIC2)    (S) : Nom complet du fichier résultat GRANU
C                 			  (répertoire + nom fichier + extension)
C
C     FICHYD_LOG  (CHAR*CTE_LENFIC2) (S) : Nom du fichier Trace de la routine 
C
C     DIRMIE (CHAR*CTE_LENDIR)	     (E) : Répertoire de stockage des fichiers MIE
C
C     FICMIE_LOG (CHAR*CTE_LENFIC2)  (S) : Nom du fichier Trace des calculs de MIE
C                                
C     IER (I4)                       (S) : indice d'erreur : 0 si pas d'erreur, -1 sinon.
C      
C
C Résultat fourni par la procédure
C --------------------------------
C 
C    Un fichier de paramètres radiatifs (sections efficaces, etc) 
C    et de coefficients de matrice de phase des hydrosols:
C
C    Un fichier Trace optionnel.
C
C
C Description des fichiers utilisés	
C ---------------------------------
C
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
C    Fichier trace global de la routine : FICHYD_LOG
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
C   OSOAA_INIT_MATR_PHAZE
C   SOS_NOM_FICMIE
C   SOS_MIE
C   OSOAA_GRANU
C   SOS_DECOMPO_LEGENDRE
C   OSOAA_ENREG_GRANU
C
C==============================================================================

      SUBROUTINE OSOAA_HYDROSOLS(FICANGLES, 
     &                           WA, IMOD, CONC,
     &                           JD_SLOPE, JD_RMIN, JD_RMAX, 
     &                           JD_MR, JD_MI, JD_RATE,
     &			         LND_SM_RADIUS, LND_SM_VAR, 
     &			         LND_SM_MR, LND_SM_MI, LND_SM_RATE,
     &  		         LND_TM_RADIUS,LND_TM_VAR, 
     &			         LND_TM_MR, LND_TM_MI, LND_TM_RATE,
     & 		  	         FICEXTDATA,
     &			         FICGRANU, FICHYD_LOG, 
     &                           DIRMIE, FICMIE_LOG, IER)



	
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
      
      INTEGER*2 INCTE_NB_MODE   ! Nombre de modes des composants Hydrosols
      PARAMETER(INCTE_NB_MODE=3)
       
C Constantes  HYDROSOLS
C ---------------------       
      DOUBLE PRECISION ALPHAMIN	  	!Valeur minimale du paramètre de taille Alpha des 
      PARAMETER (ALPHAMIN=0.0001D+00) 	!calculs de MIE (impérativement entre 0.0001 et 10) 

      	    
C* Définition des variables                         
C*-----------------------------------------------------------------
      DOUBLE PRECISION XMU(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)  
                                !Cosinus des angles de Gauss (et utilisateur).
      DOUBLE PRECISION XHR(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)  
                                ! Poids des angles de Gauss.				
     
      DOUBLE PRECISION WA	! Longueur d'onde sélectionnée pour les calculs (en microns)

      DOUBLE PRECISION CONC	! Concentration de l'hydrosol en surface       
	!Modèle Hydrosol : Distribution principale selon le modèle de Junge
      DOUBLE PRECISION JD_SLOPE    	! (E) Pente de la distribution de Junge
      DOUBLE PRECISION JD_RMIN    	! (E) Rayon min de la distribution de Junge (mic)
      DOUBLE PRECISION JD_RMAX    	! (E) Rayon max de la distribution de Junge (mic)
      DOUBLE PRECISION JD_MR    	! (E) Partie réelle de l'indice de réfraction
      DOUBLE PRECISION JD_MI    	! (E) Partie imaginaire de l'indice de réfraction
      DOUBLE PRECISION JD_RATE    	! (E) Proportion de la distribution de Junge 
      					!     devant les autres modes
	!Modèle Hydrosol : Distribution secondaire selon le modèle LND
      DOUBLE PRECISION LND_SM_RADIUS 	! (E) Rayon modal de la distribution LND secondaire
      					!     (microns)
      DOUBLE PRECISION LND_SM_VAR    	! (E) Variance de la distribution LND secondaire
      DOUBLE PRECISION LND_SM_MR    	! (E) Partie réelle de l'indice de réfraction
      DOUBLE PRECISION LND_SM_MI    	! (E) Partie imaginaire de l'indice de réfraction
      DOUBLE PRECISION LND_SM_RATE    	! (E) Proportion de la distribution LND secondaire 
      					!    devant les autres modes
	!Modèle Hydrosol : Distribution tertiaire selon le modèle LND
      DOUBLE PRECISION LND_TM_RADIUS    ! (E) Rayon modal de la distribution LND tertiaire 
      					!     (microns)
      DOUBLE PRECISION LND_TM_VAR    	! (E) Variance de la distribution LND tertiaire
      DOUBLE PRECISION LND_TM_MR    	! (E) Partie réelle de l'indice de réfraction
      DOUBLE PRECISION LND_TM_MI    	! (E) Partie imaginaire de l'indice de réfraction
      DOUBLE PRECISION LND_TM_RATE    	! (E) Proportion de la distribution LND tertiaire 
      					!     devant les autres modes
	 
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

      DOUBLE PRECISION SOMME_NR	! nb de particules : intégration sur les rayons de NR * PR.
      				    
      DOUBLE PRECISION KMAT1	! Section efficace d'extinction (micron^2). Non tronquée.
      DOUBLE PRECISION KMAT2 	! Section efficace de diffusion (micron^2). Non tronquée. 
      DOUBLE PRECISION A_TRONCA	! Coefficient de troncature.
      DOUBLE PRECISION PIZ	! Albédo de simple diffusion.
      DOUBLE PRECISION PIZTR	! Albédo de simple diffusion après troncature.
                     
      DOUBLE PRECISION RN_MOY	! Indice de réfraction moyen (partie réelle) 
      				! représentatif d'un mélange de particules  

      DOUBLE PRECISION V_MOY	! Volume d'une particule moyenne 
      				! d'un mélange de particules (micron^3).        

      DOUBLE PRECISION Z_MOY	! Profondeur correspondant aux particules (m).
  
    
      DOUBLE PRECISION INT_0_180  ! Valeur de la fonction de phase intégrée de 0 à 180°
      DOUBLE PRECISION INT_90_180 ! Valeur de la fonction de phase intégrée de 90 à 180°


 	!   Paramètres pour différents modes 
	!-----------------------------------
      DOUBLE PRECISION C(INCTE_NB_MODE)	  ! Proportion des différents modes de particules
      DOUBLE PRECISION MR(INCTE_NB_MODE)  ! Partie réelle de l'indice de réfraction
      					  ! pour chaque mode de particules.
      DOUBLE PRECISION MI(INCTE_NB_MODE)  ! Partie imaginaire de l'indice de réfraction.
      					  ! pour chaque mode de particules.
     
      DOUBLE PRECISION VARGRANU1I(INCTE_NB_MODE)   ! Paramètre 1 de la granulométrie
      						   ! pour chaque modèle de constituant.
      DOUBLE PRECISION VARGRANU2I(INCTE_NB_MODE)   ! Paramètre 2 de la granulométrie
      						   ! pour chaque modèle de constituant.
      DOUBLE PRECISION VARGRANU3I(INCTE_NB_MODE)   ! Paramètre 3 de la granulométrie
      						   ! pour chaque modèle de constituant.      

      DOUBLE PRECISION KMAT1I	! Section efficace d'extinction (micron^2)
      				! pour un constituant I. Non tronquée.
      DOUBLE PRECISION KMAT2I 	! Section efficace de diffusion (micron^2)
      				! pour un constituant I.  Non tronquée.    
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
      DOUBLE PRECISION MU1_TRONCA ! Cosinus des angles Theta1 et Theta2 pour la définition  
      DOUBLE PRECISION MU2_TRONCA ! du segment tronquant la fonction de phase		       

						       
C---- Paramètres de type entier
      INTEGER*4 NBMU_GAUSS ! Nombre d'angles de Gauss utilisés 
      INTEGER*4 MIE_NBMU   ! Nombre d'angles effectifs des simulations
      INTEGER*4 OS_NB 	   ! Ordre des développements des fonctions de phase 
			   ! en Polynômes de Legendre.   

      INTEGER*4 J	   ! Indice de tableau 	
      INTEGER*4 I	   ! Indice de tableau
      INTEGER*4 K	   ! Indice de tableau		   

      INTEGER*4 IGRANU_MMD ! Numéro du modèle de granulométrie mono-modale
      INTEGER*4 ITRONC	   ! Option de troncature  (1 pour la réaliser).  

      INTEGER*4 LDIRMIE	   ! longueur de la chaine de caractères DIRMIE
      INTEGER*4 LFICMIE	   ! longueur de la chaine de caractères FICMIE  

      INTEGER*4 IMOD	   ! Type de caractérisation des hydrosols.

      INTEGER*4 IFIN       ! nb de caractères significatifs d'une chaine  

      INTEGER*4 NB_FICEXT_ANGLES ! Nombre d'angles du fichier de fct de phase externe

      INTEGER*4 NVAL 	! Integer pour la lecture de fichier ANGLES
            			    
C---- Code d'erreur
      INTEGER*4 IER     ! Code d'erreur =0 si pas d'erreur, =-1 sinon
      
                        
C---- Les fichiers utilisés
      CHARACTER*CTE_LENFIC2 FICANGLES   ! Chemin et nom du fichier des angles utilisés 
                                        ! pour les calculs de Mie      
      
      CHARACTER*CTE_LENFIC2 FICANGLES_USER      ! Chemin et nom du fichier 
      						! des angles "utilisateurs"
 
      CHARACTER*1000 CVAL	        ! Chaine de caractères pour lecture de valeurs

      CHARACTER*LENLINE LIGNE_TEXTE	!Ligne de caractères pour décodage de paramètres


      CHARACTER*CTE_LENDIR  DIRMIE              ! Chemin d'accès au fichier MIE
      CHARACTER*CTE_LENFIC1 FICS		! Nom du fichier MIE 
      CHARACTER*CTE_LENFIC2 FICMIE	        ! Chemin et nom du fichier MIE
      CHARACTER*CTE_LENFIC2 FICMIE_LOG          ! Nom du fichier TRACE de MIE 
 
      CHARACTER*CTE_LENFIC2 FICGRANU_PHYTO  ! Nom du fichier résultat des calculs de phase 
      					    ! pour le phytoplancton
      CHARACTER*CTE_LENFIC2 FICGRANU_MLP    ! Nom du fichier résultat des calculs de phase 
      					    ! pour les sédiments
      CHARACTER*CTE_LENFIC2 FICGRANU        ! Nom du fichier résultat d'un traitement 
      					    ! (PHYTO ou MLP)      
      CHARACTER*CTE_LENFIC2 FICEXTDATA  ! Chemin et nom du fichier de données externes 
      					! de fonctions de phase et paramètres radiatifs 
					! (coefficients extinction et diffusion)  
      CHARACTER*CTE_LENFIC2 FICHYD_LOG  ! Nom du fichier trace des calculs hydrosols

      CHARACTER*20 MODE(INCTE_NB_MODE)  ! Nom des différents modes des particules

 	
C---- Expressions logiques
      LOGICAL TRACE     ! = vrai si écriture dans le fichier trace      
      LOGICAL EX	! Vérifie l'existence d'un fichier

      
 

C*-------------
C* Traitements                        
C*-------------    
 
      
C* Initialisation                     
C*-----------------------------------------------------------------
      IER=0

      MU1_TRONCA = CTE_HYD_MU1_TRONCA	
      MU2_TRONCA = CTE_HYD_MU2_TRONCA	
      
      ITRONC=1	!Troncature des fonctions de phase imposée par défaut
                !car les particules marines sont grosses
      	
C* Contrôle arguments
C*-------------------
      IF((IMOD.LT.1).OR.(IMOD.GT.2)) GOTO 900
           	
		
C* Ouverture du fichier Trace 
C------------------------------------------------------
      IF (FICHYD_LOG.EQ.'NO_LOG_FILE') THEN
	 TRACE=.FALSE.
      ELSE
         TRACE=.TRUE.
	 OPEN (IDLOG,FILE=FICHYD_LOG,ERR=911)

         WRITE (IDLOG,*,ERR=921) ""
	 WRITE (IDLOG,*,ERR=921) ""
	 WRITE (IDLOG,*,ERR=921) "****************************"	 
	 WRITE (IDLOG,*,ERR=921) "OSOAA_HYDROSOLS :  "
	 WRITE (IDLOG,*,ERR=921) "****************************"
	 WRITE (IDLOG,*,ERR=921) ""
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
	 WRITE (IDLOG,*,ERR=921) "Max order NB ", OS_NB
	 WRITE (IDLOG,*,ERR=921) " " 
	 WRITE (IDLOG,*,ERR=921) " " 
	 WRITE (IDLOG,*,err=921) "List of used angles :"
	 WRITE (IDLOG,*,err=921) "   Index, mu, angle (degrees), weigth"
	 DO 1000 J=-MIE_NBMU,MIE_NBMU
	    IF (J.EQ.0) GOTO 1000
	    IF (J.LT.0) THEN 
	        WRITE(IDLOG,222,ERR=921) J, XMU(J),
     &	              -DACOS(-XMU(J))*180./INCTE_PI, XHR(J)
            ELSE
	        WRITE(IDLOG,222,ERR=921) J, XMU(J),
     &	               DACOS(XMU(J))*180./INCTE_PI, XHR(J)
	    ENDIF	    
1000     CONTINUE
	 WRITE (IDLOG,*,ERR=921) " " 
	 WRITE (IDLOG,*,ERR=921) " "  	
      ENDIF !Fichier Trace




C ==========================================================
C* Calculs pour une représentation des hydrosols par modèles
C ==========================================================

      IF(IMOD.EQ.1) THEN
 
         ! Initialisation des paramètres à 0
	 !----------------------------------
         CALL OSOAA_INIT_MATR_PHAZE(OS_NB,ALP,BETA11,BETA22,GAMMA12,
     &                              DELTA33,ZETA,
     &                              MIE_NBMU,P11,P12,P22,P33,TTT,
     &                              KMAT1,KMAT2,A_TRONCA,
     &                              Z_MOY, V_MOY, RN_MOY)
           	 	  
C* Calcul des paramètres de la matrice de phase 
C* et de l'albédo de simple diffusion.
C------------------------------------------------------------
         IF (CONC.EQ.0.D+00) THEN !Pas de calcul 

	    PIZTR = 0.D+00
	    	    
	    GOTO 1111	!Enregistrement du fichier résultat FICGRANU
      
         ENDIF

         !Poursuite des traitements pour CONC > 0
         !=====================================

         !  Longueur du nom du répertoire des fichiers de Mie
         !--------------------------------------------------- 
         LDIRMIE = INDEX(DIRMIE,' ')
         LDIRMIE = LDIRMIE - 1
         IF((LDIRMIE.LE.0).OR.(LDIRMIE.GT.CTE_LENDIR)) GOTO 927
         
	 WRITE(6,*) 'Hydrosols --> Mie files repertory : ', 
     &               DIRMIE(1:LDIRMIE)

         ! Contrôle de la valeur de pente de la loi de Junge
         !  ------------------------------------------------
	 IF (JD_SLOPE.EQ.3.D+00) THEN
	     JD_SLOPE = JD_SLOPE + CTE_JUNGE_SLOPE_COR
	     
	     IF (TRACE) THEN 
		 WRITE (IDLOG,*,ERR=921) " "
	         WRITE (IDLOG,*,ERR=921) "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                 WRITE (IDLOG,*,ERR=921) "WARNING : The defined value"
	         WRITE (IDLOG,*,ERR=921) "          for slope of the "
	         WRITE (IDLOG,*,ERR=921) "          Junge model is 3.0"
	         WRITE (IDLOG,*,ERR=921) "  ==> It is a singular value "
		 WRITE (IDLOG,*,ERR=921) "      [Morel, 1973]"
		 WRITE (IDLOG,*,ERR=921) 
     &		 "  ==> The value is changed to : ", JD_SLOPE
	         WRITE (IDLOG,*,ERR=921) "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
	         WRITE (IDLOG,*,ERR=921) " "
                ENDIF !Trace
	 ENDIF
	       
         ! Initialisation
         !  --------------
         ! Nom des différents modes
	 MODE(1)='JUNGE Law'
         MODE(2)='LND secondary mode'
         MODE(3)='LND tertiary mode'
	 
	 MR(1)=JD_MR
	 MR(2)=LND_SM_MR
	 MR(3)=LND_TM_MR
	 	 
	 MI(1)=JD_MI 
	 MI(2)=LND_SM_MI 
	 MI(3)=LND_TM_MI
	 	 
	 VARGRANU1I(1)=JD_RMIN 
	 VARGRANU1I(2)=LND_SM_RADIUS
	 VARGRANU1I(3)=LND_TM_RADIUS
	 
	 VARGRANU2I(1)=JD_SLOPE
	 VARGRANU2I(2)=LND_SM_VAR 
	 VARGRANU2I(3)=LND_TM_VAR 
	 
	 VARGRANU3I(1)=JD_RMAX
	 VARGRANU3I(2)=CTE_NOT_DEFINED_VALUE_DBLE
	 VARGRANU3I(3)=CTE_NOT_DEFINED_VALUE_DBLE
	 	 
	 C(1)=JD_RATE  
	 IF (LND_SM_RATE.NE.CTE_NOT_DEFINED_VALUE_DBLE) THEN       
	     C(2)=LND_SM_RATE
	 ELSE
	     C(2)=0.D+00 
	 ENDIF
	 IF (LND_TM_RATE.NE.CTE_NOT_DEFINED_VALUE_DBLE) THEN       
	     C(3)=LND_TM_RATE
	 ELSE
	     C(3)=0.D+00 
	 ENDIF
	     
         IF (TRACE) THEN
	    WRITE(IDLOG,*,err=921) ' '
	    WRITE(IDLOG,*,err=921) 'Proportion by mode :'
	    WRITE(IDLOG,*,err=921) '   JUNGE Law          : ',C(1)
	    WRITE(IDLOG,*,err=921) '   LND secondary mode : ',C(2)
	    WRITE(IDLOG,*,err=921) '   LND tertiary mode  : ',C(3)
	    WRITE(IDLOG,*,err=921) ' '
	 ENDIF	    
 				
         IF ( ((C(1)+C(2)+C(3)).LT.0.9999).
     &	       OR.((C(1)+C(2)+C(3)).GT.1.0001) ) GOTO 850
     
         !Pour chaque mode de l'hydrosol
         !------------------------------	       
	 DO 105 I=1,3
	    
	    IF(C(I).EQ.0.) GOTO 105
	    
	    ! Contrôle le signe de l'argument MI (indice imaginaire)
	    IF (MI(I).GT.0.D+00) GOTO 1010

            IF (I.EQ.1) THEN
	       IGRANU_MMD=2  !Loi de Junge 
	    ELSE
	       IGRANU_MMD=1  !LND     
	    ENDIF
	    
	    IF (TRACE) THEN
	       WRITE(IDLOG,*,err=921) MODE(I)
	       WRITE(IDLOG,*,err=921)'===================='
	       WRITE(IDLOG,*,err=921) ' '
	    ENDIF
	    		
C* Constitution du nom du fichier MIE
C  ----------------------------------
            IF (IGRANU_MMD.EQ.1) THEN	! Cas LND
	        X = VARGRANU2I(I)     
	        RMAX = VARGRANU1I(I) * DEXP(X*X) *
     &	               DEXP(X*DSQRT(-2.D+00*LOG(CTE_COEF_NRMAX)))
     
                IF (TRACE) THEN
                   WRITE(IDLOG,*,ERR=921) 
     &		   "Size distribution : log-normal"
	           WRITE(IDLOG,*,ERR=921) "  Modal radius (microns) : " , 
     &	           VARGRANU1I(I) 
	           WRITE(IDLOG,*,ERR=921) "  Standard deviation  : " , 
     &	           VARGRANU2I(I)
                   WRITE(IDLOG,*,ERR=921) 
     &             "  Calculated Rmax limit (mic) : ", RMAX
                ENDIF !Trace
	     
	    ENDIF ! Fin cas IGRANU_MMD = 1
	
	    IF (IGRANU_MMD.EQ.2) THEN	! Cas loi Junge avec Rmax défini
	        RMAX = VARGRANU3I(I)

                IF (TRACE) THEN	     
	     	    WRITE(IDLOG,*,ERR=921) 
     &		    "Size distribution : Junge's law"
	            WRITE(IDLOG,*,ERR=921)  
     &	            "  Minimal radius (microns) : ", VARGRANU1I(I)
	            WRITE(IDLOG,*,ERR=921) "  Slope : " , VARGRANU2I(I)
	            WRITE(IDLOG,*,ERR=921)  
     &	            "  Maximal radius Rmax (microns) : ", RMAX
                ENDIF !Trace
	     
	    ENDIF ! Fin cas IGRANU_MMD = 2 

            ALPHAF = REAL(100
     &	             +100*DINT(2.*INCTE_PI*RMAX / (100.*CTE_WAMIN)))

            IF (TRACE) THEN  
	        WRITE(IDLOG,*,ERR=921) "  AlphaF limit : ", ALPHAF
	        WRITE (IDLOG,*,ERR=921) " "	
            ENDIF !Trace
	 
	    IF ((ALPHAMIN.GT.ALPHAF).OR.(ALPHAF.GE.1D5)) GOTO 860 

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
     &                     FICMIE,FICMIE_LOG,IER)
     
    	      IF (IER.NE.0) GOTO 993   ! cas d'erreur
         
            ENDIF
	 
C* Calcul des sections efficaces d'extinction et de diffusion 
C  et des fonctions de phase I, Q, U du constituant I
C  ---------------------------------------------------------- 
	    CALL OSOAA_GRANU(FICMIE,IGRANU_MMD,VARGRANU1I(I),
     &                       VARGRANU2I(I),VARGRANU3I(I),
     &                       WA,MIE_NBMU,XMU,TRACE,KMAT1I,KMAT2I,
     &			     SOMME_NR,VMOYI,P11I,P12I,P33I,IER)
			
            IF (IER.NE.0) GOTO 995   ! cas d'erreur  

C* Calcul des sections efficaces d'extinction et de diffusion du mélange
C  ---------------------------------------------------------------------	     
	    KMAT1 = KMAT1 + C(I)*KMAT1I
	    KMAT2 = KMAT2 + C(I)*KMAT2I
	    
	    DO 104 J=-MIE_NBMU,MIE_NBMU	     
	       P11(J) = P11(J) + C(I)*P11I(J)*KMAT2I
	       P12(J) = P12(J) + C(I)*P12I(J)*KMAT2I
	       P33(J) = P33(J) + C(I)*P33I(J)*KMAT2I
  104	    CONTINUE  !Fin boucle sur les angles de gauss

C* Estimation de l'indice de réfraction moyen
C  et du volume moyen des particules 
C  ----------------------------------------------------- 
	 RN_MOY = RN_MOY + C(I)*MR(I)
	 V_MOY  =  V_MOY + C(I)*VMOYI
    
  105	 CONTINUE	 !Fin boucle sur les constituants de l'hydrosol
  

		
C* Normalisation des fonctions de phase du mélange
C  -----------------------------------------------
	 DO 120 J=-MIE_NBMU,MIE_NBMU   
	    P11(J) = P11(J) / KMAT2
	    P12(J) = P12(J) / KMAT2
	    P33(J) = P33(J) / KMAT2
  120	 CONTINUE


		
C* Intégration de la fonction de phase (non tronquée)
C* sur toutes les directions ou en rétro-diffusion uniquement 
C  ----------------------------------------------------------
      INT_0_180 = 0.
      INT_90_180 = 0.
      DO J=-MIE_NBMU,MIE_NBMU
         INT_0_180 = INT_0_180 + P11(J)*XHR(J)
      ENDDO

      DO J=-MIE_NBMU,-1
         INT_90_180 = INT_90_180 + P11(J)*XHR(J)
      ENDDO


      IF (TRACE) THEN
         WRITE (IDLOG,*,ERR=921) ' '
         WRITE (IDLOG,*,ERR=921)
     &	    'Phase function integration (no truncation)'
	 WRITE (IDLOG,*,ERR=921)
     &	 '  Phase function integration from 0 to 180 deg:',INT_0_180
         WRITE (IDLOG,*,ERR=921)
     &	 '  Phase function integration from 90 to 180 deg:',INT_90_180
	 WRITE (IDLOG,*,err=921) ' '
      ENDIF


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
	 
      ENDIF	!Fin test sur Modalite IMOD=1 (Modèle)



C ==========================================================
C* Calculs pour une représentation des hydrosols par mesures
C ==========================================================

      IF(IMOD.EQ.2) THEN    

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
	    WRITE (IDLOG,*,ERR=921) "Scattering coef  : ", KMAT2
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
	    WRITE (IDLOG,*,ERR=921) " Functions F11, F12, F22 and F33 "
	    WRITE (IDLOG,*,ERR=921) " for the simulation angles "
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
      ENDIF	!Fin test sur Modalite IMOD=2 (Mesures)
      
      




      
      
C ======================================
C* Calculs communs pour tous les cas
C ======================================
	 
C* Calcul de l'albédo de simple diffusion
C-------------------------------------------
      PIZ=KMAT2/KMAT1

C* Troncature de l'albédo de simple diffusion
C--------------------------------------------
      PIZTR = PIZ*(1.-A_TRONCA/2.)/(1.-PIZ*A_TRONCA/2.)      
    
C* Ecriture du fichier trace
C-------------------------------------------
      IF (TRACE) THEN
         WRITE(IDLOG,*,err=921) ' '
	 WRITE(IDLOG,*,err=921) ' '
	 WRITE(IDLOG,*,err=921) '==================================='
         WRITE(IDLOG,*,err=921) '* HYDROSOL COMPUTATIONS: RESULTS  *'
	 WRITE(IDLOG,*,err=921) '==================================='
         WRITE(IDLOG,*,err=921) ' '
	 WRITE(IDLOG,*,err=921) ' '
	
	 IF(ITRONC.EQ.1) THEN
	      WRITE(IDLOG,*,err=921) 
     &	      '  Phase function truncation applied '
	 ELSE
	      WRITE(IDLOG,*,err=921) '  No phase function truncation'	   
	 ENDIF
	 
         WRITE(IDLOG,*,err=921) ' '
            	 
	 IF((A_TRONCA.NE.0.D+00).OR.(IMOD.NE.0)) THEN
	    WRITE(IDLOG,*,err=921) '  Truncation coefficient : ',
     &	    A_TRONCA
	    WRITE(IDLOG,*,err=921) 
     &	    '  Extinction cross-section (mic^2) - no trunca. applied :',
     &          KMAT1
	    WRITE(IDLOG,*,err=921) 
     &	    '  Scattering cross-section (mic^2) - no trunca. applied :', 
     &          KMAT2
	    WRITE(IDLOG,*,err=921) 
     &	     '  Mean refractive index (real part) : ',RN_MOY
	    WRITE(IDLOG,*,err=921) 
     &	     '  Mean volume of a particle (micron^3) : ',V_MOY
	    IF (A_TRONCA.NE.0.D+00) THEN
	       WRITE(IDLOG,*,err=921) 
     &	       '  Single scattering albedo - adjusted to the trunca. :',
     &         PIZTR 
	       WRITE(IDLOG,*,err=921) 
     &		 '  Single scattering albedo - no trunca. applied :',
     &             PIZTR / (1.-A_TRONCA/2.+PIZTR*A_TRONCA/2.)      
	       WRITE(IDLOG,*,err=921)
     &	       '  Beta11(0) non-normalized - adjusted to the trunca. :',
     &	       Z1  
               WRITE(IDLOG,*,err=921) 
     &	         '  Asymmetry factor - adjusted to the truncation: ',
     &           BETA11(1)/3.
               WRITE(IDLOG,*,err=921) 
     &	        '  Asymmetry factor - no trunca. applied: ',
     &          A_TRONCA/2.+(1.-A_TRONCA/2.)*BETA11(1)/3.     
	    ELSE
	       WRITE(IDLOG,*,err=921) 
     & 		 '  Single scattering albedo - no trunca. applied : ',
     &           PIZTR
	       WRITE(IDLOG,*,err=921) 
     & 		 '  Beta11(0) non-normalized - no trunca. applied : ',
     &           Z1
               WRITE(IDLOG,*,err=921) 
     &	         '  Asymmetry factor - no trunca. applied: ',
     &           BETA11(1)/3.
     	    ENDIF !Fin test si troncature   
            WRITE(IDLOG,*,err=921) ' '
	    WRITE(IDLOG,*,err=921) '  1st column : Scattering angle'
	    WRITE(IDLOG,*,err=921) 
     &	      '  2nd column : Truncated phase function P11' 	 
	    WRITE(IDLOG,*,err=921)
     &        '  3rd column : Original phase function P11'
	    WRITE(IDLOG,*,err=921) 
     &        '   4th column : Truncated polarized phase function '
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
         END IF
    
         WRITE(IDLOG,*,err=921) ' '  
	 WRITE(IDLOG,35,err=921)'I','ALP(I)','BETA11(I)','GAMMA12(I)',
     &                       'DELTA33(I)','ZETA(I)'  
         DO 45 K=0,OS_NB
	    WRITE(IDLOG,25,err=921) K,ALP(K),BETA11(K),GAMMA12(K),
     &	                            DELTA33(K),ZETA(K)
   45    CONTINUE
   	 WRITE(IDLOG,*,err=921) ' '
	 	   	   	   	   	   
      ENDIF   !Trace   
      


C* Enregistrement des paramètres de granulométrie dans le fichier Résultat
C  -----------------------------------------------------------------------
1111  CALL OSOAA_ENREG_GRANU(KMAT1, KMAT2, A_TRONCA, PIZTR,
     &                       CTE_NOT_DEFINED_VALUE_DBLE, V_MOY, RN_MOY,
     &                       OS_NB, ALP, BETA11, GAMMA12, ZETA,
     &                       FICGRANU, IER) 
     
      IF (IER.NE.0) GOTO 998   ! cas d'erreur
	   
	   
	    
C* Fermeture fichiers
C-------------------
 
C    NB : On ne ferme pas le fichier TRACE pour permettre 
C	  de le compléter pour un appel ultérieur de la routine.


      
C* Fin nominale 
C-------------------
      GOTO 9999
            
C* Cas d'erreur et retour du status -1 au programme appelant  
C----------------------------------------------

 850  WRITE(6,*) '  OSOAA_HYDROSOLS : ERROR_850' 
      WRITE(6,*) '    Inconsistante values for mode concentrations'
      WRITE(6,*) '    for hydrosol'
      WRITE(6,*) '  	  Junge law mode     : ', C(1)
      WRITE(6,*) '  	+ LND secondary mode : ', C(2)
      WRITE(6,*) '  	+ LND tertiary mode  : ', C(3)
      WRITE(6,*) '  	                     = ', C(1)+C(2)+C(3) 
      WRITE(6,*) '  	--> Different of 1'          
      GOTO 9998

 860  WRITE(6,*) '  OSOAA_HYDROSOLS : ERROR_860 '
      WRITE(6,*) '      on max value of parameter size ALPHAF '
      GOTO 9998 
            
 900  WRITE(6,*) '  OSOAA_HYDROSOLS : ERROR_900' 
      WRITE(6,*) '  ERROR on -HYD.Model value  : '
      WRITE(6,*) '  --> out of available range  '
      WRITE(6,*) '      (1 : Use of models or 2 : Use of measurements)'
      GOTO 9998
            
 911  WRITE(6,*) '  OSOAA_HYDROSOLS : ERROR_911 on logfile opening' 
      GOTO 9998
      
 921  WRITE(6,*) '  OSOAA_HYDROSOLS : ERROR_921 on logfile writing' 
      GOTO 9998
 
 927  WRITE(6,*) '  OSOAA_HYDROSOLS : ERROR_927: '
      WRITE(6,*) '  => Error on DIRMIE : maybe too long'
      WRITE(6,*) '     (Name of the storage directory for MIE files)'
      WRITE(6,*) '  => Check if CTE_LENDIR has to be adjusted.' 
      GOTO 9998
                  
 930  WRITE(6,*) '  OSOAA_HYDROSOLS : ERROR_930: '
      WRITE(6,*) '  => Error on angles file opening'
      GOTO 9998  

 931  WRITE(6,*) '  OSOAA_HYDROSOLS : ERROR_930: '
      WRITE(6,*) '  => Error on angles file reading'
      GOTO 9998  


  940 WRITE(6,*) '  OSOAA_HYDROSOLS : ERROR_940:'
      WRITE(6,*) '  on external phase functions file opening'
      GOTO 9998
      
  950 WRITE(6,*) '  OSOAA_HYDROSOLS : ERROR_950:  '
      WRITE(6,*) '  The number of angles for the external phase'
      WRITE(6,*) '  functions description is higher than the defined '
      WRITE(6,*) '  size of internal tables'
      WRITE(6,*) '  --> Check the value of CTE_MAXNB_ANG_EXT '
      WRITE(6,*) '      in OSOAA.h compared to the value associated'
      WRITE(6,*) '      to NbAng in the external phase functions file'
      GOTO 9998

  941 WRITE(6,*) '  OSOAA_HYDROSOLS : ERROR_941:'
      WRITE(6,*) '  on external phase functions file reading'
      GOTO 9998
            
  942 WRITE(6,*) '  OSOAA_HYDROSOLS : ERROR_942:'
      WRITE(6,*) '  on external phaze functions file reading : '
      WRITE(6,*) '  End of file encountered'
      WRITE(6,*) '  --> Check if the value associated to NbAng is '
      WRITE(6,*) '      correct, in the external phase functions file'
      GOTO 9998 
          
  945 WRITE(6,*) '  OSOAA_HYDROSOLS : ERROR_945:'
      WRITE(6,*) '  while interpolating external phaze functions.'
      WRITE(6,*) '  --> Check that the abscissas of the phaze '
      WRITE(6,*) '      functions are correct : '
      WRITE(6,*) '  	   do not use twice same values'
      GOTO 9998            


  993 WRITE(6,*) '  OSOAA_HYDROSOLS : ERROR_993 on subroutine SOS_MIE'
      GOTO 9998

      
  995 WRITE(6,*) '  OSOAA_HYDROSOLS : ERROR_995 on sub. OSOAA_GRANU'
      GOTO 9998 
      
  996 WRITE(6,*) '  OSOAA_HYDROSOLS : ERROR_996 '
      WRITE(6,*) '    on subroutine SOS_DECOMPO_LEGENDRE'
      GOTO 9998
 
        
  998 WRITE(6,*) '  OSOAA_HYDROSOLS : ERROR_998 '
      WRITE(6,*) '  		      on subroutine OSOAA_ENREG_GRANU'
      GOTO 9998 
      
       
 1010 WRITE(6,*) '  OSOAA_HYDROSOLS : ERROR_1010 '
      WRITE(6,*) '   --> imaginary part of refractive'
      WRITE(6,*) '       indexes have to be negative'
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
  
      END      !FIN DE LA PROCEDURE OSOAA_HYDROSOLS
      
      
      




C*==============================================================================
C* PROCEDURE: OSOAA_INIT_MATR_PHAZE
C* ==========
C
C      Cette procédure initialise les paramètres d'un fichier de matrice de phase 
C      sur les valeurs à appliquer si la quantite de particules correspondantes
C      est nulle.
C
C
C Description des paramètres
C ----------------------------
C
C     OS_NB   (I4) (E)	    : Ordre effectif des développements des fonctions de phase 
C			      en Polynômes de Legendre.  
C     ALP(0:CTE_OS_NB_MAX)     (double) (S) : Paramètres alpha(k).
C     BETA11(0:CTE_OS_NB_MAX)  (double) (S) : Paramètres beta11(k).
C     BETA22(0:CTE_OS_NB_MAX)  (double) (S) : Paramètres beta22(k).
C     GAMMA12(0:CTE_OS_NB_MAX) (double) (S) : Paramètres gamma12(k).
C     DELTA33(0:CTE_OS_NB_MAX) (double) (S) : Paramètres delta33(k).
C     ZETA(0:CTE_OS_NB_MAX)    (double) (S) : Paramètres zeta(k).
C
C     MIE_NBMU (I4) (E) : Nombre d'angles (positifs) effectivement utiles
C     P11(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX) (double) (S) : 
C					Fonction de phase P11 calculée. 
C     P12(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX) (double) (S) : 
C					Elément P12 de la matrice de phase.
C     P33(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX) (double) (S) : 
C					Elément P33 de la matrice de phase.
C     TTT(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX) (double) (S) : 
C					Copie de la fonction de phase (P11) avant troncature
C
C     KMAT1  (DOUBLE)   (S) : Section efficace d'extinction (mic^2) pour WA de simulation. 
C			      Non tronquée.
C     KMAT2  (DOUBLE)   (S) : Section efficace de diffusion (mic^2) pour WA de simulation. 
C			      Non tronquée.
C
C     A_TRONCA (DOUBLE) (S) : Coefficient de troncature de la fonction de phase
C
C     Z_MOY    (DOUBLE) (E) : Elévation ou profondeur (m) des particules dont les propriétés
C                             sont décrites dans le fichier résultat
C
C     V_MOY    (DOUBLE) (E) : Volume d'une particule moyenne 
C			      du mélange de particules (micron^3).  
C				      
C     RN_MOY   (DOUBLE) (E) : Indice de réfraction moyen (partie réelle) 
C      			      représentatif du mélange de particules.  
C
C
C Constantes utilisées :  
C----------------------
C    CTE_OS_NB_MAX :  Valeur maximale pour l'ordre limite du développement en 
C                     polynômes de Legendre.  
C    CTE_MIE_NBMU_MAX : Nombre maximal d'angles positifs.
C    CTE_NOT_DEFINED_VALUE_DBLE : Identifiant de valeur double non définie.
C
C Common utilisé:
C --------------
C  Aucun
C
C Cas d'erreur :
C ------------
C  Aucun   
C
C  Sous programmes utilisés:
C --------------------------
C  Aucun 
C
C==============================================================================
      SUBROUTINE OSOAA_INIT_MATR_PHAZE(OS_NB,ALP,BETA11,BETA22,GAMMA12,
     &                                 DELTA33,ZETA,
     &                                 MIE_NBMU,P11,P12,P22,P33,TTT,
     &                                 KMAT1,KMAT2,A_TRONCA,
     &                                 Z_MOY, V_MOY, RN_MOY)

      IMPLICIT NONE  
      
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
                      
      DOUBLE PRECISION ALP(0:CTE_OS_NB_MAX)	! Paramètres alpha(k).
      DOUBLE PRECISION BETA11(0:CTE_OS_NB_MAX)	! Paramètres beta11(k).
      DOUBLE PRECISION BETA22(0:CTE_OS_NB_MAX)	! Paramètres beta22(k).
      DOUBLE PRECISION GAMMA12(0:CTE_OS_NB_MAX)	! Paramètres gamma12(k).
      DOUBLE PRECISION ZETA(0:CTE_OS_NB_MAX)	! Paramètres zeta(k).
      DOUBLE PRECISION DELTA33(0:CTE_OS_NB_MAX)	! Paramètres delta33(k).
      				    
      DOUBLE PRECISION KMAT1	! Section efficace d'extinction (micron^2). Non tronquée.
      DOUBLE PRECISION KMAT2 	! Section efficace de diffusion (micron^2). Non tronquée.
      DOUBLE PRECISION A_TRONCA	! Coefficient de troncature.

                     
      DOUBLE PRECISION RN_MOY	! Indice de réfraction moyen (partie réelle) 
      				! représentatif d'un mélange de particules  

      DOUBLE PRECISION V_MOY	! Volume d'une particule moyenne 
      				! d'un mélange de particules (micron^3).        

      DOUBLE PRECISION Z_MOY	! Elévation ou profondeur correspondant aux particules (m).

      
      
C---- Paramètres de type entier  
      INTEGER*4 MIE_NBMU   ! Nombre d'angles effectifs des simulations
      INTEGER*4 OS_NB 	   ! Ordre des développements des fonctions de phase 
			   ! en Polynômes de Legendre.   

      INTEGER*4 J	   ! Indice de tableau 
      INTEGER*4 K	   ! Indice de tableau		
         
    
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

C* Initialisation du coefficient de troncature
C--------------------------------------------------------------------
      A_TRONCA=0.
      
C* Initialisation de l'indice de réfraction moyen du modèle
C--------------------------------------------------------------------
      RN_MOY=0. 

C* Initialisation de la profondeur des particules 
C--------------------------------------------------------------------
      Z_MOY=CTE_NOT_DEFINED_VALUE_DBLE 
            
C* Initialisation du volume moyen des particules du modèle
C--------------------------------------------------------------------
      V_MOY=0.        


      END    !FIN DE LA PROCEDURE OSOAA_INIT_MATR_PHAZE 

