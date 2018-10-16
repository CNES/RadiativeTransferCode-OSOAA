C******************************************************************************
C* FICHIER: OSOAA_SOS.F
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
C* ROLE: Routines pour la préparation d'une simulation de transfert radiatif à partir 
C*       des données de propriétés radiatives des particules (aérosols et hydrosols),  
C*       des profils d'épaisseurs optiques (atmosphère et mer) et des matrices d'interface,
C*       ainsi que pour la génération des fichiers résultats.
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
C*
C*
C* ROUTINES COUVERTES PAR LE FICHIER : 
C*   - OSOAA_SOS
C*   - OSOAA_POLAR
C*
C*
C* MOD:VERSION:1.0: 02/04/2015: Initial version of the OSOAA code
C* MOD:VERSION:1.1: 15/06/2015: 
C*         - All comments for log files are now in english
C*         - Changing the way of reading the formatted files (Angles, ...)
C*           to be compatible with the compiler gfortran.
C* MOD:VERSION:1.2: 25/03/2016: 
C*         - APPEL_SOS_CORE is now defined as argument of the routine.
C*         - Change of reading the string FICANGLES_USER in file FICANGLES
C*           (for gfortran compilation - linux system)  
C*         - New output file FIC_FLUX: Profile of downwelling and upwelling 
C*                                     flux from TOA to the sea bottom 
C*                                     normalised to the solar irradiance at TOA  
C*         - OSOAA_SOS_CORE has new arguments : PROFIL_Z and FIC_FLUX
C*         - Change format 800 : F7.2 becomes F8.2 for TPOL in order to allow
C*           the value -999.00 in case of the polarization rate is not defined
C*           (if L = 0).                                     
C* MOD:VERSION:1.3: 03/11/2016:
C*         - Change a variable type to read the file FICANGLES
C*           to be compliant with a f77 compilation
C* MOD:VERSION:1.4: 04/06/2018:
C*         - Routine OSOAA_POLAR is changed in version VERSION:1.1    
C*         - The unit for I, Q and U is added in output files (steradian-1)
C*   
C******************************************************************************

C----------------------------------------------------------------------------
C Définition des constantes  
C---------------------------------------------------------------------------- 
C Constantes utilisées : 
C
C    CTE_LENDIR : Longueur des noms de répertoires. 
C    CTE_LENFIC1 : Longueur des noms de fichiers sans arborescence.
C    CTE_LENFIC2 : Longueur des noms de fichiers avec arborescence.
C    CTE_OS_NBMU_MAX  : Nombre maximal d'angles de Gauss positifs 
C                       pour les calculs de luminance
C    CTE_OS_NB_MAX :  Valeur maximale pour l'ordre limite du développement en 
C                     polynômes de Legendre.
C    CTE_NT_ATM : nombre de couches du profil atmosphérique
C    CTE_NT_SEA : nombre de couches du profil océanique
C
C
C Constantes spécifiques :     
C    INCTE_IDLOG_SOS : ID du ficher Trace de la routine
C    INCTE_PI : Valeur de PI (calculé par la machine)
C    LENLINE  : Longueur maximale d'une ligne de données lue dans un fichier
C    VALEUR_INDEFINIE : Valeur par défaut pour les grandeurs indéfinies
C                       (angle de polarisation si polarisation nulle,
C                        taux de polarisation si intensité nulle).
C----------------------------------------------------------------------------
#include "OSOAA.h"
#define INCTE_IDLOG_SOS 99
#define INCTE_PI DACOS(-1.D+00)
#define LENLINE 1000
#define VALEUR_INDEFINIE -999.





C==============================================================================
C PROCEDURE: OSOAA_SOS
C ==========
C
C     Programme permettant 
C     - la lecture :
C         - des données de propriétés radiatives des particules (aérosols et hydrosols)
C         - des profils d'épaisseurs optiques atmosphériques et marines
C           (avec ajustement à la troncature des fonctions de phase)
C         - des matrices d'interface.
C     - l'appel du coeur de calcul de transfert radiatif par ordres successifs de diffusion.
C     - la génération des fichiers résultats.
C
C
C Description des paramètres 			
C ------------------------------
C
C     FICANGLES (CHAR*CTE_LENFIC2)     (E) : Nom complet du fichier des angles utilisés 
C                                            pour les calculs de luminance et BRDF/BPDF
C                 			     (répertoire + nom fichier + extension)
C
C     FICGRANU_AER (CHAR*CTE_LENFIC2)  (E) : Nom complet du fichier GRANU des aérosols
C                 			     (répertoire + nom fichier + extension)
C
C     FICPROFIL_ATM (CHAR*CTE_LENFIC2) (E) : Nom du fichier contenant le profil 
C                                            atmosphérique (chemin complet)
C
C     FICGRANU_CHL (CHAR*CTE_LENFIC2)  (E) : Nom complet du fichier GRANU du phytoplancton
C                 			     (répertoire + nom fichier + extension)
C
C     FICGRANU_MLP (CHAR*CTE_LENFIC2)  (E) : Nom complet du fichier GRANU des sédiments
C                 			     (répertoire + nom fichier + extension)
C
C     FICPROFIL_SEA (CHAR*CTE_LENFIC2) (E) : Nom du fichier contenant le profil maritime 
C                                            (chemin complet)
C
C     IMAT_SURF  (I4)   	       (E) : Indice précisant s'il y a usage des fichiers 
C                                            SURFACE  d'interface air / mer agitée 
C					     (1 si c'est le cas, 0 sinon).
C
C     DIRS (CHAR*CTE_LENDIR)           (E) : Nom du répertoire de stockage des fichiers 
C                                            SURFACE (chemin complet).    	
C
C     SEA_IND	(double)    	       (E) : Indice de réfraction eau / air à la longueur 
C					     d'onde de simulation des luminances.
C
C     WIND	(double)    	       (E) : Vitesse du vent (m/s).
C
C     RHO_SURF  (double)    	       (E) : Albédo de surface à la longueur d'onde
C					     de simulation des luminances.
C
C     RHO_BOT  (double)    	       (E) : Albédo de fond de mer à la longueur d'onde
C					     de simulation des luminances.
C
C     IGMAX  (I4)   		       (E) : Ordre maximal d'interaction.
C     
C     FICSOS_RES_BIN (CHAR*CTE_LENFIC2)
C                                      (S) : Nom du fichier résultat binaire des calculs SOS
C
C     PHI  (double)		       (E) : Azimut relatif (degrés) entre la direction de 
C				             visée et celle du Soleil.
C
C     FICSOS_RES_ADV_UP  (CHAR*CTE_LENFIC2)
C				       (S) : Nom du fichier résultat donnant la luminance 
C	     				     montante en fonction de la profondeur 
C	     				     (ou de l'altitude) pour Phi et VZA fixés.
C                                            (chemin complet)
C
C     FICSOS_RES_ADV_DOWN  (CHAR*CTE_LENFIC2) 
C				       (S) : Nom du fichier résultat donnant la luminance 
C	     				     descendante en fonction de la profondeur 
C	     				     (ou de l'altitude) pour Phi et VZA fixés.
C                                            (chemin complet)
C
C     LEVEL_OUT  (I4)   	       (E) : Indicateur du choix du niveau de sortie 
C                                            des luminances (pour le fichier de résultats en
C                                            fonction de l'angle de visée FICSOS_RES_VS_VZA)
C      						1 : Top of Atmosphere
C						2 : Sea Bottom
C						3 : Sea Surface 0+
C						4 : Sea Surface 0-
C						5 : User's definition of altitude or depth 
C
C     USER_Z_OUT  (double)    (E) : Altitude ou profondeur pour la sortie, selon
C                                   la définition de l'utilisateur
C
C     FICSOS_RES_VS_VZA (CHAR*CTE_LENFIC2) 
C				       (S) : Nom du fichier de résultats en fonction de 
C					     l'angle de visée (pour Phi fixé).
C
C     VZA_OUT  (double)    	       (E) : Valeur de l'angle de propagapation de la  
C                                            luminance pour une sortie en fonction du niveau
C                                            (pour le fichier de résultats en fonction de 
C                                             l'angle de visée FICSOS_RES_VS_Z)
C
C     FICSOS_RES_VS_Z (CHAR*CTE_LENFIC2) 
C				       (S) : Nom du fichier résultat de luminance
C                                            en fonction de la profondeur, 
C                                            pour un angle zénithal de visée 
C				             et pour l'azimut Phi fixé.
C                                            (chemin complet)     
C
C     FIC_FLUX (CHAR*CTE_LENFIC2)    
C                     		       (S) : Nom du fichier de profil du flux descendant et montant,
C                                            normalisé par l'éclairement solaire au TOA (chemin complet).
C
C     FICSOS_LOG (CHAR*CTE_LENFIC2)    (S) : Nom du fichier de log (chemin complet)
C
C
C     APPEL_SOS_CORE (BOOLEAN)         (E) : Indicateur si cas de reprise 
C					     (saut des calculs en SF) ou non.
C
C     IER (I4)                         (S) : indice d'erreur : 0 si pas d'erreur, -1 sinon.
C   
C
C
C Résultats fournis par la procédure	
C ----------------------------------
C 
C  - FICSOS_RES_VS_VZA : Sortie standard
C      Fichier du champ de luminance montante en fonction de l'angle zénithal de visée 
C      pour une valeur fixée de l'azimut relatif et du niveau du profil.
C
C  - FICSOS_RES_VS_Z : Sortie standard
C      Fichier du champ de luminance marine en fonction de la profondeur 
C      pour une valeur fixée de l'azimut relatif et pour une direction de propagation fixée
C      (montante ou descendante).   
C
C  - FICSOS_RES_BIN : Sortie avancée dédié aux utilisateurs expert
C      Fichier binaire du champ de luminance simulé (I,Q,U) pour chaque niveau du profil 
C      atmosphérique et marin, par ordre de la décomposition en série de Fourier.
C               .
C  - FICSOS_RES_ADV_UP : Sortie avancée dédié aux utilisateurs expert
C      Fichier du champ de luminance montante en fonction du niveau (profondeur ou altitude)
C      et de l'angle zénithal de visée, pour une valeur fixée de l'azimut relatif.
C               .
C  - FICSOS_RES_ADV_DOWN : Sortie avancée dédié aux utilisateurs expert
C      Fichier du champ de luminance descendante en fonction du niveau (profondeur ou 
C      altitude) et de l'angle zénithal de visée, pour une valeur fixée de l'azimut relatif.
C 
C  - FIC_FLUX : Sortie imposée (ce fichier est nécessaire car OSOAA_SOS_CORE n'est pas 
C                               exécutée si les paramètres de simulation du champ de 
C                               luminance sont les mêmes que ceux du traitement 
C                               précédent sous le même espace de travail)
C      Fichier Ascii du profil de flux montant descendant du TOA au fond marin
C      normalisé par l'éclairement solaire au TOA.
C   
C  - Fichier Trace optionnel.
C
C
C Description des fichiers utilisés	 
C ---------------------------------
C
C    Fichier FICANGLES:
C      ==>   contient les informations suivantes sur les angles 
C            de propagation du rayonnement
C		NB_TOTAL_ANGLES :    nb d'angles total à utiliser.
C               NB_GAUSS_ANGLES :    nb d'angles de Gauss.
C               ANGLES_USERFILE :    Nom du fichier des angles utilisateur.
C	        SOLAR ZENITH ANGLE : Angle zénithal solaire (degrés) dans l'air.
C		INTERNAL_IMUS :      Numéro d'indice de table correspondant.
C	        TRANSMITTED SOLAR ZENITH ANGLE IN WATER : Angle zénithal solaire (degrés)
C                                                         transmis dans la mer (plate).
C		INTERNAL_IMUSW :     Numéro d'indice de table correspondant.
C               INTERNAL_OS_NB :     Ordre des développements des fonctions de phase 
C				     en Polynômes de Legendre à utiliser.
C                                    Ordre maximal à utiliser pour le développement en
C                                    séries de Fourier des calculs de luminance et
C				     des matrices d'interface.
C               INTERNAL_OS_NS :     Ordre maximal à utiliser pour le développement en
C                                    polynômes de Legendre des éléments de la matrice de 
C                                    Fresnel.
C               INTERNAL_OS_NM :     Ordre maximal du développement en séries de Fourier.  
C                                    de la fonction G des calculs de matrice d'interface.
C               INDEX COS_ANGLE WEIGHT OUTPUT: lignes donnant le numéro d'angle, son cosinus
C                                              et son poids, s'il est donné en sortie 
C                                              "utilisateur", au format I4,1X,2D21.14,1X,I4
C 
C
C  
C    Fichiers FICGRANU_AER, FICGRANU_CHL et FICGRANU_MLP :
C      ==>   contiennent les informations suivantes sur les propriétés radiatives 
C            des aérosols (FICGRANU_AER), du phytoplancton (FICGRANU_CHL)
C            et des sédiments (FICGRANU_MLP).
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
C    Fichier FICPROFIL_ATM: 
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
C
C    Fichier FICPROFIL_SEA: 
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
C
C    Fichier résultat FICSOS_RES_VS_VZA: 
C      ==>   contient le champ de luminance montante en fonction de l'angle zénithal 
C            de visée, pour une valeur fixée de l'azimut relatif et du niveau du profil.
C
C		format : 28 lignes d'entête
C                        puis 1 ligne par direction de visée (7 colonnes)
C		chaque ligne contient:
C			- Colonne 1 : l'angle zénithal de visée (degrés)
C			- Colonne 2 : l'angle de diffusion (degrés)
C			- Colonne 3 : le paramètre de Stokes I pour le niveau Z requis
C                                     normalisé par l'éclairement solaire au TOA 
C                                     (PI * L(z) / Esun)
C			- Colonne 4 : la réflectance au niveau Z (PI * L(z) / Ed(z))
C			- Colonne 5 : le taux de polarisation (%)
C			- Colonne 6 : l'intensité polarisée pour le niveau Z requis
C                                     normalisé par l'éclairement solaire au TOA 
C                                     (PI * Lpol(z) / Esun)
C			- Colonne 7 : la réflectance polarisée au niveau Z 
C				      (PI * Lpol(z) / Ed(z))
C
C
C    Fichier résultat FICSOS_RES_VS_Z: 
C      ==>   contient le champ de luminance marine en fonction de la profondeur 
C            pour une valeur fixée de l'azimut relatif et pour une direction de 
C            propagation fixée (montante ou descendante).   
C
C		format : 26 lignes d'entête
C                        puis 1 ligne par niveau du profil marin (7 colonnes)
C		chaque ligne contient:
C			- Colonne 1 : la profondeur du profil marin (m)
C			- Colonne 2 : l'angle de diffusion (degrés)
C			- Colonne 3 : le paramètre de Stokes I pour le niveau Z requis
C                                     normalisé par l'éclairement solaire au TOA 
C                                     (PI * L(z) / Esun)
C			- Colonne 4 : la réflectance au niveau Z (PI * L(z) / Ed(z))
C			- Colonne 5 : le taux de polarisation (%)
C			- Colonne 6 : l'intensité polarisée pour le niveau Z requis
C                                     normalisé par l'éclairement solaire au TOA 
C                                     (PI * Lpol(z) / Esun)
C			- Colonne 7 : la réflectance polarisée au niveau Z 
C				      (PI * Lpol(z) / Ed(z))
C
C
C    Fichiers résultats FICSOS_RES_ADV_UP et FICSOS_RES_ADV_DOWN: 
C      ==>   contiennent les champs de luminance montante ou descendante 
C            en fonction du niveau (profondeur ou altitude)
C            et de l'angle zénithal de visée, pour une valeur fixée de l'azimut relatif.
C
C		format : 31 lignes d'entête
C                        puis 1 ligne par niveau du profil marin (10 colonnes)
C		chaque ligne contient:
C			- Colonne 1 : la numéro du niveau dans le profil global
C                                     (0 pour le TOA).
C			- Colonne 2 : le niveau dans le profil global (m)
C				      (altitude > 0 ou profondeur < 0)
C			- Colonne 3 : l'angle zénithal de propagation du champ (degrés)
C			- Colonne 4 : l'angle de diffusion (degrés)
C			- Colonne 5 : le paramètre de Stokes I pour le niveau Z requis
C                                     normalisé par l'éclairement solaire au TOA 
C                                     (PI * L(z) / Esun)
C			- Colonne 6 : idem pour le paramètre de Stokes Q 
C			- Colonne 7 : idem pour le paramètre de Stokes U 
C			- Colonne 8 : l'angle de polarisation (degrés)
C			- Colonne 9 : le taux de polarisation (%)
C			- Colonne 10: l'intensité polarisée pour le niveau Z requis
C                                     normalisé par l'éclairement solaire au TOA 
C                                     (PI * Lpol(z) / Esun)
C                .
C    
C    Fichier Trace optionnel.
C      ==>   contient des informations sur les étapes de traitements
C
C
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
C       et des fichiers E/S :  fichier des angles, fichiers des paramètres aérosols
C                              et hydrosols, fichiers de profil et fichiers résultats
C     - Paramètres non valables
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant
C
C
C  Sous programmes utilisés: 
C --------------------------
C  Ce programme fait appel aux routines:
C      - OSOAA_NOM_FIC_INTERFACE
C      - OSOAA_SOS_CORE
C      - OSOAA_TRPHI
C      - OSOAA_POLAR
C
C==============================================================================

      SUBROUTINE OSOAA_SOS(FICANGLES, FICGRANU_AER, FICPROFIL_ATM,
     &                     FICGRANU_CHL, FICGRANU_MLP, FICPROFIL_SEA, 
     &			   IMAT_SURF, DIRS, SEA_IND, WIND, 
     & 			   RHO_SURF, RHO_BOT, IGMAX,
     &			   FICSOS_RES_BIN,
     &                     PHI, FICSOS_RES_ADV_UP,FICSOS_RES_ADV_DOWN,
     &    	           LEVEL_OUT,USER_Z_OUT, FICSOS_RES_VS_VZA,
     &		           VZA_OUT, FICSOS_RES_VS_Z,FIC_FLUX,
     &                     FICSOS_LOG, APPEL_SOS_CORE, IER)

      IMPLICIT NONE




C Constantes SOS
C -----------------
            
      INTEGER*2 IDLOG			! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_SOS)


      INTEGER*4 NT_TOT		! Nombre total de couches (atmosphère + mer)      
      PARAMETER(NT_TOT=CTE_NT_ATM+CTE_NT_SEA+1)
       
                       	    
C* Définition des variables                         
C*-----------------------------------------------------------------
  	    
      DOUBLE PRECISION RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
                                             !Cosinus des angles de Gauss (et utilisateur).
      
      DOUBLE PRECISION GA(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
                                             !Poids des angles de Gauss (et utilisateur).
      DOUBLE PRECISION DRMU	! Différence de valeurs de rmu
 
      DOUBLE PRECISION TETAS	! Angle zénithal solaire en degrés (0 < tetas < 90).
      				! (angle complémentaire : mus= -cos(tetas))

      DOUBLE PRECISION TETASW	! Angle zénithal solaire en degrés (0 < tetasw < 90).
      				! transmis dans l'eau.
      				! (angle complémentaire : musw= -cos(tetasw))

	!Paramètres des propriétés radiatives des aérosols
      DOUBLE PRECISION COEF_TRONCA_AER	! Coefficient de troncature de la fonction de phase.
      					! NB : F = 0.5 * COEF_TRONCA_AER
      DOUBLE PRECISION PIZ_AER		! Albédo de simple diffusion.
      DOUBLE PRECISION PIZTR_AER	! Albédo de simple diffusion après troncature.

      DOUBLE PRECISION ALPHA_AER(0:CTE_OS_NB_MAX)	! Paramètres alpha(k).
      DOUBLE PRECISION BETA_AER(0:CTE_OS_NB_MAX)	! Paramètres beta11(k).
      DOUBLE PRECISION GAMMA_AER(0:CTE_OS_NB_MAX)	! Paramètres gamma12(k).
      DOUBLE PRECISION ZETA_AER(0:CTE_OS_NB_MAX)	! Paramètres zeta(k).

	!Paramètres des propriétés radiatives du phytoplancton
      DOUBLE PRECISION COEF_TRONCA_CHL	! Coefficient de troncature de la fonction de phase.
      DOUBLE PRECISION PIZ_CHL		! Albédo de simple diffusion.
      DOUBLE PRECISION PIZTR_CHL	! Albédo de simple diffusion après troncature.

      DOUBLE PRECISION ALPHA_CHL(0:CTE_OS_NB_MAX)	! Paramètres alpha(k).
      DOUBLE PRECISION BETA_CHL(0:CTE_OS_NB_MAX)	! Paramètres beta11(k).
      DOUBLE PRECISION GAMMA_CHL(0:CTE_OS_NB_MAX)	! Paramètres gamma12(k).
      DOUBLE PRECISION ZETA_CHL(0:CTE_OS_NB_MAX)	! Paramètres zeta(k).

	!Paramètres des propriétés radiatives des sédiments
      DOUBLE PRECISION COEF_TRONCA_MLP	! Coefficient de troncature de la fonction de phase.
      DOUBLE PRECISION PIZ_MLP		! Albédo de simple diffusion.
      DOUBLE PRECISION PIZTR_MLP	! Albédo de simple diffusion après troncature.

      DOUBLE PRECISION ALPHA_MLP(0:CTE_OS_NB_MAX)	! Paramètres alpha(k).
      DOUBLE PRECISION BETA_MLP(0:CTE_OS_NB_MAX)	! Paramètres beta11(k).
      DOUBLE PRECISION GAMMA_MLP(0:CTE_OS_NB_MAX)	! Paramètres gamma12(k).
      DOUBLE PRECISION ZETA_MLP(0:CTE_OS_NB_MAX)	! Paramètres zeta(k).


	!Profils atmosphériques
      DOUBLE PRECISION ATM_Z(0:CTE_NT_ATM)	! Profil d'altitudes dans l'atmosphère (m).
      DOUBLE PRECISION ATM_TAUEXT(0:CTE_NT_ATM)	! Profil d'épaisseur optique d'extinction 
      						! de l'atmosphère.     
      DOUBLE PRECISION ATM_PCRAY(0:CTE_NT_ATM)  ! Proportion de molécules pour la diffusion
      						! dans chaque couche atmosphérique
      DOUBLE PRECISION ATM_PCAER(0:CTE_NT_ATM)  ! Proportion d'aérosols pour la diffusion 
      						! dans chaque couche atmosphérique      

	!Profils atmosphériques ajustés à la troncature aérosols
      DOUBLE PRECISION ATM_TAUEXT_TR(0:CTE_NT_ATM) ! Profil d'épaisseur optique d'extinction
      						   ! de l'atmosphère ajusté à la troncature.

      DOUBLE PRECISION ATM_PCRAY_TR(0:CTE_NT_ATM)  ! Proportion de molécules pour la 
      						   ! diffusion dans chaque couche 
						   ! atmosphérique, ajustée à la troncature.
      DOUBLE PRECISION ATM_PCAER_TR(0:CTE_NT_ATM)  ! Proportion d'aérosols pour la diffusion
      						   ! dans chaque couche atmosphérique,
						   ! ajustée à la troncature.     


      DOUBLE PRECISION DTAUEXT		! Ep opt d'extinction (totale) d'une couche
      DOUBLE PRECISION DTAUEXT_TR	! Ep opt d'extinction d'une couche,  
      					! ajustée à la troncature.
      
      DOUBLE PRECISION AER_DTAUSCA	! Ep opt de diffusion des aérosols d'une couche
      DOUBLE PRECISION AER_DTAUSCA_TR	! Ep opt de diffusion des aérosols d'une couche, 
      					! ajustée à la troncature.
      DOUBLE PRECISION AER_DTAUEXT_TR	! Ep opt d'extinction des aérosols d'une couche, 
      					! ajustée à la troncature.
					
      DOUBLE PRECISION MOL_DTAUEXT	! Ep opt d'extinction des molécules d'une couche
      DOUBLE PRECISION MOL_DTAUSCA	! Ep opt de diffusion des molécules d'une couche

      DOUBLE PRECISION MOL_YS_DET_DTAUABS ! Ep opt d'absorption des molécules, particules 
      					  ! jaunes et sédiments d'une couche
					
	!Profils marins      
      DOUBLE PRECISION SEA_Z(0:CTE_NT_SEA)	! Profil de profondeur dans la mer (m).     
      DOUBLE PRECISION SEA_TAUEXT(0:CTE_NT_SEA)	! Profil d'épaisseur optique d'extinction 
      						! de la mer.     
      DOUBLE PRECISION SEA_PCRAY(0:CTE_NT_SEA)  ! Proportion de molécules pour la diffusion 
      						! dans chaque couche marine.
      DOUBLE PRECISION SEA_PC_CHL(0:CTE_NT_SEA) ! Proportion de phytoplancton pour la 
      						! diffusion dans chaque couche marine.
      DOUBLE PRECISION SEA_PC_MLP(0:CTE_NT_SEA) ! Proportion de sédiments pour la diffusion 
      						! dans chaque couche marine.
	
      DOUBLE PRECISION Z_I	!Altitude ou profondeur du niveau I
      					
	!Profils marins ajustés à la troncature des fonctions de phase 
	!des sédiments et du phytoplancton      
      DOUBLE PRECISION SEA_TAUEXT_TR(0:CTE_NT_SEA) ! Profil d'épaisseur optique d'extinction
      						   ! de la mer, ajusté aux troncatures.     
      DOUBLE PRECISION SEA_PCRAY_TR(0:CTE_NT_SEA)  ! Proportion de molécules pour la 
      						   ! diffusion dans chaque couche marine, 
						   ! ajusté aux troncatures.
      DOUBLE PRECISION SEA_PC_CHL_TR(0:CTE_NT_SEA) ! Proportion de phytoplancton pour la 
      						   ! diffusion dans chaque couche marine, 
						   ! ajusté aux troncatures.
      DOUBLE PRECISION SEA_PC_MLP_TR(0:CTE_NT_SEA) ! Proportion de sédiments pour la  
      						   ! diffusion dans chaque couche marine, 
						   ! ajusté aux troncatures.

      DOUBLE PRECISION MLP_DTAUSCA	! Ep opt de diffusion des sédiments d'une couche.
      DOUBLE PRECISION MLP_DTAUEXT	! Ep opt d'extinction des sédiments d'une couche.
      DOUBLE PRECISION MLP_DTAUSCA_TR	! Ep opt de diffusion des sédiments d'une couche, 
      					! ajustée à la troncature.
      DOUBLE PRECISION MLP_DTAUEXT_TR	! Ep opt d'extinction des sédiments d'une couche, 
      					! ajustée à la troncature.

      DOUBLE PRECISION CHL_DTAUSCA	! Ep opt de diffusion du phytoplancton d'une couche.
      DOUBLE PRECISION CHL_DTAUEXT	! Ep opt d'extinction du phytoplancton d'une couche.
      DOUBLE PRECISION CHL_DTAUSCA_TR	! Ep opt de diffusion du phytoplancton d'une couche,
      					! ajustée à la troncature.
      DOUBLE PRECISION CHL_DTAUEXT_TR	! Ep opt d'extinction du phytoplancton d'une couche,
      					! ajustée à la troncature.

					
      DOUBLE PRECISION PROFIL_Z(0:NT_TOT) ! Profil global d'altitudes et de profondeurs (m).
					     
	!Profil de flux descendant, normalisé par rapport à l'éclairement incident 
	!au TOA Es (avec Es = PI).
      DOUBLE PRECISION PROFIL_FLUX_DOWN_TOT(0:NT_TOT)   ! Profil du flux descendant total 
      							! (direct + diffus), normalisé par 
							! rapport à Es 
				     
	!Champ de luminance pour l'azimut phi
	!en fonction du niveau z et de l'angle mu
	!    mu < 0 : champ descendant.
	!    mu > 0 : champ montant.
	!
	!    phi1 : direction azimutale définie par l'utilisateur
	!    phi2 : direction phi1 + Pi
	!
	!   NB : normalisé à l'éclairement solaire au TOA (PI * I(z) / Esun)
      DOUBLE PRECISION XIT_PHI1(0:NT_TOT,
     &                          -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  !Paramètre de Stokes I 
      DOUBLE PRECISION XQT_PHI1(0:NT_TOT,
     &                          -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  !Paramètre de Stokes Q.
      DOUBLE PRECISION XUT_PHI1(0:NT_TOT,
     &                          -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  !Paramètre de Stokes U.
				
      DOUBLE PRECISION XIT_PHI2(0:NT_TOT,
     &                          -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  !Paramètre de Stokes I 
      DOUBLE PRECISION XQT_PHI2(0:NT_TOT,
     &                          -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  !Paramètre de Stokes Q.
      DOUBLE PRECISION XUT_PHI2(0:NT_TOT,
     &                          -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  !Paramètre de Stokes U.

			
	!Angle de diffusion pour l'azimut phi
	!en fonction du niveau z et de l'angle mu
	!    mu < 0 : champ descendant.
	!    mu > 0 : champ montant.
	!
	!    phi1 : direction azimutale définie par l'utilisateur
	!    phi2 : direction phi1 + Pi
      DOUBLE PRECISION ANGDIFF_PHI1(0:NT_TOT,
     &                              -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)
     
      DOUBLE PRECISION ANGDIFF_PHI2(0:NT_TOT,
     &                              -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  			
      
	
      DOUBLE PRECISION ANGDIFF	! Valeur de l'angle de diffusion
      				                      
C---- Variables liées à la définition de l'interface air - mer  

      DOUBLE PRECISION SEA_IND    ! (E) Indice de réfraction de la mer pour la  
      				  !  longueur d'onde de simulation de la luminance.
      DOUBLE PRECISION WIND	  ! (E) Vitesse du vent (m/s).
      

C----

      DOUBLE PRECISION RHO_SURF	! (E)  Albédo de la surface (composante lambertienne)
      DOUBLE PRECISION RHO_BOT	! (E)  Albédo du fond marin (composante lambertienne)     
         

C-- Variables liées aux calculs d'Ordres Successifs et à la production
C   des fichiers résultats
      DOUBLE PRECISION PHI	! (E)  Azimut relatif (degrés) entre la direction de visée
      				!      et celle du Soleil.
      DOUBLE PRECISION PHI1	! Azimut relatif (radians)
      DOUBLE PRECISION PHI2	! Azimut relatif PHI1 + Pi (radians)
      INTEGER*4 LEVEL_OUT	! (E)  Indice de type de niveau de sortie
      DOUBLE PRECISION USER_Z_OUT	! (E)  Altitude ou profondeur (mètres) pour la 
      					!      représentation de sortie de la luminance
					!      en fonction de l'angle zénithal de visée 
					!      (Phi et Z fixés)
					!      --> Valeur définie par l'utilisateur.
      DOUBLE PRECISION Z_OUT	!  Altitude ou profondeur (mètres) pour la 
      				!  représentation de sortie de la luminance
				!  en fct de l'angle zénithal de visée (Phi et Z fixés)

      DOUBLE PRECISION VZA_OUT	! (E)  Angle zénithal de visée (degrés) pour la
      				!      représentation de sortie de la luminance en fct 
				!      de la profondeur (ou altitude) (Phi et VZA fixés)

      DOUBLE PRECISION COS_VZA_OUT ! Cosinus de VZA_OUT
				
      DOUBLE PRECISION Z1	! Altitude ou profondeur (mètres) du niveau juste supérieur
      				! à l'altitude ou profondeur définie par l'utilisateur.	
      DOUBLE PRECISION Z2	! Altitude ou profondeur (mètres) du niveau juste inférieur
      				! à l'altitude ou profondeur définie par l'utilisateur.	

      DOUBLE PRECISION DZ	! Différence Z2-Z1
 
      DOUBLE PRECISION VZA1	! Angle zénithal de visée (degrés) juste supérieur
      				! à l'angle défini par l'utilisateur. 
      DOUBLE PRECISION VZA2	! Angle zénithal de visée (degrés) juste inférieur
      				! à l'angle défini par l'utilisateur. 

      DOUBLE PRECISION COEF_A   ! Coef. pour l'interpolation linéaire sur Z de I, Q ou U
      DOUBLE PRECISION COEF_B   ! Coef. pour l'interpolation linéaire sur Z de I, Q ou U
      
      
      DOUBLE PRECISION TPOL     ! Taux de polarisation (%)
      DOUBLE PRECISION LPOL     ! Intensité en polarisation
      DOUBLE PRECISION XAN      ! Angle de polarisation (degrés)

      DOUBLE PRECISION TETA	! Angle de visée
      DOUBLE PRECISION COSMIN	! Cosinus de l'angle de visée minimal
      DOUBLE PRECISION COSMAX	! Cosinus de l'angle de visée maximal

      DOUBLE PRECISION XI	! Paramètre de Stokes I
      DOUBLE PRECISION XQ	! Paramètre de Stokes Q      
      DOUBLE PRECISION XU	! Paramètre de Stokes U      
      
      
      DOUBLE PRECISION REFL	! Reflectance pour le niveau de sortie sollicité 
      				! (altitude ou profondeur)
      DOUBLE PRECISION REFL_POL	! Reflectance polarisée pour le niveau de sortie sollicité 
      DOUBLE PRECISION FLUX_DOWN_NORMA	! Flux descendant total (direct + diffus)
      				        ! normalisé par l'éclairement solaire incident 
					! au TOA (avec Es = PI). 
					! Ce flux intègre les diffusions vers le bas
					! du rayonnement réfléchi sur la surface ou le fond.
      
      DOUBLE PRECISION VALUE_DBLE       ! Valeur lue dans fichier (type double)

      INTEGER*4  VALUE_INT       	! Valeur lue dans fichier (type integer)
				      
      INTEGER*4 N0	! Numéro de mu de Gauss pour l'angle
      			! zénithal solaire dans l'air.
			! (angle complémentaire : mus = -rmu(N0))

      INTEGER*4 M0	! Numéro de mu de Gauss pour l'angle
      			! zénithal solaire dans l'eau (transmission mer plate).
			! (angle complémentaire : musw = -rmu(M0))
					
      INTEGER*4 NBMU_GAUSS      ! Nombre d'angles de Gauss utilisés       
      INTEGER*4 NBMU            ! Nombre d'angles effectifs des simulations
      INTEGER*4 OS_NB 	        ! Ordre des développements des fonctions de phase 
			        ! en Polynômes de Legendre.      

      INTEGER*4 IND_ANGOUT(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
                                ! Indices des tableaux d'angles pour lesquels il doit y
      				! avoir une sortie dans le fichier résultat des angles 
				! utilisateur  (1 : pour une sortie requise)	

      INTEGER*4 J       ! Indice des tableaux d'angles
      INTEGER*4 J1	! Indice de l'angle au cos supérieur à celui  
      			! de l'angle défini par l'utilisateur
      INTEGER*4 J2	! Indice de l'angle au cos inférieur à celui  
      			! de l'angle défini par l'utilisateur	
      INTEGER*4 K	! Indice des tableaux de matrice de phase  
      INTEGER*4 I	! Niveau des profils
      INTEGER*4 IPROF 	! Niveau des profils (pour distinction profil marin / profil total)
      INTEGER*4 I1	! Niveau du profil supérieur à celui de la hauteur ou  
      			! profondeur définie par l'utilisateur
      INTEGER*4 I2	! Niveau du profil inférieur à celui de la hauteur ou  
      			! profondeur définie par l'utilisateur			

      INTEGER*4 IBORM	! Ordre maximal du développement en séries de Fourier
      			! en cours de traitement :
			!   _ 2 pour une atmosphère & mer purement moléculaire,
			!   _ OS_NB en présence d'aérosols ou d'hydrosols.

      INTEGER*4 IMAT_SURF ! (E) Option de BRDF / BPDF de surface :
      			  !   0 : mer plate.
			  !   1 : mer agitée.


      INTEGER*4 LDIRS	! longueur de la chaine de caractères DIRS. 
      INTEGER*4 LFICS	! longueur de la chaine de caractères FICS.     
      

      INTEGER*4 IS	! Ordre en cours du développement en séries 
                        ! de Fourier associé à la matrice de phase
			   
      INTEGER*4 IG	! Ordre d'interaction en cours (diffusion + réflexion).
     
      INTEGER*4 IGMAX	! Ordre maximal de diffusion.

      INTEGER*4 IFIN    ! nb de caractères significatifs d'une chaine

      INTEGER*4 NVAL 	! Integer pour la lecture de fichier ANGLES

C---- Fichiers utilisés
      CHARACTER*CTE_LENFIC2 FICANGLES        ! (E) Chemin et nom du fichier des angles 
      					     !     effectifs      
      CHARACTER*CTE_LENFIC2 FICANGLES_USER   ! Chemin et nom du fichier des angles 
                                             ! "utilisateurs"

		! Nom des fichiers de coefficients de matrice de phase (chemin complet) :
      CHARACTER*CTE_LENFIC2 FICGRANU_CHL     ! (E) pour le phytoplancton. 
      CHARACTER*CTE_LENFIC2 FICGRANU_MLP     ! (E) pour les sédiments.
      CHARACTER*CTE_LENFIC2 FICGRANU_AER     ! (E) pour les aérosols. 

		! Nom des fichiers de profil (chemin complet) :	    
      CHARACTER*CTE_LENFIC2 FICPROFIL_SEA    ! (E) pour le profil marin . 
      CHARACTER*CTE_LENFIC2 FICPROFIL_ATM    ! (E) pour le profil atmosphérique.    
   	
      CHARACTER*CTE_LENDIR DIRS		! (E) Chemin d'accès aux fichiers SURFACE.      
      CHARACTER*CTE_LENFIC1 FICS   	! Nom d'un fichier d'interface SURFACE 
      					! (sans arborescence).

		! Nom des fichiers de Matrice d'interface de surface (chemin complet) :
      CHARACTER*CTE_LENFIC2 FICRAA 	! pour la réflexion air / air.
      CHARACTER*CTE_LENFIC2 FICTAW 	! pour la transmission air / eau.
      CHARACTER*CTE_LENFIC2 FICRWW 	! pour la réflexion eau / eau.
      CHARACTER*CTE_LENFIC2 FICTWA 	! pour la transmission eau / air .      	    


      CHARACTER*CTE_LENFIC2 FICSOS_RES_VS_VZA  	! (S) Nom du fichier résultat donnant la
	     				       	!luminance en fct de l'angle zénithal 
	     					!de visée (Phi et Z fixés)
						
      CHARACTER*CTE_LENFIC2 FICSOS_RES_VS_Z  	! (S) Nom du fichier résultat donnant la
	     				       	!luminance en fct de la profondeur 
	     					!ET de l'angle de visée (Phi fixé)
	
      CHARACTER*CTE_LENFIC2 FICSOS_RES_ADV_UP  	! (S) Nom du fichier résultat donnant la
	     				       	!luminance montante en fct de la profondeur 
	     					!(ou altitude) (Phi et VZA fixés)

      CHARACTER*CTE_LENFIC2 FICSOS_RES_ADV_DOWN	! (S) Nom du fichier résultat donnant la
	     				       	!luminance descendante en fct de la 
						!profondeur (ou altitude) (Phi et VZA fixés)
						
      CHARACTER*CTE_LENFIC2 FICSOS_RES_BIN  	! (S) Nom du fichier résultat binaire
      						!en sortie des calculs SOS	

      CHARACTER*CTE_LENFIC2 FIC_FLUX    	! (S) Fichier des profils de flux
      
      CHARACTER*CTE_LENFIC2 FICSOS_LOG          ! (S) Nom du fichier trace 

      CHARACTER*1000 CVAL		! Chaine de caractères pour la lecture 
      					! de valeurs inutilisées
      CHARACTER*LENLINE LIGNE_TEXTE	! Ligne de caractères pour décodage de paramètres

C---- Variables logiques
      LOGICAL TRACE     ! = vrai si écriture dans le fichier trace    
      LOGICAL LTAER     ! = vrai si épaisseur optique des aérosols nulle
      LOGICAL LTCHL     ! = vrai si épaisseur optique du phytoplancton nulle
      LOGICAL LTMLP     ! = vrai si épaisseur optique des sédiments nulle

      LOGICAL APPEL_SOS_CORE ! = vrai si le fichier FICSOS_RES_BIN est à calculer
      			     ! (i.e. s'il n'est pas déjà calculé) 
			     
      LOGICAL ADD_TETAS_NOMFICS	   ! =vrai s'il faut ajouter TETAS au nom 
      				   ! du fichier de matrice de surface
      LOGICAL ADD_TETASW_NOMFICS   ! =vrai s'il faut ajouter TETASW au nom 
      				   ! du fichier de matrice de surface

      LOGICAL LRECH_OK  ! = vrai si recherche d'une valeur dans table fructueuse
              
C---- Code d'erreur
      INTEGER*4 IER     ! (S) Code d'erreur =0 si pas d'erreur, =-1 sinon
         
     
     
   
C* Initialisation                     
C*-----------------------------------------------------------------
      IER=0

      LTAER=.TRUE.		!épaisseur optique des aérosols nulle
      LTCHL=.TRUE.		!épaisseur optique du phytoplancton nulle
      LTMLP=.TRUE.		!épaisseur optique des sédiments nulle
           
      
C* Ouverture du fichier Trace 
C------------------------------------------------------
      IF (FICSOS_LOG.EQ.'NO_LOG_FILE') THEN
	 TRACE=.FALSE.
      ELSE
         TRACE=.TRUE.
	 OPEN (IDLOG,FILE=FICSOS_LOG,ERR=1000)
      ENDIF


C* Lecture des tables d'angles et de poids utiles aux simulations
C* --> Lecture de chaque ligne d'information et stockage en tant
C*     que chaine de caractères. Identification du séparateur (:)
C*     "texte : valeur utile" et extraction du champ suivant le 
C*     séparateur avec affectation au paramètre du type final
C*     souhaité (integer, float ou chaine de caractères)
C* -----------------------------------------------------------------
      OPEN(10,FILE=FICANGLES,STATUS='OLD',ERR=1020)

      READ(10,'(a)',ERR=1021) LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')
      READ(LIGNE_TEXTE(IFIN+1:LENLINE),*) NBMU
      
      READ(10,'(a)',ERR=1021) LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')
      READ(LIGNE_TEXTE(IFIN+1:LENLINE),*) NBMU_GAUSS
      
      READ(10,'(a)',ERR=1021) LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')
      FICANGLES_USER=LIGNE_TEXTE(IFIN+1:LENLINE)
      IFIN=INDEX(FICANGLES,' ')
      FICANGLES_USER=FICANGLES_USER(1:IFIN)
      
      READ(10,'(a)',ERR=1021) LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')
      READ(LIGNE_TEXTE(IFIN+1:LENLINE),*) TETAS
      
      READ(10,'(a)',ERR=1021) LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')
      READ(LIGNE_TEXTE(IFIN+1:LENLINE),*) N0
      
      READ(10,'(a)',ERR=1021) LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')
      READ(LIGNE_TEXTE(IFIN+1:LENLINE),*) TETASW
      
      READ(10,'(a)',ERR=1021) LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')
      READ(LIGNE_TEXTE(IFIN+1:LENLINE),*) M0
      
      READ(10,'(a)',ERR=1021) LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')
      READ(LIGNE_TEXTE(IFIN+1:LENLINE),*) OS_NB
      
      READ(10,*,ERR=1021) CVAL
      READ(10,*,ERR=1021) CVAL
      READ(10,*,ERR=1021) CVAL

      DO J=1,NBMU
	  READ(10,*,ERR=1021) NVAL, RMU(J), GA(J), IND_ANGOUT(J)
      ENDDO 

      CLOSE(10)	! Fermeture du fichier des angles
      
      DO J=1,NBMU
         RMU(-J) = -RMU(J)
	 GA(-J) =  GA(J)
	 IND_ANGOUT(-J) = IND_ANGOUT(J)
      ENDDO 
      
      
      !Contrôle d'applicabilite
      IF (ABS(N0).GT.NBMU) GOTO 1030
      IF ((TETAS.LT.0.D-00).OR.(TETAS.GE.90.D-00)) GOTO 1031
      
    
       
      IF (TRACE) THEN
         WRITE (IDLOG,*,ERR=1010) "USED ANGLES "
	 WRITE (IDLOG,*,ERR=1010) "************"
	 WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) 
     &	 	"Total number of angles : ", NBMU
	 WRITE (IDLOG,*,ERR=1010) 
     &	 	"Number of Gauss angles : ", NBMU_GAUSS
	 WRITE (IDLOG,*,ERR=1010) "Solar incidence : "
	 WRITE (IDLOG,*,ERR=1010) 
     &	 	 "  --> Solar zenith angle in the air : ", TETAS
	 WRITE (IDLOG,*,ERR=1010) 
     &	 	 "  --> Index in the angles table : ", N0
	 WRITE (IDLOG,*,ERR=1010) 
     &	 	 "  --> Solar zenith angle in the sea : ", TETASW
	 WRITE (IDLOG,*,ERR=1010) 
     &	 	 "  --> Index in the angles table : ", M0	
      
	 WRITE (IDLOG,*,ERR=1010) " "
         IF (FICANGLES_USER.NE.'NO_USER_ANGLES') THEN
             IFIN=INDEX(FICANGLES_USER,' ')
	     WRITE(IDLOG,*,ERR=1010)  
     &	       "A file of user's angles is used :",
     &         FICANGLES_USER(1:IFIN)
     	     WRITE(IDLOG,*,ERR=1010) 
     &       "Number of user's angles + solar angles SZA (air & sea):",
     &         (NBMU-NBMU_GAUSS)
	 ELSE
	     WRITE(IDLOG,*,ERR=1010)  
     &	       "No file of user's angles"  
	 ENDIF !Fin test Pas de fichier d'angles utilisateur
         WRITE (IDLOG,*,ERR=1010) " " 
	 WRITE (IDLOG,*,ERR=1010) "Max order NB : ", OS_NB
	 WRITE (IDLOG,*,ERR=1010) " " 
	 WRITE (IDLOG,*,err=1010) "List of used angles :"
	 WRITE (IDLOG,*,err=1010) 
     &	 	 "   Index, mu, angle (degrees), weigth"
     
	 DO 10 J=-NBMU,NBMU
	    IF (J.EQ.0) GOTO 10
	    IF (J.LT.0) THEN 
	        WRITE(IDLOG,223,ERR=1010) J, RMU(J),
     &	              -DACOS(-RMU(J))*180./INCTE_PI,GA(J),
     &                 IND_ANGOUT(J)
            ELSE
	        WRITE(IDLOG,223,ERR=1010) J, RMU(J),
     &	               DACOS(RMU(J))*180./INCTE_PI,GA(J), 
     &      	       IND_ANGOUT(J)
	    ENDIF
10       CONTINUE	
	 WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) " "
      ENDIF !Fichier Trace	 




	
C* Lecture du fichier AEROSOLS
C* -----------------------------------------------------------------
      OPEN(10,FILE=FICGRANU_AER,STATUS='OLD',ERR=1040)

      !Lecture des informations de contenu du fichier   
      READ(10,'(a)',ERR=1041) LIGNE_TEXTE
      READ(10,'(a)',ERR=1041) LIGNE_TEXTE

      ! Section efficace d'extinction
      READ(10,'(a)',ERR=1041) LIGNE_TEXTE

      ! Section efficace de diffusion
      READ(10,'(a)',ERR=1041) LIGNE_TEXTE

      ! Facteur d'asymétrie (non tronqué)
      READ(10,'(a)',ERR=1041) LIGNE_TEXTE
	 
      ! Altitude / profondeur des particules
      READ(10,'(a)',ERR=1041) LIGNE_TEXTE
	 
      !Volume moyen d'une particule 
      READ(10,'(a)',ERR=1041) LIGNE_TEXTE

      !Indice moyen d'une particule (partie réelle)      
      READ(10,'(a)',ERR=1041) LIGNE_TEXTE
          
      ! Coefficient de troncature  
      READ(10,'(a)',ERR=1041) LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')
      READ(LIGNE_TEXTE(IFIN+1:LENLINE),*) COEF_TRONCA_AER
	 
      ! Albédo de simple diffusion (éventuellement tronqué) 
      READ(10,'(a)',ERR=1041) LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')
      READ(LIGNE_TEXTE(IFIN+1:LENLINE),*)  PIZTR_AER 

      !Lecture des informations de contenu des colonnes
      READ(10,'(a)',ERR=1041) LIGNE_TEXTE
      READ(10,'(a)',ERR=1041) LIGNE_TEXTE
      READ(10,'(a)',ERR=1041) LIGNE_TEXTE
      
      DO K=0,OS_NB
         READ(10,60,ERR=1041) ALPHA_AER(K),BETA_AER(K),
     &	 		      GAMMA_AER(K),ZETA_AER(K)
      ENDDO

      CLOSE(10)	! Fermeture du fichier de phase aérosols
    	
   
      IF (TRACE) THEN
         WRITE (IDLOG,*,ERR=1010) "AEROSOLS  COEFF."
	 WRITE (IDLOG,*,ERR=1010) "****************"
	 WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) " File : ", FICGRANU_AER
	 WRITE (IDLOG,*,ERR=1010) " " 
	 WRITE (IDLOG,*,ERR=1010) 
     &	 	"Truncation coef : ", COEF_TRONCA_AER
	 WRITE (IDLOG,*,ERR=1010) 
     &	   "Single scattering albedo - adjusted to the truncation : ",
     &     PIZTR_AER
	 WRITE (IDLOG,*,ERR=1010) " "
	 
	 WRITE (IDLOG,*,ERR=1010) "Phase matrix coefficients "
	 WRITE (IDLOG,*,ERR=1010) "(adjusted to the truncation) : "
         WRITE (IDLOG,*,ERR=1010) 
     &	 "   ALPHA_AER       BETA_AER        GAMMA_AER       ZETA_AER"
	 DO K=0,OS_NB
            WRITE(IDLOG,60,ERR=1010) ALPHA_AER(K),BETA_AER(K),
     &	 		             GAMMA_AER(K),ZETA_AER(K)
         ENDDO
	 WRITE (IDLOG,*,ERR=1010) " " 
	 WRITE (IDLOG,*,ERR=1010) " "  	
      ENDIF !Fichier Trace	 


	

C* Lecture du fichier profil atmosphérique
C* -----------------------------------------------------------------
      OPEN(10,FILE=FICPROFIL_ATM,STATUS='OLD',ERR=1042)
      
      !Lecture des informations de contenu du fichier   
      DO I=1,10
         READ(10,'(a)',ERR=1043) LIGNE_TEXTE
      ENDDO
      
      !Lecture des données de profil
      DO I=0,CTE_NT_ATM
         READ(10,70,ERR=1043)J,ATM_Z(I),ATM_TAUEXT(I),
     &	 		     ATM_PCAER(I),ATM_PCRAY(I)
         ATM_Z(I)=1000.*ATM_Z(I) !Passage km en m
      ENDDO
	
      CLOSE(10)	! Fermeture du fichier de profil atmosphérique
 

C* Ajustement du profil à la troncature :
C*   --> Fourni les tableaux qui sont utilisés dans le code OS
C* 	  ATM_TAUEXT_TR : ep opt d'extinction totale ajusté à la troncature
C*	  ATM_PCAER_TR  : taux de mélange des aérosols ajusté à la troncature
C*	  ATM_PCRAY_TR  : taux de mélange des molécules ajusté à la troncature
C  -------------------------------------------------       
      ATM_TAUEXT_TR(0) = ATM_TAUEXT(0)
      ATM_PCAER_TR(0)  = ATM_PCAER(0) 
      ATM_PCRAY_TR(0)  = ATM_PCRAY(0) 

      IF ( COEF_TRONCA_AER.NE.0.) THEN 
          
         DO I=1,CTE_NT_ATM
      
            !Ep opt d'extinction totale de la couche I
	    DTAUEXT = ATM_TAUEXT(I) - ATM_TAUEXT(I-1)
	 
            !Calcul de l'ep opt aérosols d'extinction tronquée, de la couche I
            AER_DTAUSCA = DTAUEXT*ATM_PCAER(I)
	    AER_DTAUSCA_TR = AER_DTAUSCA*(1.-0.5*COEF_TRONCA_AER)
	    AER_DTAUEXT_TR = AER_DTAUSCA_TR / PIZTR_AER   
	 
	 
	    !Identification si l'ep opt aérosols est non nulle
	    IF(AER_DTAUEXT_TR.NE.0.) LTAER=.FALSE.	!TAU_AER non nulle
	 
	    !Calcul de l'ep opt d'extinction des molécules de la couche I
	    !(= ep opt de diffusion) 
	    MOL_DTAUEXT = DTAUEXT*ATM_PCRAY(I) 
	 
	    !Calcul de l'ep opt d'extinction totale de la couche I
	    DTAUEXT_TR = AER_DTAUEXT_TR + MOL_DTAUEXT
	 
	    !Calcul de l'ep opt d'extinction totale, au niveau I, ajustée à la troncature
	    ATM_TAUEXT_TR(I) = ATM_TAUEXT_TR(I-1) + DTAUEXT_TR 
     
            !Calcul des taux de mélange en diffusion, au niveau I, ajustés à la troncature
            ATM_PCAER_TR(I) = AER_DTAUSCA_TR / DTAUEXT_TR
	    ATM_PCRAY_TR(I) = MOL_DTAUEXT / DTAUEXT_TR
     
         ENDDO 
	 
      ELSE !Cas sans troncature (pouvant aussi être un cas sans aérosols)
      
         DO I=1,CTE_NT_ATM
	    ATM_TAUEXT_TR(I) = ATM_TAUEXT(I)
            ATM_PCAER_TR(I)  = ATM_PCAER(I)
	    ATM_PCRAY_TR(I)  = ATM_PCRAY(I)  
	    
	    !Identification si l'ep opt aérosols est non nulle
	    IF(ATM_PCAER_TR(I).NE.0.) LTAER=.FALSE.	!TAU_AER non nulle
         ENDDO 
	 
      ENDIF  !Fin test si troncature
      

      IF (TRACE) THEN
         WRITE (IDLOG,*,ERR=1010) "ATMOSPHERIC PROFILE   "
	 WRITE (IDLOG,*,ERR=1010) "*******************"
	 WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) " File : ", FICPROFIL_ATM
	 WRITE (IDLOG,*,ERR=1010) " " 
	 IF (LTAER) THEN
	    WRITE (IDLOG,*,ERR=1010) " Simulation without aerosols"
	 ELSE
	    WRITE (IDLOG,*,ERR=1010) 	 
     &	     " Adjustement to the aerosol phase function truncation : "
	    WRITE (IDLOG,*,ERR=1010) " "
	    WRITE (IDLOG,*,ERR=1010) 
     &	     "   -- Truncation coef (A) : ",COEF_TRONCA_AER
	    WRITE (IDLOG,*,ERR=1010) 
     &	     "   -- Single scatt. albedo - adjusted to the trunca. : ",
     &       PIZTR_AER
     
            ! Calcul de l'albédo de simple diffusion non tronquée 
            ! à partir de la valeur tronquée
	    PIZ_AER=PIZTR_AER/(1+0.5*COEF_TRONCA_AER*(PIZTR_AER-1))

            WRITE (IDLOG,*,ERR=1010) 
     &	          "   -- Single scattering albedo : ",PIZ_AER
         ENDIF
	 
         WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) 
     &	 "            Z(m)  TAUEXT   TAUEXT_TR  PCAER     PCAER_TR",
     &   "  PCRAY     PCRAY_TR"
	 DO I=0,CTE_NT_ATM
            WRITE(IDLOG,71,ERR=1010) I, ATM_Z(I),
     &				     ATM_TAUEXT(I), ATM_TAUEXT_TR(I),
     &	    			     ATM_PCAER(I),  ATM_PCAER_TR(I),
     &	    			     ATM_PCRAY(I),  ATM_PCRAY_TR(I)
         ENDDO
	 WRITE (IDLOG,*,ERR=1010) " " 
	 WRITE (IDLOG,*,ERR=1010) " "  	
      ENDIF !Fichier Trace
      
      		
		
C* Lecture du fichier HYDROSOLS
C* -----------------------------------------------------------------
      OPEN(10,FILE=FICGRANU_CHL,STATUS='OLD',ERR=1044)

      !Lecture des informations de contenu du fichier   
      READ(10,'(a)',ERR=1045) LIGNE_TEXTE
      READ(10,'(a)',ERR=1045) LIGNE_TEXTE

      ! Section efficace d'extinction
      READ(10,'(a)',ERR=1045) LIGNE_TEXTE

      ! Section efficace de diffusion
      READ(10,'(a)',ERR=1045) LIGNE_TEXTE

      ! Facteur d'asymétrie (non tronqué)
      READ(10,'(a)',ERR=1045) LIGNE_TEXTE
	 
      ! Altitude / profondeur des particules
      READ(10,'(a)',ERR=1045) LIGNE_TEXTE
	 
      !Volume moyen d'une particule 
      READ(10,'(a)',ERR=1045) LIGNE_TEXTE

      !Indice moyen d'une particule (partie réelle)      
      READ(10,'(a)',ERR=1045) LIGNE_TEXTE
          
      ! Coefficient de troncature  
      READ(10,'(a)',ERR=1045) LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')
      READ(LIGNE_TEXTE(IFIN+1:LENLINE),*) COEF_TRONCA_CHL
	 
      ! Albédo de simple diffusion (éventuellement tronqué) 
      READ(10,'(a)',ERR=1045) LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')
      READ(LIGNE_TEXTE(IFIN+1:LENLINE),*)  PIZTR_CHL

      !Lecture des informations de contenu des colonnes
      READ(10,'(a)',ERR=1045) LIGNE_TEXTE
      READ(10,'(a)',ERR=1045) LIGNE_TEXTE
      READ(10,'(a)',ERR=1045) LIGNE_TEXTE
      
      DO K=0,OS_NB
         READ(10,60,ERR=1045) ALPHA_CHL(K),BETA_CHL(K),
     &	 		      GAMMA_CHL(K),ZETA_CHL(K)
      ENDDO

      CLOSE(10)	! Fermeture du fichier de phase hydrosols
    	
   
      IF (TRACE) THEN
         WRITE (IDLOG,*,ERR=1010) "HYDROSOLS COEFF "
	 WRITE (IDLOG,*,ERR=1010) "****************"
	 WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) " File : ", FICGRANU_CHL
	 WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) 
     &	 	"Truncation coef (A) : ", COEF_TRONCA_CHL
	 WRITE (IDLOG,*,ERR=1010)
     &	   "Single scattering albedo - adjusted to the truncation : ",
     &     PIZTR_CHL
	 WRITE (IDLOG,*,ERR=1010) " "
	 
	 WRITE (IDLOG,*,ERR=1010) "Phase matrix coefficients "
	 WRITE (IDLOG,*,ERR=1010) "(adjusted to the truncation) : "
         WRITE (IDLOG,*,ERR=1010) 
     &	 "   ALPHA_CHL       BETA_CHL        GAMMA_CHL       ZETA_CHL"
	 DO K=0,OS_NB
            WRITE(IDLOG,60,ERR=1010) ALPHA_CHL(K),BETA_CHL(K),
     &	 		             GAMMA_CHL(K),ZETA_CHL(K)
         ENDDO
	 WRITE (IDLOG,*,ERR=1010) " " 
	 WRITE (IDLOG,*,ERR=1010) " "  	
      ENDIF !Fichier Trace	 
	
	

C* Lecture du fichier SEDIMENTS
C* -----------------------------------------------------------------
      OPEN(10,FILE=FICGRANU_MLP,STATUS='OLD',ERR=1046)

      !Lecture des informations de contenu du fichier   
      READ(10,'(a)',ERR=1047) LIGNE_TEXTE
      READ(10,'(a)',ERR=1047) LIGNE_TEXTE

      ! Section efficace d'extinction
      READ(10,'(a)',ERR=1047) LIGNE_TEXTE

      ! Section efficace de diffusion
      READ(10,'(a)',ERR=1047) LIGNE_TEXTE

      ! Facteur d'asymétrie (non tronqué)
      READ(10,'(a)',ERR=1047) LIGNE_TEXTE
	 
      ! Altitude / profondeur des particules
      READ(10,'(a)',ERR=1047) LIGNE_TEXTE
	 
      !Volume moyen d'une particule 
      READ(10,'(a)',ERR=1047) LIGNE_TEXTE

      !Indice moyen d'une particule (partie réelle)      
      READ(10,'(a)',ERR=1047) LIGNE_TEXTE
          
      ! Coefficient de troncature  
      READ(10,'(a)',ERR=1047) LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')
      READ(LIGNE_TEXTE(IFIN+1:LENLINE),*) COEF_TRONCA_MLP
	 
      ! Albédo de simple diffusion (éventuellement tronqué) 
      READ(10,'(a)',ERR=1047) LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')
      READ(LIGNE_TEXTE(IFIN+1:LENLINE),*)  PIZTR_MLP

      !Lecture des informations de contenu des colonnes
      READ(10,'(a)',ERR=1047) LIGNE_TEXTE
      READ(10,'(a)',ERR=1047) LIGNE_TEXTE
      READ(10,'(a)',ERR=1047) LIGNE_TEXTE
         
      DO K=0,OS_NB
         READ(10,60,ERR=1047) ALPHA_MLP(K),BETA_MLP(K),
     &	 		      GAMMA_MLP(K),ZETA_MLP(K)
      ENDDO

      CLOSE(10)	! Fermeture du fichier de phase sédiments
    	
   
      IF (TRACE) THEN
         WRITE (IDLOG,*,ERR=1010) "MINERAL-LIKE PARTICLES COEFF"
	 WRITE (IDLOG,*,ERR=1010) "****************************"
	 WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) " File : ", FICGRANU_MLP
	 WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) 
     &	 	"Truncation coef (A) : ", COEF_TRONCA_MLP
	 WRITE (IDLOG,*,ERR=1010)
     &	   "Single scattering albedo - adjusted to the truncation : ",
     &      PIZTR_MLP
	 WRITE (IDLOG,*,ERR=1010) " "
	 
	 WRITE (IDLOG,*,ERR=1010) "Phase matrix coefficients "
	 WRITE (IDLOG,*,ERR=1010) "(adjusted to the truncation) : "
         WRITE (IDLOG,*,ERR=1010) 
     &	 "   ALPHA_MLP       BETA_MLP        GAMMA_MLP       ZETA_MLP"
	 DO K=0,OS_NB
            WRITE(IDLOG,60,ERR=1010) ALPHA_MLP(K),BETA_MLP(K),
     &	 		             GAMMA_MLP(K),ZETA_MLP(K)
         ENDDO
	 WRITE (IDLOG,*,ERR=1010) " " 
	 WRITE (IDLOG,*,ERR=1010) " "  	
      ENDIF !Fichier Trace	  	
	

C* Lecture du fichier profil marin 
C* et ajustement à la troncature des hydrosols et sédiments
C* -----------------------------------------------------------------
      OPEN(10,FILE=FICPROFIL_SEA,STATUS='OLD',ERR=1048)
      
      !Lecture des informations de contenu du fichier   
      DO I=1,11
         READ(10,'(a)',ERR=1049) LIGNE_TEXTE
      ENDDO
      
      !Lecture des données de profil
      
      DO I=0,CTE_NT_SEA
         READ(10,80,ERR=1049)J,SEA_Z(I),SEA_TAUEXT(I),
     &	 		     SEA_PCRAY(I),SEA_PC_CHL(I),SEA_PC_MLP(I)
      ENDDO

      CLOSE(10)	! Fermeture du fichier de profil marin			


C* Ajustement du profil à la troncature :
C*   --> Fourni les tableaux qui sont utilisés dans le code OS
C* 	  SEA_TAUEXT_TR  : ep opt d'extinction totale ajustée à la troncature
C*	  SEA_PC_CHL_TR  : taux de mélange du phytoplancton ajusté à la troncature
C*	  SEA_PC_MLP_TR  : taux de mélange des sédiments ajusté à la troncature
C*	  SEA_PCRAY_TR   : taux de mélange de l'eau ajusté à la troncature
C  -------------------------------------------------       
      !Définition de l'épaisseur optique tronquée du premier niveau infinitésimal
      SEA_TAUEXT_TR(0) = SEA_TAUEXT(0)
      
      ! Calcul de l'albédo de simple diffusion non tronquée 
      ! à partir de la valeur tronquée : sédiments et phytoplancton
      PIZ_MLP=PIZTR_MLP/(1+0.5*COEF_TRONCA_MLP*(PIZTR_MLP-1))
      PIZ_CHL=PIZTR_CHL/(1+0.5*COEF_TRONCA_CHL*(PIZTR_CHL-1))
	            
      DO I=1,CTE_NT_SEA
      
            !Ep opt d'extinction totale de la couche I
	    DTAUEXT = SEA_TAUEXT(I) - SEA_TAUEXT(I-1)
	 
            !Calcul de l'ep opt d'extinction tronquée pour les sédiments (couche I)
            MLP_DTAUSCA = DTAUEXT*SEA_PC_MLP(I)
	    MLP_DTAUSCA_TR = MLP_DTAUSCA*(1.-0.5*COEF_TRONCA_MLP) 
	    
	    IF (PIZTR_MLP.NE.0.) THEN 
	       !Cas en présence de sédiments       
	       MLP_DTAUEXT_TR = MLP_DTAUSCA_TR / PIZTR_MLP 
	    ELSE
	       !Cas sans sédiments 	    
	       MLP_DTAUEXT_TR = 0.
	    ENDIF

	    !Calcul de l'ep opt d'extinction non tronquée pour les sédiments  (couche I)
	    MLP_DTAUEXT = MLP_DTAUEXT_TR 
     &  		  / (1.-0.5*COEF_TRONCA_MLP*PIZ_MLP)
     
     	    
	    !Calcul de l'ep opt d'extinction tronquée pour le phytoplancton  (couche I)
            CHL_DTAUSCA = DTAUEXT*SEA_PC_CHL(I)
	    CHL_DTAUSCA_TR = CHL_DTAUSCA*(1.-0.5*COEF_TRONCA_CHL) 
	    
	    IF (PIZTR_CHL.NE.0.) THEN 
	       !Cas en présence de phytoplancton      
	       CHL_DTAUEXT_TR = CHL_DTAUSCA_TR / PIZTR_CHL 
	    ELSE
	       !Cas sans phytoplancton 	    
	       CHL_DTAUEXT_TR = 0.
	    ENDIF	      
	  
	    !Calcul de l'ep opt d'extinction non tronquée pour le phytoplancton  (couche I)
	    CHL_DTAUEXT = CHL_DTAUEXT_TR 
     &  		  / (1.-0.5*COEF_TRONCA_CHL*PIZ_CHL)
	    
	    
	    !Identification si l'ep opt des sédiments ou du phytoplancton est non nulle
	    IF(MLP_DTAUEXT_TR.NE.0.) LTMLP=.FALSE.	!TAU_MLP non nulle
	    IF(CHL_DTAUEXT_TR.NE.0.) LTCHL=.FALSE.	!TAU_CHL non nulle
	 
	    !Calcul de l'ep opt de diffusion des molécules de la couche I
	    MOL_DTAUSCA = DTAUEXT*SEA_PCRAY(I) 

            !Ep opt de d'absorption des molécules,
	    !particules jaunes et détritus de la couche I
	    MOL_YS_DET_DTAUABS = DTAUEXT - MLP_DTAUEXT - CHL_DTAUEXT 
     &	                         - MOL_DTAUSCA
	    	 
	    !Calcul de l'ep opt d'extinction totale de la couche I
	    DTAUEXT_TR = MLP_DTAUEXT_TR + CHL_DTAUEXT_TR + MOL_DTAUSCA 
     & 			 + MOL_YS_DET_DTAUABS
	 		
	    !Calcul de l'ep opt d'extinction totale, au niveau I, ajustée à la troncature
	    SEA_TAUEXT_TR(I) = SEA_TAUEXT_TR(I-1) + DTAUEXT_TR 
     
            !Calcul des taux de mélange en diffusion, au niveau I, ajustés à la troncature
            SEA_PC_MLP_TR(I) = MLP_DTAUSCA_TR / DTAUEXT_TR
	    SEA_PC_CHL_TR(I) = CHL_DTAUSCA_TR / DTAUEXT_TR
	    SEA_PCRAY_TR(I) = MOL_DTAUSCA / DTAUEXT_TR
     
      ENDDO 
	
      !Définition des taux de mélange ajustés à la troncature 
      !du premier niveau infinitésimal	 	 
      SEA_PC_CHL_TR(0) = SEA_PC_CHL_TR(1)
      SEA_PC_MLP_TR(0) = SEA_PC_MLP_TR(1) 
      SEA_PCRAY_TR(0)  = SEA_PCRAY_TR(1)
       

      IF (TRACE) THEN
         WRITE (IDLOG,*,ERR=1010) "MARINE PROFILE"
	 WRITE (IDLOG,*,ERR=1010) "**************"
	 WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) " File : ", FICPROFIL_SEA
	 WRITE (IDLOG,*,ERR=1010) " " 
	 WRITE (IDLOG,*,ERR=1010) 
     &	 " Adjustement to the hydrosols phase function truncation: " 
	 WRITE (IDLOG,*,ERR=1010) " "
	 
	 IF (LTMLP) THEN
	    WRITE (IDLOG,*,ERR=1010) 
     &	    " Simulation without mineral-like particles"
	 ELSE
	    WRITE (IDLOG,*,ERR=1010) " Mineral-like particles :"
	    WRITE (IDLOG,*,ERR=1010) 
     &	     "   -- Truncation coef (A) : ",COEF_TRONCA_MLP
	    WRITE (IDLOG,*,ERR=1010) 
     &	     "   -- Single scatt. albedo - adjusted to the trunca.: ",
     &       PIZTR_MLP
            WRITE (IDLOG,*,ERR=1010) 
     &	     "   -- Single scattering albedo : ",PIZ_MLP
            WRITE (IDLOG,*,ERR=1010) " "
	 ENDIF
	 
	 
	 IF (LTCHL) THEN
	    WRITE (IDLOG,*,ERR=1010) " Simulation without phytoplankton"
	 ELSE
	    WRITE (IDLOG,*,ERR=1010) " Phytoplankton :"
	    WRITE (IDLOG,*,ERR=1010) 
     &	       "   -- Truncation coef (A) : ",COEF_TRONCA_CHL
	    WRITE (IDLOG,*,ERR=1010) 
     &	       "Single scatt. albedo - adjusted to the trunca. : ",
     &         PIZTR_CHL
            WRITE (IDLOG,*,ERR=1010) 
     &	       "   -- Single scattering albedo : ",PIZ_CHL
            WRITE (IDLOG,*,ERR=1010) " "
	 ENDIF
	 
	 WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) 
     &	 "            Z(m)  TAUEXT   TAUEXT_TR  PCRAY     "  
     &   //"PCRAY_TR  PC_CHL   PC_CHL_TR  PC_MLP   PC_MLP_TR"
	 DO I=0,CTE_NT_SEA
            WRITE(IDLOG,81,ERR=1010) I, SEA_Z(I),
     &	    			     SEA_TAUEXT(I), SEA_TAUEXT_TR(I),
     &	    			     SEA_PCRAY(I),  SEA_PCRAY_TR(I),
     &	    			     SEA_PC_CHL(I), SEA_PC_CHL_TR(I),
     &	    			     SEA_PC_MLP(I), SEA_PC_MLP_TR(I)
         ENDDO
	 WRITE (IDLOG,*,ERR=1010) " " 
	 WRITE (IDLOG,*,ERR=1010) " "  	
      ENDIF !Fichier Trace
      



C* Passage des altitudes et profondeurs des profils atmosphériques et marin
C* dans un unique tableau  
C  -------------------------------------------------       
	 DO I=0,CTE_NT_ATM
            PROFIL_Z(I)=ATM_Z(I)   
         ENDDO
         DO I=0,CTE_NT_SEA
            PROFIL_Z(I+CTE_NT_ATM+1)=-SEA_Z(I)   
         ENDDO   

      IF (TRACE) THEN
         WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) "GLOBAL PROFILE"
	 WRITE (IDLOG,*,ERR=1010) "**************"
	 WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) 
     &	 " Level, Altitude/Depth, Truncated Opt. Thick."
	 IPROF=0
	 DO I=0,CTE_NT_ATM
            WRITE (IDLOG,90,ERR=1010) IPROF,PROFIL_Z(IPROF),
     &	                              ATM_TAUEXT_TR(I) 
	    IPROF=IPROF+1
         ENDDO
	 WRITE(IDLOG,*,ERR=1010) "Sea / air interface"
	 DO I=0,CTE_NT_SEA  
	    WRITE (IDLOG,90,ERR=1010) IPROF,PROFIL_Z(IPROF),
     &	                  (ATM_TAUEXT_TR(CTE_NT_ATM)+SEA_TAUEXT_TR(I)) 
	    IPROF=IPROF+1
         ENDDO
	 WRITE (IDLOG,*,ERR=1010) " " 
	 WRITE (IDLOG,*,ERR=1010) " "  	
      ENDIF !Fichier Trace

C* Ordre maximal des développements en séries de Fourier
      IBORM = OS_NB
      if ((LTAER).AND.(LTCHL).AND.(LTMLP)) IBORM = 2	!Purement moléculaire air & mer

      IF (TRACE) THEN
         WRITE (IDLOG,*,ERR=1010) 
     &	 "MAX ORDER OF FOURIER SERIES EXPANSION"
	 WRITE (IDLOG,*,ERR=1010) 
     &	 "*************************************"
	 WRITE (IDLOG,*,ERR=1010) " "
	 WRITE (IDLOG,*,ERR=1010) " IBORM : ", IBORM
	 WRITE (IDLOG,*,ERR=1010) " " 
	 WRITE (IDLOG,*,ERR=1010) " "  	
      ENDIF !Fichier Trace
      


      
C* Constitution du nom des fichiers de matrice d'interface
C---------------------------------------------------------------------

      IF (IMAT_SURF.EQ.1) THEN

	 ! Calcul de la longueur de la chaine de caractères 
	 ! pour le nom du répertoire des fichiers SURFACE
         LDIRS = INDEX(DIRS,' ')
         LDIRS = LDIRS - 1      
      
         IF (TRACE) THEN
            WRITE (IDLOG,*,ERR=1010) "BRDF/BPDF SURFACE MATRIX"
	    WRITE (IDLOG,*,ERR=1010) "*************************"
	    WRITE (IDLOG,*,ERR=1010) " "
            WRITE (IDLOG,*,ERR=1010) 
     &	    	   "   Files directory : " , DIRS(1:LDIRS)
         ENDIF !Fichier Trace  
	 

	 ! Détermination du besoin d'extension par TETAS et/ou TETASW
	 ! ==> Si pas angle de gauss ni angle nul (défini dans la liste des angles)
         IF ((GA(N0).EQ.0.D+00).AND.(TETAS.NE.0.D+00)) THEN
             ADD_TETAS_NOMFICS=.TRUE.
         ELSE
             ADD_TETAS_NOMFICS=.FALSE.
         ENDIF 	
        
         IF ((GA(M0).EQ.0.D+00).AND.(TETASW.NE.0.D+00)) THEN
             ADD_TETASW_NOMFICS=.TRUE.
         ELSE
             ADD_TETASW_NOMFICS=.FALSE.
         ENDIF 	  
      
      	 
         ! Constitution du nom du fichier RAA
         ! ----------------------------------   
         CALL OSOAA_NOM_FIC_INTERFACE("RAA",NBMU_GAUSS,OS_NB,
     &	                              TETAS,ADD_TETAS_NOMFICS,
     &				      TETASW,ADD_TETASW_NOMFICS,
     &                                FICANGLES_USER,SEA_IND,WIND,FICS)
	      	
         FICRAA = DIRS(1:LDIRS)//'/'//FICS
         LFICS=INDEX(FICRAA,' ')-1
	 
         IF (TRACE) THEN
            WRITE (IDLOG,*,ERR=1010) 
     &	    	   "   - FICRAA : " , FICRAA(LDIRS+2:LFICS)
         ENDIF !Fichier Trace  


         ! Constitution du nom du fichier TAW
         ! ----------------------------------   
         CALL OSOAA_NOM_FIC_INTERFACE("TAW",NBMU_GAUSS,OS_NB,
     &	                              TETAS,ADD_TETAS_NOMFICS,
     &				      TETASW,ADD_TETASW_NOMFICS,
     &                                FICANGLES_USER,SEA_IND,WIND,FICS)
	      	
         FICTAW = DIRS(1:LDIRS)//'/'//FICS
         LFICS=INDEX(FICTAW,' ')-1
	 
         IF (TRACE) THEN
            WRITE (IDLOG,*,ERR=1010) 
     &	    	   "   - FICTAW : " , FICTAW(LDIRS+2:LFICS)
         ENDIF !Fichier Trace  
      

         ! Constitution du nom du fichier RWW
         ! ----------------------------------   
         CALL OSOAA_NOM_FIC_INTERFACE("RWW",NBMU_GAUSS,OS_NB,
     &	                              TETAS,ADD_TETAS_NOMFICS,
     &				      TETASW,ADD_TETASW_NOMFICS,
     &                                FICANGLES_USER,SEA_IND,WIND,FICS)
	      	
         FICRWW = DIRS(1:LDIRS)//'/'//FICS
         LFICS=INDEX(FICRWW,' ')-1
	 
         IF (TRACE) THEN
            WRITE (IDLOG,*,ERR=1010) 
     &	    	   "   - FICRWW : " , FICRWW(LDIRS+2:LFICS)
         ENDIF !Fichier Trace  
      
      
         ! Constitution du nom du fichier TWA
         ! ----------------------------------   
         CALL OSOAA_NOM_FIC_INTERFACE("TWA",NBMU_GAUSS,OS_NB,
     &	                              TETAS,ADD_TETAS_NOMFICS,
     &				      TETASW,ADD_TETASW_NOMFICS,
     &                                FICANGLES_USER,SEA_IND,WIND,FICS)
	      	
         FICTWA = DIRS(1:LDIRS)//'/'//FICS
         LFICS=INDEX(FICTWA,' ')-1
	 
         IF (TRACE) THEN
            WRITE (IDLOG,*,ERR=1010) 
     &	    	   "   - FICTWA : " , FICTWA(LDIRS+2:LFICS)
	    WRITE (IDLOG,*,ERR=1010) " "
            
         ENDIF !Fichier Trace  
         
           
      ELSE !Cas mer plate

      
         IF (TRACE) THEN
            WRITE (IDLOG,*,ERR=1010) "CASE : FLAT SEA SIMULATION"
	    WRITE (IDLOG,*,ERR=1010) "**************************"
	    WRITE (IDLOG,*,ERR=1010) " "
         ENDIF !Fichier Trace  
      
      ENDIF  !Fin cas avec matrices de surface
      


     
C* Gestion de l'appel du coeur de simulation du transfert radiatif
C---------------------------------------------------------------------

      IF (TRACE) THEN
            WRITE (IDLOG,*,ERR=1010) " "
	    WRITE (IDLOG,*,ERR=1010) " "
	    WRITE (IDLOG,*,ERR=1010) "********************************"
	    WRITE (IDLOG,*,ERR=1010) "SUCCESSIVE ORDERS CALCULATIONS  "
	    WRITE (IDLOG,*,ERR=1010) "********************************"
	    WRITE (IDLOG,*,ERR=1010) " "
	    WRITE (IDLOG,*,ERR=1010) " "
      ENDIF !Fichier Trace  


      IF (APPEL_SOS_CORE) THEN
      
         IF (TRACE) THEN
	    WRITE (IDLOG,*,ERR=1010) 
     &      "  --> Start of OSOAA_SOS_CORE"
	    WRITE (IDLOG,*,ERR=1010) " " 
         ENDIF !Fichier Trace  
        
         CALL OSOAA_SOS_CORE(N0,M0,NBMU,RMU,GA,OS_NB,
     &			     IMAT_SURF,FICRAA,FICTAW,FICRWW,FICTWA,
     &			     SEA_IND,RHO_SURF,RHO_BOT,
     &			     ATM_TAUEXT_TR,ATM_PCAER_TR,ATM_PCRAY_TR,
     &			     SEA_TAUEXT_TR,SEA_PCRAY_TR,
     &			     SEA_PC_CHL_TR,SEA_PC_MLP_TR,
     &   		     ALPHA_AER,BETA_AER,GAMMA_AER,ZETA_AER, 
     &	 		     ALPHA_CHL,BETA_CHL,GAMMA_CHL,ZETA_CHL,
     &	 		     ALPHA_MLP,BETA_MLP,GAMMA_MLP,ZETA_MLP,
     &			     IGMAX,IBORM,
     &                       PROFIL_Z, FIC_FLUX,
     &                       FICSOS_LOG,FICSOS_RES_BIN,IER)
     
         IF (IER.NE.0) GOTO 1060   ! cas d'erreur
	 
      ELSE

         IF (TRACE) THEN
	    WRITE (IDLOG,*,ERR=1010) 
     &	      " File FICSOS_RES_BIN has already been calculated :",
     &         FICSOS_RES_BIN
	    WRITE (IDLOG,*,ERR=1010) 
     &	      "  --> No OSOAA_SOS_CORE calculations" 
	    WRITE (IDLOG,*,ERR=1010) " " 
         ENDIF !Fichier Trace  
	       
      ENDIF !Test sur APPEL_SOS_CORE
      

C* Récupération du profil de flux calculé par OSOAA_SOS_CORE
C-----------------------------------------------------------
      OPEN(UNIT=11,FILE=FIC_FLUX,ERR=1300) 
      DO I=1,5
         READ(11,*,ERR=1301) 
      ENDDO
      DO I=0,NT_TOT
	 READ(11,600,ERR=1301) VALUE_INT,VALUE_DBLE,VALUE_DBLE,
     &                         VALUE_DBLE,PROFIL_FLUX_DOWN_TOT(I),
     &                         VALUE_DBLE,VALUE_DBLE,VALUE_DBLE
      ENDDO
      CLOSE(11)


C* Recombinaison sur l'azimut de visée
C------------------------------------- 

      IF (TRACE) THEN
          WRITE(IDLOG,*,ERR=1010) 
	  WRITE(IDLOG,*,ERR=1010) 
     &	     "********************************************************"
	  WRITE(IDLOG,*,ERR=1010) 
     &	      "RECOMPOSITION OF THE RADIANCE FIELD FOR A GIVEN AZIMUTH"
	  WRITE(IDLOG,*,ERR=1010) 
     &	      "*******************************************************"
	  WRITE(IDLOG,*,ERR=1010) 
	  WRITE(IDLOG,*,ERR=1010) "   Azimuth (degrees) : ",PHI
	  WRITE(IDLOG,*,ERR=1010) 	  
      ENDIF !TRACE
           
      PHI1 = PHI * INCTE_PI / 180.D+00
      CALL OSOAA_TRPHI(FICSOS_RES_BIN,NBMU,RMU,PHI1,
     &                 IMAT_SURF,N0,M0,WIND,SEA_IND,ATM_TAUEXT, 
     &                 SEA_TAUEXT,ANGDIFF_PHI1,			 
     &                 XIT_PHI1, XQT_PHI1, XUT_PHI1, IER)
      IF (IER.NE.0) GOTO 1070   ! cas d'erreur  

	
      PHI2 = PHI1 + INCTE_PI 
      CALL OSOAA_TRPHI(FICSOS_RES_BIN,NBMU,RMU,PHI2,
     &                 IMAT_SURF,N0,M0,WIND,SEA_IND,ATM_TAUEXT, 	
     &                 SEA_TAUEXT,ANGDIFF_PHI2,		 		
     &                 XIT_PHI2, XQT_PHI2, XUT_PHI2, IER)
      IF (IER.NE.0) GOTO 1070   ! cas d'erreur  

	
C **************************************************************
C --- Création du fichier FICSOS_RES_ADV_UP donnant le champ
C     de rayonnement sur tout le profil (marin et atmosphérique)
C     pour toutes les directions de propagation montantes.
C **************************************************************
      IF (TRACE) THEN
      		
	 IF (FICSOS_RES_ADV_UP.NE."NO_ADVANCED_FILE") THEN	
	    WRITE (IDLOG,*,ERR=1010) 
     &	    "  --> Creation of file Advanced Up : ",
     & 	    FICSOS_RES_ADV_UP
	 ELSE
	    WRITE (IDLOG,*,ERR=1010) 
     &	    "  --> No Advanced Up file to produce"
	 ENDIF
	    
	 IF (FICSOS_RES_ADV_DOWN.NE."NO_ADVANCED_FILE") THEN
	    WRITE (IDLOG,*,ERR=1010) 
     &	    "  --> Creation of file Advanced Down : ",
     & 	    FICSOS_RES_ADV_DOWN
         ELSE
	    WRITE (IDLOG,*,ERR=1010) 
     &	    "  --> No Advanced Down file to produce"
	 ENDIF
	    
	 WRITE (IDLOG,*,ERR=1010) " " 
	 
      ENDIF !Fichier Trace  


      IF (FICSOS_RES_ADV_UP.NE."NO_ADVANCED_FILE") THEN
         
	  OPEN(10,FILE=FICSOS_RES_ADV_UP,FORM='FORMATTED',ERR=1100)

         WRITE(10,*) 'ADVANCED RESULTS : UPWARD RADIANCE FIELD'
	  WRITE(10,*) '           VERSUS ALTITUDE / DEPTH'
	  WRITE(10,*) '           AND VERSUS THE VIEWING ZENITH ANGLE'
	  WRITE(10,*) '           (RELATIVE AZIMUTH IS FIXED)'
	  WRITE(10,*) '---------------------------------------------'//
     &    '---------------------------------------------------------'//
     &    '----------'	
	  
	  WRITE(10,*) 'Relative azimuth (degrees) :'
         WRITE(10,*) ' '
         WRITE(10,*) '   Relative azimuth convention : '
         WRITE(10,*) 
     &     '       180° <-> Satellite and Sun in the same half-plane'
         WRITE(10,*) 
     &     '         0° <-> Satellite and Sun in opposite half-planes'//
     &     ' with respect to the zenith direction'
         WRITE(10,*) ' '
         WRITE(10,*) '   Simulated relative azimuth (degrees) :'
         WRITE(10,*) '       for VZA < 0 (sign convention):',
     &	                  PHI2*180./INCTE_PI
         WRITE(10,*) '       for VZA > 0 (sign convention):',
     &	                  PHI1*180./INCTE_PI
        
	  WRITE(10,*) ' '
	  WRITE(10,*) 'Columns parameters :'
	  WRITE(10,*) '  LEVEL   :  Level I of the profile'
	  WRITE(10,*) '               TOA is level 0'
	  WRITE(10,*) '		      0+ (over surface) is level ',
     &	  				 CTE_NT_ATM
	  WRITE(10,*) '		      0- (under surface) is level',
     &	  				 (CTE_NT_ATM+1)
	  WRITE(10,*) '  Z       :  Altitude or depth (meters)'
	  WRITE(10,*) '  VZA     :  Viewing Zenith Angle (deg)'
	  WRITE(10,*) '  SCA_ANG :  Scattering angle (deg)'
	  WRITE(10,*)  
     &	    '  I, Q, U :  Stokes parameters at level Z (in sr-1)'
         WRITE(10,*) 
     &	    '             normalised to the extraterrestrial solar'// 
     &     ' irradiance (PI * L(z) / Esun)'
	  WRITE(10,*) '  POL_ANG :  Polarization angle (deg)'
	  WRITE(10,*) '  POL_RATE:  Degree of polarization (%)'
	  WRITE(10,*) 
     &	    '  LPOL    :  Polarized intensity at level Z (in sr-1)'
         WRITE(10,*) 
     &	    '             normalised to the extraterrestrial solar'// 
     &      ' irradiance (PI * Lpol(z) / Esun)'
	  WRITE(10,*) '---------------------------------------------' //
     &    '---------------------------------------------------------' //
     &    '----------'		   
	  
	  
	  WRITE(10,*) ' LEVEL    Z         VZA     SCA_ANG'//
     &                '       I              Q              U'//
     &                '         POL_ANG POL_RATE    LPOL'
	  
    
	  
	  DO I = 0,NT_TOT
	  
	     Z_I=PROFIL_Z(I)
	      
             ! Demi plan PHIos + 180 degrés : Angles de visée négatifs par convention
             !------------------------------------------------------------------------ 	    
             DO J=NBMU,1,-1
	     
	        IF (IND_ANGOUT(J).EQ.1) THEN
                   TETA=DACOS(RMU(J))*180.D+00/INCTE_PI
		   ANGDIFF = ANGDIFF_PHI2(I,J)
		
		   XI = XIT_PHI2(I,J)
		   XQ = XQT_PHI2(I,J)
		   XU = XUT_PHI2(I,J)
		
		   CALL OSOAA_POLAR(XI,XQ,XU,XAN,TPOL,LPOL)
     
	           !Enregistrement du champ montant
                   WRITE(10,700,ERR=1101)
     &		         I,Z_I,-TETA,ANGDIFF,XI,XQ,XU,XAN,TPOL,LPOL
	        ENDIF
             ENDDO !Boucle sur directions 


             ! Demi plan PHIos  : Angles de visée positifs par convention
             !------------------------------------------------------------------------ 	    
             DO J=1,NBMU
	     
	        IF (IND_ANGOUT(J).EQ.1) THEN
                   TETA=DACOS(RMU(J))*180.D+00/INCTE_PI
		   ANGDIFF = ANGDIFF_PHI1(I,J)
		
		   XI = XIT_PHI1(I,J)
		   XQ = XQT_PHI1(I,J)
		   XU = XUT_PHI1(I,J)
		
		   CALL OSOAA_POLAR(XI,XQ,XU,XAN,TPOL,LPOL)
     
	           !Enregistrement du champ montant
                   WRITE(10,700,ERR=1101)
     &		         I,Z_I,TETA,ANGDIFF,XI,XQ,XU,XAN,TPOL,LPOL
                ENDIF
		
             ENDDO !Boucle sur directions             
	  ENDDO	 !Boucle sur niveaux marins
	   
	   
	  CLOSE(10) !Fermeture du fichier FICSOS_RES_ADV_UP
      ENDIF   


C --- Création du fichier FICSOS_RES_ADV_DOWN donnant le champ
C     de rayonnement sur tout le profil (marin et atmosphérique)
C     pour toutes les directions de propagation descendantes.
      IF (FICSOS_RES_ADV_DOWN.NE."NO_ADVANCED_FILE") THEN
           
	  OPEN(10,FILE=FICSOS_RES_ADV_DOWN,FORM='FORMATTED',ERR=1100)

          WRITE(10,*) 'ADVANCED RESULTS : DOWNWARD RADIANCE FIELD'
	  WRITE(10,*) '           VERSUS ALTITUDE / DEPTH'
	  WRITE(10,*) '           AND VERSUS THE VIEWING ZENITH ANGLE'
	  WRITE(10,*) '           (RELATIVE AZIMUTH IS FIXED)'
	  WRITE(10,*) '---------------------------------------------'//
     &    '---------------------------------------------------------'//
     &    '----------'	 
	  	  
	  WRITE(10,*) 'Relative azimuth (degrees) :'
         WRITE(10,*) ' '
         WRITE(10,*) '   Relative azimuth convention : '
         WRITE(10,*) 
     &     '       180° <-> Satellite and Sun in the same half-plane'
         WRITE(10,*) 
     &     '         0° <-> Satellite and Sun in opposite half-planes'//
     &     ' with respect to the zenith direction'
         WRITE(10,*) ' '
         WRITE(10,*) '   Simulated relative azimuth (degrees) :'
         WRITE(10,*) '       for VZA < 0 (sign convention):',
     &	                  PHI2*180./INCTE_PI
         WRITE(10,*) '       for VZA > 0 (sign convention):',
     &	                  PHI1*180./INCTE_PI
     
	  WRITE(10,*) ' '	  
	  WRITE(10,*) 'Columns parameters :'
	  WRITE(10,*) '  LEVEL   :  Level I of the profile'
	  WRITE(10,*) '               TOA is level 0'
	  WRITE(10,*) '		      0+ (over surface) is level ',
     &	  				 CTE_NT_ATM
	  WRITE(10,*) '		      0- (under surface) is level',
     &	  				 (CTE_NT_ATM+1)
	  WRITE(10,*) '  Z       :  Altitude or depth (meters)'
	  WRITE(10,*) '  VZA     :  Viewing Zenith Angle (deg)'
	  WRITE(10,*) '  SCA_ANG :  Scattering angle (deg)'
	  WRITE(10,*) 
     &	     '  I, Q, U :  Stokes parameters at level Z (in sr-1)'
         WRITE(10,*) 
     &	    '             normalised to the extraterrestrial solar'// 
     &     ' irradiance (PI * L(z) / Esun)'
	  WRITE(10,*) '  POL_ANG :  Polarization angle (deg)'
	  WRITE(10,*) '  POL_RATE:  Degree of polarization (%)'
	  WRITE(10,*)  
     &	    '  LPOL    :  Polarized intensity at level Z (in sr-1)'
         WRITE(10,*) 
     &	    '             normalised to the extraterrestrial solar'// 
     &      ' irradiance (PI * Lpol(z) / Esun)'
	  WRITE(10,*) '---------------------------------------------' //
     &   '---------------------------------------------------------' //
     &   '----------'	    
	  
	  
	  WRITE(10,*) ' LEVEL    Z         VZA     SCA_ANG'//
     &                '       I              Q              U'//
     &                '         POL_ANG POL_RATE    LPOL'
     
     
	  DO I = 0,NT_TOT
	  
	     Z_I=PROFIL_Z(I)
	     	      
             ! Demi plan PHIos + 180 degrés : Angles de visée négatifs par convention
             !------------------------------------------------------------------------ 	    
             DO J=NBMU,1,-1
	        
                IF (IND_ANGOUT(J).EQ.1) THEN
		   TETA=DACOS(RMU(J))*180.D+00/INCTE_PI
		   ANGDIFF = ANGDIFF_PHI2(I,-J)
		
		   XI = XIT_PHI2(I,-J)
		   XQ = XQT_PHI2(I,-J)
		   XU = XUT_PHI2(I,-J)
		
		   CALL OSOAA_POLAR(XI,XQ,XU,XAN,TPOL,LPOL)
     
	           !Enregistrement du champ descendant
                   WRITE(10,700,ERR=1101)
     &		         I,Z_I,-TETA,ANGDIFF,XI,XQ,XU,XAN,TPOL,LPOL
	        ENDIF
             ENDDO !Boucle sur directions 


             ! Demi plan PHIos  : Angles de visée positifs par convention
             !------------------------------------------------------------------------ 	    
             DO J=1,NBMU
	        
                IF (IND_ANGOUT(J).EQ.1) THEN
		   TETA=DACOS(RMU(J))*180.D+00/INCTE_PI
		   ANGDIFF = ANGDIFF_PHI1(I,-J)
		
		   XI = XIT_PHI1(I,-J)
		   XQ = XQT_PHI1(I,-J)
		   XU = XUT_PHI1(I,-J)
		
		   CALL OSOAA_POLAR(XI,XQ,XU,XAN,TPOL,LPOL)
     
	           !Enregistrement du champ descendant
                   WRITE(10,700,ERR=1101)
     &		         I,Z_I,TETA,ANGDIFF,XI,XQ,XU,XAN,TPOL,LPOL
                ENDIF
		
             ENDDO !Boucle sur directions             
	  ENDDO	 !Boucle sur niveaux marins
	   
	   
	  CLOSE(10) !Fermeture du fichier FICSOS_RES_ADV_DOWN
      ENDIF   

   
  
   
   
C ************************************************************   
C --- Création du fichier FICSOS_RES_VS_VZA donnant le champ
C     de rayonnement à une profondeur ou à une altitude donnée
C     pour les directions de propagation montantes.   
C ************************************************************
      
      !Gestion de l'altitude ou profondeur du profil (Z_OUT) selon
      !le cas de type de niveau de sortie
      IF (LEVEL_OUT.EQ.1) Z_OUT = PROFIL_Z(0)		 !TOA
      IF (LEVEL_OUT.EQ.2) Z_OUT = PROFIL_Z(NT_TOT)	 !Sea bottom
      IF (LEVEL_OUT.EQ.3) Z_OUT = PROFIL_Z(CTE_NT_ATM)	 !Sea surface 0+
      IF (LEVEL_OUT.EQ.4) Z_OUT = PROFIL_Z(CTE_NT_ATM+1) !Sea surface 0-
      				!==Juste sous la surface.
      IF (LEVEL_OUT.EQ.5) Z_OUT = USER_Z_OUT		 !user's value
 
 
      !Copie de l'altitude/profondeur de sortie requise par l'utilisateur
      !(valeur utilisateur ou option choix TOA/fond, Surface 0+/O-)
      USER_Z_OUT = Z_OUT
	   
	   
      IF (TRACE) THEN
          WRITE (IDLOG,*,ERR=1010) " "
	  WRITE (IDLOG,*,ERR=1010) 
     &	    "  --> Creation of the Standard Up file for a given Z : ",
     & 	    FICSOS_RES_VS_VZA
          WRITE (IDLOG,*,ERR=1010) " "
	  IF (LEVEL_OUT.EQ.1) THEN
	     WRITE(IDLOG,*,ERR=1010) "      Output profile level : TOA" 
	  ENDIF
	  IF (LEVEL_OUT.EQ.2) THEN
	     WRITE(IDLOG,*,ERR=1010) 
     &	        "      Output profile level : Sea bottom"   
	  ENDIF	  
	  IF (LEVEL_OUT.EQ.3) THEN
	     WRITE(IDLOG,*,ERR=1010) 
     &	        "      Output profile level : Sea surface 0+"   
	  ENDIF	  
	  IF (LEVEL_OUT.EQ.4) THEN
	     WRITE(IDLOG,*,ERR=1010) 
     &	        "      Output profile level : Sea surface 0-"   
	  ENDIF	  
	  IF (LEVEL_OUT.EQ.5) THEN
	     WRITE(IDLOG,*,ERR=1010) 
     &	        "      Output profile level : User's definition"   
	  ENDIF	  	  
	  
          WRITE (IDLOG,*,ERR=1010) 
     &	    "      Output level of required results (m) : ", Z_OUT
          WRITE (IDLOG,*,ERR=1010) " "
      ENDIF ! Fin Trace
      
         
      ! Interpolation pour le niveau sollicité
      !------------------------------------------------------------------------       
       
      ! Cas Z_OUT supérieur à l'altitude max
      IF (Z_OUT.GT.PROFIL_Z(0)) THEN
      
          !Ajustement de Z_OUT à l'altitude max
	  Z_OUT = PROFIL_Z(0)
	   
	  IF (TRACE) THEN
              WRITE (IDLOG,*,ERR=1010) 
     &	      "      Output altitude > max value: ", PROFIL_Z(0)
              WRITE (IDLOG,*,ERR=1010) 
     &	      "      ==> Output altitude (m) becomes : ",PROFIL_Z(0)
              WRITE (IDLOG,*,ERR=1010) " "
          ENDIF ! Fin Trace
	   
      ENDIF 
       
      ! Cas Z_OUT inférieur à la profondeur max
      IF (Z_OUT.LT.PROFIL_Z(NT_TOT)) THEN
      
        !Ajustement de Z_OUT à la profondeur max
          Z_OUT = PROFIL_Z(NT_TOT)
	   
	  IF (TRACE) THEN
              WRITE (IDLOG,*,ERR=1010) 
     &	      "      Output depth > max value : ", 
     &        PROFIL_Z(NT_TOT)
              WRITE (IDLOG,*,ERR=1010) 
     &	      "      ==> Output depth (m) becomes : ", 
     &        PROFIL_Z(NT_TOT)
              WRITE (IDLOG,*,ERR=1010) " "
          ENDIF ! Fin Trace
	   
      ENDIF 
       
      ! Estimation des niveaux Z1 et Z2 encadrant Z_OUT
      IF ((LEVEL_OUT.GE.1).AND.(LEVEL_OUT.LE.4)) THEN
         !Cas 0+ et 0 - : gestion spécifique car tous deux à Z=0
	 
	 
	 IF (LEVEL_OUT.EQ.1) THEN    !Sortie TOA
	    I1 = 0
	 ENDIF
	 
	 IF (LEVEL_OUT.EQ.2) THEN    !Sortie fond mer
	    I1 = NT_TOT 
	 ENDIF
	 
	 IF (LEVEL_OUT.EQ.3) THEN    !Sortie niveau 0+
	    I1 = CTE_NT_ATM
	 ENDIF
	 
	 IF (LEVEL_OUT.EQ.4) THEN    !Sortie niveau 0-
	    I1 = CTE_NT_ATM + 1	     
	 ENDIF
	 
	 I2 = I1
	 
      ELSE
         !Cas LEVEL_OUT = 5 (Z_OUT utilisateur)
      
         !Recherche si Z_OUT correspond à une valeur du profil
         LRECH_OK=.FALSE.
	 I=0
         DO WHILE (I.LE.NT_TOT)
	     IF (Z_OUT.EQ.PROFIL_Z(I)) THEN
	        I1=I
	        I2=I  
		LRECH_OK=.TRUE.
		I=NT_TOT
	     ENDIF 
	     I = I+1
         ENDDO
	 
	 !Recherche des niveaux du profil encadrant Z_OUT 
	 IF (LRECH_OK.EQV..FALSE.) THEN
	    I=0
            DO WHILE (Z_OUT.LT.PROFIL_Z(I)) 
	        I = I+1
            ENDDO
            I1 = I-1
            I2 = I
         ENDIF
	 
      ENDIF ! Fin test sur LEVEL_OUT 
      Z1 = PROFIL_Z(I1)
      Z2 = PROFIL_Z(I2)
      
      IF (TRACE) THEN
         WRITE (IDLOG,*,ERR=1010) 
     &	 "      upper level I Z(m) : ", I1, Z1
         WRITE (IDLOG,*,ERR=1010) 
     &	 "      Lower level I Z(m) : ", I2, Z2
      ENDIF ! Fin Trace
 
      
      OPEN(10,FILE=FICSOS_RES_VS_VZA,FORM='FORMATTED',ERR=1200)   

      WRITE(10,*) 'STANDARD RESULTS : '
      WRITE(10,*) '  UPWARD RADIANCE FIELD VERSUS THE'//
     &            ' VIEWING ZENITH ANGLE'
      WRITE(10,*) '  (RELATIVE AZIMUTH AND ALTITUDE/DEPTH ARE FIXED)'
      WRITE(10,*) '----------------------------------------------'//
     &'-------------------------------------------------'   
      WRITE(10,*) 'Relative azimuth (degrees) :'
      WRITE(10,*) ' '
      WRITE(10,*) '   Relative azimuth convention : '
      WRITE(10,*) 
     &   '       180° <-> Satellite and Sun in the same half-plane'
      WRITE(10,*) 
     &   '         0° <-> Satellite and Sun in opposite half-planes'//
     &   ' with respect to the zenith direction'
      WRITE(10,*) ' '
      WRITE(10,*) '   Simulated relative azimuth (degrees) :'
      WRITE(10,*) '       for VZA < 0 (sign convention):',
     &	                  PHI2*180./INCTE_PI
      WRITE(10,*) '       for VZA > 0 (sign convention):',
     &	                  PHI1*180./INCTE_PI
      WRITE(10,*) ' '
      
        
     
      !TOA
      IF (LEVEL_OUT.EQ.1) THEN
          WRITE(10,*) 'TOA level - Altitude (km) :', Z_OUT/1000.
      ENDIF
      
      !Sea bottom
      IF (LEVEL_OUT.EQ.2) THEN
         WRITE(10,*) 'Sea bottom level - Depth (m) :', Z_OUT,
     &	          ' (limited to the maximum computed depth)'
      ENDIF
      
      !Sea surface 0+
      IF (LEVEL_OUT.EQ.3) THEN
          WRITE(10,*) 'Above sea surface level 0+ - Altitude (km) :', 
     &	              Z_OUT
      ENDIF      
      
      !Sea surface 0-
      IF (LEVEL_OUT.EQ.4) THEN
          WRITE(10,*) 'Under sea surface level 0- - Depth (m) :', Z_OUT
      ENDIF 
      
      !Z user's value
      IF (LEVEL_OUT.EQ.5) THEN
             
	 IF (Z_OUT.GE.0.) THEN !Dans atmosphère
      
           IF (Z_OUT.NE.USER_Z_OUT) THEN
	       WRITE(10,*) 'Altitude (km) :', USER_Z_OUT/1000.,
     &	                   ' limited to the maximum altitude', 
     &                     Z_OUT/1000.
	    ELSE
	       WRITE(10,*) 'Altitude (km) :', Z_OUT/1000.
	    ENDIF
          
         ELSE !Dans mer
      
            IF (Z_OUT.NE.USER_Z_OUT) THEN !Cas d'ajustement de la profondeur 
	    				  !de sortie par le code
	       WRITE(10,*)  
     &	         'Requested value of the depth for the output(m) :',  
     &	         USER_Z_OUT,
     &	         ' ==> limited to the max computed depth :', Z_OUT	        
	    ELSE !Cas nominal
	       WRITE(10,*) 
     &	         'Value of the depth selected for the output (m) :', 
     &	         Z_OUT
	    ENDIF
	       
         ENDIF !Fin test sur Z_OUT
	 
      ENDIF !Fin LEVEL_OUT = 5
    
      
      WRITE(10,*) ' '
      WRITE(10,*) 'Columns parameters :'
      WRITE(10,*) '  VZA     :  Viewing Zenith Angle (deg)'
      WRITE(10,*) '  SCA_ANG :  Scattering angle (deg)'
      WRITE(10,*) 
     &	 '  I       :  Stokes parameter at output level Z (in sr-1)'
      WRITE(10,*) 
     &	 '             normalised to the extraterrestrial solar'//
     &   ' irradiance (PI * L(z) / Esun)'
      WRITE(10,*) 
     &  '  REFL    :  Reflectance at output level Z (PI * L(z) / Ed(z))'
      WRITE(10,*) '  POL_RATE:  Degree of polarization (%)'
      WRITE(10,*) 
     &	 '  LPOL    :  Polarized intensity at output level Z (in sr-1)'
      WRITE(10,*) 
     &	 '             normalised to the extraterrestrial solar'//
     &   ' irradiance (PI * Lpol(z) / Esun)'
      
      WRITE(10,*) 
     &   '  REFL_POL:  Polarized reflectance at output level Z'//
     &   ' (PI * Lpol(z) / Ed(z))'

      WRITE(10,*) '----------------------------------------------'//
     &'------------------------------------------------'   
      WRITE(10,*) '  VZA    SCA_ANG       I           REFL     '//
     &            '   POL_RATE     LPOL        REFL_POL'
     
     
        
      DZ = Z2-Z1
      
      IF (DZ.NE.0.D+00) THEN
      
          !Interpolation du flux descendant (normalisé par Es = PI) pour le niveau Z
          COEF_A = PROFIL_FLUX_DOWN_TOT(I2)-PROFIL_FLUX_DOWN_TOT(I1) 
	  COEF_A = COEF_A / DZ
	  COEF_B = Z2*PROFIL_FLUX_DOWN_TOT(I1) 
	  COEF_B = COEF_B -Z1*PROFIL_FLUX_DOWN_TOT(I2)
	  COEF_B = COEF_B / DZ	
 	  FLUX_DOWN_NORMA = COEF_A*Z_OUT+COEF_B  ! Flux descendant total (direct + diffus) 
	  					 ! normalisé par Es = PI
			
					
         ! Demi plan PHIos + 180 degrés : Angles de visée négatifs par convention
         !------------------------------------------------------------------------ 	    
         DO J=NBMU,1,-1
	    
	    IF (IND_ANGOUT(J).EQ.1) THEN
	    
               TETA=DACOS(RMU(J))*180.D+00/INCTE_PI
	
	       COEF_A = (ANGDIFF_PHI2(I2,J)-ANGDIFF_PHI2(I1,J)) / DZ
	       COEF_B = (Z2*ANGDIFF_PHI2(I1,J)-Z1*ANGDIFF_PHI2(I2,J))
	       COEF_B = COEF_B / DZ		
 	       ANGDIFF = COEF_A*Z_OUT+COEF_B
	 
	       COEF_A = (XIT_PHI2(I2,J)-XIT_PHI2(I1,J)) / DZ
	       COEF_B = (Z2*XIT_PHI2(I1,J)-Z1*XIT_PHI2(I2,J)) / DZ	
 	       XI = COEF_A*Z_OUT+COEF_B
	 
	       COEF_A = (XQT_PHI2(I2,J)-XQT_PHI2(I1,J)) / DZ
	       COEF_B = (Z2*XQT_PHI2(I1,J)-Z1*XQT_PHI2(I2,J)) / DZ
 	       XQ = COEF_A*Z_OUT+COEF_B
	 
	       COEF_A = (XUT_PHI2(I2,J)-XUT_PHI2(I1,J)) / DZ
	       COEF_B = (Z2*XUT_PHI2(I1,J)-Z1*XUT_PHI2(I2,J)) / DZ
 	       XU = COEF_A*Z_OUT+COEF_B
		
	       CALL OSOAA_POLAR(XI,XQ,XU,XAN,TPOL,LPOL)
     
               !Calcul de la réflectance et réflectance polarisée
	       REFL = XI/FLUX_DOWN_NORMA
	       REFL_POL = LPOL/FLUX_DOWN_NORMA

	       !Enregistrement du champ montant
               WRITE(10,800,ERR=1201) -TETA,ANGDIFF,XI,REFL,TPOL,LPOL,
     &	                               REFL_POL
	    ENDIF !Fin test si angle de sortie
         ENDDO !Boucle sur directions 


         ! Demi plan PHIos  : Angles de visée positifs par convention
         !------------------------------------------------------------------------ 	    
         DO J=1,NBMU
	    
	    IF (IND_ANGOUT(J).EQ.1) THEN
	       
	       TETA=DACOS(RMU(J))*180.D+00/INCTE_PI
	
	       COEF_A = (ANGDIFF_PHI1(I2,J)-ANGDIFF_PHI1(I1,J)) / DZ 
	       COEF_B = (Z2*ANGDIFF_PHI1(I1,J)-Z1*ANGDIFF_PHI1(I2,J)) 
	       COEF_B = COEF_B / DZ	
 	       ANGDIFF = COEF_A*Z_OUT+COEF_B
	 	
	       COEF_A = (XIT_PHI1(I2,J)-XIT_PHI1(I1,J)) / DZ 
	       COEF_B = (Z2*XIT_PHI1(I1,J)-Z1*XIT_PHI1(I2,J)) / DZ	
 	       XI = COEF_A*Z_OUT+COEF_B
	 
	       COEF_A = (XQT_PHI1(I2,J)-XQT_PHI1(I1,J)) / DZ
	       COEF_B = (Z2*XQT_PHI1(I1,J)-Z1*XQT_PHI1(I2,J)) / DZ
 	       XQ = COEF_A*Z_OUT+COEF_B
	 
	       COEF_A = (XUT_PHI1(I2,J)-XUT_PHI1(I1,J)) / DZ
	       COEF_B = (Z2*XUT_PHI1(I1,J)-Z1*XUT_PHI1(I2,J)) / DZ
 	       XU = COEF_A*Z_OUT+COEF_B
		
	       CALL OSOAA_POLAR(XI,XQ,XU,XAN,TPOL,LPOL)

               !Calcul de la réflectance et réflectance polarisée	
 	       REFL = XI/FLUX_DOWN_NORMA
	       REFL_POL = LPOL/FLUX_DOWN_NORMA
 
	       !Enregistrement du champ montant
               WRITE(10,800,ERR=1201) TETA,ANGDIFF,XI,REFL,TPOL,LPOL,
     &	                              REFL_POL
	    ENDIF !Fin test si angle de sortie
	    
         ENDDO !Boucle sur directions   


      ELSE !Cas DZ = 0. (peut apparaitre juste sous la surface : 
           !             cas de limitation affichage numérique de la profondeur
	   !             dans le fichier de profil marin)
	   ! ==> Dans ce cas, le niveau est I1 = CTE_NT_ATM + 1


         !Flux descendant total (normalisé par Es = PI) pour le niveau Z
	 FLUX_DOWN_NORMA = PROFIL_FLUX_DOWN_TOT(I1)
				       
         ! Demi plan PHIos + 180 degrés : Angles de visée négatifs par convention
         !------------------------------------------------------------------------ 	    
         DO J=NBMU,1,-1

	    IF (IND_ANGOUT(J).EQ.1) THEN   

	       TETA=DACOS(RMU(J))*180.D+00/INCTE_PI

 	       ANGDIFF = ANGDIFF_PHI2(I1,J)
 	       XI = XIT_PHI2(I1,J)
 	       XQ = XQT_PHI2(I1,J)
 	       XU = XUT_PHI2(I1,J)
	  	
	       CALL OSOAA_POLAR(XI,XQ,XU,XAN,TPOL,LPOL)

               !Calcul de la réflectance et réflectance polarisée
	       REFL = XI/FLUX_DOWN_NORMA
	       REFL_POL = LPOL/FLUX_DOWN_NORMA
	    
	       !Enregistrement du champ montant
               WRITE(10,800,ERR=1201) -TETA,ANGDIFF,XI,REFL,TPOL,LPOL,
     &	                               REFL_POL
	   
	    ENDIF !Fin test si angle de sortie
    
         ENDDO !Boucle sur directions 


         ! Demi plan PHIos  : Angles de visée positifs par convention
         !------------------------------------------------------------------------ 	    
         DO J=1,NBMU
	 
	    IF (IND_ANGOUT(J).EQ.1) THEN   

               TETA=DACOS(RMU(J))*180.D+00/INCTE_PI
	
 	       ANGDIFF = ANGDIFF_PHI1(I1,J)
 	       XI = XIT_PHI1(I1,J)
 	       XQ = XQT_PHI1(I1,J)
 	       XU = XUT_PHI1(I1,J)
		
	       CALL OSOAA_POLAR(XI,XQ,XU,XAN,TPOL,LPOL)
 
               !Calcul de la réflectance et réflectance polarisée
 	       REFL = XI/FLUX_DOWN_NORMA
	       REFL_POL = LPOL/FLUX_DOWN_NORMA
     
	       !Enregistrement du champ montant
               WRITE(10,800,ERR=1201) TETA,ANGDIFF,XI,REFL,TPOL,LPOL,
     &	                              REFL_POL     
	    
	    ENDIF !Fin test si angle de sortie
	    
         ENDDO !Boucle sur les directions   
      
      ENDIF ! Fin test DZ = 0.
      
      CLOSE(10) !Fermeture du fichier FICSOS_RES_VS_VZA
      
   
      
     
C ********************************************************   
C --- Création du fichier FICSOS_RES_VS_Z donnant le champ
C     profil marin du rayonnement 
C     pour une direction de propagation donnée.   
C ******************************************************** 
   
      IF (TRACE) THEN
         
	 WRITE (IDLOG,*,ERR=1010) " "
	 
	 IF (FICSOS_RES_VS_Z.NE."NO_RES_VS_Z_FILE") THEN	
	   WRITE (IDLOG,*,ERR=1010) 
     &	   "  --> Creation of file providing the standard marine"
           WRITE (IDLOG,*,ERR=1010) 
     &	   "      radiance profile for a given direction : ",
     & 	   FICSOS_RES_VS_Z
           WRITE (IDLOG,*,ERR=1010) " "
           WRITE (IDLOG,*,ERR=1010) 
     &	   "      Required direction (deg) : ", VZA_OUT
           WRITE (IDLOG,*,ERR=1010) 
     &	   "      Cosine of the required direction : ",
     &     DCOS(VZA_OUT*INCTE_PI/180.D+00)
           WRITE (IDLOG,*,ERR=1010) " "  
	 ELSE
	    WRITE (IDLOG,*,ERR=1010) 
     &	    "  --> No file of marine radiance profile to produce"
	 ENDIF
	  
      ENDIF ! Fin Trace
      

    
      IF (FICSOS_RES_VS_Z.NE."NO_RES_VS_Z_FILE") THEN
         
	 COS_VZA_OUT = DCOS(VZA_OUT*INCTE_PI/180.D+00)
	  
         ! Interpolation pour la direction sollicitée
         !-------------------------------------------       
       
         ! Cas VZA_OUT supérieure à l'angle max
         COSMAX=RMU(1)
         IF (COS_VZA_OUT.GT.COSMAX) THEN
            COS_VZA_OUT = COSMAX
	    VZA_OUT = DACOS(COSMAX)*180.D+00/INCTE_PI
	   
	    IF (TRACE) THEN
               WRITE (IDLOG,*,ERR=1010) 
     &	        "      Cos of angle of output direction > max value :",
     &          COSMAX
               WRITE (IDLOG,*,ERR=1010) 
     &	          "      ==> Output direction (deg) becomes : ",
     &            VZA_OUT
                WRITE (IDLOG,*,ERR=1010) " "
            ENDIF ! Fin Trace
	   
         ENDIF 
       
         ! Cas VZA_OUT inférieure à l'angle max
         COSMIN=RMU(-1)
         IF (COS_VZA_OUT.LT.COSMIN) THEN
            COS_VZA_OUT = COSMIN
	    VZA_OUT = DACOS(COSMIN)*180.D+00/INCTE_PI
	   
	    IF (TRACE) THEN
               WRITE (IDLOG,*,ERR=1010) 
     &	         "      Cos of angle of output direction < min value ",
     &           COSMIN
               WRITE (IDLOG,*,ERR=1010) 
     &	         "      ==> Output direction (deg) becomes : ",
     &           VZA_OUT
               WRITE (IDLOG,*,ERR=1010) " "
            ENDIF ! Fin Trace
	   
         ENDIF 

         ! Estimation des angles VZA1 et VZA2 encadrant VZA_OUT
         IF (COS_VZA_OUT.GT.DCOS(INCTE_PI/2.D+00)) THEN !Cas COS_VZA_OUT > 0

            IF (COS_VZA_OUT.EQ.COSMAX) THEN
               J1=2
	       J2=1
            ELSE
	       J=1
	       DO WHILE (COS_VZA_OUT.LT.RMU(J)) 
	           J = J+1
	       ENDDO
	       J1 = J
               J2 = J-1
	    ENDIF
     
         ELSE 

            IF (COS_VZA_OUT.LT.DCOS(INCTE_PI/2.D+00)) THEN !Cas COS_VZA_OUT < 0
               IF (COS_VZA_OUT.EQ.COSMIN) THEN
                  J1=-2
	          J2=-1
               ELSE
	          J=-NBMU
	          DO WHILE (COS_VZA_OUT.LT.RMU(J)) 
	             J = J+1
	          ENDDO
	          J1 = J
                  J2 = J-1
	       ENDIF
	  
	    ELSE   !Cas COS_VZA_OUT = 0   
	       J1=NBMU
	       J2=-NBMU
	    ENDIF 
	   
         ENDIF !Fin test sur signe de COS_VZA_OUT
      
         VZA1 = DACOS(RMU(J1))*180.D+00/INCTE_PI
         VZA2 = DACOS(RMU(J2))*180.D+00/INCTE_PI
	     
      
         IF (TRACE) THEN
            WRITE (IDLOG,*,ERR=1010) 
     &	    "      Angle sup J VZA(deg) Cos : ", J1, VZA1, RMU(J1)
            WRITE (IDLOG,*,ERR=1010) 
     &	    "      Angle inf J VZA(deg) Cos : ", J2, VZA2, RMU(J2)
         ENDIF ! Fin Trace


       
         OPEN(10,FILE=FICSOS_RES_VS_Z,FORM='FORMATTED',ERR=1210)   
     
         WRITE(10,*) 'STANDARD RESULTS : '
         WRITE(10,*) '  RADIANCE FIELD VERSUS THE SEA DEPTH'
         WRITE(10,*) '  (RELATIVE AZIMUTH AND VIEWING ZENITH ANGLE'// 
     &	             ' ARE FIXED)'
         WRITE(10,*) '----------------------------------------------'//
     &   '-------------------------------------------------'  
         WRITE(10,*) 'Relative azimuth (degrees) :'
         WRITE(10,*) ' '
         WRITE(10,*) '   Relative azimuth convention : '
         WRITE(10,*) 
     &     '       180° <-> Satellite and Sun in the same half-plane'
         WRITE(10,*) 
     &     '         0° <-> Satellite and Sun in opposite half-planes'//
     &     ' with respect to the zenith direction'
         WRITE(10,*) ' '
         WRITE(10,*) '   Simulated relative azimuth (degrees) :',
     &	              PHI1*180./INCTE_PI
         WRITE(10,*) ' '
     
	 
	 
	 
	 IF (VZA_OUT.LT.90.) THEN
	    WRITE(10,*) 'Upward direction VZA (degrees) :', VZA_OUT
	 ELSE
	    WRITE(10,*) 'Downward direction VZA '//
     &	                '(degrees from zenith direction) :', VZA_OUT
	 ENDIF
         WRITE(10,*) ' '
         WRITE(10,*) 'Columns parameters :'
         WRITE(10,*) '  Z       :  Depth in the sea (meters)'
         WRITE(10,*) '  SCA_ANG :  Scattering angle (deg)'
         WRITE(10,*) 
     &	    '  I       :  Stokes parameter at level Z (in sr-1)'
         WRITE(10,*) 
     &	    '             normalised to the extraterrestrial solar'//
     &      ' irradiance (PI * L(z) / Esun)'
	 WRITE(10,*) '  REFL    :  Reflectance at level Z'//
     &   ' (PI * L(z) / Ed(z))'
         WRITE(10,*) '  POL_RATE:  Degree of polarization (%)'
         WRITE(10,*) 
     &	 '  LPOL    :  Polarized intensity at level Z (in sr-1)'
          WRITE(10,*) 
     &	    '             normalised to the extraterrestrial solar '//
     &      ' irradiance (PI * Lpol(z) / Esun)'
	 WRITE(10,*) '  REFL_POL:  Polarized reflectance at level Z'//
     &   ' (PI * Lpol(z) / Ed(z))'
         WRITE(10,*) '----------------------------------------------'//
     &   '------------------------------------------------'  
     	  
         WRITE(10,*) '      Z     SCA_ANG       I            REFL'//
     &	             '       POL_RATE     LPOL        REFL_POL'
      
    
               
         DRMU = RMU(J2)-RMU(J1)
         ! Profil marin
         !------------------------------------------------------------------------ 	    
         DO I=0,CTE_NT_SEA
      
            IPROF = CTE_NT_ATM+1+I		!Indice sur profil complet
	 
            COEF_A = (ANGDIFF_PHI1(IPROF,J2)-ANGDIFF_PHI1(IPROF,J1)) 
	    COEF_A = COEF_A / DRMU
	    COEF_B = (RMU(J2)*ANGDIFF_PHI1(IPROF,J1)
     &	             -RMU(J1)*ANGDIFF_PHI1(IPROF,J2)) / DRMU
 	    ANGDIFF = COEF_A*COS_VZA_OUT+COEF_B
	 
	    COEF_A = (XIT_PHI1(IPROF,J2)-XIT_PHI1(IPROF,J1)) / DRMU
	    COEF_B = ( RMU(J2)*XIT_PHI1(IPROF,J1)
     &	              -RMU(J1)*XIT_PHI1(IPROF,J2) ) / DRMU
 	    XI = COEF_A*COS_VZA_OUT+COEF_B
	 
	    COEF_A = (XQT_PHI1(IPROF,J2)-XQT_PHI1(IPROF,J1)) / DRMU
	    COEF_B = (RMU(J2)*XQT_PHI1(IPROF,J1)
     &	             -RMU(J1)*XQT_PHI1(IPROF,J2) ) / DRMU
 	    XQ = COEF_A*COS_VZA_OUT+COEF_B
	 
	    COEF_A = (XUT_PHI1(IPROF,J2)-XUT_PHI1(IPROF,J1)) / DRMU
	    COEF_B = (RMU(J2)*XUT_PHI1(IPROF,J1)
     &	             -RMU(J1)*XUT_PHI1(IPROF,J2) ) / DRMU
 	    XU = COEF_A*COS_VZA_OUT+COEF_B
		
	    CALL OSOAA_POLAR(XI,XQ,XU,XAN,TPOL,LPOL)


            !Calcul de la réflectance et réflectance polarisée
            FLUX_DOWN_NORMA=PROFIL_FLUX_DOWN_TOT(IPROF) ! Flux descendant total (direct 
	    						! + diffus) normalisé par Es = PI

 	    REFL = XI/FLUX_DOWN_NORMA
	    REFL_POL = LPOL/FLUX_DOWN_NORMA
	         
	    !Enregistrement du champ pour profondeur Z
            WRITE(10,900,ERR=1211) -SEA_Z(I),ANGDIFF,XI,REFL,TPOL,
     &	    			    LPOL,REFL_POL
	    
         ENDDO !Boucle sur directions 
    
         CLOSE(10) !Fermeture du fichier FICSOS_RES_VS_Z
      
      	  
      ENDIF !Fin test sur FICSOS_RES_VS_Z


      

     
      
      
C* Fin nominale 
C-------------------
      GOTO 9999
            
C* Cas d'erreur et retour du status -1 au programme appelant 
C----------------------------------------------
 1000 WRITE(6,*) '  OSOAA_SOS ERROR_1000 on logfile opening' 
      GOTO 9998             

 1010 WRITE(6,*) '  OSOAA_SOS ERROR_1010 on logfile writing' 
      GOTO 9998
      
 1020 WRITE(6,*) '  OSOAA_SOS ERROR_1020 on angles file opening'
      GOTO 9998

 1021 WRITE(6,*) '  OSOAA_SOS ERROR_1021 on angles file reading'      
      GOTO 9998

 1030 WRITE(6,*) '  OSOAA_SOS ERROR_1021 on angles file value for IMUS'
      WRITE(6,*) '  Out of available range.'
      WRITE(6,*) '  --> Defined value : ',N0
      WRITE(6,*) '      Has to be define between 1 and ',NBMU
      GOTO 9998
            
 1031 WRITE(6,*) '  OSOAA_SOS ERROR_1031 on -ANG.Thetas value :'
      WRITE(6,*) '  --> out of available range'
      GOTO 9998      

            
 1040 WRITE(6,*) '  OSOAA_SOS ERROR_1040 on AEROSOLS file opening'
      GOTO 9998      

 1041 WRITE(6,*) '  OSOAA_SOS ERROR_1041 on AEROSOLS file reading'
      GOTO 9998 

 1042 WRITE(6,*) '  OSOAA_SOS ERROR_1042 on ATM_PROFILE file opening'
      GOTO 9998      

 1043 WRITE(6,*) '  OSOAA_SOS ERROR_1043 on ATM_PROFILE file reading'
      GOTO 9998 

 1044 WRITE(6,*) '  OSOAA_SOS ERROR_1044 on HYDROSOLS file opening'
      GOTO 9998      

 1045 WRITE(6,*) '  OSOAA_SOS ERROR_1045 on HYDROSOLS file reading'
      GOTO 9998 

 1046 WRITE(6,*) '  OSOAA_SOS ERROR_1046 on MLP file opening'
      GOTO 9998      

 1047 WRITE(6,*) '  OSOAA_SOS ERROR_1047 on MLP file reading'
      GOTO 9998 

 1048 WRITE(6,*) '  OSOAA_SOS ERROR_1048 on SEA_PROFILE file opening'
      GOTO 9998      

 1049 WRITE(6,*) '  OSOAA_SOS ERROR_1049 on SEA_PROFILE file reading'
      GOTO 9998 

 1060 WRITE(6,*) '  OSOAA_SOS ERROR_1060 on subroutine OSOAA_SOS_CORE'
      GOTO 9998 
       
 1070 WRITE(6,*) '  OSOAA_SOS ERROR_1070 on subroutine OSOAA_TRPHI'
      GOTO 9998   
         
 1100 WRITE(6,*) 
     &   '  OSOAA_SOS ERROR_1100 on ADVANCED RESULT FILE opening'
      GOTO 9998    

 1101 WRITE(6,*) 
     &   '  OSOAA_SOS ERROR_1101 on ADVANCED RESULT FILE writing'
      GOTO 9998      

 1150 WRITE(6,*) '  OSOAA_SOS ERROR_1150 on altitude or depth defined'
      WRITE(6,*) '  for the radiance output : '
      WRITE(6,*) '     OSOAA.View.Z (m) = ', Z_OUT
      WRITE(6,*) '  Max altitude is : ', PROFIL_Z(0)
      WRITE(6,*) '  Max depth is : ', PROFIL_Z(NT_TOT)  
      GOTO 9998    
      
 1200 WRITE(6,*) 
     &   '  OSOAA_SOS ERROR_1200 on STANDARD RESULT FILE opening'
      WRITE(6,*) 
     &    '  --> radiance as a function of the upward viewing angle'
      WRITE(6,*) '  for a given depth or altitude'
      GOTO 9998

 1201 WRITE(6,*) 
     &   '  OSOAA_SOS ERROR_1201 on STANDARD RESULT FILE writing'
      WRITE(6,*) 
     &    '  --> radiance as a function of the upward viewing angle'
      WRITE(6,*) '  for a given depth or altitude'
      GOTO 9998

 1210 WRITE(6,*) 
     &   '  OSOAA_SOS ERROR_1210 on STANDARD RESULT FILE opening'
      WRITE(6,*) 
     &    '  --> radiance as a function of the depth'
      WRITE(6,*) '  for a given direction'
      GOTO 9998

 1211 WRITE(6,*) 
     &   '  OSOAA_SOS ERROR_1211 on STANDARD RESULT FILE writing'
      WRITE(6,*) 
     &    '  --> radiance as a function of the depth'
      WRITE(6,*) '  for a given direction'
      GOTO 9998

 1300 WRITE(6,*) '  OSOAA_SOS ERROR_1300 '
      WRITE(6,*) '     Error while opening the file Flux.txt'
      GOTO 9998   

 1301 WRITE(6,*) '  OSOAA_SOS ERROR_1301 '
      WRITE(6,*) '     Error while reading the file Flux.txt'
      GOTO 9998   
                                   
9998  IER=-1             
9999  RETURN   

C* Fermeture fichiers
C-------------------
      CLOSE(IDLOG)	! Fermeture du fichier Trace
      
C* Format
C--------- 

   60 FORMAT(E15.8,3(1X,E15.8))  		!Format fichier GRANU
   70 FORMAT(2X,I4,F9.3,3(1X,F9.5))		!Format fichier PROFIL_ATM
   80 FORMAT(I5,1X,F9.3,1X,E14.7,1X,3(1X,F7.5))	!Format fichier PROFIL_SEA
  
   71 FORMAT(2X,I4,1X,F9.1,6(1X,F9.5))    	!Format Trace PROFIL_ATM
   81 FORMAT(2X,I4,1X,F9.3,8(1X,F9.5))		!Format fichier PROFIL_SEA
   
   90 FORMAT(2X,I4,4X,F12.3,7X,F15.10)		!Format Trace PROFIL global
      
  610 FORMAT(I4,1X,2D21.14,1X,I4)		!Format fichier ANGLES
  
  223 FORMAT(I4,1X,D21.14,2X,F7.3,2X,D21.14,2X,I2)	!Format Trace
      
  700 FORMAT(I4,1X,F12.3,2(2X,F7.2),2X,3(E13.6,2X),2(F7.2,2X),E13.6)
  800 FORMAT(2(F7.2,2X),2(E13.6,1X),2X,F7.2,2X,2(E13.6,1X))
  900 FORMAT(F10.5,2X,F7.2,2X,2(E13.6,1X),1X,F7.2,2X,2(E13.6,1X))

  600 FORMAT(2X,I4,1X,F12.5,2X,6(2X,E12.6))    		!Format fichier Flux.txt	
      END      !FIN DE LA PROCEDURE OSOAA_SOS
      
      
      
      



C==============================================================================
C PROCEDURE: OSOAA_POLAR
C ==========
C      Routine calculant l'angle de la polarisation, le taux de polarisation
C      et l'intensité polarisée pour un vecteur (I,Q,U) en entrée
C
C      L'angle de polarisation XAN est définie de -PI/2 à +PI/2.
C
C      Les cas Q>0 et Q<0 sont établis à partir de l'analyse des valeurs de
C      cos(2*XAN) et sin(2*XAN) couplés au domaine de définition de XAN.
C
C      Le cas Q = 0 est établi à partir des valeurs possibles de XAN pour Q = 0.
C      Q = U = 0 correspond à une absence de polarisation ==> Angle indéterminé.
C
C Description des paramètres 			
C ------------------------------
C
C     XI (double)     (E) : Valeur du paramètre de Stokes I
C     XQ (double)     (E) : Valeur du paramètre de Stokes Q
C     XU (double)     (E) : Valeur du paramètre de Stokes U
C     XAN (double)    (S) : Valeur du l'angle de la polarisation (degrés)
C     TPOL (double)   (S) : Valeur du taux de polarisation (%)
C     LPOL (double)   (S) : Intensité polarisée normalisée à l'éclairement solaire au TOA 
C                           (PI * Lpol(z) / Esun)	       
C
C Common utilisé:
C --------------
C  Aucun
C
C Cas d'erreur :
C --------------
C  Aucun
C
C  Sous programmes utilisés:
C -------------------------
C  Aucun
C
C
C MOD:VERSION:1.0: 02/04/2015: Initial version of the OSOAA code
C MOD:VERSION:1.1: 04/06/2018: 
C    - Change of the global implementation
C    - Improvement for case Q = U = 0
C
C==============================================================================

      SUBROUTINE OSOAA_POLAR(XI,XQ,XU,XAN,TPOL,LPOL)

      IMPLICIT NONE     
                       	    
C* Définition des variables                         
C*-----------------------------------------------------------------
      
      DOUBLE PRECISION XI 	! (E) Paramètre de Stokes I 
      DOUBLE PRECISION XQ 	! (E) Paramètre de Stokes Q.
      DOUBLE PRECISION XU 	! (E) Paramètre de Stokes U.
      
      DOUBLE PRECISION XT 	! Rapport U / Q
     					     
      DOUBLE PRECISION XAN 	! (S) Angle de la polarisation (degrés)
	
      DOUBLE PRECISION LPOL	! (S) Intensité polarisée normalisée  
      				!     à l'éclairement solaire au TOA 	       
      DOUBLE PRECISION TPOL	! (S) Taux de polarisation en %.
	

C* Traitements                       
C*------------      
      
      IF (XQ.NE.0.) THEN !Cas XQ!=0
 
         XT= XU/XQ
		 
         IF (XQ.GT.0.) THEN ! Cas si XQ > 0

            XAN= 90.*ATAN(XT)/INCTE_PI    
   
         ELSE ! Cas si XQ < 0

	     IF (XU.GT.0.) THEN
               XAN=90.+90.*DATAN(XT)/INCTE_PI
            ELSE
              XAN= -90.+90.*DATAN(XT)/INCTE_PI
            ENDIF

         ENDIF             

      ELSE !Cas XQ=0

          IF (XU.GT.0.) THEN
              XAN=  45.
          ELSE
             IF (XU.LT.0) THEN
                XAN= -45.
             ELSE !Cas XQ=XU indéfinie
                XAN= VALEUR_INDEFINIE
             ENDIF
          ENDIF

      ENDIF
	
      LPOL = SQRT(XQ*XQ+XU*XU)
      
      IF (XI.NE.0.D+00) THEN
         TPOL = 100.*LPOL/XI
      ELSE
         TPOL = VALEUR_INDEFINIE
      ENDIF
            
9999  RETURN   
            
      END      !FIN DE LA PROCEDURE OSOAA_POLAR   
      
      
       
       
