C******************************************************************************
C* FICHIER: OSOAA_ANGLES.F
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
C* ROLE: Définition des angles utilisés pour la discrétisation des propriétés 
C*       radiatives simulées : fonctions de phase, BRDF/BPDF, champs de luminance. 
C*       - Pour les calculs de fonctions de phase.
C*       - Pour les calculs de matrice de BRDF/BPDF et champs de luminance.
C*       ==> Estime les angles de Gauss et les poids associés pour les intégrations spatiales.
C*       ==> Ajoute des angles utilisateurs de poids nuls (sans impact sur les valeurs intégrées).
C*       ==> Ajoute l'angle zénithal solaire (de poids nul).
C*
C* AUTEURS:  
C*   Code de Gauss initial: Laboratoire d'Optique Atmosphérique (LOA, Villeneuve d'Ascq).
C*   Reprise du codage et développements complémentaires : CS SI, Bruno Lafrance.
C*
C*
C* ROUTINES COUVERTES PAR LE FICHIER : 
C*   - OSOAA_ANGLES
C*   - OSOAA_ANGLES_GAUSS_USER
C*   - OSOAA_GAUSS  
C*   - OSOAA_ANGLES_ADD_TETAS
C*
C*
C* MOD:VERSION:1.0: 02/04/2015: Initial version of the OSOAA code
C* MOD:VERSION:1.1: 15/06/2015: All comments for log files are now in english
C* MOD:VERSION:1.2: 25/03/2016: 
C*    - Adjustment of the string length for the filename of FICANGLES_USER_LUM 
C*      and FICANGLES_USER_MIE written in the result files.
C*    - Modification to avoid duplicating the angle 0 degree in the list of angles
C*      in case of the user defines this value in a list of user's angles.
C*                                      
C******************************************************************************

C----------------------------------------------------------------------------
C Définition des constantes  
C---------------------------------------------------------------------------- 
C Constantes utilisées : 
C
C    CTE_NOT_DEFINED_VALUE_INT : ID pour les valeurs entières non définie par l'utilisateur
C    CTE_LENFIC2 : longueur maximale de la chaine de caractères d'un nom de fichier 
C                  (arborescence complète)
C    CTE_NBMAX_USER_ANGLES : Nombre maximal d'angles utilisateurs
C    CTE_MIE_NBMU_MAX : Nombre maximal d'angles de Gauss positifs 
C                       pour les calculs de MIE
C    CTE_OS_NBMU_MAX  : Nombre maximal d'angles de Gauss positifs 
C                       pour les calculs de luminance
C    CTE_OS_NB_MAX :    Valeur maximale pour l'ordre limite du développement en 
C                       polynômes de Legendre des fonctions de phase des particules
C                       et pour le développement en séries de Fourier des matrices
C                       d'interface et des champs de luminance.
C    CTE_OS_NS_MAX :    Valeur maximale pour l'ordre limite du développement en polynômes
C                       de Legendre des éléments de la matrice de Fresnel.
C    CTE_OS_NM_MAX :    Valeur maximale pour l'ordre limite  du développement en séries 
C                       de Fourier de la fonction G (probabilité de pente des vagues).
C    CTE_NBANGLES_MAX : Nombre maximal d'angles positifs pour la 
C                       déclaration des tableaux XMU et XHR de la routine    
C    CTE_DEFAULT_NBMU_MIE : Nombre d'angles de Gauss positifs par défaut pour les 
C                           calculs de fonctions de phase (si non renseigné par l'utilisateur).
C    CTE_DEFAULT_OS_NB : Ordre par défaut des développements des fonctions de phase 
C                        en Polynômes de Legendre, et des développements en séries de Fourier
C                        des matrices d'interface et du champ de luminance.
C    CTE_DEFAULT_NBMU_LUM : Nombre d'angles de Gauss positifs par défaut pour les 
C                           calculs de luminance (si non renseigné par l'utilisateur).
C    CTE_DEFAULT_OS_NS    : Ordre maximal par défaut du développement en polynômes de Legendre   
C                           des éléments de la matrice de Fresnel.
C    CTE_DEFAULT_OS_NM    : Ordre maximal par défaut du développement en séries de Fourier  
C                           de la fonction G des calculs de matrice de réflexion.
C    CTE_SEUIL_ECART_MU   : Seuil pour le test d'écart entre cos(teta(j)) et cos(tetas) ou rmu(j)
C                           cosinus d'un angle de Gauss.
C
C Constantes spécifiques :
C    INCTE_IDLOG_ANGLES : ID du ficher Trace de la routine
C    INCTE_PI : Valeur de PI (calculé par la machine)
C    LENLINE  : Longueur maximale d'une ligne de données lue dans un fichier
C----------------------------------------------------------------------------
#include "OSOAA.h"
#define INCTE_IDLOG_ANGLES 99
#define INCTE_PI DACOS(-1.D+00)
#define LENLINE 50



C==============================================================================
C PROCEDURE: OSOAA_ANGLES
C ==========
C
C    Programme de génération des fichiers d'angles utilisés par le code OSOAA
C
C
C Description des paramètres 
C ------------------------------
C
C     NBMU_GAUSS_LUM (I4) (E) : Nombre d'angles de Gauss souhaités pour les calculs de 
C                               luminance.
C				Si cette valeur n'a pas été définie par l'utilisateur,
C                               elle vaut CTE_NOT_DEFINED_VALUE_INT en entrée de la routine.
C
C     TETAS (double)     (E) : Angle zénithal solaire (0 < tetas < 90).
C                              (angle complémentaire : mus= -cos(tetas))
C
C     TETASW (double)    (E) : Angle zénithal solaire (0 < tetas < 90) transmis 
C                              par mer plate.
C                              (angle complémentaire : mus= -cos(tetas))
C
C     FICANGLES_USER_LUM (CHAR*CTE_LENFIC2)  (E) : 
C                              Nom complet du fichier des angles "utilisateurs"
C                              pour les calculs de luminance et BRDF
C                 	       (répertoire + nom fichier + extension)
C                              Si ce fichier n'a pas été défini par l'utilisateur,
C                              ce nom en entrée est 'NO_USER_ANGLES'.
C
C     NBMU_GAUSS_MIE (I4) (E) : Nombre d'angles de Gauss souhaités pour les calculs de Mie.
C                               Si cette valeur n'a pas été définie par l'utilisateur,
C                               elle vaut CTE_NOT_DEFINED_VALUE_INT en entrée de la routine.
C
C     FICANGLES_USER_MIE (CHAR*CTE_LENFIC2)  (E) : 
C                               Nom complet du fichier des angles "utilisateurs"
C                               pour les calculs de fonctions de phase.
C                 		(répertoire + nom fichier + extension)
C                               Si ce fichier n'a pas été défini par l'utilisateur,
C                               ce nom en entrée est 'NO_USER_ANGLES'.
C
C     FICANGLES_LOG (CHAR*CTE_LENFIC2)  (E) : 
C                               Nom complet du fichier des Trace de la routine
C                               OSOAA_ANGLES (répertoire + nom fichier + extension) 
C                               Si ce fichier n'a pas été défini par l'utilisateur,
C                               ce nom en entrée est 'NO_LOG_FILE',
C				et le fichier Trace n'est pas créé.    							
C
C     FICANGLES_RES_LUM (CHAR*CTE_LENFIC2)  (E) :  
C				Nom complet du fichier des angles utilisés 
C                               pour les calculs de luminance et BRDF
C                 		(répertoire + nom fichier + extension)
C
C     FICANGLES_RES_MIE (CHAR*CTE_LENFIC2)  (E) : 
C				Nom complet du fichier des angles utilisés 
C                               pour les calculs de fonctions de phase
C                 		(répertoire + nom fichier + extension)              
C
C     IER (I4)         (S) : indice d'erreur : 0 si pas d'erreur, -1 sinon.
C   
C
C Résultats fournis par la procédure
C ----------------------------------
C   ==> Fichiers décrivant les angles à utiliser pour les calculs de fonction de phase 
C   et pour les calculs de luminance + matrices de réflexion / transmission.
C   Ces fichiers contienent également les paramètres précisant les ordres de développement
C   des diverses fonctions : OS_NB (fonctions de phase, matrices d'interface, luminance), 
C                            OS_NS (ordre max des développements des fonctions de Fresnel)
C   			     OS_NM (ordre maximal du développement en séries de Fourier  
C                                   de la fonction G des calculs de matrice d'interface)
C 
C Description des fichiers utilisés	
C ---------------------------------
C
C    Fichier utilisateur FICANGLES_USER: 
C       ==> contient une liste d'angles (en degrés) à ajouter aux angles de Gauss
C           pour lesquels le poids attribué sera nul (=> sans impact sur les 
C           intégrations spatiales)
C
C        Format : 
C            Première ligne (pour FICANGLES_USER_LUM uniquement): 
C                 Champ OUTPUT_GAUSS_ANGLES=1/0  
C                           1 si les angles de Gauss font aussi partis des angles 
C                             des fichiers de champ de luminance en sortie.
C                           0 si juste les angles du fichier utilisateur font
C                             partis des directions de propagation 
C                             des fichiers de champ de luminance en sortie.
C            Lignes suivantes :
C                 Une valeur d'angle par ligne d'enregistrement.
C                 Les angles doivent être compris entre 0 et 90 degrés.
C                 Nombre d'enregistrements maximal : CTE_NBMAX_USER_ANGLES
C
C    Fichier trace : identifiant = IDLOG = INCTE_IDLOG_ANGLES
C
C    Fichier résultat FICANGLES_RES_MIE:
C      ==>   contient les informations suivantes
C		NB_TOTAL_ANGLES :  nb d'angles total à utiliser
C               NB_GAUSS_ANGLES :  nb d'angles de Gauss
C               ANGLES_USERFILE :  Nom du fichier des angles utilisateur
C               INTERNAL_OS_NB :   Ordre des développements des fonctions de phase 
C				   en Polynômes de Legendre à utiliser.
C               INDEX COS_ANGLE WEIGHT : lignes donnant le numéro d'angle, son cosinus et 
C                                        son poids au format I4,1X,2D21.14
C
C    Fichier résultat FICANGLES_RES_LUM:
C      ==>   contient les informations suivantes
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
C Common utilisé:
C --------------
C  Aucun
C
C
C Cas d'erreur :
C ------------
C     - Erreur dans un sous-programme.
C     - Erreur à l'ouverture, la lecture et l'écriture du fichier Trace 
C       et des fichiers E/S :  fichier des angles utilisateurs et fichiers résultats.
C     - Paramètres non valables.
C     - Incohérence de la dimension CTE_NBANGLES_MAX des tableaux d'angles de la routine 
C       par rapport à celle des tableaux utilisées dans le code OSOAA :
C       calculs de MIE : CTE_MIE_NBMU_MAX et de luminance : CTE_OS_NBMU_MAX.
C     - Incohérence d'un angle utilisateur par rapport au domaine attendu (0 à 90 degrés).
C     - Nombre d'angles utilisateurs trop important devant CTE_NBMAX_USER_ANGLES.
C     - Nombre d'angles utiles (de Gauss + utilisateurs) trop important devant  
C       le dimensionnement des tableaux pour les calculs de Mie CTE_OS_NBMU_MAX 
C       ou pour les calculs de luminance CTE_OS_NBMU_MAX.  
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant.
C
C
C  Sous programmes utilisés:
C --------------------------
C  Ce programme fait appel aux routines:
C      - OSOAA_ANGLES_GAUSS_USER
C      - OSOAA_GAUSS  
C      - OSOAA_ANGLES_ADD_TETAS
C
C==============================================================================

      SUBROUTINE OSOAA_ANGLES(NBMU_GAUSS_LUM, TETAS, TETASW, 
     &                        FICANGLES_USER_LUM,                 
     &			      NBMU_GAUSS_MIE, FICANGLES_USER_MIE, 
     &                        FICANGLES_LOG,
     &                        FICANGLES_RES_LUM, FICANGLES_RES_MIE, IER)

      IMPLICIT NONE

C Constantes
C ----------
      DOUBLE PRECISION CONVDEGRAD       !Coefficient de conversion des degrés en radians.
                  
      INTEGER*2 IDLOG			! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_ANGLES)
      	    
	    
C* Définition des variables                         
C*-----------------------------------------------------------------      
      INTEGER*4 NBMU_GAUSS_LUM   ! Nombre d'angles de Gauss utilisés pour les calculs 
      				 ! de luminance      
      INTEGER*4 NBMU_LUM         ! Nombre d'angles effectifs des simulations de luminance
      INTEGER*4 NBMU_LUMW        ! Nombre d'angles effectifs des simulations de luminance 
      				 ! après ajout de l'angle solaire transmis
      INTEGER*4 NBMU_GAUSS_MIE   ! Nombre d'angles de Gauss utilisés pour les calculs de Mie
      INTEGER*4 NBMU_MIE         ! Nombre d'angles effectifs des calculs de Mie
            
      INTEGER*4 IER        ! Code d'erreur =0 si pas d'erreur, =-1 sinon.
      INTEGER*4 J          ! Indices des tableaux d'angles.
      INTEGER*4 IMUS       ! Indice de l'angle zénithal solaire dans la table des angles.
      INTEGER*4 IMUSW      ! Indice de l'angle zénithal solaire transmis dans la table
      			   ! des angles.
      
      INTEGER*4 OS_NB 	   ! Ordre des développements des fonctions de phase 
			   ! en Polynômes de Legendre, des matrices d'interface et
			   ! des champs de luminance.
      INTEGER*4 OS_NS      ! Ordre maximal du développement en polynômes de Legendre   
			   ! des éléments de la matrice de Fresnel.
      INTEGER*4 OS_NM      ! Ordre maximal du développement en séries de Fourier de la  
			   ! fonction G des calculs de matrice de réflexion / transmission.

      INTEGER*4 IND_USER_ANGLE(-CTE_NBANGLES_MAX:CTE_NBANGLES_MAX) 
      			   ! Tableau d'indices précisant si les angles sont des angles
			   ! de la liste des angles utilisateurs : 0 non / 1 oui
			   ! (potentiellement aussi angle de Gauss)

      INTEGER*4 IND_OUTPUT_GAUSS_ANGLES  ! Indice précisant si les angles de Gauss doivent 
                                         ! faire partis des directions de propagation fournies 
                                         ! dans les fichiers résultats (0 non / 1 oui).      			    

      INTEGER*4 IFIN_CH         ! Position de fin d'une chaine de caractères
					
      DOUBLE PRECISION TETAS	! Angle zénithal solaire en degrés (0 < tetas < 90).
      				! (angle complémentaire : mus= -cos(tetas))      
								
      DOUBLE PRECISION TETASW	! Angle zénithal solaire transmis dans mer plate
      				! en degrés (0 < tetasw < 90).
      				! (angle complémentaire : musw= -cos(tetasw)) 
				     				      
      DOUBLE PRECISION XMUS     ! Cosinus de l'angle zénithal solaire
      DOUBLE PRECISION XMU(-CTE_NBANGLES_MAX:CTE_NBANGLES_MAX) !Cosinus des angles 
      							       !(de Gauss et utilisateur)
      DOUBLE PRECISION XHR(-CTE_NBANGLES_MAX:CTE_NBANGLES_MAX) !Poids des angles 
      							       !(de Gauss et utilisateur)
      INTEGER*2 IND_ANGOUT(-CTE_NBANGLES_MAX:CTE_NBANGLES_MAX) 
      			!Tableau d'indices associés aux angles, précisant pour lesquels 
      			!il doit y avoir  une sortie dans le fichier résultat des angles 
			!utilisateur    (1 : pour une sortie requise)
   
   
      CHARACTER*CTE_LENFIC2 FICANGLES_LOG       ! Nom du fichier trace des calculs d'angles
      	
	!Calculs de luminance :         
      CHARACTER*CTE_LENFIC2 FICANGLES_USER_LUM  
      			! Chemin et nom du fichier des angles "utilisateurs" 
                        ! pour les calculs de luminance.
      CHARACTER*CTE_LENFIC2 FICANGLES_RES_LUM   
      			! Chemin et nom du fichier des angles utilisés 
                        ! pour les calculs de luminance.    
      	
	!Calculs de fonctions de phase :  
      CHARACTER*CTE_LENFIC2 FICANGLES_USER_MIE  
      			! Chemin et nom du fichier des angles "utilisateurs" 
                        ! pour les calculs de fonctions de phase. 
      CHARACTER*CTE_LENFIC2 FICANGLES_RES_MIE   
      			! Chemin et nom du fichier des angles utilisés    
                        ! pour les calculs de fonctions de phase. 
						  
      LOGICAL TRACE     ! = vrai si écriture dans le fichier trace
   
      				
						        
C* Initialisation                     
C*-----------------------------------------------------------------
      IER=0
      
      !Coefficient de conversion des degrés en radians.
      CONVDEGRAD=INCTE_PI/180.D+00
      
C* Contrôle des paramètres                       
C*-----------------------------------------------------------------
      
C     --> Affectation de la valeur par défaut si NBMU_GAUSS_MIE non renseigné
      IF (NBMU_GAUSS_MIE.EQ.CTE_NOT_DEFINED_VALUE_INT) THEN
          NBMU_GAUSS_MIE = CTE_DEFAULT_NBMU_MIE
	  OS_NB = CTE_DEFAULT_OS_NB
      ELSE
          !Pour une fonction de phase discrétisée en NBMU_GAUSS_MIE angles de Gauss 
	  !(positifs), sa décomposition en polynômes de Legendre ne peut être correcte 
	  !que si elle est poussée jusqu'à un ordre valant au moins 2 fois le nb d'angles 
	  !de Gauss utilisés dans la discrétisation de la fonction.
          OS_NB = 2*NBMU_GAUSS_MIE	  
      ENDIF 

C     --> Affectation de la valeur par défaut si NBMU_GAUSS_LUM non renseigné
      IF (NBMU_GAUSS_LUM.EQ.CTE_NOT_DEFINED_VALUE_INT) THEN
          NBMU_GAUSS_LUM = CTE_DEFAULT_NBMU_LUM
	  OS_NS = CTE_DEFAULT_OS_NS
	  OS_NM = CTE_DEFAULT_OS_NM
      ELSE
          !Pour un champs de luminance discrétisé en NBMU_GAUSS_LUM angles de Gauss
	  !(positifs), la décomposition en polynômes de Legendre des fonctions de réflexion
	  !de Fresnel n'est correcte que si elle est poussée jusqu'à un ordre valant 
	  !au moins 2 fois le nb d'angles de Gauss utilisés pour discrétiser les directions
	  !de propagation.
          OS_NS = 2*NBMU_GAUSS_LUM
	  
	  !Le calcul de la fonction G des calculs de BRDF (glitter, ...) impose un ordre 
	  ! maximal des développements en séries de Fourier tel que OS_NM >= OS_NB + OS_NS
	  OS_NM = OS_NB + OS_NS
      ENDIF 

C     --> Contrôle des ordres limites des développements pour les calculs 
C         des coefficients des matrices de phase et de réflexion
      IF (OS_NB.LT.2*NBMU_GAUSS_MIE) GOTO 1000
      IF (OS_NS.LT.2*NBMU_GAUSS_LUM) GOTO 1010
      IF (OS_NM.LT.(OS_NB+OS_NS))    GOTO 1020
       
      IF (OS_NB.GT.CTE_OS_NB_MAX) GOTO 1030
      IF (OS_NS.GT.CTE_OS_NS_MAX) GOTO 1040
      IF (OS_NM.GT.CTE_OS_NM_MAX) GOTO 1050


C* Ouverture du fichier Trace 
C------------------------------------------------------
      IF (FICANGLES_LOG.EQ.'NO_LOG_FILE') THEN
	 TRACE=.FALSE.
      ELSE
         TRACE=.TRUE.
	 OPEN (IDLOG,FILE=FICANGLES_LOG,ERR=2000)
      ENDIF


C* Préparation des angles pour les calculs de fonction de phase 
C--------------------------------------------------------------      
      CALL OSOAA_ANGLES_GAUSS_USER("MIE",NBMU_GAUSS_MIE,
     &				   FICANGLES_USER_MIE,TRACE,
     &                             NBMU_MIE, XMU, XHR,
     &                             IND_USER_ANGLE,
     &                             IND_OUTPUT_GAUSS_ANGLES,
     &                             IER) 
      
      IF (IER.EQ.-1) GOTO 3000
  
C* Ecriture du fichier d'angles résultat pour les calculs de fonction
C* de phase 
C-------------------------------------------------------------------         
      OPEN(UNIT=10,FILE=FICANGLES_RES_MIE,FORM='FORMATTED',ERR=2100)
      WRITE(10,100,ERR=2110) NBMU_MIE      
      WRITE(10,110,ERR=2110) NBMU_GAUSS_MIE 
      IFIN_CH=INDEX(FICANGLES_USER_MIE,' ')
      WRITE(10,120,ERR=2110) FICANGLES_USER_MIE(1:IFIN_CH)
      WRITE(10,200,ERR=2110) OS_NB
      WRITE(10,250,ERR=2110) 
      DO J=1,NBMU_MIE
         WRITE(10,300,ERR=2110) J, XMU(J), XHR(J)
      ENDDO
         
	 
	 
	 
C* Préparation des angles de Gauss et Utilisateur
C* pour les calculs de luminance et BRDF/BPDF 
C-------------------------------------------------------------------      
      CALL OSOAA_ANGLES_GAUSS_USER("LUM",NBMU_GAUSS_LUM,
     &                             FICANGLES_USER_LUM,TRACE,
     &                             NBMU_LUM, XMU, XHR,
     &                             IND_USER_ANGLE,
     &                             IND_OUTPUT_GAUSS_ANGLES,
     &                             IER) 
      
      IF (IER.EQ.-1) GOTO 3000     
     
C* Préparation de la table des indices à retenir pour une sortie des
C* angles utilisateurs 
C-------------------------------------------------------------------         
      DO J=1,NBMU_LUM
      
         IF (IND_OUTPUT_GAUSS_ANGLES.EQ.1) THEN
	   
	    !Tous les angles sont à fournir en sortie
	    IND_ANGOUT(J)=1
	    
         ELSE
	   
	    !Ne fournir en sortie que les angles utilisateurs
	    !(potentiellement aussi angles de Gauss)
            IF (IND_USER_ANGLE(J).EQ.1) THEN	
	       IND_ANGOUT(J)=1
	    ELSE
	       IND_ANGOUT(J)=0
	    ENDIF
	   
         ENDIF
      
      ENDDO      
      

C* Ajout de l'angle d'incidence solaire dans l'atmosphère
C-------------------------------------------------------------------         
      CALL OSOAA_ANGLES_ADD_TETAS(TETAS,IMUS,NBMU_LUM,
     &                            XMU,XHR,IND_ANGOUT,IER)
 
      IF (IER.EQ.-1) GOTO 4000 


C* Ajout de l'angle d'incidence solaire transmis par mer plate
C-------------------------------------------------------------------         
      !On initialise NBMU_LUMW
      NBMU_LUMW=NBMU_LUM
      !On ajoute TETASW
      CALL OSOAA_ANGLES_ADD_TETAS(TETASW,IMUSW,NBMU_LUMW,
     &                            XMU,XHR,IND_ANGOUT,IER)
 
      IF (IER.EQ.-1) GOTO 4000 


C* Correction de IMUS 
C* (qui doit changer si TETASW n'est pas associé à un mu de Gauss
C*  et si l'indice de l'eau n'est pas forcé égale à 1)
C------------------------------------------------------------------- 
      !S'il y a eu un élément ajouté à la liste des angles pour inclure
      !TETASW ==> IMUS doit être mis à jour et le nb d'elt final aussi.
      IF (NBMU_LUMW.GT.NBMU_LUM) THEN
          !Mise à jour de l'indice d'angle solaire dans l'air
	  IF (IMUSW.LE.IMUS) IMUS = IMUS+1
	  !Mise à jour du nb total d'angles
	  NBMU_LUM = NBMU_LUMW
      ENDIF


C* On déclare les angles solaires faisant partis des angles des fichiers 
C* de sortie dans le cas où les angles de Gauss en font partis.
C* Sinon, ils ne sont intégrés aux angles des fichiers de sortie que s'ils
C* font partis de la liste des angles utilisateurs.
      IF (IND_OUTPUT_GAUSS_ANGLES.EQ.1) THEN
	  IND_ANGOUT(IMUS)=1
	  IND_ANGOUT(IMUSW)=1
      ENDIF            
      
C* Information de Trace 
C-------------------------------------------------------------------         
      IF (TRACE) THEN
         WRITE (IDLOG,*,ERR=2010) "Addition of the solar zenith angle"
	 WRITE (IDLOG,*,ERR=2010) "----------------------------------"
	 WRITE (IDLOG,*,ERR=2010) " "
	 WRITE(IDLOG,*,ERR=2010) "TETAS : ", TETAS
	 WRITE(IDLOG,*,ERR=2010) "IMUS  : ", IMUS
	 WRITE(IDLOG,*,ERR=2010) "TETASW : ", TETASW
	 WRITE(IDLOG,*,ERR=2010) "IMUSW  : ", IMUSW
	 WRITE(IDLOG,*,ERR=2010) "Final number of angles : ", NBMU_LUM 
         WRITE (IDLOG,*,ERR=2010) " " 
	 WRITE (IDLOG,*,ERR=2010) "List of used angles :"
	 WRITE (IDLOG,*,ERR=2010) 
     &   "   Index, mu, angle (deg.), weight, output angle flag"
	 DO J=1,NBMU_LUM
	     WRITE(IDLOG,700,ERR=2010) J, XMU(J),
     &	         DACOS(XMU(J))/CONVDEGRAD,XHR(J),IND_ANGOUT(J)
	 ENDDO
	 WRITE (IDLOG,*,ERR=2010) " " 
	 WRITE (IDLOG,*,ERR=2010) " "  	
      ENDIF !Fichier Trace	       
		

C* Ecriture du fichier d'angles résultat pour les calculs de fonction
C* de phase 
C-------------------------------------------------------------------         
      OPEN(UNIT=20,FILE=FICANGLES_RES_LUM,FORM='FORMATTED',ERR=2100)
      WRITE(20,100,ERR=2110) NBMU_LUM
      WRITE(20,110,ERR=2110) NBMU_GAUSS_LUM
      IFIN_CH=INDEX(FICANGLES_USER_LUM,' ')
      WRITE(20,120,ERR=2110) FICANGLES_USER_LUM(1:IFIN_CH)
      WRITE(20,410,ERR=2110) TETAS
      WRITE(20,420,ERR=2110) IMUS
      WRITE(20,415,ERR=2110) TETASW
      WRITE(20,425,ERR=2110) IMUSW
      WRITE(20,200,ERR=2110) OS_NB
      WRITE(20,500,ERR=2110) OS_NS
      WRITE(20,510,ERR=2110) OS_NM
      WRITE(20,550,ERR=2110) 
      DO J=1,NBMU_LUM
         WRITE(20,600,ERR=2110) J, XMU(J), XHR(J), IND_ANGOUT(J)
      ENDDO
     
     	
C* Fermeture fichier
C-------------------
      CLOSE(10)
      CLOSE(20)
      
C* Fin nominale 
C-------------------
      GOTO 9999
            
C* Cas d'erreur et retour du status -1 au shell 
C----------------------------------------------
 1000 WRITE(6,*) '  OSOAA_ANGLES : ERROR_1000: '
      WRITE(6,*) '  => CTE_DEFAULT_OS_NB <= 2*CTE_DEFAULT_NBMU_MIE'
      WRITE(6,*) '  --> check value in OSOAA.h and compile again'
      GOTO 9998 

 1010 WRITE(6,*) '  OSOAA_ANGLES : ERROR_1010 : '
      WRITE(6,*) '  => CTE_DEFAULT_OS_NS <= 2*CTE_DEFAULT_NBMU_LUM'
      WRITE(6,*) '  --> check value in OSOAA.h and compile again'
      GOTO 9998   

 1020 WRITE(6,*) '  OSOAA_ANGLES : ERROR_1020 : '
      WRITE(6,*) '=> ', 
     & 'CTE_DEFAULT_OS_NM < CTE_DEFAULT_OS_NB + CTE_DEFAULT_OS_NS'
      WRITE(6,*) '  --> check value in OSOAA.h and compile again'
      GOTO 9998                

 1030 WRITE(6,*) '  OSOAA_ANGLES : ERROR_1030 : '
      WRITE(6,*) '  => OS_NB =',OS_NB,' > CTE_OS_NB_MAX'
      WRITE(6,*) '  --> check value in OSOAA.h and compile again'
      GOTO 9998 

 1040 WRITE(6,*) '  OSOAA_ANGLES : ERROR_1040 : '
      WRITE(6,*) '  => OS_NS =', OS_NS, ' > CTE_OS_NS_MAX'
      WRITE(6,*) '  --> check value in OSOAA.h and compile again'
      GOTO 9998   

 1050 WRITE(6,*) '  OSOAA_ANGLES : ERROR_1050 : '
      WRITE(6,*) '  => OS_NM =', OS_NM, ' > CTE_OS_NM_MAX'
      WRITE(6,*) '  --> check value in OSOAA.h and compile again'
      GOTO 9998 
                   
 2000 WRITE(6,*) '  OSOAA_ANGLES : ERROR_2000 on logfile opening' 
      GOTO 9998             

 2010 WRITE(6,*) '  OSOAA_ANGLES : ERROR_2010 on logfile writing' 
      GOTO 9998   
      
 3000 WRITE(6,*) '  OSOAA_ANGLES : ERROR_3000  '
      WRITE(6,*) '      on subroutine OSOAA_ANGLES_GAUSS_USER'
      GOTO 9998

 4000 WRITE(6,*) '  OSOAA_ANGLES : ERROR_4000  '
      WRITE(6,*) '      on subroutine OSOAA_ANGLES_ADD_TETAS'
      GOTO 9998
      
 2100 WRITE(6,*) '  OSOAA_ANGLES : ERROR_2100 on ANGLES file opening'
      GOTO 9998  

 2110 WRITE(6,*) '  OSOAA_ANGLES : ERROR_2110 on ANGLES file writing'
      GOTO 9998  

9998  IER=-1             
9999  RETURN   

C* Format
C---------   

      
  100 FORMAT(17hNB_TOTAL_ANGLES :,I4)
  110 FORMAT(17hNB_GAUSS_ANGLES :,I4)
  120 FORMAT(17hANGLES_USERFILE :,A)
  200 FORMAT(16hINTERNAL_OS_NB :,I4)
  250 FORMAT(35hINDEX   COS_ANGLE            WEIGHT)
  300 FORMAT(I4,1X,2D21.14)

  410 FORMAT(20hSOLAR ZENITH ANGLE :,F7.3)
  420 FORMAT(15hINTERNAL_IMUS :,I4)
  415 FORMAT(41hTRANSMITTED SOLAR ZENITH ANGLE IN WATER :,F7.3)
  425 FORMAT(16hINTERNAL_IMUSW :,I4)
  500 FORMAT(16hINTERNAL_OS_NS :,I4)
  510 FORMAT(16hINTERNAL_OS_NM :,I4)
  550 FORMAT(55hINDEX   COS_ANGLE            WEIGHT              OUTPUT)
  600 FORMAT(I4,1X,2D21.14,1X,I4)
  700 FORMAT(I4,1X,D21.14,2X,F7.3,2X,D21.14,2X,I2)        
      END      !FIN DE LA PROCEDURE OSOAA_ANGLES




C==============================================================================
C PROCEDURE: OSOAA_ANGLES_GAUSS_USER
C ==========
C
C  La routine calcule les angles et poids de Gauss et ajoute, si requis, 
C  les angles du fichier utilisateur.
C
C  Elle trie les angles.
C
C Description des paramètres
C ----------------------------		  
C
C       USAGE (CHAR*3)  (E) : indicateur de l'usage qui sera fait du fichier d'angles : 
C                             calculs de MIE ou simulation du champs de rayonnement LUM.	
C	NBMU_GAUSS (I4) (E) : nombre d'angles de Gauss souhaités.
C       FICANGLES_USER (CHAR*CTE_LENFIC2) (E) : nom du fichier des angles "utilisateur"
C                                               (chemin complet)
C       TRACE (LOGICAL) (E)  : vrai si écriture dans le fichier trace.
C       NBMU_UTILES (I4) (S) : nombre d'angles après concaténation des angles de Gauss 
C                              et des angles "utilisateur".
C	XMU (double)     (S) : tableau des cosinus des angles calculés.
C	XHR (double)     (S) : tableau des poids associés aux angles calculés.
C       IND_USER_ANGLE (double) (S) :  tableau d'indices précisant si les angles 
C                                      du tableau xmu font partis de la liste 
C                                      des angles utilisateurs : 0 non / 1 oui	
C       IND_OUTPUT_GAUSS_ANGLES (I4) (E) : Indice précisant si les angles de Gauss doivent 
C                                          faire partis des directions de propagation fournies 
C                                          dans les fichiers résultats. 0 pour non / 1 pour oui.  
C       IER (I4)         (S) : indice d'erreur : 0 si pas d'erreur, -1 sinon.
C     
C Résultats fournis par la procédure
C ----------------------------------
C   Les tableaux XMU et XHR des cosinus et poids des angles utiles.
C   Le nb d'angles NBMU_UTILES.
C   Un retour d'erreur IER. 
C   
C Description des fichiers utilisés
C ---------------------------------
C
C    Fichier utilisateur FICANGLES_USER: 
C       ==> contient une liste d'angles (en degrés) à ajouter aux angles de Gauss
C           pour lesquels le poids attribué sera nul (=> sans impact sur les 
C           intégrations spatiales)
C
C        Format : 
C            Première ligne (pour le cas USAGE = LUM uniquement): 
C                 Champ OUTPUT_GAUSS_ANGLES=1/0  
C                           1 si les angles de Gauss font aussi partis des angles 
C                             des fichiers de champ de luminance en sortie.
C                           0 si juste les angles du fichier utilisateur font
C                             partis des directions de propagation 
C                             des fichiers de champ de luminance en sortie.
C            Lignes suivantes :
C                 Une valeur d'angle par ligne d'enregistrement.
C                 Les angles doivent être compris entre 0 et 90 degrés.
C                 Nombre d'enregistrements maximal : CTE_NBMAX_USER_ANGLES
C
C
C    Fichier trace : identifiant = IDLOG (conforme à l'ouverture par le code appelant)
C
C Common utilisé:
C --------------
C  Aucun
C
C Cas d'erreur :
C ------------
C     - Paramètre USAGE non valable.
C     - Incohérence de la dimension CTE_NBANGLES_MAX des tableaux d'angles de la routine 
C       par rapport à celle des tableaux utilisées dans le code OSOAA :
C       calculs de MIE : CTE_MIE_NBMU_MAX et de luminance : CTE_OS_NBMU_MAX.
C     - Erreur à l'ouverture du fichier des angles utilisateurs.
C     - Incohérence d'un angle utilisateur par rapport au domaine attendu (0 à 90 degrés).
C     - Nombre d'angles utilisateurs trop important devant CTE_NBMAX_USER_ANGLES.
C     - Nombre d'angles utiles (de Gauss + utilisateurs) trop important devant  
C       le dimensionnement des tableaux pour les calculs de Mie CTE_OS_NBMU_MAX 
C       ou pour les calculs de luminance CTE_OS_NBMU_MAX.  
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant.
C
C
C  Sous programmes utilisés:
C --------------------------
C  OSOAA_GAUSS
C
C==============================================================================
      
      SUBROUTINE  OSOAA_ANGLES_GAUSS_USER
     &               (USAGE,NBMU_GAUSS,FICANGLES_USER,
     &                TRACE,NBMU_UTILES,XMU,XHR,
     &                IND_USER_ANGLE,IND_OUTPUT_GAUSS_ANGLES,
     &                IER) 

      IMPLICIT NONE


C Constantes ANGLES
C -----------------
      DOUBLE PRECISION CONVDEGRAD       !Coefficient de conversion des degrés en radians.
      
      INTEGER*2 IDLOG			! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_ANGLES)
      
C* Définition des variables                                                 
C--------------------------------------------
      INTEGER*4 NBMU_GAUSS      ! Nombre d'angles de Gauss utilisés.       
      INTEGER*4 NBMU_USER       ! Nombre d'angles contenus dans le fichier utilisateur. 
      INTEGER*4 NBMU_USER_FILTERED  ! Nombre d'angles utilisateur n'étant pas des angles 
      				    ! de Gauss utilisés
      INTEGER*4 NBMU_UTILES     ! Nombre d'angles effectifs des simulations. 
      INTEGER*4 IER             ! code d'erreur =0 si pas d'erreur, =-1 sinon.
      INTEGER*4 J, K            ! Indices des tableaux d'angles.
      INTEGER*4 IND_USER_ANGLE(-CTE_NBANGLES_MAX:CTE_NBANGLES_MAX) 
      			        ! Tableau d'indices précisant si les angles sont des angles
			        ! de la liste des angles utilisateurs : 0 non / 1 oui
			        ! (potentiellement aussi angle de Gauss)

      INTEGER*4 IND_OUTPUT_GAUSS_ANGLES  ! Indice précisant si les angles de Gauss doivent 
                                         ! faire partis des directions de propagation fournies
                                         ! dans les fichiers résultats (0 non / 1 oui). 
      INTEGER*4 IVAL	!Valeur temporaire du tableau IND_USER_ANGLE.
      INTEGER*4 IFIN_CH         ! Position de fin d'une chaine de caractères
					      			    
      DOUBLE PRECISION XMU(-CTE_NBANGLES_MAX:CTE_NBANGLES_MAX) !Cosinus des angles 
      							       !(de Gauss et utilisateur)
      DOUBLE PRECISION XHR(-CTE_NBANGLES_MAX:CTE_NBANGLES_MAX) !Poids des angles 
      							       !(de Gauss et utilisateur)
      DOUBLE PRECISION VAL	!Valeur d'angle lue dans le fichier utilisateur	
      DOUBLE PRECISION MU_COMP(CTE_NBMAX_USER_ANGLES) !Cosinus des angles utilisateur
 
      CHARACTER*CTE_LENFIC2 FICANGLES_USER  ! chemin et nom du fichier 
      				            ! des angles "utilisateurs"
      CHARACTER*3 USAGE     ! Indicateur de l'usage qui sera fait du fichier d'angles : 
      			    ! calculs de MIE ou simulation du champs de rayonnement LUM	

      CHARACTER*LENLINE LIGNE_TEXTE	! Ligne de caractères pour décodage de paramètres

      LOGICAL TRACE     ! = vrai,si écriture dans le fichier trace
      
      LOGICAL MU_COMP_A_AJOUTER(CTE_NBMAX_USER_ANGLES)   !Défini si l'angle utilisateur doit
      							 !être ajouté à la liste des angles
							 !(car distinct d'un angle de Gauss utilisé)

C* Initialisation
C-----------------
      IER=0
      NBMU_USER=0 

      !Coefficient de conversion des degrés en radians.
      CONVDEGRAD=INCTE_PI/180.D+00
       
C* Contrôle de validité du paramètre USAGE
C------------------------------------------------------      
      IF ((USAGE.NE.'MIE').AND.(USAGE.NE.'LUM')) GOTO 950
      
C* Contrôle de cohérence des dimensionnements
C------------------------------------------------------      
      IF (USAGE.EQ.'MIE') THEN
          IF (CTE_NBANGLES_MAX.LT.CTE_MIE_NBMU_MAX) GOTO 960
      ENDIF
      
      IF (USAGE.EQ.'LUM') THEN
          IF (CTE_NBANGLES_MAX.LT.CTE_OS_NBMU_MAX) GOTO 961
      ENDIF
      
      IF (NBMU_GAUSS.GE.CTE_NBANGLES_MAX) GOTO 962
           
      
C* Calcul des angles et poids de Gauss
C  -----------------------------------
      CALL OSOAA_GAUSS(NBMU_GAUSS+1, XMU, XHR)

C* Positionnement par défaut de IND_OUTPUT_GAUSS_ANGLES
C* ==> 1 = angles de Gauss joints aux angles des fichiers
C*     de champs de luminance.
C  -----------------------------------
      IND_OUTPUT_GAUSS_ANGLES=1
      	
C* Lecture des angles utilisateurs
C------------------------------------------------------ 	
      IF (FICANGLES_USER.NE.'NO_USER_ANGLES') THEN
      	 
         OPEN(1,FILE=FICANGLES_USER,STATUS='OLD',ERR=970)      
      
         !Lecture de l'indicateur d'ajout des angles de Gauss aux angles utilisateurs
	 !Gestion des cas d'erreur
         IF (USAGE.EQ.'LUM') THEN
            READ(1,'(a)',ERR=9711) LIGNE_TEXTE
	    IFIN_CH=INDEX(LIGNE_TEXTE,' ')
	    IF (IFIN_CH.NE.22) GOTO 9712
	    READ(LIGNE_TEXTE(21:21),*,ERR=9712) IND_OUTPUT_GAUSS_ANGLES
	    
	    IF ((IND_OUTPUT_GAUSS_ANGLES.NE.0).AND.
     & 	        (IND_OUTPUT_GAUSS_ANGLES.NE.1)) GOTO 9712
         ENDIF
      
          J=0  
   20	  READ(1,*,ERR=971,END=30) VAL
               IF ((VAL.LT.0.).OR.(VAL.GT.90.)) GOTO 972              
	       J=J+1
	       IF (J.GT.CTE_NBMAX_USER_ANGLES) GOTO 973
	       MU_COMP(J)=DCOS(VAL*CONVDEGRAD)
	  GOTO 20
   30	  CLOSE(1)
          NBMU_USER=J
    
      ENDIF


C*  Information au fichier TRACE
C------------------------------------------------------   
      IF (TRACE) THEN
         IF (USAGE.EQ.'MIE') THEN
	     WRITE (IDLOG,*,ERR=990) "PHASE FUNCTIONS CALCULATIONS"
	     WRITE (IDLOG,*,ERR=990) "****************************"
	 ENDIF
	 IF (USAGE.EQ.'LUM') THEN
	     WRITE (IDLOG,*,ERR=990) "RADIANCE CALCULATIONS"
	     WRITE (IDLOG,*,ERR=990) "*********************"
	 ENDIF
	 WRITE (IDLOG,*,ERR=990) " "
	 WRITE(IDLOG,*,ERR=990) "Number of Gauss angles : ", NBMU_GAUSS
	 WRITE (IDLOG,*,ERR=990) " "
         IF (FICANGLES_USER.NE.'NO_USER_ANGLES') THEN
	     WRITE(IDLOG,*,ERR=990)  
     &	       "A file of user's angles is used :", FICANGLES_USER
     	     WRITE(IDLOG,*,ERR=990) 
     &	       "Number of user's angles before filtering:", NBMU_USER
     	     WRITE(IDLOG,*,ERR=990) "List of user's angles:"
	     DO J=1,NBMU_USER
	        WRITE(IDLOG,*,ERR=990) j, DACOS(MU_COMP(J))/CONVDEGRAD
	     ENDDO     
	     IF (IND_OUTPUT_GAUSS_ANGLES.EQ.1) THEN
	        WRITE(IDLOG,*,ERR=990) 
     &		"Addition of Gauss angles to the list of output angles"
	     ELSE
	     	WRITE(IDLOG,*,ERR=990) 
     &		"No Gauss angles in output files"
	     ENDIF
    
	 ELSE
	     WRITE(IDLOG,*,ERR=990)  
     &	       "No file of user's angles"
	 ENDIF  
	 WRITE (IDLOG,*,ERR=990) " " 
	 WRITE (IDLOG,*,ERR=990) " "  	
      ENDIF !Fichier Trace	        
      
      
      
      
C* Elimination des angles correspondant à des angles de Gauss 
C* faisant déjà partis de la liste d'angles
C* Elimination du cas MU_COMP = 1 car systématiquement ajouté 
C* en fin de préparation de la liste des angles: 
C------------------------------------------------------  

      ! Initialisation pour le cas où il n'y a pas de fichier d'angles utilisateurs
      ! Par initialisation, on considère que les angles de Gauss ne sont pas des angles utilisateur
      DO K=1,NBMU_GAUSS          
	 IND_USER_ANGLE(K)=0		    			
      ENDDO
    
      !Détermination des angles à ajouter
      DO J=1,NBMU_USER
      		
         MU_COMP_A_AJOUTER(J)=.TRUE.
         DO 111 K=1,NBMU_GAUSS
 
            IF (DABS(MU_COMP(J)-1D+00).LE.CTE_SEUIL_ECART_MU) THEN
               MU_COMP_A_AJOUTER(J)=.FALSE.
            ELSE
	       IF (DABS(MU_COMP(J)-XMU(K)).LE.CTE_SEUIL_ECART_MU) THEN
	          MU_COMP_A_AJOUTER(J)=.FALSE.
	          IND_USER_ANGLE(K)=1   !L'angle de Gauss d'indice K = angle utilisateur
		  GOTO 112     
	       ENDIF
	    ENDIF
111      CONTINUE

112      CONTINUE
	 
      ENDDO

      !Recomposition de la liste des angles à ajouter
      K=1
      NBMU_USER_FILTERED=0
      DO J=1,NBMU_USER
      
         IF (MU_COMP_A_AJOUTER(J).EQV..TRUE.) THEN
	     MU_COMP(K)=MU_COMP(J)
	     K=K+1
	     NBMU_USER_FILTERED=NBMU_USER_FILTERED+1
	 ENDIF
 
      ENDDO    
      
      !Redéfinition du nb d'angles utilisateurs après rejet des doublons
      !par rapport aux angles de Gauss
      NBMU_USER=NBMU_USER_FILTERED  
      
C* Détermination du nombre d'angles utiles : 
C*    angles de Gauss + angles Utilisateurs + angle nul (mu = 1)
C------------------------------------------------------      
      NBMU_UTILES = NBMU_GAUSS + NBMU_USER + 1

      IF (USAGE.EQ.'MIE') THEN
          IF (NBMU_UTILES.GT.CTE_MIE_NBMU_MAX) GOTO 980
      ENDIF 
      
      IF (USAGE.EQ.'LUM') THEN
          IF (NBMU_UTILES.GT.CTE_OS_NBMU_MAX) GOTO 981
      ENDIF 

C*  Ajout des angles utilisateurs aux angles de Gauss
C------------------------------------------------------           
      IF (NBMU_USER.NE.0) THEN
      
          DO J=1,NBMU_USER
	     XMU(NBMU_GAUSS+J) = MU_COMP(J)
	     XHR(NBMU_GAUSS+J) = 0.D+00
	     IND_USER_ANGLE(NBMU_GAUSS+J)=1
	  ENDDO 

      ENDIF

C*  Ajout de mu = 1 de poids nul
C------------------------------------------------------   
      XMU(NBMU_UTILES) = 1.D+00
      XHR(NBMU_UTILES) = 0.D+00
      IND_USER_ANGLE(NBMU_UTILES)=1
      
      
      
C*  Tri des angles : cas USAGE="MIE" --> Trie de MUmin à MUmax
C*                   cas USAGE="LUM" --> Trie de MUmax à MUmin
C*    Tri sur les angles définis sur J=1,NBMU_UTILES
C------------------------------------------------------           
      IF (USAGE.EQ.'MIE') THEN
 
          DO J=1,NBMU_UTILES	     
	     DO K=J+1,NBMU_UTILES
	        IF (XMU(J).GT.XMU(K)) THEN		    		    
		    !Permutation des valeurs XMU(J) et XMU(K)
		    VAL=XMU(J)
		    XMU(J)=XMU(K)
		    XMU(K)=VAL
		    !Permutation des valeurs XMU(J) et XHR(K)
		    VAL=XHR(J)
		    XHR(J)=XHR(K)
		    XHR(K)=VAL   
		    !Permutation des valeurs IND_USER_ANGLE(J) et IND_USER_ANGLE(K)
		    IVAL=IND_USER_ANGLE(J)
		    IND_USER_ANGLE(J)=IND_USER_ANGLE(K)
		    IND_USER_ANGLE(K)=IVAL   	    	    
		ENDIF
	     ENDDO 
	  ENDDO 
	  
      ENDIF !USAGE=MIE	  

      IF (USAGE.EQ.'LUM') THEN
 
          DO J=1,NBMU_UTILES	     
	     DO K=J+1,NBMU_UTILES
	        IF (XMU(J).LT.XMU(K)) THEN		    		    
		    !Permutation des valeurs XMU(J) et XMU(K)
		    VAL=XMU(J)
		    XMU(J)=XMU(K)
		    XMU(K)=VAL
		    !Permutation des valeurs XMU(J) et XHR(K)
		    VAL=XHR(J)
		    XHR(J)=XHR(K)
		    XHR(K)=VAL   
		    !Permutation des valeurs IND_USER_ANGLE(J) et IND_USER_ANGLE(K)
		    IVAL=IND_USER_ANGLE(J)
		    IND_USER_ANGLE(J)=IND_USER_ANGLE(K)
		    IND_USER_ANGLE(K)=IVAL   	      
		ENDIF
	     ENDDO 
	  ENDDO 
	  
      ENDIF !USAGE=LUM	  
      	  	
  	  
C*  Complétudes des angles définis sur J=-NBMU_UTILES,0
C------------------------------------------------------      	  
      DO J=1,NBMU_UTILES
	     XMU(-J) = -XMU(J)
	     XHR(-J) = XHR(J)
	     IND_USER_ANGLE(-J) = IND_USER_ANGLE(J)
      ENDDO 
      XMU(0) = 0.D+00
      XHR(0) = 0.D+00
	
C*  Information au fichier TRACE
C------------------------------------------------------   
      IF (TRACE) THEN
         
         IF (FICANGLES_USER.NE.'NO_USER_ANGLES') THEN
	     
     	     WRITE(IDLOG,*,ERR=990) 
     &          "Number of remaining user's angles after filtering:",
     &           NBMU_USER
     	     WRITE(IDLOG,*,ERR=990) "List of complementary angles:"
	     DO J=1,NBMU_USER
	        WRITE(IDLOG,*,ERR=990) j, DACOS(MU_COMP(J))/CONVDEGRAD
	     ENDDO     
	 ENDIF
         WRITE (IDLOG,*,ERR=990) " " 
	 WRITE (IDLOG,*,err=990) "List of used angles :"
	 WRITE (IDLOG,*,err=990) 
     &	 "   Index, mu, angle (degrees), weigth, user's angle (O/1)"
	 DO J=1,NBMU_UTILES
	     WRITE(IDLOG,222,ERR=990) J, XMU(J),
     &	                           DACOS(XMU(J))/CONVDEGRAD, XHR(J),
     &                             IND_USER_ANGLE(J)  
	 ENDDO
	 WRITE (IDLOG,*,ERR=990) " " 
	 WRITE (IDLOG,*,ERR=990) " "  	
      ENDIF !Fichier Trace	        
      
      
C* Fin nominale 
C-------------------
      GOTO 9999


C* Cas d'erreur et retour du status -1 au programme appelant
C-----------------------------------------------------------   
         
  950  WRITE(6,*) '  OSOAA_ANGLES_GAUSS_USER : ERROR_950: '
       WRITE(6,*) "Error on USAGE parameters : has to be 'MIE' or 'LUM'"
       IER=-1
       GOTO 9999        
       
  960  WRITE(6,*) '  OSOAA_ANGLES_GAUSS_USER : ERROR_960: '
       WRITE(6,*) "Inconsistence on size of tables in OSOAA.h."
       WRITE(6,*) " --> required : CTE_NBANGLES_MAX "
       WRITE(6,*) "                  >= CTE_MIE_NBMU_MAX"
       IER=-1
       GOTO 9999   

  961  WRITE(6,*) '  OSOAA_ANGLES_GAUSS_USER : ERROR_961: '
       WRITE(6,*) "Inconsistence on size of tables in OSOAA.h."
       WRITE(6,*) " --> required : CTE_NBANGLES_MAX "
       WRITE(6,*) "                  >= CTE_OS_NBMU_MAX"
       IER=-1
       GOTO 9999        

  962  WRITE(6,*) '  OSOAA_ANGLES_GAUSS_USER : ERROR_962: '
       WRITE(6,*) "Inconsistence on size of tables in OSOAA.h."
       WRITE(6,*) " --> Number of required Gauss angles is ", NBMU_GAUSS
       WRITE(6,*) "     greater than the limit CTE_NBANGLES_MAX :", 
     &            CTE_NBANGLES_MAX
       IER=-1
       GOTO 9999        
          
  970  WRITE(6,*) '  OSOAA_ANGLES_GAUSS_USER : ERROR_970: '
       WRITE(6,*) "Error while opening the user's angles file : "
       WRITE(6,*)  FICANGLES_USER
       WRITE(6,*) " --> check its localization"
       IER=-1
       GOTO 9999      

 9711  WRITE(6,*) '  OSOAA_ANGLES_GAUSS_USER : ERROR_9711: '
       WRITE(6,*) "Error while reading the user's angles file :"
       WRITE(6,*)  FICANGLES_USER
       WRITE(6,*) "Error on the first line OUTPUT_GAUSS_ANGLES=1",
     & " or OUTPUT_GAUSS_ANGLES=0"
       IER=-1
       GOTO 9999   

 9712  WRITE(6,*) '  OSOAA_ANGLES_GAUSS_USER : ERROR_9712: '
       WRITE(6,*) "Error while reading the user's angles file :"
       WRITE(6,*)  FICANGLES_USER
       WRITE(6,*) "Error on the first line OUTPUT_GAUSS_ANGLES=1",
     & " or OUTPUT_GAUSS_ANGLES=0"
       IER=-1
       GOTO 9999   
       
  971  WRITE(6,*) '  OSOAA_ANGLES_GAUSS_USER : ERROR_971: '
       WRITE(6,*) "Error while reading the user's angles file :"
       WRITE(6,*)  FICANGLES_USER
       IER=-1
       GOTO 9999   

  972  WRITE(6,*) '  OSOAA_ANGLES_GAUSS_USER : ERROR_972: '
       WRITE(6,*) "Error on an user's angles value in file: "
       WRITE(6,*)  FICANGLES_USER
       WRITE(6,*) "  --> have to defined between 0 and 90°"
       IER=-1
       GOTO 9999   

  973  WRITE(6,*) '  OSOAA_ANGLES_GAUSS_USER : ERROR_973: '
       WRITE(6,*) "Error on the user's angles file : "
       WRITE(6,*)  FICANGLES_USER
       WRITE(6,*) "  --> The number of angles has to be < or = to"
       WRITE(6,*) "       CTE_NBMAX_USER_ANGLES =",
     &                    CTE_NBMAX_USER_ANGLES
       IER=-1
       GOTO 9999

  980  WRITE(6,*) '  OSOAA_ANGLES_GAUSS_USER : ERROR_980: '
       WRITE(6,*) "Error on the number of used phaze function angles : "
       WRITE(6,*) "  --> greater than the maximal number allowed "
       WRITE(6,*) "       CTE_MIE_NBMU_MAX =",CTE_MIE_NBMU_MAX
       IER=-1
       GOTO 9999 
       
  981  WRITE(6,*) '  OSOAA_ANGLES_GAUSS_USER : ERROR_981: '
       WRITE(6,*) "Error on the number of used radiance angles : "
       WRITE(6,*) "  --> greater than the maximal number allowed "
       WRITE(6,*) "       CTE_OS_NBMU_MAX =",CTE_OS_NBMU_MAX
       IER=-1
       GOTO 9999 
      
  990 WRITE(6,*) '  OSOAA_ANGLES_GAUSS_USER : ERROR_990 '
      WRITE(6,*) '  Error on logfile writing' 
      IER=-1
      GOTO 9999
              
9999  RETURN


C* Format
C---------   
  221 FORMAT(A20,I4)
  222 FORMAT(I4,1X,D21.14,2X,F7.3,2X,D21.14,2X,I4)
   
      END !FIN DE LA PROCEDURE OSOAA_ANGLES_GAUSS_USER       
             
	     
	     
	     
C==============================================================================
C PROCEDURE: OSOAA_GAUSS
C ==========
C
C  Routine originale fournie par le LOA (M. Herman, octobre 2009).
C  Pas de documentation.
C
C  La routine calcule les angles et poids de Gauss pour MM-1 valeurs.
C
C  Les tableaux générés couvrent les valeurs XMU(-MM:MM) et XHR(-MM:MM)
C  avec pour XMU(-MM)=-1 et XMU(MM)=1 de poids nuls.
C
C  Une contrainte est d'avoir CTE_NBANGLES_MAX >= MM
C
C Description des paramètres
C ----------------------------
C	MM (I4) (E) : nombre d'angles de Gauss souhaités
C	AMU (double) (S) : tableau des cosinus des angles calculés.
C	PMU (double) (S) : tableau des poids associés aux angles calculés.			  
C
C Résultat fourni par la procédure
C --------------------------------
C   Les tableaux XMU et XHR
C   
C Description des fichiers utilisés
C ---------------------------------
C  Aucun
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
      
      SUBROUTINE  OSOAA_GAUSS(MM,AMU,PMU)
C     N : ORDRE DE LA QUADRATURE SUR XL A XU

C* Définition des variables                         
C*-----------------------------------------------------------------
      PARAMETER (IX=2*CTE_NBANGLES_MAX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION Z(IX),PA(IX),W(IX),R(IX)
      DOUBLE PRECISION AMU(-CTE_NBANGLES_MAX:CTE_NBANGLES_MAX)  !Angles de Gauss
      DOUBLE PRECISION PMU(-CTE_NBANGLES_MAX:CTE_NBANGLES_MAX)  !Poids de Gauss

      TOL = 1.0D-15
      PI = 3.141592653589793D+00
      N=2*MM-2
      XL=-1
      XU=+1
      AA = 2.0D+00/PI**2
      AB = -62.0D+00/(3.0D+00*PI**4)
      AC = 15116.0D+00/(15.0D+00*PI**6)
      AD = -12554474.D+00/(105.0D+00*PI**8)
      PA(1) = 1.D0
      EN = N
      NP1 = N+1
      U= 1.0D+00-(2.0D+00/PI)**2
      D = 1.0D+00/DSQRT((EN+0.5D+00)**2+U/4.0D+00)

      DO 100 I= 1,N
         SM = I
         AZ = 4.0D+00*SM-1.0D+00
         AE = AA/AZ
         AF = AB/AZ**3
         AG = AC/AZ**5
         AH = AD/AZ**7
         Z(I) = 0.25D+00*PI*(AZ+AE+AF+AG+AH)
	 
100   CONTINUE
	
      DO 200 K = 1,N
         X = DCOS(Z(K)*D)
    1    PA(2) = X
    	
         DO 201  NN = 3,NP1
         ENN = NN-1
         PA(NN) =
     &   ((2.0D+00*ENN-1.0D+00)*X*PA(NN-1)-(ENN-1.0D+00)*PA(NN-2))/ENN
  201    CONTINUE
  
         PNP = EN*(PA(N)-X*PA(NP1))/(1.0D+00-X*X)
         XI = X-PA(NP1)/PNP
         XD = DABS(XI-X)
         XDD = XD-TOL
	 
         IF (XDD) 3,3,2
    2       X = XI
            GO TO 1
    3       R(K) = X
            W(K) = 2.0D+00*(1.0D+00-X*X)/(EN*PA(N))**2
  200 CONTINUE
	  
      AP = (XU-XL)/2.D0
      BP = (XU+XL)/2.D0
      DO I=1,MM-1
         K=MM-I
         AMU(K)=BP+AP*R(I)
         PMU(K)=AP*W(I)
         AMU(-K)=-AMU(K)
         PMU(-K)=PMU(K)
      ENDDO
    	
      AMU(-MM)=-1.
      AMU(MM)=1.
      AMU(0)=0.
      PMU(0)=0.
      PMU(-MM)=0.
      PMU(MM)=0.
C
      RETURN
      END !FIN DE LA PROCEDURE OSOAA_GAUSS
      






	     
C==============================================================================
C PROCEDURE: OSOAA_ANGLES_ADD_TETAS
C ==========
C
C  Routine ajoutant l'angle TETAS à la table des angles déjà définis.
C
C
C Description des paramètres
C ----------------------------
C
C     TETAS (double)    (E) : Angle zénithal solaire (degrés).
C     IMUS  (I4)        (S) : Indice de l'angle zénithal solaire positionné
C                             dans la table des angles.
C     NBMU_LUM (I4)   (E/S) : Nombre d'angles effectifs des simulations de luminance
C                             à l'appel de la routine et mis à jour en sortie de routine.
C     XMU  (double)   (E/S) : Tableau des cosinus des angles (de Gauss et utilisateur)
C                             à l'appel de la routine et mis à jour  en sortie de routine.
C     XHR  (double)   (E/S) : Tableau des poids des angles (de Gauss et utilisateur)
C                             à l'appel de la routine et mis à jour  en sortie de routine.
C     IND_ANGOUT (I2) (E/S) : Tableau d'indices associés aux angles, précisant pour 
C                             lesquels il doit y avoir  une sortie dans le fichier 
C 			      résultat des angles utilisateur,
C                             à l'appel de la routine et mis à jour en sortie de routine.
C     IER (I4)          (S) : Indice d'erreur : 0 si pas d'erreur, -1 sinon.
C		  
C
C Résultat fourni par la procédure
C --------------------------------
C   Les tableaux XMU et XHR
C   
C Description des fichiers utilisés
C ---------------------------------
C  Aucun
C
C Common utilisé:
C --------------
C  Aucun
C
C Cas d'erreur :
C ------------
C  Nombre d'angles utiles (de Gauss + utilisateurs) trop important devant  
C  le dimensionnement des tableaux pour les calculs de luminance CTE_OS_NBMU_MAX. 
C       
C
C  Sous programmes utilisés:
C --------------------------
C  Aucun
C
C==============================================================================
      
      SUBROUTINE  OSOAA_ANGLES_ADD_TETAS(TETAS,IMUS,NBMU_LUM,
     &                                   XMU,XHR,IND_ANGOUT,IER)

      IMPLICIT NONE

C Constantes S
C -----------------
      DOUBLE PRECISION CONVDEGRAD       !Coefficient de conversion des degrés en radians.
 

C* Définition des variables                         
C*-----------------------------------------------------------------
      INTEGER*4 IMUS          	! (S) Indice de l'angle zénithal solaire dans la table 
      				!     des angles

      INTEGER*4 J		! Indice de direction de propagation.
      INTEGER*4 IER        	! (S) Code d'erreur =0 si pas d'erreur, =-1 sinon
      INTEGER*4 NBMU_LUM        ! (E/S) Nombre d'angles effectifs des simulations 
      				!       de luminance
             					
      DOUBLE PRECISION TETAS	! (E) Angle zénithal solaire en degrés (0 < tetas < 90).
      				! (angle complémentaire : mus= -cos(tetas))            
      DOUBLE PRECISION XMUS     ! Cosinus de l'angle zénithal solaire
      DOUBLE PRECISION XMU(-CTE_NBANGLES_MAX:CTE_NBANGLES_MAX) ! (E/S) Cosinus des angles 
      							       !  (de Gauss et utilisateur)
      DOUBLE PRECISION XHR(-CTE_NBANGLES_MAX:CTE_NBANGLES_MAX) ! (E/S) Poids des angles
      							       !  (de Gauss et utilisateur)
      INTEGER*2 IND_ANGOUT(-CTE_NBANGLES_MAX:CTE_NBANGLES_MAX) 
      				! (E/S) Indices des tableaux d'angles pour lesquels 
      				! il doit y avoir une sortie dans le fichier résultat 
				! des angles utilisateur  (1 : pour une sortie requise)
      DOUBLE PRECISION XMU_copy(-CTE_NBANGLES_MAX:CTE_NBANGLES_MAX) 
      				!copie de XMU avant ajout de TETAS
      DOUBLE PRECISION XHR_copy(-CTE_NBANGLES_MAX:CTE_NBANGLES_MAX) 
      				!copie de XHR avant ajout de TETAS
      INTEGER*2 IND_ANGOUT_copy(-CTE_NBANGLES_MAX:CTE_NBANGLES_MAX) 
      				!copie de IND_ANGOUT avant ajout de TETAS


C* Initialisation                     
C*-----------------------------------------------------------------
      IER=0      
      
      !Coefficient de conversion des degrés en radians.
      CONVDEGRAD=INCTE_PI/180.D+00

      IMUS=-1
      XMUS=DCOS(TETAS*CONVDEGRAD)
      
      
      !Recherche d'une correspondance avec un angle déjà défini
      !-->On relève juste la valeur de IMUS, les tables ne sont pas à modifier
      J=1
      DO J=1,NBMU_LUM 
         IF (DABS(XMUS-XMU(J)).LT.CTE_SEUIL_ECART_MU) IMUS=J
      ENDDO
      	 
C    !Si TETAS n'est pas un angle déjà défini
      !-->Ajout dans la table des angles, sans sortie requise pour les angles utilisateur
      IF (IMUS.EQ.-1) THEN 
         !On estime l'indice IMUS dans la nouvelle table de NBMU_LUM+1 éléments.
	 
	 !-->Archivage des tables sans TETAS
         DO J=1,NBMU_LUM
	    XMU_copy(J) = XMU(J)
	    XHR_copy(J) = XHR(J)
	    IND_ANGOUT_copy(J) = IND_ANGOUT(J)
	 ENDDO
	 
	 !La table XMU_copy est ordonnée du plus grand au plus petit élément
	 !-->On cherche si XMUS doit être le premier élément de la nouvelle table
	 IF (XMUS.GT.XMU_copy(1)) THEN 
	     IMUS=1
	 ELSE
	     !-->On cherche si XMUS doit être le dernier élément de la nouvelle table
	     IF (XMUS.LT.XMU_copy(NBMU_LUM)) THEN 
	        IMUS=NBMU_LUM+1
	     ELSE
	      !-->On cherche quel élément J de la table initiale doit être remplacé par XMUS
	        DO J=1,NBMU_LUM-1
		   IF ( (XMUS.GT.XMU_copy(J+1)).
     &                   AND.(XMUS.LT.XMU_copy(J)) ) IMUS=J+1
                ENDDO 
	     ENDIF
	 ENDIF
	 
	 
	 !-->Mise à jour de la taille des tables
	 NBMU_LUM=NBMU_LUM+1
	 IF (NBMU_LUM.GT.CTE_OS_NBMU_MAX) GOTO 1100
	 
	 !-->Positionnement de TETAS dans les tables
	 XMU(IMUS) = XMUS
	 XHR(IMUS) = 0.D+00
	 IND_ANGOUT(IMUS)= 0
	 
	 !-->Complétude des tables avec décalage des indices
	 DO J=IMUS+1,NBMU_LUM  
	    XMU(J) = XMU_copy(J-1)
	    XHR(J) = XHR_copy(J-1)
	    IND_ANGOUT(J) = IND_ANGOUT_copy(J-1)
	 ENDDO
	    
        !-->Complétudes des angles définis sur J=-NBMU_UTILES,0
         DO J=1,NBMU_LUM
	        XMU(-J) = -XMU(J)
	        XHR(-J) =  XHR(J)
		IND_ANGOUT(-J) = IND_ANGOUT(J)
         ENDDO 
         XMU(0) = 0.D+00
         XHR(0) = 0.D+00
	 IND_ANGOUT(0) = 0
	
	 	 
      ENDIF !Test si IMUS non défini
      
C
C* Fin nominale 
C-------------------
      GOTO 9999
            
C* Cas d'erreur et retour du status -1 au programme appelant 
C----------------------------------------------
 1100 WRITE(6,*) '  OSOAA_ANGLES_ADD_TETAS : ERROR_1100 : '
      WRITE(6,*) '  => Error on the number of used radiance angles '
      WRITE(6,*) '    (gauss angles + possibly solar angle)  : '
      WRITE(6,*) '     --> greater than the maximal number allowed '
      WRITE(6,*) '       CTE_OS_NBMU_MAX =',CTE_OS_NBMU_MAX
      GOTO 9998
      
9998  IER=-1             
9999  RETURN   
      END !FIN DE LA PROCEDURE OSOAA_ANGLES_ADD_TETAS
      
