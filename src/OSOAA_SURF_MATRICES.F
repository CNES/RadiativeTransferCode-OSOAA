C******************************************************************************
C* FICHIER: OSOAA_SURF_MATRICES.F
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
C* ROLE: Ensemble de routines propres au calcul des matrices de réflexion 
C*       et de transmission pour l'interface air / mer
C*
C* AUTEURS:  
C*
C*   Fondement algorithmique pour le Glitter :
C*      - Laboratoire d'Optique Atmosphérique (LOA, Villeneuve d'Ascq) 
C*          Jean-Luc Deuzé, Maurice Herman, Richard Santer
C*        ==> code des Ordres Successifs de Diffusion 
C*            (version atmosphérique et glitter).
C*      - Communication & Systèmes (CS, Toulouse) pour le compte du CNES
C*          Bruno Lafrance
C*          ==> code SOS (version industrialisée du code des OS du LOA).
C*
C*   Code OSOAA : Spécification de l'interface air/mer agitée, implémentation 
C*      - Communication & Systemes (CS, Toulouse)
C*          Bruno Lafrance
C*
C*
C* ROUTINES COUVERTES PAR LE FICHIER : 
C*   - OSOAA_SURFACE_CASE
C*   - OSOAA_MAT_FRESNEL
C*   - OSOAA_SURF_DECOMP_LEGENDRE
C*   - OSOAA_MAT_INTERFACE
C*   - SOS_NOYAUX_FRESNEL
C*   - OSOAA_MISE_FORMAT
C*   - OSOAA_GSF
C*   - SOS_CALC_GR
C*   - OSOAA_CALC_GT
C*   - OSOAA_CALC_COSI_TRANS
C*
C*
C* MOD:VERSION:1.0: 02/04/2015: Initial version of the OSOAA code
C* MOD:VERSION:1.1: 15/06/2015: All comments for log files are now in english
C*                                   
C*   
C******************************************************************************

C----------------------------------------------------------------------------
C Définition des constantes  
C---------------------------------------------------------------------------- 
C Constantes utilisées :
C
C   CTE_LENFIC2 : Longueur des noms de fichiers avec arborescence.
C   CTE_OS_NBMU_MAX  : Nombre maximal d'angles de Gauss positifs 
C                      pour les calculs de luminance
C   CTE_OS_NB_MAX    : Ordre maximal pour le développement en polynômes
C                      de Legendre des fonctions de phase des particules
C                      et pour le développement en séries de Fourier des matrices
C                      d'interface et des champs de luminance.
C   CTE_OS_NS_MAX    : Ordre maximal du développement en polynômes de Legendre
C                      des fonctions de la matrice de Fresnel. 
C   CTE_OS_NM_MAX    : Ordre maximal du développement en série de Fourier de la
C                      fonction G associée à la statistique de pente des vagues.
C   CTE_PH_TEST      : Facteur pour comparer les valeurs Gmin et Gmax de la fonction
C                      G associée à la statistique de pente des vagues.
C   CTE_PH_NU	     : Nombre d'angles pour la discrétisation d'un domaine d'azimuts.
C   CTE_PH_NQ        : Valeur telle que CTE_PH_NU = 2 exposant CTE_PH_NQ
C   CTE_THRESHOLD_GMAX  : Seuil sur la valeur maximal Gmax de la fonction G associée
C                         à la statistique de pente des vagues.
C   CTE_THRESHOLD_G_SMAX : Seuil pour le test de détermination de l'ordre maximal de 
C                          développement en série de Fourier de la fonction G associée
C                          à la statistique de pente des vagues.
C   CTE_THRESHOLD_GEO_CONFIG : Seuil pour test de configuration géométrique possible
C                              dans le cas d'une transmission.
C   CTE_THRESHOLD_DICHO : Seuil angulaire pour sortie de calculs de dichotomie 
C   CTE_THRESHOLD_COSTHETAN : Seuil minimal sur le cosinus de l'angle zénithal de vague
C                             pour estimer un cas possible de transmission air -> mer.
C
C
C Constantes spécifiques :
C    INCTE_IDLOG_SURFACE : ID du ficher Trace de la routine
C    INCTE_PI : Valeur de PI (calculé par la machine)
C----------------------------------------------------------------------------
#include "OSOAA.h"
#define INCTE_IDLOG_SURFACE 99
#define INCTE_PI DACOS(-1.D+00)


C Complete mode TRACE to activate for expert analysis
#define TRACE_DEBUG .FALSE.	




C==============================================================================
C PROCEDURE: OSOAA_SURFACE_CASE
C ==========     
C 
C      Cette procédure gère l'enchainement des traitements pour calculer un fichier
C      décrivant la matrice d'interface de surface pour une réflexion ou une transmission 
C      (passage air - mer ou mer - air).
C
C	Incidence depuis l'air --> Calcul pour un milieu incident d'indice N1 = 1 
C                                  et un milieu de réfraction d'indice N2 = IND
C	    				==> coef N1SN2 = 1. / IND
C
C	Incidence depuis la mer --> Calcul pour un milieu incident d'indice N1 = IND 
C                                   et un milieu de réfraction d'indice N2 = 1
C	    				==> coef N1SN2 = IND
C
C Description des paramètres   
C --------------------------
C    
C       CINTER (CHAR*3) : (E) Chaine de 3 caractères précisant le type d'interface
C			      à simuler :
C   				une réflexion air -> air : "RAA"  
C			  	une réflexion mer -> mer : "RWW"
C				une transmission air -> mer : "TAW"
C				une transmission mer -> air : "TWA"
C			==> Champ nommé "XXX" dans la suite du texte
C       LUM_NBMU (I4) : (E) Nombre d'angles (positifs) effectivement utiles
C	RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (DOUBLE) : (E) Cosinus des angles.
C	CHR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) (DOUBLE) : (E) Poids des angles.
C 	WIND  (DOUBLE)  : Vitesse du vent en m/s. 
C       IND   (DOUBLE)  : Indice de réfraction de l'eau / air.
C       OS_NB (I4) : (E) Ordre des développements des fonctions de phase 
C		         en Polynômes de Legendre à utiliser, 
C                        des matrices d'interface et des champs de luminance.
C       OS_NS (I4) : (E) Ordre maximal à utiliser pour le développement en
C                        polynômes de Legendre des éléments de la matrice de Fresnel.
C       OS_NM (I4) : (E) Ordre maximal du développement en séries de Fourier  
C                        de la fonction G des calculs de matrice d'interface.
C       FICRES  (CHARACTER*CTE_LENFIC2) : (E) nom complet du fichier d'interface à générer.
C       TRACE  (LOGICAL)  (E)  ! = vrai si écriture dans le fichier trace.
C       IER  (I4)   (S) : code d'erreur =0 si pas d'erreur, =-1 sinon
C
C
C Résultats fournis par le programme
C ----------------------------------
C
C   Fichier FICRES de matrice d'interface
C
C   Fichier de trace (optionnel)
C              (No d'unité logique du fichier trace = IDLOG = INCTE_IDLOG_SURFACE )
C
C 
C Variable d'environnement
C ------------------------
C   Aucune
C
C
C Description des fichiers utilisés
C ---------------------------------
C
C   -->Contenu du fichier résultat FICRES : Fichier d'interface air / mer agitée
C       Fichier binaire non formaté, créé par OSOAA_MISE_FORMAT
C       Pour chaque ordre IS de la décomposition en série de Fourier (IS = 0 à OS_NB),
C       le fichier contient l'enregistrement des éléments Pij de la matrice,
C       limité aux angles utiles : 
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
C        --> Enregistrement sur le domaine limité aux angles utiles
C                                  --------------------------------
C
C   -->Contenu du fichier temporaire RES_GSF_XXX
C       Fichier binaire non formaté, créé par OSOAA_GSF
C       Fichier créé dans le répertoire de lancement du programme
C       Si un fichier de même nom existe déjà, il est écrasé
C       Chaque enregistrement contient :
C           -numéro d'angle Teta1 (valeur entière)
C           -numéro d'angle Teta2 (valeur entière)
C           -ordre maximale utile de la décomposition : IL  (valeur entière)
C           -Fonction G décomposée pour (Teta1,Teta2) : (E(J),J=0,IL)
C            (IL+1 valeurs double précision)
C
C   -->Contenu du fichier temporaire RES_FRESNEL_XXX
C      Fichier Ascii, format d'un enregistrement 4E15.8, 
C      créé par OSOAA_MAT_FRESNEL
C      Fichier créé dans le répertoire de lancement du programme
C      Si un fichier de même nom existe déjà, il est écrasé
C      Nombre d'enregistrements : de k=0 à OS_NS
C      Chaque enregistrement contient les paramètres ALPHA(k), BETA(k), 
C      GAMMA(k) et ZETA(k) associés à la matrice de Fresnel
C
C   -->Contenu du fichier temporaire RES_MAT_INTERFACE_XXX
C       Fichier binaire non formaté, créé par OSOAA_MAT_INTERFACE
C       Fichier créé dans le répertoire de lancement du programme
C       Si un fichier de même nom existe déjà, il est écrasé
C       Enregistrement pour chaque couple (Teta1, Teta2) de
C       la décomposition en séries de Fourier de la matrice de Fresnel:
C       M11,M12,M13,M21,M22,M23,M31,M32,M33 
C       (n valeurs réelles)
C
C    Les 3 fichiers temporaires sont écrasés s'ils existent, puis 
C    détruits en fin de programme.
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
C     - Erreur de définition du type d'interface
C     - Erreur d'écriture dans le fichier Trace
C     - Erreur d'ouverture d'un fichier temporaire
C     - Erreur de suppression d'un fichier temporaire
C
C     Affichage d'un message à l'écran, arrêt du programme et 
C     retour du status -1 au programme appelant
C        
C
C  Sous programmes utilisés:
C --------------------------
C  Cette procédure fait appel aux routines:
C      - OSOAA_GSF
C      - OSOAA_MAT_FRESNEL 
C      - OSOAA_MAT_INTERFACE 
C      - OSOAA_MISE_FORMAT
C
C=============================================================================

      SUBROUTINE OSOAA_SURFACE_CASE(CINTER,LUM_NBMU,RMU,CHR,WIND,IND,
     &                              OS_NB,OS_NS,OS_NM,
     &                              FICRES,TRACE,IER)

      IMPLICIT NONE


C Constantes 
C -----------------            
      INTEGER*2 IDLOG			! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_SURFACE)
      
C* Définition des variables   
C*--------------------------------------------------------------------------
      CHARACTER*3 CINTER	! Chaine de caractères précisant le cas d'interface :
      				!   une réflexion air -> air : "RAA"
				!   une réflexion mer -> mer : "RWW"
				!   une transmission air -> mer : "TAW"
				!   une transmission mer -> air : "TWA"
				
      CHARACTER*15 FICRES_FRESNEL ! Nom du fichier de Fresnel produit:
      				  !  "RES_FRESNEL_RAA" pour une réflexion air -> air
				  !  "RES_FRESNEL_RWW" pour une réflexion mer -> mer
      				  !  "RES_FRESNEL_TAW" pour une transmission air -> mer
				  !  "RES_FRESNEL_TWA" pour une transmission mer -> air

      CHARACTER*11 FICRES_GSF     ! Nom du fichier de décomposition de la fonction G
      				  ! de probabilite de pente de vagues produit:
      				  !  "RES_GSF_RAA" pour une réflexion air -> air
				  !  "RES_GSF_RWW" pour une réflexion mer -> mer
      				  !  "RES_GSF_TAW" pour une transmission air -> mer
				  !  "RES_GSF_TWA" pour une transmission mer -> air
				
      CHARACTER*21 FICRES_MAT     ! Nom du fichier de Matrice d'interface produit 
      				  ! avant mise en forme:
      				  !  "RES_MAT_INTERFACE_RAA" pour réflexion air -> air
				  !  "RES_MAT_INTERFACE_RWW" pour réflexion mer -> mer
      				  !  "RES_MAT_INTERFACE_TAW" pour transmission air -> mer
				  !  "RES_MAT_INTERFACE_TWA" pour transmission mer -> air
				    
      CHARACTER*CTE_LENFIC2 FICRES   ! Nom complet du fichier matrice d'interface résultat.
				            
      DOUBLE PRECISION WIND	! Vitesse du vent (m/s).
      
      DOUBLE PRECISION IND	! Indice de réfraction de l'eau / air
	
      DOUBLE PRECISION RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   ! Tableau des mu de Gauss
      DOUBLE PRECISION CHR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   ! Poids de Gauss.
      DOUBLE PRECISION N1SN2	! Rapport de l'indice N1 du milieu incident
      				! sur l'indice N2 du milieu de réfraction.
				
      LOGICAL TRACE     ! = vrai si écriture dans le fichier trace

      INTEGER*4 OS_NB 	! Ordre des développements des fonctions de phase 
			! en Polynômes de Legendre, des matrices d'interface et
			! des champs de luminance.
      INTEGER*4 OS_NS   ! Ordre maximal du développement en polynômes de Legendre   
			! des éléments de la matrice de Fresnel.
      INTEGER*4 OS_NM   ! Ordre maximal du développement en séries de Fourier de la  
			! fonction G des calculs de matrice de réflexion / transmission.
			
      INTEGER*4 LUM_NBMU ! Nombre d'angles effectifs des simulations
      INTEGER*4 I	 ! Indice de tableau.
      INTEGER*4 IFIN     ! nb caractères significatifs d'une chaine
      INTEGER*4 IER      ! code d'erreur =0 si pas d'erreur, =-1 sinon


C* Initialisation du code d'erreur
C------------------------------------------------------
      IER=0

C* Contrôle du type de transition à l'interface 
C----------------------------------------------
      IF ((CINTER.NE."RAA").AND.(CINTER.NE."TAW").AND.
     &    (CINTER.NE."RWW").AND.(CINTER.NE."TWA")) GOTO 920

      IF (CINTER.EQ."RAA") THEN	!Cas de la réflexion air --> air  
          FICRES_MAT = "RES_MAT_INTERFACE_RAA"
	  FICRES_FRESNEL = "RES_FRESNEL_RAA"
	  FICRES_GSF = "RES_GSF_RAA"
	  N1SN2 = 1.D+00/IND  !milieu incident d'indice 1, milieu de réfraction d'indice IND 
      ENDIF
      
      IF (CINTER.EQ."TAW") THEN !Cas de la transmission air --> mer
          FICRES_MAT = "RES_MAT_INTERFACE_TAW"
	  FICRES_FRESNEL = "RES_FRESNEL_TAW"
	  FICRES_GSF = "RES_GSF_TAW"
	  N1SN2 = 1.D+00/IND  !milieu incident d'indice 1, milieu de réfraction d'indice IND 
      ENDIF
      
      IF (CINTER.EQ."RWW") THEN !Cas de la réflexion mer --> mer
          FICRES_MAT = "RES_MAT_INTERFACE_RWW"
	  FICRES_FRESNEL = "RES_FRESNEL_RWW"
	  FICRES_GSF = "RES_GSF_RWW"
	  N1SN2 = IND !milieu incident d'indice IND, milieu de réfraction d'indice 1
      ENDIF
      
      IF (CINTER.EQ."TWA") THEN !Cas de la transmission mer --> air 
          FICRES_MAT = "RES_MAT_INTERFACE_TWA"
	  FICRES_FRESNEL = "RES_FRESNEL_TWA"
	  FICRES_GSF = "RES_GSF_TWA"
	  N1SN2 = IND !milieu incident d'indice IND, milieu de réfraction d'indice 1
      ENDIF      
            
C* Informations pour fichier Trace 
C------------------------------------------------------
      	 
      IF (TRACE) THEN
         WRITE(IDLOG,*,err=921)	' ' 
	 WRITE(IDLOG,*,err=921) '*****************************'
         WRITE(IDLOG,*,err=921) 'OSOAA_SURFACE_CASE subroutine' 
	 WRITE(IDLOG,*,err=921) '*****************************'  
         WRITE(IDLOG,*,err=921)	 
         WRITE(IDLOG,*,err=921) 'Calculation of the file : ',FICRES
         WRITE(IDLOG,*,err=921) ' '
         WRITE(IDLOG,*,err=921)'Simulation parameters: ' 
         WRITE(IDLOG,*,err=921)'   * Type of interface : ',CINTER
         WRITE(IDLOG,*,err=921)'   * Wind velocity (m/s) ',WIND
         WRITE(IDLOG,*,err=921)'   * Sea /air refractive index: ',IND
	 WRITE(IDLOG,*,err=921) ' ' 	 	 
      END IF
        

C* Décomposition en séries de Fourier de la fonction G(Teta1,Teta2,phi).
C---------------------------------------------------------------------
      CALL OSOAA_GSF(CINTER,(1.D+00/N1SN2),LUM_NBMU,RMU,WIND,OS_NM,
     &               TRACE,IER)
      IF (IER.NE.0) GOTO 995   ! cas d'erreur
      
C* Développement en fonctions de Legendre de la matrice de Fresnel.
C------------------------------------------------------------------     
      CALL OSOAA_MAT_FRESNEL(CINTER,LUM_NBMU,RMU,CHR,N1SN2,OS_NS,
     &                       TRACE,IER)
      IF (IER.NE.0) GOTO 996   ! cas d'erreur
      
C* Calcul de la matrice de réflexion développée en séries de Fourier.
C------------------------------------------------------------------
      CALL OSOAA_MAT_INTERFACE(CINTER,LUM_NBMU,RMU,
     & 			       OS_NB,OS_NS,OS_NM,TRACE,IER)
      IF (IER.NE.0) GOTO 997   ! cas d'erreur
      
C* Réarrangement des résultats pour être compatible avec le code des OS.
C-----------------------------------------------------------------------
      CALL OSOAA_MISE_FORMAT(FICRES_MAT,FICRES,
     &                      LUM_NBMU,OS_NB,TRACE,IER)
      IF (IER.NE.0) GOTO 998   ! cas d'erreur


C* Destruction des fichiers temporaires
C---------------------------------------   
      OPEN(UNIT=3,FILE=FICRES_GSF,FORM='UNFORMATTED',
     &     STATUS='OLD',ERR=1000) 
      CLOSE(3,STATUS='DELETE',ERR=1001)     
      OPEN(UNIT=3,FILE=FICRES_FRESNEL,STATUS='OLD',ERR=1010) 
      CLOSE(3,STATUS='DELETE',ERR=1011)       
      OPEN(UNIT=3,FILE=FICRES_MAT,FORM='UNFORMATTED',
     &     STATUS='OLD',ERR=1020)
      CLOSE(3,STATUS='DELETE',ERR=1021) 
      
      
C* Fin nominale 
C-------------------
      GOTO 9999
      
      
 
C* Cas d'erreur et retour du status -1 au programme appelant
C----------------------------------------------
  920 WRITE(6,*) '  OSOAA_SURFACE_CASE : ERROR_920 '
      WRITE(6,*) '     on mode interface definition'
      GOTO 9998
      
  921 WRITE(6,*) '  OSOAA_SURFACE_CASE : ERROR_921 on logfile writing'
      GOTO 9998   
        
  995 WRITE(6,*) '  OSOAA_SURFACE_CASE : ERROR_995  '
      WRITE(6,*) '      on subroutine OSOAA_GSF'
      WRITE(6,*) '      for case : ', CINTER
      GOTO 9998     
      
  996 WRITE(6,*) '  OSOAA_SURFACE_CASE : ERROR_996  '
      WRITE(6,*) '      on subroutine OSOAA_MAT_FRESNEL'
      WRITE(6,*) '      for case : ', CINTER
      GOTO 9998
      
  997 WRITE(6,*) '  OSOAA_SURFACE_CASE : ERROR_997  '
      WRITE(6,*) '      on subroutine OSOAA_MAT_INTERFACE'
      WRITE(6,*) '      for case : ', CINTER
      GOTO 9998
  
  998 WRITE(6,*) '  OSOAA_SURFACE_CASE : ERROR_998  '
      WRITE(6,*) '      on subroutine OSOAA_MISE_FORMAT'
      WRITE(6,*) '      for case : ', CINTER
      GOTO 9998       
           
 1000 WRITE(6,*) '  OSOAA_SURFACE_RAA : ERROR_1000  '
      WRITE(6,*) '      on file opening :', FICRES_GSF
      GOTO 9998      

 1001 WRITE(6,*) '  OSOAA_SURFACE_RAA : ERROR_1001  '
      WRITE(6,*) '      on file deleting :', FICRES_GSF
      GOTO 9998  
               
 1010 WRITE(6,*) '  OSOAA_SURFACE_RAA : ERROR_1010  '
      WRITE(6,*) '      on file opening :', FICRES_FRESNEL
      GOTO 9998      

 1011 WRITE(6,*) '  OSOAA_SURFACE_RAA : ERROR_1011  '
      WRITE(6,*) '      on file deleting :', FICRES_FRESNEL
      GOTO 9998      
               
 1020 WRITE(6,*) '  OSOAA_SURFACE_RAA : ERROR_1020  '
      WRITE(6,*) '      on file opening :', FICRES_MAT
      GOTO 9998      

 1021 WRITE(6,*) '  OSOAA_SURFACE_RAA : ERROR_1021  '
      WRITE(6,*) '      on file deleting :', FICRES_MAT
      GOTO 9998  	       	       
	       
	       
 9998 IER=-1             
 9999 CONTINUE

C* Format
C---------   
 2    FORMAT(2(D21.14)) 
                                                      
      RETURN
      END    !FIN DU PROGRAMME OSOAA_SURFACE_CASE  





C==============================================================================
C PROCEDURE: OSOAA_MAT_FRESNEL
C ==========
C      Cette procédure calcule le développement de la matrice de Fresnel
C      dans la base des polynômes de Legendre.
C      ==> Correspond au développement de la matrice exprimée en 
C          fonction de l'angle de diffusion.
C      ==> Introduit une pondération de la matrice de Fresnel à proprement parlé
C          par un coefficient C défini ci-dessous (valeur théorique en relation
C          avec le rapport des variations d'angle solide autour de la normale 
C          au dioptre et autour de la direction de transmission). 
C          Applicable au cas des transmissions.
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
C           _ C = [ cos(t)**2 ] / { N * [ N*cos(i)-cos(t) ]**2 }
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
C      Le développement donne les paramètres ALPHA(k), BETA(k), GAMMA(k) et
C      ZETA(k) associés à la matrice de Fresnel.
C      Ils sont enregistrés de k=0 à OS_NS dans le fichier FICRES_FRESNEL nommé :
C  		"RES_FRESNEL_RAA" pour une réflexion air -> air
C  		"RES_FRESNEL_RWW" pour une réflexion mer -> mer
C  		"RES_FRESNEL_TAW" pour une transmission air -> mer
C  		"RES_FRESNEL_TWA" pour une transmission mer -> air
C
C
C
C
C Description des paramètres
C --------------------------
C       CINTER (CHAR*3) : (E) Chaine de 3 caractères précisant le type d'interface
C			      à simuler :
C   				une réflexion air -> air : "RAA"
C			  	une réflexion mer -> mer : "RWW"
C				une transmission air -> mer : "TAW"
C				une transmission mer -> air : "TWA"
C       LUM_NBMU (I4) : (E) Nombre d'angles (positifs) effectivement utiles.
C	RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double) : (E) Tableau des mu de Gauss 
C                               (angles complémentaires des angles de diffusion).
C	CHR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double) : (E) Tableau des poids de Gauss.
C 	N1SN2   (double) :   (E) Rapport de l'indice N1 du milieu incident
C				 sur l'indice N2 du milieu de réfraction.
C       OS_NS (I4) : (E) Ordre maximal à utiliser pour le développement en
C                        polynômes de Legendre des éléments de la matrice de Fresnel.
C       TRACE (logical)  : (E) = vrai si écriture dans le fichier trace
C                               (No d'unité logique du fichier trace =99)
C       IER  (I4)  :  (S)  code d'erreur =0 si pas d'erreur, =-1 sinon 
C
C
C
C Résultats fournis par la procédure
C ----------------------------------
C
C      Fichier temporaire FICRES_FRESNEL de stockage des coefficients de décomposition
C      de la matrice de Fresnel.
C
C      Informations dans le fichier de trace (optionnel)
C         (No d'unité logique du fichier trace = IDLOG =INCTE_IDLOG_SURFACE , 
C          fichier créé/ouvert par le programme appelant)
C
C 
C Description des fichiers utilisés
C ---------------------------------
C   -->Contenu du fichier temporaire FICRES_FRESNEL
C      Fichier Ascii, format d'un enregistrement 4E15.8.
C      Fichier créé dans le répertoire de lancement du programme
C      Si un fichier de même nom existe déjà, il est écrasé.
C      Nombre d'enregistrements : de k=0 à OS_NS
C      Chaque enregistrement contient les paramètres ALPHA(k), BETA(k), 
C      GAMMA(k) et ZETA(k) associés à la matrice de Fresnel
C
C
C Common utilisé:
C --------------
C     Aucun
C
C
C Cas d'erreur :
C ------------
C     - Erreur dans un sous-programme
C     - Erreur de définition du type d'interface
C     - Erreur d'écriture dans le fichier Trace
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant
C
C
C  Sous programmes utilisés:
C --------------------------
C  Cette procédure fait appel à la routine:
C      - OSOAA_SURF_DECOMP_LEGENDRE
C
C==============================================================================
      SUBROUTINE OSOAA_MAT_FRESNEL(CINTER,LUM_NBMU,RMU,CHR,N1SN2,OS_NS,
     &                             TRACE,IER)

      IMPLICIT NONE

C Constantes 
C -----------------            
      INTEGER*2 IDLOG			! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_SURFACE)
      
C* Définition des variables                    
C*----------------------------------------------------------------------
      DOUBLE PRECISION RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      				! Tableau des mu de Gauss 
      				! (angles complémentaires des angles de diffusion).
      DOUBLE PRECISION CHR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      				! Tableau des poids de Gauss.

      DOUBLE PRECISION COSDEV	! Cosinus de l'angle de déviation
      
      DOUBLE PRECISION COS_I	! Cosinus de l'angle d'incidence.
      DOUBLE PRECISION SIN2_I	! Sinus carré de l'angle d'incidence.
      DOUBLE PRECISION COS_T	! Cosinus de l'angle réfracté.
      DOUBLE PRECISION COS2_T	! Cosinus carré de l'angle réfracté.
      DOUBLE PRECISION SIN2_T	! Sinus carré de l'angle réfracté.
      DOUBLE PRECISION N1SN2	! Rapport de l'indice N1 du milieu incident
      				! sur l'indice N2 du milieu de réfraction.		
      DOUBLE PRECISION CL	! Coefficient de réflexion ou transmission parallèle.
      DOUBLE PRECISION CR	! Coefficient de réflexion ou transmission perpendiculaire.
      
      DOUBLE PRECISION COEFT	! Coefficient de pondération de la matrice de Fresnel
      				! en transmission.
      
      DOUBLE PRECISION F11(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
                                ! Elément R11 de la matrice de Fresnel en réflexion
      				! ou transmission en fonction de l'angle de réflexion.
      DOUBLE PRECISION F12(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)     
      				! Elément R12 de la matrice de Fresnel.
      DOUBLE PRECISION F33(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)    
      				! Elément R33 de la matrice de Fresnel


      LOGICAL TRACE        ! = vrai si écriture dans le fichier trace

      INTEGER*4 LUM_NBMU ! Nombre d'angles effectifs des simulations

      INTEGER*4 OS_NS   ! Ordre maximal du développement en polynômes de Legendre   
			! des éléments de la matrice de Fresnel.
      INTEGER*4 IER     ! code d'erreur =0 si pas d'erreur, =-1 sinon  
      INTEGER*4 J	! Indice de tableau.
      

      CHARACTER*3 CINTER	! Chaine de caractères précisant le cas d'interface 
      				!   une réflexion air -> air : "RAA"
				!   une réflexion mer -> mer : "RWW"
				!   une transmission air -> mer : "TAW"
				!   une transmission mer -> air : "TWA"
				
      CHARACTER*15 FICRES_FRESNEL ! Nom du fichier de Fresnel produit
      				  !  "RES_FRESNEL_RAA" pour une réflexion air -> air
				  !  "RES_FRESNEL_RWW" pour une réflexion mer -> mer
      				  !  "RES_FRESNEL_TAW" pour une transmission air -> mer
				  !  "RES_FRESNEL_TWA" pour une transmission mer -> air

      LOGICAL CONFIG_STATUS     ! Status possible (TRUE) ou impossible (FALSE) de la 
      				! configuration géométrique (incidence - transmission).

      INTEGER ICONFIG_STATUS    ! Indicateur numérique de la possibilité ou non d'un cas 
      				! de configuration géométrique (incidence - transmission):
				! 1 si status possible, 0 sinon.
      
      
      
      
C* Contrôle du type de transition à l'interface 
C----------------------------------------------
      IF ((CINTER.NE."RAA").AND.(CINTER.NE."TAW").AND.
     &    (CINTER.NE."RWW").AND.(CINTER.NE."TWA")) GOTO 920

      IF (CINTER.EQ."RAA") FICRES_FRESNEL = "RES_FRESNEL_RAA"
      IF (CINTER.EQ."TAW") FICRES_FRESNEL = "RES_FRESNEL_TAW"
      IF (CINTER.EQ."RWW") FICRES_FRESNEL = "RES_FRESNEL_RWW"
      IF (CINTER.EQ."TWA") FICRES_FRESNEL = "RES_FRESNEL_TWA"
      
C* Fichier trace 
C-----------------
      IF (TRACE) THEN
         WRITE(IDLOG,*,err=921)	' ' 
         WRITE(IDLOG,*,err=921) 'OSOAA_MAT_FRESNEL' 
	 WRITE(IDLOG,*,err=921) '-----------------'  
         WRITE(IDLOG,*,err=921)	 
         WRITE(IDLOG,*,err=921) 
     &	    'Calculation of coefficients of the Fresnel matrix ' //
     &	    'expansion into Legendre functions' 
         WRITE(IDLOG,*,err=921) ' '	 
         WRITE(IDLOG,*,err=921)'Simulation parameters: '
         WRITE(IDLOG,*,err=921)'   * Type of interface : ',CINTER
	 WRITE(IDLOG,*,err=921)'   * Ratio ind N1 / N2 :',N1SN2
	 WRITE(IDLOG,*,err=921),
     &	    'File of Fresnel matrix to be created : ',FICRES_FRESNEL
	 WRITE(IDLOG,*,err=921) ' ' 	 	 
      END IF
      
     
	 			
C* Calcul des éléments de la matrice de réflexion de Fresnel	
C--------------------------------------------------------------
      DO 104 J=-LUM_NBMU,LUM_NBMU
         IF(J.EQ.0) GOTO 104

C* Cosinus de l'angle de déviation :
C-------------------------------------------------------------	 	
	 COSDEV=RMU(J)  	     
	
C* Cosinus de l'angle de réflexion (angle d'incidence / normale au dioptre) :
C* Angle de déviation = PI - 2 * angle de réflexion
C*
C* Cosinus de l'angle d'incidence calculé en fonction du cosinus de l'angle
C* de déviation	
C----------------------------------------------------------------------------

         IF ((CINTER.EQ."RAA").OR.(CINTER.EQ."RWW")) THEN
         
	    COS_I=DSQRT(.5*(1-COSDEV))
            CONFIG_STATUS=.TRUE.
	 
         ELSE

            CALL OSOAA_CALC_COSI_TRANS(1.D+00/N1SN2,COSDEV,
     & 		  	               CONFIG_STATUS,COS_I)
     
         ENDIF
 
         ICONFIG_STATUS=1
     	 IF (CONFIG_STATUS.EQV..TRUE.) THEN
	    SIN2_I= 1.D+00-COS_I*COS_I

C* Cosinus de l'angle de réfraction (angle / normale au dioptre) :
C---------------------------------------------------------------------
	    SIN2_T = N1SN2*N1SN2*SIN2_I
	 
	    IF (SIN2_T.LE.1.D+00) THEN
	 
	        COS2_T = 1.D+00 - SIN2_T
                COS_T = DSQRT(COS2_T)

C* Coefficients de réflexion parallèle et perpendiculaire
C--------------------------------------------------------
	        CL= (COS_I - N1SN2*COS_T) / (COS_I + N1SN2*COS_T)
	        CR= (N1SN2*COS_I - COS_T) / (N1SN2*COS_I + COS_T)

C* Coefficients de réflexion ou transmission parallèle et perpendiculaire
C------------------------------------------------------------------------	 
	        COEFT = 1.D+00
	        IF ((CINTER.EQ."TAW").OR.(CINTER.EQ."TWA")) THEN    
	           IF (COS_I.NE.0.) THEN
		      COEFT = COS2_T  / N1SN2
		      COEFT = COEFT  / (N1SN2*COS_I - COS_T)
		      COEFT = COEFT  / (N1SN2*COS_I - COS_T)  	
		   ELSE
		      !Cas d'une incidence à 90 degrés ==> Pas de transmission
		      COEFT = 0.
		   ENDIF   
	           CL= N1SN2 * (1.D+00+CL)
	           CR= 1.D+00 + CR
	        ENDIF 
	 	
C* Eléments de la matrice de Fresnel en réflexion ou transmission
C----------------------------------------------------------------	  	   		
                
		F11(J)=.5*(CL*CL+CR*CR) * COEFT
                F12(J)=.5*(CL*CL-CR*CR) * COEFT
                F33(J)=CL*CR * COEFT
		
	    ELSE ! Gestion dépassement de l'angle limite

	        IF ((CINTER.EQ."TAW").OR.(CINTER.EQ."TWA")) THEN    
	           F11(J)=0.D+00
                   F12(J)=0.D+00
                   F33(J)=0.D+00
	        ELSE
	           F11(J)=1.D+00
                   F12(J)=0.D+00
                   F33(J)=1.D+00
	        ENDIF
	     
	    ENDIF !Fin test sur sin2_I	


     	 ELSE !Gestion des cas de transmission impossible ==> nulle
	 
	    ICONFIG_STATUS=0
	    
	    F11(J)=0.D+00
            F12(J)=0.D+00
            F33(J)=0.D+00	 
	 ENDIF
	 	 	 
C* Fichier trace 
C-----------------
         IF (TRACE) THEN
	   WRITE(IDLOG,200,err=921) 
     &           ' Refracted angle :',DACOS(COSDEV)*180./INCTE_PI,
     &           ' Incidence angle :',DACOS(COS_I)*180./INCTE_PI,
     &           ' F11 ',F11(J),' F12 ',F12(J),' F33 ',F33(J),
     &           ' COEFT ', COEFT, ' CONFIG_STATUS :',ICONFIG_STATUS
         END IF
	 
 104  CONTINUE


C* Calcul des paramètres ALPHA(k),BETA(k),GAMMA(k),ZETA(k) 
C* de la décomposition en fonction de Legendre des éléments
C* de la matrice de Fresnel     
      CALL OSOAA_SURF_DECOMP_LEGENDRE(LUM_NBMU,RMU,CHR,
     & 				      F11,F12,F33,OS_NS,
     &                                FICRES_FRESNEL,TRACE,IER)

      IF (IER.NE.0) GOTO 993   ! cas d'erreur   
      

                           
C* Fin nominale 
C-------------------
      IER=0
      goto 9999
      
C* Cas d'erreur : retour du status -1 au programme appelant
C------------------------------------------------------
  920 WRITE(6,*) 
     & '  OSOAA_MAT_FRESNEL : ERROR_920 on mode interface def'
      GOTO 9998
      
  921 WRITE(6,*) '  OSOAA_MAT_FRESNEL : ERROR_921 on logfile writing'
      GOTO 9998
      
  993 WRITE(6,*) '  OSOAA_MAT_FRESNEL : ERROR_993  '
      WRITE(6,*) '      on subroutine OSOAA_SURF_DECOMP_LEGENDRE'
      GOTO 9998
  
 9998 IER=-1   
 9999 CONTINUE


   
C* Format
C---------   
  200 FORMAT(2(A17,F8.4),3(A6,E13.7),A7,F6.3,A16,I4)
  
      RETURN
      END         !FIN DE LA PROCEDURE OSOAA_MAT_FRESNEL







C==============================================================================
C PROCEDURE: OSOAA_SURF_DECOMP_LEGENDRE
C ==========
C      Cette procédure calcule le développement de la matrice de Fresnel
C      dans la base des polynômes de Legendre.
C      ==> Correspond au développement de la matrice exprimée en 
C          fonction de l'angle de diffusion.
C			
C      La matrice de Fresnel, exprimée pour un repère lié au plan de réflexion,
C      s'écrit :
C                  | F11(teta)  F12(teta)    0      |
C         F(teta)= | F12(teta)  F11(teta)    0      |
C                  |    0          0      F33(teta) |
C
C         avec teta : angle de déviation du rayonnement
C
C      Le développement donne les paramètres ALPHA(k), BETA(k), GAMMA(k) et
C      ZETA(k) associés à la matrice de Fresnel.
C      Ils sont enregistrés de k=0 à OS_NS dans le fichier FICRES.
C
C
C Description des paramètres
C --------------------------
C       LUM_NBMU (I4) : (E) Nombre d'angles (positifs) effectivement utiles.
C	RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double) : (E) Tableau des mu de Gauss 
C                               				(angles de déviation).
C	CHR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double) : (E) Tableau des poids de Gauss.
C	
C	F11(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double) : (E) 	
C			Tableau des valeurs de l'élément F11 de la matrice de Fresnel
C	                en fonction de l'angle de déviation (donné par RMU).
C	F12(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double) : (E)      
C	      		Tableau des valeurs de l'élément F12 de la matrice de Fresnel.
C	F33(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double) : (E)     
C	      		Tableau des valeurs de l'élément F33 de la matrice de Fresnel
C	
C       OS_NS (I4) : (E) Ordre maximal à utiliser pour le développement en
C                        polynômes de Legendre des éléments de la matrice de Fresnel.
C       TRACE (logical)  : (E) = vrai,si écriture dans le fichier trace
C                               (No d'unité logique du fichier trace =99)
C
C       FICRES (CHAR*15)  (E) :  Nom du fichier résultat
C                 		(sur 15 caractères)
C
C       IER  (I4)  :  (S)  code d'erreur =0 si pas d'erreur, =-1 sinon 
C
C
C
C Résultats fournis par la procédure
C ----------------------------------
C
C      Fichier temporaire FICRES de stockage des coefficients de décomposition
C      de la matrice de Fresnel.
C
C      Informations dans le fichier de trace (optionnel)
C         (No d'unité logique du fichier trace = IDLOG =INCTE_IDLOG_SURFACE , 
C          fichier créé/ouvert par le programme appelant)
C
C 
C Description des fichiers utilisés
C ---------------------------------
C   -->Contenu du fichier temporaire FICRES
C      Fichier Ascii, format d'un enregistrement 4E15.8.
C      Fichier créé dans le répertoire de lancement du programme.
C      Si un fichier de même nom existe déjà, il est écrasé.
C      Nombre d'enregistrements : de k=0 à OS_NS
C      Chaque enregistrement contient les paramètres ALPHA(k), BETA(k), 
C      GAMMA(k) et ZETA(k) associés à la matrice de Fresnel
C
C
C Common utilisé:
C --------------
C     Aucun
C
C
C Cas d'erreur :
C ------------
C     -Erreur à l'ouverture d'un fichier
C     -Erreur à la lecture/écriture dans un fichier
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant
C
C
C==============================================================================
      SUBROUTINE OSOAA_SURF_DECOMP_LEGENDRE(LUM_NBMU,RMU,CHR,
     & 					    F11,F12,F33,OS_NS,
     &                                      FICRES,TRACE,IER)

      IMPLICIT NONE

C Constantes 
C -----------------            
      INTEGER*2 IDLOG			! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_SURFACE)
      
C* Définition des variables                    
C*----------------------------------------------------------------------
      DOUBLE PRECISION RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      				! Tableau des mu de Gauss 
      				! (angles complémentaires des angles de diffusion).
      DOUBLE PRECISION CHR(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      				! Tableau des poids de Gauss.
	
      DOUBLE PRECISION ALPHA(0:CTE_OS_NS_MAX)   ! Paramètres alpha(k).
      DOUBLE PRECISION BETA(0:CTE_OS_NS_MAX)	! Paramètres beta(k).
      DOUBLE PRECISION GAMMA(0:CTE_OS_NS_MAX)   ! Paramètres gamma(k).
      DOUBLE PRECISION ZETA(0:CTE_OS_NS_MAX)	! Paramètres zeta(k).
      DOUBLE PRECISION DELTA(0:CTE_OS_NS_MAX)	! Paramètres delta(k).

      DOUBLE PRECISION F11(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
                                ! Elément F11 de la matrice de Fresnel
      				! en fonction de l'angle de déviation.
      DOUBLE PRECISION F12(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)     
      				! Elément F12 de la matrice de Fresnel.
      DOUBLE PRECISION F33(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)    
      				! Elément F33 de la matrice de Fresnel

      DOUBLE PRECISION IT	! Valeur de F11 recalculée à partir de sa
      				! décomposition en BETA(k).
      DOUBLE PRECISION QT	! Valeur de F12 recalculée à partir de sa
      				! décomposition en GAMMA(k).	
      DOUBLE PRECISION UT	! Valeur de F33 recalculée à partir de sa
      				! décomposition en DELTA(k).			

      DOUBLE PRECISION PL(-1:CTE_OS_NS_MAX+1)
                      !Polynômes de Legendre à l'ordre L calculé pour un angle
      DOUBLE PRECISION POL(0:CTE_OS_NS_MAX+1)
                      !Fonction de Legendre d'ordre S=2 et d'ordre L variable
		      !(calculée pour un angle)
		       			
      DOUBLE PRECISION D	!Variable pour le calcul de POL.
      DOUBLE PRECISION E	!Variable pour le calcul de POL.

      DOUBLE PRECISION X
      DOUBLE PRECISION XRMU	!Valeur utilisée du tableau des angles de Gauss

      DOUBLE PRECISION X2				
      DOUBLE PRECISION XX	
      DOUBLE PRECISION XXX

      DOUBLE PRECISION CO1     !Variable pour le calcul des ALPHA(k) et ZETA(k)
      DOUBLE PRECISION CO2     !Variable pour le calcul des ALPHA(k) et ZETA(k)
      DOUBLE PRECISION CO3     !Variable pour le calcul des ALPHA(k) et ZETA(k)

      DOUBLE PRECISION SOM1    !Variable pour le calcul des ALPHA(k) et ZETA(k)
      DOUBLE PRECISION SOM2    !Variable pour le calcul des ALPHA(k) et ZETA(k)
      DOUBLE PRECISION SOM3    !Variable pour le calcul des ALPHA(k) et ZETA(k)
      DOUBLE PRECISION SOM4    !Variable pour le calcul des ALPHA(k) et ZETA(k)
      
      DOUBLE PRECISION COS_I   !Cosinus de l'angle d'incidence
      DOUBLE PRECISION ANG_INC !Angle d'incidence (degrés)
      DOUBLE PRECISION ANG_DEV !Angle de déviation (degrés)
      DOUBLE PRECISION ECART   !Ecart (F-Frecomp) entre la valeur F d'une fonction de 
      			       !Fresnel et sa valeur recomposée Frecomp 
			       !(avec ses coefficients en fonctions de Legendre)
      DOUBLE PRECISION ECART_REL  	 !Même chose en relatif (F-Frecomp)/F
      DOUBLE PRECISION MIN_EC		 !Valeur minimale de l'écart  F-Frecomp
      DOUBLE PRECISION MAX_EC		 !Valeur maximale de l'écart  F-Frecomp
      DOUBLE PRECISION INTEGR_ABS_EC	 !Intégrale de l'écart absolu     
      DOUBLE PRECISION MIN_EC_REL	 !Valeur minimale de l'écart relatif (F-Frecomp)/F
      DOUBLE PRECISION MAX_EC_REL	 !Valeur maximale de l'écart relatif (F-Frecomp)/F
      DOUBLE PRECISION INTEGR_ABS_EC_REL !Intégrale de l'écart relatif absolu 
      DOUBLE PRECISION VARF     	 !Somme des écarts au carré (F-Frecomp)**2
      					 !normalisée par le nb d'angles 

      LOGICAL TRACE        ! = vrai si écriture dans le fichier trace

      INTEGER*4 LUM_NBMU ! Nombre d'angles effectifs des simulations

      INTEGER*4 OS_NS   ! Ordre maximal du développement en polynômes de Legendre   
			! des éléments de la matrice de Fresnel.
      INTEGER*4 IER     ! Code d'erreur =0 si pas d'erreur, =-1 sinon  
      INTEGER*4 J	! Indice de tableau.
      INTEGER*4 K	! Indice de tableau.
      INTEGER*4	NN
      INTEGER*4	MM
      INTEGER*4 I	
      INTEGER*4 NB_GAUSS    ! Nb d'angle de Gauss
      INTEGER*4 ANG_GAUSS   ! Indicateur si angle = angle de Gauss (1 si oui, 0 si non)

      CHARACTER*15 FICRES   ! Nom du fichier résultat à produire



C* Ouverture du fichier résultat
C---------------------------------
      OPEN(UNIT=3,FILE=FICRES,ERR=991)

C* Initilisation
C---------------------------------	
      DO 101 K=0,OS_NS
         BETA(K)=0.D+00
         GAMMA(K)=0.D+00
         ALPHA(K)=0.D+00
         ZETA(K)=0.D+00
         DELTA(K)=0.D+00
  101 CONTINUE
  
C* Fichier trace 
C-----------------
      IF (TRACE) THEN
         WRITE(IDLOG,*,err=921)	' ' 
         WRITE(IDLOG,*,err=921) 'OSOAA_SURF_DECOMP_LEGENDRE' 
	 WRITE(IDLOG,*,err=921) '--------------------------'  
         WRITE(IDLOG,*,err=921)	 
         WRITE(IDLOG,*,err=921) 
     &	 'Fresnel matrix expansion into Legendre functions' 
         WRITE(IDLOG,*,err=921)
	 WRITE(IDLOG,*,err=921) '--> Result file : ', FICRES
      END IF
			


C* Calcul des paramètres BETA(k) de la décomposition de la fonction
C* F11(teta) en polynômes de Legendre.
C------------------------------------------------------------------
      DO 4 J=-LUM_NBMU,LUM_NBMU
         IF(J.EQ.0) GO TO 4
         X=F11(J)*CHR(J)
         XRMU=RMU(J)

C* Calcul des polynômes de Legendre d'ordre K, pour la valeur de mu fixée.
C------------------------------------------------------
         PL(-1)=0.D+00
         PL(0)=1.D+00
         DO 5 K=0,OS_NS
            PL(K+1)=((2*K+1.)*XRMU*PL(K)-K*PL(K-1))/(K+1.)
            BETA(K)=BETA(K)+X*PL(K)
    5    CONTINUE
    4 CONTINUE

      DO 6 K=0,OS_NS
           BETA(K)=(2*K+1)*BETA(K)*.5
    6 CONTINUE
    
C* Fichier trace 
C-----------------
      IF (TRACE) THEN
           WRITE(IDLOG,*,err=921) '  '
	   WRITE(IDLOG,*,err=921) ' Recombination of F11'
      
C* Contrôle de la recombinaison de F11  
C--------------------------------------
           MIN_EC=+9.99D+99
	   MAX_EC=-9.99D+99
	   INTEGR_ABS_EC=0.D+00
	   MIN_EC_REL=+9.99D+99
	   MAX_EC_REL=-9.99D+99
	   INTEGR_ABS_EC_REL=0.D+00
	   VARF=0.D+00
	   NB_GAUSS=0
	   
           DO 8 J=-LUM_NBMU,LUM_NBMU
              IT=0.D+00
              IF(J.EQ.0) GO TO 8
              XRMU=RMU(J)
              PL(-1)=0.D+00
              PL(0)=1.D+00
              DO 9 K=0,OS_NS
                PL(K+1)=((2*K+1.)*XRMU*PL(K)-K*PL(K-1))/(K+1.)
                IT=IT+BETA(K)*PL(K)
    9         CONTINUE
       	      
	      ECART=F11(J)-IT
	      ECART_REL=ECART/F11(J)	      
	      ANG_GAUSS=0
	      IF (CHR(J).NE.0.D+00) THEN
	         IF (ECART.LT.MIN_EC) MIN_EC=ECART
	         IF (ECART.GT.MAX_EC) MAX_EC=ECART
	         INTEGR_ABS_EC= INTEGR_ABS_EC + DABS(ECART)*CHR(J)
		 IF (ECART_REL.LT.MIN_EC_REL) MIN_EC_REL=ECART_REL
	         IF (ECART_REL.GT.MAX_EC_REL) MAX_EC_REL=ECART_REL
	         INTEGR_ABS_EC_REL = INTEGR_ABS_EC_REL 
     &		 			+ DABS(ECART_REL)*CHR(J)
                 VARF = VARF + ECART*ECART
		 NB_GAUSS = NB_GAUSS+1	
		 ANG_GAUSS=1	
	      ENDIF
	      
	      ANG_DEV=DACOS(XRMU)*180./INCTE_PI
	      
	      WRITE(IDLOG,203,err=921) ' Dev ', ANG_DEV,
     &	                    ' F11 ',F11(J),
     &	                    ' F11 recomb  ',IT , 
     &                      ' Ecart rel (%) ', 100.*(F11(J)-IT)/F11(J),
     &                      '  Ang gauss ',  ANG_GAUSS
    8      CONTINUE
    
           WRITE(IDLOG,*,err=921)
     &     '==> Error budget on gauss angles: F11'
           WRITE(IDLOG,210,err=921)' F11 Min : ',MIN_EC, 
     &	   			   ' | Max : ',MAX_EC,
     &	   			   ' | Abs integral : ',INTEGR_ABS_EC
           WRITE(IDLOG,212,err=921)' F11 Min rel : ',MIN_EC_REL, 
     &			' | Max rel : ',MAX_EC_REL,
     &	   		' | Abs integral rel : ',INTEGR_ABS_EC_REL
	   WRITE(IDLOG,213,err=921)' F11 RMS     : ',VARF
       
      END IF !Fichier Trace
      
C* Calcul des paramètres GAMMA(k) et DELTA(k) de la décomposition des fonctions
C* F12 et F33 en polynômes ou fonctions de Legendre.
C-----------------------------------------------------------------------------
      DO 11 J=-LUM_NBMU,LUM_NBMU
         IF(J.EQ.0)  GO TO 11
         XXX=CHR(J)*F12(J)
         XX=CHR(J)*F33(J)
         POL(0)=0.D+00
         POL(1)=0.D+00
         XRMU=RMU(J)
         PL(-1)=0.D+00
         PL(0)=1.D+00
         POL(2)=3.*(1.-XRMU**2)/2./DSQRT(6.0D+00)
	
         DO 12 K=2,OS_NS
            D=(2.*K+1.)/DSQRT(1.D+00*(K+3.)*(K-1.))
            E=DSQRT(1.D+00*(K+2.)*(K-2.))/(2.*K+1.)
            POL(K+1)=D*(XRMU*POL(K)-E*POL(K-1))
            GAMMA(K)=GAMMA(K)+XXX*POL(K)
  12     CONTINUE

         DO 14 K=0,OS_NS
            PL(K+1)=((2.*K+1.)*XRMU*PL(K)-K*PL(K-1))/(K+1.)
            DELTA(K)=DELTA(K)+XX*PL(K)
  14     CONTINUE

   11 CONTINUE

      DO 15 K=0,OS_NS
         DELTA(K)=DELTA(K)*(2.*K+1.)*.5
         GAMMA(K)=GAMMA(K)*(2.*K+1.)*.5
   15 CONTINUE



C* Fichier trace 
C-----------------
      IF (TRACE) THEN
           WRITE(IDLOG,*,err=921) '  '
	   WRITE(IDLOG,*,err=921) ' Recombination of F12'
      
C* Contrôle de la recombinaison de F12  
C--------------------------------------
           MIN_EC=+9.99D+99
	   MAX_EC=-9.99D+99
	   INTEGR_ABS_EC=0.D+00
	   MIN_EC_REL=+9.99D+99
	   MAX_EC_REL=-9.99D+99
	   INTEGR_ABS_EC_REL=0.D+00
	   VARF=0.D+00
	   NB_GAUSS=0
	   
           DO 81 J=-LUM_NBMU,LUM_NBMU
              QT=0.D+00
	      
	      IF(J.EQ.0)  GO TO 81
                 POL(0)=0.D+00
                 POL(1)=0.D+00
                 XRMU=RMU(J)
                 POL(2)=3.*(1.-XRMU**2)/2./DSQRT(6.0D+00)
   
                 DO 91 K=2,OS_NS
		    D=(2.*K+1.)/DSQRT(1.D+00*(K+3.)*(K-1.))
                    E=DSQRT(1.D+00*(K+2.)*(K-2.))/(2.*K+1.)
		    POL(K+1)=D*(XRMU*POL(K)-E*POL(K-1))
		    QT=QT+GAMMA(K)*POL(K)
  91             CONTINUE


	         ECART=F12(J)-QT
	         ECART_REL=ECART/F12(J)	      
	         ANG_GAUSS=0
	         IF (CHR(J).NE.0.D+00) THEN
	            IF (ECART.LT.MIN_EC) MIN_EC=ECART
	            IF (ECART.GT.MAX_EC) MAX_EC=ECART
	            INTEGR_ABS_EC= INTEGR_ABS_EC + DABS(ECART)*CHR(J)
		    IF (ECART_REL.LT.MIN_EC_REL) MIN_EC_REL=ECART_REL
	            IF (ECART_REL.GT.MAX_EC_REL) MAX_EC_REL=ECART_REL
	            INTEGR_ABS_EC_REL = INTEGR_ABS_EC_REL 
     &		 			+ DABS(ECART_REL)*CHR(J)
     	 	    VARF = VARF + ECART*ECART
		    NB_GAUSS = NB_GAUSS+1
                    ANG_GAUSS=1	
	         ENDIF
	      
	          
                 ANG_DEV=DACOS(XRMU)*180./INCTE_PI
	         WRITE(IDLOG,203,err=921)' Dev ', ANG_DEV,
     &					 ' F12 ',F12(J),
     &	                                 ' F12 recomb  ',QT, 
     &                      ' Ecart rel (%) ', 100.*(F12(J)-QT)/F12(J),
     &                      '  Ang gauss ',  ANG_GAUSS 
  81       CONTINUE

           VARF = VARF / NB_GAUSS
	   
           WRITE(IDLOG,*,err=921)
     &     '==> Error budget on gauss angles: F12'
           WRITE(IDLOG,210,err=921)' F12 Min : ',MIN_EC, 
     &	   			   ' | Max : ',MAX_EC,
     &	   			   ' | Abs integral : ',INTEGR_ABS_EC
           WRITE(IDLOG,212,err=921)' F12 Min rel : ',MIN_EC_REL,
     &			' | Max rel : ',MAX_EC_REL,
     &	   		' | Abs integral rel : ',INTEGR_ABS_EC_REL
	   WRITE(IDLOG,213,err=921)' F12 RMS     : ',VARF 
   
C* Contrôle de la recombinaison de F33  
C--------------------------------------
           WRITE(IDLOG,*,err=921) '  '
	   WRITE(IDLOG,*,err=921) ' Recombinaison de F33'

           MIN_EC=+9.99D+99
	   MAX_EC=-9.99D+99
	   INTEGR_ABS_EC=0.D+00
	   MIN_EC_REL=+9.99D+99
	   MAX_EC_REL=-9.99D+99
	   INTEGR_ABS_EC_REL=0.D+00
	   VARF=0.D+00
	   NB_GAUSS=0
	   	   
           DO 82 J=-LUM_NBMU,LUM_NBMU
	      UT=0.D+00
	      
	      IF(J.EQ.0)  GO TO 82
                 PL(-1)=0.D+00
                 PL(0)=1.D+00
		 XRMU=RMU(J)

                 DO 92 K=0,OS_NS
                    PL(K+1)=((2*K+1.)*XRMU*PL(K)-K*PL(K-1))/(K+1.)
                    UT=UT+DELTA(K)*PL(K)
  92             CONTINUE

	         ECART=F33(J)-UT
	         ECART_REL=ECART/F33(J)	      
	         ANG_GAUSS=0
	         IF (CHR(J).NE.0.D+00) THEN
	            IF (ECART.LT.MIN_EC) MIN_EC=ECART
	            IF (ECART.GT.MAX_EC) MAX_EC=ECART
	            INTEGR_ABS_EC= INTEGR_ABS_EC + DABS(ECART)*CHR(J)
		    IF (ECART_REL.LT.MIN_EC_REL) MIN_EC_REL=ECART_REL
	            IF (ECART_REL.GT.MAX_EC_REL) MAX_EC_REL=ECART_REL
	            INTEGR_ABS_EC_REL = INTEGR_ABS_EC_REL 
     &		 			+ DABS(ECART_REL)*CHR(J)
                    VARF = VARF + ECART*ECART
		    NB_GAUSS = NB_GAUSS+1
		    ANG_GAUSS=1	
	         ENDIF
	
                 ANG_DEV=DACOS(XRMU)*180./INCTE_PI 
	         WRITE(IDLOG,203,err=921)' Dev ',ANG_DEV,
     &				 	 ' F33 ',F33(J),
     &                                   ' F33 recomb  ',UT, 
     &                      ' Ecart rel (%) ', 100.*(F33(J)-UT)/F33(J),
     &                      '  Ang gauss ',  ANG_GAUSS 
  82       CONTINUE
  
           WRITE(IDLOG,*,err=921)
     &     '==> Error budget on gauss angles: F33'
           WRITE(IDLOG,210,err=921)' F33 Min : ',MIN_EC, 
     &	   			   ' | Max : ',MAX_EC,
     &	   			   ' | Abs integral : ',INTEGR_ABS_EC
           WRITE(IDLOG,212,err=921)' F33 Min rel : ',MIN_EC_REL,
     &			' | Max rel : ',MAX_EC_REL,
     &	   		' | Abs integral rel : ',INTEGR_ABS_EC_REL
	   WRITE(IDLOG,213,err=921)' F33 RMS     : ',VARF
       
      END IF !Fichier Trace
      
      
      
C* Calcul des paramètres ALPHA(k) et ZETA(k) en fonction des BETA(k) et DELTA(k).
C-------------------------------------------------------------------------------
      DO 16 I=2,OS_NS
         CO1=4*(2*I+1.)/FLOAT(I)/(I-1.)/(I+1.)/(I+2.)
         CO2=I*(I-1.)/((I+1.)*(I+2.))
         CO3=CO2*DELTA(I)
         CO2=CO2*BETA(I)
         NN=I*.5
         MM=(I-1)*.5
         SOM1=0.D+00
         SOM2=0.D+00
         SOM3=0.D+00
         SOM4=0.D+00
         DO 17 J=1,NN
            X2=(I-1.)*(I-1.)-3.*(2*J-1.)*(I-J)
            SOM1=SOM1+X2*BETA(I-2*J)
            SOM2=SOM2+X2*DELTA(I-2*J)
  17     CONTINUE

         DO 18 J=0,MM
            X2=(I-1.)*(I-1.)-3.*J*(2*I-2*J-1.)
            SOM3=SOM3+X2*BETA(I-2*J-1)
            SOM4=SOM4+X2*DELTA(I-2*J-1)
  18     CONTINUE

         ZETA(I)=CO3-CO1*(SOM2-SOM3)
         ALPHA(I)=CO2-CO1*(SOM1-SOM4)
  16  CONTINUE


C* Enregistrement des résultats
C---------------------------------
      DO 20 K=0,OS_NS
         WRITE(3,207,err=992)ALPHA(K),BETA(K),GAMMA(K),ZETA(K)
   20 CONTINUE



C* Fichier trace 
C-----------------
      IF (TRACE) THEN
           WRITE(IDLOG,*,err=921) '  '
	   WRITE(IDLOG,*,err=921) 'Limit order : ',OS_NS
	   WRITE(IDLOG,215,err=921)'K','ALPHA(K)','BETA(K)','GAMMA(K)',
     &                      'DELTA(I)','ZETA(K)'  
           DO 45 K=0,OS_NS
	     WRITE(IDLOG,205,err=921) K,ALPHA(K),BETA(K),GAMMA(K),
     &	                              DELTA(K),ZETA(K)
   45      CONTINUE
      END IF 
   
   
       
C* Fin nominale 
C-------------------
      IER=0
      goto 9999
      
C* Cas d'erreur : retour du status -1 au programme appelant
C------------------------------------------------------
  921 WRITE(6,*) '  OSOAA_SURF_DECOMP_LEGENDRE : ERROR_921'
      WRITE(6,*) '  --> Error on logfile writing'
      IER=-1
      goto 9999
      
  991 WRITE(6,*) '  OSOAA_SURF_DECOMP_LEGENDRE : ERROR_991'
      WRITE(6,*) '  --> Error on a file opening'
      IER=-1
      goto 9999
  
  992 WRITE(6,*) '  OSOAA_SURF_DECOMP_LEGENDRE : ERROR_992'
      WRITE(6,*) '  --> Error on a file reading/writing' 
      IER=-1 
      goto 9999

 9999 CONTINUE

C* Fermeture fichier
C-------------------
      CLOSE(3)
   
C* Format
C---------   
  207 FORMAT(4(E15.8))
  200 FORMAT(A17,F6.2,3(A5,E13.7))
  203 FORMAT(A5,F8.4,A5,E13.7,A12,E13.7,A14,F9.4,A12,I4)
  205 FORMAT(1X,I3,5(2X,E13.5))
  210 FORMAT(A11,E11.4,A9,E11.4,A18,E11.4)
  212 FORMAT(A15,E11.4,A13,E11.4,A22,E11.4)
  213 FORMAT(A15,E11.4)
  215 FORMAT(3X,A1,7X,A8,7X,A7,8X,A8,7X,A8,7X,A7)    
      RETURN
      END         !FIN DE LA PROCEDURE OSOAA_SURF_DECOMP_LEGENDRE







C==============================================================================
C PROCEDURE: OSOAA_MAT_INTERFACE
C ==========
C      Cette procédure calcule la matrice d'interface de surface 
C      à partir de la décomposition en séries de Fourier de la matrice de Fresnel
C      et de la décomposition de la fonction poids de statistiques de pentes des vagues.

C      Les angles d'incidence et de réflexion correspondent aux angles vrais
C      ==> Valeurs de 0 à 180 degrés selon les cas.
C
C	   On nomme RMU1 le cosinus de l'angle zénithal d'incidence
C	         et RMU2 le cosinus de l'angle zénithal de la direction après interaction
C					(direction de réflexion ou transmission)
C
C 	   Disposant d'une table des angles de Gauss (RMU),
C          pour laquelle on utilise les RMU > 0,
C          il faut appliquer un signe à la valeur RMU, selon les cas :
C               * Cas de la réflexion air --> air :
C			RMU1 = -RMU < 0		--> Incidence descente
C			RMU2 = +RMU > 0		--> Réflexion montante
C               * Cas de la transmission air --> mer :
C			RMU1 = -RMU < 0		--> Incidence descente
C			RMU2 = -RMU > 0		--> Transmission descente
C               * Cas de la réflexion mer --> mer :
C			RMU1 = +RMU < 0		--> Incidence montante
C			RMU2 = -RMU > 0		--> Réflexion descente
C               * Cas de la transmission mer --> air :
C			RMU1 = +RMU < 0		--> Incidence montante
C			RMU2 = +RMU > 0		--> Transmission montante
C
C
C Description des paramètres
C --------------------------
C     CINTER (CHAR*3) : (E) Chaine de 3 caractères précisant le type d'interface
C			    à simuler :
C   			     une réflexion air -> air : "RAA"
C			     une réflexion mer -> mer : "RWW"
C			     une transmission air -> mer : "TAW"
C			     une transmission mer -> air : "TWA"
C     LUM_NBMU (I4) : (E) Nombre d'angles (positifs) effectivement utiles.
C     RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double)    (E) Tableau des mu de Gauss 
C     OS_NB (I4) : (E) Ordre des développements des fonctions de phase 
C		       en Polynômes de Legendre à utiliser,
C                      des matrices d'interface et des champs de luminance.
C     OS_NS (I4) : (E) Ordre maximal à utiliser pour le développement en
C                      polynômes de Legendre des éléments de la matrice de Fresnel.
C     OS_NM (I4) : (E) Ordre maximal du développement en séries de Fourier  
C                      de la fonction G associée à la statistique de pentes des vagues.
C     TRACE  (LOGICAL)  (E)  ! = vrai si écriture dans le fichier trace. 
C     IER  (I4)   (S) : code d'erreur =0 si pas d'erreur, =-1 sinon 
C
C
C Résultats fournis par la procédure
C ----------------------------------
C
C      Fichier temporaire RES_MAT_INTERFACE de la matrice d'interface de surface
C
C      Informations dans le fichier de trace (optionnel)
C         (No d'unité logique du fichier trace = IDLOG =INCTE_IDLOG_SURFACE , 
C          fichier créé/ouvert par le programme appelant)
C
C 
C Description des fichiers utilisés
C ---------------------------------
C
C   Pour XXX = RAA, RWW, TAW ou TWA selon la valeur de CINTER :
C
C   -->Contenu du fichier temporaire RES_GSF_XXX
C       Fichier binaire non formaté, créé par OSOAA_GSF
C       Fichier créé dans le répertoire de lancement du programme
C       Chaque enregistrement contient :
C           -numéro d'angle Teta1 (valeur entière)
C           -numéro d'angle Teta2 (valeur entière)
C           -ordre maximale utile de la décomposition : IL  (valeur entière)
C           -Fonction G décomposée pour (Teta1,Teta2) : (E(J),J=0,IL)
C            (IL+1 valeurs double précision)
C
C   -->Contenu du fichier temporaire RES_FRESNEL_XXX
C      Fichier Ascii, format d'un enregistrement 4E15.8, 
C      créé par OSOAA_MAT_FRESNEL
C      Fichier créé dans le répertoire de lancement du programme
C      Nombre d'enregistrements : de k=0 à OS_NS
C      Chaque enregistrement contient les paramètres ALPHA(k), BETA(k), 
C      GAMMA(k) et ZETA(k) associés à la matrice de Fresnel
C
C   -->Contenu du fichier temporaire RES_MAT_INTERFACE_XXX
C       Fichier binaire non formaté, créé par OSOAA_MAT_INTERFACE
C       Fichier créé dans le répertoire de lancement du programme
C       Si un fichier de même nom existe déjà, il est écrasé
C       Enregistrement pour le couple (Teta1=incidence, Teta2=déviation) 
C       de la décomposition en séries de Fourier de la matrice de Fresnel:
C       M11,M12,M13,M21,M22,M23,M31,M32,M33 
C       (n valeurs réelles)
C
C
C Common utilisé:
C --------------
C    Aucun
C
C Cas d'erreur :
C ------------
C     - Erreur dans un sous-programme
C     - Erreur de définition du type d'interface
C     - Erreur d'ouverture/lecture/écriture dans un fichier
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant
C
C     
C  Routines utilisées par la procédure:
C -------------------------------------
C  Ce programme fait appel à la routine:
C         -SOS_NOYAUX_FRESNEL
C
C=============================================================================
      SUBROUTINE OSOAA_MAT_INTERFACE(CINTER,LUM_NBMU,RMU,
     &                               OS_NB,OS_NS,OS_NM,TRACE,IER)

      IMPLICIT NONE

C Constantes 
C -----------------            
      INTEGER*2 IDLOG		! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_SURFACE)
      
C* Définition des variables     
C*--------------------------------------------------------------------
      
      DOUBLE PRECISION RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)	
      				! Tableau des mu de Gauss 

      DOUBLE PRECISION RMU1	! Cosinus de l'angle zénithal d'incidence 
      				!(angle vrai  : par rapport au zénith)
      DOUBLE PRECISION RMU2	! Cosinus de l'angle zénithal de déviation 
      				!(angle vrai  : par rapport au zénith)
				
      DOUBLE PRECISION ALPHA(0:CTE_OS_NS_MAX)   ! Paramètres alpha(k).
      DOUBLE PRECISION BETA(0:CTE_OS_NS_MAX)	! Paramètres beta(k).
      DOUBLE PRECISION GAMMA(0:CTE_OS_NS_MAX)   ! Paramètres gamma(k).
      DOUBLE PRECISION ZETA(0:CTE_OS_NS_MAX)	! Paramètres zeta(k).

      DOUBLE PRECISION G(0:CTE_OS_NM_MAX)	! Fonction G décomposée en séries 
      					        ! de Fourier.

        ! Elément Pij de la matrice (de réflexion ou de transmission) de Fresnel 
	! décomposée en séries de Fourier pour un ordre IS = 0 à OS_NS.
	! Calcul pour la direction d'incidence Teta1 (RMU1)
	! et de déviation Teta2 (RMU2).
      DOUBLE PRECISION BP(0:CTE_OS_NS_MAX,2)    !   Elément P11.
      DOUBLE PRECISION GR(0:CTE_OS_NS_MAX,2)    !   Elément P12 ou P21.
      DOUBLE PRECISION GT(0:CTE_OS_NS_MAX,2)    !   Elément -P13 ou -P31.
      DOUBLE PRECISION ARR(0:CTE_OS_NS_MAX,2)   !   Elément P22.
      DOUBLE PRECISION ART(0:CTE_OS_NS_MAX,2)   !   Elément -P23 ou -P32.
      DOUBLE PRECISION ATT(0:CTE_OS_NS_MAX,2)   !   Elément P33.

      DOUBLE PRECISION R11    	! Variable de calcul du terme M11(IS,1).
      DOUBLE PRECISION R12 
      DOUBLE PRECISION R13 
      DOUBLE PRECISION R21 
      DOUBLE PRECISION R22 
      DOUBLE PRECISION R23
      DOUBLE PRECISION R31
      DOUBLE PRECISION R32
      DOUBLE PRECISION R33

      DOUBLE PRECISION X,Y	! Coefficients multiplicatifs de la matrice de Fresnel

      DOUBLE PRECISION SIGN_MU_INC  ! Facteur 1 ou -1 à appliquer au cosinus RMU pour avoir
      				    ! le cosinus de l'angle zénithal d'incidence vrai
				
      DOUBLE PRECISION SIGN_MU_DEV  ! Facteur 1 ou -1 à appliquer au cosinus RMU pour avoir 
      				    ! le cosinus de l'angle zénithal vrai de la direction
				    ! après interaction (direction de réflexion ou de
				    ! transmission)
	    
      	! Eléments Mij de la matrice d'interface en fonction de l'ordre IS
	! de la série de Fourier, pour
	!     _ une incidence sous Teta1,
	!     _ une déviation sous Teta2.
      REAL M11(0:CTE_OS_NB_MAX)		
      REAL M12(0:CTE_OS_NB_MAX)	
      REAL M13(0:CTE_OS_NB_MAX)		
      REAL M21(0:CTE_OS_NB_MAX)	
      REAL M22(0:CTE_OS_NB_MAX)		
      REAL M23(0:CTE_OS_NB_MAX)		
      REAL M31(0:CTE_OS_NB_MAX)	
      REAL M32(0:CTE_OS_NB_MAX)		
      REAL M33(0:CTE_OS_NB_MAX)	

      INTEGER*4 LUM_NBMU ! Nombre d'angles effectifs des simulations

      INTEGER*4 OS_NB 	 ! Ordre des développements des fonctions de phase 
			 ! en Polynômes de Legendre, des matrices d'interface et
			 ! des champs de luminance..
      INTEGER*4 OS_NS    ! Ordre maximal du développement en polynômes de Legendre   
			 ! des éléments de la matrice de Fresnel.
      INTEGER*4 OS_NM    ! Ordre maximal du développement en séries de Fourier  
			 ! de la fonction G des calculs de matrice d'interface.
 
      INTEGER*4 I	! Indice de l'angle Teta1.
      INTEGER*4 J	! Indice de l'angle Teta2.

      INTEGER*4 L	! Ordre de la décomposition en polynômes de Legendre.
      INTEGER*4 LIM	! Ordre maximal de la décomposition en polynômes de Legendre.
      INTEGER*4 II,II2	! Indices enregistrés avec la fonction G(IS).
      INTEGER*4 IJ	
      INTEGER*4 IS	! Ordre de la décomposition en séries de Fourier.
      INTEGER*4 K
      INTEGER*4 I1	! Valeur K+IS
      INTEGER*4 I2	! Valeur ABS(K-IS)
      INTEGER*4 IER     ! code d'erreur =0 si pas d'erreur, =-1 sinon  

      CHARACTER*3 CINTER	! Chaine de caractères précisant le cas d'interface 
      				!   une réflexion air -> air : "RAA"
				!   une réflexion mer -> mer : "RWW"
				!   une transmission air -> mer : "TAW"
				!   une transmission mer -> air : "TWA"

      CHARACTER*15 FICRES_FRESNEL ! Nom du fichier de Fresnel à utiliser
      				  !  "RES_FRESNEL_RAA" pour une réflexion air -> air
				  !  "RES_FRESNEL_RWW" pour une réflexion mer -> mer
      				  !  "RES_FRESNEL_TAW" pour une transmission air -> mer
				  !  "RES_FRESNEL_TWA" pour une transmission mer -> air

      CHARACTER*11 FICRES_GSF     ! Nom du fichier de décomposition de la fonction G
      				  ! de probabilite de pente de vagues à utiliser
      				  !  "RES_GSF_RAA" pour une réflexion air -> air
				  !  "RES_GSF_RWW" pour une réflexion mer -> mer
      				  !  "RES_GSF_TAW" pour une transmission air -> mer
				  !  "RES_GSF_TWA" pour une transmission mer -> air
				
      CHARACTER*21 FICRES_MAT     ! Nom du fichier de Matrice d'interface produit
      				  !  "RES_MAT_INTERFACE_RAA" : réflexion air -> air
				  !  "RES_MAT_INTERFACE_RWW" : réflexion mer -> mer
      				  !  "RES_MAT_INTERFACE_TAW" : transmission air -> mer
				  !  "RES_MAT_INTERFACE_TWA" : transmission mer -> air

     
      LOGICAL TRACE     ! = vrai si écriture dans le fichier trace
      
      
      
C* Contrôle du type de transition à l'interface 
C----------------------------------------------
      IF ((CINTER.NE."RAA").AND.(CINTER.NE."TAW").AND.
     &    (CINTER.NE."RWW").AND.(CINTER.NE."TWA")) GOTO 920


      IF (CINTER.EQ."RAA") THEN	!Cas de la réflexion air --> air  
          FICRES_MAT = "RES_MAT_INTERFACE_RAA"
	  FICRES_FRESNEL = "RES_FRESNEL_RAA"
	  FICRES_GSF = "RES_GSF_RAA"
	  SIGN_MU_INC = -1.D+00		!RMU1 = -RMU < 0 --> Incidence descente
	  SIGN_MU_DEV = +1.D+00		!RMU2 = +RMU > 0 --> Réflexion montante
      ENDIF
      
      IF (CINTER.EQ."TAW") THEN !Cas de la transmission air --> mer
          FICRES_MAT = "RES_MAT_INTERFACE_TAW"
	  FICRES_FRESNEL = "RES_FRESNEL_TAW"
	  FICRES_GSF = "RES_GSF_TAW"
	  SIGN_MU_INC = -1.D+00		!RMU1 = -RMU < 0 --> Incidence descente
	  SIGN_MU_DEV = -1.D+00		!RMU2 = -RMU < 0 --> Transmission descente
      ENDIF
      
      IF (CINTER.EQ."RWW") THEN !Cas de la réflexion mer --> mer
          FICRES_MAT = "RES_MAT_INTERFACE_RWW"
	  FICRES_FRESNEL = "RES_FRESNEL_RWW"
	  FICRES_GSF = "RES_GSF_RWW"
	  SIGN_MU_INC = +1.D+00		!RMU1 = +RMU > 0 --> Incidence montante
	  SIGN_MU_DEV = -1.D+00		!RMU2 = -RMU < 0 --> Réflexion descente
      ENDIF
      
      IF (CINTER.EQ."TWA") THEN !Cas de la transmission mer --> air 
          FICRES_MAT = "RES_MAT_INTERFACE_TWA"
	  FICRES_FRESNEL = "RES_FRESNEL_TWA"
	  FICRES_GSF = "RES_GSF_TWA"
	  SIGN_MU_INC = +1.D+00		!RMU1 = +RMU > 0 --> Incidence montante
	  SIGN_MU_DEV = +1.D+00		!RMU2 = +RMU > 0 --> Transmission montante
      ENDIF	  
      
      
C* Informations pour fichier Trace 
C------------------------------------------------------
      IF (TRACE) THEN
         WRITE(IDLOG,*,err=910)	' ' 
         WRITE(IDLOG,*,err=910) 'Routine OSOAA_MAT_INTERFACE' 
	 WRITE(IDLOG,*,err=910) '---------------------------'  
         WRITE(IDLOG,*,err=910) ' '
         WRITE(IDLOG,*,err=910)'Case processing '
         WRITE(IDLOG,*,err=910)
     &   '   - Type interface : ',CINTER
	 WRITE(IDLOG,*,err=910)
     &   '   - FRESNEL file : ',FICRES_FRESNEL
	 WRITE(IDLOG,*,err=910)
     &   '   - Wave slopes probability file : ',FICRES_GSF
	 WRITE(IDLOG,*,err=910)
     &   '   - Interface Matrix file to be created :', FICRES_MAT 
	 WRITE(IDLOG,*,err=910)
     &   "   - Sign applied to incidence directions :",SIGN_MU_INC	 
	 WRITE(IDLOG,*,err=910)
     &   "   - Sign applied to refracted directions:",SIGN_MU_DEV
	 WRITE(IDLOG,*,err=910) ' ' 
      END IF
            
C* Création du fichier résultat de OSOAA_MAT_INTERFACE
C---------------------------------------------------------
      OPEN(UNIT=4,FILE=FICRES_MAT,FORM='UNFORMATTED',ERR=991)


C* Lecture des paramètres de la décomposition de la matrice de Fresnel
C* en polynômes ou fonctions de Legendre.
C----------------------------------------------------------------------
      OPEN(UNIT=2,FILE=FICRES_FRESNEL,STATUS='OLD',ERR=991)

      DO 951 L=0,OS_NS
         READ(2,1300,err=992) ALPHA(L),BETA(L),GAMMA(L),ZETA(L)
  951 CONTINUE
  
      CLOSE(2)
      
C* Ouverture du fichier RES_GSF
C---------------------------------------------------------
      OPEN(UNIT=3,FILE=FICRES_GSF,FORM='UNFORMATTED',
     &     STATUS='OLD',ERR=991)

C* Boucle sur les directions d'incidence et de déviation.
C--------------------------------------------------------
      DO 25 I=1,LUM_NBMU   !Teta1 : angle zénithal d'incidence  
		
         RMU1 = SIGN_MU_INC*RMU(I)	
	 	 
         DO 2 J=1,LUM_NBMU      !Teta2 : angle zénithal après interaction 
			      
            RMU2 = SIGN_MU_DEV*RMU(J)	
	    
C* Lecture de la décomposition en séries de Fourier de la fonction G
C* pour les angles Teta1 et Teta2,avec un ordre maximal utile L.
C-------------------------------------------------------------------
            READ(3,err=992)II,II2,LIM,(G(K),K=0,LIM)
		
C* Complement du tableau a 0.
C---------------------------------
            DO 246 IJ=LIM+1,OS_NM
  246          G(IJ)=0.
  
C------------------------------------------------------------------------
C* Décomposition en séries de Fourier de la matrice de Fresnel
C* pour les directions Teta1 et Teta2
C--------------------------------------------------------------------------

C*  Calcul des fonctions BP,GR,GT,ARR,ART et ATT en fonction de l'ordre IS
C*  pour les directions Teta1 et Teta2
C---------------------------------------------------------------------------
            CALL SOS_NOYAUX_FRESNEL(RMU1,RMU2,OS_NS,	
     &                              ALPHA,BETA,GAMMA,ZETA,
     &                              BP,GR,GT,ARR,ART,ATT)

C* Calcul des éléments de la matrice d'interface pour chaque ordre IS.
C---------------------------------------------------------------------
            DO 24 IS=0,OS_NB
               X=G(IS)
	       
C*   Eléments pour IS=0.
               R11     =X*BP(0,1)	       
               R12     =X*GR(0,1)
               R13     =0.
               R21     =X*GR(0,2)
               R22     =X*ARR(0,1)
	       R23     =0.
               R31     =0.
               R32     =0.
               R33     =X*ATT(0,1)

C*   Eléments pour la sommation de K=1 à OS_NS
               DO 3 K=1,OS_NS
                 I1=K+IS
                 I2=ABS(K-IS)
                 IF ((I1.GT.LIM).AND.(I2.GT.LIM)) GOTO 3	
                 X=G(I2)+G(I1)			
                 Y=G(I2)-G(I1)			
                 R11     =R11     +BP(K,1)*X		 
                 R12     =R12     +GR(K,1)*X
                 R13     =R13     +GT(K,1)*Y
                 R21     =R21     +GR(K,2)*X
                 R22     =R22     +ARR(K,1)*X
                 R23     =R23     +ART(K,2)*Y
                 R31     =R31     +GT(K,2)*Y
                 R32     =R32     +ART(K,1)*Y
                 R33     =R33     +ATT(K,1)*X
  3            CONTINUE

C* Affectation des résultats aux tableaux résultats Rij(IS)
 244           M11(IS)=  R11
               M12(IS)=  R12
	       M13(IS)= -R13
               M21(IS)=  R21
               M22(IS)=  R22
               M23(IS)= -R23		
               M31(IS)= -R31
               M32(IS)= -R32
               M33(IS)=  R33		
 24         CONTINUE	!Fin de boucle sur IS

C* Enregistrement des ordres IS pour le couple (Teta1, Teta2) de
C* la décomposition en séries de Fourier de la matrice de Fresnel.
C-----------------------------------------------------------------
            WRITE(4,err=992) M11,M12,M13,M21,M22,M23,M31,M32,M33

 2       CONTINUE	!Fin de boucle sur Teta2 (J)

 25   CONTINUE	!Fin de boucle sur Teta1 (I)
 
 
C* Fin nominale 
C-------------------
      IER=0
      goto 9999
      
C* Cas d'erreur : retour du status -1 au programme appelant
C----------------------------------------------------------  
  910 WRITE(6,*) '  OSOAA_MAT_INTERFACE : ERROR_910 on logfile writing'
      GOTO 9998
      
  920 WRITE(6,*) '  OSOAA_MAT_INTERFACE : ERROR_920 ' 
      WRITE(6,*) '  --> Error on mode interface definition'
      GOTO 9998
        
  991 WRITE(6,*) '  OSOAA_MAT_INTERFACE : ERROR_991 on a file opening'
      GOTO 9998
      
  992 WRITE(6,*) '  OSOAA_MAT_INTERFACE : ERROR_992 '
      WRITE(6,*) '  --> Error on a file reading/writing'
      GOTO 9998

 9998 IER=-1
 9999 CONTINUE

C* Fermeture fichier
C-------------------
      CLOSE(3)
      CLOSE(4)
 
C* Format
C---------   
 1300 FORMAT(4(E15.8))  
   
      RETURN
      END         !FIN DE LA PROCEDURE OSOAA_MAT_INTERFACE





C==============================================================================
C PROCEDURE: SOS_NOYAUX_FRESNEL
C ==========
C      Cette procédure calcule la décomposition en séries de Fourier de
C      la matrice de Fresnel selon l'azimut, pour les directions Teta1 et Teta2.
C
C      On calcule les éléments de matrice suivants :
C         BP(IS,mu,mup)    GR(IS,mu,mup)    GT(IS,mu,mup)
C         ARR(IS,mu,mup)   ART(IS,mu,mup)   ATT(IS,mu,mup)
C      pour les ordres IS de la décomposition en séries de Fourier
C      
C      pour mu = cos(Teta2) & mup = cos(Teta1)		--> configuration 1
C      pour mu = cos(Teta1) & mup = cos(Teta2)		--> configuration 2
C
C
C Description des paramètres
C --------------------------
C	RMU1   (double)	     (E) Cos(Teta1).
C	RMU2   (double)	     (E) Cos(Teta2).
C       OS_NS (I4)           (E) Ordre maximal à utiliser pour le développement en
C                                polynômes de Legendre des éléments de la matrice 
C                                de Fresnel.
C
C	ALPHA(0:CTE_OS_NS_MAX)  (double)	(E) Paramètres alpha(k).
C	BETA(0:CTE_OS_NS_MAX)   (double)	(E) Paramètres beta(k).
C	GAMMA(0:CTE_OS_NS_MAX)  (double)	(E) Paramètres gamma(k).
C	ZETA(0:CTE_OS_NS_MAX)   (double)	(E) Paramètres zeta(k).
C
C       Elément Pij de la matrice de Fresnel (en réflexion ou en transmission)
C       décomposée en séries de Fourier pour un ordre IS = 0 à OS_NS.
C      		Calcul en configuration 1 : pour la direction d'incidence Teta1 (mup=RMU1) 
C				 	    et de déviation Teta2 (mu=RMU2)
C      		Calcul en configuration 2 : pour la direction d'incidence Teta2 (mup=RMU2)
C					    et de déviation Teta1 (mu=RMU1)
C	BP(0:CTE_OS_NS_MAX,2)   (double)     (S) Coefficients pour l'élément P11.
C	GR(0:CTE_OS_NS_MAX,2)   (double)     (S) Coefficients pour les éléments P12 ou P21.
C	GT(0:CTE_OS_NS_MAX,2)   (double)     (S) Coefficients pour les éléments -P13 ou -P31
C	ARR(0:CTE_OS_NS_MAX,2)  (double      (S) Coefficients pour l'élément P22.
C	ART(0:CTE_OS_NS_MAX,2)  (double)     (S) Coefficients pour les éléments -P23 ou -P32
C	ATT(0:CTE_OS_NS_MAX,2)  (double)     (S) Coefficients pour l'élément P33.
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
C
C=============================================================================
      SUBROUTINE SOS_NOYAUX_FRESNEL(RMU1,RMU2,OS_NS,
     &                              ALPHA,BETA,GAMMA,ZETA,
     &                              BP,GR,GT,ARR,ART,ATT)

      IMPLICIT NONE

C* Définition des variables                                  
C*----------------------------------------------------------------------
      DOUBLE PRECISION RMU1	! Cos(Teta1).
      DOUBLE PRECISION RMU2	! Cos(Teta2).

      DOUBLE PRECISION ALPHA(0:CTE_OS_NS_MAX)   ! Paramètres alpha(k).
      DOUBLE PRECISION BETA(0:CTE_OS_NS_MAX)	! Paramètres beta(k).
      DOUBLE PRECISION GAMMA(0:CTE_OS_NS_MAX)   ! Paramètres gamma(k).
      DOUBLE PRECISION ZETA(0:CTE_OS_NS_MAX)	! Paramètres zeta(k).

      DOUBLE PRECISION R(2)	! tableau contenant cos(Teta1) et cos(Teta2).

      	! Elément Pij de la matrice de Fresnel (en réflexion ou en transmission) 
	! décomposée en séries de Fourier pour un ordre IS = 0 à OS_NS.
	! Calcul pour la direction d'incidence Teta1 (RMU1)
	! et de déviation Teta2 (RMU2).	
      DOUBLE PRECISION BP(0:CTE_OS_NS_MAX,2)    ! Elément P11.
      DOUBLE PRECISION GR(0:CTE_OS_NS_MAX,2)    ! Elément P12 ou P21.
      DOUBLE PRECISION GT(0:CTE_OS_NS_MAX,2)    ! Elément -P13 ou -P31.
      DOUBLE PRECISION ARR(0:CTE_OS_NS_MAX,2)   ! Elément P22.
      DOUBLE PRECISION ART(0:CTE_OS_NS_MAX,2)   ! Elément -P23 ou -P32.
      DOUBLE PRECISION ATT(0:CTE_OS_NS_MAX,2)   ! Elément P33.

      DOUBLE PRECISION PSL(-1:CTE_OS_NS_MAX,2)  
                                ! Fonction de Legendre PSL pour l'ordre S=IS
      				! en fonction de l'ordre L et de l'angle mu.
      DOUBLE PRECISION RSL(-1:CTE_OS_NS_MAX,2)  ! Fonction RSL.
      DOUBLE PRECISION TSL(-1:CTE_OS_NS_MAX,2)  ! Fonction TSL.

      DOUBLE PRECISION C	     ! Valeur ponctuelle du mu de Gauss.
      DOUBLE PRECISION A,B,D,E,F     ! Coefficients pour la récurrence 
      				     ! sur PSL, RSL et TSL.

      DOUBLE PRECISION X	! Variable intermédiaire de calcul.
      DOUBLE PRECISION XX	! Variable intermédiaire de calcul.
      DOUBLE PRECISION YY	! Variable intermédiaire de calcul.

      DOUBLE PRECISION RAC3	! Racine de 3.
      DOUBLE PRECISION X26	! 2*racine(6)

      INTEGER*4 OS_NS   ! Ordre maximal du développement en polynômes de Legendre   
			! des éléments de la matrice de Fresnel.

      INTEGER*4 IS	! Ordre de la décomposition en séries de Fourier.
      INTEGER*4 I
      INTEGER*4 J
      INTEGER*4 K
      INTEGER*4 L



C* Initialisation
C-------------------------
      RAC3=DSQRT(3.D+00)
      X26=2.*DSQRT(6.D+00)

C* Stockage de cos(Teta1) et cos(Teta2) dans le tableau R.
C------------------------------------------------------
      R(1)=RMU1
      R(2)=RMU2

C* Décomposition en séries de Fourier de IS = 0 à OS_NS
C----------------------------------------------------------
      DO 1 IS=0,OS_NS

C-----------------------------------------------------
C* Initialisation des fonctions d'ordre IS pour les
C* ordres L = 0, 1 et 2.
C-----------------------------------------------------

C* Initialisation pour l'ordre IS = 0
C-------------------------------------
         IF (IS.NE.0) GOTO 700  
         DO 25 J=1,2
            C=R(J)
            PSL(0,J)=1
            PSL(1,J)=C
            X=(3*C*C-1)*0.5
            PSL(2,J)=X
            RSL(1,J)=0
            X=3*(1-C*C)/X26
            RSL(2,J)=X
            TSL(1,J)=0.
            TSL(2,J)=0.
   25    CONTINUE
         GOTO 501

C* Initialisation pour l'ordre IS = 1
C-------------------------------------
  700    IF (IS.NE.1) GOTO 701
         DO 26 J=1,2
            C=R(J)
            X=1-C*C
            PSL(0,J)=0
            PSL(1,J)=DSQRT(X*0.5)
            PSL(2,J)=C*PSL(1,J)*RAC3
            TSL(1,J)=0.
            RSL(1,J)=0
            RSL(2,J)=-C*DSQRT(X)*0.5
            TSL(2,J)=-DSQRT(X)*0.5
   26    CONTINUE
         GOTO 501

C* Initialisation pour l'ordre IS >= 2
C----------------------------------------
  701    A=1
         DO 27 I=1,IS
            X=I
   27       A=A*DSQRT((I+IS)/X)*0.5
   
         B=A*DSQRT(IS/(IS+1.D+00))*DSQRT((IS-1.D+00)/(IS+2.)) 
         DO 28 J=1,2
             C=R(J)
             XX=1-C*C
             YY=IS*0.5
             PSL(IS-1,J)=0.
             RSL(IS-1,J)=0.
             TSL(IS-1,J)=0.
             X=XX**YY
             PSL(IS,J)=A*X
             YY=YY-1
             X=XX**YY
             RSL(IS,J) = B*(1+C*C)*X
             TSL(IS,J)= 2*B*C*X
   28    CONTINUE

C* Calcul des fonctions PSL(L,mu), RSL(L,mu) et TSL(L,mu)
C* par récurrence sur l'ordre L pour IS fixé.
C------------------------------------------------------
  501    K=2
         IF (IS.GT.2) K=IS

         DO 30 L=K,OS_NS-1
C* Coefficients pour la récurrence sur PSL
             A=(2*L+1.)/DSQRT((L+IS+1.D+00)*(L-IS+1.))
             B=DSQRT(DFLOAT((L+IS)*(L-IS)))/(2.*L+1.) 

C* Coefficients pour la récurrence sur RSL et TSL
             D=(L+1.)*(2*L+1.)/DSQRT((L+3.D+00)*(L-1.)*(L+IS+1.)
     &         *(L-IS+1.))
             E=DSQRT((L+2.D+00)*(L-2.)*(L+IS)*(L-IS))/(L*(2.*L+1.))
             F=2.*IS/(L*(L+1.))

C* Application de la récurrence pour chaque mu de Gauss
C------------------------------------------------------
             DO 31 J=1,2
                C=R(J)
                X=A*(C*PSL(L,J)-B*PSL(L-1,J))
                PSL(L+1,J)=X
                X=D*(C*RSL(L,J)-F*TSL(L,J)-E*RSL(L-1,J))
                RSL(L+1,J)=X
                X=D*(C*TSL(L,J)-F*RSL(L,J)-E*TSL(L-1,J))
                TSL(L+1,J)=X
   31        CONTINUE
   30    CONTINUE


C------------------------------------------------------
C* Calcul des éléments de la matrice d'interface
C------------------------------------------------------

C* Boucles sur les directions Teta1 et Teta2
C------------------------------------------------------
         DO 32 K=1,2
             J=3-K

C* Initialisation des tableaux
             BP(IS,K)=0.
             ARR(IS,K)=0.
             ATT(IS,K)=0
             GR(IS,K)=0.
             GT(IS,K)=0.
             ART(IS,K)=0

C* Boucle de sommation sur L = IS a OS_NS
C-----------------------------------------------
	
             DO 32 L=IS,OS_NS
                BP(IS,K)=BP(IS,K)+BETA(L)*PSL(L,J)*PSL(L,K)
	        GR(IS,K)=GR(IS,K)+GAMMA(L)*PSL(L,J)*RSL(L,K)
                GT(IS,K)=GT(IS,K)+GAMMA(L)*PSL(L,J)*TSL(L,K)
                ATT(IS,K)=ATT(IS,K)+ALPHA(L)*TSL(L,J)*TSL(L,K)+
     &                    ZETA(L)*RSL(L,J)*RSL(L,K)
                ARR(IS,K)=ARR(IS,K)+ZETA(L)*TSL(L,J)*TSL(L,K)+
     &		          ALPHA(L)*RSL(L,J)*RSL(L,K)
                ART(IS,K)=ART(IS,K)+ALPHA(L)*RSL(L,K)*TSL(L,J)+
     &                    ZETA(L)*RSL(L,J)*TSL(L,K)

   32    CONTINUE  ! boucle sur K et L
 
   1  CONTINUE
      RETURN
      END         !FIN DE LA PROCEDURE SOS_NOYAUX_FRESNEL






C==============================================================================
C PROCEDURE:  OSOAA_MISE_FORMAT
C ==========
C      Cette procédure réordonne les éléments de la matrice Mij en une matrice
C      Pij utilisable par le code des OS.
C
C      Les éléments Mij, enregistrés dans le fichier FICMAT_INTERFACE, donnent
C      pour le couple (Teta1=incidence, Teta2=déviation) la décomposition en  
C      séries de Fourier associée. 
C
C      Les éléments Pij, enregistrés dans le fichier de sortie, donnent
C      pour chaque ordre de la décomposition en séries de Fourier
C      la réflexion ou transmission associée à l'ensemble des couples (Teta1, Teta2) .
C
C
C Description des paramètres
C --------------------------
C      FICMAT_INTERFACE  (CHARACTER*21 ) : (E) Nom du fichier de Matrice 
C                                              d'interface à mettre en forme
C      FICRES  (CHARACTER*CTE_LENFIC2) : (E)  Nom complet du fichier résultat.
C      NBMU (I4)  : (E)  Nombre d'angles (positifs) effectivement utiles.
C      OS_NB (I4) : (E)  Ordre des développements des fonctions de phase 
C		         en Polynômes de Legendre à utiliser.
C      TRACE  (LOGICAL)  (E)  ! = vrai si écriture dans le fichier trace.
C      IER  (I4)  : (S)  code d'erreur =0 si pas d'erreur, =-1 sinon
C
C
C
C Résultats fournis par la procédure
C ----------------------------------
C
C      Fichier résultat : FICRES, fichier de matrice d'interface
C 
C Description des fichiers utilisés
C ---------------------------------
C   -->Contenu du fichier temporaire FICMAT_INTERFACE
C       Fichier binaire non formaté, créé par OSOAA_MAT_INTERFACE.
C       Fichier créé dans le répertoire de lancement du programme.
C       Enregistrement pour chaque couple (Teta1, Teta2) de
C       la décomposition en séries de Fourier de la matrice de Fresnel:
C       M11,M12,M13,M21,M22,M23,M31,M32,M33 
C       (n valeurs réelles)
C
C   -->Contenu du fichier FICRES
C       Fichier binaire non formaté.		
C       Pour chaque ordre IS de la décomposition en série de Fourier (IS = 0 à OS_NB),
C       le fichier contient l'enregistrement des éléments Pij de la matrice,
C       limité aux angles utiles : 
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
C        --> Enregistrements sur le domaine limité aux angles utiles
C                                   --------------------------------
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     -Erreur à l'ouverture d'un fichier
C      		Le fichier FICMAT_INTERFACE n'existe pas
C      		Le fichier Résultat existe déjà
C     -Erreur à la lecture/écriture dans un fichier
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant
C
C
C=============================================================================
      SUBROUTINE OSOAA_MISE_FORMAT(FICMAT_INTERFACE,FICRES,NBMU,OS_NB,
     & 				   TRACE,IER)

      IMPLICIT NONE


C Constantes 
C -----------------            
      INTEGER*2 IDLOG			! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_SURFACE)
      
C* Définition des variables                    
C*--------------------------------------------------------------------
      CHARACTER*21 FICMAT_INTERFACE    ! Nom du fichier de Matrice d'interface 
      				       ! à mettre en forme
      CHARACTER*CTE_LENFIC2 FICRES     ! Nom du fichier résultat.
 
      	! Eléments Mij de la matrice d'interface en fonction de l'ordre IS
	! de la série de Fourier, pour
	!     _ une incidence sous Teta1,
	!     _ une déviation sous Teta2.
      REAL M11(0:CTE_OS_NB_MAX)		
      REAL M12(0:CTE_OS_NB_MAX)		
      REAL M13(0:CTE_OS_NB_MAX)			
      REAL M21(0:CTE_OS_NB_MAX)	
      REAL M22(0:CTE_OS_NB_MAX)		
      REAL M23(0:CTE_OS_NB_MAX)		
      REAL M31(0:CTE_OS_NB_MAX)	
      REAL M32(0:CTE_OS_NB_MAX)	
      REAL M33(0:CTE_OS_NB_MAX)

      	! Eléments Pij de la matrice d'interface en fonction de la direction
	! d'incidence et de déviation, pour un ordre IS du développement
	! en séries de Fourier
      REAL P11(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL P12(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL P13(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)	
      REAL P21(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL P22(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL P23(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL P31(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL P32(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)
      REAL P33(CTE_OS_NBMU_MAX,CTE_OS_NBMU_MAX)

      INTEGER*4 NBMU    ! Nombre d'angles effectifs des simulations
      INTEGER*4 OS_NB 	! Ordre des développements des fonctions de phase 
			! en Polynômes de Legendre.
      INTEGER*4 IS	! Ordre de la décomposition en séries de Fourier.
      INTEGER*4 I	! Indice de l'angle d'incidence Teta1.
      INTEGER*4 J	! Indice de l'angle de déviation Teta2.
      INTEGER*4 IER     ! code d'erreur =0 si pas d'erreur, =-1 sinon  

      LOGICAL TRACE     ! = vrai si écriture dans le fichier trace


      	
C* Informations pour fichier Trace
C------------------------------------------------------------
      IF (TRACE) THEN
         WRITE(IDLOG,*,err=910)	' ' 
         WRITE(IDLOG,*,err=910) 'Routine OSOAA_MISE_FORMAT' 
	 WRITE(IDLOG,*,err=910) '-------------------------'  
         WRITE(IDLOG,*,err=910) ' '
         WRITE(IDLOG,*,err=910)'Conversion of file format :'
         WRITE(IDLOG,*,err=910)'  Initial file : ',FICMAT_INTERFACE
	 WRITE(IDLOG,*,err=910)'  Result file  : ',FICRES
	 WRITE(IDLOG,*,err=910) ' ' 	 	 
      END IF
      
      	
C* Ouverture fichier
C------------------------------------------------------------	
	
      OPEN(UNIT=1,FILE=FICMAT_INTERFACE,FORM='UNFORMATTED',
     &     STATUS='OLD',ERR=991)
      OPEN(UNIT=2,FILE=FICRES,FORM='UNFORMATTED',
     &     STATUS='NEW',ERR=991)

C* Boucle sur l'ordre de la décomposition en séries de Fourier
C------------------------------------------------------------
      DO 1 IS=0,OS_NB
		
C* Boucle sur les directions d'incidence Teta1
C---------------------------------------------
         DO 2 I=1,NBMU

C* Boucle sur les directions de déviation Teta2
C-----------------------------------------------

C*       Lecture du fichier FICMAT_INTERFACE enregistré pour la taille max
C*       des tableaux : CTE_OS_NBMU_MAX 
            DO 3 J=1,NBMU			
               READ(1,err=992) M11,M12,M13,M21,M22,M23,M31,M32,M33
	       P11(I,J)=M11(IS)		
	       P12(I,J)=M12(IS)		
               P13(I,J)=M13(IS)		
               P21(I,J)=M21(IS)
               P22(I,J)=M22(IS)		
               P23(I,J)=M23(IS)
               P31(I,J)=M31(IS)
	       P32(I,J)=M32(IS)		
	       P33(I,J)=M33(IS)		
 3          CONTINUE
 2       CONTINUE
 
C*       Enregistrement des tableaux de la matrice d'interface 
C*       à l'ordre IS de la décomposition en séries de Fourier.
C*       Bornes d'enregistrement des tableaux limitées au 
C*       dimensionnement utile -NBMU:NBMU
C-------------------------------------------------------------- 
         WRITE(2,err=992) ((P11(I,J),I=1,NBMU),J=1,NBMU),
     &	                  ((P12(I,J),I=1,NBMU),J=1,NBMU),
     &	                  ((P13(I,J),I=1,NBMU),J=1,NBMU),
     &	                  ((P21(I,J),I=1,NBMU),J=1,NBMU),
     &	                  ((P22(I,J),I=1,NBMU),J=1,NBMU),
     &	                  ((P23(I,J),I=1,NBMU),J=1,NBMU),
     &	                  ((P31(I,J),I=1,NBMU),J=1,NBMU),
     &	                  ((P32(I,J),I=1,NBMU),J=1,NBMU),
     &	                  ((P33(I,J),I=1,NBMU),J=1,NBMU)

C* Retour au début du fichier avant passage à l'ordre IS suivant.
C---------------------------------------------------------------
         REWIND (1)
 1    CONTINUE


      
C* Fin nominale 
C-------------------
      IER=0
      GOTO 9999

C* Cas d'erreur : retour du status -1 au programme appelant
C------------------------------------------------------
  910 WRITE(6,*) '  OSOAA_MISE_FORMAT : ERROR_910 on logfile writing'
      GOTO 9998
      
  991 WRITE(6,*) '  OSOAA_MISE_FORMAT : ERROR_991 on a file opening'
      GOTO 9998
      
  992 WRITE(6,*) '  OSOAA_MISE_FORMAT : ERROR_992 '
      WRITE(6,*) '  --> Error on a file reading/writing'
      GOTO 9998

 9998 IER=-1 
 9999 CONTINUE

C* Fermeture fichier
C-------------------
      CLOSE(1)
      CLOSE(2)
      
      RETURN
      END         !FIN DE LA PROCEDURE OSOAA_MISE_FORMAT

      
 




C==============================================================================
C PROCEDURE: OSOAA_GSF
C ==========
C      Cette procédure réalise la décomposition en séries de Fourier
C      (sur phi) de la fonction G(Teta1,Teta2,phi) :
C
C	 G(Teta1,Teta2,phi) = exp [-tan(TetaN)**2 / SIG] / cos(TetaN)**4
C
C	 avec Teta1 : angle zénithal d'incidence  (0 < Teta1 < PI/2),
C             Teta2 : angle zénithal de déviation (0 < Teta2 < PI/2),
C             phi   : angle azimutal relatif entre les directions
C                     d'incidence et de déviation,
C
C	      TetaN : angle zénithal de la normale à la facette de vague,
C                     calculé à partir de Teta1, Teta2 et phi.
C			
C             SIG   : carré du paramètre Sigma de la théorie de Cox et Munck.
C
C
C      Enregistre les résultats dans le fichier "RES_GSF_XXX" (XXX = RAA, RWW, TAW ou TWA)
C      sous la forme:
C	  Numéro d'angle Teta1 : I1,
C         Numéro d'angle Teta2 : I2,
C         Ordre maximal utile de la décomposition: IL,
C         Fonction G décomposée pour (Teta1,Teta2) : (E(J),J=0,IL)
C
C
C Description des paramètres
C --------------------------
C      CINTER (CHAR*3) : (E) Chaine de 3 caractères précisant le type d'interface
C			     à simuler :
C   			        une réflexion air -> air : "RAA"
C			        une réflexion mer -> mer : "RWW"
C			        une transmission air -> mer : "TAW"
C			        une transmission mer -> air : "TWA"
C      INDP  (double) : (E) Transmission air -> mer : Indice de la mer par rapport à l'air.
C      			    Transmission mer -> air : Inverse de l'indice de la mer.
C      LUM_NBMU (I4) : (E) Nombre d'angles (positifs) effectivement utiles.
C      RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)   (double) : (E) Tableau des mu de Gauss.
C      WIND	(double) : (E)  Vitesse du vent (m/s).
C      OS_NM (I4) : (E) Ordre maximal du développement en séries de Fourier  
C                       de la fonction G des calculs de matrice de réflexion.
C      TRACE  (LOGICAL)  (E)  ! = vrai si écriture dans le fichier trace.
C      IER  (I4) : (S) code d'erreur =0 si pas d'erreur, =-1 sinon 
C
C
C Résultats fournis par la procédure
C ----------------------------------
C
C      Fichier résultat temporaire : RES_GSF_XXX  
C      pour XXX = RAA, RWW, TAW ou TWA selon le type d'interface simulé.
C 
C      Fichier de trace (optionnel)
C              (No d'unité logique du fichier trace = IDLOG = INCTE_IDLOG_SURFACE )
C
C
C Description des fichiers utilisés
C --------------------------------- 
C   -->Contenu du fichier temporaire FICRES_GSF nommé :
C  		"RES_GSF_RAA" pour une réflexion air -> air
C  		"RES_GSF_RWW" pour une réflexion mer -> mer
C  		"RES_GSF_TAW" pour une transmission air -> mer
C  		"RES_GSF_TWA" pour une transmission mer -> air
C       Fichier binaire non formaté, créé par OSOAA_GSF.
C       Fichier créé dans le répertoire de lancement du programme.
C       Si un fichier de même nom existe déjà, il est écrasé
C       Chaque enregistrement contient :
C           -numéro d'angle Teta1 (valeur entière)
C           -numéro d'angle Teta2 (valeur entière)
C           -ordre maximale utile de la decompositon : IL  (valeur entière)
C           -Fonction G décomposée pour (Teta1,Teta2) : (E(J),J=0,IL)
C            (IL+1 valeurs double précision)
C
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     - Erreur de définition du type d'interface
C     - Erreur d'écriture dans le fichier Trace
C     - Erreur d'ouverture/lecture d'un fichier temporaire
C     - Erreur sur test d'évolution des configurations géométriques en fonction de l'azimut
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant
C
C     
C  Routines utilisées par la procédure:
C -------------------------------------
C  Ce programme fait appel aux routines:
C     - SOS_CALC_GR : calcul de la fonction de proba G connaissant les directions 
C                     d'incidence et de réflexion.
C     - OSOAA_CALC_GT : calcul de la fonction de proba G connaissant les directions 
C			d'incidence et de transmission.
C
C==============================================================================
      SUBROUTINE OSOAA_GSF(CINTER,INDP,LUM_NBMU,RMU,WIND,OS_NM,
     &                     TRACE,IER)
 	
      IMPLICIT NONE


C Constantes 
C -----------------            
      INTEGER*2 IDLOG		! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_SURFACE)
      
      INTEGER INCTE_NB_PHI		! Nb d'angles azimutaux
      PARAMETER	(INCTE_NB_PHI=CTE_PH_NU)
      
      INTEGER*4 ICOUNT_PAS_I1		! Pas sur le numéro d'angles d'incidence 
      PARAMETER(ICOUNT_PAS_I1=3)	! pour échantillonnage des traces		
      
      INTEGER*4 ICOUNT_PAS_I2		! Pas sur le numéro d'angles de réflexion ou de
      PARAMETER(ICOUNT_PAS_I2=3)	! transmission pour échantillonnage des traces
      					
      
C* Définition des variables    
C*-------------------------------------------------------------------

      DOUBLE PRECISION INDP	! Indice de l'eau par rapport à l'air pour une transmission 
      				! air -> mer. 
				! Valeur inverse pour une transmission mer -> air.
								
      DOUBLE PRECISION RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX) ! Tableau des mu de Gauss

      DOUBLE PRECISION WIND	! Vitesse du vent (m/s).

      DOUBLE PRECISION E(0:CTE_OS_NM_MAX) ! Résultat de la décomposition en 
      				          ! séries de Fourier de la fonction G.
	
      DOUBLE PRECISION SIG 	! Carré du paramètre Sigma de la théorie de Cox et Munck.

      DOUBLE PRECISION PHI	! Azimut relatif (radians).

      DOUBLE PRECISION G	! Valeur de la fonction G(phi).

      DOUBLE PRECISION C1	! Cos(Teta1).
      DOUBLE PRECISION S1	! Sin(Teta1).
      DOUBLE PRECISION C2	! Cos(Teta2).
      DOUBLE PRECISION S2	! Sin(Teta2).

      DOUBLE PRECISION PHILIM	! Valeur de l'angle azimutal phi à la limite des cas de 
      				! configurations  géométriques nominales, 
				! pour un couple (Teta1, Teta2).
				
      DOUBLE PRECISION PHIB	! Angle azimutal phi maximal pour la 
      				! description de la fonction G(phi).
			
      DOUBLE PRECISION PHI1	! Angle intermédiaire pour le calcul de PHIB.
      DOUBLE PRECISION PHI2	! Angle intermédiaire pour le calcul de PHIB.
      
      DOUBLE PRECISION PHI5PC   ! Valeur de l'angle azimutal phi au-delà de laquelle
      				! l'erreur de recomposition de la fonction G(phi)
				! est supérieure à 5%.
				
      DOUBLE PRECISION G_PI     ! Valeur de la fonction G(phi=pi) 
                                ! pour Theta1 et Theta2 fixés.
      DOUBLE PRECISION GMIN	! Valeur minimale retenue pour la fonction 
      				! G(phi) pour Theta1 et Theta2 fixés : phi = phib.
      DOUBLE PRECISION GMAX	! Valeur maximale de la fonction G(phi) = G(0).
      DOUBLE PRECISION GMOY     ! Valeur de la fonction G(phi) pour phi = phib/2.
			       				       		
      DOUBLE PRECISION U(0:CTE_PH_NU)  ! Fonction G(phi) entre 0 et PHIB
      			               ! pour Theta1 et Theta2 fixés.

      DOUBLE PRECISION Q	! Pas en phi.
	
		! Recombinaison de la fonction G(phi) à partir de E(IS) 
      DOUBLE PRECISION T_GMIN	! Recombinaison pour phi = phib.
      DOUBLE PRECISION T_GMOY	! Recombinaison pour phi = phib/2.
      DOUBLE PRECISION T_GMAX	! Recombinaison pour phi = 0.
      DOUBLE PRECISION T_GPI	! Recombinaison pour phi = PI.
      DOUBLE PRECISION RECOMP   ! Recombinaison pour phi quelconque.
      
      DOUBLE PRECISION X,Y,Z
      DOUBLE PRECISION XT
      
      DOUBLE PRECISION ERR_GMIN	  ! Erreur de recomposition pour phi = phib.
      DOUBLE PRECISION ERR_GMOY   ! Erreur de recomposition pour phi = phib/2.
      DOUBLE PRECISION ERR_GMAX	  ! Erreur de recomposition pour phi = 0.
      DOUBLE PRECISION ERR_GPI    ! Erreur de recomposition pour phi = PI.
      
      DOUBLE PRECISION B_GMIN	  ! Erreur relative de recomposition pour phi = phib.
      DOUBLE PRECISION B_GMOY	  ! Erreur relative de recomposition pour phi = phib/2.
      DOUBLE PRECISION B_GMAX	  ! Erreur relative de recomposition pour phi = 0.
      DOUBLE PRECISION B_GPI	  ! Erreur relative de recomposition pour phi = PI
      
      DOUBLE PRECISION ERR_GMOY_SUR_GMAX ! Erreur de recomposition pour phi = phib/2 
      					 ! par rapport à G(phi=0)
      DOUBLE PRECISION ERR_GMIN_SUR_GMAX ! Erreur de recomposition pour phi = phib  
      					 ! par rapport à G(phi=0)
      DOUBLE PRECISION ERR_GPI_SUR_GMAX  ! Erreur de recomposition pour phi = 180 
      					 ! par rapport à G(phi=0)
					 				 
      DOUBLE PRECISION ERR_RECOMP_ABS ! Erreur absolue de recomposition pour phi quelconque.
      DOUBLE PRECISION ERR_RECOMP_REL ! Erreur relative de recomposition pour phi quelconque


      DOUBLE PRECISION MAX_B_GMAX	 ! Valeur max de l'erreur de recomposition 
      					 ! pour phi = 0 par rapport à G(phi=0)
      DOUBLE PRECISION MAX_ERR_GMOY_SUR_GMAX	  ! Valeur max de l'erreur de recomposition 
      					          ! pour phi = phib/2 par rapport à G(phi=0)
      DOUBLE PRECISION MAX_ERR_GMIN_SUR_GMAX	  ! Valeur max de l'erreur de recomposition 
      					          ! pour phi = phib par rapport à G(phi=0)
      DOUBLE PRECISION MAX_ERR_GPI_SUR_GMAX	  ! Valeur max de l'erreur de recomposition 
      					          ! pour phi = 180 par rapport à G(phi=0)
      
      DOUBLE PRECISION TAUX_PHI_SOUS_PHI5PC ! Pour une configuration (Teta1,Teta2),
      					    ! taux de couverture sur le domaine [ 0, phib ]
      					    ! avec G(phi) à mieux que 5% d'erreur. 	
						
      DOUBLE PRECISION SOM_TAUX_PHI_SOUS_PHI5PC ! Somme des taux de couverture sur Phi 
      						! donnant G(phi) à mieux que 5% d'erreur. 
						! Cumul sur toutes les configurations 
						! (Teta1,Teta2) pour les cas où Gmax n'est 
						! pas forcé à zéro. 
      
      INTEGER*4 OS_NM    ! Ordre maximal du développement en séries de Fourier  
			 ! de la fonction G des calculs de matrice de réflexion.
			 
      INTEGER*4 LUM_NBMU ! Nombre d'angles effectifs des simulations
      
      INTEGER*4 IL	! Ordre maximale utile pour la fonction E.
      INTEGER*4 I1	! Indice de numéro d'angle Teta1.
      INTEGER*4 I2	! Indice de numéro d'angle Teta2.
      INTEGER*4 IPHI	! Indice de numéro d'angle Phi.
      INTEGER*4 IS	! Ordre IS de la décomposition en séries de Fourier.
      INTEGER*4 IER     ! code d'erreur =0 si pas d'erreur, =-1 sinon  

      INTEGER*4 I
      INTEGER*4 IA,IP,K,J
			      			      
      INTEGER*4 NB_GMAX_NONNUL	! Nombre de cas où Gmax n'est pas posé à zéro.
       
      INTEGER*4 NB_CAS_ORDREMAX ! Nombre de cas couples (Teta1, Teta2) pour lesquels la 
      				! décomposition de G(phi) est faite avec l'ordre max OS_NM
            
      INTEGER*4 ICOUNT_I1,ICOUNT_I2 ! Compteur de boucle pour échantillonner 
      				    ! les traces des traitements
      
      INTEGER*2 PASIPHI	      ! Pas sur IPHI pour la génération de diagrammes polaires 
      			      ! (mode option interne)
      	
		      
      LOGICAL GEO_OK          ! Status possible (TRUE) ou impossible (FALSE) de la 
      			      ! configuration géométrique (incidence - transmission).
	
      LOGICAL THETAN_OK       ! Status de cas à angle zénithal de vague estimé possible (TRUE) 
      			      ! ou non (FALSE).
			                
      LOGICAL TRACE           ! = vrai si écriture dans le fichier trace

      LOGICAL LOOP_IN	      ! Indicateur de poursuite d'itération de la dichotomie
      				      	
      LOGICAL SEARCH_PHI5PC   ! Indicateur de poursuite de la recherche de l'angle PHI5PC.
      				      			      
      CHARACTER*3 CINTER      ! Chaine de caractères précisant le cas d'interface 
      			      !   une réflexion air -> air : "RAA"
			      !   une réflexion mer -> mer : "RWW"
			      !   une transmission air -> mer : "TAW"
			      !   une transmission mer -> air : "TWA"
				
      CHARACTER*11 FICRES_GSF ! Nom du fichier GSF produit
      			      !  "RES_GSF_RAA" pour une réflexion air -> air
      			      !  "RES_GSF_RWW" pour une réflexion mer -> mer
      			      !  "RES_GSF_TAW" pour une transmission air -> mer
      			      !  "RES_GSF_TWA" pour une transmission mer -> air

      CHARACTER*3 CWIND	      ! Chaines de caractères
      CHARACTER*4 CTHETAINC
      CHARACTER*4 CTHETADEV
      
      CHARACTER*21 FICEXTRA_G   ! Nom du fichier d'option interne pour sortir les
      			        ! diagrammes polaires de la fonction G(Theta,Phi). 
      CHARACTER*18 FICEXTRA_GEO   ! Nom du fichier d'option interne pour sortir les
      			          ! status des configurations géométriques.
		
      CHARACTER*25 FICEXTRA_GPHI   ! Nom du fichier d'option interne pour sortir 
      			           ! la fonction G(Theta,Phi) calculée et recomposée.
				
				
				   	         
     
C* Traitements    
C*------------      

C* Positionnement du paramètre de sorties de fichiers de diagrammes polaires
C* (option interne au code : recompiler si à modifier)
C---------------------------------------------------------------------------
      PASIPHI = INT(INCTE_NB_PHI/60.)	!Dimension du pas sur IPHI représentant un DPHI
      					! de l'ordre de 3 deg

C* Initialisation du calcul de la somme des taux de couverture
C* des angles Phi donnant une recomposition de la fonction G(phi)
C* à mieux que 5% d'erreur (cumul sur les cas où Gmax n'est pas forcé à zéro)
C----------------------------------------------------------------------------
      SOM_TAUX_PHI_SOUS_PHI5PC = 0.D+00
      NB_GMAX_NONNUL = 1

C* Valeur initiale pour la recherche des valeurs maximales de l'erreur 
C* de recomposition pour les Phi tests par rapport à G(phi=0)
      MAX_B_GMAX = -999.   
      MAX_ERR_GMOY_SUR_GMAX = -999.
      MAX_ERR_GMIN_SUR_GMAX = -999.
      MAX_ERR_GPI_SUR_GMAX  = -999.

C* Initialisation du décompte des couples (Teta1, Teta2) pour lesquels 
C* la décomposition de G(phi) est faite avec l'ordre max OS_NM
      NB_CAS_ORDREMAX = 0
           	      
C* Contrôle du type de transition à l'interface 
C----------------------------------------------
      IF ((CINTER.NE."RAA").AND.(CINTER.NE."TAW").AND.
     &    (CINTER.NE."RWW").AND.(CINTER.NE."TWA")) GOTO 920

      IF (CINTER.EQ."RAA") FICRES_GSF = "RES_GSF_RAA"
      IF (CINTER.EQ."TAW") FICRES_GSF = "RES_GSF_TAW"
      IF (CINTER.EQ."RWW") FICRES_GSF = "RES_GSF_RWW"
      IF (CINTER.EQ."TWA") FICRES_GSF = "RES_GSF_TWA"

C* Carré du paramètre Sigma de la théorie de Cox et Munck.
C--------------------------------------------------------
      SIG = .003 + .00512*WIND
      
      
C* Informations Trace
C* ------------------
      IF (TRACE) THEN
         WRITE(IDLOG,*,err=990)	' ' 
         WRITE(IDLOG,*,err=990) 'Routine OSOAA_GSF' 
	 WRITE(IDLOG,*,err=990) '-------------------------'  
         WRITE(IDLOG,*,err=990)	 
         WRITE(IDLOG,*,err=990) 'Calculation of Fourier series'//
     &                ' expansion for the wave slopes probability' 
         WRITE(IDLOG,*,err=990) ' '
         WRITE(IDLOG,*,err=990)'Simulation parameters: '
         WRITE(IDLOG,*,err=990)'   * Wind velocity (m/s) ',WIND,
     &	                       '  | Coeff SIG :',SIG
         WRITE(IDLOG,*,err=920)'   * Type of interface : ',CINTER
	 WRITE(IDLOG,*,err=920)'GSF file to be created : ',FICRES_GSF 
	 WRITE(IDLOG,*,err=920) ' ' 	 	 
	 	 	 
      ENDIF ! Fin Trace
      

C* Ouverture du fichier résultat
C--------------------------------
      OPEN(UNIT=30,FILE=FICRES_GSF,FORM='UNFORMATTED',ERR=991)
      
 
C*===============================================================
C* RECHERCHE DE L'ANGLE PHIB MAXIMAL UTILE POUR CHAQUE COUPLE 
C* (Teta1, Teta2) ET DECOMPOSITION EN SERIES DE FOURIER SELON Phi
C*===============================================================	
     
       IF (TRACE) THEN
	  WRITE(IDLOG,*,err=990) ''
	  WRITE(IDLOG,*,err=990) ''
	  WRITE(IDLOG,*,err=990) 'Statistic on G function versus Phi :'
	  WRITE(IDLOG,*,err=990) '------------------------------------'
	  WRITE(IDLOG,*,err=990) 'Gmax threshold ',CTE_THRESHOLD_GMAX
	  WRITE(IDLOG,*,err=990) ''
	  WRITE(IDLOG,108,err=990) 
	  IF ((CINTER.EQ."TAW").OR.(CINTER.EQ."TWA")) THEN
	      WRITE(IDLOG,1091,err=990) 
          ELSE
	      WRITE(IDLOG,1092,err=990)
	  ENDIF
	  
	  WRITE(IDLOG,1093,err=990) 
	  WRITE(IDLOG,1094,err=990)
	  WRITE(IDLOG,1095,err=990)
	      	  
       ENDIF !Trace
	   
C* Boucle sur l'angle Teta1 > 0 : incidence
C------------------------------------------
      ICOUNT_I1 = 0
      DO  3 I1=1 ,LUM_NBMU

         IF (ICOUNT_I1.EQ.ICOUNT_PAS_I1) ICOUNT_I1 = 1
	 
C* Cosinus et sinus de Teta1
C---------------------------------
         C1=RMU(I1)
         S1=DSQRT(1-C1*C1)

C*   Boucle sur l'angle teta2 > 0 : réflexion ou transmission
C------------------------------------------------------------
         ICOUNT_I2 = 0
	 DO 4 I2=1 ,LUM_NBMU				

           IF (ICOUNT_I2.EQ.ICOUNT_PAS_I2) ICOUNT_I2 = 1
	   
C*   Cosinus et sinus de Teta2
C---------------------------------
           C2=RMU(I2)
           S2=DSQRT(1-C2*C2)

C*   Détermination de G pour phi = 0.
C-------------------------------------
           PHI=0.0
           IF ((CINTER.EQ."RAA").OR.(CINTER.EQ."RWW")) THEN
	       CALL SOS_CALC_GR(C1,C2,S1,S2,SIG,PHI,G)
           ELSE   	
	       CALL OSOAA_CALC_GT(C1,C2,S1,S2,INDP,SIG,PHI,
     &		 	          GEO_OK,THETAN_OK,G)  
	   ENDIF
	   U(0)=G
           GMAX=G

C*   Détermination de G pour phi = PI.
C-----------------------------------------
           PHI=INCTE_PI
	   IF ((CINTER.EQ."RAA").OR.(CINTER.EQ."RWW")) THEN
               CALL SOS_CALC_GR(C1,C2,S1,S2,SIG,PHI,G)
	   ELSE
	       CALL OSOAA_CALC_GT(C1,C2,S1,S2,INDP,SIG,PHI,
     &			          GEO_OK,THETAN_OK,G)
	   ENDIF
           U(CTE_PH_NU)=G
	   G_PI=G
	   	    

C*   Détermination de PHILIM et valeur initiale de GMIN  
C*   pour PHI extrême du domaine des cas valides
C------------------------------------------------------
           
	   !Détermination de PHILIM
	   Q=INCTE_PI/(INCTE_NB_PHI)
	   
	   LOOP_IN=.TRUE.
	   IPHI=1
           DO WHILE(LOOP_IN)
	 	
              PHI = Q*IPHI  

	      !Calcul des indicateurs GEO_OK et THETAN_OK 
              IF ((CINTER.EQ."RAA").OR.(CINTER.EQ."RWW")) THEN
		  GEO_OK=.TRUE.
		  THETAN_OK=.TRUE.
	      ELSE
		  CALL OSOAA_CALC_GT(C1,C2,S1,S2,INDP,SIG,PHI,
     &				     GEO_OK,THETAN_OK,G)
	      ENDIF   
		  	    	
	      IF (GEO_OK.EQV..FALSE.) THEN
		  !Configuration géo de rupture de continuité
		  LOOP_IN=.FALSE.
		  PHILIM=Q*(IPHI-1)
	      ELSE
		  !Configuration géo OK
		  IPHI = IPHI + 1
		  IF (IPHI.GT.(INCTE_NB_PHI)) THEN
		      LOOP_IN=.FALSE.
		      PHILIM=INCTE_PI   
		  ENDIF     
	      ENDIF
    
	   ENDDO !Fin de la recherche de PHILIM

           !Calcul de GMIN = G(PHILIM)  
           IF ((CINTER.EQ."RAA").OR.(CINTER.EQ."RWW")) THEN
	       CALL SOS_CALC_GR(C1,C2,S1,S2,SIG,PHILIM,GMIN)
	   ELSE
	       CALL OSOAA_CALC_GT(C1,C2,S1,S2,INDP,SIG,PHILIM,
     &				  GEO_OK,THETAN_OK,GMIN)
	   ENDIF   
	      


C*   Détermination de PHIB, azimut maximal utile
C----------------------------------------------------
	       
C*   Evaluation de l'importance de l'écart entre Gmin et Gmax
C*   ==> CAS GENERAL DES FONCTIONS G(PHI) STRICTEMENT DECROISSANTES
C*      1er cas: L'écart est modéré.
C*               Pour Teta1 et Teta2 fixés, on définit la fonction
C*               G(phi) entre 0 et PI par pas de PI/CTE_PH_NU.
C*      2nd cas: L'écart est important.
C*               La fonction G(phi) devient insignifiante devant Gmax
C*		 à partir d'un angle PHIB à déterminer.
C*               Pour Teta1 et Teta2 fixés, on définit la fonction
C*               G(phi) entre 0 et PHIB par pas de PHIB/CTE_PH_NU.
C----------------------------------------------------------------------
           X=CTE_PH_TEST*GMIN
           IF( (X.GE.GMAX).OR.(GMAX.LT.CTE_THRESHOLD_GMAX) ) THEN
C* 1er cas: L'écart entre Gmin et Gmax est modéré.
C--------------------------------------------------
C*      Cas 1A: L'écart entre Gmin et Gmax est négligeable car Gmax est très faible
C*      Cas 1B: L'écart n'est pas négligeable

              PHIB=PHILIM

              Q=PHILIM/CTE_PH_NU
	      
              IF (GMAX.LT.CTE_THRESHOLD_GMAX) THEN
	         !Cas 1A : Gmax est très faible ==> La fonction G(phi) est forcée à 0
		 DO 5 I=1,CTE_PH_NU    
                    U(I)=0.D+00
 5               CONTINUE	
                 GMIN=0.D+00
		 GMAX=0.D+00
		 PHIB=INCTE_PI
	      ELSE
                 !Cas 1B: L'écart entre Gmin et Gmax n'est pas négligeable
                 DO 6 I=1,CTE_PH_NU
                    PHI=Q*I
                    IF ((CINTER.EQ."RAA").OR.(CINTER.EQ."RWW")) THEN
		        CALL SOS_CALC_GR(C1,C2,S1,S2,SIG,PHI,G)
		    ELSE
		        CALL OSOAA_CALC_GT(C1,C2,S1,S2,INDP,SIG,PHI,
     &					   GEO_OK,THETAN_OK,G)	
		    ENDIF    
                    U(I)=G
 6               CONTINUE
		
	      ENDIF !Fin Cas 1A ou 1B
	      
           ELSE
C* 2eme cas: L'écart entre Gmin et Gmax est important.
C*          On recherche l'angle PHIB au-delà duquel G(phi) devient
C*          insignifiante devant Gmax.
C------------------------------------------------------------------
C*      Initialisation
              PHI1=0
              PHI2=PHILIM
              LOOP_IN=.TRUE.
		
	      !Recherche dichotomique de la valeur de PHIB
	      DO WHILE(LOOP_IN)
	 	
                 !Estimation de PHIB
		 PHIB=.5*(PHI1+PHI2)	    

		 !Calcul de G(PHIB)     
                 IF ((CINTER.EQ."RAA").OR.(CINTER.EQ."RWW")) THEN
		     CALL SOS_CALC_GR(C1,C2,S1,S2,SIG,PHIB,G)
	         ELSE
		     CALL OSOAA_CALC_GT(C1,C2,S1,S2,INDP,SIG,PHIB,
     &				        GEO_OK,THETAN_OK,G)
	         ENDIF   
		  	    	
		 !Contrôle de l'importance de l'écart entre G(PHIB) et Gmax.
		 !Cas 2A: L'écart reste important.
		 !        PHIB doit être ré-estimé.
		 !        On resserre PHI1 ou PHI2, selon le signe de l'écart.
		 !        On estime à nouveau PHIB comme la moyenne de PHI1 et PHI2
		 !        et on calcule la nouvelle valeur G(PHIB).
		 !Cas 2B: L'écart est modéré ==> Sortie de boucle d'itération.
		 !        PHIB est une estimation satisfaisante de la valeur de
		 !        l'angle phi jusqu'à laquelle G(phi) reste significative
		 !        devant Gmax. La fonction G(phi) est décrite entre 0 et 
		 !        PHIB par pas de PHIB/CTE_PH_NU.	
		 !-------------------------------------------------------------------
            
	         X=CTE_PH_TEST*G
                 IF (DABS(X-GMAX).LT..01*GMAX) THEN
		    LOOP_IN=.FALSE.
	         ELSE
		    !Cas 2A: L'écart entre G(PHIB) et GMAX reste important.
		    !        On affine la valeur de PHIB.
		    !-------------------------------------------------------------
                    IF (X.LE.GMAX) THEN
	               PHI2=PHIB
	            ELSE
	               PHI1=PHIB
	            ENDIF
		    
		    !Test de sortie de boucle si PHI1 et PHI2 sont très proches
                    IF (DABS(PHI2-PHI1).LT.CTE_THRESHOLD_DICHO) THEN
		       LOOP_IN=.FALSE.
		    ENDIF
	         ENDIF
	    
	      ENDDO !Fin de la recherche dichotomique de PHIB

	
C*      Cas 2B: L'écart entre G(PHIB) et GMAX est modéré.
C*              L'estimation de PHIB est satisfaisante.
C*              On définit G(phi) entre 0 et PHIB par pas de 
C*              PHIB/CTE_PH_NU.
C*              Gmin devient G(PHIB).	
C--------------------------------------------------------------------
 8            Q=PHIB/CTE_PH_NU
	          
              DO 10 I=0,CTE_PH_NU
                 PHI=Q*I
		 
		 IF ((CINTER.EQ."RAA").OR.(CINTER.EQ."RWW")) THEN
		    CALL SOS_CALC_GR(C1,C2,S1,S2,SIG,PHI,G)
		 ELSE
		    CALL OSOAA_CALC_GT(C1,C2,S1,S2,INDP,SIG,PHI,
     &				       GEO_OK,THETAN_OK,G)
		 ENDIF
                 
                 U(I)=G
 10           CONTINUE
              	
           ENDIF !Fin test cas importance de l'écart entre Gmin et Gmax

	   

C*   Réalisation de la décomposition en série de Fourier.
C*   Boucle sur l'ordre de la série.                     	
C--------------------------------------------------------------------
           GMIN=U(CTE_PH_NU)
	   GMAX=U(0)			!On repositionne GMAX = G(phi=0) pour la suite.
	   GMOY=U(CTE_PH_NU/2)
	   
	   T_GMAX=0.
	   T_GMOY=0.
           T_GMIN=0.
	   T_GPI=0.
	   
	   IF (GMAX.LT.CTE_THRESHOLD_GMAX) THEN
	   
	      !Décomposition pour une fonction G(phi) nulle
	      E(0)=0
	      IL=0
	      GMAX=0.
	      GMOY=0.
	      GMIN=0. 
	      G_PI=0. 
	      B_GMAX=0. 
	      B_GMOY=0. 
	      B_GMIN=0. 
	      B_GPI=0.  
	      ERR_GMAX=0.
	      ERR_GMOY=0.
	      ERR_GMIN=0.
	      ERR_GPI=0.
	      ERR_GMOY_SUR_GMAX=0.
	      ERR_GMIN_SUR_GMAX=0.
	      ERR_GPI_SUR_GMAX=0.
	      PHI5PC=PHIB
	      
	   ELSE
	      !Décomposition pour une fonction G(phi) non nulle
	      DO 13 IS=0,OS_NM
                 Z=.5*(GMAX+GMIN*DCOS(IS*PHIB))

                 IA=1
                 DO 14 I=1,CTE_PH_NQ
                    IA=2*IA
                    IP=CTE_PH_NU/IA
                    Y=0
                    DO 15 J=1,IA,2
                       K=IP*J
                       Y=Y+U(K)*DCOS(IS*K*Q)
 15                 CONTINUE
                    Y=2*Y/IA
	
                    XT=DABS(Z-Y)/Z
                    IF (XT.LT..0001) GOTO 16
                    Z=.5*(Y+Z)
 14              CONTINUE

 16              E(IS)=PHIB*Z/INCTE_PI
 
                 IF (IS.NE.0) GOTO 133
                 T_GMAX=E(0)
	         T_GMOY=E(0)
                 T_GMIN=E(0)
		 T_GPI=E(0)
                 GOTO 13
	
 133             T_GMAX = T_GMAX+2*E(IS)			!Recombinaison pour Phi=0
                 T_GMOY = T_GMOY+2*DCOS(IS*PHIB*.5)*E(IS)	!Recombinaison pour Phi=Phib/2
	         T_GMIN = T_GMIN+2*DCOS(IS*PHIB)*E(IS)	    	!Recombinaison pour Phi=Phib
                 T_GPI =  T_GPI+2*DCOS(IS*INCTE_PI*1.D+00)*E(IS)!Recombinaison pour Phi=PI
		 
C*   Test d'arrêt de la décomposition - poursuite de la boucle sur IS sinon.
C---------------------------------------------------------------------------
                 ERR_GMAX = DABS(T_GMAX-GMAX)
	         ERR_GMOY = DABS(T_GMOY-GMOY)
	         ERR_GMIN = DABS(T_GMIN-GMIN)
		 ERR_GPI  = DABS(T_GPI-G_PI)
		 
		 B_GMAX = ERR_GMAX/GMAX
	         B_GMOY = ERR_GMOY/GMOY
	         B_GMIN = ERR_GMIN/GMIN
		 B_GPI  = ERR_GPI/G_PI
		 
		 ERR_GMOY_SUR_GMAX = ERR_GMOY/GMAX
		 ERR_GMIN_SUR_GMAX = ERR_GMIN/GMAX
		 ERR_GPI_SUR_GMAX  = ERR_GPI/GMAX
		 
                 IF (B_GMAX.GT.CTE_THRESHOLD_G_SMAX) GOTO 13
                 IL=IS
                 GOTO 17	!Sortie de boucle
 13           CONTINUE	!Fin de boucle sur IS
              IL=OS_NM
	      NB_CAS_ORDREMAX = NB_CAS_ORDREMAX + 1

	   ENDIF !Fin décomposition pour une fonction G(phi) non nulle
	   
C*   Enregistrement des résultats
C---------------------------------
 17        WRITE(30,err=992)I1,I2,IL,(E(J),J=0,IL)


C*   Détermination de l'angle phi à partir duquel l'erreur de restitution dépasse 5%
C-----------------------------------------------------------------------------------

           SEARCH_PHI5PC=.TRUE.
	   PHI5PC=PHIB
	   I=0
	  
	   DO WHILE ((I.LT.CTE_PH_NU).AND.(SEARCH_PHI5PC)) 
	      PHI = I*PHIB/CTE_PH_NU
	     
	      RECOMP=E(0)
	      DO IS=1,IL
	         RECOMP = RECOMP + 2.*DCOS(IS*PHI)*E(IS)	!Recombinaison pour Phi
	      ENDDO
	     
	      ERR_RECOMP_REL = (RECOMP - U(I)) / U(I)
	      IF (DABS(ERR_RECOMP_REL).GT.0.05) THEN
	          SEARCH_PHI5PC=.FALSE.
	  	  PHI5PC=PHI
	      ENDIF

	      I=I+1  !Passage à la valeur de phi suivante	      
	   ENDDO 



	   IF (TRACE) THEN
	       TAUX_PHI_SOUS_PHI5PC = 100.*PHI5PC/PHIB
	       IF (GMAX.NE.0.D+00) THEN
	           SOM_TAUX_PHI_SOUS_PHI5PC = SOM_TAUX_PHI_SOUS_PHI5PC
     &				              + TAUX_PHI_SOUS_PHI5PC
     		   NB_GMAX_NONNUL = NB_GMAX_NONNUL + 1 
	       ENDIF
	       			
	       IF ((ICOUNT_I1.EQ.1).AND.(ICOUNT_I2.EQ.1)) THEN
	 
	         WRITE(IDLOG,110,err=990) I1, I2, 
     &		   DACOS(C1)*180./INCTE_PI, DACOS(C2)*180./INCTE_PI, 
     &             PHILIM*180./INCTE_PI, PHIB*180./INCTE_PI, 
     &             PHI5PC*180./INCTE_PI, TAUX_PHI_SOUS_PHI5PC,IL,
     &		   GMAX, ERR_GMAX, 100*B_GMAX, 
     &		   GMOY, ERR_GMOY, 100*B_GMOY, 100*ERR_GMOY_SUR_GMAX,
     &		   GMIN, ERR_GMIN, 100*B_GMIN, 100*ERR_GMIN_SUR_GMAX,
     &		   G_PI, ERR_GPI,  100*B_GPI,  100*ERR_GPI_SUR_GMAX 

              ENDIF

	      ICOUNT_I2 = ICOUNT_I2 + 1
	      IF(ICOUNT_I2.GT.ICOUNT_PAS_I2) ICOUNT_I2=0

	       
	      IF ((B_GMAX).GT.MAX_B_GMAX) THEN
	          MAX_B_GMAX = B_GMAX 
	      ENDIF
	            
              IF ((ERR_GMOY_SUR_GMAX).GT.MAX_ERR_GMOY_SUR_GMAX) THEN
	          MAX_ERR_GMOY_SUR_GMAX = ERR_GMOY_SUR_GMAX 
	      ENDIF
	       
	      IF ((ERR_GMIN_SUR_GMAX).GT.MAX_ERR_GMIN_SUR_GMAX) THEN
	          MAX_ERR_GMIN_SUR_GMAX = ERR_GMIN_SUR_GMAX 
	      ENDIF
	       
	      IF ((ERR_GPI_SUR_GMAX).GT.MAX_ERR_GPI_SUR_GMAX) THEN
	          MAX_ERR_GPI_SUR_GMAX = ERR_GPI_SUR_GMAX 
	      ENDIF
	       
	   ENDIF !Trace
	   
	   
	   
 4       CONTINUE	!Fin de boucle sur Teta2 (I2)
 
 	 !Compteur pour sortie trace
         ICOUNT_I1 = ICOUNT_I1 + 1
	 IF(ICOUNT_I1.GT.ICOUNT_PAS_I1) ICOUNT_I1=0
	 
         	       
 3    CONTINUE	!Fin de boucle sur Teta1 (I1)


      IF (TRACE) THEN
         WRITE(IDLOG,*,err=990) ' '
	 WRITE(IDLOG,120,err=990) 100.*MAX_B_GMAX,
     &	 			  100.*MAX_ERR_GMOY_SUR_GMAX,
     &	 			  100.*MAX_ERR_GMIN_SUR_GMAX,
     &	 			  100.*MAX_ERR_GPI_SUR_GMAX
	 WRITE(IDLOG,*,err=990) 
     &	   "==> Global perfo. : Accumulated % of Phi angles for which"
         WRITE(IDLOG,*,err=990) 
     &	   " the error of G(phi) recombination is lower than 5% "
         WRITE(IDLOG,*,err=990) 
     &	 "(not including cases for which Gmax is forced to zero) : ",
     &     (SOM_TAUX_PHI_SOUS_PHI5PC / NB_GMAX_NONNUL)
         WRITE(IDLOG,*,err=990) ' '     
	 WRITE(IDLOG,*,err=990) 'Max relative error (%) ' //
     &	 	'for G(phi=0) over all couples (Teta1, Teta2) : '
	 
	 WRITE(IDLOG,*,err=990) ' - forr Phi = 0       : ',
     &                          100.*MAX_B_GMAX  
         WRITE(IDLOG,*,err=990) ' - forr Phi = Phib/2  : ',
     &                          100.*MAX_ERR_GMOY_SUR_GMAX 
         WRITE(IDLOG,*,err=990) ' - forr Phi = Phib    : ',
     &                          100.*MAX_ERR_GMIN_SUR_GMAX 
         WRITE(IDLOG,*,err=990) ' - for Phi = 180     : ',
     &                          100.*MAX_ERR_GPI_SUR_GMAX 
     
         WRITE(IDLOG,*,err=990) ' '
         WRITE(IDLOG,*,err=990) 
     &     'Statistic of couples (Teta1, Teta2) for which '
         WRITE(IDLOG,*,err=990) 
     &     'the G(phi) expansion reaches the max order : ', OS_NM
	 WRITE(IDLOG,*,err=990) '    Nb of cases      : ', 
     &                          NB_CAS_ORDREMAX
	 WRITE(IDLOG,*,err=990) '    Ratio (%) : ',
     &			100.*NB_CAS_ORDREMAX/(LUM_NBMU*LUM_NBMU)	 
	 
      ENDIF !Trace

            
C* Fin nominale 
C-------------------
      IER=0
      GOTO 9999
      
C* Cas d'erreur : retour du status -1 au programme appelant
C------------------------------------------------------
  920 WRITE(6,*) '  OSOAA_GSF : ERROR_920 on mode interface definition'
      GOTO 9998      
              
  990 WRITE(6,*) '  OSOAA_GSF : ERROR_990 on logfile writing' 
      GOTO 9998
      
  991 WRITE(6,*) '  OSOAA_GSF : ERROR_991 on a file opening'
      GOTO 9998
      
  992 WRITE(6,*) '  OSOAA_GSF : ERROR_992 on file opening'
      WRITE(6,*) '  --> Error on a file reading/writing'
      GOTO 9998

 9998 IER=-1
 9999 CONTINUE

C* Fermeture fichier
C-------------------
      CLOSE(30)

C* Format 
C-------------------  
100   FORMAT(A42,X,I8,X,F7.2,A1) 
		
108   FORMAT(65X,
     &       '|                    BILAN RECOMPOSITION SUR PHI    ')
1091  FORMAT(' I1  I2 ThetaI ThetaT  PhiLim   Phib    Phi5%  ',
     &       'Rate_5% Ordre_dev |            GMAX : Phi = 0',12X, 
     &       '|            GMOY : Phi = Phib/2',14X,
     &       '|            GMIN : Phi = Phib  ',14X,
     &       '|            G_PI : Phi = 180  ') 

1092  FORMAT(' I1  I2 ThetaI ThetaR  PhiLim   Phib    Phi5%  ',
     &       'Rate_5% Ordre_dev |            GMAX : Phi = 0',12X, 
     &       '|            GMOY : Phi = Phib/2',14X,
     &       '|            GMIN : Phi = Phib  ',14X,
     &       '|            G_PI : Phi = 180  ') 
          
1093  FORMAT(65X,'|--------------------------------------',
     &           '|---------------------------------------------',
     &           '|---------------------------------------------',
     &           '|---------------------------------------------')
1094  FORMAT(65X,'|      G        Err_abs      Err_rel(%)',
     &           '|      G        Err_abs    |      Err_rel(%)  ',
     &           '|      G        Err_abs    |      Err_rel(%)  ',
     &           '|      G        Err_abs    |      Err_rel(%)  ')
1095  FORMAT(65X,'|                             / GMAX   ',
     &           '|                          |  / GMOY   / GMAX ',
     &           '|                          |  / GMIN   / GMAX ',
     &           '|                          |  / G_PI   / GMAX ')
     

110   FORMAT(2(I3,1X),2(F5.2,2X),3(F7.2,1X),1X,F6.2,
     &       2X,I4,6X,2h| ,(2(E11.4,1X),F9.2,4X),
     &       3(2h| ,2(E11.4,1X),2(F9.2,1X))) 
120   FORMAT(85X,'MAX : ',F9.2,34X,'MAX : ',F9.2,2(31X,'MAX : ',F9.2))
     
200   FORMAT(3(F12.7,X))  
201   FORMAT(2(F12.7,X),I2) 

205   FORMAT(I5,4X,F6.2,9X,2(I4,8X),3(E11.4,1X))
210   FORMAT(I5,4X,F6.2,3X,2(E11.4,6X))

      RETURN
      END         !FIN DE LA PROCEDURE OSOAA_GSF





C==============================================================================
C PROCEDURE: SOS_CALC_GR
C ==========
C      Cette procédure calcule la fonction de probabilite d'une pente de 
C      vague connaissant les directions d'incidence et de réflexion :
C	 G(Teta1,Teta2,phi) = exp [-tan(TetaN)**2 / SIG] / [ cos(TetaN)**4 * SIG ]
C
C	 avec Teta1 : angle zénithal d'incidence  (0 < Teta1 < PI/2),
C             Teta2 : angle zénithal de réflexion (0 < Teta2 < PI/2),
C             phi   : angle azimutal relatif entre les directions
C                     d'incidence et de réflexion,
C
C	      TetaN : angle zénithal de la normale à la facette de vague,
C                     calculé à partir de Teta1, Teta2 et phi.
C			
C             SIG   : carré du paramètre Sigma de la théorie de Cox et Munck.
C                     SIG = 0.003 + 0.00512 * vent (m/s)
C
C			
C
C Description des paramètres
C --------------------------
C	PHI	(double)   (E) Azimut relatif (radians).
C	C1 	(double)   (E) Cos(Teta1).
C	C2 	(double)   (E) Cos(Teta1).
C	S1	(double)   (E) Sin(Teta1).
C	S2	(double)   (E) Sin(Teta2).
C 	SIG 	(double)   (E) Carré du paramètre Sigma de la théorie de Cox et Munck.
C 	G	(double)   (S) Valeur de G(TetaN).
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
      SUBROUTINE SOS_CALC_GR(C1,C2,S1,S2,SIG,PHI,G)

      IMPLICIT NONE

C* Définition des variables       
C*-----------------------------------------------------------------------
      DOUBLE PRECISION PHI	! Azimut relatif (radians).

      DOUBLE PRECISION C1	! Cos(Teta1).
      DOUBLE PRECISION S1	! Sin(Teta1).
      DOUBLE PRECISION C2	! Cos(Teta2).
      DOUBLE PRECISION S2	! Sin(Teta2).
      
      DOUBLE PRECISION C12	! Cos(Teta1) * Cos(Teta2).
      DOUBLE PRECISION S12	! Sin(Teta1) * Sin(Teta2).
      DOUBLE PRECISION CS12	! 0.5 * [ Cos(Teta1) + Cos(Teta2) ]**2 .

      DOUBLE PRECISION SIG 	! Carré du paramètre Sigma de la théorie
      				! de Cox et Munck.

      DOUBLE PRECISION G	! Valeur de G(TetaN).	
      
      DOUBLE PRECISION COSTETAD	! Cosinus de l'angle de diffusion entre les
      				! directions d'incidence et de réflexion.
      DOUBLE PRECISION X	! 1 / cos(TetaN)**2.

C* Calcul
C---------------------------------
      C12=C1*C2
      S12=S1*S2
      CS12=(C1+C2)
      CS12=.5*CS12*CS12
	   
      COSTETAD=-C12+S12*DCOS(PHI)
      X=(1-COSTETAD)/CS12
	
      G=X*X*DEXP(-(X-1)/SIG) / SIG 
      
      RETURN
      END         !FIN DE LA PROCEDURE SOS_CALC_GR




C==============================================================================
C PROCEDURE: OSOAA_CALC_GT
C ==========
C      Cette procédure calcule la fonction de probabilite d'une pente de 
C      vague connaissant les directions d'incidence et de transmission :
C	 G(Teta1,Teta2,phi) = exp [-tan(TetaN)**2 / SIG] / [ cos(TetaN)**4 * SIG ]
C
C	 avec Teta1 : angle zénithal d'incidence  (0 < Teta1 < PI/2),
C             Teta2 : angle zénithal de transmission (0 < Teta2 < PI/2),
C             phi   : angle azimutal relatif entre les directions
C                     d'incidence et de transmission,
C
C	      TetaN : angle zénithal de la normale à la facette de vague,
C                     calculé à partir de Teta1, Teta2 et phi.
C			
C             SIG   : carré du paramètre Sigma de la théorie de Cox et Munck.
C                     SIG = 0.003 + 0.00512 * vent (m/s)
C
C	
C	      Fp(i) =  sin(i)**2 + cos(i)*racine[p**2 - sin(i)**2] 
C
C 		  avec p = m (indice de l'eau de mer par rapport à l'air) 
C			   dans le cas d'une transmission air -> mer.
C		       p = 1/m dans le cas d'une transmission mer -> air.
C
C		       i : l'angle d'incidence de la réflexion de Fresnel.
C
C                 Une configuration géométrique d'incidence et de transmission donnée,
C                 doit vérifier Fp(i) = Cp
C                 avec Cp  = p*{ cos(Teta1)*cos(Teta2) + sin(Teta1)*sin(Teta2)*cos(phi) }
C                 qui est une constante pour la configuration géométrique fixée.
C
C                 La fonction Fp(i) étant strictement croissante de 0 à pi/2, on estime 
C                 la valeur de i par une recherche dichotomique.
C		
C
C Description des paramètres :
C --------------------------
C
C	PHI	(double)   (E) Azimut relatif (radians).
C	C1 	(double)   (E) Cos(Teta1).
C	C2 	(double)   (E) Cos(Teta1).
C	S1	(double)   (E) Sin(Teta1).
C	S2	(double)   (E) Sin(Teta2).
C       INDP    (double)   (E) Indice p : 
C                                  Indice de l'eau par rapport à l'air pour une  
C                                  transmission air -> mer.
C                                  Valeur inverse pour une transmission mer -> air.
C 	SIG 	(double)   (E) Carré du paramètre Sigma de la théorie de Cox et Munck.
C 	G	(double)   (S) Valeur de G(TetaN).
C
C       CONFIG_STATUS       (boolean)  (S)  Indicateur de validité de la configuration 
C                                           géométrique (True si cas possible).
C       CAS_THETAN_OK  (boolean)  
C                          (S)  Indicateur de validité de l'angle zénithal de vague
C                               (True si cas possible).
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     - Erreur d'écriture dans le fichier Trace
C
C==============================================================================      
      SUBROUTINE OSOAA_CALC_GT(C1,C2,S1,S2,INDP,SIG,PHI,
     & 			       CONFIG_STATUS,CAS_THETAN_OK,G)

      IMPLICIT NONE

C Constantes 
C -----------------            
      INTEGER*2 IDLOG			! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_SURFACE)
      
C* Définition des variables       
C*-----------------------------------------------------------------------
      DOUBLE PRECISION PHI	! Azimut relatif (radians).

      DOUBLE PRECISION C1	! Cos(Teta1) : cosinus de l'angle zénithal d'incidence
      DOUBLE PRECISION S1	! Sin(Teta1).
      DOUBLE PRECISION C2	! Cos(Teta2) : cosinus de l'angle zénithal de transmission
      DOUBLE PRECISION S2	! Sin(Teta2).
      DOUBLE PRECISION CR	! Cos(TetaR) : cosinus de l'angle zénithal de réflexion
      DOUBLE PRECISION SR	! Sin(TetaR) : cosinus de l'angle zénithal de réflexion
      DOUBLE PRECISION SIN_PHI_R ! Sinus de l'angle azimutal de réflexion : sin(PhiR)
      DOUBLE PRECISION COS_PHI_R ! Cosinus de l'angle azimutal de réflexion : cos(PhiR)
      DOUBLE PRECISION PHI_R 	! Valeur de l'angle azimutal de réflexion : PhiR (radians)
      DOUBLE PRECISION ERR_COS  ! Valeur du dépassement autour de 1 et -1.     
      
      DOUBLE PRECISION C12	! Cos(Teta1) * Cos(Teta2).			
      DOUBLE PRECISION S12	! Sin(Teta1) * Sin(Teta2).			
      DOUBLE PRECISION CS12	! 0.5 * [ Cos(Teta1) + Cos(Teta2) ]**2 .	
      DOUBLE PRECISION COSTETAD ! Cosinus de l'angle de diffusion.
      				   
      DOUBLE PRECISION INDP	! Indice p :
      				!   Indice de l'eau par rapport à l'air pour une 
				!   transmission air -> mer.
      				!   Valeur inverse pour une transmission mer -> air.

      DOUBLE PRECISION INDP2	! Carré de l'indice INDP.
      
      DOUBLE PRECISION SIG 	! Carré du paramètre Sigma de la théorie de Cox et Munck.

      DOUBLE PRECISION G	! Valeur de G(TetaN).	

      DOUBLE PRECISION FPI 	! Valeur de la fonction Fp(i) pour un angle d'incidence i

      DOUBLE PRECISION ANG_I    ! Valeur d'un angle d'incidence i
      DOUBLE PRECISION SIN_I    ! Sinus de l'angle d'incidence i
      DOUBLE PRECISION SIN2_I   ! Sinus carré de l'angle d'incidence i
      DOUBLE PRECISION COS_I    ! Cosinus de l'angle d'incidence i
      
      DOUBLE PRECISION PCOS_T   ! Cosinus de l'angle de transmission t (au sens de Fresnel)
      				! multiplié par l'indice p.

      DOUBLE PRECISION COS_N    ! Cosinus de l'angle zénithal de vague : cos(TetaN)
      DOUBLE PRECISION SIN_N    ! Sinus de l'angle zénithal de vague : sin(TetaN)
      DOUBLE PRECISION X	! 1 / cos(TetaN)**2.			      
      
      DOUBLE PRECISION SIN_PHI_N ! Sinus de l'angle azimutal de vague : sin(PhiN)
      DOUBLE PRECISION COS_PHI_N ! Cosinus de l'angle azimutal de vague : cos(PhiN)
      DOUBLE PRECISION PHI_N 	 ! Valeur de l'angle azimutal de vague : PhiN (en radians)
       	          
      DOUBLE PRECISION ANGMIN   ! Valeur minimale de l'angle d'incidence i : 0.
      
      DOUBLE PRECISION F_ANGMIN ! Valeur de la fonction Fp(i) pour i = ANG_MIN  : p
            			! ==> valeur maximale possible de la fonction Fp(i) 
				!     car la fonction est strictement décroissante.
				 
      DOUBLE PRECISION ANGMAX   ! Valeur maximale de l'angle d'incidence i : 
      				! sin(ANG_MAX) = min(1 , p)
      
      DOUBLE PRECISION F_ANGMAX ! Valeur de la fonction Fp(i) pour i = ANG_MAX  : min(1,p*p)
      				! ==> valeur minimale possible de la fonction Fp(i) 
				!     car la fonction est strictement décroissante.
      
      DOUBLE PRECISION ANG_A    ! Valeur d'un angle d'incidence A inf à l'angle i recherché
      DOUBLE PRECISION ANG_B    ! Valeur d'un angle d'incidence B inf à l'angle i recherché
      DOUBLE PRECISION F_ANG_A  ! Valeur de la fonction Fp(A)
      DOUBLE PRECISION F_ANG_B  ! Valeur de la fonction Fp(B)
      				! On a A <= i <= B et Fp(A) >= Cp >= Fp(B)
	    
      DOUBLE PRECISION ANG_I_PREC  ! Valeur de l'angle d'incidence estimée à l'itération 
      				   ! précédente de la dichotomie

      DOUBLE PRECISION DELTA_F  ! Ecart entre Cp et Fp(i) pour i estimé à l'itération 
      				! en cours.
      
      DOUBLE PRECISION DELTA_ANG  ! Ecart entre l'angle i estimé à l'itération en cours 
      				  ! et sa valeur à l'itération précédente.
               
      DOUBLE PRECISION CP 	! Valeur de la fonction Fp(i) correspondant à la 
      				! configuration géométrique d'incidence et de transmission.
				
      LOGICAL CONFIG_STATUS     ! Status possible (TRUE) ou impossible (FALSE) de la 
      				! configuration géométrique incidence - transmission.
      
      LOGICAL CAS_THETAN_OK     ! Indicateur d'une configuration d'angle zénithal de
                                ! vague estimée possible (TRUE si cas rencontré)				
				
      LOGICAL LOOP_IN		! Indicateur de poursuite d'itération de la dichotomie
      
      	
	
C* Traitements
C---------------------------------

      INDP2 = INDP*INDP
      
C- Estimation de l'angle d'incidence de Fresnel
C  ********************************************

C---- Détermination de la valeur Cp que doit valoir Fp(i)
C     pour la configuration géométrique incidence - transmission 
      CP = INDP*(C1*C2+S1*S2*DCOS(PHI))

C---- Détermination des valeurs min et max possibles pour la fonction Fp(i)      
      IF (INDP.LT.1.D+00) THEN
	 ANGMAX = DASIN(INDP)
	 F_ANGMAX = INDP*INDP
      ELSE
	 ANGMAX = INCTE_PI/2		
	 F_ANGMAX = 1.D+00	 
      ENDIF
      
      ANGMIN = 0.D+00
      F_ANGMIN = INDP
      

C---- Contrôle que Cp est dans le domaine de la fonction Fp(i) : 
C         Si non --> configuration incidence - transmission impossible : G = 0.
C         Si oui --> recherche de l'angle d'incidence de Fresnel et calcul de G.    
      	
      !Test sur l'écart entre CP et F_ANGMAX ou F_ANGMIN 
      !plutôt que test CP < F_ANGMAX ou CP > F_ANGMIN
      !pour éviter des pbs d'arrondi de calcul entrainant des fausses
      !déclaration de configuration impossible.
      IF (((CP-F_ANGMAX).LT.CTE_THRESHOLD_GEO_CONFIG).OR.
     &    ((CP-F_ANGMIN).GT.CTE_THRESHOLD_GEO_CONFIG)) THEN
         CONFIG_STATUS = .FALSE.
	 G =  0.D+00
	 
	 CAS_THETAN_OK = .TRUE.   !Non évalué ==> Ne doit pas être compté comme la cause du cas GEO KO
	 
      ELSE  ! Fin du cas : configuration géo impossible
         CONFIG_STATUS = .TRUE.
	 
	 !Recherche de l'angle d'incidence par dichotomie
	 ANG_A   = ANGMIN	!Angle minimal
	 F_ANG_A = F_ANGMIN	!Valeur de Fp correspondant à l'angle minimal 
	 ANG_B   = ANGMAX	!Angle maximal
	 F_ANG_B = F_ANGMAX	!Valeur de Fp correspondant à l'angle maximal 
	 
	 LOOP_IN=.TRUE.
	 ANG_I_PREC = 100.D+00
	 
	 DO WHILE(LOOP_IN)
	 	
	    ANG_I = (ANG_A + ANG_B) / 2.D+00
	    SIN_I = DSIN(ANG_I)
	    COS_I = DCOS(ANG_I)
	    SIN2_I = SIN_I*SIN_I
	    
	    PCOS_T = DSQRT(INDP2-SIN2_I)
	    FPI = SIN2_I + COS_I*PCOS_T
	    
	    !Réaffectation des valeurs de Fp(i1) et Fp(i2) encadrant Cp
	    !==> en considérant que la fonction Fp(i) est décroissante
	    IF (CP.LT.FPI) THEN
	       ANG_A   = ANG_I
	       F_ANG_A = FPI
	    ELSE
	       ANG_B   = ANG_I
	       F_ANG_B = FPI
	    ENDIF
	    
	    DELTA_F = DABS(CP-FPI)
	    DELTA_ANG = DABS(ANG_I-ANG_I_PREC)
	    	
	    !On estime l'écart entre la constante Cp et la valeur Fp(i) estimée
	    !ainsi que l'écart entre l'angle i estimé et celui de l'itération précédente.
	    !==> On stoppe le calcul si un de ces écarts est inférieur au seuil 
	    !    CTE_THRESHOLD_DICHO
	    !
	    ! NB : Le second test a lieu d'être car 
	    !      on peut rencontrer des cas où l'angle n'évolue plus 
	    !      alors que la valeur Fp(i) n'est pas égale à Cp au seuil près.	
	    IF ((DELTA_F.LE.CTE_THRESHOLD_DICHO).OR.
     &	        (DELTA_ANG.LE.CTE_THRESHOLD_DICHO)) THEN
	       LOOP_IN=.FALSE.
	    ENDIF
	    
	    ANG_I_PREC = ANG_I	
	    
	 ENDDO !Fin de la recherche dichotomique
		
	 		
C---- Calcul du cosinus de l'angle zénithal de vague :   
	 COS_N = (C1 -INDP*C2) / (COS_I - PCOS_T)
	 
C---- Calcul de la fonction G     
C     Si l'angle zénithal de vague dépasse 90° (ou est trop rasant), la configuration n'est pas physique : 
C     ==> on impose nulle la probabilité d'avoir cette configuration
C     NB : On applique une probabilité nulle également aux cas d'angle zénithal de vague rasant la
C          surface car elles peuvent induire un problème numérique avec une valeur 1/COS_N qui
C           diverge et une remontée (fausse) de la probabilité pour ces configurations.
         IF (COS_N.GT.CTE_THRESHOLD_COSTHETAN) THEN
	    !Cas d'un angle zénithal de vague < 90°
	    X = 1.D+00/(COS_N*COS_N)
	    G = X*X*DEXP(-(X-1)/SIG) / SIG

	    CAS_THETAN_OK = .TRUE.		 	
	 ELSE
	    !Cas d'un angle de vague impossible (>90° ou rasant) : 
	    !on pose nulle la probabilite d'une vague donnant ce cas 
	    !et on lève l'indicateur de cas d'angle zénithal de vague impossible.
	    !
	    G=0.D+00		
	    
	    CONFIG_STATUS = .FALSE.		  
	    CAS_THETAN_OK = .FALSE.					 
	 ENDIF

		
	 !******* Traces d'expertise ***********************************
         IF (TRACE_DEBUG) THEN


	    !Calcul du cosinus de l'angle zénithal de réflexion :   
	    CR = (1.D+00 + PCOS_T/COS_I)*C1 -2.D+00*INDP*C2	
	    CR = CR / (1.D+00 - PCOS_T/COS_I)	
	 	    
	    !Calcul du sinus de l'angle Thetan (angle zénithal de la normale du dioptre)
	    SIN_N = DSQRT(1.D+00-COS_N*COS_N)
	    
	    !Calcul de l'angle Phin (angle azimutal de la normale du dioptre)
  	    IF (SIN_N.NE.0.D+00) THEN
	       SIN_PHI_N= (INDP*S2*DSIN(PHI)) / ( (COS_I-PCOS_T)*SIN_N)
	       COS_PHI_N= (INDP*S2*DCOS(PHI)-S1) /
     &                    ( (COS_I-PCOS_T)*SIN_N)
	    
	       !Gestion des dépassements de domaine par erreur numérique
	       IF (COS_PHI_N.GT.1.D+00) THEN
	           ERR_COS=COS_PHI_N-1.D+00
	           COS_PHI_N=1.D+00
		   IF(DABS(ERR_COS).GT.1.D-8) THEN
		      WRITE(IDLOG,*,ERR=1010) 
     &		     " Débordement COS_PHI_N > 1 de ",ERR_COS
                   ENDIF
	       ENDIF
	       IF (COS_PHI_N.LT.-1.D+00) THEN
	           ERR_COS=COS_PHI_N+1.D+00
	           COS_PHI_N=-1.D+00
		   IF(DABS(ERR_COS).GT.1.D-8) THEN
		      WRITE(IDLOG,*,ERR=1010) 
     &		     " Débordement COS_PHI_N < -1 de ",ERR_COS
                   ENDIF
	       ENDIF
	       
	       !Calcul de PhiN
	       IF (SIN_PHI_N.GE.0.D+00) THEN
	          PHI_N = DACOS(COS_PHI_N)
	       ELSE
	          PHI_N = -DACOS(COS_PHI_N)
	       ENDIF
	    
	    ELSE
	       PHI_N=-999.
	    ENDIF
	    
	    
	    !Calcul du sinus de l'angle Thetar (angle zénithal de la direction de réflexion)
	    SR = DSQRT(1.D+00-CR*CR)
	    
	    !Calcul de l'angle Phir (angle azimutal de la direction de réflexion)
	    !--> Approche en fixant la direction d'incidence et celle transmise
	    IF (SR.NE.0.D+00) THEN
	       SIN_PHI_R = 2*INDP*S2*DSIN(PHI)
	       SIN_PHI_R = SIN_PHI_R / ( (1.D+00 - PCOS_T/COS_I)*SR )
	    
	       COS_PHI_R = 2*INDP*S2*DCOS(PHI)-(1.D+00+PCOS_T/COS_I)*S1
	       COS_PHI_R = COS_PHI_R / ( (1.D+00 - PCOS_T/COS_I)*SR )
	    
	       !Gestion des dépassements de domaine par erreur numérique
	       IF (COS_PHI_R.GT.1.D+00) THEN
	           ERR_COS=COS_PHI_R-1.D+00
	           COS_PHI_R=1.D+00
		   IF(DABS(ERR_COS).GT.1.D-8) THEN
		      WRITE(IDLOG,*,ERR=1010) 
     &		     " Débordement COS_PHI_R > 1 de ",ERR_COS
                   ENDIF
	       ENDIF
	       IF (COS_PHI_R.LT.-1.D+00) THEN
	           ERR_COS=COS_PHI_R+1.D+00
	           COS_PHI_R=-1.D+00
		   IF(DABS(ERR_COS).GT.1.D-8) THEN
		      WRITE(IDLOG,*,ERR=1010) 
     &		     " Débordement COS_PHI_R < -1 de ",ERR_COS
                   ENDIF
	       ENDIF
	       
	       !Calcul de PhiR
	       IF (SIN_PHI_R.GE.0.D+00) THEN
	          PHI_R = DACOS(COS_PHI_R)
	       ELSE
	          PHI_R = -DACOS(COS_PHI_R)
	       ENDIF	  
	    ELSE
	       PHI_R=-999.
	    ENDIF
                
	    !Calcul de l'angle Thetan (angle zénithal de la normale du dioptre)
	    !--> Approche en fixant la direction d'incidence et celle de réflexion
	    !    en utilisant les valeurs Phir et Thetar déduites du couple de directions
	    !    incidente et transmise.
	    IF (CR.GT.0.D+00) THEN !Cas d'une réflexion montante
	       C12=C1*CR
               S12=S1*SR
               CS12=(C1+CR)
               CS12=.5*CS12*CS12
	   
               COSTETAD=-C12+S12*DCOS(PHI_R)
               X=DSQRT(CS12/(1-COSTETAD))
	       
	       !Contrôle de la cohérence du cosinus de l'angle de vague
	       !pour une estimation à partir des directions d'incidence et de transmission
	       !et à partir des directions d'incidence et de réflexion
	       IF(DABS(X-COS_N).GT.1.D-03) THEN
	          WRITE(IDLOG,*,ERR=1010) " "
	    
		  IF (INDP.GT.1.D+00) THEN
		    WRITE(IDLOG,*,ERR=1010) 
     &             'Incohérence du triplet inc. - transm. - réflexion',
     &             ' ==> cas TAW'
		  ELSE
		    WRITE(IDLOG,*,ERR=1010) 
     &             'Incohérence du triplet inc. - transm. - réflexion',
     &             ' ==> cas TWA'
		  ENDIF
	       
	          WRITE(IDLOG,*,ERR=1010) 
     &            '--> Couple incidence - transmission :'
	          WRITE(IDLOG,100,ERR=1010) 
     &            '  C1:',C1,'C2:   ',C2,'PHI_T:',PHI,
     &	 	  'COS_THETA_N:',COS_N,'PHI_N:',PHI_N,
     &	 	  'COS_I:',COS_I,'COS_T:',PCOS_T/INDP
	          WRITE(IDLOG,*,ERR=1010) 
     &            '--> Couple incidence - réflexion :'
	          WRITE(IDLOG,110,ERR=1010) 
     &	          '  C1:',C1,'COS_R:',CR,'PHI_R:',PHI_R,
     &            'COS_THETA_N:',X,
     &	          'SIN_PHI_R:',SIN_PHI_R,'COS_PHI_R:',COS_PHI_R,
     &	          'SIN_PHI_N:',SIN_PHI_N,'COS_PHI_N:',COS_PHI_N
	       ENDIF
	    
	    ENDIF      
    	    
	 ENDIF !Fin Trace d'expertise
	 !**************************************************************


	
      ENDIF ! Fin du cas : configuration géo possible

      	
C* Fin nominale 
C-------------------
      GOTO 9999
            
C* Cas d'erreur et retour du status -1 au programme appelant
C----------------------------------------------
 1010 WRITE(6,*) '  OSOAA_CALC_GT ERROR_1010 on logfile writing' 
      
      
 9999 CONTINUE     	 
	  
      RETURN
      
      
C* Format 
C-------------------  
100   FORMAT(A5,1X,F8.4,1X,A6,1X,F8.4,1X,A6,1X,F8.4,1X,A12,1X,F8.4,1X,
     &       3(A6,1X,F8.4,1X))  
110   FORMAT(A5,1X,F8.4,1X,2(A6,1X,F8.4,1X),A12,1X,F8.4,1X,
     &       4(A10,1X,F8.4,1X))  
      
      END         !FIN DE LA PROCEDURE OSOAA_CALC_GT









C==============================================================================
C PROCEDURE: OSOAA_CALC_COSI_TRANS
C ==========
C      Cette procédure calcule le cos(i) de l'incidence d'une transmission
C      à partir de la valeur du cosinus de l'angle de déviation
C      cos(ThetaDevTransmission)
C
C      Méthode :
C	
C	      Fp(i) =  sin(i)**2 + cos(i)*racine[p**2 - sin(i)**2] 
C
C 		  avec p = rapport N2 / N1 
C                         (indice du milieu de réfraction / incidence du milieu d'incidence)

C                      p = m (indice de l'eau de mer par rapport à l'air) 
C			   dans le cas d'une transmission air -> mer.
C		       p = 1/m dans le cas d'une transmission mer -> air.
C
C		       i : l'angle d'incidence de la réflexion de Fresnel.
C
C                 Une configuration géométrique d'incidence et de transmission donnée,
C                 doit vérifier Fp(i) = Cp
C                 avec Cp  = p*{ cos(Teta1)*cos(Teta2) + sin(Teta1)*sin(Teta2)*cos(phi) }
C
C                      soit Cp = p*cos(ThetaDevTransmission)
C                 une constante pour la configuration géométrique fixée.
C
C                 La fonction Fp(i) étant strictement croissante de 0 à pi/2, on estime 
C                 la valeur de i par une recherche dichotomique.
C		
C
C Description des paramètres : 
C --------------------------
C	INDP		(double)   (E) Valeur de l'indice p.
C	COSDEVTR	(double)   (E) Cosinus de l'angle de déviation.
C       CONFIG_STATUS   (boolean)  (S) Indicateur de validité de la configuration 
C                                      géométrique (True si cas possible)
C 	COS_I		(double)   (S) Valeur du cosinus de l'angle d'incidence.
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
      SUBROUTINE OSOAA_CALC_COSI_TRANS(INDP,COSDEVTR,
     & 			               CONFIG_STATUS,COS_I)

      IMPLICIT NONE


      
C* Définition des variables       
C*-----------------------------------------------------------------------
      DOUBLE PRECISION COSDEVTR	! Cosinus de l'angle de déviation.
				   
      DOUBLE PRECISION INDP	! Indice p :
      				!   Indice de l'eau par rapport à l'air pour une 
				!   transmission air -> mer.
      				!   Valeur inverse pour une transmission mer -> air.

      DOUBLE PRECISION INDP2	! Carré de l'indice INDP.
      
      DOUBLE PRECISION FPI 	! Valeur de la fonction Fp(i) pour un angle d'incidence i

      DOUBLE PRECISION ANG_I    ! Valeur d'un angle d'incidence i
      DOUBLE PRECISION SIN_I    ! Sinus de l'angle d'incidence i
      DOUBLE PRECISION SIN2_I   ! Sinus carré de l'angle d'incidence i
      DOUBLE PRECISION COS_I    ! Cosinus de l'angle d'incidence i
      
      DOUBLE PRECISION PCOS_T   ! Cosinus de l'angle de transmission t (au sens de Fresnel)
      				! multiplié par l'indice p.
       	          
      DOUBLE PRECISION ANGMIN   ! Valeur minimale de l'angle d'incidence i : 0.
      
      DOUBLE PRECISION F_ANGMIN ! Valeur de la fonction Fp(i) pour i = ANG_MIN  : p
            			! ==> valeur maximale possible de la fonction Fp(i) 
				!     car la fonction est strictement décroissante.
				 
      DOUBLE PRECISION ANGMAX   ! Valeur maximale de l'angle d'incidence i : 
      				! sin(ANG_MAX) = min(1 , p)
      
      DOUBLE PRECISION F_ANGMAX ! Valeur de la fonction Fp(i) pour i = ANG_MAX  : min(1,p*p)
      				! ==> valeur minimale possible de la fonction Fp(i) 
				!     car la fonction est strictement décroissante.
      
      DOUBLE PRECISION ANG_A    ! Valeur d'un angle d'incidence A inf à l'angle i recherché
      DOUBLE PRECISION ANG_B    ! Valeur d'un angle d'incidence B inf à l'angle i recherché
      DOUBLE PRECISION F_ANG_A  ! Valeur de la fonction Fp(A)
      DOUBLE PRECISION F_ANG_B  ! Valeur de la fonction Fp(B)
      				! On a A <= i <= B et Fp(A) >= Cp >= Fp(B)
	    
      DOUBLE PRECISION ANG_I_PREC  ! Valeur de l'angle d'incidence estimée à l'itération 
      				   ! précédente de la dichotomie

      DOUBLE PRECISION DELTA_F     ! Ecart entre Cp et Fp(i) pour i estimé à l'itération 
      				   ! en cours.
      
      DOUBLE PRECISION DELTA_ANG  ! Ecart entre l'angle i estimé à l'itération en cours 
      				  ! et sa valeur à l'itération précédente.
               
      DOUBLE PRECISION CP 	! Valeur de la fonction Fp(i) correspondant à la 
      				! configuration géométrique d'incidence et de transmission.
				
      LOGICAL CONFIG_STATUS     ! Status possible (TRUE) ou impossible (FALSE) de la 
      				! configuration géométrique incidence - transmission.
      	
      LOGICAL LOOP_IN		! Indicateur de poursuite d'itération de la dichotomie
      
      	
	  
	
C* Traitements
C---------------------------------

      INDP2 = INDP*INDP
      
C- Estimation de l'angle d'incidence de Fresnel
C  ********************************************

C---- Détermination de la valeur Cp que doit valoir Fp(i)
C     pour la configuration géométrique incidence - transmission 
      CP = INDP*COSDEVTR
      
C---- Détermination des valeurs min et max possibles pour la fonction Fp(i)      
      IF (INDP.LT.1.D+00) THEN
	 ANGMAX = DASIN(INDP)
	 F_ANGMAX = INDP*INDP
      ELSE
	 ANGMAX = INCTE_PI/2		
	 F_ANGMAX = 1.D+00	 
      ENDIF
      
      ANGMIN = 0.D+00
      F_ANGMIN = INDP
      

C---- Contrôle que Cp est dans le domaine de la fonction Fp(i) : 
C         Si non --> configuration incidence - transmission impossible : G = 0.
C         Si oui --> recherche de l'angle d'incidence de Fresnel et calcul de G.    
      	
      IF ((CP.LT.F_ANGMAX).OR.(CP.GT.F_ANGMIN)) THEN
         CONFIG_STATUS = .FALSE.
	 COS_I = -999.
	 
      ELSE  ! Fin du cas : configuration géo impossible
         CONFIG_STATUS = .TRUE.
	 
	 !Recherche de l'angle d'incidence par dichotomie
	 ANG_A   = ANGMIN	!Angle minimal
	 F_ANG_A = F_ANGMIN	!Valeur de Fp correspondant à l'angle minimal 
	 ANG_B   = ANGMAX	!Angle maximal
	 F_ANG_B = F_ANGMAX	!Valeur de Fp correspondant à l'angle maximal 
	 
	 LOOP_IN=.TRUE.
	 ANG_I_PREC = 100.D+00
	 
	 DO WHILE(LOOP_IN)
	 	
	    ANG_I = (ANG_A + ANG_B) / 2.D+00
	    SIN_I = DSIN(ANG_I)
	    COS_I = DCOS(ANG_I)
	    SIN2_I = SIN_I*SIN_I
	    
	    PCOS_T = DSQRT(INDP2-SIN2_I)
	    FPI = SIN2_I + COS_I*PCOS_T
	    
	    !Reaffectation des valeurs de Fp(i1) et Fp(i2) encadrant Cp
	    !==> en considérant que la fonction Fp(i) est décroissante
	    IF (CP.LT.FPI) THEN
	       ANG_A   = ANG_I
	       F_ANG_A = FPI
	    ELSE
	       ANG_B   = ANG_I
	       F_ANG_B = FPI
	    ENDIF
	    
	    DELTA_F = DABS(CP-FPI)
	    DELTA_ANG = DABS(ANG_I-ANG_I_PREC)
	    	
	    !On estime l'écart entre la constante Cp et la valeur Fp(i) estimée
	    !ainsi que l'écart entre l'angle i estimé et celui de l'itération précédente.
	    !==> On stoppe le calcul si un de ces écarts est inférieur au seuil 
	    !    CTE_THRESHOLD_DICHO
	    !
	    ! NB : Le second test a lieu d'être car 
	    !      on peut rencontrer des cas où l'angle n'évolue plus 
	    !      alors que la valeur Fp(i) n'est pas égale à Cp au seuil près.	
	    IF ((DELTA_F.LE.CTE_THRESHOLD_DICHO).OR.
     &	        (DELTA_ANG.LE.CTE_THRESHOLD_DICHO)) THEN
	       LOOP_IN=.FALSE.
	    ENDIF
	    
	    ANG_I_PREC = ANG_I	
	    
	 ENDDO !Fin de la recherche dichotomique

      ENDIF ! Fin du cas : configuration geo possible
	  
      RETURN

      
      END         !FIN DE LA PROCEDURE OSOAA_CALC_COSI_TRANS



