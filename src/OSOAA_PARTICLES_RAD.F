C******************************************************************************
C* FICHIER: OSOAA_PARTICLES_RAD.F
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
C* ROLE: Ensemble de routines propres au calcul des propriétés radiatives
C*       des aérosols et hydrosols.
C*
C* AUTEURS:  
C*
C*   Fondement algorithmique pour les calculs des fonctions de phase et 
C*   leurs développements fonctions de Legendre :
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
C*   - SOS_INTERPOL
C*   - SOS_DECOMPO_LEGENDRE
C*   - OSOAA_GRANU
C*   - SOS_INTERPO_SPLINT
C*   - SOS_SPLINE
C*   - SOS_SPLINT
C*   - OSOAA_ENREG_GRANU
C*
C*
C*
C* MOD:VERSION:1.0: 02/04/2015: Initial version of the OSOAA code
C* MOD:VERSION:1.1: 15/06/2015: All comments for log files are now in english
C* MOD:VERSION:1.2: 23/05/2018: 
C*      Routine OSOAA_ENREG_GRANU is changed in version VERSION:1.1    
C*
C*                                   
C*   
C******************************************************************************

C----------------------------------------------------------------------------
C Définition des constantes  
C---------------------------------------------------------------------------- 
C Constantes utilisées : 
C
C    CTE_LENFIC2 : Longueur des noms de fichiers avec arborescence.
C    CTE_MIE_NBMU_MAX : Nombre maximal d'angles de Gauss positifs 
C                       pour les calculs de MIE
C    CTE_OS_NB_MAX : Ordre maximal de la décomposition en séries de Fourier
C    et ordre limite de la décomposition en polynômes de Legendre des fonctions
C    de phase et de la luminance.
C    CTE_PH_SEUIL_TRONCA : Valeur seuil pour la troncature.
C
C Constantes spécifiques :
C    INCTE_IDLOG_PARTICULES : ID du ficher Trace de la routine
C	==> Il faut le mme ID que l'appelant soit la routine OSOAA_AEROSOLS 
C           ou OSOAA_HYDROSOLS
C    INCTE_PI : Valeur de PI (calculé par la machine)
C----------------------------------------------------------------------------
#include "OSOAA.h"
#define INCTE_IDLOG_PARTICULES 99
#define INCTE_PI DACOS(-1.D+00)



C==============================================================================
C PROCEDURE: SOS_INTERPOL
C ==========
C      Cette procédure permet d'interpoler linéairement en X une fonction f
C      dont on connait les valeurs (X1,Y1) et (X2,Y2).
C
C Description des paramètres
C --------------------------
C	X1 (double) : (E)
C	X2 (double) : (E)
C	Y1 (double) : (E) Valeur f(X1)
C	Y2 (double) : (E) Valeur f(X2)
C	X (double)  : (E) Abscisse pour laquelle on interpole la valeur f(X)
C       Y (double)  : (S) Valeur f(X) interpolée
C 
C
C==============================================================================
      SUBROUTINE SOS_INTERPOL(Y1,Y2,X1,X2,X,Y)

      IMPLICIT NONE

C* Définition des variables       
C*-----------------------------------------

      DOUBLE PRECISION Y	
      DOUBLE PRECISION Y1
      DOUBLE PRECISION Y2
      DOUBLE PRECISION X
      DOUBLE PRECISION X1
      DOUBLE PRECISION X2      
      
C* Calcul du paramètre interpole
C  -----------------------------
      Y = ((Y2-Y1)/(X2-X1))*(X-X2) + Y2

      END	!FIN DE LA PROCEDURE SOS_INTERPOL





C==============================================================================
C PROCEDURE: SOS_DECOMPO_LEGENDRE
C ==========
C       Cette procédure calcule les paramètres alpha, beta, gamma delta et zeta
C	de la décomposition de la fonction de phase en polynômes de Legendre.
C	Application éventuelle de la troncature.
C
C       Les fonctions de phase sont stockées dans des tableaux définis jusqu'à
C       CTE_MIE_NBMU_MAX mais sont utilisées jusqu'à MIE_NBMU (angles effectifs).
C
C Description des paramètres
C --------------------------
C	ITRONC (I4) : (E/S) Option de troncature (1 pour la réaliser).
C                           Mise a 0 en sortie si la troncature est trop faible.
C       MU1_TRONCA (double) : (E) cosinus des angles Theta1 et Theta2 pour la définition  
C       MU2_TRONCA (double) : (E) du segment tronquant la fonction de phase

C	TRACE (logical) : (E)  =vrai si écriture dans le fichier trace
C       MIE_NBMU (I4) : (E) Nombre d'angles (positifs) effectivement utiles
C	XMU(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX) (double) : (E) Cosinus des angles de Gauss.
C	XHR(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX) (double) : (E) Poids des angles de Gauss.
C	P11(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX) (double)  : 
C						 (E) Fonction de phase calculée 
C					         à partir des fonctions la théorie de MIE.
C	P12(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX) (double)  : 
C						 (E) Elément P12 de la matrice de phase.
C	P33(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)	(double) : 
C						 (E) Elément P33 de la matrice de phase.
C	TTT(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX) (double) : 
C 						 (S) Copie de la fonction de phase (P11) 
C					 	 avant troncature.
C	A (double)		     : (S) Coefficient de troncature.
C	Z1 (double)		     : (S) Valeur de BETA11(0) avant normalisation.
C	ALP(0:CTE_OS_NB_MAX) (double)     : (S) Paramètres alpha(k).
C	BETA11(0:CTE_OS_NB_MAX) (double)  : (S) Paramètres beta11(k).
C	BETA22(0:CTE_OS_NB_MAX) (double)  : (S) Paramètres beta22(k).
C	GAMMA12(0:CTE_OS_NB_MAX) (double) : (S) Paramètres gamma12(k).
C	ZETA(0:CTE_OS_NB_MAX) (double)    : (S) Paramètres zeta(k).
C	DELTA33(0:CTE_OS_NB_MAX) (double)  : (S) Paramètres delta33(k).
C	IER (I4)		     : (S) Code d'erreur =0 si pas d'erreur, sinon =-1.	
C
C 
C Description des fichiers utilisés
C ---------------------------------
C     Aucun
C
C Constantes utilisées :
C --------------------
C    CTE_MIE_NBMU_MAX : Nombre maximal d'angles positifs.
C    CTE_OS_NB_MAX :    Valeur maximale pour l'ordre limite du développement en 
C                       polynômes de Legendre.
C    CTE_PH_SEUIL_TRONCA : Valeur seuil pour la troncature.
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     -Ecriture dans un fichier
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant
C
C==============================================================================
      SUBROUTINE SOS_DECOMPO_LEGENDRE(ITRONC,MU1_TRONCA,MU2_TRONCA,
     &                                TRACE,MIE_NBMU,XMU,XHR,
     &				      OS_NB,P11,TTT,P12,P22,P33,A,Z1,
     &				      ALP,BETA11,BETA22,
     &                                GAMMA12,DELTA33,ZETA,IER)

      IMPLICIT NONE

C Index de fichier
C -----------------
      INTEGER*2 IDLOG		! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_PARTICULES)
      
C* Définition des variables       
C*-----------------------------------------
      DOUBLE PRECISION XMU(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)  
                                                  !Cosinus des angles de Gauss.
      DOUBLE PRECISION XHR(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)  
                                                  !Poids des angles de Gauss.
      
      DOUBLE PRECISION P11(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)	
                       !Fonction de phase calculée à partir des fonctions de 
		       !la théorie de MIE.
      DOUBLE PRECISION P12(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)	
                       !Elément P12 de la matrice de phase.
      DOUBLE PRECISION P22(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)	
                       !Elément P22 de la matrice de phase.
      DOUBLE PRECISION P33(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)	
                       !Elément P33 de la matrice de phase.

      DOUBLE PRECISION TTT(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)	
                       !Copie de la fonction de phase (P11) avant troncature.

      DOUBLE PRECISION MU1_TRONCA ! cosinus des angles Theta1 et Theta2 pour la définition  
      DOUBLE PRECISION MU2_TRONCA ! du segment tronquant la fonction de phase		       
		
      DOUBLE PRECISION PL(-1:CTE_OS_NB_MAX+1) 
                       !Polynômes de Legendre à l'ordre L calculé pour un angle.
      DOUBLE PRECISION POL(0:CTE_OS_NB_MAX+1) 
                       !Fonction de Legendre d'ordre S=2 et d'ordre L variable
		       !(calculé pour un angle).
		       			
      DOUBLE PRECISION D	!Variable pour le calcul de POL.
      DOUBLE PRECISION E	!Variable pour le calcul de POL.
      		       
      DOUBLE PRECISION X
      DOUBLE PRECISION XRMU	!Valeur utilisée du tableau des angles de Gauss.
				
      DOUBLE PRECISION XX	
      DOUBLE PRECISION XXX

      DOUBLE PRECISION A	!Coefficient de troncature.    
      DOUBLE PRECISION AA	!Pente de la droite de troncature.
      DOUBLE PRECISION X1	
      DOUBLE PRECISION X2


      DOUBLE PRECISION CO1	!Variable pour le calcul des ALP(k) et ZETA(k).
      DOUBLE PRECISION CO2	!Variable pour le calcul des ALP(k) et ZETA(k).
      DOUBLE PRECISION CO3	!Variable pour le calcul des ALP(k) et ZETA(k).

      DOUBLE PRECISION SOM1  	!Variable pour le calcul des ALP(k) et ZETA(k).
      DOUBLE PRECISION SOM2	!Variable pour le calcul des ALP(k) et ZETA(k).
      DOUBLE PRECISION SOM3	!Variable pour le calcul des ALP(k) et ZETA(k).
      DOUBLE PRECISION SOM4	!Variable pour le calcul des ALP(k) et ZETA(k).
      
      DOUBLE PRECISION Z1	!Valeur de BETA11(0) avant normalisation.

      DOUBLE PRECISION ALP(0:CTE_OS_NB_MAX)	! Paramètres alpha(k).
      DOUBLE PRECISION BETA11(0:CTE_OS_NB_MAX)	! Paramètres beta11(k).
      DOUBLE PRECISION BETA22(0:CTE_OS_NB_MAX)	! Paramètres beta22(k).
      DOUBLE PRECISION GAMMA12(0:CTE_OS_NB_MAX)	! Paramètres gamma12(k).
      DOUBLE PRECISION ZETA(0:CTE_OS_NB_MAX)	! Paramètres zeta(k).
      DOUBLE PRECISION DELTA33(0:CTE_OS_NB_MAX)	! Paramètres delta33(k).


      DOUBLE PRECISION P11_INIT_TR !Elément P11 de la matrice de phase tronquée.
      DOUBLE PRECISION P12_INIT_TR !Elément P12 de la matrice de phase tronquée.
      DOUBLE PRECISION P22_INIT_TR !Elément P22 de la matrice de phase tronquée.
      DOUBLE PRECISION P33_INIT_TR !Elément P33 de la matrice de phase tronquée.                        
      DOUBLE PRECISION P11_RECOMP  !Elément P11 de la matrice de phase recomposée.
      DOUBLE PRECISION P12_RECOMP  !Elément P12 de la matrice de phase recomposée.
      DOUBLE PRECISION P22_RECOMP  !Elément P22 de la matrice de phase recomposée.
      DOUBLE PRECISION P33_RECOMP  !Elément P33 de la matrice de phase recomposée.
		       
      INTEGER*4 MIE_NBMU   ! Nombre d'angles (positifs) effectivement utiles 
      INTEGER*4 ITRONC	   ! Option de troncature (1 pour la réaliser).
      INTEGER*4 OS_NB 	   ! Ordre des développements des fonctions de phase 
			   ! en Polynômes de Legendre.      
      INTEGER*4 K          ! Indice de tableau.
      INTEGER*4 KK	   ! Indice du tableau des angles de Gauss pour le mu
      			   ! le plus proche de 0.94 par valeur supérieure.			               
      INTEGER*4	NN
      INTEGER*4	MM				
      INTEGER*4 I	   ! Indice.  
      INTEGER*4 J	   ! Indice.      
      INTEGER*4 IER	   ! Code d'erreur =0 si pas d'erreur, =1 sinon.         

      INTEGER*4 ANG_GAUSS  ! Indicateur si angle = angle de Gauss (1 si oui, 0 si non)


      LOGICAL TESS	   !Variable pour le calcul de Troncature. 
      LOGICAL TRACE	   !=vrai si écriture dans le fichier trace 
      
      
C* Copie de la fonction de phase en intensité avant la troncature.
C-----------------------------------------------------------------
      DO 55 J=-MIE_NBMU,MIE_NBMU
         TTT(J)=P11(J)
  55  CONTINUE 
      
C==============
C* TRONCATURE
C============== 

      IF(ITRONC.EQ.0) GO TO 59

C* Troncature de la fonction de phase en intensité pour les angles 
C* inférieurs à 19.95 degrés (mu = 0.94) ==> MU1_TRONCA
C------------------------------------------------------------------------------

C* Détermination des angles de Gauss juste inférieurs à 36.87 degrés (mu=0.8 : MU1_TRONCA)
C* et à 19.95 degrés (mu = 0.94 : MU2_TRONCA) par exemple typiquement pour les aérosols.
C--------------------------------------------------------------------
      TESS=.TRUE.
      DO 56 J=1,MIE_NBMU
         IF (.NOT.(TESS.AND.(XMU(J).GT.MU1_TRONCA)
     &	     .AND.(XHR(J).NE.0.))) GO TO 56
         K=J-1
         TESS=.FALSE.
  56  CONTINUE
  
      TESS=.TRUE.
      DO 57 J=1,MIE_NBMU
         IF (.NOT.(TESS.AND.(XMU(J).GT.MU2_TRONCA)
     &	     .AND.(XHR(J).NE.0.)))  GO TO 57
         KK=J-1
         TESS=.FALSE.
  57  CONTINUE

      IF (TRACE) THEN
         WRITE(IDLOG,*,err=921) 
     &	    'Truncation applied for a segment defined between :'
         WRITE(IDLOG,*,err=921) 'Mu1 :',MU1_TRONCA, 
     &	       ' --> Theta1 = ', DACOS(MU1_TRONCA)*180./INCTE_PI
         WRITE(IDLOG,*,err=921) 'Mu1_effective :',XMU(K), 
     &	       ' --> Theta1_eff = ', DACOS(XMU(K))*180./INCTE_PI
	 WRITE(IDLOG,*,err=921) 'Mu2 :',MU2_TRONCA, 
     &	       ' --> Theta2 = ', DACOS(MU2_TRONCA)*180./INCTE_PI
         WRITE(IDLOG,*,err=921) 'Mu2_effective :',XMU(KK), 
     &	       ' --> Theta2_eff = ', DACOS(XMU(KK))*180./INCTE_PI
	 WRITE(IDLOG,*,err=921) ""
      ENDIF
      
C* Détermination de la pente de la droite "log10(p(cos(teta))) = AA * teta + BB"
C* passant par les valeurs de log10 (p) pour les mu de Gauss ci-dessus.
C--------------------------------------------------------------------------
      AA=(DLOG10(P11(KK))-DLOG10(P11(K)))/(DACOS(XMU(KK))-DACOS(XMU(K)))

C* Application de la troncature
C-------------------------------
      X1=DLOG10(P11(KK))
      X2=DACOS(XMU(KK))
      
      DO 58 J=KK+1,MIE_NBMU
         IF(DABS(XMU(J)-1.).LE.1D-08) A=X1-AA*X2
         A=X1+AA*(DACOS(XMU(J))-X2)
         P11(J)=10**A
  58  CONTINUE

  59  CONTINUE


C* Calcul des paramètres BETA11(k) de la décomposition de la fonction
C* de phase (éventuellement tronquée) en polynômes de Legendre.
C------------------------------------------------------------------
      DO 61 J=-MIE_NBMU,MIE_NBMU
         IF(J.EQ.0) GO TO 61
         X=P11(J)*XHR(J)
         XRMU=XMU(J)

C* Calcul des polynômes de Legendre d'ordre K, pour la valeur de mu fixée.
C------------------------------------------------------------------------
         DO 60 K=0,OS_NB
            PL(-1)=0.
            PL(0)=1.
            PL(K+1)=((2*K+1.)*XRMU*PL(K)-K*PL(K-1))/(K+1.)
            BETA11(K)=BETA11(K)+X*PL(K)
  60     CONTINUE
  61  CONTINUE

      DO 62 K=0,OS_NB
         BETA11(K)=(2*K+1)*BETA11(K)*.5
  62  CONTINUE



C* Calcul du coefficient de troncature
C--------------------------------------
      IF (ITRONC.EQ.1) THEN
         A=2*(1-BETA11(0))
      ELSE
         A=0.0
      ENDIF

C* Si le coefficient de troncature est inférieur au seuil Seuil_Tronca
C* la troncature est annulée.   
C----------------------------------------------------------------------	
      IF((ITRONC.EQ.1).AND.(A.LT.CTE_PH_SEUIL_TRONCA)) THEN
      
         IF (TRACE) THEN
            WRITE(IDLOG,*,err=921) 'Troncation coefficient A = ',A,
     &            ' Value too weak : TRONCATION IS CANCELED'
   
	 ENDIF
	 
C* Recopie de la fonction de phase en intensité avant la troncature
C* et re-initialisation des paramètres de la décomposition de la matrice
C* de phase
C-------------------------------------------------------------------
	 DO 66 J=-MIE_NBMU,MIE_NBMU
            P11(J)=TTT(J)
  66  	 CONTINUE
    
         ITRONC=0
	 
	 DO 67 K=0,OS_NB
             BETA11(K)=0.D+00
	     BETA22(K)=0.D+00
             GAMMA12(K)=0.D+00
             DELTA33(K)=0.D+00
             ALP(K)=0.D+00
             ZETA(K)=0.D+00
 67      CONTINUE
  
	 GOTO 59 
      ENDIF    !Fin test si troncature trop faible.
  

C* Calcul des paramètres GAMMA12(k), BETA22(K) et DELTA33(k) de la décomposition des 
C* fonctions de phase polarisées P12, P22 et P33 en polynômes ou fonctions de Legendre.
C* Application de l'éventuelle troncature par le rapport de la fonction de phase
C* en intensité tronquée sur celle initiale: P11(J)/TTT(J).
C-----------------------------------------------------------------------------	
      DO 11 J=-MIE_NBMU,MIE_NBMU       
	 IF(J.EQ.0)  GO TO 11
       
         XRMU=XMU(J)
	 
         PL(-1)=0.
         PL(0)=1.
	 POL(0)=0.
         POL(1)=0.
         POL(2)=3.*(1.-XRMU**2)/2./DSQRT(6.0D+00)
	 
	 XXX=XHR(J)*P12(J)*P11(J)/TTT(J)
         DO 12 K=2,OS_NB
            D=(2.*K+1.)/DSQRT(1.D+00*(K+3.)*(K-1.))
            E=DSQRT(1.D+00*(K+2.)*(K-2.))/(2.*K+1.)
            POL(K+1)=D*(XRMU*POL(K)-E*POL(K-1))
            GAMMA12(K)=GAMMA12(K)+XXX*POL(K)
  12     CONTINUE
    	
	 
         X=XHR(J)*P22(J)*(P11(J)/TTT(J))   	!Parentheses necessaires pour assurer 
         XX=XHR(J)*P33(J)*(P11(J)/TTT(J))	!la non-régression stricte
         DO 14 K=0,OS_NB
            PL(K+1)=((2.*K+1.)*XRMU*PL(K)-K*PL(K-1))/(K+1.)
	    BETA22(K)=BETA22(K)+X*PL(K)
            DELTA33(K)=DELTA33(K)+XX*PL(K)
  14     CONTINUE
  
  11  CONTINUE
   
      DO 15 K=0,OS_NB
         BETA22(K)  = BETA22(K) *(2.*K+1.)*.5
         DELTA33(K) = DELTA33(K)*(2.*K+1.)*.5
         GAMMA12(K) = GAMMA12(K)*(2.*K+1.)*.5
   15 CONTINUE

    
C* Calcul des paramètres ALP(k) et ZETA(k) en fonctions des BETA22(k) et DELTA33(k).
C----------------------------------------------------------------------------------
      DO 16 I=2,OS_NB
         CO1=4*(2*I+1.)/I/(I-1.)/(I+1.)/(I+2.)
         CO2=I*(I-1.)/((I+1.)*(I+2.))
         CO3=CO2*DELTA33(I)
         CO2=CO2*BETA22(I)
         NN=I*.5
         MM=(I-1)*.5
         SOM1=0.
         SOM2=0.
         SOM3=0.
         SOM4=0.
         DO 17 J=1,NN
            X2=(I-1.)*(I-1.)-3.*(2*J-1.)*(I-J)
            SOM1=SOM1+X2*BETA22(I-2*J)
            SOM2=SOM2+X2*DELTA33(I-2*J)
  17     CONTINUE
  
         DO 18 J=0,MM
            X2=(I-1.)*(I-1.)-3.*J*(2*I-2*J-1.)
            SOM3=SOM3+X2*BETA22(I-2*J-1)
            SOM4=SOM4+X2*DELTA33(I-2*J-1)
  18     CONTINUE
  
         ZETA(I)=CO3-CO1*(SOM2-SOM3)
         ALP(I)=CO2-CO1*(SOM1-SOM4)
  16  CONTINUE


    
C* Fichier trace : Contrôle de la recombinaison de la matrice de phase
C----------------- sur éléments avec application de la troncature
      IF (TRACE) THEN
           WRITE(IDLOG,*,err=921)'  '
	   WRITE(IDLOG,*,err=921)
     &	      ' Recombining of the truncated phase matrix'
        
           DO 8 J=-MIE_NBMU,MIE_NBMU
	   
	      IF(J.EQ.0) GO TO 8
	      
              P11_RECOMP=0.D+00
	      P12_RECOMP=0.D+00
	      P22_RECOMP=0.D+00
	      P33_RECOMP=0.D+00
              
              XRMU=XMU(J)
              
	      PL(-1)=0.D+00
              PL(0)=1.D+00
	      DO 9 K=0,OS_NB
                 PL(K+1)=((2*K+1.)*XRMU*PL(K)-K*PL(K-1))/(K+1.)
                 P11_RECOMP=P11_RECOMP+BETA11(K)*PL(K)
		 P22_RECOMP=P22_RECOMP+BETA22(K)*PL(K)
		 P33_RECOMP=P33_RECOMP+DELTA33(K)*PL(K)
    9         CONTINUE
       	      
              POL(0)=0.D+00
              POL(1)=0.D+00
	      POL(2)=3.*(1.-XRMU**2)/2./DSQRT(6.0D+00)
	      DO 91 K=2,OS_NB
		 D=(2.*K+1.)/DSQRT(1.D+00*(K+3.)*(K-1.))
                 E=DSQRT(1.D+00*(K+2.)*(K-2.))/(2.*K+1.)
		 POL(K+1)=D*(XRMU*POL(K)-E*POL(K-1))
		 P12_RECOMP=P12_RECOMP+GAMMA12(K)*POL(K)
  91         CONTINUE
	      		
        
	      IF (XHR(J).NE.0.D+00) THEN  
		 ANG_GAUSS=1	
	      ELSE
		 ANG_GAUSS=0	      
	      ENDIF
	      
      
              P11_INIT_TR=P11(J)		!Tronquee
	      P12_INIT_TR=P12(J)*P11(J)/TTT(J)	!Ajustement à la troncature
	      P22_INIT_TR=P22(J)*P11(J)/TTT(J)
	      P33_INIT_TR=P33(J)*P11(J)/TTT(J)
	      
	      WRITE(IDLOG,100,err=921) 
     &               ' Dev ', DACOS(XRMU)*180./INCTE_PI,
     &	             ' | P11 ',P11_INIT_TR, ' P11_recomb ',P11_RECOMP,
     &               ' Rel_diffe (%) ', 
     &                 100.*(P11_INIT_TR-P11_RECOMP)/P11_INIT_TR,
     &	             ' | P12 ',P12_INIT_TR, ' P12_recomb ',P12_RECOMP, 
     &               ' Rel_diffe (%) ',  
     &                 100.*(P12_INIT_TR-P12_RECOMP)/P12_INIT_TR,
     &	             ' | P22 ',P22_INIT_TR, ' P22_recomb ',P22_RECOMP, 
     &               ' Rel_diffe (%) ',  
     &                 100.*(P22_INIT_TR-P22_RECOMP)/P22_INIT_TR,
     &	             ' | P33 ',P33_INIT_TR, ' P33_recomb ',P33_RECOMP, 
     &               ' Ecart_rel (%) ',  
     &                 100.*(P33_INIT_TR-P33_RECOMP)/P33_INIT_TR,
     &               ' | Ang_gauss ',  ANG_GAUSS
    8      CONTINUE
      
      END IF !Fichier Trace
      



C* Normalisation des paramètres ALP(K), BETA(K), GAMMA(K), DELTA(K) et ZETA(K)
C* de la matrice de phase.
C----------------------------------------------------------------------------
      Z1=BETA11(0)
      DO 20 K=0,OS_NB
         ALP(K)=ALP(K)/Z1
         BETA11(K)=BETA11(K)/Z1
	 BETA22(K)=BETA22(K)/Z1
         GAMMA12(K)=GAMMA12(K)/Z1
         DELTA33(K)=DELTA33(K)/Z1
         ZETA(K)=ZETA(K)/Z1
  20  CONTINUE


C* Fin nominale 
C-------------------
       GOTO 9999

C* Cas d'erreur et retour du status -1 au programme appelant
C----------------------------------------------------------
  921  WRITE(6,*) 'Error while writing GRANU logfile'
       IER=-1
       GOTO 9999

       
 9999  CONTINUE

C* Format
C---------   
  100 FORMAT(A5,F6.2,4(A7,E10.4,A12,E10.4,A15,F8.3),A13,I4)
  
   
       RETURN
       END	!FIN DE LA PROCEDURE SOS_DECOMPO_LEGENDRE
       
       
       
       

C*==============================================================================
C* PROCEDURE: OSOAA_GRANU
C* ==========
C      Cette procédure calcule la fonction de phase P(mu) et les fonctions
C      de phase polarisée Q(mu) et U(mu) à partir des calculs de MIE réalisés
C      par le programme "SOS_MIE", pour une longueur d'onde donnée et pour
C      une distribution en taille des particules donnée (renseignée par igranu).
C
C      Si igranu = 1 => Distribution Log-Normale : 
C                N(r) = exp(-0.5 *(ln(r/rm)/sig)**2) / r
C		 varGranu1 = rm : rayon modal en microns,
C		 varGranu2 = sig : variance.
C
C      Si igranu = 2  => Loi de Junge : N(r) = 0 pour r <= Rmin 
C				        N(r) = r**(-xnu) pour Rmin < r <= Rmax
C				        N(r) = 0 pour r > Rmax
C		 varGranu1 = Rmin : rayon minimal en microns,
C		 varGranu2 = xnu : puissance appliquée au rayon (pente de la loi de Junge).
C		 varGranu3 = Rmax : rayon maximal en microns,
C
C      Si igranu = 3  => Loi de Junge : N(r) = Rmin**(-xnu) pour r <= Rmin 
C				        N(r) = r**(-xnu) pour Rmin < r <= Rmax
C				        N(r) = 0 pour r > Rmax
C		 varGranu1 = Rmin : rayon minimal en microns,
C		 varGranu2 = xnu : puissance appliquée au rayon (pente de la loi de Junge).
C		 varGranu3 = Rmax : rayon maximal en microns,
C
C
C       Les fonctions de phase sont stockées dans des tableaux définis jusqu'à
C       CTE_MIE_NBMU_MAX mais sont utilisées jusqu'à MIE_NBMU (angles effectifs).
C
C
C Description des paramètres
C ----------------------------
C       FICMIE(CHARACTER*CTE_LENFIC2)  (E) : Nom du fichier MIE 
C                                 (répertoire + nom fichier + extension).
C       IGRANU  (I4)         (E) : Numéro du modèle de granulométrie à utiliser.
C       VARGRANU1 (double)   (E) : Paramètre 1 de la granulométrie.
C	VARGRANU2 (double)   (E) : Paramètre 2 de la granulométrie
C	VARGRANU3 (double)   (E) : Paramètre 3 de la granulométrie
C	WA  (double)         (E) : Longueur d'onde en microns.
C       MIE_NBMU (I4) 	     (E) : Nombre d'angles (positifs) effectivement utiles
C	XMU(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX) (double) : (E) Cosinus des angles de Gauss.
C	TRACE (logical)      (E) : = vrai,si écriture dans le fichier trace
C                                 (No d'unité logique du fichier trace =88).
C	KMAT1 (double)       (S) : Section efficace d'extinction (micron^2).
C	KMAT2 (double)       (S) : Section efficace de diffusion (micron^2).
C	SOMME_NR (double)    (S) : Nombre de particules intégré sur la granulométrie.
C	VMOY (double)        (S) : Volume moyen des particules intégré sur la 
C                                  granulométrie (micron^3).
C       P11 (double)          (S) : Fonction de phase calculée à partir des 
C                                  fonctions de la théorie de MIE. 
C       P12 (double)          (S) : Elément P12 de la matrice de phase.
C	P33 (double)          (S) : Elément P33 de la matrice de phase.
C       IER  (I4)            (S) : Code d'erreur =0 si pas d'erreur, =-1 sinon.   
C
C
C Autre résultat fourni par la procédure
C ----------------------------------------
C   Informations dans le fichier de trace (optionnel)
C   (fichier créé/ouvert par le programe appelant)
C
C
C
C Description des fichiers utilisés
C ---------------------------------
C   -->Contenu du fichier de MIE
C       Fichier binaire non formaté, créé par SOS_MIE
C       1 enregistrement d'entête avec
C                Indice de réfraction (partie réelle)
C                Indice de réfraction (partie imaginaire)
C                Paramètre de taille limite
C                Nombre d'angles de Gauss positifs
C
C      1 enregistrement pour chaque valeur du paramètre de taille avec:
C        - Paramètre de taille (réel),
C        - Coefficient d'efficacité en extinction (réel),
C        - Coefficient d'efficacité en diffusion (réel),
C        - Facteur d'anisotropie (double précision),
C        - Tableau de la fonction de phase Imie(mu) (réel),
C        - Tableau de la fonction de phase polarisée Qmie(mu) (réel),
C        - Tableau de la fonction de phase polarisée Umie(mu) (réel).
C
C    --> Les tableaux ont une taille utile limitée à MIE_NBMU : 
C        ils sont lus avec cette limitation.
C
C
C Constantes utilisées :  
C----------------------
C    CTE_LENFIC2 : Longueur des noms de fichiers avec arborescence.
C    CTE_MIE_NBMU_MAX : Nombre maximal d'angles positifs.
C    CTE_OS_NB_MAX :  Valeur maximale pour l'ordre limite du développement en 
C                     polynômes de Legendre.
C
C Common utilisé:
C --------------
C  Aucun
C
C
C Cas d'erreur :
C ------------
C     -Incohérence sur le nombre d'angles de Gauss
C     -Ouverture d'un fichier: Erreur si le fichier de MIE  n'existe pas
C     -Lecture/écriture dans un fichier
C     -Incohérence sur le nombre d'angles utilisés par rapport à celui défini 
C      dans le fichier de Mie
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant
C           
C==============================================================================
 
      SUBROUTINE OSOAA_GRANU(FICMIE,IGRANU,VARGRANU1,
     &                       VARGRANU2,VARGRANU3,WA,MIE_NBMU,XMU,TRACE,
     &                       KMAT1,KMAT2,SOMME_NR,VMOY,P11,P12,P33,IER)

      IMPLICIT NONE

C Index de fichier
C -----------------
      INTEGER*2 IDLOG		! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_PARTICULES)
      
C* Définition des variables
C----------------------------------------------------------------------------
      
      CHARACTER*CTE_LENFIC2 FICMIE	! Chemin et nom du fichier MIE
      
      DOUBLE PRECISION VARGRANU1   	! Paramètre 1 de la granulométrie.
      DOUBLE PRECISION VARGRANU2   	! Paramètre 2 de la granulométrie.
      DOUBLE PRECISION VARGRANU3   	! Paramètre 3 de la granulométrie.
      DOUBLE PRECISION RM		! Rayon modal d'une granulométrie LND.
      DOUBLE PRECISION SIG		! Variance d'une granulométrie LND
      DOUBLE PRECISION B		! Variable intermedaire.
      DOUBLE PRECISION R0		! Rayon seuil min d'une granulométrie en loi de Junge
      DOUBLE PRECISION RMAX		! Rayon seuil max d'une granulométrie en loi de Junge
      DOUBLE PRECISION XNU		! Puissance d'une granulométrie en loi de Junge.
      DOUBLE PRECISION NR0		! Nombre de particules pour un rayon < r0 
                                	! d'une loi de Junge.
					
      DOUBLE PRECISION REFF		! Rayon effectif de la granulométrie (microns).
      DOUBLE PRECISION VEFF		! Variance effective de la granulométrie.
      
      DOUBLE PRECISION WA		! Longueur d'onde en microns.

      DOUBLE PRECISION XMU(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)
                                        ! Cosinus des angles de Gauss.
      DOUBLE PRECISION TETA     	! Angle de diffusion.
      
      DOUBLE PRECISION KMAT1		! Section efficace d'extinction (micron^2).
      DOUBLE PRECISION KMAT2 		! Section efficace de diffusion (micron^2). 
      
      DOUBLE PRECISION P11(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)	
                       !Fonction de phase calculée à partir des fonctions de 
		       !la théorie de MIE.
      DOUBLE PRECISION P12(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)	
                       !Elément P12 de la matrice de phase.
      DOUBLE PRECISION P33(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)	
                       !Elément P33 de la matrice de phase.
		      
      DOUBLE PRECISION XG	!Facteur d'anisotropie.


      DOUBLE PRECISION ALPHAF	!Valeur maximale du paramètre de taille contenue
      				!dans le fichier des calculs de MIE.

      DOUBLE PRECISION R	!Rayon de particule pour le paramètre de taille 
      				!ALPHA et la longueur d'onde du traitement WA.
      DOUBLE PRECISION PR	!Pas entre deux valeurs successives des rayons de
      				!particules (en microns).
      DOUBLE PRECISION NR	!NR*PR = Nombre de particules de rayon compris
      				!entre R et R + PR (par micron**3).

      DOUBLE PRECISION SOMME_NR	!Intégration sur les rayons de NR * PR 
      				!==> nb de particule par unité de volume.
      DOUBLE PRECISION SOMME_R2	!Intégration sur les rayons de R^2 * NR * PR.
      DOUBLE PRECISION SOMME_R3	!Intégration sur les rayons de R^3 * NR * PR.
      DOUBLE PRECISION SOMME_R4	!Intégration sur les rayons de R^4 * NR * PR.
      DOUBLE PRECISION R2NRPR	!Valeur de R*R * NR * PR.
      DOUBLE PRECISION VMOY     !Volume moyen des particules intégré sur la 
                                !granulométrie (micron^3).

      DOUBLE PRECISION RN	!partie réelle de l'indice de réfraction.
      DOUBLE PRECISION RI	!partie imaginaire de l'indice de réfraction.
	
      DOUBLE PRECISION X1		

      
      REAL*4 ALPHA		!Valeur du paramètre de taille.
      REAL*4 PAS		!Pas entre deux valeurs de ALPHA.
      REAL*4 QEXT		!Coefficient d'efficacité en extinction.
      REAL*4 QSCA		!Coefficient d'efficacité en diffusion.
      REAL*4 IMIE(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)         
                                !Fonction de phase : Imie(mu) pour ALPHA.
      REAL*4 QMIE(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)         
                                !Fonction de phase polarisée : Qmie(mu).
      REAL*4 UMIE(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)         
                                !Fonction de phase polarisée : Umie(mu).

      INTEGER*4 MIE_NBMU  	! Nombre d'angles (positifs) effectivement utiles 
      INTEGER*4 IGRANU  	! Numéro du modèle de granulométrie.
      INTEGER*4 J	   	! Indice.
      INTEGER*4 NBMUMIE	   	! Nombre d'angle de GAUSS positifs utilisés pour 
      			   	! les calculs de MIE (enregistrement du fichier MIE).			   
      INTEGER*4 IER        	! Code d'erreur =0 si pas d'erreur, =-1 sinon

      LOGICAL TRACE        	! = vrai si écriture dans le fichier trace.
      

C* Initialisation du code d'erreur
C------------------------------------------------------
      IER=0

C* Initialisation des tableaux de fonction de phase
C------------------------------------------------------
      DO 102 J=-MIE_NBMU,MIE_NBMU
         P11(J)=0.
         P12(J)=0.
         P33(J)=0.
  102 CONTINUE

C* Initialisation des coefficient d'extinction et de diffusion
C-------------------------------------------------------------
      KMAT1=0.
      KMAT2=0.

C* Initialisation des cumuls d'intégration sur la granulométrie
C-------------------------------------------------------------
      SOMME_NR=0.
      SOMME_R2=0.
      SOMME_R3=0.
      SOMME_R4=0.
                  
C* Ouverture des fichiers 
C------------------------------------------------------
C     Fichier MIE
      OPEN(1,FILE=FICMIE,FORM='UNFORMATTED',
     &      STATUS='OLD',ERR=991)
 
C* Lecture de l'entête du fichier de MIE
C-------------------------------------------      
      READ(1,ERR=992) RN,RI,ALPHAF,NBMUMIE
        
C* Contrôle paramètre
C--------------------
      IF (NBMUMIE.NE.MIE_NBMU) GOTO 996

C* Lecture des fonctions calculées par la théorie de MIE
C* pour chaque valeur du paramètre de taille ALPHA
C*
C*      Lecture des tableaux de fonctions de phase
C*      qui ont été enregistrés avec des bornes 
C*      limitées au dimensionnement utile -MIE_NBMU:MIE_NBMU
C*       
C  (boucle sur l'étiquette 123,
C   condition d'arrêt : fin du fichier MIE)
C------------------------------------------------------
      PAS = 0.0001

 123     READ(1,err=9992) ALPHA,QEXT,QSCA,XG,
     &                    (IMIE(J),J=-MIE_NBMU,MIE_NBMU),
     &                    (QMIE(J),J=-MIE_NBMU,MIE_NBMU),
     &                    (UMIE(J),J=-MIE_NBMU,MIE_NBMU)
		
C* Calcul du rayon de particule associé a ALPHA pour la longueur d'onde WA
C--------------------------------------------------------------------------
         R=ALPHA*WA/2.D+00/INCTE_PI
	 
C* Test sur la limite du fichier de MIE
C--------------------------------------
         IF (ALPHA.GE.(ALPHAF-PAS)) GOTO 40
		
C* Estimation du pas pour la valeur de ALPHA suivante
C------------------------------------------------------
         IF (ALPHA.GT.0.10) PAS=0.001
         IF (ALPHA.GT.1.00 ) PAS=0.01
         IF (ALPHA.GT.10.) PAS=0.05
         IF (ALPHA.GT.30.) PAS= 0.10
         IF (ALPHA.GT.100.) PAS=1.00


C* Expression des différentes granulométries possibles:
C------------------------------------------------------

C*  Distribution log-normale :
C***************************
C*  RM : Rayon modal (microns).
C*  SIG : Variance
C------------------------------
		
         IF (IGRANU.eq.1) THEN
            RM = VARGRANU1
            SIG= VARGRANU2

            B = dlog(R/RM)/SIG
            NR = dexp(-B*B/2.)/(R*SIG*sqrt(2*INCTE_PI))
         ENDIF

C*  Loi de Junge : NR = 0 pour r <= Rmin
C****************
C*  R0 : Rayon minimal (microns).
C---------------------------
         IF (IGRANU.eq.2) THEN
            R0  = VARGRANU1
            XNU = VARGRANU2
	    RMAX = VARGRANU3

	    IF (R.GT.RMAX) GOTO 40 !Sortie de boucle sur taille des particules
            IF (R.LE.R0) THEN
	       NR = 0.D+00
	    ELSE
	       NR = R**(-XNU)   
	    ENDIF
	    
         ENDIF


C*  Loi de Junge : NR = plateau Rmin**(-XNU) pour r <= Rmin
C****************
C*  R0 : Rayon minimal (microns).
C---------------------------
         IF (IGRANU.eq.3) THEN
            R0  = VARGRANU1
            XNU = VARGRANU2
	    RMAX = VARGRANU3

	    IF (R.GT.RMAX) GOTO 40 !Sortie de boucle sur taille des particules
            IF (R.LE.R0) THEN
	       NR = R0**(-XNU)
	    ELSE
	       NR = R**(-XNU)   
	    ENDIF
	    
         ENDIF	 
	 
C* Calcul des coefficients d'extinction (kMAT1) et de diffusion (KMAT2).
C* Calcul des fonctions de phase en intensité (P11) et en polarisation (P12 et P33).
C----------------------------------------------------------------------------------
         PR=WA*PAS/2./INCTE_PI
         X1=NR*PR*INCTE_PI*R**2
         KMAT1=KMAT1+X1*QEXT
	 KMAT2=KMAT2+QSCA*X1
	
         X1=QSCA*X1
         DO 1 J=-MIE_NBMU,MIE_NBMU
            P11(J)=P11(J)+IMIE(J)*X1
            P12(J)=P12(J)+QMIE(J)*X1
            P33(J)=P33(J)+UMIE(J)*X1
   1     CONTINUE


C* Calcul du nombre de particules total
C-------------------------------------------------------------------------------
      SOMME_NR = SOMME_NR + NR*PR
 
C* Calcul des termes pour le rayon effectif et volume moyen
C-------------------------------------------------------------------------------
      R2NRPR = R*R*NR*PR
      SOMME_R2 = SOMME_R2 + R2NRPR
      SOMME_R3 = SOMME_R3 + R2NRPR*R
      SOMME_R4 = SOMME_R4 + R2NRPR*R*R
        
C* Passage à la valeur suivante de ALPHA
C-----------------------------------------
      GOTO 123 	
  999 CONTINUE
   40 CONTINUE

C* Calcul du rayon effectif et de la variance effective
C-------------------------------------------------------------------------------      
      IF (TRACE) THEN
         REFF = SOMME_R3 / SOMME_R2
	 VEFF =  1.D+00 +  
     &	         (SOMME_R4 - 2.*REFF*SOMME_R3) / (REFF*REFF * SOMME_R2)
      END IF  

C* Calcul du volume d'une particule moyenne de la granulométrie
C-------------------------------------------------------------------------------    
      VMOY = (4.D+00*INCTE_PI/3.D+00)*SOMME_R3 / SOMME_NR


C* Normalisation des fonctions de phase.
C* Copie de la fonction de phase en intensité.
C------------------------------------------------------
      DO 55 J=-MIE_NBMU,MIE_NBMU
         P11(J)=P11(J)/KMAT2
         P12(J)=P12(J)/KMAT2
         P33(J)=P33(J)/KMAT2
  55  CONTINUE
  

C* Normalisation à une particule des coefficients 
C* de diffusion et d'extinction.
C* ==> Conversion en sections efficaces de diffusion
C*     et d'extinction
C------------------------------------------------------  
       KMAT1= KMAT1 / SOMME_NR
       KMAT2= KMAT2 / SOMME_NR
       
       
C* Ecriture du fichier trace
C-------------------------------------------
      IF (TRACE) THEN
         WRITE(IDLOG,*,err=921) '* SUBROUTINE GRANU'
	 WRITE(IDLOG,*,err=921) '=================='
         WRITE(IDLOG,*,err=921) ' '
         WRITE(IDLOG,*,err=921) 'SIMULATION PARAMETERS'
	 WRITE(IDLOG,*,err=921) '---------------------'
	 WRITE(IDLOG,*,err=921) '  MIE file : ',FICMIE
	 WRITE(IDLOG,*,err=921) '  Wavelength in microns : ',WA
	 WRITE(IDLOG,*,err=921) '  Max size parameter : ',ALPHAF
	 IF (IGRANU.EQ.1) THEN
	      WRITE(IDLOG,*,err=921)
     &	           '  Model 1 : Log-Normale Distribution '
	      WRITE(IDLOG,*,err=921) '     rm in microns : ',VARGRANU1
	      WRITE(IDLOG,*,err=921) '     sig : ',VARGRANU2
	      WRITE(IDLOG,*,err=921) '     sig/ln10 : ',
     &	                                VARGRANU2/DLOG(10.D+00)
	 ENDIF
	 IF (IGRANU.EQ.2) THEN
	      WRITE(IDLOG,*,err=921) "  Model 2 : Junge's Law "
	      WRITE(IDLOG,*,err=921) '	   R0 in microns : ',VARGRANU1
	      WRITE(IDLOG,*,err=921) '     Slope  : ',VARGRANU2	
	      WRITE(IDLOG,*,err=921) '	   RMAX in microns : ',VARGRANU3    	   
	 ENDIF
	 WRITE(IDLOG,*,err=921) ' '
         WRITE(IDLOG,*,err=921) 'Last effective used value of R : ', R
         WRITE(IDLOG,*,err=921) ' '
         WRITE(IDLOG,*,err=921) 'RESULTS'
	 WRITE(IDLOG,*,err=921) '------- '
	 WRITE(IDLOG,*,err=921)
     &	     '  Extinction coefficient (in micr.) - no trunca. applied:'
     &	      ,KMAT1*SOMME_NR
	 WRITE(IDLOG,*,err=921) 
     &	     '  Scattering coefficient (in micr.) - no trunca. applied:'
     &       ,KMAT2*SOMME_NR
         WRITE(IDLOG,*,err=921) 
     &	     '  Single scattering albedo - no trunca. applied:  ' 
     &       ,KMAT2/KMAT1
         WRITE(IDLOG,*,err=921) 
     &	     '  Number of particles (by unit volume micron^3) : '
     &       ,SOMME_NR
         WRITE(IDLOG,*,err=921) 
     &	     '  Mean volume of a particle (micron^3) : ',VMOY
         WRITE(IDLOG,*,err=921)
     &	     "  Extinction cross-section (mic^2) - no trunca. applied:"
     &       ,KMAT1
	 WRITE(IDLOG,*,err=921) 
     &	     "  Scattering cross-section (mic^2) - no trunca. applied:" 
     &       ,KMAT2
	 WRITE(IDLOG,*,err=921) 
     &	     '  Effective radius of the size distribution (microns) : '
     &       ,REFF
         WRITE(IDLOG,*,err=921) 
     &	     '  Effective variance of the size distribution : ',VEFF
         WRITE(IDLOG,*,err=921) ' '
	 WRITE(IDLOG,*,err=921) '  1st column : Scattering angle '
	 WRITE(IDLOG,*,err=921) 
     &       '  2nd column : Original phase function '
         WRITE(IDLOG,*,err=921) '                 P11(mu) '
	 WRITE(IDLOG,*,err=921) 
     &       '  3rd column : Original polarized phase function '
         WRITE(IDLOG,*,err=921) '                 P12(mu) '
	 WRITE(IDLOG,*,err=921) 
     &       '  4th column : Original polarized phase function '
         WRITE(IDLOG,*,err=921) '                 P33(mu) '
	 WRITE(IDLOG,*,err=921) ' '
         DO 44 J=-MIE_NBMU,MIE_NBMU
	    TETA=DACOS(XMU(J))*180./INCTE_PI
	    WRITE(IDLOG,10,err=921) TETA,P11(J),P12(J),P33(J)

   44    CONTINUE
   	 WRITE(IDLOG,*,err=921) ' '

      END IF  


C* Fermeture fichier
C-------------------
      CLOSE(1)
      
C* Fin nominale 
C-------------------
      GOTO 9999

      
C* Cas d'erreur et retour du status -1 au programme appelant
C----------------------------------------------------------
  991 WRITE(6,*) 'Error while opening the MIE file'
      IER=-1
      GOTO 9999
  992 WRITE(6,*) 'Error while reading the first line of the MIE file'
      WRITE(6,*) '  --> MIE file probably incomplete'
      WRITE(6,*) '      ==> Check the left space on disk'
      IER=-1
      GOTO 9999
 9992 WRITE(6,*) 'Error while reading the MIE file'
      WRITE(6,*) '  --> Maybe the end of file is reached'
      WRITE(6,*) '  --> MIE file probably incomplete'
      WRITE(6,*) '      ==> Check the left space on disk'
      IER=-1
      GOTO 9999    
  921 WRITE(6,*) 'Error while writing the logfile'
      IER=-1
      GOTO 9999      
  996 WRITE(6,*) 'Inconsistent number of gauss angles'
      WRITE(6,*) 'NBMUMIE=',NBMUMIE,' MIE_NBMU=',MIE_NBMU
      IER=-1  
      GOTO 9999

C* Format
C---------   
   10 FORMAT(1X,F6.2,6(2X,E12.4))
   
         
 9999 CONTINUE
 
  
      RETURN
      END    !FIN DE LA PROCEDURE OSOAA_GRANU
      
      
      
      
  

C*==============================================================================
C* PROCEDURE: SOS_INTERPO_SPLINT
C* ==========
C      Cette procédure interpole une fonction définie par (XIN, YIN) sur NBIN noeuds
C      aux abscisses XOUT pour NBOUT noeuds (XOUT, YOUT).
C
C      La démarche repose sur l'utilisation des fonctions SPLINE et SPLINT
C         - Les noeuds (XIN, YIN) sont en premier triés par ordre croissant d'abscisse XIN.
C         - La fonction SPLINE calcule les dérivées secondes de YIN_TRIE en XIN_TRIE
C           à partir des valeurs YIN_TRIE de la fonction aux abscisses XIN_TRIE et des
C           valeurs de dérivée première de la fonction au premier et dernier noeud 
C           (min et max de XIN_TRIE).
C	  - La fonction SPLINT calcule les valeurs YOUT aux abscisses XOUT en fonction
C           des valeurs de la fonction et de sa dérivée seconde précédemment calculées
C           aux abscisses XIN_TRIE.
C
C Description des paramètres
C ----------------------------
C       NBIN (I4)		(E) : Nombre d'éléments des tableaux XIN et YIN.
C       XIN(1:NBIN)   (double)	(E) : Tableau des abscisses de la fonction pour lesquels 
C				      l'ordonnée est connue.
C       YIN(1:NBIN)   (double)	(E) : Tableau des valeurs connues de la fonction 
C				      (correspond aux abscisses XIN).
C       NBOUT (I4)		(E) : Nombre d'éléments des tableaux XOUT et YOUT.
C       XOUT(1:NBOUT) (double)	(E) : Tableau des abscisses souhaitées de la fonction.
C       YOUT(1:NBOUT) (double)	(S) : Tableau des valeurs estimées de la fonction aux 
C                                     abscisses XOUT.
C       IER  (I4)               (S) : Code d'erreur =0 si pas d'erreur, =-1 sinon.   
C
C Constantes utilisées :  
C----------------------
C  Aucune
C
C Common utilisé:
C --------------
C  Aucun
C
C Cas d'erreur :
C ------------
C     - Retour d'erreur de la routine SOS_SPLINT.
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant
C        
C  Sous programmes utilisés:
C --------------------------
C  Ce programme fait appel aux routines: SOS_SPLINE et SOS_SPLINT
C
C==============================================================================

      SUBROUTINE SOS_INTERPO_SPLINT(NBIN,XIN,YIN,NBOUT,XOUT,YOUT,IER)

      IMPLICIT NONE


C* Définition des variables
C----------------------------------------------------------------------------
      INTEGER*4 NBIN  	! Nombre de noeuds (XIN,YIN) de la fonction Y 
      INTEGER*4 NBOUT  	! Nombre de noeuds (XOUT,YOUT) de la fonction Y 
      INTEGER*4 J	! Indice.
      INTEGER*4 K	! Indice.	   
      INTEGER*4 IER     ! Code d'erreur =0 si pas d'erreur, =-1 sinon
      
      DOUBLE PRECISION XIN(NBIN)   !Valeurs des abscisses des noeuds (XIN,YIN) 
      DOUBLE PRECISION YIN(NBIN)   !Valeurs des ordonnées des noeuds (XIN,YIN) 
      DOUBLE PRECISION XOUT(NBOUT) !Valeurs des abscisses des noeuds (XOUT,YOUT) 
      DOUBLE PRECISION YOUT(NBOUT) !Valeurs recherchées des ordonnées des noeuds (XOUT,YOUT)

      DOUBLE PRECISION XIN_TRIE(NBIN)   !Valeurs croissantes des abscisses 
      					!des noeuds (XIN,YIN)
      DOUBLE PRECISION YIN_TRIE(NBIN)   !Valeurs des ordonnées des noeuds (XIN,YIN) 
      				        !pour XIN trié par ordre croissant.
      DOUBLE PRECISION VAL		!Valeur temporaire
      
      DOUBLE PRECISION DY1	!Dérivée d'ordre 1 de YIN_TRIE en abscisse XIN_TRIE(1)
      DOUBLE PRECISION DYN	!Dérivée d'ordre 1 de YIN_TRIE en abscisse XIN_TRIE(NBIN)
      DOUBLE PRECISION DSEC_YIN_TRIE(NBIN)  !Valeurs des dérivées secondes de la fonction 
      					    !YIN_TRIE aux noeuds XIN_TRIE  
    

C* Initialisation du code d'erreur
C------------------------------------------------------
      IER=0

      
C* Trie des tables (XIN, YIN) par ordre croissant de XIN
C------------------------------------------------------
      DO J=1,NBIN
         XIN_TRIE(J)=XIN(J)
	 YIN_TRIE(J)=YIN(J)
      ENDDO
      
      DO J=1,NBIN	     
	 DO K=J+1,NBIN
	    IF (XIN_TRIE(J).GT.XIN_TRIE(K)) THEN		    		    
	       !Permutation des valeurs XIN(J) et XIN(K)
	        VAL=XIN_TRIE(J)
	        XIN_TRIE(J)=XIN_TRIE(K)
	        XIN_TRIE(K)=VAL
	      !Permutation des valeurs YIN(J) et YIN(K)
		VAL=YIN_TRIE(J)
		YIN_TRIE(J)=YIN_TRIE(K)
		YIN_TRIE(K)=VAL   	    	    
	    ENDIF
	 ENDDO 
      ENDDO 
   
      
C* Calcul des dérivées secondes de YIN_TRIE en XIN_TRIE
C------------------------------------------------------
      DO J=1,NBIN
         DSEC_YIN_TRIE(J)=0.D+00
      ENDDO

C     Dérivées d'ordre 1 de YIN_TRIE en XIN_TRIE(1) et XIN_TRIE(NBIN)
      DY1 = (YIN_TRIE(2) - YIN_TRIE(1)) / (XIN_TRIE(2) - XIN_TRIE(1))
      DYN = (YIN_TRIE(NBIN) - YIN_TRIE(NBIN-1)) / 
     &      (XIN_TRIE(NBIN) - XIN_TRIE(NBIN-1))
      
      CALL SOS_SPLINE(NBIN,XIN_TRIE,YIN_TRIE,DY1,DYN,DSEC_YIN_TRIE)

C* Application pour l'interpolation SPLINT aux abscisses XOUT
C------------------------------------------------------------       
       DO J=1,NBOUT
	 
	 CALL SOS_SPLINT(NBIN,XIN_TRIE,YIN_TRIE,DSEC_YIN_TRIE,
     &	                 XOUT(J),YOUT(J),IER)
     	 
         IF (IER.NE.0) GOTO 901
      ENDDO
            
      
C* Fin nominale 
C-------------------
      GOTO 9999

      
C* Cas d'erreur et retour du status -1 au programme appelant
C----------------------------------------------------------
  901 WRITE(6,*) '  ERROR for SOS_INTERPO_SPLINT'
      GOTO 9999  
     
 9999 CONTINUE
  
      RETURN
      END    !FIN DE LA PROCEDURE SOS_INTERPO_SPLINT     
      



C*==============================================================================
C* PROCEDURE: SOS_SPLINE
C* ==========
C      Cette procédure calcule les valeurs de dérivées secondes d'une fonction définie
C      par (X, Y) sur N noeuds (supposés triés par ordre croissant de X) et 
C      par ses valeurs de dérivées premières aux noeuds extrêmes (X(1),DY1) et (X(N),DYN).
C
C      La routine renvoie les valeurs de dérivées secondes en chaque point X.
C
C Description des paramètres
C ----------------------------
C       N  (I4)		(E) : Nombre d'éléments des tableaux X, Y et DSEC_Y
C       X(1:N) (double)	(E) : Tableau des abscisses de la fonction pour lesquels 
C                             l'ordonnée est connue.
C       Y(1:N) (double)	(E) : Tableau des valeurs connues de la fonction 
C                             (correspond aux abscisses X).
C       DY1    (double)	(E) : Dérivée première de la fonction en X(1).
C       DYN    (double)	(E) : Dérivée première de la fonction en X(N).
C       DSEC_Y(1:N) (double) (S) : Tableau des valeurs de dérivées secondes 
C                                  de la fonction aux abscisses X.
C
C Constantes utilisées :  
C----------------------
C  Aucune    
C
C Common utilisé:
C --------------
C  Aucun
C
C Cas d'erreur :
C ------------
C  Aucun         
C
C==============================================================================

      SUBROUTINE SOS_SPLINE(N,X,Y,DY1,DYN,DSEC_Y)

      IMPLICIT NONE

      INTEGER*4 N		 !Nb d'éléments des tableaux d'entrée (X,Y) 
      				 !et de sortie (DSEC_Y)
      DOUBLE PRECISION X(N)	 !Tableau des abscisses de la fonction 
      DOUBLE PRECISION Y(N)	 !Tableau des valeurs de la fonction aux abscisses X
      DOUBLE PRECISION DY1       !Dérivée première de la fonction en X(1)
      DOUBLE PRECISION DYN	 !Dérivée première de la fonction en X(N)
      DOUBLE PRECISION DSEC_Y(N) !Tableau des valeurs de dérivées secondes de la fonction 
      				 !aux abscisses X
      
      DOUBLE PRECISION U(N)

      DOUBLE PRECISION SIG
      DOUBLE PRECISION P
      DOUBLE PRECISION QN
      DOUBLE PRECISION UN
      
      INTEGER*4 K ! Indice.
      
C* Définition des variables
C----------------------------------------------------------------------------


      IF (DY1.GT..99E30) THEN
        DSEC_Y(1)=0.
        U(1)=0.
      ELSE
        DSEC_Y(1)=-0.5
        U(1)=(3./(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-DY1)
      ENDIF
      DO 11 K=2,N-1
        SIG=(X(K)-X(K-1))/(X(K+1)-X(K-1))
        P=SIG*DSEC_Y(K-1)+2.
        DSEC_Y(K)=(SIG-1.)/P
        U(K)=(6.*((Y(K+1)-Y(K))/(X(K+1)-X(K))-(Y(K)-Y(K-1))
     *      /(X(K)-X(K-1)))/(X(K+1)-X(K-1))-SIG*U(K-1))/P
11    CONTINUE
      IF (DYN.GT..99E30) THEN
        QN=0.
        UN=0.
      ELSE
        QN=0.5
        UN=(3./(X(N)-X(N-1)))*(DYN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
      ENDIF
      DSEC_Y(N)=(UN-QN*U(N-1))/(QN*DSEC_Y(N-1)+1.)
      DO 12 K=N-1,1,-1
        DSEC_Y(K)=DSEC_Y(K)*DSEC_Y(K+1)+U(K)
12    CONTINUE
      RETURN
      END   !FIN DE LA PROCEDURE SOS_SPLINE   





C*==============================================================================
C* PROCEDURE: SOS_SPLINT
C* ==========
C
C      Cette procédure interpole une fonction en un point XVAL à partir
C      de trois vecteurs de même dimension X(1:N), Y(1:N) et DSEC_Y(1:N) 
C      qui donnent respectivement les abscisses, les ordonnées et les 
C      dérivées secondes de la fonction.
C
C Description des paramètres
C ----------------------------
C       N  (I4)		(E) : Nombre d'éléments des tableaux X, Y et DSEC_Y
C       X(1:N) (double)	(E) : Tableau des abscisses de la fonction pour lesquels 
C                             l'ordonnée est connue.
C       Y(1:N) (double)	(E) : Tableau des valeurs connues de la fonction 
C                             (correspond aux abscisses X).
C       DSEC_Y(1:N) (double) (E) : Tableau des valeurs de dérivées secondes 
C                                  de la fonction aux abscisses X.
C       XVAL (double)	(E) : Valeur de l'abscisse pour laquelle la fonction 
C                             doit être interpolée.
C       YVAL (double)	(S) : Valeur de la fonction interpolée en XVAL.
C       IER  (I4)       (S) : Code d'erreur =0 si pas d'erreur, =-1 sinon.   
C
C Constantes utilisées :  
C----------------------
C  Aucune    
C
C Common utilisé:
C --------------
C  Aucun
C
C Cas d'erreur :
C ------------
C  Identité des abscisses de la fonction pour les indices associés aux valeurs 
C  qui encadrent l'abscisse souhaitée.       
C
C==============================================================================

      SUBROUTINE SOS_SPLINT(N,X,Y,DSEC_Y,XVAL,YVAL,IER)

      IMPLICIT NONE

      INTEGER*4 N	! Nb d'éléments des tableaux d'entrée (X,Y et DSEC_Y)
      INTEGER*4 KLO	! Lower value of index
      INTEGER*4 KHI	! Higher value of index   
      INTEGER*4 K	! Indice
      INTEGER*4 IER     ! Code d'erreur =0 si pas d'erreur, =-1 sinon
      
      DOUBLE PRECISION X(N)	 !Tableau des abscisses de la fonction 
      DOUBLE PRECISION Y(N)	 !Tableau des valeurs de la fonction aux abscisses X
      DOUBLE PRECISION DSEC_Y(N) !Tableau des valeurs de dérivées secondes de la fonction 
      				 !aux abscisses X

      DOUBLE PRECISION XVAL	!Valeur de l'abscisse pour laquelle la fonction 
      				!doit être interpolée.
      DOUBLE PRECISION YVAL	!Valeur de la fonction interpolée en XVAL.
      
      DOUBLE PRECISION A,B
      DOUBLE PRECISION H


C* Initialisation du code d'erreur
C------------------------------------------------------
      IER=0
      
C* Définition des variables
C----------------------------------------------------------------------------
      
      KLO=1
      KHI=N
1     IF (KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2
        IF(X(K).GT.XVAL)THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
      GOTO 1
      ENDIF
      H=X(KHI)-X(KLO)
      IF (H.NE.0.) THEN
         A=(X(KHI)-XVAL)/H
         B=(XVAL-X(KLO))/H
         YVAL=A*Y(KLO)+B*Y(KHI)+
     *      ((A**3-A)*DSEC_Y(KLO)+(B**3-B)*DSEC_Y(KHI))*(H**2)/6.
      ELSE
         IER=-1
      ENDIF
      
      IF (IER.EQ.-1) GOTO 901
      
C* Fin nominale 
C-------------------
      GOTO 9999
      
C* Cas d'erreur et retour du status -1 au programme appelant
C----------------------------------------------------------
  901 WRITE(6,*) '  ERROR for SPLINT interpolation'
      WRITE(6,*) '  --> Bad X table input'
      GOTO 9999  
     
 9999 CONTINUE      
      RETURN
      END    !FIN DE LA PROCEDURE SOS_SPLINT     






C*==============================================================================
C* PROCEDURE: OSOAA_ENREG_GRANU
C* ==========
C
C      Cette procédure réalise l'enregistrement d'un fichier GRANU 
C      des paramètres radiatifs et de décomposition de la matrice de phase.
C
C MOD:VERSION:1.1: 23/05/2018:
C      Changement du format d'enregistrement de la valeur de l'indice de réfraction moyen
C      (etiquette : 45 FORMAT) : F8.4 --> F10.4
C
C
C Description des paramètres
C ----------------------------

C     KMAT1  (DOUBLE)   (E) : Section efficace d'extinction (micron^2) pour WA de simulation
C			      Non tronquée.
C
C     KMAT2  (DOUBLE)   (E) : Section efficace de diffusion (micron^2) pour WA de simulation
C			      Non tronquée.
C
C     A_TRONCA (DOUBLE) (E) : Coefficient de troncature de la fonction de phase
C
C     PIZTR    (DOUBLE) (E) : Albédo de simple diffusion après troncature.
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
C     OS_NB   (I4) (E)	    : Ordre effectif des développements des fonctions de phase 
C			      en Polynômes de Legendre.  
C
C     ALP(0:CTE_OS_NB_MAX)     (double) (E) : Paramètres alpha(k).
C     BETA11(0:CTE_OS_NB_MAX)  (double) (E) : Paramètres beta11(k).
C     GAMMA12(0:CTE_OS_NB_MAX) (double) (E) : Paramètres gamma12(k).
C     ZETA(0:CTE_OS_NB_MAX)    (double) (E) : Paramètres zeta(k).
C
C     FICGRANU (CHAR*CTE_LENFIC2) (S)    Nom complet du fichier résultat GRANU
C                 			(répertoire + nom fichier + extension)
C
C     IER (I4) (S) :  Code d'erreur =0 si pas d'erreur, sinon =1.	
C
C
C Constantes utilisées :  
C----------------------
C    CTE_OS_NB_MAX :  Valeur maximale pour l'ordre limite du développement en 
C                     polynômes de Legendre.   
C
C Common utilisé:
C --------------
C  Aucun
C
C Cas d'erreur :
C ------------
C  Erreur ouverte ou écriture du fichier résultat.       
C
C==============================================================================

      SUBROUTINE OSOAA_ENREG_GRANU(KMAT1, KMAT2, A_TRONCA, PIZTR,
     &                             Z_MOY, V_MOY, RN_MOY,
     &                             OS_NB, ALP, BETA11, GAMMA12, ZETA,
     &                             FICGRANU,IER)

      IMPLICIT NONE

C Index de fichier
C -----------------
      INTEGER*2 IDFICRES	! Numéro identifiant du fichier Résultat Granu	    
      PARAMETER(IDFICRES=20)    
      
C* Définition des variables                         
C*-----------------------------------------------------------------      
      DOUBLE PRECISION KMAT1	! Section efficace d'extinction (micron^2). Non tronquée.
      DOUBLE PRECISION KMAT2 	! Section efficace de diffusion (micron^2). Non tronquée.      
      DOUBLE PRECISION A_TRONCA	! Coefficient de troncature.
      DOUBLE PRECISION PIZTR	! Albédo de simple diffusion après troncature.

      DOUBLE PRECISION Z_MOY	! Elévation ou profondeur correspondant aux particules (m).
       
      DOUBLE PRECISION V_MOY	! Volume d'une particule moyenne 
      				! d'un mélange de particules (micron^3).  
				      
      DOUBLE PRECISION RN_MOY	! Indice de réfraction moyen (partie réelle) 
      				! représentatif d'un mélange de particules  

      DOUBLE PRECISION ALP(0:CTE_OS_NB_MAX)	! Paramètres alpha(k).
      DOUBLE PRECISION BETA11(0:CTE_OS_NB_MAX)	! Paramètres beta11(k).
      DOUBLE PRECISION GAMMA12(0:CTE_OS_NB_MAX)	! Paramètres gamma12(k).
      DOUBLE PRECISION ZETA(0:CTE_OS_NB_MAX)	! Paramètres zeta(k).

C---- Paramètres de type entier
      INTEGER*4 OS_NB 	   ! Ordre des développements des fonctions de phase 
			   ! en Polynômes de Legendre.   

      INTEGER*4 K	! Indice de tableau			   
C---- Code d'erreur
      INTEGER*4 IER     ! Code d'erreur =0 si pas d'erreur, =-1 sinon
  			    
C---- Les fichiers utilisés
      CHARACTER*CTE_LENFIC2 FICGRANU	! nom du fichier résultat GRANU  
     
C---- Expressions logiques
      LOGICAL TRACE     ! = vrai,si écriture dans le fichier trace  
      
      
C* Initialisation du code d'erreur
C------------------------------------------------------
      IER=0
      

C* Enregistrement des paramètres de granulométrie dans le fichier Résultat
C  -----------------------------------------------------------------------
      OPEN(IDFICRES,FILE=FICGRANU,ERR=900)

      !Ecriture des informations de contenu du fichier   
      WRITE(IDFICRES,38,ERR=910)
      WRITE(IDFICRES,39,ERR=910)
      
      ! Section efficace d'extinction
      WRITE(IDFICRES,40,ERR=910) KMAT1

      ! Section efficace de diffusion
      WRITE(IDFICRES,41,ERR=910) KMAT2
     
      ! Facteur d'asymétrie (non tronqué)
      WRITE(IDFICRES,42,ERR=910)
     &                 ( A_TRONCA/2.+(1.-A_TRONCA/2.)*BETA11(1)/3. )

      ! Altitude / profondeur des particules
      WRITE(IDFICRES,43,ERR=910) Z_MOY
      
      ! Volume d'une particule moyenne
      WRITE(IDFICRES,44,ERR=910) V_MOY

      ! Indice moyen des particules (partie réelle)      
      WRITE(IDFICRES,45,ERR=910) RN_MOY

      ! Coefficient de troncature      
      WRITE(IDFICRES,46,ERR=910) A_TRONCA
      
      ! Albédo de simple diffusion (éventuellement tronqué)      
      WRITE(IDFICRES,47,ERR=910) PIZTR

      ! Info sur le contenu des colonnes suivantes 
      WRITE(IDFICRES,39,ERR=910) 
      WRITE(IDFICRES,48,ERR=910) OS_NB
      WRITE(IDFICRES,49,ERR=910)
      
      ! Paramètres de la matrice de phase (éventuellement tronqués)       
      DO K=0,OS_NB
         WRITE(IDFICRES,50,ERR=910) ALP(K),BETA11(K),GAMMA12(K),ZETA(K)
      ENDDO

      
            
C* Fermeture fichiers
C-------------------
      CLOSE(IDFICRES)		! Fermeture du fichier résultat GRANU
C
C    NB : On ne ferme pas le fichier TRACE car il est enrichi pour les 
C         différents appels de la routine OSOAA_AEROSOLS (appel pour les 
C         différentes longueurs d'onde).
                  
C* Fin nominale 
C-------------------
      GOTO 9999
      
C* Cas d'erreur et retour du status -1 au programme appelant
C----------------------------------------------------------
 900  WRITE(6,*) '  OSOAA_ENREG_GRANU : ERROR_900 result file opening'
      GOTO 9998 
      
 910  WRITE(6,*) '  OSOAA_ENREG_GRANU : ERROR_910 result file writing'
      GOTO 9998         
     
9998  IER=-1             
9999  RETURN   
      
      
C* Formats
C--------- 
   38 FORMAT(33hRADIATIVE PROPERTIES OF PARTICLES)
   39 FORMAT(33h---------------------------------)
   40 FORMAT(38hEXTINCTION CROSS SECTION (mic^2)     :,E13.5)
   41 FORMAT(38hSCATTERING CROSS SECTION (mic^2)     :,E13.5) 
   42 FORMAT(38hASYMMETRY FACTOR (no truncation)     :,E13.5)  
   43 FORMAT(38hMEAN PARTICULES ALTITUDE/DEPTH (m)   :,F10.3) 
   44 FORMAT(38hVOLUME OF A MEAN PARTICULE (mic^3)   :,E13.5) 
   45 FORMAT(38hMEAN REFRACTIVE INDEX (real part)    :,F10.4) 
   46 FORMAT(38hTRUNCATION COEFFICIENT               :,F9.5)  
   47 FORMAT(38hSINGLE SCATTERING ALBEDO (truncation):,F9.5) 
   48 FORMAT(36hPHASE MATRIX COEFFICIENTS FOR K=0 TO,I4)
   49 FORMAT(55hALPHA(K)        BETA11(K)       GAMMA12(K)      ZETA(K))  
   50 FORMAT(E15.8,3(1X,E15.8))  
  
      END    !FIN DE LA PROCEDURE OSOAA_ENREG_GRANU   
