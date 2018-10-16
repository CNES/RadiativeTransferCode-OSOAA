C******************************************************************************
C* FICHIER: OSOAA_TRPHI.F
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
C* ROLE: Recomposition de la luminance à partir du champ développé en 
C*       séries de Fourier.
C*       Ajout des termes de réflexion et transmission directes
C*
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
C*   - OSOAA_TRPHI
C*   - OSOAA_ANGLE_ROTATION
C*   - SOS_REFLEX
C*   - OSOAA_MATRICE_PLAN_MERIDIEN
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
C    CTE_LENFIC2 : Longueur des noms de fichiers avec arborescence.
C    CTE_OS_NBMU_MAX  : Nombre maximal d'angles de Gauss positifs 
C                       pour les calculs de luminance
C    CTE_NT_ATM : nombre de couches du profil atmosphérique
C    CTE_NT_SEA : nombre de couches du profil océanique
C
C    CTE_SEUIL_Z : Valeur seuil pour le calcul des angles de rotation
C    CTE_SEUIL_EPSILON : Valeur seuil pour le calcul des cosinus et sinus
C                        du double des angles de rotation
C    CTE_THRESHOLD_Q_U_NULL : Valeur seuil sous laquelle Q et U sont posés nuls.
C
C    CTE_SOLAR_DISC_SOLID_ANGLE : Valeur de l'angle solide du disque solaire
C                                 apparent (pour la distance Terre-Soleil 
C                                 moyenne).
C Constantes spécifiques :
C    INCTE_IDLOG_SOS : ID du ficher Trace de la routine
C    INCTE_PI : Valeur de PI (calcule par la machine)
C----------------------------------------------------------------------------
#include "OSOAA.h"
#define INCTE_IDLOG_SOS 99
#define INCTE_PI DACOS(-1.D+00)



C==============================================================================
C PROCEDURE: OSOAA_TRPHI
C ==========
C      Programme de recomposition du champ de luminance (I,Q,U) sur tout le profil
C      et pour toutes les directions zénithales de propagation, pour un azimut
C      relatif donné.
C
C      Ajoute la composante de réflexion de l'éclairement solaire direct sur la mer
C      (qui ne fait pas partie du fichier de champ de luminance décomposée en série
C       de Fourier, mais qui est bien pris en compte pour les ordres d'interaction
C       suivants).
C
C      Ajoute la composante de transmission de l'éclairement solaire direct 
C      au passage air -> mer (transmission prise en compte pour calculer la fonction
C      source de diffusion marine d'ordre 1, mais le terme transmis atténué dans
C      la mer ne fait pas partie du fichier de champ de luminance décomposée en série
C      de Fourier).
C
C      Ajoute la luminance solaire directe, descendante du TOA à la surface, 
C      atténuée par l'atmosphère, pour la direction d'incidence solaire.
C      NB : Ce terme est calculé pour un éclairement solaire au TOA valant PI,
C           pour une distance Terre-Soleil moyenne,  
C           la constante CTE_SOLAR_DISC_SOLID_ANGLE donnant l'angle solide du disque
C           solaire apparent pour la distance Terre-Soleil moyenne. 
C
C Description des paramètres 			
C ------------------------------
C
C     FICHOS (CHAR*CTE_LENFIC2)
C                       (E) : Nom du fichier résultat binaire des calculs SOS.
C   		              (chemin complet)
C
C     NBMU (I4)         (E) : Nombre d'angles (positifs) effectivement utiles.
C
C     RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)  
C                       (E) : Tableau des cosinus des angles utilisés.
C
C     PHI  (double)     (E) : Azimut relatif (radians) entre la direction de 
C			      visée et celle du Soleil.
C
C     IMAT_SURF  (I4)   (E) : Indice précisant s'il y a usage des fichiers 
C                             SURFACE  d'interface air / mer agitée 
C			      (1 si c'est le cas, 0 sinon).
C
C     N0  (I4)          (E) : Numéro de mu de Gauss pour l'angle zénithal solaire dans l'air
C                             (angle complémentaire : mus = -rmu(N0)) 
C
C     M0  (I4)          (E) : Numéro de mu de Gauss pour l'angle zénithal solaire dans l'eau
C      			      (transmission mer plate).
C			      (angle complémentaire : musw = -rmu(M0))
C 
C
C     WIND (double)     (E) : Vitesse du vent (m/s).
C  
C     SEA_IND (double)  (E) : Indice de réfraction eau / air à la longueur 
C			      d'onde de simulation des luminances.
C
C     TAU_ATM(0:CTE_NT_ATM) (double)
C                       (E) : Profil d'épaisseur optique d'extinction de l'atmosphère. 
C
C     TAU_SEA(0:CTE_NT_SEA) (double)
C                       (E) : Profil d'épaisseur optique d'extinction de la mer. 
C
C     ANGDIFF(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)
C                       (S) : Angle de diffusion en fonction de l'angle mu
C			      et du niveau dans le profil, pour l'azimut phi.
C				  mu < 0 : champ descendant.
C				  mu > 0 : champ montant.
C
C     XIT(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)
C     XQT(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)
C     XUT(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  (double)
C                       (S) : Paramètres de Stokes I, Q et U en fonction de l'angle mu
C			      et du niveau dans le profil, pour l'azimut phi.
C				  mu < 0 : champ descendant.
C				  mu > 0 : champ montant.
C
C     IER (I4)          (S) : Indice d'erreur : 0 si pas d'erreur, -1 sinon.
C   
C
C
C Résultats fournis par la procédure	
C ----------------------------------
C 
C    Tableaux des paramètres de Stokes I, Q et U 
C    Tableau des angles de diffusion
C    
C    Un fichier Trace optionnel.
C
C
C Description des fichiers utilisés	
C ---------------------------------
C
C    Fichier FICHOS
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
C    Fichier Trace optionnel.
C      ==>   contient des informations sur les étapes de traitements
C
C
C Common utilisé:
C --------------
C  Aucun
C
C Cas d'erreur :  
C ------------
C     - Erreur d'ouverture / lecture du fichier binaire des champs de luminance
C     - Erreur d'écriture dans le fichier Trace
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant
C
C
C  Sous programmes utilisés: 
C --------------------------
C  Ce programme fait appel aux routines:
C     - SOS_CALC_GR
C     - OSOAA_CALC_GT
C     - OSOAA_ANGLE_ROTATION
C     - SOS_REFLEX
C     - OSOAA_MAT_FRESNEL_TRANS
C     - OSOAA_MATRICE_PLAN_MERIDIEN
C
C==============================================================================

      SUBROUTINE OSOAA_TRPHI(FICHOS, NBMU, RMU, PHI,
     &                       IMAT_SURF,N0,M0,WIND,SEA_IND,
     &                       TAU_ATM,TAU_SEA,
     &                       ANGDIFF, XIT, XQT, XUT, IER)

      IMPLICIT NONE


C Constantes SOS
C -----------------
            
      INTEGER*2 IDLOG			! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_SOS)

      INTEGER*2 IDFICOS			! Numéro fichier résultat binaire OS    
      PARAMETER(IDFICOS=10)

      INTEGER*4 NT_TOT		! Nombre total de couches (atmosphère + mer)      
      PARAMETER(NT_TOT=CTE_NT_ATM+CTE_NT_SEA+1)
      
      
                       	    
C* Définition des variables                         
C*-----------------------------------------------------------------
      DOUBLE PRECISION RMU(-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  ! (E) Tableau des mu de Gauss 

	!    Fonctions recomposées sur l'azimut,
	!    du champ émergeant (mu > 0) et descendant au sol (mu < 0),
	!    pour tous les niveaux du profils d'épaisseurs optiques

      DOUBLE PRECISION XIT(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
      					!(S) Paramètre de Stokes I 
      					!    en fonction de l'angle mu, et du niveau z,
					!    pour l'azimut phi.
					!    mu < 0 : champ descendant.
					!    mu > 0 : champ montant.
      DOUBLE PRECISION XQT(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
      					!(S) Paramètre de Stokes Q.
      DOUBLE PRECISION XUT(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
      					!(S) Paramètre de Stokes U.
     					     
	!    Angle de diffusion 
	!    pour le champ émergeant (mu > 0) et descendant au sol (mu < 0),
	!    pour tous les niveaux du profils d'épaisseurs optiques

      DOUBLE PRECISION ANGDIFF(0:NT_TOT,
     &                         -CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
       					!(S) Angle de diffusion en fonction de l'angle mu 
      					!    et du niveau dans le profil,
					!    pour l'azimut phi.
					!    mu < 0 : champ descendant.
					!    mu > 0 : champ montant.
					
      DOUBLE PRECISION COSDIFF_ATM	! Cosinus de l'angle de diffusion dans l'atmosphère
      					! (incidence = faisceau solaire direct)
					
      DOUBLE PRECISION COSDIFF_SEA	! Cosinus de l'angle de diffusion dans la mer
      					! (incidence = faisceau solaire direct transmis, 
					!  pour mer plate)	
					
									
	!    Fonctions pour le développement en séries géométriques,
	!    champ émergeant au TOA (mu > 0) et descendant au sol (mu < 0).
	!    Les fonctions sont établies pour un ordre S du développement
	!    en séries de Fourier, en fonction de l'angle mu et du niveau du profil.	
      DOUBLE PRECISION I3(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)		
                                ! Paramètre de Stokes I pour le cumul des
      				! ordres de diffusion precedent IG en cours.
      DOUBLE PRECISION Q3(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
      				! Paramètre de Stokes Q.
      DOUBLE PRECISION U3(0:NT_TOT,-CTE_OS_NBMU_MAX:CTE_OS_NBMU_MAX)  
      				! Paramètre de Stokes U.



      DOUBLE PRECISION PHI	! (E) Azimut relatif en radians.	       
      DOUBLE PRECISION XPHI	! Produit de PHI par IS.
	
      DOUBLE PRECISION C0	!Cosinus de l'angle d'élévation solaire rmu(N0)
      DOUBLE PRECISION S0	!Sinus de l'angle d'élévation solaire rmu(N0)
      DOUBLE PRECISION C1	!Cosinus de l'angle de visée rmu(J) > 0.
      DOUBLE PRECISION S1	!Sinus de l'angle de visée rmu(J) > 0.

      DOUBLE PRECISION COSDEV   !Cosinus de l'angle de déviation entre direction
                                !d'incidence solaire dans l'air et direction de 
				!propagation après réflexion montante dans l'air
				!ou après transmission air/mer.

      DOUBLE PRECISION COS2KI	! Cosinus de l'angle de rotation ki * 2
      DOUBLE PRECISION SIN2KI	! Sinus de l'angle de rotation ki * 2
      				
      DOUBLE PRECISION TAU_ATM(0:CTE_NT_ATM) ! Profil d'épaisseur optique d'extinction 
      					     ! de l'atmosphère. 

      DOUBLE PRECISION TAU_SEA(0:CTE_NT_SEA) ! Profil d'épaisseur optique d'extinction
      					     ! de la mer.     

      DOUBLE PRECISION AT0	! Transmission du TOA au sol pour la direction 
      				! d'incidence.

      DOUBLE PRECISION ATJ	! Valeur de transmission pour la direction J et le niveau I:
      				! direction d'incidence (du TOA à la surface) +
      				! direction d'émergence (de la surface au niveau de
				! sortie pour le champ montant ou descendant).  
      
      DOUBLE PRECISION SIGMA2	! Carré du paramètre Sigma de la théorie de Cox
      				! et Munck.
      
      DOUBLE PRECISION P        ! Probabilite de réflexion vers la direction (mu, phi).        

      DOUBLE PRECISION RF11	! Eléments de la matrice de réflexion de Fresnel
      DOUBLE PRECISION RF12     ! pour un repère lié au plan de réflexion.
      DOUBLE PRECISION RF33

      DOUBLE PRECISION TF11	! Eléments de la matrice de transmission de Fresnel
      DOUBLE PRECISION TF12     ! pour un repère lié au plan de réflexion.
      DOUBLE PRECISION TF33
            
              
      DOUBLE PRECISION M11	! Elément de la matrice de réflexion dans le 
      DOUBLE PRECISION M21      ! repère lié au plan méridien.
      DOUBLE PRECISION M31

      DOUBLE PRECISION VAL      ! Valeur intermédiaire de calcul: racine(m2-sin2(thetas))
		
      DOUBLE PRECISION RL	! Coefficient de réflexion parallèle.
      DOUBLE PRECISION RR	! Coefficient de réflexion perpendiculaire.
      DOUBLE PRECISION TL	! Coefficient de transmission parallèle.
      DOUBLE PRECISION TR	! Coefficient de transmission perpendiculaire.

      DOUBLE PRECISION COEF_SUN	! Valeur du rapport de l'éclairement solaire au TOA 
      				! (PI pour OSOAA) sur l'angle solide du disque solaire.   
				
C---- Variables liées à la définition de l'interface air - mer  

      DOUBLE PRECISION SEA_IND    ! (E) Indice de réfraction de la mer 
      				  ! pour la longueur d'onde de simulation de la luminance.
      DOUBLE PRECISION WIND	  ! (E) Vitesse du vent (m/s).
      
      DOUBLE PRECISION IND2	  ! Carré de l'indice de réfraction de la mer
 
              
C---- Fichiers utilisés          
      CHARACTER*CTE_LENFIC2 FICHOS      ! (E) Nom du fichier résultat des OS.


C---- Paramètres de type entier
      INTEGER*4 IMAT_SURF ! (E) Option de BRDF / BPDF de surface :
      			  !   0 : mer plate.
			  !   1 : mer agitée.

      INTEGER*4 N0	  ! (E) Numéro de mu de Gauss pour l'angle
      			  ! zénithal solaire dans l'air.
			  ! (angle complémentaire : mus = -rmu(N0))

      INTEGER*4 M0	  ! (E) Numéro de mu de Gauss pour l'angle
      			  ! zénithal solaire dans l'eau (transmission mer plate).
			  ! (angle complémentaire : musw = -rmu(M0))

      INTEGER*4 J	! Indice du tableau des angles de Gauss.
      INTEGER*4 I	! Indice de niveau du profil.
      INTEGER*4 ISEA	! Indice de niveau du profil marin.

      INTEGER*4 IS	! Ordre de la décomposition en séries de Fourier.  
      INTEGER*4 NBMU    ! (E) nombre d'angles utiles	
      
C---- Code d'erreur
      INTEGER*4 IER     ! (S) Code d'erreur =0 si pas d'erreur, =-1 sinon
         

C---- Variables logiques      
      LOGICAL GEO_OK    ! Status possible (TRUE) ou impossible (FALSE) de la 
      			! configuration géométrique (incidence - transmission).
	
      LOGICAL REFL_SURF ! Status de cas à réflexion vers la surface (TRUE) 
      			! ou non (FALSE) 
      			! ==> Cas non géré par le code des OS (d'où TRUE ==> KO).

 
  
       
      
C* Initialisation du code d'erreur
C------------------------------------------------------
      IER=0


      
C* Ouverture des fichiers 
C------------------------------------------------     
C     Ouverture du fichier résultat des OS
      OPEN(UNIT=IDFICOS,FILE=FICHOS,FORM='UNFORMATTED',
     &      STATUS='OLD',ERR=1000) 

     
C******************************************************************     
C* Recombinaison pour l'azimut PHI et pour chaque niveau 
C* des paramètres de Stokes I, Q et U 
C* à partir de la décomposition en séries de Fourier.  
C******************************************************************
C*    Initialisation pour IS = 0    

C*    Lecture du fichier qui a été enregistré avec des 
C*    bornes de tableaux limitées au dimensionnement utile -NBMU:NBMU
      READ(IDFICOS,ERR=1001) ((I3(I,J),I=0,NT_TOT),J=-NBMU,NBMU),
     & 			     ((Q3(I,J),I=0,NT_TOT),J=-NBMU,NBMU),
     &			     ((U3(I,J),I=0,NT_TOT),J=-NBMU,NBMU)
      
      DO 2 J= -NBMU, NBMU
      
         IF(J.EQ.0) GOTO 2
	
	 DO I = 0,NT_TOT
            XQT(I,J) = Q3(I,J)
            XUT(I,J) = U3(I,J)
            XIT(I,J) = I3(I,J)   
	 ENDDO
	 
	 !Calcul de l'angle de diffusion (indépendant des niveaux et ordres S)
	 COSDIFF_ATM = -RMU(N0)*RMU(J)
     &	               +SIN(ACOS(RMU(N0)))*SIN(ACOS(RMU(J)))*DCOS(PHI)
     	 COSDIFF_SEA = -RMU(M0)*RMU(J)
     &	               +SIN(ACOS(RMU(M0)))*SIN(ACOS(RMU(J)))*DCOS(PHI)
	  
	 DO I = 0,CTE_NT_ATM
            ANGDIFF(I,J) = ACOS(COSDIFF_ATM)*180.D+00/INCTE_PI
	 ENDDO
	 
         DO I = CTE_NT_ATM+1,NT_TOT
            ANGDIFF(I,J) = ACOS(COSDIFF_SEA)*180.D+00/INCTE_PI
	 ENDDO	 
	 
    2 CONTINUE
    
 
C*   Boucle sur l'ordre IS.    
      IS=1
      
C*    Lecture du fichier qui a été enregistré avec des 
C*    bornes de tableaux limitées au dimensionnement utile -NBMU:NBMU
   10 READ(IDFICOS,END=9,ERR=1001) ((I3(I,J),I=0,NT_TOT),J=-NBMU,NBMU),
     & 			           ((Q3(I,J),I=0,NT_TOT),J=-NBMU,NBMU),
     &			           ((U3(I,J),I=0,NT_TOT),J=-NBMU,NBMU)
   
      XPHI= IS*PHI
      
      DO 4 J= -NBMU,NBMU

         IF(J.EQ.0) GOTO 4			
	 
	 DO I = 0,NT_TOT
            XQT(I,J) = XQT(I,J) +2.*Q3(I,J)*DCOS(XPHI)
            XUT(I,J) = XUT(I,J) +2.*U3(I,J)*DSIN(XPHI)
            XIT(I,J) = XIT(I,J) +2.*I3(I,J)*DCOS(XPHI)
	 ENDDO
   4  CONTINUE
   
      IS=IS+1
      GOTO 10
      
 9    CONTINUE      

	
	
C******************************************************************
C* Ajout de la luminance (normalisée à Es=PI) du faisceau solaire 
C* direct descendant, transmis du TOA à la surface.
C* ==> On affecte toute la luminance solaire à la direction stricte
C*     du faisceau solaire : MU = MUS, PHI=0

C******************************************************************
      IF (PHI.EQ.0.) THEN
      
         COEF_SUN = INCTE_PI / CTE_SOLAR_DISC_SOLID_ANGLE
      
         DO I=0,CTE_NT_ATM    
	    XIT(I,-N0) = XIT(I,-N0) + COEF_SUN*DEXP(-TAU_ATM(I)/RMU(N0))	      	       
         ENDDO	
	 	
      ENDIF	
	
C******************************************************************
C* Ajout du terme de réflexion sur la mer et de transmission 
C* dans la mer du faisceau solaire direct incident en surface.
C*
C* Distinction du cas mer agitée et du cas mer plate
C******************************************************************
	
      IF (IMAT_SURF.EQ.1) THEN	!CAS MER AGITEE
      				!--------------

C        Cosinus (positif) et sinus de la direction d'incidence
C        ---------------------------------------------------- 
         C0 = RMU(N0)
	 S0 = DSQRT(1.D+00-C0*C0)
		 
C        Transmission du TOA au sol pour la direction d'incidence
C        ---------------------------------------------------- 
         AT0=DEXP(-TAU_ATM(CTE_NT_ATM)/C0)
	 
C        Carré du paramètre Sigma de la théorie de Cox et Munck.
C        ---------------------------------------------------- 
         SIGMA2 = .003 + .00512*WIND


C        GESTION DE LA REFLEXION DU FAISCEAU SOLAIRE DIRECT
C        ----------------------------------------------------         
         DO 101 J=1,NBMU			
            C1=RMU(J)
	    S1 = DSQRT(1.D+00-C1*C1)

C           Calcul de la probabilite P de réflexion dans la direction J 
C           pour la statistique de pentes de vague  
C           -------------------------------------------------    
            CALL SOS_CALC_GR(C0,C1,S0,S1,SIGMA2,PHI,P) 
	          
C           Calcul des angles de diffusion et de rotation. 
C           	mu d'incidence : -CO <0
C           	mu de la direction de transmission : C1 >0
C           -------------------------------------------------     
            CALL OSOAA_ANGLE_ROTATION(-C0,C1,PHI,COS2KI,SIN2KI,COSDEV)
      
C           Calcul des éléments de matrice de la réflexion de Fresnel 
C           pour un repère lié au plan de réflexion.
C           -------------------------------------------------     
            CALL SOS_REFLEX(COSDEV,SEA_IND,RF11,RF12,RF33)
      
C           Calcul des éléments de matrice de la réflexion de Fresnel 
C           pour un repère lié au plan méridien (rotation). 
C           -------------------------------------------------     
            CALL OSOAA_MATRICE_PLAN_MERIDIEN(COS2KI,SIN2KI,RF11,RF12,
     &				    	     M11,M21,M31)

C           Ajout de la réflexion directe pour un éclairement solaire 
C           normalisé (PI) atténué (ATJ) avec la probabilite P liée 
C           aux pentes de vagues.
C           -------------------------------------------------     
            DO I=0,CTE_NT_ATM
	    
	       !Transmission pour la direction d'incidence et pour chaque
               !direction d'émergence J du sol au niveau I. 
	       ATJ = AT0 *DEXP(-(TAU_ATM(CTE_NT_ATM)-TAU_ATM(I))/RMU(J))
	       
	       !Pondération finale
	       ATJ = ATJ*P/(4.*C1)
	       
	       !Ajout du terme de réflexion directe au vecteur de Stokes	       
	       XIT(I,J)= XIT(I,J) + M11*ATJ
               XQT(I,J)= XQT(I,J) + M21*ATJ
               XUT(I,J)= XUT(I,J) + M31*ATJ
	       
	    ENDDO
	
  101    CONTINUE !Fin boucle sur J



C        GESTION DE LA TRANSMISSION DU FAISCEAU SOLAIRE DIRECT
C        -----------------------------------------------------  
         DO 111 J=1,NBMU	
            C1=RMU(J)
	    S1 = DSQRT(1.D+00-C1*C1)
	    
C           Calcul de la probabilite P de transmission dans la direction J 
C           pour la statistique de pentes de vague  
C           -------------------------------------------------  
            CALL OSOAA_CALC_GT(C0,C1,S0,S1,SEA_IND,SIGMA2,PHI,
     &		 	       GEO_OK,REFL_SURF,P)
	    
	    
C           Calcul des angles de déviation et de rotation. 
C              mu d'incidence : -CO <0
C              mu de la direction de transmission : -C1 <0
C           -------------------------------------------------    
            CALL OSOAA_ANGLE_ROTATION(-C0,-C1,PHI,COS2KI,SIN2KI,COSDEV)


C           Calcul des éléments de matrice de la transmission de Fresnel 
C            pour un repère lié au plan de déviation, avec pondération
C           préalable au passage à l'expression dans un repère lié au plan
C           méridien.
C           -------------------------------------------------    
            CALL OSOAA_MAT_FRESNEL_TRANS(COSDEV,SEA_IND,
     &	    				 TF11,TF12,TF33)
      
C           Calcul des éléments de matrice de la transmission de Fresnel 
C           pour un repère lié au plan méridien (rotation). 
C           -------------------------------------------------    
            CALL OSOAA_MATRICE_PLAN_MERIDIEN(COS2KI,SIN2KI,TF11,TF12,
     &	    				     M11,M21,M31)


C           Ajout de la transmission directe pour un éclairement solaire 
C           normalisé (PI) atténué (ATJ) avec la probabilite P liée 
C           aux pentes de vagues.
C           -------------------------------------------------     
            DO ISEA=0,CTE_NT_SEA
	    
	    
	       I=ISEA+CTE_NT_ATM+1
	       
	       !Atténuation du TOA à la surface pour la direction d'incidence 
               !et dans la mer pour chaque direction d'émergence J de la
	       !surface au niveau I. 
	       ATJ = AT0 * DEXP(-TAU_SEA(ISEA)/C1)
	       
	      
	       !Pondération finale
	       ATJ = ATJ*P/C1
	       
	       
	       !Ajout du terme de transmission directe au vecteur de Stokes	       	       
               XIT(I,-J)= XIT(I,-J)+ M11*ATJ
               XQT(I,-J)= XQT(I,-J)+ M21*ATJ
               XUT(I,-J)= XUT(I,-J)+ M31*ATJ

	    ENDDO
	
  111    CONTINUE !Fin boucle sur J  
  
     
      ELSE	!CAS MER PLATE
      		!-------------

C        Test si le plan d'incidence correspond au plan principal solaire
C        ---------------------------------------------------- 
C* 
         IF (DCOS(PHI).EQ.1.D+00) THEN

            IND2 = SEA_IND*SEA_IND
	    
            !Transmission du TOA au sol pour la direction d'incidence 
            C0  = RMU(N0)
            AT0=DEXP(-TAU_ATM(CTE_NT_ATM)/C0)

            !Calcul des coefficients de la matrice de réflexion et de 
	    !transmission de Fresnel pour la direction d'incidence solaire
	    VAL = DSQRT(IND2+C0*C0-1.D+00)
		
            RL = (IND2*C0 - VAL) / (IND2*C0 + VAL)
  	    RR = (C0 - VAL) / (C0 + VAL)
		
            TL = (1. + RL) / SEA_IND
  	    TR = 1. + RR
		
            RF11 = 0.5*(RL*RL+RR*RR)
            RF12 = 0.5*(RL*RL-RR*RR)
		
            TF11 = 0.5*(TL*TL+TR*TR)
            TF12 = 0.5*(TL*TL-TR*TR)
	   
	    
	    !Réflexion du faisceau solaire direct (Es = PI)
            DO I=0,CTE_NT_ATM
	    
	       !Transmission pour la direction d'incidence 
	       !et de réflexion au niveau I
	       ATJ = AT0 * DEXP(-(TAU_ATM(CTE_NT_ATM)-TAU_ATM(I))/C0)
	       
               XIT(I,N0) = XIT(I,N0) + RF11*COEF_SUN*ATJ 
	       XQT(I,N0) = XQT(I,N0) + RF12*COEF_SUN*ATJ  

	    ENDDO

		
		
	    !Transmission du faisceau solaire direct (Es = PI)
            DO ISEA=0,CTE_NT_SEA
	    
	       I=ISEA+CTE_NT_ATM+1
	       
	       !Atténuation du TOA à la surface pour la direction d'incidence 
               !et dans la mer pour la direction de transmission dans la mer
	       !de la surface au niveau I. 
	       ATJ = AT0 * DEXP(-TAU_SEA(ISEA)/RMU(M0))
	       
	       !Pondération finale
	       ATJ = ATJ*(SEA_IND*SEA_IND*SEA_IND)*COEF_SUN
	       ATJ = ATJ*RMU(M0)/RMU(N0)
	       
	       !Ajout du terme de transmission directe au vecteur de Stokes	       	       
               XIT(I,-M0)= XIT(I,-M0)+ TF11*ATJ
               XQT(I,-M0)= XQT(I,-M0)+ TF12*ATJ
	       
	    ENDDO
	            
	 ENDIF  !Test dans PPS
  
      ENDIF	!Test mer agitée / mer plate




C******************************************************************
C* Mise à zéro des termes de polarisation trop faible
C*
C* --> Evite des erreurs d'arrondi (U non nul) 
C*     pour le passage de PHI=0 à PHI=PI  
C*
C* Mise à zéro du terme I si inférieur à 1.E-99.
C****************************************************************** 
      DO J= -NBMU,NBMU
	 DO I = 0,NT_TOT
	    IF (XIT(I,J).LE.1.D-99) THEN
	        XIT(I,J)=0.D+00
	    ENDIF
	    
            IF (DABS(XQT(I,J)).LT.CTE_THRESHOLD_Q_U_NULL) THEN
	        XQT(I,J)=0.D+00
            ENDIF
	    IF (DABS(XUT(I,J)).LT.CTE_THRESHOLD_Q_U_NULL) THEN
	        XUT(I,J)=0.D+00
            ENDIF
	 ENDDO
      ENDDO
      
      
    
      
C* Fermeture fichiers
C-------------------
      CLOSE(IDFICOS)    ! Fermeture du fichier binaire des OS      
      
      
C* Fin nominale 
C-------------------
      GOTO 9999
            
C* Cas d'erreur et retour du status -1 au programme appelant  
C----------------------------------------------
 1000 WRITE(6,*) 
     &  '  OSOAA_TRPHI ERROR_1000 on  binary SOS result file opening'
      GOTO 9998      

 1001 WRITE(6,*) 
     &  '  OSOAA_TRPHI ERROR_1001 on  binary SOS result file reading'
      GOTO 9998  
      
 1010 WRITE(6,*) '  OSOAA_TRPHI ERROR_1010 on logfile writing' 
      GOTO 9998
                       
9998  IER=-1             
9999  RETURN   


            
      END      !FIN DE LA PROCEDURE OSOAA_TRPHI
      
      
      
      
      

C==============================================================================
C PROCEDURE: OSOAA_ANGLE_ROTATION
C ==========
C      Cette procédure calcule :
C         - le cosinus de l'angle de diffusion entre la direction d'incidence 
C	    solaire et la direction de visée.
C	  - le cosinus de l'angle de rotation KI. 
C      en fonction d'une direction d'incidence et d'une direction de visée
C      (direction de propagation)
C
C      Reprise de la routine SOS_ANGLE du code SOS.
C      L'angle de rotation calculé est KI et non pas KI'.
C      Généralisation de la routine pour une réflexion comme une transmission.
C
C Description des paramètres
C --------------------------
C       XMUS      (double)  (E)	 Valeur du cosinus de l'angle zénithal solaire.
C       XMUV	  (double)  (E)	 Valeur absolue du cosinus de l'angle zénithal de visée.
C       PHI	  (double)  (E)	 Azimut relatif entre la direction de visée
C                                et celle du Soleil (en radians).
C       COS2KI	  (double)  (S)  Cosinus du double de l'angle de rotation Ki.
C       SIN2KI	  (double)  (S)  Sinus du double de l'angle de rotation Ki.
C       COSDEV	  (double)  (S)  Cosinus de l'angle de déviation.
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
      SUBROUTINE OSOAA_ANGLE_ROTATION(XMUS,XMUV,PHI,
     &                                COS2KI,SIN2KI,COSDEV)
      
      IMPLICIT NONE


C* Définition des variables   
C*----------------------------------------
      DOUBLE PRECISION XMUS     !Cosinus de l'angle zénithal solaire (<0)
      DOUBLE PRECISION XMUV     !Cosinus de l'angle zénithal de visée 
      				! (>0 si réflexion, <0 si transmission)

      DOUBLE PRECISION PHI	!Azimut relatif entre la direction de visée
      				!et celle du Soleil (en radians).
      
      DOUBLE PRECISION COS2KI	!(S) Cosinus du double de l'angle de rotation Ki.
      DOUBLE PRECISION SIN2KI	!(S) Sinus du double de l'angle de rotation Ki.
      DOUBLE PRECISION COSDEV	!(S) Cosinus de l'angle de déviation entre direction
      				!    d'incidence et direction de visée.
            				 
      DOUBLE PRECISION S	! 1 ou -1.
      DOUBLE PRECISION Z	! 
      DOUBLE PRECISION COSKI	! Cosinus de l'angle de rotation Ki.

C* Traitement                
C--------------         
      COSDEV = XMUS*XMUV + DSQRT(1-XMUS**2)*DSQRT(1-XMUV**2)*DCOS(PHI)
  
      S=1.
      IF (DSIN(PHI).GT.0.D+00)  S=-1.

      Z=S*(DSQRT(1-COSDEV**2))*(DSQRT(1-XMUV*XMUV))
      
      
      IF (DABS(Z).GT.CTE_SEUIL_Z)  THEN
      
         !Cas général Z non proche de zéro
	 !-------------------------------- 
         COSKI=(XMUV*COSDEV-XMUS)/Z	
	  
	 IF ((1.-DABS(COSKI)).GE.CTE_SEUIL_EPSILON) THEN

	     !COSKI pas trop proche de 1 ou -1
	     COS2KI = 2.*COSKI*COSKI-1.
             SIN2KI = 2.*COSKI*DSQRT(1.-COSKI*COSKI)
	 ELSE
	     !COSKI proche de 1 ou -1 
	     !==> risque d'arrondi numérique donnant une valeur > 1 ou < -1.
	     COS2KI=1.D+00
	     SIN2KI=0.D+00
	 ENDIF
	  
      ELSE
         !Cas Z proche de zéro
	 !--------------------
	        
         !Valeur de COSKI pour le cas où Z tend vers zéro :
         !   cas d'une direction de propagation vers le zénith : 
         !       --> réflexion avec XMUV qui tend vers 1
	 !           ==> COSKI tend vers S*COS(PHI)
         !   cas d'une direction de propagation vers le nadir : 
         !       --> transmission avec XMUV qui tend vers -1  
	 !           ==> COSKI tend vers -S*COS(PHI)
	 !   cas d'une réflexion en rétrodiffusion :
	 !       --> réflexion avec COSDEV qui tend vers -1  
	 !   cas d'une transmission en diffusion avant:
	 !       --> transmission avec COSDEV qui tend vers 1 
	 
	 IF (DABS(DABS(COSDEV)-1.).LE.CTE_SEUIL_EPSILON) THEN
	 
	     !Cas COSDEV tend vers 1 ou -1
	     COS2KI=1.D+00
	     SIN2KI=0.D+00
	     
	 ELSE
	 
	     !Cas direction de propagation tend vers nadir (transmission)
	     !ou zénith (réflexion)
	  
            IF(XMUV.GT.0.D+00) THEN
	       !Cas réflexion avec XMUV qui tend vers 1
	       COSKI=S*DCOS(PHI)
	    ELSE
	       !Cas transmission avec XMUV qui tend vers -1
	       COSKI=-S*DCOS(PHI)
	    ENDIF

	    IF (DABS(1.-DABS(COSKI)).GE.CTE_SEUIL_EPSILON) THEN
	        !COSKI pas trop proche de 1 ou -1
	        COS2KI = 2.*COSKI*COSKI-1.
               SIN2KI = 2.*COSKI*DSQRT(1.-COSKI*COSKI)
	    ELSE
	        !COSKI proche de 1 ou -1 
	        !==> risque d'arrondi numérique donnant une valeur > 1 ou < -1.
	        COS2KI=1.D+00
	        SIN2KI=0.D+00
	    ENDIF
	 	    
         ENDIF !Fin test COSDEV proche de 1 ou -1
      
  
      ENDIF  ! Fin gestion Z proche de zéro ou non
      
      
      
      RETURN
      END  	!FIN DE LA PROCEDURE OSOAA_ANGLE_ROTATION
      
 
      
      
C==============================================================================
C PROCEDURE: SOS_REFLEX
C ==========
C      Cette procédure calcule les éléments R11, R12 et R33 de la matrice
C      de réflexion de Fresnel exprimée dans un repère lié au plan de réflexion.
C
C      La matrice de Fresnel s'écrit :
C                  | R11(teta)  R12(teta)    0      |
C         F(teta)= | R12(teta)  R11(teta)    0      |
C                  |    0          0      R33(teta) |
C
C         avec :
C           _ teta : angle de réflexion par rapport à la normale au dioptre,
C           _ R11(teta) = 0.5 * (RL*RL + RR*RR),
C           _ R12(teta) = 0.5 * (RL*RL - RR*RR),
C           _ R33(teta) = RL*RR,
C
C         L'indice de réfraction relatif du dioptre, entre le milieu de 
C         réflexion et le milieu de transmission, est IND.
C
C         On calcule RL et RR en fonction de IND et teta, par :
C           _ RL : le coefficient de réflexion parallèle
C
C               RL(teta) = [ IND**2 * cos(teta) - SQRT(IND**2 - sin(teta)**2) ]
C                        / [ IND**2 * cos(teta) + SQRT(IND**2 - sin(teta)**2) ]
C
C           _ RR : le coefficient de réflexion perpendiculaire
C
C               RR(teta) = [ cos(teta) - SQRT(IND**2 - sin(teta)**2) ]
C                        / [ cos(teta) + SQRT(IND**2 - sin(teta)**2) ]
C
C
C
C Description des paramètres
C --------------------------
C       COSDIF	  (double)  (E)  Cosinus de l'angle de diffusion.
C       IND	  (double)  (E)  indice de réfraction relatif 
C     				 (eau / air pour une réflexion sur mer).
C       R11	  (double)  (S) Elément de la matrice de réflexion de Fresnel.
C       R12	  (double)  (S) idem R11
C       R33	  (double)  (S) idem R11
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
      SUBROUTINE SOS_REFLEX(COSDIF,IND,R11,R12,R33)
      
      IMPLICIT NONE      

C* Définition des variables                
C*---------------------------    
      DOUBLE PRECISION COSDIF	! Cosinus de l'angle de diffusion.
      
      DOUBLE PRECISION IND	! Indice de réfraction relatif 
      				! (eau / air pour une réflexion sur mer).
      
      DOUBLE PRECISION R11	!Elément de la matrice de réflexion de Fresnel.
      DOUBLE PRECISION R12
      DOUBLE PRECISION R33
      
      
      DOUBLE PRECISION IND2	! Carré de l'indice de réfraction relatif.
				
      DOUBLE PRECISION COSW	! Cosinus de l'angle de réflexion W
      				! (angle d'incidence par rapport à la 
				!  normale du dioptre)
      
      DOUBLE PRECISION V	! Sinus carré de W.
      DOUBLE PRECISION X	! 
      
      DOUBLE PRECISION RL	! Coefficient de réflexion parallèle.
      DOUBLE PRECISION RR	! Coefficient de réflexion perpendiculaire.
      

C* Traitements                
C--------------     
      IND2=IND*IND
      
      COSW=DSQRT(.5*(1-COSDIF))
      V=.5*(1+COSDIF)
      X=DSQRT(IND2-V)     
      RL=(IND2*COSW-X)/(IND2*COSW+X)
      RR=(COSW-X)/(COSW+X)
      R11=(RL**2+RR**2)/2.
      R12=(RL**2-RR**2)/2.
      R33=RR*RL
      RETURN
      END  	!FIN DE LA PROCEDURE SOS_REFLEX





C==============================================================================
C PROCEDURE: OSOAA_MAT_FRESNEL_TRANS
C ==========
C      Cette procédure calcule les éléments F11, F12 et F33 de la matrice
C      de transmission de Fresnel exprimée dans un repère lié au plan de déviation.
C      Cas d'une transmission air -> mer.
C
C      L'indice de réfraction relatif du dioptre, entre le milieu de 
C      réflexion (N1) et le milieu de transmission (N2), est N = N1/N2.
C			
C      La matrice de Fresnel en transmission, exprimée pour un repère lié au plan 
C      de déviation s'écrit :
C                   | F11(i)  F12(i)    0    |
C         F(i)= C * | F12(i)  F11(i)    0    |
C                   |  0       0      F33(i) |
C
C       * Avec :                              
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
C Description des paramètres
C --------------------------
C       COSDEVTR  (double)  (E)  Cosinus de l'angle de déviation entre direction
C				 d'incidence solaire dans l'air et direction de 
C				 propagation dans la mer après transmission air/mer.
C       N1SN2	  (double)  (E)  Indice de réfraction relatif (air / eau).
C       F11	  (double)  (S)  Elément de la matrice de transmission de Fresnel.
C       F12	  (double)  (S)  idem F11
C       F33	  (double)  (S)  idem F11
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
      SUBROUTINE OSOAA_MAT_FRESNEL_TRANS(COSDEVTR,IND,F11,F12,F33)
      
      IMPLICIT NONE      

C* Définition des variables                
C*---------------------------    
      DOUBLE PRECISION COSDEVTR	! Cosinus de l'angle de déviation entre direction
                                ! d'incidence solaire dans l'air et direction de 
				! propagation dans la mer après transmission air/mer.

      DOUBLE PRECISION IND      ! Indice de réfraction de la mer par rapport à l'air
				           				
      DOUBLE PRECISION N1SN2    ! Rapport d'indice du milieu d'incidence 
      				! sur le milieu de réfraction: N1SN2 = 1 / IND
				
      DOUBLE PRECISION F11	! Elément de la matrice de transmission de Fresnel.
      DOUBLE PRECISION F12
      DOUBLE PRECISION F33

      DOUBLE PRECISION COS_I    ! Cosinus de l'angle d'incidence par rapport à la 
      				! normale de vague.
      DOUBLE PRECISION SIN2_I	! Sinus carré de l'angle d'incidence.
      DOUBLE PRECISION COS_T	! Cosinus de l'angle réfracté.
      DOUBLE PRECISION COS2_T	! Cosinus carré de l'angle réfracté.
      DOUBLE PRECISION SIN2_T	! Sinus carré de l'angle réfracté.

      DOUBLE PRECISION CL	! Coefficient de réflexion ou transmission parallèle.
      DOUBLE PRECISION CR	! Coefficient de réflexion ou transmission perpendiculaire.
      
      DOUBLE PRECISION COEFT    ! Coefficient de pondération de la matrice de Fresnel
      				! en transmission.
      
C---- Variables logiques
      LOGICAL CONFIG_STATUS     ! Status possible (TRUE) ou impossible (FALSE) de la 
      				! configuration géométrique incidence - transmission.
      
      

C* Traitements                
C--------------     
      N1SN2 = 1.D+00 / IND

C* Cosinus de l'angle d'incidence (angle / normale au dioptre) :
C---------------------------------------------------------------------      
      CALL OSOAA_CALC_COSI_TRANS(IND,COSDEVTR,CONFIG_STATUS,COS_I)
      	
      IF (CONFIG_STATUS.EQV..TRUE.) THEN
	    SIN2_I= 1.D+00-COS_I*COS_I

C* Cosinus de l'angle de réfraction (angle / normale au dioptre) :
C---------------------------------------------------------------------
	    SIN2_T = N1SN2*N1SN2*SIN2_I
	 
	    COS2_T = 1.D+00 - SIN2_T
            COS_T = DSQRT(COS2_T)

C* Coefficients de réflexion parallèle et perpendiculaire
C--------------------------------------------------------
	    CL= (COS_I - N1SN2*COS_T) / (COS_I + N1SN2*COS_T)
	    CR= (N1SN2*COS_I - COS_T) / (N1SN2*COS_I + COS_T)
		
C* Coefficients de transmission parallèle et perpendiculaire
C------------------------------------------------------------------------	 
	    CL= N1SN2 * (1.D+00+CL)
	    CR= 1.D+00 + CR
		
C* Coefficients de pondération de la matrice de Fresnel
C------------------------------------------------------------------------	 	
	    IF (COS_I.NE.0.) THEN
		COEFT = COS2_T  * (IND*IND*IND)
		COEFT = COEFT  / (IND*COS_T - COS_I)
		COEFT = COEFT  / (IND*COS_T - COS_I)	
	    ELSE
		!Cas d'une incidence à 90 degrés ==> Pas de transmission
		COEFT = 0.
	    ENDIF   
	        
	 	
C* Eléments de la matrice de Fresnel en réflexion ou transmission
C----------------------------------------------------------------	                
	    F11=.5*(CL*CL+CR*CR) * COEFT
            F12=.5*(CL*CL-CR*CR) * COEFT
            F33=CL*CR * COEFT

      ELSE !Gestion des cas de transmission impossible ==> nulle
	    
	    F11=0.D+00
            F12=0.D+00
            F33=0.D+00	 
	    
      ENDIF ! Fin test config_status

     
     
      RETURN
      END  	!FIN DE LA PROCEDURE OSOAA_MAT_FRESNEL_TRANS




C==============================================================================
C PROCEDURE: OSOAA_MATRICE_PLAN_MERIDIEN
C ==========
C      Cette procédure calcule les éléments M11, M21 et M31 de la matrice
C      de réflexion/transmission exprimée dans un repère lié au plan méridien 
C      (1ère colonne).
C      Application de la rotation selon l'angle KI.
C      
C
C      Reprise de la routine SOS_MATRIC du code SOS.
C      L'angle de rotation appliqué est KI et non pas KI'.
C      Généralisation de la routine pour une réflexion comme une transmission.
C    
C
C Description des paramètres
C --------------------------
C       COS2KI	  (double)  (E) Cosinus du double de l'angle de rotation KI 
C       SIN2KI	  (double)  (E) Sinus du double de l'angle de rotation KI      
C       F11	  (double)  (E)	Elément de la matrice de réflexion/transmission 
C     				de Fresnel dans le repère lié au plan de déviation.
C       F12	  (double)  (E)	idem F11
C       M11	  (double)  (S) Elément de la matrice de réflexion/transmission  
C                               dans le  repère lié au plan méridien.
C       M21	  (double)  (S) idem M11
C       M31	  (double)  (S) idem M11
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
      SUBROUTINE OSOAA_MATRICE_PLAN_MERIDIEN(C2,S2,F11,F12,M11,M21,M31)
      
      IMPLICIT NONE

C* Définition des variables   
C*------------------------------------------------        
      DOUBLE PRECISION F11	!Elément de la matrice de réflexion
      				!de Fresnel dans le repère lié au plan de déviation.
      DOUBLE PRECISION F12
      
      DOUBLE PRECISION M11	!Elément de la matrice de réflexion/transmission dans le 
      				!repère lié au plan méridien.
      DOUBLE PRECISION M21
      DOUBLE PRECISION M31
   
      DOUBLE PRECISION C2	! Cosinus de 2*KI (KI: angle de rotation)
      DOUBLE PRECISION S2	! Sinus de 2*KI.
      

C* Traitements                
C--------------                     
      M11=F11
      M21=C2*F12
      M31=S2*F12
      
      RETURN
      END  	!FIN DE LA PROCEDURE OSOAA_MATRICE_PLAN_MERIDIEN



       
       
