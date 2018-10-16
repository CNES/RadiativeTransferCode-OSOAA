C******************************************************************************
C* FICHIER: OSOAA_MIE.F
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
C* PROJET: Ordres successifs de diffusion
C*
C* ROLE: Ce module gère les procédures permettant les calculs de la théorie
C*       de MIE.
C*
C* AUTEURS: 
C*   Code initial: Laboratoire d'Optique Atmosphérique (LOA, Villeneuve d'Ascq).
C*   Reprise du codage et développement : CS SI, Bruno Lafrance, Claudie Casteras.
C*          ==> code SOS (version industrialisée du code des OS du LOA).
C*
C*
C* ROUTINES COUVERTES PAR LE FICHIER : 
C*   - SOS_MIE
C*   - SOS_XERO
C*   - SOS_FPHASE_MIE
C*   - SOS_NOM_FICMIE
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
C    CTE_LENFIC1 : Longueur des noms de fichiers sans arborescence.
C    CTE_LENFIC2 : Longueur des noms de fichiers avec arborescence.
C    CTE_MIE_NBMU_MAX : Nombre maximal d'angles de Gauss positifs
C    CTE_MIE_DIM : Dimension des tableaux
C
C Constantes spécifiques :
C    INCTE_IDLOG_MIE : ID du ficher Trace de la routine  MIE 
C    INCTE_PI : Valeur de PI (calculé par la machine)                          
C----------------------------------------------------------------------------
#include "OSOAA.h"
#define INCTE_IDLOG_MIE 88
#define INCTE_PI DACOS(-1.D+00)



C==============================================================================
C PROCEDURE: SOS_MIE
C ==========      
C
C      Cette procédure applique la théorie de MIE pour des particules sphériques.
C
C      La nature chimique des particules est caractérisée par leur indice de 
C      réfraction relativement à l'environnement : m.
C      m = mr + i * mi, avec "mr" la partie réelle et "mi" la partie imaginaire.
C      La partie imaginaire est entrée négative. Elle est responsable des effets
C      d'absorption.
C
C      Les calculs de Mie sont réalisés en fonction du paramètre de taille : 
C      alpha = 2*pi * r / wa, où r est le rayon de la particule et wa est la 
C      longueur d'onde du rayonnement. 
C
C      Le pas sur alpha est:
C         si        alpha  <= 0.1  ===> pas = 0.0001
C         si 0.1  < alpha  <= 1.0  ===> pas = 0.001
C         si 1.0  < alpha  <= 10   ===> pas = 0.01
C         si 10   < alpha  <= 30   ===> pas = 0.05
C         si 30   < alpha  <= 100  ===> pas = 0.1
C         si 100  < alpha          ===> pas = 1.0
C
C
C      Ce programme calcule pour l'indice m :      
C      
C         - Le coefficient d'efficacité en extinction: Qext(alpha) 
C           de alpha0 a alphaF
C         - Le coefficient d'efficacité en diffusion:  Qsca(alpha) 
C           de alpha0 a alphaF
C         - Le facteur d'anisotropie: G
C         - Les fonctions de phases : Imie(alpha,mu), 
C                                     Qmie(alpha,mu),
C				      Umie(alpha,mu)
C 	    de alpha0 a alphaF et pour mu, cosinus de l'angle de diffusion, des
C           angles de Gauss entre 0 et pi.
C
C Description des paramètres
C ----------------------------
C       MIE_NBMU (I4) : (E) Nombre d'angles (positifs) effectivement utiles
C	RMU(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX) (double) : (E) Cosinus des angles.
C	CHR(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX) (double) : (E) Poids des angles.
C	RN (double) (E) : partie réelle de l'indice de réfraction.
C	IN (double) (E) : partie imaginaire de l'indice de réfraction.
C	ALPHAO (double) (E) : valeur de alpha à partir de laquelle 
C			      on commence le calcul.
C	ALPHAF (double) (E) : valeur de alpha pour laquelle on arrête le calcul.			  
C	FICMIE (CHARACTER*CTE_LENFIC2) (E) : chemin et nom du fichier MIE.
C	FICLOG (CHARACTER*CTE_LENFIC2) (E) : nom du fichier TRACE de MIE .
C	IER (I4) (S) : code d'erreur = 0 si pas d'erreur, sinon =1	
C
C
C Résultat fourni par la procédure
C --------------------------------
C   Fichier de MIE
C   Fichier de trace (optionnel)
C
C
C Variable d'environnement
C ------------------------
C   Aucune
C 
C Description des fichiers utilisés
C ---------------------------------
C   -->Contenu du fichier de MIE (créé)
C       Fichier binaire non formaté, créé par SOS_MIE
C       1 enregistrement d'entête avec
C                Indice de réfraction (partie réelle)
C                Indice de réfraction (partie imaginaire)
C                Paramètre de taille limite
C                Nombre d'angles de Gauss positif
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
C
C Common utilisé:
C --------------
C  Aucun
C
C
C Cas d'erreur :
C ------------
C     - Décodage des paramètres incorrect
C     - Paramètres du programme incorrects : ALPHAF trop grand par rapport à CTE_MIE_DIM
C     - Ouverture d'un fichier
C     - Erreur si le fichier de MIE existe déjà
C     - Lecture/écriture dans un fichier
C     - Erreur dans un sous-programme
C
C     Affichage d'un message à l'écran, arrêt du programme et 
C     retour du status -1 au programme appelant
C        
C   
C  Sous programmes utilisés:      
C --------------------------
C  Ce programme fait appel aux routines:
C      - SOS_XERO
C      - SOS_FPHASE_MIE
C
C==============================================================================

      SUBROUTINE SOS_MIE(MIE_NBMU,RMU,CHR,RN,IN,ALPHAO,ALPHAF,
     &                   FICMIE,FICLOG,IER)


      IMPLICIT NONE

C Index de fichier
C -----------------
      INTEGER*2 IDLOG	! Numéro identifiant du fichier Trace des calculs de Mie	    
      PARAMETER(IDLOG=INCTE_IDLOG_MIE)

C* Définition des variables                                                   
C----------------------------------------------------------------------------        
      DOUBLE PRECISION RN	! Partie réelle de l'indice de réfraction.
      DOUBLE PRECISION IN	! Partie imaginaire de l'indice de réfraction.
      DOUBLE PRECISION ALPHAO   ! Valeur de alpha à partir de laquelle 
      				! on commence le calcul.				
      DOUBLE PRECISION ALPHAF   ! Valeur de alpha pour laquelle on arrête le calcul.
                                          
      DOUBLE PRECISION RMU(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)   !Cosinus des angles.
      DOUBLE PRECISION CHR(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)   !Poids des angles.

      DOUBLE PRECISION CNA(-1:CTE_MIE_DIM)   !Fonction réelle Cn(alpha). 
      DOUBLE PRECISION SNA(-1:CTE_MIE_DIM)   !Fonction réelle Sn(alpha).
           
      DOUBLE PRECISION RGNA(-1:CTE_MIE_DIM)  !Partie réelle de la fonction Gn(alpha).	
      DOUBLE PRECISION IGNA(-1:CTE_MIE_DIM)  !Partie imaginaire de Gn(alpha).
      
      DOUBLE PRECISION RDNA(0:CTE_MIE_DIM)   !Fonction réelle Dn(alpha).
      DOUBLE PRECISION RDNB(0:CTE_MIE_DIM)   !Partie réelle de la fonction Dn(alpha*m).
      DOUBLE PRECISION IDNB(0:CTE_MIE_DIM)   !Partie imaginaire de  Dn(alpha*m).
      
      DOUBLE PRECISION RA(0:CTE_MIE_DIM)     !Partie réelle de la fonction An(alpha). 
      DOUBLE PRECISION IA(0:CTE_MIE_DIM)     !Partie imaginaire de An(alpha). 
      DOUBLE PRECISION RB(0:CTE_MIE_DIM)     !Partie réelle de la fonction Bn(alpha). 
      DOUBLE PRECISION IB(0:CTE_MIE_DIM)     !Partie imaginaire de Bn(alpha).
      
      

      
      DOUBLE PRECISION QEXT		!Coefficient d'efficacité en extinction.
      DOUBLE PRECISION QSCA		!Coefficient d'efficacité en diffusion.
      DOUBLE PRECISION G		!Facteur d'anisotropie.
      
      DOUBLE PRECISION ALPHA		!Valeur courante du paramètre de taille.
      DOUBLE PRECISION PAS		!Pas entre deux valeurs de ALPHA.
      
      DOUBLE PRECISION RBETA		!Partie réelle de m*alpha.
      DOUBLE PRECISION IBETA            !Partie imaginaire de m*alpha.
      

      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
      DOUBLE PRECISION W
      
      DOUBLE PRECISION X1
      DOUBLE PRECISION X2
      DOUBLE PRECISION X3
      DOUBLE PRECISION X4
      DOUBLE PRECISION X5      
      DOUBLE PRECISION X6
      DOUBLE PRECISION X7
           
      DOUBLE PRECISION Y1
      DOUBLE PRECISION Y2
      DOUBLE PRECISION Y3
      DOUBLE PRECISION Y4
      DOUBLE PRECISION Y5
      DOUBLE PRECISION Y6
      DOUBLE PRECISION Y7
      DOUBLE PRECISION Y8
      
      DOUBLE PRECISION Z3
      DOUBLE PRECISION Z4
      DOUBLE PRECISION Z5      
      DOUBLE PRECISION Z6
      DOUBLE PRECISION Z7
      DOUBLE PRECISION Z8
      DOUBLE PRECISION Z9
      
      DOUBLE PRECISION T
      DOUBLE PRECISION Q
      
      DOUBLE PRECISION W6		!Valeur de 2 / ALPHA**2
      DOUBLE PRECISION XX
      DOUBLE PRECISION YY
      DOUBLE PRECISION ZZ
      DOUBLE PRECISION TT
      
      DOUBLE PRECISION A2

      INTEGER*4 MIE_NBMU   		! Nombre d'angles effectifs des simulations      
      INTEGER*4 UN			! Valeur 1 ou -1
      INTEGER*4 TEST
      
      INTEGER*4 N1			! Ordre maximal de calcul des suites Dn et Sn
      INTEGER*4 N2			! Ordre maximal de calcul des suites Gn et Cn
      INTEGER*4 N2P1			
      INTEGER*4 M
       
      INTEGER*4 J				
      INTEGER*4 I
      INTEGER*4 N
      INTEGER*4 DIM                   ! dimension
      INTEGER*4 IFIN                  ! nb caractères significatifs d'une chaine
      
      LOGICAL TRACE                   ! = vrai,si écriture dans le fichier trace
       
      CHARACTER*CTE_LENFIC2 FICMIE    ! chemin d'accès au fichier MIE, nom du fichier MIE
      CHARACTER*CTE_LENFIC2 FICLOG    ! nom du fichier TRACE de MIE
      
      INTEGER*4 IER       	      ! code d'erreur =0 si pas d'erreur, =-1 sinon      
      
                       
C* Initialisation du code d'erreur
C------------------------------------------------------
      IER=0     

C* Test de compatibilite entre alphaF et l'ordre maximal 
C* des développements pour le calcul des fonctions de 
C* Ricatti-Bessel (et de leurs dérivées)
C------------------------------------------------------
      IF (DINT(ALPHAF+ALPHAF+20).GT.CTE_MIE_DIM) GOTO 997



C* Fichier trace
C---------------
      IF (FICLOG.EQ.'NO_LOG_FILE') THEN
	 TRACE=.FALSE.
      ELSE
         TRACE=.TRUE.
	 
	 OPEN (IDLOG,FILE=FICLOG,ERR=913)
	 WRITE(IDLOG,*,err=923) ' '
	 WRITE(IDLOG,*,err=923) ' '
	 WRITE(IDLOG,*,err=923) '-----------------------'
         WRITE(IDLOG,*,err=923) 'Calculation of MIE file'
	 WRITE(IDLOG,*,err=923) '-----------------------'     
      END IF     
      
C* Ouverture des fichiers 
C------------------------------------------------------
      OPEN (1,FILE=FICMIE,FORM='UNFORMATTED',
     *      STATUS='NEW',ERR=911)
            
             
C* Initialisation des tableaux à 0   
C------------------------------------------------------  
      DIM=CTE_MIE_DIM 
      CALL SOS_XERO(CNA,-1,DIM)
      CALL SOS_XERO(SNA,-1,DIM)
      CALL SOS_XERO(RGNA,-1,DIM)  
      CALL SOS_XERO(IGNA,-1,DIM)
      CALL SOS_XERO(RDNA,0,DIM) 
      CALL SOS_XERO(RDNB,0,DIM)           
      CALL SOS_XERO(IDNB,0,DIM)    
      
                                                       
C* Fichier trace 
C------------------------------------------------------  
      IF (TRACE) THEN
        WRITE(IDLOG,*,err=923)'SIMULATION PARAMETERS '
        WRITE(IDLOG,*,err=923)
     *      ' Refractive index (real part): ',RN
        WRITE(IDLOG,*,err=923)
     *      ' Refractive index (imaginary part): ',IN
        WRITE(IDLOG,*,err=923)' Limit values of the size parameter: ',
     *           ALPHAO,' et ',ALPHAF
        WRITE(IDLOG,*,err=923)' '
        WRITE(IDLOG,*,err=923)' List of used angles : '
	WRITE(IDLOG,*,err=923) "   Index, mu, angle (degrees), weigth"
	DO J=-MIE_NBMU,MIE_NBMU
	    WRITE(IDLOG,222,ERR=923) J, RMU(J),
     &	                DACOS(RMU(J))*180./INCTE_PI, CHR(J)
	ENDDO
	WRITE(IDLOG,*,err=923)' '
	WRITE(IDLOG,*,err=923)'RESULTS'
	WRITE(IDLOG,*,err=923)' Value of the size parameter alpha'
	WRITE(IDLOG,*,err=923)
     *      ' Value of the Mie extinction efficiency factor Qext'
	WRITE(IDLOG,*,err=923)
     *      ' Value of the Mie scattering efficiency factor Qsca'
	WRITE(IDLOG,*,err=923)' Value of the anisotropy factor G'
	WRITE(IDLOG,*,err=923)' '
        WRITE(IDLOG,10,err=923)'  alpha','Qext','Qsca','G'
      END IF

C* Ecriture de l'entête du fichier de sortie  
C------------------------------------------------------                         
      WRITE(1,err=921)RN,IN,ALPHAF,MIE_NBMU
      
  
  
C* Boucle sur les valeurs du paramètre de taille ALPHA de
C* ALPHA0 à ALPHAF par pas variable. (étiquette 7777)
C------------------------------------------------------          
      ALPHA=ALPHAO
           
 7777 CONTINUE   
 
C* Estimation du pas pour la valeur de ALPHA suivante  
C------------------------------------------------------
         PAS=0.0001
         IF(ALPHA.gt.0.1)  PAS =0.001
         IF(ALPHA.gt.1.00) PAS =0.01
         IF(ALPHA.gt.10.)  PAS =0.05
         IF(ALPHA.gt.30.)  PAS =0.10
         IF(ALPHA.gt.100.) PAS =1.00
 
 
      
C* Calcul des fonctions dérivées de Ricatti-Bessel : 
C* Gn(alpha), Dn(alpha), Dn(alpha*m) - parties réelle et imaginaire
C*
C* Calcul des fonctions réelles Sn(alpha) et Cn(alpha)
C------------------------------------------------------    

C*  Ordres de développement limités : N1 pour Dn et Sn
C*                                    N2 pour Gn et Cn
                 
         N1   = DINT(ALPHA+ALPHA+20)
	 IF(N1.GT.CTE_MIE_DIM) GOTO 997  
         N2   = DINT(ALPHA+ALPHA+5)  
         N2P1 = N2+1  
	 
	 

C*  Récurrence sur Gn(alpha) : partie réelle RGNA et imaginaire IGNA, 
C*  et sur Cn(alpha) de n=1 a N2.
C------------------------------------------------------  
      
C*   Initialisation des suites Cn (CNA réel)
C*   et Gn (partie réelle RGNA et imaginaire IGNA)    

         CNA(-1) = -DSIN(ALPHA)   
	 CNA(0)  =  DCOS(ALPHA)
	     
         RGNA(-1) =  0.D+00
         RGNA(0)  =  0.D+00         
         IGNA(-1) =  0.D+00
         IGNA(0)  = -1.D+00


C*  Boucle de récurrence
    
         DO 25 I=1,N2
            X=RGNA(I-1)
            Z=I/ALPHA
            Y=IGNA(I-1)
            W=((Z-X)*(Z-X)+(Y*Y)) 
            RGNA(I)=(Z-X)/W-Z 
            IGNA(I)=Y/W

            CNA(I)=(2*I-1.D+00)*CNA(I-1)/ALPHA-CNA(I-2)	
	    
C*    Contrôle : 
C*      Si Cn(alpha) ne diverge pas => on poursuit la récurrence.   
C*      Sinon, on redéfinit les ordres limites N2 et N1, puis on quitte
C*      la boucle de récurrence.	    
            IF(CNA(I).LT.1.D+304) GO TO 25	    
            N2=I 
            N2P1=I+1
            N1=I+15
            GOTO 100
	    
   25    CONTINUE
  100    CONTINUE



C*  Récurrence sur Dn(alpha)   : fonction réelle RDNA
C*                 Dn(alpha*m) : partie réelle RDNB et imaginaire IDNB
C*                 Sn(alpha)   : fonction réelle SNA
C*  Boucle de N1 à 0
C------------------------------------------------------         

         RBETA=RN*ALPHA       
         IBETA=IN*ALPHA    
         X1=RBETA*RBETA+IBETA*IBETA 
         X2=RBETA/X1
         X3=IBETA/X1
	 
C*   Initialisation a N1            
         RDNA(N1)=0.D+00
         RDNB(N1)=0.D+00
         IDNB(N1)=0.D+00
      
         SNA(N1)=0.D+00 
         SNA(N1-1)=1.D+00

C*   Boucle de récurrence de N1-1 à 0    
         DO 30 I=N1-1,0,-1
	    
            X=RDNB(I+1) 
            Y=IDNB(I+1)
            Z=X+(I+1.D+00)*X2
            W=Y-(I+1.D+00)*X3 
            X4=Z*Z+W*W 
            RDNB(I)=(I+1.D+00)*X2-Z/X4  
            IDNB(I)=-(I+1.D+00)*X3+W/X4
	    
            Z=(I+1.D+00)/ALPHA
            X=RDNA(I+1)  
            RDNA(I)=Z-1.D+00/(X+Z)
	    
            SNA(I-1)=(2.D+00*I+1.D+00)*SNA(I)/ALPHA-SNA(I+1)

C*   Si les valeurs de Sn deviennent trop grandes, on normalise
C*   l'ensemble des termes calculés par Sn-1. 	    
            IF (SNA(I-1).GT.1D+304) THEN    	     	
                TEST=I-1 
                X=SNA(TEST)
                DO 35 J=TEST,N2
                   SNA(J)=SNA(J)/X 
   35           CONTINUE
            ENDIF
	    		  
   30    CONTINUE 
 
 
C*   Normalisation de Sn(alpha) par sin(alpha)/S0(alpha)
C*   pour vérifier la condition n=0 : S0(alpha) = sin(alpha)                 
         Q=SNA(0)/DSIN(ALPHA)    
                                                           
         DO 45 I=0,N2       
             SNA(I)=SNA(I)/Q 
   45    CONTINUE
   
         
          


C* Calcul des fonctions An(alpha) et Bn(alpha) :
C* parties réelle (RA, RB) et imaginaire (IA, IB)
C------------------------------------------------------    
         UN=1 
	                                        
         DO 50 I=1,N2     
            X1=SNA(I)
            X2=CNA(I)
            X3=RDNB(I)
            X4=IDNB(I)                
            X5=RDNA(I)
            X6=RGNA(I)         
            X7=IGNA(I)           
            Y1=X3-RN*X5          
            Y2=X4-IN*X5         
            Y3=X3-RN*X6+IN*X7         
            Y4=X4-RN*X7-IN*X6       
            Y5=RN*X3-IN*X4-X5        
            Y6=IN*X3+RN*X4        
            Y7=RN*X3-IN*X4-X6       
            Y8=IN*X3+RN*X4-X7       
            Z4=Y2*Y3-Y1*Y4       
            Z3=Y1*Y3+Y2*Y4        
            Z5=X1*X1+X2*X2       
            Z6=Y3*Y3+Y4*Y4
            Z7=Y5*Y7+Y6*Y8       
            Z8=Y6*Y7-Y5*Y8         
            Z9=Y7*Y7+Y8*Y8          
            Q=(I+I+1.D+00)/I/(I+1.D+00)*UN

C*   Test sur X2 (fonction Cn(alpha)) : tend à diverger pour n important.
C*   Z5 tend vers l'infini en X2**2. La valeur de X2 / Z5 tend alors vers 0.
C*   Ce test évite un problème numérique de rapport Inf / Inf.          	    
	    IF (X2.GT.1D+300) THEN
		Y1=0.D+00
		Y2=0.D+00
		Y3=0.D+00
		Y4=0.D+00		
	    ELSE
                Y1=X1*(X1*Z3+X2*Z4)/Z5/Z6          
                Y2=X1*(X1*Z4-X2*Z3)/Z5/Z6         
                Y3=X1*(X1*Z7+X2*Z8)/Z5/Z9          
                Y4=X1*(X1*Z8-X2*Z7)/Z5/Z9  
	    ENDIF
            
            RA(I)=Y2*Q               
            IB(I)=Y3*Q           
            Q=-Q              
            RB(I)=Y4*Q         
            IA(I)=Y1*Q        
            UN=-UN    
   50    CONTINUE
    	 
                                                                 
         RA(0)=0.D+00
         IA(0)=0.D+00
         RB(0)=0.D+00
         IB(0)=0.D+00
         RA(N2P1)=0.D+00 
         IA(N2P1)=0.D+00
         RB(N2P1)=0.D+00
         IB(N2P1)=0.D+00


C* Calcul des coefficients d'efficacité en diffusion (Qsca) 
C* et en extinction (Qext).
C* Calcul du facteur d'anisotropie (G).
C------------------------------------------------------
         QEXT=0.D+00
         QSCA=0.D+00
         G=0.D+00
         J=-1
         X=RA(1)      
         Y=IA(1)       
         Z=RB(1)        
         T=IB(1)
                                                                         
         DO 55 N=1,N2         
            M=N+1
            XX=RA(M) 
            YY=IA(M)
            ZZ=RB(M) 
            TT=IB(M) 
            A2=(N+1.D+00)
            QEXT=QEXT+N*A2*J*(Y-T) 
            QSCA=QSCA+N*N*A2*A2/(N+A2)*(X*X+Y*Y+Z*Z+T*T)    
            J=-J  
            G=G-A2*N/(A2+N)*(N*(A2+1.)*(A2+1.)/(2.*N+3.)*
     1        (Y*YY+X*XX+T*TT+Z*ZZ)+Y*T+X*Z)
            X=XX
            Y=YY
            Z=ZZ
            T=TT
   55    CONTINUE 
                                                             
         W6=2.D+00/ALPHA/ALPHA  
         QEXT=W6*QEXT         
         QSCA=W6*QSCA             
         G=4.D+00*G/QSCA/ALPHA/ALPHA

C* Appel de la routine permettant le calcul des fonctions de phase de Mie
C* et enregistrant les résultats pour ALPHA.       
C------------------------------------------------------
         CALL SOS_FPHASE_MIE(MIE_NBMU,RMU,RA,IA,RB,IB,ALPHA,
     *                      QEXT,QSCA,G,N2,TRACE,IER) 
         IF (IER.NE.0) GOTO 996   ! cas d'erreur
      
C* Passage à la valeur suivante du paramètre de taille ALPHA (arrêt si la 
C* limite ALPHAF est dépassée).                 
C------------------------------------------------------
         ALPHA=ALPHA+PAS 
      IF (ALPHA.LE.ALPHAF) GOTO 7777 
                                                   
 
C* Fermeture fichier
C-------------------
      CLOSE(1)	!Fermeture du fichier de MIE

C    NB : On ne ferme pas le fichier TRACE pour permettre 
C	  de le compléter pour un appel ultérieur de la routine.

      
C* Fin nominale 
C-------------------
      GOTO 9999
      
C* Cas d'erreur et retour du status -1 au programme appelant
C-----------------------------------------------------------
  911 WRITE(6,*) 'Error while opening the MIE file'
      IER=-1  
      GOTO 9999
  913 WRITE(6,*) 'Error while opening the MIE log file'
      IER=-1  
      GOTO 9999        
  921 WRITE(6,*) 'Error while writing the MIE file'
      IER=-1  
      GOTO 9999
  922 WRITE(6,*) 'Error on angles file reading'
      IER=-1  
      GOTO 9999
  923 WRITE(6,*) 'Error while writing the MIE log file'
      IER=-1  
      GOTO 9999    
  996 WRITE(6,*) 'OSOAA_MIE : Error from subroutine SOS_PHASE_MIE'
      IER=-1
      GOTO 9999
  997 WRITE(6,*) 'Value AlphaMax too large by comparison to CTE_MIE_DIM'
      IER=-1
      GOTO 9999      
  
C* Format
C---------   
   10 FORMAT(A7,17X,A4,16X,A4,15X,A1)          
  222 FORMAT(I4,1X,D21.14,2X,F7.3,2X,D21.14,2X)   
                                                   
 9999 CONTINUE
 
      RETURN
      END    !FIN DE LA PROCEDURE SOS_MIE   
      

 
                                                                      

C==============================================================================
C PROCEDURE: SOS_XERO
C ==========      
C
C  Cette procédure affecte zéro aux éléments d'un tableau IT(N:M)
C
C
C Description des paramètres
C --------------------------
C    N,M (I4) (E)  Dimensions du tableau IT           
C    IT(N:M) (double) (E/S)  Tableau à initialiser
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
      SUBROUTINE SOS_XERO(IT,N,M) 
   
      IMPLICIT NONE
      
C* Définition des variables 
C---------------------------
      INTEGER*4 N,M			! Dimensions du tableau   
      DOUBLE PRECISION IT(N:M)		! Tableau 
               
      INTEGER*4 I			!Indice du tableau
      
C*   Initialisation du tableau 
C-----------------------------           
      DO 1 I=N,M  
    1 IT(I)=0.D+00
    
      RETURN 
      END  	!FIN DE LA PROCEDURE SOS_XERO       
      
 
                         
                                                        
C==============================================================================
C PROCEDURE: SOS_FPHASE_MIE
C ==========     
C 
C      Cette procédure calcule les fonctions de phase Imie(mu), Qmie(mu),
C      Umie(mu) pour une valeur fixée du paramètre de taille ALPHA et 
C      pour les mu de Gauss du tableau RMU.
C
C      Il y a enregistrement (non formaté) des résultats:
C        - Paramètre de taille (réel),
C        - Coefficient d'efficacité en extinction (réel),
C        - Coefficient d'efficacité en diffusion (réel),
C        - Facteur d'anisotropie (double précision),
C        - Tableau de la fonction de phase Imie(mu) (réel),
C        - Tableau de la fonction de phase polarisée Qmie(mu) (réel),
C        - Tableau de la fonction de phase polarisée Umie(mu) (réel).
C
C    --> Les tableaux ont une taille utile limitée à MIE_NBMU : 
C        ils sont écris avec cette limitation.
C
C     
C Description des paramètres
C --------------------------
C    MIE_NBMU (I4)   (E)   Nombre d'angles (positifs) effectivement utiles
C    RMU(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX) (double) (E) Cosinus des angles de Gauss  
C    RA(0:CTE_MIE_DIM)  (double) (E)   Partie réelle de la fonction An(alpha).
C    IA(0:CTE_MIE_DIM)  (double) (E)   Partie imaginaire de An(alpha).  
C    RB(0:CTE_MIE_DIM)  (double) (E)   Partie réelle de la fonction Bn(alpha). 
C    IB(0:CTE_MIE_DIM)  (double) (E)   Partie imaginaire de Bn(alpha).
C    ALPHA (double)  (E)   Valeur du paramètre de taille. 
C    KMA1  (double)  (E)   Coefficient d'efficacité en extinction.
C    KMA2  (double)  (E)   Coefficient d'efficacité en diffusion.
C    G     (double)  (E)   Facteur d'anisotropie.
C    N2    (I4)      (E)   Ordre maximal de sommation sur N.
C    TRACE (logical) (E)   vrai,si écriture dans le fichier trace
C    IER (I4) (S) : code d'erreur = 0 si pas d'erreur, sinon =1	
C
C
C Résultat stocké dans le fichier MIE:
C ------------------------------------
C    AY1   (R4)	        Valeur du paramètre de taille ALPHA.
C    AY2   (R4)	        Coefficient d'efficacité en extinction.
C    AY3   (R4)	        Coefficient d'efficacité en diffusion.
C    AY4   (double)     Facteur d'anisotropie.
C    IMIE(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)    (R4)	      
C                 fonction de phase : Imie(mu) pour ALPHA.
C    QMIE(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)    (R4)	     
C                  fonction de phase polarisée: Qmie(mu).
C    UMIE(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)    (R4)     
C                  fonction de phase polarisée: Umie(mu).       
C      
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C
C     Erreur d'écriture dans fichier résultat ou fichier Trace.
C
C     Affichage d'un message à l'écran, arrêt du programme et 
C     retour du status -1 au programme appelant
C
C==============================================================================
      SUBROUTINE SOS_FPHASE_MIE(MIE_NBMU,RMU,RA,IA,RB,IB,ALPHA,
     *                          KMA1,KMA2,G,N2,TRACE,IER)
 

      IMPLICIT NONE

C Index de fichier
C -----------------
      INTEGER*2 IDLOG		! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_MIE)
      
C* Définition des variables
C----------------------------------------------------------------------------
      DOUBLE PRECISION RMU(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX) 
                                        !Cosinus des angles de Gauss 	
      
      DOUBLE PRECISION ALPHA       !Valeur du paramètre de taille. 
      DOUBLE PRECISION KMA1	   !Coefficient d'efficacité en extinction.
      DOUBLE PRECISION KMA2        !Coefficient d'efficacité en diffusion.
      DOUBLE PRECISION G 	   !Facteur d'anisotropie.


      DOUBLE PRECISION RA(0:CTE_MIE_DIM)     !Partie réelle de la fonction An(alpha). 
      DOUBLE PRECISION IA(0:CTE_MIE_DIM)     !Partie imaginaire de An(alpha).
      DOUBLE PRECISION RB(0:CTE_MIE_DIM)     !Partie réelle de la fonction Bn(alpha).
      DOUBLE PRECISION IB(0:CTE_MIE_DIM)     !Partie imaginaire de Bn(alpha).
      DOUBLE PRECISION AR	!Valeur du tableau RA pour n donné.
      DOUBLE PRECISION AI       !Valeur du tableau IA pour n donné.
      DOUBLE PRECISION BR       !Valeur du tableau RB pour n donné.
      DOUBLE PRECISION BI       !Valeur du tableau IB pour n donné.   	      
            
      DOUBLE PRECISION COEF
      DOUBLE PRECISION X        !Valeur mu = -cos(tetad) 
      				!avec tetad l'angle de diffusion.
				           
      DOUBLE PRECISION PIM      !Fonction PI(mu) d'ordre n-1.
      DOUBLE PRECISION PIV      !Fonction PI(mu) d'ordre n.
      DOUBLE PRECISION PIP     	!Fonction PI(mu) d'ordre n+1.	
      DOUBLE PRECISION TAU      !Fonction TAU(mu) d'ordre n.
      
      DOUBLE PRECISION RES1     !Partie réelle de la fonction S1(alpha,m,mu).
      DOUBLE PRECISION IMS1     !Partie imaginaire de S1(alpha,m,mu).
      DOUBLE PRECISION RES2     !Partie réelle de la fonction S2(alpha,m,mu).
      DOUBLE PRECISION IMS2     !Partie imaginaire de S2(alpha,m,mu).      
      
      DOUBLE PRECISION Y1
      DOUBLE PRECISION Y2
      DOUBLE PRECISION Y3
      DOUBLE PRECISION Y4
      DOUBLE PRECISION AY4	!Facteur d'anisotropie.
      
      REAL*4 AY1		!Valeur du paramètre de taille ALPHA.
      REAL*4 AY2		!Coefficient d'efficacité en extinction.
      REAL*4 AY3		!Coefficient d'efficacité en diffusion.
      REAL*4 IMIE(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)     
                             !fonction de phase : Imie(mu) pour ALPHA.
      REAL*4 QMIE(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)     
                             !fonction de phase polarisée: Qmie(mu).
      REAL*4 UMIE(-CTE_MIE_NBMU_MAX:CTE_MIE_NBMU_MAX)
                             !fonction de phase polarisée: Umie(mu).       

      LOGICAL TRACE          ! = vrai,si écriture dans le fichier trace

      INTEGER*4 MIE_NBMU     ! Nombre d'angles effectifs des simulations         
      INTEGER*4 N2           !Ordre maximal de sommation sur N 
      INTEGER*4 J
      INTEGER*4 N
      
      INTEGER*4 IER        ! code d'erreur =0 si pas d'erreur, =-1 sinon           


      AY1=ALPHA           
      AY2=KMA1      
      AY3=KMA2    
      AY4=G
      COEF=2.D+00/KMA2/ALPHA**2
      
      DO 1 J=-MIE_NBMU,MIE_NBMU              
         X=-RMU(J)            
         PIM=0.D+00 
         PIV=1.D+00  
         TAU=X          
         RES1=0.D+00
         RES2=0.D+00
         IMS1=0.D+00
         IMS2=0.D+00
	 
         DO 2 N=1,N2           
            AI=IA(N)
            BI=IB(N)
            AR=RA(N)
            BR=RB(N)
            RES1=RES1-AI*PIV-BI*TAU  
            RES2=RES2+AI*TAU+BI*PIV  
            IMS1=IMS1+AR*PIV+BR*TAU 
            IMS2=IMS2-AR*TAU-BR*PIV  
            PIP=((2.D+00*N+1.D+00)*X*PIV-(N+1.D+00)*PIM)/N 
            PIM=PIV
            PIV=PIP  
            TAU=(N+1.D+00)*X*PIV-(N+2.D+00)*PIM 
    2    CONTINUE 
                                                                     
         Y1=RES1*RES1+IMS1*IMS1  
         Y2=RES2*RES2+IMS2*IMS2 
         Y3=2.D+00*RES2*RES1   
         Y4=2.D+00*IMS2*IMS1
         IMIE(J)=COEF*(Y1+Y2)      
         QMIE(J)=COEF*(Y2-Y1)
         UMIE(J)=COEF*(Y3+Y4)
    1 CONTINUE    
    
C* Enregistrement des résultats pour ALPHA
C*    Enregistrement des tableaux de fonctions de phase 
C*    pour des bornes d'enregistrement limitées au dimensionnement 
C*    utile -MIE_NBMU:MIE_NBMU
C------------------------------------------------------
      WRITE(1,err=921) AY1,AY2,AY3,AY4,
     &                (IMIE(J),J=-MIE_NBMU,MIE_NBMU),
     &                (QMIE(J),J=-MIE_NBMU,MIE_NBMU),
     &                (UMIE(J),J=-MIE_NBMU,MIE_NBMU)
      
C* Fichier trace :alpha,Qext,Qsca,G
C------------------------------------------------------  
      IF (TRACE) THEN
	   WRITE(IDLOG,20,err=923) AY1,AY2,AY3,AY4
      END IF


C* Fin nominale 
C-------------------
      GOTO 9999
      
C* Cas d'erreur et retour du status -1 au progamme appelant
C----------------------------------------------------------
  921 WRITE(6,*) 'Erreur d''écriture sur le fichier MIE'
      IER=-1 
      GOTO 9999
  923 WRITE(6,*) 'Erreur d''écriture sur le fichier trace MIE'
      IER=-1 
      GOTO 9999

C* Format 
C----------        
20    FORMAT(1X,E15.7,5X,E15.7,5X,E15.7,5X,E15.7)  

 9999 CONTINUE
 
      RETURN                                                                    
      END         !FIN DE LA PROCEDURE SOS_FPHASE_MIE       


 
 
 
C==============================================================================
C PROCEDURE: SOS_NOM_FICMIE
C ==========
C      Cette procédure génère le nom du fichier de Mie en fonction de :
C      	- l'indice de réfraction : partie réelle RN
C	- l'indice de réfraction : partie imaginaire IN
C	- le paramètre de taille minimal ALPHAO
C	- le paramètre de taille maximal ALPHAF
C       - le nombre d'angles de Gauss utilisés NBMU_GAUSS
C       - le nom du fichier d'angles utilisateur FICANGLES_USER
C          (en supprimant son arborescence) 
C
C      On impose d'avoir :
C         RN au format F5.3 : précision à 1.E-O3 près.
C         IN au format F8.5 : précision à 1.E-O5 près pour valeur négative.
C         ALPHAO au format F6.4 : 1.E-04 <= ALPHA0 < 10
C         ALPHAF au format F8.2 : 1.E-02 <= ALPHA0 < 1.E+05
C         NBMU_GAUSS au format I4 : 1 < NBMU_GAUSS < 9999
C
C      Le nom du fichier de Mie sera sous la forme:
C
C      MIE-RN-IN-ALPHAO-ALPHAF-MUnb-NomFicUser avec 
C      - RN au format F5.3 (ex : 1.400)
C      - IN (opposé à la valeur vraie négative) au format F7.5 (ex : 0.00800)
C      - ALPHAO au format F6.4 (ex : 0.0001)
C      - ALPHAF au format F8.2 (ex : 00500.00)
C      - MUnb le nombre d'angles de Gauss (ex : MU40)
C      - NomFicUser le nom du fichier utilisateur (sans arborescence) (ex : MyAngles)
C
C      On convertit RN en une chaine de caractères CRN(1:4) 
C      Pour cela on passe du Format F5.3 * 1000 => A4
C           Le premier caractère CRN(1) est le chiffre unité de RN,
C           Le second caractère CRN(2) est la première décimale de RN,
C           etc.
C
C
C Description des paramètres entrée / sortie
C ------------------------------------------
C	NBMU_GAUSS (I4)	: (E) Nombre d'angles de Gauss utilisés
C       FICANGLES_USER (CHARACTER*CTE_LENFIC2)	: (E) Nom du fichier des angles 
C                                                 "utilisateurs" (chemin complet)
C	RN (double)	: (E) partie réelle de l'indice de réfraction
C	IN (double)	: (E) partie imaginaire de l'indice de réfraction
C       ALPHAO (double) : (E) paramètre de taille minimal
C       ALPHAF (double) : (E) paramètre de taille maximal
C	FICS (CHARACTER*CTE_LENFIC1) : (S) nom du fichier de Mie (sans arborescence)	
C
C Constantes utilisées:
C --------------------
C    CTE_LENFIC1 : Longueur des noms de fichiers sans arborescence.
C    CTE_LENFIC2 : Longueur des noms de fichiers avec arborescence.
C
C Common utilisé:
C --------------
C     Aucun
C
C Cas d'erreur :
C ------------
C     Aucun
C==============================================================================
      SUBROUTINE SOS_NOM_FICMIE(NBMU_GAUSS,FICANGLES_USER,
     &                          RN,IN,ALPHAO,ALPHAF,FICS) 

      IMPLICIT NONE
         
      DOUBLE PRECISION RN	! Partie réelle de l'indice de réfraction.
      DOUBLE PRECISION IN	! Partie imaginaire de l'indice de réfraction.
      DOUBLE PRECISION ALPHAO   ! Valeur de alpha à partir de laquelle 
      				! on commence le calcul de Mie.				
      DOUBLE PRECISION ALPHAF   ! Valeur de alpha pour laquelle on arrête le calcul.

      CHARACTER*CTE_LENFIC2 FICANGLES_USER  ! chemin et nom du fichier des 
      					    ! angles "utilisateurs"	
      CHARACTER*CTE_LENFIC1 FICS	    ! nom du fichier MIE 	
          
      CHARACTER*4 CRN		! Chaines de caractères
      CHARACTER*6 CIN 
      CHARACTER*5 CALPHAO   
      CHARACTER*7 CALPHAF
      CHARACTER*4 CNBMU
      
      INTEGER*2 I
      INTEGER*4 IDEB, IDEB_CH  ! Position de début d'une chaine de caractères
      INTEGER*4 IFIN, IFIN_CH  ! Position de fin d'une chaine de caractères
      INTEGER*4 NBMU_GAUSS     ! Nombre d'angles de Gauss utilisés 
      			      	 
	    
C --------- Conversion en chaines de caractères des paramètres d'entrée	   
      WRITE(CRN,'(I4)') int(RN*1000)  		
      WRITE(CIN,'(I6)') int(-IN*100000)		!Valeur positive
      
      WRITE(CALPHAO,'(I5)') int(ALPHAO*10000)
      WRITE(CALPHAF,'(I7)') int(ALPHAF*100)
      
      WRITE(CNBMU,'(I4)') NBMU_GAUSS

C --------- Les blancs sont remplacés par des 0	      
      DO I=1,4
         IF( CRN(I:I).EQ.' ') CRN(I:I) = '0'
      ENDDO
      
      DO I=1,6
         IF( CIN(I:I).EQ.' ') CIN(I:I) = '0'
      ENDDO
      
      DO I=1,5
         IF( CALPHAO(I:I).EQ.' ') CALPHAO(I:I) = '0'
      ENDDO
      
      DO I=1,7
         IF( CALPHAF(I:I).EQ.' ') CALPHAF(I:I) = '0'
      ENDDO
      	 
	 
      FICS = 'MIE'
      
C -------- Construction de la chaine de caractères pour l'indice RN	    
      FICS(4:4) = CRN(1:1)     ! Chiffre des unités
      FICS(5:5) = '.'	       ! Virgule
      FICS(6:8) = CRN(2:4)     ! Trois premières décimales   
      FICS(9:9) = '-'
      
C -------- Construction de la chaine de caractères pour l'indice IN	    
      FICS(10:10) = CIN(1:1)     ! Chiffre des unités
      FICS(11:11) = '.'	         ! Virgule
      FICS(12:16) = CIN(2:6)     ! Cinq premières décimales   	    
      FICS(17:17) = '-'	
      
C -------- Construction de la chaine de caractères pour AlphaO	    
      FICS(18:18) = CALPHAO(1:1)     ! Chiffre des unités
      FICS(19:19) = '.'	             ! Virgule
      FICS(20:23) = CALPHAO(2:5)     ! Quatre premières décimales   	    
      FICS(24:24) = '-'	

C -------- Construction de la chaine de caractères pour AlphaF	    
      FICS(25:29) = CALPHAF(1:5)     ! Chiffres des unités
      FICS(30:30) = '.'	             ! Virgule
      FICS(31:32) = CALPHAF(6:7)     ! Deux premières décimales   	    
      FICS(33:33) = '-'	            

C -------- Construction de la chaine de caractères pour MUgauss
C --------Structuration pour avoir NBMU_GAUSS à gauche (sans "blancs")	            
      
      FICS(34:35) = 'MU'
      
      IF (NBMU_GAUSS.LT.10) THEN
          FICS(36:36) = CNBMU(4:4)	!MUn
      ELSE 
          IF (NBMU_GAUSS.LT.100) THEN
              FICS(36:37) = CNBMU(3:4)  !MUnn
	  ELSE 
	     IF (NBMU_GAUSS.LT.1000) THEN
                 FICS(36:38) = CNBMU(2:4) !MUnnn
	     ELSE !NBMU_GAUSS < 10000
	     	 FICS(36:39) = CNBMU(1:4) !MUnnnn
	     ENDIF !NBMU_GAUSS < 1000
	  ENDIF !NBMU_GAUSS < 100
      ENDIF !NBMU_GAUSS < 10
  
       
C -------- Construction de la chaine de caractères pour NomFicUser
C -------- Structuration pour avoir NBMU_GAUSS à gauche (sans "blancs")	            


      IF (FICANGLES_USER.NE.'NO_USER_ANGLES') THEN

C       Détermination de la position du nom de fichier sans son arborescence          
	  IFIN_CH=INDEX(FICANGLES_USER,' ')
          IFIN_CH=IFIN_CH-1
	  IF (IFIN_CH.LE.0) IFIN_CH=CTE_LENDIR
	  
	  IDEB_CH=0
	  	  
10	  IDEB=INDEX(FICANGLES_USER(IDEB_CH:IFIN_CH),'/')
	  IDEB_CH=IDEB_CH+IDEB
          IF (IDEB.NE.0) GOTO 10	  

C       Ajout du nom de fichier d'angles "utilisateur" au nom du fichier de MIE    
          IFIN=INDEX(FICS,' ')  
	  FICS(IFIN:IFIN) = '-'
	  
	  IFIN=IFIN+1
	  FICS(IFIN:IFIN+IFIN_CH-IDEB_CH+1) = 
     &                 FICANGLES_USER(IDEB_CH:IFIN_CH)
	  
      ENDIF 
     	
	
      END !SOS_NOM_FICMIE
 
 
 
