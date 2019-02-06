C******************************************************************************
C* FICHIER: OSOAA_MAIN.F
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
C* ROLE: Programme principal du code OSOAA.
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
C*      - Communication & Systemes (CS, Toulouse)
C*          Bruno Lafrance :
C*              Responsable technique
C*              Rétro-ingénierie sur OSOA, industrialisation du code.
C*              Spécification de l'interface air/mer agitée, implémentation.
C*          Xavier Lenot :
C*              Rétro-ingénierie sur la gestion des profils marins 
C*              et atmosphériques, industrialisation du module.
C*          Sylvain Marty, Oscar Picas-Puig, Nicolas Ducoin :
C*              Développement des IHM.
C*
C*
C* ROUTINE COUVERTE PAR LE FICHIER : OSOAA_MAIN
C*
C*
C* MOD:VERSION:1.0: 02/04/2015: Initial version of the OSOAA code
C* MOD:VERSION:1.1: 15/06/2015: All comments for log files are now in english
C* MOD:VERSION:1.2: 22/03/2016: 
C*       Introduction of the possibility to perform a new simulation 
C*       based on same bio-physical parameters but for different output 
C*	 parameters (Phi, Z, ...) without recalculating the radiance field as a
C*       Fourier series expansion but by using that of a previous process.                      
C*   
C*       Introduction of the possibility to not perform a radiative transfer                       
C*       calculation if IGMAX = 0. In this case, only profiles and phase                           
C*       functions are calculated.
C*
C*       Introduction of a new file, FIC_FLUX = "Flux.txt"
C*       --> Profile of downwelling and upwelling flux from TOA 
C*           to the sea bottom normalised to the solar irradiance at TOA.
C* MOD:VERSION:1.3: 03/11/2016:
C*     Changes to be compliant with a f77 compilation
C*         - CALL FLUSH() changed to CALL FLUSH(6)
C*
C* MOD:VERSION:1.4: 16/05/2018:
C*     - Change of the writing format for the SEA.BotAlb parameter in the file 
C*       listing the process parameters (label 1917)
C*       Format F8.5 becomes F12.5 to avoid "****" in case of this parameter 
C*       is not defined and the associated value is CTE_NOT_DEFINED_VALUE_DBLE value.
C*
C*     - Introduction of the calculation of the sea bottom reflectance in case of 
C*       use of tabulated data in CTE_FIC_BOT_SPECTRAL_DATA.
C*       --> Estimate the RHO_BOT reflectance for light sand (IBOT_TYPE=2), 
C*           Green algae (IBOT_TYPE=3), Brown algae (IBOT_TYPE=3), Red algae (IBOT_TYPE=4)
C*
C* MOD:VERSION:1.5: 11/01/2019:
C*     - In case of a null wind velocity, the directory to compute Glitter files is 
C*       no more required in the list of parameters.
C*
C******************************************************************************

C----------------------------------------------------------------------------
C Définition des constantes  
C---------------------------------------------------------------------------- 
C Constantes utilisées :
C    CTE_LENKEYWORD : Taille maximale des Keywords pour le passage des arguments.
C    CTE_LENDIR : Longueur des noms de répertoires. 
C    CTE_LENFIC2 : Longueur des noms de fichiers avec arborescence.
C    CTE_LENCOM  : Longueur des chaines de commande système.
C    CTE_NOT_DEFINED_VALUE_INT : Identifiant de valeur entière non définie
C                                par l'utilisateur.
C    CTE_NOT_DEFINED_VALUE_DBLE : Identifiant de valeur double non définie
C                                 par l'utilisateur.
C    CTE_DEFAULT_FICANGLES_RES_LUM : Nom par défaut du fichier de définition  
C			             des angles pour les calculs de luminance.
C    CTE_DEFAULT_FICANGLES_RES_MIE : Nom par défaut du fichier de définition 
C			             des angles pour les calculs de Mie.
C    CTE_DEFAULT_FICGRANU_AER : Nom par défaut du fichier de définition 
C			        de la matrice de phase des aérosols.
C    CTE_DEFAULT_FICGRANU_PHYTO : Nom par défaut du fichier de définition 
C			          de la matrice de phase du phytoplancton.
C    CTE_DEFAULT_FICGRANU_MLP : Nom par défaut du fichier de définition 
C			        de la matrice de phase des "Mineral-Like Particles".
C    CTE_DEFAULT_FICPROFIL_ATM_RES : Nom par défaut du fichier de profil atmosphérique.
C    CTE_DEFAULT_FICPROFIL_SEA_RES : Nom par défaut du fichier de profil marin.
C    CTE_DEFAULT_FICSOS_RES_BIN :    Nom par défaut du fichier résultat binaire de luminance
C                                    décomposée en séries de Fourier.
C    CTE_DEFAULT_FICSOS_RES_VS_VZA : Nom par défaut du fichier résultat de luminance 
C                                    en fonction de l'angle zénithal.
C    CTE_NB_WMO : Nombre de modèles WMO possibles.
C    CTE_NB_SF  : Nombre de modèles Shettle & Fenn possibles.
C    CTE_DEFAULT_IGMAX : Nombre d'interactions maximal par défaut 
C    CTE_DEFAULT_AER_JUNGE_RMAX : Valeur par défaut du rayon max d'un modèle de Junge 
C				  pour les aérosols.
C    CTE_DEFAULT_HYD_JUNGE_RMIN: Valeur par défaut du rayon min d'un modèle de Junge 
C				 pour les hydrosols.
C    CTE_DEFAULT_HYD_JUNGE_RMAX: Valeur par défaut du rayon max d'un modèle de Junge 
C 				 pour les hydrosols. 
C    CTE_WAMIN : Longueur d'onde minimale de simulation.    
C    CTE_FIC_BOT_SPECTRAL_DATA : Nom du fichier contenant les données spectrales de fond.  
C    CTE_NB_WA_FIC_BOT : Nombre de longueurs d'onde dans le fichier des données spectrales 
C                        de fond.
      
C Constantes spécifiques :
C    INCTE_IDLOG_MAIN : ID du ficher Trace principal
C    INCTE_PI : Valeur de PI (calculé par la machine)
C    INCTE_LENLINE  : Longueur maximale d'une ligne de données lue dans un fichier
C----------------------------------------------------------------------------
#include "OSOAA.h"
#define INCTE_IDLOG_MAIN 77
#define INCTE_PI DACOS(-1.D+00)
#define INCTE_LENLINE 1000

C==============================================================================
C PROGRAMME: OSOAA_MAIN
C ==========  
C
C      Programme principal du code OSOAA
C
C      Ce programme gère la lecture des paramètres de simulation :
C      --> passage des arguments par couples (mot clef, valeur)
C
C      Il gère les erreurs de type des valeurs fournies 
C      et assure des contrôles de cohérence des arguments.
C
C      Le programme assure l'enchainement des routines de simulations:
C
C	 - OSOAA_ANGLES : 
C          pour la définition des angles de propagation	du champ de luminance
C          et pour la définition des angles décrivant les fonctions de phase.
C
C        - OSOAA_AEROSOLS :
C          pour le calcul des paramètres radiatifs des aérosols 
C          (matrice de phase, coefficients de diffusion et d'extinction).
C          Le programme OSOAA_MAIN gère :
C             - le cas d'une AOT posée nulle ou non nulle par l'utilisateur.
C             - le cas d'une longueur d'onde de simulation (WA) du champ de luminance
C               différente de la longueur d'onde de référence de l'AOT (WAREF) 
C               en convertissant l'AOT à la longueur d'onde de simulation.			
C
C        - OSOAA_HYDROSOLS :
C          pour le calcul des paramètres radiatifs du phytoplancton 
C          et sédiments (Mineral Like-Particles) à longueur d'onde de 
C          simulation (WA) du champ de luminance
C          (matrice de phase, coefficients de diffusion et d'extinction).       
C          Le programme OSOAA_MAIN gère le passage des paramètres pour
C          une définition des hydrosols :
C             - par modèles (types prédéfinis).
C             - par mesures externes.
C
C        - OSOAA_PROFILE :
C          pour le calcul des profils d'épaisseurs optiques
C          dans l'atmosphère et dans la mer.
C
C        - OSOAA_SURFACE :
C          pour le calcul des matrices d'interface air/mer agitée
C          en réflexion et transmission.
C            
C        - OSOAA_SOS :
C          pour réaliser les calculs de simulation du champ de rayonnement
C          par ordre successif de diffusion et pour produire les fichiers
C          résultats finaux des traitements.
C
C      Un fichier trace est produit (si requis par l'utilisateur).
C      Il rappelle le paramétrage pour l'enchainement des traitements.
C
C
C
C
C Données en entrée du programme
C ------------------------------
C     -->Les paramètres/arguments du programme
C
C        Les arguments sont passés sous la forme d'une liste de couples (-Keyword Value)
C
C        Définition des mots clef, statut obligatoire / option, format de la valeur associée
C
C 	 ------- Arguments propres au programme principal	
C 	 ------------------------------------------------
C
C         -OSOAA.ResRoot ==> Obligatoire 
C             RESROOT   (CHARACTER*CTE_LENDIR) : répertoire de travail du traitement 
C						 OSOAA (chemin complet)
C 	
C         -OSOAA.Log  ==> Optionnel :  
C            FICMAIN_LOG (CHARACTER*CTE_LENFIC2) : Nom du fichier Trace du programme
C                                          	   principal (nom fichier + extension) 
C						   ==> Pas de trace si non défini.
C
C         -OSOAA.Wa ==> Obligatoire 
C	     PROG_ARG  (CHARACTER*10)  : Longueur d'onde de simulation de la luminance 
C                                        en microns.	
C                                       (format de décodage : libre --> Float)
C
C
C
C 	 ------- Arguments propres à la définition des angles		
C 	 ----------------------------------------------------		
C         -ANG.Rad.NbGauss  ==> définition par l'utilisateur optionnelle 
C                                 --> si non défini, on utilise par défaut 
C                                     la valeur CTE_DEFAULT_NBMU_LUM 
C            PROG_ARG (CHARACTER*10) : Nb d'angles de Gauss pour les calculs de luminance.
C                                          (format de décodage : libre --> Integer) 
C
C         -ANG.Rad.UserAngFile  ==> Optionnel : 
C             FICANGLES_USER_LUM (CHARACTER*CTE_LENFIC2) : 
C                                       Nom complet du fichier des angles "utilisateurs"
C                                       pour les calculs de luminance et BRDF
C                 			(répertoire + nom fichier + extension)
C
C         -ANG.Thetas ==> Obligatoire 
C 	      PROG_ARG (CHARACTER*10)  : Angle zénithal solaire en degrés (0 < tetas < 90).
C      		                         (angle complémentaire : mus= -cos(tetas))
C                             	         (format de décodage : libre --> Float)
C
C         -ANG.Rad.ResFile  ==> Définition par l'utilisateur optionnelle 
C                                 --> si non défini, on utilise par défaut 
C                                     la valeur CTE_DEFAULT_FICANGLES_RES_LUM
C             FICANGLES_RES_LUM (CHARACTER*CTE_LENFIC2) : 
C                                       Nom du fichier des angles utilisés 
C                                       pour les calculs de luminance et BRDF
C                                       (nom fichier + extension)
C
C         -ANG.Mie.NbGauss  ==> Définition par l'utilisateur optionnelle 
C                                 --> si non défini, on utilise par défaut 
C                                     la valeur CTE_DEFAULT_NBMU_MIE 
C             PROG_ARG (CHARACTER*10) : Nb d'angles de Gauss pour les calculs de fonctions 
C                                       de phase  (format de décodage : libre --> Integer)  
C
C         -ANG.Mie.UserAngFile  ==> Optionnel : 
C             FICANGLES_USER_MIE (CHARACTER*CTE_LENFIC2) : 
C                                    Nom complet du fichier des angles 
C                                    "utilisateurs" pour les calculs de fonctions de phase.
C                 		     répertoire + nom fichier + extension)
C
C         -ANG.Mie.ResFile  ==> Définition par l'utilisateur optionnelle 
C                                 --> si non défini, on utilise par défaut 
C                                     la valeur CTE_DEFAULT_FICANGLES_RES_MIE
C             FICANGLES_RES_MIE (CHARACTER*CTE_LENFIC2) :
C                                       Nom du fichier des angles utilisés 
C                                       pour les calculs de fonctions de phase.
C                 			(nom fichier + extension)
C
C         -ANG.Log ==> Optionnel
C             FICANGLES_LOG (CHARACTER*CTE_LENFIC2): 
C                                       Nom du fichier trace des calculs d'angles
C                                       (nom fichier + extension)
C                                       ==> Pas de trace si non défini.	 
C
C
C 	 ------- Arguments propres à la définition des modèles d'aérosols		
C 	 ----------------------------------------------------------------	
C     		       
C         -AER.AOTref ==> Obligatoire 
C      	     PROG_ARG (CHARACTER*10)   : Epaisseur optique des aérosols (non tronquée)
C                                        pour la longueur d'onde de référence.
C                                        (format de décodage : libre --> Float)
C      		       
C         -AER.Waref ==> Obligatoire  (pour cas général AER.AOTref > 0)
C	     PROG_ARG  (CHARACTER*10)  : Longueur d'onde de référence de l'épaisseur 
C                                        optique des aérosols en microns.	
C                                       (format de décodage : libre --> Float)
C
C         -AER.Tronca ==> Optionnel (pour cas général AER.AOTref > 0)  
C	     PROG_ARG (CHARACTER*10)   : Option de troncature de la fonction de phase
C					 des aérosols ==> 1 pour la réaliser.
C                                        (format de décodage : libre --> Integer)
C 
C      		       
C         -AER.Model ==> Obligatoire  (pour cas général AER.AOTref > 0)
C	     PROG_ARG (CHARACTER*10)  : Granulométrie mono-modal ou multi-modale.
C                                (format de décodage : libre --> Integer)
C      		        0 : Modèles mono-mode
C			1 : Modèles WMO
C			2 : Modèles Shettle & Fenn
C		        3 : Modèles bimodaux de LND
C                       4 : Utilisation de fonctions de phase externes
C
C	  
C         -AER.MMD.MRwa ==> Associé à -AER.Model 0
C            PROG_ARG   (CHARACTER*10)  : Partie réelle de l'indice de réfraction 
C                                         pour modèle aérosol mono-modal à la longueur
C                                         d'onde de simulation des luminances (-OSOAA.Wa)
C                                (format de décodage : libre --> Float)	
C
C         -AER.MMD.MIwa ==> Associé à -AER.Model 0
C            PROG_ARG   (CHARACTER*10)  : Partie imaginaire de l'indice de réfraction 
C                                         pour modèle mono-modal à la longueur 
C                                         d'onde de simulation des luminances (-OSOAA.Wa)
C                                (format de décodage : libre --> Float)					  
C	  
C         -AER.MMD.MRwaref ==> Associé à -AER.Model 0
C            PROG_ARG   (CHARACTER*10)  : Partie réelle de l'indice de réfraction 
C                                         pour modèle aérosol mono-modal à la longueur 
C                                         d'onde de référence de l'AOT (-AER.Waref)
C                                (format de décodage : libre --> Float)	
C
C         -AER.MMD.MIwaref ==> Associé à -AER.Model 0
C            PROG_ARG   (CHARACTER*10)  : Partie imaginaire de l'indice de réfraction 
C                                         pour modèle mono-modal à la longueur 
C                                         d'onde de simulation de l'AOT (-AER.Waref)
C                                (format de décodage : libre --> Float)
C	
C         -AER.MMD.SDtype ==> Associé à -AER.Model 0
C            PROG_ARG  (CHARACTER*4)   : Indice de type de granulométrie pour cas mono-modal
C                          1 : LND (paramètres : rayon modal en microns et  variance);  
C                          2 : Loi de Junge (paramètres : rayon minimal et maximal 
C                                            en microns, pente).
C				 (format de décodage : libre --> Integer)
C
C         -AER.MMD.LNDradius ==> Associé à -AER.Model 0 et -AER.MMD.SDtype 1
C            PROG_ARG  (CHARACTER*10)  : Rayon modal (microns) du modèle LND pour cas mono-modal
C                                        (format de décodage : libre --> Float)					
C	  
C         -AER.MMD.LNDvar ==> Associé à -AER.Model 0 et -AER.MMD.SDtype 1
C            PROG_ARG  (CHARACTER*10)  : Variance du modèle LND pour cas mono-modal
C                                       (format de décodage : libre --> Float)					
C
C         -AER.MMD.JD.slope ==> Associé à -AER.Model 0 et -AER.MMD.SDtype 2
C            PROG_ARG  (CHARACTER*10)  : Pente de la loi de Junge
C                                        (format de décodage : libre --> Float)
C
C         -AER.MMD.JD.rmin ==> Associé à -AER.Model 0 et -AER.MMD.SDtype 2
C            PROG_ARG  (CHARACTER*10)  : Rayon minimal de la loi de Junge (microns)
C                                        (format de décodage : libre --> Float)
C
C         -AER.MMD.JD.rmax ==> Définition par l'utilisateur optionnelle
C				(associé a -AER.Model 0 et -AER.MMD.SDtype 2 :
C                                 --> si non défini, on utilise par défaut 
C                                     la valeur CTE_DEFAULT_AER_JUNGE_RMAX)
C            PROG_ARG  (CHARACTER*10)  : Rayon maximal de la loi de Junge (microns)
C                                        (format de décodage : libre --> Float)	
C
C         -AER.WMO.Model ==> Associé à -AER.Model 1
C      	     PROG_ARG   (CHARACTER*10)  : Indice de choix du modèle WMO
C				 (format de décodage : libre --> Integer)
C      		        1 : Modèle WMO Continental
C			2 : Modèle WMO Maritime
C			3 : Modèle WMO Urban
C			4 : Modèle WMO "user definition"
C
C         -AER.WMO.DL ==> Associé à -AER.Model 1 et -AER.WMO.Model 4
C      	     PROG_ARG  (CHARACTER*10)  : Pourcentage volumique C des particules DL (WMO).
C				 (format de décodage : libre --> Float)
C
C         -AER.WMO.WS ==> Associé à -AER.Model 1 et -AER.WMO.Model 4
C      	     PROG_ARG  (CHARACTER*10)  : Pourcentage volumique C des particules WS (WMO).
C				 (format de décodage : libre --> Float)
C
C         -AER.WMO.OC ==> Associé à -AER.Model 1 et -AER.WMO.Model 4
C      	     PROG_ARG  (CHARACTER*10)  : Pourcentage volumique C des particules OC (WMO).
C				 (format de décodage : libre --> Float)
C
C         -AER.WMO.SO ==> Associé à -AER.Model 1 et -AER.WMO.Model 4
C      	     PROG_ARG  (CHARACTER*10)  : Pourcentage volumique C des particules SO (WMO).
C				 (format de décodage : libre --> Float)
C
C         -AER.SF.Model ==> Associé à -AER.Model 2 
C      	     PROG_ARG  (CHARACTER*10)  : Indice de choix du modèle Shettle & Fenn
C				 (format de décodage : libre --> Integer)
C      		        1 : Modèle SF Tropospheric
C			2 : Modèle SF Urban
C			3 : Modèle SF Maritime
C			4 : Modèle SF Coastal
C
C         -AER.SF.RH ==> Associé à -AER.Model 2
C      	     PROG_ARG  (CHARACTER*10)  : Humidité relative pour modèle Shettle & Fenn
C				(format de décodage : libre --> Float) 
C
C         -AER.BMD.VCdef ==> Associé à -AER.Model 3
C      	     PROG_ARG (CHARACTER*10) : Indice de type de description 
C                                      du mélange bimodal LND. 
C                               (format de décodage : libre --> Integer)
C      		        1 : Utilisation des concentrations volumiques de l'utilisateur
C			2 : Utilisation du rapport AOT_coarse / AOT_totale
C
C         -AER.BMD.CoarseVC ==> Associé à -AER.Model 3 et -AER.BMD.VCdef 1
C      	     PROG_ARG (CHARACTER*10) : Concentration volumique de l'utilisateur pour  
C                                      le mode "LND coarse" 
C                                      (format de décodage : libre --> Float)
C	 
C         -AER.BMD.FineVC ==> Associé à -AER.Model 3 et -AER.BMD.VCdef 1
C      	     PROG_ARG (CHARACTER*10) : Concentration volumique de l'utilisateur pour  
C                                      le mode "LND fine" 
C                                      (format de décodage : libre --> Float)
C	 
C         -AER.BMD.RAOT ==> Associé à -AER.Model 3 et -AER.BMD.VCdef 2
C      	     PROG_ARG (CHARACTER*10) : rapport AOT_coarse / AOT_tot pour la longueur 
C                                      d'onde WAREF (format de décodage : libre --> Float)	 
C	 
C         -AER.BMD.CM.MRwa ==> Associé à -AER.Model 3 
C      	     PROG_ARG (CHARACTER*10) : Partie réelle de l'indice de réfraction
C                                      des particules du mode "LND coarse" pour WA 
C                                      de simulation (format de décodage : libre --> Float)
C
C         -AER.BMD.CM.MIwa ==> Associé à -AER.Model 3 
C      	     PROG_ARG (CHARACTER*10) : Partie imaginaire de l'indice de réfraction
C                                      des particules du mode "LND coarse" pour WA 
C                                      de simulation (format de décodage : libre --> Float)
C
C         -AER.BMD.CM.MRwaref ==> Associé à -AER.Model 3 et -AER.BMD.VCdef 2 
C      	     PROG_ARG1 (CHARACTER*10) : Partie réelle de l'indice de réfraction
C                                       des particules du mode "LND coarse" pour WAREF
C                                      (format de décodage : libre --> Float)
C
C         -AER.BMD.CM.MIwaref ==> Associé à -AER.Model 3 et -AER.BMD.VCdef 2 
C      	     PROG_ARG (CHARACTER*10) : Partie imaginaire de l'indice de réfraction
C                                      des particules du mode "LND coarse" pour WAREF
C                                     (format de décodage : libre --> Float)
C
C         -AER.BMD.CM.SDradius ==> Associé à -AER.Model 3 
C      	     PROG_ARG (CHARACTER*10) : Rayon modal de la "LND coarse"
C                                     (format de décodage : libre --> Float)
C
C         -AER.BMD.CM.SDvar ==> Associé à -AER.Model 3 
C      	     PROG_ARG (CHARACTER*10) : Variance de la "LND coarse"
C                                     (format de décodage : libre --> Float)
C	 
C         -AER.BMD.FM.MRwa ==> Associé à -AER.Model 3 
C      	     PROG_ARG (CHARACTER*10) : Partie réelle de l'indice de réfraction
C                                      des particules du mode "LND fine" pour WA 
C                                      de simulation (format de décodage : libre --> Float)
C
C         -AER.BMD.FM.MIwa ==> Associé à -AER.Model 3 
C      	     PROG_ARG (CHARACTER*10) : Partie imaginaire de l'indice de réfraction
C                                      des particules du mode "LND fine" pour WA 
C                                      de simulation (format de décodage : libre --> Float)
C
C         -AER.BMD.FM.MRwaref ==> Associé à -AER.Model 3 et -AER.BMD.VCdef 2
C      	     PROG_ARG (CHARACTER*10) : Partie réelle de l'indice de réfraction
C                                      des particules du mode "LND fine" pour WAREF
C                                     (format de décodage : libre --> Float)
C
C         -AER.BMD.FM.MIwaref ==> Associé à -AER.Model 3 et -AER.BMD.VCdef 2
C      	     PROG_ARG (CHARACTER*10) : Partie imaginaire de l'indice de réfraction
C                                      des particules du mode "LND fine" pour WAREF
C                                     (format de décodage : libre --> Float)
C
C         -AER.BMD.FM.SDradius ==> Associé à -AER.Model 3 
C      	     PROG_ARG (CHARACTER*10) : Rayon modal de la "LND fine"
C                                     (format de décodage : libre --> Float)
C
C         -AER.BMD.FM.SDvar ==> Associé à -AER.Model 3 
C      	     PROG_ARG (CHARACTER*10) : Variance de la "LND fine"
C                                     (format de décodage : libre --> Float)
C 
C         -AER.ExtData ==> Associé à -AER.Model 4 
C            FICEXTDATA_AER  (CHARACTER*CTE_LENFIC2) : 
C                     Nom du fichier de données externes de fonctions de phase
C                     et paramètres radiatifs (coefficients extinction et diffusion) 
C                    (répertoire + nom fichier + extension) 
C
C         -AER.DirMie ==> Obligatoire   (pour cas général AER.AOTref > 0)
C             DIRMIE_AER   (CHARACTER*CTE_LENDIR)  : répertoire de stockage des fichiers MIE
C
C         -AER.MieLog ==> Optionnel
C            FICAER_LOG_MIE (CHARACTER*CTE_LENFIC2) : Fichier trace des calculs de MIE 
C                 				     (nom fichier + extension)
C                                                     ==> Pas de trace si non défini.
C	
C         -AER.ResFile ==> Définition par l'utilisateur optionnelle 
C                                 -->  si non défini, on utilise par défaut 
C                                      la valeur CTE_DEFAULT_FICGRANU_AER
C            FICGRANU_AER  (CHARACTER*CTE_LENFIC2) :
C                     Nom du fichier résultat des calculs Aérosols
C                     --> coef de fonctions de phase et paramètres radiatifs
C                        (coefficients d'extinction et de diffusion)
C  			 (nom fichier + extension)                        
C
C         -AER.Log ==> Optionnel
C             FICAER_LOG (CHARACTER*CTE_LENFIC2): Nom du fichier trace des calculs 
C					          de matrice de phase des aérosols
C                                            	  (nom fichier + extension)
C                                            	  ==> Pas de trace si non défini.	 
C    
C
C 	 ------- Arguments propres à la définition des modèles de hydrosols		
C 	 ------------------------------------------------------------------	
C
C         -PHYTO.ResFile ==> Définition par l'utilisateur optionnelle 
C                            (pour cas général PHYTO.Chl > 0) :
C                                 -->  si non défini, on utilise par défaut  
C                                      la valeur CTE_DEFAULT_FICGRANU_PHYTO
C            FICGRANU_PHYTO (CHARACTER*CTE_LENFIC2) : 
C                     Nom du fichier résultat des calculs Hydrosols pour le phytoplancton
C                     --> coef de fonctions de phase et paramètres radiatifs
C                        (coefficients extinction et diffusion)
C                    (répertoire + nom fichier + extension)   
C				NB : contient les résultats du milieu diffusant global
C                                    si on utilise une mesure de fonction de phase 
C				     (donnée utilisateur globale)            
C
C         -MLP.ResFile ==> Définition par l'utilisateur optionnelle 
C                          (pour cas général SED.Csed > 0) :
C                                 -->  si non défini, on utilise par défaut 
C                                      la valeur CTE_DEFAULT_FICGRANU_MLP
C            FICGRANU_MLP (CHARACTER*CTE_LENFIC2) : 
C                     Nom du fichier résultat des calculs Hydrosols pour les sédiments
C                     --> coef de fonctions de phase et paramètres radiatifs
C                        (coefficients extinction et diffusion)
C                    (répertoire + nom fichier + extension)  
C				NB : mise à zéro des paramètres radiatifs
C                                    si on utilise une mesure de fonction de phase 
C				     (donnée utilisateur globale)                
C         -HYD.Log ==> Optionnel
C             FICHYD_LOG (CHARACTER*CTE_LENFIC2): Nom du fichier trace des calculs 
C					          de matrice de phase des hydrosols
C                                            	  (nom fichier + extension)
C                                            	   ==> Pas de trace si non défini.	 
C
C         -HYD.DirMie ==> Obligatoire   (pour cas général PHYTO.Chl ou SED.Csed > 0)
C             DIRMIE_HYD   (CHARACTER*CTE_LENDIR)  : répertoire de stockage des fichiers MIE
C
C         -HYD.MieLog ==> Optionnel
C            FICHYD_LOG_MIE (CHARACTER*CTE_LENFIC2) : Fichier trace des calculs de MIE 
C                 				     (nom fichier + extension)	
C      		       
C         -HYD.Model ==> Obligatoire  (pour cas général PHYTO.Chl ou SED.Csed > 0)
C	     PROG_ARG (CHARACTER*10)  : Type de caractérisation des hydrosols.
C                                (format de décodage : libre --> Integer)
C			1 : Par modélisation de granulométries
C			2 : Utilisation de fonctions de phase externes
C     		       
C         -PHYTO.JD.slope ==> Obligatoire  (pour cas général PHYTO.Chl > 0)
C	     PROG_ARG (CHARACTER*10)  : Pente de la distribution de Junge pour le 
C                                       phytoplancton (format de décodage : libre --> Float)
C    		       
C         -PHYTO.JD.rmin ==> Définition par l'utilisateur optionnelle   
C                            (pour cas général PHYTO.Chl > 0)
C                                 --> si non défini, on utilise par défaut 
C                                     la valeur CTE_DEFAULT_HYD_JUNGE_RMIN) 
C	     PROG_ARG (CHARACTER*10)  : Rayon min de la distribution de Junge pour le   
C                                       phytoplancton (format de décodage : libre --> Float)                                    
C    		       
C         -PHYTO.JD.rmax ==> Définition par l'utilisateur optionnelle
C                            (pour cas général PHYTO.Chl > 0)
C                                 --> si non défini, on utilise par défaut 
C                                     la valeur CTE_DEFAULT_HYD_JUNGE_RMAX)
C	     PROG_ARG (CHARACTER*10)  : Rayon max de la distribution de Junge pour le 
C                                       phytoplancton (format de décodage : libre --> Float)
C    		       
C         -PHYTO.JD.MRwa ==> Obligatoire  (pour cas général PHYTO.Chl > 0)
C	     PROG_ARG (CHARACTER*10)  : Partie réelle de l'indice de réfraction 
C					de la distribution de Junge pour le phytoplancton
C                                      (format de décodage : libre --> Float)
C      
C         -PHYTO.JD.MIwa ==> Obligatoire  (pour cas général PHYTO.Chl > 0)
C	     PROG_ARG (CHARACTER*10)  : Partie imaginaire de l'indice de réfraction 
C					de la distribution de Junge pour le phytoplancton
C                                      (format de décodage : libre --> Float)
C    		       
C         -PHYTO.JD.rate ==> Obligatoire  (pour cas général PHYTO.Chl > 0)
C	     PROG_ARG (CHARACTER*10)  : Proportion de la distribution de Junge pour le 
C                                       phytoplancton (format de décodage : libre --> Float)
C			En proportion des particules du mode de la distribution de Junge
C			par rapport au nombre total de particules incluant tous les modes
C			des particules de phytoplancton.                                      
C  
C         -PHYTO.LND.SM.SDradius ==> Optionnel  (pour cas général PHYTO.Chl > 0)
C				 La définition d'un des paramètres du modèle PHYTO.LND.SM.*
C				 demande à définir tous les autres paramètres de ce modèle.
C	     PROG_ARG (CHARACTER*10)  : Rayon modal de la distribution LND secondaire pour
C                                     le phytoplancton (format de décodage: libre --> Float)
C    		       
C         -PHYTO.LND.SM.SDvar ==> Optionnel  (pour cas général PHYTO.Chl > 0)
C			          La définition d'un des paramètres du modèle PHYTO.LND.SM.*
C			          demande à définir tous les autres paramètres de ce modèle.
C	     PROG_ARG (CHARACTER*10)  : Variance de la distribution LND secondaire pour le 
C                                       phytoplancton (format de décodage : libre --> Float)
C    		       
C         -PHYTO.LND.SM.MRwa ==> Optionnel  (pour cas général PHYTO.Chl > 0)
C				 La définition d'un des paramètres du modèle PHYTO.LND.SM.*
C				 demande à définir tous les autres paramètres de ce modèle.
C	     PROG_ARG (CHARACTER*10)  : Partie réelle de l'indice de réfraction de la 
C					distribution LND secondaire pour le phytoplancton 
C                                       (format de décodage : libre --> Float)
C
C         -PHYTO.LND.SM.MIwa ==> Optionnel (pour cas général PHYTO.Chl > 0)
C				 La définition d'un des paramètres du modèle PHYTO.LND.SM.*
C				 demande à définir tous les autres paramètres de ce modèle.
C	     PROG_ARG (CHARACTER*10)  : Partie imaginaire de l'indice de réfraction de la 
C					distribution LND secondaire pour le phytoplancton 
C                                       (format de décodage : libre --> Float)
C    		       
C         -PHYTO.LND.SM.rate ==> Optionnel  (pour cas général PHYTO.Chl > 0)
C				 La définition d'un des paramètres du modèle PHYTO.LND.SM.*
C				 demande à définir tous les autres paramètres de ce modèle.
C	     PROG_ARG (CHARACTER*10)  : Proportion de la distribution LND secondaire 
C                                       pour le phytoplancton 
C					(format de décodage : libre --> Float).
C			En proportion des particules du mode de la distribution de Junge
C			par rapport au nombre total de particules incluant tous les modes
C			des particules de phytoplancton.                              
C  
C         -PHYTO.LND.TM.SDradius ==> Optionnel  (pour cas général PHYTO.Chl > 0)
C				 La définition d'un des paramètres du modèle PHYTO.LND.TM.*
C				 demande à définir tous les autres paramètres de ce modèle.
C	     PROG_ARG (CHARACTER*10)  : Rayon modal de la distribution LND tertiaire 
C                                       pour le phytoplancton 
C					(format de décodage : libre --> Float)
C    		       
C         -PHYTO.LND.TM.SDvar ==> Optionnel  (pour cas général PHYTO.Chl > 0)
C				  La définition d'un des paramètres du modèle PHYTO.LND.TM.*
C				  demande à définir tous les autres paramètres de ce modèle.
C	     PROG_ARG (CHARACTER*10)  : Variance de la distribution LND tertiaire 
C                                       pour le phytoplancton 
C					(format de décodage : libre --> Float)
C    		       
C         -PHYTO.LND.TM.MRwa ==> Optionnel  (pour cas général PHYTO.Chl > 0)
C				 La définition d'un des paramètres du modèle PHYTO.LND.TM.*
C				 demande à définir tous les autres paramètres de ce modèle.
C	     PROG_ARG (CHARACTER*10)  : Partie réelle de l'indice de réfraction de la 
C			                distribution LND tertiaire pour le phytoplancton 
C                                       (format de décodage : libre --> Float)
C
C         -PHYTO.LND.TM.MIwa ==> Optionnel  (pour cas général PHYTO.Chl > 0)
C				 La définition d'un des paramètres du modèle PHYTO.LND.TM.*
C				 demande à définir tous les autres paramètres de ce modèle.
C	     PROG_ARG (CHARACTER*10)  : Partie imaginaire de l'indice de réfraction de la 
C					distribution LND tertiaire pour le phytoplancton 
C                                       (format de décodage : libre --> Float)
C    		       
C         -PHYTO.LND.TM.rate ==> Optionnel  (pour cas général PHYTO.Chl > 0)
C				 La définition d'un des paramètres du modèle PHYTO.LND.TM.*
C				 demande à définir tous les autres paramètres de ce modèle.
C	     PROG_ARG (CHARACTER*10)  : Proportion de la distribution LND tertiaire 
C                                       pour le phytoplancton 
C					(format de décodage : libre --> Float).
C			En proportion des particules du mode de la distribution de Junge
C			par rapport au nombre total de particules incluant tous les modes
C			des particules de phytoplancton.                          
C     		       
C         -SED.JD.slope ==> Obligatoire  (pour cas général SED.Csed > 0)
C	     PROG_ARG (CHARACTER*10)  : Pente de la distribution de Junge pour les sédiments
C                                       (format de décodage : libre --> Float)
C    		       
C         -SED.JD.rmin ==> Définition par l'utilisateur optionnelle 
C                          (pour cas général SED.Csed > 0)
C                                 --> si non défini, on utilise par défaut 
C                                     la valeur CTE_DEFAULT_HYD_JUNGE_RMIN 
C	     PROG_ARG (CHARACTER*10)  : Rayon min de la distribution de Junge pour les 
C                                       sédiments (format de décodage : libre --> Float)
C
C    		       
C         -SED.JD.rmax ==> Définition par l'utilisateur optionnelle   
C			   (pour cas général SED.Csed > 0)
C                                 --> si non défini, on utilise par défaut 
C                                     la valeur CTE_DEFAULT_HYD_JUNGE_RMAX 
C	     PROG_ARG (CHARACTER*10)  : Rayon max de la distribution de Junge pour les 
C                                       sédiments (format de décodage : libre --> Float)
C    		       
C         -SED.JD.MRwa ==> Obligatoire  (pour cas général SED.Csed > 0)
C	     PROG_ARG (CHARACTER*10)  : Partie réelle de l'indice de réfraction 
C					de la distribution de Junge pour les sédiments
C                                       (format de décodage : libre --> Float)
C
C         -SED.JD.MIwa ==> Obligatoire  (pour cas général SED.Csed > 0)
C	     PROG_ARG (CHARACTER*10)  : Partie imaginaire de l'indice de réfraction 
C					de la distribution de Junge pour les sédiments
C                                       (format de décodage : libre --> Float)
C    		        
C         -SED.JD.rate ==> Obligatoire  (pour cas général SED.Csed > 0)
C	     PROG_ARG (CHARACTER*10)  : Proportion de la distribution de Junge pour les 
C				        sédiments (format de décodage : libre --> Float)
C			En proportion des particules du mode de la distribution de Junge
C			par rapport au nombre total de particules incluant tous les modes
C			des particules de sédiments.
C                                
C         -SED.LND.SM.SDradius ==> Optionnel  (pour cas général SED.Csed > 0)
C				   La définition d'un des paramètres du modèle SED.LND.SM.*
C				   demande à définir tous les autres paramètres de ce modèle
C	     PROG_ARG (CHARACTER*10)  : Rayon modal de la distribution LND secondaire pour  
C                                       les sédiments (format de décodage : libre --> Float)
C    		       
C         -SED.LND.SM.SDvar ==> Optionnel  (pour cas général SED.Csed > 0)
C			        La définition d'un des paramètres du modèle SED.LND.SM.*
C				demande à définir tous les autres paramètres de ce modèle.
C	     PROG_ARG (CHARACTER*10)  : Variance de la distribution LND secondaire pour les
C                                       sédiments (format de décodage : libre --> Float)
C    		       
C         -SED.LND.SM.MRwa ==> Optionnel  (pour cas général SED.Csed > 0)
C			       La définition d'un des paramètres du modèle SED.LND.SM.*
C			       demande à définir tous les autres paramètres de ce modèle.
C	     PROG_ARG (CHARACTER*10)  : Partie réelle de l'indice de réfraction 
C					de la distribution LND secondaire pour les sédiments
C                                	(format de décodage : libre --> Float)
C
C         -SED.LND.SM.MIwa ==> Optionnel  (pour cas général SED.Csed > 0)
C			       La définition d'un des paramètres du modèle SED.LND.SM.*
C			       demande à définir tous les autres paramètres de ce modèle.
C	     PROG_ARG (CHARACTER*10)  : Partie imaginaire de l'indice de réfraction 
C					de la distribution LND secondaire pour les sédiments
C                                	(format de décodage : libre --> Float)
C    		       
C         -SED.LND.SM.rate ==> Optionnel  (pour cas général SED.Csed > 0)
C			       La définition d'un des paramètres du modèle SED.LND.SM.*
C			       demande à définir tous les autres paramètres de ce modèle.
C	     PROG_ARG (CHARACTER*10)  : Proportion de la distribution LND secondaire pour
C				        les sédiments (format de décodage : libre --> Float)
C			En proportion des particules du mode de la distribution de Junge
C			par rapport au nombre total de particules incluant tous les modes
C			des particules de sédiments.
C                                
C         -SED.LND.TM.SDradius ==> Optionnel  (pour cas général SED.Csed > 0)
C				   La définition d'un des paramètres du modèle SED.LND.TM.*
C				   demande à définir tous les autres paramètres de ce modèle
C	     PROG_ARG (CHARACTER*10)  : Rayon modal de la distribution LND tertiaire pour 
C					les sédiments (format de décodage : libre --> Float)
C    		       
C         -SED.LND.TM.SDvar ==> Optionnel  (pour cas général SED.Csed > 0)
C				La définition d'un des paramètres du modèle SED.LND.TM.*
C				demande à définir tous les autres paramètres de ce modèle.
C	     PROG_ARG (CHARACTER*10)  : Variance de la distribution LND tertiaire pour les
C					sédiments (format de décodage : libre --> Float)
C    		       
C         -SED.LND.TM.MRwa ==> Optionnel  (pour cas général SED.Csed > 0)
C			       La définition d'un des paramètres du modèle SED.LND.TM.*
C			       demande à définir tous les autres paramètres de ce modèle.
C	     PROG_ARG (CHARACTER*10)  : Partie réelle de l'indice de réfraction 
C					de la distribution LND tertiaire pour les sédiments
C                                	(format de décodage : libre --> Float)
C
C         -SED.LND.TM.MIwa ==> Optionnel  (pour cas général SED.Csed > 0)
C			       La définition d'un des paramètres du modèle SED.LND.TM.*
C			       demande à définir tous les autres paramètres de ce modèle.
C	     PROG_ARG (CHARACTER*10)  : Partie imaginaire de l'indice de réfraction 
C					de la distribution LND tertiaire pour les sédiments
C                                	(format de décodage : libre --> Float)
C    		       
C         -SED.LND.TM.rate ==> Optionnel  (pour cas général SED.Csed > 0)
C			       La définition d'un des paramètres du modèle SED.LND.TM.*
C			       demande à définir tous les autres paramètres de ce modèle.
C	     PROG_ARG (CHARACTER*10)  : Proportion de la distribution LND tertiaire pour 
C					les sédiments (format de décodage : libre --> Float)
C			En proportion des particules du mode de la distribution de Junge
C			par rapport au nombre total de particules incluant tous les modes
C			des particules de sédiments.                            
C  	  
C         -HYD.ExtData ==> Associé à -HYD.Model 2 
C            FICEXTDATA_HYD  (CHARACTER*CTE_LENFIC2) : 
C                     Nom du fichier de données externes de fonctions de phase
C                     et paramètres radiatifs (coefficients extinction et diffusion) 
C 		      pour la diffusion hydrosols globale dans la mer : phyto + sédiments.
C                     Données applicables à la longueur d'onde de simulation.
C                    (répertoire + nom fichier + extension) 
C
C
C 	 ------- Arguments propres à la définition des profils		
C 	 ------------------------------------------------------------------	
C
C         -AP.MOT ==> Optionnel   (AP.MOT ou AP.Pressure requis)
C      	     PROG_ARG   (CHARACTER*10)  : Epaisseur optique rayleigh pour la longueur 
C                                         d'onde de simulation de la luminance.
C                                	  (format de décodage : libre --> Float)
C	
C         -AP.Pressure ==> Optionnel   (AP.MOT ou AP.Pressure requis)
C      	     PROG_ARG   (CHARACTER*10)  : Pression atmosphérique au niveau de la mer (mbar).
C                                	  (format de décodage : libre --> Float)
C	              
C         -AP.HR ==> Optionnel : Associé à AP.MOT >= 0.0001 FICEXTDATA  
C      	     PROG_ARG   (CHARACTER*10)  : Echelle de hauteur moléculaire (km).
C                                	  (format de décodage : libre --> Float)
C	       	       
C         -AP.HA ==> Optionnel : à définir uniquement si AOT >= 0.0001
C      	     PROG_ARG   (CHARACTER*10)  : Echelle de hauteur du profil d'aérosols (km).
C                                	  (format de décodage : libre --> Float)
C	       	       
C         -SEA.Depth ==> Optionnel : Si non définie, on estime la profondeur euphotique
C                                    à partir des données tabulées de Morel en relation
C                                    avec la concentration de phytoplancton en surface.
C      	     PROG_ARG   (CHARACTER*10)  : Profondeur de la mer (m).
C                                	  (format de décodage : libre --> Float)
C
C	       	       
C         -PHYTO.Chl ==> Obligatoire 
C      	     PROG_ARG   (CHARACTER*10)  : Concentration de chlorophylle en surface (mg/m3)
C                                	 (format de décodage : libre --> Float)
C
C
C         -PHYTO.ProfilType ==> Optionnel : Associé à CHL > 0.
C      	     PROG_ARG   (CHARACTER*10)  : Indice de type de profil du phytoplancton
C                                	  (format de décodage : libre --> Integer)
C		Possibilités :	1 : Profil homogène
C				2 : Profil gaussien
C				3 : Profil de l'utilisateur
C	       	       
C         -PHYTO.GP.Chlbg ==> Optionnel : Associé à -PHYTO.ProfilType = 2. 
C      	     PROG_ARG   (CHARACTER*10)  : Constante de biomasse (mg/m3)
C 					  du profil gaussien de chlorophylle
C                                	 (format de décodage : libre --> Float)
C	       	       
C         -PHYTO.GP.Deep ==> Optionnel : Associé à -PHYTO.ProfilType = 2. 
C      	     PROG_ARG   (CHARACTER*10)  : Profondeur maximale du profil gaussien 
C					  de la chlorophylle (mètres)
C                                	 (format de décodage : libre --> Float)
C	       	       
C         -PHYTO.GP.Width ==> Optionnel : Associé à -PHYTO.ProfilType = 2. 
C      	     PROG_ARG   (CHARACTER*10)  : largeur du pic (mètres) du profil gaussien de 
C                                	  chlorophylle (format de décodage: libre --> Float)
C       	       
C         -PHYTO.Userfile ==> Optionnel : Associé à -PHYTO.ProfilType = 3. 
C      	     PROG_ARG   (CHARACTER*CTE_LENFIC2)  : Nom complet du fichier utilisateur   
C					           donnant le profil de chlorophylle
C                                                  (répertoire + nom fichier + extension) 
C	       	       
C         -SED.Csed ==> Obligatoire 
C      	     PROG_ARG   (CHARACTER*10)  : Concentration des sédiments en surface (mg/litre)
C                                	 (format de décodage : libre --> Float)
C	       	       
C         -YS.Abs440 ==> Obligatoire 
C      	     PROG_ARG   (CHARACTER*10)  : Coeff d'absorption de la substance jaune  
C                                	  à 440 nm (en m-1)
C                                         (format de décodage : libre --> Float)
C
C         -YS.Swa ==> Définition par l'utilisateur optionnelle 
C                        --> si non défini, on utilise par défaut 
C                            la valeur CTE_DEFAULT_SPECTRAL_YS
C      	     PROG_ARG   (CHARACTER*10)  : Coeff pour la conversion de l'absorption de la 
C					  substance jaune à 440 nm vers la longueur d'onde 
C                                	  de simulation 
C        				  (format de décodage : libre --> Float)
C
C         -DET.Abs440 ==> Obligatoire 
C      	     PROG_ARG   (CHARACTER*10)  : Coeff d'absorption des détritus à 440 nm (m-1)
C                                	 (format de décodage : libre --> Float)
C
C         -DET.Swa ==> Définition par l'utilisateur optionnelle 
C                        --> si non défini, on utilise par défaut 
C			     la valeur CTE_DEFAULT_SPECTRAL_DET
C      	     PROG_ARG   (CHARACTER*10)  : Coeff pour la conversion de l'absorption des 
C					  détritus à 440 nm vers la longueur d'onde 
C                                	  de simulation 
C					  (format de décodage : libre --> Float)
C
C         -PROFILE_ATM.ResFile  ==> Définition par l'utilisateur optionnelle 
C                                   --> si non défini, on utilise par défaut 
C                                       la valeur CTE_DEFAULT_FICPROFIL_ATM_RES
C             FICPROFIL_ATM_RES (CHARACTER*CTE_LENFIC2) : 
C				Nom du fichier de profil atmosphérique 
C                               utilisé pour les calculs de luminance 
C                               (nom fichier + extension)
C
C         -PROFILE_SEA.ResFile  ==> Définition par l'utilisateur optionnelle 
C                                   --> si non défini, on utilise par défaut 
C                                       la valeur CTE_DEFAULT_FICPROFIL_SEA_RES  
C             FICPROFIL_SEA_RES (CHARACTER*CTE_LENFIC2) : 
C				Nom du fichier de profil marin 
C                               utilisé pour les calculs de luminance 
C                               (nom fichier + extension)
C
C         -PROFILE.Log ==> Optionnel
C             FICPROFIL_LOG (CHARACTER*CTE_LENFIC2): Nom du fichier trace des 
C                                            	     calculs de profils
C						     (nom fichier + extension)
C                                            	     ==> Pas de trace si non défini.
C
C	 
C 	 ------- Arguments propres à la définition de l'interface air / mer		
C 	 ------------------------------------------------------------------	
C
C         -SEA.Dir ==> Obligatoire
C             DIRSEA   (CHARACTER*CTE_LENDIR)  : Répertoire de stockage des fichiers SURFACE
C
C         -SEA.Log  ==> Optionnel :  
C            FICSURF_LOG (CHARACTER*CTE_LENFIC2) : Nom du fichier Trace des 
C						   calculs de matrices d'interface air / mer
C                                          	   (nom fichier + extension) 
C                                            	   ==> Pas de trace si non défini.	 
C         -SEA.Ind ==> Obligatoire 
C      	     PROG_ARG   (CHARACTER*10)  : Indice de réfraction de la mer (air = 1).
C                             		 (format de décodage : libre --> Float)
C
C         -SEA.Wind ==> Obligatoire 
C      	     PROG_ARG   (CHARACTER*10)  : Vitesse du vent m/s.
C                             		 (format de décodage : libre --> Float)
C
C
C
C 	 ------- Arguments propres aux calculs d'Ordres Successifs 
C 	 ------- et à la production des fichiers résultats
C 	 ---------------------------------------------------------	
C
C         -SEA.SurfAlb ==> Obligatoire 
C      	     PROG_ARG   (CHARACTER*10)  : Albédo de surface à la longueur d'onde
C					  de simulation des luminances.
C                             		 (format de décodage : libre --> Float)
C
C         -SEA.BotType ==> Obligatoire 
C      	     PROG_ARG   (CHARACTER*10)  : Type de définition de l'albédo de fond marin.
C                             		 (format de décodage : libre --> Integer)
C			1 : Lambertian value	(user data -SEA.BotAlb)
C			2 : Light sand		(tabulated data)
C			3 : Green algua		(tabulated data)
C			4 : Brown algae		(tabulated data)
C			5 : Red algae		(tabulated data)
C    
C         -SEA.BotAlb ==> Optionnel : Associé à -SEA.botType = 1.  #MOD: 16/05/2018
C      	     PROG_ARG   (CHARACTER*10)  : Albédo de fond à la longueur d'onde
C					  de simulation des luminances.
C                             		 (format de décodage : libre --> Float)
C
C         -OSOAA.View.Phi ==> Obligatoire 
C      	     PROG_ARG   (CHARACTER*10)  : Azimut relatif (degrés).
C                             		 (format de décodage : libre --> Float)
C
C         -OSOAA.View.Level ==> Obligatoire 
C      	     PROG_ARG   (CHARACTER*10)  : Choix du niveau de sortie des luminances
C                             		 (format de décodage : libre --> Integer)
C			1 : Top of Atmosphere
C			2 : Sea Bottom
C			3 : Sea Surface 0+
C			4 : Sea Surface 0-
C			5 : User's definition of altitude or depth 
C				(user data -OSOAA.View.Z)
C
C         -OSOAA.View.Z ==> Optionnel : Associé à -OSOAA.View.Level = 5. 
C      	     PROG_ARG   (CHARACTER*10)  : Altitude ou profondeur (mètres)
C					  pour la représentation du champ de 
C                                         luminance en fonction de l'angle zénithal
C                                         pour l'azimut Phi fixé.
C                             		 (format de décodage : libre --> Float)
C
C         -OSOAA.ResFile.vsVZA  ==> Définition par l'utilisateur optionnelle 
C                                 -->  si non défini, on utilise par défaut 
C                                      la valeur CTE_DEFAULT_FICSOS_RES_VS_VZA    
C             FICSOS_RES_VS_VZA (CHARACTER*CTE_LENFIC2) : 
C				Nom du fichier résultat de luminance
C                               en fonction de l'angle zénithal de visée, 
C                               pour une profondeur (ou altitude) fixée
C				et pour l'azimut Phi fixé.
C                               (nom fichier + extension)
C
C         -OSOAA.View.VZA ==> Optionnel (si défini demande à définir aussi -OSOAA.ResFile.vsZ)
C      	     PROG_ARG   (CHARACTER*10)  : Angle zénithal de visée (degrés)
C					  pour la représentation du champ de 
C                                         luminance en fonction de la profondeur  
C					  (ou altitude) pour l'azimut Phi fixé.
C                             		  (format de décodage : libre --> Float)
C
C         -OSOAA.ResFile.vsZ  ==> Optionnel  (si défini demande à définir aussi -OSOAA.View.VZA)
C             FICSOS_RES_VS_Z (CHARACTER*CTE_LENFIC2) : 
C				Nom du fichier résultat de luminance 
C                               en fonction de la profondeur, 
C                               pour un angle zénithal de visée fixé
C				et pour l'azimut Phi fixé.
C                               (nom fichier + extension)
C
C         -OSOAA.ResFile.Adv.Up  ==> Optionnel  
C             FICSOS_RES_ADV_UP (CHARACTER*CTE_LENFIC2) : 
C				Nom du fichier résultats avancés de luminance montante
C                               en fonction de la profondeur (ou altitude), 
C                               ET de l'angle zénithal de visée,
C				pour l'azimut Phi fixé.
C                               (nom fichier + extension)
C                               ==> Pas produit si non défini.
C
C         -OSOAA.ResFile.Adv.Down  ==> Optionnel  
C             FICSOS_RES_ADV_DOWN (CHARACTER*CTE_LENFIC2) : 
C				Nom du fichier résultats avancés de luminance descendante
C                               en fonction de la profondeur (ou altitude), 
C                               ET de l'angle zénithal de visée,
C				pour l'azimut Phi fixé.
C                               (nom fichier + extension)
C                               ==> Pas produit si non défini.	 
C
C         -SOS.Log ==> Optionnel
C             FICSOS_LOG (CHARACTER*CTE_LENFIC2): Nom du fichier trace des 
C                                            	  calculs de luminance par OSOAA_SOS
C						  (nom fichier + extension)
C                                            	  ==> Pas de trace si non défini.
C
C         -SOS.IGmax ==> Optionnel : si non défini, on utilise par défaut 
C				     la valeur CTE_DEFAULT_IGMAX
C      	     PROG_ARG   (CHARACTER*10)  : Ordre maximal d'interaction.
C                             		 (format de décodage : libre --> Integer)
C	
C         -SOS.ResFile.Bin ==> Définition par l'utilisateur optionnelle 
C                                 --> si non défini, on utilise par défaut 
C                                     la valeur CTE_DEFAULT_FICSOS_RES_BIN   
C             FICSOS_RES_BIN (CHARACTER*CTE_LENFIC2) : Nom du fichier résultat 
C						       binaire en sortie de OSOAA_SOS.
C                               		      (nom fichier + extension)
C  	
C       
C Résultats fournis par le programme
C ----------------------------------     
C
C   Les résultats sont rangés sous le répertoire de travail défini par l'utilisateur
C   via le mot clef -OSOAA.ResRoot (répertoire RESROOT).
C
C   L'espace de travail se décline en deux sous-répertoires, générés par le code:
C
C     1) RESROOT/Standard_outputs : qui contient les fichiers de résultats standards.
C        ------------------------
C         - Fichier du champ de luminance montante en fonction de l'angle zénithal de visée
C           pour une valeur fixée de l'azimut relatif et du niveau du profil.
C           (-OSOAA.ResFile.vsVZA ==>  FICSOS_RES_VS_VZA)  
C
C         - Fichier du champ de luminance marine en fonction de la profondeur 
C           pour une valeur fixée de l'azimut relatif 
C           et pour une direction de propagation fixée (montante ou descendante).
C           (-OSOAA.ResFile.vsZ ==>  FICSOS_RES_VS_Z)        
C
C
C     2) RESROOT/Advanced_outputs : qui contient les fichiers de résultats avancés, 
C 	 ------------------------   dédiés à un utilisateur expert.
C
C
C         Fichiers traces : - Fichier Trace du programme principal si requis
C         ---------------     (-OSOAA.Log ==> FICMAIN_LOG)
C			      Identifiant = IDLOG = INCTE_IDLOG_MAIN
C
C			    - Fichier Trace, si requis, de la routine OSOAA_ANGLES 
C			      assurant la génération des fichiers d'angles pour 
C                             1) le calcul des propriétés radiatives des diffusants
C                             2) le calcul de la propagation du rayonnement dans
C                                l'atmosphère et dans la mer			    
C                             (-ANG.Log ==> FICANGLES_LOG)
C
C			    - Fichier Trace, si requis, de la routine OSOAA_AEROSOLS 
C			      assurant la génération des fichiers de propriétés 
C                             radiatives des aérosols
C			      (-AER.Log ==> FICAER_LOG)
C			      et, si requis, de la génération des fichiers de MIE
C			      par la sous routine OSOAA_MIE
C			      (-AER.MieLog ==> FICAER_LOG_MIE)
C
C			    - Fichier Trace, si requis, de la routine OSOAA_HYDROSOLS 
C			      assurant la génération des fichiers de propriétés 
C                             radiatives des hydrosols
C			      (-HYD.Log ==> FICHYD_LOG)    
C			      et, si requis, de la génération des fichiers de MIE
C			      par la sous routine OSOAA_MIE
C			      (-HYD.MieLog ==> FICHYD_LOG_MIE)
C
C			    - Fichier Trace, si requis, de la routine OSOAA_PROFILE
C			      qui génère les fichiers de profil d'épaisseur optique
C                             dans l'atmosphère et dans la mer
C			      (-PROFILE.Log ==> FICPROFIL_LOG)    
C
C			    - Fichier Trace, si requis, de la routine OSOAA_SURFACE 
C			      qui génère les fichiers de matrices de BRDF/BPDF à 
C                             l'interface air/mer agitée
C			      (-SEA.Log ==> FICSURF_LOG)    
C
C			    - Fichier Trace, si requis, de la routine OSOAA_SOS 
C			      qui réalise les calculs d'Ordres Successifs
C			      (-SOS.Log ==> FICSOS_LOG)            
C
C
C         Fichiers de définition des angles :
C         ---------------------------------       
C             - Fichier des angles utilisés pour les calculs de MIE
C               (-ANG.Mie.ResFile ==>  FICANGLES_RES_MIE)      
C
C             - Fichier des angles utilisés pour les calculs de luminance
C               (-ANG.Rad.ResFile ==>  FICANGLES_RES_LUM)   
C         
C          Ces fichiers précisent les angles de Gauss utilisés et les poids associés, 
C          ainsi que les éventuels angles définis par le fichier utilisateur. 
C          Ils fournissent la valeur des paramètres de limite des développements
C          mathématiques internes au code.
C          Pour le calcul des luminances, le fichier donne l'angle zénithal solaire
C          dans l'air et dans la mer, ainsi que la position des angles correspondant
C          dans le fichier.      
C
C
C         Fichiers des propriétés radiatives des particules :
C         -------------------------------------------------       
C             - Fichier des propriétés radiatives des aérosols
C               (-AER.ResFile ==>  FICGRANU_AER)      
C
C             - Fichier des propriétés radiatives du phytoplancton
C               (-PHYTO.ResFile ==>  FICGRANU_PHYTO)   
C
C             - Fichier des propriétés radiatives des sédiments (Mineral-Like Particles)
C               (-MLP.ResFile ==>  FICGRANU_MLP)   
C         
C          Ces fichiers contiennnent les informations sur les propriétés radiatives des
C          particules : sections efficaces de diffusion et d'extinction, 
C          facteur d'asymétrie, volume moyen, coefficient de troncature de la fonction
C          de phase et coefficients de la matrice de phase décomposée en fonctions de
C          Legendre (coefficients ajustés à la troncature).
C
C
C         Fichiers des profils atmosphérique et marin :
C         -------------------------------------------       
C             - Fichier de définition du profil d'épaisseur optique dans l'atmosphère
C               (-PROFILE_ATM.ResFile ==>  FICPROFIL_ATM_RES)      
C
C             - Fichier de définition du profil d'épaisseur optique dans la mer
C               (-PROFILE_SEA.ResFile ==>  FICPROFIL_SEA_RES)   
C         
C          Ces fichiers donnent par niveau du profil, la valeur de l'épaisseur optique 
C          totale d'extinction et les taux de mélange par espèce pour la diffusion. 
C
C
C         Fichiers du champs de luminance simulé :
C         --------------------------------------        
C             - Fichier binaire de luminance en séries de Fourier
C               (-SOS.ResFile.Bin ==>  FICSOS_RES_BIN)      
C	        Ce fichier contient les enregistrements, par ordre de la série de Fourier,
C               du champ de luminance simulé pour chaque niveau du profil atmosphérique
C               et marin.
C               Il est destiné à un usage d'expertise par un spécialiste du code OSOAA.      
C
C             - Fichiers du champ de luminance montante ou descendante en fonction
C               du niveau (profondeur ou altitude) et de l'angle zénithal de visée, 
C               pour une valeur fixée de l'azimut relatif.
C               (-OSOAA.ResFile.Adv.Up   ==>  FICSOS_RES_ADV_UP
C		 -OSOAA.ResFile.Adv.Down ==>  FICSOS_RES_ADV_DOWN)  
C
C
C         Fichier de flux (Flux.txt) : Donne le profil de flux descendant du TOA 
C         ---------------------------  au fond marin normalisé par l'éclairement solaire au TOA.       
C				       (nom imposé par le code)
C
C
C     3) RESROOT : A la racine de l'espace de travail sont positionnés les fichiers
C        --------  suivants (imposés par le code):
C         - config.txt : copie du fichier OSOAA.h qui a servi à la compilation du code.
C         - ListParam.txt : liste de l'ensemble des paramètres de la simulation réalisée.
C
C
C   Une base de stockage des fichiers de MIE et des fichiers de matrice d'interface air/mer
C   est également créée:
C
C   - L'utilisateur définit un répertoire de stockage des fichiers de MIE pour les aérosols
C     via le mot clef -AER.DirMie (répertoire DIRMIE_AER). Ce répertoire stocke 
C     l'ensemble des fichiers issus des calculs de MIE pour les aérosols.
C
C   - L'utilisateur définit un répertoire de stockage des fichiers de MIE pour les hydrosols
C     via le mot clef -HYD.DirMie (répertoire DIRMIE_HYD) qui stocke l'ensemble des fichiers
C     issus des calculs de MIE pour les hydrosols.
C
C   - L'utilisateur définit un répertoire de stockage des fichiers d'interface air/mer 
C     agitée en réflexion et transmision, via le mot clef -SEA.Dir (répertoire DIRSEA).
C
C   La dénomination de ces fichiers est gérée par le code. 
C   Si un de ces fichiers est déjà existant, il n'est pas recalculé.
C             
C
C
C Variable d'environnement
C ------------------------
C
C   L'unique variable d'environnement prédéfinie est : OSOAA_ROOT
C   qui définit l'arborescence d'accès au répertoire principal du code OSOAA
C   contenant les sous-répertoires doc, exe, fic, gen, ihm, inc, obj, src
C
C   Cette variable d'environnement permet de localiser les fichiers
C   sous $OSOAA_ROOT/fic pour les routines OSOAA_AEROSOLS et OSOAA_PROFILE.
C
C   Elle permet aussi de localiser $OSOAA_ROOT/gen/OSOAA.h pour le contrôle par OSOAA_MAIN
C   du besoin de reprendre une simulation à son début ou de juste recombiner
C   le champ de luminance en séries de Fourier pour une nouvelle valeur d'azimut relatif.
C
C
C Description des fichiers d'entrée utilisés
C ------------------------------------------
C
C    Fichiers utilisateurs FICANGLES_USER_LUM (-ANG.Rad.UserAngFile)
C                       et FICANGLES_USER_MIE (-ANG.Mie.UserAngFile)
C       ==> contiennent une liste d'angles (en degrés) à ajouter aux angles de Gauss
C           pour lesquels le poids attribués sera nuls (=> sans impact sur les 
C           intégrations spatiales)
C
C       Format : Une valeur d'angle par ligne d'enregistrement.
C                Les angles doivent être compris entre 0 et 90 degrés.
C                Nombre d'enregistrements maximal : CTE_NBMAX_USER_ANGLES
C  
C  
C    Fichier utilisateur FICPHYTO_USER (-PHYTO.Userfile)
C       ==> contient le profil en chlorophylle fourni par l'utilisateur
C
C       Format : 1 ligne par couche du profil (2 colonnes / ligne).
C		 chaque ligne contient:
C			- l'altitude de la couche (en m)
C			- la concentration en chlorophylle (mg.m-3)
C  
C  
C    Fichier utilisateur FICEXTDATA_AER	(-AER.ExtData)
C       ==> contient les données externes (utilisateur) de fonctions de phase
C           et paramètres radiatifs des aérosols (coefficients extinction et diffusion).
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
C  
C    Fichier utilisateur FICEXTDATA_HYD	(-HYD.ExtData)
C       ==> contient les données externes (utilisateur) de fonctions de phase
C           et paramètres radiatifs (coefficients extinction et diffusion)
C 	    pour la diffusion hydrosols globale dans la mer : phytoplancton + sédiments.
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
C
C Common utilisé:
C --------------
C  Aucun
C
C
C Cas d'erreur :
C ------------        
C     -Erreur à l'ouverture du fichier Trace
C     -Décodage des paramètres incorrect
C     -Paramètres non valables
C     -Paramètres requis non définis
C     -Erreur dans un sous-programme
C
C     Affichage d'un message à l'écran, arrêt du programme et 
C     retour du status 1 au shell
C   
C
C  Sous programmes utilisés:
C --------------------------
C  Ce programme fait appel aux routines:
C     - OSOAA_ANGLES
C     - OSOAA_AEROSOLS
C     - OSOAA_HYDROSOLS
C     - OSOAA_PROFILE
C     - OSOAA_SURFACE
C     - OSOAA_SOS
C
C     - OSOAA_COMPA_SIMU_FILENAME
C     - OSOAA_COMPA_SIMU_VALUE
C
C     - SOS_INTERPOL
C
C============================================================================


      PROGRAM OSOAA_MAIN
      
      IMPLICIT NONE

C Constantes 
C -----------------            
      DOUBLE PRECISION CONVDEGRAD       ! Coefficient de conversion des degrés en radians.
      
      INTEGER*2 IDLOG			! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_MAIN)
    
      	            
C* Définition des variables                         
C*-----------------------------------------------------------------
    
C-- Variables liées aux paramètres d'entrée 
      INTEGER*2 NBARGS		        ! Nombre d'arguments en entrée 
      INTEGER*4 IARGC     	        ! Type de la fonction IARGC
      INTEGER*4 NPARAM 		        ! Numéro du paramètre en entrée 
                                        ! (type INTEGER*4 obligatoire pour le f77)
	
      CHARACTER*10 PROG_ARG		! Arguments du programme
					
      CHARACTER*CTE_LENKEYWORD KEYWORD  ! Mot clef du passage d'arguments.

C-- Variables liées à l'arborescence de travail 
      CHARACTER*CTE_LENDIR RESROOT      ! (E) Répertoire de travail
      CHARACTER*CTE_LENDIR RESROOT_STD  ! Répertoire des sorties "Standard"
      CHARACTER*CTE_LENDIR RESROOT_ADV  ! Répertoire des sorties "Advanced"

      CHARACTER*CTE_LENDIR DIREXE       ! chemin d'accès au repertoire $OSOAA_ROOT/exe

      CHARACTER*CTE_LENDIR DIRFIC	     ! chemin d'accès répertoire fic de l'arborescence OSOAA
        
      CHARACTER*CTE_LENCOM  COMMAND     ! Chaine de commande pour appel système.
         
      INTEGER*2 DEF_RESROOT		! Indicateur d'affectation d'une valeur à RESROOT
      
      INTEGER*4 LDIR     	        ! longueur de la chaine de caractères RESROOT
      INTEGER*4 LDIRSTD     	        ! longueur de la chaine de caractères RESROOT_STD
      INTEGER*4 LDIRADV     	        ! longueur de la chaine de caractères RESROOT_ADV
      INTEGER*4 LCOM     	        ! longueur de la chaine de caractères COMMAND
      
      INTEGER*4 LFIC    	        ! longueur de la chaine de caractères d'un nom 
      					! de fichier

C-- Variables globales      
      DOUBLE PRECISION WA_SIMU	! Longueur d'onde de simulation des luminances (en microns)
      DOUBLE PRECISION WA_SIMU_NM	! Longueur d'onde de simulation des luminances (en nanometres)
      
      INTEGER*2 DEF_FICMAIN_LOG	! Indicateur d'affectation d'une valeur à FICMAIN_LOG 
      INTEGER*2 DEF_WA_SIMU	! Indicateur d'affectation d'une valeur à DEF_WA_SIMU
      
      INTEGER*4 IER        	! code d'erreur =0 si pas d'erreur, =-1 sinon

      CHARACTER*CTE_LENFIC2 FICMAIN_LOG  ! Nom du fichier trace principal 
      
      LOGICAL TRACE     	! = vrai si écriture dans le fichier trace

      LOGICAL EX		! Vérifie l'existence d'un fichier
      
      LOGICAL CAS_REPRISE	! Indicateur si la simulation est une reprise du même 
      				! traitement pour un azimut différent (TRUE si c'est le cas)

      LOGICAL APPEL_SOS_CORE 	! = vrai si le fichier FICSOS_RES_BIN est à calculer
      			     	! (i.e. s'il n'est pas déjà calculé) 
      
      INTEGER*4 IO		! Indice IOSTAT pour test fin de fichier.

					    
C---- Variables liées à la définition des angles  

      INTEGER*2 DEF_TETAS		! Indicateur d'affectation d'une valeur à TETAS
      INTEGER*2 DEF_FICANGLES_RES_LUM  	! Indicateur d'affectation pour FICANGLES_RES_LUM
      INTEGER*2 DEF_FICANGLES_RES_MIE  	! Indicateur d'affectation pour FICANGLES_RES_MIE
     
      INTEGER*4 NBMU_GAUSS_LUM          ! (E) Nombre d'angles de Gauss pour calculs 
                                        !     de luminance
      INTEGER*4 NBMU_GAUSS_MIE          ! (E) Nombre d'angles de Gauss pour calculs de Mie        

      DOUBLE PRECISION TETAS	! (E) Angle zénithal solaire en degrés (0 < tetas < 90).
      				! (angle complémentaire : mus= -cos(tetas))

      DOUBLE PRECISION TETASW	! Angle zénithal solaire en degrés (0 < tetas < 90).
      				! transmis dans la mer plate
      				! (angle complémentaire : musw= -cos(tetasw))

      DOUBLE PRECISION SIN_TETASW	! Sinus de l'angle TETASW.
            				      
      CHARACTER*CTE_LENFIC2 FICANGLES_LOG       ! (S) Fichier trace des calculs d'angles
      	!Calculs de luminance :         
      CHARACTER*CTE_LENFIC2 FICANGLES_USER_LUM  ! (E) Chemin et nom du fichier des angles  
                                                ! "utilisateurs" pour les calculs de 
						! luminance
      CHARACTER*CTE_LENFIC2 FICANGLES_RES_LUM   ! (S) Chemin et nom du fichier des angles  
                                                ! utilisés pour les calculs de luminance
      	!Calculs de fonctions de phase :  
      CHARACTER*CTE_LENFIC2 FICANGLES_USER_MIE  ! (E) Chemin et nom du fichier des angles 
                                                ! "utilisateurs" pour les calculs de
						! de fonctions phase 
      CHARACTER*CTE_LENFIC2 FICANGLES_RES_MIE   ! (S) Chemin et nom du fichier des angles
                                                ! utilisés pour les calculs de fonctions 
						! de phase 

  
C---- Variables liées à la définition des modèles d'aérosols
	
      INTEGER*2 DEF_WAREF_AOT		! Indicateur d'affectation d'une valeur à WAREF_AOT
      INTEGER*2 DEF_AOT_REF		! Indicateur d'affectation pour AOT_REF
      INTEGER*2 DEF_IMOD_AER		! Indicateur d'affectation pour IMOD_AER
      INTEGER*2 DEF_RN_MMD_AER		! Indicateur d'affectation pour RN_MMD_AER
      INTEGER*2 DEF_IN_MMD_AER		! Indicateur d'affectation pour IN_MMD_AER
      INTEGER*2 DEF_RN_MMD_AER_WAREF	! Indicateur d'affectation pour RN_MMD_AER_WAREF
      INTEGER*2 DEF_IN_MMD_AER_WAREF	! Indicateur d'affectation pour IN_MMD_AER_WAREF
      INTEGER*2 DEF_IGRANU_MMD_AER	! Indicateur d'affectation pour IGRANU_MMD_AER
      INTEGER*2 DEF_LND_RADIUS_MMD_AER	! Indicateur d'affectation pour LND_RADIUS_MMD_AER
      INTEGER*2 DEF_LND_LNVAR_MMD_AER	! Indicateur d'affectation pour LND_LNVAR_MMD_AER
      INTEGER*2 DEF_JD_SLOPE_MMD_AER	! Indicateur d'affectation pour JD_SLOPE_MMD_AER
      INTEGER*2 DEF_JD_RMIN_MMD_AER	! Indicateur d'affectation pour JD_RMIN_MMD_AER
      INTEGER*2 DEF_JD_RMAX_MMD_AER	! Indicateur d'affectation pour JD_RMAX_MMD_AER
       
      INTEGER*2 DEF_IMODELE_WMO ! Indicateur d'affectation pour IMODELE_WMO
      INTEGER*2 DEF_WMO_DL 	! Indicateur d'affectation pour C_WMO_DL pour cas WMO
      INTEGER*2 DEF_WMO_WS 	! Indicateur d'affectation pour C_WMO_WS pour cas WMO
      INTEGER*2 DEF_WMO_OC 	! Indicateur d'affectation pour C_WMO_OC pour cas WMO
      INTEGER*2 DEF_WMO_SO 	! Indicateur d'affectation pour C_WMO_SO pour cas WMO
      INTEGER*2 DEF_IMODELE_SF  ! Indicateur d'affectation pour IMODELE_SF
      INTEGER*2 DEF_RH  	! Indicateur d'affectation pour RH pour cas S&F
           
      INTEGER*2 DEF_MODE_PARAM_BILND ! Indicateur d'affectation pour DEF_MODE_PARAM_BILND
      INTEGER*2 DEF_USER_CV_COARSE   ! Indicateur d'affectation pour USER_CV_COARSE
      INTEGER*2 DEF_USER_CV_FINE     ! Indicateur d'affectation pour USER_CV_FINE
      INTEGER*2 DEF_RTAUct_WAREF     ! Indicateur d'affectation pour RTAUct_WAREF
      INTEGER*2 DEF_BMD_CM_MRWA	     ! Indicateur d'affectation pour BMD_CM_MRWA 
      INTEGER*2 DEF_BMD_CM_MIWA	     ! Indicateur d'affectation pour BMD_CM_MIWA
      INTEGER*2 DEF_BMD_CM_MRWAREF   ! Indicateur d'affectation pour BMD_CM_MRWAREF
      INTEGER*2 DEF_BMD_CM_MIWAREF   ! Indicateur d'affectation pour BMD_CM_MIWAREF
      INTEGER*2 DEF_BMD_CM_RMODAL    ! Indicateur d'affectation pour BMD_CM_RMODAL
      INTEGER*2 DEF_BMD_CM_VAR  ! Indicateur d'affectation pour BMD_CM_VAR
      INTEGER*2 DEF_BMD_FM_MRWA	     ! Indicateur d'affectation pour BMD_FM_MRWA
      INTEGER*2 DEF_BMD_FM_MIWA	     ! Indicateur d'affectation pour BMD_FM_MIWA
      INTEGER*2 DEF_BMD_FM_MRWAREF   ! Indicateur d'affectation pour BMD_FM_MRWAREF
      INTEGER*2 DEF_BMD_FM_MIWAREF   ! Indicateur d'affectation pour BMD_FM_MIWAREF
      INTEGER*2 DEF_BMD_FM_RMODAL    ! Indicateur d'affectation pour BMD_FM_RMODAL
      INTEGER*2 DEF_BMD_FM_VAR  ! Indicateur d'affectation pour BMD_FM_VAR

      INTEGER*2 DEF_FICEXTDATA_AER   ! Indicateur d'affectation pour FICEXTDATA_AER
      
      INTEGER*2 DEF_DIRMIE_AER	     ! Indicateur d'affectation pour DIRMIE_AER
      INTEGER*2 DEF_FICGRANU_AER     ! Indicateur d'affectation pour FICGRANU_AER
      

      INTEGER*4 ITRONC_AER	! (E) Option de troncature de la fonction de phase des 
      				!     aérosols (1 pour la réaliser : valeur par défaut). 

      INTEGER*4 ITRONC_AER_IN	! Option de troncature de la fonction de phase des aérosols
      				!  (=ITRONC_AER en entrée d'appel de OSOAA_AEROSOLS,
				!   peut basculer de 1 à 0 en sortie de OSOAA_AEROSOLS
				!   si la troncature est automatiquement annulée). 
								
      INTEGER*4 IGRANU_MMD_AER	! (E) Numéro du modèle de granulométrie 
                                !     (cas mono-mode aérosols)				

      DOUBLE PRECISION WAREF_AOT  ! (E) Longueur d'onde de référence pour la donnée 
 				  !     d'épaisseur optique des aérosols (en microns).
      DOUBLE PRECISION AOT_REF	  ! (E) Epaisseur optique des aérosols (non tronquée) 
 				  !     pour la longueur d'onde de référence.
					
   	!Cas mono-mode aérosols : 	
   		! partie réelle et imaginaire de l'indice de réfraction 
   		! à la longueur de simulation de la luminance		
      DOUBLE PRECISION RN_MMD_AER	  ! (E) partie réelle 
      DOUBLE PRECISION IN_MMD_AER	  ! (E) partie imaginaire 
   		! partie réelle et imaginaire de l'indice de réfraction 
		! à la longueur de référence
      DOUBLE PRECISION RN_MMD_AER_WAREF	  ! (E) partie réelle 
      DOUBLE PRECISION IN_MMD_AER_WAREF	  ! (E) partie imaginaire 			
					
      DOUBLE PRECISION LND_RADIUS_MMD_AER ! (E) Rayon modal de la LND.	
      DOUBLE PRECISION LND_LNVAR_MMD_AER  ! (E) Variance de la LND. 	
      DOUBLE PRECISION JD_SLOPE_MMD_AER   ! (E) Pente de la loi de Junge. 
      DOUBLE PRECISION JD_RMIN_MMD_AER    ! (E) Rayon minimal de la loi de Junge.
      DOUBLE PRECISION JD_RMAX_MMD_AER    ! (E) Rayon maximal de la loi de Junge.

 		! Noms généraux des paramètres de granulométrie pour modèle mono-mode       
      DOUBLE PRECISION VARGRANU1_MMD_AER    ! Paramètre 1 de la granulométrie.
      DOUBLE PRECISION VARGRANU2_MMD_AER    ! Paramètre 2 de la granulométrie.
      DOUBLE PRECISION VARGRANU3_MMD_AER    ! Paramètre 3 de la granulométrie.
            
    	!Proportion volumique des constituants WMO
      DOUBLE PRECISION C_WMO_DL	    	! (E) --> Valeur utilisateur pour particule DL
      DOUBLE PRECISION C_WMO_WS	    	! (E) --> Valeur utilisateur pour particule WS
      DOUBLE PRECISION C_WMO_OC	    	! (E) --> Valeur utilisateur pour particule OC
      DOUBLE PRECISION C_WMO_SO	    	! (E) --> Valeur utilisateur pour particule SO
    				   
      DOUBLE PRECISION RH               ! (E) % humidité relative des modèles Shettle & Fenn
      
	!Paramètres pour un modèle bimodal de LND
      INTEGER*2 MODE_PARAM_BILND       	! (E) Indice du choix de description du mélange 
      					!     de 2 LND.
      DOUBLE PRECISION USER_CV_COARSE	! (E) Valeur utilisateur de la concentration 
      					!     volumique de la "LND coarse"
      DOUBLE PRECISION USER_CV_FINE	! (E) Valeur utilisateur de la concentration
      					!     volumique de la "LND fine"	
      DOUBLE PRECISION RTAUct_WAREF	! (E) Rapport des ep. opt. AOT_coarse / AOT_total 
					
      		!Partie réelle de l'indice de réfraction de la 
      		!"LND coarse" et de la "LND fine"  pour la longeur d'onde WA.		
      DOUBLE PRECISION BMD_CM_MRWA	! (E) coarse mode
      DOUBLE PRECISION BMD_FM_MRWA	! (E) fine mode    
       
      		!Partie imaginaire de l'indice de réfraction de la 
      		!"LND coarse" et de la "LND fine" pour la longeur d'onde WA.		
      DOUBLE PRECISION BMD_CM_MIWA	! (E) coarse mode
      DOUBLE PRECISION BMD_FM_MIWA	! (E) fine mode 
      
      		!Partie réelle de l'indice de réfraction de la 
      		!"LND coarse" et de la "LND fine"pour la longueur d'onde WAREF.
      DOUBLE PRECISION BMD_CM_MRWAREF	! (E) coarse mode
      DOUBLE PRECISION BMD_CM_MIWAREF	! (E) fine mode 
      
      		!Partie imaginaire de l'indice de réfraction de la 
      		!"LND coarse" et de la "LND fine" pour la longueur d'onde WAREF.
      DOUBLE PRECISION BMD_FM_MRWAREF	! (E) coarse mode
      DOUBLE PRECISION BMD_FM_MIWAREF	! (E) fine mode      
      
      		!Rayon modal des "LND coarse" et "fine" .
      DOUBLE PRECISION BMD_CM_RMODAL	! (E) coarse mode
      DOUBLE PRECISION BMD_FM_RMODAL	! (E) fine mode  
      
      		!Variance des "LND coarse" et "fine".
      DOUBLE PRECISION BMD_CM_VAR	! (E) coarse mode
      DOUBLE PRECISION BMD_FM_VAR	! (E) fine mode       
      
      
      DOUBLE PRECISION KMAT1_WA      ! (E) Section efficace d'extinction des aérosols pour
      				     !     la longueur d'onde de simulation de la luminance.
      DOUBLE PRECISION KMAT1_WAREF   ! (E) Section efficace d'extinction des aérosols pour 
      				     !     la longueur d'onde de référence de l'épaisseur
				     !     optique des aérosols.
					
      INTEGER*2 IMOD_AER	     ! (E) Granulométrie aérosols mono ou multi-modale.    
      INTEGER*2 IMODELE_WMO 	     ! (E) Type du modèle WMO.
      INTEGER*2 IMODELE_SF  	     ! (E) Type du modèle Shettle & Fenn.
      

      CHARACTER*CTE_LENDIR  DIRMIE_AER      ! (E) Chemin d'accès au fichier MIE aérosols
      CHARACTER*CTE_LENFIC2 FICAER_LOG_MIE  ! (S) Nom du fichier TRACE de MIE 
      
      CHARACTER*CTE_LENFIC2 FICGRANU_AER    ! (S) Fichier résultat des calculs de phase aérosols
      CHARACTER*CTE_LENFIC2 FICAER_LOG      ! (S) Fichier trace des calculs de phase aérosols
   	
      CHARACTER*CTE_LENFIC2 FICEXTDATA_AER  ! (E) Chemin et nom du fichier de données  
      				  	    ! externes de fonctions de phase et paramètres  
					    ! radiatifs (coeff. extinction et diffusion)
					
  
C---- Variables liées à la définition des modèles de hydrosols
      INTEGER*2 DEF_FICGRANU_PHYTO	! Indicateur d'affectation pour FICGRANU_PHYTO
      INTEGER*2 DEF_FICGRANU_MLP	! Indicateur d'affectation pour FICGRANU_MLP
      INTEGER*2 DEF_DIRMIE_HYD	        ! Indicateur d'affectation pour DIRMIE_HYD
      INTEGER*2 DEF_IMOD_HYD		! Indicateur d'affectation pour IMOD_HYD 
      INTEGER*2 DEF_PHYTO_JD_SLOPE	! Indicateur d'affectation pour PHYTO_JD_SLOPE
      INTEGER*2 DEF_PHYTO_JD_MR		! Indicateur d'affectation pour PHYTO_JD_MR
      INTEGER*2 DEF_PHYTO_JD_MI		! Indicateur d'affectation pour PHYTO_JD_MI
      INTEGER*2 DEF_PHYTO_JD_RATE	! Indicateur d'affectation pour PHYTO_JD_RATE
      INTEGER*2 DEF_PHYTO_LND_SM_RADIUS	! Indicateur d'affectation pour PHYTO_LND_SM_RADIUS
      INTEGER*2 DEF_PHYTO_LND_SM_VAR	! Indicateur d'affectation pour PHYTO_LND_SM_VAR
      INTEGER*2 DEF_PHYTO_LND_SM_MR	! Indicateur d'affectation pour PHYTO_LND_SM_MR
      INTEGER*2 DEF_PHYTO_LND_SM_MI	! Indicateur d'affectation pour PHYTO_LND_SM_MI
      INTEGER*2 DEF_PHYTO_LND_SM_RATE	! Indicateur d'affectation pour PHYTO_LND_SM_RATE
      INTEGER*2 DEF_PHYTO_LND_TM_RADIUS	! Indicateur d'affectation pour PHYTO_LND_TM_RADIUS
      INTEGER*2 DEF_PHYTO_LND_TM_VAR	! Indicateur d'affectation pour PHYTO_LND_TM_VAR
      INTEGER*2 DEF_PHYTO_LND_TM_MR	! Indicateur d'affectation pour PHYTO_LND_TM_MR
      INTEGER*2 DEF_PHYTO_LND_TM_MI	! Indicateur d'affectation pour PHYTO_LND_TM_MI
      INTEGER*2 DEF_PHYTO_LND_TM_RATE	! Indicateur d'affectation pour PHYTO_LND_TM_RATE 
      INTEGER*2 DEF_SED_JD_SLOPE	! Indicateur d'affectation pour SED_JD_SLOPE
      INTEGER*2 DEF_SED_JD_MR		! Indicateur d'affectation pour SED_JD_MR
      INTEGER*2 DEF_SED_JD_MI		! Indicateur d'affectation pour SED_JD_MI
      INTEGER*2 DEF_SED_JD_RATE		! Indicateur d'affectation pour SED_JD_RATE
      INTEGER*2 DEF_SED_LND_SM_RADIUS	! Indicateur d'affectation pour SED_LND_SM_RADIUS
      INTEGER*2 DEF_SED_LND_SM_VAR	! Indicateur d'affectation pour SED_LND_SM_VAR
      INTEGER*2 DEF_SED_LND_SM_MR	! Indicateur d'affectation pour SED_LND_SM_MR
      INTEGER*2 DEF_SED_LND_SM_MI	! Indicateur d'affectation pour SED_LND_SM_MI
      INTEGER*2 DEF_SED_LND_SM_RATE	! Indicateur d'affectation pour SED_LND_SM_RATE
      INTEGER*2 DEF_SED_LND_TM_RADIUS	! Indicateur d'affectation pour SED_LND_TM_RADIUS
      INTEGER*2 DEF_SED_LND_TM_VAR	! Indicateur d'affectation pour SED_LND_TM_VAR
      INTEGER*2 DEF_SED_LND_TM_MR	! Indicateur d'affectation pour SED_LND_TM_MR
      INTEGER*2 DEF_SED_LND_TM_MI	! Indicateur d'affectation pour SED_LND_TM_MI
      INTEGER*2 DEF_SED_LND_TM_RATE	! Indicateur d'affectation pour SED_LND_TM_RATE  
      INTEGER*2 DEF_FICEXTDATA_HYD	! Indicateur d'affectation pour FICEXTDATA_HYD

      INTEGER*4 IMOD_HYD	   	! Type de caractérisation des hydrosols.   

      INTEGER*4 IFIN                    ! Nb de caractères significatifs d'une chaine  

	!Modèle Phytoplancton : Distribution principale selon le modèle de Junge
      DOUBLE PRECISION PHYTO_JD_SLOPE    ! (E) Pente de la distribution de Junge
      DOUBLE PRECISION PHYTO_JD_RMIN     ! (E) Rayon min de la distribution de Junge (microns)
      DOUBLE PRECISION PHYTO_JD_RMAX     ! (E) Rayon max de la distribution de Junge (microns)
      DOUBLE PRECISION PHYTO_JD_MR    	 ! (E) Partie réelle de l'indice de réfraction
      DOUBLE PRECISION PHYTO_JD_MI    	 ! (E) Partie imaginaire de l'indice de réfraction
      DOUBLE PRECISION PHYTO_JD_RATE     ! (E) Proportion de la distribution de Junge 
      					 !     devant les autres modes.
	!Modèle Phytoplancton : Distribution secondaire selon le modèle LND
      DOUBLE PRECISION PHYTO_LND_SM_RADIUS ! (E) Rayon modal  (microns)
      DOUBLE PRECISION PHYTO_LND_SM_VAR    ! (E) Variance 
      DOUBLE PRECISION PHYTO_LND_SM_MR     ! (E) Partie réelle de l'indice de réfraction
      DOUBLE PRECISION PHYTO_LND_SM_MI     ! (E) Partie imaginaire de l'indice de réfraction
      DOUBLE PRECISION PHYTO_LND_SM_RATE   ! (E) Proportion de la distribution LND 
      					   !     secondaire devant les autres modes.
	!Modèle Phytoplancton : Distribution tertiaire selon le modèle LND
      DOUBLE PRECISION PHYTO_LND_TM_RADIUS ! (E) Rayon modal (microns)
      DOUBLE PRECISION PHYTO_LND_TM_VAR    ! (E) Variance 
      DOUBLE PRECISION PHYTO_LND_TM_MR     ! (E) Partie réelle de l'indice de réfraction
      DOUBLE PRECISION PHYTO_LND_TM_MI     ! (E) Partie imaginaire de l'indice de réfraction
      DOUBLE PRECISION PHYTO_LND_TM_RATE   ! (E) Proportion de la distribution LND 
      					   !     tertiaire devant les autres modes.
	 

	!Modèle Sédiments : Distribution principale selon le modèle de Junge
      DOUBLE PRECISION SED_JD_SLOPE    	 ! (E) Pente de la distribution de Junge
      DOUBLE PRECISION SED_JD_RMIN    	 ! (E) Rayon min de la distribution de Junge (microns)
      DOUBLE PRECISION SED_JD_RMAX    	 ! (E) Rayon max de la distribution de Junge (microns)
      DOUBLE PRECISION SED_JD_MR    	 ! (E) Partie réelle de l'indice de réfraction
      DOUBLE PRECISION SED_JD_MI    	 ! (E) Partie imaginaire de l'indice de réfraction
      DOUBLE PRECISION SED_JD_RATE    	 ! (E) Proportion de la distribution de Junge 
      					 !     devant les autres modes
	!Modèle Sédiments : Distribution secondaire selon le modèle LND
      DOUBLE PRECISION SED_LND_SM_RADIUS ! (E) Rayon modal (microns)
      DOUBLE PRECISION SED_LND_SM_VAR    ! (E) Variance 
      DOUBLE PRECISION SED_LND_SM_MR     ! (E) Partie réelle de l'indice de réfraction
      DOUBLE PRECISION SED_LND_SM_MI     ! (E) Partie imaginaire de l'indice de réfraction
      DOUBLE PRECISION SED_LND_SM_RATE   ! (E) Proportion de la distribution LND secondaire
      					 !     devant les autres modes
	!Modèle Sédiments : Distribution tertiaire selon le modèle LND
      DOUBLE PRECISION SED_LND_TM_RADIUS ! (E) Rayon modal (microns)
      DOUBLE PRECISION SED_LND_TM_VAR    ! (E) Variance 
      DOUBLE PRECISION SED_LND_TM_MR     ! (E) Partie réelle de l'indice de réfraction
      DOUBLE PRECISION SED_LND_TM_MI     ! (E) Partie imaginaire de l'indice de réfraction
      DOUBLE PRECISION SED_LND_TM_RATE   ! (E) Proportion de la distribution LND tertiaire
      					 !     devant les autres modes
	 	 
      CHARACTER*CTE_LENDIR  DIRMIE_HYD        ! (E) Chemin d'accès au fichier MIE hydrosols
      CHARACTER*CTE_LENFIC2 FICHYD_LOG_MIE    ! (S) Nom du fichier TRACE de MIE 

      CHARACTER*CTE_LENFIC2 FICGRANU_PHYTO    ! (S) Nom du fichier résultat des calculs 
      					      !     de phase pour le phytoplancton
      CHARACTER*CTE_LENFIC2 FICGRANU_MLP      ! (S) Nom du fichier résultat des calculs
      					      !     de phase pour les sédiments
      
         
      CHARACTER*CTE_LENFIC2 FICHYD_LOG        ! (S) Nom du fichier trace des calculs 
      					      !     de phase hydrosols   

      CHARACTER*CTE_LENFIC2 FICEXTDATA_HYD    ! (E) Chemin et nom du fichier de données
      				!  externes de fonctions de phase et paramètres radiatifs 
				!  (coefficients d'extinction et de diffusion)  


      CHARACTER*INCTE_LENLINE LIGNE_TEXTE   !Ligne de caractères pour décodage de paramètres
      					    !d'un fichier produit
					

C       
C---- Variables liées à la définition des profils atmosphérique et marin  

C*   Atmosphere :
      DOUBLE PRECISION TR    	! (E) Epaisseur optique rayleigh.
      DOUBLE PRECISION PRESSION ! (E) Pression atmosphérique au niveau de la mer (mbar).
      DOUBLE PRECISION HR    	! (E) Echelle de hauteur du profil moléculaire (km).
      DOUBLE PRECISION HA    	! (E) Echelle de hauteur du profil d'aérosols (km).
      DOUBLE PRECISION TA    	! Epaisseur optique des aérosols pour la longueur d'onde
      				! de simulation.
      DOUBLE PRECISION PIZAER   ! Albédo de simple diffusion des aérosols pour la longueur
      				! d'onde de simulation.
      
C*   Mer
      DOUBLE PRECISION SEA_DEPTH    ! (E) Profondeur de la mer (m)      
      DOUBLE PRECISION CHL	    ! (E) Concentration de chlorophylle en surface (mg/m3)
      DOUBLE PRECISION CHL_GP_BG    ! (E) Constante de biomasse du profil gaussien 
      				    !     de chlorophylle (mg/m3)
      DOUBLE PRECISION CHL_GP_DEEP  ! (E) Profondeur max du profil gaussien de chl. (m)
      DOUBLE PRECISION CHL_GP_WIDTH ! (E) Largeur du pic du profil gaussien de chl. (m)
      DOUBLE PRECISION SED_CSED     ! (E) Concentration des sédiments en surface (mg/litre)
          
      DOUBLE PRECISION SED_MRWA	   ! Partie réelle de l'indice de réfraction moyen 
      				   ! des sédiments (à la longueur d'onde de simulation 
				   ! des luminances).
      DOUBLE PRECISION SED_VSED    ! Volume moyen d'une particule de "sédiment" (mic^3).
      DOUBLE PRECISION SED_KMAT2   ! Section efficace des sédiments (mic^2) 
      				   ! (à la longueur d'onde de simulation des luminances)
      DOUBLE PRECISION SED_PIZ	   ! Albédo de simple diffusion des sédiments 
      				   ! (à la longueur d'onde de simulation des luminances)
      DOUBLE PRECISION SED_PIZTR   ! Albédo de simple diffusion des sédiments après 
      				   ! troncature (à la longueur d'onde de simulation 
				   ! des luminances)
      DOUBLE PRECISION SED_A_TRONCA! Coefficient de troncature de la fonction de phase 
      				   ! des sédiments				   
      DOUBLE PRECISION YS_A440     ! (E)  Coeff d'abs. de la substance jaune à 440 nm (m-1) 
      DOUBLE PRECISION YS_SWA      ! (E)  Coefficient de conversion spectrale de 
      				   !      l'absorption de la substance jaune 
      DOUBLE PRECISION DET_A440    ! (E)  Coeff d'absorption des détritus à 440 nm (m-1) 
      DOUBLE PRECISION DET_SWA     ! (E)  Coefficient de conversion spectrale 
      				   !      de l'absorption des détritus  
	
      DOUBLE PRECISION VAL      ! Valeur lue dans fichier				
   	      
      INTEGER*2 DEF_TR 		      ! Indicateur d'affectation d'une valeur à TR
      INTEGER*2 DEF_PRESSION	      ! Indicateur d'affectation pour PRESSION
      INTEGER*2 DEF_HR 		      ! Indicateur d'affectation pour HR
      INTEGER*2 DEF_HA 		      ! Indicateur d'affectation pour HA
      INTEGER*2 DEF_CHL 	      ! Indicateur d'affectation pour CHL
      INTEGER*2 DEF_IPHYTO_PROFIL     ! Indicateur d'affectation pour IPHYTO_PROFIL
      INTEGER*2 DEF_CHL_GP_BG         ! Indicateur d'affectation pour CHL_GP_BG
      INTEGER*2 DEF_CHL_GP_DEEP       ! Indicateur d'affectation pour CHL_GP_DEEP
      INTEGER*2 DEF_CHL_GP_WIDTH      ! Indicateur d'affectation pour CHL_GP_WIDTH
      INTEGER*2 DEF_FICPHYTO_USER     ! Indicateur d'affectation pour FICPHYTO_USER
      INTEGER*2 DEF_SED_CSED	      ! Indicateur d'affectation pour SED_CSED
      INTEGER*2 DEF_YS_A440	      ! Indicateur d'affectation pour YS_A440
      INTEGER*2 DEF_DET_A440	      ! Indicateur d'affectation pour DET_A440
      INTEGER*2 DEF_FICPROFIL_ATM_RES ! Indicateur d'affectation pour FICPROFIL_ATM_RES
      INTEGER*2 DEF_FICPROFIL_SEA_RES ! Indicateur d'affectation pour FICPROFIL_SEA_RES

    	
      INTEGER*2 IPHYTO_PROFIL   ! (E) Indice de type de profil de phytoplancton
    
      CHARACTER*CTE_LENFIC2 FICPHYTO_USER     ! Nom du fichier utilisateur (chemin complet)
	     				      ! donnant le profil de chlorophylle
      CHARACTER*CTE_LENFIC2 FICPROFIL_SEA_RES ! Chemin et nom du fichier du profil marin 
                                              ! utilisé pour les calculs de luminance
      CHARACTER*CTE_LENFIC2 FICPROFIL_ATM_RES ! Chemin et nom du fichier du profil 
                                              ! atmosphérique pour les calculs de luminance      
      CHARACTER*CTE_LENFIC2 FICPROFIL_LOG     ! Nom du fichier trace des calculs de profils

C---- Variables liées à la définition de l'interface air - mer  

      DOUBLE PRECISION SEA_IND    ! (E) Indice de réfraction de la mer 
      				  ! pour la longueur d'onde de simulation de la luminance.
      DOUBLE PRECISION WIND	  ! (E) Vitesse du vent (m/s).
       
      INTEGER*2 DEF_SEA_IND 	! Indicateur d'affectation d'une valeur à SEA_IND
      INTEGER*2 DEF_WIND 	! Indicateur d'affectation d'une valeur à WIND
      INTEGER*2 DEF_DIRSEA	! Indicateur d'affectation d'une valeur à DIRSEA

      INTEGER*4 IMAT_SURF 	! Option de BRDF / BPDF de surface :
      			  	!   0 : mer plate si WIND = 0.
			  	!   1 : mer agitée  si WIND non nul.
			  
      CHARACTER*CTE_LENDIR DIRSEA	  ! Chemin d'accès aux fichiers SURFACE à produire.
      CHARACTER*CTE_LENFIC2 FICSURF_LOG   ! Nom du fichier trace des calculs de matrices
      					  ! d'interface air / mer      
       
C-- Variables liées aux calculs de la réflectance de fond si usage d'un fichier de valeurs
C   tabulées        
      DOUBLE PRECISION TAB_WA(CTE_NB_WA_FIC_BOT)	   ! Table des longueurs d'onde du fichier (en nm)
      DOUBLE PRECISION TAB_RSAND(CTE_NB_WA_FIC_BOT) ! Table des réflectances pour "clean coral sand"
      DOUBLE PRECISION TAB_RGREEN(CTE_NB_WA_FIC_BOT)! Table des réflectances pour "green algae"
      DOUBLE PRECISION TAB_RBROWN(CTE_NB_WA_FIC_BOT)! Table des réflectances pour "brown algae"
      DOUBLE PRECISION TAB_RRED(CTE_NB_WA_FIC_BOT)  ! Table des réflectances pour "red algae"
      DOUBLE PRECISION TAB_R_BOT(CTE_NB_WA_FIC_BOT) ! Table des réflectances retenues
      DOUBLE PRECISION WA_INF, WA_SUP	! Valeurs de longueurs d'onde tabulées encadrant WA_SIMU
      DOUBLE PRECISION R_INF, R_SUP	! Valeurs de réflectance de fond correspondant aux longueurs 
                                          ! d'onde WA_INF et WA_SUP
      INTEGER*4 I
	 
C-- Variables liées aux calculs d'Ordres Successifs et à la production
C   des fichiers résultats
      INTEGER*2 DEF_RHO_SURF		! Indicateur d'affectation pour DEF_RHO_SURF
      INTEGER*2 DEF_IBOT_TYPE		! Indicateur d'affectation pour DEF_IBOT_TYPE
      INTEGER*2 DEF_RHO_BOT		! Indicateur d'affectation pour DEF_RHO_BOT
      INTEGER*2 DEF_PHI			! Indicateur d'affectation pour DEF_PHI
      INTEGER*2 DEF_LEVEL_OUT		! Indicateur d'affectation pour DEF_LEVEL_OUT
      INTEGER*2 DEF_USER_Z_OUT		! Indicateur d'affectation pour DEF_USER_Z_OUT
      INTEGER*2 DEF_FICSOS_RES_VS_VZA	! Indicateur d'affectation pour FICSOS_RES_VS_VZA
      INTEGER*2 DEF_VZA_OUT		! Indicateur d'affectation pour VZA_OUT
      INTEGER*2 DEF_IGMAX		! Indicateur d'affectation pour IGMAX
      INTEGER*2 DEF_FICSOS_RES_BIN	! Indicateur d'affectation pour FICSOS_RES_BIN 

      DOUBLE PRECISION RHO_SURF	! (E)  Albédo de la surface (composante lambertienne)
      DOUBLE PRECISION RHO_BOT	! (E)  Albédo du fond marin (composante lambertienne)     
      DOUBLE PRECISION PHI	! (E)  Azimut relatif (degrés)
      DOUBLE PRECISION USER_Z_OUT ! (E)  Altitude ou profondeur (mètres) pour la 
      				  !      représentation de sortie de la luminance
				  !      en fonction de l'angle zénithal de visée 
				  !      (Phi et Z fixés)
				  !      --> Valeur définie par l'utilisateur
      DOUBLE PRECISION VZA_OUT	! (E)  Angle zénithal de visée (degrés) pour la
      				!      représentation de sortie de la luminance en fonction
				!      de la profondeur (ou altitude) (Phi et VZA fixés)


      INTEGER*4 IBOT_TYPE	! (E)  Indice de type de fond marin
      INTEGER*4 IGMAX		! (E)  Ordre maximal de diffusion.
      INTEGER*4 LEVEL_OUT	! (E)  Indice de type de niveau de sortie

      CHARACTER*CTE_LENFIC2 FIC_BOT_DATA  	! Nom du fichier des données spectrales 
                                                 ! tabulées de réflectance de fond marin
      
      CHARACTER*CTE_LENFIC2 FICSOS_RES_VS_VZA  	! (S) Nom du fichier résultat donnant la
	     				       	! luminance en fonction de l'angle zénithal
	     					! de visée (Phi et Z fixés)
						
      CHARACTER*CTE_LENFIC2 FICSOS_RES_VS_Z  	! (S) Nom du fichier résultat donnant la
	     				       	! luminance en fonction de la profondeur 
	     					! ET de l'angle de visée (Phi fixé)
	
      CHARACTER*CTE_LENFIC2 FICSOS_RES_ADV_UP  	! (S) Nom du fichier résultat donnant la
	     				       	! luminance montante en fonction de la
	     					! profondeur (ou altitude) 
						! (Phi et VZA fixés)
	
      CHARACTER*CTE_LENFIC2 FICSOS_RES_ADV_DOWN ! (S) Nom du fichier résultat donnant la
	     				       	! luminance descendante en fonction de la
	     					! profondeur (ou altitude) 
						! (Phi et VZA fixés)
						
      CHARACTER*CTE_LENFIC2 FICSOS_RES_BIN  	! (S) Nom du fichier résultat binaire
      						! en sortie des calculs SOS	

      CHARACTER*CTE_LENFIC2 FIC_FLUX    	! (S) Fichier des profils de flux
				
      CHARACTER*CTE_LENFIC2 FICSOS_LOG  	! Nom du fichier trace des calculs d'OS




C =========================================
C --- Initialisations 
C =========================================

C --- Initialisation de l'indicateur d'erreur de traitement
C----------------------------------------------------------
      IER=0

      !Coefficient de conversion des degrés en radians.
      CONVDEGRAD=INCTE_PI/180.D+00
      
C --- Initialisation de la valeur par défaut des paramètres optionnels
C---------------------------------------------------------------------
C     Nombre d'angles de Gauss pour les calculs de luminance : non défini
      NBMU_GAUSS_LUM = CTE_NOT_DEFINED_VALUE_INT       
      
C     Absence de fichier d'angles "utilisateur" pour les calculs de luminance 
      FICANGLES_USER_LUM = "NO_USER_ANGLES"       

C     Nom par défaut du fichier des angles pour les calculs de luminance 
      FICANGLES_RES_LUM = CTE_DEFAULT_FICANGLES_RES_LUM
      
C     Nombre d'angles de Gauss pour les calculs de Mie : non défini
      NBMU_GAUSS_MIE = CTE_NOT_DEFINED_VALUE_INT
      
C     Absence de fichier d'angles "utilisateur" pour les calculs de Mie 
      FICANGLES_USER_MIE = "NO_USER_ANGLES"

C     Nom par défaut du fichier des angles pour les calculs de luminance 
      FICANGLES_RES_MIE = CTE_DEFAULT_FICANGLES_RES_MIE
      
C     Pas de fichier trace de la définition des angles
      FICANGLES_LOG = "NO_LOG_FILE"

C     Nom par défaut du fichier des propriétés radiatives des aérosols
      FICGRANU_AER = CTE_DEFAULT_FICGRANU_AER

C     Pas de fichier trace de la définition des matrices de phase aérosols
      FICAER_LOG     = "NO_LOG_FILE"
      FICAER_LOG_MIE = "NO_LOG_FILE"
 
C     Troncature de la pointe avant de la fonction de phase aérosols
      ITRONC_AER = 1
 
C     Nom par défaut du fichier des propriétés radiatives du phytoplancton
      FICGRANU_PHYTO = CTE_DEFAULT_FICGRANU_PHYTO 
      
C     Nom par défaut du fichier des propriétés radiatives des "Mineral-Like Particles"
      FICGRANU_MLP = CTE_DEFAULT_FICGRANU_MLP
      
C     Pas de fichier trace de la définition des matrices de phase hydrosols
      FICHYD_LOG = "NO_LOG_FILE"  
      FICHYD_LOG_MIE = "NO_LOG_FILE" 
           
C     Profondeur de la mer : non définie
      SEA_DEPTH = CTE_NOT_DEFINED_VALUE_DBLE      

C     Coeff de conversion spectrale de la matière jaune : non défini 
      YS_SWA = CTE_NOT_DEFINED_VALUE_DBLE
      
C     Coeff de conversion spectrale des détritus : non défini 
      DET_SWA = CTE_NOT_DEFINED_VALUE_DBLE

C     Nom par défaut du fichier de profil atmosphérique
      FICPROFIL_ATM_RES = CTE_DEFAULT_FICPROFIL_ATM_RES

C     Nom par défaut du fichier de profil marin
      FICPROFIL_SEA_RES = CTE_DEFAULT_FICPROFIL_SEA_RES
            
C     Pas de fichier trace de la définition des profils 
      FICPROFIL_LOG = "NO_LOG_FILE" 

C     Pas de fichier trace des calculs de matrices d'interface air / mer
      FICSURF_LOG = "NO_LOG_FILE"      

C     L'albédo de fond marin peut ne pas être directement renseigné : non défini   
      RHO_BOT = CTE_NOT_DEFINED_VALUE_DBLE
	
C     Pas de fichier trace des calculs de luminance par Ordres Successifs
      FICSOS_LOG = "NO_LOG_FILE" 

C     Nom par défaut du fichier résultat de luminance en fonction de l'angle zénithal
      FICSOS_RES_VS_VZA = CTE_DEFAULT_FICSOS_RES_VS_VZA

C     Nom par défaut du fichier résultat de luminance en SF (fichier binaire sortie des OS)
      FICSOS_RES_BIN = CTE_DEFAULT_FICSOS_RES_BIN

C     Valeur d'altitude ou profondeur pour les valeurs de luminance fonction de VZA
C     définie par l'utilisateur : non définie			
      USER_Z_OUT = CTE_NOT_DEFINED_VALUE_DBLE      		

C     Par défaut, il n'y a pas de sortie de luminance en fonction du niveau 
C     pour un angle zénithal fixé 
      VZA_OUT = CTE_NOT_DEFINED_VALUE_DBLE 
                
C     Nom par défaut du fichier résultat de luminance en fonction du niveau
      FICSOS_RES_VS_Z = "NO_RES_VS_Z_FILE"
    
C     Pas de fichier résultat "Advanced"
      FICSOS_RES_ADV_UP = "NO_ADVANCED_FILE" 
      FICSOS_RES_ADV_DOWN = "NO_ADVANCED_FILE"        

C     Nombre d'interactions maximal par défaut positionné à la valeur
C     de la constante CTE_DEFAULT_IGMAX
      IGMAX = CTE_DEFAULT_IGMAX    
	 
C --- Initialisation de la valeur par défaut des paramètres 
C     dont la définition dépend des paramètres obligatoires
C----------------------------------------------------------
C     Paramètres aérosols
      IMOD_AER 		= CTE_NOT_DEFINED_VALUE_INT     
      RN_MMD_AER 	= CTE_NOT_DEFINED_VALUE_DBLE
      IN_MMD_AER 	= CTE_NOT_DEFINED_VALUE_DBLE
      RN_MMD_AER_WAREF 	= CTE_NOT_DEFINED_VALUE_DBLE 
      IN_MMD_AER_WAREF 	= CTE_NOT_DEFINED_VALUE_DBLE 	
      IGRANU_MMD_AER 	= CTE_NOT_DEFINED_VALUE_INT  
      LND_RADIUS_MMD_AER = CTE_NOT_DEFINED_VALUE_DBLE
      LND_LNVAR_MMD_AER = CTE_NOT_DEFINED_VALUE_DBLE	
      JD_SLOPE_MMD_AER 	= CTE_NOT_DEFINED_VALUE_DBLE
      JD_RMIN_MMD_AER 	= CTE_NOT_DEFINED_VALUE_DBLE
      JD_RMAX_MMD_AER   = CTE_DEFAULT_AER_JUNGE_RMAX		!Rmax par défaut  
      IMODELE_WMO 	= CTE_NOT_DEFINED_VALUE_INT 
      C_WMO_DL 		= CTE_NOT_DEFINED_VALUE_DBLE
      C_WMO_WS 		= CTE_NOT_DEFINED_VALUE_DBLE
      C_WMO_OC 		= CTE_NOT_DEFINED_VALUE_DBLE
      C_WMO_SO 		= CTE_NOT_DEFINED_VALUE_DBLE
      IMODELE_SF 	= CTE_NOT_DEFINED_VALUE_INT 
      RH 		= CTE_NOT_DEFINED_VALUE_DBLE 
      MODE_PARAM_BILND 	= CTE_NOT_DEFINED_VALUE_INT 
      USER_CV_COARSE 	= CTE_NOT_DEFINED_VALUE_DBLE 
      USER_CV_FINE 	= CTE_NOT_DEFINED_VALUE_DBLE  
      RTAUct_WAREF 	= CTE_NOT_DEFINED_VALUE_DBLE  
      BMD_CM_MRWA 	= CTE_NOT_DEFINED_VALUE_DBLE 
      BMD_CM_MIWA 	= CTE_NOT_DEFINED_VALUE_DBLE 
      BMD_CM_MRWAREF 	= CTE_NOT_DEFINED_VALUE_DBLE  
      BMD_CM_MIWAREF 	= CTE_NOT_DEFINED_VALUE_DBLE 
      BMD_CM_RMODAL 	= CTE_NOT_DEFINED_VALUE_DBLE  
      BMD_CM_VAR 	= CTE_NOT_DEFINED_VALUE_DBLE 
      BMD_FM_MRWA	= CTE_NOT_DEFINED_VALUE_DBLE  
      BMD_FM_MIWA	= CTE_NOT_DEFINED_VALUE_DBLE
      BMD_FM_MRWAREF 	= CTE_NOT_DEFINED_VALUE_DBLE 
      BMD_FM_MIWAREF 	= CTE_NOT_DEFINED_VALUE_DBLE
      BMD_FM_RMODAL	= CTE_NOT_DEFINED_VALUE_DBLE 
      BMD_FM_VAR 	= CTE_NOT_DEFINED_VALUE_DBLE 	 
      FICEXTDATA_AER 	= "NO_USER_AEROSOLS_PHAZE_FCT" 

      DIRMIE_AER	= "UNDEFINED MIE REPOSITORY"
      VARGRANU1_MMD_AER = CTE_NOT_DEFINED_VALUE_DBLE 
      VARGRANU2_MMD_AER = CTE_NOT_DEFINED_VALUE_DBLE 
      VARGRANU3_MMD_AER = CTE_NOT_DEFINED_VALUE_DBLE 
      
C     Paramètres hydrosols
      IMOD_HYD 			= CTE_NOT_DEFINED_VALUE_INT  
      PHYTO_JD_SLOPE 		= CTE_NOT_DEFINED_VALUE_DBLE 	
      PHYTO_JD_RMIN  		= CTE_DEFAULT_HYD_JUNGE_RMIN		!Rmin par défaut
      PHYTO_JD_RMAX 		= CTE_DEFAULT_HYD_JUNGE_RMAX		!Rmax par défaut    
      PHYTO_JD_MR 		= CTE_NOT_DEFINED_VALUE_DBLE
      PHYTO_JD_MI 		= CTE_NOT_DEFINED_VALUE_DBLE
      PHYTO_JD_RATE 		= CTE_NOT_DEFINED_VALUE_DBLE
      PHYTO_LND_SM_RADIUS 	= CTE_NOT_DEFINED_VALUE_DBLE 
      PHYTO_LND_SM_VAR 		= CTE_NOT_DEFINED_VALUE_DBLE 
      PHYTO_LND_SM_MR 		= CTE_NOT_DEFINED_VALUE_DBLE 
      PHYTO_LND_SM_MI 		= CTE_NOT_DEFINED_VALUE_DBLE 
      PHYTO_LND_SM_RATE 	= CTE_NOT_DEFINED_VALUE_DBLE 
      PHYTO_LND_TM_RADIUS 	= CTE_NOT_DEFINED_VALUE_DBLE 
      PHYTO_LND_TM_VAR 		= CTE_NOT_DEFINED_VALUE_DBLE 
      PHYTO_LND_TM_MR 		= CTE_NOT_DEFINED_VALUE_DBLE 
      PHYTO_LND_TM_MI 		= CTE_NOT_DEFINED_VALUE_DBLE 
      PHYTO_LND_TM_RATE 	= CTE_NOT_DEFINED_VALUE_DBLE 
	 
      SED_JD_SLOPE 		= CTE_NOT_DEFINED_VALUE_DBLE 	
      SED_JD_RMIN  		= CTE_DEFAULT_HYD_JUNGE_RMIN		!Rmin par défaut
      SED_JD_RMAX  		= CTE_DEFAULT_HYD_JUNGE_RMAX		!Rmax par défaut     
      SED_JD_MR 		= CTE_NOT_DEFINED_VALUE_DBLE
      SED_JD_MI 		= CTE_NOT_DEFINED_VALUE_DBLE
      SED_JD_RATE 		= CTE_NOT_DEFINED_VALUE_DBLE
      SED_LND_SM_RADIUS 	= CTE_NOT_DEFINED_VALUE_DBLE 
      SED_LND_SM_VAR 		= CTE_NOT_DEFINED_VALUE_DBLE 
      SED_LND_SM_MR 		= CTE_NOT_DEFINED_VALUE_DBLE 
      SED_LND_SM_MI 		= CTE_NOT_DEFINED_VALUE_DBLE 
      SED_LND_SM_RATE 		= CTE_NOT_DEFINED_VALUE_DBLE 
      SED_LND_TM_RADIUS 	= CTE_NOT_DEFINED_VALUE_DBLE 
      SED_LND_TM_VAR 		= CTE_NOT_DEFINED_VALUE_DBLE 
      SED_LND_TM_MR 		= CTE_NOT_DEFINED_VALUE_DBLE 
      SED_LND_TM_MI 		= CTE_NOT_DEFINED_VALUE_DBLE 
      SED_LND_TM_RATE 		= CTE_NOT_DEFINED_VALUE_DBLE 
      
      FICEXTDATA_HYD 		= "NO_USER_HYDROSOLS_PHAZE_FCT" 
      FICPHYTO_USER 		= "NO_USER_PHYTO_PROFILE"
      
      DIRMIE_HYD		= "UNDEFINED MIE REPOSITORY"
      
      
C --- Initialisation des indicateurs de définition des paramètres obligatoires 
C     (0 pour indéfini)
C-----------------------------------------------------------------------------

      DEF_RESROOT=0
      DEF_FICMAIN_LOG=0
      DEF_WA_SIMU=0
      
      DEF_TETAS=0
      DEF_FICANGLES_RES_LUM=0
      DEF_FICANGLES_RES_MIE=0
      
      DEF_WAREF_AOT=0
      DEF_AOT_REF=0
      DEF_IMOD_AER=0
      DEF_RN_MMD_AER=0
      DEF_IN_MMD_AER=0
      DEF_RN_MMD_AER_WAREF=0
      DEF_IN_MMD_AER_WAREF=0
      DEF_IGRANU_MMD_AER=0   
      DEF_LND_RADIUS_MMD_AER=0	
      DEF_LND_LNVAR_MMD_AER=0
      DEF_JD_SLOPE_MMD_AER=0	 
      DEF_JD_RMIN_MMD_AER=0
      DEF_JD_RMAX_MMD_AER=0	 
      DEF_IMODELE_WMO=0
      DEF_WMO_DL=0	    
      DEF_WMO_WS=0
      DEF_WMO_OC=0
      DEF_WMO_SO=0
      DEF_IMODELE_SF=0
      DEF_RH=0 
      DEF_MODE_PARAM_BILND=0
      DEF_USER_CV_COARSE=0
      DEF_USER_CV_FINE=0
      DEF_RTAUct_WAREF=0
      DEF_BMD_CM_MRWA=0
      DEF_BMD_CM_MIWA=0
      DEF_BMD_CM_MRWAREF=0
      DEF_BMD_CM_MIWAREF=0
      DEF_BMD_CM_RMODAL=0
      DEF_BMD_CM_VAR=0
      DEF_BMD_FM_MRWA=0
      DEF_BMD_FM_MIWA=0
      DEF_BMD_FM_MRWAREF=0
      DEF_BMD_FM_MIWAREF=0
      DEF_BMD_FM_RMODAL=0
      DEF_BMD_FM_VAR=0
      DEF_FICEXTDATA_AER=0
      DEF_DIRMIE_AER=0
      DEF_FICGRANU_AER=0

      DEF_FICGRANU_PHYTO=0   
      DEF_FICGRANU_MLP=0
      DEF_DIRMIE_HYD=0
      DEF_IMOD_HYD=0 
      DEF_PHYTO_JD_SLOPE=0
      DEF_PHYTO_JD_MR=0
      DEF_PHYTO_JD_MI=0
      DEF_PHYTO_JD_RATE=0
      DEF_PHYTO_LND_SM_RADIUS=0
      DEF_PHYTO_LND_SM_VAR=0
      DEF_PHYTO_LND_SM_MR=0
      DEF_PHYTO_LND_SM_MI=0
      DEF_PHYTO_LND_SM_RATE=0
      DEF_PHYTO_LND_TM_RADIUS=0
      DEF_PHYTO_LND_TM_VAR=0
      DEF_PHYTO_LND_TM_MR=0
      DEF_PHYTO_LND_TM_MI=0
      DEF_PHYTO_LND_TM_RATE=0
      DEF_SED_JD_SLOPE=0
      DEF_SED_JD_MR=0
      DEF_SED_JD_MI=0
      DEF_SED_JD_RATE=0
      DEF_SED_LND_SM_RADIUS=0
      DEF_SED_LND_SM_VAR=0
      DEF_SED_LND_SM_MR=0
      DEF_SED_LND_SM_MI=0
      DEF_SED_LND_SM_RATE=0
      DEF_SED_LND_TM_RADIUS=0
      DEF_SED_LND_TM_VAR=0
      DEF_SED_LND_TM_MR=0
      DEF_SED_LND_TM_MI=0
      DEF_SED_LND_TM_RATE=0
      DEF_FICEXTDATA_HYD=0
          
      DEF_TR=0
      DEF_PRESSION=0
      DEF_HR=0
      DEF_HA=0
      DEF_CHL=0
      DEF_IPHYTO_PROFIL=0
      DEF_CHL_GP_BG=0
      DEF_CHL_GP_DEEP=0
      DEF_CHL_GP_WIDTH=0
      DEF_FICPHYTO_USER=0
      DEF_SED_CSED=0
      DEF_YS_A440=0
      DEF_DET_A440=0
      DEF_FICPROFIL_ATM_RES=0
      DEF_FICPROFIL_SEA_RES=0
      
      DEF_SEA_IND=0
      DEF_WIND=0
      DEF_DIRSEA=0

      DEF_RHO_SURF=0
      DEF_IBOT_TYPE=0
      DEF_RHO_BOT=0	          	  
      DEF_PHI=0
      DEF_LEVEL_OUT=0
      DEF_USER_Z_OUT=0
      DEF_FICSOS_RES_VS_VZA=0
      DEF_VZA_OUT=0
      DEF_IGMAX=0
      DEF_FICSOS_RES_BIN=0   
   
   
   
C =========================================================
C --- Lecture des paramètres/arguments d'entrée et décodage
C
C     Le format de décodage des paramètres est libre (*)
C     pour éviter des problèmes de compilation avec certains
C     compilateurs comme gfortran.
C     Le contrôle de type est fait via l'affectation de la 
C     valeur à la variable (par exemple, une saisie float 
C     pour une variable en integer engendre un cas d'erreur).
C =========================================================           				
      
C --- Nombre d'arguments en entrée
      NBARGS = IARGC()

C --- Boucle de lecture des couples d'arguments       
      DO NPARAM=1,NBARGS, 2
         CALL GETARG(NPARAM,KEYWORD)
		
C ------- Contrôle élémentaire de validité du keyword	  
         IF (KEYWORD(1:1).NE.'-') GOTO 1000

C ------- Arguments propres au programme principal
	 IF (KEYWORD.EQ."-OSOAA.ResRoot") THEN
	     CALL GETARG (NPARAM+1,RESROOT)		!Nom du répertoire de travail.
	     DEF_RESROOT=1
	 ENDIF	

	 IF (KEYWORD.EQ."-OSOAA.Log") THEN
	     CALL GETARG (NPARAM+1,FICMAIN_LOG)		!Nom du fichier Trace global.
	     DEF_FICMAIN_LOG=1
	 ENDIF		 
		
	 IF (KEYWORD.EQ."-OSOAA.Wa") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)             !Longueur d'onde de simulation
	     READ(PROG_ARG,*,ERR=1050) WA_SIMU    	 !de la luminance
	     DEF_WA_SIMU=1
	 ENDIF	
	 
C ------- Arguments propres à la définition des angles		
	 IF (KEYWORD.EQ."-ANG.Rad.NbGauss") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)	        !Nb d'angles de Gauss pour les 
	     READ(PROG_ARG,*,ERR=1100) NBMU_GAUSS_LUM   !simulations de luminance 
	 ENDIF	 
	 	 
	 IF (KEYWORD.EQ."-ANG.Rad.UserAngFile") THEN    !Nom du fichier des angles
	     CALL GETARG (NPARAM+1,FICANGLES_USER_LUM)	!"utilisateur" pour les calculs		
	 ENDIF	  	  				!de luminance 
	 		 
	 IF (KEYWORD.EQ."-ANG.Thetas") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		!Angle zénithal solaire (degrés)
 	     READ(PROG_ARG,*,ERR=1110) TETAS  
             DEF_TETAS=1
	 ENDIF				

	 IF (KEYWORD.EQ."-ANG.Rad.ResFile") THEN	!Nom du fichier d'angles
	     CALL GETARG (NPARAM+1,FICANGLES_RES_LUM)	!pour les calculs de luminance	  
	     DEF_FICANGLES_RES_LUM=1			!(sans arborescence)	 
	 ENDIF	  					 
				
	 IF (KEYWORD.EQ."-ANG.Mie.NbGauss") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)  		!Nb d'angles de Gauss 
	     READ(PROG_ARG,*,ERR=1120) NBMU_GAUSS_MIE	!pour les calculs de Mie
	 ENDIF	 
	 	 
	 IF (KEYWORD.EQ."-ANG.Mie.UserAngFile") THEN    !Nom du fichier des angles
	     CALL GETARG (NPARAM+1,FICANGLES_USER_MIE)	!"utilisateurs" pour les calculs 
	 ENDIF	  					!de Mie (chemin complet)	 
	 	 	 
	 IF (KEYWORD.EQ."-ANG.Mie.ResFile") THEN	!Nom du fichier d'angles
	     CALL GETARG (NPARAM+1,FICANGLES_RES_MIE)	!pour les calculs de Mie	
	     DEF_FICANGLES_RES_MIE=1			!(sans arborescence)		
	 ENDIF	  	
	 	 
	 IF (KEYWORD.EQ."-ANG.Log") THEN
	     CALL GETARG (NPARAM+1,FICANGLES_LOG)	!Nom du fichier Trace des calculs 
	 ENDIF		 				!d'angles.


C ------- Arguments propres à la définition des modèles d'aérosols
	 IF (KEYWORD.EQ."-AER.Waref") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)     	!Longueur d'onde de référence 
	     READ(PROG_ARG,*,ERR=1200) WAREF_AOT 	!pour l'ep. opt aérosol.
	     DEF_WAREF_AOT=1
	 ENDIF


	 IF (KEYWORD.EQ."-AER.AOTref") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		!AOT pour la longueur d'onde 
	     READ(PROG_ARG,*,ERR=1210) AOT_REF		!de référence
	     DEF_AOT_REF=1
	 ENDIF

	 IF (KEYWORD.EQ."-AER.Tronca") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)    	!Indice de troncature de la 
	     READ(PROG_ARG,*,ERR=1220) ITRONC_AER	!fonction de phase des aérosols
	 ENDIF	

	 IF (KEYWORD.EQ."-AER.Model") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		!Type de modèle d'aérosols
	     READ(PROG_ARG,*,ERR=1230) IMOD_AER
	     DEF_IMOD_AER=1
	 ENDIF	 

	 IF (KEYWORD.EQ."-AER.MMD.MRwa") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		!Partie réelle de l'indice de 
	     READ(PROG_ARG,*,ERR=1240) RN_MMD_AER	!réfraction pour modèle aérosol 
	     DEF_RN_MMD_AER=1				!mono-mode à la longueur d'onde 
	 ENDIF	 					!de simulation de la luminance

	 IF (KEYWORD.EQ."-AER.MMD.MIwa") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		!Partie imaginaire de l'indice de  
	     READ(PROG_ARG,*,ERR=1241) IN_MMD_AER 	!réfraction pour modèle aérosol 
	     DEF_IN_MMD_AER=1				!mono-mode à la longueur d'onde 
	 ENDIF						!de simulation de la luminance

	 IF (KEYWORD.EQ."-AER.MMD.MRwaref") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		  !Partie réelle de l'indice  
	     READ(PROG_ARG,*,ERR=1242) RN_MMD_AER_WAREF	  !de réfraction pour modèle 
	     DEF_RN_MMD_AER_WAREF=1			  !aérosol mono-mode à la 
	 ENDIF	 					  !longueur d'onde de référence

	 IF (KEYWORD.EQ."-AER.MMD.MIwaref") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		  !Partie imaginaire de l'indice
	     READ(PROG_ARG,*,ERR=1243) IN_MMD_AER_WAREF   !de réfraction pour modèle 
	     DEF_IN_MMD_AER_WAREF=1			  !aérosol mono-mode à la 
	 ENDIF						  !longueur d'onde de référence
	 	 
	 IF (KEYWORD.EQ."-AER.MMD.SDtype") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		  !Indice de type de 
	     READ(PROG_ARG,*,ERR=1245) IGRANU_MMD_AER	  !granulométrie pour modèle 
	     DEF_IGRANU_MMD_AER=1			  !aérosol mono-mode
	 ENDIF

	 IF (KEYWORD.EQ."-AER.MMD.LNDradius") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)			!Rayon modal (microns) de  
	     READ(PROG_ARG,*,ERR=1246) LND_RADIUS_MMD_AER	!granulométrie LND pour 
	     DEF_LND_RADIUS_MMD_AER=1				!aérosol mono-mode
	 ENDIF
	 
	 IF (KEYWORD.EQ."-AER.MMD.LNDvar") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)			!Variance de  
	     READ(PROG_ARG,*,ERR=1247) LND_LNVAR_MMD_AER	!granulométrie LND pour 
	     DEF_LND_LNVAR_MMD_AER=1				!aérosol mono-mode
	 ENDIF
	 
	 IF (KEYWORD.EQ."-AER.MMD.JD.slope") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)			!Pente de loi de Junge
	     READ(PROG_ARG,*,ERR=1248) JD_SLOPE_MMD_AER		!pour modèle
	     DEF_JD_SLOPE_MMD_AER=1				!aérosol mono-mode
	 ENDIF
	 
	 IF (KEYWORD.EQ."-AER.MMD.JD.rmin") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)			!Rayon minimal de la loi de 
	     READ(PROG_ARG,*,ERR=1249) JD_RMIN_MMD_AER		!Junge (microns) pour modèle
	     DEF_JD_RMIN_MMD_AER=1				!aérosol mono-mode
	 ENDIF
	 
	 IF (KEYWORD.EQ."-AER.MMD.JD.rmax") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)			!Rayon maximal de la loi de 
	     READ(PROG_ARG,*,ERR=1250) JD_RMAX_MMD_AER		!Junge (microns) pour modèle
	     DEF_JD_RMAX_MMD_AER=1				!aérosol mono-mode
	 ENDIF							
	
	 
	 IF (KEYWORD.EQ."-AER.WMO.Model") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		!Indice de choix du modèle WMO
	     READ(PROG_ARG,*,ERR=1253) IMODELE_WMO 
	     DEF_IMODELE_WMO=1
	 ENDIF
	 
	 IF (KEYWORD.EQ."-AER.WMO.DL") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		!Pourcentage volumique des 
	     READ(PROG_ARG,*,ERR=1254) C_WMO_DL		!particules DL du modèle WMO
	     DEF_WMO_DL=1
	 ENDIF

	 IF (KEYWORD.EQ."-AER.WMO.WS") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)    	!Pourcentage volumique des 
	     READ(PROG_ARG,*,ERR=1254) C_WMO_WS		!particules WS du modèle WMO
	     DEF_WMO_WS=1
	 ENDIF
	 
	 IF (KEYWORD.EQ."-AER.WMO.OC") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)   		!Pourcentage volumique des 
	     READ(PROG_ARG,*,ERR=1254) C_WMO_OC 	!particules OC du modèle WMO
	     DEF_WMO_OC=1
	 ENDIF
	 
	 IF (KEYWORD.EQ."-AER.WMO.SO") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)   		!Pourcentage volumique des
	     READ(PROG_ARG,*,ERR=1254) C_WMO_SO		!particules SO du modèle WMO
	     DEF_WMO_SO=1
	 ENDIF	
	 
	 IF (KEYWORD.EQ."-AER.SF.Model") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		!Indice de choix du modèle S&F
	     READ(PROG_ARG,*,ERR=1255) IMODELE_SF  
	     DEF_IMODELE_SF=1
	 ENDIF 	
	 
	 IF (KEYWORD.EQ."-AER.SF.RH") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		!Humidité relative du modèle S&F
	     READ(PROG_ARG,*,ERR=1256) RH 
	     DEF_RH=1
	 ENDIF 	 

	 IF (KEYWORD.EQ."-AER.BMD.VCdef") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)   		!Indice de type de 
	     READ(PROG_ARG,*,ERR=1260) MODE_PARAM_BILND !description du mélange 
	     DEF_MODE_PARAM_BILND=1			!bimodal LND
	 ENDIF 	 	 

	 IF (KEYWORD.EQ."-AER.BMD.CoarseVC") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)   		!Concentration volumique 
	     READ(PROG_ARG,*,ERR=1270) USER_CV_COARSE	!du mode "coarse"
	     DEF_USER_CV_COARSE=1
	 ENDIF 	 

	 IF (KEYWORD.EQ."-AER.BMD.FineVC") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)   		!Concentration volumique 
	     READ(PROG_ARG,*,ERR=1271) USER_CV_FINE 	!du mode "fine"
	     DEF_USER_CV_FINE=1
	 ENDIF 	 	 

	 IF (KEYWORD.EQ."-AER.BMD.RAOT") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)	  	!AOT_coarse / AOT_tot 
	     READ(PROG_ARG,*,ERR=1280) RTAUct_WAREF	!pour la longueur d'onde 
	     DEF_RTAUct_WAREF=1				!de référence WAREF 
	 ENDIF 	 

	 IF (KEYWORD.EQ."-AER.BMD.CM.MRwa") THEN	
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Partie réelle de l'indice de		
	     READ(PROG_ARG,*,ERR=1290) BMD_CM_MRWA	!réfraction pour le mode "coarse" à
	     DEF_BMD_CM_MRWA=1				!la longueur d'onde de simulation	
	 ENDIF 						!de la luminance
	 
	 IF (KEYWORD.EQ."-AER.BMD.CM.MIwa") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Partie imaginaire de l'indice de
	     READ(PROG_ARG,*,ERR=1291) BMD_CM_MIWA	!réfraction pour le mode "coarse" à
	     DEF_BMD_CM_MIWA=1				!la longueur d'onde de simulation
	 ENDIF						!de la luminance 

	 IF (KEYWORD.EQ."-AER.BMD.CM.MRwaref") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Partie réelle de l'indice 
	     READ(PROG_ARG,*,ERR=1292) BMD_CM_MRWAREF	!de réfraction pour le mode 
	     DEF_BMD_CM_MRWAREF=1			!"coarse" à la longueur  
	 ENDIF 						!d'onde de référence
	 
	 IF (KEYWORD.EQ."-AER.BMD.CM.MIwaref") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Partie imaginaire de l'ind
	     READ(PROG_ARG,*,ERR=1293) BMD_CM_MIWAREF	!de réfraction pour le mode 
	     DEF_BMD_CM_MIWAREF=1			!"coarse" à la longueur 
	 ENDIF	 					!d'onde de référence

	 IF (KEYWORD.EQ."-AER.BMD.CM.SDradius") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Rayon modal de la LND
	     READ(PROG_ARG,*,ERR=1294) BMD_CM_RMODAL	!pour le mode "coarse"
	     DEF_BMD_CM_RMODAL=1
	 ENDIF 

	 IF (KEYWORD.EQ."-AER.BMD.CM.SDvar") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Variance de la 
	     READ(PROG_ARG,*,ERR=1295) BMD_CM_VAR	!LND pour le mode "coarse"
	     DEF_BMD_CM_VAR=1
	 ENDIF 
	 	 
	 IF (KEYWORD.EQ."-AER.BMD.FM.MRwa") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Partie réelle de l'indice de
	     READ(PROG_ARG,*,ERR=1300) BMD_FM_MRWA	!réfraction pour le mode "fine" à
	     DEF_BMD_FM_MRWA=1				!la longueur d'onde de simulation
	 ENDIF 						!de la luminance
	 
	 IF (KEYWORD.EQ."-AER.BMD.FM.MIwa") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Partie imaginaire de l'indice de
	     READ(PROG_ARG,*,ERR=1301) BMD_FM_MIWA	!réfraction pour le mode "fine" à
	     DEF_BMD_FM_MIWA=1				!la longueur d'onde de simulation
	 ENDIF	 					!de la luminance

	 IF (KEYWORD.EQ."-AER.BMD.FM.MRwaref") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Partie réelle de l'indice 
	     READ(PROG_ARG,*,ERR=1302) BMD_FM_MRWAREF	!de réfraction pour le mode
	     DEF_BMD_FM_MRWAREF=1			!"fine" à la longueur
	 ENDIF 	 					!d'onde de référence
	 
	 IF (KEYWORD.EQ."-AER.BMD.FM.MIwaref") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Partie imaginaire de l'ind
	     READ(PROG_ARG,*,ERR=1303) BMD_FM_MIWAREF	!de réfraction pour le mode
	     DEF_BMD_FM_MIWAREF=1			!"fine" à la longueur
	 ENDIF	 					!d'onde de référence	 

	 IF (KEYWORD.EQ."-AER.BMD.FM.SDradius") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Rayon modal de la LND
	     READ(PROG_ARG,*,ERR=1304) BMD_FM_RMODAL 	!pour le mode "fine" 
	     DEF_BMD_FM_RMODAL=1
	 ENDIF 

	 IF (KEYWORD.EQ."-AER.BMD.FM.SDvar") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Variance de la 
	     READ(PROG_ARG,*,ERR=1305) BMD_FM_VAR	!LND pour le mode "fine"	 
	     DEF_BMD_FM_VAR=1
	 ENDIF 	

	 IF (KEYWORD.EQ."-AER.ExtData") THEN		!Fichier de fonctions de phase 
	     CALL GETARG (NPARAM+1,FICEXTDATA_AER) 	!de source externe (utilisateur)
	     DEF_FICEXTDATA_AER=1
	 ENDIF 	 

	 IF (KEYWORD.EQ."-AER.MieLog") THEN
	     CALL GETARG (NPARAM+1,FICAER_LOG_MIE) 	!Fichier Trace des calculs de Mie   
	 ENDIF	 	  	 

         IF (KEYWORD.EQ."-AER.DirMie") THEN
	     CALL GETARG (NPARAM+1,DIRMIE_AER)		!Répertoire de stockage des fichiers
	     DEF_DIRMIE_AER=1				!de Mie (produits pour les aérosols)
	 ENDIF
	 
	 IF (KEYWORD.EQ."-AER.ResFile") THEN
	     CALL GETARG (NPARAM+1,FICGRANU_AER)	!Nom du fichier FICGRANU_AER 
	     DEF_FICGRANU_AER=1				!de matrice de phase des aérosols.
         ENDIF
	 
	 IF (KEYWORD.EQ."-AER.Log") THEN
	     CALL GETARG (NPARAM+1,FICAER_LOG)		!Nom du fichier Trace des calculs 
	 ENDIF						!de matrice de phase des aérosols.
       

C ------- Arguments propres à la définition des modèles de hydrosols	 	
	 IF (KEYWORD.EQ."-PHYTO.ResFile") THEN
	     CALL GETARG (NPARAM+1,FICGRANU_PHYTO)	!Nom du fichier FICGRANU_PHYTO 
	     DEF_FICGRANU_PHYTO=1			!de matrice de phase du 
         ENDIF						!phytoplancton.    
	 
	 IF (KEYWORD.EQ."-MLP.ResFile") THEN
	     CALL GETARG (NPARAM+1,FICGRANU_MLP)	!Nom du fichier FICGRANU_MLP
	     DEF_FICGRANU_MLP=1				!de matrice de phase des 
         ENDIF						!"Mineral Like Particles".    

	 IF (KEYWORD.EQ."-HYD.Log") THEN
	     CALL GETARG (NPARAM+1,FICHYD_LOG)		!Nom du fichier Trace des calculs 
	 ENDIF						!de matrice de phase des hydrosols

         IF (KEYWORD.EQ."-HYD.DirMie") THEN
	     CALL GETARG (NPARAM+1,DIRMIE_HYD)		!Répertoire de stockage des fichiers
	     DEF_DIRMIE_HYD=1				!de Mie produits pour les hydrosols
	 ENDIF
	 	 
         IF (KEYWORD.EQ."-HYD.MieLog") THEN
	     CALL GETARG (NPARAM+1,FICHYD_LOG_MIE)	!Fichier Trace des calculs de Mie   
	 ENDIF	

	 IF (KEYWORD.EQ."-HYD.Model") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		!Type de caractérisation
	     READ(PROG_ARG,*,ERR=1330) IMOD_HYD		!des hydrosols
	     DEF_IMOD_HYD=1
	 ENDIF	         

	 IF (KEYWORD.EQ."-PHYTO.JD.slope") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		!Pente de la loi de Junge 
	     READ(PROG_ARG,*,ERR=1340) PHYTO_JD_SLOPE	!pour le phytoplancton 
	     DEF_PHYTO_JD_SLOPE=1				
	 ENDIF	
	 
	 IF (KEYWORD.EQ."-PHYTO.JD.rmin") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		!Rayon min de la loi de Junge
	     READ(PROG_ARG,*,ERR=1341) PHYTO_JD_RMIN	!pour le phytoplancton
	 ENDIF						
	 
	 IF (KEYWORD.EQ."-PHYTO.JD.rmax") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		!Rayon max de la loi de Junge
	     READ(PROG_ARG,*,ERR=1342) PHYTO_JD_RMAX	!pour le phytoplancton
	 ENDIF							 	
	  
	 IF (KEYWORD.EQ."-PHYTO.JD.MRwa") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		!Partie réelle de l'indice  
	     READ(PROG_ARG,*,ERR=1343) PHYTO_JD_MR   	!de réfraction de la loi de Junge
	     DEF_PHYTO_JD_MR=1				!pour le phytoplancton
	 ENDIF		 				  

	 IF (KEYWORD.EQ."-PHYTO.JD.MIwa") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		!Partie imaginaire de l'indice
	     READ(PROG_ARG,*,ERR=1344) PHYTO_JD_MI   	!de réfraction de la loi de Junge
	     DEF_PHYTO_JD_MI=1				!pour le phytoplancton
	 ENDIF		 

	 IF (KEYWORD.EQ."-PHYTO.JD.rate") THEN		!Proportion relative en nombre 
	     CALL GETARG (NPARAM+1,PROG_ARG)		!de particules de la 		
	     READ(PROG_ARG,*,ERR=1345) PHYTO_JD_RATE	!distribution de Junge
	     DEF_PHYTO_JD_RATE=1			!devant les autres modes 
	 ENDIF		 				!pour le phytoplancton 
	 
	 IF (KEYWORD.EQ."-PHYTO.LND.SM.SDradius") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !Rayon modal de la
	     READ(PROG_ARG,*,ERR=1350) PHYTO_LND_SM_RADIUS    !distribution LND 
	     DEF_PHYTO_LND_SM_RADIUS=1			      !secondaire pour  
	 ENDIF						      !le phytoplancton
	 	 
	 IF (KEYWORD.EQ."-PHYTO.LND.SM.SDvar") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !Variance de la distribution
	     READ(PROG_ARG,*,ERR=1351) PHYTO_LND_SM_VAR       !LND secondaire 
	     DEF_PHYTO_LND_SM_VAR=1			      !pour le phytoplancton
	 ENDIF	
	  
	 IF (KEYWORD.EQ."-PHYTO.LND.SM.MRwa") THEN	      !Partie réelle de l'indice
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !de réfraction de la 
	     READ(PROG_ARG,*,ERR=1352) PHYTO_LND_SM_MR 	      !distribution LND secondaire
	     DEF_PHYTO_LND_SM_MR=1			      !pour le phytoplancton	
	 ENDIF		 

	 IF (KEYWORD.EQ."-PHYTO.LND.SM.MIwa") THEN	      !Partie imaginaire de 
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !l'indice de réfraction de la
	     READ(PROG_ARG,*,ERR=1353) PHYTO_LND_SM_MI 	      !distribution LND secondaire 
	     DEF_PHYTO_LND_SM_MI=1			      !pour le phytoplancton
	 ENDIF		 		

	 IF (KEYWORD.EQ."-PHYTO.LND.SM.rate") THEN	      !Proportion relative en nb
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !de particules de la
	     READ(PROG_ARG,*,ERR=1354) PHYTO_LND_SM_RATE      !distribution LND secondaire
	     DEF_PHYTO_LND_SM_RATE=1			      !devant les autres modes
	 ENDIF		 	 			      !pour le phytoplancton
 
	 IF (KEYWORD.EQ."-PHYTO.LND.TM.SDradius") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !Rayon modal de la
	     READ(PROG_ARG,*,ERR=1355) PHYTO_LND_TM_RADIUS    !distribution LND
	     DEF_PHYTO_LND_TM_RADIUS=1			      !tertiaire pour  
	 ENDIF						      !le phytoplancton

	 IF (KEYWORD.EQ."-PHYTO.LND.TM.SDvar") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !Variance de la distribution
	     READ(PROG_ARG,*,ERR=1356) PHYTO_LND_TM_VAR	      !LND tertiaire
	     DEF_PHYTO_LND_TM_VAR=1			      !pour le phytoplancton
	 ENDIF		    
	  
	 IF (KEYWORD.EQ."-PHYTO.LND.TM.MRwa") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !Partie réelle de l'indice
	     READ(PROG_ARG,*,ERR=1357) PHYTO_LND_TM_MR        !de réfraction de la 
	     DEF_PHYTO_LND_TM_MR=1			      !distribution LND tertiaire
	 ENDIF		 				      !pour le phytoplancton

	 IF (KEYWORD.EQ."-PHYTO.LND.TM.MIwa") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !Partie imaginaire de 
	     READ(PROG_ARG,*,ERR=1358) PHYTO_LND_TM_MI 	      !l'indice de réfraction de 
	     DEF_PHYTO_LND_TM_MI=1			      !la distribution LND tertiaire
	 ENDIF		 				      !pour le phytoplancton
     
	 IF (KEYWORD.EQ."-PHYTO.LND.TM.rate") THEN	      !Proportion relative en nb
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !de particules de la
	     READ(PROG_ARG,*,ERR=1359) PHYTO_LND_TM_RATE      !distribution LND tertiaire
	     DEF_PHYTO_LND_TM_RATE=1			      !devant les autres modes
	 ENDIF		 				      !pour le phytoplancton

	 IF (KEYWORD.EQ."-SED.JD.slope") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !Pente de la loi de Junge 
	     READ(PROG_ARG,*,ERR=1360) SED_JD_SLOPE	      !pour les sédiments
	     DEF_SED_JD_SLOPE=1
	 ENDIF	
	 
	 IF (KEYWORD.EQ."-SED.JD.rmin") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !Rayon min de la loi de Junge
	     READ(PROG_ARG,*,ERR=1361) SED_JD_RMIN	      !pour les sédiments
	 ENDIF								
	 	 
	 IF (KEYWORD.EQ."-SED.JD.rmax") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !Rayon max de la loi de Junge
	     READ(PROG_ARG,*,ERR=1362) SED_JD_RMAX	      !pour les sédiments
	 ENDIF							
	  
	 IF (KEYWORD.EQ."-SED.JD.MRwa") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		  !Partie réelle de l'indice  
	     READ(PROG_ARG,*,ERR=1363) SED_JD_MR    	  !de réfraction de la loi de Junge
	     DEF_SED_JD_MR=1				  !pour les sédiments
	 ENDIF		 

	 IF (KEYWORD.EQ."-SED.JD.MIwa") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		  !Partie imaginaire de l'indice
	     READ(PROG_ARG,*,ERR=1364) SED_JD_MI          !de réfraction de la loi de Junge
	     DEF_SED_JD_MI=1				  !pour les sédiments
	 ENDIF		 

	 IF (KEYWORD.EQ."-SED.JD.rate") THEN		  !Proportion relative en nombre
	     CALL GETARG (NPARAM+1,PROG_ARG)		  !de particules de la
	     READ(PROG_ARG,*,ERR=1365) SED_JD_RATE	  !distribution de Junge
	     DEF_SED_JD_RATE=1				  !devant les autres modes
	 ENDIF		 				  !pour les sédiments 

	 IF (KEYWORD.EQ."-SED.LND.SM.SDradius") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)			!Rayon modal de la 
	     READ(PROG_ARG,*,ERR=1370) SED_LND_SM_RADIUS	!distribution LND 
	     DEF_SED_LND_SM_RADIUS=1				!secondaire pour
	 ENDIF							!pour les sédiments
	 	 
	 IF (KEYWORD.EQ."-SED.LND.SM.SDvar") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !Variance de la distribution
	     READ(PROG_ARG,*,ERR=1371) SED_LND_SM_VAR         !LND secondaire 
	     DEF_SED_LND_SM_VAR=1			      !pour les sédiments
	 ENDIF	
	  
	 IF (KEYWORD.EQ."-SED.LND.SM.MRwa") THEN	      !Partie réelle de l'indice
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !de réfraction de la 
	     READ(PROG_ARG,*,ERR=1372) SED_LND_SM_MR 	      !distribution LND secondaire
	     DEF_SED_LND_SM_MR=1			      !pour les sédiments
	 ENDIF		 

	 IF (KEYWORD.EQ."-SED.LND.SM.MIwa") THEN	      !Partie imaginaire de l'indice
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !de réfraction de la
	     READ(PROG_ARG,*,ERR=1373) SED_LND_SM_MI 	      !distribution LND secondaire
	     DEF_SED_LND_SM_MI=1			      !pour les sédiments
	 ENDIF		 

	 IF (KEYWORD.EQ."-SED.LND.SM.rate") THEN	      !Proportion relative en nb
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !de particules de la
	     READ(PROG_ARG,*,ERR=1374) SED_LND_SM_RATE	      !distribution LND secondaire
	     DEF_SED_LND_SM_RATE=1		      	      !devant les autres modes
	 ENDIF		 				      !pour les sédiments
	   	 
	 IF (KEYWORD.EQ."-SED.LND.TM.SDradius") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !Rayon modal de la
	     READ(PROG_ARG,*,ERR=1375) SED_LND_TM_RADIUS      !distribution LND
	     DEF_SED_LND_TM_RADIUS=1 			      !tertiaire pour  
	 ENDIF		 				      !pour les sédiments
	 	 
	 IF (KEYWORD.EQ."-SED.LND.TM.SDvar") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !Variance de la distribution
	     READ(PROG_ARG,*,ERR=1376) SED_LND_TM_VAR	      !LND tertiaire
	     DEF_SED_LND_TM_VAR=1 			      !pour les sédiments
	 ENDIF			
	  
	 IF (KEYWORD.EQ."-SED.LND.TM.MRwa") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !Partie réelle de l'indice
	     READ(PROG_ARG,*,ERR=1377) SED_LND_TM_MR 	      !de réfraction de la 
	     DEF_SED_LND_TM_MR=1			      !distribution LND tertiaire
	 ENDIF		 				      !pour les sédiments

	 IF (KEYWORD.EQ."-SED.LND.TM.MIwa") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !Partie imaginaire de l'indice
	     READ(PROG_ARG,*,ERR=1378) SED_LND_TM_MI 	      !de réfraction de la 
	     DEF_SED_LND_TM_MI=1			      !distribution LND tertiaire
	 ENDIF		 				      !pour les sédiments 

	 IF (KEYWORD.EQ."-SED.LND.TM.rate") THEN	      !Proportion relative en nb
	     CALL GETARG (NPARAM+1,PROG_ARG)		      !de particules de la
	     READ(PROG_ARG,*,ERR=1379) SED_LND_TM_RATE	      !distribution LND tertiaire
	     DEF_SED_LND_TM_RATE=1			      !devant les autres modes
	 ENDIF		 				      !pour les sédiments	  
	 	 
	 IF (KEYWORD.EQ."-HYD.ExtData") THEN		!Fichier de fonctions de phase 
	     CALL GETARG (NPARAM+1,FICEXTDATA_HYD)	!de source externe (utilisateur) 
	     DEF_FICEXTDATA_HYD=1
	 ENDIF 	 
	 	 
	 
C ------- Arguments propres à la définition des profils atmosphérique et marin	
	 IF (KEYWORD.EQ."-AP.MOT") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Epaisseur optique moléculaire
	     READ(PROG_ARG,*,ERR=1500) TR
	     DEF_TR=1
	 ENDIF			

	 IF (KEYWORD.EQ."-AP.Pressure") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Pression atmosphérique 
	     READ(PROG_ARG,*,ERR=1510) PRESSION  	!au niveau de la mer (mbar)
	     DEF_PRESSION=1
	 ENDIF

	 IF (KEYWORD.EQ."-AP.HR") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Echelle de hauteur 
	     READ(PROG_ARG,*,ERR=1520) HR		!moléculaire (km)
	     DEF_HR=1
	 ENDIF

	 IF (KEYWORD.EQ."-AP.HA") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Echelle de hauteur des 
	     READ(PROG_ARG,*,ERR=1530) HA		!aérosols (km)
	     DEF_HA=1
	 ENDIF

	 IF (KEYWORD.EQ."-SEA.Depth") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Profondeur de la mer (m)
	     READ(PROG_ARG,*,ERR=1540) SEA_DEPTH
	 ENDIF	
	 
	 IF (KEYWORD.EQ."-PHYTO.Chl") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Concentration de la chlorophylle 
	     READ(PROG_ARG,*,ERR=1550) CHL		!en surface (mg/m3)
	     DEF_CHL=1
	 ENDIF	
	 
	 IF (KEYWORD.EQ."-PHYTO.ProfilType") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)    	!Indice de type de profil 
	     READ(PROG_ARG,*,ERR=1560) IPHYTO_PROFIL	!du phytoplancton
	     DEF_IPHYTO_PROFIL=1
	 ENDIF	
	 
	 IF (KEYWORD.EQ."-PHYTO.GP.Chlbg") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Constante de biomasse du profil 
	     READ(PROG_ARG,*,ERR=1570) CHL_GP_BG	!gaussien de chlorophylle (mg/m3)
	     DEF_CHL_GP_BG=1
	 ENDIF	

	 IF (KEYWORD.EQ."-PHYTO.GP.Deep") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Profondeur max du profil   
	     READ(PROG_ARG,*,ERR=1571) CHL_GP_DEEP	!gaussien de chlorophylle 
	     DEF_CHL_GP_DEEP=1				!(m)
	 ENDIF	
   
	 IF (KEYWORD.EQ."-PHYTO.GP.Width") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG) 		!Largeur du pic du profil 
	     READ(PROG_ARG,*,ERR=1572) CHL_GP_WIDTH	!gaussien de chlorophylle
	     DEF_CHL_GP_WIDTH=1				! (m)
	 ENDIF
   
	 IF (KEYWORD.EQ."-PHYTO.Userfile") THEN
	     CALL GETARG (NPARAM+1,FICPHYTO_USER)  	!Nom du fichier utilisateur 
	     DEF_FICPHYTO_USER=1		   	!donnant le profil de chlorophylle
	 ENDIF					   	!(chemin complet)
	 
	 IF (KEYWORD.EQ."-SED.Csed") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)          	! Concentration des sédiments 
	     READ(PROG_ARG,*,ERR=1580) SED_CSED 	! en surface (mg/litre)
	     DEF_SED_CSED=1
	 ENDIF		 
	 
	 IF (KEYWORD.EQ."-YS.Abs440") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)            !Coeff d'absorption de la substance
	     READ(PROG_ARG,*,ERR=1590) YS_A440   	!jaune à 440 nm (m-1)
	     DEF_YS_A440=1
	 ENDIF		 
	 	 	 
	 IF (KEYWORD.EQ."-YS.Swa") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)            !Coeff de conversion spectrale       				     
	     READ(PROG_ARG,*,ERR=1591) YS_SWA    	!de l'absorption de la substance
	 ENDIF		 				!jaune

	 IF (KEYWORD.EQ."-DET.Abs440") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)            !Coeff d'absorption des détritus 
	     READ(PROG_ARG,*,ERR=1592) DET_A440  	!à 440 nm (m-1)
	     DEF_DET_A440=1
	 ENDIF		 
	 	 	 
	 IF (KEYWORD.EQ."-DET.Swa") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)            !Coeff de conversion spectrale
	     READ(PROG_ARG,*,ERR=1593) DET_SWA   	!de l'absorption des détritus
	 ENDIF			

	 IF (KEYWORD.EQ."-PROFILE_ATM.ResFile") THEN
	     CALL GETARG (NPARAM+1,FICPROFIL_ATM_RES)  	!Nom du fichier résultat pour la
	     DEF_FICPROFIL_ATM_RES=1		        !définition du profil 
	 ENDIF	  				        !atmosphérique (chemin complet)

	 IF (KEYWORD.EQ."-PROFILE_SEA.ResFile") THEN
	     CALL GETARG (NPARAM+1,FICPROFIL_SEA_RES)   !Nom du fichier résultat pour la 
	     DEF_FICPROFIL_SEA_RES=1		        !définition du profil marin 
	 ENDIF	  				        !(chemin complet)
	 	 	 
	 IF (KEYWORD.EQ."-PROFILE.Log") THEN
	     CALL GETARG (NPARAM+1,FICPROFIL_LOG)       !Nom du fichier Trace des calculs 
	 ENDIF					        !de profils.


C ------- Arguments propres à la définition de l'interface air / mer
	 IF (KEYWORD.EQ."-SEA.Ind") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)
	     READ(PROG_ARG,*,ERR=1600) SEA_IND   	!Indice de réfraction 
	     DEF_SEA_IND=1				!de l'eau de mer
	 ENDIF
	  
	 IF (KEYWORD.EQ."-SEA.Wind") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)
	     READ(PROG_ARG,*,ERR=1610) WIND   		!Vitesse du vent en surface (m/s)
	     DEF_WIND=1
	 ENDIF	 

	 IF (KEYWORD.EQ."-SEA.Dir") THEN	 !Nom du répertoire de stockage des  
	     CALL GETARG (NPARAM+1,DIRSEA)  	 !fichiers SURFACE résultats 
	     DEF_DIRSEA=1			 !(chemin complet)
	 ENDIF		
	  
	 IF (KEYWORD.EQ."-SEA.Log") THEN
	     CALL GETARG (NPARAM+1,FICSURF_LOG)	 !Nom du fichier Trace des calculs de 
	 ENDIF					 !matrices d'interface air / mer. 


C ------- Arguments propres aux calculs d'Ordres Successifs et à la production
C         des fichiers résultats
	 IF (KEYWORD.EQ."-SEA.SurfAlb") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)
	     READ(PROG_ARG,*,ERR=1680) RHO_SURF 	!Albédo de surface
	     DEF_RHO_SURF=1
	 ENDIF	 

	 IF (KEYWORD.EQ."-SEA.BotType") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)
	     READ(PROG_ARG,*,ERR=1690) IBOT_TYPE 	!Indice de type de fond
	     DEF_IBOT_TYPE=1
	 ENDIF
	 
	 IF (KEYWORD.EQ."-SEA.BotAlb") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)
	     READ(PROG_ARG,*,ERR=1695) RHO_BOT 		!Albédo de fond
	     DEF_RHO_BOT=1
	 ENDIF

	 IF (KEYWORD.EQ."-OSOAA.View.Phi") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)
	     READ(PROG_ARG,*,ERR=1700) PHI   		!Azimut relatif (degrés)
	     DEF_PHI=1			
	 ENDIF

	 IF (KEYWORD.EQ."-OSOAA.View.Level") THEN	
	     CALL GETARG (NPARAM+1,PROG_ARG)		!Indice du type de niveau de
	     READ(PROG_ARG,*,ERR=1710) LEVEL_OUT	!sortie 
	     DEF_LEVEL_OUT=1				
	 ENDIF						


	 IF (KEYWORD.EQ."-OSOAA.View.Z") THEN	
	     CALL GETARG (NPARAM+1,PROG_ARG)		!Altitude ou profondeur (mètres)
	     READ(PROG_ARG,*,ERR=1710) USER_Z_OUT 	!pour la représentation de sortie 
	     DEF_USER_Z_OUT=1				!de la luminance en fct de l'angle
	 ENDIF						!zénithal de visée (Phi et Z fixés)

	 IF (KEYWORD.EQ."-OSOAA.ResFile.vsVZA") THEN    !Nom du fichier résultat pour la
	     CALL GETARG (NPARAM+1,FICSOS_RES_VS_VZA)	!représentation de sortie de la 
	     DEF_FICSOS_RES_VS_VZA=1		        !luminance en fct de l'angle	
	 ENDIF						!zénithal de visée (Phi et Z fixés)
	 
	 IF (KEYWORD.EQ."-OSOAA.View.VZA") THEN		!Angle zénithal de visée (degrés)
	     CALL GETARG (NPARAM+1,PROG_ARG)   		!pour la représentation de sortie
	     READ(PROG_ARG,*,ERR=1720) VZA_OUT		!de la luminance en fct de la
	     DEF_VZA_OUT=1		 		!profondeur (ou altitude) 
	 ENDIF						!(pour Phi et VZA fixés)

	 IF (KEYWORD.EQ."-OSOAA.ResFile.vsZ") THEN	!Nom du fichier résultat pour la
	     CALL GETARG (NPARAM+1,FICSOS_RES_VS_Z)	!représentation de sortie de la 
	      					        !luminance en fct de la profondeur
	 ENDIF						!(ou altitude) (Phi et VZA fixés)

	 IF (KEYWORD.EQ."-OSOAA.ResFile.Adv.Up") THEN	!Nom du fichier résultats avancés 
	     CALL GETARG (NPARAM+1,FICSOS_RES_ADV_UP)	!pour la représentation de la   
	 ENDIF   	        			!luminance montante en fct de la 
	 						!profondeur (ou altitude) 
	 						!ET de l'angle de visée (Phi fixé).
	 IF (KEYWORD.EQ."-OSOAA.ResFile.Adv.Down") THEN	!Nom du fichier résultats avancés 
	     CALL GETARG (NPARAM+1,FICSOS_RES_ADV_DOWN)	!pour la représentation de la   
	 ENDIF   	        			!luminance descendante en fct de 
	 						!la profondeur	(ou altitude) 
	 						!ET de l'angle devisée (Phi fixé).


	 IF (KEYWORD.EQ."-SOS.Log") THEN
	     CALL GETARG (NPARAM+1,FICSOS_LOG)	 !Nom du fichier Trace des calculs de 
	 ENDIF					 !luminance par Ordres Successifs     

         IF (KEYWORD.EQ."-SOS.IGmax") THEN
	     CALL GETARG (NPARAM+1,PROG_ARG)	 	!Ordre maximal d'interaction
             READ(PROG_ARG,*,ERR=1730) IGMAX
             DEF_IGMAX=1
         ENDIF

	 IF (KEYWORD.EQ."-SOS.ResFile.Bin") THEN
	     CALL GETARG (NPARAM+1,FICSOS_RES_BIN)	!Nom du fichier OS binaire.
	     DEF_FICSOS_RES_BIN=1
	 ENDIF
	 	 		
      ENDDO	!Fin de boucle de lecture des arguments




      
C =========================================================
C --- Préparation de l'arborescence de travail
C =========================================================           				

C ---- Contrôle de la définition de l'espace de travail
      IF (DEF_RESROOT.EQ.0) GOTO 1990
      
      LDIR = INDEX(RESROOT,' ') - 1
      IF((LDIR.LE.0).OR.(LDIR.GT.CTE_LENDIR)) GOTO 1991

C ---- Définition des répertoires de résultats standards et avancés     
      LDIRSTD = LDIR + 17      
      IF(LDIRSTD.GT.CTE_LENDIR) GOTO 1992
      RESROOT_STD(1:LDIRSTD) = RESROOT(1:LDIR)//'/'//'Standard_outputs'
     
      LDIRADV = LDIR + 17     
      IF(LDIRADV.GT.CTE_LENDIR) GOTO 1993 
      RESROOT_ADV(1:LDIRADV) = RESROOT(1:LDIR)//'/'//'Advanced_outputs'
    

C ---- Génération des répertoires de résultats standards et avancés         
      COMMAND='mkdir -p '//RESROOT_STD(1:LDIRSTD)
      LCOM = INDEX(COMMAND(10:CTE_LENCOM),' ') - 1
      IF((LCOM.LE.0).OR.(LCOM.GT.CTE_LENCOM)) GOTO 1995
      CALL SYSTEM(COMMAND)
      
      COMMAND='mkdir -p '//RESROOT_ADV(1:LDIRADV)
      LCOM = INDEX(COMMAND(10:CTE_LENCOM),' ') - 1
      IF((LCOM.LE.0).OR.(LCOM.GT.CTE_LENCOM)) GOTO 1995
      CALL SYSTEM(COMMAND)

C ---- Position de l'arborescence complète des différents fichiers

       !Fichier Trace principal
       IF (DEF_FICMAIN_LOG.EQ.0) THEN
	  TRACE=.FALSE.
       ELSE
          TRACE=.TRUE.
       
          LFIC = INDEX(FICMAIN_LOG,' ') - 1
          IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
          FICMAIN_LOG = RESROOT_ADV(1:LDIRADV)//'/'//FICMAIN_LOG(1:LFIC)
       		
       ENDIF ! Fin Trace 
       
       
       !Fichier Angles pour luminances
       LFIC = INDEX(FICANGLES_RES_LUM,' ') - 1
       IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
       FICANGLES_RES_LUM = RESROOT_ADV(1:LDIRADV)//'/'
     &                    //FICANGLES_RES_LUM(1:LFIC)
         
       !Fichier Angles pour calculs de Mie
       LFIC = INDEX(FICANGLES_RES_MIE,' ') - 1
       IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
       FICANGLES_RES_MIE = RESROOT_ADV(1:LDIRADV)//'/'
     &                    //FICANGLES_RES_MIE(1:LFIC)

       !Fichier Trace Angles
       IF (FICANGLES_LOG.NE."NO_LOG_FILE") THEN
          LFIC = INDEX(FICANGLES_LOG,' ') - 1
          IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
          FICANGLES_LOG = RESROOT_ADV(1:LDIRADV)//'/'
     &                    //FICANGLES_LOG(1:LFIC)
       ENDIF 
        
       !Fichier matrice de phase des aérosols
       LFIC = INDEX(FICGRANU_AER,' ') - 1
       IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
       FICGRANU_AER = RESROOT_ADV(1:LDIRADV)//'/'//FICGRANU_AER(1:LFIC)

       !Fichier Trace des calculs de Mie des aérosols
       IF (FICAER_LOG_MIE.NE."NO_LOG_FILE") THEN
          LFIC = INDEX(FICAER_LOG_MIE,' ') - 1
          IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
          FICAER_LOG_MIE = RESROOT_ADV(1:LDIRADV)//'/'
     &                    //FICAER_LOG_MIE(1:LFIC)
       ENDIF 

       !Fichier Trace des calculs de phase des aérosols
       IF (FICAER_LOG.NE."NO_LOG_FILE") THEN
          LFIC = INDEX(FICAER_LOG,' ') - 1
          IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
          FICAER_LOG = RESROOT_ADV(1:LDIRADV)//'/'//FICAER_LOG(1:LFIC)
       ENDIF 
              
       !Fichier matrice de phase du phytoplancton
       LFIC = INDEX(FICGRANU_PHYTO,' ') - 1
       IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
       FICGRANU_PHYTO = RESROOT_ADV(1:LDIRADV)//'/'
     &                  //FICGRANU_PHYTO(1:LFIC)

       !Fichier matrice de phase des Mineral-Like Particles
       LFIC = INDEX(FICGRANU_MLP,' ') - 1
       IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
       FICGRANU_MLP = RESROOT_ADV(1:LDIRADV)//'/'//FICGRANU_MLP(1:LFIC)

       !Fichier Trace des calculs de Mie des hydrosols
       IF (FICHYD_LOG_MIE.NE."NO_LOG_FILE") THEN
          LFIC = INDEX(FICHYD_LOG_MIE,' ') - 1
          IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
          FICHYD_LOG_MIE = RESROOT_ADV(1:LDIRADV)//'/'
     &                    //FICHYD_LOG_MIE(1:LFIC)
       ENDIF 

       !Fichier Trace des calculs de phase des hydrosols
       IF (FICHYD_LOG.NE."NO_LOG_FILE") THEN
          LFIC = INDEX(FICHYD_LOG,' ') - 1
          IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
          FICHYD_LOG = RESROOT_ADV(1:LDIRADV)//'/'//FICHYD_LOG(1:LFIC)
       ENDIF     
          	
       !Fichier de profil atmosphérique
       LFIC = INDEX(FICPROFIL_ATM_RES,' ') - 1
       IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
       FICPROFIL_ATM_RES = RESROOT_ADV(1:LDIRADV)//'/'
     &                    //FICPROFIL_ATM_RES(1:LFIC)	
	
       !Fichier de profil marin
       LFIC = INDEX(FICPROFIL_SEA_RES,' ') - 1
       IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
       FICPROFIL_SEA_RES = RESROOT_ADV(1:LDIRADV)//'/'
     &                    //FICPROFIL_SEA_RES(1:LFIC)

       !Fichier Trace des calculs de profils
       IF (FICPROFIL_LOG.NE."NO_LOG_FILE") THEN
          LFIC = INDEX(FICPROFIL_LOG,' ') - 1
          IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
          FICPROFIL_LOG = RESROOT_ADV(1:LDIRADV)//'/'
     &	  		 //FICPROFIL_LOG(1:LFIC)
       ENDIF     

       !Fichier Trace des calculs de matrices de surface
       IF (FICSURF_LOG.NE."NO_LOG_FILE") THEN
          LFIC = INDEX(FICSURF_LOG,' ') - 1
          IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
          FICSURF_LOG = RESROOT_ADV(1:LDIRADV)//'/'//FICSURF_LOG(1:LFIC)
       ENDIF     
              
       !Fichier binaire de luminance en séries de Fourier
       LFIC = INDEX(FICSOS_RES_BIN,' ') - 1
       IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
       FICSOS_RES_BIN = RESROOT_ADV(1:LDIRADV)//'/'
     &                  //FICSOS_RES_BIN(1:LFIC)
     
       !Fichier ascii de luminance en fct de l'angle zénithal
       LFIC = INDEX(FICSOS_RES_VS_VZA,' ') - 1
       IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
       FICSOS_RES_VS_VZA = RESROOT_STD(1:LDIRSTD)//'/'
     &                     //FICSOS_RES_VS_VZA(1:LFIC)
     
       !Fichier ascii de luminance en fct du niveau
       IF (FICSOS_RES_VS_Z.NE."NO_RES_VS_Z_FILE") THEN
           LFIC = INDEX(FICSOS_RES_VS_Z,' ') - 1
           IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
           FICSOS_RES_VS_Z = RESROOT_STD(1:LDIRSTD)//'/'
     &                       //FICSOS_RES_VS_Z(1:LFIC)      
       ENDIF     
      
       !Fichier ascii de luminance en fct du niveau et de l'angle zénithal
       IF (FICSOS_RES_ADV_UP.NE."NO_ADVANCED_FILE") THEN
           LFIC = INDEX(FICSOS_RES_ADV_UP,' ') - 1
           IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
           FICSOS_RES_ADV_UP = RESROOT_ADV(1:LDIRADV)//'/'
     &                      //FICSOS_RES_ADV_UP(1:LFIC)      
       ENDIF     

       IF (FICSOS_RES_ADV_DOWN.NE."NO_ADVANCED_FILE") THEN
           LFIC = INDEX(FICSOS_RES_ADV_DOWN,' ') - 1
           IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
           FICSOS_RES_ADV_DOWN = RESROOT_ADV(1:LDIRADV)//'/'
     &                      //FICSOS_RES_ADV_DOWN(1:LFIC)      
       ENDIF  
                 	     	
       !Fichier Trace des calculs SOS
       IF (FICSOS_LOG.NE."NO_LOG_FILE") THEN
          LFIC = INDEX(FICSOS_LOG,' ') - 1
          IF((LFIC.LE.0).OR.(LFIC.GT.CTE_LENFIC2)) GOTO 1996
          FICSOS_LOG = RESROOT_ADV(1:LDIRADV)//'/'//FICSOS_LOG(1:LFIC)
       ENDIF     

       !Fichier des profils de flux descendant et montant
       FIC_FLUX=RESROOT_ADV(1:LDIRADV)//'/Flux.txt'


C =========================================================
C --- Copie du fichier listant les parametres d'un traitement
C     précédent potentiel
C =========================================================        
	
      LDIR = INDEX(RESROOT,' ') - 1
      INQUIRE(FILE=RESROOT(1:LDIR)//'/ListParam.txt',EXIST=EX)

C --- Si le fichier Trace existe sous l'espace des résultats, 
C     on le copie pour contrôler si la simulation sollicitée est une
C     reprise d'un même traitement pour une valeur d'azimut différente
C     ou pour un niveau de sortie différent (pas de reprise du calcul 
C     du fichier de champ de luminance décomposé en Séries de Fourier).    
   
      IF (EX) THEN
         CAS_REPRISE=.TRUE.
	 
	 COMMAND = 
     &	 'cp '//RESROOT(1:LDIR)//'/ListParam.txt copy_ListParam.txt'
         CALL SYSTEM(COMMAND)      

     
      ELSE
         !Si pas existant : impossibilité de faire le contrôle
	 !=> La simulation est reprise depuis le début
         CAS_REPRISE=.FALSE. 
	 
      ENDIF        
         


	   
C =========================================================
C --- Ouverture du fichier Trace
C =========================================================        

      IF (TRACE) THEN
	 OPEN (IDLOG,FILE=FICMAIN_LOG,ERR=900)
      ENDIF
       
      IF (TRACE) THEN
      	  WRITE (IDLOG,*,ERR=910) "WORKING FOLDER DEFINITION"
	  WRITE (IDLOG,*,ERR=910) "-------------------------"
          WRITE (IDLOG,*,ERR=910) "Working folder :", RESROOT(1:LDIR)
	  WRITE (IDLOG,*,ERR=910) " "
	  WRITE (IDLOG,*,ERR=910) 
     &	  "Standard result folder :", RESROOT_STD(1:LDIRSTD)
          WRITE (IDLOG,*,ERR=910) " "
	  WRITE (IDLOG,*,ERR=910) 
     &	  "Advanced result folder :", RESROOT_ADV(1:LDIRADV)
          WRITE (IDLOG,*,ERR=910) " "
      ENDIF
      
            
C =========================================
C --- Gestion de la cohérence des arguments 
C =========================================

      IF (TRACE) THEN
          WRITE (IDLOG,*,ERR=910) " "
          WRITE (IDLOG,*,ERR=910) 
     &	  "--> Starting the check of user parameters "
          WRITE (IDLOG,*,ERR=910) " "
      ENDIF



 
C --- Contrôle de la définition de la longueur d'onde de simulation
      IF (DEF_WA_SIMU.EQ.0) GOTO 2000
      
      IF (WA_SIMU.LT.CTE_WAMIN) GOTO 2010
 
 
 
C --------------------------------------------------------------        
C --- Contrôle du passage des arguments de définition des angles
C --------------------------------------------------------------  
C  
C     Angle solaire : obligatoire           
      IF (DEF_TETAS.EQ.0) GOTO 2100
      IF ((TETAS.LT.0.D-00).OR.(TETAS.GE.90.D-00)) GOTO 2105
      
       
      IF (TRACE) THEN

C         Fichier résultat pour les luminances : optionnel
          IF (DEF_FICANGLES_RES_LUM.EQ.0) THEN 
	     WRITE (IDLOG,*,ERR=910) "Default filename ",
     &       "for the radiance angles definition :"
          ELSE
	     WRITE (IDLOG,*,ERR=910) "User's filename ",
     &       "for the radiance angles definition :"
	  ENDIF 
          WRITE (IDLOG,*,ERR=910) FICANGLES_RES_LUM
	  
C         Fichier résultat pour les fonctions de phase : optionnel    	  
          WRITE (IDLOG,*,ERR=910) " "
	  IF (DEF_FICANGLES_RES_MIE.EQ.0) THEN
	     WRITE (IDLOG,*,ERR=910) "Default filename ",
     &       "for the Mie angles definition :"
          ELSE
	     WRITE (IDLOG,*,ERR=910) "User's filename ",
     &       "for the Mie angles definition :"
	  ENDIF 
          WRITE (IDLOG,*,ERR=910) FICANGLES_RES_MIE
	  	  	  
      ENDIF ! Fin Trace

        
   

C --------------------------------------------------------------  
C --- Contrôle du passage des arguments de modèle hydrosols
C --------------------------------------------------------------  
C
      IF (TRACE) THEN

C         Def du fichier GRANU résultat des calculs Hydrosols - Phytoplancton:  optionnel
          WRITE (IDLOG,*,ERR=910) " "   
          IF (DEF_FICGRANU_PHYTO.EQ.0) THEN 
	     WRITE (IDLOG,*,ERR=910) "Default filename ",
     &       "for the Phytoplankton phase function description :"
          ELSE
	     WRITE (IDLOG,*,ERR=910) "User's filename ",
     &       "for the Phytoplankton phase function description :"
	  ENDIF 
          WRITE (IDLOG,*,ERR=910) FICGRANU_PHYTO

C         Def du fichier GRANU résultat des calculs Hydrosols - Sédiments :  optionnel
          WRITE (IDLOG,*,ERR=910) " "   
          IF (DEF_FICGRANU_MLP.EQ.0) THEN 
	     WRITE (IDLOG,*,ERR=910) "Default filename for the ",
     &       "Mineral-Like Particles phase function description :"
          ELSE
	     WRITE (IDLOG,*,ERR=910) "User's filename for the ",
     &       "Mineral-Like Particles phase function description :"
	  ENDIF 
          WRITE (IDLOG,*,ERR=910) FICGRANU_MLP
	  	  
      ENDIF ! Fin Trace


C     Cas CHL > 0 ou SED_CSED > 0:
      IF ((CHL.GT.0.0000).OR.(SED_CSED.GT.0.0000)) THEN

         !Définition du répertoire de stockage des fichiers de Mie : obligatoire  
          IF (DEF_DIRMIE_HYD.EQ.0) GOTO 2610   
	  
	  !Création du répertoire de stockage des fichiers de Mie
	  LDIR = INDEX(DIRMIE_HYD,' ') - 1
          IF((LDIR.LE.0).OR.(LDIR.GT.CTE_LENDIR)) GOTO 1997

          COMMAND='mkdir -p '//DIRMIE_HYD(1:LDIR)
          LCOM = INDEX(COMMAND(10:CTE_LENCOM),' ') - 1
          IF((LCOM.LE.0).OR.(LCOM.GT.CTE_LENCOM)) GOTO 1995
          CALL SYSTEM(COMMAND)
	  
	  	    
          !Définition du type de caractérisation des hydrosols : obligatoire   
          IF (DEF_IMOD_HYD.EQ.0) GOTO 2620
	  
	  IF ((IMOD_HYD.LT.1).OR.(IMOD_HYD.GT.2)) GOTO 2621

C         ----------------------------------------------------
C         Cas d'une définition des propriétés radiatives des
C         hydrosols par modèle  : IMOD_HYD = 1  
C         ----------------------------------------------------	  
	  IF (IMOD_HYD.EQ.1) THEN 
	  	   
             IF (CHL.GT.0.0000) THEN
	     
	         !Test que le mode Junge obligatoire est renseigné
		 IF (DEF_PHYTO_JD_SLOPE.EQ.0) GOTO 2630
		 IF (DEF_PHYTO_JD_MR.EQ.0)    GOTO 2630
		 IF (DEF_PHYTO_JD_MI.EQ.0)    GOTO 2630
		 IF (DEF_PHYTO_JD_RATE.EQ.0)  GOTO 2630
		 
		 !Test que si un param du mode LND secondaire est renseigné
		 !alors l'ensemble des param du mode LND secondaire doit être renseigné
		 IF ( (DEF_PHYTO_LND_SM_RADIUS.EQ.1).OR.
     &	              (DEF_PHYTO_LND_SM_VAR.EQ.1).OR.
     &	              (DEF_PHYTO_LND_SM_MR.EQ.1).OR.
     &	              (DEF_PHYTO_LND_SM_MI.EQ.1).OR.
     &	              (DEF_PHYTO_LND_SM_RATE.EQ.1) ) THEN
		      
		         IF (DEF_PHYTO_LND_SM_RADIUS.EQ.0) GOTO 2632
		         IF (DEF_PHYTO_LND_SM_VAR.EQ.0)    GOTO 2632
		         IF (DEF_PHYTO_LND_SM_MR.EQ.0)     GOTO 2632
		         IF (DEF_PHYTO_LND_SM_MI.EQ.0)     GOTO 2632
		         IF (DEF_PHYTO_LND_SM_RATE.EQ.0)   GOTO 2632
		 ENDIF !Fin si un param LND secondary mode défini     

		 !Test que si un param du mode LND tertiaire est renseigné
		 !alors l'ensemble des param du mode LND tertiaire doit être renseigné
		 IF ( (DEF_PHYTO_LND_TM_RADIUS.EQ.1).OR.
     &	              (DEF_PHYTO_LND_TM_VAR.EQ.1).OR.
     &	              (DEF_PHYTO_LND_TM_MR.EQ.1).OR.
     &	              (DEF_PHYTO_LND_TM_MI.EQ.1).OR.
     &	              (DEF_PHYTO_LND_TM_RATE.EQ.1) ) THEN
		      
		         IF (DEF_PHYTO_LND_TM_RADIUS.EQ.0) GOTO 2634
		         IF (DEF_PHYTO_LND_TM_VAR.EQ.0)    GOTO 2634
		         IF (DEF_PHYTO_LND_TM_MR.EQ.0)     GOTO 2634
		         IF (DEF_PHYTO_LND_TM_MI.EQ.0)     GOTO 2634
		         IF (DEF_PHYTO_LND_TM_RATE.EQ.0)   GOTO 2634
		 ENDIF !Fin si un param LND tertiary mode défini     
		 	
	     ENDIF ! Cas concentration phyto CHL non nulle
	     
	     
             IF (SED_CSED.GT.0.0000) THEN
	         
		 !Test que le mode Junge obligatoire est renseigné
		 IF (DEF_SED_JD_SLOPE.EQ.0) GOTO 2640
		 IF (DEF_SED_JD_MR.EQ.0)    GOTO 2640
		 IF (DEF_SED_JD_MI.EQ.0)    GOTO 2640
		 IF (DEF_SED_JD_RATE.EQ.0)  GOTO 2640

		 !Test que si un param du mode LND secondaire est renseigné
		 !alors l'ensemble des param du mode LND secondaire doit être renseigné
		 IF ( (DEF_SED_LND_SM_RADIUS.EQ.1).OR.
     &	              (DEF_SED_LND_SM_VAR.EQ.1).OR.
     &	              (DEF_SED_LND_SM_MR.EQ.1).OR.
     &	              (DEF_SED_LND_SM_MI.EQ.1).OR.
     &	              (DEF_SED_LND_SM_RATE.EQ.1) ) THEN
		      
		         IF (DEF_SED_LND_SM_RADIUS.EQ.0) GOTO 2642
		         IF (DEF_SED_LND_SM_VAR.EQ.0)    GOTO 2642
		         IF (DEF_SED_LND_SM_MR.EQ.0)     GOTO 2642
		         IF (DEF_SED_LND_SM_MI.EQ.0)     GOTO 2642
		         IF (DEF_SED_LND_SM_RATE.EQ.0)   GOTO 2642
		 ENDIF !Fin si un param LND secondary mode défini     

		 !Test que si un param du mode LND tertiaire est renseigné
		 !alors l'ensemble des param du mode LND tertiaire doit être renseigné
		 IF ( (DEF_SED_LND_TM_RADIUS.EQ.1).OR.
     &	              (DEF_SED_LND_TM_VAR.EQ.1).OR.
     &	              (DEF_SED_LND_TM_MR.EQ.1).OR.
     &	              (DEF_SED_LND_TM_MI.EQ.1).OR.
     &	              (DEF_SED_LND_TM_RATE.EQ.1) ) THEN
		      
		         IF (DEF_SED_LND_TM_RADIUS.EQ.0) GOTO 2644
		         IF (DEF_SED_LND_TM_VAR.EQ.0)    GOTO 2644
		         IF (DEF_SED_LND_TM_MR.EQ.0)     GOTO 2644
		         IF (DEF_SED_LND_TM_MI.EQ.0)     GOTO 2644
		         IF (DEF_SED_LND_TM_RATE.EQ.0)   GOTO 2644
		 ENDIF !Fin si un param LND tertiary mode défini     
	 
	     ENDIF ! Cas concentration sédiment SED_CSED non nulle
	     
	     
	  ENDIF !Fin cas IMOD_HYD = 1


C         ----------------------------------------------------
C         Cas de données de fonction de phase hydrosols 
C         fournies par l'utilisateur : IMOD_HYD = 2  
C         ----------------------------------------------------	  
	  IF (IMOD_HYD.EQ.2) THEN 	!Cas fonctions externes
             ! Contrôle de la récupération du nom du fichier de données externes
	     IF (DEF_FICEXTDATA_HYD.EQ.0) GOTO 2700
	          
	  ENDIF !Fin cas IMOD_HYD = 2 (fonctions phases externes)


      ELSE ! CHL = 0 et SED_CSED = 0 : Cas sans chloro ni sédiments.
	   ! ==> on impose IMOD=1 pour que la routine OSOAA_HYDROSOLS 
	   !     puisse produire une matrice de phase nulle.
         IMOD_HYD = 1

	  	  	  	  	  	  
      ENDIF ! Fin du cas CHL > 0 ou SED_CSED > 0

	 
C ------------------------------------------------------------------------  
C --- Contrôle du passage des arguments des profils atmosphérique et marin 
C ------------------------------------------------------------------------  
C
C     L'ep opt moléculaire est renseignée directement par -AP.MOT (pour la longueur 
C     d'onde de simulation de la luminance) ou bien est renseignée par -AP.Pressure
C     donnant la pression atmosphérique au niveau de la mer.
C     ==> Il faut que l'un ou l'autre de ces paramètres soit défini (pas les deux). 
      IF (DEF_TR.EQ.0) THEN ! Cas TR non définie
	 IF (DEF_PRESSION.EQ.0) GOTO 2200   !Il faut que PRESSION soit défini
	 TR = CTE_NOT_DEFINED_VALUE_DBLE
      ELSE ! Cas TR définie
         IF (DEF_PRESSION.EQ.1) GOTO 2200  !PRESSION ne doit pas être défini
	 PRESSION = CTE_NOT_DEFINED_VALUE_DBLE
      ENDIF
	
C     L'échelle de hauteur moléculaire doit être définie 
C     si l'ep opt moléculaire n'est pas négligeable
      IF ((DEF_HR.EQ.0).AND.
     &    ((TR.GT.0.0001).OR.(PRESSION.GT.0.0001))) GOTO 2210
         
C     L'échelle de hauteur des aérosols doit être définie 
C     si l'ep opt aérosols n'est pas négligeable
C     Rem : Le test est fait sur l'AOT de référence à WAREF.
C           Il pourrait être fait sur l'AOT de la longueur 
C           d'onde de simulation de la luminance WA
C           --> Il est refait après calcul de l'AOT à WA 
      IF ((DEF_HA.EQ.0).AND.(AOT_REF.GT.0.0001)) GOTO 2220 


C     Quantite de chlorophille en surface : obligatoire           
      IF (DEF_CHL.EQ.0) GOTO 2230

C     Définition du type de profile de chlorophylle : 
C     ==> à définir si la quantite de chlorophylle n'est pas nulle          
      IF (CHL.GT.0.0000) THEN
      
         IF (DEF_IPHYTO_PROFIL.EQ.0) GOTO 2240
	 
C        Contrôle du domaine de  IPHYTO_PROFIL  
	 IF ((IPHYTO_PROFIL.LT.1).OR.(IPHYTO_PROFIL.GT.3)) GOTO 2250
	 
C        Contrôle du renseignement des paramètres de profil gaussien de chlorophylle   
         IF (IPHYTO_PROFIL.EQ.2) THEN	!Cas du profil gaussien
            IF (DEF_CHL_GP_BG.EQ.0) GOTO 2251
	    IF (DEF_CHL_GP_DEEP.EQ.0) GOTO 2251
	    IF (DEF_CHL_GP_WIDTH.EQ.0) GOTO 2251  
         ENDIF	
	 
C        Contrôle du renseignement du fichier Utilisateur   
         IF (IPHYTO_PROFIL.EQ.3) THEN	!Cas de l'utilisation d'un fichier utilisateur
            IF (DEF_FICPHYTO_USER.EQ.0) GOTO 2252  
	    CHL_GP_BG = CTE_NOT_DEFINED_VALUE_DBLE
	    CHL_GP_DEEP = CTE_NOT_DEFINED_VALUE_DBLE
	    CHL_GP_WIDTH = CTE_NOT_DEFINED_VALUE_DBLE
         ENDIF		
	   
      ELSE	!Cas Chl=0. ==> On force à "indéfini" tous les paramètres associés 
	 IPHYTO_PROFIL = 1     !sauf le profil qui est fixé homogène par défaut
	 CHL_GP_BG = CTE_NOT_DEFINED_VALUE_DBLE
	 CHL_GP_DEEP = CTE_NOT_DEFINED_VALUE_DBLE
	 CHL_GP_WIDTH = CTE_NOT_DEFINED_VALUE_DBLE
	 FICPHYTO_USER = "NO_USER_PHYTO_PROFILE"
      ENDIF
      
C     Quantite de sédiment en surface : obligatoire           
      IF (DEF_SED_CSED.EQ.0) GOTO 2260      

C     Substance jaune : Coeff d'absorption à 440 nm (m-1) : obligatoire  
C			Coeff de conversion spectrale     : obligatoire 
C 						            si le coef d'abs. n'est pas nul.
      IF (DEF_YS_A440.EQ.0) GOTO 2270  
      
C     Détritus : Coeff d'absorption à 440 nm (m-1) : obligatoire
      IF (DEF_DET_A440.EQ.0) GOTO 2275        


      IF (TRACE) THEN

C         Définition du fichier de profil atmosphérique :  optionnel
          WRITE (IDLOG,*,ERR=910) " "   
          IF (DEF_FICPROFIL_ATM_RES.EQ.0) THEN 
	     WRITE (IDLOG,*,ERR=910) "Default filename ",
     &       "for the atmospheric profile description :"
          ELSE
	     WRITE (IDLOG,*,ERR=910) "User's filename ",
     &       "for the atmospheric profile description :"
	  ENDIF 
          WRITE (IDLOG,*,ERR=910) FICPROFIL_ATM_RES

C         Définition du fichier de profil marin :  optionnel
          WRITE (IDLOG,*,ERR=910) " "   
          IF (DEF_FICPROFIL_SEA_RES.EQ.0) THEN 
	     WRITE (IDLOG,*,ERR=910) "Default filename for the ",
     &       "sea profile description :"
          ELSE
	     WRITE (IDLOG,*,ERR=910) "User's filename for the ",
     &       "sea profile description :"
	  ENDIF 
          WRITE (IDLOG,*,ERR=910) FICPROFIL_SEA_RES
	  	  
      ENDIF ! Fin Trace


  
C --------------------------------------------------------------  
C --- Contrôle du passage des arguments de l'interface air / mer 
C --------------------------------------------------------------  
C
C     Indice de l'eau de mer : obligatoire  
      IF (DEF_SEA_IND.EQ.0) GOTO 2300                   

C     Vitesse du vent : obligatoire  
      IF (DEF_WIND.EQ.0) GOTO 2310  
 
C     Argument DIRSEA obligatoire si vent > 0 
C     (répertoire de stockage des fichiers SURFACE)       
      IF ((DEF_DIRSEA.EQ.0).AND.(WIND.GT.0.)) GOTO 2320
	  
C     Création du répertoire de stockage des fichiers de Mie
      IF (WIND.GT.0.) THEN
         LDIR = INDEX(DIRSEA,' ') - 1
         IF((LDIR.LE.0).OR.(LDIR.GT.CTE_LENDIR)) GOTO 1999

         COMMAND='mkdir -p '//DIRSEA(1:LDIR)
         LCOM = INDEX(COMMAND(10:CTE_LENCOM),' ') - 1
         IF((LCOM.LE.0).OR.(LCOM.GT.CTE_LENCOM)) GOTO 1995
         CALL SYSTEM(COMMAND)
      ENDIF      

C --------------------------------------------------------------  
C --- Contrôle du passage des arguments de modèle d'aérosols
C --------------------------------------------------------------  
C 
C     Définition de l'ep opt aérosols à la longueur d'onde de référence : 
C     obligatoire si ep opt des aérosols non nulle
      IF (DEF_AOT_REF.EQ.0) GOTO 2410  	

  
      IF (TRACE) THEN

C         Définition du fichier GRANU résultat des calculs Aérosols :  optionnel
          WRITE (IDLOG,*,ERR=910) " "   
          IF (DEF_FICGRANU_AER.EQ.0) THEN 
	     WRITE (IDLOG,*,ERR=910) "Default filename ",
     &       "for the aerosol phase function description :"
          ELSE
	     WRITE (IDLOG,*,ERR=910) "User's filename ",
     &       "for the aerosol phase function description :"
	  ENDIF 
          WRITE (IDLOG,*,ERR=910) FICGRANU_AER
	  
      ENDIF ! Fin Trace
      
              
C     Cas AOT_REF > 0 :
      IF ( AOT_REF.GT.0.D+00) THEN

          !Définition de la longueur d'onde de référence pour l'ep opt aérosols : 
          !obligatoire si ep opt des aérosols non nulle
          IF (DEF_WAREF_AOT.EQ.0) GOTO 2400 
          IF (WAREF_AOT.LT.CTE_WAMIN) GOTO 2405      

          !Définition du répertoire de stockage des fichier de Mie : obligatoire  
          IF (DEF_DIRMIE_AER.EQ.0) GOTO 2415   
	  
	  !Création du répertoire de stockage des fichiers de Mie
	  LDIR = INDEX(DIRMIE_AER,' ') - 1
          IF((LDIR.LE.0).OR.(LDIR.GT.CTE_LENDIR)) GOTO 1998

          COMMAND='mkdir -p '//DIRMIE_AER(1:LDIR)
          LCOM = INDEX(COMMAND(10:CTE_LENCOM),' ') - 1
          IF((LCOM.LE.0).OR.(LCOM.GT.CTE_LENCOM)) GOTO 1995
          CALL SYSTEM(COMMAND)
	    
          !Définition du type de modèle d'aérosols : obligatoire  
          IF (DEF_IMOD_AER.EQ.0) GOTO 2420
	  
	  IF ((IMOD_AER.LT.0).OR.(IMOD_AER.GT.4)) GOTO 2421

C         ----------------------------------------------------
C         Cas d'un modèle d'aérosols mono-modal : IMOD_AER = 0  
C         ----------------------------------------------------	  
	  IF (IMOD_AER.EQ.0) THEN 	   
             ! Contrôle de la récupération de la valeur de l'indice de réfraction
	     IF ((DEF_RN_MMD_AER.EQ.0).OR.
     &	         (DEF_IN_MMD_AER.EQ.0)) GOTO 2430
             ! Contrôle de la récupération du type de granu mono-modale
	     IF (DEF_IGRANU_MMD_AER.EQ.0) GOTO 2440
	     ! Contrôle de la récupération des paramètres du modèle de LND
	     IF (IGRANU_MMD_AER.EQ.1) THEN
	         IF (DEF_LND_RADIUS_MMD_AER.EQ.0) GOTO 2441
		 IF (DEF_LND_LNVAR_MMD_AER.EQ.0) GOTO 2441
	     ENDIF     

	     ! Contrôle de la récupération des paramètres obligatoires du modèle de Junge
	     IF (IGRANU_MMD_AER.EQ.2) THEN
	         IF (DEF_JD_SLOPE_MMD_AER.EQ.0) GOTO 2442
		 IF (DEF_JD_RMIN_MMD_AER.EQ.0) GOTO 2442
	     ENDIF	     

	     	     	     
	     ! Si l'AOT est définie pour une autre longueur d'onde que celle de simulation :
	     ! ==> Contrôle de la récupération de la valeur de l'indice de réfraction
	     !     pour la longueur d'onde de référence.
	     IF (WA_SIMU.NE.WAREF_AOT) THEN
	        IF ((DEF_RN_MMD_AER_WAREF.EQ.0).OR.
     &	            (DEF_IN_MMD_AER_WAREF.EQ.0)) GOTO 2450
	     ENDIF ! Fin test si Wa != WaRef

	  ENDIF !Fin cas IMOD_AER = 0


C         ----------------------------------------------------
C         Cas d'un modèle d'aérosols WMO : IMOD_AER = 1  
C         ----------------------------------------------------	  	  
	  IF (IMOD_AER.EQ.1) THEN 	!Cas WMO	
	     ! Contrôle de la récupération du choix de modèle WMO
	     IF (DEF_IMODELE_WMO.EQ.0) GOTO 2460
	     
	     ! Si modèle libre de l'utilisateur, contrôle de la récupération 
	     ! des proportions des 4 composants
	     IF (IMODELE_WMO.EQ.4) THEN     
	         IF ((DEF_WMO_DL.EQ.0).OR.(DEF_WMO_WS.EQ.0).OR.
     &		     (DEF_WMO_OC.EQ.0).OR.(DEF_WMO_SO.EQ.0)) GOTO 2461
	     ENDIF !Cas modèle utilisateur WMO
	  
	  ENDIF !Fin cas IMOD_AER = 1 (WMO)  


C         ----------------------------------------------------
C         Cas d'un modèle d'aérosols S&F : IMOD_AER = 2  
C         ----------------------------------------------------
	  IF (IMOD_AER.EQ.2) THEN 	!Cas S&F
	     ! Contrôle de la récupération du choix de modèle S&F 
	     IF (DEF_IMODELE_SF.EQ.0) GOTO 2470
	     ! Contrôle de la récupération de la valeur de l'humidité relative
	     IF (DEF_RH.EQ.0) GOTO 2471
	  ENDIF !Fin cas IMOD_AER = 2 (S&F)  


C         ----------------------------------------------------
C         Cas d'un modèle d'aérosols bimode LND : IMOD_AER = 3  
C         ----------------------------------------------------	  
	  IF (IMOD_AER.EQ.3) THEN 	!Cas bi LND	

             !Contrôle de la récupération des paramètres spécifiques au calcul bimodal LND
             IF (DEF_MODE_PARAM_BILND.EQ.0)  GOTO 2480
          
      
             !Récupération de l'information de la description du mélange
	     !    Cas 1 : Utilisation des concentrations volumiques de l'utilisateur
             !    --> Contrôle que les paramètres utilisateurs sont renseignés pour l'option
             IF (MODE_PARAM_BILND.EQ.1) THEN          
	        IF (DEF_USER_CV_COARSE.EQ.0) GOTO 2481
	        IF (DEF_USER_CV_FINE.EQ.0)   GOTO 2481  
             ENDIF
    
            !    Cas 2 : Utilisation du rapport AOT_coarse / AOT_totale
            !    --> Contrôle que les paramètres sont tous renseignés pour l'option
            IF (MODE_PARAM_BILND.EQ.2) THEN 
	        IF (DEF_RTAUct_WAREF.EQ.0) GOTO 2482
            ENDIF
   
             !Récupération de l'information d'indice et granulométrie des 2 LND
             !-----------------------------------------------------------------  
             IF (DEF_BMD_CM_MRWA.EQ.0)     GOTO 2483
             IF (DEF_BMD_CM_MIWA.EQ.0)     GOTO 2483 
             IF (DEF_BMD_CM_RMODAL.EQ.0)   GOTO 2483
             IF (DEF_BMD_CM_VAR.EQ.0)      GOTO 2483 
             IF (DEF_BMD_FM_MRWA.EQ.0)     GOTO 2484
             IF (DEF_BMD_FM_MIWA.EQ.0)     GOTO 2484 
             IF (DEF_BMD_FM_RMODAL.EQ.0)   GOTO 2484
             IF (DEF_BMD_FM_VAR.EQ.0)      GOTO 2484            
     
             !Récupération de l'information d'indice des 2 LND pour WAREF 
             !-----------------------------------------------------------------     
             IF (MODE_PARAM_BILND.EQ.2) THEN 
	     
	        IF (WA_SIMU.NE.WAREF_AOT) THEN
	           IF (DEF_BMD_CM_MRWAREF.EQ.0) GOTO 2485
	           IF (DEF_BMD_CM_MIWAREF.EQ.0) GOTO 2485
	           IF (DEF_BMD_FM_MRWAREF.EQ.0) GOTO 2485
	           IF (DEF_BMD_FM_MIWAREF.EQ.0) GOTO 2485   
		ELSE !Si waref=wa ==> On impose de récupérer l'info d'incide pour wa 
		     !                et de l'affecter à waref
		   BMD_CM_MRWAREF = BMD_CM_MRWA
		   BMD_CM_MIWAREF = BMD_CM_MIWA
		   BMD_FM_MRWAREF = BMD_FM_MRWA
		   BMD_FM_MIWAREF = BMD_FM_MIWA  
	        ENDIF ! Fin test si Wa != WaRef
	        
             ENDIF !Fin cas description du mélange bi LND par rapport d'AOT
	     
	  ENDIF !Fin cas IMOD_AER = 3 (bi LND)


C         ----------------------------------------------------
C         Cas d'un modèle d'aérosols fourni par l'utilisateur : IMOD_AER = 4  
C         ----------------------------------------------------	  
	  IF (IMOD_AER.EQ.4) THEN 	!Cas fonctions externes
             ! Contrôle de la récupération du nom du fichier de données externes
	     IF (DEF_FICEXTDATA_AER.EQ.0) GOTO 2500
	     ! Contrôle que la longueur d'onde de référence pour les aérosols et
	     ! identique à celle de simulation du champs de luminance 
	     IF (WA_SIMU.NE.WAREF_AOT) GOTO 2505
	  ENDIF !Fin cas IMOD_AER = 4 (fonctions phases externes)
	  	  	  	  	  
      ENDIF ! Fin du cas AOT_REF > 0

	
C -------------------------------------------------------------------  
C --- Contrôle des paramètres propres aux calculs d'Ordres Successifs 
C     et à la production des fichiers résultats
C -------------------------------------------------------------------  
C  
      !Définition de l'albédo de surface (composante lambertienne) : obligatoire  
      IF (DEF_RHO_SURF.EQ.0) GOTO 2510
      
      !Définition du choix de type de fond : obligatoire  
      IF (DEF_IBOT_TYPE.EQ.0) GOTO 2520      

      !Définition de l'albédo de fond (composante lambertienne) doit être définie
      !Si IBOT_TYPE=1 
      IF ((IBOT_TYPE.EQ.1).AND.(DEF_RHO_BOT.EQ.0)) GOTO 2525

      !Définition de l'azimut relatif : obligatoire  
      IF (DEF_PHI.EQ.0) GOTO 2540

      !Définition du type de niveau de sortie: obligatoire  
      IF (DEF_LEVEL_OUT.EQ.0) GOTO 2545
      IF ((LEVEL_OUT.LT.1).OR.(LEVEL_OUT.GT.5)) GOTO 2546       
          
      !Définition du niveau pour lum vs angle zénithal 
      !et fichiers résultat associés: obligatoire  
      IF ((LEVEL_OUT.EQ.5).AND.(DEF_USER_Z_OUT.EQ.0)) GOTO 2550
	 
      !S'il y a eu définition de l'angle zénithal pour lum vs niveau 
      !==> Il doit y avoir un fichier résultat associé.
      !Et réciproquement.  
      IF (DEF_VZA_OUT.EQ.0) THEN 
      	 !Cas VZA non défini ==> erreur si le fichier vs Z est défini
         IF (FICSOS_RES_VS_Z.NE."NO_RES_VS_Z_FILE") GOTO 2560
      ELSE
         !Cas VZA défini ==> on doit avoir 0 <= VZA <= 180°
	 IF ((VZA_OUT.LT.0.).OR.(VZA_OUT.GT.180.)) GOTO 2565
	 !Cas VZA défini ==> erreur si le fichier vs Z n'est pas défini
         IF (FICSOS_RES_VS_Z.EQ."NO_RES_VS_Z_FILE") GOTO 2560
      ENDIF

      IF (TRACE) THEN

C         Définition du fichier résultat binaire :  optionnel
          WRITE (IDLOG,*,ERR=910) " "   
          IF (DEF_FICSOS_RES_BIN.EQ.0) THEN 
	     WRITE (IDLOG,*,ERR=910) "Default filename for the ",
     &       "OSOAA binary result file :"
          ELSE
	     WRITE (IDLOG,*,ERR=910) "User's filename for the ",
     &       "OSOAA binary result file :"
	  ENDIF 
          WRITE (IDLOG,*,ERR=910) FICSOS_RES_BIN
	  
C         Définition du fichier résultat fct de VZA :  optionnel
          WRITE (IDLOG,*,ERR=910) " "   
          IF (DEF_FICSOS_RES_VS_VZA.EQ.0) THEN 
	     WRITE (IDLOG,*,ERR=910) "Default filename ",
     &       "for the OSOAA result file : radiance vs zenith angle :"
          ELSE
	     WRITE (IDLOG,*,ERR=910) "User's filename ",
     &       "for the OSOAA result file : radiance vs zenith angle :"
	  ENDIF 
          WRITE (IDLOG,*,ERR=910) FICSOS_RES_VS_VZA
	  	  
      ENDIF ! Fin Trace
   



	

C =========================================================
C --- Génération du fichier listant les parametres du traitement
C     en cours
C =========================================================        
      LDIR = INDEX(RESROOT,' ') - 1
      OPEN(11,FILE=RESROOT(1:LDIR)//'/ListParam.txt',ERR=3500)

C ------- Arguments propres au programme principal      
C     -OSOAA.ResRoot:
      WRITE(11,1800,ERR=3501) RESROOT(1:LDIR)
      
C     -OSOAA.Log:
      LFIC = INDEX(FICMAIN_LOG,' ') - 1
      WRITE(11,1801,ERR=3501) FICMAIN_LOG(1:LFIC)

C     -OSOAA.Wa:
      WRITE(11,1802,ERR=3501) WA_SIMU

C ------- Arguments propres à la définition des angles		
C     -ANG.Rad.NbGauss:
      WRITE(11,1803,ERR=3501) NBMU_GAUSS_LUM  

C     -ANG.Rad.UserAngFile:
      LFIC = INDEX(FICANGLES_USER_LUM,' ') - 1
      WRITE(11,1804,ERR=3501) FICANGLES_USER_LUM(1:LFIC)  

C     -ANG.Thetas:
      WRITE(11,1805,ERR=3501) TETAS 

C     -ANG.Rad.ResFile:
      LFIC = INDEX(FICANGLES_RES_LUM,' ') - 1
      WRITE(11,1806,ERR=3501) FICANGLES_RES_LUM(1:LFIC)

C     -ANG.Mie.NbGauss:
      WRITE(11,1807,ERR=3501) NBMU_GAUSS_MIE  

C     -ANG.Mie.UserAngFile:
      LFIC = INDEX(FICANGLES_USER_MIE,' ') - 1
      WRITE(11,1808,ERR=3501) FICANGLES_USER_MIE(1:LFIC)
	 	 
C     -ANG.Mie.ResFile:
      LFIC = INDEX(FICANGLES_RES_MIE,' ') - 1
      WRITE(11,1809,ERR=3501) FICANGLES_RES_MIE(1:LFIC)

C     -ANG.Log:
      LFIC = INDEX(FICANGLES_LOG,' ') - 1
      WRITE(11,1810,ERR=3501) FICANGLES_LOG(1:LFIC)



C ------- Arguments propres à la définition des modèles d'aérosols
C     -AER.Waref:
      WRITE(11,1811,ERR=3501) WAREF_AOT

C     -AER.AOTref:
      WRITE(11,1812,ERR=3501) AOT_REF
      
C     -AER.Tronca:
      WRITE(11,1813,ERR=3501) ITRONC_AER
	 		 
C     -AER.Model:
      WRITE(11,1814,ERR=3501) IMOD_AER
      
C     -AER.MMD.MRwa:
      WRITE(11,1815,ERR=3501) RN_MMD_AER

C     -AER.MMD.MIwa:
      WRITE(11,1816,ERR=3501) IN_MMD_AER
       	  				
C     -AER.MMD.MRwaref:
      WRITE(11,1817,ERR=3501) RN_MMD_AER_WAREF
      
C     -AER.MMD.MIwaref:
      WRITE(11,1818,ERR=3501) IN_MMD_AER_WAREF
	 		 
C     -AER.MMD.SDtype:
      WRITE(11,1819,ERR=3501) IGRANU_MMD_AER
      
C     -AER.MMD.LNDradius:
      WRITE(11,1820,ERR=3501) LND_RADIUS_MMD_AER

C     -AER.MMD.LNDvar:
      WRITE(11,1821,ERR=3501) LND_LNVAR_MMD_AER

C     -AER.MMD.JD.slope:
      WRITE(11,1822,ERR=3501) JD_SLOPE_MMD_AER

C     -AER.MMD.JD.rmin:
      WRITE(11,1823,ERR=3501) JD_RMIN_MMD_AER

C     -AER.MMD.JD.rmax:
      WRITE(11,1824,ERR=3501) JD_RMAX_MMD_AER

C     -AER.WMO.Model:
      WRITE(11,1825,ERR=3501) IMODELE_WMO

C     -AER.WMO.DL:
      WRITE(11,1826,ERR=3501) C_WMO_DL 

C     -AER.WMO.WS:
      WRITE(11,1827,ERR=3501) C_WMO_WS

C     -AER.WMO.OC:
      WRITE(11,1828,ERR=3501) C_WMO_OC

C     -AER.WMO.SO:
      WRITE(11,1829,ERR=3501) C_WMO_SO

C     -AER.SF.Model:
      WRITE(11,1830,ERR=3501) IMODELE_SF

C     -AER.SF.RH:
      WRITE(11,1831,ERR=3501) RH

C     -AER.BMD.VCdef:
      WRITE(11,1832,ERR=3501) MODE_PARAM_BILND

C     -AER.BMD.CoarseVC:
      WRITE(11,1833,ERR=3501) USER_CV_COARSE

C     -AER.BMD.FineVC:
      WRITE(11,1834,ERR=3501) USER_CV_FINE

C     -AER.BMD.RAOT:
      WRITE(11,1835,ERR=3501) RTAUct_WAREF

C     -AER.BMD.CM.MRwa:
      WRITE(11,1836,ERR=3501) BMD_CM_MRWA

C     -AER.BMD.CM.MIwa:
      WRITE(11,1837,ERR=3501) BMD_CM_MIWA

C     -AER.BMD.CM.MRwaref:
      WRITE(11,1838,ERR=3501) BMD_CM_MRWAREF

C     -AER.BMD.CM.MIwaref:
      WRITE(11,1839,ERR=3501) BMD_CM_MIWAREF

C     -AER.BMD.CM.SDradius:
      WRITE(11,1840,ERR=3501) BMD_CM_RMODAL

C     -AER.BMD.CM.SDvar:
      WRITE(11,1841,ERR=3501) BMD_CM_VAR

C     -AER.BMD.FM.MRwa:
      WRITE(11,1842,ERR=3501) BMD_FM_MRWA

C     -AER.BMD.FM.MIwa:
      WRITE(11,1843,ERR=3501) BMD_FM_MIWA

C    -AER.BMD.FM.MRwaref:
      WRITE(11,1844,ERR=3501) BMD_FM_MRWAREF

C     -AER.BMD.FM.MIwaref:
      WRITE(11,1845,ERR=3501) BMD_FM_MIWAREF

C     -AER.BMD.FM.SDradius:
      WRITE(11,1846,ERR=3501) BMD_FM_RMODAL

C     -AER.BMD.FM.SDvar:
      WRITE(11,1847,ERR=3501) BMD_FM_VAR

C     -AER.ExtData:
      LFIC = INDEX(FICEXTDATA_AER,' ') - 1
      WRITE(11,1848,ERR=3501) FICEXTDATA_AER(1:LFIC)

C     -AER.MieLog:
      LFIC = INDEX(FICAER_LOG_MIE,' ') - 1
      WRITE(11,1849,ERR=3501) FICAER_LOG_MIE(1:LFIC)

C     -AER.DirMie:
      LDIR = INDEX(DIRMIE_AER,' ') - 1
      WRITE(11,1850,ERR=3501) DIRMIE_AER(1:LDIR)

C     -AER.ResFile:
      LFIC = INDEX(FICGRANU_AER,' ') - 1
      WRITE(11,1851,ERR=3501) FICGRANU_AER(1:LFIC)

C     -AER.Log:
      LFIC = INDEX(FICAER_LOG,' ') - 1
      WRITE(11,1852,ERR=3501) FICAER_LOG(1:LFIC)


C ------- Arguments propres à la définition des modèles de hydrosols	
C     -PHYTO.ResFile:
      LFIC = INDEX(FICGRANU_PHYTO,' ') - 1
      WRITE(11,1853,ERR=3501) FICGRANU_PHYTO(1:LFIC)

C     -MLP.ResFile:
      LFIC = INDEX(FICGRANU_MLP,' ') - 1
      WRITE(11,1854,ERR=3501) FICGRANU_MLP(1:LFIC)

C     -HYD.Log:
      LFIC = INDEX(FICHYD_LOG,' ') - 1
      WRITE(11,1855,ERR=3501) FICHYD_LOG(1:LFIC)

C     -HYD.DirMie:
      LDIR = INDEX(DIRMIE_HYD,' ') - 1
      WRITE(11,1856,ERR=3501) DIRMIE_HYD(1:LDIR)

C     -HYD.MieLog:
      LFIC = INDEX(FICHYD_LOG_MIE,' ') - 1
      WRITE(11,1857,ERR=3501) FICHYD_LOG_MIE(1:LFIC)

C     -HYD.Model:
      WRITE(11,1858,ERR=3501) IMOD_HYD

C     -PHYTO.JD.slope:
      WRITE(11,1859,ERR=3501)PHYTO_JD_SLOPE

C     -PHYTO.JD.rmin:
      WRITE(11,1860,ERR=3501) PHYTO_JD_RMIN

C     -PHYTO.JD.rmax:
      WRITE(11,1861,ERR=3501) PHYTO_JD_RMAX

C     -PHYTO.JD.MRwa:
      WRITE(11,1862,ERR=3501) PHYTO_JD_MR 

C     -PHYTO.JD.MIwa:
      WRITE(11,1863,ERR=3501) PHYTO_JD_MI 

C     -PHYTO.JD.rate:
      WRITE(11,1864,ERR=3501) PHYTO_JD_RATE

C     -PHYTO.LND.SM.SDradius:
      WRITE(11,1865,ERR=3501) PHYTO_LND_SM_RADIUS

C     -PHYTO.LND.SM.SDvar:
      WRITE(11,1866,ERR=3501) PHYTO_LND_SM_VAR

C     -PHYTO.LND.SM.MRwa:
      WRITE(11,1867,ERR=3501) PHYTO_LND_SM_MR 

C     -PHYTO.LND.SM.MIwa:
      WRITE(11,1868,ERR=3501) PHYTO_LND_SM_MI

C     -PHYTO.LND.SM.rate:
      WRITE(11,1869,ERR=3501) PHYTO_LND_SM_RATE

C     -PHYTO.LND.TM.SDradius:
      WRITE(11,1870,ERR=3501) PHYTO_LND_TM_RADIUS

C     -PHYTO.LND.TM.SDvar:
      WRITE(11,1871,ERR=3501) PHYTO_LND_TM_VAR 

C     -PHYTO.LND.TM.MRwa:
      WRITE(11,1872,ERR=3501) PHYTO_LND_TM_MR 

C     -PHYTO.LND.TM.MIwa:
      WRITE(11,1873,ERR=3501) PHYTO_LND_TM_MI

C     -PHYTO.LND.TM.rate:
      WRITE(11,1874,ERR=3501) PHYTO_LND_TM_RATE

C     -SED.JD.slope:
      WRITE(11,1875,ERR=3501) SED_JD_SLOPE

C     -SED.JD.rmin:
      WRITE(11,1876,ERR=3501) SED_JD_RMIN

C     -SED.JD.rmax:
      WRITE(11,1877,ERR=3501) SED_JD_RMAX

C     -SED.JD.MRwa:
      WRITE(11,1878,ERR=3501) SED_JD_MR

C     -SED.JD.MIwa:
      WRITE(11,1879,ERR=3501) SED_JD_MI

C     -SED.JD.rate:
      WRITE(11,1880,ERR=3501) SED_JD_RATE

C     -SED.LND.SM.SDradius:
      WRITE(11,1881,ERR=3501) SED_LND_SM_RADIUS

C     -SED.LND.SM.SDvar:
      WRITE(11,1882,ERR=3501) SED_LND_SM_VAR

C     -SED.LND.SM.MRwa:
      WRITE(11,1883,ERR=3501) SED_LND_SM_MR

C     -SED.LND.SM.MIwa:
      WRITE(11,1884,ERR=3501) SED_LND_SM_MI

C     -SED.LND.SM.rate:
      WRITE(11,1885,ERR=3501) SED_LND_SM_RATE

C     -SED.LND.TM.SDradius:
      WRITE(11,1886,ERR=3501) SED_LND_TM_RADIUS

C     -SED.LND.TM.SDvar:
      WRITE(11,1887,ERR=3501) SED_LND_TM_VAR

C     -SED.LND.TM.MRwa:
      WRITE(11,1888,ERR=3501) SED_LND_TM_MR 

C     -SED.LND.TM.MIwa:
      WRITE(11,1889,ERR=3501) SED_LND_TM_MI

C     -SED.LND.TM.rate:
      WRITE(11,1890,ERR=3501) SED_LND_TM_RATE

C     -HYD.ExtData:
      LFIC = INDEX(FICEXTDATA_HYD,' ') - 1
      WRITE(11,1891,ERR=3501) FICEXTDATA_HYD(1:LFIC)
      
      
C ------- Arguments propres à la définition des profils atmosphérique et marin	
C     -AP.MOT:
      WRITE(11,1892,ERR=3501) TR

C     -AP.Pressure:
      WRITE(11,1893,ERR=3501) PRESSION

C     -AP.HR:
      WRITE(11,1894,ERR=3501) HR

C     -AP.HA:
      WRITE(11,1895,ERR=3501) HA

C     -SEA.Depth:
      WRITE(11,1896,ERR=3501) SEA_DEPTH

C     -PHYTO.Chl:
      WRITE(11,1897,ERR=3501) CHL

C     -PHYTO.ProfilType:
      WRITE(11,1898,ERR=3501) IPHYTO_PROFIL

C     -PHYTO.GP.Chlbg:
      WRITE(11,1899,ERR=3501) CHL_GP_BG

C     -PHYTO.GP.Deep:
      WRITE(11,1900,ERR=3501) CHL_GP_DEEP

C     -PHYTO.GP.Width:
      WRITE(11,1901,ERR=3501) CHL_GP_WIDTH

C     -PHYTO.Userfile:
      WRITE(11,1902,ERR=3501) FICPHYTO_USER

C     -SED.Csed:
      WRITE(11,1903,ERR=3501) SED_CSED 

C     -YS.Abs440:
      WRITE(11,1904,ERR=3501) YS_A440

C     -YS.Swa:
      WRITE(11,1905,ERR=3501) YS_SWA   

C     -DET.Abs440:
      WRITE(11,1906,ERR=3501) DET_A440

C     -DET.Swa:
      WRITE(11,1907,ERR=3501) DET_SWA

C     -PROFILE_ATM.ResFile:
      LFIC = INDEX(FICPROFIL_ATM_RES,' ') - 1
      WRITE(11,1908,ERR=3501) FICPROFIL_ATM_RES(1:LFIC)

C     -PROFILE_SEA.ResFile:
      LFIC = INDEX( FICPROFIL_SEA_RES,' ') - 1
      WRITE(11,1909,ERR=3501) FICPROFIL_SEA_RES(1:LFIC)

C     -PROFILE.Log:
      LFIC = INDEX(FICPROFIL_LOG,' ') - 1
      WRITE(11,1910,ERR=3501) FICPROFIL_LOG(1:LFIC)


C ------- Arguments propres à la définition de l'interface air / mer
C     -SEA.Ind:
      WRITE(11,1911,ERR=3501) SEA_IND

C     -SEA.Wind:
      WRITE(11,1912,ERR=3501) WIND

C     -SEA.Dir:
      LDIR = INDEX(DIRSEA,' ') - 1
      WRITE(11,1913,ERR=3501) DIRSEA(1:LDIR)

C     -SEA.Log:
      LFIC = INDEX(FICSURF_LOG,' ') - 1
      WRITE(11,1914,ERR=3501) FICSURF_LOG(1:LFIC)


C ------- Arguments propres aux calculs d'Ordres Successifs et à la production
C         des fichiers résultats
C     -SEA.SurfAlb:
      WRITE(11,1915,ERR=3501) RHO_SURF

C     -SEA.BotType:
      WRITE(11,1916,ERR=3501) IBOT_TYPE

C     -SEA.BotAlb:
      WRITE(11,1917,ERR=3501) RHO_BOT

C     -OSOAA.View.Phi:
      WRITE(11,1918,ERR=3501) PHI 

C     -OSOAA.View.Level:
      WRITE(11,1919,ERR=3501) LEVEL_OUT

C     -OSOAA.View.Z:
      WRITE(11,1920,ERR=3501) USER_Z_OUT
      
C     -OSOAA.ResFile.vsVZA:
      LFIC = INDEX(FICSOS_RES_VS_VZA,' ') - 1
      WRITE(11,1921,ERR=3501) FICSOS_RES_VS_VZA(1:LFIC)
      
C     -OSOAA.View.VZA:
      WRITE(11,1922,ERR=3501) VZA_OUT
      
C     -OSOAA.ResFile.vsZ:
      LFIC = INDEX(FICSOS_RES_VS_Z,' ') - 1
      WRITE(11,1923,ERR=3501) FICSOS_RES_VS_Z(1:LFIC)
      
C     -OSOAA.ResFile.Adv.Up:
      LFIC = INDEX(FICSOS_RES_ADV_UP,' ') - 1
      WRITE(11,1924,ERR=3501) FICSOS_RES_ADV_UP(1:LFIC)
      
C     -OSOAA.ResFile.Adv.Down:
      LFIC = INDEX(FICSOS_RES_ADV_DOWN,' ') - 1
      WRITE(11,1925,ERR=3501) FICSOS_RES_ADV_DOWN(1:LFIC)
      
C     -SOS.Log:
      LFIC = INDEX(FICSOS_LOG,' ') - 1
      WRITE(11,1926,ERR=3501) FICSOS_LOG(1:LFIC)
      
C     -SOS.IGmax:
      WRITE(11,1927,ERR=3501) IGMAX
      
C     -SOS.ResFile.Bin:
      LFIC = INDEX(FICSOS_RES_BIN,' ') - 1
      WRITE(11,1928,ERR=3501) FICSOS_RES_BIN(1:LFIC)

      CLOSE(11)
      


C =========================================================
C --- Contrôle si le traitement a déjà été réalisé pour un 
C     azimut relatif différent
C
C     --> Vérification de l'identité du fichier OSOAA.h utilisé
C
C     --> Vérification de l'identité des paramètres de traitement
C         en entrée de OSOAA_MAIN.
C         Seuls les paramètres suivants ne sont pas comparés:
C		-OSOAA.ResRoot
C            Fichiers Trace: 
C		-OSOAA.Log	-ANG.Log  	-SOS.Log   
C        	-AER.MieLog	-AER.Log
C      		-HYD.MieLog	-HYD.Log	 
C        	-PROFILE.Log	-SEA.Log
C            Fichier des angles pour les calculs de Mie: 
C		-ANG.Mie.ResFile
C            Répertoires de stockage des fichiers MIE et BRDF/BPDF:
C        	-AER.DirMie	-HYD.DirMie	-SEA.Dir
C	     Paramètres de type de sorties utilisant le fichier de champ 
C            de luminance en séries de Fourier:         
C        	-OSOAA.View.Phi
C	        -OSOAA.View.Level	-OSOAA.View.Z
C        	-OSOAA.ResFile.vsVZA	-OSOAA.View.VZA    
C   	     	-OSOAA.ResFile.vsZ   
C        	-OSOAA.ResFile.Adv.Up	-OSOAA.ResFile.Adv.Down
C        
C =========================================================         

C --- Contrôle de l'égalité des paramètres de OSOAA.h (config.txt)
C     utilisés pour compiler le code
C ---------------------------------------------------------------

C --- Chemin d'accès à config.txt sous $OSOAA_ROOT/exe
C     (copie de OSOAA.h appliqué lors de la compilation)   
      CALL GETENV('OSOAA_ROOT',DIREXE)
      IFIN=INDEX(DIREXE,' ') - 1
      
      IF (IFIN.LE.0) GOTO 4000
      IF ((IFIN+4).GT.CTE_LENDIR) GOTO 4001
      
      DIREXE(IFIN+1:IFIN+4)='/exe'
      IFIN=IFIN+4

C --- Contrôle si config.txt existe sous l'espace des résultats
C     (copie de config.txt sous $OSOAA_ROOT/exe lors de l'execution 
C     d'une simulation précédente)  
      LDIR = INDEX(RESROOT,' ') - 1
      
      INQUIRE(FILE=RESROOT(1:LDIR)//'/config.txt',EXIST=EX)

C --- Si le fichier config.txt existe sous l'espace des résultats, 
C     on contrôle qu'il est identique à celui qui a été utilisé 
C     pour compiler le code 
C     (pas de nouvelle compilation avec un fichier OSOAA.h modifié)    
C
C     S'il n'existe pas on le positionne par copie de celui sous 
C     $OSOAA_ROOT/exe   
      IF (EX) THEN
            
         COMMAND = 'diff '//DIREXE(1:IFIN)//'/config.txt '
     &            //RESROOT(1:LDIR)//'/config.txt > out_diff' 
         CALL SYSTEM(COMMAND)      
         
	 !Si le fichier out_diff n'est pas vide c'est qu'il y a eu une 
	 !recompilation avec un fichier OSOAA.h modifié
	 OPEN(11,FILE="out_diff",ERR=35011)
	 READ(11,*,IOSTAT=IO) LIGNE_TEXTE 	 

	 IF(IO.GE.0)  CAS_REPRISE=.FALSE.
	 
	 CLOSE(11,STATUS='DELETE',ERR=4004)

      ELSE		
         COMMAND = 'cp '//DIREXE(1:IFIN)//'/config.txt '
     &             //RESROOT(1:LDIR)//'/config.txt'
         CALL SYSTEM(COMMAND)
	 
	 CAS_REPRISE=.FALSE. 
      ENDIF


      IF (TRACE) THEN
 
          WRITE (IDLOG,*,ERR=910) " "
	  WRITE (IDLOG,*,ERR=910) " "
	  WRITE (IDLOG,*,ERR=910) 
     &	  "--> Starting the check of reprocessing with same parameters"
	  WRITE (IDLOG,*,ERR=910) " "  
          IF (CAS_REPRISE.EQV..TRUE.) THEN 
	     WRITE (IDLOG,*,ERR=910) 
     &	     "No change in OSOAA.h file since last process"
          ELSE
	     WRITE (IDLOG,*,ERR=910) 
     &       "First process on this working folder."
	     WRITE (IDLOG,*,ERR=910) 
     &       "A copy of OSOAA.h is made in ", 
     &       RESROOT(1:LDIR)//'/config.txt '
	  ENDIF 
          WRITE (IDLOG,*,ERR=910) " "
	   	  
      ENDIF ! Fin Trace  

       
C --- Contrôle de l'égalité des paramètres du script de lancement
C ---------------------------------------------------------------
      IF (CAS_REPRISE.EQV..TRUE.) THEN !Si pas déjà rejeté par défaut de fichier 
      				       !ListParam.txt d'un traitement précédent
				       !ou par modification de OSOAA.h du code
				       !compilé

         OPEN(11,FILE="copy_ListParam.txt",ERR=3502)

C        -OSOAA.ResRoot: 
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE
		
	 
C        -OSOAA.Log:      
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE

C        -OSOAA.Wa:   
         CALL OSOAA_COMPA_SIMU_VALUE(WA_SIMU,"DBL",'-OSOAA.Wa:',
     &                               11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -ANG.Rad.NbGauss:   
         CALL OSOAA_COMPA_SIMU_VALUE(DBLE(NBMU_GAUSS_LUM),"INT",
     &             '-ANG.Rad.NbGauss:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -ANG.Rad.UserAngFile: 
         CALL OSOAA_COMPA_SIMU_FILENAME(
     &          FICANGLES_USER_LUM,'-ANG.Rad.UserAngFile:',
     &          11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3504    
      
C        -ANG.Thetas:   
         CALL OSOAA_COMPA_SIMU_VALUE(TETAS,"DBL",'-ANG.Thetas:',
     &                            11,TRACE,CAS_REPRISE,IER)   
         IF(IER.NE.0) GOTO 3505	
            
C        -ANG.Rad.ResFile: Lu par OSOAA_SOS
         CALL OSOAA_COMPA_SIMU_FILENAME(
     &          FICANGLES_RES_LUM,'-ANG.Rad.ResFile:',
     &          11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3504

C        -ANG.Mie.NbGauss:  
         CALL OSOAA_COMPA_SIMU_VALUE(DBLE(NBMU_GAUSS_MIE),"INT",
     &          '-ANG.Mie.NbGauss:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -ANG.Mie.UserAngFile:
         CALL OSOAA_COMPA_SIMU_FILENAME(
     &          FICANGLES_USER_MIE,'-ANG.Mie.UserAngFile:',
     &          11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3504

C        -ANG.Mie.ResFile: Non lu par OSOAA_SOS    
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE
      
C        -ANG.Log:      
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE

C        -AER.Waref:
         CALL OSOAA_COMPA_SIMU_VALUE(WAREF_AOT,"DBL",'-AER.Waref:',
     &                               11,TRACE,CAS_REPRISE,IER)   
         IF(IER.NE.0) GOTO 3505	
   	  				
C        -AER.AOTref:
         CALL OSOAA_COMPA_SIMU_VALUE(AOT_REF,"DBL",'-AER.AOTref:',
     &                               11,TRACE,CAS_REPRISE,IER)   
         IF(IER.NE.0) GOTO 3505	
      
C        -AER.Tronca:
         CALL OSOAA_COMPA_SIMU_VALUE(DBLE(ITRONC_AER),"INT",
     &             '-AER.Tronca:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  	 
      		 
C        -AER.Model:
         CALL OSOAA_COMPA_SIMU_VALUE(DBLE(IMOD_AER),"INT",
     &             '-AER.Model:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  	
	  
C        -AER.MMD.MRwa:
         CALL OSOAA_COMPA_SIMU_VALUE(RN_MMD_AER,"DBL",
     &             '-AER.MMD.MRwa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  
      
C        -AER.MMD.MIwa:
         CALL OSOAA_COMPA_SIMU_VALUE(IN_MMD_AER,"DBL",
     &             '-AER.MMD.MIwaX:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  
             	  				
C        -AER.MMD.MRwaref: 
         CALL OSOAA_COMPA_SIMU_VALUE(RN_MMD_AER_WAREF,"DBL",
     &             '-AER.MMD.MRwaref:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  
            
C        -AER.MMD.MIwaref:
         CALL OSOAA_COMPA_SIMU_VALUE(IN_MMD_AER_WAREF,"DBL",
     &             '-AER.MMD.MIwaref:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  
      	 		 
C        -AER.MMD.SDtype:
         CALL OSOAA_COMPA_SIMU_VALUE(DBLE(IGRANU_MMD_AER),"INT",
     &             '-AER.MMD.SDtype:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.MMD.LNDradius:
         CALL OSOAA_COMPA_SIMU_VALUE(LND_RADIUS_MMD_AER,"DBL",
     &             '-AER.MMD.LNDradius:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.MMD.LNDvar:
         CALL OSOAA_COMPA_SIMU_VALUE(LND_LNVAR_MMD_AER,"DBL",
     &             '-AER.MMD.LNDvar:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.MMD.JD.slope:
         CALL OSOAA_COMPA_SIMU_VALUE(JD_SLOPE_MMD_AER,"DBL",
     &             '-AER.MMD.JD.slope:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.MMD.JD.rmin:
         CALL OSOAA_COMPA_SIMU_VALUE(JD_RMIN_MMD_AER,"DBL",
     &             '-AER.MMD.JD.rmin:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.MMD.JD.rmax:
         CALL OSOAA_COMPA_SIMU_VALUE(JD_RMAX_MMD_AER,"DBL",
     &             '-AER.MMD.JD.rmax:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  
	 
C        -AER.WMO.Model:
         CALL OSOAA_COMPA_SIMU_VALUE(DBLE(IMODELE_WMO),"INT",
     &             '-AER.WMO.Model:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.WMO.DL:
         CALL OSOAA_COMPA_SIMU_VALUE(C_WMO_DL,"DBL",
     &             '-AER.WMO.DL:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.WMO.WS:
         CALL OSOAA_COMPA_SIMU_VALUE(C_WMO_WS,"DBL",
     &             'AER.WMO.W:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.WMO.OC:
         CALL OSOAA_COMPA_SIMU_VALUE(C_WMO_OC,"DBL",
     &             '-AER.WMO.OC:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.WMO.SO:
         CALL OSOAA_COMPA_SIMU_VALUE(C_WMO_SO,"DBL",
     &             '-AER.WMO.SO:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.SF.Model:
         CALL OSOAA_COMPA_SIMU_VALUE(DBLE(IMODELE_SF),"INT",
     &             '-AER.SF.Model:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.SF.RH:
         CALL OSOAA_COMPA_SIMU_VALUE(RH,"DBL",
     &             '-AER.SF.RH:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.BMD.VCdef:
         CALL OSOAA_COMPA_SIMU_VALUE(DBLE(MODE_PARAM_BILND),"INT",
     &             '-AER.BMD.VCdef:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.BMD.CoarseVC:
         CALL OSOAA_COMPA_SIMU_VALUE(USER_CV_COARSE,"DBL",
     &             '-AER.BMD.CoarseVC:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.BMD.FineVC:
         CALL OSOAA_COMPA_SIMU_VALUE(USER_CV_FINE,"DBL",
     &             '-AER.BMD.FineV:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.BMD.RAOT:
         CALL OSOAA_COMPA_SIMU_VALUE(RTAUct_WAREF,"DBL",
     &             '-AER.BMD.RAOT:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.BMD.CM.MRwa:
         CALL OSOAA_COMPA_SIMU_VALUE(BMD_CM_MRWA,"DBL",
     &             '-AER.BMD.CM.MRwa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.BMD.CM.MIwa:
         CALL OSOAA_COMPA_SIMU_VALUE(BMD_CM_MIWA,"DBL",
     &             '-AER.BMD.CM.MIwa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.BMD.CM.MRwaref:
         CALL OSOAA_COMPA_SIMU_VALUE(BMD_CM_MRWAREF,"DBL",
     &             '-AER.BMD.CM.MRwaref:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.BMD.CM.MIwaref:
         CALL OSOAA_COMPA_SIMU_VALUE(BMD_CM_MIWAREF,"DBL",
     &             '-AER.BMD.CM.MIwaref:',11,TRACE,CAS_REPRISE,IER)

C        -AER.BMD.CM.SDradius:
         CALL OSOAA_COMPA_SIMU_VALUE(BMD_CM_RMODAL,"DBL",
     &             '-AER.BMD.CM.SDradius:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.BMD.CM.SDvar:
         CALL OSOAA_COMPA_SIMU_VALUE(BMD_CM_VAR,"DBL",
     &             '-AER.BMD.CM.SDvar:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.BMD.FM.MRwa:
         CALL OSOAA_COMPA_SIMU_VALUE(BMD_FM_MRWA,"DBL",
     &             '-AER.BMD.FM.MRwa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.BMD.FM.MIwa:
         CALL OSOAA_COMPA_SIMU_VALUE(BMD_FM_MIWA,"DBL",
     &             '-AER.BMD.FM.MIwa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C       -AER.BMD.FM.MRwaref:
         CALL OSOAA_COMPA_SIMU_VALUE(BMD_FM_MRWAREF,"DBL",
     &             '-AER.BMD.FM.MRwaref:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.BMD.FM.MIwaref:
         CALL OSOAA_COMPA_SIMU_VALUE(BMD_FM_MIWAREF,"DBL",
     &             '-AER.BMD.FM.MIware:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.BMD.FM.SDradius:
         CALL OSOAA_COMPA_SIMU_VALUE(BMD_FM_RMODAL,"DBL",
     &             '-AER.BMD.FM.SDradius:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.BMD.FM.SDvar:
         CALL OSOAA_COMPA_SIMU_VALUE(BMD_FM_VAR,"DBL",
     &             '-AER.BMD.FM.SDvar:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AER.ExtData:
         CALL OSOAA_COMPA_SIMU_FILENAME(FICEXTDATA_AER,'-AER.ExtData:',
     &          			11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3504  
	 	 
C        -AER.MieLog:
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE

C        -AER.DirMie:
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE

C        -AER.ResFile: Lu par OSOAA_SOS
         CALL OSOAA_COMPA_SIMU_FILENAME(FICGRANU_AER,'-AER.ResFile:',
     &          			11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3504  

C        -AER.Log:
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE

C        -PHYTO.ResFile: Lu par OSOAA_SOS
         CALL OSOAA_COMPA_SIMU_FILENAME(FICGRANU_PHYTO,
     &                  '-PHYTO.ResFile:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3504  

C        -MLP.ResFile: Lu par OSOAA_SOS
         CALL OSOAA_COMPA_SIMU_FILENAME(FICGRANU_MLP,'-MLP.ResFilex:',
     &          			11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3504  

C        -HYD.Log:
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE

C        -HYD.DirMie:
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE

C        -HYD.MieLog:
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE

C        -HYD.Model:
         CALL OSOAA_COMPA_SIMU_VALUE(DBLE(IMOD_HYD),"INT",
     &             '-HYD.Model:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.JD.slope:
         CALL OSOAA_COMPA_SIMU_VALUE(PHYTO_JD_SLOPE,"DBL",
     &             '-PHYTO.JD.slope:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.JD.rmin:
         CALL OSOAA_COMPA_SIMU_VALUE(PHYTO_JD_RMIN,"DBL",
     &             '-PHYTO.JD.rmin:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.JD.rmax:
         CALL OSOAA_COMPA_SIMU_VALUE(PHYTO_JD_RMAX,"DBL",
     &             '-PHYTO.JD.rmax:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.JD.MRwa:
         CALL OSOAA_COMPA_SIMU_VALUE(PHYTO_JD_MR,"DBL",
     &             '-PHYTO.JD.MRwa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.JD.MIwa:
         CALL OSOAA_COMPA_SIMU_VALUE(PHYTO_JD_MI,"DBL",
     &             '-PHYTO.JD.MIwa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.JD.rate: 
         CALL OSOAA_COMPA_SIMU_VALUE(PHYTO_JD_RATE,"DBL",
     &             '-PHYTO.JD.rate:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.LND.SM.SDradius:
         CALL OSOAA_COMPA_SIMU_VALUE(PHYTO_LND_SM_RADIUS,"DBL",
     &             '-PHYTO.LND.SM.SDradius:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.LND.SM.SDvar:
         CALL OSOAA_COMPA_SIMU_VALUE(PHYTO_LND_SM_VAR,"DBL",
     &             '-PHYTO.LND.SM.SDvar:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.LND.SM.MRwa: 
         CALL OSOAA_COMPA_SIMU_VALUE(PHYTO_LND_SM_MR,"DBL",
     &             '-PHYTO.LND.SM.MRwa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.LND.SM.MIwa:
         CALL OSOAA_COMPA_SIMU_VALUE(PHYTO_LND_SM_MI,"DBL",
     &             '-PHYTO.LND.SM.MIwa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.LND.SM.rate:
         CALL OSOAA_COMPA_SIMU_VALUE(PHYTO_LND_SM_RATE,"DBL",
     &             '-PHYTO.LND.SM.rate:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.LND.TM.SDradius:
         CALL OSOAA_COMPA_SIMU_VALUE(PHYTO_LND_TM_RADIUS,"DBL",
     &             '-PHYTO.LND.TM.SDradius:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.LND.TM.SDvar:
         CALL OSOAA_COMPA_SIMU_VALUE(PHYTO_LND_TM_VAR,"DBL",
     &             '-PHYTO.LND.TM.SDvar:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.LND.TM.MRwa:
         CALL OSOAA_COMPA_SIMU_VALUE(PHYTO_LND_TM_MR,"DBL",
     &             '-PHYTO.LND.TM.MRwa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.LND.TM.MIwa:
         CALL OSOAA_COMPA_SIMU_VALUE(PHYTO_LND_TM_MI,"DBL",
     &             '-PHYTO.LND.TM.MIwa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.LND.TM.rate:
         CALL OSOAA_COMPA_SIMU_VALUE(PHYTO_LND_TM_RATE,"DBL",
     &             '-PHYTO.LND.TM.rate:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SED.JD.slope:
         CALL OSOAA_COMPA_SIMU_VALUE(SED_JD_SLOPE,"DBL",
     &             '-SED.JD.slope:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SED.JD.rmin:
         CALL OSOAA_COMPA_SIMU_VALUE(SED_JD_RMIN,"DBL",
     &             '-SED.JD.rmin:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SED.JD.rmax:
         CALL OSOAA_COMPA_SIMU_VALUE(SED_JD_RMAX,"DBL",
     &             '-SED.JD.rmax:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SED.JD.MRwa: 
         CALL OSOAA_COMPA_SIMU_VALUE(SED_JD_MR,"DBL",
     &             '-SED.JD.MRwa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SED.JD.MIwa:
         CALL OSOAA_COMPA_SIMU_VALUE(SED_JD_MI,"DBL",
     &             '-SED.JD.MIwa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SED.JD.rate:
         CALL OSOAA_COMPA_SIMU_VALUE(SED_JD_RATE,"DBL",
     &             '-SED.JD.rate:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SED.LND.SM.SDradius:
         CALL OSOAA_COMPA_SIMU_VALUE(SED_LND_SM_RADIUS,"DBL",
     &             '-SED.LND.SM.SDradius:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SED.LND.SM.SDvar:
         CALL OSOAA_COMPA_SIMU_VALUE(SED_LND_SM_VAR,"DBL",
     &             '-SED.LND.SM.SDvar:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SED.LND.SM.MRwa:
          CALL OSOAA_COMPA_SIMU_VALUE(SED_LND_SM_MR,"DBL",
     &             '-SED.LND.SM.MRwa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SED.LND.SM.MIwa:
         CALL OSOAA_COMPA_SIMU_VALUE(SED_LND_SM_MI,"DBL",
     &             '-SED.LND.SM.MIwa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SED.LND.SM.rate:
         CALL OSOAA_COMPA_SIMU_VALUE(SED_LND_SM_RATE,"DBL",
     &             '-SED.LND.SM.rate:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SED.LND.TM.SDradius:
         CALL OSOAA_COMPA_SIMU_VALUE(SED_LND_TM_RADIUS,"DBL",
     &             '-SED.LND.TM.SDradiu:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SED.LND.TM.SDvar:
         CALL OSOAA_COMPA_SIMU_VALUE(SED_LND_TM_VAR,"DBL",
     &             '-SED.LND.TM.SDvar:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SED.LND.TM.MRwa: 
         CALL OSOAA_COMPA_SIMU_VALUE(SED_LND_TM_MR,"DBL",
     &             '-SED.LND.TM.MRwa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SED.LND.TM.MIwa:
         CALL OSOAA_COMPA_SIMU_VALUE(SED_LND_TM_MI,"DBL",
     &             '-SED.LND.TM.MIwa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SED.LND.TM.rate:
         CALL OSOAA_COMPA_SIMU_VALUE(SED_LND_TM_RATE,"DBL",
     &             '-SED.LND.TM.rate:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -HYD.ExtData:
         CALL OSOAA_COMPA_SIMU_FILENAME(FICEXTDATA_HYD,'-HYD.ExtData:',
     &          			11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3504  

C        -AP.MOT:
         CALL OSOAA_COMPA_SIMU_VALUE(TR,"DBL",
     &             '-AP.MOT:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AP.Pressure: 
         CALL OSOAA_COMPA_SIMU_VALUE(PRESSION,"DBL",
     &             '-AP.Pressure:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AP.HR:
         CALL OSOAA_COMPA_SIMU_VALUE(HR,"DBL",
     &             '-AP.HR:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -AP.HA:
         CALL OSOAA_COMPA_SIMU_VALUE(HA,"DBL",
     &             '-AP.HA:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SEA.Depth:
         CALL OSOAA_COMPA_SIMU_VALUE(SEA_DEPTH,"DBL",
     &             '-SEA.Depth:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.Chl:
         CALL OSOAA_COMPA_SIMU_VALUE(CHL,"DBL",
     &             '-PHYTO.Chl:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.ProfilType:
         CALL OSOAA_COMPA_SIMU_VALUE(DBLE(IPHYTO_PROFIL),"INT",
     &             '-PHYTO.ProfilType:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.GP.Chlbg:
         CALL OSOAA_COMPA_SIMU_VALUE(CHL_GP_BG,"DBL",
     &             '-PHYTO.GP.Chlbg:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.GP.Deep:
         CALL OSOAA_COMPA_SIMU_VALUE(CHL_GP_DEEP,"DBL",
     &             '-PHYTO.GP.Deep:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.GP.Width:
         CALL OSOAA_COMPA_SIMU_VALUE(CHL_GP_WIDTH,"DBL",
     &             '-PHYTO.GP.Width:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PHYTO.Userfile:
         CALL OSOAA_COMPA_SIMU_FILENAME(FICPHYTO_USER,
     &          	 '-PHYTO.Userfile:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3504  

C        -SED.Csed:
         CALL OSOAA_COMPA_SIMU_VALUE(SED_CSED,"DBL",
     &             '-SED.Csed:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -YS.Abs440:
         CALL OSOAA_COMPA_SIMU_VALUE(YS_A440,"DBL",
     &             '-YS.Abs440:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -YS.Swa:   
         CALL OSOAA_COMPA_SIMU_VALUE(YS_SWA,"DBL",
     &             '-YS.Swa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -DET.Abs440:
         CALL OSOAA_COMPA_SIMU_VALUE(DET_A440,"DBL",
     &             '-DET.Abs440:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -DET.Swa:
         CALL OSOAA_COMPA_SIMU_VALUE(DET_SWA,"DBL",
     &             '-DET.Swa:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -PROFILE_ATM.ResFile: lu par OSOAA_SOS
         CALL OSOAA_COMPA_SIMU_FILENAME(FICPROFIL_ATM_RES,
     &             '-PROFILE_ATM.ResFile:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3504  

C        -PROFILE_SEA.ResFile: lu par OSOAA_SOS
         CALL OSOAA_COMPA_SIMU_FILENAME(FICPROFIL_SEA_RES,
     &             '-PROFILE_SEA.ResFile:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3504  

C        -PROFILE.Log:
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE

C        -SEA.Ind:
         CALL OSOAA_COMPA_SIMU_VALUE(SEA_IND,"DBL",
     &             '-SEA.Ind:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SEA.Wind:
         CALL OSOAA_COMPA_SIMU_VALUE(WIND,"DBL",
     &             '-SEA.Wind:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SEA.Dir:
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE

C        -SEA.Log:
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE

C        -SEA.SurfAlb: 
         CALL OSOAA_COMPA_SIMU_VALUE(RHO_SURF,"DBL",
     &             '-SEA.SurfAlb:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SEA.BotType: 
         CALL OSOAA_COMPA_SIMU_VALUE(DBLE(IBOT_TYPE),"INT",
     &             '-SEA.BotType:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -SEA.BotAlb:
         CALL OSOAA_COMPA_SIMU_VALUE(RHO_BOT,"DBL",
     &             '-SEA.BotAl:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  

C        -OSOAA.View.Phi:
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE

C        -OSOAA.View.Level:
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE

C        -OSOAA.View.Z:
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE
      
C        -OSOAA.ResFile.vsVZA:
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE
      
C        -OSOAA.View.VZA:
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE
      
C        -OSOAA.ResFile.vsZ:
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE
      
C        -OSOAA.ResFile.Adv.Up:
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE
      
C        -OSOAA.ResFile.Adv.Down:
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE
      
C        -SOS.Log:
         READ(11,'(a)',ERR=3503) LIGNE_TEXTE
      
C        -SOS.IGmax:
         CALL OSOAA_COMPA_SIMU_VALUE(DBLE(IGMAX),"INT",
     &             '-SOS.IGmax:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3505  
      
C        -SOS.ResFile.Bin: Lu par OSOAA_TRPHI
         CALL OSOAA_COMPA_SIMU_FILENAME( FICSOS_RES_BIN,
     &          	 '-SOS.ResFile.Bin:',11,TRACE,CAS_REPRISE,IER)
         IF(IER.NE.0) GOTO 3504  
      
         CLOSE(11,STATUS='DELETE',ERR=3506)
	 
      ENDIF !Fin Si CAS_REPRISE=.TRUE. : Controle des parametres de simulation
      



C --- Gestion de la poursuite des traitements ou saut direct à OSOAA_SOS
   
      IF (CAS_REPRISE.EQV..TRUE.) THEN

         APPEL_SOS_CORE=.FALSE.
	  
         IF (TRACE) THEN

             IF (CAS_REPRISE.EQV..TRUE.) THEN 
	        WRITE (IDLOG,*,ERR=910) 
     &	        "No change in simulation parameters since last process"
                WRITE (IDLOG,*,ERR=910) 
     &          "except for output parameters after Fourier "
     &          //"series expansion (relative azimuth, viewing angle "
     &          //"or altitude output)"
                WRITE (IDLOG,*,ERR=910) 
     &		"==> Use of the previous radiance field "
     &          //"decomposed in Fourier expansion"
             ELSE
	        WRITE (IDLOG,*,ERR=910) 
     &          "Change in simulation parameters since last process" 
                WRITE (IDLOG,*,ERR=910) 
     &          "==>Simulation restarts from the beginning" 
	     ENDIF 
             WRITE (IDLOG,*,ERR=910) " "
	   	  
         ENDIF ! Fin Trace      
          
	  
	 GOTO 3000 !Passage à OSOAA_SOS directement avec APPEL_SOS_CORE=.FALSE.
	 
      ELSE

         APPEL_SOS_CORE=.TRUE.
      
      ENDIF
      


C =========================================================
C --- Préparation des angles pour les traitements
C =========================================================           				
      WRITE(6,*) '==> Angles calculation'
      CALL FLUSH(6)	!Force l'envoie du message à l'écran      

      SIN_TETASW = DSIN(TETAS*CONVDEGRAD) / SEA_IND
      TETASW = DASIN(SIN_TETASW)/CONVDEGRAD
      
      IF (TRACE) THEN
          WRITE (IDLOG,*,ERR=910) " "
	  WRITE (IDLOG,*,ERR=910) "ANGLES CALCULATION"
	  WRITE (IDLOG,*,ERR=910) "------------------"
          WRITE (IDLOG,*,ERR=910) 
     &	  "--> OSOAA_ANGLES is called with the parameters :"
	  WRITE (IDLOG,800,ERR=910) NBMU_GAUSS_LUM
	  WRITE (IDLOG,801,ERR=910) TETAS
	  WRITE (IDLOG,802,ERR=910) TETASW
	  WRITE (IDLOG,803,ERR=910) FICANGLES_USER_LUM
	  WRITE (IDLOG,804,ERR=910) NBMU_GAUSS_MIE
	  WRITE (IDLOG,805,ERR=910) FICANGLES_USER_MIE
	  WRITE (IDLOG,806,ERR=910) FICANGLES_LOG
	  WRITE (IDLOG,807,ERR=910) FICANGLES_RES_LUM
	  WRITE (IDLOG,808,ERR=910) FICANGLES_RES_MIE
      ENDIF
      
      CALL OSOAA_ANGLES(NBMU_GAUSS_LUM, TETAS, TETASW, 
     &                  FICANGLES_USER_LUM,                 
     &			NBMU_GAUSS_MIE, FICANGLES_USER_MIE, 
     &                  FICANGLES_LOG,
     &                  FICANGLES_RES_LUM, FICANGLES_RES_MIE, IER)
     
     	
      IF (IER.NE.0) THEN ! cas d'erreur
      		
          IF (TRACE) THEN
	    WRITE (IDLOG,*,ERR=910) 
     &	    "   ==> Error on subroutine OSOAA_ANGLES"
 	  ENDIF
	  GOTO 5000   
      ENDIF
      

C =========================================================
C --- Préparation de la matrice de phase des aérosols
C =========================================================  
      WRITE(6,*) '==> Aerosols radiative properties computation'
      CALL FLUSH(6)	!Force l'envoie du message à l'écran      
               
      IF (TRACE) THEN
          WRITE (IDLOG,*,ERR=910) " "
	  WRITE (IDLOG,*,ERR=910) " "
	  WRITE (IDLOG,*,ERR=910) 
     &	  "-----------------------------------------"
	  WRITE (IDLOG,*,ERR=910)
     &	  "AEROSOLS RADIATIVE PROPERTIES COMPUTATION"
	  WRITE (IDLOG,*,ERR=910) 
     &	  "-----------------------------------------"	  
      ENDIF !Fin Trace 	     



      IF (AOT_REF.EQ.0.D+00) THEN  ! Cas AOT_REF nulle

	 ! L'ep opt aérosol pour la longueur d'onde de simulation 
	 ! est nulle
	 TA = 0.0D+00

	 ! Génération d'un fichier Aérosol à coefficients nuls 
	 IF (TRACE) THEN
            WRITE (IDLOG,*,ERR=910) " "
	    WRITE (IDLOG,*,ERR=910) 
     &	     "=> Calculation of aerosol file for AOT nul"
            WRITE (IDLOG,*,ERR=910) 
     &	     "--> OSOAA_AEROSOLS is called with the parameters :"
	    WRITE (IDLOG,808,ERR=910) FICANGLES_RES_MIE
	    WRITE (IDLOG,841,ERR=910) WA_SIMU
	    WRITE (IDLOG,842,ERR=910) TA
	    WRITE (IDLOG,843,ERR=910) WAREF_AOT
	    WRITE (IDLOG,844,ERR=910) AOT_REF
	    WRITE (IDLOG,845,ERR=910) ITRONC_AER
	    WRITE (IDLOG,846,ERR=910) IMOD_AER
	    WRITE (IDLOG,*,ERR=910) "      ------------------"
	    WRITE (IDLOG,*,ERR=910) "      mono-modal model :"
	    WRITE (IDLOG,*,ERR=910) "      ------------------"
 	    WRITE (IDLOG,847,ERR=910) RN_MMD_AER, IN_MMD_AER 
	    WRITE (IDLOG,849,ERR=910) IGRANU_MMD_AER
	    WRITE (IDLOG,850,ERR=910) VARGRANU1_MMD_AER, 
     &	     			      VARGRANU2_MMD_AER, 
     &	     			      VARGRANU3_MMD_AER
	          
            WRITE (IDLOG,*,ERR=910) "      -----------"
	    WRITE (IDLOG,*,ERR=910) "      WMO model :"
	    WRITE (IDLOG,*,ERR=910) "      -----------"
            WRITE (IDLOG,851,ERR=910) IMODELE_WMO
	    WRITE (IDLOG,852,ERR=910) C_WMO_DL,C_WMO_WS,C_WMO_OC,
     &	    			      C_WMO_SO
	    WRITE (IDLOG,*,ERR=910) "      ----------------------"
	    WRITE (IDLOG,*,ERR=910) "      Shettle & Fenn model :"
	    WRITE (IDLOG,*,ERR=910) "      ----------------------"
	    WRITE (IDLOG,853,ERR=910) IMODELE_SF
	    WRITE (IDLOG,854,ERR=910) RH
	    WRITE (IDLOG,*,ERR=910) "      -----------------------"
	    WRITE (IDLOG,*,ERR=910) "      Bi-modal distribution :"
	    WRITE (IDLOG,*,ERR=910) "      -----------------------"
	    WRITE (IDLOG,855,ERR=910) MODE_PARAM_BILND
	    WRITE (IDLOG,856,ERR=910) USER_CV_COARSE, USER_CV_FINE
	    WRITE (IDLOG,857,ERR=910) RTAUct_WAREF
	    WRITE (IDLOG,*,ERR=910) "      Coarse mode :"
	    WRITE (IDLOG,858,ERR=910) BMD_CM_MRWA, BMD_CM_MIWA
	    WRITE (IDLOG,859,ERR=910) BMD_CM_MRWAREF, BMD_CM_MIWAREF
	    WRITE (IDLOG,860,ERR=910) BMD_CM_RMODAL, BMD_CM_VAR
	    WRITE (IDLOG,*,ERR=910) "      Fine mode :"
	    WRITE (IDLOG,858,ERR=910) BMD_FM_MRWA, BMD_FM_MIWA
	    WRITE (IDLOG,859,ERR=910) BMD_FM_MRWAREF, BMD_FM_MIWAREF
	    WRITE (IDLOG,860,ERR=910) BMD_FM_RMODAL, BMD_FM_VAR
	    WRITE (IDLOG,*,ERR=910) "      ---------------"
 	    WRITE (IDLOG,*,ERR=910) "      External data :"
	    WRITE (IDLOG,*,ERR=910) "      ---------------"
 	    WRITE (IDLOG,861,ERR=910) FICEXTDATA_AER  
	    WRITE (IDLOG,*,ERR=910) "      ------------------"
	    WRITE (IDLOG,*,ERR=910) "      MIE computations :"
	    WRITE (IDLOG,*,ERR=910) "      ------------------"
	    WRITE (IDLOG,864,ERR=910) DIRMIE_AER 
	    WRITE (IDLOG,865,ERR=910) FICAER_LOG_MIE 
	    WRITE (IDLOG,*,ERR=910) "      --------------"
	    WRITE (IDLOG,*,ERR=910) "      Output files :"
	    WRITE (IDLOG,*,ERR=910) "      --------------"
 	    WRITE (IDLOG,862,ERR=910) FICAER_LOG 
	    WRITE (IDLOG,863,ERR=910) FICGRANU_AER
         ENDIF !Fin Trace 
	          
	 ITRONC_AER_IN = ITRONC_AER  !Si ITRONC_AER=1 mais que le code estime la troncature
	 			     !inutile seul le paramètre ITRONC_AER_IN basculera	
				     !de 1 à 0 sans impacter la valeur de ITRONC_AER
	 CALL OSOAA_AEROSOLS(FICANGLES_RES_MIE, 
     &	    	             WA_SIMU, TA, WAREF_AOT, AOT_REF,	  
     &	  	             ITRONC_AER_IN, IMOD_AER,
     &	  	             RN_MMD_AER, IN_MMD_AER,	
     &	  	             IGRANU_MMD_AER, 
     &	  	             VARGRANU1_MMD_AER, VARGRANU2_MMD_AER, 
     &	  	             VARGRANU3_MMD_AER, IMODELE_WMO, 
     &	  	             C_WMO_DL, C_WMO_WS, C_WMO_OC, C_WMO_SO,
     &	  	             IMODELE_SF, RH, 
     &	  	             MODE_PARAM_BILND, 
     &	  	             USER_CV_COARSE, USER_CV_FINE, RTAUct_WAREF,
     &	  	             BMD_CM_MRWA, BMD_CM_MIWA, 
     &	  	             BMD_CM_MRWAREF, BMD_CM_MIWAREF,
     &	  	             BMD_CM_RMODAL, BMD_CM_VAR,
     &	  	             BMD_FM_MRWA, BMD_FM_MIWA, 
     &	  	             BMD_FM_MRWAREF, BMD_FM_MIWAREF,
     &	  	             BMD_FM_RMODAL, BMD_FM_VAR,	 
     &	  	             FICEXTDATA_AER, 
     &			     DIRMIE_AER, FICAER_LOG_MIE,
     &	  	             FICGRANU_AER, FICAER_LOG, KMAT1_WA,
     &			     PIZAER, IER)
		    
      ELSE ! Cas AOT_REF non nulle
      
         IF (IGRANU_MMD_AER.EQ.1) THEN
             VARGRANU1_MMD_AER = LND_RADIUS_MMD_AER
	     VARGRANU2_MMD_AER = LND_LNVAR_MMD_AER
	     VARGRANU3_MMD_AER = CTE_NOT_DEFINED_VALUE_DBLE
         ELSE
	     VARGRANU1_MMD_AER = JD_RMIN_MMD_AER
	     VARGRANU2_MMD_AER = JD_SLOPE_MMD_AER
	     VARGRANU3_MMD_AER = JD_RMAX_MMD_AER
         ENDIF
      
  	        
         IF (WA_SIMU.EQ.WAREF_AOT) THEN
             ! L'ep opt aérosol pour la longueur d'onde de simulation 
	     ! de la luminanceest celle de référence
	     TA = AOT_REF
	  
             ! Un appel unique de la fonction OSOAA_AEROSOLS pour WAREF_AOT
	     IF (TRACE) THEN
	        WRITE (IDLOG,*,ERR=910) "Case WA = WAREF"
                WRITE (IDLOG,*,ERR=910) 
     &	        "--> OSOAA_AEROSOLS is called with the parameters :"
	        WRITE (IDLOG,808,ERR=910) FICANGLES_RES_MIE
	        WRITE (IDLOG,841,ERR=910) WAREF_AOT
	        WRITE (IDLOG,842,ERR=910) AOT_REF 
	        WRITE (IDLOG,843,ERR=910) WAREF_AOT
	        WRITE (IDLOG,844,ERR=910) AOT_REF
	        WRITE (IDLOG,845,ERR=910) ITRONC_AER
	        WRITE (IDLOG,846,ERR=910) IMOD_AER
	        WRITE (IDLOG,*,ERR=910) "      ------------------"
	        WRITE (IDLOG,*,ERR=910) "      mono-modal model :"		
	        WRITE (IDLOG,*,ERR=910) "      ------------------"
	        WRITE (IDLOG,847,ERR=910) RN_MMD_AER,IN_MMD_AER
	        WRITE (IDLOG,849,ERR=910) IGRANU_MMD_AER
	        WRITE (IDLOG,850,ERR=910) VARGRANU1_MMD_AER, 
     &	     			          VARGRANU2_MMD_AER, 
     &	     			          VARGRANU3_MMD_AER
                WRITE (IDLOG,*,ERR=910) "      -----------"
	        WRITE (IDLOG,*,ERR=910) "      WMO model :"
	        WRITE (IDLOG,*,ERR=910) "      -----------"
                WRITE (IDLOG,851,ERR=910) IMODELE_WMO
	        WRITE (IDLOG,852,ERR=910) C_WMO_DL,C_WMO_WS,C_WMO_OC,
     &	    			          C_WMO_SO
                WRITE (IDLOG,*,ERR=910) "      ----------------------"
	        WRITE (IDLOG,*,ERR=910) "      Shettle & Fenn model :"
	        WRITE (IDLOG,*,ERR=910) "      ----------------------"
	        WRITE (IDLOG,853,ERR=910) IMODELE_SF
	        WRITE (IDLOG,854,ERR=910) RH
	        WRITE (IDLOG,*,ERR=910) "      -----------------------"
	        WRITE (IDLOG,*,ERR=910) "      Bi-modal distribution :"
	        WRITE (IDLOG,*,ERR=910) "      -----------------------"
	        WRITE (IDLOG,855,ERR=910) MODE_PARAM_BILND
	        WRITE (IDLOG,856,ERR=910) USER_CV_COARSE, USER_CV_FINE
	        WRITE (IDLOG,857,ERR=910) RTAUct_WAREF
	        WRITE (IDLOG,*,ERR=910) "      Coarse mode :"
	        WRITE (IDLOG,858,ERR=910) BMD_CM_MRWA, BMD_CM_MIWA  
	        WRITE (IDLOG,859,ERR=910) BMD_CM_MRWAREF, BMD_CM_MIWAREF
	        WRITE (IDLOG,860,ERR=910) BMD_CM_RMODAL, BMD_CM_VAR
	        WRITE (IDLOG,*,ERR=910) "      Fine mode :"
	        WRITE (IDLOG,858,ERR=910) BMD_FM_MRWA, BMD_FM_MIWA
	        WRITE (IDLOG,859,ERR=910) BMD_FM_MRWAREF, BMD_FM_MIWAREF
	        WRITE (IDLOG,860,ERR=910) BMD_FM_RMODAL, BMD_FM_VAR
	        WRITE (IDLOG,*,ERR=910) "      ---------------"
 	        WRITE (IDLOG,*,ERR=910) "      External data :"
	        WRITE (IDLOG,*,ERR=910) "      ---------------"
 	        WRITE (IDLOG,861,ERR=910) FICEXTDATA_AER  
	        WRITE (IDLOG,*,ERR=910) "      ------------------"
	        WRITE (IDLOG,*,ERR=910) "      MIE computations :"
	        WRITE (IDLOG,*,ERR=910) "      ------------------"
	        WRITE (IDLOG,864,ERR=910) DIRMIE_AER 
	        WRITE (IDLOG,865,ERR=910) FICAER_LOG_MIE 
	        WRITE (IDLOG,*,ERR=910) "      --------------"
	        WRITE (IDLOG,*,ERR=910) "      Output files :"
	        WRITE (IDLOG,*,ERR=910) "      --------------"
 	        WRITE (IDLOG,862,ERR=910) FICAER_LOG 
	        WRITE (IDLOG,863,ERR=910) FICGRANU_AER
             ENDIF !Fin Trace 
	  
	  
	     ITRONC_AER_IN = ITRONC_AER  !Si ITRONC_AER=1 mais que le code estime la 
	 			         !troncature inutile seul le paramètre ITRONC_AER_IN
				         !basculera de 1 à 0 
					 !sans impacter la valeur de ITRONC_AER
	     CALL OSOAA_AEROSOLS(FICANGLES_RES_MIE, 
     &	  	                 WAREF_AOT, AOT_REF, WAREF_AOT, AOT_REF,	  
     &	  	                 ITRONC_AER_IN, IMOD_AER,
     &	  	                 RN_MMD_AER, IN_MMD_AER, 	
     &	  	                 IGRANU_MMD_AER, 
     &	  	                 VARGRANU1_MMD_AER, VARGRANU2_MMD_AER, 
     &	  	                 VARGRANU3_MMD_AER, IMODELE_WMO, 
     &	  	                 C_WMO_DL, C_WMO_WS, C_WMO_OC, C_WMO_SO,
     &	  	                 IMODELE_SF, RH, 
     &	  	                 MODE_PARAM_BILND, 
     &	  	                 USER_CV_COARSE, USER_CV_FINE, 
     &	  	                 RTAUct_WAREF, 
     &	  	                 BMD_CM_MRWA, BMD_CM_MIWA,
     &	  	                 BMD_CM_MRWAREF, BMD_CM_MIWAREF,
     &	  	                 BMD_CM_RMODAL, BMD_CM_VAR,
     &	  	                 BMD_FM_MRWA, BMD_FM_MIWA,
     &	  	                 BMD_FM_MRWAREF, BMD_FM_MIWAREF,
     &	  	                 BMD_FM_RMODAL, BMD_FM_VAR,	 
     &	  	                 FICEXTDATA_AER, 
     &                           DIRMIE_AER, FICAER_LOG_MIE,
     &	  	                 FICGRANU_AER, FICAER_LOG, KMAT1_WAREF,
     &			         PIZAER, IER)
	 
	     IF (IER.NE.0) THEN ! cas d'erreur 
	  			
                 IF (TRACE) THEN
	             WRITE (IDLOG,*,ERR=910) 
     &	             "   ==> Error on subroutine OSOAA_AEROSOLS"
 	         ENDIF
	         GOTO 6000   
              ENDIF 	  
	   
         ELSE	!Cas WA != WAREF
          
             ! Appel de la fonction OSOAA_AEROSOLS pour WAREF_AOT
	     IF (TRACE) THEN
	        WRITE (IDLOG,*,ERR=910) "Case WA != WAREF"
                WRITE (IDLOG,*,ERR=910) 
     &	        "=> Calculation of aerosol extinction coeff. for WAREF"
     	        WRITE (IDLOG,*,ERR=910) 
     &	        "--> OSOAA_AEROSOLS is called with the parameters :"
	        WRITE (IDLOG,808,ERR=910) FICANGLES_RES_MIE
	        WRITE (IDLOG,841,ERR=910) WAREF_AOT
	        WRITE (IDLOG,842,ERR=910) AOT_REF
	        WRITE (IDLOG,843,ERR=910) WAREF_AOT
	        WRITE (IDLOG,844,ERR=910) AOT_REF
	        WRITE (IDLOG,845,ERR=910) ITRONC_AER
	        WRITE (IDLOG,846,ERR=910) IMOD_AER
	        WRITE (IDLOG,*,ERR=910) "      ------------------"
	        WRITE (IDLOG,*,ERR=910) "      mono-modal model :"
	        WRITE (IDLOG,*,ERR=910) "      ------------------"
	        WRITE (IDLOG,848,ERR=910) RN_MMD_AER_WAREF,
     &	             	  	          IN_MMD_AER_WAREF
	        WRITE (IDLOG,849,ERR=910) IGRANU_MMD_AER
	        WRITE (IDLOG,850,ERR=910) VARGRANU1_MMD_AER, 
     &	     			          VARGRANU2_MMD_AER, 
     &	     			          VARGRANU3_MMD_AER
	        WRITE (IDLOG,*,ERR=910) "      -----------"
	        WRITE (IDLOG,*,ERR=910) "      WMO model :"
	        WRITE (IDLOG,*,ERR=910) "      -----------"
                WRITE (IDLOG,851,ERR=910) IMODELE_WMO
	        WRITE (IDLOG,852,ERR=910) C_WMO_DL,C_WMO_WS,C_WMO_OC,
     &	    			          C_WMO_SO
                WRITE (IDLOG,*,ERR=910) "      ----------------------"
	        WRITE (IDLOG,*,ERR=910) "      Shettle & Fenn model :"
	        WRITE (IDLOG,*,ERR=910) "      ----------------------"
	        WRITE (IDLOG,853,ERR=910) IMODELE_SF
	        WRITE (IDLOG,854,ERR=910) RH
	        WRITE (IDLOG,*,ERR=910) "      -----------------------"
	        WRITE (IDLOG,*,ERR=910) "      Bi-modal distribution :"
	        WRITE (IDLOG,*,ERR=910) "      -----------------------"
	        WRITE (IDLOG,855,ERR=910) MODE_PARAM_BILND
	        WRITE (IDLOG,856,ERR=910) USER_CV_COARSE, USER_CV_FINE
	        WRITE (IDLOG,857,ERR=910) RTAUct_WAREF
	        WRITE (IDLOG,*,ERR=910) "      Coarse mode :"
	        WRITE (IDLOG,858,ERR=910) BMD_CM_MRWAREF, BMD_CM_MIWAREF  
	        WRITE (IDLOG,859,ERR=910) BMD_CM_MRWAREF, BMD_CM_MIWAREF
	        WRITE (IDLOG,860,ERR=910) BMD_CM_RMODAL, BMD_CM_VAR
	        WRITE (IDLOG,*,ERR=910) "      Fine mode :"
	        WRITE (IDLOG,858,ERR=910) BMD_FM_MRWAREF, BMD_FM_MIWAREF
	        WRITE (IDLOG,859,ERR=910) BMD_FM_MRWAREF, BMD_FM_MIWAREF
	        WRITE (IDLOG,860,ERR=910) BMD_FM_RMODAL, BMD_FM_VAR
	        WRITE (IDLOG,*,ERR=910) "      ---------------"
 	        WRITE (IDLOG,*,ERR=910) "      External data :"
	        WRITE (IDLOG,*,ERR=910) "      ---------------"
 	        WRITE (IDLOG,861,ERR=910) FICEXTDATA_AER  
	        WRITE (IDLOG,*,ERR=910) "      ------------------"
	        WRITE (IDLOG,*,ERR=910) "      MIE computations :"
	        WRITE (IDLOG,*,ERR=910) "      ------------------"
	        WRITE (IDLOG,864,ERR=910) DIRMIE_AER 
	        WRITE (IDLOG,865,ERR=910) FICAER_LOG_MIE 
	        WRITE (IDLOG,*,ERR=910) "      --------------"
	        WRITE (IDLOG,*,ERR=910) "      Output files :"
	        WRITE (IDLOG,*,ERR=910) "      --------------"
 	        WRITE (IDLOG,862,ERR=910) FICAER_LOG 
	        WRITE (IDLOG,863,ERR=910) FICGRANU_AER
             ENDIF !Fin Trace 
	  
	     ITRONC_AER_IN = ITRONC_AER  !Si ITRONC_AER=1 mais que le code estime la 
	 			         !troncature inutile seul le paramètre ITRONC_AER_IN
				         !basculera de 1 à 0 
					 !sans impacter la valeur de ITRONC_AER
	     CALL OSOAA_AEROSOLS(FICANGLES_RES_MIE, 
     &	  	                 WAREF_AOT, AOT_REF, WAREF_AOT, AOT_REF,	  
     &	  	                 ITRONC_AER_IN, IMOD_AER,
     &	  	                 RN_MMD_AER_WAREF, IN_MMD_AER_WAREF, 	
     &	  	                 IGRANU_MMD_AER, 
     &	  	                 VARGRANU1_MMD_AER, VARGRANU2_MMD_AER, 
     &	  	                 VARGRANU3_MMD_AER, IMODELE_WMO, 
     &	  	                 C_WMO_DL, C_WMO_WS, C_WMO_OC, C_WMO_SO,
     &	  	                 IMODELE_SF, RH, 
     &	  	                 MODE_PARAM_BILND, 
     &	  	                 USER_CV_COARSE, USER_CV_FINE, 
     &	  	                 RTAUct_WAREF, 
     &	  	                 BMD_CM_MRWAREF, BMD_CM_MIWAREF,
     &	  	                 BMD_CM_MRWAREF, BMD_CM_MIWAREF,
     &	  	                 BMD_CM_RMODAL, BMD_CM_VAR,
     &	  	                 BMD_FM_MRWAREF, BMD_FM_MIWAREF,
     &	  	                 BMD_FM_MRWAREF, BMD_FM_MIWAREF,
     &	  	                 BMD_FM_RMODAL, BMD_FM_VAR,	 
     &	  	                 FICEXTDATA_AER, 
     &                           DIRMIE_AER, FICAER_LOG_MIE,
     &	  	                 FICGRANU_AER, FICAER_LOG, KMAT1_WAREF,
     &			         PIZAER, IER)

             IF (IER.NE.0) THEN ! cas d'erreur 
	  	    		
                IF (TRACE) THEN
	            WRITE (IDLOG,*,ERR=910) 
     &	            "   ==> Error on subroutine OSOAA_AEROSOLS"
 	        ENDIF
	        GOTO 6000   
             ENDIF
	  
	     ! Appel de la fonction OSOAA_AEROSOLS pour WA_SIMU
	     ! --> On pose TA = 0.1 pour que les calculs se fassent
	     TA = 0.1
	  
	     IF (TRACE) THEN
                WRITE (IDLOG,*,ERR=910) " "
	        WRITE (IDLOG,*,ERR=910) 
     &	     "=> Calculation of aerosol radiative fonctions for WA"
                WRITE (IDLOG,*,ERR=910) 
     &	     "--> OSOAA_AEROSOLS is called with the parameters :"
	        WRITE (IDLOG,808,ERR=910) FICANGLES_RES_MIE
	        WRITE (IDLOG,841,ERR=910) WA_SIMU
	        WRITE (IDLOG,842,ERR=910) TA
	        WRITE (IDLOG,843,ERR=910) WAREF_AOT
	        WRITE (IDLOG,844,ERR=910) AOT_REF
	        WRITE (IDLOG,845,ERR=910) ITRONC_AER
	        WRITE (IDLOG,846,ERR=910) IMOD_AER
	        WRITE (IDLOG,*,ERR=910) "      ------------------"
	        WRITE (IDLOG,*,ERR=910) "      mono-modal model :"
	        WRITE (IDLOG,*,ERR=910) "      ------------------"
 	        WRITE (IDLOG,847,ERR=910) RN_MMD_AER, IN_MMD_AER 
	        WRITE (IDLOG,849,ERR=910) IGRANU_MMD_AER
	        WRITE (IDLOG,850,ERR=910) VARGRANU1_MMD_AER, 
     &	     			          VARGRANU2_MMD_AER, 
     &	     			          VARGRANU3_MMD_AER
	        WRITE (IDLOG,*,ERR=910) "      -----------"
	        WRITE (IDLOG,*,ERR=910) "      WMO model :"
	        WRITE (IDLOG,*,ERR=910) "      -----------"
                WRITE (IDLOG,851,ERR=910) IMODELE_WMO
	        WRITE (IDLOG,852,ERR=910) C_WMO_DL,C_WMO_WS,C_WMO_OC,
     &	    			          C_WMO_SO
	        WRITE (IDLOG,*,ERR=910) "      ----------------------"
	        WRITE (IDLOG,*,ERR=910) "      Shettle & Fenn model :"
	        WRITE (IDLOG,*,ERR=910) "      ----------------------"
	        WRITE (IDLOG,853,ERR=910) IMODELE_SF
	        WRITE (IDLOG,854,ERR=910) RH
	        WRITE (IDLOG,*,ERR=910) "      -----------------------"
	        WRITE (IDLOG,*,ERR=910) "      Bi-modal distribution :"
	        WRITE (IDLOG,*,ERR=910) "      -----------------------"
	        WRITE (IDLOG,855,ERR=910) MODE_PARAM_BILND
	        WRITE (IDLOG,856,ERR=910) USER_CV_COARSE, USER_CV_FINE
	        WRITE (IDLOG,857,ERR=910) RTAUct_WAREF
	        WRITE (IDLOG,*,ERR=910) "      Coarse mode :"
	        WRITE (IDLOG,858,ERR=910) BMD_CM_MRWA, BMD_CM_MIWA
	        WRITE (IDLOG,859,ERR=910) BMD_CM_MRWAREF, BMD_CM_MIWAREF
	        WRITE (IDLOG,860,ERR=910) BMD_CM_RMODAL, BMD_CM_VAR
	        WRITE (IDLOG,*,ERR=910) "      Fine mode :"
	        WRITE (IDLOG,858,ERR=910) BMD_FM_MRWA, BMD_FM_MIWA
	        WRITE (IDLOG,859,ERR=910) BMD_FM_MRWAREF, BMD_FM_MIWAREF
	        WRITE (IDLOG,860,ERR=910) BMD_FM_RMODAL, BMD_FM_VAR
	        WRITE (IDLOG,*,ERR=910) "      ---------------"
 	        WRITE (IDLOG,*,ERR=910) "      External data :"
	        WRITE (IDLOG,*,ERR=910) "      ---------------"
 	        WRITE (IDLOG,861,ERR=910) FICEXTDATA_AER  
	        WRITE (IDLOG,*,ERR=910) "      ------------------"
	        WRITE (IDLOG,*,ERR=910) "      MIE computations :"
	        WRITE (IDLOG,*,ERR=910) "      ------------------"
	        WRITE (IDLOG,864,ERR=910) DIRMIE_AER 
	        WRITE (IDLOG,865,ERR=910) FICAER_LOG_MIE 
	        WRITE (IDLOG,*,ERR=910) "      --------------"
	        WRITE (IDLOG,*,ERR=910) "      Output files :"
	        WRITE (IDLOG,*,ERR=910) "      --------------"
 	        WRITE (IDLOG,862,ERR=910) FICAER_LOG 
	        WRITE (IDLOG,863,ERR=910) FICGRANU_AER
             ENDIF !Fin Trace 
	  
	     ITRONC_AER_IN = ITRONC_AER  !Si ITRONC_AER=1 mais que le code estime la 
	 			         !troncature inutile seul le paramètre ITRONC_AER_IN 
				         !basculera de 1 à 0 
					 !sans impacter la valeur de ITRONC_AER
	     CALL OSOAA_AEROSOLS(FICANGLES_RES_MIE, 
     &	    	                 WA_SIMU, TA, WAREF_AOT, AOT_REF,	  
     &	  	                 ITRONC_AER_IN, IMOD_AER,
     &	  	                 RN_MMD_AER, IN_MMD_AER,	
     &	  	                 IGRANU_MMD_AER, 
     &	  	                 VARGRANU1_MMD_AER, VARGRANU2_MMD_AER, 
     &	  	                 VARGRANU3_MMD_AER, IMODELE_WMO, 
     &	  	                 C_WMO_DL, C_WMO_WS, C_WMO_OC, C_WMO_SO,
     &	  	                 IMODELE_SF, RH, 
     &	  	                 MODE_PARAM_BILND, 
     &	  	                 USER_CV_COARSE, USER_CV_FINE, 
     &	  	                 RTAUct_WAREF, 
     &	  	                 BMD_CM_MRWA, BMD_CM_MIWA, 
     &	  	                 BMD_CM_MRWAREF, BMD_CM_MIWAREF,
     &	  	                 BMD_CM_RMODAL, BMD_CM_VAR,
     &	  	                 BMD_FM_MRWA, BMD_FM_MIWA, 
     &	  	                 BMD_FM_MRWAREF, BMD_FM_MIWAREF,
     &	  	                 BMD_FM_RMODAL, BMD_FM_VAR,	 
     &	  	                 FICEXTDATA_AER, 
     &			         DIRMIE_AER, FICAER_LOG_MIE,
     &	  	                 FICGRANU_AER, FICAER_LOG, KMAT1_WA,
     &			         PIZAER, IER)

             IF (IER.NE.0) THEN ! cas d'erreur   
	  	  		
                IF (TRACE) THEN
	            WRITE (IDLOG,*,ERR=910) 
     &	            "   ==> Error on subroutine OSOAA_AEROSOLS"
 	        ENDIF
	        GOTO 6000   
             ENDIF  
	  
	  
	     ! Calcul de l'ep opt des aérosols à la longueur d'onde WA_SIMU
             TA = (KMAT1_WA / KMAT1_WAREF) * AOT_REF
	  
	     IF (TRACE) THEN
	         WRITE (IDLOG,*,ERR=910) " "
	         WRITE (IDLOG,*,ERR=910) "=> AOT estimation at WA : "
	         WRITE (IDLOG,*,ERR=910) "    WAREF   : ", WAREF_AOT
		 WRITE (IDLOG,*,ERR=910) "    AOT_REF : ", AOT_REF
	         WRITE (IDLOG,*,ERR=910) "    WA      : ", WA_SIMU         
	         WRITE (IDLOG,*,ERR=910) 
     &	      "    Sect eff ext (WAREF) : ", KMAT1_WAREF
	         WRITE (IDLOG,*,ERR=910) 
     &	      "    Sect eff ext (WA)    : ", KMAT1_WA
	         WRITE (IDLOG,*,ERR=910) 
     &	      "    ===> AOT estimated at WA : ", TA
 	     ENDIF !Fin Trace 


	     ! On s'assure que si l'AOT à la longueur d'onde de simulation 
	     ! de la luminance est supérieure au seuil de négligence alors
	     ! on a bien défini l'échelle de hauteur 
             IF ((DEF_HA.EQ.0).AND.(TA.GT.0.0001)) GOTO 2220 
      	  
         ENDIF ! Fin test sur WA_SIMU.EQ.WAREF_AOT

      ENDIF ! Fin cas AOT_REF non nulle     	
      
	
C =========================================================
C --- Préparation des matrices de phase marines
C =========================================================  
      WRITE(6,*) '==> Hydrosols radiative properties computation'
      CALL FLUSH(6)	!Force l'envoie du message à l'écran      

         
      IF (TRACE) THEN
          WRITE (IDLOG,*,ERR=910) " "
	  WRITE (IDLOG,*,ERR=910) " "
	  WRITE (IDLOG,*,ERR=910) 
     &	  "------------------------------------------"
	  WRITE (IDLOG,*,ERR=910)
     &	  "HYDROSOLS RADIATIVE PROPERTIES COMPUTATION"
	  WRITE (IDLOG,*,ERR=910) 
     &	  "------------------------------------------"
      ENDIF !Fin Trace 	  

     
      	  
      IF (IMOD_HYD.EQ.1) THEN
C     Cas d'une représentation des hydrosols par modèle
C     --> Génération du fichier de matrice de phase pour le phytoplancton
C     --> Génération du fichier de matrice de phase pour les Mineral-Like
C         Particles

C        Appel de OSOAA_HYDROSOLS pour le traitement du PHYTOPLANCTON 
C        par modèle

         IF (TRACE) THEN
      		
            WRITE (IDLOG,*,ERR=910) " "
	    WRITE (IDLOG,*,ERR=910) "=> Characterization by models"
	    WRITE (IDLOG,*,ERR=910) "   for PHYTOPLANKTON :"
	    WRITE (IDLOG,*,ERR=910) "   ----------------- "
	    WRITE (IDLOG,*,ERR=910) " "
            WRITE (IDLOG,*,ERR=910) 
     &	    "--> OSOAA_HYDROSOLS is called with the parameters :"
	    WRITE (IDLOG,808,ERR=910) FICANGLES_RES_MIE
	    WRITE (IDLOG,841,ERR=910) WA_SIMU  
 	    WRITE (IDLOG,871,ERR=910) 1  
	    WRITE (IDLOG,879,ERR=910) CHL  
	    WRITE (IDLOG,*,ERR=910) "      --------------------------"
	    WRITE (IDLOG,*,ERR=910) "      Phytoplancton : Mode Junge"
	    WRITE (IDLOG,*,ERR=910) "      --------------------------" 
	    WRITE (IDLOG,872,ERR=910) PHYTO_JD_SLOPE, PHYTO_JD_RMIN, 
     &	    PHYTO_JD_RMAX
	    WRITE (IDLOG,873,ERR=910) PHYTO_JD_MR, PHYTO_JD_MI
	    WRITE (IDLOG,874,ERR=910) PHYTO_JD_RATE
	    WRITE (IDLOG,*,ERR=910) "      --------------------------"
	    WRITE (IDLOG,*,ERR=910) "      Phytoplancton : Mode LND 1"
	    WRITE (IDLOG,*,ERR=910) "      --------------------------" 
	    WRITE (IDLOG,875,ERR=910) PHYTO_LND_SM_RADIUS,
     &	    PHYTO_LND_SM_VAR
	    WRITE (IDLOG,873,ERR=910) PHYTO_LND_SM_MR, PHYTO_LND_SM_MI
	    WRITE (IDLOG,874,ERR=910) PHYTO_LND_SM_RATE
	    WRITE (IDLOG,*,ERR=910) "      --------------------------"
	    WRITE (IDLOG,*,ERR=910) "      Phytoplancton : Mode LND 2"
	    WRITE (IDLOG,*,ERR=910) "      --------------------------" 
	    WRITE (IDLOG,875,ERR=910) PHYTO_LND_TM_RADIUS,
     &	    PHYTO_LND_TM_VAR
	    WRITE (IDLOG,873,ERR=910) PHYTO_LND_TM_MR, PHYTO_LND_TM_MI
	    WRITE (IDLOG,874,ERR=910) PHYTO_LND_TM_RATE
	    WRITE (IDLOG,*,ERR=910) "      ---------------"
 	    WRITE (IDLOG,*,ERR=910) "      External data :"
	    WRITE (IDLOG,*,ERR=910) "      ---------------"
 	    WRITE (IDLOG,876,ERR=910) FICEXTDATA_HYD  
	    WRITE (IDLOG,*,ERR=910) "      ------------------"
	    WRITE (IDLOG,*,ERR=910) "      MIE computations :"
	    WRITE (IDLOG,*,ERR=910) "      ------------------"
	    WRITE (IDLOG,864,ERR=910) DIRMIE_HYD 
	    WRITE (IDLOG,865,ERR=910) FICHYD_LOG_MIE 
	    WRITE (IDLOG,*,ERR=910) "      --------------"
	    WRITE (IDLOG,*,ERR=910) "      Output files :"
	    WRITE (IDLOG,*,ERR=910) "      --------------"
	    WRITE (IDLOG,870,ERR=910) FICHYD_LOG
	    WRITE (IDLOG,877,ERR=910) FICGRANU_PHYTO
	
         ENDIF !Fin Trace
      
         CALL OSOAA_HYDROSOLS(FICANGLES_RES_MIE, 
     & 			   WA_SIMU, 1, CHL,
     &                     PHYTO_JD_SLOPE, PHYTO_JD_RMIN,PHYTO_JD_RMAX, 
     &                     PHYTO_JD_MR, PHYTO_JD_MI, PHYTO_JD_RATE,
     &			   PHYTO_LND_SM_RADIUS, PHYTO_LND_SM_VAR, 
     &			   PHYTO_LND_SM_MR, PHYTO_LND_SM_MI, 
     &                     PHYTO_LND_SM_RATE,
     &  		   PHYTO_LND_TM_RADIUS,PHYTO_LND_TM_VAR, 
     &			   PHYTO_LND_TM_MR, PHYTO_LND_TM_MI, 
     &                     PHYTO_LND_TM_RATE,
     & 		  	   FICEXTDATA_HYD,
     &                     FICGRANU_PHYTO, 
     &	  	           FICHYD_LOG, DIRMIE_HYD, FICHYD_LOG_MIE, IER)

         IF (IER.NE.0) THEN ! cas d'erreur     		
            IF (TRACE) THEN
	        WRITE (IDLOG,*,ERR=910) 
     &	        "   ==> Error on subroutine OSOAA_HYDROSOLS"
 	    ENDIF
	    GOTO 6500   
         ENDIF
      
      
C        Appel de OSOAA_HYDROSOLS pour le traitement des MINERAL-LIKE PARTICLES
C        par modèle

         IF (TRACE) THEN
      		
            WRITE (IDLOG,*,ERR=910) " "
	    WRITE (IDLOG,*,ERR=910) "=> Characterization by models"
	    WRITE (IDLOG,*,ERR=910) "   for MINERAL-LIKE PARTICLES :"
	    WRITE (IDLOG,*,ERR=910) "   -------------------------- "
	    WRITE (IDLOG,*,ERR=910) " "
            WRITE (IDLOG,*,ERR=910) 
     &	    "--> OSOAA_HYDROSOLS is called with the parameters :"
	    WRITE (IDLOG,808,ERR=910) FICANGLES_RES_MIE
	    WRITE (IDLOG,841,ERR=910) WA_SIMU   
 	    WRITE (IDLOG,871,ERR=910) 1  
	    WRITE (IDLOG,879,ERR=910) SED_CSED 
	    WRITE (IDLOG,*,ERR=910) "      ----------------------"
	    WRITE (IDLOG,*,ERR=910) "      Sediments : Mode Junge"
	    WRITE (IDLOG,*,ERR=910) "      ----------------------" 
	    WRITE (IDLOG,872,ERR=910) SED_JD_SLOPE,SED_JD_RMIN,
     &	                              SED_JD_RMAX
	    WRITE (IDLOG,873,ERR=910) SED_JD_MR, SED_JD_MI
	    WRITE (IDLOG,874,ERR=910) SED_JD_RATE
	    WRITE (IDLOG,*,ERR=910) "      ----------------------"
	    WRITE (IDLOG,*,ERR=910) "      Sediments : Mode LND 1"
	    WRITE (IDLOG,*,ERR=910) "      ----------------------" 
	    WRITE (IDLOG,875,ERR=910) SED_LND_SM_RADIUS,SED_LND_SM_VAR
	    WRITE (IDLOG,873,ERR=910) SED_LND_SM_MR, SED_LND_SM_MI
	    WRITE (IDLOG,874,ERR=910) SED_LND_SM_RATE
	    WRITE (IDLOG,*,ERR=910) "      ----------------------"
	    WRITE (IDLOG,*,ERR=910) "      Sediments : Mode LND 2"
	    WRITE (IDLOG,*,ERR=910) "      ----------------------" 
	    WRITE (IDLOG,875,ERR=910) SED_LND_TM_RADIUS,SED_LND_TM_VAR
	    WRITE (IDLOG,873,ERR=910) SED_LND_TM_MR, SED_LND_TM_MI
	    WRITE (IDLOG,874,ERR=910) SED_LND_TM_RATE
	    WRITE (IDLOG,*,ERR=910) "      ---------------"
 	    WRITE (IDLOG,*,ERR=910) "      External data :"
	    WRITE (IDLOG,*,ERR=910) "      ---------------"
 	    WRITE (IDLOG,876,ERR=910) FICEXTDATA_HYD  
	    WRITE (IDLOG,*,ERR=910) "      ------------------"
	    WRITE (IDLOG,*,ERR=910) "      MIE computations :"
	    WRITE (IDLOG,*,ERR=910) "      ------------------"
	    WRITE (IDLOG,864,ERR=910) DIRMIE_HYD
	    WRITE (IDLOG,865,ERR=910) FICHYD_LOG_MIE 
	    WRITE (IDLOG,*,ERR=910) "      --------------"
	    WRITE (IDLOG,*,ERR=910) "      Output files :"
	    WRITE (IDLOG,*,ERR=910) "      --------------"
	    WRITE (IDLOG,870,ERR=910) FICHYD_LOG
	    WRITE (IDLOG,878,ERR=910) FICGRANU_MLP
	
         ENDIF !Fin Trace
	 
	 CALL OSOAA_HYDROSOLS(FICANGLES_RES_MIE, 
     & 			   WA_SIMU, 1, SED_CSED, 
     &                     SED_JD_SLOPE,SED_JD_RMIN,SED_JD_RMAX,
     &			   SED_JD_MR, SED_JD_MI, SED_JD_RATE,
     &		 	   SED_LND_SM_RADIUS,SED_LND_SM_VAR,
     &			   SED_LND_SM_MR, SED_LND_SM_MI, 
     &			   SED_LND_SM_RATE,
     &			   SED_LND_TM_RADIUS,SED_LND_TM_VAR,
     &  		   SED_LND_TM_MR, SED_LND_TM_MI, 
     &                     SED_LND_TM_RATE,
     & 		  	   FICEXTDATA_HYD,
     &                     FICGRANU_MLP, 
     &	  	           FICHYD_LOG, DIRMIE_HYD, FICHYD_LOG_MIE, IER)

         IF (IER.NE.0) THEN ! cas d'erreur     		
            IF (TRACE) THEN
	        WRITE (IDLOG,*,ERR=910) 
     &	        "   ==> Error on subroutine OSOAA_HYDROSOLS"
 	    ENDIF
	    GOTO 6500   
         ENDIF
	             
      ENDIF !Fin cas IMOD_HYD=1

      
      IF (IMOD_HYD.EQ.2) THEN
C     Cas d'une représentation des hydrosols par mesures externes
C     --> Génération du fichier de matrice de phase. 
C     --> Enregistrement arbitrairement affecté au phytoplancton
C         ==> Génération du fichier de matrice de phase pour les Mineral-Like
C             Particles en posant à zéro les paramètres radiatifs.


C        Appel de OSOAA_HYDROSOLS pour générer le fichier MINERAL-LIKE PARTICLES 
C        avec valeurs à zéro : 
C            - Choix du type de caractérisation des hydrosols : IMOD = 1
C            - Concentration posée nulle : CONC = 0. 
C            - Sortie = fichier FICGRANU_MLP

         IF (TRACE) THEN
      		
            WRITE (IDLOG,*,ERR=910) " "
	    WRITE (IDLOG,*,ERR=910) 
     &	    "=> Characterization by external measurements"
            WRITE (IDLOG,*,ERR=910) " "
	    WRITE (IDLOG,*,ERR=910) 
     &	    "MINERAL-LIKE PARTICLES file is zero filled"
	    WRITE (IDLOG,*,ERR=910) 
     &	    "------------------------------------------"
            WRITE (IDLOG,*,ERR=910) 
     &	    "--> OSOAA_HYDROSOLS is called with the parameters :"
	    WRITE (IDLOG,808,ERR=910) FICANGLES_RES_MIE
	    WRITE (IDLOG,841,ERR=910) WA_SIMU     
 	    WRITE (IDLOG,871,ERR=910) 1  
	    WRITE (IDLOG,879,ERR=910) 0.D+00 
	    WRITE (IDLOG,*,ERR=910) "      ----------------------"
	    WRITE (IDLOG,*,ERR=910) "      Sediments : Mode Junge"
	    WRITE (IDLOG,*,ERR=910) "      ----------------------" 
	    WRITE (IDLOG,872,ERR=910) SED_JD_SLOPE,SED_JD_RMIN,
     &	                              SED_JD_RMAX
	    WRITE (IDLOG,873,ERR=910) SED_JD_MR, SED_JD_MI
	    WRITE (IDLOG,874,ERR=910) SED_JD_RATE
	    WRITE (IDLOG,*,ERR=910) "      ----------------------"
	    WRITE (IDLOG,*,ERR=910) "      Sediments : Mode LND 1"
	    WRITE (IDLOG,*,ERR=910) "      ----------------------" 
	    WRITE (IDLOG,875,ERR=910) SED_LND_SM_RADIUS,SED_LND_SM_VAR
	    WRITE (IDLOG,873,ERR=910) SED_LND_SM_MR, SED_LND_SM_MI
	    WRITE (IDLOG,874,ERR=910) SED_LND_SM_RATE
	    WRITE (IDLOG,*,ERR=910) "      ----------------------"
	    WRITE (IDLOG,*,ERR=910) "      Sediments : Mode LND 2"
	    WRITE (IDLOG,*,ERR=910) "      ----------------------" 
	    WRITE (IDLOG,875,ERR=910) SED_LND_TM_RADIUS,SED_LND_TM_VAR
	    WRITE (IDLOG,873,ERR=910) SED_LND_TM_MR, SED_LND_TM_MI
	    WRITE (IDLOG,874,ERR=910) SED_LND_TM_RATE
	    WRITE (IDLOG,*,ERR=910) "      ---------------"
 	    WRITE (IDLOG,*,ERR=910) "      External data :"
	    WRITE (IDLOG,*,ERR=910) "      ---------------"
 	    WRITE (IDLOG,876,ERR=910) FICEXTDATA_HYD  
	    WRITE (IDLOG,*,ERR=910) "      ------------------"
	    WRITE (IDLOG,*,ERR=910) "      MIE computations :"
	    WRITE (IDLOG,*,ERR=910) "      ------------------"
	    WRITE (IDLOG,864,ERR=910) DIRMIE_HYD 
	    WRITE (IDLOG,865,ERR=910) FICHYD_LOG_MIE 
	    WRITE (IDLOG,*,ERR=910) "      --------------"
	    WRITE (IDLOG,*,ERR=910) "      Output files :"
	    WRITE (IDLOG,*,ERR=910) "      --------------"
	    WRITE (IDLOG,870,ERR=910) FICHYD_LOG
	    WRITE (IDLOG,878,ERR=910) FICGRANU_MLP
	
         ENDIF !Fin Trace

         CALL OSOAA_HYDROSOLS(FICANGLES_RES_MIE, 
     & 			   WA_SIMU, 1, 0.D+00, 
     &                     SED_JD_SLOPE,SED_JD_RMIN,SED_JD_RMAX,
     &			   SED_JD_MR, SED_JD_MI, SED_JD_RATE,
     &		 	   SED_LND_SM_RADIUS,SED_LND_SM_VAR,
     &			   SED_LND_SM_MR, SED_LND_SM_MI, 
     &			   SED_LND_SM_RATE,
     &			   SED_LND_TM_RADIUS,SED_LND_TM_VAR,
     &  		   SED_LND_TM_MR, SED_LND_TM_MI, 
     &                     SED_LND_TM_RATE,
     & 		  	   FICEXTDATA_HYD,
     &                     FICGRANU_MLP, 
     &	  	           FICHYD_LOG, DIRMIE_HYD, FICHYD_LOG_MIE, IER)


         IF (IER.NE.0) THEN ! cas d'erreur     		
            IF (TRACE) THEN
	        WRITE (IDLOG,*,ERR=910) 
     &	        "   ==> Error on subroutine OSOAA_HYDROSOLS"
 	    ENDIF
	    GOTO 6500   
         ENDIF
	 
C        Appel de OSOAA_HYDROSOLS pour générer le fichier PHYTOPLANCTON 
C        avec les données du fichier utilisateur FICEXTDATA_HYD : 
C            - Choix du type de caractérisation des hydrosols : IMOD = 2
C            - Sortie = fichier FICGRANU_PHYTO

         IF (TRACE) THEN
      		
            WRITE (IDLOG,*,ERR=910) " "
	    WRITE (IDLOG,*,ERR=910) 
     &	    "=> Characterization by external measurements"
            WRITE (IDLOG,*,ERR=910) " "
	    WRITE (IDLOG,*,ERR=910) 
     &      "PHYTOPLANKTON file is filled with global data measurements"
            WRITE (IDLOG,*,ERR=910) 
     &	    "----------------------------------------------------------"
            WRITE (IDLOG,*,ERR=910)    
     &	    "--> OSOAA_HYDROSOLS is called with the parameters :"
	    WRITE (IDLOG,808,ERR=910) FICANGLES_RES_MIE
	    WRITE (IDLOG,841,ERR=910) WA_SIMU      
 	    WRITE (IDLOG,871,ERR=910) 2  
	    WRITE (IDLOG,879,ERR=910) CHL  
	    WRITE (IDLOG,*,ERR=910) "      --------------------------"
	    WRITE (IDLOG,*,ERR=910) "      Phytoplancton : Mode Junge"
	    WRITE (IDLOG,*,ERR=910) "      --------------------------" 
	    WRITE (IDLOG,872,ERR=910) PHYTO_JD_SLOPE, PHYTO_JD_RMIN, 
     &	    PHYTO_JD_RMAX
	    WRITE (IDLOG,873,ERR=910) PHYTO_JD_MR, PHYTO_JD_MI
	    WRITE (IDLOG,874,ERR=910) PHYTO_JD_RATE
	    WRITE (IDLOG,*,ERR=910) "      --------------------------"
	    WRITE (IDLOG,*,ERR=910) "      Phytoplancton : Mode LND 1"
	    WRITE (IDLOG,*,ERR=910) "      --------------------------" 
	    WRITE (IDLOG,875,ERR=910) PHYTO_LND_SM_RADIUS,
     &	    PHYTO_LND_SM_VAR
	    WRITE (IDLOG,873,ERR=910) PHYTO_LND_SM_MR, PHYTO_LND_SM_MI
	    WRITE (IDLOG,874,ERR=910) PHYTO_LND_SM_RATE
	    WRITE (IDLOG,*,ERR=910) "      --------------------------"
	    WRITE (IDLOG,*,ERR=910) "      Phytoplancton : Mode LND 2"
	    WRITE (IDLOG,*,ERR=910) "      --------------------------" 
	    WRITE (IDLOG,875,ERR=910) PHYTO_LND_TM_RADIUS,
     &	    PHYTO_LND_TM_VAR
	    WRITE (IDLOG,873,ERR=910) PHYTO_LND_TM_MR, PHYTO_LND_TM_MI
	    WRITE (IDLOG,874,ERR=910) PHYTO_LND_TM_RATE
	    WRITE (IDLOG,*,ERR=910) "      ---------------"
 	    WRITE (IDLOG,*,ERR=910) "      External data :"
	    WRITE (IDLOG,*,ERR=910) "      ---------------"
 	    WRITE (IDLOG,876,ERR=910) FICEXTDATA_HYD  
	    WRITE (IDLOG,*,ERR=910) "      ------------------"
	    WRITE (IDLOG,*,ERR=910) "      MIE computations :"
	    WRITE (IDLOG,*,ERR=910) "      ------------------"
	    WRITE (IDLOG,864,ERR=910) DIRMIE_HYD
	    WRITE (IDLOG,865,ERR=910) FICHYD_LOG_MIE 
	    WRITE (IDLOG,*,ERR=910) "      --------------"
	    WRITE (IDLOG,*,ERR=910) "      Output files :"
	    WRITE (IDLOG,*,ERR=910) "      --------------"
	    WRITE (IDLOG,870,ERR=910) FICHYD_LOG
	    WRITE (IDLOG,877,ERR=910) FICGRANU_PHYTO
	
         ENDIF !Fin Trace
	 
         CALL OSOAA_HYDROSOLS(FICANGLES_RES_MIE, 
     & 			   WA_SIMU, 2, CHL,
     &                     PHYTO_JD_SLOPE, PHYTO_JD_RMIN,PHYTO_JD_RMAX, 
     &                     PHYTO_JD_MR, PHYTO_JD_MI, PHYTO_JD_RATE,
     &			   PHYTO_LND_SM_RADIUS, PHYTO_LND_SM_VAR, 
     &			   PHYTO_LND_SM_MR, PHYTO_LND_SM_MI, 
     &                     PHYTO_LND_SM_RATE,
     &  		   PHYTO_LND_TM_RADIUS,PHYTO_LND_TM_VAR, 
     &			   PHYTO_LND_TM_MR, PHYTO_LND_TM_MI, 
     &                     PHYTO_LND_TM_RATE,
     & 		  	   FICEXTDATA_HYD,
     &                     FICGRANU_PHYTO, 
     &	  	           FICHYD_LOG, DIRMIE_HYD, FICHYD_LOG_MIE, IER)

         IF (IER.NE.0) THEN ! cas d'erreur     		
            IF (TRACE) THEN
	        WRITE (IDLOG,*,ERR=910) 
     &	        "   ==> Error on subroutine OSOAA_HYDROSOLS"
 	    ENDIF
	    GOTO 6500   
         ENDIF
           
      ENDIF !Fin cas IMOD_HYD=2
      


C =========================================================
C --- Préparation des profils atmosphérique et marin
C =========================================================    
      WRITE(6,*) '==> Atmospheric and sea profiles computation'
      CALL FLUSH(6)	!Force l'envoie du message à l'écran      
       
      IF (TRACE) THEN
          WRITE (IDLOG,*,ERR=910) " "
	  WRITE (IDLOG,*,ERR=910) " "
	  WRITE (IDLOG,*,ERR=910) 
     &	  "----------------------------------------"
	  WRITE (IDLOG,*,ERR=910)
     &	  "ATMOSPHERIC AND SEA PROFILES COMPUTATION"
	  WRITE (IDLOG,*,ERR=910) 
     &	  "----------------------------------------"
          WRITE (IDLOG,*,ERR=910) " "
      ENDIF !Fin Trace 	       
     	

C --- Détermination des paramètres liés au modèle de sédiments :
C --- SED_KMAT2, SED_VSED, SED_MRWA, SED_PIZ
      IF (SED_CSED.NE.0.D+00) THEN
      
         ! Lecture des paramètres utiles du fichier GRANU_MLP
         !  -------------------------------------------------
         OPEN(60,FILE=FICGRANU_MLP,STATUS='OLD',ERR=2650)

         !Lecture des informations de contenu du fichier   
         READ(60,'(a)',ERR=2660) LIGNE_TEXTE
         READ(60,'(a)',ERR=2660) LIGNE_TEXTE

    
         ! Section efficace d'extinction
         READ(60,'(a)',ERR=2660) LIGNE_TEXTE

         ! Section efficace de diffusion
         READ(60,'(a)',ERR=2660) LIGNE_TEXTE
         IFIN=INDEX(LIGNE_TEXTE,':')
         READ(LIGNE_TEXTE(IFIN+1:INCTE_LENLINE),*) SED_KMAT2

         ! Facteur d'asymétrie (non tronqué)
         READ(60,'(a)',ERR=2660) LIGNE_TEXTE
	 
	 ! Altitude / profondeur des particules
         READ(60,'(a)',ERR=2660) LIGNE_TEXTE
	 
         !Volume moyen d'une particule
	 READ(60,'(a)',ERR=2660) LIGNE_TEXTE
         IFIN=INDEX(LIGNE_TEXTE,':')
         READ(LIGNE_TEXTE(IFIN+1:INCTE_LENLINE),*) SED_VSED

	 !Indice moyen d'une particule (partie réelle)      
         READ(60,'(a)',ERR=2660) LIGNE_TEXTE
         IFIN=INDEX(LIGNE_TEXTE,':')
         READ(LIGNE_TEXTE(IFIN+1:INCTE_LENLINE),*) SED_MRWA
          
	 ! Coefficient de troncature  
         READ(60,'(a)',ERR=2660) LIGNE_TEXTE
         IFIN=INDEX(LIGNE_TEXTE,':')
         READ(LIGNE_TEXTE(IFIN+1:INCTE_LENLINE),*) SED_A_TRONCA	
	 
	 ! Albédo de simple diffusion (éventuellement tronqué) 
         READ(60,'(a)',ERR=2660) LIGNE_TEXTE
         IFIN=INDEX(LIGNE_TEXTE,':')
         READ(LIGNE_TEXTE(IFIN+1:INCTE_LENLINE),*)  SED_PIZTR	

	 CLOSE(60)
	 
	 ! Calcul de l'albédo de simple diffusion (sans troncature) 
         SED_PIZ = SED_PIZTR / (1.-SED_A_TRONCA*(1.-SED_PIZTR)/2.)

         IF (TRACE) THEN  
	    WRITE (IDLOG,*,ERR=910) 
     &	    "=> Mineral-Like Particles information retrieval"
            WRITE (IDLOG,*,ERR=910) " "           
	    WRITE (IDLOG,*,ERR=910) 
     &	    "Scattering Cross section SED_KMAT2 :",SED_KMAT2 
	    WRITE (IDLOG,*,ERR=910) 
     &	     "Mean volume of one particle SED_VSED :",SED_VSED
	    WRITE (IDLOG,*,ERR=910) 
     &	     "Mean refractive index (real part) SED_MRWA :",SED_MRWA
	    WRITE (IDLOG,*,ERR=910) 
     &	     "Single scattering albedo (no trunca) SED_PIZ :",SED_PIZ
	    WRITE (IDLOG,*,ERR=910) "deduced from :"
	    WRITE (IDLOG,*,ERR=910) 
     &	     "  Single scattering albedo (trunca) SED_PIZTR :",SED_PIZTR
	    WRITE (IDLOG,*,ERR=910) 
     &	     "  Truncature coefficient :",SED_A_TRONCA
            WRITE (IDLOG,*,ERR=910) " "
	 ENDIF ! Trace   
	    	  
         
      ELSE
           SED_MRWA = CTE_NOT_DEFINED_VALUE_DBLE
           SED_VSED = CTE_NOT_DEFINED_VALUE_DBLE
           SED_KMAT2 = CTE_NOT_DEFINED_VALUE_DBLE
           SED_PIZ = CTE_NOT_DEFINED_VALUE_DBLE
      ENDIF
      
      IF (TRACE) THEN
          WRITE (IDLOG,*,ERR=910) " "
          WRITE (IDLOG,*,ERR=910) 
     &	  "--> OSOAA_PROFILE is called with the parameters :"
          WRITE (IDLOG,*,ERR=910) 
     &	  "Wavelength for radiance computation :"
          WRITE (IDLOG,*,ERR=910) "-----------------------------------"
          WRITE (IDLOG,809,ERR=910) WA_SIMU
	  WRITE (IDLOG,*,ERR=910) "SEA PARAMETERS :"
	  WRITE (IDLOG,*,ERR=910) "--------------"
	  WRITE (IDLOG,*,ERR=910) "Depth of sea :"
	  WRITE (IDLOG,810,ERR=910) SEA_DEPTH
	  WRITE (IDLOG,*,ERR=910) "Chlorophylle :"
	  WRITE (IDLOG,811,ERR=910) CHL
	  WRITE (IDLOG,812,ERR=910) IPHYTO_PROFIL
	  IF (IPHYTO_PROFIL.EQ.1) THEN
	      WRITE (IDLOG,*,ERR=910) 
     &	      '         ==> Homogeneous chlorophyll profile'
	  ENDIF
	  IF (IPHYTO_PROFIL.EQ.2) THEN
	      WRITE (IDLOG,*,ERR=910) 
     &	      '         ==> Gaussian chlorophyll profile'
	  ENDIF
	  IF (IPHYTO_PROFIL.EQ.3) THEN
	      WRITE (IDLOG,*,ERR=910) 
     &	      '         ==> Chlorophyll profile from Userfile'
	  ENDIF
	  WRITE (IDLOG,813,ERR=910) CHL_GP_BG
	  WRITE (IDLOG,814,ERR=910) CHL_GP_DEEP
	  WRITE (IDLOG,815,ERR=910) CHL_GP_WIDTH
	  WRITE (IDLOG,816,ERR=910) FICPHYTO_USER
	  WRITE (IDLOG,*,ERR=910) "Sediment :"
	  WRITE (IDLOG,817,ERR=910) SED_CSED
	  WRITE (IDLOG,818,ERR=910) SED_MRWA
	  WRITE (IDLOG,819,ERR=910) SED_VSED
	  WRITE (IDLOG,820,ERR=910) SED_KMAT2
	  WRITE (IDLOG,821,ERR=910) SED_PIZ
	  WRITE (IDLOG,*,ERR=910) "Yellow substance :"
	  WRITE (IDLOG,822,ERR=910) YS_A440
	  WRITE (IDLOG,823,ERR=910) YS_SWA
	  WRITE (IDLOG,*,ERR=910) "Detritus :"
	  WRITE (IDLOG,824,ERR=910) DET_A440
	  WRITE (IDLOG,825,ERR=910) DET_SWA 
	  WRITE (IDLOG,*,ERR=910) "ATMOSPHERIC PARAMETERS :"
	  WRITE (IDLOG,*,ERR=910) "---------------------"
	  WRITE (IDLOG,*,ERR=910) "Molecules :"
	  WRITE (IDLOG,730,ERR=910) TR
	  WRITE (IDLOG,733,ERR=910) PRESSION
	  WRITE (IDLOG,731,ERR=910) HR
	  WRITE (IDLOG,*,ERR=910) "Aerosols :"
	  WRITE (IDLOG,730,ERR=910) TA
	  WRITE (IDLOG,731,ERR=910) HA
	  WRITE (IDLOG,732,ERR=910) PIZAER
	  WRITE (IDLOG,*,ERR=910) "PRODUCT FILES :"
	  WRITE (IDLOG,*,ERR=910) "-------------"
	  WRITE (IDLOG,826,ERR=910) FICPROFIL_LOG
	  WRITE (IDLOG,827,ERR=910) FICPROFIL_ATM_RES
	  WRITE (IDLOG,828,ERR=910) FICPROFIL_SEA_RES
      ENDIF
 	
      CALL OSOAA_PROFILE(WA_SIMU, TR, HR, PRESSION, TA, HA, PIZAER, 
     &                   SEA_DEPTH, CHL, IPHYTO_PROFIL, 
     &                   CHL_GP_BG, CHL_GP_DEEP, CHL_GP_WIDTH,
     &                   FICPHYTO_USER, SED_CSED,  
     &			 SED_MRWA, SED_VSED, SED_KMAT2, SED_PIZ,
     &			 YS_A440, YS_SWA, DET_A440, DET_SWA,   
     &                   FICPROFIL_LOG, FICPROFIL_ATM_RES, 
     &                   FICPROFIL_SEA_RES, IER)
     
     	
      IF (IER.NE.0) THEN ! cas d'erreur
      		
          IF (TRACE) THEN
	      WRITE (IDLOG,*,ERR=910) 
     &	      "   ==> Error on subroutine OSOAA_PROFILE"
 	  ENDIF
	  GOTO 7000   
      ENDIF




C =========================================================
C --- Préparation des matrices d'interface 
C =========================================================    
      WRITE(6,*) '==> Sea / atmosphere interface matrices computation'
      CALL FLUSH(6)	!Force l'envoie du message à l'écran      
	
      IF (WIND.NE.0.) THEN 
      
         IMAT_SURF=1	!Cas mer agitée
	       
         IF (TRACE) THEN
             WRITE (IDLOG,*,ERR=910) " "
	     WRITE (IDLOG,*,ERR=910) " "
	     WRITE (IDLOG,*,ERR=910) 
     &	     "-----------------------------------------------"
	     WRITE (IDLOG,*,ERR=910)
     &	     "SEA / ATMOSPHERE INTERFACE MATRICES COMPUTATION"
	     WRITE (IDLOG,*,ERR=910) 
     &	     "-----------------------------------------------"
             WRITE (IDLOG,*,ERR=910) " "
             WRITE (IDLOG,*,ERR=910) 
     &	     "--> OSOAA_SURFACE is called with the parameters :"
	     WRITE (IDLOG,830,ERR=910) SEA_IND
	     WRITE (IDLOG,831,ERR=910) WIND
	     WRITE (IDLOG,832,ERR=910) FICANGLES_RES_LUM	
	     WRITE (IDLOG,833,ERR=910) DIRSEA
	     WRITE (IDLOG,834,ERR=910) FICSURF_LOG  
         ENDIF !Fin Trace 	  
	   
         CALL OSOAA_SURFACE(DIRSEA, FICANGLES_RES_LUM, SEA_IND, WIND, 
     &			    FICSURF_LOG, IER)
     
     	
         IF (IER.NE.0) THEN ! cas d'erreur
      		
             IF (TRACE) THEN
	         WRITE (IDLOG,*,ERR=910) 
     &	         "   ==> Error on subroutine OSOAA_SURFACE"
 	     ENDIF
	     GOTO 8000   
         ENDIF
     
      ELSE
         IMAT_SURF=0 	!Cas mer plate
	 
	 IF (TRACE) THEN
             WRITE (IDLOG,*,ERR=910) " "
	     WRITE (IDLOG,*,ERR=910) " "
	     WRITE (IDLOG,*,ERR=910) 
     &	     "-----------------------------------------------"
	     WRITE (IDLOG,*,ERR=910)
     &	     "SEA / ATMOSPHERE INTERFACE : FLAT SEA"
	     WRITE (IDLOG,*,ERR=910) 
     &	     "-----------------------------------------------"
             WRITE (IDLOG,*,ERR=910) " "
	     WRITE (IDLOG,830,ERR=910) SEA_IND
	     WRITE (IDLOG,831,ERR=910) WIND
         ENDIF !Fin Trace 	  
      ENDIF 
      
      

C =========================================================
C --- Simulation de la luminance 
C =========================================================  
3000  WRITE(6,*) '==> Radiative transfer computation'
      CALL FLUSH(6)	!Force l'envoie du message à l'écran      

      IF (WIND.NE.0.) THEN 
         IMAT_SURF=1	!Cas mer agitée
      ELSE
         IMAT_SURF=0 	!Cas mer plate
      ENDIF 
        
      IF (TRACE) THEN
         WRITE (IDLOG,*,ERR=910) " "
	  WRITE (IDLOG,*,ERR=910) " "
	  WRITE (IDLOG,*,ERR=910) 
     &	  "--------------------"
	  WRITE (IDLOG,*,ERR=910)
     &	  "RADIANCE COMPUTATION"
	  WRITE (IDLOG,*,ERR=910) 
     &	  "--------------------"      
      ENDIF !Fin Trace 

      !Estimation de la réflectance de fond dans le cas d'un modèle tabulé 	      
      IF ((IBOT_TYPE.GE.2).AND.(IBOT_TYPE.LE.5)) THEN

         !Récupération du nom du répertoire fic contenant le fichier de réflectances tabulées
         CALL GETENV('OSOAA_ROOT',DIRFIC)
         IFIN=INDEX(DIRFIC,' ')
         IFIN=IFIN-1
         IF (IFIN.LE.0) GOTO 4000
         IF ((IFIN+4).GT.CTE_LENDIR) GOTO 4002

         DIRFIC(IFIN+1:IFIN+4)='/fic'
         IFIN=IFIN+4
         IF (IFIN.LE.0) IFIN=CTE_LENDIR

         FIC_BOT_DATA = DIRFIC(1:IFIN)//'/'
     &                  //CTE_FIC_BOT_SPECTRAL_DATA

         IF (TRACE) THEN
            WRITE (IDLOG,*,ERR=910) " "	
            WRITE (IDLOG,*,ERR=910) 
     &      "   Interpolation of the sea bottom reflectance: "	  
            WRITE (IDLOG,*,ERR=910) 
     &      "      Data file: ", 	FIC_BOT_DATA  
         ENDIF !Fin Trace 

         !Lecture des réflectances spectrales tabulées
         OPEN(20,FILE=FIC_BOT_DATA,STATUS='OLD',ERR=2670)
         READ(20,'(a)',ERR=2671) LIGNE_TEXTE
         DO I=1,CTE_NB_WA_FIC_BOT
            READ(20,*,ERR=2671) TAB_WA(I), TAB_RSAND(I), 
     &           TAB_RGREEN(I), TAB_RBROWN(I), TAB_RRED(I)

            IF (IBOT_TYPE.EQ.2) THEN
               TAB_R_BOT(I) = TAB_RSAND(I)
            ELSE
               IF (IBOT_TYPE.EQ.3) THEN
                  TAB_R_BOT(I) = TAB_RGREEN(I)
               ELSE
                  IF (IBOT_TYPE.EQ.4) THEN
                     TAB_R_BOT(I) = TAB_RBROWN(I)
                  ELSE
                     TAB_R_BOT(I) = TAB_RRED(I)
                  ENDIF
               ENDIF
            ENDIF

	  ENDDO
3010     CLOSE(20)

         
         !Interpolation spectrale
         !  Sortie en erreur si WA_SIMU en dehors des valeurs tabulées
         WA_SIMU_NM = WA_SIMU * 1000.
         IF ( (WA_SIMU_NM.LT.TAB_WA(1)).OR.
     &       (WA_SIMU_NM.GT.TAB_WA(CTE_NB_WA_FIC_BOT)) ) THEN

            IF (TRACE) THEN
               WRITE (IDLOG,*,ERR=910) 
     &         " Simulation wavelength out of the range of"
               WRITE (IDLOG,*,ERR=910) 
     &         " wavelengths for tabulated spectral data"	
               WRITE (IDLOG,*,ERR=910) 
     &         "   Simulation wavelength (nm): ", WA_SIMU_NM
               WRITE (IDLOG,*,ERR=910) 
     &         "   Min wavelength for spectral data (nm): ", TAB_WA(1)
               WRITE (IDLOG,*,ERR=910) 
     &         "   Max wavelength for spectral data (nm): ", 
     &         TAB_WA(CTE_NB_WA_FIC_BOT)
               WRITE (IDLOG,*,ERR=910)
     &         "===> Use a wavelength is the range of spectral data."	
            ENDIF !Fin Trace 

            GOTO 4003

         !  Recherche des longueurs d'onde qui encadrent WA_SIMU
         !  et interpolation linéaire pour calculer RHO_BOT	
         ELSE 

            I=1
            DO WHILE (TAB_WA(I).LT.WA_SIMU_NM)
               I = I+1
            ENDDO

            !Gestion du cas WA_SIMU = WA max tabulée 
            !(I trouvée = CTE_NB_WA_FIC_BOT+1)
            IF (I.GT.CTE_NB_WA_FIC_BOT) I=CTE_NB_WA_FIC_BOT

            WA_SUP = TAB_WA(I)
            WA_INF = TAB_WA(I-1)
 
            R_SUP = TAB_R_BOT(I)
            R_INF = TAB_R_BOT(I-1)

            CALL SOS_INTERPOL(R_INF,R_SUP,WA_INF,WA_SUP,WA_SIMU_NM,
     &                        RHO_BOT)

            IF (TRACE) THEN               
               WRITE (IDLOG,*,ERR=910) 
     &         " Estimated sea bottom reflectance: ", RHO_BOT
            ENDIF !Fin Trace 
         ENDIF 



      ENDIF ! Fin cas gestion de réflectance de fond

     
      !Information si IGMAX n'est pas à sa valeur par défaut 	
      IF ((DEF_IGMAX.EQ.1).AND.(TRACE))	THEN
	  WRITE (IDLOG,*,ERR=910) " "
	  WRITE (IDLOG,*,ERR=910) "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! "
         WRITE (IDLOG,*,ERR=910) "WARNING : The maximal order of "
	  WRITE (IDLOG,*,ERR=910) "          interaction has been "
	  WRITE (IDLOG,*,ERR=910) "          changed by the user."
	  WRITE (IDLOG,*,ERR=910) "          ==> IGMAX =",IGMAX
	  WRITE (IDLOG,*,ERR=910) "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! "
	  WRITE (IDLOG,*,ERR=910) " "
      ENDIF ! Fin de Trace
      
C     Appel de OSOAA_SOS pour la simulation du champ de luminance
C
      IF (TRACE) THEN
          WRITE (IDLOG,*,ERR=910) " "
	  WRITE (IDLOG,*,ERR=910) 
     &	  "--> OSOAA_SOS is called with the parameters :"
          WRITE (IDLOG,*,ERR=910) "File for the angles definition:"
	  WRITE (IDLOG,832,ERR=910) FICANGLES_RES_LUM
	  WRITE (IDLOG,*,ERR=910) 
     &	  "Files for the particles radiative properties definition:"
	  WRITE (IDLOG,880,ERR=910) FICGRANU_AER
	  WRITE (IDLOG,881,ERR=910) FICGRANU_PHYTO
	  WRITE (IDLOG,882,ERR=910) FICGRANU_MLP
	  WRITE (IDLOG,*,ERR=910) "Files for the profiles definition:"
	  WRITE (IDLOG,883,ERR=910) FICPROFIL_ATM_RES
	  WRITE (IDLOG,884,ERR=910) FICPROFIL_SEA_RES
	  WRITE (IDLOG,*,ERR=910) "Sea surface parameters:"
	  WRITE (IDLOG,885,ERR=910) IMAT_SURF
	  WRITE (IDLOG,833,ERR=910) DIRSEA
	  WRITE (IDLOG,830,ERR=910) SEA_IND
	  WRITE (IDLOG,831,ERR=910) WIND	    
	  WRITE (IDLOG,886,ERR=910) RHO_SURF
	  WRITE (IDLOG,887,ERR=910) RHO_BOT
	  WRITE (IDLOG,*,ERR=910) "Maximal order of scattering:"
	  WRITE (IDLOG,888,ERR=910) IGMAX
	  WRITE (IDLOG,*,ERR=910) 
     &	  "Output binary file of fourier expansions:"
	  WRITE (IDLOG,889,ERR=910) FICSOS_RES_BIN
	  WRITE (IDLOG,*,ERR=910) "Advanced output files:"
	  WRITE (IDLOG,890,ERR=910) FICSOS_RES_ADV_UP
	  WRITE (IDLOG,891,ERR=910) FICSOS_RES_ADV_DOWN	 
	  WRITE (IDLOG,*,ERR=910) "Output parameters:"
	  WRITE (IDLOG,892,ERR=910) PHI 
	  WRITE (IDLOG,*,ERR=910) 
     &	  "  - For an output as a function of the propagation angle:"
          WRITE (IDLOG,893,ERR=910) LEVEL_OUT
	  WRITE (IDLOG,894,ERR=910) USER_Z_OUT
	  WRITE (IDLOG,895,ERR=910) FICSOS_RES_VS_VZA
	  WRITE (IDLOG,*,ERR=910) 
     &	  "  - For an output as a function of Z profile:"
          WRITE (IDLOG,896,ERR=910) VZA_OUT
	  WRITE (IDLOG,897,ERR=910) FICSOS_RES_VS_Z
          WRITE (IDLOG,8971,ERR=910) FIC_FLUX
	  WRITE (IDLOG,*,ERR=910) "Trace file:"
	  WRITE (IDLOG,898,ERR=910) FICSOS_LOG
	  IF (APPEL_SOS_CORE) THEN
	     WRITE (IDLOG,899,ERR=910) "TRUE"
	  ELSE   
	     WRITE (IDLOG,899,ERR=910) "FALSE"
	  ENDIF   
      ENDIF
       
      IF (IGMAX.NE.0) THEN     
          CALL OSOAA_SOS(FICANGLES_RES_LUM,
     &               FICGRANU_AER, FICPROFIL_ATM_RES,
     &               FICGRANU_PHYTO, FICGRANU_MLP, FICPROFIL_SEA_RES,
     &               IMAT_SURF, DIRSEA, SEA_IND, WIND, 
     &               RHO_SURF, RHO_BOT, IGMAX,
     &    	     FICSOS_RES_BIN,
     &               PHI, FICSOS_RES_ADV_UP, FICSOS_RES_ADV_DOWN,              
     &    	     LEVEL_OUT,USER_Z_OUT, FICSOS_RES_VS_VZA,
     &		     VZA_OUT, FICSOS_RES_VS_Z,FIC_FLUX,
     &    	     FICSOS_LOG, APPEL_SOS_CORE, IER)
      ELSE

          IF (TRACE) THEN
	      WRITE (IDLOG,*,ERR=910) " "
	      WRITE (IDLOG,*,ERR=910) 
     &        "   *************************************"
	      WRITE (IDLOG,*,ERR=910)  
     &       "   IGMAX = 0 ==> No radiance computation"	      
	      WRITE (IDLOG,*,ERR=910)  
     &       "   *************************************"
              WRITE (IDLOG,*,ERR=910) " "
 	  ENDIF

          IER = 0
      ENDIF     
     	 
	
      IF (IER.NE.0) THEN ! cas d'erreur
      		
          IF (TRACE) THEN
	      WRITE (IDLOG,*,ERR=910) 
     &	      "   ==> Error on subroutine OSOAA_SOS"
 	  ENDIF
	  GOTO 9000   
      ENDIF





C* Fin nominale 
C-------------------
      IF (TRACE) THEN
          WRITE (IDLOG,*,ERR=910) " "
          WRITE (IDLOG,*,ERR=910) "--> Nominal end of processes"
	  WRITE (IDLOG,*,ERR=910) "JOB_STATUS=OK"
	  CLOSE(IDLOG)
      ENDIF
      
      GOTO 9999
      



C =========================================================
C --- Cas d'erreur et retour du status 1 au shell  
C =========================================================                
  900 WRITE(6,*) '  OSOAA_MAIN : ERROR_900 on Log file opening'
      GOTO 9998

  910 WRITE(6,*) '  OSOAA_MAIN : ERROR_910 on Log file writing'
      GOTO 9998
            
 1000 WRITE(6,*) '  OSOAA_MAIN : ERROR_1000 on argument loading'
      WRITE(6,*) '  --> Check and correct the keyword : ', KEYWORD
      GOTO 9998  

 1050 WRITE(6,*) '  OSOAA_MAIN : ERROR_1050 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -OSOAA.Wa value : Wavelength for '
      WRITE(6,*) '       radiance simulation (microns) (Float)'
      GOTO 9998
            
 1100 WRITE(6,*) '  OSOAA_MAIN : ERROR_1100 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -ANG.Rad.NbGauss value : Number of Gauss'
      WRITE(6,*) '        angles for radiance computations (Integer).'
      GOTO 9998    

 1110 WRITE(6,*) '  OSOAA_MAIN : ERROR_1110 on argument loading'
      WRITE(6,*) '  --> Check required formats for -ANG.Thetas value :'
      WRITE(6,*) '        Solar zenithal angle (Float, degrees).'
      GOTO 9998  

 1120 WRITE(6,*) '  OSOAA_MAIN : ERROR_1120 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -ANG.Mie.NbGauss value : Number of Gauss'
      WRITE(6,*) '        angles for Mie computations (Integer).' 
      GOTO 9998     

 1200 WRITE(6,*) '  OSOAA_MAIN : ERROR_1200 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.Waref : Reference wavelength for the '
      WRITE(6,*) '       aerosol optical thickness (Float).' 
      GOTO 9998    

 1210 WRITE(6,*) '  OSOAA_MAIN : ERROR_1210 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.AOTref : Aerosol optical thickness at '
      WRITE(6,*) '        reference wavelength (Float).' 
      GOTO 9998   

 1220 WRITE(6,*) '  OSOAA_MAIN : ERROR_1220 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.Tronca : Option of aerosols phase '
      WRITE(6,*) '        function truncature (Integer).' 
      GOTO 9998   

 1230 WRITE(6,*) '  OSOAA_MAIN : ERROR_1230 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.Model : Type of aerosols model (Integer).' 
      GOTO 9998            

 1240 WRITE(6,*) '  OSOAA_MAIN : ERROR_1240 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.MMD.MRwa : Mono-modal aerosol model -'
      WRITE(6,*) '       Real part of the aerosol refractive index '
      WRITE(6,*) '       for the simulation wavelength (Float).' 
      GOTO 9998            

 1241 WRITE(6,*) '  OSOAA_MAIN : ERROR_1241 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.MMD.MIwa : Mono-modal aerosol model -'
      WRITE(6,*) '       Imaginary part of the aerosol refractive index'
      WRITE(6,*) '       for the simulation wavelength (Float).' 
      GOTO 9998             

 1242 WRITE(6,*) '  OSOAA_MAIN : ERROR_1242 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.MMD.MRwaref : Mono-modal aerosol model -'
      WRITE(6,*) '       Real part of the aerosol refractive index '
      WRITE(6,*) '       for the aerosol reference wavelength (Float).' 
      GOTO 9998            

 1243 WRITE(6,*) '  OSOAA_MAIN : ERROR_1243 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.MMD.MIwaref : Mono-modal aerosol model -'
      WRITE(6,*) '       Imaginary part of the aerosol refractive index'
      WRITE(6,*) '       for the aerosol reference wavelength (Float).' 
      GOTO 9998             

 1245 WRITE(6,*) '  OSOAA_MAIN : ERROR_1245 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.MMD.SDtype (Integer) : '
      WRITE(6,*) '       Mono-modal aerosol model : 1 LND '
      WRITE(6,*) '                                  2 Junge Law '
      GOTO 9998            
 
 1246 WRITE(6,*) '  OSOAA_MAIN : ERROR_1246 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.MMD.LNDradius : Mono-modal aerosol model -'
      WRITE(6,*) '       Modal radius (microns) of the LND (Float)'
      GOTO 9998      
            
 1247 WRITE(6,*) '  OSOAA_MAIN : ERROR_1247 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.MMD.LNDvar : Mono-modal aerosol model -'
      WRITE(6,*) '       Variance of the LND (Float)'
      GOTO 9998           
            
 1248 WRITE(6,*) '  OSOAA_MAIN : ERROR_1248 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.MMD.JD.slope : Aerosol Junge model - '
      WRITE(6,*) '       Slope (Float)'
      GOTO 9998  
            
 1249 WRITE(6,*) '  OSOAA_MAIN : ERROR_1249 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.MMD.JD.rmin : Aerosol Junge model - '
      WRITE(6,*) '       Minimal radius in microns (Float)'
      GOTO 9998  
            
 1250 WRITE(6,*) '  OSOAA_MAIN : ERROR_1250 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.MMD.JD.rmax : Aerosol Junge model - '
      WRITE(6,*) '       Maximal radius in microns (Float)'
      GOTO 9998  
	             
 1253 WRITE(6,*) '  OSOAA_MAIN : ERROR_1253 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.WMO.Model : Type of WMO Model (Integer)'
      GOTO 9998       	

 1254 WRITE(6,*) '  OSOAA_MAIN : ERROR_1254 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      Volumetric concentration of WMO user model :'
      WRITE(6,*) '      -AER.WMO.DL :  Dust-Like component (Float)'
      WRITE(6,*) '      -AER.WMO.WS :  Water Soluble component (Float)'
      WRITE(6,*) '      -AER.WMO.OC :  Oceanic component (Float)'
      WRITE(6,*) '      -AER.WMO.SO :  Soot component (Float)'
      GOTO 9998      	 
            
 1255 WRITE(6,*) '  OSOAA_MAIN : ERROR_1255 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.SF.Model : Type of Shettle & Fenn Model'
      WRITE(6,*) '       (Integer)'
      GOTO 9998       	

 1256 WRITE(6,*) '  OSOAA_MAIN : ERROR_1256 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.SF.RH : % of humidity relative (Float)'
      GOTO 9998       	

 1260 WRITE(6,*) '  OSOAA_MAIN : ERROR_1260 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.BMD.VCdef : Type of selected method for'
      WRITE(6,*) '       bi-modal LND volumetric concentration'
      WRITE(6,*) '       definition (Integer)'
      GOTO 9998       	
	
 1270 WRITE(6,*) '  OSOAA_MAIN : ERROR_1270 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.BMD.CoarseVC : User volumetric '
      WRITE(6,*) '       concentration for the "LND coarse mode" '
      WRITE(6,*) '       (Float)'
      GOTO 9998       	 
	
 1271 WRITE(6,*) '  OSOAA_MAIN : ERROR_1271 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.BMD.FineVC : User volumetric '
      WRITE(6,*) '       concentration for the "LND fine mode" (Float)'
      GOTO 9998       

 1280 WRITE(6,*) '  OSOAA_MAIN : ERROR_1280 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.BMD.RAOT : User value of the ration '
      WRITE(6,*) '       AOT_coarse / AOT_total '
      WRITE(6,*) '       for the aerosol reference wavelength (Float)'
      GOTO 9998       	 	

 1290 WRITE(6,*) '  OSOAA_MAIN : ERROR_1290 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.BMD.CM.MRwa : '
      WRITE(6,*) '       Real part of the refractive '
      WRITE(6,*) '       index for the "LND coarse mode" '
      WRITE(6,*) '       at the wavelength of aerosol properties '
      WRITE(6,*) '       calculation (Float)'
      GOTO 9998       	 		

 1291 WRITE(6,*) '  OSOAA_MAIN : ERROR_1291 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.BMD.CM.MIwa : '
      WRITE(6,*) '       Imaginary part of the refractive '
      WRITE(6,*) '       index for the "LND coarse mode" '
      WRITE(6,*) '       at the wavelength of aerosol properties '
      WRITE(6,*) '       calculation (Float)'
      GOTO 9998       
	 
 1292 WRITE(6,*) '  OSOAA_MAIN : ERROR_1292 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.BMD.CM.MRwaref : '
      WRITE(6,*) '       Real part of the refractive '
      WRITE(6,*) '       index for the "LND coarse mode" '
      WRITE(6,*) '        at the aerosol reference wavelength'
      WRITE(6,*) '       calculation (Float)'
      GOTO 9998       	 		

 1293 WRITE(6,*) '  OSOAA_MAIN : ERROR_1293 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.BMD.CM.MIwaref : '
      WRITE(6,*) '       Imaginary part of the refractive '
      WRITE(6,*) '       index for the "LND coarse mode" '
      WRITE(6,*) '       at the aerosol reference wavelength'
      WRITE(6,*) '       calculation (Float)'
      GOTO 9998  
           
 1294 WRITE(6,*) '  OSOAA_MAIN : ERROR_1294 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.BMD.CM.SDradius : '
      WRITE(6,*) '       Modal radius of the "LND coarse mode"'
      WRITE(6,*) '       in microns (Float)'
      GOTO 9998       	 		
           
 1295 WRITE(6,*) '  OSOAA_MAIN : ERROR_1295 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.BMD.CM.SDvar : '
      WRITE(6,*) '       Variance of the "LND coarse mode" '
      WRITE(6,*) '       (Float)'
      GOTO 9998       	 			

 1300 WRITE(6,*) '  OSOAA_MAIN : ERROR_1300 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.BMD.FM.MRwa : '
      WRITE(6,*) '       Real part of the refractive '
      WRITE(6,*) '       index for the "LND fine mode" '
      WRITE(6,*) '       at the wavelength of aerosol properties '
      WRITE(6,*) '       calculation (Float)'
      GOTO 9998       	 		

 1301 WRITE(6,*) '  OSOAA_MAIN : ERROR_1301 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.BMD.FM.MIwa : '
      WRITE(6,*) '       Imaginary part of the refractive '
      WRITE(6,*) '       index for the "LND fine mode" '
      WRITE(6,*) '       at the wavelength of aerosol properties '
      WRITE(6,*) '       calculation (Float)'
      GOTO 9998       
	 
 1302 WRITE(6,*) '  OSOAA_MAIN : ERROR_1302 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.BMD.FM.MRwaref : '
      WRITE(6,*) '       Real part of the refractive '
      WRITE(6,*) '       index for the "LND fine mode" '
      WRITE(6,*) '        at the aerosol reference wavelength'
      WRITE(6,*) '       calculation (Float)'
      GOTO 9998       	 		

 1303 WRITE(6,*) '  OSOAA_MAIN : ERROR_1303 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.BMD.FM.MIwaref : '
      WRITE(6,*) '       Imaginary part of the refractive '
      WRITE(6,*) '       index for the "LND fine mode" '
      WRITE(6,*) '       at the aerosol reference wavelength'
      WRITE(6,*) '       calculation (Float)'
      GOTO 9998  
           
 1304 WRITE(6,*) '  OSOAA_MAIN : ERROR_1304 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.BMD.FM.SDradius : '
      WRITE(6,*) '       Modal radius of the "LND fine mode"'
      WRITE(6,*) '       in microns (Float)'
      GOTO 9998       	 		
           
 1305 WRITE(6,*) '  OSOAA_MAIN : ERROR_1305 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AER.BMD.FM.SDvar : '
      WRITE(6,*) '       Variance of the "LND fine mode" (Float)'
      GOTO 9998       	 				

 1330 WRITE(6,*) '  OSOAA_MAIN : ERROR_1330 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -HYD.Model : '
      WRITE(6,*) '       Type of hydrosol characterization (Integer)'
      GOTO 9998       	 			

 1340 WRITE(6,*) '  OSOAA_MAIN : ERROR_1340 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.JD.slope : '
      WRITE(6,*) "       Slope of Junge's law "
      WRITE(6,*) '       for phytoplankton particles (microns) (Float)'
      GOTO 9998       	 			

 1341 WRITE(6,*) '  OSOAA_MAIN : ERROR_1341 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.JD.rmin : '
      WRITE(6,*) "       Minimal radius of Junge's law "
      WRITE(6,*) '       for phytoplankton particles (microns) (Float)'
      GOTO 9998 
        
 1342 WRITE(6,*) '  OSOAA_MAIN : ERROR_1342 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.JD.rmax : '
      WRITE(6,*) "       Maximal radius of Junge's law "
      WRITE(6,*) '       for phytoplankton particles (microns) (Float)'
      GOTO 9998 
        
 1343 WRITE(6,*) '  OSOAA_MAIN : ERROR_1343 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.JD.MRwa : '
      WRITE(6,*) "       Real part of the refractive index  (Float)"
      WRITE(6,*) "       of Junge's law for phytoplankton particles at"
      WRITE(6,*) "       the radiance simulation wavelength (-OSOAA.Wa)"
      GOTO 9998 
        
 1344 WRITE(6,*) '  OSOAA_MAIN : ERROR_1344 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.JD.MIwa : '
      WRITE(6,*) "       Imaginary part of the refractive index (Float)"
      WRITE(6,*) "       of Junge's law for phytoplankton particles at"
      WRITE(6,*) "       the radiance simulation wavelength (-OSOAA.Wa)"
      GOTO 9998 
        
 1345 WRITE(6,*) '  OSOAA_MAIN : ERROR_1345 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.JD.rate : '
      WRITE(6,*) "       Ratio of the Junge's law in the global "
      WRITE(6,*) "       distribution for phytoplankton particles"
      WRITE(6,*) "       (Float)"
      GOTO 9998 
		 
 1350 WRITE(6,*) '  OSOAA_MAIN : ERROR_1350 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.LND.SM.SDradius : '
      WRITE(6,*) '       Modal radius of the LND for secondary mode ' 
      WRITE(6,*) '       of phytoplankton particles (microns) (Float)'
      GOTO 9998 
	
 1351 WRITE(6,*) '  OSOAA_MAIN : ERROR_1351 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.LND.SM.SDvar : '
      WRITE(6,*) '       Variance of the LND for secondary mode ' 
      WRITE(6,*) '       of phytoplankton particles  (Float)'
      GOTO 9998 
		 
 1352 WRITE(6,*) '  OSOAA_MAIN : ERROR_1352 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.LND.SM.MRwa : '
      WRITE(6,*) '       Real part of the refractive index  (Float)'
      WRITE(6,*) '       of the LND secondary mode for phytoplankton '
      WRITE(6,*) '       particles atthe radiance simulation wavelength'
      WRITE(6,*) '       (-OSOAA.Wa)'
      GOTO 9998 
        
 1353 WRITE(6,*) '  OSOAA_MAIN : ERROR_1353 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.LND.SM.MIwa : '
      WRITE(6,*) '       Imaginary part of the refractive index (Float)'
      WRITE(6,*) '       of the LND secondary mode for phytoplankton '
      WRITE(6,*) '       particles atthe radiance simulation wavelength'
      WRITE(6,*) '       (-OSOAA.Wa)'
      GOTO 9998 	
	 	 
 1354 WRITE(6,*) '  OSOAA_MAIN : ERROR_1354 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.LND.SM.rate : '
      WRITE(6,*) '       Ratio of LND secondary mode in the global '
      WRITE(6,*) '       distribution for phytoplankton particles'
      WRITE(6,*) '       (Float)'
      GOTO 9998 	
		 
 1355 WRITE(6,*) '  OSOAA_MAIN : ERROR_1355 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.LND.TM.SDradius : '
      WRITE(6,*) '       Modal radius of the LND for tertiary mode ' 
      WRITE(6,*) '       of phytoplankton particles (microns) (Float)'
      GOTO 9998 
	
 1356 WRITE(6,*) '  OSOAA_MAIN : ERROR_1356 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.LND.TM.SDvar : '
      WRITE(6,*) '       Variance of the LND for tertiary mode ' 
      WRITE(6,*) '       of phytoplankton particles  (Float)'
      GOTO 9998 
		 
 1357 WRITE(6,*) '  OSOAA_MAIN : ERROR_1357 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.LND.TM.MRwa : '
      WRITE(6,*) '       Real part of the refractive index  (Float)'
      WRITE(6,*) '       of the LND tertiary mode for phytoplankton '
      WRITE(6,*) '       particles atthe radiance simulation wavelength'
      WRITE(6,*) '       (-OSOAA.Wa)'
      GOTO 9998 
        
 1358 WRITE(6,*) '  OSOAA_MAIN : ERROR_1358 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.LND.TM.MIwa : '
      WRITE(6,*) '       Imaginary part of the refractive index (Float)'
      WRITE(6,*) '       of the LND tertiary mode for phytoplankton '
      WRITE(6,*) '       particles atthe radiance simulation wavelength'
      WRITE(6,*) '       (-OSOAA.Wa)'
      GOTO 9998 	
	 	 
 1359 WRITE(6,*) '  OSOAA_MAIN : ERROR_1359 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.LND.TM.rate : '
      WRITE(6,*) '       Ratio of LND tertiary mode in the global '
      WRITE(6,*) '       distribution for phytoplankton particles'
      WRITE(6,*) '	 (Float)'
      GOTO 9998 	  
	

 1360 WRITE(6,*) '  OSOAA_MAIN : ERROR_1360 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SED.JD.slope : '
      WRITE(6,*) "       Slope of Junge's law "
      WRITE(6,*) '       for mineral-like particles (microns) (Float)'
      GOTO 9998       	 			

 1361 WRITE(6,*) '  OSOAA_MAIN : ERROR_1361 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SED.JD.rmin : '
      WRITE(6,*) "       Minimal radius of Junge's law "
      WRITE(6,*) '       for mineral-like particles (microns) (Float)'
      GOTO 9998 
        
 1362 WRITE(6,*) '  OSOAA_MAIN : ERROR_1362 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SED.JD.rmax : '
      WRITE(6,*) "       Maximal radius of Junge's law "
      WRITE(6,*) '       for mineral-like particles (microns) (Float)'
      GOTO 9998 
        
 1363 WRITE(6,*) '  OSOAA_MAIN : ERROR_1363 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SED.JD.MRwa : '
      WRITE(6,*) "       Real part of the refractive index  (Float)"
      WRITE(6,*) "       of Junge's law for mineral-like particles at"
      WRITE(6,*) "       the radiance simulation wavelength (-OSOAA.Wa)"
      GOTO 9998 
        
 1364 WRITE(6,*) '  OSOAA_MAIN : ERROR_1364 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SED.JD.MIwa : '
      WRITE(6,*) "       Imaginary part of the refractive index (Float)"
      WRITE(6,*) "       of Junge's law for mineral-like particles at"
      WRITE(6,*) "       the radiance simulation wavelength (-OSOAA.Wa)"
      GOTO 9998 
        
 1365 WRITE(6,*) '  OSOAA_MAIN : ERROR_1365 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SED.JD.rate : '
      WRITE(6,*) "       Ratio of the Junge's law in the global "
      WRITE(6,*) "       distribution for mineral-like particles"
      WRITE(6,*) "	 (Float)"
      GOTO 9998 
		 
 1370 WRITE(6,*) '  OSOAA_MAIN : ERROR_1370 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SED.LND.SM.SDradius : '
      WRITE(6,*) '       Modal radius of the LND for secondary mode ' 
      WRITE(6,*) '       of mineral-like particles (microns) (Float)'
      GOTO 9998 
	
 1371 WRITE(6,*) '  OSOAA_MAIN : ERROR_1371 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SED.LND.SM.SDvar : '
      WRITE(6,*) '       Variance of the LND for secondary mode ' 
      WRITE(6,*) '       of mineral-like particles  (Float)'
      GOTO 9998 
		 
 1372 WRITE(6,*) '  OSOAA_MAIN : ERROR_1372 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SED.LND.SM.MRwa : '
      WRITE(6,*) '       Real part of the refractive index  (Float)'
      WRITE(6,*) '       of the LND secondary mode for mineral-like '
      WRITE(6,*) '       particles atthe radiance simulation wavelength'
      WRITE(6,*) '       (-OSOAA.Wa)'
      GOTO 9998 
        
 1373 WRITE(6,*) '  OSOAA_MAIN : ERROR_1373 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SED.LND.SM.MIwa : '
      WRITE(6,*) '       Imaginary part of the refractive index (Float)'
      WRITE(6,*) '       of the LND secondary mode for mineral-like '
      WRITE(6,*) '       particles atthe radiance simulation wavelength'
      WRITE(6,*) '       (-OSOAA.Wa)'
      GOTO 9998 	
	 	 
 1374 WRITE(6,*) '  OSOAA_MAIN : ERROR_1374 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SED.LND.SM.rate : '
      WRITE(6,*) '       Ratio of LND secondary mode in the global '
      WRITE(6,*) '       distribution for mineral-like particles'
      WRITE(6,*) '       (Float)'
      GOTO 9998 	
		 
 1375 WRITE(6,*) '  OSOAA_MAIN : ERROR_1375 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SED.LND.TM.SDradius : '
      WRITE(6,*) '       Modal radius of the LND for tertiary mode ' 
      WRITE(6,*) '       of mineral-like particles (microns) (Float)'
      GOTO 9998 
	
 1376 WRITE(6,*) '  OSOAA_MAIN : ERROR_1376 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SED.LND.TM.SDvar : '
      WRITE(6,*) '       Variance of the LND for tertiary mode ' 
      WRITE(6,*) '       of mineral-like particles  (Float)'
      GOTO 9998 
		 
 1377 WRITE(6,*) '  OSOAA_MAIN : ERROR_1377 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SED.LND.TM.MRwa : '
      WRITE(6,*) '       Real part of the refractive index (Float)'
      WRITE(6,*) '       of the LND tertiary mode for mineral-like '
      WRITE(6,*) '       particles atthe radiance simulation wavelength'
      WRITE(6,*) '       (-OSOAA.Wa)'
      GOTO 9998 
        
 1378 WRITE(6,*) '  OSOAA_MAIN : ERROR_1378 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SED.LND.TM.MIwa : '
      WRITE(6,*) '       Imaginary part of the refractive index (Float)'
      WRITE(6,*) '       of the LND tertiary mode for mineral-like '
      WRITE(6,*) '       particles atthe radiance simulation wavelength'
      WRITE(6,*) '       (-OSOAA.Wa)'
      GOTO 9998 	
	 	 
 1379 WRITE(6,*) '  OSOAA_MAIN : ERROR_1379 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      SED.LND.TM.rate : '
      WRITE(6,*) '       Ratio of LND tertiary mode in the global '
      WRITE(6,*) '       distribution for mineral-like particles '
      WRITE(6,*) '       (Float)'
      GOTO 9998 	  	 
	    
 1500 WRITE(6,*) '  OSOAA_MAIN : ERROR_1500 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AP.MOT : Molecular atmos optical thickness '
      WRITE(6,*) '     (for the radiance simulation wavelength) (Float)'
      GOTO 9998   

 1510 WRITE(6,*) '  OSOAA_MAIN : ERROR_1510 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AP.Pressure : Atmospheric pressure at sea '
      WRITE(6,*) '        level (mbar) (Float).' 
      GOTO 9998   
      
 1520 WRITE(6,*) '  OSOAA_MAIN : ERROR_1520 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AP.HR : Height scale of the molecular '
      WRITE(6,*) '        atmospheric profile (Float).' 
      GOTO 9998   
      
 1530 WRITE(6,*) '  OSOAA_MAIN : ERROR_1530 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -AP.HA : Height scale of the aerosol '
      WRITE(6,*) '        atmospheric profile (Float).' 
      GOTO 9998   

 1540 WRITE(6,*) '  OSOAA_MAIN : ERROR_1540 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SEA.Depth : Sea depth value (meters)  (Float)'
      GOTO 9998   

 1550 WRITE(6,*) '  OSOAA_MAIN : ERROR_1550 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.Chl : Chlorophylle concentration at sea' 
      WRITE(6,*) '        surface (mg/m3) (Float).'
      GOTO 9998
      
 1560 WRITE(6,*) '  OSOAA_MAIN : ERROR_1560 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.ProfilType : Type of chlorophylle ' 
      WRITE(6,*) '        profile (Integer).'
      GOTO 9998
  
 1570 WRITE(6,*) '  OSOAA_MAIN : ERROR_1570 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.GP.Chlbg :  ' 
      WRITE(6,*) '        Constant biomass background (mg/m3) (Float).'
      GOTO 9998  
   
 1571 WRITE(6,*) '  OSOAA_MAIN : ERROR_1571 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.GP.Deep :  ' 
      WRITE(6,*) '        Maximum deep of gaussian chlorophyll '
      WRITE(6,*) '        profile(m) (Float).'
      GOTO 9998  
    
 1572 WRITE(6,*) '  OSOAA_MAIN : ERROR_1572 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -PHYTO.GP.Width : Width of the chlorophyll peak' 
      WRITE(6,*) '        of gaussian profile (m) (Float).'
      GOTO 9998  

 1580 WRITE(6,*) '  OSOAA_MAIN : ERROR_1580 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SED.Csed :  Sediment concentration' 
      WRITE(6,*) '         at sea surface (mg/liter) (Float).'
      GOTO 9998     

 1590 WRITE(6,*) '  OSOAA_MAIN : ERROR_1590 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -YS.Abs440 :  Absorption coefficient (m-1) of' 
      WRITE(6,*) '         yellow substance at 440 nm (Float).'
      GOTO 9998     

 1591 WRITE(6,*) '  OSOAA_MAIN : ERROR_1591 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -YS.Swa :  Coefficient of spectral variation' 
      WRITE(6,*) '         for yellow substance absorption (Float).'
      GOTO 9998  	 
   
 1592 WRITE(6,*) '  OSOAA_MAIN : ERROR_1592 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -DET.Abs440 :  Absorption coefficient (m-1) of' 
      WRITE(6,*) '                     detritus at 440 nm (Float).'
      GOTO 9998     

 1593 WRITE(6,*) '  OSOAA_MAIN : ERROR_1593 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -DET.Swa :  Coefficient of spectral variation' 
      WRITE(6,*) '                  for detritus  (Float).'
      GOTO 9998	 

 1600 WRITE(6,*) '  OSOAA_MAIN : ERROR_1600 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SEA.Ind : Refractive index of sea water '
      WRITE(6,*) '                 (Float)'
      GOTO 9998	 
      	 	 	 
 1610 WRITE(6,*) '  OSOAA_MAIN : ERROR_1610 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SEA.Wind :  Wind velocity (m/s)   (Float).'
      GOTO 9998	 
      	 	 	 
 1680 WRITE(6,*) '  OSOAA_MAIN : ERROR_1680 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SEA.SurfAlb :  Sea surface albedo (Float).'
      GOTO 9998	 
      	 	 	 
 1690 WRITE(6,*) '  OSOAA_MAIN : ERROR_1690 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SEA.BotType :  Type of sea bottom (Integer).'
      WRITE(6,*) '      	1 : Lambertian value  (user data)'
      WRITE(6,*) '      	2 : Light sand	      (tabulated data)'
      WRITE(6,*) '      	3 : Green algua	      (tabulated data)'
      WRITE(6,*) '      	4 : Brown algae	      (tabulated data)'
      WRITE(6,*) '      	5 : Red algae	      (tabulated data)'
      GOTO 9998	 

 1695 WRITE(6,*) '  OSOAA_MAIN : ERROR_1695 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SEA.BotAlb :  Sea bottom albedo   (Float).'
      GOTO 9998	 
   
 1700 WRITE(6,*) '  OSOAA_MAIN : ERROR_1700 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -OSOAA.View.Phi : Relative azimuth angle  '
      WRITE(6,*) '                        (degrees)   (Float).'
      GOTO 9998	 

 1710 WRITE(6,*) '  OSOAA_MAIN : ERROR_1710 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -OSOAA.View.Z : Altitude or depth for which the'
      WRITE(6,*) '      		radiance has to be given versus'
      WRITE(6,*) '      		the viewing zenith angle'
      WRITE(6,*) '      	       (for the given relative azimuth)'
      WRITE(6,*) '                     (meters)   (Float).'    
      GOTO 9998	 

 1720 WRITE(6,*) '  OSOAA_MAIN : ERROR_1720 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -OSOAA.View.VZA : Viewing zenith angle for '
      WRITE(6,*) '      		  which the radiance has to be '
      WRITE(6,*) '      		  given vs the depth (or alt.)'
      WRITE(6,*) '      		 (for a given relative azimuth)'
      WRITE(6,*) '                       (degrees)   (Float).'    
      GOTO 9998	 

 1730 WRITE(6,*) '  OSOAA_MAIN : ERROR_1730 on argument loading'
      WRITE(6,*) '  --> Check required formats for '
      WRITE(6,*) '      -SOS.IGmax : maximal order of interaction '
      WRITE(6,*) '      (Integer)'
      GOTO 9998	 

 1990 WRITE(6,*) '  OSOAA_MAIN : ERROR_1990 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -OSOAA.ResRoot Value ' 
      WRITE(6,*) '  ==>  Working folder of OSOAA computations'
      GOTO 9998 

 1991 WRITE(6,*) '  OSOAA_MAIN : ERROR_1991 : '
      WRITE(6,*) '  => Working folder name : maybe too long'
      WRITE(6,*) '  => Check if CTE_LENDIR has to be adjusted.' 
      GOTO 9998                  	

 1992 WRITE(6,*) '  OSOAA_MAIN : ERROR_1992 : '
      WRITE(6,*) '  => Standard folder name : maybe too long'
      WRITE(6,*) '  => Check if CTE_LENDIR has to be adjusted.' 
      GOTO 9998  
          
 1993 WRITE(6,*) '  OSOAA_MAIN : ERROR_1993 : '
      WRITE(6,*) '  => Advanced folder name : maybe too long'
      WRITE(6,*) '  => Check if CTE_LENDIR has to be adjusted.' 
      GOTO 9998         

 1995 WRITE(6,*) '  OSOAA_MAIN : ERROR_1995 : '
      WRITE(6,*) '  => Command system chain : maybe too long'
      WRITE(6,*) '  => Check if CTE_LENCOM has to be adjusted.' 
      GOTO 9998      

 1996 WRITE(6,*) '  OSOAA_MAIN : ERROR_1996 : '
      WRITE(6,*) '  => File name with complete access : maybe too long'
      WRITE(6,*) '  => Check if CTE_LENFIC2 has to be adjusted.' 
      GOTO 9998  

 1997 WRITE(6,*) '  OSOAA_MAIN : ERROR_1997 : '
      WRITE(6,*) '  => Mie Hydrosols repository dir: maybe too long'
      WRITE(6,*) '  => Check if CTE_LENDIR has to be adjusted.' 
      GOTO 9998                  	

 1998 WRITE(6,*) '  OSOAA_MAIN : ERROR_1998 : '
      WRITE(6,*) '  => Mie Aerosols repository dir: maybe too long'
      WRITE(6,*) '  => Check if CTE_LENDIR has to be adjusted.' 
      GOTO 9998                  	

 1999 WRITE(6,*) '  OSOAA_MAIN : ERROR_1999 : '
      WRITE(6,*) '  => Surface matrices repository dir: maybe too long'
      WRITE(6,*) '  => Check if CTE_LENDIR has to be adjusted.' 
      GOTO 9998  
      
                                        	 
 2000 WRITE(6,*) '  OSOAA_MAIN : ERROR_2000 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -OSOAA.Wa Value (format : Float)' 
      WRITE(6,*) '  ==>  Wavelength for radiance simulation (microns)'
      GOTO 9998 

 2010 WRITE(6,*) '  OSOAA_MAIN : ERROR_2010 on parameters'
      WRITE(6,*) '  --> -OSOAA.Wa Value is lower than the minimal '
      WRITE(6,*) '      available value : constante CTE_WAMIN : '
      WRITE(6,*) '         OSOAA.Wa = ', WA_SIMU
      WRITE(6,*) '         CTE_WAMIN = ', CTE_WAMIN
      GOTO 9998 
                                                                                                           
 2100 WRITE(6,*) '  OSOAA_MAIN : ERROR_2100 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -ANG.Thetas Value (format : Float)' 
      WRITE(6,*) '  ==> Solar zenithal angle (degrees)'
      GOTO 9998 

 2105 WRITE(6,*) '  OSOAA_MAIN : ERROR_2105 on parameters'
      WRITE(6,*) '  -->  -ANG.Thetas value : SZA out of available range'
      GOTO 9998  	

 2200 WRITE(6,*) '  OSOAA_MAIN : ERROR_2200 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '         -AP.MOT : Molecular atmos optical thickness',
     &		 '         (at radiance simulation wavelength) ',
     &		 '      OR -AP.Pressure : Atmos pressure at sea level',
     &           '      ==> Can not be defined together : ',
     &           '	    only -AP.MOT or -AP.Pressure  (Float).'
      GOTO 9998    
	
 2210 WRITE(6,*) '  OSOAA_MAIN : ERROR_2210 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AP.HR Value   (format : Float)' 
      WRITE(6,*) '  ==> Height scale of the molecular atmos. profile'
      GOTO 9998 		

 2220 WRITE(6,*) '  OSOAA_MAIN : ERROR_2220 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AP.HA Value   (format : Float)' 
      WRITE(6,*) '  ==> Height scale of the aerosol atmospheric profile'
      GOTO 9998     

 2230 WRITE(6,*) '  OSOAA_MAIN : ERROR_2230 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -PHYTO.Chl Value   (format : Float)' 
      WRITE(6,*) '  ==> Chlorophylle concentration at surface (mg/m3)'
      GOTO 9998     

 2240 WRITE(6,*) '  OSOAA_MAIN : ERROR_2240 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -PHYTO.ProfilType Value   (format : Integer)' 
      WRITE(6,*) '  ==> Option of chrolophylle profile type'
      WRITE(6,*) '		1 : Homogeneous profile'
      WRITE(6,*) '		2 : Gaussian profile'
      WRITE(6,*) '		3 : Userfile profile'
      GOTO 9998  

 2250 WRITE(6,*) '  OSOAA_MAIN : ERROR_2250 '
      WRITE(6,*) '  --> Wrong option of chrolophylle profile type'
      WRITE(6,*) '      Check -PHYTO.ProfilType Value   '
      WRITE(6,*) '		1 : Homogeneous profile'
      WRITE(6,*) '		2 : Gaussian profile'
      WRITE(6,*) '		3 : Userfile profile'
      GOTO 9998

 2251 WRITE(6,*) '  OSOAA_MAIN : ERROR_2251 '
      WRITE(6,*) '  --> Gaussian chrolophylle profile'
      WRITE(6,*) '      requires (-Keyword Value): ' 
      WRITE(6,*) '     -PHYTO.GP.Chlbg Value   (format : Float)' 
      WRITE(6,*) '  ==> Constant biomass background (mg/m3)'
      WRITE(6,*) '     -PHYTO.GP.Deep  Value   (format : Float)' 
      WRITE(6,*) '  ==> Maximum deep of gaussian chlorophyll profile(m)'
      WRITE(6,*) '     -PHYTO.GP.Width Value   (format : Float)' 
      WRITE(6,*) '  ==> Width of chloro. peak of gaussian profile (m)'
      GOTO 9998

 2252 WRITE(6,*) '  OSOAA_MAIN : ERROR_2252 '
      WRITE(6,*) '  --> Using a chrolophylle profile defined by user'
      WRITE(6,*) '      requires (-Keyword Value): ' 
      WRITE(6,*) '     -PHYTO.Userfile Value   ' 
      WRITE(6,*) '  ==> Name of user phyto. profile file '
      WRITE(6,*) '      (complete access)'
      GOTO 9998          

 2260 WRITE(6,*) '  OSOAA_MAIN : ERROR_2260 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -SED.Csed Value    (format : Float)' 
      WRITE(6,*) '  ==> Sediment concentration at sea surf. (mg/liter)'
      GOTO 9998  

 2270 WRITE(6,*) '  OSOAA_MAIN : ERROR_2270 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -YS.Abs440 Value    (format : Float)' 
      WRITE(6,*) '  ==> Yellow substance abs. coef. (m-1)  at 440 nm'
      GOTO 9998  
      	  
 2275 WRITE(6,*) '  OSOAA_MAIN : ERROR_2275 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -DET.Abs440 Value    (format : Float)'
      WRITE(6,*) '  ==> Detritus absorption coef. (m-1)  at 440 nm'
      GOTO 9998  	
      
 2300 WRITE(6,*) '  OSOAA_MAIN : ERROR_2300 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -SEA.Ind Value    (format : Float)'
      WRITE(6,*) '  ==> Refractive index of sea water '
      GOTO 9998   

 2310 WRITE(6,*) '  OSOAA_MAIN : ERROR_2310 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -SEA.Wind Value    (format : Float)'
      WRITE(6,*) '  ==> Wind velocity (m/s) '
      GOTO 9998   

 2320 WRITE(6,*) '  OSOAA_MAIN : ERROR_2320 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -SEA.Dir Value    '
      WRITE(6,*) '  ==> Depository repertory for interface matrices'
      WRITE(6,*) '      (complete access) '
      GOTO 9998   

 2400 WRITE(6,*) '  OSOAA_MAIN : ERROR_2400 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AER.Waref Value    (format : Float)'
      WRITE(6,*) '  ==> Reference wavelength for the '
      WRITE(6,*) '      aerosol optical thickness'
      GOTO 9998   

 2405 WRITE(6,*) '  OSOAA_MAIN : ERROR_2405 on parameters'
      WRITE(6,*) '  --> -AER.Waref Value is lower than the minimal '
      WRITE(6,*) '      available value : constante CTE_WAMIN : '
      WRITE(6,*) '         AER.Waref = ', WAREF_AOT
      WRITE(6,*) '         CTE_WAMIN = ', CTE_WAMIN
      GOTO 9998 
      
 2410 WRITE(6,*) '  OSOAA_MAIN : ERROR_2410 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AER.AOTref Value    (format : Float)'
      WRITE(6,*) '  ==> Aerosol optical thickness at  '
      WRITE(6,*) '      reference wavelength '
      GOTO 9998   

 2415 WRITE(6,*) '  OSOAA_MAIN : ERROR_2415 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AER.DirMie Value   '
      WRITE(6,*) '  ==> Storage directory for MIE files producted by'
      WRITE(6,*) '      OSOAA_AEROSOLS computations (complete path) '
      GOTO 9998   
            
 2420 WRITE(6,*) '  OSOAA_MAIN : ERROR_2420 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AER.Model Value (format : Integer)'
      WRITE(6,*) '  ==> Type of aerosol model : '
      WRITE(6,*) '         0 : Mono-modal '
      WRITE(6,*) '         1 : WMO multi-modal '   
      WRITE(6,*) '         2 : Shettle & Fenn bi-modal '    
      WRITE(6,*) '         3 : Log-Normal bi-modal '
      WRITE(6,*) '         4 : Phaze functions from an external source'
      GOTO 9998   

 2421 WRITE(6,*) '  OSOAA_MAIN : ERROR_2421 on parameters'
      WRITE(6,*) '  -->  -AER.Model value : out of available range.'
      WRITE(6,*) '  ==> Type of aerosol model : '
      WRITE(6,*) '         0 : Mono-modal '
      WRITE(6,*) '         1 : WMO multi-modal '   
      WRITE(6,*) '         2 : Shettle & Fenn bi-modal '    
      WRITE(6,*) '         3 : Log-Normal bi-modal '
      WRITE(6,*) '         4 : Phaze functions from an external source'
      GOTO 9998  
        
 2430 WRITE(6,*) '  OSOAA_MAIN : ERROR_2430 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AER.MMD.MRwa Value (format : Float)',
     &           '     -AER.MMD.MIwa Value (format : Float)'
      WRITE(6,*) '  ==> Aerosol refractive index '
      WRITE(6,*) '      for mono-modal size distribution'
      WRITE(6,*) '      (real and imaginary parts)'
      WRITE(6,*) '      at the wavelength of radiance calculation.'
      GOTO 9998   

 2440 WRITE(6,*) '  OSOAA_MAIN : ERROR_2440 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AER.MMD.SDtype Value    (format : Integer)'
      WRITE(6,*) '  ==> Model index for mono-modal size distribution :'
      WRITE(6,*) '         1 : LND '
      WRITE(6,*) '         2 : Junge law '
      GOTO 9998    
	     
 2441 WRITE(6,*) '  OSOAA_MAIN : ERROR_2441 on parameters'
      WRITE(6,*) '  --> Case AER.MMD.SDtype = 1 '
      WRITE(6,*) '      requires (-Keyword Value) definition : ',
     &           '     -AER.MMD.LNDradius Value    (format : Float)',
     &           '     -AER.MMD.LNDvar Value       (format : Float)'
      WRITE(6,*) '  ==> Parameters of LND mono-modal aerosol size '
      WRITE(6,*) '      distribution :'
      WRITE(6,*) '         - modal radius (microns) '
      WRITE(6,*) '         - and standard deviation'
      GOTO 9998  	     
	     
 2442 WRITE(6,*) '  OSOAA_MAIN : ERROR_2442 on parameters'
      WRITE(6,*) '  --> Case AER.MMD.SDtype = 2' 
      WRITE(6,*) '      requires (-Keyword Value) definition : ',
     &           '     -AER.MMD.JD.slope Value    (format : Float)',
     &           '     -AER.MMD.JD.rmin  Value    (format : Float)'
      WRITE(6,*) '  ==> Parameters of Junge law mono-modal aerosol size'
      WRITE(6,*) '      distribution :'
      WRITE(6,*) '         - slope'
      WRITE(6,*) '         - minimal radius (microns)'
      GOTO 9998  	
            	     
 2450 WRITE(6,*) '  OSOAA_MAIN : ERROR_2450 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AER.MMD.MRwaref Value (format : Float)',
     &           '     -AER.MMD.MIwaref Value (format : Float)'
      WRITE(6,*) '  ==> Aerosol refractive index '
      WRITE(6,*) '      for mono-modal size distribution'
      WRITE(6,*) '      (real and imaginary parts)'
      WRITE(6,*) '      at the wavelength of reference.'
      GOTO 9998        
	     
 2460 WRITE(6,*) '  OSOAA_MAIN : ERROR_2460 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AER.WMO.Model Value    (format : Integer)'
      WRITE(6,*) '  ==> Type of WMO model :'
      WRITE(6,*) '         1 : Continental WMO model. '
      WRITE(6,*) '         2 : Maritime WMO model. '
      WRITE(6,*) '         3 : Urban WMO model. '
      WRITE(6,*) '         4 : WMO model by user definition. '
      GOTO 9998    
        	     
 2461 WRITE(6,*) '  OSOAA_MAIN : ERROR_2461 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AER.WMO.DL Value    (format : Float)',
     &           '     -AER.WMO.WS Value    (format : Float)',
     &           '     -AER.WMO.OC Value    (format : Float)',
     &           '     -AER.WMO.SO Value    (format : Float)'
      WRITE(6,*) '  ==> Volumetric concentration (%) for '
      WRITE(6,*) '         WMO Dust-Like particles'
      WRITE(6,*) '         WMO Water Soluble particles'	    
      WRITE(6,*) '         WMO OCeanic particles'
      WRITE(6,*) '         WMO SOot particles'	    
      GOTO 9998 
            	  
 2470 WRITE(6,*) '  OSOAA_MAIN : ERROR_2470 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AER.SF.Model Value    (format : Integer)'
      WRITE(6,*) '  ==> Type of Shettle & Fenn model :'		  
      WRITE(6,*) '         1 : Tropospheric S&F model. '
      WRITE(6,*) '         2 : Urban S&F model. '
      WRITE(6,*) '         3 : Maritime S&F model. '
      WRITE(6,*) '         4 : Coastal S&F model. '
      GOTO 9998    		 

 2471 WRITE(6,*) '  OSOAA_MAIN : ERROR_2471 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AER.SF.RH Value    (format : Float)'
      WRITE(6,*) '  ==> % of relative humidity'
      GOTO 9998   		  

 2480 WRITE(6,*) '  OSOAA_MAIN : ERROR_2480 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AER.BMD.VCdef Value (format : Integer)'
      WRITE(6,*) '  ==> Type of selected method for bi-modal LND'
      WRITE(6,*) '      volumetric concentration definition :'
      WRITE(6,*) '        1 : Use of user volumetric concentrations.'
      WRITE(6,*) '        2 : Use of user ratio AOT_coarse / AOT_total.'
      GOTO 9998   		  
      
 2481 WRITE(6,*) '  OSOAA_MAIN : ERROR_2481 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AER.BMD.CoarseVC (format : Float)',
     &           '     -AER.BMD.FineVC   (format : Float)'
      WRITE(6,*) '  ==> Volumetric concentration for bimodal LND'      
      GOTO 9998   		  
      
 2482 WRITE(6,*) '  OSOAA_MAIN : ERROR_2482 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AER.BMD.RAOT (format : Float)'
      WRITE(6,*) '  ==> User volumetric concentration for bimodal LND'  
      WRITE(6,*) '      defined by AOT ratio coarse / total'  
      GOTO 9998  

 2483 WRITE(6,*) '  OSOAA_MAIN : ERROR_2483 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AER.BMD.CM.MRwa       (format : Float)',
     &           '     -AER.BMD.CM.MIwa       (format : Float)',
     &           '     -AER.BMD.CM.SDradius   (format : Float)',
     &           '     -AER.BMD.CM.SDvar      (format : Float)'
      WRITE(6,*) '  ==> Bimode LND coarse mode for wavelength -OSOAA.Wa'   
      GOTO 9998  
       		       
 2484 WRITE(6,*) '  OSOAA_MAIN : ERROR_2484 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AER.BMD.FM.MRwa       (format : Float)',
     &           '     -AER.BMD.FM.MIwa       (format : Float)',
     &           '     -AER.BMD.FM.SDradius   (format : Float)',
     &           '     -AER.BMD.FM.SDvar      (format : Float)'
      WRITE(6,*) '  ==> Bimode LND fine mode for wavelength -OSOAA.Wa'    
      GOTO 9998    

 2485 WRITE(6,*) '  OSOAA_MAIN : ERROR_2485 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -AER.BMD.CM.MRwaref (format : Float)',
     &           '     -AER.BMD.CM.MIwaref (format : Float)',
     &           '     -AER.BMD.FM.MRwaref (format : Float)',
     &           '     -AER.BMD.FM.MIwaref (format : Float)'
      WRITE(6,*) '  ==> Volumetric concentration for bimodal LND' 
      WRITE(6,*) '      defined by AOT ratio coarse / total'
      WRITE(6,*) '      requires (-Keyword Value) '
      WRITE(6,*) '      for the refractive indexes of coarse'
      WRITE(6,*) '      and fine modes at wavelength -AER.Waref' 
      GOTO 9998       
            
 2500 WRITE(6,*) '  OSOAA_MAIN : ERROR_2500 on parameters'
      WRITE(6,*) '  --> External phaze functions file -AER.ExtData'
      WRITE(6,*) '      requires (-Keyword Value)  '
      GOTO 9998   		  
	 
 2505 WRITE(6,*) '  OSOAA_MAIN : ERROR_2505 on parameters'
      WRITE(6,*) '  --> When using an external aerosol phaze function '
      WRITE(6,*) '      (-AER.Model 4 -AER.ExtData MyFile.txt) '
      WRITE(6,*) '      the reference aerosol wavelength (-AER.Waref) '
      WRITE(6,*) '      has to be equal to the radiance simulation '
      WRITE(6,*) '      wavelength (-OSOAA.Wa)   '
      GOTO 9998   

 2510 WRITE(6,*) '  OSOAA_MAIN : ERROR_2510 on parameters'
      WRITE(6,*) '  --> Sea surface albedo -SEA.SurfAlb'
      WRITE(6,*) '      requires (-Keyword Value)  '
      GOTO 9998 

 2520 WRITE(6,*) '  OSOAA_MAIN : ERROR_2520 on parameters'
      WRITE(6,*) '  --> Type of sea bottom -SEA.BotType'
      WRITE(6,*) '      requires (-Keyword Value)  '
      GOTO 9998 

 2525 WRITE(6,*) '  OSOAA_MAIN : ERROR_2525 on parameters'
      WRITE(6,*) '  --> When using the sea bottom albedo definition'
      WRITE(6,*) '      by an user value(-SEA.BotType 1) '
      WRITE(6,*) '      the sea bottom albedo has to be defined',
     &           '      -SEA.BotAlb (-Keyword Value) (format : Float)'
      GOTO 9998 

 2540 WRITE(6,*) '  OSOAA_MAIN : ERROR_2540 on parameters'
      WRITE(6,*) '  --> Relative azimuth -OSOAA.View.Phi'
      WRITE(6,*) '      requires (-Keyword Value)  '
      GOTO 9998 

 2545 WRITE(6,*) '  OSOAA_MAIN : ERROR_2545 on parameters'
      WRITE(6,*) 
     &'  --> Type of output level in profile -OSOAA.View.Level'
      WRITE(6,*) '      required (-Keyword Value)  '
      WRITE(6,*) '         1 : Top of Atmosphere'
      WRITE(6,*) '         2 : Sea Bottom'
      WRITE(6,*) '         3 : Sea Surface 0+'
      WRITE(6,*) '         4 : Sea Surface 0-'
      WRITE(6,*) "         5 : User's definition of altitude or depth"  
      GOTO 9998 

 2546 WRITE(6,*) '  OSOAA_MAIN : ERROR_2546 on parameters'
      WRITE(6,*) '      -OSOAA.View.Level value :'
      WRITE(6,*) '  --> out of available range'
      WRITE(6,*) '  --> Defined value : ',LEVEL_OUT
      WRITE(6,*) '      Has to be define between 1 and 5'  
      WRITE(6,*) '         1 : Top of Atmosphere'
      WRITE(6,*) '         2 : Sea Bottom'
      WRITE(6,*) '         3 : Sea Surface 0+'
      WRITE(6,*) '         4 : Sea Surface 0-'
      WRITE(6,*) "         5 : User's definition of altitude or depth"  
      GOTO 9998  
                  
 2550 WRITE(6,*) '  OSOAA_MAIN : ERROR_2550 on parameters'
      WRITE(6,*) '  --> Altitude or depth (meters) for radiance '
      WRITE(6,*) '      vs viewing zenith angle : -OSOAA.View.Z'
      WRITE(6,*) '      requires (-Keyword Value)  '
      GOTO 9998       
      
 2560 WRITE(6,*) '  OSOAA_MAIN : ERROR_2560 on parameters'
      WRITE(6,*) '  --> Viewing zenith angle (degrees) for radiance '
      WRITE(6,*) '      vs depth (or altitude) : -OSOAA.View.VZA'
      WRITE(6,*) '      and associated result file -OSOAA.ResFile.vsZ'
      WRITE(6,*) '      have to be defined together (-Keyword Value) '
      WRITE(6,*) '      if one of them is defined '
      GOTO 9998       

 2565 WRITE(6,*) '  OSOAA_MAIN : ERROR_2565 on parameters'
      WRITE(6,*) '  --> Viewing zenith angle (degrees) for radiance '
      WRITE(6,*) '      vs depth (or altitude) : -OSOAA.View.VZA'
      WRITE(6,*) '      has to be definer between 0 and 180° :'
      WRITE(6,*) '           0 - 90° : upward radiance '
      WRITE(6,*) '           90° - 180° : downward radiance '
      GOTO 9998       
      
 2610 WRITE(6,*) '  OSOAA_MAIN : ERROR_2610 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -HYD.DirMie Value   '
      WRITE(6,*) '  ==> Storage directory for MIE files producted by'
      WRITE(6,*) '      OSOAA_HYDROSOLS computations (complete path) '
      GOTO 9998  
             
 2620 WRITE(6,*) '  OSOAA_MAIN : ERROR_2620 on parameters'
      WRITE(6,*) '  --> Required (-Keyword Value) definition : ',
     &           '     -HYD.Model Value (format : Integer)'
      WRITE(6,*) '  ==> Type of hydrosol characterization : '
      WRITE(6,*) '         1 : From size distribution models.  '
      WRITE(6,*) '         2 : Phaze functions from an external source'
      GOTO 9998         

 2621 WRITE(6,*) '  OSOAA_MAIN : ERROR_2621 on parameters'
      WRITE(6,*) '  -->  -HYD.Model value : out of available range.'
      WRITE(6,*) '  ==> Type of hydrosol characterization : '
      WRITE(6,*) '         1 : From size distribution models.  '
      WRITE(6,*) '         2 : Phaze functions from an external source'
      GOTO 9998  

 2630 WRITE(6,*) '  OSOAA_MAIN : ERROR_2630 on parameters'
      WRITE(6,*) '  -->  Junge model for phytoplankton is required.'
      WRITE(6,*) '        Required (-Keyword Value) definition : '
      WRITE(6,*) '          -PHYTO.JD.slope (Float): Slope.'
      WRITE(6,*) '          -PHYTO.JD.MRwa (Float): '
      WRITE(6,*) '               real part of refractive index.'
      WRITE(6,*) '          -PHYTO.JD.MIwa (Float): '
      WRITE(6,*) '               imaginary part of refractive index.'
      WRITE(6,*) '          -PHYTO.JD.rate (Float): Ratio of this mode'
      WRITE(6,*) '               for phytoplankton particles.'
      GOTO 9998  

 2632 WRITE(6,*) '  OSOAA_MAIN : ERROR_2632 on parameters'
      WRITE(6,*) '  -->  LND secondary model for phytoplankton'
      WRITE(6,*) '       if used all the associated parameters '
      WRITE(6,*) '       have to be defined Required (-Keyword Value) :'
      WRITE(6,*) '          -PHYTO.LND.SM.SDradius (Float): '
      WRITE(6,*) '               modal radius (mic.).'
      WRITE(6,*) '          -PHYTO.LND.SM.SDvar  (Float): '
      WRITE(6,*) '               standard deviation of the LND.'
      WRITE(6,*) '          -PHYTO.LND.SM.MRwa (Float): '
      WRITE(6,*) '               real part of refractive index.'
      WRITE(6,*) '          -PHYTO.LND.SM.MIwa (Float): '
      WRITE(6,*) '               imaginary part of refractive index.'
      WRITE(6,*) '          -PHYTO.LND.SM.rate (Float): Ratio of this'
      WRITE(6,*) '               mode for phytoplankton particles.'
      GOTO 9998  

 2634 WRITE(6,*) '  OSOAA_MAIN : ERROR_2634 on parameters'
      WRITE(6,*) '  -->  LND tertiary model for phytoplankton'
      WRITE(6,*) '       if used all the associated parameters '
      WRITE(6,*) '       have to be defined Required (-Keyword Value) :'
      WRITE(6,*) '          -PHYTO.LND.TM.SDradius (Float): '
      WRITE(6,*) '               modal radius (mic.).'
      WRITE(6,*) '          -PHYTO.LND.TM.SDvar  (Float): '
      WRITE(6,*) '               standard deviation of the LND.'
      WRITE(6,*) '          -PHYTO.LND.TM.MRwa (Float): '
      WRITE(6,*) '               real part of refractive index.'
      WRITE(6,*) '          -PHYTO.LND.TM.MIwa (Float): '
      WRITE(6,*) '               imaginary part of refractive index.'
      WRITE(6,*) '          -PHYTO.LND.TM.rate (Float): Ratio of this '
      WRITE(6,*) '               mode for phytoplankton particles.'
      GOTO 9998  

 2640 WRITE(6,*) '  OSOAA_MAIN : ERROR_2640 on parameters'
      WRITE(6,*) '  -->  Junge model for Mineral-Like Particles'
      WRITE(6,*) '       is required (-Keyword Value)  : '
      WRITE(6,*) '          -SED.JD.slope (Float): Slope.'
      WRITE(6,*) '          -SED.JD.MRwa (Float): '
      WRITE(6,*) '               real part of refractive index.'
      WRITE(6,*) '          -SED.JD.MIwa (Float): '
      WRITE(6,*) '               imaginary part of refractive index.'
      WRITE(6,*) '          -SED.JD.rate (Float): Ratio of this mode'
      WRITE(6,*) '               for Mineral-Like Particles.'
      GOTO 9998  	           

 2642 WRITE(6,*) '  OSOAA_MAIN : ERROR_2642 on parameters'
      WRITE(6,*) '  -->  LND secondary model for Mineral-Like Particles'
      WRITE(6,*) '       if used all the associated parameters '
      WRITE(6,*) '       have to be defined Required (-Keyword Value) :'
      WRITE(6,*) '          -SED.LND.SM.SDradius (Float): '
      WRITE(6,*) '               modal radius (mic.).'
      WRITE(6,*) '          -SED.LND.SM.SDvar  (Float): '
      WRITE(6,*) '               standard deviation of the LND.'
      WRITE(6,*) '          -SED.LND.SM.MRwa (Float): '
      WRITE(6,*) '               real part of refractive index.'
      WRITE(6,*) '          -SED.LND.SM.MIwa (Float): '
      WRITE(6,*) '               imaginary part of refractive index.'
      WRITE(6,*) '          -SED.LND.SM.rate (Float): Ratio of this '
      WRITE(6,*) '               mode for Mineral-Like Particles.'
      GOTO 9998  

 2644 WRITE(6,*) '  OSOAA_MAIN : ERROR_2644 on parameters'
      WRITE(6,*) '  -->  LND tertiary model for Mineral-Like Particles'
      WRITE(6,*) '       if used all the associated parameters '
      WRITE(6,*) '       have to be defined Required (-Keyword Value) :'
      WRITE(6,*) '          -SED.LND.TM.SDradius (Float): '
      WRITE(6,*) '               modal radius (mic.).'
      WRITE(6,*) '          -SED.LND.TM.SDvar  (Float): '
      WRITE(6,*) '               standard deviation of the LND.'
      WRITE(6,*) '          -SED.LND.TM.MRwa (Float): '
      WRITE(6,*) '               real part of refractive index.'
      WRITE(6,*) '          -SED.LND.TM.MIwa (Float): '
      WRITE(6,*) '               imaginary part of refractive index.'
      WRITE(6,*) '          -SED.LND.TM.rate (Float): Ratio of this '
      WRITE(6,*) '               mode for Mineral-Like Particles.'
      GOTO 9998  

 2650 WRITE(6,*) '  OSOAA_MAIN : ERROR_2650 on parameters'
      WRITE(6,*) '               on file GRANU_MLP opening'
      GOTO 9998   	
        
 2660 WRITE(6,*) '  OSOAA_MAIN : ERROR_2660 on parameters'
      WRITE(6,*) '               on file GRANU_MLP reading'
      GOTO 9998 
 
 2670 WRITE(6,*) '  OSOAA_MAIN : ERROR_2670'
      WRITE(6,*) 
     &     'Error while opening the sea bottom reflectance datafile',
     &     'CTE_FIC_BOT_SPECTRAL_DATA in fic/ directory:',
     &     CTE_FIC_BOT_SPECTRAL_DATA
      GOTO 9998 

 2671 WRITE(6,*) '  OSOAA_MAIN : ERROR_2671 on file reading:'
      WRITE(6,*) '  --> sea bottom spectral reflectance :',FIC_BOT_DATA 
      GOTO 9998
           
 2700 WRITE(6,*) '  OSOAA_MAIN : ERROR_2700 on parameters'
      WRITE(6,*) '  --> External phaze functions file -HYD.ExtData'
      WRITE(6,*) '      requires (-Keyword Value)  '
      GOTO 9998   		          

 3500 WRITE(6,*) '  OSOAA_MAIN : ERROR_3500: '
      WRITE(6,*) '  => Error while opening ListParam.txt'
      GOTO 9998

 3501 WRITE(6,*) '  OSOAA_MAIN : ERROR_3501: '
      WRITE(6,*) '  => Error while writing ListParam.txt'
      GOTO 9998

35011 WRITE(6,*) '  OSOAA_MAIN : ERROR_35011: '
      WRITE(6,*) '  => Error while opening the file out_diff'
      GOTO 9998

 3502 WRITE(6,*) '  OSOAA_MAIN : ERROR_3502: '
      WRITE(6,*) '  => Error while opening copy_ListParam.txt'
      GOTO 9998 

 3503 WRITE(6,*) '  OSOAA_MAIN : ERROR_3503: '
      WRITE(6,*) '  => Error while reading copy_ListParam.txt'
      GOTO 9998 

 3504 WRITE(6,*) '  OSOAA_MAIN : ERROR_3504: '
      WRITE(6,*) '  => Error in OSOAA_COMPA_SIMU_FILENAME'
      GOTO 9998 

 3505 WRITE(6,*) '  OSOAA_MAIN : ERROR_3505: '
      WRITE(6,*) '  => Error in OSOAA_COMPA_SIMU_VALUE'
      GOTO 9998 

 3506 WRITE(6,*) '  OSOAA_MAIN : ERROR_3506: '
      WRITE(6,*) '      on file deleting temporary file : '
      WRITE(6,*) '      copy_ListParam.txt'
      GOTO 9998  
      
 4000 WRITE(6,*) '  OSOAA_MAIN : ERROR_4000: '
      WRITE(6,*) '  => Error while getting OSOAA_ROOT variable'
      GOTO 9998

 4001 WRITE(6,*) '  OSOAA_MAIN : ERROR_4001: '
      WRITE(6,*) '  => Length of $OSOAA_ROOT/exe is too long' 
      WRITE(6,*) '  => CTE_LENDIR has to be adjusted.' 
      GOTO 9998

 4002 WRITE(6,*) '  OSOAA_MAIN : ERROR_4002: '
      WRITE(6,*) '  => Length of $OSOAA_ROOT/fic is too long' 
      WRITE(6,*) '  => CTE_LENDIR has to be adjusted.' 
      GOTO 9998

 4003 WRITE(6,*) '  OSOAA_MAIN : ERROR_4003: '
      WRITE(6,*) '  => Simulation wavelength out of the range of' 
      WRITE(6,*) '     wavelengths for sea bottom reflectance data.'
      WRITE(6,*) '       Simulation wavelength (nm): ', WA_SIMU_NM     
      WRITE(6,*) '       Min wavelength for spectral data (nm): ', 
     &           TAB_WA(1)   
      WRITE(6,*) '       Max wavelength for spectral data (nm): ', 
     &           TAB_WA(CTE_NB_WA_FIC_BOT)
      GOTO 9998

 4004 WRITE(6,*) '  OSOAA_MAIN : ERROR_4004 :  '
      WRITE(6,*) '      on file deleting temporary file : out_diff'
      GOTO 9998  
                  	               	
5000  WRITE(6,*) '  OSOAA_MAIN : ERROR_5000 on subroutine OSOAA_ANGLES'
      GOTO 9998 

6000  WRITE(6,*) 
     &  '  OSOAA_MAIN : ERROR_6000 on subroutine OSOAA_AEROSOLS'
      GOTO 9998 

6500  WRITE(6,*) 
     &  '  OSOAA_MAIN : ERROR_6500 on subroutine OSOAA_HYDROSOLS'
      GOTO 9998
      
7000  WRITE(6,*) '  OSOAA_MAIN : ERROR_7000 on subroutine OSOAA_PROFILE'
      GOTO 9998 

8000  WRITE(6,*) '  OSOAA_MAIN : ERROR_8000 on subroutine OSOAA_SURFACE'
      GOTO 9998
            
9000  WRITE(6,*) '  OSOAA_MAIN : ERROR_9000 on subroutine OSOAA_SOS'
      GOTO 9998 
                    
 9998 IF (TRACE) THEN
          WRITE (IDLOG,*,ERR=910) " "
          WRITE (IDLOG,*,ERR=910) "--> Error on processes"
	  WRITE (IDLOG,*,ERR=910) "JOB_STATUS=ERROR"
	  CLOSE(IDLOG)
      ENDIF 
 
      CALL EXIT(1)    
 9999 STOP



C =========================================================
C --- Formats
C =========================================================            
C
C --- Formats pour le fichier Trace : entrées de OSOAA_ANGLES
  800 FORMAT(28h       NBMU_GAUSS_LUM :     ,I4)
  801 FORMAT(28h       TETAS :              ,F9.4)
  802 FORMAT(28h       TETASW :             ,F9.4)
  803 FORMAT(28h       FICANGLES_USER_LUM : ,A)
  804 FORMAT(28h       NBMU_GAUSS_MIE :     ,I4)
  805 FORMAT(28h       FICANGLES_USER_MIE : ,A)
  806 FORMAT(28h       FICANGLES_LOG :      ,A)
  807 FORMAT(28h       FICANGLES_RES_LUM :  ,A)
  808 FORMAT(28h       FICANGLES_RES_MIE :  ,A)      

C --- Formats pour le fichier Trace : entrées de OSOAA_PROFILE
  809 FORMAT(28h       WA_SIMU        :     ,F8.5)
  810 FORMAT(28h       SEA_DEPTH      :     ,F10.3) 
  811 FORMAT(28h       CHL            :     ,F10.3) 
  812 FORMAT(28h       IPHYTO_PROFIL  :     ,I4)
  813 FORMAT(28h       CHL_GP_BG      :     ,F10.3) 
  814 FORMAT(28h       CHL_GP_DEEP    :     ,F10.3) 
  815 FORMAT(28h       CHL_GP_WIDTH   :     ,F10.3) 
  816 FORMAT(28h       FICPHYTO_USER  :     ,A)   
  817 FORMAT(28h       SED_CSED       :     ,F10.3)   
  818 FORMAT(28h       SED_MRWA       :     ,F10.4)   
  819 FORMAT(28h       SED_VSED       :     ,F10.3)   
  820 FORMAT(28h       SED_KMAT2      :     ,F10.5)   
  821 FORMAT(28h       SED_PIZ        :     ,F10.5)
  822 FORMAT(28h       YS_A440        :     ,F10.5)
  823 FORMAT(28h       YS_SWA         :     ,F10.5)
  824 FORMAT(28h       DET_A440       :     ,F10.5)
  825 FORMAT(28h       DET_SWA        :     ,F10.5) 
  826 FORMAT(28h       FICPROFIL_LOG  :     ,A) 
  827 FORMAT(32h       FICPROFIL_ATM_RES  :     ,A)
  828 FORMAT(32h       FICPROFIL_SEA_RES  :     ,A)  
	    
C --- Formats pour le fichier Trace : entrées de OSOAA_SURFACE	  
  830 FORMAT(32h       SEA_IND            :     ,F8.5)
  831 FORMAT(32h       WIND               :     ,F8.5)
  832 FORMAT(32h       FICANGLES_RES_LUM  :     ,A) 
  833 FORMAT(32h       DIRSEA             :     ,A)  
  834 FORMAT(32h       FICSURF_LOG        :     ,A)   	 

  730 FORMAT(32h       Optical thickness  :     ,F10.5)
  731 FORMAT(32h       High scale         :     ,F10.5)
  732 FORMAT(32h       Single sca alb     :     ,F10.5)
  733 FORMAT(32h       Atmos pressure     :     ,F10.5)
  	 
C --- Formats pour le fichier Trace : entrées de OSOAA_AEROSOLS
  841 FORMAT(45h       Simulation wavelength (Wa)          : ,F12.5)
  842 FORMAT(45h       AOT                                 : ,F12.5) 
  843 FORMAT(45h       Reference wavelength (Waref)        : ,F12.5) 
  844 FORMAT(45h       Reference AOT                       : ,F12.5) 
  845 FORMAT(45h       Truncature option                   : ,I4) 
  846 FORMAT(45h       Type of aerosols model              : ,I4)
  847 FORMAT(45h       MMD : Refractive index for Wa       : ,
     &      (F12.3,2X,F12.5))
  848 FORMAT(45h       MMD : Refractive index for Waref    : ,
     &      (F12.3,2X,F12.5))  
  849 FORMAT(45h       Type of size distribution           : ,I4)
  850 FORMAT(45h       Size distribution parameters        : ,
     &      (F12.5,2X,F12.5,2X,F12.5))    
  851 FORMAT(45h       Type of WMO model                   : ,I4)  
  852 FORMAT(45h       WMO component conc. (DL, WS, OC, SO): ,
     &      4(F12.5,2X))    
  853 FORMAT(45h       Type of Shettle & Fenn model        : ,I4)    
  854 FORMAT(45h       S&F model : relative humidity       : ,F12.5)
  855 FORMAT(45h       BMD : Choice of mixture description : ,I4) 
  856 FORMAT(45h       BMD : user conc. Coarse / Fine      : ,
     &      (F12.5,2X,F12.5))
  857 FORMAT(45h       BMD : AOT coarse / AOTtot for Waref : ,F12.5)   
  858 FORMAT(45h         Refractive index for Wa           : ,
     &      (F12.3,2X,F12.5))      
  859 FORMAT(45h         Refractive index for Waref        : ,
     &      (F12.3,2X,F12.5))      
  860 FORMAT(45h         Size distr parameters             : ,
     &      (F12.5,2X,F12.5))  
  861 FORMAT(45h       User phaze function file            : ,A)  
  862 FORMAT(45h       Log file                            : ,A)  
  863 FORMAT(45h       Res file                            : ,A) 
  864 FORMAT(45h       Storage directory for MIE files     : ,A)  
  865 FORMAT(45h       Mie computations log file           : ,A)    

C --- Formats pour le fichier Trace : entrées de OSOAA_HYDROSOLS
  870 FORMAT(31h       FICHYD_LOG        :     ,A) 
  871 FORMAT(31h       IMOD_HYD          :     ,I4)   
  872 FORMAT(31h       Slope Rmin Rmax   :     ,3(F12.5,2X))   
  873 FORMAT(31h       MR MI             :     ,2(F12.5,2X))   
  874 FORMAT(31h       RATE              :     ,F12.5)   
  875 FORMAT(31h       Mean Radius, Var  :     ,2(F12.5,2X))  
  876 FORMAT(31h       User data file    :     ,A) 
  877 FORMAT(31h       Res file Phyto    :     ,A) 
  878 FORMAT(31h       Res file MLP      :     ,A) 
  879 FORMAT(31h       Concentration     :     ,F12.5)

C --- Formats pour le fichier Trace : entrées de OSOAA_SOS
  880 FORMAT(32h       FICGRANU_AER       :     ,A) 
  881 FORMAT(32h       FICGRANU_PHYTO     :     ,A)   
  882 FORMAT(32h       FICGRANU_MLP       :     ,A)   
  883 FORMAT(32h       FICPROFIL_ATM_RES  :     ,A)   
  884 FORMAT(32h       FICPROFIL_SEA_RES  :     ,A)        
  885 FORMAT(50h       Index for surface matrix use IMAT_SURF :   ,I4)
  886 FORMAT(40h       Sea surface albedo RHO_SURF  :   ,F8.4)         
  887 FORMAT(40h       Sea bottom  albedo RHO_BOT   :   ,F8.4)       
  888 FORMAT(32h       IGMAX              :     ,I4) 
  889 FORMAT(32h       FICSOS_RES_BIN     :     ,A)         
  890 FORMAT(32h       FICSOS_RES_ADV_UP  :     ,A)
  891 FORMAT(32h       FICSOS_RES_ADV_DOWN:     ,A)            
  892 FORMAT(32h       Relative azimuth PHI:    ,F8.3)     
  893 FORMAT(32h       LEVEL_OUT          :     ,I4)        
  894 FORMAT(32h       USER_Z_OUT         :     ,F10.4)    
  895 FORMAT(40h       Output file FICSOS_RES_VS_VZA:   ,A)        
  896 FORMAT(32h       Angle VZA_OUT      :     ,F8.3)       
  897 FORMAT(40h       Output file FICSOS_RES_VS_Z  :   ,A)
 8971 FORMAT(40h       Output file FIC_FLUX         :   ,A)
  898 FORMAT(40h       FICSOS_LOG                   :   ,A)
  899 FORMAT(40h       CALL OF SOS_CORE             :   ,A)
 

C --- Formats pour le fichier ListParam.txt
 1800 FORMAT(30h -OSOAA.ResRoot         :     ,A) 

 1801 FORMAT(30h -OSOAA.Log             :     ,A)
 1802 FORMAT(30h -OSOAA.Wa              :     ,F8.5)
 1803 FORMAT(30h -ANG.Rad.NbGauss       :     ,I4)
 1804 FORMAT(30h -ANG.Rad.UserAngFile   :     ,A)
 1805 FORMAT(30h -ANG.Thetas            :     ,F9.4)
 1806 FORMAT(30h -ANG.Rad.ResFile       :     ,A)
 1807 FORMAT(30h -ANG.Mie.NbGauss       :     ,I4)
 1808 FORMAT(30h -ANG.Mie.UserAngFile   :     ,A)
 1809 FORMAT(30h -ANG.Mie.ResFile       :     ,A)
 1810 FORMAT(30h -ANG.Log               :     ,A)
 1811 FORMAT(30h -AER.Waref             :     ,F8.5)
 1812 FORMAT(30h -AER.AOTref            :     ,F9.5)
 1813 FORMAT(30h -AER.Tronca            :     ,I4)
 1814 FORMAT(30h -AER.Model             :     ,I4)
 1815 FORMAT(30h -AER.MMD.MRwa          :     ,F12.3)
 1816 FORMAT(30h -AER.MMD.MIwa          :     ,F12.5)
 1817 FORMAT(30h -AER.MMD.MRwaref       :     ,F12.3)
 1818 FORMAT(30h -AER.MMD.MIwaref       :     ,F12.5)
 1819 FORMAT(30h -AER.MMD.SDtype        :     ,I4)
 1820 FORMAT(30h -AER.MMD.LNDradius     :     ,F12.5)
 1821 FORMAT(30h -AER.MMD.LNDvar        :     ,F12.5)
 1822 FORMAT(30h -AER.MMD.JD.slope      :     ,F12.5)
 1823 FORMAT(30h -AER.MMD.JD.rmin       :     ,F12.5)
 1824 FORMAT(30h -AER.MMD.JD.rmax       :     ,F12.5)
 1825 FORMAT(30h -AER.WMO.Model         :     ,I4)
 1826 FORMAT(30h -AER.WMO.DL            :     ,F12.5)
 1827 FORMAT(30h -AER.WMO.WS            :     ,F12.5)
 1828 FORMAT(30h -AER.WMO.OC            :     ,F12.5)
 1829 FORMAT(30h -AER.WMO.SO            :     ,F12.5)
 1830 FORMAT(30h -AER.SF.Model          :     ,I4)
 1831 FORMAT(30h -AER.SF.RH             :     ,F12.5)
 1832 FORMAT(30h -AER.BMD.VCdef         :     ,I4)
 1833 FORMAT(30h -AER.BMD.CoarseVC      :     ,F12.5)
 1834 FORMAT(30h -AER.BMD.FineVC        :     ,F12.5)
 1835 FORMAT(30h -AER.BMD.RAOT          :     ,F12.5)
 1836 FORMAT(30h -AER.BMD.CM.MRwa       :     ,F12.3)
 1837 FORMAT(30h -AER.BMD.CM.MIwa       :     ,F12.5)
 1838 FORMAT(30h -AER.BMD.CM.MRwaref    :     ,F12.3)
 1839 FORMAT(30h -AER.BMD.CM.MIwaref    :     ,F12.5)
 1840 FORMAT(30h -AER.BMD.CM.SDradius   :     ,F12.5)
 1841 FORMAT(30h -AER.BMD.CM.SDvar      :     ,F12.5)
 1842 FORMAT(30h -AER.BMD.FM.MRwa       :     ,F12.3)
 1843 FORMAT(30h -AER.BMD.FM.MIwa       :     ,F12.5)
 1844 FORMAT(30h -AER.BMD.FM.MRwaref    :     ,F12.3)
 1845 FORMAT(30h -AER.BMD.FM.MIwaref    :     ,F12.5)
 1846 FORMAT(30h -AER.BMD.FM.SDradius   :     ,F12.5)
 1847 FORMAT(30h -AER.BMD.FM.SDvar      :     ,F12.5)
 1848 FORMAT(30h -AER.ExtData           :     ,A)
 1849 FORMAT(30h -AER.MieLog            :     ,A)
 1850 FORMAT(30h -AER.DirMie            :     ,A)
 1851 FORMAT(30h -AER.ResFile           :     ,A)
 1852 FORMAT(30h -AER.Log               :     ,A)
 1853 FORMAT(30h -PHYTO.ResFile         :     ,A)
 1854 FORMAT(30h -MLP.ResFile           :     ,A)
 1855 FORMAT(30h -HYD.Log               :     ,A)
 1856 FORMAT(30h -HYD.DirMie            :     ,A)
 1857 FORMAT(30h -HYD.MieLog            :     ,A)
 1858 FORMAT(30h -HYD.Model             :     ,I4)
 1859 FORMAT(30h -PHYTO.JD.slope        :     ,F12.5)
 1860 FORMAT(30h -PHYTO.JD.rmin         :     ,F12.5)
 1861 FORMAT(30h -PHYTO.JD.rmax         :     ,F12.5)
 1862 FORMAT(30h -PHYTO.JD.MRwa         :     ,F12.3)
 1863 FORMAT(30h -PHYTO.JD.MIwa         :     ,F12.5)
 1864 FORMAT(30h -PHYTO.JD.rate         :     ,F12.5)
 1865 FORMAT(30h -PHYTO.LND.SM.SDradius :     ,F12.5)
 1866 FORMAT(30h -PHYTO.LND.SM.SDvar    :     ,F12.5)
 1867 FORMAT(30h -PHYTO.LND.SM.MRwa     :     ,F12.3)
 1868 FORMAT(30h -PHYTO.LND.SM.MIwa     :     ,F12.5)
 1869 FORMAT(30h -PHYTO.LND.SM.rate     :     ,F12.5)
 1870 FORMAT(30h -PHYTO.LND.TM.SDradius :     ,F12.5)
 1871 FORMAT(30h -PHYTO.LND.TM.SDvar    :     ,F12.5)
 1872 FORMAT(30h -PHYTO.LND.TM.MRwa     :     ,F12.3)
 1873 FORMAT(30h -PHYTO.LND.TM.MIwa     :     ,F12.5)
 1874 FORMAT(30h -PHYTO.LND.TM.rate     :     ,F12.5)
 1875 FORMAT(30h -SED.JD.slope          :     ,F12.5)
 1876 FORMAT(30h -SED.JD.rmin           :     ,F12.5)
 1877 FORMAT(30h -SED.JD.rmax           :     ,F12.5)
 1878 FORMAT(30h -SED.JD.MRwa           :     ,F12.3)
 1879 FORMAT(30h -SED.JD.MIwa           :     ,F12.5)
 1880 FORMAT(30h -SED.JD.rate           :     ,F12.5)
 1881 FORMAT(30h -SED.LND.SM.SDradius   :     ,F12.5)
 1882 FORMAT(30h -SED.LND.SM.SDvar      :     ,F12.5)
 1883 FORMAT(30h -SED.LND.SM.MRwa       :     ,F12.3)
 1884 FORMAT(30h -SED.LND.SM.MIwa       :     ,F12.5)
 1885 FORMAT(30h -SED.LND.SM.rate       :     ,F12.5)
 1886 FORMAT(30h -SED.LND.TM.SDradius   :     ,F12.5)
 1887 FORMAT(30h -SED.LND.TM.SDvar      :     ,F12.5)
 1888 FORMAT(30h -SED.LND.TM.MRwa       :     ,F12.3)
 1889 FORMAT(30h -SED.LND.TM.MIwa       :     ,F12.5)
 1890 FORMAT(30h -SED.LND.TM.rate       :     ,F12.5)
 1891 FORMAT(30h -HYD.ExtData           :     ,A)
 1892 FORMAT(30h -AP.MOT                :     ,F12.5)
 1893 FORMAT(30h -AP.Pressure           :     ,F12.5)
 1894 FORMAT(30h -AP.HR                 :     ,F12.5)
 1895 FORMAT(30h -AP.HA                 :     ,F12.5)
 1896 FORMAT(30h -SEA.Depth             :     ,F12.5)
 1897 FORMAT(30h -PHYTO.Chl             :     ,F12.5)
 1898 FORMAT(30h -PHYTO.ProfilType      :     ,I4)
 1899 FORMAT(30h -PHYTO.GP.Chlbg        :     ,F12.5)
 1900 FORMAT(30h -PHYTO.GP.Deep         :     ,F12.5)
 1901 FORMAT(30h -PHYTO.GP.Width        :     ,F12.5)
 1902 FORMAT(30h -PHYTO.Userfile        :     ,A)
 1903 FORMAT(30h -SED.Csed              :     ,F12.5)
 1904 FORMAT(30h -YS.Abs440             :     ,F12.5)
 1905 FORMAT(30h -YS.Swa                :     ,F12.5)
 1906 FORMAT(30h -DET.Abs440            :     ,F12.5)
 1907 FORMAT(30h -DET.Swa               :     ,F12.5)
 1908 FORMAT(30h -PROFILE_ATM.ResFile   :     ,A)
 1909 FORMAT(30h -PROFILE_SEA.ResFile   :     ,A)
 1910 FORMAT(30h -PROFILE.Log 	        :     ,A)
 1911 FORMAT(30h -SEA.Ind               :     ,F8.5)
 1912 FORMAT(30h -SEA.Wind              :     ,F8.5)
 1913 FORMAT(30h -SEA.Dir               :     ,A)
 1914 FORMAT(30h -SEA.Log               :     ,A)
 1915 FORMAT(30h -SEA.SurfAlb           :     ,F8.5)
 1916 FORMAT(30h -SEA.BotType           :     ,I4)
C --- MOD: 16/05/2018 : Changement du format d'écriture de SEA.BotAlb de F8.5 -> F12.5
 1917 FORMAT(30h -SEA.BotAlb            :     ,F12.5) 
C --- FIN MOD 
 1918 FORMAT(30h -OSOAA.View.Phi        :     ,F12.5)
 1919 FORMAT(30h -OSOAA.View.Level      :     ,I4)
 1920 FORMAT(30h -OSOAA.View.Z          :     ,F12.5)
 1921 FORMAT(30h -OSOAA.ResFile.vsVZA   :     ,A)
 1922 FORMAT(30h -OSOAA.View.VZA        :     ,F12.5)
 1923 FORMAT(30h -OSOAA.ResFile.vsZ     :     ,A)
 1924 FORMAT(30h -OSOAA.ResFile.Adv.Up  :     ,A)
 1925 FORMAT(30h -OSOAA.ResFile.Adv.Down:     ,A)
 1926 FORMAT(30h -SOS.Log               :     ,A)
 1927 FORMAT(30h -SOS.IGmax             :     ,I4)
 1928 FORMAT(30h -SOS.ResFile.Bin       :     ,A)

  
      END          !FIN DU PROGRAMME OSOAA_MAIN  





C==============================================================================
C PROCEDURE: OSOAA_COMPA_SIMU_FILENAME
C ==========
C
C      Cette procédure compare les noms de deux fichiers.
C      Le nom du fichier de référence est donné par la variable NOM_FIC
C      qui correspond à l'argument du programme NOM_PARAM.
C      Le nom à comparer est extrait du fichier "copy_ListParam.txt"
C      qui liste les paramètres d'un traitement antérieur sur le même espace
C      de travail ==> Suppose le fichier ouvert en lecture (FIC_UNIT).
C
C      Si les noms sont différents, l'indicateur de cas de reprise d'un
C      traitement antérieur (CAS_REPRISE) est positionné à FALSE.
C      On ne change pas la valeur de CAS_REPRISE sinon.
C
C      La routine gère le passage d'informations au fichier de Trace.
C
C
C Description des paramètres entrée / sortie
C ------------------------------------------
C	NOM_FIC   (CHARACTER*CTE_LENFIC2)  : (E) Nom du fichier (chemin complet)
C	NOM_PARAM (CHARACTER*CTE_LENFIC2)  : (E) Nom de l'argument d'entrée du paramètre
C       FIC_UNIT  (I4) 			   : (E) Numéro du fichier
C       TRACE     (Logical) 		   : (E) Indicateur de fichier Trace
C       CAS_REPRISE 		           : (S) Indicateur d'un cas de reprise de traitement
C                                                avec les mêmes paramètres (TRUE si c'est le cas)
C       IER (I4)                 	   : (S) Indice d'erreur : 0 si pas d'erreur, -1 sinon.
C 
C Constantes utilisées :
C --------------------
C    CTE_LENFIC2 : Longueur des noms de fichiers avec arborescence.
C    INCTE_IDLOG_MAIN : ID du ficher Trace principal
C
C Cas d'erreur :
C ------------    
C     -Lecture du fichier FIC_UNIT
C     -Ecriture dans le fichier Trace IDLOG
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant
C
C==============================================================================

      SUBROUTINE OSOAA_COMPA_SIMU_FILENAME(NOM_FIC,NOM_PARAM,FIC_UNIT,
     &                                     TRACE,CAS_REPRISE,IER)
      
      IMPLICIT NONE 

C Constantes 
C -----------------            
      INTEGER*2 IDLOG			! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_MAIN)


      CHARACTER*INCTE_LENLINE LIGNE_TEXTE   !Ligne de caractères pour décodage de paramètres
      					    !d'un fichier produit

      CHARACTER*CTE_LENFIC2  PREVIOUS_SIMU_FILE	! Nom d'un fichier en entrée d'un traitement antérieur
      CHARACTER*CTE_LENFIC2  NOM_FIC		! (E) Nom du fichier en entrée du traitement courant
      CHARACTER*CTE_LENFIC2  NOM_PARAM          ! (E) Nom du parametre dans le script de lancement       

 
      LOGICAL CAS_REPRISE	! (S) Indicateur si le paramètre n'est pas le même (FALSE si c'est le cas)
      LOGICAL TRACE     	! (E) = vrai si écriture dans le fichier trace
 

      INTEGER*4 FIC_UNIT	! (E) Numéro d'unité du fichier lu
      INTEGER*4 IER     	! (S) Code d'erreur =0 si pas d'erreur, =-1 sinon
      INTEGER*4 IFIN 	        ! Nb de caractères significatifs d'une chaine  
      INTEGER*4 IFIN2,IFIN3     ! Nb de caractères significatifs d'une chaine  
      INTEGER*4 I,J

      CHARACTER*CTE_LENFIC2  NOM_FIC_TMP ! Nom du fichier en entrée du traitement courant, sans "blancs


      
C* Initialisation                     
C*-----------------------------------------------------------------
      IER=0

C* Lecture du nom du fichier dans "copy_ListParam.txt" (FIC_UNIT)                     
C*-----------------------------------------------------------------
      READ(FIC_UNIT,'(a)',ERR=90) LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':     ')
      PREVIOUS_SIMU_FILE=LIGNE_TEXTE(IFIN+6:INCTE_LENLINE)
	
	
C* Comparaison au fichier du fichier pour les paramètres de simulation
C* du traitement en cours                    
C*-----------------------------------------------------------------      
      IF(NOM_FIC.NE.PREVIOUS_SIMU_FILE) THEN
         CAS_REPRISE=.FALSE.
	      	 
	 IF (TRACE) THEN
	 	
	    J=0
	    DO I=1,CTE_LENFIC2    
	      IF (NOM_FIC(I:I).NE.' ') THEN
	         J=J+1
	         NOM_FIC_TMP(J:J)=NOM_FIC(I:I)
	      ENDIF   
	    ENDDO
	    IFIN2=J
		
	    J=0
	    DO I=1,CTE_LENFIC2    
	      IF (PREVIOUS_SIMU_FILE(I:I).NE.' ') THEN
	         J=J+1
	      ENDIF   
	    ENDDO
	    IFIN3=J
		
	    IFIN=INDEX(NOM_PARAM,':') - 1  !Le nom de l'argument passé à la routine doit être suivi de :
	   
	    WRITE (IDLOG,*,ERR=100) 
     &	       "Difference for parameter ", NOM_PARAM(1:IFIN) ," : ",
     &         NOM_FIC_TMP(1:IFIN2)," / ", PREVIOUS_SIMU_FILE(1:IFIN3)
	    WRITE (IDLOG,*,ERR=100) " "
	 ENDIF
	 
      ENDIF   
      

C* Fin nominale 
C-------------------
       GOTO 9999
              
C* Cas d'erreur et retour du status -1 au programme appelant
C----------------------------------------------------------
   90 WRITE(6,*) '  OSOAA_MAIN : ERROR_90 in OSOAA_COMPA_SIMU_FILE'
      WRITE(6,*) 
     &'      --> Error while reading the file copy_ListParam.txt'
      IER=-1
      GOTO 9999
 
 
       
  100 WRITE(6,*) '  OSOAA_MAIN : ERROR_100 in OSOAA_COMPA_SIMU_FILE'
      WRITE(6,*) '      --> Error while writing the main log file'
      IER=-1
      GOTO 9999
     
 9999 CONTINUE     
    
      END ! FIN DE LA ROUTINE OSOAA_COMPA_SIMU_FILENAME





C==============================================================================
C PROCEDURE: OSOAA_COMPA_SIMU_VALUE
C ==========
C
C      Cette procédure compare les valeurs de deux paramètres de traitement.
C      La valeur de référence est donnée par la variable VALUE
C      qui correspond à l'argument du programme NOM_PARAM.
C      La valeur à comparer est extraite du fichier "copy_ListParam.txt"
C      qui liste les paramètres d'un traitement antérieur sur le même espace
C      de travail ==> Suppose le fichier ouvert en lecture (FIC_UNIT).
C
C      Si les valeurs sont différentes, l'indicateur de cas de reprise d'un
C      traitement antérieur (CAS_REPRISE) est positionné à FALSE.
C      On ne change pas la valeur de CAS_REPRISE sinon.
C
C      La routine gère le passage d'informations au fichier de Trace.
C
C
C Description des paramètres entrée / sortie
C ------------------------------------------
C	VALUE   (DBLE)  		   : (E) Valeur du paramètre pour le traitement en cours
C       VALUE_TYPE  		   	   : (E) Type vrai de la valeur VALUE (INT ou DBL)
C	NOM_PARAM (CHARACTER*CTE_LENFIC2)  : (E) Nom de l'argument d'entrée du paramètre
C       FIC_UNIT  (I4) 			   : (E) Numéro du fichier
C       TRACE     (Logical) 		   : (E) Indicateur de fichier Trace
C       CAS_REPRISE 		           : (S) Indicateur d'un cas de reprise de traitement
C                                                avec les mêmes paramètres (TRUE si c'est le cas)
C       IER (I4)                 	   : (S) Indice d'erreur : 0 si pas d'erreur, -1 sinon.
C 
C Constantes utilisées :
C --------------------
C    CTE_LENFIC2 : Longueur des noms de fichiers avec arborescence.
C    INCTE_IDLOG_MAIN : ID du ficher Trace principal
C
C Cas d'erreur :
C ------------    
C     -Lecture du fichier FIC_UNIT
C     -Ecriture dans le fichier Trace IDLOG
C
C     Affichage d'un message à l'écran, la routine interrompt ses calculs et 
C     retour du status -1 au programme appelant
C
C==============================================================================

      SUBROUTINE OSOAA_COMPA_SIMU_VALUE(VALUE,VALUE_TYPE,NOM_PARAM,
     &                                  FIC_UNIT,TRACE,CAS_REPRISE,IER)
      
      IMPLICIT NONE 

C Constantes 
C -----------------            
      INTEGER*2 IDLOG			! Numéro identifiant du fichier Trace	    
      PARAMETER(IDLOG=INCTE_IDLOG_MAIN)

      CHARACTER*3 VALUE_TYPE		! (E) : Type de la valeur VALUE (INT ou DBL)
      
      CHARACTER*INCTE_LENLINE LIGNE_TEXTE   !Ligne de caractères pour décodage de paramètres
      					    !d'un fichier produit

      DOUBLE PRECISION  PREVIOUS_VALUE_DBLE	! Valeur du parametre en entrée d'un traitement antérieur
      DOUBLE PRECISION  VALUE			! (E) Valeur du parametre en entrée du traitement courant
      CHARACTER*CTE_LENFIC2  NOM_PARAM  ! (E) Nom du parametre dans le script de lancement       

 
      LOGICAL CAS_REPRISE	! (S) Indicateur si le paramètre n'est pas le même (FALSE si c'est le cas)
      LOGICAL TRACE     	! (E) = vrai si écriture dans le fichier trace
 

      INTEGER*4 FIC_UNIT	! (E) Numéro d'unité du fichier lu
      INTEGER*4 IFIN 	        ! Nb de caractères significatifs d'une chaine  
      INTEGER*4 IER     	! (S) Code d'erreur =0 si pas d'erreur, =-1 sinon
      
      
C* Initialisation                     
C*-----------------------------------------------------------------
      IER=0

C* Lecture du nom du fichier dans "copy_ListParam.txt" (FIC_UNIT)                     
C*-----------------------------------------------------------------
      READ(FIC_UNIT,'(a)',ERR=90) LIGNE_TEXTE
      IFIN=INDEX(LIGNE_TEXTE,':')
      READ(LIGNE_TEXTE(IFIN+1:INCTE_LENLINE),*) PREVIOUS_VALUE_DBLE	
	
C* Comparaison au fichier du fichier pour les paramètres de simulation
C* du traitement en cours                    
C*-----------------------------------------------------------------      
      IF(VALUE.NE.PREVIOUS_VALUE_DBLE) THEN
         CAS_REPRISE=.FALSE.
	      	 
	 IF (TRACE) THEN
    
	    IFIN=INDEX(NOM_PARAM,':') - 1  !Le nom de l'argument passé à la routine doit être suivi de ":"
	   
	    IF (VALUE_TYPE.EQ.'INT') THEN

	       WRITE (IDLOG,*,ERR=100) 
     &	          "Difference for parameter ", NOM_PARAM(1:IFIN) ," : ",
     &            INT(VALUE)," / ", INT(PREVIOUS_VALUE_DBLE)
	       	       	    
	    ELSE
	    
	       WRITE (IDLOG,*,ERR=100) 
     &	          "Difference for parameter ", NOM_PARAM(1:IFIN) ," : ",
     &            VALUE," / ", PREVIOUS_VALUE_DBLE
	    
	    ENDIF
	    
	    WRITE (IDLOG,*,ERR=100) " "
	    
	 ENDIF
	 
      ENDIF   
      

C* Fin nominale 
C-------------------
       GOTO 9999
              
C* Cas d'erreur et retour du status -1 au programme appelant
C----------------------------------------------------------
   90 WRITE(6,*) '  OSOAA_MAIN : ERROR_90 in OSOAA_COMPA_SIMU_VALUE'
      WRITE(6,*) 
     &'      --> Error while reading the file copy_ListParam.txt'
      IER=-1
      GOTO 9999

  100 WRITE(6,*) '  OSOAA_MAIN : ERROR_100 in OSOAA_COMPA_SIMU_VALUE'
      WRITE(6,*) '      --> Error while writing the main log file'
      IER=-1
      GOTO 9999
     
 9999 CONTINUE     
    
      END ! FIN DE LA ROUTINE OSOAA_COMPA_SIMU_VALUE



