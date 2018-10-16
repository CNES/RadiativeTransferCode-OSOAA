#!/bin/ksh

# Important advertissement :
# Do not keep any "blank" after the character "\" for end of lines.
####################################################
#
# List of all available -Keyword parameter
# (see the end of this script to a complete description of -Keyword parameters)
#
#   --------------------------------------------------------------------------------------------
#   Note: A given simulation requires defining only the useful input parameters. 
#         The useless input parameters should not be defined
#         (i.e, those who are not concerned by the options selected for the given simulation). 
#
#         However, note that if a useless parameter is still defined, it will not be taken into 
#         account by the software during the run.
#
#         Note also that if a useful parameter is missing, the software stops processing the run 
#         and the user is informed about the name of the parameter that should be filled
#   --------------------------------------------------------------------------------------------
#
#
#   Definition of the working folder :
#   ----------------------------------  
#        -OSOAA.ResRoot	
#
#   Angles calculation parameters :
#   --------------------------------  
#        -ANG.Thetas	
#        -ANG.Rad.NbGauss -ANG.Rad.UserAngFile -ANG.Rad.ResFile 
#        -ANG.Mie.NbGauss -ANG.Mie.UserAngFile -ANG.Mie.ResFile 
#        -ANG.Log
#
#   Radiance calculation parameters :
#   --------------------------------
#        -OSOAA.Log  
#        -OSOAA.Wa   
#  
#        -SEA.SurfAlb  -SEA.BotType  -SEA.BotAlb 
#   
#        -OSOAA.View.Phi
#        -OSOAA.View.Level
#        -OSOAA.View.Z    -OSOAA.ResFile.vsVZA
#        -OSOAA.View.VZA  -OSOAA.ResFile.vsZ  
#        -OSOAA.ResFile.Adv.Up   -OSOAA.ResFile.Adv.Down
#
#        -SOS.Log 
#        -SOS.IGmax  
#        -SOS.ResFile.Bin 
#
#   Profile parameters :
#   -------------------
#        -PROFILE.Log 
#
#     Atmospheric Profile parameters
#        -PROFILE_ATM.ResFile  
#        -AP.MOT -AP.Pressure  
#        -AP.HR  -AP.HA	       
#
#     Sea Profile parameters   
#        -PROFILE_SEA.ResFile
#        -SEA.Depth   
#        -PHYTO.Chl  
#        -PHYTO.ProfilType 
#        -PHYTO.GP.Chlbg  -PHYTO.GP.Deep -PHYTO.GP.Width 
#        -PHYTO.Userfile       
#        -SED.Csed             
#        -YS.Abs440  -YS.Swa               
#        -DET.Abs440 -DET.Swa                
#
#   Aerosols parameters :
#   ---------------------
#        -AER.ResFile -AER.Log
#        -AER.DirMie  -AER.MieLog
#        -AER.Waref  -AER.AOTref  
#        -AER.Tronca 
#        -AER.Model
#     Aerosols parameters for mono-modal models :
#        -AER.MMD.MRwa -AER.MMD.MIwa
#        -AER.MMD.MRwaref -AER.MMD.MIwaref
#        -AER.MMD.SDtype 
#        -AER.MMD.LNDradius -AER.MMD.LNDvar
#        -AER.MMD.JD.slope -AER.MMD.JD.rmin -AER.MMD.JD.rmax
#     Aerosols parameters for WMO models :
#        -AER.WMO.Model -AER.WMO.DL -AER.WMO.WS -AER.WMO.OC -AER.WMO.SO
#     Aerosols parameters for Shettle&Fenn models :
#        -AER.SF.Model -AER.SF.RH
#     Aerosols parameters for LND bi-modal models :
#        -AER.BMD.VCdef -AER.BMD.CoarseVC  -AER.BMD.FineVC  -AER.BMD.RAOT  \
#        -AER.BMD.CM.MRwa -AER.BMD.CM.MIwa -AER.BMD.CM.MRwaref -AER.BMD.CM.MIwaref -AER.BMD.CM.SDradius -AER.BMD.CM.SDvar 	\
#        -AER.BMD.FM.MRwa -AER.BMD.FM.MIwa -AER.BMD.FM.MRwaref -AER.BMD.FM.MIwaref -AER.BMD.FM.SDradius -AER.BMD.FM.SDvar 	\
#     Aerosols parameters for external data (phase functions, scattering and extinction coefficients) :
#        -AER.ExtData
#
#   Hydrosols parameters :
#   ---------------------      
#        -PHYTO.ResFile -MLP.ResFile -HYD.Log
#        -HYD.DirMie  -HYD.MieLog
#        -HYD.Model
#     Phytoplankton model : 
#        Junge main mode :
#          -PHYTO.JD.slope -PHYTO.JD.rmin -PHYTO.JD.rmax -PHYTO.JD.MRwa -PHYTO.JD.MIwa -PHYTO.JD.rate 
#        Secondary LND mode :
#          -PHYTO.LND.SM.SDradius -PHYTO.LND.SM.SDvar -PHYTO.LND.SM.MRwa -PHYTO.LND.SM.MIwa -PHYTO.LND.SM.rate 
#        Tertiary LND mode :
#          -PHYTO.LND.TM.SDradius -PHYTO.LND.TM.SDvar -PHYTO.LND.TM.MRwa -PHYTO.LND.TM.MIwa -PHYTO.LND.TM.rate 
#     Mineral-like particles model :
#        Junge main mode :
#          -SED.JD.slope -SED.JD.rmin -SED.JD.rmax -SED.JD.MRwa -SED.JD.MIwa -SED.JD.rate 
#        Secondary LND mode :
#          -SED.LND.SM.SDradius -SED.LND.SM.SDvar -SED.LND.SM.MRwa -SED.LND.SM.MIwa -SED.LND.SM.rate 
#        Tertiary LND mode :
#          -SED.LND.TM.SDradius -SED.LND.TM.SDvar -SED.LND.TM.MRwa -SED.LND.TM.MIwa -SED.LND.TM.rate 
#     Hydrosols parameters for external data (phase functions, scattering and extinction coefficients) :       
#        -HYD.ExtData
#			        
#
#   Sea / atmosphere interface parameters :
#   --------------------------------------
#        -SEA.Dir
#        -SEA.Log
#        -SEA.Ind -SEA.Wind
#
#
#
#   In order to perform a simulation  : $OSOAA_ROOT/exe/run_OSOAA_demo.ksh 
#
#
#######################################################################################

dirRESULTS=${OSOAA_ROOT}/OSOAA_RESULTS_DEMO

dirMIE_AER=${OSOAA_ROOT}/DATABASE/MIE_AER  && mkdir -p ${dirMIE_AER}
dirMIE_HYD=${OSOAA_ROOT}/DATABASE/MIE_HYD  && mkdir -p ${dirMIE_HYD} 
dirSURF=${OSOAA_ROOT}/DATABASE/SURF_MATR   && mkdir -p ${dirSURF}

	 
${OSOAA_ROOT}/exe/OSOAA_MAIN.exe \
         -OSOAA.ResRoot ${dirRESULTS} \
         -OSOAA.Log Main.Log \
	  -OSOAA.Wa 0.440 \
         -ANG.Thetas 30. \
	  -AP.Pressure 1013.0  -AP.HR 8.0 -AP.HA 2.0 \
	  -AER.Waref 0.550 -AER.AOTref 0.1  \
	  -AER.DirMie ${dirMIE_AER} \
	  -AER.Model 2 \
	  -AER.SF.Model 3 -AER.SF.RH 98.  \
	  -PHYTO.Chl 0.2 \
	  -SED.Csed  0.0 -PHYTO.ProfilType 1 \
	  -YS.Abs440 0.00  -DET.Abs440 0.00 \
	  -SEA.Depth 15.000 \
	  -HYD.DirMie ${dirMIE_HYD}  \
	  -HYD.Model 1 \
	  -PHYTO.JD.slope 4.0 -PHYTO.JD.rmin 0.01 -PHYTO.JD.rmax 200. \
	  -PHYTO.JD.MRwa 1.05 -PHYTO.JD.MIwa -0.000 -PHYTO.JD.rate 1.0 \
	  -SEA.Dir ${dirSURF} -SEA.Ind 1.34  -SEA.Wind 7   \
	  -SEA.SurfAlb 0.0  -SEA.BotType 1 -SEA.BotAlb 0.30 \
 	  -OSOAA.View.Phi 0.0 \
	  -OSOAA.View.Level 5\
         -OSOAA.View.Z -10.0   -OSOAA.ResFile.vsVZA RESLUM_vsVZA.txt \
	  -OSOAA.View.VZA 0.0 -OSOAA.ResFile.vsZ RESLUM_vsZ.txt \
         -OSOAA.ResFile.Adv.Up   RESLUM_Advanced_UP.txt \
         -OSOAA.ResFile.Adv.Down RESLUM_Advanced_DOWN.txt 



#-----------------------------------------------------------------
#  DETAILLED LIST OF POSSIBLE ARGUMENTS 
#-----------------------------------------------------------------
#    Use of couples (-Keyword Value) for arguments definition
#
#    Keyword  : -OSOAA.ResRoot
#    -------------------------   
#       Status : Required     
#       Value :  Working folder for the OSOAA computations (complete path). 
#       Value format : Encoding as string (CTE_LENDIR characters max : see OSOAA.h).
# 
#    Keyword  : -ANG.Thetas  
#    ----------------------   
#       Status : Required    
#       Value :  Solar zenith angle (degrees) 
#       Value format : Encoding as real F9 (10 characters max).	
#    
#    Keyword  : -ANG.Rad.NbGauss  
#    ---------------------------   
#       Status : Optional user's definition
#		 	(if not defined, the default value CTE_DEFAULT_NBMU_LUM is used : 
#                 	 see OSOAA.h )       
#       Value :  Number of Gauss angles to be used for radiance computations 
#       Value format : Encoding as integer I4 (4 characters max)
#
#    Keyword  : -ANG.Rad.UserAngFile  
#    -------------------------------   
#       Status : Optional      
#       Value :  Filename of the complementary list of user's angles to complete the 
#                ANG.Rad.NbGauss angles (complete path). 
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#
#    Keyword  : -ANG.Rad.ResFile  
#    ---------------------------   
#       Status : Optional user's definition
#		 	(if not defined, the default value CTE_DEFAULT_FICANGLES_RES_LUM  
#                 	 is used :see OSOAA.h )       
#       Value :  Filename of list of angles used to BRDF/BPDF and radiance computations.
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder).
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#
#    Keyword  : -ANG.Mie.NbGauss  
#    ---------------------------   
#       Status : Optional user's definition
#                	(if not defined, the default value CTE_DEFAULT_NBMU_MIE is used : 
#                 	 see OSOAA.h )       
#       Value :  Number of Gauss angles to be used for phase matrix computations 
#       Value format : Encoding as integer I4 (4 characters max)
#
#    Keyword  : -ANG.Mie.UserAngFile 
#    -------------------------------   
#       Status : Optional      
#       Value :  Filename of the complementary list of user's angles to complete the 
#                ANG.Mie.NbGauss angles (complete path). 
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#
#    Keyword  : -ANG.Mie.ResFile  
#    ----------------------------   
#       Status : Optional user's definition
#	         	(if not defined, the default value CTE_DEFAULT_FICANGLES_RES_MIE  
#                 	 is used :see OSOAA.h )       
#       Value :  Filename of list of angles used to the matrix phase function computations. 
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder).
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#
#    Keyword  : -ANG.Log
#    -------------------   
#       Status : Optional 
#       Value :  log filename for ANGLES computations 
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder).
#      		 Only created if the log filename is specified. 
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten
#
#    Keyword  : -OSOAA.Log
#    ---------------------   
#       Status : Optional 
#       Value :  log filename for MAIN computations
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder).
#      		 Only created if the log filename is specified. 
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten
#
#    Keyword  : -OSOAA.Wa  
#    --------------------  
#       Status : Required      
#       Value :  Wavelength of radiance calculation (microns).
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -SEA.SurfAlb  
#    -----------------------  
#       Status : Required      
#       Value :  Foam lambertian reflectance for the wavelength of radiance 
#                calculation (i.e., albedo of the foam at the sea surface).
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -SEA.BotType 
#    ----------------------- 
#       Status : Required      
#       Value :  Type of sea bottom for albedo definition
#			Cases :	1 : Lambertian value (user data -SEA.BotAlb)
#				2 : Light sand	     (tabulated data)
#				3 : Green algua	     (tabulated data)
#				4 : Brown algae	     (tabulated data)
#				5 : Red algae	     (tabulated data)
#       Value format : Encoding as integer I4 (10 characters max).
#
#    Keyword  : -SEA.BotAlb  
#    ----------------------  
#       Status : Associated to -SEA.BotType 1    
#       Value :  Sea bottom albedo for the wavelength of radiance 
#                calculation (lambertian component).
#       Value format : Encoding as real F9.5 (10 characters max).
#		 	 	 
#    Keyword  : -OSOAA.View.Phi  
#    --------------------------   
#       Status : Required  
#       Value :  Relative azimuth angle (degrees).  
#       Value format : Encoding as real F9 (10 characters max).   	 	 	
#
#    Keyword  : -OSOAA.View.Level  
#    ----------------------------   
#       Status : Required  
#       Value :  Index for the output level definition
#                	1 : Top of Atmosphere
#			2 : Sea Bottom
#			3 : Sea Surface 0+
#			4 : Sea Surface 0-
#			5 : User's definition of altitude or depth 
#				(user data -OSOAA.View.Z)
#       Value format : Encoding as integer I4 (10 characters max).   	 	 	
#		 	 	 
#    Keyword  : -OSOAA.View.Z  
#    ------------------------   
#       Status : Associated to -OSOAA.View.Level 5
#       Value :  Altitude or depth (meters) for which the radiance has to be
#                given versus the viewing zenith angle 
#               (for the given relative azimuth angle).  
#       Value format : Encoding as real F9 (10 characters max).
#
#    Keyword  : -OSOAA.ResFile.vsVZA  
#    -------------------------------   
#       Status : Optional user's definition 
#			(if not defined, the default value CTE_DEFAULT_FICSOS_RES_VS_VZA 
#                        is used :see OSOAA.h )      
#       Value :  Filename of the ascii file resulting from SOS computations 
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Standard_outputs of the working folder) :  
#                ==> Output radiance field versus the viewing zenith angle 
#                    (for the given relative azimuth angle and given altitude or depth).  
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten     	 	 	
#
#    Keyword  : -OSOAA.View.VZA  
#    --------------------------   
#       Status : Optional (if defined, it is required to also define -OSOAA.ResFile.vsZ)  
#       Value :  Viewing zenith angle (degrees) for which the radiance has to be
#                given versus the depth (or altitude) 
#               (for the given relative azimuth angle).  
#       Value format : Encoding as real F9 (10 characters max).   	 	 	
#	 	 	 
#    Keyword  : -OSOAA.ResFile.vsZ  
#    -----------------------------   
#       Status : Optional (if defined, it is required to also define -OSOAA.View.VZA)     
#       Value :  Filename of the ascii file resulting from SOS computations 
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Standard_outputs of the working folder) :  
#                ==> Output radiance field versus the depth (or altitude) 
#                    (for the given relative azimuth angle and given viewing zenith angle).  
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten     	 	 	
#	 	 	 
#    Keyword  : -OSOAA.ResFile.Adv.Up  
#    --------------------------------   
#       Status : Optionnal  
#       Value :  Filename of the ascii file resulting from SOS computations 
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder) :  
#                ==> Advanced output upward radiance field versus the depth (or altitude) 
#                    AND versus the viewing zenith angle
#                    (for the given relative azimuth angle).  
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten     	 	 	
#	 	 	 
#    Keyword  : -OSOAA.ResFile.Adv.Down  
#    ----------------------------------  
#       Status : Optionnal  
#       Value :  Filename of the ascii file resulting from SOS computations 
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder) :  
#                ==> Advanced output downward radiance field versus the depth (or altitude) 
#                    AND versus the viewing zenith angle
#                    (for the given relative azimuth angle).  
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten    
#
#    Keyword  : -SOS.Log
#    -------------------   
#       Status : Optionnal 
#       Value :  log filename for SOS computations
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder).
#      		 Only created if the log filename is specified. 
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten
#
#    Keyword  : -SOS.IGmax  
#    ---------------------   
#       Status : Optional user's definition 
#			(if not defined, the default value CTE_DEFAULT_IGMAX 
#                        is used :see OSOAA.h )            
#       Value :  Maximal order of interaction (scattering & surface reflexion). 
#       Value format : Encoding as integer I4 (10 characters max).   	 	 	
#       
#    Keyword  : -SOS.ResFile.Bin  
#    ---------------------------   
#       Status : Optional user's definition
#			(if not defined, the default value CTE_DEFAULT_FICSOS_RES_BIN
#                        is used :see OSOAA.h )          
#       Value :  Filename of the binary file resulting from SOS computations
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder). 
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten     
#
#    Keyword  : -PROFILE.Log
#    -----------------------   
#       Status : Optional
#       Value :  log filename for PROFILE computations
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder).
#      		 Only created if the log filename is specified.
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten     
#
#    Keyword  : -PROFILE_ATM.ResFile  
#    -------------------------------   
#       Status : Optional user's definition
#			(if not defined, the default valueCTE_DEFAULT_FICPROFIL_ATM_RES
#                        is used :see OSOAA.h )           
#       Value :  Filename of the result for atmospheric PROFILE computations
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder). 
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten     
#
#    Keyword  : -AP.MOT  
#    ------------------   
#       Status : Optional (-AP.MOT or AP.Pressure has to be defined)      
#       Value :  Molecular optical thickness for the wavelength of radiance simulation
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -AP.Pressure 
#    -----------------------   
#       Status : Optional (-AP.MOT or AP.Pressure has to be defined)      
#       Value :  Atmospheric pressure at sea level (mbar)
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -AP.HR  
#    ------------------   
#       Status : Associated to -AP.MOT >= 0.0001      
#       Value :  Molecular heigth scale (km).
#       Value format : Encoding as real F9 (10 characters max).
#
#    Keyword  : -AP.HA  
#    ------------------   
#       Status : Associated to AOT >= 0.0001    
#       Value :  Aerosol heigth scale (km).
#       Value format : Encoding as real F9 (10 characters max).
#
#    Keyword  : -PROFILE_SEA.ResFile  
#    -------------------------------   
#       Status : Optional user's definition
#			(if not defined, the default valueCTE_DEFAULT_FICPROFIL_SEA_RES
#                        is used :see OSOAA.h )        
#       Value :  Filename of the result for the marine PROFILE computations
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder). 
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten     
#
#    Keyword  : -SEA.Depth  
#    ---------------------   
#       Status : Optional 
#		 (if not defined, euphotic depth will be used from Morel tabulated data 
#		  with regards to the chlorophyll concentration at sea surface)      
#       Value :  Sea depth value  (meters)
#       Value format : Encoding as real F9.2 (10 characters max).
#
#    Keyword  : -PHYTO.Chl  
#    ---------------------   
#       Status : Required     
#       Value :  chlorophyll concentration at sea surface (mg/m3)
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -PHYTO.ProfilType  
#    ----------------------------   
#       Status : Optional => Associated to CHL > 0.   
#       Value :  Type of chlorophyll profile.
#			Cases : 1 : Homogeneous profile
#				 2 : Gaussian profile
#				 3 : User profile
#       Value format : integer I4 (4 characters max)
#
#    Keyword  : -PHYTO.GP.Chlbg 
#    --------------------------   
#       Status : Optional => Associated to -PHYTO.ProfilType = 2.    
#       Value :  Constant biomass background (mg/m3)
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -PHYTO.GP.Deep 
#    -------------------------   
#       Status : Optional => Associated to -PHYTO.ProfilType = 2.    
#       Value :  Maximum deep of the gaussian chlorophyll profile (m)
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -PHYTO.GP.Width 
#    --------------------------   
#       Status : Optional => Associated to -PHYTO.ProfilType = 2.    
#       Value :  Peak width of the gaussian chlorophyll profile (m)
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -PHYTO.Userfile
#    --------------------------   
#       Status : Optional => Associated to -PHYTO.ProfilType = 3.
#       Value :  Name of user phytoplankton profile file (complete access)
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#
#    Keyword  : -SED.Csed  
#    --------------------   
#       Status : Required     
#       Value :  Concentration of mineral-like particles at sea surface (mg/liter)
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -YS.Abs440  
#    ---------------------   
#       Status : Required     
#       Value :  Yellow substance abs. coef. (m-1)  at 440 nm
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -YS.Swa 
#    ------------------   
#       Status : Optional user's definition
#			(if not defined, the default value CTE_DEFAULT_SPECTRAL_YS is used : see OSOAA.h )     
#       Value :  Coefficient of spectral variation for yellow substance absorption (m-1)
#       Value format : Encoding as real F9.5 (10 characters max).
#
#
#    Keyword  : -DET.Abs440  
#    ----------------------   
#       Status : Required     
#       Value :  Detritus absorption coef. (m-1)  at 440 nm
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -DET.Swa 
#    -------------------   
#       Status : Optional user's definition
#			(if not defined, the default value CTE_DEFAULT_SPECTRAL_DET is used : see OSOAA.h )     
#       Value :  Coefficient of spectral variation for detritus absorption (m-1)
#       Value format : Encoding as real F9.5 (10 characters max).
#                    	    
#       
#    Keyword  : -AER.Waref  
#    ---------------------   
#       Status : Required (for common case with AER.AOTref > 0)      
#       Value :  Wavelength (microns) for reference aerosol optical thickness.
#       Value format : Encoding as real F9.5 (10 characters max).
#       	  
#    Keyword  : -AER.AOTref  
#    ----------------------   
#       Status : Required   
#       Value :  Aerosol optical thickness for the reference wavelength.
#                --> real value, without applied truncation.
#       Value format : Encoding as real F9.5 (10 characters max). 
#
#    Keyword  : -AER.Model 
#    ----------------------   
#       Status : Required (for common case with AER.AOTref > 0)     
#       Value :  Type of aerosol model
#      		0 : Mono-modal
#			1 : WMO multi-modal
#			2 : Shettle & Fenn bi-modal
#		       3 : Log-Normal bi-modal
#                    4 : Phase function from an external source
#       Value format : Encoding as integer I2 (10 characters max).
#					
#    Keyword  : -AER.MMD.MRwa  
#    ------------------------ 
#       Status : Associated to -AER.Model 0 (for common case with AER.AOTref > 0)     
#       Value :  Real part of the refractive index at the wavelength of radiance calculation.
#       Value format : Encoding as real F5.3 (10 characters max).
#	  
#    Keyword  : -AER.MMD.MIwa  
#    ------------------------ 
#       Status : Associated to -AER.Model 0 (for common case with AER.AOTref > 0)     
#       Value :  Imaginary part of the refractive index for the wavelength of radiance calculation.
#       Value format : Encoding as real F8.5 (10 characters max).
#	  	  
#    Keyword  : -AER.MMD.MRwaref  
#    ---------------------------   
#       Status : Associated to -AER.Model 0 (for common case with AER.AOTref > 0).
#       Value :  Real part of the refractive index at the reference wavelength of aerosol properties calculation.
#       Value format : Encoding as real F5.3 (10 characters max).
#	  
#    Keyword  : -AER.MMD.MIwaref  
#    --------------------------- 
#       Status : Associated to -AER.Model 0 (for common case with AER.AOTref > 0).         
#       Value :  Imaginary part of the refractive index for the reference wavelength of aerosol properties calculation.
#       Value format : Encoding as real F8.5 (10 characters max).
#	                
#    Keyword  : -AER.MMD.SDtype 
#    --------------------------   
#       Status : Associated to -AER.Model 0 (for common case with AER.AOTref > 0)  
#       Value :  Type of mono-modal size distribution 
#			1 : LND.
#			2 : Junge's law     
#       Value format : Encoding as integer I4 (4 characters max).				    
#	    
#    Keyword  : -AER.MMD.LNDradius  
#    ----------------------------- 
#       Status : Associated to -AER.Model 0 and -AER.MMD.SDtype 1 (for common case with AER.AOTref > 0)     
#       Value :  Modal radius of the LND (microns)
#       Value format : Encoding as real F9.5 (10 characters max).	  
#	  
#    Keyword  : -AER.MMD.LNDvar  
#    -------------------------- 
#       Status : Associated to -AER.Model 0 and -AER.MMD.SDtype 1 (for common case with AER.AOTref > 0)     
#       Value :  Standard deviation of the LND
#       Value format : Encoding as real F9.5 (10 characters max).	  
#	    
#    Keyword  : -AER.MMD.JD.slope  
#    ----------------------------- 
#       Status : Associated to -AER.Model 0 and -AER.MMD.SDtype 2  (for common case with AER.AOTref > 0)     
#       Value :  Slope of the Junge's law
#       Value format : Encoding as real F9.5 (10 characters max).	  
#	    
#    Keyword  : -AER.MMD.JD.rmin  
#    --------------------------- 
#       Status : Associated to -AER.Model 0 and -AER.MMD.SDtype 2  (for common case with AER.AOTref > 0)     
#       Value :  Minimal radius of the Junge's law (microns).
#       Value format : Encoding as real F9.5 (10 characters max).	  
#	    
#    Keyword  : -AER.MMD.JD.rmax  
#    --------------------------- 
#       Status : Optional user's definition
#			(and associated to -AER.Model 0 and -AER.MMD.SDtype 2 
#                	 for common case with AER.AOTref > 0)     
#               (if not defined, the default value CTE_DEFAULT_AER_JUNGE_RMAX is used :  
#                see OSOAA.h )
#       Value :  Maximal radius of the Junge's law (microns).
#       Value format : Encoding as real F9.5 (10 characters max).	  
#	  	  	  
#    Keyword  : -AER.WMO.Model  
#    ------------------------- 
#       Status : Associated to -AER.Model 1 (for common case with AER.AOT > 0)     
#       Value :  Type of WMO model.
#      		1 : Continental WMO model.
#			2 : Maritime WMO model.
#			3 : Urban WMO model.
#		       4 : WMO model by user definition.
#       Value format : Encoding as integer I2 (10 characters max).	  
#
#    Keyword  : -AER.WMO.DL  
#    ---------------------- 
#       Status : Associated to -AER.Model 1 and -AER.WMO.Model 4 (for common case with AER.AOTref > 0)     
#       Value :  Volume concentration (ratio between 0 and 1) of WMO Dust-Like particles.
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -AER.WMO.WS  
#    ---------------------- 
#       Status : Associated to -AER.Model 1 and -AER.WMO.Model 4 (for common case with AER.AOTref > 0)     
#       Value :  Volume concentration (ratio between 0 and 1) of WMO Water Soluble particles.
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -AER.WMO.OC  
#    ---------------------- 
#       Status : Associated to -AER.Model 1 and -AER.WMO.Model 4 (for common case with AER.AOTref > 0)     
#       Value :  Volume concentration (ratio between 0 and 1) of WMO OCeanic particles.
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -AER.WMO.SO  
#    ---------------------- 
#       Status : Associated to -AER.Model 1 and -AER.WMO.Model 4 (for common case with AER.AOTref > 0)     
#       Value :  Volume concentration (ratio between 0 and 1) of WMO SOot particles.
#       Value format : Encoding as real F9.5 (10 characters max).	  
#	  
#    Keyword  : -AER.SF.Model  
#    ------------------------ 
#       Status : Associated to -AER.Model 2 (for common case with AER.AOTref > 0)     
#       Value :  Type of Shettle & Fenn model.
#      		1 : Tropospheric S&F model.
#			2 : Urban S&F model.
#			3 : Maritime S&F model.
#		       4 : Coastal S&F model.
#       Value format : Encoding as integer I2 (10 characters max).	  
#
#    Keyword  : -AER.SF.RH  
#    --------------------- 
#       Status : Associated to -AER.Model 2 (for common case with AER.AOTref > 0)     
#       Value :  Relative humidity (%) for Shettle & Fenn model.
#       Value format : Encoding as real F5.2 (10 characters max).
#	  
#    Keyword  : -AER.BMD.VCdef 
#    ---------------------   
#       Status : Associated to -AER.Model 3 (for common case with AER.AOTref > 0)  
#       Value :  Type of selected method for bi-modal LND volume concentration definition 
#			1 : Use of user volume concentrations.
#			2 : Use of user ratio AOT_coarse / AOT_total.
#       Value format : Encoding as integer I2 (10 characters max).			
#
#    Keyword  : -AER.BMD.CoarseVC  
#    ---------------------------- 
#       Status : Associated to -AER.Model 3 and -AER.BMD.VCdef 1 (for common case with AER.AOTref > 0)     
#       Value :  User volume concentration of the "LND coarse mode".
#       Value format : Encoding as real F8.5 (10 characters max).
#
#    Keyword  : -AER.BMD.FineVC  
#    -------------------------- 
#       Status : Associated to -AER.Model 3 and -AER.BMD.VCdef 1 (for common case with AER.AOTref > 0)     
#       Value :  User volume concentration of the "LND fine mode".
#       Value format : Encoding as real F8.5 (10 characters max).
#
#    Keyword  : -AER.BMD.RAOT  
#    ------------------------ 
#       Status : Associated to -AER.Model 3 and -AER.BMD.VCdef 2 (for common case with AER.AOTref > 0)
#       Value :  User value of the ration AOT_coarse / AOT_total for the aerosol reference wavelength.
#       Value format : Encoding as real F9.5 (10 characters max).
#	         
#    Keyword  : -AER.BMD.CM.MRwa  
#    ---------------------------   
#       Status : Associated to -AER.Model 3 (for common case with AER.AOTref > 0)
#       Value :  Real part of the refractive index for the "LND coarse mode" 
#                at the wavelength of radiance calculation.
#       Value format : Encoding as real F5.3 (10 characters max).
#	  
#    Keyword  : -AER.BMD.CM.MIwa  
#    ---------------------------   
#       Status : Associated to -AER.Model 3 (for common case with AER.AOTref > 0)
#       Value :  Imaginary part of the refractive index for the "LND coarse mode" 
#                at the wavelength of radiance calculation.
#       Value format : Encoding as real F8.5 (10 characters max).
#	  
#    Keyword  : -AER.BMD.CM.MRwaref  
#    ------------------------------   
#       Status : Associated to -AER.Model 3 and -AER.BMD.VCdef 2 (for common case with AER.AOTref > 0).
#                Must to be defined if AER.Waref different of OSOAA.Wa.
#       Value :  Real part of the refractive index for the "LND coarse mode" 
#                at the aerosol reference wavelength.
#       Value format : Encoding as real F5.3 (10 characters max).
#	  
#    Keyword  : -AER.BMD.CM.MIwaref  
#    -----------------------------   
#       Status : Associated to -AER.Model 3 and -AER.BMD.VCdef 2 (for common case with AER.AOTref > 0).
#                Must to be defined if AER.Waref different of OSOAA.Wa.
#       Value :  Imaginary part of the refractive index for the "LND coarse mode" 
#                at the aerosol reference wavelength.
#       Value format : Encoding as real F8.5 (10 characters max).
#	  
#    Keyword  : -AER.BMD.CM.SDradius  
#    -------------------------------   
#       Status : Associated to -AER.Model 3 (for common case with AER.AOTref > 0).    
#       Value :  Modal radius of the "LND coarse mode" (microns).
#       Value format : Encoding as real F9.5 (10 characters max).
#	  
#    Keyword  : -AER.BMD.CM.SDvar  
#    ----------------------------   
#       Status : Associated to -AER.Model 3 (for common case with AER.AOTref > 0).    
#       Value :  Standard deviation of the "LND coarse mode" .
#       Value format : Encoding as real F9.5 (10 characters max).
#	  	 	  
#    Keyword  : -AER.BMD.FM.MRwa  
#    ---------------------------   
#       Status : Associated to -AER.Model 3 (for common case with AER.AOT > 0)
#       Value :  Real part of the refractive index for the "LND fine mode" 
#                at the wavelength of radiance calculation.
#       Value format : Encoding as real F5.3 (10 characters max).
#	  
#    Keyword  : -AER.BMD.FM.MIwa  
#    ---------------------------   
#       Status : Associated to -AER.Model 3 (for common case with AER.AOT > 0)
#       Value :  Imaginary part of the refractive index for the "LND fine mode" 
#                at the wavelength of radiance calculation.
#       Value format : Encoding as real F8.5 (10 characters max).
#	  
#    Keyword  : -AER.BMD.FM.MRwaref  
#    ------------------------------   
#       Status : Associated to -AER.Model 3 and -AER.BMD.VCdef 2 (for common case with AER.AOT > 0)
#                Must to be defined if AER.Waref different of OSOAA.Wa.
#       Value :  Real part of the refractive index for the "LND fine mode" 
#                at the aerosol reference wavelength.
#       Value format : Encoding as real F5.3 (10 characters max).
#	  
#    Keyword  : -AER.BMD.FM.MIwaref  
#    -----------------------------   
#       Status : Associated to -AER.Model 3 and -AER.BMD.VCdef 2 (for common case with AER.AOT > 0)
#                Must to be defined if AER.Waref different of OSOAA.Wa.
#       Value :  Imaginary part of the refractive index for the "LND fine mode" 
#                at the aerosol reference wavelength.
#       Value format : Encoding as real F8.5 (10 characters max).
#	  
#    Keyword  : -AER.BMD.FM.SDradius  
#    -------------------------------   
#       Status : Associated to -AER.Model 3 (for common case with AER.AOT > 0)    
#       Value :  Modal radius of the "LND fine mode" (microns).
#       Value format : Encoding as real F9.5 (10 characters max).
#	  
#    Keyword  : -AER.BMD.FM.SDvar  
#    ----------------------------   
#       Status : Associated to -AER.Model 3 (for common case with AER.AOT > 0)    
#       Value :  Standard deviation of the "LND fine mode" .
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -AER.ExtData
#    -----------------------   
#       Status : Associated to -AER.Model 4 (for common case with AER.AOTref > 0)       
#       Value :  Filename (complete path) of user's external phase functions 
#                and radiative parameters (extinction and scattering coefficients)
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#       Special requirement : the reference aerosol wavelength (-AER.Waref) and 
#                             the radiance simulation wavelength (-OSOAA.Wa) have to be equal.
#
#    Keyword  : -AER.Tronca 
#    ----------------------   
#       Status : Optional (for common case with AER.AOTref > 0)     
#       Value :  Option for no aerosol phase function troncation (0 to not apply a troncation)
#       Value format : Encoding as integer I4 (4 characters max).
#
#    Keyword  : -AER.ResFile  
#    -----------------------   
#       Status : Optional user's definition
#			(if not defined, the default value CTE_DEFAULT_FICGRANU_AER
#                        is used :see OSOAA.h )          
#       Value :  Filename of the result OSOAA_AEROSOLS computations
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder).
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten     
#
#    Keyword  : -AER.Log  
#    -------------------  
#       Status : Optional     
#       Value :  log filename for AEROSOLS file computations
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder).
#      		 Only created if the log filename is specified.
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten
#
#    Keyword  : -AER.MieLog  
#    ----------------------   
#       Status : Optional           
#       Value :  log filename for MIE aerosols computations
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder).
#      		 Only created if the log filename is specified.
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten
#
#    Keyword  : -AER.DirMie
#    ----------------------   
#       Status : Required  (for common case with AER.AOTref > 0)    
#       Value :  Storage directory for MIE files producted by OSOAA_AEROSOLS computations (complete path). 
#       Value format : Encoding as string (CTE_LENDIR characters max : see OSOAA.h).
# 
#    Keyword  : -PHYTO.ResFile  
#    -------------------------   
#       Status : Optional user's definition
#			(if not defined, the default value CTE_DEFAULT_FICGRANU_PHYTO
#                        is used :see OSOAA.h )          
#       Value :  Filename of the result OSOAA_HYDROSOLS computations 
#                for phytoplankton particles
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder).
#		 NB : Include the result of phase matrix development from user file
#                     global phase function in case of an user file is used (-HYD.ExtData).  
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten     

#    Keyword  : -MLP.ResFile  
#    -----------------------   
#       Status : Optional user's definition
#			(if not defined, the default value CTE_DEFAULT_FICGRANU_MLP
#                        is used :see OSOAA.h )          
#       Value :  Filename of the result OSOAA_HYDROSOLS computations 
#                for Mineral Like particles
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder). 
#		 NB : Nul coefficients of phase matrix development 
#                     in case of an user file is used (-HYD.ExtData).  
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten     
#
#    Keyword  : -HYD.Log  
#    -------------------  
#       Status : Optional     
#       Value :  log filename for HYDROSOLS file computations
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder).
#      		 Only created if the log filename is specified.
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten
#
#    Keyword  : -HYD.MieLog  
#    ----------------------   
#       Status : Optional           
#       Value :  log filename for MIE hydrosols computations
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder).
#      		 Only created if the log filename is specified.
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten
#
#    Keyword  : -HYD.DirMie
#    ----------------------   
#       Status : Required  (for common case with PHYTO.Chl or SED.Csed > 0)    
#       Value :  Storage directory for MIE files producted by HYDROSOLS_AEROSOLS computations 
#               (complete path). 
#       Value format : Encoding as string (CTE_LENDIR characters max : see OSOAA.h).
# 
#    Keyword  : -HYD.Model 
#    ----------------------   
#       Status : Required (for common case with PHYTO.Chl or SED.Csed > 0)     
#       Value :  Type of hydrosol characterization
#      		1 : From size distribution models.
#			2 : Use of external phase functions
#       Value format : Encoding as integer I2 (10 characters max).
#					
#    Keyword  : -PHYTO.JD.slope 
#    --------------------------   
#       Status : Required (for common case with PHYTO.Chl > 0)     
#       Value :  Slope of Junge's law for phytoplankton particles
#       Value format : Encoding as real F6.3 (10 characters max).
#					
#    Keyword  : -PHYTO.JD.rmin 
#    --------------------------   
#       Status : Optional user's definition (for common case with PHYTO.Chl > 0)        
#               (if not defined, the default value CTE_DEFAULT_HYD_JUNGE_RMIN is used : 
#		 see OSOAA.h )   
#       Value :  Minimal radius of Junge's law for phytoplankton particles (microns)
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -PHYTO.JD.rmax 
#    --------------------------   
#       Status : Optional user's definition (for common case with PHYTO.Chl > 0)    
#                (if not defined, the default value CTE_DEFAULT_HYD_JUNGE_RMAX is used : 
#		  see OSOAA.h )      
#       Value :  Maximal radius of Junge's law for phytoplankton particles (microns)
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -PHYTO.JD.MRwa 
#    --------------------------   
#       Status : Required (for common case with PHYTO.Chl > 0)     
#       Value :  Real part of the refractive index for phytoplankton particles
#                at the simulation wavelength: main mode of particles (Junge distribution)
#       Value format : Encoding as real F5 (10 characters max).
#
#    Keyword  : -PHYTO.JD.MIwa 
#    --------------------------   
#       Status : Required (for common case with PHYTO.Chl > 0)     
#       Value :  Imaginary part of the refractive index for phytoplankton particles
#                at the simulation wavelength: main mode of particles (Junge distribution)
#       Value format : Encoding as real F8 (10 characters max).
#
#    Keyword  : -PHYTO.JD.rate 
#    --------------------------   
#       Status : Required (for common case with PHYTO.Chl > 0)     
#       Value :  Ratio of the Junge's law in the global distribution for phytoplankton  
#                 particles  ==> as a proportion of the Junge distribution particles  
#		                 versus the global amount of phytoplankton particles.
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -PHYTO.LND.SM.SDradius 
#    ---------------------------------   
#       Status : Optional (for common case with PHYTO.Chl > 0)     
#		 --> BUT, the definition of one parameter PHYTO.LND.SM.* requires the  
#		     definition of the others.
#       Value :  Modal radius of the LND for the secondary mode of phytoplankton particles (mic)
#       Value format : Encoding as real F9.5 (10 characters max).
#					
#    Keyword  : -PHYTO.LND.SM.SDvar 
#    ------------------------------   
#       Status : Optional (for common case with PHYTO.Chl > 0)   
#		 --> BUT, the definition of one parameter PHYTO.LND.SM.* requires the  
#		     definition of the others.  
#       Value :  Standard deviation of the LND for the secondary mode of phytoplankton particles
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -PHYTO.LND.SM.MRwa
#    -----------------------------   
#       Status : Optional (for common case with PHYTO.Chl > 0)   
#		 --> BUT, the definition of one parameter PHYTO.LND.SM.* requires the  
#		     definition of the others.  
#       Value :  Real part of the refractive index for phytoplankton particles
#                at the simulation wavelength: secondary mode of particles (LND distribution)
#       Value format : Encoding as real F5 (10 characters max).
#
#    Keyword  : -PHYTO.LND.SM.MIwa
#    -----------------------------   
#       Status : Optional (for common case with PHYTO.Chl > 0)  
#		 --> BUT, the definition of one parameter PHYTO.LND.SM.* requires the  
#		     definition of the others.   
#       Value :  Imaginary part of the refractive index for phytoplankton particles
#                at the simulation wavelength: secondary mode of particles (LND distribution)
#       Value format : Encoding as real F8 (10 characters max).
#
#    Keyword  : -PHYTO.LND.SM.rate 
#    -----------------------------   
#       Status : Optional (for common case with PHYTO.Chl > 0)
#		 --> BUT, the definition of one parameter PHYTO.LND.SM.* requires the  
#		     definition of the others.        
#       Value :  Ratio of the LND secondary mode in the global distribution for phytoplankton  
#                particles ==> as a proportion of the number of LND for secondary mode 
#		               particles versus the global amount of phytoplankton particles.
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -PHYTO.LND.TM.SDradius 
#    ---------------------------------   
#       Status : Optional (for common case with PHYTO.Chl > 0)
#		 --> BUT, the definition of one parameter PHYTO.LND.TM.* requires the  
#		     definition of the others.        
#       Value :  Modal radius of the LND for the tertiary mode of phytoplankton particles (mic)
#       Value format : Encoding as real F9.5 (10 characters max).
#					
#    Keyword  : -PHYTO.LND.TM.SDvar 
#    ------------------------------   
#       Status : Optional (for common case with PHYTO.Chl > 0)  
#		 --> BUT, the definition of one parameter PHYTO.LND.TM.* requires the  
#		     definition of the others.      
#       Value :  Standard deviation of the LND for the tertiary mode of phytoplankton particles 
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -PHYTO.LND.TM.MRwa
#    -----------------------------   
#       Status : Optional (for common case with PHYTO.Chl > 0)  
#		 --> BUT, the definition of one parameter PHYTO.LND.TM.* requires the  
#		     definition of the others.      
#       Value :  Real part of the refractive index for phytoplankton particles
#                at the simulation wavelength: tertiary mode of particles (LND distribution)
#       Value format : Encoding as real F5 (10 characters max).
#
#    Keyword  : -PHYTO.LND.TM.MIwa
#    -----------------------------   
#       Status : Optional (for common case with PHYTO.Chl > 0)   
#		 --> BUT, the definition of one parameter PHYTO.LND.TM.* requires the  
#		     definition of the others.     
#       Value :  Imaginary part of the refractive index for phytoplankton particles
#                at the simulation wavelength: tertiary mode of particles (LND distribution)
#       Value format : Encoding as real F8 (10 characters max).
#
#    Keyword  : -PHYTO.LND.TM.rate 
#    -----------------------------   
#       Status : Optional (for common case with PHYTO.Chl > 0)     
#		 --> BUT, the definition of one parameter PHYTO.LND.TM.* requires the  
#		     definition of the others.   
#       Value :  Ratio of the LND tertiary mode in the global distribution for phytoplankton particles 
#                ==> as a proportion of the number of LND for the tertiary mode particles versus 
#		     the global amount of phytoplankton particles.
#       Value format : Encoding as real F9.5 (10 characters max).
#					
#    Keyword  : -SED.JD.slope 
#    ------------------------  
#       Status : Required (for common case with SED.Csed > 0)     
#       Value :  Slope of Junge's law for mineral-like particles
#       Value format : Encoding as real F6.3 (10 characters max).
#					
#    Keyword  : -SED.JD.rmin 
#    --------------------------   
#       Status : Optional user's definition (for common case with SED.Csed > 0)    
#                (if not defined, the default value CTE_DEFAULT_HYD_JUNGE_RMIN is used : 
#                 see OSOAA.h )   
#       Value :  Minimal radius of Junge's law for mineral-like particles (microns)
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -SED.JD.rmax 
#    --------------------------   
#       Status : Optional user's definition (for common case with SED.Csed > 0)    
#                (if not defined, the default value CTE_DEFAULT_HYD_JUNGE_RMAX is used : 
#                 see OSOAA.h )        
#       Value :  Maximal radius of Junge's law for mineral-like particles (microns)
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -SED.JD.MRwa 
#    --------------------------   
#       Status : Required (for common case with SED.Csed > 0)   
#       Value :  Real part of the refractive index for mineral-like particles
#                at the simulation wavelength: main mode of particles (Junge distribution)
#       Value format : Encoding as real F5 (10 characters max).
#
#    Keyword  : -SED.JD.MIwa 
#    --------------------------   
#       Status : Required (for common case with SED.Csed > 0)   
#       Value :  Imaginary part of the refractive index for mineral-like particles
#                at the simulation wavelength: main mode of particles (Junge distribution)
#       Value format : Encoding as real F8 (10 characters max).
#
#    Keyword  : -SED.JD.rate 
#    --------------------------   
#       Status : Required (for common case with SED.Csed > 0)     
#       Value :  Ratio of the Junge's law in the global distribution for mineral-like particles 
#                ==> as a proportion of the Junge distribution particles versus the global 
#		     amount of mineral-like particles.
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -SED.LND.SM.SDradius 
#    -------------------------------   
#       Status : Optional (for common case with SED.Csed > 0)    
#		 --> BUT, the definition of one parameter SED.LND.SM.* requires the  
#		     definition of the others.    
#       Value :  Modal radius of the LND for the secondary mode of mineral-like particles (microns)
#       Value format : Encoding as real F9.5 (10 characters max).
#					
#    Keyword  : -SED.LND.SM.SDvar 
#    ----------------------------   
#       Status : Optional (for common case with SED.Csed > 0)    
#		 --> BUT, the definition of one parameter SED.LND.SM.* requires the  
#		     definition of the others.    
#       Value :  Standard deviation of the LND for the secondary mode of mineral-like particles 
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -SED.LND.SM.MRwa
#    ---------------------------   
#       Status : Optional (for common case with SED.Csed > 0)    
#		 --> BUT, the definition of one parameter SED.LND.SM.* requires the  
#		     definition of the others.      
#       Value :  Real part of the refractive index for mineral-like particles
#                at the simulation wavelength: secondary mode of particles (LND distribution)
#       Value format : Encoding as real F5 (10 characters max).
#
#    Keyword  : -SED.LND.SM.MIwa
#    --------------------------- 
#       Status : Optional (for common case with SED.Csed > 0)    
#		 --> BUT, the definition of one parameter SED.LND.SM.* requires the  
#		     definition of the others.    
#       Value :  Imaginary part of the refractive index for mineral-like particles
#                at the simulation wavelength: secondary mode of particles (LND distribution)
#       Value format : Encoding as real F8 (10 characters max).
#
#    Keyword  : -SED.LND.SM.rate 
#    ---------------------------  
#       Status : Optional (for common case with SED.Csed > 0)    
#		 --> BUT, the definition of one parameter SED.LND.SM.* requires the  
#		     definition of the others.     
#       Value :  Ratio of the LND secondary mode in the global distribution for mineral-like particles 
#                ==> as a proportion of the number of LND for secondary mode particles versus 
#		     the global amount of mineral-like particles.
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -SED.LND.TM.SDradius 
#    -------------------------------   
#       Status : Optional (for common case with SED.Csed > 0)    
#		 --> BUT, the definition of one parameter SED.LND.TM.* requires the  
#		     definition of the others.  
#       Value :  Modal radius of the LND for tertiary mode of mineral-like particles (microns)
#       Value format : Encoding as real F9.5 (10 characters max).
#					
#    Keyword  : -SED.LND.TM.SDvar 
#    ------------------------------   
#       Status : Optional (for common case with SED.Csed > 0)    
#		 --> BUT, the definition of one parameter SED.LND.TM.* requires the  
#		     definition of the others.   
#       Value :  Standard deviation of the LND for tertiary mode of mineral-like particles 
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -SED.LND.TM.MRwa
#    -----------------------------   
#       Status : Optional (for common case with SED.Csed > 0)    
#		 --> BUT, the definition of one parameter SED.LND.TM.* requires the  
#		     definition of the others.  
#       Value :  Real part of the refractive index for mineral-like particles
#                at the simulation wavelength: tertiary mode of particles (LND distribution)
#       Value format : Encoding as real F5 (10 characters max).
#
#    Keyword  : -SED.LND.TM.MIwa
#    -----------------------------   
#       Status : Optional (for common case with SED.Csed > 0)    
#		 --> BUT, the definition of one parameter SED.LND.TM.* requires the  
#		     definition of the others.   
#       Value :  Imaginary part of the refractive index for mineral-like particles
#                at the simulation wavelength: tertiary mode of particles (LND distribution)
#       Value format : Encoding as real F8 (10 characters max).
#
#    Keyword  : -SED.LND.TM.rate 
#    -----------------------------   
#       Status : Optional (for common case with SED.Csed > 0)    
#		 --> BUT, the definition of one parameter SED.LND.TM.* requires the  
#		     definition of the others.  
#       Value :  Ratio of the LND tertiary mode in the global distribution for mineral-like particles 
#                ==> as a proportion of the number of LND for secondary mode particles versus 
#		     the global amount of mineral-like particles.
#       Value format : Encoding as real F9.5 (10 characters max).
#
#    Keyword  : -HYD.ExtData
#    -----------------------   
#       Status : Associated to -HYD.Model 4 (for common case with PHYTO.Chl or SED.Csed > 0)    
#       Value :  Filename (complete path) of user's external phase functions 
#                and radiative parameters (extinction and scattering coefficients)
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#       Special requirement : the radiative measurements have to be available at
#                             the radiance simulation wavelength (-OSOAA.Wa).
#
#    Keyword  : -SEA.Dir  
#    -------------------  
#       Status : Required      
#       Value :  Directory for SURFACE files storage (complete path). 
#       Value format : Encoding as string (CTE_LENDIR characters max : see OSOAA.h).
#
#    Keyword  : -SEA.Log  
#    -------------------  
#       Status : Optional     
#       Value :  log filename for SURFACE file computations
#                (defined without directory tree ==> this file will be located 
#                 in the sub-directory Advanced_outputs of the working folder).
#      		 Only created if the log filename is specified.
#       Value format : Encoding as string (CTE_LENFIC2 characters max : see OSOAA.h).
#        ==> Already existing file will be overwritten
#	
#    Keyword  : -SEA.Ind  
#    -------------------   
#       Status : Required     
#       Value :  Surface / atmosphere refractive index (air = 1).. 
#       Value format : Encoding as real F5.3 (10 characters max).
#	 		 
#    Keyword  : -SEA.Wind  
#    --------------------   
#       Status : Required    
#       Value :  Wind velocity (m/s). 
#       Value format : Encoding as real F4 (10 characters max).
#       
