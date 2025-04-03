# Ocean Successive Orders with Atmosphere - Advanced (OSOAA V2.0 - March 26th 2025)

The OSOAA model enables accurate calculation of the radiative tranfer into the Ocean-Atmosphere system including polarization, coupling terms, and the rugged surface. 
      
Based on the successive orders of scattering method, the initial OSOA (Ocean Successive Orders with Atmosphere) code was the first version of a radiative transfer model for the computation of radiance and polarization in an ocean-atmosphere system, accounting for a flat surface. 
The OSOA-Advanced code (so-called OSOAA) introduces the capability to simulate a more realistic air / sea interface by taking into account the roughness of the sea surface as modelled by Cox & Munk. 
Note that OSOAA model allows the computation of the polarization state of light (it is a vector radiative transfer model). 
The code also offers a user friendly interface (GUI) and simplified command lines to perform a set of simulation. 

OSOAA allows simulating:
* Atmospheric and sea profiles: 
For the characterization of the atmosphere, the user can define the molecular and aerosol optical thickness. 
For the characterization of the water column, the chlorophyll and mineral-like particles concentrations are used as inputs of the code. The chlorophyll profile can be a homogeneous profile, a Gaussian profile or a user’s one. The absorption of yellow substance and detritus (dead phytoplankton particles) is also modelled.
Users can also use their own profile of absorption and scattering coefficients, as obtained from in-situ measurements for instance (new feature in version V2.0).
* Aerosol models: 
A wide variety of aerosol size distribution is available in OSOAA model to make the optical properties of the atmosphere highly close to real-world conditions. Log-normal (LND) or Junge mono-modal size distributions, bimodal LND, or pre-calculated WMO or Shettle & Fenn models can be used.
Users can also use their own aerosol phase function and radiative properties. 
* Hydrosol models: 
Phytoplankton and Mineral-Like Particles, including their scattering and absorbing properties, can be simulated. 
Users can also use their own hydrosol phase function and radiative properties. 
* Sea surface interface: 
The air / sea interface can be modelled either for a flat surface or by taking into account of the sea roughness defined by the wind speed and the correlated waves. 

OSOAA calculates the light transmission through the sea surface from air to sea and from sea to air. It calculates the upward reflection of the downwelling radiance field of the atmosphere on the surface and also the downward reflection of the upwelling radiance field below the surface.
  
The user can define specific angles for which output simulated radiance are requested.
OSOAA provides the radiance field for a given altitude in the atmosphere or depth in the ocean. It can also provide the radiance profile for a specified direction. 
It is also possible to get the complete radiance field (upward and downward radiance in term of intensity and polarized radiance throughout the atmospheric and marine profiles).
 
**REFERENCE** 
This OSOAA code can be referenced as : 
*Chami M, Lafrance B, Fougnie B, Chowdhary J, Harmel T, Waquet F (2015), "OSOAA: a vector radiative transfer model of coupled atmosphere-ocean system for a rough sea surface application to the estimates of the directional variations of the water leaving reflectance to better process multi-angular satellite sensors data over the ocean", Opt Express 23: 27829-27852, doi: 10.1364/OE.23.027829*

**Copyright**  
© 2015–2025 **Université Pierre et Marie Curie - Paris 6** (renamed as Sorbonne Université since 2018) and **Centre National d’Études Spatiales (CNES)**

**License**  
This program is free software, licensed under the terms of the **GNU General Public License v3.0 (GPLv3)** or (at your option) any later version.  

**Funding**  
This project has been funded by the **Centre National d’Études Spatiales (CNES)**.

**Development**  
Development was carried out by **CS GROUP**.

