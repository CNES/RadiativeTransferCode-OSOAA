package osoaa.bll.properties;

import osoaa.bll.exception.InitException;

import javax.swing.*;
import java.math.BigDecimal;

public interface IPropertiesManager {

	void init() throws InitException;

	BigDecimal getCtePi();
	Integer getCteMieDim();
	String getCteFicMolSpectralData();
	String getCteFicEuphDepth();
	String getCteDefaultFicanglesResMie();
	Integer getCteLendir();
	String getCteDefaultFicgranuAer();
	String getCteDefaultFicanglesResLum();
	Integer getCteLenkeyword();
	BigDecimal getCteSeaTLimit();
	String getCteDefaultFicprofilAtmRes();
	Integer getCteNotDefinedValueInt();
	Integer getCteLencom();
	BigDecimal getCteDefaultHydJungeRmax();
	Integer getCtePhTest();
	Integer getCteDefaultIgmax();
	Integer getCteDefaultOsNs();
	String getCteFicPhytoSpectralData();
	Integer getCteDefaultNbmuMie();
	BigDecimal getCteDefaultSpectralYs();
	String getCteDefaultFicgranuMlp();
	BigDecimal getCteSeaDepthStep();
	BigDecimal getCteDefaultSpectralDet();
	Integer getCteDefaultOsNm();
	BigDecimal getCteMieNbmuMax();
	Integer getCteOsNmMax();
	Integer getCteDefaultNbmuLum();
	Integer getCteOsNbMax();
	Integer getCteMaxnbAngExt();
	BigDecimal getCteTransOptThickness();
	BigDecimal getCteWamin();
	Integer getCtePhNu();
	Integer getCteDefaultOsNb();
	BigDecimal getCteSeuilEcartMus();
	Integer getCtePhNq();
	BigDecimal getCteJungeSlopeCor();
	Integer getCteOsNbmuMax();
	BigDecimal getCtePhSeuilTronca();
	String getCteDefaultFicsosResVsVza();
	BigDecimal getCteAltToa();
	Integer getCteNtSea();
	Integer getCteStdPressure();
	String getCteNotDefinedValueDble();
	Integer getCteNbmaxUserAngles();
	String getCteDefaultFicgranuPhyto();
	Integer getCteNbanglesMax();
    BigDecimal getCteDefaultAerJungeRmax();
	Integer getCteNtAtm();
	BigDecimal getCteCoefNrmax();
	Integer getCteLenfic2();
	Integer getCteLenfic1();
	String getCteDefaultFicprofilSeaRes();
	BigDecimal getCteDefaultHydJungeRmin();
	Integer getCteNzMax();
	String getCteDefaultFicsosResBin();
	Integer getCteNbwaMax();
	Integer getCteOsNsMax();

    void register(JLabel ysSwa_title, JSpinner ysSwa_spinner);
}
