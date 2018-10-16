package osoaa.bll.properties;

import osoaa.bll.exception.InitException;
import osoaa.dal.properties.DALPropertiesManager;
import osoaa.dal.properties.IDALPropertiesManager;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class PropertiesManager implements IPropertiesManager {

	private static IPropertiesManager s_instance;
	private IDALPropertiesManager m_dalPropertiesManager;
    private static Set<String> s_specialKeys;
    private static Map<String, Object> s_Buttons;
    private static Map<String, Object> s_specialKeyMap;
	
	static {
		s_instance = new PropertiesManager();
        s_specialKeys = new HashSet<String>();
        s_Buttons = new HashMap<String, Object>();
        s_specialKeyMap = new HashMap<String, Object>();

        s_specialKeys.add("AER.MMD.JD.rmax");
        s_specialKeys.add("AER.ResFile");
        s_specialKeys.add("PROFILE_ATM.ResFile");
        s_specialKeys.add("DET.Swa");
        s_specialKeys.add("YS.Swa");
        s_specialKeys.add("ANG.Mie.NbGauss");
        s_specialKeys.add("ANG.Mie.ResFile");
        s_specialKeys.add("ANG.Rad.NbGauss");
        s_specialKeys.add("ANG.Rad.ResFile");
        s_specialKeys.add("MLP.ResFile");
        s_specialKeys.add("PHYTO.JD.rmax");
        s_specialKeys.add("PHYTO.JD.rmin");
        s_specialKeys.add("PHYTO.ResFile");
        s_specialKeys.add("SED.JD.rmax");
        s_specialKeys.add("SED.JD.rmin");
        s_specialKeys.add("OSOAA.ResFile.vsVZA");
        s_specialKeys.add("SOS.IGmax");
        s_specialKeys.add("SOS.ResFile.Bin");
	}

	public static IPropertiesManager getInstance(){
		return s_instance;
	}

    public static Map<String, Object> getButtons()
    {
        return s_Buttons;
    }
	
	private PropertiesManager(){
	}
	
	@Override
	public void init() throws InitException {
		m_dalPropertiesManager = new DALPropertiesManager();
		m_dalPropertiesManager.init();

        Map theMap = s_specialKeyMap;

        theMap.put("AER.MMD.JD.rmax", getCteDefaultAerJungeRmax());
        theMap.put("AER.ResFile", getCteDefaultFicgranuAer());
        theMap.put("PROFILE_ATM.ResFile", getCteDefaultFicprofilAtmRes());
        theMap.put("DET.Swa", getCteDefaultSpectralDet());
        theMap.put("YS.Swa", getCteDefaultSpectralYs());
        theMap.put("ANG.Mie.NbGauss", getCteDefaultNbmuMie());
        theMap.put("ANG.Mie.ResFile", getCteDefaultFicanglesResMie());
        theMap.put("ANG.Rad.NbGauss", getCteDefaultNbmuLum());
        theMap.put("ANG.Rad.ResFile", getCteDefaultFicanglesResLum());
        theMap.put("MLP.ResFile", getCteDefaultFicgranuMlp());
        theMap.put("PHYTO.JD.rmax", getCteDefaultHydJungeRmax());
        theMap.put("PHYTO.JD.rmin", getCteDefaultHydJungeRmin());
        theMap.put("PHYTO.ResFile", getCteDefaultFicgranuPhyto());
        theMap.put("SED.JD.rmax", getCteDefaultHydJungeRmax());
        theMap.put("SED.JD.rmin", getCteDefaultHydJungeRmin());
        theMap.put("OSOAA.ResFile.vsVZA", getCteDefaultFicsosResVsVza());
        theMap.put("SOS.IGmax", getCteDefaultIgmax());
        theMap.put("SOS.ResFile.Bin", getCteDefaultFicsosResBin());
	}
	
	@Override
	public BigDecimal getCtePi(){ return (BigDecimal)m_dalPropertiesManager.getProperty("CTE_PI", BigDecimal.class); }

	@Override
	public Integer getCteMieDim(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_MIE_DIM", Integer.class); }

	@Override
	public String getCteFicMolSpectralData(){ return (String)m_dalPropertiesManager.getProperty("CTE_FIC_MOL_SPECTRAL_DATA", String.class); }

	@Override
	public String getCteFicEuphDepth(){ return (String)m_dalPropertiesManager.getProperty("CTE_FIC_EUPH_DEPTH", String.class); }

	@Override
	public String getCteDefaultFicanglesResMie(){ return (String)m_dalPropertiesManager.getProperty("CTE_DEFAULT_FICANGLES_RES_MIE", String.class); }

	@Override
	public Integer getCteLendir(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_LENDIR", Integer.class); }

	@Override
	public String getCteDefaultFicgranuAer(){ return (String)m_dalPropertiesManager.getProperty("CTE_DEFAULT_FICGRANU_AER", String.class); }

	@Override
	public String getCteDefaultFicanglesResLum(){ return (String)m_dalPropertiesManager.getProperty("CTE_DEFAULT_FICANGLES_RES_LUM", String.class); }

	@Override
	public Integer getCteLenkeyword(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_LENKEYWORD", Integer.class); }

	@Override
	public BigDecimal getCteSeaTLimit(){ return (BigDecimal)m_dalPropertiesManager.getProperty("CTE_SEA_T_LIMIT", BigDecimal.class); }

	@Override
	public String getCteDefaultFicprofilAtmRes(){ return (String)m_dalPropertiesManager.getProperty("CTE_DEFAULT_FICPROFIL_ATM_RES", String.class); }

	@Override
	public Integer getCteNotDefinedValueInt(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_NOT_DEFINED_VALUE_INT", Integer.class); }

	@Override
	public Integer getCteLencom(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_LENCOM", Integer.class); }

	@Override
	public BigDecimal getCteDefaultHydJungeRmax(){ return (BigDecimal)m_dalPropertiesManager.getProperty("CTE_DEFAULT_HYD_JUNGE_RMAX", BigDecimal.class); }

	@Override
	public Integer getCtePhTest(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_PH_TEST", Integer.class); }

	@Override
	public Integer getCteDefaultIgmax(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_DEFAULT_IGMAX", Integer.class); }

	@Override
	public Integer getCteDefaultOsNs(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_DEFAULT_OS_NS", Integer.class); }

	@Override
	public String getCteFicPhytoSpectralData(){ return (String)m_dalPropertiesManager.getProperty("CTE_FIC_PHYTO_SPECTRAL_DATA", String.class); }

	@Override
	public Integer getCteDefaultNbmuMie(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_DEFAULT_NBMU_MIE", Integer.class); }

	@Override
	public BigDecimal getCteDefaultSpectralYs(){ return (BigDecimal)m_dalPropertiesManager.getProperty("CTE_DEFAULT_SPECTRAL_YS", BigDecimal.class); }

	@Override
	public String getCteDefaultFicgranuMlp(){ return (String)m_dalPropertiesManager.getProperty("CTE_DEFAULT_FICGRANU_MLP", String.class); }

	@Override
	public BigDecimal getCteSeaDepthStep(){ return (BigDecimal)m_dalPropertiesManager.getProperty("CTE_SEA_DEPTH_STEP", BigDecimal.class); }

	@Override
	public BigDecimal getCteDefaultSpectralDet(){ return (BigDecimal)m_dalPropertiesManager.getProperty("CTE_DEFAULT_SPECTRAL_DET", BigDecimal.class); }

	@Override
	public Integer getCteDefaultOsNm(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_DEFAULT_OS_NM", Integer.class); }

	@Override
	public BigDecimal getCteMieNbmuMax(){ return (BigDecimal)m_dalPropertiesManager.getProperty("CTE_MIE_NBMU_MAX", BigDecimal.class); }

	@Override
	public Integer getCteOsNmMax(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_OS_NM_MAX", Integer.class); }

	@Override
	public Integer getCteDefaultNbmuLum(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_DEFAULT_NBMU_LUM", Integer.class); }

	@Override
	public Integer getCteOsNbMax(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_OS_NB_MAX", Integer.class); }

	@Override
	public Integer getCteMaxnbAngExt(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_MAXNB_ANG_EXT", Integer.class); }

	@Override
	public BigDecimal getCteTransOptThickness(){ return (BigDecimal)m_dalPropertiesManager.getProperty("CTE_TRANS_OPT_THICKNESS", BigDecimal.class); }

	@Override
	public BigDecimal getCteWamin(){ return (BigDecimal)m_dalPropertiesManager.getProperty("CTE_WAMIN", BigDecimal.class); }

	@Override
	public Integer getCtePhNu(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_PH_NU", Integer.class); }

	@Override
	public Integer getCteDefaultOsNb(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_DEFAULT_OS_NB", Integer.class); }

	@Override
	public BigDecimal getCteSeuilEcartMus(){ return (BigDecimal)m_dalPropertiesManager.getProperty("CTE_SEUIL_ECART_MUS", BigDecimal.class); }

	@Override
	public Integer getCtePhNq(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_PH_NQ", Integer.class); }

	@Override
	public BigDecimal getCteJungeSlopeCor(){ return (BigDecimal)m_dalPropertiesManager.getProperty("CTE_JUNGE_SLOPE_COR", BigDecimal.class); }

	@Override
	public Integer getCteOsNbmuMax(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_OS_NBMU_MAX", Integer.class); }

	@Override
	public BigDecimal getCtePhSeuilTronca(){ return (BigDecimal)m_dalPropertiesManager.getProperty("CTE_PH_SEUIL_TRONCA", BigDecimal.class); }

	@Override
	public String getCteDefaultFicsosResVsVza(){ return (String)m_dalPropertiesManager.getProperty("CTE_DEFAULT_FICSOS_RES_VS_VZA", String.class); }

	@Override
	public BigDecimal getCteAltToa(){ return (BigDecimal)m_dalPropertiesManager.getProperty("CTE_ALT_TOA", BigDecimal.class); }

	@Override
	public Integer getCteNtSea(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_NT_SEA", Integer.class); }

	@Override
	public Integer getCteStdPressure(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_STD_PRESSURE", Integer.class); }

	@Override
	public String getCteNotDefinedValueDble(){ return (String)m_dalPropertiesManager.getProperty("CTE_NOT_DEFINED_VALUE_DBLE", String.class); }

	@Override
	public Integer getCteNbmaxUserAngles(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_NBMAX_USER_ANGLES", Integer.class); }

	@Override
	public String getCteDefaultFicgranuPhyto(){ return (String)m_dalPropertiesManager.getProperty("CTE_DEFAULT_FICGRANU_PHYTO", String.class); }

	@Override
	public Integer getCteNbanglesMax(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_NBANGLES_MAX", Integer.class); }

	@Override
	public BigDecimal getCteDefaultAerJungeRmax(){ return (BigDecimal)m_dalPropertiesManager.getProperty("CTE_DEFAULT_AER_JUNGE_RMAX", BigDecimal.class); }

	@Override
	public Integer getCteNtAtm(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_NT_ATM", Integer.class); }

	@Override
	public BigDecimal getCteCoefNrmax(){ return (BigDecimal)m_dalPropertiesManager.getProperty("CTE_COEF_NRMAX", BigDecimal.class); }

	@Override
	public Integer getCteLenfic2(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_LENFIC2", Integer.class); }

	@Override
	public Integer getCteLenfic1(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_LENFIC1", Integer.class); }

	@Override
	public String getCteDefaultFicprofilSeaRes(){ return (String)m_dalPropertiesManager.getProperty("CTE_DEFAULT_FICPROFIL_SEA_RES", String.class); }

	@Override
	public BigDecimal getCteDefaultHydJungeRmin(){ return (BigDecimal)m_dalPropertiesManager.getProperty("CTE_DEFAULT_HYD_JUNGE_RMIN", BigDecimal.class); }

	@Override
	public Integer getCteNzMax(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_NZ_MAX", Integer.class); }

	@Override
	public String getCteDefaultFicsosResBin(){ return (String)m_dalPropertiesManager.getProperty("CTE_DEFAULT_FICSOS_RES_BIN", String.class); }

	@Override
	public Integer getCteNbwaMax(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_NBWA_MAX", Integer.class); }

	@Override
	public Integer getCteOsNsMax(){ return (Integer)m_dalPropertiesManager.getProperty("CTE_OS_NS_MAX", Integer.class); }

    @Override
    public void register(JLabel ysSwa_title, final JSpinner ysSwa_spinner) {
        String original_text = ysSwa_title.getText().trim();
        String working_copy = original_text;
        if(original_text.contains("*"))
        {
            working_copy = original_text.replaceAll("\\*", " ");
        }
        working_copy = working_copy.substring(0, working_copy.indexOf(' '));
        final String inner = working_copy;

        if(s_specialKeys.contains(working_copy))
        {
            ysSwa_spinner.addChangeListener(new ChangeListener()
            {
                public void stateChanged(ChangeEvent e)
                {
                    final String myKey = inner;
                    JTextField tf = ((JSpinner.DefaultEditor)ysSwa_spinner.getEditor()).getTextField();

                    boolean areEqual = false;

                    if(ysSwa_spinner.getValue() instanceof BigDecimal)
                    {
                        BigDecimal first = (BigDecimal) s_specialKeyMap.get(myKey);
                        BigDecimal second = (BigDecimal) ysSwa_spinner.getValue();
                        areEqual = (first.compareTo(second) == 0);
                    }
                    else if(!((s_specialKeyMap.get(myKey)).equals(ysSwa_spinner.getValue())))
                    {
                        areEqual = false;
                    }
                    else
                    {
                        areEqual = true;
                    }

                    if(areEqual)
                    {
                        tf.setBackground(new Color(176, 0xFF, 182));
                        if(!tf.getToolTipText().contains("Expert"))
                        {
                            tf.setToolTipText(tf.getToolTipText() + ". Expert field, original value.");
                        }
                        else
                        {
                            String source = tf.getToolTipText().split(". Expert")[0];
                            tf.setToolTipText(source + ". Expert field, original value.");
                        }
                    }
                    else
                    {
                        tf.setBackground(new Color(0xFF, 233, 124));
                        if(!tf.getToolTipText().contains("Expert"))
                        {
                            tf.setToolTipText(tf.getToolTipText() + ". Expert field, modified value.");
                        }
                        else
                        {
                            String source = tf.getToolTipText().split(". Expert")[0];
                            tf.setToolTipText(source + ". Expert field, modified value.");
                        }
                    }
                    if(!tf.getToolTipText().contains("Expert"))
                    {
                        tf.setToolTipText(tf.getToolTipText() + " Expert field.");
                    }
                }
            });

            s_Buttons.put(working_copy, ysSwa_spinner);
        }
    }


}
