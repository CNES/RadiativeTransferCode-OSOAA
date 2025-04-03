package osoaa.usl.forms;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.math.BigDecimal;

import javax.swing.ButtonGroup;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSeparator;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.MaskFormatter;

import org.apache.log4j.Logger;

import osoaa.bll.domain.PhytoProfiletTypeEnum;
import osoaa.bll.domain.SeaBotTypeEnum;
import osoaa.bll.exception.InitException;
import osoaa.bll.preferences.IAtmosphericAndSeaProfiles;
import osoaa.bll.preferences.PreferencesFactory;
import osoaa.bll.properties.PropertiesManager;
import osoaa.usl.common.ui.forms.FormUtils;
import osoaa.usl.common.ui.jspinner.JSpinnerOptionalValue;

import com.jgoodies.forms.factories.DefaultComponentFactory;
import com.jgoodies.forms.factories.FormFactory;
import com.jgoodies.forms.layout.ColumnSpec;
import com.jgoodies.forms.layout.FormLayout;
import com.jgoodies.forms.layout.RowSpec;
import osoaa.usl.common.ui.jspinner.JSpinnerRangedValue;
import osoaa.usl.common.ui.jspinner.SpinnerBigDecimalModel;

public class AtmosphericAndSeaProfilesJPanel extends AbstractForm {
	/**
	 * 
	 */
	private Logger log = Logger.getLogger(this.getClass().getCanonicalName());
	
	private static final long serialVersionUID = 1L;
	private final ButtonGroup buttonGroup = new ButtonGroup();
	private JTextField atmosphericProfile_txtField;

	private IAtmosphericAndSeaProfiles atmosphericAndSeaProfiles;
	private JLabel SEABotType_Title;
	private JSpinner SEABotAlbedo_Spinner;
	private JLabel SEABotType_Desc;
	private JTextField profileLog_TxtField;
	private JSpinner APPressure_Spinner;
	private JSpinner APHR_Spinner;
	private JSpinner APMot_Spinner;
	private JLabel APHR_Desc;
	private JLabel APHR_title;
	private JLabel APPressureTitle;
	private JLabel APMot_title;
	private JSpinner AERWaref_Spinner;
	private JSpinner AERAOTRef_Spinner;
	private JLabel APHA_Desc;
	private JSpinner APHA_Spinner;
	private JLabel APHA_Title;
	private JTextField ProfileSeaResFile_JTxtField;
	private JLabel PHYTOProfilType_Desc;
	private JSpinner PHYTOChl_Spinner;
	private JLabel PHYTOChl_Title;
	private JLabel PHYTOChl_Desc;
	private JLabel PhytoGPChlbg_Title;
	private JLabel PhytoGPDeep_Title;
	private JSpinner PhytoGPDeep_Spinner;
	private JSpinner PhytoGPChlbg_Spinner;
	private JLabel PhytoGPChlbg_Desc;
	private JLabel PhytoGPChlzmax_Title;
	private JSpinner PhytoGPChlzmax_Spinner;
	private JLabel PhytoGPChlzmax_Desc;
	private JLabel PhytoGPDeep_Desc;
	private JTextField PHYTOUserfile_Jtextfield;
	private JLabel PHYTOUserfile_Title;
	private JLabel PHYTOUserfile_Desc;
	private JSpinner SEDCsed_Spinner;
	private JSpinner YSAbs440_Spinner;
	private JSpinner YSSwa_Spinner;
	private JSpinner DETAbs440_Spinner;
	private JSpinner DETSwa_Spinner;
	private JLabel DETSwa_Title;
	private JLabel DETSwa_Desc;
	private JLabel DETAbs440_Desc;
	private JLabel YSSwa_Title;
	private JLabel YSSwa_Desc;
	private JSpinner PhytoGPWidth_Spinner;
	private JLabel PhytoGPWidth_Title;
	private JLabel PhytoGPWidth_Desc;
	private JSpinner SeaSurfAlb_Spinner;
	private JLabel SeaSurfAlb_Title;
	private JComboBox SeaBotType_ComboBox;
	private JLabel Seabottype_title;
	private JLabel profileLog_Title;
	private JLabel profileAtm_title;
	private JRadioButton APMOT_radioBtn;
	private JRadioButton APPressure_radioBtn;
	private JLabel AERWaref_Title;
	private JLabel AERAOTRef_Title;
	private JLabel ProfileSeaResFile_title;
	private JLabel PHYTOProfilType_Title;
	private JLabel SEDCsed_Title;
	private JLabel YSAbs440_Title;
	private JSpinner SeaDepth_Spinner;
	private JLabel SeaDepth_Title;
	private JComboBox PHYTOProfilType_JCombobox;
	private JLabel DETAbs440_Title;
	private JPanel panel_1;
	private JPanel panel_2;

	private Integer currentY=1;

	public void init() {
		atmosphericAndSeaProfiles = PreferencesFactory.getInstance()
				.getAtmosphericAndSeaProfiles();

		SeaSurfAlb_Spinner.setValue(atmosphericAndSeaProfiles.getSeaSurfAlb());
		SeaBotType_ComboBox.setSelectedIndex(atmosphericAndSeaProfiles
				.getSeaBottomType() - 1);
		SEABotAlbedo_Spinner.setValue(atmosphericAndSeaProfiles
				.getSeaBottomAlbedo());

		atmosphericProfile_txtField.setText(atmosphericAndSeaProfiles
				.getAtmosphericProfile());
		profileLog_TxtField.setText(atmosphericAndSeaProfiles.getProfileLog());

		APMOT_radioBtn
				.setSelected(atmosphericAndSeaProfiles.getAPMotSelected());
		APMot_Spinner.setValue(atmosphericAndSeaProfiles.getAPMot());
		APHR_Spinner.setValue(atmosphericAndSeaProfiles.getAPHR());

		APPressure_Spinner.setValue(atmosphericAndSeaProfiles.getAPPressure());
        APPressure_radioBtn.setSelected(atmosphericAndSeaProfiles.getAPPressureSelected());

		AERWaref_Spinner.setValue(atmosphericAndSeaProfiles.getAERWaref());
		AERAOTRef_Spinner.setValue(atmosphericAndSeaProfiles.getAERAOTRef());
		APHA_Spinner.setValue(atmosphericAndSeaProfiles.getAPHA());

		ProfileSeaResFile_JTxtField.setText(atmosphericAndSeaProfiles
				.getProfileSeaResFile());
		SeaDepth_Spinner.setValue(atmosphericAndSeaProfiles.getSeaDepth());

		PHYTOChl_Spinner.setValue(atmosphericAndSeaProfiles.getPhytoChl());
		PHYTOProfilType_JCombobox.setSelectedIndex(atmosphericAndSeaProfiles
				.getPhytoprofileType() - 1);
		PhytoGPChlbg_Spinner.setValue(atmosphericAndSeaProfiles
				.getPhytoGPChlbg());
		PhytoGPChlzmax_Spinner.setValue(atmosphericAndSeaProfiles
				.getPhytoGPChlzmax());
		PhytoGPDeep_Spinner
				.setValue(atmosphericAndSeaProfiles.getPhytoGPDeep());
		PhytoGPWidth_Spinner.setValue(atmosphericAndSeaProfiles
				.getPhytoGPWidth());
		PHYTOUserfile_Jtextfield.setText(atmosphericAndSeaProfiles
				.getPhytoUserFile());

		SEDCsed_Spinner.setValue(atmosphericAndSeaProfiles.getSEDCsed());
		YSAbs440_Spinner.setValue(atmosphericAndSeaProfiles.getYSAbs440());
		YSSwa_Spinner.setValue(atmosphericAndSeaProfiles.getYSSwa());
		DETAbs440_Spinner.setValue(atmosphericAndSeaProfiles.getDETAbs400());
		DETSwa_Spinner.setValue(atmosphericAndSeaProfiles.getDETSwa());

		validateForm();
	}

	private void validateForm() {
		validateAtmosphericProfileForm();
		validateSeaProfileForm();

		notifyFormValidated();
	}

	private String getStr(String mask) {
		return String.format(mask,currentY);
	}

	private void validateSeaProfileFormInitToFalse() {
		FormUtils.setFieldState(false, ProfileSeaResFile_title);
		FormUtils.setFieldState(false, PHYTOChl_Title);
		FormUtils.setFieldState(false, PHYTOProfilType_Title);
		FormUtils.setFieldState(false, PhytoGPChlbg_Title);
		FormUtils.setFieldState(false, PhytoGPChlzmax_Title);
		FormUtils.setFieldState(false, PhytoGPDeep_Title);
		FormUtils.setFieldState(false, PhytoGPWidth_Title);

		FormUtils.setFieldState(false, PHYTOUserfile_Title);

		FormUtils.setFieldState(false, SEDCsed_Title);
		FormUtils.setFieldState(false, YSAbs440_Title);
		FormUtils.setFieldState(false, YSSwa_Title);

		FormUtils.setFieldState(false, DETAbs440_Title);
		FormUtils.setFieldState(false, DETSwa_Title);

	}

	private boolean validateSeaProfileForm() {
		validateSeaProfileFormInitToFalse();
		boolean isValid = true;

		/* Remove PROFILE_SEA.ResFile mandatory
		isValid &= FormUtils.setFieldState(getProfileSeaResFile() == null
				|| getProfileSeaResFile().trim().length() <= 0,
				ProfileSeaResFile_title);
		*/
		isValid &= FormUtils.setFieldState(getPhytoChl() == null,
				PHYTOChl_Title);
		if (getPhytoChl() != null) {
			if (getPhytoChl().doubleValue() == 0.0) {
				isValid &= FormUtils.setFieldState(
						getPhytoprofileType() == null, PHYTOProfilType_Title);
				if (getPhytoprofileType() != null && getPhytoprofileType() == 2) {
					isValid &= FormUtils.setFieldState(
							getPhytoGPChlbg() == null, PhytoGPChlbg_Title);
					isValid &= FormUtils.setFieldState(
							getPhytoGPDeep() == null, PhytoGPDeep_Title);
					isValid &= FormUtils.setFieldState(
							getPhytoGPWidth() == null, PhytoGPWidth_Title);

				} else if (getPhytoprofileType() != null
						&& getPhytoprofileType() == 3) {
					isValid &= FormUtils.setFieldState(
							getPhytoUserFile() == null
									|| getPhytoUserFile().trim().length() <= 0,
							PHYTOUserfile_Title);
				}
			}
		}

		isValid &= FormUtils.setFieldState(getSEDCsed() == null, SEDCsed_Title);
		isValid &= FormUtils.setFieldState(getYSAbs440() == null,
				YSAbs440_Title);
		/* Remove YS.Swa mandatory */
		if (getYSAbs440() != null && getYSAbs440().doubleValue() > 0.0) {
			isValid &= FormUtils.setFieldState(getYSSwa() == null, YSSwa_Title);
		}
		

		isValid &= FormUtils.setFieldState(getDETAbs400() == null,
				DETAbs440_Title);
		
		/* Remove DET.Swa mandatory */
		if (getDETSwa() != null && getDETAbs400().doubleValue() > 0.0) {
			isValid &= FormUtils.setFieldState(getDETSwa() == null,
					DETSwa_Title);
		}
		return isValid;
	}

	private void validateAtmosphericProfileFormToFalse() {
		FormUtils.setFieldState(false, SeaSurfAlb_Title);
		FormUtils.setFieldState(false, Seabottype_title);
		FormUtils.setFieldState(false, SeaSurfAlb_Title);

		FormUtils.setFieldState(false, profileAtm_title);

		FormUtils.setFieldState(false, APMot_title);
		FormUtils.setFieldState(false, APHR_title);
		FormUtils.setFieldState(false, APPressureTitle);
		FormUtils.setFieldState(false, APMOT_radioBtn);
		FormUtils.setFieldState(false, APPressure_radioBtn);

		FormUtils.setFieldState(false, AERWaref_Title);
		FormUtils.setFieldState(false, AERAOTRef_Title);
		FormUtils.setFieldState(false, APHA_Title);
	}

	private boolean validateAtmosphericProfileForm() {
		validateAtmosphericProfileFormToFalse();
		boolean isValid = true;
		isValid &= FormUtils.setFieldState(getSeaSurfAlb() == null,
				SeaSurfAlb_Title);
		isValid &= FormUtils.setFieldState(getSeaBottomType() == null,
				Seabottype_title);
		isValid &= FormUtils.setFieldState(getSeaBottomAlbedo() == null,
				SeaSurfAlb_Title);

		/* remove PROFILE_ATM.ResFile mandatory 
		isValid &= FormUtils.setFieldState(getAtmosphericProfile() == null
				|| getAtmosphericProfile().trim().length() <= 0,
				profileAtm_title);
		*/

		if (APMOT_radioBtn.isSelected()) {
			isValid &= FormUtils.setFieldState(getAPMot() == null, APMot_title);
			if (getAPMot() != null && getAPMot().doubleValue() >= 0.0001) {
				isValid &= FormUtils.setFieldState(getAPHR() == null,
						APHR_title);
			}
		} else if (APPressure_radioBtn.isSelected()) {
			isValid &= FormUtils.setFieldState(getAPPressure() == null,
					APPressureTitle);
		} else {
			isValid &= FormUtils.setFieldState(true, APMOT_radioBtn);
			isValid &= FormUtils.setFieldState(true, APPressure_radioBtn);
		}

		isValid &= FormUtils.setFieldState(getAERWaref() == null,
				AERWaref_Title);
		isValid &= FormUtils.setFieldState(getAERAOTRef() == null,
				AERAOTRef_Title);
		if (getAERAOTRef() != null && getAERAOTRef().doubleValue() >= 0.0001) {
			isValid &= FormUtils.setFieldState(getAPHA() == null, APHA_Title);
		}

		return isValid;
	}

	public String getProfileSeaResFile() {
		return ProfileSeaResFile_JTxtField.getText();
	}

	private void saveProfileSeaResFile() {
		if (getProfileSeaResFile() != null) {
			atmosphericAndSeaProfiles
					.setProfileSeaResFile(getProfileSeaResFile());
		}
	}

	public BigDecimal getSeaDepth() {
		SeaDepth_Spinner.validate();
		try {
			return (BigDecimal) SeaDepth_Spinner.getValue();
		} catch (java.lang.ClassCastException e) {
			return null;
		}
	}

	private void saveSeaDepth() {
		if (getSeaDepth() != null) {
			atmosphericAndSeaProfiles.setSeaDepth(getSeaDepth());
		}
	}

	public BigDecimal getPhytoChl() {
		PHYTOChl_Spinner.validate();
		return (BigDecimal) PHYTOChl_Spinner.getValue();
	}

	private void savePhytoChl() {
		if (getPhytoChl() != null) {
			atmosphericAndSeaProfiles.setPhytoChl(getPhytoChl());
		}
	}

	public Integer getPhytoprofileType() {
		PHYTOProfilType_JCombobox.validate();
		return PHYTOProfilType_JCombobox.getModel().getSelectedItem() == null ? null
				: ((PhytoProfiletTypeEnum) PHYTOProfilType_JCombobox.getModel()
						.getSelectedItem()).ordinal() + 1;
	}

	private void savePhytoprofileType() {
		if (getPhytoprofileType() != null) {
			atmosphericAndSeaProfiles
					.setPhytoprofileType(getPhytoprofileType());
		}
	}

	public BigDecimal getPhytoGPChlbg() {
		PhytoGPChlbg_Spinner.validate();
		return (BigDecimal) PhytoGPChlbg_Spinner.getValue();
	}

	private void savePhytoGPChlbg() {
		if (getPhytoGPChlbg() != null) {
			atmosphericAndSeaProfiles.setPhytoGPChlbg(getPhytoGPChlbg());
		}
	}

	public BigDecimal getPhytoGPChlzmax() {
		PhytoGPChlzmax_Spinner.validate();
		return (BigDecimal) PhytoGPChlzmax_Spinner.getValue();
	}

	private void savePhytoGPChlzmax() {
		if (getPhytoGPChlzmax() != null) {
			atmosphericAndSeaProfiles.setPhytoGPChlzmax(getPhytoGPChlzmax());
		}
	}

	public BigDecimal getPhytoGPDeep() {
		PhytoGPDeep_Spinner.validate();
		return (BigDecimal) PhytoGPDeep_Spinner.getValue();
	}

	private void savePhytoGPDeep() {
		if (getPhytoGPDeep() != null) {
			atmosphericAndSeaProfiles.setPhytoGPDeep(getPhytoGPDeep());
		}
	}

	public BigDecimal getPhytoGPWidth() {
		PhytoGPWidth_Spinner.validate();
		return (BigDecimal) PhytoGPWidth_Spinner.getValue();
	}

	private void savePhytoGPWidth() {
		if (getPhytoGPWidth() != null) {
			atmosphericAndSeaProfiles.setPhytoGPWidth(getPhytoGPWidth());
		}
	}

	public String getPhytoUserFile() {
		return PHYTOUserfile_Jtextfield.getText();
	}

	private void savePhytoUserFile() {
		if (getPhytoUserFile() != null) {
			atmosphericAndSeaProfiles.setPhytoUserFile(getPhytoUserFile());
		}
	}

	public BigDecimal getSEDCsed() {
		SEDCsed_Spinner.validate();
		return (BigDecimal) SEDCsed_Spinner.getValue();
	}

	private void saveSEDCsed() {
		if (getSEDCsed() != null) {
			atmosphericAndSeaProfiles.setSEDCsed(getSEDCsed());
		}
	}

	public BigDecimal getYSAbs440() {
		YSAbs440_Spinner.validate();
		return (BigDecimal) YSAbs440_Spinner.getValue();
	}

	private void saveYSAbs440() {
		if (getYSAbs440() != null) {
			atmosphericAndSeaProfiles.setYSAbs440(getYSAbs440());
		}
	}

	public BigDecimal getYSSwa() {
		if (YSSwa_Spinner.getValue()!=null) {
			YSSwa_Spinner.validate();
		}
		return (BigDecimal) YSSwa_Spinner.getValue();
	}

	private void saveYSSwa() {
		if (getYSSwa() != null) {
			atmosphericAndSeaProfiles.setYSSwa(getYSSwa());
		}
	}

	public BigDecimal getDETAbs400() {
		DETAbs440_Spinner.validate();
		return (BigDecimal) DETAbs440_Spinner.getValue();
	}

	private void saveDETAbs400() {
		if (getDETAbs400() != null) {
			atmosphericAndSeaProfiles.setDETAbs400(getDETAbs400());
		}
	}

	public BigDecimal getDETSwa() {
		DETSwa_Spinner.validate();
		return (BigDecimal) DETSwa_Spinner.getValue();
	}

	private void saveDETSwa() {
		if (getDETSwa() != null) {
			atmosphericAndSeaProfiles.setDETSwa(getDETSwa());
		}
	}

	public BigDecimal getSeaSurfAlb() {
		SeaSurfAlb_Spinner.validate();
		return (BigDecimal) SeaSurfAlb_Spinner.getValue();
	}

	private void saveSeaSurfAlb() {
		if (getSeaSurfAlb() != null) {
			atmosphericAndSeaProfiles.setSeaSurfAlb(getSeaSurfAlb());
		}
	}

	public Integer getSeaBottomType() {
		SEABotAlbedo_Spinner.validate();
		return SeaBotType_ComboBox.getModel().getSelectedItem() == null ? null
				: ((SeaBotTypeEnum) SeaBotType_ComboBox.getModel()
						.getSelectedItem()).ordinal() + 1;
	}

	private void saveSeaBottomType() {
		if (getSeaBottomType() != null) {
			atmosphericAndSeaProfiles.setSeaBottomType(getSeaBottomType());
		}
	}

	public BigDecimal getSeaBottomAlbedo() {
		SEABotAlbedo_Spinner.validate();
		return (BigDecimal) SEABotAlbedo_Spinner.getValue();
	}

	private void saveSeaBottomAlbedo() {
		if (getSeaBottomAlbedo() != null) {
			atmosphericAndSeaProfiles.setSeaBottomAlbedo(getSeaBottomAlbedo());
		}
	}

	public String getAtmosphericProfile() {
		return atmosphericProfile_txtField.getText();
	}

	private void saveAtmosphericProfile() {
		if (getAtmosphericProfile() != null) {
			atmosphericAndSeaProfiles
					.setAtmosphericProfile(getAtmosphericProfile());
		}
	}

	public String getProfileLog() {
		return profileLog_TxtField.getText();
	}

	private void saveProfileLog() {
		if (getProfileLog() != null) {
			atmosphericAndSeaProfiles.setProfileLog(getProfileLog());
		}
	}

	public BigDecimal getAPMot() {
		APMot_Spinner.validate();
		return (BigDecimal) APMot_Spinner.getValue();
	}

	private void saveAPMot() {
		if (getAPMot() != null) {
			atmosphericAndSeaProfiles.setAPMot(getAPMot());
		}
	}

	public BigDecimal getAPHR() {
		APHR_Spinner.validate();
		return (BigDecimal) APHR_Spinner.getValue();
	}

	private void saveAPHR() {
		if (getAPHR() != null) {
			atmosphericAndSeaProfiles.setAPHR(getAPHR());
		}
	}

	public BigDecimal getAPPressure() {
		APPressure_Spinner.validate();
		return (BigDecimal) APPressure_Spinner.getValue();
	}

	private void saveAPPressure() {
		if (getAPPressure() != null) {
			atmosphericAndSeaProfiles.setAPPressure(getAPPressure());
		}
	}

	public BigDecimal getAERWaref() {
		AERWaref_Spinner.validate();
		return (BigDecimal) AERWaref_Spinner.getValue();
	}

	private void saveAERWaref() {
		if (getAERWaref() != null) {
			atmosphericAndSeaProfiles.setAERWaref(getAERWaref());
		}
	}

	public BigDecimal getAERAOTRef() {
		AERAOTRef_Spinner.validate();
		return (BigDecimal) AERAOTRef_Spinner.getValue();
	}

	private void saveAERAOTRef() {
		if (getAERAOTRef() != null) {
			atmosphericAndSeaProfiles.setAERAOTRef(getAERAOTRef());
		}
	}

	public BigDecimal getAPHA() {
		APHA_Spinner.validate();
		return (BigDecimal) APHA_Spinner.getValue();
	}

	private void saveAPHA() {
		atmosphericAndSeaProfiles.setAPHA(getAPHA());
	}

	private void saveAPMotSelected(boolean isSelected) {
		atmosphericAndSeaProfiles.setAPMotSelected(isSelected);
	}

    private void saveAPPressureSelected(boolean isSelected) {
        atmosphericAndSeaProfiles.setAPPressureSelected(isSelected);
    }

	protected MaskFormatter createFormatter(String s) {
		MaskFormatter formatter = null;
		try {
			formatter = new MaskFormatter(s);
		} catch (java.text.ParseException exc) {
			log.error("formatter is bad: " + exc.getMessage());
			System.exit(-1);
		}
		return formatter;
	}

	/**
	 * Create the panel.
	 */
	public AtmosphericAndSeaProfilesJPanel() {
		super("Atmospheric and maritime profiles");

		getFormFieldsPanel().setLayout(
				new FormLayout(new ColumnSpec[] {
						FormFactory.RELATED_GAP_COLSPEC,
						ColumnSpec.decode("max(67dlu;default)"),
						FormFactory.RELATED_GAP_COLSPEC,
						ColumnSpec.decode("max(119dlu;default):grow"),
						FormFactory.RELATED_GAP_COLSPEC,
						FormFactory.DEFAULT_COLSPEC,
						FormFactory.RELATED_GAP_COLSPEC,
						ColumnSpec.decode("max(4dlu;default)"),
						ColumnSpec.decode("default:grow"), }, new RowSpec[] {
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						RowSpec.decode("max(14dlu;default)"),
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						FormFactory.DEFAULT_ROWSPEC,
						FormFactory.RELATED_GAP_ROWSPEC,
						RowSpec.decode("default:grow"),
						FormFactory.RELATED_GAP_ROWSPEC, }));

		JLabel lblNewLabel_28 = new JLabel(
				"    |--> Reflectance limit conditions parameters");
		lblNewLabel_28.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblNewLabel_28.setForeground(Color.BLUE);
		lblNewLabel_28.setHorizontalAlignment(SwingConstants.LEFT);
		getFormFieldsPanel().add(lblNewLabel_28, "2, 1, 3, 1");

		JSeparator separator_4 = new JSeparator();
		separator_4.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		getFormFieldsPanel().add(separator_4, "9, 1");

		SeaSurfAlb_Title = DefaultComponentFactory.getInstance().createLabel(
				"SEA.SurfAlb *:");
		getFormFieldsPanel().add(SeaSurfAlb_Title, "2, 3, right, default");

		SeaSurfAlb_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.0"), new BigDecimal("0.0"), new BigDecimal("1.0"), new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(SeaSurfAlb_Title, SeaSurfAlb_Spinner);
		SeaSurfAlb_Spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSeaSurfAlb();
				validateForm();
			}
		});
		getFormFieldsPanel().add(SeaSurfAlb_Spinner, "4, 3");

		JLabel lblSeaSurfaceAlbedo = DefaultComponentFactory.getInstance()
				.createLabel("Lambertian reflectance of the foam at the simulation wavelength (albedo of the foam at the sea surface)");
		getFormFieldsPanel().add(lblSeaSurfaceAlbedo, "9, 3");

		Seabottype_title = DefaultComponentFactory.getInstance().createLabel(
				"SEA.BotType *:");
		getFormFieldsPanel().add(Seabottype_title, "2, 5, right, default");

		SeaBotType_ComboBox = new JComboBox();
		SeaBotType_ComboBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e_) {
				JComboBox cb = (JComboBox) e_.getSource();
				int selIndex = cb.getSelectedIndex();
				setSEABotTypeVisible(selIndex == 0);

				saveSeaBottomType();
				validateForm();
			}
		});
		SeaBotType_ComboBox.setModel(new DefaultComboBoxModel(SeaBotTypeEnum
				.values()));
		getFormFieldsPanel().add(SeaBotType_ComboBox, "4, 5, fill, default");

		JLabel lblTypeOfSea = DefaultComponentFactory.getInstance()
				.createLabel("Type of seabed composition");
		getFormFieldsPanel().add(lblTypeOfSea, "9, 5");

		SEABotType_Title = new JLabel("SEA.BotAlb *:");
		SEABotType_Title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(SEABotType_Title, "2, 7");

		SEABotAlbedo_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.0"), new BigDecimal("0.0"), new BigDecimal("1.0"), new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(SEABotType_Title, SEABotAlbedo_Spinner);
		SEABotAlbedo_Spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSeaBottomAlbedo();
				validateForm();
			}
		});
		getFormFieldsPanel().add(SEABotAlbedo_Spinner, "4, 7");

		SEABotType_Desc = new JLabel(
				"Seabed albedo at the simulation wavelength (lambertian component)");
		SEABotType_Desc
				.setToolTipText("Sea bottom albedo for the wavelength of radiance calculation (lambertian component)");
		getFormFieldsPanel().add(SEABotType_Desc, "9, 7");

		JLabel lblAtmosphericProfile = DefaultComponentFactory.getInstance()
				.createTitle("ATMOSPHERIC PROFILE");
		lblAtmosphericProfile.setForeground(Color.BLUE);
		lblAtmosphericProfile.setFont(new Font("Tahoma", Font.BOLD, 11));
		getFormFieldsPanel().add(lblAtmosphericProfile, "2, 11");

		JSeparator separator_1 = new JSeparator();
		separator_1.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		getFormFieldsPanel().add(separator_1, "4, 11, 6, 1");

		JLabel lblAtmosphericProfile_1 = new JLabel(
				"    |--> Atmospheric profile definition parameters");
		lblAtmosphericProfile_1.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblAtmosphericProfile_1.setForeground(Color.BLUE);
		getFormFieldsPanel().add(lblAtmosphericProfile_1, "2, 13, 6, 1");

		JSeparator separator = new JSeparator();
		separator.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		getFormFieldsPanel().add(separator, "9, 13");

		profileAtm_title = new JLabel("PROFILE_ATM.ResFile:");
		profileAtm_title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(profileAtm_title, "2, 15, right, default");

		atmosphericProfile_txtField = new JTextField();
		atmosphericProfile_txtField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				saveAtmosphericProfile();
				validateForm();
			}
		});
		getFormFieldsPanel().add(atmosphericProfile_txtField,
				"4, 15, fill, default");
		atmosphericProfile_txtField.setColumns(10);

		JLabel PROFILE_ATM_Desc = new JLabel(
				"Filename of the output of the atmospheric profile (result file)");
		getFormFieldsPanel().add(PROFILE_ATM_Desc, "9, 15");

		profileLog_Title = new JLabel("PROFILE.Log :");
		getFormFieldsPanel().add(profileLog_Title, "2, 17, right, default");

		profileLog_TxtField = new JTextField();
		profileLog_TxtField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
				saveProfileLog();
				validateForm();
			}
		});
		getFormFieldsPanel().add(profileLog_TxtField, "4, 17, fill, default");
		profileLog_TxtField.setColumns(10);

		JLabel lblNewLabel_27 = new JLabel(
				"Name of log file for OSOAA_PROFILE calculations");
		getFormFieldsPanel().add(lblNewLabel_27, "9, 17");

		JSeparator separator_7 = new JSeparator();
		separator_7.setForeground(new Color(230, 230, 250));
		getFormFieldsPanel().add(separator_7, "2, 19, 2, 1");

		JLabel lblMolecules = DefaultComponentFactory.getInstance()
				.createTitle("|--> Molecules");
		lblMolecules.setFont(new Font("Tahoma", Font.BOLD, 11));
		getFormFieldsPanel().add(lblMolecules, "4, 19");

		JSeparator separator_3 = new JSeparator();
		separator_3.setForeground(new Color(230, 230, 250));
		getFormFieldsPanel().add(separator_3, "9, 19");

		APMot_title = new JLabel("AP.MOT *:");
		getFormFieldsPanel().add(APMot_title, "2, 21, right, default");

		panel_1 = new JPanel();
		getFormFieldsPanel().add(panel_1, "4, 21, fill, fill");
		panel_1.setLayout(new BorderLayout(0, 0));

		APMOT_radioBtn = new JRadioButton("");
		panel_1.add(APMOT_radioBtn, BorderLayout.WEST);
		APMOT_radioBtn.setToolTipText("Check to use the molecular value");
		APMOT_radioBtn.setFont(new Font("Tahoma", Font.ITALIC, 11));

        APMOT_radioBtn.setSelected(true);
		APMOT_radioBtn.setBackground(Color.WHITE);
		APMOT_radioBtn.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent arg0) {
                int state = arg0.getStateChange () ;
                if (state == ItemEvent.SELECTED)
                {
                    setAPMotValueVisible(true);
                    saveAPMot();
                    // atmosphericAndSeaProfiles.setAPMotSelected(true);
                    checkAPHRValueEnable();
                    validateForm();
                }
            }
        });
		buttonGroup.add(APMOT_radioBtn);

		APMot_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.000"), new BigDecimal("0.000"), null, new BigDecimal("0.001")));
        PropertiesManager.getInstance().register(APMot_title, APMot_Spinner);
		APMot_Spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveAPMot();
				checkAPHRValueEnable();
				validateForm();
			}
		});
		panel_1.add(APMot_Spinner);

		JLabel lblNewJgoodiesLabel_1 = DefaultComponentFactory
				.getInstance()
				.createLabel(
						"Molecular optical thickness for the atmospheric profile (for the radiance simulation wavelength).");
		getFormFieldsPanel().add(lblNewJgoodiesLabel_1, "9, 21");

		APPressureTitle = new JLabel("AP.Pressure *:");
		APPressureTitle.setEnabled(false);
		APPressureTitle.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(APPressureTitle, "2, 23");

		panel_2 = new JPanel();
		getFormFieldsPanel().add(panel_2, "4, 23, fill, fill");
		panel_2.setLayout(new BorderLayout(0, 0));

		APPressure_radioBtn = new JRadioButton("");
		panel_2.add(APPressure_radioBtn, BorderLayout.WEST);
		APPressure_radioBtn.setFont(new Font("Tahoma", Font.ITALIC, 11));
		APPressure_radioBtn.setBackground(Color.WHITE);
		APPressure_radioBtn.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent arg0) {
                int state = arg0.getStateChange () ;
                if (state == ItemEvent.SELECTED)
                {
                    setAPPressureValueVisible(true);
                    saveAPPressure();
                    // atmosphericAndSeaProfiles.setAPPressureSelected(true);
                    checkAPHRValueEnable();
                    validateForm();
                }
			}
		});
		buttonGroup.add(APPressure_radioBtn);

		APPressure_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal(0),
                new BigDecimal(0), null, new BigDecimal(1)));
        PropertiesManager.getInstance().register(APPressureTitle, APPressure_Spinner);
		APPressure_Spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveAPPressure();
				checkAPHRValueEnable();
				validateForm();
			}
		});
		panel_2.add(APPressure_Spinner, BorderLayout.CENTER);
		APPressure_Spinner.setEnabled(false);

		JLabel lblNewLabel_12 = new JLabel(
				"Atmospheric pressure at sea level (mbar)");
		getFormFieldsPanel().add(lblNewLabel_12, "9, 23");

		APHR_title = new JLabel("AP.HR*:");
		APHR_title.setEnabled(false);
		APHR_title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(APHR_title, "2, 25");

		APHR_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.0"), new BigDecimal(
                "0.0"), null, new BigDecimal("0.1")));
        PropertiesManager.getInstance().register(APHR_title, APHR_Spinner);
		APHR_Spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveAPHR();
				validateForm();
			}
		});

		APHR_Spinner.setEnabled(false);
		getFormFieldsPanel().add(APHR_Spinner, "4, 25");

		APHR_Desc = new JLabel("Molecular heigth scale (km)");
		APHR_Desc.setEnabled(false);
		getFormFieldsPanel().add(APHR_Desc, "9, 25");

		JSeparator separator_6 = new JSeparator();
		separator_6.setForeground(new Color(230, 230, 250));
		getFormFieldsPanel().add(separator_6, "2, 27");

		JLabel lblAerosols = new JLabel("|--> Aerosols");
		lblAerosols.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblAerosols.setForeground(Color.BLUE);
		getFormFieldsPanel().add(lblAerosols, "4, 27");

		JSeparator separator_5 = new JSeparator();
		separator_5.setForeground(new Color(230, 230, 250));
		getFormFieldsPanel().add(separator_5, "9, 27");

		AERWaref_Title = new JLabel("AER.Waref *:");
		AERWaref_Title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(AERWaref_Title, "2, 29");

		AERWaref_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.000"), new BigDecimal("0.000"), null, new BigDecimal("0.001")));
        PropertiesManager.getInstance().register(AERWaref_Title, AERWaref_Spinner);
		AERWaref_Spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveAERWaref();
				validateForm();
			}
		});
		getFormFieldsPanel().add(AERWaref_Spinner, "4, 29");

		JLabel lblNewLabel_2 = new JLabel(
				"Reference wavelength (\u00B5m) for the aerosol optical thickness (AER.AOTref)");
		lblNewLabel_2
				.setToolTipText("Reference wavelength (\u00B5m) related to the aerosol optical thickness for the reference wavelength (AER.AOTref)");
		getFormFieldsPanel().add(lblNewLabel_2, "9, 29");

		AERAOTRef_Title = new JLabel("AER.AOTref *:");
		AERAOTRef_Title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(AERAOTRef_Title, "2, 31");

		AERAOTRef_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.000"), new BigDecimal("0.000"), null, new BigDecimal("0.001")));
        PropertiesManager.getInstance().register(AERAOTRef_Title, AERAOTRef_Spinner);
		AERAOTRef_Spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				BigDecimal limit = new BigDecimal("0.0001");
				if (getAERAOTRef() != null && (getAERAOTRef().doubleValue() >= limit.doubleValue())) {
					setAPHAVisible(true);
				} else {
					setAPHAVisible(false);
				}
				saveAERAOTRef();
				validateForm();
			}
		});
		getFormFieldsPanel().add(AERAOTRef_Spinner, "4, 31");

		JLabel lblNewLabel_3 = new JLabel(
				"Aerosol optical thickness for the reference wavelength ");
		getFormFieldsPanel().add(lblNewLabel_3, "9, 31");

		APHA_Title = new JLabel("AP.HA*:");
		APHA_Title.setEnabled(false);
		APHA_Title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(APHA_Title, "2, 33, default, top");

		APHA_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.0"), new BigDecimal(
                "0.0"), null, new BigDecimal("0.1")));
        PropertiesManager.getInstance().register(APHA_Title, APHA_Spinner);
		APHA_Spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent arg0) {
				saveAPHA();
				validateForm();
			}
		});
		APHA_Spinner.setEnabled(false);

		getFormFieldsPanel().add(APHA_Spinner, "4, 33");

		APHA_Desc = new JLabel("Aerosol height scale (in km)");
		APHA_Desc.setEnabled(false);
		getFormFieldsPanel().add(APHA_Desc, "9, 33");

		JLabel lblSeaProfile = new JLabel("SEA PROFILE");
		lblSeaProfile.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblSeaProfile.setForeground(Color.BLUE);
		getFormFieldsPanel().add(lblSeaProfile, "2, 37");

		JSeparator separator_8 = new JSeparator();
		separator_8.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		getFormFieldsPanel().add(separator_8, "4, 37, 6, 1");

		ProfileSeaResFile_title = new JLabel("PROFILE_SEA.ResFile :");
		getFormFieldsPanel().add(ProfileSeaResFile_title,
				"2, 39, right, default");

		ProfileSeaResFile_JTxtField = new JTextField();
		ProfileSeaResFile_JTxtField.addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent arg0) {
				saveProfileSeaResFile();
				validateForm();
			}
		});
		getFormFieldsPanel().add(ProfileSeaResFile_JTxtField,
				"4, 39, fill, default");
		ProfileSeaResFile_JTxtField.setColumns(10);

		JLabel lblNewLabel_4 = new JLabel(
				"Filename of the output of the sea profile ");
		getFormFieldsPanel().add(lblNewLabel_4, "9, 39");

		SeaDepth_Title = new JLabel("SEA.Depth :");
		SeaDepth_Title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(SeaDepth_Title, "2, 41");

		SeaDepth_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal(0), new BigDecimal(
                0), null, new BigDecimal(1)));
        PropertiesManager.getInstance().register(SeaDepth_Title, SeaDepth_Spinner);
		SeaDepth_Spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSeaDepth();
				validateForm();
			}
		});
		getFormFieldsPanel().add(SeaDepth_Spinner, "4, 41");

		JLabel lblNewLabel_13 = new JLabel("Sea depth value  (meters)");
		lblNewLabel_13
				.setToolTipText("If not defined, euphotic depth will be used from Morel tabulated data ");
		getFormFieldsPanel().add(lblNewLabel_13, "9, 41");

		JLabel lblPhytoplancton = new JLabel("    |--> Phytoplancton");
		lblPhytoplancton.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblPhytoplancton.setForeground(Color.BLUE);
		getFormFieldsPanel().add(lblPhytoplancton, "2, 43, 6, 1");

		JSeparator separator_11 = new JSeparator();
		separator_11.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		getFormFieldsPanel().add(separator_11, "9, 43");

		// ----------------
		PHYTOProfilType_Title = new JLabel("PHYTO.ProfilType* :");
		getFormFieldsPanel()
				.add(PHYTOProfilType_Title, "2, 45, right, default");

		PHYTOProfilType_JCombobox = new JComboBox();
		PHYTOProfilType_JCombobox.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e_) {
				JComboBox cb = (JComboBox) e_.getSource();
				int selIndex = cb.getSelectedIndex();
				setHomogeneousPorfileVisible(selIndex == 0);
				setPhytoGaussianProfileVisible(selIndex == 1);
				setPhytoUserProfileVisible(selIndex == 2);

				savePhytoprofileType();
				validateForm();
			}
		});
		PHYTOProfilType_JCombobox.setModel(new DefaultComboBoxModel(
				PhytoProfiletTypeEnum.values()));
		getFormFieldsPanel().add(PHYTOProfilType_JCombobox,
				"4, 45, fill, default");

		PHYTOProfilType_Desc = new JLabel("Type of chlorophyll profile ");
		getFormFieldsPanel().add(PHYTOProfilType_Desc, "9, 45");
		// ----------------
		PHYTOChl_Title = new JLabel("PHYTO.Chl :");
		PHYTOChl_Title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(PHYTOChl_Title, "2, 47");

		PHYTOChl_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"), new BigDecimal("0.00"), null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(PHYTOChl_Title, PHYTOChl_Spinner);
		PHYTOChl_Spinner.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                PHYTOChl_Spinner.validate();
                BigDecimal limit = new BigDecimal("0.0");
                try {
                    BigDecimal val = (BigDecimal) PHYTOChl_Spinner.getValue();
                    if (val.doubleValue() >= limit.doubleValue()) {
                        setPhytoPrfilTypeVisible(true);
                    } else {
                        setPhytoPrfilTypeVisible(false);
                    }
                } catch (Exception e_) {
                    log.warn(e_);
                }

                savePhytoChl();
                validateForm();
            }

        });
		getFormFieldsPanel().add(PHYTOChl_Spinner, "4, 47");

		PHYTOChl_Desc = new JLabel(
				"Chlorophyll concentration at sea surface (mg.m-3)");
		PHYTOChl_Desc
				.setToolTipText("Chlorophylle concentration at sea surface (mg.m-3)");
		getFormFieldsPanel().add(PHYTOChl_Desc, "9, 47");

		// ----------

		PhytoGPChlbg_Title = new JLabel("PHYTO.GP.Chlbg :");
		PhytoGPChlbg_Title.setEnabled(false);
		PhytoGPChlbg_Title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(PhytoGPChlbg_Title, "2, 49");

		PhytoGPChlbg_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"),
                new BigDecimal("0.00"), null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(PhytoGPChlbg_Title, PhytoGPChlbg_Spinner);
		PhytoGPChlbg_Spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePhytoGPChlbg();
				validateForm();
			}
		});
		PhytoGPChlbg_Spinner.setEnabled(false);
		getFormFieldsPanel().add(PhytoGPChlbg_Spinner, "4, 49");

		PhytoGPChlbg_Desc = new JLabel("Chlorophyll concentration background (mg.m-3)");
		PhytoGPChlbg_Desc.setEnabled(false);
		getFormFieldsPanel().add(PhytoGPChlbg_Desc, "9, 49");

		/* PHYTO.GP.Chlzmax */
		currentY = 51;

		PhytoGPChlzmax_Title = new JLabel("PHYTO.GP.Chlzmax :");
		PhytoGPChlzmax_Title.setEnabled(false);
		PhytoGPChlzmax_Title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(PhytoGPChlzmax_Title, getStr("2, %d"));

		PhytoGPChlzmax_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"),
                new BigDecimal("0.00"), null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(PhytoGPChlzmax_Title, PhytoGPChlzmax_Spinner);
		PhytoGPChlzmax_Spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePhytoGPChlzmax();
				validateForm();
			}
		});
		PhytoGPChlzmax_Spinner.setEnabled(false);
		getFormFieldsPanel().add(PhytoGPChlzmax_Spinner, getStr("4, %d"));

		PhytoGPChlzmax_Desc = new JLabel("Maximum chlorophyll-a concentration (mg.m-3) in the water column (reached at depth PHYTO.GP.Deep)");
		PhytoGPChlzmax_Desc.setEnabled(false);
		getFormFieldsPanel().add(PhytoGPChlzmax_Desc, getStr("9, %d"));

		/* ================ */
		currentY += 2;
		PhytoGPDeep_Title = new JLabel("PHYTO.GP.Deep :");
		PhytoGPDeep_Title.setEnabled(false);
		PhytoGPDeep_Title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(PhytoGPDeep_Title, getStr("2, %d"));

		PhytoGPDeep_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal(0),
                new BigDecimal(0), null, new BigDecimal(1)));
        PropertiesManager.getInstance().register(PhytoGPDeep_Title, PhytoGPDeep_Spinner);
		PhytoGPDeep_Spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePhytoGPDeep();
				validateForm();
			}
		});
		PhytoGPDeep_Spinner.setEnabled(false);
		getFormFieldsPanel().add(PhytoGPDeep_Spinner, getStr("4, %d"));

		PhytoGPDeep_Desc = new JLabel("Depth of the maximum chlorophyll-a value (m)");
		PhytoGPDeep_Desc.setEnabled(false);
		getFormFieldsPanel().add(PhytoGPDeep_Desc, getStr("9, %d"));

		currentY += 2;
		PhytoGPWidth_Title = new JLabel("PHYTO.GP.Width :");
		PhytoGPWidth_Title.setEnabled(false);
		PhytoGPWidth_Title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(PhytoGPWidth_Title, getStr("2, %d"));

		PhytoGPWidth_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.0"),
                new BigDecimal("0.0"), null, new BigDecimal("0.1")));
        PropertiesManager.getInstance().register(PhytoGPWidth_Title, PhytoGPWidth_Spinner);
		PhytoGPWidth_Spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				savePhytoGPWidth();
				validateForm();
			}
		});
		PhytoGPWidth_Spinner.setEnabled(false);
		getFormFieldsPanel().add(PhytoGPWidth_Spinner, getStr("4, %d"));

		PhytoGPWidth_Desc = new JLabel("Half-width of the chlorophyll gaussian peak (m)");
		PhytoGPWidth_Desc.setEnabled(false);
		getFormFieldsPanel().add(PhytoGPWidth_Desc, getStr("9, %d"));

		currentY += 2;
		PHYTOUserfile_Title = new JLabel("PHYTO.Userfile :");
		PHYTOUserfile_Title.setEnabled(false);
		getFormFieldsPanel().add(PHYTOUserfile_Title, getStr("2, %d, right, default"));

		PHYTOUserfile_Jtextfield = new JTextField();
		PHYTOUserfile_Jtextfield.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				savePhytoUserFile();
				validateForm();
			}
		});
		PHYTOUserfile_Jtextfield.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent arg0) {
				onUserFileClicked();
				validateForm();
			}
		});
		PHYTOUserfile_Jtextfield.setEnabled(false);
		getFormFieldsPanel().add(PHYTOUserfile_Jtextfield,
				getStr("4, %d, fill, default"));
		PHYTOUserfile_Jtextfield.setColumns(10);

		PHYTOUserfile_Desc = new JLabel(
				"Userfile describing the chlorophyll profile");
		PHYTOUserfile_Desc.setEnabled(false);
		getFormFieldsPanel().add(PHYTOUserfile_Desc, getStr("9, %d"));

		currentY += 2;
		JLabel lblNewLabel_5 = new JLabel("    |--> Mineral-like particles");
		lblNewLabel_5.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblNewLabel_5.setForeground(Color.BLUE);
		getFormFieldsPanel().add(lblNewLabel_5, getStr("2, %d, 3, 1"));

		currentY += 2;
		JSeparator separator_10 = new JSeparator();
		separator_10.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		getFormFieldsPanel().add(separator_10, getStr("9, %d"));

		currentY += 2;
		SEDCsed_Title = new JLabel("SED.Csed* :");
		SEDCsed_Title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(SEDCsed_Title, getStr("2, %d"));

		SEDCsed_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"), new BigDecimal("0.00"), null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(SEDCsed_Title, SEDCsed_Spinner);
		SEDCsed_Spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveSEDCsed();
				validateForm();
			}
		});
		getFormFieldsPanel().add(SEDCsed_Spinner, getStr("4, %d"));

		JLabel lblNewLabel_7 = new JLabel(
				"Concentration of mineral-like particles at sea surface (mg.\u2113-1)");
		getFormFieldsPanel().add(lblNewLabel_7, getStr("9, %d"));

		currentY +=2;
		JLabel lblNewLabel_8 = new JLabel(
				"    |--> Yellow substance and detritus");
		lblNewLabel_8.setFont(new Font("Tahoma", Font.BOLD, 11));
		lblNewLabel_8.setForeground(Color.BLUE);
		getFormFieldsPanel().add(lblNewLabel_8, getStr("2, %d, 3, 1"));

		JSeparator separator_9 = new JSeparator();
		separator_9.setForeground(UIManager
				.getColor("InternalFrame.inactiveTitleGradient"));
		getFormFieldsPanel().add(separator_9, getStr("9, %d"));

		currentY += 2;
		YSAbs440_Title = new JLabel("YS.Abs440* :");
		YSAbs440_Title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(YSAbs440_Title, getStr("2, %d"));

		YSAbs440_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"), new BigDecimal("0.00"), null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(YSAbs440_Title, YSAbs440_Spinner);
		YSAbs440_Spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				BigDecimal limit = new BigDecimal("0.0");
				setYSSwaVisible(getYSAbs440().doubleValue() >= limit.doubleValue());

				saveYSAbs440();
				validateForm();
			}
		});
		getFormFieldsPanel().add(YSAbs440_Spinner, getStr("4, %d"));

		JLabel lblNewLabel_10 = new JLabel(
				"Absorption coefficient of yellow substance at 440 nm (m-1)");
		getFormFieldsPanel().add(lblNewLabel_10, getStr("9, %d"));

		currentY += 2;
		YSSwa_Title = new JLabel("YS.Swa* :");
		YSSwa_Title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(YSSwa_Title, getStr("2, %d"));

		YSSwa_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"), new BigDecimal("0.00"), null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(YSSwa_Title, YSSwa_Spinner);
        YSSwa_Spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				/*saveYSSwa();
				validateForm();*/
			}
		});
		getFormFieldsPanel().add(YSSwa_Spinner, getStr("4, %d"));

		YSSwa_Desc = new JLabel(
				"Exponential slope of the spectral variation of the yellow substance absorption coefficient (m-1)");
		YSSwa_Desc
				.setToolTipText("Exponential slope of the spectral variation of the yellow substance absorption coefficient (m-1)");
		getFormFieldsPanel().add(YSSwa_Desc, getStr("9, %d"));

		currentY += 2;
		DETAbs440_Title = new JLabel("DET.Abs440* :");
		DETAbs440_Title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(DETAbs440_Title, getStr("2, %d"));

		DETAbs440_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"), null, null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(DETAbs440_Title, DETAbs440_Spinner);
		DETAbs440_Spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveDETAbs400();
				validateForm();
			}
		});
		getFormFieldsPanel().add(DETAbs440_Spinner, getStr("4, %d"));

		DETAbs440_Desc = new JLabel(
				"Absorption coefficient of detritus at 440 nm (m-1)");
		getFormFieldsPanel().add(DETAbs440_Desc, getStr("9, %d"));

		currentY += 2;
		DETSwa_Title = new JLabel("DET.Swa* :");
		DETSwa_Title.setHorizontalAlignment(SwingConstants.RIGHT);
		getFormFieldsPanel().add(DETSwa_Title, getStr("2, %d"));

		DETSwa_Spinner = new JSpinnerRangedValue(new SpinnerBigDecimalModel(new BigDecimal("0.00"), new BigDecimal("0.00"), null, new BigDecimal("0.01")));
        PropertiesManager.getInstance().register(DETSwa_Title, DETSwa_Spinner);
		DETSwa_Spinner.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				saveDETSwa();
				validateForm();
			}
		});
		getFormFieldsPanel().add(DETSwa_Spinner, getStr("4, %d"));

		DETSwa_Desc = new JLabel(
				"Exponential slope of the spectral variation of the detritus absorption coefficient (m-1)");
		getFormFieldsPanel().add(DETSwa_Desc, getStr("9, %d"));
	}

	protected void onUserFileClicked() {
		final JFileChooser fc = new JFileChooser();
		fc.setApproveButtonText("OK");
		fc.setDialogTitle("Select your file");
		fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
		int returnVal = fc.showOpenDialog(this);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            //This is where a real application would open the file.
            PHYTOUserfile_Jtextfield.setText(file.getAbsolutePath());
            PHYTOUserfile_Jtextfield.setToolTipText(file.getAbsolutePath());
        }
	}

	@Override
	protected void onResetBtnAction() {
		int n = JOptionPane
				.showConfirmDialog(
						this,
						"This action erases all user modifications, and reload form fields\n as if the application just come to be installed.\n\nAre you sure you want to reset all fields ?",
						"Confirm reset action ?", JOptionPane.YES_NO_OPTION);
		if (n == JOptionPane.YES_OPTION) {
			try {
				PreferencesFactory.getInstance().getAtmosphericAndSeaProfiles()
						.reset();
			} catch (InitException e_) {
				log.error(e_.getMessage(), e_);
			}
			init();
		}
	}

	protected void setYSSwaVisible(boolean isVisible_) {
		YSSwa_Desc.setEnabled(isVisible_);
		YSSwa_Spinner.setEnabled(isVisible_);
		YSSwa_Title.setEnabled(isVisible_);

	}
	protected void setHomogeneousPorfileVisible(boolean isVisible_) {
		PHYTOChl_Desc.setEnabled(isVisible_);
		PHYTOChl_Spinner.setEnabled(isVisible_);
		PHYTOChl_Title.setEnabled(isVisible_);
	}
	protected void setPhytoGaussianProfileVisible(boolean isVisible_) {
		PhytoGPChlbg_Desc.setEnabled(isVisible_);
		PhytoGPChlbg_Spinner.setEnabled(isVisible_);
		PhytoGPChlbg_Title.setEnabled(isVisible_);

		PhytoGPChlzmax_Desc.setEnabled(isVisible_);
		PhytoGPChlzmax_Spinner.setEnabled(isVisible_);
		PhytoGPChlzmax_Title.setEnabled(isVisible_);

		PhytoGPDeep_Desc.setEnabled(isVisible_);
		PhytoGPDeep_Spinner.setEnabled(isVisible_);
		PhytoGPDeep_Title.setEnabled(isVisible_);

		PhytoGPWidth_Desc.setEnabled(isVisible_);
		PhytoGPWidth_Spinner.setEnabled(isVisible_);
		PhytoGPWidth_Title.setEnabled(isVisible_);
	}

	protected void setPhytoUserProfileVisible(boolean isVisible_) {
		PHYTOUserfile_Title.setEnabled(isVisible_);
		PHYTOUserfile_Jtextfield.setEnabled(isVisible_);
		PHYTOUserfile_Desc.setEnabled(isVisible_);
	}

	protected void setPhytoPrfilTypeVisible(boolean isVisible_) {
		PHYTOChl_Desc.setEnabled(isVisible_);
		PHYTOChl_Spinner.setEnabled(isVisible_);
		PHYTOChl_Title.setEnabled(isVisible_);
	}

	protected void setAPMotValueVisible(boolean isVisible_) {
		APMot_Spinner.setEnabled(isVisible_);
		APMot_title.setEnabled(isVisible_);

		APPressure_Spinner.setEnabled(!isVisible_);
		APPressureTitle.setEnabled(!isVisible_);

		checkAPHRValueEnable();
		validateForm();
	}

    protected void setAPPressureValueVisible(boolean isVisible_) {
        APMot_Spinner.setEnabled(!isVisible_);
        APMot_title.setEnabled(!isVisible_);

        APPressure_Spinner.setEnabled(isVisible_);
        APPressureTitle.setEnabled(isVisible_);

        checkAPHRValueEnable();
        validateForm();
    }

	protected void checkAPHRValueEnable() {
		BigDecimal limit = new BigDecimal("0.0001");
		if ((APMOT_radioBtn.isSelected() && (getAPMot() != null && getAPMot().doubleValue() >= limit.doubleValue()))
				|| (APPressure_radioBtn.isSelected() && (getAPPressure() != null && getAPPressure().doubleValue() >= limit.doubleValue()))) {
			setAPHRVisible(true);
		} else {
			setAPHRVisible(false);
		}
	}

	protected void setAPHRVisible(boolean isVisible_) {
		APHR_Desc.setEnabled(isVisible_);
		APHR_Spinner.setEnabled(isVisible_);
		APHR_title.setEnabled(isVisible_);
	}

	protected void setAPHAVisible(boolean isVisible_) {
		APHA_Desc.setEnabled(isVisible_);
		APHA_Spinner.setEnabled(isVisible_);
		APHA_Title.setEnabled(isVisible_);
	}

	private void setSEABotTypeVisible(boolean isVisible_) {
		SEABotType_Title.setEnabled(isVisible_);
		SEABotAlbedo_Spinner.setEnabled(isVisible_);
		SEABotType_Desc.setEnabled(isVisible_);
	}

	@Override
	public boolean isFormValid() {
		return validateAtmosphericProfileForm() && validateSeaProfileForm();
	}

}
