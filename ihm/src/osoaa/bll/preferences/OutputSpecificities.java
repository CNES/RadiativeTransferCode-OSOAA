package osoaa.bll.preferences;

import java.math.BigDecimal;
import java.util.Properties;

import osoaa.bll.exception.InitException;
import osoaa.bll.properties.PropertiesManager;
import osoaa.common.string.StringUtils;
import osoaa.dal.preferences.DALOutputSpecificities;

public class OutputSpecificities implements IOutputSpecificities {

	private DALOutputSpecificities dalPreferences;

	public OutputSpecificities() throws InitException {
		dalPreferences = new DALOutputSpecificities();
	}


	@Override
	public void init() {
		// We check each properties, to check if a value already exists
		// if no value is defined, we set the value from the properties

        setOSOAAViewPhi(getOSOAAViewPhi());

        setOSOAAViewZ(getOSOAAViewZ());

		setOSOAAViewVZA(getOSOAAViewVZA());

		setSOSIGmax( getSOSIGmax() );

		setSOSLog(getSOSLog());

		setSOSResFileBin( getSOSResFileBin() );

		setOSOAAResFilevsVZA( getOSOAAResFilevsVZA() );

		setOSOAAResFilevsZ(getOSOAAResFilevsZ());

		setOSOAAResFileAdv(getOSOAAResFileAdv());

        setOSOAAResFileAdvDown(getOSOAAResFileAdvDown());

        setOSOAALog(getOSOAALog());
	}

    @Override
    public void setOSOAALog(String osoaaLog) {
        dalPreferences.putProperty("OSOAA.Log", osoaaLog);
    }

    @Override
    public String getOSOAALog()
    {
        return StringUtils.asStringValue(dalPreferences.getProperty("OSOAA.Log", String.class, ""));
    }

    @Override
    public void setOSOAAViewLevel(Integer value_) {
        dalPreferences.putProperty("OSOAA.View.Level", value_);
    }

    @Override
    public Integer getOSOAAViewLevel() {
        return (Integer)dalPreferences.getProperty("OSOAA.View.Level", Integer.class, "0");
    }

    @Override
    public void enablePreferences(String prefix) {
        dalPreferences.enablePreferences(prefix);
    }

    @Override
    public void disablePreferences(String prefix) {
        dalPreferences.disablePreferences(prefix);
    }

    @Override
    public boolean getCheckBoxStatus() {
        return (Boolean)dalPreferences.getProperty("@OSOAA.checkbox", Boolean.class, "false");
    }

    @Override
    public void setCheckBoxStatus(boolean status) {
        dalPreferences.putProperty("@OSOAA.checkbox", status);
    }

    @Override
	public void setOSOAAViewPhi(BigDecimal value_) {
	    dalPreferences.putProperty("OSOAA.View.Phi", value_);
	}

	@Override
	public BigDecimal getOSOAAViewPhi() {
		return (BigDecimal)dalPreferences.getProperty("OSOAA.View.Phi", BigDecimal.class, "0.0");
	}


	@Override
	public void setOSOAAViewZ(BigDecimal value_) {
	    dalPreferences.putProperty("OSOAA.View.Z", value_);
	}

	@Override
	public BigDecimal getOSOAAViewZ() {
		return (BigDecimal)dalPreferences.getProperty("OSOAA.View.Z", BigDecimal.class, "0.0");
	}


	@Override
	public void setOSOAAViewVZA(BigDecimal value_) {
	    dalPreferences.putProperty("OSOAA.View.VZA", value_);
	}

	@Override
	public BigDecimal getOSOAAViewVZA() {
        Object value = dalPreferences.getProperty("OSOAA.View.VZA", BigDecimal.class, "0");
        if(value instanceof BigDecimal)
        {
            return (BigDecimal) value;
        }
        else
        {
            return null;
        }
	}


	@Override
	public void setSOSIGmax(Integer value_) {
	    dalPreferences.putProperty("SOS.IGmax", value_);
	}

	@Override
	public Integer getSOSIGmax() {
		return (Integer)dalPreferences.getProperty("SOS.IGmax", Integer.class, PropertiesManager.getInstance().getCteDefaultIgmax().toString());
	}


	@Override
	public void setSOSLog(String value_) {
	    dalPreferences.putProperty("SOS.Log", value_);
	}

	@Override
	public String getSOSLog() {
		return StringUtils.asStringValue(dalPreferences.getProperty("SOS.Log", String.class, ""));
	}


	@Override
	public void setSOSResFileBin(String value_) {
	    dalPreferences.putProperty("SOS.ResFile.Bin", value_);
	}

	@Override
	public String getSOSResFileBin() {
		return StringUtils.asStringValue(dalPreferences.getProperty("SOS.ResFile.Bin", String.class, PropertiesManager.getInstance().getCteDefaultFicsosResBin()));
	}


	@Override
	public void setOSOAAResFilevsVZA(String value_) {
	    dalPreferences.putProperty("OSOAA.ResFile.vsVZA", value_);
	}

	@Override
	public String getOSOAAResFilevsVZA() {
		return StringUtils.asStringValue(dalPreferences.getProperty("OSOAA.ResFile.vsVZA", String.class, PropertiesManager.getInstance().getCteDefaultFicsosResVsVza()));
	}


	@Override
	public void setOSOAAResFilevsZ(String value_) {
	    dalPreferences.putProperty("OSOAA.ResFile.vsZ", value_);
	}

	@Override
	public String getOSOAAResFilevsZ() {
		return StringUtils.asStringValue(dalPreferences.getProperty("OSOAA.ResFile.vsZ", String.class, "LUM_vsZ.txt"));
	}


	@Override
	public void setOSOAAResFileAdv(String value_) {
	    dalPreferences.putProperty("OSOAA.ResFile.Adv.Up", value_);
	}

	@Override
	public String getOSOAAResFileAdv() {
		return StringUtils.asStringValue(dalPreferences.getProperty("OSOAA.ResFile.Adv.Up", String.class, "LUM_Advanced_Up.txt"));
	}

	@Override
	public void reset() throws InitException {
		dalPreferences.reset();
		init();
	}
	
	@Override
	public Properties getAllCommandProperties() {
		return dalPreferences.getAllCommandProperties();
	}

	@Override
	public void setOSOAAResFileAdvDown(String value_) {
		dalPreferences.putProperty("OSOAA.ResFile.Adv.Down", value_);
	}

	@Override
	public String getOSOAAResFileAdvDown() {
		return StringUtils.asStringValue(dalPreferences.getProperty("OSOAA.ResFile.Adv.Down", String.class, "LUM_Advanced_Down.txt"));
	}

}