package osoaa.bll.preferences;

import java.math.BigDecimal;
import java.util.Properties;

import osoaa.bll.exception.InitException;

public interface IOutputSpecificities {
	
	Properties getAllCommandProperties();
	
	void reset() throws InitException;
	
	void setOSOAAViewPhi(BigDecimal value_);

    BigDecimal getOSOAAViewPhi();


	void setOSOAAViewZ(BigDecimal value_);

    BigDecimal getOSOAAViewZ();


	void setOSOAAViewVZA(BigDecimal value_);

    BigDecimal getOSOAAViewVZA();


	void setSOSIGmax(Integer value_);

	Integer getSOSIGmax();


	void setSOSLog(String value_);

	String getSOSLog();


	void setSOSResFileBin(String value_);

	String getSOSResFileBin();


	void setOSOAAResFilevsVZA(String value_);

	String getOSOAAResFilevsVZA();


	void setOSOAAResFilevsZ(String value_);

	String getOSOAAResFilevsZ();


	void setOSOAAResFileAdv(String value_);

	String getOSOAAResFileAdv();
	
	
	void setOSOAAResFileAdvDown(String value_);

	String getOSOAAResFileAdvDown();

	void init();

    void setOSOAALog(String osoaaLog);

    String getOSOAALog();

    void setOSOAAViewLevel(Integer value_);

    Integer getOSOAAViewLevel();

    void enablePreferences(String prefix);

    void disablePreferences(String prefix);

    boolean getCheckBoxStatus();

    void setCheckBoxStatus(boolean status);
}
