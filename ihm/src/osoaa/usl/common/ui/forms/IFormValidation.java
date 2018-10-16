package osoaa.usl.common.ui.forms;

public interface IFormValidation {

	boolean isFormValid();
	
	void addFormValidationListener(IFormValidationListener listener_);
	void removeFormValidationListener(IFormValidationListener listener_);
}
