package osoaa.bll.exception;


public class InitException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public InitException(Exception e_) {
		super(e_);
	}

	public InitException(String message_) {
		super(message_);
	}

}
