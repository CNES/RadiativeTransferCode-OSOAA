package osoaa.usl.resources.icons;

import java.awt.Image;
import java.awt.Toolkit;

import javax.swing.Icon;
import javax.swing.ImageIcon;

public class IconResources {

	private static Image getIcon(String name_){
		return Toolkit.getDefaultToolkit().getImage(IconResources.class.getResource("/osoaa/usl/resources/icons/" + name_));
	}
	
	public static Image getFavIcon(){
		return getIcon("favIcon.gif");
	}
	
	public static Image getItemCheckOKIcon(){
		return getIcon("itemCheckOKIcon.gif");
	}
	
	public static Image getItemCheckKOIcon(){
		return getIcon("itemCheckKOIcon.gif");
	}
	
	public static Image getRunIcon(){
		return getIcon("runIcon.gif");
	}
	
	public static Image getloadingIcon(){
		return getIcon("loadingIcon.gif");
	}
	
	public static Image getResetFormIcon(){
		return getIcon("resetFormIcon.gif");
	}
	
	public static Image getHomeIcon(){
		return getIcon("homeIcon.gif");
	}
	
	public static Image getShowCommandIcon(){
		return getIcon("showCommandIcon.gif");
	}
	
	public static Image getFormValidIcon(){
		return getIcon("formValidIcon.gif");
	}
	public static Image getFormNormalIcon(){
		return getIcon("formNormalIcon.gif");
	}
	public static Image getFormWarningIcon(){
		return getIcon("formWarningIcon.gif");
	}
	
	public static Image getCopyIcon(){
		return getIcon("copyIcon.gif");
	}
	
	public static Image getFormatCommandIcon(){
		return getIcon("formatCommandIcon.gif");
	}
	
	
	
	public static Icon getIconFromImage(Image img_){
		return new ImageIcon(img_);
	}

	public static Image getStopCommandIcon() {
		return getIcon("stopCommandIcon.gif");
	}
	
	public static Image getProcessIcon() {
		return getIcon("processIcon.gif");
	}
	
	public static Image getLoadingAnimateIcon() {
		return getIcon("loadingAnimateIcon.gif");
	}
	
}
