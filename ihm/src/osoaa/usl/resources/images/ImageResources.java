package osoaa.usl.resources.images;

import java.awt.Image;
import java.awt.Toolkit;

import javax.swing.Icon;
import javax.swing.ImageIcon;

public class ImageResources {

	private static Image getImage(String name_){
		return Toolkit.getDefaultToolkit().getImage(ImageResources.class.getResource("/osoaa/usl/resources/images/" + name_));
	}
	
	public static Image getCNRSINSUImage(){
		return getImage("CNRS_INSU_h70.png");
	}
	
	public static Image getCNRSImage(){
		return getImage("logo-CNRS_sansINSU_h90.png");
	}
	
	public static Image getLogoCNESImage(){
		return getImage("logo_cnes_2010_h70.png");
	}
	
	public static Image getLogoCNESNewImage(){
		return getImage("logoCNES_h70.png");
	}
	
	public static Image getLogoCNESNewImageH110(){
		return getImage("logoCNES_h110.png");
	}
	
	public static Image getLogIUFImage(){
		return getImage("logo_iuf_h70.png");
	}
	
	public static Image getLogIUF2015Image(){
		return getImage("logo_iuf_2015_h70.png");
	}

	public static Image getLogRepFrancaiseImage(){
		return getImage("logo_rf_h130.png");
	}
	
	public static Image getUPMImage(){
		return getImage("UPMC_3_h70.png");
	}
	
	public static Image getSorbonneImage(){
		return getImage("LOGO_Sorbonne_h70.png");
	}
	
	public static Image getFavIcoAsImage(){
		return getImage("favIconImg.png");
	}
	
	public static Icon getIconFromImage(Image img_){
		return new ImageIcon(img_);
	}

	public static Image getCSLogoImage() {
		return getImage("csLogo_h50.png");
	}

	public static Image getCSBigLogoImage() {
		return getImage("csLogo_hbig.png");
	}
	
	public static Image getOceanImg() {
		return getImage("oceanimg.png");
	}
}
