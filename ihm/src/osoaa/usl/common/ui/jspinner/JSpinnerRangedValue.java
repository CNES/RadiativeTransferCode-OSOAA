package osoaa.usl.common.ui.jspinner;

import org.apache.log4j.Logger;
import osoaa.usl.forms.SpinnerNumberErrorInterface;
import osoaa.usl.forms.SpinnerNumberModelVerifier;

import javax.swing.*;
import javax.swing.text.DefaultFormatter;
import java.awt.*;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

public class JSpinnerRangedValue extends JSpinner {

    private Logger log = Logger.getLogger(this.getClass().getCanonicalName());

    private JSpinnerRangedValue() {
        this(null);
    }

    public JSpinnerRangedValue(SpinnerNumberModel snm_) {
        super();
        SpinnerNumberModel sm = snm_;
        if( sm == null ){
            sm = new SpinnerNumberModel();
        }
        setModel(sm);
        prepare();
    }

    public void prepare()
    {
        final JComponent comp = this.getEditor();
        comp.setInputVerifier(new SpinnerNumberModelVerifier((SpinnerNumberModel) this.getModel()));
        final JFormattedTextField field = (JFormattedTextField) comp.getComponent(0);
        final JSpinnerRangedValue reference = this;

        field.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent e) {
                if((e.getKeyCode() == KeyEvent.VK_ENTER) || (e.getKeyCode() == KeyEvent.VK_BACK_SPACE) || (e.getKeyCode() == KeyEvent.VK_DELETE))
                {
                    boolean result = comp.getInputVerifier().verify(field);
                    if(!result)
                    {
                        int oldCaretPos = field.getCaretPosition();
                        field.setCaretPosition(oldCaretPos);
                        requestFocus();
                    }
                    else
                    {
                        reference.fireStateChanged();
                    }
                }
            }
        });

        field.addFocusListener(new FocusListener() {
            @Override
            public void focusGained(FocusEvent e) {
                //To change body of implemented methods use File | Settings | File Templates.
            }

            @Override
            public void focusLost(FocusEvent e)
            {
                boolean result = comp.getInputVerifier().verify(field);
                if(!result)
                {
                    if(comp.getInputVerifier() instanceof SpinnerNumberErrorInterface)
                    {
                        SpinnerNumberErrorInterface snu = (SpinnerNumberErrorInterface) comp.getInputVerifier();
                        JOptionPane.showMessageDialog(field, snu.getErrorMessage(), snu.getErrorTitle(), JOptionPane.ERROR_MESSAGE);
                    }

                    field.requestFocusInWindow();
                }
                else
                {
                    reference.fireStateChanged();
                }
            }
        });
        DefaultFormatter formatter = (DefaultFormatter) field.getFormatter();
        formatter.setCommitsOnValidEdit(true);

        SpinnerNumberModel sm = (SpinnerNumberModel) getModel();

        if((sm.getMinimum() != null) && (sm.getMaximum() != null))
        {
            String tooltip_text = "Valid range: [" + sm.getMinimum() + ", " + sm.getMaximum() + " ]";
            tooltip_text = tooltip_text.replaceAll("null", "undefined");
            this.setToolTipText(tooltip_text);
        }
        else
        {
            if((sm.getMinimum() != null))
            {
                String tooltip_text = "Valid range: [ Min: " + sm.getMinimum() + " ]";
                this.setToolTipText(tooltip_text);
            }
            else
            {
                String tooltip_text = "Valid range: [ Max: " + sm.getMaximum() + " ]";
                this.setToolTipText(tooltip_text);
            }
        }
    }
}
