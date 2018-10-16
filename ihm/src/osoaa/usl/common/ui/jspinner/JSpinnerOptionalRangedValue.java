package osoaa.usl.common.ui.jspinner;

import org.apache.log4j.Logger;
import osoaa.usl.forms.SpinnerNumberErrorInterface;
import osoaa.usl.forms.SpinnerNumberModelVerifier;
import osoaa.usl.forms.SpinnerOptionalNumberModelVerifier;

import javax.swing.*;
import javax.swing.text.DefaultFormatter;
import java.awt.*;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

public class JSpinnerOptionalRangedValue extends JSpinner {

    private Logger log = Logger.getLogger(this.getClass().getCanonicalName());

    private JSpinnerOptionalRangedValue() {
        this(null);
    }

    public JSpinnerOptionalRangedValue(SpinnerNumberModel snm_) {
        super();
        SpinnerNumberModel sm = snm_;
        if( sm == null ){
            sm = new SpinnerNumberModel();
        }
        setModel(new OptionalSpinnerNumberModel(sm));
        setEditor(new NumberEditorOptionalValue(this));
        prepare();
    }

    public void prepare()
    {
        final JComponent comp = this.getEditor();
        comp.setInputVerifier(new SpinnerOptionalNumberModelVerifier(((OptionalSpinnerNumberModel) this.getModel()).getSpinnerNumberModel()));
        final JFormattedTextField field = (JFormattedTextField) comp.getComponent(0);
        final JSpinnerOptionalRangedValue reference = this;

        field.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent e) {
                if((e.getKeyCode() == KeyEvent.VK_ENTER) || (e.getKeyCode() == KeyEvent.VK_BACK_SPACE) || (e.getKeyCode() == KeyEvent.VK_DELETE))
                {
                    boolean result = comp.getInputVerifier().verify(field);
                    String thing = field.getText();

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
                String thing = field.getText();

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
    }
}