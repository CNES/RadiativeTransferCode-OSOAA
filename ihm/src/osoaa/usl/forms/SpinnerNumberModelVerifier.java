package osoaa.usl.forms;

import javax.swing.*;
import java.math.BigDecimal;

public class SpinnerNumberModelVerifier extends InputVerifier implements SpinnerNumberErrorInterface
{
    private SpinnerNumberModel _model;
    private String _errorTitle;
    private String _errorMessage;

    public SpinnerNumberModelVerifier(SpinnerNumberModel model)
    {
        assert(model != null);
        _model = model;
    }

    public boolean verify(JComponent input)
    {
        JFormattedTextField  tf = (JFormattedTextField ) input;

        String text = tf.getText();
        try
        {
            if(text.contains(","))
            {
                text = text.replaceAll(",","");
            }
            BigDecimal anum = new BigDecimal(text);
        }
        catch (NumberFormatException nfe)
        {
            _errorMessage = "Wrong number format !";
            _errorTitle = "Invalid Value";
            return false;
        }

        try
        {
            Integer num = Integer.parseInt(text);

            // we need some padding...
            text = text + ".0";
        }
        catch (NumberFormatException nfe)
        {
            // do nothing...
        }

        BigDecimal num = new BigDecimal(text);

        if ((_model != null) && (num != null))
        {
            if(_model.getMinimum() != null)
            {
                BigDecimal theMin = null;
                if(_model.getMinimum() instanceof Integer)
                {
                    Integer min = (Integer) _model.getMinimum();
                    theMin = new BigDecimal(min.toString());
                }
                else if (_model.getMinimum() instanceof BigDecimal)
                {
                    BigDecimal min = (BigDecimal) _model.getMinimum();
                    theMin = min;
                }

                if(theMin.compareTo(num) > 0)
                {
                    _errorTitle = "Invalid Value";
                    _errorMessage = "Specified value below minimum ! : " + _model.getMinimum();
                    return false;
                }
            }
            if(_model.getMaximum() != null)
            {
                BigDecimal theMax = null;
                if(_model.getMaximum() instanceof Integer)
                {
                    Integer max = (Integer) _model.getMaximum();
                    theMax = new BigDecimal(max.toString());
                }
                else if (_model.getMaximum() instanceof BigDecimal)
                {
                    BigDecimal max = (BigDecimal) _model.getMaximum();
                    theMax = max;
                }

                if(theMax.compareTo(num) < 0)
                {
                    _errorTitle = "Invalid Value";
                    _errorMessage = "Specified value above maximum ! :" + _model.getMaximum();
                    return false;
                }
            }
        }

        return true;
    }

    public boolean shouldYieldFocus(JComponent input) {
        return verify(input);
    }

    public String getErrorMessage() {
        return _errorMessage;
    }

    public String getErrorTitle() {
        return _errorTitle;
    }
}
