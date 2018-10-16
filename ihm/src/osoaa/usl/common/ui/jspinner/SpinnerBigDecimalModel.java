package osoaa.usl.common.ui.jspinner;

import java.math.BigDecimal;
import javax.swing.SpinnerNumberModel;
public class SpinnerBigDecimalModel extends SpinnerTypedNumberModel<BigDecimal>
{
    public SpinnerBigDecimalModel(final BigDecimal value, final Comparable<BigDecimal> min, final Comparable<BigDecimal> max, final BigDecimal stepSize)
    {
        super(BigDecimal.class, value, min, max, stepSize);
     }

    // -- SpinnerTypedNumberModel methods --
    @Override
    protected BigDecimal stepUp()
    {
        return getValue().add(getStepSize());
    }

    @Override
    protected BigDecimal stepDown() {
        return getValue().subtract(getStepSize());
    }
}