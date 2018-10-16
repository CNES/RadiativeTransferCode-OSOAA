package osoaa.common.types;

import java.math.BigDecimal;

interface Factory<T> {
	  T apply(String parsingCandidate);
	}

class IntegerFactory<T> implements Factory<Integer>
{
	@Override
	public Integer apply(String parsingCandidate) {
		return Integer.parseInt(parsingCandidate);
	}
}

class DoubleFactory<T> implements Factory<Double>
{
	@Override
	public Double apply(String parsingCandidate) {
		return Double.parseDouble(parsingCandidate);
	}
}

class BooleanFactory<T> implements Factory<Boolean>
{
	@Override
	public Boolean apply(String parsingCandidate) {
		return Boolean.parseBoolean(parsingCandidate);
	}
}

class BigDecimalFactory<T> implements Factory<BigDecimal>
{
    @Override
    public BigDecimal apply(String parsingCandidate)
    {
        return new BigDecimal(parsingCandidate);
    }
}


public class WithTypeExtractor {

	public static <T> T extractObjectType(Class<T> klass, String objStr){
		
		Factory<T> fact = null;

        if(klass.isAssignableFrom(BigDecimal.class))
        {
            fact = (Factory<T>) new BigDecimalFactory<T>();
        }
		
		if(klass.isAssignableFrom(Double.class))
		{
			fact = (Factory<T>) new DoubleFactory<T>();
		}
		
		if(klass.isAssignableFrom(Integer.class))
		{
			fact = (Factory<T>) new IntegerFactory<T>();
		}
		
		if(klass.isAssignableFrom(Boolean.class))
		{
			fact = (Factory<T>) new BooleanFactory<T>();
		}
		
		if(fact == null)
		{
			return (T) objStr;
		}
		
		return fact.apply(objStr);
	}
	
}
