package org.ergvein;

import java.util.*;
import java.util.concurrent.*;

public class Clock {
    private static int getCurrentTimezoneOffset () {
        TimeZone tz = new GregorianCalendar().getTimeZone();

        return (int) TimeUnit.MINUTES.convert(tz.getRawOffset(), TimeUnit.MILLISECONDS);
    }
}
