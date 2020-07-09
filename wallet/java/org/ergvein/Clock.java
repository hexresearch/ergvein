package org.ergvein;

import java.util.*;
import java.util.concurrent.*;

public class Clock {
    private static int getCurrentTimeZoneOffset () {
        TimeZone tz = TimeZone.getDefault();
        return (int) TimeUnit.MINUTES.convert(tz.getRawOffset(), TimeUnit.MILLISECONDS);
    }

    private static String getCurrentTimeZoneId () {
        TimeZone tz = TimeZone.getDefault();
        return tz.getID();
    }
}
