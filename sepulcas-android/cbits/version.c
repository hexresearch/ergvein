#include <sys/system_properties.h>

int android_get_version() {
    char sdk_ver_str[PROP_VALUE_MAX];
    if (__system_property_get("ro.build.version.sdk", sdk_ver_str)) {
        int sdk_ver = atoi(sdk_ver_str);
        return sdk_ver;
    } else {
        // Not running on Android or SDK version is not available
        return 0;
    }
}