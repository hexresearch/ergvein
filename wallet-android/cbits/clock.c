#include <jni.h>
#include <assert.h>
#include <stdlib.h>
#include <android/log.h>
#include <HaskellActivity.h>
#include "MainWidget.h"

jint android_timezone_offset() {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "android_timezone_offset", "attached to jvm");

  jclass clockClass = (*env)->FindClass(env, "org/panax/Clock");
  assert(clockClass);
  __android_log_write(ANDROID_LOG_DEBUG, "android_timezone_offset", "got Clock class");

  jmethodID getCurrentTimezoneOffset = (*env)->GetStaticMethodID(env, clockClass, "getCurrentTimezoneOffset", "()I");
  assert(getCurrentTimezoneOffset);
  __android_log_write(ANDROID_LOG_DEBUG, "android_timezone_offset", "got method getCurrentTimezoneOffset");

  jint resInt = (*env)->CallStaticIntMethod(env, clockClass, getCurrentTimezoneOffset);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "android_timezone_offset", "Failed to call getCurrentTimezoneOffset");
    (*env)->ExceptionDescribe(env);
  }
  assert(resInt);
  __android_log_write(ANDROID_LOG_DEBUG, "android_timezone_offset", "called getCurrentTimezoneOffset method");

  return resInt;
}
