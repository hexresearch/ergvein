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

  jclass clockClass = (*env)->FindClass(env, "org/ergvein/Clock");
  assert(clockClass);
  __android_log_write(ANDROID_LOG_DEBUG, "android_timezone_offset", "got Clock class");

  jmethodID getCurrentTimeZoneOffset = (*env)->GetStaticMethodID(env, clockClass, "getCurrentTimeZoneOffset", "()I");
  assert(getCurrentTimeZoneOffset);
  __android_log_write(ANDROID_LOG_DEBUG, "android_timezone_offset", "got method getCurrentTimeZoneOffset");

  jint resInt = (*env)->CallStaticIntMethod(env, clockClass, getCurrentTimeZoneOffset);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "android_timezone_offset", "Failed to call getCurrentTimeZoneOffset");
    (*env)->ExceptionDescribe(env);
  }
  assert(resInt);
  __android_log_write(ANDROID_LOG_DEBUG, "android_timezone_offset", "called getCurrentTimeZoneOffset method");

  return resInt;
}

const char* android_timezone_id() {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "android_timezone_id", "attached to jvm");

  jclass clockClass = (*env)->FindClass(env, "org/ergvein/Clock");
  assert(clockClass);
  __android_log_write(ANDROID_LOG_DEBUG, "android_timezone_id", "got Clock class");

  jmethodID getCurrentTimeZoneId = (*env)->GetStaticMethodID(env, clockClass, "getCurrentTimeZoneId", "()Ljava/lang/String;");
  assert(getCurrentTimeZoneId);
  __android_log_write(ANDROID_LOG_DEBUG, "android_timezone_id", "got method getCurrentTimeZoneId");

  jobject resStrObj = (*env)->CallStaticObjectMethod(env, clockClass, getCurrentTimeZoneId);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "android_timezone_id", "Failed to call getCurrentTimeZoneId");
    (*env)->ExceptionDescribe(env);
  }
  assert(resStrObj);
  __android_log_write(ANDROID_LOG_DEBUG, "android_timezone_id", "called getCurrentTimeZoneId method");

  const char* resStr = (*env)->GetStringUTFChars(env, resStrObj, NULL);
  assert(resStr);

  return resStr;
}