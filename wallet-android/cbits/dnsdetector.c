#include <jni.h>
#include <assert.h>
#include <stdlib.h>
#include <android/log.h>
#include <HaskellActivity.h>
#include "MainWidget.h"

jstring android_detectdns(jobject activity) {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "android_detectdns", "attached to jvm");

  jclass detectorClass = (*env)->FindClass(env, "org/ergvein/DnsDetector");
  assert(detectorClass);
  __android_log_write(ANDROID_LOG_DEBUG, "android_detectdns", "got DnsDetector class");

  jmethodID getServers = (*env)->GetStaticMethodID(env, detectorClass, "getServers", "()Ljava/lang/String;");
  assert(getServers);
  __android_log_write(ANDROID_LOG_DEBUG, "android_detectdns", "got method getServers");

  jobject resStrObj = (*env)->CallStaticObjectMethod(env, detectorClass, getServers, activity);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "android_detectdns", "Failed to call getServers");
    (*env)->ExceptionDescribe(env);
  }
  assert(resStrObj);
  __android_log_write(ANDROID_LOG_DEBUG, "android_detectdns", "called getServers method");

  const char* resStr = (*env)->GetStringUTFChars(env, resStrObj, NULL);
  assert(resStr);

  return resStr;
}
