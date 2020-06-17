#include <jni.h>
#include <assert.h>
#include <stdlib.h>
#include <android/log.h>
#include <HaskellActivity.h>
#include "MainWidget.h"

void android_open_url(jobject activity, const char* str) {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "android_open_url", "attached to jvm");

  jclass openUrlClass = (*env)->FindClass(env, "org/ergvein/OpenUrl");
  assert(openUrlClass);
  __android_log_write(ANDROID_LOG_DEBUG, "android_open_url", "got OpenUrl class");

  jmethodID openUrl = (*env)->GetStaticMethodID(env, openUrlClass, "openUrl", "(Lsystems/obsidian/HaskellActivity;Ljava/lang/String;)V");
  assert(openUrl);
  __android_log_write(ANDROID_LOG_DEBUG, "android_open_url", "got method openUrl");

  jstring urlStr = (*env)->NewStringUTF(env, str);
  assert(urlStr);
  __android_log_write(ANDROID_LOG_DEBUG, "android_open_url", "created url string");

  (*env)->CallStaticVoidMethod(env, openUrlClass, openUrl, activity, urlStr);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "android_open_url", "Failed to call openUrl");
    (*env)->ExceptionDescribe(env);
  }
}
