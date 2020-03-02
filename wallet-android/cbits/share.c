#include <jni.h>
#include <assert.h>
#include <stdlib.h>
#include <android/log.h>
#include <HaskellActivity.h>
#include "MainWidget.h"

void android_share_url(jobject activity, const char* str) {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "android_share_url", "attached to jvm");

  jclass shareClass = (*env)->FindClass(env, "org/ergvein/Share");
  assert(shareClass);
  __android_log_write(ANDROID_LOG_DEBUG, "android_share_url", "got Share class");

  jmethodID shareUrl = (*env)->GetStaticMethodID(env, shareClass, "shareUrl", "(Lsystems/obsidian/HaskellActivity;Ljava/lang/String;)V");
  assert(shareUrl);
  __android_log_write(ANDROID_LOG_DEBUG, "android_share_url", "got method shareUrl");

  jstring urlStr = (*env)->NewStringUTF(env, str);
  assert(urlStr);
  __android_log_write(ANDROID_LOG_DEBUG, "android_share_url", "created strings for share");

  (*env)->CallStaticVoidMethod(env, shareClass, shareUrl, activity, urlStr);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "android_share_url", "Failed to call shareUrl");
    (*env)->ExceptionDescribe(env);
  }
}

void android_send_url(jobject activity, const char* str) {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "android_send_url", "attached to jvm");

  jclass shareClass = (*env)->FindClass(env, "org/ergvein/Share");
  assert(shareClass);
  __android_log_write(ANDROID_LOG_DEBUG, "android_send_url", "got Share class");

  jmethodID sendUrl = (*env)->GetStaticMethodID(env, shareClass, "sendUrl", "(Lsystems/obsidian/HaskellActivity;Ljava/lang/String;)V");
  assert(sendUrl);
  __android_log_write(ANDROID_LOG_DEBUG, "android_send_url", "got method sendUrl");

  jstring urlStr = (*env)->NewStringUTF(env, str);
  assert(urlStr);
  __android_log_write(ANDROID_LOG_DEBUG, "android_send_url", "created strings for share send");

  (*env)->CallStaticVoidMethod(env, shareClass, sendUrl, activity, urlStr);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "android_send_url", "Failed to call sendUrl");
    (*env)->ExceptionDescribe(env);
  }
}

void android_share_img(jobject activity, const char* str) {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "android_share_img", "attached to jvm");

  jclass shareClass = (*env)->FindClass(env, "org/ergvein/Share");
  assert(shareClass);
  __android_log_write(ANDROID_LOG_DEBUG, "android_share_img", "got Share class");

  jmethodID shareImg = (*env)->GetStaticMethodID(env, shareClass, "shareImg", "(Lsystems/obsidian/HaskellActivity;Ljava/lang/String;)V");
  assert(shareImg);
  __android_log_write(ANDROID_LOG_DEBUG, "android_share_img", "got method shareImg");

  jstring imgStr = (*env)->NewStringUTF(env, str);
  assert(imgStr);
  __android_log_write(ANDROID_LOG_DEBUG, "android_share_img", "created strings for share img");

  (*env)->CallStaticVoidMethod(env, shareClass, shareImg, activity, imgStr);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "android_share_img", "Failed to call shareImg");
    (*env)->ExceptionDescribe(env);
  }
}
