#include <jni.h>
#include <assert.h>
#include <stdlib.h>
#include <android/log.h>
#include <HaskellActivity.h>
#include "MainWidget.h"

void android_set_screen_flag(jobject activity) {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "android_set_screen_flag", "attached to jvm");

  jclass screenManagerClass = (*env)->FindClass(env, "org/ergvein/ScreenManager");
  assert(screenManagerClass);
  __android_log_write(ANDROID_LOG_DEBUG, "android_set_screen_flag", "got ScreenManager class");

  jmethodID setScreenFlag = (*env)->GetStaticMethodID(env, screenManagerClass, "setScreenFlag", "(Lsystems/obsidian/HaskellActivity;)V");
  assert(setScreenFlag);
  __android_log_write(ANDROID_LOG_DEBUG, "android_set_screen_flag", "got method setScreenFlag");

  (*env)->CallStaticVoidMethod(env, screenManagerClass, setScreenFlag, activity);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "android_set_screen_flag", "Failed to call setScreenFlag");
    (*env)->ExceptionDescribe(env);
  }
}

void android_clear_screen_flag(jobject activity) {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "android_clear_screen_flag", "attached to jvm");

  jclass screenManagerClass = (*env)->FindClass(env, "org/ergvein/ScreenManager");
  assert(screenManagerClass);
  __android_log_write(ANDROID_LOG_DEBUG, "android_clear_screen_flag", "got ScreenManager class");

  jmethodID clearScreenFlag = (*env)->GetStaticMethodID(env, screenManagerClass, "clearScreenFlag", "(Lsystems/obsidian/HaskellActivity;)V");
  assert(clearScreenFlag);
  __android_log_write(ANDROID_LOG_DEBUG, "android_clear_screen_flag", "got method clearScreenFlag");

  (*env)->CallStaticVoidMethod(env, screenManagerClass, clearScreenFlag, activity);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "android_clear_screen_flag", "Failed to call clearScreenFlag");
    (*env)->ExceptionDescribe(env);
  }
}
