#include <jni.h>
#include <assert.h>
#include <stdlib.h>
#include <android/log.h>
#include <HaskellActivity.h>
#include "MainWidget.h"

void android_camera_open(jobject activity, const char* str) {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "android_camera_open", "attached to jvm");

  jclass cameraClass = (*env)->FindClass(env, "org/ergvein/Camera");
  assert(cameraClass);
  __android_log_write(ANDROID_LOG_DEBUG, "android_camera_open", "got Camera class");

  jmethodID cameraOpen = (*env)->GetStaticMethodID(env, cameraClass, "cameraOpen", "(Lsystems/obsidian/HaskellActivity;Ljava/lang/String;)V");
  assert(cameraOpen);
  __android_log_write(ANDROID_LOG_DEBUG, "android_camera_open", "got method cameraOpen");

  jstring urlStr = (*env)->NewStringUTF(env, str);
  assert(urlStr);
  __android_log_write(ANDROID_LOG_DEBUG, "android_camera_open", "created strings for camera");

  (*env)->CallStaticVoidMethod(env, cameraClass, cameraOpen, activity, urlStr);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "android_camera_open", "Failed to call cameraOpen");
    (*env)->ExceptionDescribe(env);
  }
}
