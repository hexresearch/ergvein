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

const char* android_camera_get_result(jobject activity) {
   JNIEnv *env;
   jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
   assert(attachResult == JNI_OK);
   __android_log_write(ANDROID_LOG_DEBUG, "android_camera_get_result", "attached to jvm");

   jclass cameraClass = (*env)->FindClass(env, "org/ergvein/Camera");
   assert(cameraClass);
   __android_log_write(ANDROID_LOG_DEBUG, "android_camera_get_result", "got Camera class");

   jmethodID cameraGetResult = (*env)->GetStaticMethodID(env, cameraClass, "cameraGetResult", "(Lsystems/obsidian/HaskellActivity;)Ljava/lang/String;");
   assert(cameraGetResult);
   __android_log_write(ANDROID_LOG_DEBUG, "android_camera_get_result", "got method cameraGetResult");

   jobject resStrObj = (*env)->CallStaticObjectMethod(env, cameraClass, cameraGetResult, activity);
   if((*env)->ExceptionOccurred(env)) {
     __android_log_write(ANDROID_LOG_DEBUG, "android_camera_get_result", "Failed to call cameraGetResult");
     (*env)->ExceptionDescribe(env);
   }
   assert(resStrObj);
   __android_log_write(ANDROID_LOG_DEBUG, "android_camera_get_result", "called cameraGetResult method");

   const char* resStr = (*env)->GetStringUTFChars(env, resStrObj, NULL);
   assert(resStr);

   return resStr;
 }
