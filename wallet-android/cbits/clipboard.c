#include <jni.h>
#include <assert.h>
#include <stdlib.h>
#include <android/log.h>
#include <HaskellActivity.h>
#include "MainWidget.h"

void android_copy_str(jobject activity, const char* str) {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "android_copy_str", "attached to jvm");

  jclass cntxClass = (*env)->FindClass(env, "android/content/Context");
  assert(cntxClass);
  __android_log_write(ANDROID_LOG_DEBUG, "android_copy_str", "got context");

  jmethodID getSystemService = (*env)->GetMethodID(env, cntxClass, "getSystemService", "(Ljava/lang/String;)Ljava/lang/Object;");
  assert(getSystemService);
  __android_log_write(ANDROID_LOG_DEBUG, "android_copy_str", "got method getSystemService");

  jstring CLIPBOARD_SERVICE = (*env)->NewStringUTF(env, "clipboard");
  assert(CLIPBOARD_SERVICE);
  __android_log_write(ANDROID_LOG_DEBUG, "android_copy_str", "got created str for 'clipboard'");

  jobject mngObj = (*env)->CallObjectMethod(env, activity, getSystemService, CLIPBOARD_SERVICE);
  (*env)->DeleteLocalRef(env, CLIPBOARD_SERVICE);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "android_copy_str", "Failed to get ClipboardManager");
    (*env)->ExceptionDescribe(env);
  }
  assert(mngObj);
  __android_log_write(ANDROID_LOG_DEBUG, "android_copy_str", "got ClipboardManager");

  jclass clipDataClass = (*env)->FindClass(env, "android/content/ClipData");
  assert(clipDataClass);
  __android_log_write(ANDROID_LOG_DEBUG, "android_copy_str", "got class for ClipData");

  jmethodID newPlainText = (*env)->GetStaticMethodID(env, clipDataClass, "newPlainText", "(Ljava/lang/CharSequence;Ljava/lang/CharSequence;)Landroid/content/ClipData;");
  assert(newPlainText);
  __android_log_write(ANDROID_LOG_DEBUG, "android_copy_str", "got method newPlainText");

  jstring clipLabel = (*env)->NewStringUTF(env, "panax_clipboard");
  assert(clipLabel);
  jstring clipStr = (*env)->NewStringUTF(env, str);
  assert(clipStr);
  __android_log_write(ANDROID_LOG_DEBUG, "android_copy_str", "created strings for clipboard");

  jobject clipData = (*env)->CallStaticObjectMethod(env, clipDataClass, newPlainText, clipLabel, clipStr);
  (*env)->DeleteLocalRef(env, clipLabel);
  (*env)->DeleteLocalRef(env, clipStr);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "android_copy_str", "Failed to create ClipData");
    (*env)->ExceptionDescribe(env);
  }
  assert(clipData);
  __android_log_write(ANDROID_LOG_DEBUG, "android_copy_str", "called newPlainText");

  jclass mngClass = (*env)->FindClass(env, "android/content/ClipboardManager");
  assert(mngClass);
  __android_log_write(ANDROID_LOG_DEBUG, "android_copy_str", "Got ClipboardManager class");

  jmethodID setPrimaryClip = (*env)->GetMethodID(env, mngClass, "setPrimaryClip", "(Landroid/content/ClipData;)V");
  assert(setPrimaryClip);
  __android_log_write(ANDROID_LOG_DEBUG, "android_copy_str", "Got setPrimaryClip method");

  (*env)->CallVoidMethod(env, mngObj, setPrimaryClip, clipData);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "android_copy_str", "Failed to call setPrimaryClip");
    (*env)->ExceptionDescribe(env);
  }
  assert(clipData);
  __android_log_write(ANDROID_LOG_DEBUG, "android_copy_str", "called setPrimaryClip method");

}

jstring android_paste_str(jobject activity) {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "android_paste_str", "attached to jvm");

  jclass clipboardClass = (*env)->FindClass(env, "org/panax/Clipboard");
  assert(clipboardClass);
  __android_log_write(ANDROID_LOG_DEBUG, "android_paste_str", "got Clipboard class");

  jmethodID pasteStr = (*env)->GetStaticMethodID(env, clipboardClass, "pasteStr", "(Lsystems/obsidian/HaskellActivity;)Ljava/lang/String;");
  assert(pasteStr);
  __android_log_write(ANDROID_LOG_DEBUG, "android_paste_str", "got method pasteStr");

  jobject resStrObj = (*env)->CallStaticObjectMethod(env, clipboardClass, pasteStr, activity);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "android_paste_str", "Failed to call pasteStr");
    (*env)->ExceptionDescribe(env);
  }
  assert(resStrObj);
  __android_log_write(ANDROID_LOG_DEBUG, "android_paste_str", "called pasteStr method");

  return resStrObj;
}

const char* read_paste_str(jstring jstr) {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "read_paste_str", "attached to jvm");

  const char* resStr = (*env)->GetStringUTFChars(env, jstr, NULL);
  assert(resStr);

  return resStr;
}

void release_paste_str(jstring jstr, const char* str) {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "release_paste_str", "attached to jvm");

  (*env)->ReleaseStringUTFChars(env, jstr, str);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "release_paste_str", "Failed to call ReleaseStringUTFChars");
    (*env)->ExceptionDescribe(env);
  }

}
