#include <jni.h>
#include <assert.h>
#include <stdlib.h>
#include <android/log.h>
#include <HaskellActivity.h>

void android_load_certs(jobject activity) {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "load_certs", "attached to jvm");

  jclass certStoreClass = (*env)->FindClass(env, "haskell/x509android/Certificates");
  assert(certStoreClass);
  __android_log_write(ANDROID_LOG_DEBUG, "load_certs", "got Certificates class");

  jmethodID loadCerts = (*env)->GetStaticMethodID(env, certStoreClass, "loadCerts", "(Lsystems/obsidian/HaskellActivity;)V");
  assert(loadCerts);
  __android_log_write(ANDROID_LOG_DEBUG, "load_certs", "got method loadCerts");

  (*env)->CallStaticVoidMethod(env, certStoreClass, loadCerts, activity);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "load_certs", "Failed to call loadCerts");
    (*env)->ExceptionDescribe(env);
  }
}

int android_certs_length() {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "certs_length", "attached to jvm");

  jclass certStoreClass = (*env)->FindClass(env, "haskell/x509android/Certificates");
  assert(certStoreClass);
  __android_log_write(ANDROID_LOG_DEBUG, "certs_length", "got Certificates class");

  jmethodID lengthMethod = (*env)->GetStaticMethodID(env, certStoreClass, "length", "()I");
  assert(lengthMethod);
  __android_log_write(ANDROID_LOG_DEBUG, "certs_length", "got method lengthMethod");

  int res = (*env)->CallStaticIntMethod(env, certStoreClass, lengthMethod);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "certs_length", "Failed to call lengthMethod");
    (*env)->ExceptionDescribe(env);
  }

  return res;
}

jstring android_get_cert(int i) {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "get_cert", "attached to jvm");

  jclass certStoreClass = (*env)->FindClass(env, "haskell/x509android/Certificates");
  assert(certStoreClass);
  __android_log_write(ANDROID_LOG_DEBUG, "get_cert", "got Certificates class");

  jmethodID getCert = (*env)->GetStaticMethodID(env, certStoreClass, "getCert", "(I)Ljava/lang/String;");
  assert(getCert);
  __android_log_write(ANDROID_LOG_DEBUG, "get_cert", "got method getCert");

  jobject res = (*env)->CallStaticObjectMethod(env, certStoreClass, getCert, i);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "get_cert", "Failed to call getCert");
    (*env)->ExceptionDescribe(env);
  }
  assert(res);
  __android_log_write(ANDROID_LOG_DEBUG, "android_paste_str", "called pasteStr method");

  return res;
}

const char* read_jstring(jstring jstr) {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "read_jstring", "attached to jvm");

  const char* resStr = (*env)->GetStringUTFChars(env, jstr, NULL);
  assert(resStr);

  return resStr;
}

void release_jstring(jstring jstr, const char* str) {
  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, (void **)&env, NULL);
  assert(attachResult == JNI_OK);
  __android_log_write(ANDROID_LOG_DEBUG, "release_jstring", "attached to jvm");

  (*env)->ReleaseStringUTFChars(env, jstr, str);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "release_jstring", "Failed to call ReleaseStringUTFChars");
    (*env)->ExceptionDescribe(env);
  }

}
