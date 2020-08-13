#ifdef EMBEDED_VERSION
extern char* _binary_version_start;
#else
char* _binary_version_start = "development";
#endif

extern char* get_binary_version_start(void) {
  return _binary_version_start;
}
