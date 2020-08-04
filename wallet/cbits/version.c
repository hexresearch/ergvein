#ifdef EMBEDED_VERSION
extern char* _binary_version_start;
extern int _binary_version_size;
#else
char* _binary_version_start = "development";
int _binary_version_size = 11;
#endif

extern int get_binary_version_size(void) {
  return _binary_version_size;
}

extern char* get_binary_version_start(void) {
  return _binary_version_start;
}
