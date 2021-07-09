# Container based wallet build
The script builds release APK inside a container. One can use the reproducible isolated build for independent app distribution systems like FDroid.

Build command:

    docker build -t imagename -f Dockerfile .
