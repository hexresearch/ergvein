# Container based wallet build
Builds release apk inside container. This reproducible isolated build can be used for independent app distribution system like FDroid.

Build command:

    docker build -t imagename -f Dockerfile .
