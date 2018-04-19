cd "${0%/*}"/proto # cd to proto directory

mkdir -p cpp
mkdir -p python
protoc --python_out=python --cpp_out=cpp DeviceConfig.proto
