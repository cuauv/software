cd "${0%/*}"/proto # cd to proto directory

mkdir -p cpp
protoc --cpp_out=cpp AUVFirmware.proto
